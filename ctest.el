;;; ctest.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Szymon Pawlus
;;
;; Author: Szymon Pawlus <szymonp@szymon-81lk>
;; Maintainer: Szymon Pawlus <szymonp@szymon-81lk>
;; Created: lutego 25, 2026
;; Modified: lutego 25, 2026
;; Version: 0.1.0
;; Keywords: tools c ctest conveninece
;; Homepage: https://github.com/SzymonPawlus/ctest.el
;; Package-Requires: ((emacs "28.1") (transient "0.3.0") (projectile "1.4.0"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; An interactive, transient-based UI for executing and managing
;; CMake ctest runs within Emacs, functioning similarly to Magit.
;;
;;; Code:


(require 'transient)
(require 'projectile)
(require 'seq)
(require 'json)
(require 'magit-section)
(require 'transient)
(require 'subr-x) ; Required for string-join

(defgroup ctest nil
  "Magit-like interface for CMake ctest."
  :group 'tools
  :prefix "ctest-")

;; Customization variables will be defined here using `defcustom`
(defcustom ctest-executable "ctest"
  "The path to the ctest executable."
  :type 'string
  :group 'ctest)


;;; Variables
(defvar-local ctest-current-build-dir nil
  "The currently active build directory for ctest operations.
This is buffer-local to allow different buffers to target different builds."
  )

(defvar-local ctest-marked-tests nil
  "List of currently marked tests.")

(defvar-local ctest-test-results nil
  "Hash table mapping test names to their result plists.")

(defvar-local ctest-cached-tests nil
  "Cached list of tests to prevent redundant ctest invocations on UI redraw.")

;;; Internal functions
(defun ctest--project-root ()
  "Retrieve the project root from Projectile. Throws an error if not in the project."
  (if(and
      (bound-and-true-p projectile-mode)
      (projectile-project-p))
      (projectile-project-root)
    (error "ctest: Not in a valid Projectile poject")))

(defun ctest--find-build-directories (root)
  "Find top-level CMake build directories under ROOT.
A valid top-level build directory contains 'CMakeCache.txt' and 'CTestTestfile.cmake'."
  (let* ((cache-files (directory-files-recursively root "^CMakeCache\\.txt$"))
         ;; Extract the directory path from the cache file paths
         (build-dirs (mapcar #'file-name-directory cache-files))

         ;; Filter out build directories where testing is not enabled
         (valid-dirs (seq-filter (lambda (dir)
                                   (file-exists-p (expand-file-name "CTestTestfile.cmake" dir)))
                                 build-dirs)))
    valid-dirs))

(defun ctest--extract-test-log (test-name build-dir)
  "Extract the log for TEST-NAME from CTest's LastTest.log in BUILD-DIR."
  (let ((log-file (expand-file-name "Testing/Temporary/LastTest.log" build-dir))
        ;; CTest formats log headers as: "1/5 Testing: test_name"
        (start-regex (format "^[0-9]+/[0-9]+ Testing: %s$" (regexp-quote test-name)))
        ;; The next test starts with a similar pattern
        (end-regex "^[0-9]+/[0-9]+ Testing: ")
        log-content)

    (if (not (file-exists-p log-file))
        (error "ctest: Log file not found at %s" log-file)

      (with-temp-buffer
        ;; Load the entire file into the temporary buffer
        (insert-file-contents log-file)
        (goto-char (point-min))

        ;; Search for the beginning of the specific test's log
        (if (re-search-forward start-regex nil t)
            (let ((start-pos (line-beginning-position)))
              (forward-line 1)

              ;; Search for the beginning of the NEXT test to mark the end,
              ;; or default to the end of the file if it is the last test.
              (let ((end-pos (if (re-search-forward end-regex nil t)
                                 (match-beginning 0)
                               (point-max))))
                ;; Extract the string
                (setq log-content (buffer-substring-no-properties start-pos end-pos))))
          (error "ctest: Log for test '%s' not found in LastTest.log" test-name))))

    log-content))

;;; Interactive Commands
(defun ctest-select-build-directory ()
  "Interactively select a ctest build-directory from the current project."
  (interactive)
  (let* ((root (ctest--project-root))
         (build-dirs (ctest--find-build-directories root))
         (relative-dirs (mapcar (lambda (dir) (file-relative-name dir root)) build-dirs)))
    (if (not relative-dirs)
        (error "ctest: No build directories containing CTestTestfile.cmake found")

      (let* ((choice (completing-read "Select ctest build directory: " relative-dirs nil t))
             (absolute-choice (expand-file-name choice root)))

        (setq ctest-current-build-dir absolute-choice)
        (message "ctest build directory set to %s" absolute-choice)))))


(defun ctest--get-test-list ()
  "Retrieve the list of tests from the current build directory.
Returns a list of test names as strings."
  (unless ctest-current-build-dir
    (error "ctest: No build directory selected. Run `ctest-select-build-directory` first"))

  ;; Bind default-directory so subprocesses execute in the correct path
  (let* ((default-directory ctest-current-build-dir)
         (json-output-buffer (generate-new-buffer " *ctest-json-output*")))

    ;; unwind-protect ensures cleanup occurs regardless of errors
    (unwind-protect
        (progn
          ;; Synchronous call to ctest. Safe here as --show-only is instantaneous.
          ;; Arguments: PROGRAM INFILE BUFFER DISPLAY &rest ARGS
          (call-process ctest-executable nil json-output-buffer nil "--show-only=json-v1")

          (with-current-buffer json-output-buffer
            (goto-char (point-min))
            (let* ((json-data (json-parse-buffer :object-type 'alist :array-type 'list))
                   (tests-array (alist-get 'tests json-data)))

              ;; Extract the 'name' field from each test object
              (mapcar (lambda (test-obj)
                        (alist-get 'name test-obj))
                      tests-array))))

      ;; Cleanup block
      (kill-buffer json-output-buffer))))

(define-derived-mode ctest-status-mode magit-section-mode "CTest Status"
  "Major mode for interacting with CMake ctest results.
Provides Magit-like section navigation and folding.")

(require 'nerd-icons)

(defun ctest-status-refresh ()
  "Redraw the ctest status buffer using local state variables and nerd-icons."
  (let ((inhibit-read-only t)
        (current-line (line-number-at-pos)))
    (erase-buffer)

    (magit-insert-section (ctest-root)
      (magit-insert-heading
        (propertize "CTest Status: " 'face 'bold)
        (propertize ctest-current-build-dir 'face 'magit-section-heading))
      (insert "\n")

      (magit-insert-section (ctest-tests)
        (insert (propertize (format "  %-3s %-45s %-20s %s\n" "MRK" "TEST NAME" "STATUS" "TIME") 'face 'magit-section-heading))
        (insert (make-string 85 ?-) "\n")

        (dolist (test ctest-cached-tests)
          (magit-insert-section (ctest-test test)
            (let* ((is-marked (member test ctest-marked-tests))

                   ;; 1. Determine Checkbox Icon
                   (mark-icon (if is-marked
                                  (nerd-icons-mdicon "nf-md-checkbox_marked" :face 'font-lock-keyword-face)
                                (nerd-icons-mdicon "nf-md-checkbox_blank_outline" :face 'font-lock-comment-face)))

                   (result (when ctest-test-results (gethash test ctest-test-results)))
                   (status (if result (plist-get result :status) "Not Run"))
                   (time (if result (plist-get result :time) "-"))

                   ;; 2. Determine Status Icon and Face
                   (status-icon (cond ((string-match-p "Passed" status)
                                       (nerd-icons-mdicon "nf-md-check_circle" :face 'success))
                                      ((string-match-p "Not Run" status)
                                       (nerd-icons-mdicon "nf-md-help_circle" :face 'font-lock-comment-face))
                                      ((string-match-p "Running" status)
                                       (nerd-icons-mdicon "nf-md-loading" :face 'warning))
                                      (t
                                       (nerd-icons-mdicon "nf-md-close_circle" :face 'error))))

                   (face (cond ((string-match-p "Passed" status) 'success)
                               ((string-match-p "Not Run" status) 'font-lock-comment-face)
                               ((string-match-p "Running" status) 'warning)
                               (t 'error)))

                   ;; 3. Column Formatting
                   (test-padded (truncate-string-to-width test 45 nil ?\s))
                   ;; Pad the text to 18 chars, leaving 2 spaces for the icon
                   (status-padded (format "%-18s" status))
                   (time-str (if (string= time "-") "-" (format "%ss" time))))

              ;; 4. UI Insertion
              (insert (format "  %s %s " mark-icon test-padded))
              (insert (format "%s %s " status-icon (propertize status-padded 'face face)))
              (insert (propertize time-str 'face 'font-lock-keyword-face))
              (insert "\n"))))))

    (magit-section-show-level-4-all)
    (goto-char (point-min))
    (forward-line (1- current-line))))

(defun ctest-status ()
  "Display the ctest status buffer.
If the buffer already exists, switches to it preserving state.
If it does not exist, prompts the user to select a build directory and initializes."
  (interactive)
  (let ((buf (get-buffer "*ctest-status*")))
    (if (buffer-live-p buf)
        ;; Context A: Buffer exists. Shift focus and terminate.
        (pop-to-buffer buf)

      ;; Context B: Buffer does not exist. Initialize from scratch.
      ;; 1. Force the user to select the build directory interactively.
      (call-interactively #'ctest-select-build-directory)

      ;; 2. Fetch tests and construct the new buffer.
      (let* ((tests (ctest--get-test-list))
             (new-buf (get-buffer-create "*ctest-status*"))
             (active-build-dir ctest-current-build-dir))

        (with-current-buffer new-buf
          (ctest-status-mode)
          ;; Inject state variables into the local buffer scope
          (setq ctest-current-build-dir active-build-dir)
          (setq ctest-cached-tests tests)
          (setq ctest-test-results (make-hash-table :test 'equal))
          (setq ctest-marked-tests nil)

          ;; Draw the UI
          (ctest-status-refresh))

        ;; Display the newly constructed buffer
        (pop-to-buffer new-buf)))))

(defun ctest-mark-toggle ()
  "Toggle mark for the test at point."
  (interactive)
  (let ((test-name (magit-section-value-if 'ctest-test)))
    (when test-name
      (if (member test-name ctest-marked-tests)
          (setq ctest-marked-tests (delete test-name ctest-marked-tests))
        (push test-name ctest-marked-tests))
      (ctest-status-refresh)
      (forward-line 1)))) ; Move to next line for rapid marking

(defun ctest-unmark-all ()
  "Clear all test marks."
  (interactive)
  (setq ctest-marked-tests nil)
  (ctest-status-refresh))

(defvar ctest-output-buffer-name "*ctest-output*"
  "Name of the buffer used for ctest execution output.")

(defun ctest--run-async (test-names)
  "Execute ctest asynchronously for the given list of TEST-NAMES.
If TEST-NAMES is nil, executes all tests."
  (unless ctest-current-build-dir
    (error "ctest: Build directory not set"))

  (let ((buf (get-buffer-create ctest-output-buffer-name))
        (default-directory ctest-current-build-dir)
        ;; Construct the base command list
        (cmd-args (list ctest-executable "--output-on-failure")))

    ;; Prepare the buffer
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Starting ctest in %s...\n\n" default-directory))))

    ;; If specific tests are provided, construct a regex for the -R flag
    (when test-names
      (let ((regex (format "^(%s)$" (string-join test-names "|"))))
        (setq cmd-args (append cmd-args (list "-R" regex)))))

    ;; Asynchronous process invocation
    (make-process
     :name "ctest"
     :buffer buf
     :command cmd-args
     :connection-type 'pipe
     :sentinel (lambda (process event)
                 (message "CTest process %s" (string-trim event))))

    (message "CTest execution started. Press 'l' to view logs.")))

;;; Actions

(defun ctest-run-current ()
  "Run the test currently under the cursor in the status buffer."
  (interactive)
  (let ((test-name (magit-section-value-if 'ctest-test)))
    (if test-name
        (ctest--run-async (list test-name))
      (error "ctest: No test under cursor"))))

(defun ctest-run-all ()
  "Run all tests in the current build directory."
  (interactive)
  (ctest--run-async nil))

(require 'ansi-color)

(defun ctest-show-log ()
  "Display the log dynamically based on cursor context.
Parses ANSI escape codes to render native Emacs colors."
  (interactive)
  (let ((test-name (magit-section-value-if 'ctest-test)))
    (if test-name
        (let ((log-text (ctest--extract-test-log test-name ctest-current-build-dir))
              (buf-name (format "*ctest-log: %s*" test-name)))

          (with-current-buffer (get-buffer-create buf-name)
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert log-text)

              ;; 1. Translate raw ANSI codes into Emacs text properties
              (ansi-color-apply-on-region (point-min) (point-max))

              ;; 2. Apply compilation mode for clickable file paths
              (compilation-mode)
              (setq buffer-read-only t)))

          (pop-to-buffer buf-name))

      (let ((buf (get-buffer ctest-output-buffer-name)))
        (if buf
            (display-buffer buf)
          (error "ctest: No general output buffer found"))))))

;;; Transient Menu Definition

(defun ctest--parse-output (process output-string)
  "Process filter to parse ctest output and update UI in real-time."
  (let ((out-buf (process-buffer process))
        (status-buf (get-buffer "*ctest-status*")))
    (when (buffer-live-p out-buf)
      (with-current-buffer out-buf
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert output-string))

        (save-excursion
          (goto-char (point-min))
          ;; The regex now captures 'Not Run' and any subsequent non-numeric characters before the time.
          (while (re-search-forward "Test[ \t]+#[0-9]+:[ \t]+\\([^ \t\n]+\\).*?\\(Passed\\|Failed\\|Timeout\\|Not Run[^0-9]*?\\)[ \t]+\\([0-9.]+\\)[ \t]+sec" nil t)
            (let ((name (match-string 1))
                  ;; Trim whitespace from the captured status string
                  (status (string-trim (match-string 2)))
                  (time (match-string 3)))
              (when (and status-buf (buffer-live-p status-buf))
                (with-current-buffer status-buf
                  (puthash name (list :status status :time time) ctest-test-results))))))))

    (when (and status-buf (buffer-live-p status-buf))
      (with-current-buffer status-buf
        (ctest-status-refresh)))))

;; Update the async runner to attach the filter
(defun ctest--run-async (test-names)
  "Execute ctest asynchronously for TEST-NAMES."
  (unless ctest-current-build-dir (error "ctest: Build directory not set"))

  (let ((buf (get-buffer-create ctest-output-buffer-name))
        (default-directory ctest-current-build-dir)
        (cmd-args (list ctest-executable "--output-on-failure")))

    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Starting ctest in %s...\n\n" default-directory))))

    (when test-names
      (let ((regex (format "^(%s)$" (string-join test-names "|"))))
        (setq cmd-args (append cmd-args (list "-R" regex)))))

    ;; Reset hash table for the specific tests being run
    (with-current-buffer "*ctest-status*"
      (if test-names
          (dolist (test test-names)
            (puthash test (list :status "Running" :time "-") ctest-test-results))
        (clrhash ctest-test-results))
      (ctest-status-refresh))

    (make-process
     :name "ctest"
     :buffer buf
     :command cmd-args
     :connection-type 'pipe
     :filter #'ctest--parse-output  ;; Attach the parser here
     :sentinel (lambda (process event)
                 (message "CTest process %s" (string-trim event))))))

(defun ctest-run-marked ()
  "Run all marked tests."
  (interactive)
  (if ctest-marked-tests
      (ctest--run-async ctest-marked-tests)
    (error "ctest: No tests marked")))

(defun ctest-mark-toggle-region-or-line ()
  "Toggle mark for the test at point, or all tests in the active visual region."
  (interactive)
  (if (use-region-p)
      (let ((end (region-end)))
        ;; save-excursion prevents the cursor from physically moving during the loop
        (save-excursion
          (goto-char (region-beginning))
          (while (< (point) end)
            (let ((test-name (magit-section-value-if 'ctest-test)))
              (when test-name
                (if (member test-name ctest-marked-tests)
                    (setq ctest-marked-tests (delete test-name ctest-marked-tests))
                  (push test-name ctest-marked-tests))))
            (forward-line 1)))

        ;; Deactivate visual mode and refresh UI
        (deactivate-mark)
        (ctest-status-refresh))

    ;; Fallback to single line toggle
    (ctest-mark-toggle)))

(defun ctest--get-test-command (test-name)
  "Retrieve the execution command array for TEST-NAME via ctest JSON output."
  (let* ((default-directory ctest-current-build-dir)
         (json-buf (generate-new-buffer " *ctest-json-cmd*"))
         cmd-list)
    (unwind-protect
        (progn
          ;; Query ctest exclusively for the requested test
          (call-process ctest-executable nil json-buf nil
                        "--show-only=json-v1" "-R" (format "^%s$" (regexp-quote test-name)))

          (with-current-buffer json-buf
            (goto-char (point-min))
            (let* ((json-data (json-parse-buffer :object-type 'alist :array-type 'list))
                   (tests-array (alist-get 'tests json-data)))
              (when tests-array
                ;; The 'command' field is an array of strings [executable, arg1, arg2...]
                (setq cmd-list (alist-get 'command (car tests-array)))))))
      (kill-buffer json-buf))
    cmd-list))

(defun ctest-debug-current ()
  "Launch GDB for the test under the cursor."
  (interactive)
  (let ((test-name (magit-section-value-if 'ctest-test)))
    (if (not test-name)
        (error "ctest: No test under cursor")

      (let ((cmd-list (ctest--get-test-command test-name)))
        (if (not cmd-list)
            (error "ctest: Could not extract command for test '%s'" test-name)

          (let* ((exe (car cmd-list))
                 (args (cdr cmd-list))
                 ;; Construct GDB command: gdb -i=mi --args executable arg1 arg2
                 (gdb-cmd (string-join (append (list "gdb" "-i=mi" "--args" exe) args) " ")))

            (message "Starting GDB for %s..." test-name)
            ;; Invoke Emacs's GDB-MI interface
            (gdb gdb-cmd)))))))

;; Update Transient Menu
(transient-define-prefix ctest-dispatch ()
  "Main transient menu for ctest actions."
  ["CTest Actions"
   ("r" "Run test at point" ctest-run-current)
   ("m" "Run marked tests"  ctest-run-marked)
   ("a" "Run all tests"     ctest-run-all)
   ("d" "Debug test at point" ctest-debug-current)
   ("l" "Show log buffer"   ctest-show-log)])

;; Update Evil Keybindings
(map! :map ctest-status-mode-map
      :n "?" #'ctest-dispatch
      :n "l" #'ctest-show-log
      :n "r" #'ctest-run-current
      :n "d" #'ctest-debug-current
      :n "u" #'ctest-unmark-all
      :nv "m" #'ctest-mark-toggle-region-or-line)


(provide 'ctest)
;;; ctest.el ends here
