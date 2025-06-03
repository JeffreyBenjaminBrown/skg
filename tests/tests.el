;;; tests/tests.el --- Run all project tests

;; Load project configuration
(let ((test-dir (file-name-directory
                 (or load-file-name buffer-file-name default-directory))))
  (load (expand-file-name "../elisp/init.el" test-dir)))

;; Auto-discover and load all test files
(let ((test-dir (file-name-directory
                 (or load-file-name buffer-file-name default-directory)))
      (current-file (or load-file-name buffer-file-name)))
  (dolist (file (directory-files test-dir t "^test-.*\\.el$"))
    (when current-file  ; Only check if we have a current file
      (unless (string= file current-file)
        (message "Loading test file: %s" file)
        (load-file file)))))

;; Run all tests
(if noninteractive
    ;; Batch mode (command line)
    (ert-run-tests-batch-and-exit)
  ;; Interactive mode (within Emacs)
  (ert-run-tests-interactively t))
