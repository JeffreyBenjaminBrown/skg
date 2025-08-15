;;; tests/tests.el --- Run all project tests

(let ((test-dir
       (file-name-directory
        (or load-file-name
            buffer-file-name
            default-directory))))
  (load ;; Load project configuration
   (expand-file-name "../../elisp/init.el" test-dir))
  (let* ((project-root (expand-file-name "../.." test-dir))
         (elisp-dir (expand-file-name "elisp" project-root)))
    (dolist ;; Reload non-abandoned files in elisp/
        ;; PITFALL: 'provide and 'require don't do this!
        (file (directory-files-recursively elisp-dir "\\.el$"))
      (unless (or (string-match-p "/abandoned/" file)
                  (string-match-p "/experim/" file))
        (message "Reloading source file: %s" file)
        (load-file file)))))

;; Auto-discover and load all test files
(let ((test-dir (file-name-directory
                 (or load-file-name
                     buffer-file-name
                     default-directory)))
      (current-file (or load-file-name
                        buffer-file-name)))
  (dolist (file (directory-files-recursively
                 test-dir "\\.el$"))
    (when current-file ; Only check if we have a current file
      (unless (or (string= file current-file)
                  (string-match-p "/test\\.el$" file)) ; exclude this test runner itself
        (message "Loading test file: %s" file)
        (load-file file)))))

;; Run all tests
(if noninteractive
    ;; Batch mode (command line)
    (ert-run-tests-batch-and-exit)
  ;; Interactive mode (within Emacs)
  (ert-run-tests-interactively t))
