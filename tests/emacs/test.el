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
      (unless (string-match-p "/abandoned/" file)
        (message "Reloading source file: %s" file)
        (load-file file)))))

;; Auto-discover and load all test files
(let ((test-dir (file-name-directory
                 (or load-file-name
                     buffer-file-name
                     default-directory)))
      (current-file (or load-file-name
                        buffer-file-name)))
  (dolist (file (directory-files
                 test-dir t "^test-.*\\.el$"))
    (when current-file ; Only check if we have a current file
      (unless (string= file current-file)
        (message "Loading test file: %s" file)
        (load-file file)))))

;; Run all tests
(if noninteractive
    ;; Batch mode (command line)
    (ert-run-tests-batch-and-exit)
  ;; Interactive mode (within Emacs)
  (ert-run-tests-interactively t))
