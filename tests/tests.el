;;; PURPOSE: Run all tests in this directory.

;; Load project configuration
(load
 (expand-file-name
  "../elisp/init.el"
  (file-name-directory
   (or load-file-name buffer-file-name))))

;; Load and run all test files
(let ((test-dir (file-name-directory
                 (or load-file-name buffer-file-name)))
      (current-file (or load-file-name buffer-file-name)))
  (dolist (file (directory-files test-dir t "\\.el$"))
    (unless ;; Load only *other* files.
        (string= file current-file)
      (message "Loading test file: %s" file)
      (load-file file))))

;; TODO: Can't assume they all passed.
;; (message "All tests completed!")
