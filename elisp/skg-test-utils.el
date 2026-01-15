;;; -*- lexical-binding: t; -*-
;;;
;;; Utilities for skg tests (unit and integration).
;;;
;;; For unit tests in tests/elisp/, load this file first to set up load-path:
;;;   (load-file "../../elisp/skg-test-utils.el")
;;;   (require 'ert)
;;;   (require 'your-module-here)

(require 'cl-lib)

;; Set up load-path for test files.
;; This adds both elisp/ and elisp/skg-sexpr/ directories.
(when load-file-name
  (let ((elisp-dir (file-name-directory load-file-name)))
    (add-to-list 'load-path elisp-dir)
    (add-to-list 'load-path (expand-file-name "skg-sexpr" elisp-dir))))

(defun find-skg-content-buffer ()
  "Find a *skg:* buffer that's not a search buffer."
  (car (cl-remove-if-not
        (lambda (b)
          (let ((name (buffer-name b)))
            (and (string-prefix-p "*skg:" name)
                 (not (string-match-p "search:" name)))))
        (buffer-list))))

(provide 'skg-test-utils)
