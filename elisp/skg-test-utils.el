;;; -*- lexical-binding: t; -*-
;;;
;;; Utilities for skg integration tests.

(require 'cl-lib)

(defun find-skg-content-buffer ()
  "Find a *skg:* buffer that's not a search buffer."
  (car (cl-remove-if-not
        (lambda (b)
          (let ((name (buffer-name b)))
            (and (string-prefix-p "*skg:" name)
                 (not (string-match-p "search:" name)))))
        (buffer-list))))

(provide 'skg-test-utils)
