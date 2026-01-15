;;; -*- lexical-binding: t; -*-
;;;
;;; USER-FACING FUNCTIONS
;;;   skg-request-title-matches
;;;
;;; DATA USED/ASSUMED: See /api.md.

(require 'skg-client)
(require 'skg-buffer)
(require 'heralds-minor-mode)

(defun skg-request-title-matches (search-terms)
  "Request title matches from the Rust server."
  (interactive "sSearch terms: ")
  (let* ((tcp-proc (skg-tcp-connect-to-rust))
         (clean-terms (if (stringp search-terms)
                          (substring-no-properties search-terms)
                        search-terms))
         (request-s-exp
          (concat (prin1-to-string
                   `((request . "title matches")
                     (terms . ,clean-terms)))
                  "\n")))
    (setq skg-doc--response-handler
          ;; Prepare for response.
          (lambda (tcp-proc string)
            (skg-display-search-results tcp-proc string clean-terms)))
    (process-send-string tcp-proc request-s-exp)))

(defun skg-display-search-results (_tcp-proc string search-terms)
  "Display title search results from the Rust server."
  (with-current-buffer
      (get-buffer-create (skg-search-buffer-name search-terms))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (string-trim string))
      (when (> (length (string-trim string)) 0)
        (insert "\n"))
      (org-mode)
      (heralds-minor-mode)
      (goto-char (point-min)))
    (set-buffer-modified-p nil)
    (switch-to-buffer (current-buffer))))

(provide 'skg-request-title-matches)
