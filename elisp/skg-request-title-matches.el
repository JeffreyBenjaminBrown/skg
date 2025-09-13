(defun skg-request-title-matches (search-terms)
  "Request title matches from the Rust server."
  (interactive "sSearch terms: ")
  (let* ((tcp-proc (skg-tcp-connect-to-rust))
         (request-sexp
          (format "((request . \"title matches\") (terms . \"%s\"))\n" search-terms)))
    (setq skg-doc--response-handler
          ;; Prepare for response.
          #'skg-display-search-results)
    (process-send-string tcp-proc request-sexp)))

(defun skg-display-search-results (tcp-proc string)
  "Display title search results from the Rust server."
  (with-current-buffer
      (get-buffer-create "*skg-title-search*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (string-trim string))
      (when (> (length (string-trim string)) 0)
        (insert "\n"))
      (org-mode)
      (heralds-minor-mode)
      (goto-char (point-min)))
    (set-buffer-modified-p nil)
    (switch-to-buffer (current-buffer)))
  )

(provide 'skg-request-title-matches)
