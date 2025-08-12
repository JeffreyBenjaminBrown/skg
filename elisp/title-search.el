(defun request-title-matches (search-terms)
  "Request title matches from the Rust server."
  (interactive "sSearch terms: ")
  (let* ((proc (skg-tcp-connect-to-rust))
         (request-sexp
          (format "((request . \"title matches\") (terms . \"%s\"))\n" search-terms)))
    (setq skg-doc--response-handler
          ;; Prepare for response.
          #'skg-display-search-results)
    (process-send-string proc request-sexp)))

(defun skg-display-search-results (proc string)
  "Display title search results from the Rust server."
  (with-current-buffer
      (get-buffer-create skg-search-buffer-name)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "Title Search Results:\n")
      (insert "======================\n\n")
      (insert (string-trim string))
      (when (> (length (string-trim string)) 0)
        (insert "\n"))
      (goto-char (point-min)))
    (set-buffer-modified-p nil)
    (switch-to-buffer (current-buffer)))
  )

(provide 'title-search)
