;; DATA USED/ASSUMED: See /api.md.

(defun request-org-doc-from-node (node-id)
  "Ask Rust for an Org document view of NODE-ID.
Installs a length-prefixed response handler."
  (interactive "sNode ID: ")
  (let* ((proc (skg-tcp-connect-to-rust))
         (request-sexp
          (format "((request . \"org document\") (id . \"%s\"))\n"
                  node-id )) )
    (setq ;; Prepare LP state and handler
     skg-lp--buf                (unibyte-string) ;; empty string
     skg-lp--bytes-left         nil
     skg-doc--response-handler  #'skg-lp-handle-org-chunk)
    (process-send-string proc request-sexp)) )

(defun skg-open-org-buffer-from-rust-org (_proc org-text)
  "Open a new buffer and insert ORG-TEXT, enabling org-mode."
  (with-current-buffer (get-buffer-create "*skg-content-view*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert org-text)
      (org-mode))
    (set-buffer-modified-p nil)
    (goto-char (point-min))
    (switch-to-buffer (current-buffer))))

(provide 'get-document)
