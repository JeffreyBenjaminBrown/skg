;; DATA USED/ASSUMED: See /api.md.

(require 'skg-length-prefix)

(defun skg-request-single-root-content-view-from-node (node-id)
  "Ask Rust for an single root content view view of NODE-ID.
Installs a length-prefixed response handler."
  (interactive "sNode ID: ")
  (let* ((proc (skg-tcp-connect-to-rust))
         (request-sexp
          (format "((request . \"single root content view\") (id . \"%s\"))\n"
                  node-id )) )
    (setq ;; Prepare LP state and handler
     skg-lp--buf               (unibyte-string) ;; empty string
     skg-lp--bytes-left        nil
     skg-doc--response-handler
     (lambda (proc chunk)
       (skg-lp-handle-generic-chunk
        #'skg-open-org-buffer-from-rust-org proc chunk)))
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

(provide 'skg-request-single-root-content-view)
