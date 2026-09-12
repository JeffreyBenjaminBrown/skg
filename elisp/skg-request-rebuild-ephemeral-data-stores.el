;;; -*- lexical-binding: t; -*-

(require 'skg-length-prefix)

(defun skg-rebuild-ephemeral-data-stores ()
  "Rebuild the in-memory graph and Tantivy from authoritative .skg files.
The .skg files are not changed. Useful after importing data or if a
derived store is stale."
  (interactive)
  (message "Rebuilding ephemeral data stores (this may take a while) ...")
  (let* ((tcp-proc (skg-tcp-connect-to-rust))
         (request-sexp "((request . \"rebuild ephemeral data stores\"))\n"))
    (skg-register-response-handler
     'rebuild-ephemeral-data-stores
     (lambda (_tcp-proc payload)
       (let* ((response (read payload))
              (content (cadr (assoc 'content response)))
              (full-msg (concat (or content "Rebuild complete.")
                                "\nExisting skg views are now invalid."
                                " Run M-x skg-close-all-skg-buffers to close them.")))
         (ding) ;; Audible signal: rebuilds take long enough to walk away from.
         (message "%s" full-msg)))
     t)
    (skg-lp-reset)
    (process-send-string tcp-proc request-sexp)))

(provide 'skg-request-rebuild-ephemeral-data-stores)
