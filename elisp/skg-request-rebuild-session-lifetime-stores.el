;;; -*- lexical-binding: t; -*-

(require 'skg-length-prefix)
(require 'skg-request-herald-rules)

(defun skg-rebuild-session-lifetime-stores ()
  "Reload the server's config, and rebuild the in-memory graph and
Tantivy from authoritative .skg files. The .skg files are not changed.
Useful after editing the config (its skgrepo list, say), importing
data, or if a session-lifetime store is stale. An invalid config
fails the rebuild, and the server keeps serving the old one."
  (interactive)
  (message "Rebuilding session-lifetime stores (this may take a while) ...")
  (let* ((tcp-proc (skg-tcp-connect-to-rust))
         (request-sexp "((request . \"rebuild session-lifetime stores\"))\n"))
    (skg-register-response-handler
     'rebuild-session-lifetime-stores
     #'skg--rebuild-session-lifetime-stores-handler
     t)
    (skg-lp-reset)
    (process-send-string tcp-proc request-sexp)))

(defun skg--rebuild-session-lifetime-stores-handler (_tcp-proc payload)
  "Report the rebuild's answer in PAYLOAD. After a successful one,
re-fetch the herald rules, whose skgrepo labels the reloaded config
may have changed."
  (let* ((response (read payload))
         (content (cadr (assoc 'content response)))
         (failed (and (stringp content)
                      (string-prefix-p "Rebuild failed" content)))
         (full-msg (concat (or content "Rebuild complete.")
                           (unless failed
                             (concat "\nExisting skg views are now invalid."
                                     " Run M-x skg-close-all-skg-buffers"
                                     " to close them.")))))
    (unless failed
      (skg-request-herald-rules))
    (ding) ;; Audible signal: rebuilds take long enough to walk away from.
    (message "%s" full-msg)))

(provide 'skg-request-rebuild-session-lifetime-stores)
