;;; -*- lexical-binding: t; -*-

(require 'skg-length-prefix)
(require 'skg-request-herald-rules)
(require 'skg-request-rerender-all-views)

(defun skg-rebuild-session-lifetime-stores ()
  "Reload the server's config, and rebuild the in-memory graph and
Tantivy from authoritative .skg files. The .skg files are not changed.
Useful after editing the config (its skgrepo list, say), importing
data, or if a session-lifetime store is stale. An invalid config
fails the rebuild, and the server keeps serving the old one. After a
successful rebuild every view without unsaved edits is rerendered."
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
may have changed, then rerender every view without unsaved edits."
  (let* ((response (read payload))
         (content (cadr (assoc 'content response)))
         (failed (and (stringp content)
                      (string-prefix-p "Rebuild failed" content))))
    (unless failed
      ;; Outside this handler: the rerender resets the receiver,
      ;; which would drop whatever follows the rebuild's answer.
      (run-at-time 0 nil #'skg--refetch-herald-rules-and-rerender-all-views))
    (ding) ;; Audible signal: rebuilds take long enough to walk away from.
    (message "%s" (or content "Rebuild complete."))))

(defun skg--refetch-herald-rules-and-rerender-all-views ()
  "Fetch the herald rules, then rerender every view without unsaved
edits. The server answers in order, so the views are redrawn under
the new rules. Nothing between the two sends reads the connection,
so the rerender's reset of the receiver cannot drop the herald
rules' answer."
  (skg-request-herald-rules)
  (skg-request-rerender-all-views))

(provide 'skg-request-rebuild-session-lifetime-stores)
