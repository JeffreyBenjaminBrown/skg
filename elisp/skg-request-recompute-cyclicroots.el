;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: Explicitly refresh the rank-only cyclic-root cache.

(require 'skg-length-prefix)
(require 'org-id)

(defun skg-recompute-cyclicroots ()
  "Recompute cyclic-root search ranking from the complete current graph.

Ordinary saves and reloads deliberately leave this rank-only cache stale.
The server computes without its graph-writer lock, retries if the graph
advances, then updates Tantivy and publishes the new cache atomically."
  (interactive)
  (let* ((tcp-proc (skg-tcp-connect-to-rust))
         (_verified
          (unless (skg-connection-handshake-ensure)
            (error "Cannot recompute cyclic roots before server verification")))
         ;; Keep this identity stable if the transport redelivers the request;
         ;; each deliberate command invocation gets a fresh UUID.
         (operation-id (org-id-uuid))
         (server-session-id
          (or skg--server-session-id
              (error "Verified SKG connection has no server session")))
         (request-sexp
          (concat
           (prin1-to-string
            `((request . "recompute cyclic roots")
              (operation-id . ,operation-id)
              (server-session-id . ,server-session-id)))
           "\n")))
    (message "Recomputing cyclic-root search ranking ...")
    (skg-register-response-handler
     'recompute-cyclic-roots
     (lambda (_tcp-proc payload)
       (let* ((response (read payload))
              (status (cadr (assoc 'terminal-status response)))
              (content (cadr (assoc 'content response))))
         (if (eq status 'failed)
             (progn
               (ding)
               (display-warning 'skg (format "%s" content) :error))
           (let ((count
                  (if (fboundp 'skg-refresh-live-searches-after-rank-repair)
                      (skg-refresh-live-searches-after-rank-repair)
                    0)))
             (message "%s Queued %d live search refresh(es)."
                      content count)))))
     t)
    (skg-submit-request tcp-proc request-sexp)))

(provide 'skg-request-recompute-cyclicroots)
