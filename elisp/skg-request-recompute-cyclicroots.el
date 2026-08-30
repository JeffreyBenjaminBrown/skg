;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: Explicitly refresh the rank-only cyclic-root cache.

(require 'skg-length-prefix)

(defun skg-recompute-cyclicroots ()
  "Recompute cyclic-root search ranking from the complete current graph.

Ordinary saves and reloads deliberately leave this rank-only cache stale.
The server computes without its graph-writer lock, retries if the graph
advances, then updates Tantivy and publishes the new cache atomically."
  (interactive)
  (message "Recomputing cyclic-root search ranking ...")
  (let ((tcp-proc (skg-tcp-connect-to-rust)))
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
           (message "%s" content))))
     t)
    (skg-submit-request
     tcp-proc "((request . \"recompute cyclic roots\"))\n")))

(provide 'skg-request-recompute-cyclicroots)
