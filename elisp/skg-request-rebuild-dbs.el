;;; -*- lexical-binding: t; -*-
;;;
;;; USER-FACING FUNCTIONS
;;;   skg-rebuild-dbs

(require 'skg-length-prefix)

(defun skg-rebuild-dbs ()
  "Wipe and rebuild TypeDB and Tantivy from .skg files on disk.
Does not touch the filesystem — only the derived databases.
Useful after importing new data or if the databases are stale."
  (interactive)
  (message "Rebuilding databases (this may take a while) ...")
  (let* ((tcp-proc (skg-tcp-connect-to-rust))
         (request-sexp "((request . \"rebuild dbs\"))\n"))
    (skg-register-response-handler
     'rebuild-dbs
     (lambda (_tcp-proc payload)
       (let* ((response (read payload))
              (content (cadr (assoc 'content response))))
         (message "%s" (or content "Rebuild complete."))))
     t)
    (skg-lp-reset)
    (process-send-string tcp-proc request-sexp)))

(provide 'skg-request-rebuild-dbs)
