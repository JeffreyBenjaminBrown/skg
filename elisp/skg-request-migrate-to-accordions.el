;;; -*- lexical-binding: t; -*-

(require 'skg-length-prefix)

(defun skg-migrate-to-accordions ()
  "Raise every owned node's edge levels to at least their defaults.
This lifts leak-shaped memberships (a public file naming a more
private node's ID) into their proper privacy-accordion sections; it
never lowers a level. Changed accordions are rewritten byte-stably
and the databases are rebuilt. The server refuses unless the active
source-set is \"all\", since migration must see every level.

What migration cannot fix: a public repo's git HISTORY keeps any
IDs it leaked before migration; repairing that is manual."
  (interactive)
  (when (yes-or-no-p
         "Migrate all owned data to privacy accordions (rewrites files, rebuilds dbs)? ")
    (message "Migrating to accordions (this may take a while) ...")
    (let* ((tcp-proc (skg-tcp-connect-to-rust))
           (request-sexp "((request . \"migrate to accordions\"))\n"))
      (skg-register-response-handler
       'migrate-to-accordions
       (lambda (_tcp-proc payload)
         (let* ((response (read payload))
                (content (cadr (assoc 'content response))))
           (ding) ;; Audible: migrations take long enough to walk away from.
           (message "%s" (or content "Migration complete."))))
       t)
      (skg-lp-reset)
      (process-send-string tcp-proc request-sexp))))

(provide 'skg-request-migrate-to-accordions)
