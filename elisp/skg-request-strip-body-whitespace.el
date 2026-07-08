;;; -*- lexical-binding: t; -*-

(require 'skg-length-prefix)

(defun skg-strip-trailing-whitespace-from-bodies ()
  "Strip trailing whitespace from every line of every body,
in every source the user owns.
Foreign sources are read-only and left untouched.
Rewrites exactly the .skg files whose bodies change;
the derived caches are refreshed to match."
  (interactive)
  (message "Stripping trailing whitespace from bodies ...")
  (let* ((tcp-proc (skg-tcp-connect-to-rust))
         (request-sexp "((request . \"strip body whitespace\"))\n"))
    (skg-register-response-handler
     'strip-body-whitespace
     (lambda (_tcp-proc payload)
       (let* ((response (read payload))
              (content (cadr (assoc 'content response))))
         (message "%s"
                  (concat (or content "Body whitespace strip complete.")
                          "\nTo verify nothing but whitespace changed,"
                          " review with 'git diff --ignore-all-space'"
                          " (it should show nothing)."))))
     t)
    (skg-lp-reset)
    (process-send-string tcp-proc request-sexp)))

(provide 'skg-request-strip-body-whitespace)
