;;; -*- lexical-binding: t; -*-

(require 'skg-length-prefix)
(require 'org-id)

(defun skg-strip-trailing-whitespace-from-bodies ()
  "Strip trailing whitespace from every line of every body,
in every source the user owns.
Foreign sources are read-only and left untouched.
Rewrites exactly the .skg files whose bodies change;
the derived caches are refreshed to match."
  (interactive)
  (message "Stripping trailing whitespace from bodies ...")
  (let* ((tcp-proc (skg-tcp-connect-to-rust))
         (_verified
          (unless (skg-connection-handshake-ensure)
            (error "Cannot strip body whitespace before server verification")))
         ;; This operation identity is durable across a transport retry, while
         ;; each deliberate command invocation receives a new UUID.
         (operation-id (org-id-uuid))
         (server-session-id
          (or skg--server-session-id
              (error "Verified SKG connection has no server session")))
         (request-sexp
          (concat
           (prin1-to-string
            `((request . "strip body whitespace")
              (operation-id . ,operation-id)
              (server-session-id . ,server-session-id)))
           "\n")))
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
    (skg-submit-request tcp-proc request-sexp)))

(provide 'skg-request-strip-body-whitespace)
