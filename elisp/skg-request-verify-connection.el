;;; -*- lexical-binding: t; -*-
;;;
;;; DATA USED/ASSUMED: See /api.md.

(require 'skg-length-prefix)

(defun skg--show-handshake-telescope-warnings (response)
  "Display structured load WARNINGS carried by RESPONSE."
  (let ((warnings (cadr (assoc 'telescope-warnings response))))
    (when warnings
      (let ((content
             (concat
              "* WARNING: Telescope load warnings\n"
              (mapconcat
               (lambda (warning)
                 (let ((pid (cadr (assoc 'pid warning)))
                       (message-text (cadr (assoc 'message warning)))
                       (winners (cadr (assoc 'winning-paths warning)))
                       (losers (cadr (assoc 'ignored-paths warning))))
                   (concat
                    (format "** %s\n%s\n" pid message-text)
                    (when winners
                      (concat "*** retained owned files\n"
                              (mapconcat (lambda (path)
                                           (format "**** %s" path))
                                         winners "\n") "\n"))
                    (when losers
                      (concat "*** ignored foreign files\n"
                              (mapconcat (lambda (path)
                                           (format "**** %s" path))
                                         losers "\n") "\n")))))
               warnings ""))))
        (skg-big-nonfatal-message
         "*SKG Telescope Warnings*"
         (format "WARNING: Skg loaded with %d telescope warning(s)."
                 (length warnings))
         content)))))

(defun skg-connection-verify ()
  "Verify connection to the Rust server,
by sending a simple ping to the Rust server
to verify the connection is working. The server responds with a
confirmation message that is displayed in the minibuffer.

Surprisingly, the TCP connection
does not need to be explicitly launched,
because each of the client's `request-*` functions
calls `(skg-tcp-connect-to-rust)`
(which is idempotent and cheap to rerun)."
  (interactive)
  (let* ((tcp-proc (skg-tcp-connect-to-rust))
         (request-sexp "((request . \"verify connection\"))\n"))
    (skg-register-response-handler
     'verify-connection
     (lambda (_tcp-proc payload)
       (let* ((response (read payload))
              (content (cadr (assoc 'content response))))
         (skg-install-source-inventory response)
         (skg--show-handshake-telescope-warnings response)
         (message "%s" (or (and content (format "%s" content))
                           "connected"))))
     t)
    (skg-lp-reset)
    (process-send-string tcp-proc request-sexp)))

(provide 'skg-request-verify-connection)
