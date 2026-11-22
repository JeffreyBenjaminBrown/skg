;;; skg-request-verify-connection.el --- Ping the server, echo its reply  -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;;
;;; 'skg-connection-verify' (M-x skg-connection-verify, and run by
;;; 'skg-client-init') pings the server and shows its reply in the
;;; echo area, or "connected".
;;;
;;; DATA USED/ASSUMED: See docs/api-and-formats.org.
;;;
;;; Code:

(require 'skg-length-prefix)

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
         (message "%s" (or (and content (format "%s" content))
                           "connected"))))
     t)
    (skg-lp-reset)
    (process-send-string tcp-proc request-sexp)))

(provide 'skg-request-verify-connection)
