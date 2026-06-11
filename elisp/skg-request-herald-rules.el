;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: Fetch the herald rule table from the server.
;;; The table lives only in Rust (server/heralds.rs); Emacs caches it
;;; in `heralds--transform-rules' for the session. `skg-client-init'
;;; calls this once per connect, so reconnecting re-fetches it.

(require 'heralds-minor-mode)
(require 'skg-length-prefix)
(require 'skg-state)

(defun skg-request-herald-rules ()
  "Fetch the herald rule table and install it via `heralds-install-rules'.
On any failure, heralds stay disabled for the session (the rule
cache remains nil and `heralds-minor-mode' refuses to enable),
reported with a one-line message. There is deliberately no local
fallback table."
  (interactive)
  (let ((tcp-proc (skg-tcp-connect-to-rust)))
    (skg-register-response-handler
     'herald-rules
     (lambda (_tcp-proc payload)
       (condition-case err
           (let* ((response (read payload))
                  (content (cadr (assoc 'content response))))
             (heralds-install-rules
              (car (read-from-string content))))
         (error
          (message "Heralds disabled for this session: %S" err))))
     t)
    ;; PITFALL: No `skg-lp-reset' here, unlike most request functions.
    ;; This request is sent immediately after `skg-connection-verify'
    ;; during `skg-client-init'; resetting the LP machine could drop a
    ;; partially received verify-connection response.
    (process-send-string
     tcp-proc
     "((request . \"herald rules\"))\n")))

(provide 'skg-request-herald-rules)
