;;; -*- lexical-binding: t; -*-
;;;
;;; USER-FACING FUNCTIONS
;;;   skg-verify-connection
;;;
;;; DATA USED/ASSUMED: See /api.md.

(defun skg-verify-connection ()
  "Verify connection to the Rust server."
  (interactive)
  (let* ((tcp-proc (skg-tcp-connect-to-rust))
         (request-sexp "((request . \"verify connection\"))\n"))
    (setq skg-doc--response-handler
          ;; Prepare for response.
          #'skg-verify-connection-result)
    (process-send-string tcp-proc request-sexp)))

(defun skg-verify-connection-result (tcp-proc string)
  "Display verify connection result from the Rust server."
  (let ((response (string-trim string)))
    (message "%s" response)
    response))

(provide 'skg-request-verify-connection)
