;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: Show what the server warned about as it started (unknown
;;; skgconfig.toml keys, say). `skg-tcp-connect-to-rust' sends the
;;; request once per connection, as soon as the connection is up and so
;;; before any view is requested. A non-empty answer opens
;;; `skg-server-warnings-buffer-name'; an empty one opens nothing.
;;;
;;; DATA USED/ASSUMED: See docs/api-and-formats.org, "Server warnings".

(require 'skg-length-prefix)
(require 'skg-request-save) ; for skg-big-nonfatal-message, skg-messages-to-org-string
(require 'skg-state)

(defconst skg-server-warnings-buffer-name "*SKG Server Warnings*"
  "The buffer that shows the server's startup warnings.")

(defun skg-request-server-warnings (tcp-proc)
  "Ask the server on TCP-PROC for its startup warnings.
A one-shot handler shows a non-empty answer.
PITFALL: No `skg-lp-reset' here, unlike most request functions.
This request is sent from inside `skg-tcp-connect-to-rust', and the
caller's own request follows it at once."
  (skg-register-response-handler
   'server-warnings
   #'skg--server-warnings-handler
   t)
  (process-send-string
   tcp-proc
   "((request . \"server warnings\"))\n"))

(defun skg--server-warnings-handler (_tcp-proc payload)
  "Show the warnings in a server-warnings response PAYLOAD, if any."
  (condition-case err
      (let ((warnings (cadr (assoc 'warnings (read payload)))))
        (when (skg--message-list-nonempty-p warnings)
          (skg-big-nonfatal-message
           skg-server-warnings-buffer-name
           (format "The skg server reported %d startup warning%s - see %s"
                   (length warnings)
                   (if (= 1 (length warnings)) "" "s")
                   skg-server-warnings-buffer-name)
           (skg-messages-to-org-string warnings))))
    (error
     (message "skg: server-warnings handler error: %S" err))))

(provide 'skg-request-server-warnings)
