;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: Fetch the herald rule table from the server.
;;; The table lives only in Rust (server/heralds.rs); Emacs caches it
;;; in `heralds--transform-rules' for the session. `skg-client-init'
;;; calls `skg-request-herald-rules' once per connect, so reconnecting
;;; re-fetches it; `heralds-minor-mode' calls `skg-herald-rules-ensure'
;;; to self-heal a missing table when the user turns heralds on.
;;;
;;; There is deliberately NO user-facing command here. A missing table
;;; is recovered automatically (see `skg-herald-rules-ensure'), and a
;;; hard failure surfaces an informative message at that point, so a
;;; manual re-fetch command is unnecessary.

(require 'heralds-minor-mode)
(require 'skg-length-prefix)
(require 'skg-state)

(defconst skg-herald-rules-max-attempts 3
  "How many times `skg-herald-rules-ensure' re-requests the table.
After this many empty replies it gives up, so a silent or wedged
server cannot freeze Emacs with fruitless retries.")

(defconst skg-herald-rules-attempt-timeout 1.0
  "Seconds `skg-herald-rules-ensure' waits for the reply to each attempt.")

(defun skg-request-herald-rules ()
  "Send a herald-rule-table request and install the reply asynchronously.
A one-shot handler calls `heralds-install-rules' when the reply
arrives; a malformed reply leaves the cache untouched and logs why.

This is the low-level fetch, used at connect time by `skg-client-init'
and per-attempt by `skg-herald-rules-ensure'. It does NOT wait for or
guarantee a result, and is intentionally not an interactive command:
callers that need the table should go through `skg-herald-rules-ensure'."
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
          (message
           "Heralds: could not parse the rule table from the server: %S"
           err))))
     t)
    ;; PITFALL: No `skg-lp-reset' here, unlike most request functions.
    ;; This request is sent immediately after `skg-connection-verify'
    ;; during `skg-client-init'; resetting the LP machine could drop a
    ;; partially received verify-connection response.
    (process-send-string
     tcp-proc
     "((request . \"herald rules\"))\n")))

(defun skg-herald-rules-ensure ()
  "Return the herald rule table, fetching it from the server if absent.
If `heralds--transform-rules' is already set, return it at once.
Otherwise re-request the table up to `skg-herald-rules-max-attempts'
times -- waiting up to `skg-herald-rules-attempt-timeout' seconds for
each reply -- and return whatever is installed at the end (nil if every
attempt came back empty).

The attempt cap is the point: it stops a silent or wedged server from
freezing Emacs by retrying forever. A connection failure propagates (as
the `user-error' from `skg-tcp-connect-to-rust') on the first attempt
rather than being retried, so the caller can report it."
  (or heralds--transform-rules
      (let ((attempt 0))
        (while (and (null heralds--transform-rules)
                    (< attempt skg-herald-rules-max-attempts))
          (setq attempt (1+ attempt))
          (skg-request-herald-rules)
          (let ((deadline (+ (float-time) skg-herald-rules-attempt-timeout)))
            (while (and (null heralds--transform-rules)
                        (process-live-p skg-rust-tcp-proc)
                        (< (float-time) deadline))
              (accept-process-output skg-rust-tcp-proc 0.05))))
        heralds--transform-rules)))

(provide 'skg-request-herald-rules)
