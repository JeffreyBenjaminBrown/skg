;;; -*- lexical-binding: t; -*-
;;;
;;; Handler for the "rerender all views" request/response.
;;; Used by skg-view-diff-mode to refresh all open views
;;; after toggling diff mode.
;;;
;;; Protocol: rerender-lock → rerender-done queues retained-session
;;; collateral offers.  Each view is updated only through its exact ACK.

(require 'skg-length-prefix)
(require 'skg-request-save) ; for skg-replace-buffer-with-new-content, skg-big-nonfatal-message
(require 'skg-buffer)       ; for skg-find-buffer-by-uri
(require 'skg-lock-buffers)

(defun skg-request-rerender-all-views (&optional approved-pids)
  "Ask the server to re-render every open view.
Locks all skg buffers, then registers handlers for the
queueing protocol: rerender-lock, rerender-done, then exact offers."
  (let ((tcp-proc (skg-tcp-connect-to-rust)))
    (skg--begin-stream "rerender")
    (skg--register-stream-request-cleanup "rerender")
    (skg--lock-all-skg-buffers)
    (skg--register-rerender-stream-handlers)
    (skg--register-rerender-ugly-confirmation
     (lambda (pids) (skg-request-rerender-all-views pids)))
    (skg-submit-request
     tcp-proc
     (concat (prin1-to-string
              (append
               '((request . "rerender all views"))
               (when approved-pids
                 `((allow-ugly-telescopes ,@approved-pids)))))
             "\n"))))

(defun skg--register-rerender-ugly-confirmation
    (retry &optional unfired-response-type)
  "End a challenged rerender and optionally retry with approved PIDs.
UNFIRED-RESPONSE-TYPE is the one-shot acknowledgement the challenged
request replaced; remove it and balance its pending count."
  (skg-register-response-handler
   'ugly-telescope-confirmation
   (lambda (_tcp-proc payload)
     (skg-remove-response-handler 'ugly-telescope-confirmation)
     (when unfired-response-type
       (skg-remove-response-handler unfired-response-type))
     (let* ((response (read payload))
            (prompt (format "%s" (cadr (assoc 'prompt response))))
            (pids (mapcar (lambda (pid) (format "%s" pid))
                          (cadr (assoc 'pids response)))))
       (skg--end-stream)
       (skg--unlock-all-save-locked)
       (when (y-or-n-p (concat prompt " "))
         (run-at-time 0 nil (lambda () (funcall retry pids))))))
   nil))

(defun skg--register-rerender-stream-handlers ()
  "Register the two handlers for a queued rerender response.
Shared by 'skg-request-rerender-all-views' and 'skg-view-diff-mode'."
  (skg-register-response-handler
   ;; 1. Lock message: unlock buffers not in the URI list.
   'rerender-lock
   (lambda (_tcp-proc payload)
     (skg-remove-response-handler 'ugly-telescope-confirmation)
     (condition-case err
         (let* ((response (read payload))
                (lock-entry (assoc 'lock-views response)))
           (when lock-entry
             (skg--unlock-buffers-not-in-uri-list
              (cadr lock-entry))))
       (error
        (skg--unlock-all-save-locked)
        (skg-log 'error 'rerender "rerender-lock handler error: %S" err))))
   t)
  (skg-register-response-handler
   ;; 2. Done message: clean up; exact offers arrive as server operations.
   'rerender-done
   (lambda (_tcp-proc payload)
     (skg--end-stream)
     (skg--unlock-all-save-locked) ;; safety net
     (condition-case err
         (let* ((response (read payload))
                (errors-list (cadr (assoc 'errors response)))
                (warnings-list (cadr (assoc 'warnings response))))
           (when (or (skg--message-list-nonempty-p errors-list)
                     (skg--message-list-nonempty-p warnings-list))
             (skg-big-nonfatal-message
              "*skg rerender messages*"
              (cond
               ((and (skg--message-list-nonempty-p errors-list)
                     (skg--message-list-nonempty-p warnings-list))
                "Rerender completed with errors and warnings")
               ((skg--message-list-nonempty-p errors-list)
                "Rerender completed with errors")
               (t
                "Rerender completed with warnings"))
              (skg-errors-and-warnings-to-org-string
               errors-list warnings-list))))
       (error
        (message "skg: rerender-done handler error: %S" err))))
   t))

(provide 'skg-request-rerender-all-views)
