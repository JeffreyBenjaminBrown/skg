;;; -*- lexical-binding: t; -*-
;;;
;;; Handler for the "rerender all views" request/response.
;;; Used by skg-view-diff-mode to refresh all open views
;;; after toggling diff mode.
;;;
;;; Protocol: rerender-lock → rerender-view* → rerender-done.
;;; Each view is unlocked and updated as its rerender-view arrives.

(require 'skg-length-prefix)
(require 'skg-request-save) ; for skg-replace-buffer-with-new-content, skg-big-nonfatal-message
(require 'skg-buffer)       ; for skg-find-buffer-by-uri
(require 'skg-lock-buffers)

(defvar skg--rerender-ugly-retry nil
  "Function to run after a challenged rerender's empty unwind completes.")

(defvar skg--rerender-ugly-challenged nil
  "Non-nil while consuming the empty unwind after a privacy challenge.")

(defun skg-request-rerender-all-views (&optional approved-pids)
  "Ask the server to re-render every open view.
Locks all skg buffers, then registers handlers for the
streaming protocol: rerender-lock, rerender-view*, rerender-done."
  (let ((tcp-proc (skg-tcp-connect-to-rust)))
    (skg--begin-stream "rerender")
    (skg--lock-all-skg-buffers)
    (skg--register-rerender-stream-handlers)
    (skg--register-rerender-ugly-confirmation
     (lambda (pids) (skg-request-rerender-all-views pids)))
    (skg-lp-reset)
    (process-send-string
     tcp-proc
     (concat (prin1-to-string
              (append
               '((request . "rerender all views"))
               (when approved-pids
                 `((allow-ugly-telescopes ,@approved-pids)))))
             "\n"))))

(defun skg--register-rerender-ugly-confirmation
    (retry &optional unfired-response-type)
  "Retry after the challenge unwind, approving its PIDs.
UNFIRED-RESPONSE-TYPE is the one-shot acknowledgement the challenged
request replaced; remove it and balance its pending count."
  (setq skg--rerender-ugly-retry nil
        skg--rerender-ugly-challenged nil)
  (skg-register-response-handler
   'ugly-telescope-confirmation
   (lambda (_tcp-proc payload)
     (setq skg-response-handler-map
           (assoc-delete-all 'ugly-telescope-confirmation
                             skg-response-handler-map))
     (when (and unfired-response-type
                (assoc unfired-response-type skg-response-handler-map))
       (setq skg-response-handler-map
             (assoc-delete-all unfired-response-type
                               skg-response-handler-map))
       (setq skg-lp--pending-count
             (max 0 (1- skg-lp--pending-count))))
     (let* ((response (read payload))
            (prompt (format "%s" (cadr (assoc 'prompt response))))
            (pids (mapcar (lambda (pid) (format "%s" pid))
                          (cadr (assoc 'pids response)))))
       (setq skg--rerender-ugly-challenged t)
       (when (y-or-n-p (concat prompt " "))
         (setq skg--rerender-ugly-retry
               (lambda () (funcall retry pids))))))
   nil))

(defun skg--register-rerender-stream-handlers ()
  "Register the three handlers for streamed rerender responses.
Shared by 'skg-request-rerender-all-views' and 'skg-view-diff-mode'."
  (skg-register-response-handler
   ;; 1. Lock message: unlock buffers not in the URI list.
   'rerender-lock
   (lambda (_tcp-proc payload)
     (unless skg--rerender-ugly-challenged
       (setq skg-response-handler-map
             (assoc-delete-all 'ugly-telescope-confirmation
                               skg-response-handler-map)))
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
   ;; 2. Per-view update: unlock and update each buffer.
   'rerender-view
   (lambda (_tcp-proc payload)
     (skg--apply-streamed-view-update payload 'rerender "rerender-view"))
   nil) ;; non-one-shot: fires for each streamed view
  (skg-register-response-handler
   ;; 3. Done message: clean up and show errors/warnings.
   'rerender-done
   (lambda (_tcp-proc payload)
     (setq skg-response-handler-map
           (assoc-delete-all 'rerender-view skg-response-handler-map))
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
        (message "skg: rerender-done handler error: %S" err)))
     (let ((retry (and skg--rerender-ugly-challenged
                       skg--rerender-ugly-retry)))
       (setq skg--rerender-ugly-retry nil
             skg--rerender-ugly-challenged nil)
       ;; The dispatcher removes this one-shot handler after return. A
       ;; zero-delay timer starts the retry after that removal, so the new
       ;; rerender-done handler is not accidentally deleted with the old one.
       (when retry (run-at-time 0 nil retry))))
   t))

(provide 'skg-request-rerender-all-views)
