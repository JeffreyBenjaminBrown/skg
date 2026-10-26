;;; -*- lexical-binding: t; -*-
;;;
;;; Handler for the "rerender all views" request/response,
;;; and the rerender stream that follows several other requests,
;;; with the prologue those requests share
;;; ('skg--send-streaming-request').
;;;
;;; Protocol: rerender-lock → rerender-view* → rerender-done.
;;; Each view is unlocked and updated as its rerender-view arrives.

(require 'skg-length-prefix)
(require 'skg-request-save) ; for skg-replace-buffer-with-new-content, skg-big-nonfatal-message
(require 'skg-buffer)       ; for skg-find-buffer-by-view-id
(require 'skg-request-server-warnings) ; for skg-show-corrections-in-server-warnings
(require 'skg-lock-buffers)

(defvar skg--rerender-overPrivateText-retry nil
  "Function to run after a challenged rerender's empty unwind completes.")

(defvar skg--rerender-overPrivateText-challenged nil
  "Non-nil while consuming the empty unwind after a privacy challenge.")

(defun skg-request-rerender-all-views (&optional approved-pids)
  "Ask the server to re-render every open view.
Locks all skg buffers, then registers handlers for the
streaming protocol: rerender-lock, rerender-view*, rerender-done."
  (skg--send-streaming-request
   "rerender" nil
   (append '((request . "rerender all views"))
           (when approved-pids
             `((approved-overPrivateText-pids ,@approved-pids))))
   nil nil nil
   (lambda (pids) (skg-request-rerender-all-views pids))))

(defun skg--send-streaming-request
    (label response-type request handler
           &optional other-handlers retry-after-empty-stream
           overPrivateText-retry)
  "Send REQUEST, an alist, for a command whose answer a rerender
stream follows. First begin the stream (LABEL names it in the error
when another is in flight), lock every skg buffer, register the
rerender stream's handlers and, one-shot, HANDLER for RESPONSE-TYPE,
then reset the receiver.
RESPONSE-TYPE and HANDLER may be nil, for a command whose only
answer is the rerender stream.
OTHER-HANDLERS, an alist of (TYPE . HANDLER), are the command's other
possible answers, also one-shot. With RETRY-AFTER-EMPTY-STREAM, a
handler may set `skg--rerender-after-empty-stream' to resend once
the empty stream that follows a confirmation is done.
OVERPRIVATETEXT-RETRY, a function of the approved PIDs, resends the
request when the server challenges it over private text; a challenge
cancels the one-shot handler for RESPONSE-TYPE, which the challenged
request no longer answers.
Each handler is called as (HANDLER TCP-PROC PAYLOAD).
If any step after the stream begins fails, the handlers registered
here are removed, the stream ends and the buffers unlock."
  (let ((tcp-proc (skg-tcp-connect-to-rust)))
    (skg--begin-stream label)
    (condition-case err
        (progn
          (skg--lock-all-skg-buffers)
          (skg--register-rerender-stream-handlers)
          (when retry-after-empty-stream
            (skg--wrap-rerender-done-with-after-empty-stream))
          (when response-type
            (skg-register-response-handler response-type handler t))
          (dolist (other other-handlers)
            (skg-register-response-handler (car other) (cdr other) t))
          (when overPrivateText-retry
            (skg--register-rerender-overPrivateText-confirmation
             overPrivateText-retry response-type))
          (skg-lp-reset)
          (process-send-string tcp-proc
                               (concat (prin1-to-string request) "\n")))
      (error
       (skg--unregister-response-handlers
        (append '(rerender-lock rerender-view rerender-done
                  overPrivateText-telescope-confirmation)
                (when response-type (list response-type))
                (mapcar #'car other-handlers)))
       (skg--end-stream)
       (skg--unlock-all-save-locked)
       (signal (car err) (cdr err))))))

(defun skg--unregister-response-handlers (response-types)
  "Remove the handler of each of RESPONSE-TYPES, balancing the pending
count for each one-shot handler removed."
  (dolist (type response-types)
    (let ((entry (assoc type skg-response-handler-map)))
      (when entry
        (when (cddr entry)
          (setq skg-lp--pending-count
                (max 0 (1- skg-lp--pending-count))))
        (setq skg-response-handler-map
              (assoc-delete-all type skg-response-handler-map))))))

(defun skg--response-errors-line (response)
  "The errors of RESPONSE, an alist with an (errors (...)) entry,
joined into one line, or nil when it has none."
  (let ((errors (cadr (assq 'errors response))))
    (when (skg--message-list-nonempty-p errors)
      (mapconcat (lambda (e) (format "%s" e)) errors "; "))))

(defun skg--register-rerender-overPrivateText-confirmation
    (retry &optional unfired-response-type)
  "Retry after the challenge unwind, approving its PIDs.
UNFIRED-RESPONSE-TYPE is the one-shot acknowledgement the challenged
request replaced; remove it and balance its pending count."
  (setq skg--rerender-overPrivateText-retry nil
        skg--rerender-overPrivateText-challenged nil)
  (skg-register-response-handler
   'overPrivateText-telescope-confirmation
   (lambda (_tcp-proc payload)
     (setq skg-response-handler-map
           (assoc-delete-all 'overPrivateText-telescope-confirmation
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
       (setq skg--rerender-overPrivateText-challenged t)
       (when (y-or-n-p (concat prompt " "))
         (setq skg--rerender-overPrivateText-retry
               (lambda () (funcall retry pids))))))
   nil))

(defun skg--register-rerender-stream-handlers ()
  "Register the three handlers for streamed rerender responses.
Shared by 'skg-request-rerender-all-views' and every request whose
response a rerender stream follows."
  (skg-register-response-handler
   ;; 1. Lock message: unlock buffers not in the view ID list.
   'rerender-lock
   (lambda (_tcp-proc payload)
     (unless skg--rerender-overPrivateText-challenged
       (setq skg-response-handler-map
             (assoc-delete-all 'overPrivateText-telescope-confirmation
                               skg-response-handler-map)))
     (condition-case err
         (let* ((response (read payload))
                (lock-entry (assoc 'lock-views response)))
           (when lock-entry
             (skg--unlock-buffers-not-in-view-id-list
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
   ;; 3. Done message: clean up and show errors/warnings and corrections.
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
               errors-list warnings-list)))
           (skg-show-corrections-in-server-warnings
            (cadr (assoc 'corrections response))))
       (error
        (message "skg: rerender-done handler error: %S" err)))
     (let ((retry (and skg--rerender-overPrivateText-challenged
                       skg--rerender-overPrivateText-retry)))
       (setq skg--rerender-overPrivateText-retry nil
             skg--rerender-overPrivateText-challenged nil)
       ;; The dispatcher removes this one-shot handler after return. A
       ;; zero-delay timer starts the retry after that removal, so the new
       ;; rerender-done handler is not accidentally deleted with the old one.
       (when retry (run-at-time 0 nil retry))))
   t))

;; A confirmation can arrive inside an otherwise empty rerender stream (the
;; delete-references and stage-and-commit commands).  The rerender handler
;; consumes that stream before invoking this one-shot retry.
(defvar skg--rerender-after-empty-stream nil)

(defun skg--wrap-rerender-done-with-after-empty-stream ()
  "Run a pending confirmation retry after rerender's empty-stream cleanup.
This keeps a command's confirmation retry local to the commands that ask
for it, rather than changing the generic rerender protocol while its
privacy-vocabulary migration is pending."
  (let* ((entry (assoc 'rerender-done skg-response-handler-map))
         (handler (cadr entry)))
    (unless handler (error "Missing rerender-done handler"))
    ;; Replace in place: the existing one-shot registration owns the pending
    ;; count, so registering it again would leave a stale pending response.
    (setq skg-response-handler-map
          (cons (cons 'rerender-done
                      (cons (lambda (tcp payload)
                              (funcall handler tcp payload)
                              (let ((after-empty skg--rerender-after-empty-stream))
                                (setq skg--rerender-after-empty-stream nil)
                                (when after-empty (run-at-time 0 nil after-empty))))
                            t))
                (assoc-delete-all 'rerender-done skg-response-handler-map)))))

(provide 'skg-request-rerender-all-views)
