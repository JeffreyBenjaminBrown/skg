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

(defun skg-request-rerender-all-views ()
  "Ask the server to re-render every open view.
Locks all skg buffers, then registers handlers for the
streaming protocol: rerender-lock, rerender-view*, rerender-done."
  (let ((tcp-proc (skg-tcp-connect-to-rust)))
    (skg--begin-stream "rerender")
    (skg--lock-all-skg-buffers)
    (skg--register-rerender-stream-handlers)
    (skg-lp-reset)
    (process-send-string
     tcp-proc
     (concat (prin1-to-string
              '((request . "rerender all views")))
             "\n"))))

(defun skg--register-rerender-stream-handlers ()
  "Register the three handlers for streamed rerender responses.
Shared by 'skg-request-rerender-all-views' and 'skg-view-diff-mode'."
  (skg-register-response-handler
   ;; 1. Lock message: unlock buffers not in the URI list.
   'rerender-lock
   (lambda (_tcp-proc payload)
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
     (condition-case err
         (let* ((response (read payload))
                (uri (cadr (assoc 'view-uri response)))
                (content (cadr (assoc 'content response)))
                (buf (skg-find-buffer-by-uri uri)))
           (when buf
             (with-current-buffer buf
               (skg--unlock-after-save)
               (skg-replace-buffer-with-new-content nil content))))
       (error
        (skg-log 'error 'rerender "rerender-view handler error: %S" err))))
   nil) ;; non-one-shot: fires for each streamed view
  (skg-register-response-handler
   ;; 3. Done message: clean up and show errors.
   'rerender-done
   (lambda (_tcp-proc payload)
     (setq skg-response-handler-map
           (assoc-delete-all 'rerender-view skg-response-handler-map))
     (skg--end-stream)
     (skg--unlock-all-save-locked) ;; safety net
     (condition-case err
         (let* ((response (read payload))
                (errors-list (cadr (assoc 'errors response))))
           (when (and errors-list (not (null errors-list)))
             (skg-big-nonfatal-message
              "*skg rerender errors*"
              "Rerender completed with errors"
              (mapconcat 'identity errors-list "\n\n"))))
       (error
        (message "skg: rerender-done handler error: %S" err))))
   t))

(provide 'skg-request-rerender-all-views)
