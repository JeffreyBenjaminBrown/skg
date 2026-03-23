;;; -*- lexical-binding: t; -*-
;;;
;;; Handler for the "rerender all views" request/response.
;;; Used by skg-view-diff-mode to refresh all open views
;;; after toggling diff mode.

(require 'skg-length-prefix)
(require 'skg-request-save) ; for skg-replace-buffer-with-new-content, skg-big-nonfatal-message
(require 'skg-buffer)       ; for skg-find-buffer-by-uri

(defun skg-request-rerender-all-views ()
  "Ask the server to re-render every open view.
Registers a one-shot handler for the response."
  (let ((tcp-proc (skg-tcp-connect-to-rust)))
    (skg-register-response-handler
     'rerender-all-views
     (lambda (_tcp-proc payload)
       (skg--handle-rerender-all-views payload))
     t)
    (skg-lp-reset)
    (process-send-string
     tcp-proc
     (concat (prin1-to-string
              '((request . "rerender all views")))
             "\n"))))

(defun skg--handle-rerender-all-views (payload)
  "Handle the rerender-all-views LP response.
PAYLOAD format: ((views ((URI1 CONTENT1) ...)) (errors (...)))."
  (condition-case err
      (let* ((response (read payload))
             (views-list (cadr (assoc 'views response)))
             (errors-list (cadr (assoc 'errors response))))
        (when views-list
          (dolist (entry views-list)
            (let* ((uri (car entry))
                   (new-content (cadr entry))
                   (buf (skg-find-buffer-by-uri uri)))
              (when buf
                (with-current-buffer buf
                  (skg-replace-buffer-with-new-content
                   nil new-content))))))
        (when (and errors-list (not (null errors-list)))
          (skg-big-nonfatal-message
           "*skg rerender errors*"
           "Rerender completed with errors"
           (mapconcat 'identity errors-list "\n\n"))))
    (error
     (message "skg: rerender-all-views handler error: %S" err))))

(provide 'skg-request-rerender-all-views)
