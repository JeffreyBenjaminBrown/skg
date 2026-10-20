;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: Integrate the changes not editable in a buffer of the
;;; diff view ('docs/diff-view.org', "Integrating unshown
;;; changes"). A save never makes an occurrence
;;; editable or appends a root on its own; this command asks the
;;; server to, for the current buffer. The answer carries the
;;; readiness summary, as a save's does, and a rerender stream
;;; follows it, carrying the redrawn buffer.
;;;
;;; DATA USED/ASSUMED: See docs/api-and-formats.org.

(require 'skg-buffer)
(require 'skg-length-prefix)
(require 'skg-lock-buffers)
(require 'skg-request-rerender-all-views)
(require 'skg-request-save) ; for skg--readiness-summary-message

(defun skg-diffView-integrate-unshown-changes ()
  "In a buffer of the diff view, make every changed node that
has no editable occurrence editable at its first occurrence, and
append as roots the origins of changes the buffer draws nowhere.
Refuses while the buffer has unsaved changes."
  (interactive)
  (unless skg-view-id
    (user-error "Not in a skg view"))
  (when (buffer-modified-p)
    (user-error "Save the buffer first."))
  (let ((tcp-proc (skg-tcp-connect-to-rust)))
    (skg--begin-stream "integrate unshown changes")
    (skg--lock-all-skg-buffers)
    (skg--register-rerender-stream-handlers)
    (skg-register-response-handler
     'integrate-unshown-changes
     (lambda (_tcp payload)
       (skg--integrate-unshown-changes-handler payload))
     t)
    (skg-lp-reset)
    (process-send-string
     tcp-proc
     (concat (prin1-to-string
              `((request . "integrate unshown changes")
                (view-id . ,skg-view-id)))
             "\n"))))

(defun skg--integrate-unshown-changes-handler (payload)
  "Show the readiness summary in PAYLOAD, or its errors."
  (condition-case err
      (let* ((response (read payload))
             (errors-list (cadr (assq 'errors response)))
             (summary (assq 'readiness-summary response)))
        (cond ((skg--message-list-nonempty-p errors-list)
               (message "%s" (mapconcat (lambda (e) (format "%s" e))
                                        errors-list "; ")))
              (summary
               (message "%s" (skg--readiness-summary-message summary)))))
    (error (message "skg: integrate-unshown-changes handler error: %S" err))))

(provide 'skg-request-integrate-unshown-changes)
