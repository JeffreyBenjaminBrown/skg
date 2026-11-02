;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: Extend a buffer of the diff view to the changes it does
;;; not make editable ('docs/diff-view.org', "Extending the diff
;;; view"). A save never makes an occurrence editable or appends a
;;; root on its own; this command asks the server to, for the current
;;; buffer. The answer carries the readiness summary, as a save's
;;; does, and a rerender stream follows it, carrying the rerendered
;;; buffer.
;;;
;;; DATA USED/ASSUMED: See docs/api-and-formats.org.

(require 'skg-buffer)
(require 'skg-readiness) ; for skg--readiness-summary-message
(require 'skg-request-rerender-all-views)

(defun skg-diffView-extend ()
  "In a buffer of the diff view, make every changed node that
has no editable occurrence editable at its first occurrence, and
append as roots the origins of changes the buffer shows nowhere.
Refuses while the buffer has unsaved changes."
  (interactive)
  (unless skg-view-id
    (user-error "Not in a skg view"))
  (when (buffer-modified-p)
    (user-error "Save the buffer first."))
  (skg--send-streaming-request
   "extend diff view"
   'extend-diff-view
   `((request . "extend diff view")
     (view-id . ,skg-view-id))
   (lambda (_tcp payload)
     (skg--extend-diff-view-handler payload))))

(defun skg--extend-diff-view-handler (payload)
  "Show the readiness summary in PAYLOAD, or its errors."
  (condition-case err
      (let* ((response (read payload))
             (errors-line (skg--response-errors-line response))
             (summary (assq 'readiness-summary response)))
        (cond (errors-line
               (message "%s" errors-line))
              (summary
               (message "%s" (skg--readiness-summary-message summary)))))
    (error (message "skg: extend-diff-view handler error: %S" err))))

(provide 'skg-request-extend-diff-view)
