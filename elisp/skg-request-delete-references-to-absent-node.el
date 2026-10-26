;;; -*- lexical-binding: t; -*-
;; The explicit cleanup command for one Unknown's raw relationship ID.

(require 'cl-lib)
(require 'skg-buffer)
(require 'skg-metadata) ; for skg--unknown-node-id-at-point
(require 'skg-request-rerender-all-views)
(require 'skg-request-save) ; for skg-big-nonfatal-message

(defun skg-delete-references-to-absent-node ()
  "Remove owned structured references to the Unknown headline at point."
  (interactive)
  (let ((gnid (skg--unknown-node-id-at-point)))
    (unless gnid
      (user-error "Point must be on an Unknown headline"))
    (when (cl-some (lambda (buf)
                     (and (buffer-local-value 'skg-view-id buf)
                          (buffer-modified-p buf)))
                   (buffer-list))
      (user-error "Save or revert every skg view before global reference cleanup"))
    (skg--delete-absent-send gnid nil)))

(defun skg--delete-absent-send (gnid approved-preview)
  "Send cleanup request for ID; APPROVED-PREVIEW is echoed verbatim on retry."
  (skg--send-streaming-request
   "delete absent references"
   'delete-references-confirmation
   (append `((request . "delete references to absent node")
             (id . ,gnid))
           (when approved-preview
             `((approved-preview . ,approved-preview))))
   (lambda (_tcp payload)
     (skg--delete-absent-confirmation-handler gnid payload))
   `((delete-references-result
      . ,(lambda (_tcp payload)
           (skg--delete-absent-result-handler payload)))
     (error
      . ,(lambda (_tcp payload)
           (skg--delete-absent-error-handler payload))))
   t))

(defun skg--delete-absent-confirmation-handler (gnid payload)
  "Show the preview in PAYLOAD; on approval, resend for GNID with its
token once the empty rerender stream that follows is done."
  ;; The confirmation replaces the result response for this request.
  ;; Balance its one-shot registration before the empty-stream retry.
  (skg--delete-absent-remove-unfired-handlers
   '(delete-references-result error))
  (let* ((response (read payload))
         (content (format "%s" (cadr (assoc 'content response))))
         (approval (format "%s" (cadr (assoc 'approved-preview response))))
         (buffer (get-buffer-create "*skg absent-reference warning*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)) (erase-buffer) (insert content) (org-mode)
           (read-only-mode 1)))
    (pop-to-buffer buffer)
    (when (yes-or-no-p "Remove the structured references? ")
      ;; The server sends an empty stream after confirmation.  A timer
      ;; after rerender-done avoids nesting a request in its filter.
      (setq skg--rerender-after-empty-stream
            (lambda () (skg--delete-absent-send gnid approval))))))

(defun skg--delete-absent-result-handler (payload)
  "Show the cleanup report in PAYLOAD."
  (let* ((response (read payload))
         (content (format "%s" (cadr (assoc 'content response)))))
    (skg--delete-absent-remove-unfired-handlers '(error))
    (skg-big-nonfatal-message "*skg absent-reference cleanup*"
                              "Absent-reference cleanup complete" content)))

(defun skg--delete-absent-error-handler (payload)
  "Report the server's refusal in PAYLOAD."
  (let* ((response (read payload))
         (content (format "%s" (cadr (assoc 'content response)))))
    ;; The dispatcher removes `error' itself.  Remove only its mutually
    ;; exclusive terminal handlers, balancing their pending counts.
    (skg--delete-absent-remove-unfired-handlers
     '(delete-references-confirmation delete-references-result))
    (message "skg absent-reference cleanup: %s" content)))

(defun skg--delete-absent-remove-unfired-handlers (types)
  "Remove unfired one-shot response TYPES and balance their pending count."
  (dolist (type types)
    (when (assoc type skg-response-handler-map)
      (setq skg-response-handler-map
            (assoc-delete-all type skg-response-handler-map)
            skg-lp--pending-count (max 0 (1- skg-lp--pending-count))))))

(provide 'skg-request-delete-references-to-absent-node)
