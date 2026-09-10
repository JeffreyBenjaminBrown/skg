;;; -*- lexical-binding: t; -*-
;; The explicit cleanup command for one Unknown's raw relationship ID.

(require 'skg-buffer)
(require 'skg-length-prefix)
(require 'skg-lock-buffers)
(require 'skg-metadata)
(require 'skg-request-rerender-all-views)
(require 'skg-request-save)

;; A cleanup confirmation arrives inside an otherwise empty stream.  The
;; rerender handler consumes that stream before invoking this one-shot retry.
(defvar skg--rerender-after-empty-stream nil)

(defun skg-delete-references-to-absent-node ()
  "Remove owned structured references to the Unknown headline at point."
  (interactive)
  (let ((metadata (skg--metadata-sexp-at-point-or-nil)))
    (unless (skg--unknown-headline-p metadata)
      (user-error "Point must be on an Unknown headline"))
    (when (cl-some (lambda (buf)
                     (and (buffer-local-value 'skg-view-uri buf)
                          (buffer-modified-p buf)))
                   (buffer-list))
      (user-error "Save or revert every skg view before global reference cleanup"))
    (let ((id (skg--relationship-member-id metadata)))
      (unless id (user-error "Unknown headline lacks its raw ID"))
      (skg--delete-absent-send id nil))))

(defun skg--delete-absent-send (id approved-preview)
  "Send cleanup request for ID; APPROVED-PREVIEW is echoed verbatim on retry."
  (let ((tcp-proc (skg-tcp-connect-to-rust)))
    (skg--begin-stream "delete absent references")
    (skg--lock-all-skg-buffers)
    (skg--register-rerender-stream-handlers)
    (skg--delete-absent-wrap-rerender-done)
    (skg-register-response-handler
     'delete-references-confirmation
     (lambda (_tcp payload)
       ;; The confirmation replaces the result response for this request.
       ;; Balance its one-shot registration before the empty-stream retry.
       (when (assoc 'delete-references-result skg-response-handler-map)
         (setq skg-response-handler-map
               (assoc-delete-all 'delete-references-result skg-response-handler-map)
               skg-lp--pending-count (max 0 (1- skg-lp--pending-count))))
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
                 (lambda () (skg--delete-absent-send id approval))))))
     t)
    (skg-register-response-handler
     'delete-references-result
     (lambda (_tcp payload)
       (let* ((response (read payload))
              (content (format "%s" (cadr (assoc 'content response)))))
         (skg-big-nonfatal-message "*skg absent-reference cleanup*"
                                   "Absent-reference cleanup complete" content)))
     t)
    (skg-lp-reset)
    (process-send-string
     tcp-proc
     (concat (prin1-to-string
              (append `((request . "delete references to absent node")
                        (id . ,id))
                      (when approved-preview
                        `((approved-preview . ,approved-preview)))))
             "\n"))))

(defun skg--delete-absent-wrap-rerender-done ()
  "Run a pending cleanup retry after rerender's empty-stream cleanup.
This keeps the cleanup's confirmation retry local rather than changing the
generic rerender protocol while its privacy-vocabulary migration is pending."
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

(provide 'skg-request-delete-references-to-absent-node)
