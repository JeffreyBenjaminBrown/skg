;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: Lock skg buffers against edits during a save-in-progress.
;;;
;;; Uses overlays rather than buffer-read-only because:
;;; - Custom error message ("skg: buffer locked -- save in progress")
;;;   instead of the generic "Buffer is read-only".
;;; - inhibit-read-only (used by skg-replace-buffer-with-new-content)
;;;   does NOT suppress overlay hooks, so we must explicitly remove
;;;   overlays before updating buffer content -- which is what we want.

(defvar skg--stream-in-progress nil
  "Client-side bookkeeping: non-nil while a streaming request
\(save, rerender, diff-toggle) is in flight.  The server handles
requests sequentially, so responses never interleave -- but Emacs
can send a second request before the first finishes.  This guard
prevents that, because overlapping operations would corrupt each
other's lock/unlock state.  Value is nil or a label string
describing the operation (shown in the error message).
Cleared by terminal handlers (save-result, rerender-done),
the TCP sentinel, and the busy-initializing handler.")

(defvar skg--stream-owner nil
  "Operation token owning `skg--stream-in-progress', or nil for legacy streams.")

(defvar-local skg--save-lock-overlay nil
  "Overlay that blocks edits while a save is in progress.")

(defvar-local skg--save-lock-owner nil
  "Operation token owning this buffer's save lock, or nil for legacy locks.")

(defun skg--save-lock-signal (&rest _)
  "Signal an error when the user tries to edit a save-locked buffer."
  (error "skg: buffer locked -- save in progress"))

(defun skg--lock-for-save (&optional owner)
  "Lock the current buffer and assign OWNER when it is newly locked."
  (unless skg--save-lock-overlay
    (let ((ov (make-overlay (point-min) (point-max))))
      (overlay-put ov 'modification-hooks       '(skg--save-lock-signal))
      (overlay-put ov 'insert-in-front-hooks    '(skg--save-lock-signal))
      (overlay-put ov 'insert-behind-hooks      '(skg--save-lock-signal))
      (setq skg--save-lock-overlay ov
            skg--save-lock-owner owner)) ))

(defun skg--unlock-after-save (&optional owner)
  "Remove the save-lock overlay from the current buffer."
  (when (and skg--save-lock-overlay
             (or (null owner)
                 (equal owner skg--save-lock-owner)))
    (delete-overlay skg--save-lock-overlay)
    (setq skg--save-lock-overlay nil
          skg--save-lock-owner nil)) )

(defun skg--unlock-all-save-locked (&optional owner)
  "Remove save locks owned by OWNER, or all legacy locks when OWNER is nil."
  (dolist (buf (buffer-list))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (skg--unlock-after-save owner)) )) )

(defun skg--lock-all-skg-buffers (&optional owner)
  "Lock every skg content-view buffer, assigning OWNER when supplied."
  (dolist (buf (buffer-list))
    (when (and (buffer-live-p buf)
               (buffer-local-value 'skg-view-uri buf))
      (with-current-buffer buf
        (skg--lock-for-save owner)) )) )

(defun skg--unlock-buffers-not-in-uri-list (uri-list &optional owner)
  "Unlock skg buffers not in URI-LIST whose lock belongs to OWNER."
  (dolist (buf (buffer-list))
    (when (and (buffer-live-p buf)
               (buffer-local-value 'skg-view-uri buf)
               (buffer-local-value 'skg--save-lock-overlay buf))
      (let ((uri (buffer-local-value 'skg-view-uri buf)))
        (unless (member uri uri-list)
          (with-current-buffer buf
            (skg--unlock-after-save owner)) )) )) )

(defun skg--unlock-non-collateral-buffers (saved-uri collateral-uris &optional owner)
  "Unlock skg buffers that are NOT SAVED-URI and NOT in COLLATERAL-URIS.
The keep-locked set is the collateral views plus the saved view itself."
  (skg--unlock-buffers-not-in-uri-list (cons saved-uri collateral-uris) owner))

(defun skg--begin-stream (label &optional owner)
  "Mark a streaming operation as in progress.
LABEL is a string describing the operation (for the error message).
Signals an error if another stream is already in flight."
  (when skg--stream-in-progress
    (error "skg: %s blocked -- %s already in progress"
           label skg--stream-in-progress))
  (setq skg--stream-in-progress label
        skg--stream-owner owner))

(defun skg--end-stream (&optional owner)
  "Clear the streaming-in-progress guard."
  (when (or (null owner)
            (equal owner skg--stream-owner))
    (setq skg--stream-in-progress nil
          skg--stream-owner nil)))

(defun skg--register-stream-request-cleanup (label &optional owner)
  "Make the current request own cleanup for transient stream LABEL."
  (skg-set-request-failure-handler
   (lambda (reason)
     (message "skg: %s interrupted: %s" label reason)))
  (skg-set-request-finalizer
   (lambda (_reason)
     (skg--end-stream owner)
     (skg--unlock-all-save-locked owner))))

(provide 'skg-lock-buffers)
