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

(defvar-local skg--save-lock-overlay nil
  "Overlay that blocks edits while a save is in progress.")

(defun skg--save-lock-signal (&rest _)
  "Signal an error when the user tries to edit a save-locked buffer."
  (error "skg: buffer locked -- save in progress"))

(defun skg--lock-for-save ()
  "Lock the current buffer against edits using an overlay."
  (unless skg--save-lock-overlay
    (let ((ov (make-overlay (point-min) (point-max))))
      (overlay-put ov 'modification-hooks       '(skg--save-lock-signal))
      (overlay-put ov 'insert-in-front-hooks    '(skg--save-lock-signal))
      (overlay-put ov 'insert-behind-hooks      '(skg--save-lock-signal))
      (setq skg--save-lock-overlay ov)) ))

(defun skg--unlock-after-save ()
  "Remove the save-lock overlay from the current buffer."
  (when skg--save-lock-overlay
    (delete-overlay skg--save-lock-overlay)
    (setq skg--save-lock-overlay nil)) )

(defun skg--unlock-all-save-locked ()
  "Remove save-lock overlays from every buffer that has one."
  (dolist (buf (buffer-list))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (skg--unlock-after-save)) )) )

(defun skg--lock-all-skg-buffers ()
  "Lock every skg content-view buffer against edits."
  (dolist (buf (buffer-list))
    (when (and (buffer-live-p buf)
               (buffer-local-value 'skg-view-uri buf))
      (with-current-buffer buf
        (skg--lock-for-save)) )) )

(defun skg--unlock-non-collateral-buffers (saved-uri collateral-uris)
  "Unlock skg buffers that are NOT SAVED-URI and NOT in COLLATERAL-URIS."
  (dolist (buf (buffer-list))
    (when (and (buffer-live-p buf)
               (buffer-local-value 'skg-view-uri buf)
               (buffer-local-value 'skg--save-lock-overlay buf))
      (let ((uri (buffer-local-value 'skg-view-uri buf)))
        (unless (or (string= uri saved-uri)
                    (member uri collateral-uris))
          (with-current-buffer buf
            (skg--unlock-after-save)) )) )) )

(defun skg--unlock-buffers-not-in-uri-list (uri-list)
  "Unlock skg buffers whose URI is NOT in URI-LIST."
  (dolist (buf (buffer-list))
    (when (and (buffer-live-p buf)
               (buffer-local-value 'skg-view-uri buf)
               (buffer-local-value 'skg--save-lock-overlay buf))
      (let ((uri (buffer-local-value 'skg-view-uri buf)))
        (unless (member uri uri-list)
          (with-current-buffer buf
            (skg--unlock-after-save)) )) )) )

(defun skg--begin-stream (label)
  "Mark a streaming operation as in progress.
LABEL is a string describing the operation (for the error message).
Signals an error if another stream is already in flight."
  (when skg--stream-in-progress
    (error "skg: %s blocked -- %s already in progress"
           label skg--stream-in-progress))
  (setq skg--stream-in-progress label))

(defun skg--end-stream ()
  "Clear the streaming-in-progress guard."
  (setq skg--stream-in-progress nil))

(provide 'skg-lock-buffers)
