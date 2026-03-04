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

(provide 'skg-lock-buffers)
