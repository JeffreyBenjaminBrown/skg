;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: Ask the server to partially reload specific .skg
;;; telescopes after they change on disk out of band (a magit discard,
;;; an external edit, a plain save of a .skg buffer). The server
;;; re-reads those telescopes from disk and updates its derived stores;
;;; it never writes .skg files. See TODO/partial-reload-and-magit/.
;;;
;;; Triggers (none requires skg-readable-ids-mode):
;;;   - magit-post-refresh-hook (global): reload whatever .skg worktree
;;;     files changed since the last scan.
;;;   - after-save-hook in skg-file-minor-mode: reload the saved file.
;;;   - M-x skg-reload-changed: reload changed files on demand.

(require 'skg-length-prefix)
(require 'skg-config)
(require 'skg-request-save) ; for skg--collateral-view-handler

;;; ---- change detection ----------------------------------------------

(defvar skg--reload-file-snapshot (make-hash-table :test 'equal)
  "Maps each known .skg worktree path to (MTIME . SIZE) as last seen.")

(defvar skg--reload-initialized nil
  "Non-nil once the snapshot has been baselined at least once, so the
first magit refresh does not treat every file as new.")

(defun skg--reload-all-skg-files ()
  "List absolute paths of every .skg file under the configured sources."
  (let ((files '()))
    (dolist (src (skg--source-paths))
      (let ((dir (cdr src)))
        (when (and dir (file-directory-p dir))
          (setq files
                (nconc files
                       (directory-files-recursively dir "\\.skg\\'"))))))
    files))

(defun skg--reload-file-stamp (path)
  "Return (MTIME . SIZE) for PATH, or nil if it cannot be stat'd."
  (let ((attrs (file-attributes path)))
    (when attrs
      (cons (file-attribute-modification-time attrs)
            (file-attribute-size attrs)))))

(defun skg--reload-refresh-snapshot ()
  "Reset the snapshot to the current on-disk state (a fresh baseline)."
  (clrhash skg--reload-file-snapshot)
  (dolist (path (skg--reload-all-skg-files))
    (let ((stamp (skg--reload-file-stamp path)))
      (when stamp
        (puthash path stamp skg--reload-file-snapshot))))
  (setq skg--reload-initialized t))

(defun skg--reload-scan-changed ()
  "Return the .skg paths that changed since the last scan (new,
modified, or deleted), and update the snapshot to the current state."
  (let ((current (make-hash-table :test 'equal))
        (changed '()))
    (dolist (path (skg--reload-all-skg-files)) ; new or modified
      (let ((stamp (skg--reload-file-stamp path)))
        (when stamp
          (puthash path stamp current)
          (unless (equal (gethash path skg--reload-file-snapshot) stamp)
            (push path changed)))))
    (maphash ; vanished: in the snapshot, gone now
     (lambda (path _stamp)
       (unless (gethash path current)
         (push path changed)))
     skg--reload-file-snapshot)
    (setq skg--reload-file-snapshot current)
    (setq skg--reload-initialized t)
    (nreverse changed)))

;;; ---- the request ---------------------------------------------------

(defun skg-reload-paths (paths)
  "Ask the server to reload the telescopes owning PATHS (absolute .skg
paths). The server streams collateral-view updates for any open view it
touches, then a final reload-paths summary."
  (when paths
    (let* ((tcp-proc (skg-tcp-connect-to-rust))
           (paths-str
            (mapconcat (lambda (p) (format "%S" p)) paths " "))
           (request-sexp
            (concat (format "((request . \"reload paths\") (paths %s))"
                            paths-str)
                    "\n")))
      (skg-register-response-handler ; refresh any open touched buffers
       'collateral-view
       (lambda (_tcp-proc payload)
         (skg--collateral-view-handler payload))
       nil) ; non-one-shot
      (skg-register-response-handler
       'reload-paths
       (lambda (_tcp-proc payload)
         (let* ((response (read payload))
                (content (cadr (assoc 'content response))))
           (when content (message "%s" content))))
       t) ; one-shot
      (skg-lp-reset)
      (process-send-string tcp-proc request-sexp))))

;;; ---- triggers ------------------------------------------------------

(defun skg--reload-on-magit-refresh ()
  "magit-post-refresh-hook: reload whatever .skg worktree files changed.
The first refresh only baselines the snapshot, so nothing is reloaded
en masse at startup. Errors are logged, never signalled, so a bug here
can never break magit's refresh."
  (condition-case err
      (if (not skg--reload-initialized)
          (skg--reload-refresh-snapshot)
        (let ((changed (skg--reload-scan-changed)))
          (when changed (skg-reload-paths changed))))
    (error
     (skg-log (format "skg reload (magit refresh): %s"
                      (error-message-string err))))))

(defun skg--reload-after-skg-save ()
  "after-save-hook for .skg buffers: reload the just-saved file and keep
its snapshot entry current so the magit hook won't re-fire on it."
  (when (and buffer-file-name
             (string-match-p "\\.skg\\'" buffer-file-name))
    (condition-case err
        (progn
          (skg-reload-paths (list buffer-file-name))
          (let ((stamp (skg--reload-file-stamp buffer-file-name)))
            (when stamp
              (puthash buffer-file-name stamp
                       skg--reload-file-snapshot))))
      (error
       (skg-log (format "skg reload (after save): %s"
                        (error-message-string err)))))))

(defun skg-reload-changed ()
  "Scan the .skg source dirs and reload any that changed on disk.
Covers edits made entirely outside Emacs."
  (interactive)
  (let ((changed (skg--reload-scan-changed)))
    (if changed
        (progn
          (skg-reload-paths changed)
          (message "skg: reloading %d changed .skg file(s)"
                   (length changed)))
      (message "skg: no changed .skg files"))))

;; Global hook: fires on every magit refresh regardless of any minor
;; mode. Mirrors the top-level find-file-hook registration style.
(add-hook 'magit-post-refresh-hook #'skg--reload-on-magit-refresh)

(provide 'skg-request-reload-paths)
