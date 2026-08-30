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
;;;   - M-x skg-reload-from-id-stack: mark arbitrary IDs TO-RELOAD.

(require 'skg-length-prefix)
(require 'skg-config)
(require 'skg-id-search)
(require 'skg-request-save) ; for skg--collateral-view-handler

;;; ---- change detection ----------------------------------------------

(defvar skg--reload-file-snapshot (make-hash-table :test 'equal)
  "Maps each known .skg worktree path to (MTIME . SIZE) as last seen.")

(defvar skg--reload-initialized nil
  "Non-nil once the snapshot has been baselined at least once, so the
first magit refresh does not treat every file as new.")

(defun skg--reload-all-skg-files ()
  "List regular direct .skg children of the configured sources."
  (let ((files '()))
    (dolist (src (skg--source-paths))
      (let ((dir (cdr src)))
        (when (and dir (file-directory-p dir))
          (dolist (path (directory-files dir t "\\.skg\\'" t))
            (when (and (file-regular-p path)
                       (not (file-symlink-p path)))
              (push path files))))))
    (nreverse files)))

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

(defun skg-reload-paths (paths &optional ids incident-id terminal-callback)
  "Ask the server to reload the telescopes owning PATHS or IDS.
PATHS are absolute .skg paths.  IDS may contain primary or extra IDs.
INCIDENT-ID identifies retries of one reconciliation episode.  Invoke
TERMINAL-CALLBACK with the parsed terminal response, when non-nil."
  (when (or paths ids)
    (let* ((tcp-proc (skg-tcp-connect-to-rust))
           (request-sexp
            (concat (prin1-to-string
                     `((request . "reload paths")
                       (paths ,@paths)
                       (ids ,@ids)))
                    "\n"))
           (incident-id (or incident-id (skg-fresh-incident-id))))
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
           (when content (message "%s" content))
           (when terminal-callback
             (funcall terminal-callback response))))
       t) ; one-shot
      (skg-submit-request tcp-proc request-sexp nil incident-id))))

;;; ---- explicit ID-stack selection ---------------------------------

(defconst skg--reload-selection-buffer-name
  "*skg-reload-from-id-stack*")

(defvar-local skg--reload-selection-entries nil
  "Pairs of headline markers and canonical ID-stack IDs in this selector.")

(defvar-local skg--reload-selection-reason-overlays nil)

(defvar skg-reload-selection-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'skg--submit-reload-selection)
    (define-key map (kbd "C-x C-s") #'skg--reload-selection-refuse-save)
    map)
  "Keymap used only by `skg-reload-selection-mode'.")

(define-minor-mode skg-reload-selection-mode
  "Transient ID-stack selection for an explicit partial reload."
  :lighter " Reload-Select"
  :keymap skg-reload-selection-mode-map)

(put 'skg-reload-selection-mode 'completion-predicate #'ignore)

(defun skg--reload-selection-todo-sequence (_sequence)
  "Replace Org's ordinary TODO sequence inside a reload selector."
  '(sequence "TO-RELOAD" "|"))

(defun skg-reload-from-id-stack ()
  "Open a transient ID-stack copy whose marked nodes will be reloaded.
Use Org's standard S-left/S-right TODO cycling to mark `TO-RELOAD',
then C-c C-c to submit.  This never edits `skg-id-stack'."
  (interactive)
  (let ((buffer (get-buffer-create skg--reload-selection-buffer-name)))
    (switch-to-buffer buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (skg--format-id-stack-as-org))
      (goto-char (point-min))
      (skg--org-mode-with-options)
      (setq-local org-todo-keywords '((sequence "TO-RELOAD" "|")))
      (add-hook 'org-todo-setup-filter-hook
                #'skg--reload-selection-todo-sequence nil t)
      (org-set-regexps-and-options)
      (setq-local skg--reload-selection-entries nil)
      (let ((entries skg-id-stack))
        (org-map-entries
         (lambda ()
           (when entries
             (push (cons (copy-marker (line-beginning-position))
                         (caar entries))
                   skg--reload-selection-entries)
             (setq entries (cdr entries))))
         nil nil))
      (setq skg--reload-selection-entries
            (nreverse skg--reload-selection-entries))
      (setq-local skg--reload-selection-reason-overlays nil)
      (skg-reload-selection-mode 1)
      (set-buffer-modified-p nil))
    (message "Mark nodes TO-RELOAD with S-left/S-right; C-c C-c submits.")))

(defun skg--reload-selection-refuse-save ()
  (interactive)
  (user-error "This is a transient selector; use C-c C-c to reload marked nodes"))

(defun skg--marked-reload-selection-entries ()
  "Return marked (MARKER . ID) entries from the current selector."
  (cl-remove-if-not
   (lambda (entry)
     (save-excursion
       (goto-char (marker-position (car entry)))
       (equal (org-get-todo-state) "TO-RELOAD")))
   skg--reload-selection-entries))

(defun skg--submit-reload-selection ()
  "Submit marked IDs in the transient ID-stack selector."
  (interactive)
  (let* ((selection-buffer (current-buffer))
         (marked (skg--marked-reload-selection-entries))
         (ids (delete-dups (mapcar #'cdr marked))))
    (if (null ids)
        (message "skg: no ID-stack nodes are marked TO-RELOAD")
      (skg-reload-paths
       nil ids (skg-fresh-incident-id)
       (lambda (response)
         (when (buffer-live-p selection-buffer)
           (with-current-buffer selection-buffer
             (skg--apply-reload-selection-result response))))))))

(defun skg--apply-reload-selection-result (response)
  "Apply RESPONSE's per-ID outcomes to the current selector."
  (mapc #'delete-overlay skg--reload-selection-reason-overlays)
  (setq skg--reload-selection-reason-overlays nil)
  (let ((outcomes (cadr (assoc 'requested-id-outcomes response))))
    (dolist (outcome outcomes)
      (let ((id (format "%s" (cadr (assoc 'requested-id outcome))))
            (status (cadr (assoc 'status outcome)))
            (reason (cadr (assoc 'reason outcome))))
        (dolist (entry skg--reload-selection-entries)
          (when (equal id (cdr entry))
            (save-excursion
              (goto-char (marker-position (car entry)))
              (if (eq status 'acknowledged)
                  (when (org-get-todo-state)
                    (let ((inhibit-message t)) (org-todo 'none)))
                (let ((overlay (make-overlay
                                (line-end-position) (line-end-position))))
                  (overlay-put overlay 'after-string
                               (propertize
                                (format "  [%s]" (or reason "rejected"))
                                'face 'error))
                  (push overlay skg--reload-selection-reason-overlays))))))))
  (set-buffer-modified-p nil)))

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
