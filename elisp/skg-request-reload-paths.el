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
(require 'filenotify)

;;; ---- change observation -------------------------------------------

(defvar skg--reload-observation-paths (make-hash-table :test 'equal)
  "Candidate paths mapped to their newest client observation sequence.")
(defvar skg--reload-observation-sequence 0)
(defvar skg--reload-observation-incident-id nil)
(defvar skg--reload-observation-timer nil)
(defvar skg--reload-observation-in-flight nil)
(defvar skg--reload-observation-full-sweep nil)
(defvar skg--reload-watch-descriptors nil)

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

(defun skg--reload-direct-source-path-p (path)
  "Whether PATH names a possible direct .skg source child."
  (and (stringp path)
       (string-match-p "\\.skg\\'" path)
       (cl-some
        (lambda (source)
          (equal (file-name-as-directory
                  (expand-file-name (file-name-directory path)))
                 (file-name-as-directory (expand-file-name (cdr source)))))
        (skg--source-paths))))

(defun skg--enqueue-reload-candidate (path)
  "Retain PATH as a candidate; do not stat, read or hash it here."
  (when (skg--reload-direct-source-path-p path)
    (puthash (expand-file-name path)
             (cl-incf skg--reload-observation-sequence)
             skg--reload-observation-paths)
    (unless skg--reload-observation-incident-id
      (setq skg--reload-observation-incident-id
            (skg-fresh-incident-id)))
    (skg--schedule-reload-observation 0.35)))

(defun skg--request-reload-full-sweep ()
  "Ask the next observation request to compare the complete manifest."
  (setq skg--reload-observation-full-sweep t)
  (unless skg--reload-observation-incident-id
    (setq skg--reload-observation-incident-id
          (skg-fresh-incident-id)))
  (skg--schedule-reload-observation 0))

(defun skg--schedule-reload-observation (delay)
  (when (timerp skg--reload-observation-timer)
    (cancel-timer skg--reload-observation-timer))
  (setq skg--reload-observation-timer
        (run-at-time delay nil #'skg--dispatch-reload-observation)))

(defun skg--dispatch-reload-observation ()
  "Dispatch the newest coalesced candidates when no predecessor is active."
  (setq skg--reload-observation-timer nil)
  (unless skg--reload-observation-in-flight
    (let ((snapshot nil)
          (full-sweep skg--reload-observation-full-sweep)
          (incident skg--reload-observation-incident-id))
      (maphash (lambda (path sequence)
                 (push (cons path sequence) snapshot))
               skg--reload-observation-paths)
      (when (or snapshot full-sweep)
        (setq skg--reload-observation-in-flight t)
        (condition-case err
            (skg-reload-paths
             (mapcar #'car snapshot) nil incident
             (lambda (response)
               (skg--finish-reload-observation
                snapshot full-sweep incident response))
             full-sweep)
          (error
           (setq skg--reload-observation-in-flight nil)
           (skg-log 'error 'reload "observation dispatch failed: %S" err)))))))

(defun skg--finish-reload-observation
    (snapshot full-sweep incident response)
  "Retire exactly SNAPSHOT after RESPONSE, or retain it when deferred."
  (setq skg--reload-observation-in-flight nil)
  (let ((deferred (cadr (assoc 'deferred response)))
        (status (cadr (assoc 'terminal-status response))))
    (cond
     (deferred
      ;; Keep the same incident and newest per-path observations.  A control
      ;; connection may bracket a slow serial pull for minutes.
      (setq skg--reload-observation-incident-id incident)
      (skg--schedule-reload-observation 1.0))
     ((eq status 'failed)
      ;; Stable invalid bytes do not spin.  A new filesystem event or explicit
      ;; sweep retries them; the retained candidates preserve their incident.
      nil)
     (t
      (dolist (entry snapshot)
        (when (equal (gethash (car entry) skg--reload-observation-paths)
                     (cdr entry))
          (remhash (car entry) skg--reload-observation-paths)))
      (when full-sweep (setq skg--reload-observation-full-sweep nil))
      (if (and (= (hash-table-count skg--reload-observation-paths) 0)
               (not skg--reload-observation-full-sweep))
          (setq skg--reload-observation-incident-id nil)
        (setq skg--reload-observation-incident-id
              (skg-fresh-incident-id))
        (skg--schedule-reload-observation 0))))))

(defun skg--reload-file-notify-callback (event)
  "Turn one file-notify EVENT into path candidates only."
  (condition-case err
      (pcase (cadr event)
        ((or 'created 'changed 'attribute-changed 'deleted)
         (skg--enqueue-reload-candidate (caddr event)))
        ('renamed
         (skg--enqueue-reload-candidate (caddr event))
         (skg--enqueue-reload-candidate (cadddr event)))
        ((or 'stopped 'watcher-stopped)
         (skg--request-reload-full-sweep)))
    (error (skg-log 'error 'reload "file notification failed: %S" err))))

(defun skg-stop-reload-observation ()
  (dolist (descriptor skg--reload-watch-descriptors)
    (ignore-errors (file-notify-rm-watch descriptor)))
  (setq skg--reload-watch-descriptors nil))

(defun skg-start-reload-observation ()
  "Install one nonrecursive watch per authoritative source directory."
  (skg-stop-reload-observation)
  (dolist (source (skg--source-paths))
    (when (file-directory-p (cdr source))
      (push (file-notify-add-watch
             (cdr source) '(change attribute-change)
             #'skg--reload-file-notify-callback)
            skg--reload-watch-descriptors)))
  (skg--request-reload-full-sweep))

;;; ---- the request ---------------------------------------------------

(defun skg-reload-paths
    (paths &optional ids incident-id terminal-callback full-sweep)
  "Ask the server to reload the telescopes owning PATHS or IDS.
PATHS are absolute .skg paths.  IDS may contain primary or extra IDs.
INCIDENT-ID identifies retries of one reconciliation episode.  Invoke
TERMINAL-CALLBACK with the parsed terminal response, when non-nil."
  (when (or paths ids full-sweep)
    (let* ((tcp-proc (skg-tcp-connect-to-rust))
           (request-sexp
            (concat (prin1-to-string
                     `((request . "reload paths")
                       (paths ,@paths)
                       (ids ,@ids)
                       (full-sweep . ,(if full-sweep "true" "false"))))
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

(defun skg--submit-reload-selection (&optional incident-id)
  "Submit marked IDs in the transient ID-stack selector."
  (interactive)
  (let* ((selection-buffer (current-buffer))
         (marked (skg--marked-reload-selection-entries))
         (ids (delete-dups (mapcar #'cdr marked)))
         (incident-id (or incident-id (skg-fresh-incident-id))))
    (if (null ids)
        (message "skg: no ID-stack nodes are marked TO-RELOAD")
      (skg-reload-paths
       nil ids incident-id
       (lambda (response)
         (when (buffer-live-p selection-buffer)
           (with-current-buffer selection-buffer
             (if (cadr (assoc 'deferred response))
                 (run-at-time
                  1.0 nil
                  (lambda ()
                    (when (buffer-live-p selection-buffer)
                      (with-current-buffer selection-buffer
                        (skg--submit-reload-selection incident-id)))))
               (skg--apply-reload-selection-result response)))))))))

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
  "Magit hook: request an authoritative full-manifest comparison."
  (condition-case err
      (skg--request-reload-full-sweep)
    (error
     (skg-log 'error 'reload "magit refresh observation: %s"
              (error-message-string err)))))

(defun skg--reload-after-skg-save ()
  "after-save-hook for raw .skg buffers: enqueue the exact saved path."
  (when (and buffer-file-name
             (string-match-p "\\.skg\\'" buffer-file-name))
    (condition-case err
        (skg--enqueue-reload-candidate buffer-file-name)
      (error
       (skg-log 'error 'reload "raw-file save observation: %s"
                (error-message-string err))))))

(defun skg-reload-changed ()
  "Compare the complete disk manifest and reload exact changed bytes."
  (interactive)
  (skg--request-reload-full-sweep)
  (message "skg: queued a complete .skg manifest comparison"))

;; Global hook: fires on every magit refresh regardless of any minor
;; mode. Mirrors the top-level find-file-hook registration style.
(add-hook 'magit-post-refresh-hook #'skg--reload-on-magit-refresh)

(provide 'skg-request-reload-paths)
