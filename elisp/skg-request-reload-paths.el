;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: Enter recovery maintenance for an explicit partial reload,
;;; or send a non-mutating observation hint to the process-owned watcher.
;;;
;;; Triggers (none requires skg-readable-ids-mode):
;;;   - magit-post-refresh-hook (global): request an exact full observation.
;;;   - M-x skg-reload-changed: request an exact full observation.
;;;   - M-x skg-reload-from-id-stack: mark arbitrary IDs TO-RELOAD.

(require 'cl-lib)
(require 'skg-id-search)
(require 'skg-maintenance)

;;; ---- change observation -------------------------------------------

(defvar skg--pending-recovery-incidents nil
  "Fatal reload incidents whose durable recovery journals remain unresolved.")

(defun skg--request-reload-full-sweep ()
  "Ask the process-owned observer to compare the complete disk manifest."
  (let ((tcp-proc (skg-tcp-connect-to-rust)))
    (skg-register-response-handler
     'reload-paths
     (lambda (_tcp-proc payload)
       (let ((response (read payload)))
         (unless (equal (format "%s"
                                (cadr (assoc 'observation-queued response)))
                        "true")
           (display-warning
            'skg (or (cadr (assoc 'content response)) payload) :warning))))
     t)
    (skg-submit-request
     tcp-proc "((request . \"reload paths\") (full-sweep . \"true\"))\n")))

;;; ---- the request ---------------------------------------------------

(defun skg-reload-paths
    (paths &optional ids _incident-id terminal-callback full-sweep
           _scalar-approved-pids)
  "Reload PATHS or IDS through recovery maintenance.
The obsolete FULL-SWEEP compatibility call only queues process-owned disk
observation.  It never selects disk or mutates a store."
  (cond
   (full-sweep (skg--request-reload-full-sweep))
   ((or paths ids)
    (when (skg--confirm-explicit-reload-with-dirty-views)
      (skg-begin-maintenance
       "explicit-partial-reload" nil paths ids terminal-callback)))
   (t (user-error "Reload names no paths or IDs"))))

(defun skg--recovery-report (response)
  "Render a successful reload recovery RESPONSE as an Org report."
  (concat
   "* Fatal reload recovery complete\n"
   (format "%s\n" (or (cadr (assoc 'content response)) "Recovered."))
   "** repositories and refs\n"
   (if-let ((repositories (cadr (assoc 'repositories response))))
       (mapconcat
        (lambda (repository)
          (format
           "*** %s\n**** pre-incident: %s (%s)\n**** legal: %s (%s)\n**** complete incident: %s (%s)"
           (cadr (assoc 'root repository))
           (cadr (assoc 'pre-ref repository))
           (cadr (assoc 'pre-commit repository))
           (cadr (assoc 'legal-ref repository))
           (cadr (assoc 'legal-commit repository))
           (cadr (assoc 'incident-ref repository))
           (cadr (assoc 'incident-commit repository))))
        repositories "\n")
     "None.\n")
   "\n** restored owned paths\n"
   (mapconcat (lambda (path) (format "*** %s" path))
              (cadr (assoc 'restored-paths response)) "\n")
   "\n** warnings\n"
   (mapconcat (lambda (warning) (format "*** %s" warning))
              (cadr (assoc 'warnings response)) "\n")
   "\n"))

(defun skg-recover-reload-incident (&optional incident-id)
  "After explicit confirmation, recover fatal reload INCIDENT-ID.
Recovery creates the three documented Git refs without checkout or staging,
then restores only owned fatal telescope paths to Skg's last-good bytes."
  (interactive)
  (let* ((ids (mapcar (lambda (entry)
                        (format "%s" (cadr (assoc 'incident-id entry))))
                      skg--pending-recovery-incidents))
         (incident-id
          (or incident-id
              (and ids (completing-read "Fatal reload incident: " ids nil t))
              (user-error "Skg knows of no unresolved recovery incident"))))
    (when (yes-or-no-p
           (format "Create recovery refs and restore fatal files for %s? "
                   incident-id))
      (let ((tcp-proc (skg-tcp-connect-to-rust)))
        (skg-register-response-handler
         'reload-recovery
         (lambda (_tcp-proc payload)
           (let* ((response (read payload))
                  (status (format "%s"
                                  (cadr (assoc 'terminal-status response))))
                  (content (cadr (assoc 'content response)))
                  (successor-required
                   (equal (format "%s"
                                  (cadr (assoc 'successor-required response)))
                          "true")))
             (if (equal status "complete")
                 (progn
                   (setq skg--pending-recovery-incidents
                         (cl-remove-if
                          (lambda (entry)
                            (equal
                             (format "%s"
                                     (cadr (assoc 'incident-id entry)))
                             incident-id))
                          skg--pending-recovery-incidents))
                   (skg-big-nonfatal-message
                    "*SKG Reload Recovery*"
                    (or (and content (format "%s" content))
                        "Fatal reload recovery complete")
                    (skg--recovery-report response)))
               (skg-big-nonfatal-message
                "*SKG Reload Recovery Failed*"
                "WARNING: Fatal reload recovery did not complete"
                (format "* Recovery stopped\n%s\n\nThe incident journal remains available.%s"
                        (or content "Unknown recovery error")
                        (if successor-required
                            "\nSkg queued a new exact sweep to classify the changed bytes as a successor incident."
                          "")))
               (when successor-required
                 (skg--request-reload-full-sweep)))))
         t)
        (skg-submit-request
         tcp-proc
         "((request . \"reload recover\") (approved . \"true\"))\n"
         nil incident-id)))))

(defun skg-dismiss-reload-recovery-incident (&optional incident-id)
  "Permanently discard automatic recovery evidence for INCIDENT-ID.
This does not repair or modify any source file or Git repository."
  (interactive)
  (let* ((ids (mapcar (lambda (entry)
                        (format "%s" (cadr (assoc 'incident-id entry))))
                      skg--pending-recovery-incidents))
         (incident-id
          (or incident-id
              (and ids (completing-read
                        "Dismiss fatal reload incident: " ids nil t))
              (user-error "Skg knows of no unresolved recovery incident"))))
    (when (yes-or-no-p
           (format "Delete recovery evidence for %s? Automatic recovery will become impossible. "
                   incident-id))
      (let ((tcp-proc (skg-tcp-connect-to-rust)))
        (skg-register-response-handler
         'reload-recovery
         (lambda (_tcp-proc payload)
           (let* ((response (read payload))
                  (status (format "%s"
                                  (cadr (assoc 'terminal-status response))))
                  (content (cadr (assoc 'content response))))
             (when (equal status "complete")
               (setq skg--pending-recovery-incidents
                     (cl-remove-if
                      (lambda (entry)
                        (equal
                         (format "%s" (cadr (assoc 'incident-id entry)))
                         incident-id))
                      skg--pending-recovery-incidents)))
             (message "%s" (or content "Recovery dismissal failed"))))
         t)
        (skg-submit-request
         tcp-proc
         "((request . \"reload recover\") (action . \"dismiss\") (approved . \"true\"))\n"
         nil incident-id)))))

(defun skg-install-pending-recovery-incidents (response)
  "Install and visibly report unresolved incidents from handshake RESPONSE."
  (setq skg--pending-recovery-incidents
        (cadr (assoc 'pending-recovery-incidents response)))
  (when skg--pending-recovery-incidents
    (skg-big-nonfatal-message
     "*SKG Pending Reload Recovery*"
     (format "WARNING: %d fatal reload recovery incident(s) remain unresolved"
             (length skg--pending-recovery-incidents))
     (concat
      "* Unresolved fatal reload incidents\n"
      (mapconcat
       (lambda (incident)
         (concat
          (format "** %s\n" (cadr (assoc 'incident-id incident)))
          (mapconcat
           (lambda (fatal)
             (format "*** %s\n%s"
                     (cadr (assoc 'pid fatal))
                     (cadr (assoc 'reason fatal))))
           (cadr (assoc 'fatal incident)) "\n")))
       skg--pending-recovery-incidents "\n")
      "\n** what to do\nRun M-x skg-recover-reload-incident to inspect and explicitly confirm recovery. Skg will not recover automatically. If you accept losing automatic recovery, M-x skg-dismiss-reload-recovery-incident deletes its private journal without changing sources or Git.\n"))))

(defun skg--conflict-review-buffer
    (name text origin role continuation-id)
  "Create one registered read-only ROLE buffer for ORIGIN's conflict."
  (let ((buffer (skg-acquire-generated-buffer name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (or text ""))
        (skg--org-mode-with-options)
        (set-buffer-modified-p nil)
        (setq buffer-read-only t))
      (skg-register-buffer
       buffer 'disk-conflict
       :lifecycle 'attached-workflow :disposable nil
       :continuation-id continuation-id :origin-buffer origin
       :origin-location (format "((role %s))" role)
       :recipe `((kind . "disk-conflict") (role . ,role))
       :last-fetched (skg-buffer-raw-text buffer)))
    buffer))

(defun skg-resolve-disk-client-conflict (&optional accept-current-text)
  "Review this view's disk conflict, or explicitly save a finished merge.
Without a prefix argument, compare the editable local view with read-only
incoming and base buffers in Ediff.  Edit the original local view until it is
correct.  Then invoke this command with a prefix argument to confirm that the
current text is the intended merge and send it through the normal save
pipeline.  The conflict marker clears only after that save succeeds."
  (interactive "P")
  (unless skg--disk-client-conflict
    (user-error "This buffer has no unresolved disk-client conflict"))
  (if accept-current-text
      (when (yes-or-no-p
             "Save the current buffer as the manually reconciled result? ")
        (setq skg--disk-conflict-resolution-in-progress t)
        (condition-case err
            (skg-request-save-buffer)
          (error
           (setq skg--disk-conflict-resolution-in-progress nil)
           (signal (car err) (cdr err)))))
    (let* ((origin (current-buffer))
           (suffix (buffer-name origin))
           (incoming (alist-get 'incoming skg--disk-client-conflict))
           (base (alist-get 'base skg--disk-client-conflict))
           (continuation-id (org-id-uuid)))
      (unless incoming
        (user-error "The incoming rendering was withheld or failed; retry the reload first"))
      (let ((incoming-buffer
             (skg--conflict-review-buffer
              (format "*SKG incoming: %s*" suffix) incoming origin
              "incoming" continuation-id))
            (base-buffer
             (skg--conflict-review-buffer
              (format "*SKG base: %s*" suffix) base origin
              "base" continuation-id)))
        (require 'ediff)
        (setf (alist-get 'review-buffers skg--disk-client-conflict)
              (list incoming-buffer base-buffer))
        (ediff-buffers3
         origin
         incoming-buffer
         base-buffer
         (list
          (lambda ()
            (let ((control (current-buffer)))
              (with-current-buffer origin
                (setf (alist-get 'ediff-control skg--disk-client-conflict)
                      control)))
            (add-hook
             'ediff-after-quit-hook-internal
             (lambda ()
               (dolist (buffer (list incoming-buffer base-buffer))
                 (when (buffer-live-p buffer)
                   (with-current-buffer buffer
                     (setq buffer-read-only nil)
                     (set-buffer-modified-p nil))
                   (kill-buffer buffer))))
             nil t))))
        (message
         "Edit the original local view; when satisfied use C-u M-x skg-resolve-disk-client-conflict")))))

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

(defun skg--reload-selection-keep-transient-clean (&rest _)
  "Keep the reload selector out of dirty-work recovery.
Its marks are transient command input, never saveable authored state."
  (set-buffer-modified-p nil))

(defun skg--reload-selection-todo-sequence (_sequence)
  "Replace Org's ordinary TODO sequence inside a reload selector."
  '(sequence "TO-RELOAD" "|"))

(defun skg-reload-from-id-stack ()
  "Open a transient ID-stack copy whose marked nodes will be reloaded.
Use Org's standard S-left/S-right TODO cycling to mark `TO-RELOAD',
then C-c C-c to submit.  This never edits `skg-id-stack'."
  (interactive)
  (let ((buffer
         (skg-acquire-generated-buffer skg--reload-selection-buffer-name)))
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
      (set-buffer-modified-p nil)
      (add-hook 'after-change-functions
                #'skg--reload-selection-keep-transient-clean nil t)
      (skg-register-buffer
       buffer 'reload-selector
       :lifecycle 'maintenance-control :disposable nil
       :continuation-id (org-id-uuid)
       :recipe '((kind . "reload-selector"))
       :last-fetched (skg-buffer-raw-text buffer)))
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
      (when (skg--confirm-explicit-reload-with-dirty-views)
        (skg-begin-maintenance
         "explicit-partial-reload" nil nil ids
         (lambda (response)
           (when (buffer-live-p selection-buffer)
             (with-current-buffer selection-buffer
               (skg--apply-reload-selection-result response)))))))))

(defun skg--confirm-explicit-reload-with-dirty-views ()
  "Obtain the one up-front authorization required for dirty rendered views."
  (let ((dirty (skg--dirty-view-buffers)))
    (or (null dirty)
        (yes-or-no-p
         (format
          (concat "Archive %d dirty Skg view(s) before the partial reload? "
                  "Impacted views will become detached recovery buffers. ")
          (length dirty))))))

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
  "Observe both worktree bytes and HEAD/index presentation after Magit."
  (condition-case err
      (progn
        (skg--request-reload-full-sweep)
        (let ((tcp-proc (skg-tcp-connect-to-rust)))
          (skg-register-response-handler
           'presentation-observed
           (lambda (_tcp-proc payload)
             (when-let ((content (cadr (assoc 'content (read payload)))))
               (skg-log 'debug 'reload "%s" content)))
           t)
          (skg-submit-request
           tcp-proc "((request . \"observe presentation\"))\n")))
    (error
     (skg-log 'error 'reload "magit refresh observation: %s"
              (error-message-string err)))))

(defun skg-reload-changed ()
  "Compare the complete disk manifest and reload exact changed bytes."
  (interactive)
  (skg--request-reload-full-sweep)
  (message "skg: queued a complete .skg manifest comparison"))

(defun skg--reconciliation-ready-handler (_tcp-proc payload)
  "Report the process-owned exact sweep queued at batch close."
  (let* ((response (read payload))
         (generation (cadr (assoc 'sweep-generation response))))
    (skg-log 'info 'reload
             "server queued post-batch full sweep generation %s"
             generation)))

(skg-register-server-push-handler
 'reconciliation-ready #'skg--reconciliation-ready-handler)

;; Global hook: fires on every magit refresh regardless of any minor
;; mode. Mirrors the top-level find-file-hook registration style.
(add-hook 'magit-post-refresh-hook #'skg--reload-on-magit-refresh)

(provide 'skg-request-reload-paths)
