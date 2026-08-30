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

(require 'cl-lib)
(require 'skg-length-prefix)
(require 'skg-config)
(require 'skg-id-search)
(require 'skg-request-save) ; for skg--collateral-view-handler
(require 'skg-worktree-guard)
(require 'filenotify)

;;; ---- change observation -------------------------------------------

(defvar skg--reload-observation-paths (make-hash-table :test 'equal)
  "Candidate paths mapped to their newest client observation sequence.")
(defvar skg--reload-observation-sequence 0)
(defvar skg--reload-observation-incident-id nil)

(defvar skg--pending-recovery-incidents nil
  "Fatal reload incidents whose durable recovery journals remain unresolved.")
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
           (skg-log 'error 'reload "observation dispatch failed: %S" err)
           (skg--schedule-reload-observation 0.5)))))))

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
     ((memq status '(failed needs-authorization))
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
    (paths &optional ids incident-id terminal-callback full-sweep
           scalar-approved-pids)
  "Ask the server to reload the telescopes owning PATHS or IDS.
PATHS are absolute .skg paths.  IDS may contain primary or extra IDs.
INCIDENT-ID identifies retries of one reconciliation episode.  Invoke
TERMINAL-CALLBACK with the parsed terminal response, when non-nil."
  (when (or paths ids full-sweep)
    (when skg--active-request-id
      (user-error "skg: reload is waiting for the active request to finish"))
    (let* ((tcp-proc (skg-tcp-connect-to-rust))
           (dirty-view-uris
            (delq nil
                  (mapcar (lambda (buffer)
                            (buffer-local-value 'skg-view-uri buffer))
                          (skg--dirty-view-buffers))))
           (request-sexp
            (concat (prin1-to-string
                     (append
                      `((request . "reload paths")
                        (paths ,@paths)
                        (ids ,@ids)
                        (dirty-view-uris ,@dirty-view-uris)
                        (full-sweep . ,(if full-sweep "true" "false")))
                      (when scalar-approved-pids
                        `((allow-ugly-telescopes
                           ,@scalar-approved-pids)))))
                    "\n"))
           (incident-id (or incident-id (skg-fresh-incident-id))))
      (condition-case err
          (progn
            (skg--begin-stream "reload")
            (skg--lock-all-skg-buffers)
            (skg-register-response-handler ; refresh safe touched buffers
             'collateral-view
             (lambda (_tcp-proc payload)
               (skg--collateral-view-handler payload))
             nil) ; non-one-shot
            (skg-register-response-handler
             'ugly-telescope-confirmation
             (lambda (_tcp-proc payload)
               (skg--reload-scalar-confirmation-handler
                payload paths ids incident-id terminal-callback full-sweep))
             nil)
            (skg-register-response-handler
             'reload-paths
             (lambda (_tcp-proc payload)
               (skg--reload-terminal-handler payload terminal-callback))
             t) ; one-shot
            (skg-submit-request tcp-proc request-sexp nil incident-id))
        (error
         (skg--end-stream)
         (skg--unlock-all-save-locked)
         (signal (car err) (cdr err)))))))

(defun skg--reload-scalar-confirmation-handler
    (payload paths ids incident-id terminal-callback full-sweep)
  "Handle a text-free reload presentation challenge."
  (skg--end-stream)
  (skg--unlock-all-save-locked)
  (let* ((response (read payload))
         (pids (mapcar (lambda (pid) (format "%s" pid))
                       (cadr (assoc 'pids response))))
         (prompt (or (cadr (assoc 'prompt response))
                     "Display protected reload text? "))
         (approved (and (not noninteractive)
                        (y-or-n-p (concat (format "%s" prompt) " ")))))
    (if approved
        ;; The request coordinator finishes the challenged request after this
        ;; handler returns.  Build the retry on the next event-loop turn so its
        ;; handlers belong to the new request record, while preserving incident.
        (run-at-time
         0 nil
         (lambda ()
           (skg-reload-paths
            paths ids incident-id terminal-callback full-sweep pids)))
      (when terminal-callback
        (funcall terminal-callback response)))))

(defun skg--reload-terminal-handler (payload terminal-callback)
  "Unlock the reload request, show conflicts and pass its terminal result on."
  (skg--end-stream)
  (skg--unlock-all-save-locked)
  (let* ((response (read payload))
         (content (cadr (assoc 'content response)))
         (incident (cadr (assoc 'incident-id response)))
         (warnings (cadr (assoc 'warnings response)))
         (recovery-available
          (equal (format "%s" (cadr (assoc 'recovery-available response)))
                 "true")))
    (skg--handle-reload-conflicts response)
    (when warnings
      (skg-big-nonfatal-message
       "*SKG Reload Warnings*"
       (format "WARNING: Reload completed with %d warning(s)"
               (length warnings))
       (skg-errors-and-warnings-to-org-string nil warnings)))
    (when content (message "%s" content))
    (when terminal-callback
      (funcall terminal-callback response))
    (when (and recovery-available incident)
      (setq skg--pending-recovery-incidents
            (cons `((incident-id . ,(format "%s" incident))
                    (fatal . ,(cadr (assoc 'requested-id-outcomes response))))
                  (cl-remove-if
                   (lambda (entry)
                     (equal (format "%s" (cadr (assoc 'incident-id entry)))
                            (format "%s" incident)))
                   skg--pending-recovery-incidents)))
      (unless noninteractive
        (run-at-time 0 nil #'skg-recover-reload-incident
                     (format "%s" incident))))))

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
                  (content (cadr (assoc 'content response))))
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
                (format "* Recovery stopped\n%s\n\nThe incident journal remains available."
                        (or content "Unknown recovery error"))))))
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

(defun skg--reload-impact-paths (impact)
  "Return IMPACT's path values as strings."
  (mapcar (lambda (path) (format "%s" path))
          (cadr (assoc 'paths impact))))

(defun skg--reload-impact-buffer (impact)
  "Return the live buffer named by IMPACT, if it remains open."
  (let ((uri (cadr (assoc 'view-uri impact))))
    (and uri (skg-find-buffer-by-uri (format "%s" uri)))))

(defun skg--conflict-heading-text (value)
  "Make VALUE safe as one line of an Org heading."
  (replace-regexp-in-string "[\n\r]+" " " (format "%s" value)))

(defun skg--insert-reload-impact-section (title impacts &optional introduction)
  "Insert an Org section TITLE describing IMPACTS, after INTRODUCTION."
  (insert "** " title "\n")
  (when introduction (insert introduction "\n"))
  (if (null impacts)
      (insert "None.\n")
    (dolist (impact impacts)
      (let* ((buffer (skg--reload-impact-buffer impact))
             (uri (cadr (assoc 'view-uri impact)))
             (name (if buffer (buffer-name buffer) (format "%s" uri))))
        (insert "*** " (skg--conflict-heading-text name) "\n"
                "Affected by these changed nodes:\n")
        (dolist (path (skg--reload-impact-paths impact))
          (insert "**** " (skg--conflict-heading-text path) "\n"))))))

(defun skg--reload-conflict-report (conflicted updated files)
  "Return the persistent Org report for a reload conflict."
  (with-temp-buffer
    (insert
     "* WARNING: Disk-client conflict(s)\n"
     "Skg detected changes to the .skg filestore made outside of Skg.\n")
    (skg--insert-reload-impact-section
     "buffers with unsaved changes that cannot be automatically updated"
     conflicted
     "Other dirty Skg buffers, if any, are unaffected and hence omitted here.")
    (skg--insert-reload-impact-section "buffers that have been updated" updated)
    (let ((path-buffers (make-hash-table :test #'equal)))
      (dolist (impact conflicted)
        (let* ((buffer (skg--reload-impact-buffer impact))
               (uri (cadr (assoc 'view-uri impact)))
               (name (if buffer (buffer-name buffer) (format "%s" uri))))
          (dolist (path (skg--reload-impact-paths impact))
            (puthash path (cons name (gethash path path-buffers))
                     path-buffers))))
      (insert "** changed nodes affecting more than one conflicted buffer\n")
      (let (shared)
        (maphash (lambda (path buffers)
                   (when (> (length (delete-dups buffers)) 1)
                     (push (cons path (delete-dups buffers)) shared)))
                 path-buffers)
        (if (null shared)
            (insert "None.\n")
          (dolist (entry (sort shared (lambda (a b) (string< (car a) (car b)))))
            (insert "*** " (skg--conflict-heading-text (car entry)) "\n")
            (dolist (name (sort (cdr entry) #'string<))
              (insert "**** " (skg--conflict-heading-text name) "\n"))))))
    (insert "** files affected\n")
    (if files
        (dolist (path files)
          (insert "*** " (skg--conflict-heading-text path) "\n"))
      (insert "None reported.\n"))
    (insert
     "** what it means, and what to do about it\n"
     "Skg applied the disk changes to its graph, databases and the buffers where doing so was safe.  It did not update the conflicted buffers above because they contain unsaved edits.  Saving them normally could clobber the external changes, so ordinary save is blocked.  Resolve each with M-x skg-resolve-disk-client-conflict, reviewing base, local and incoming text.  After one resolution, refresh the incoming side of the others before resolving them.  Skg cannot infer why the out-of-band changes happened or safely choose a merge; review and repair them manually with caution.\n")
    (buffer-string)))

(defun skg--handle-reload-conflicts (response)
  "Persist and display the dirty-view conflicts described by RESPONSE."
  (let* ((conflicted (cadr (assoc 'conflicted-views response)))
         (updated (cadr (assoc 'updated-views response)))
         (files (mapcar (lambda (path) (format "%s" path))
                        (cadr (assoc 'files-affected response))))
         (incident (cadr (assoc 'incident-id response))))
    (dolist (impact updated)
      (when-let ((buffer (skg--reload-impact-buffer impact)))
        (with-current-buffer buffer
          (setq skg--disk-client-conflict nil))))
    (dolist (impact conflicted)
      (when-let ((buffer (skg--reload-impact-buffer impact)))
        (with-current-buffer buffer
          (let ((old skg--disk-client-conflict))
            (setq skg--disk-client-conflict
                  `((incident-id . ,(format "%s" incident))
                    (paths . ,(skg--reload-impact-paths impact))
                    (pids . ,(mapcar
                              (lambda (pid) (format "%s" pid))
                              (cadr (assoc 'pids impact))))
                    (base . ,(or (alist-get 'base old)
                                 skg--last-rendered-content
                                 (buffer-string)))
                    (incoming . ,(cadr (assoc 'incoming impact)))
                    (local-token . ,(buffer-chars-modified-tick))))))))
    (when conflicted
      (skg-big-nonfatal-message
       "*SKG Disk-Client Conflicts*"
       "WARNING: Disk-client conflicts require manual resolution"
       (skg--reload-conflict-report conflicted updated files)))))

(defun skg--conflict-review-buffer (name text)
  "Create a read-only Org buffer NAME containing TEXT."
  (let ((buffer (get-buffer-create name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (or text ""))
        (skg--org-mode-with-options)
        (set-buffer-modified-p nil)
        (setq buffer-read-only t)))
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
           (base (alist-get 'base skg--disk-client-conflict)))
      (unless incoming
        (user-error "The incoming rendering was withheld or failed; retry the reload first"))
      (require 'ediff)
      (ediff-buffers3
       origin
       (skg--conflict-review-buffer
        (format "*SKG incoming: %s*" suffix) incoming)
       (skg--conflict-review-buffer
        (format "*SKG base: %s*" suffix) base))
      (message
       "Edit the original local view; when satisfied use C-u M-x skg-resolve-disk-client-conflict"))))

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

(defun skg--reconciliation-ready-handler (_tcp-proc payload)
  "Turn a server-owned end-of-batch notice into a dirty-aware full sweep."
  (let* ((response (read payload))
         (generation (cadr (assoc 'sweep-generation response))))
    (skg-log 'info 'reload
             "server requested post-batch full sweep generation %s"
             generation)
    ;; `skg-reload-paths' takes the current dirty-buffer census immediately
    ;; before dispatch. Thus the shell control connection never needs access
    ;; to client-local buffer state.
    (skg--request-reload-full-sweep)))

(skg-register-server-push-handler
 'reconciliation-ready #'skg--reconciliation-ready-handler)

;; Global hook: fires on every magit refresh regardless of any minor
;; mode. Mirrors the top-level find-file-hook registration style.
(add-hook 'magit-post-refresh-hook #'skg--reload-on-magit-refresh)

(provide 'skg-request-reload-paths)
