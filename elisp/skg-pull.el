;;; skg-pull.el --- Client-owned pull maintenance origin -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'comint)
(require 'subr-x)
(require 'vc-git)
(require 'skg-buffer-registry)
(require 'skg-config)
(require 'skg-maintenance)
(require 'skg-state)

(defun skg--pull-process-live-p (process)
  "Return non-nil when PROCESS is a live Emacs subprocess."
  (and (processp process) (process-live-p process)))

(defun skg--pull-local-repositories ()
  "Return a deterministic local repository plan for every configured source."
  (unless skg--server-source-inventory
    (user-error "Skg has no verified server source inventory"))
  (let* ((config-file (or (skg-config-file)
                          (user-error "Skg has no local skgconfig.toml")))
         (local-sources (skg-source-paths-from-toml config-file))
         (server-names (sort (mapcar (lambda (entry)
                                      (plist-get entry :name))
                                    skg--server-source-inventory)
                             #'string<))
         (local-names (sort (mapcar #'car local-sources) #'string<))
         (by-root (make-hash-table :test #'equal)))
    (unless (equal server-names local-names)
      (user-error
       "Local and server source inventories differ: %S versus %S"
       local-names server-names))
    (dolist (source local-sources)
      (let* ((name (car source))
             (path (cdr source)))
        (unless (file-directory-p path)
          (user-error "Configured source %s is not a local directory: %s"
                      name path))
        (let ((root (vc-git-root path)))
          (unless root
            (user-error "Configured source %s is not in a Git worktree: %s"
                        name path))
          (push name (gethash (file-truename root) by-root)))))
    (let (repositories)
      (maphash
       (lambda (root names)
         (setq names (sort names #'string<))
         (push (list :key (substring
                           (secure-hash
                            'sha256 (mapconcat #'identity names (string 0)))
                           0 16)
                     :root (file-name-as-directory root)
                     :sources names)
               repositories))
       by-root)
      (sort repositories
            (lambda (left right)
              (string< (plist-get left :root)
                       (plist-get right :root)))))))

(defun skg--pull-dirty-buffers ()
  (cl-remove-if-not #'skg-buffer-dirty-p (skg-registered-buffers)))

(defun skg--pull-buffer-description (buffer)
  (with-current-buffer buffer
    (format "%s [%s]" (buffer-name)
            (if skg--buffer-record
                (skg--buffer-record-kind skg--buffer-record)
              'unregistered))))

(defun skg--pull-confirm-dirty-work (dirty)
  (or (null dirty)
      (yes-or-no-p
       (format
        (concat "Pull will archive these dirty Skg buffers: %s. "
                "Impacted buffers will become detached recovery buffers; "
                "orthogonal buffers will remain editable afterward. Continue? ")
        (mapconcat #'skg--pull-buffer-description dirty ", ")))))

(defun skg-pull-all ()
  "Pull every configured repository through one durable maintenance incident."
  (interactive)
  (when skg--maintenance-client-incident
    (user-error "Maintenance is already active"))
  (unless (equal skg--active-source-set-name "all")
    (user-error
     "Pull requires source-set `all'; switch from `%s' before pulling"
     skg--active-source-set-name))
  (let* ((repositories (skg--pull-local-repositories))
         (dirty (skg--pull-dirty-buffers))
         (dirty-raw
          (cl-remove-if-not
           (lambda (buffer)
             (with-current-buffer buffer
               (and skg--buffer-record
                    (eq (skg--buffer-record-kind skg--buffer-record)
                        'raw-skg-file))))
           dirty)))
    (when dirty-raw
      (user-error "Pull refuses modified raw .skg buffers: %s"
                  (mapconcat #'buffer-name dirty-raw ", ")))
    (unless repositories
      (user-error "No configured Git repositories can be pulled"))
    (when (skg--pull-confirm-dirty-work dirty)
      (skg-begin-maintenance
       "pull" nil nil nil #'skg--pull-terminal
       (list :repositories repositories
             :remaining nil
             :current-process nil
             :advance-timer nil
             :started nil
             :details nil
             :failures nil
             :external-result nil
             :diagnostic-buffer nil)))))

(defun skg--pull-context ()
  "Return or create the active pull's process-local context."
  (let ((state skg--maintenance-client-incident))
    (unless (and state (equal (plist-get state :origin) "pull"))
      (error "No client-owned pull incident is active"))
    (or (plist-get state :origin-context)
        (let ((context (list :repositories nil :remaining nil
                             :current-process nil :advance-timer nil
                             :started nil
                             :details nil :failures nil
                             :external-result nil :diagnostic-buffer nil)))
          (setf (plist-get state :origin-context) context)
          context))))

(defun skg--pull-diagnostic-buffer (context)
  (or (and (buffer-live-p (plist-get context :diagnostic-buffer))
           (plist-get context :diagnostic-buffer))
      (let* ((incident (plist-get skg--maintenance-client-incident
                                  :incident-id))
             (short (substring incident 0 (min 8 (length incident))))
             (buffer (generate-new-buffer
                      (format "*Skg Pull %s*" short))))
        (with-current-buffer buffer
          (comint-mode)
          (let ((inhibit-read-only t))
            (insert (format "Skg pull maintenance incident %s\n\n" incident))))
        (setf (plist-get context :diagnostic-buffer) buffer)
        buffer)))

(defun skg--pull-request-authorization ()
  "Ask the server to journal the pull point of no return."
  (let* ((state skg--maintenance-client-incident)
         (incident (plist-get state :incident-id))
         (epoch (plist-get state :epoch))
         (tcp-proc (skg-tcp-connect-to-rust)))
    (setf (plist-get state :phase) 'origin-operation-start-pending)
    (skg-register-response-handler
     'maintenance-status #'skg--pull-handle-authorization t)
    (skg-set-request-failure-handler
     (lambda (reason)
       (when skg--maintenance-client-incident
         (setf (plist-get skg--maintenance-client-incident :phase)
               'origin-operation-start-pending))
       (display-warning
        'skg (format "Pull authorization was not delivered: %s" reason)
        :warning)))
    (skg-submit-request
     tcp-proc
     (concat
      (prin1-to-string
       `((request . "run maintenance origin")
         (maintenance-epoch . ,epoch)))
      "\n")
     nil incident)))

(defun skg--pull-handle-authorization (_tcp-proc payload)
  (let* ((response (read payload))
         (state skg--maintenance-client-incident)
         (context (skg--pull-context))
         (incident (skg--maintenance-text response 'incident-id))
         (epoch (skg--maintenance-field response 'maintenance-epoch)))
    (skg--maintenance-require-client-incident incident epoch)
    (unless (and (equal (skg--maintenance-text response 'status)
                        "external-mutation-authorized")
                 (equal (skg--maintenance-text response 'phase)
                        "running-external-mutation"))
      (error "Server did not authorize the client-owned pull"))
    (setf (plist-get state :phase) 'running-external-mutation)
    (if (skg--maintenance-true-p response 'replayed)
        (skg--pull-resume-running
         "authorization reply was replayed without a live owned Git child")
      (setf (plist-get context :started) t
            (plist-get context :remaining)
            (copy-sequence (plist-get context :repositories)))
      (skg--pull-start-next))))

(defun skg--pull-run-scheduled-next ()
  "Clear the active pull's scheduling marker and advance its process chain."
  (let ((context (skg--pull-context)))
    (setf (plist-get context :advance-timer) nil)
    (skg--pull-start-next)))

(defun skg--pull-schedule-next ()
  "Schedule exactly one continuation of the active pull process chain."
  (let ((context (skg--pull-context)))
    (unless (plist-get context :advance-timer)
      (setf (plist-get context :advance-timer)
            (run-at-time 0 nil #'skg--pull-run-scheduled-next)))))

(defun skg--pull-start-next ()
  "Start the next repository process, or report the complete chain."
  (let* ((context (skg--pull-context))
         (remaining (plist-get context :remaining)))
    (if (null remaining)
        (skg--pull-finish-origin
         (if (plist-get context :failures) "failed" "completed")
         (nreverse (copy-sequence (plist-get context :details))))
      (let* ((repository (car remaining))
             (root (plist-get repository :root))
             (key (plist-get repository :key))
             (buffer (skg--pull-diagnostic-buffer context)))
        (setf (plist-get context :remaining) (cdr remaining))
        (with-current-buffer buffer
          (let ((inhibit-read-only t))
            (goto-char (point-max))
            (insert (format "\n$ git -C %s pull\n" root))))
        (condition-case error-data
            (let ((process
                   (make-process
                    :name (format "skg-pull-%s" key)
                    :buffer buffer
                    :command (list "git" "-C" root "pull")
                    :connection-type 'pty
                    :coding 'utf-8-unix
                    :noquery nil
                    :sentinel #'skg--pull-process-sentinel)))
              (process-put process 'skg-pull-root root)
              (process-put process 'skg-pull-key key)
              (setf (plist-get context :current-process) process)
              (with-current-buffer buffer
                (set-marker (process-mark process) (point-max)))
              (display-buffer buffer))
          (error
           (let ((detail (format "%s could not start: %s"
                                 root (error-message-string error-data))))
             (push detail (plist-get context :details))
             (push detail (plist-get context :failures))
             (skg--pull-schedule-next))))))))

(defun skg--pull-process-sentinel (process event)
  "Advance the serial pull chain after PROCESS reaches a terminal EVENT."
  (when (and (memq (process-status process) '(exit signal failed))
             (not (process-get process 'skg-pull-finalized)))
    (process-put process 'skg-pull-finalized t)
    (let* ((context (skg--pull-context))
           (root (process-get process 'skg-pull-root))
           (success (and (eq (process-status process) 'exit)
                         (= (process-exit-status process) 0)))
           (detail (format "%s: %s" root (string-trim event))))
      (push detail (plist-get context :details))
      (unless success (push detail (plist-get context :failures)))
      (setf (plist-get context :current-process) nil)
      (when-let ((buffer (process-buffer process)))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (let ((inhibit-read-only t))
              (goto-char (point-max))
              (insert (format "[%s]\n" (string-trim event)))))))
      (skg--pull-schedule-next))))

(defun skg--pull-finish-origin (outcome details)
  "Report pull OUTCOME and diagnostic DETAILS, then observe exact final disk."
  (let* ((state skg--maintenance-client-incident)
         (context (skg--pull-context))
         (record (list :outcome outcome :details details)))
    (setf (plist-get context :external-result) record
          (plist-get context :advance-timer) nil
          (plist-get state :phase) 'origin-completion-pending)
    (skg--pull-send-finish record)))

(defun skg--pull-send-finish (record)
  (let* ((state skg--maintenance-client-incident)
         (incident (plist-get state :incident-id))
         (epoch (plist-get state :epoch))
         (tcp-proc (skg-tcp-connect-to-rust)))
    (skg-register-response-handler
     'maintenance-status #'skg--pull-handle-finish t)
    (skg-set-request-failure-handler
     (lambda (reason)
       (when skg--maintenance-client-incident
         (setf (plist-get skg--maintenance-client-incident :phase)
               'origin-completion-pending))
       (display-warning
        'skg (format "Pull completion was not delivered: %s" reason)
        :warning)))
    (skg-submit-request
     tcp-proc
     (concat
      (prin1-to-string
       `((request . "finish maintenance origin")
         (maintenance-epoch . ,epoch)
         (external-outcome . ,(plist-get record :outcome))
         (external-details ,@(plist-get record :details))))
      "\n")
     nil incident)))

(defun skg--pull-handle-finish (_tcp-proc payload)
  (let* ((response (read payload))
         (incident (skg--maintenance-text response 'incident-id))
         (epoch (skg--maintenance-field response 'maintenance-epoch)))
    (skg--maintenance-require-client-incident incident epoch)
    (unless (and (equal (skg--maintenance-text response 'status)
                        "origin-operation-finished")
                 (equal (skg--maintenance-text response 'phase)
                        "final-observation"))
      (error "Server did not begin exact post-pull observation"))
    (setf (plist-get skg--maintenance-client-incident :phase)
          'waiting-for-origin-observation)
    (message "Skg is observing exact disk after pull")))

(defun skg--pull-resume-archive-ready ()
  "Resolve any lost local pull plan before crossing the point of no return."
  (condition-case error-data
      (let ((context (skg--pull-context)))
        (unless (plist-get context :repositories)
          (let ((repositories (skg--pull-local-repositories)))
            (unless repositories
              (user-error "No configured Git repositories can be pulled"))
            (setf (plist-get context :repositories) repositories)))
        (skg--pull-request-authorization))
    (error
     (display-warning
      'skg
      (format "Pull remains archive-ready: %s"
              (error-message-string error-data))
      :error))))

(defun skg--pull-resume-running (lost-child-reason)
  "Resume a pull at its external boundary, using LOST-CHILD-REASON if needed."
  (let* ((context (skg--pull-context))
         (process (plist-get context :current-process))
         (record (plist-get context :external-result)))
    (cond
     ((skg--pull-process-live-p process) t)
     (record
      (run-at-time 0 nil #'skg--pull-send-finish record))
     ((plist-get context :advance-timer) t)
     (t
      (run-at-time
       0 nil #'skg--pull-finish-origin "indeterminate"
       (list lost-child-reason))))))

(defun skg--pull-install-server-result (response)
  "Retain the exact external mutation result journaled in RESPONSE."
  (when (assoc 'external-outcome response)
    (let* ((outcome (skg--maintenance-text response 'external-outcome))
           (details (skg--maintenance-string-list response 'external-details))
           (context (skg--pull-context))
           (record (list :outcome outcome :details details))
           (prior (plist-get context :external-result)))
      (unless (member outcome '("completed" "failed" "indeterminate"))
        (error "Server returned an invalid pull outcome: %S" outcome))
      (when (and prior (not (equal prior record)))
        (error "Server changed the journaled pull outcome"))
      (setf (plist-get context :external-result) record))))

(defun skg--pull-origin-handler (phase response)
  "Resume the client-owned pull from durable server PHASE."
  (skg--pull-context)
  (skg--pull-install-server-result response)
  (pcase phase
    ("archive-ready"
     (run-at-time 0 nil #'skg--pull-resume-archive-ready))
    ("running-external-mutation"
     (skg--pull-resume-running
      "server awaited pull completion but no owned Git child survived"))
    ("final-observation"
     (when-let ((record
                 (plist-get (skg--pull-context) :external-result)))
       (run-at-time 0 nil #'skg--pull-send-finish record)))
    (_ (cl-return-from skg--pull-origin-handler nil)))
  t)

(defun skg--pull-terminal (_response)
  (let* ((context (and skg--maintenance-client-incident
                       (plist-get skg--maintenance-client-incident
                                  :origin-context)))
         (buffer (and context (plist-get context :diagnostic-buffer))))
    (if (buffer-live-p buffer)
        (message "Skg pull maintenance completed; diagnostics remain in %s"
                 (buffer-name buffer))
      (message "Skg pull maintenance completed"))))

(skg-register-maintenance-origin-handler "pull" #'skg--pull-origin-handler)

(provide 'skg-pull)
