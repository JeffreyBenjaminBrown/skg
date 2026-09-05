;;; skg-maintenance.el --- Maintenance epoch/archive protocol -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)
(require 'skg-buffer-registry)
(require 'skg-length-prefix)
(require 'skg-recovery-archive)
(require 'skg-request-verify-connection)
(require 'skg-request-save)
(require 'skg-state)

(defvar skg--maintenance-client-incident nil
  "Client facts for the active server-owned maintenance incident.")
(defvar skg--pending-maintenance-offer nil
  "Latest unsolicited valid disk candidate offered by the server.")
(defvar skg--maintenance-origin-operation-handlers nil
  "Origin labels mapped to their post-archive client adapters.")
(defvar skg--maintenance-enrollment-census-scheduled nil
  "Non-nil while one constructor-triggered incident census is queued.")

(defun skg-register-maintenance-origin-handler (origin handler)
  "Register HANDLER for maintenance ORIGIN.
HANDLER receives the durable server phase and parsed response, and returns
non-nil when it handled that phase.  Re-registering an origin replaces its
old implementation."
  (setf (alist-get origin skg--maintenance-origin-operation-handlers
                   nil nil #'equal)
        handler))

(defun skg--maintenance-dispatch-origin (phase response)
  "Run the active incident's origin adapter for durable PHASE and RESPONSE."
  (let* ((state skg--maintenance-client-incident)
         (handler (alist-get
                   (plist-get state :origin)
                   skg--maintenance-origin-operation-handlers
                   nil nil #'equal)))
    (and handler (funcall handler phase response))))

(defun skg--maintenance-field (response key)
  (cadr (assoc key response)))

(defun skg--maintenance-text (response key)
  (when-let ((value (skg--maintenance-field response key)))
    (format "%s" value)))

(defun skg--maintenance-string-list (response key)
  (mapcar (lambda (value) (format "%s" value))
          (or (skg--maintenance-field response key) nil)))

(defun skg--maintenance-true-p (response key)
  (equal (skg--maintenance-text response key) "true"))

(defun skg--maintenance-set-handshake-summary (state &optional epoch)
  "Update the compact connection-level maintenance summary."
  (let ((census-required
         (and (listp skg--maintenance-state)
              (cdr (assq 'census-required skg--maintenance-state)))))
    (setq skg--maintenance-state
          `((epoch . ,(or epoch
                          (and (listp skg--maintenance-state)
                               (cdr (assq 'epoch skg--maintenance-state)))))
            (state . ,state)
            (census-required . ,census-required)))))

(defun skg-maintenance-adopt-handshake-epoch ()
  "Install an active handshake epoch on every current registered buffer."
  (when (and (listp skg--maintenance-state)
             (equal (format "%s" (cdr (assq 'state skg--maintenance-state)))
                    "active"))
    (let ((epoch (cdr (assq 'epoch skg--maintenance-state))))
      (unless (natnump epoch)
        (error "Active maintenance handshake has no valid epoch"))
      (when (and skg--maintenance-client-incident
                 (not (equal epoch
                             (plist-get skg--maintenance-client-incident
                                        :epoch))))
        (error "Maintenance handshake changed the active client epoch"))
      (dolist (buffer (skg-registered-buffers))
        (skg-lock-buffer-for-maintenance buffer epoch)))))

(defun skg-maintenance-handle-census-stale (buffer-ids)
  "Detach genuinely stale BUFFER-IDS, preserving active settlement debt."
  (let* ((active (and (listp skg--maintenance-state)
                      (equal (format
                              "%s"
                              (cdr (assq 'state skg--maintenance-state)))
                             "active")))
         (protected (and active skg--maintenance-client-incident
                         (plist-get skg--maintenance-client-incident
                                    :registered-buffer-ids)))
         ordinary)
    (dolist (buffer-id buffer-ids)
      (unless (member (format "%s" buffer-id) protected)
        (push buffer-id ordinary)))
    (skg-mark-census-buffers-stale (nreverse ordinary))))

(defun skg-resume-maintenance-after-census
    (&optional maintenance-incident-id maintenance-epoch)
  "Resume durable maintenance only after reconnect census completes."
  (when maintenance-incident-id
    (skg--maintenance-require-client-incident
     maintenance-incident-id maintenance-epoch))
  (let ((state (and (listp skg--maintenance-state)
                    (format "%s"
                            (cdr (assq 'state skg--maintenance-state))))))
    (when (member state '("active" "terminal"))
      (if (and skg--maintenance-client-incident
               (eq (plist-get skg--maintenance-client-incident :phase)
                   'awaiting-locked-census))
          (skg--maintenance-send-locked-census)
        (skg-maintenance-status t)))))

(defun skg-maintenance-enroll-new-buffer (buffer-id)
  "Queue one priority census when BUFFER-ID was born in active maintenance."
  (let ((state skg--maintenance-client-incident))
    (when (and state
               (not (member buffer-id
                            (plist-get state :registered-buffer-ids)))
               (not skg--maintenance-enrollment-census-scheduled))
      (setq skg--maintenance-enrollment-census-scheduled t)
      (run-at-time
       0 nil
       (lambda (incident-id epoch)
         (setq skg--maintenance-enrollment-census-scheduled nil)
         (when (and skg--maintenance-client-incident
                    (equal incident-id
                           (plist-get skg--maintenance-client-incident
                                      :incident-id))
                    (equal epoch
                           (plist-get skg--maintenance-client-incident :epoch)))
           (skg--submit-buffer-census
            (skg-tcp-connect-to-rust) incident-id epoch)))
       (plist-get state :incident-id)
       (plist-get state :epoch)))))

(defun skg--maintenance-refresh-presentation-buffer-ids (response)
  "Install RESPONSE's monotonically growing presentation buffer inventory."
  (when (assoc 'presentation-buffer-ids response)
    (let* ((state skg--maintenance-client-incident)
           (old (mapcar (lambda (id) (format "%s" id))
                        (or (plist-get state :registered-buffer-ids) nil)))
           (new (skg--maintenance-string-list
                 response 'presentation-buffer-ids)))
      (unless (cl-every (lambda (id) (member id new)) old)
        (error "Maintenance presentation census lost a registered buffer"))
      (setf (plist-get state :registered-buffer-ids) new))))

(defun skg--maintenance-send-locked-census ()
  "Ask the server to freeze the just-completed epoch-locked census."
  (let* ((state skg--maintenance-client-incident)
         (incident-id (plist-get state :incident-id))
         (epoch (plist-get state :epoch))
         (tcp-proc (skg-tcp-connect-to-rust)))
    (skg-submit-priority-request
     tcp-proc
     (concat
      (prin1-to-string
       `((request . "maintenance locked census")
         (maintenance-epoch . ,epoch)))
      "\n")
     `((maintenance-offer ,#'skg--maintenance-handle-bootstrap . t)
       (error
        ,(lambda (_tcp payload)
           (let ((reason (skg--connection-handshake-error-content payload)))
             (when skg--maintenance-client-incident
               (setf (plist-get skg--maintenance-client-incident :phase)
                     'cancelling-after-locked-census-refusal))
             (display-warning
              'skg
              (format
               (concat "Maintenance stopped before archive publication: %s. "
                       "The safe pre-archive incident is being cancelled.")
               reason)
              :error)
             (run-at-time
              0 nil #'skg-cancel-maintenance incident-id epoch)))
        . t))
     nil incident-id)))

(defun skg--maintenance-registered-ids ()
  (sort (mapcar (lambda (buffer)
                  (with-current-buffer buffer
                    (skg--buffer-record-id skg--buffer-record)))
                (skg-registered-buffers))
        #'string<))

(defun skg--maintenance-modified-raw-file-buffers ()
  "Return configured raw file buffers which have unsaved editor text."
  (cl-remove-if-not
   (lambda (buffer)
     (with-current-buffer buffer
       (and skg--buffer-record
            (eq (skg--buffer-record-kind skg--buffer-record) 'raw-skg-file)
            (buffer-modified-p buffer))))
   (skg-registered-buffers)))

(defun skg--maintenance-refuse-modified-raw-files ()
  "Refuse maintenance before allocating an incident if raw files are dirty."
  (when-let ((dirty (skg--maintenance-modified-raw-file-buffers)))
    (user-error
     "Maintenance refuses modified raw .skg buffers: %s"
     (mapconcat #'buffer-name dirty ", "))))

(defun skg--maintenance-lock-census-sha256 (ids)
  (secure-hash
   'sha256
   (skg--utf8-unix-bytes
    (if ids (concat (mapconcat #'identity (sort (copy-sequence ids) #'string<)
                               (string 0))
                    (string 0))
      ""))))

(defun skg--maintenance-offer-for-writer (response)
  `((incident-id ,(skg--maintenance-text response 'allocated-incident-id))
    (epoch ,(skg--maintenance-field response 'maintenance-epoch))
    (origin ,(skg--maintenance-text response 'origin))
    (started-at-utc ,(skg--maintenance-text response 'started-at-utc))
    (archive-name ,(skg--maintenance-text response
                                          'archive-directory-name))
    (archive-folder ,(skg--maintenance-text
                      response 'maintenance-archive-folder))
    (archive-identity ,(skg--maintenance-text
                        response 'maintenance-archive-identity))
    (source-set ,(skg--maintenance-text response 'source-set))
    (graph-generation ,(skg--maintenance-field response
                                               'g0-graph-generation))
    (manifest-revision ,(skg--maintenance-field response
                                                'g0-manifest-revision))))

(defun skg--maintenance-lock-offer (response)
  "Validate RESPONSE's census and install its epoch on every Skg buffer."
  (let* ((offered (sort (skg--maintenance-string-list
                         response 'registered-buffer-ids) #'string<))
         (actual (skg--maintenance-registered-ids))
         (claimed-sha (skg--maintenance-text response
                                             'lock-census-sha256))
         (actual-sha (skg--maintenance-lock-census-sha256 actual))
         (epoch (skg--maintenance-field response 'maintenance-epoch)))
    (unless (equal offered actual)
      (error "Maintenance census changed: server froze %S, client has %S"
             offered actual))
    (unless (equal claimed-sha actual-sha)
      (error "Maintenance census checksum does not match"))
    (dolist (buffer (skg-registered-buffers))
      (skg-lock-buffer-for-maintenance buffer epoch))
    actual-sha))

(defun skg--maintenance-publish-initial ()
  "Publish and report the active incident's initial archive."
  (let* ((state skg--maintenance-client-incident)
         (offer (plist-get state :offer))
         (incident-id (plist-get state :incident-id)))
    (condition-case error-data
        (let ((result
               (skg-recovery-archive-publish-initial
                offer
                :archive-root
                (skg-recovery--offer-value offer 'archive-folder)
                :undo-waivers (plist-get state :undo-waivers))))
          (setf (plist-get skg--maintenance-client-incident :archive) result)
          (run-at-time
           0 nil #'skg--maintenance-send-archive-ready
           incident-id
           (plist-get state :epoch)
           (plist-get state :lock-census-sha256)
           (plist-get result :manifest-sha256)))
      (skg-recovery-native-undo-error
       (let ((buffer-key (nth 2 error-data))
             (reason (nth 3 error-data)))
         (run-at-time
          0 nil #'skg--maintenance-send-undo-failure
          incident-id (plist-get state :epoch) buffer-key reason)))
      (error
       (display-warning
        'skg
        (format "Initial recovery archive failed before risky work: %s\nIncomplete staging data was retained."
                (error-message-string error-data))
        :error)
       (run-at-time 0 nil #'skg-cancel-maintenance incident-id
                    (plist-get state :epoch))))))

(defun skg--maintenance-send-archive-ready
    (incident-id epoch lock-sha manifest-sha)
  (let ((tcp-proc (skg-tcp-connect-to-rust)))
    (skg-register-response-handler
     'maintenance-status
     #'skg--maintenance-handle-selection-response
     t)
    (skg-set-request-failure-handler
     (lambda (reason)
       (when skg--maintenance-client-incident
         (setf (plist-get skg--maintenance-client-incident :phase)
               'archive-ready-ack-pending))
       (display-warning
        'skg (format "Initial archive ACK was not delivered: %s" reason)
        :warning)))
    (skg-submit-request
     tcp-proc
     (concat
      (prin1-to-string
       `((request . "maintenance archive ready")
         (maintenance-epoch . ,epoch)
         (lock-census-sha256 . ,lock-sha)
         (manifest-sha256 . ,manifest-sha)))
      "\n")
     nil incident-id)))

(defun skg--maintenance-require-client-incident (incident-id epoch)
  (unless (and skg--maintenance-client-incident
               (equal incident-id
                      (plist-get skg--maintenance-client-incident
                                 :incident-id))
               (equal epoch
                      (plist-get skg--maintenance-client-incident :epoch)))
    (error "Maintenance response names another incident or epoch"))
  skg--maintenance-client-incident)

(defun skg--maintenance-record-selection (response)
  (let* ((state skg--maintenance-client-incident)
         (source-set (skg--maintenance-text response 'source-set))
         (source-inventory (assoc 'source-inventory response))
         (archive-folder
          (skg--maintenance-text response 'maintenance-archive-folder))
         (archive-identity
          (skg--maintenance-text response 'maintenance-archive-identity))
         (values
          (list
           :g1-graph-generation
           (skg--maintenance-field response 'g1-graph-generation)
           :g1-manifest-revision
           (skg--maintenance-field response 'g1-manifest-revision)
           :tantivy-generation
           (skg--maintenance-field response 'tantivy-generation)
           :server-evidence-sha256
           (skg--maintenance-text response 'server-evidence-sha256))))
    (skg--maintenance-refresh-presentation-buffer-ids response)
    (unless (and source-set source-inventory archive-folder archive-identity
                 (natnump (plist-get values :g1-graph-generation))
                 (natnump (plist-get values :g1-manifest-revision))
                 (natnump (plist-get values :tantivy-generation))
                 (string-match-p
                  "\\`[0-9a-f]\\{64\\}\\'"
                  (or (plist-get values :server-evidence-sha256) "")))
      (error "Maintenance selection authority is incomplete"))
    (dolist (key '(:g1-graph-generation :g1-manifest-revision
                   :tantivy-generation :server-evidence-sha256))
      (let ((prior (plist-get state key))
            (value (plist-get values key)))
        (when (and prior (not (equal prior value)))
          (error "Maintenance selection changed its durable authority"))
        (setf (plist-get state key) value)))
    (let ((prior (plist-get state :selected-source-set)))
      (when (and prior (not (equal prior source-set)))
        (error "Maintenance selection changed its source-set authority"))
      (setf (plist-get state :selected-source-set) source-set))
    (skg-install-source-inventory response)
    (unless (equal skg--active-source-set-name source-set)
      (message "Skg full rebuild changed source-set from %s to %s"
               skg--active-source-set-name source-set))
    (setq skg--active-source-set-name source-set
          skg--maintenance-archive-folder archive-folder
          skg--maintenance-archive-identity archive-identity)
    (setf (plist-get state :phase) 'presenting)
    (setq skg--maintenance-client-incident state)
    state))

(defun skg--maintenance-handle-selection-response (_tcp-proc payload)
  "Continue maintenance after selection or a scalar-approval retry."
  (let* ((response (read payload))
         (status (skg--maintenance-text response 'status))
         (state skg--maintenance-client-incident))
    (unless state (error "Maintenance selection arrived without client state"))
    (when (or (assoc 'incident-id response)
              (assoc 'maintenance-epoch response))
      (skg--maintenance-require-client-incident
       (skg--maintenance-text response 'incident-id)
       (skg--maintenance-field response 'maintenance-epoch)))
    (when (member status '("candidate-selected"
                           "needs-scalar-authorization"))
      (skg--maintenance-record-selection response))
    (pcase status
      ("needs-scalar-authorization"
       (let ((challenge (skg--maintenance-status-challenge response)))
         (setf (plist-get state :phase) 'awaiting-scalar-authorization
               (plist-get state :scalar-challenge) challenge)
         (run-at-time 0 nil #'skg--maintenance-prompt-scalar challenge)))
      ("candidate-selected"
       (setf (plist-get state :scalar-challenge) nil)
       (skg--maintenance-install-settlements
        (or (skg--maintenance-field response 'view-settlements) nil)))
      ("archive-ready"
       (setf (plist-get state :phase) 'origin-operation-required)
       (unless (skg--maintenance-dispatch-origin "archive-ready" response)
         (message
          "Skg maintenance archive is durable; its origin operation is next")))
      ("view-enrollment-pending"
       (setf (plist-get state :phase) 'waiting-for-view-enrollment)
       (message "Skg maintenance is enrolling a newly opened view"))
      (_ (error "Unexpected maintenance selection status: %S" status)))))

(defun skg--maintenance-run-explicit-origin ()
  "Ask the server worker to run an archive-ready server-owned origin."
  (let* ((state skg--maintenance-client-incident)
         (incident-id (plist-get state :incident-id))
         (epoch (plist-get state :epoch))
         (origin (plist-get state :origin)))
    (unless (and state incident-id epoch
                 (member origin '("explicit-partial-reload" "full-rebuild")))
      (error "No server-owned maintenance origin is ready to run"))
    (setf (plist-get state :phase) 'origin-operation-start-pending)
    (let ((tcp-proc (skg-tcp-connect-to-rust)))
      (skg-register-response-handler
       'maintenance-status #'skg--maintenance-handle-origin-started t)
      (skg-set-request-failure-handler
       (lambda (reason)
         (when skg--maintenance-client-incident
           (setf (plist-get skg--maintenance-client-incident :phase)
                 'origin-operation-start-pending))
         (display-warning
          'skg (format "%s worker was not started: %s" origin reason)
          :warning)))
      (skg-submit-request
       tcp-proc
       (concat
        (prin1-to-string
         `((request . "run maintenance origin")
           (maintenance-epoch . ,epoch)))
        "\n")
       nil incident-id))))

(defun skg--maintenance-handle-origin-started (_tcp-proc payload)
  (let* ((response (read payload))
         (status (skg--maintenance-text response 'status))
         (incident-id (skg--maintenance-text response 'incident-id))
         (epoch (skg--maintenance-field response 'maintenance-epoch)))
    (skg--maintenance-require-client-incident incident-id epoch)
    (unless (equal status "origin-operation-started")
      (error "Server did not start the server-owned maintenance origin"))
    (setf (plist-get skg--maintenance-client-incident :phase)
          'waiting-for-origin-observation)
    (message
     (if (equal (plist-get skg--maintenance-client-incident :origin)
                "full-rebuild")
         "Skg is validating the complete disk before exclusive rebuild"
       "Skg is observing the exact partial-reload targets"))))

(defun skg--maintenance-explicit-origin-handler (phase _response)
  "Resume the explicit target observer in an appropriate durable PHASE."
  (when (member phase '("archive-ready" "final-observation"))
    (run-at-time 0 nil #'skg--maintenance-run-explicit-origin)
    t))

(skg-register-maintenance-origin-handler
 "explicit-partial-reload" #'skg--maintenance-explicit-origin-handler)
(skg-register-maintenance-origin-handler
 "full-rebuild" #'skg--maintenance-explicit-origin-handler)

(defun skg--maintenance-prompt-scalar (challenge)
  (let ((prompt (plist-get challenge :prompt)))
    (if (yes-or-no-p (concat prompt " "))
        (skg-approve-maintenance-scalar-release)
      (message
       "Skg maintenance remains locked; approve the stored scalar challenge to resume."))))

(defun skg-approve-maintenance-scalar-release ()
  "Approve the exact durable scalar challenge for active maintenance."
  (interactive)
  (let* ((state skg--maintenance-client-incident)
         (challenge (plist-get state :scalar-challenge))
         (incident-id (plist-get state :incident-id))
         (epoch (plist-get state :epoch))
         (pids (plist-get challenge :pids)))
    (unless (and state challenge incident-id epoch pids)
      (user-error "Skg has no client-known maintenance scalar challenge"))
    (let ((tcp-proc (skg-tcp-connect-to-rust)))
      (skg-register-response-handler
       'maintenance-status #'skg--maintenance-handle-selection-response t)
      (skg-set-request-failure-handler
       (lambda (reason)
         (setf (plist-get skg--maintenance-client-incident :phase)
               'awaiting-scalar-authorization)
         (display-warning
          'skg (format "Maintenance scalar approval was not acknowledged: %s"
                       reason)
          :warning)))
      (skg-submit-request
       tcp-proc
       (concat
        (prin1-to-string
         `((request . "approve maintenance scalar release")
           (maintenance-epoch . ,epoch)
           (allow-ugly-telescopes ,@pids)))
        "\n")
       nil incident-id))))

(defun skg--maintenance-validate-settlements (settlements expected-ids)
  (unless (proper-list-p settlements)
    (error "Maintenance view settlements are malformed"))
  (let ((seen (make-hash-table :test #'equal)) ids)
    (dolist (settlement settlements)
      (let ((buffer-id (skg--maintenance-text settlement 'buffer-id))
            (required (skg--maintenance-text settlement 'required-ack))
            (resolution
             (or (skg--maintenance-text
                  settlement 'settlement-resolution)
                 "pending")))
        (unless (and buffer-id
                     (member required '("retirement-ack" "release-ack"
                                        "application-ack" "close-ack"))
                     (member resolution '("pending" "client-acknowledged"
                                          "census-applied" "census-absent"))
                     (not (gethash buffer-id seen)))
          (error "Maintenance contains a duplicate or invalid settlement"))
        (puthash buffer-id t seen)
        (push buffer-id ids)))
    (unless (equal (sort ids #'string<)
                   (sort (copy-sequence expected-ids) #'string<))
      (error "Maintenance settlement inventory differs from the locked census"))
    settlements))

(defun skg--maintenance-settlement-acknowledged-p (settlement)
  (skg--maintenance-true-p settlement 'acknowledged))

(defun skg--maintenance-settlement-without-ack (settlement)
  (cl-remove-if (lambda (field)
                  (memq (car-safe field)
                        '(acknowledged settlement-resolution)))
                settlement))

(defun skg--maintenance-client-acknowledged-settlement (settlement)
  "Return SETTLEMENT with this client's exact ACK state installed."
  (cons '(acknowledged "true")
        (cons '(settlement-resolution "client-acknowledged")
              (skg--maintenance-settlement-without-ack settlement))))

(defun skg--maintenance-replace-settlement (records replacement)
  "Replace REPLACEMENT's buffer record in RECORDS without reordering it."
  (let ((buffer-id (skg--maintenance-text replacement 'buffer-id))
        found)
    (prog1
        (mapcar
         (lambda (record)
           (if (equal buffer-id
                      (skg--maintenance-text record 'buffer-id))
               (progn (setq found t) replacement)
             record))
         records)
      (unless found
        (error "Maintenance ACK names an uninstalled settlement")))))

(defun skg--maintenance-require-stable-settlements (old new)
  "Permit only acknowledgement bits to change between OLD and NEW."
  (when old
    (dolist (settlement new)
      (let* ((buffer-id (skg--maintenance-text settlement 'buffer-id))
             (prior (cl-find buffer-id old
                             :key (lambda (record)
                                    (skg--maintenance-text
                                     record 'buffer-id))
                             :test #'equal)))
        (when (and prior
                   (not (equal
                         (skg--maintenance-settlement-without-ack prior)
                         (skg--maintenance-settlement-without-ack settlement))))
          (error "Maintenance changed the durable settlement for %s"
                 buffer-id))))))

(defun skg--maintenance-install-settlements (settlements)
  (let* ((state skg--maintenance-client-incident)
         (expected (plist-get state :registered-buffer-ids))
         pending acknowledged)
    (skg--maintenance-validate-settlements settlements expected)
    (skg--maintenance-require-stable-settlements
     (plist-get state :settlements) settlements)
    (dolist (settlement settlements)
      (if (skg--maintenance-settlement-acknowledged-p settlement)
          (let* ((buffer-id
                  (skg--maintenance-text settlement 'buffer-id))
                 (resolution
                  (or (skg--maintenance-text
                       settlement 'settlement-resolution)
                      "client-acknowledged"))
                 (absent (not (buffer-live-p
                               (skg-find-buffer-by-id buffer-id)))))
            (cond
             ((equal resolution "census-absent")
              (unless absent
                (error "Server closed a settlement for a buffer still in the census")))
             ((and (not (plist-get state :adopted))
                   (not (member buffer-id (plist-get state :locally-applied))))
              (error "Server acknowledged a settlement not applied locally")))
            (push settlement acknowledged))
        (push settlement pending)))
    (setf (plist-get state :settlements) settlements
          (plist-get state :pending-settlements) (nreverse pending)
          (plist-get state :acknowledged-settlements)
          (nreverse acknowledged)
          (plist-get state :in-flight-settlement) nil
          (plist-get state :phase) 'settling-views)
    (setq skg--maintenance-client-incident state)
    (run-at-time 0 nil #'skg--maintenance-settle-next)))

(defun skg--maintenance-install-preselection-retirements (retirements)
  "Install exact dirty-buffer RETIREMENTS for invalid post-mutation disk."
  (let* ((state skg--maintenance-client-incident)
         (registered (plist-get state :registered-buffer-ids))
         (seen (make-hash-table :test #'equal))
         pending acknowledged)
    (unless (proper-list-p retirements)
      (error "Invalid-disk retirements are malformed"))
    (dolist (retirement retirements)
      (let ((buffer-id (skg--maintenance-text retirement 'buffer-id)))
        (unless (and buffer-id (member buffer-id registered)
                     (not (gethash buffer-id seen))
                     (skg--maintenance-true-p retirement 'dirty)
                     (skg--maintenance-true-p retirement 'impacted)
                     (equal (skg--maintenance-text
                             retirement 'planned-disposition)
                            "interrupted")
                     (equal (skg--maintenance-text
                             retirement 'required-ack)
                            "retirement-ack"))
          (error "Invalid-disk retirement inventory is inconsistent"))
        (puthash buffer-id t seen)
        (if (skg--maintenance-settlement-acknowledged-p retirement)
            (let ((resolution (or (skg--maintenance-text
                                   retirement 'settlement-resolution)
                                  "client-acknowledged")))
              (unless (or (member buffer-id (plist-get state :locally-applied))
                          (and (equal resolution "census-absent")
                               (not (buffer-live-p
                                     (skg-find-buffer-by-id buffer-id))))
                          (plist-get state :adopted))
                (error "Server acknowledged an unapplied dirty retirement"))
              (push retirement acknowledged))
          (push retirement pending))))
    (skg--maintenance-require-stable-settlements
     (plist-get state :preselection-retirements) retirements)
    (setf (plist-get state :preselection-retirements) retirements
          (plist-get state :pending-preselection-retirements)
          (nreverse pending)
          (plist-get state :acknowledged-preselection-retirements)
          (nreverse acknowledged)
          (plist-get state :in-flight-preselection-retirement) nil
          (plist-get state :phase) 'settling-preselection-retirements)
    (setq skg--maintenance-client-incident state)
    (run-at-time 0 nil #'skg--maintenance-settle-next-preselection-retirement)))

(defun skg--maintenance-settle-next-preselection-retirement ()
  (let* ((state skg--maintenance-client-incident)
         (pending (plist-get state :pending-preselection-retirements)))
    (when (and state
               (eq (plist-get state :phase)
                   'settling-preselection-retirements))
      (if (null pending)
          (progn
            (setf (plist-get state :phase) 'server-blocked)
            (message
             "Skg retired every dirty view; repair disk and retry maintenance"))
        (let* ((retirement (car pending))
               (buffer-id (skg--maintenance-text retirement 'buffer-id)))
          (condition-case error-data
              (progn
                (unless (member buffer-id (plist-get state :locally-applied))
                  (skg--maintenance-apply-settlement retirement)
                  (push buffer-id (plist-get state :locally-applied)))
                (setf (plist-get state :in-flight-preselection-retirement)
                      retirement)
                (skg--maintenance-send-preselection-retirement-ack retirement))
            (error
             (setf (plist-get state :phase)
                   'preselection-retirement-blocked)
             (display-warning
              'skg
              (format "Maintenance could not retire dirty buffer %s: %s"
                      buffer-id (error-message-string error-data))
              :error))))))))

(defun skg--maintenance-send-preselection-retirement-ack (retirement)
  (let* ((state skg--maintenance-client-incident)
         (tcp-proc (skg-tcp-connect-to-rust)))
    (skg-register-response-handler
     'maintenance-status
     #'skg--maintenance-handle-preselection-retirement-ack t)
    (skg-set-request-failure-handler
     (lambda (reason)
       (when skg--maintenance-client-incident
         (setf (plist-get skg--maintenance-client-incident :phase)
               'preselection-retirement-ack-pending))
       (display-warning
        'skg (format "Dirty-buffer retirement ACK was not delivered: %s"
                     reason)
        :warning)))
    (skg-submit-request
     tcp-proc
     (concat
      (prin1-to-string
       (append
        `((request . "maintenance view settled")
          (maintenance-epoch . ,(plist-get state :epoch)))
        (skg--maintenance-ack-fields retirement)))
      "\n")
     nil (plist-get state :incident-id))))

(defun skg--maintenance-handle-preselection-retirement-ack
    (_tcp-proc payload)
  (let* ((response (read payload))
         (state skg--maintenance-client-incident)
         (retirement (plist-get state :in-flight-preselection-retirement))
         (status (skg--maintenance-text response 'status))
         (buffer-id (and retirement
                         (skg--maintenance-text retirement 'buffer-id)))
         (first (car (plist-get state
                                :pending-preselection-retirements))))
    (unless (and retirement first
                 (member status '("invalid-dirty-buffer-retired"
                                  "all-invalid-dirty-buffers-retired"))
                 (equal buffer-id
                        (skg--maintenance-text response 'buffer-id))
                 (equal buffer-id
                        (skg--maintenance-text first 'buffer-id))
                 (equal (skg--maintenance-text response 'required-ack)
                        "retirement-ack"))
      (error "Dirty-buffer retirement ACK changed identity"))
    (setq retirement
          (skg--maintenance-client-acknowledged-settlement retirement))
    (setf (plist-get state :preselection-retirements)
          (skg--maintenance-replace-settlement
           (plist-get state :preselection-retirements) retirement))
    (push retirement
          (plist-get state :acknowledged-preselection-retirements))
    (setf (plist-get state :pending-preselection-retirements)
          (cdr (plist-get state :pending-preselection-retirements))
          (plist-get state :in-flight-preselection-retirement) nil
          (plist-get state :phase) 'settling-preselection-retirements)
    (run-at-time 0 nil #'skg--maintenance-settle-next-preselection-retirement)))

(defun skg--maintenance-application (settlement)
  (let ((application (skg--maintenance-field settlement 'application)))
    (unless (proper-list-p application)
      (error "Maintenance application settlement has no rendered offer"))
    application))

(defun skg--maintenance-apply-rendered-view
    (buffer settlement application state)
  (unless (buffer-live-p buffer)
    (error "Maintenance cannot apply text to a missing buffer"))
  (let* ((content (skg--maintenance-field application 'content))
         (content-sha (skg--maintenance-text application 'content-sha256))
         (base-token (skg--maintenance-settlement-nat
                      settlement 'base-application-token))
         (base-revision (skg--maintenance-settlement-nat
                         settlement 'base-server-revision))
         (result-token (skg--maintenance-settlement-nat
                        application 'resulting-application-token))
         (result-revision (skg--maintenance-settlement-nat
                           application 'resulting-server-revision))
         (result-graph (skg--maintenance-settlement-nat
                        application 'resulting-graph-generation))
         (result-presentation
          (skg--maintenance-settlement-nat
           application 'resulting-presentation-generation))
         (uri (skg--maintenance-settlement-uri settlement))
         (buffer-id (skg--maintenance-text settlement 'buffer-id)))
    (unless (and (stringp content)
                 (string-match-p "\\`[0-9a-f]\\{64\\}\\'" content-sha)
                 (equal content-sha (skg--sha256-text content))
                 (= result-token (1+ base-token))
                 (= result-revision (1+ base-revision))
                 (= result-graph (plist-get state :g1-graph-generation)))
      (error "Maintenance application offer is internally inconsistent"))
    (with-current-buffer buffer
      (skg-validate-maintenance-buffer-base
       buffer settlement (plist-get state :epoch))
      (let ((old-point (point))
            (window-starts
             (mapcar (lambda (window) (cons window (window-start window)))
                     (get-buffer-window-list buffer nil t)))
            (inhibit-modification-hooks t))
        (skg-replace-buffer-with-new-content
         nil content nil
         (list
          :client-buffer-id buffer-id
          :view-uri uri
          :base-server-revision base-revision
          :base-graph-generation
          (skg--maintenance-settlement-nat
           settlement 'base-graph-generation)
          :base-presentation-generation
          (skg--maintenance-settlement-nat
           settlement 'base-presentation-generation)
          :expected-application-token base-token
          :graph-generation result-graph
          :presentation-generation result-presentation
          :server-revision result-revision
          :application-token result-token
          :require-clean t))
        (goto-char (min old-point (point-max)))
        (dolist (entry window-starts)
          (when (window-live-p (car entry))
            (set-window-start (car entry)
                              (min (cdr entry) (point-max)) t))))
      (when (eq (skg--buffer-record-kind skg--buffer-record) 'search-view)
        (setf (skg--buffer-record-search-stale skg--buffer-record) t))
      (unless (and (= result-token
                      (skg--buffer-record-application-token
                       skg--buffer-record))
                   (= result-revision
                      (skg--buffer-record-server-revision
                       skg--buffer-record))
                   (= result-graph
                      (skg--buffer-record-graph-generation
                       skg--buffer-record))
                   (= result-presentation
                      (skg--buffer-record-presentation-generation
                       skg--buffer-record))
                   (equal content-sha
                          (skg--buffer-record-last-fetched-sha256
                           skg--buffer-record)))
        (error "Maintenance application did not install its exact authority")))
    (dolist (warning (or (skg--maintenance-field application 'warnings) nil))
      (display-warning 'skg (format "%s" warning) :warning))))

(defun skg--maintenance-apply-settlement (settlement)
  "Apply one SETTLEMENT locally, without acknowledging it to the server."
  (let* ((state skg--maintenance-client-incident)
         (buffer-id (skg--maintenance-text settlement 'buffer-id))
         (requirement (skg--maintenance-text settlement 'required-ack))
         (buffer (skg-find-buffer-by-id buffer-id))
         (epoch (plist-get state :epoch))
         (incident-id (plist-get state :incident-id)))
    (pcase requirement
      ("application-ack"
       (skg--maintenance-apply-rendered-view
        buffer settlement (skg--maintenance-application settlement) state))
      ("release-ack"
       (unless (buffer-live-p buffer)
         (error "Maintenance cannot release missing buffer %s" buffer-id))
       (skg-release-buffer-across-maintenance
        buffer settlement epoch (plist-get state :g1-graph-generation)))
      ("retirement-ack"
       (if (buffer-live-p buffer)
           (skg-retire-buffer-for-maintenance
            buffer settlement epoch incident-id)
         (display-warning
          'skg
          (format "Retired buffer %s is absent; its recovery archive remains durable"
                  buffer-id)
          :warning)))
      ("close-ack"
       (skg-close-buffer-for-maintenance buffer settlement epoch))
      (_ (error "Unknown maintenance settlement action: %S" requirement)))))

(defun skg--maintenance-ack-fields (settlement)
  (let ((fields
         `((buffer-id . ,(skg--maintenance-text settlement 'buffer-id))
           (required-ack . ,(skg--maintenance-text settlement 'required-ack))
           (view-uri . ,(or (skg--maintenance-settlement-uri settlement)
                            "none"))
           (base-graph-generation
            . ,(skg--maintenance-settlement-nat
                settlement 'base-graph-generation))
           (base-presentation-generation
            . ,(skg--maintenance-settlement-nat
                settlement 'base-presentation-generation))
           (base-server-revision
            . ,(skg--maintenance-settlement-nat
                settlement 'base-server-revision))
           (base-application-token
            . ,(skg--maintenance-settlement-nat
                settlement 'base-application-token)))))
    (if (equal (skg--maintenance-text settlement 'required-ack)
               "application-ack")
        (let ((application (skg--maintenance-application settlement)))
          (append
           fields
           `((content-sha256
              . ,(skg--maintenance-text application 'content-sha256))
             (resulting-graph-generation
              . ,(skg--maintenance-settlement-nat
                  application 'resulting-graph-generation))
             (resulting-presentation-generation
              . ,(skg--maintenance-settlement-nat
                  application 'resulting-presentation-generation))
             (resulting-server-revision
              . ,(skg--maintenance-settlement-nat
                  application 'resulting-server-revision))
             (resulting-application-token
              . ,(skg--maintenance-settlement-nat
                  application 'resulting-application-token)))))
      fields)))

(defun skg--maintenance-settle-next ()
  (let* ((state skg--maintenance-client-incident)
         (pending (plist-get state :pending-settlements)))
    (when (and state (eq (plist-get state :phase) 'settling-views))
      (if (null pending)
          (skg--maintenance-resume-finalization)
        (let* ((settlement (car pending))
               (buffer-id (skg--maintenance-text settlement 'buffer-id)))
          (condition-case error-data
              (progn
                (unless (member buffer-id (plist-get state :locally-applied))
                  (skg--maintenance-apply-settlement settlement)
                  (push buffer-id (plist-get state :locally-applied)))
                (setf (plist-get state :in-flight-settlement) settlement)
                (skg--maintenance-send-settlement-ack settlement))
            (error
             (setf (plist-get state :phase) 'view-settlement-blocked)
             (display-warning
              'skg
              (format "Maintenance left buffer %s locked: %s"
                      buffer-id (error-message-string error-data))
              :error))))))))

(defun skg--maintenance-resume-finalization ()
  (if (plist-get skg--maintenance-client-incident :final-archive)
      (skg--maintenance-send-final-archive-ack)
    (skg--maintenance-request-evidence)))

(defun skg--maintenance-send-settlement-ack (settlement)
  (let* ((state skg--maintenance-client-incident)
         (incident-id (plist-get state :incident-id))
         (epoch (plist-get state :epoch))
         (tcp-proc (skg-tcp-connect-to-rust)))
    (skg-register-response-handler
     'maintenance-status #'skg--maintenance-handle-settlement-ack t)
    (skg-set-request-failure-handler
     (lambda (reason)
       (when skg--maintenance-client-incident
         (setf (plist-get skg--maintenance-client-incident :phase)
               'view-settlement-ack-pending))
       (display-warning
        'skg (format "Maintenance settlement ACK was not delivered: %s" reason)
        :warning)))
    (skg-submit-request
     tcp-proc
     (concat
      (prin1-to-string
       (append
        `((request . "maintenance view settled")
          (maintenance-epoch . ,epoch))
        (skg--maintenance-ack-fields settlement)))
      "\n")
     nil incident-id)))

(defun skg--maintenance-handle-settlement-ack (_tcp-proc payload)
  (let* ((response (read payload))
         (state skg--maintenance-client-incident)
         (settlement (plist-get state :in-flight-settlement))
         (status (skg--maintenance-text response 'status))
         (buffer-id (skg--maintenance-text settlement 'buffer-id)))
    (unless (and settlement
                 (member status '("view-settlement-recorded"
                                  "all-views-settled"))
                 (equal buffer-id
                        (skg--maintenance-text response 'buffer-id))
                 (equal (skg--maintenance-text settlement 'required-ack)
                        (skg--maintenance-text response 'required-ack)))
      (error "Maintenance settlement ACK response changed identity"))
    (unless (equal buffer-id
                   (skg--maintenance-text
                    (car (plist-get state :pending-settlements)) 'buffer-id))
      (error "Maintenance settlement response arrived out of order"))
    (setq settlement
          (skg--maintenance-client-acknowledged-settlement settlement))
    (setf (plist-get state :settlements)
          (skg--maintenance-replace-settlement
           (plist-get state :settlements) settlement))
    (push settlement (plist-get state :acknowledged-settlements))
    (setf (plist-get state :pending-settlements)
          (cdr (plist-get state :pending-settlements))
          (plist-get state :in-flight-settlement) nil
          (plist-get state :phase) 'settling-views)
    (run-at-time 0 nil #'skg--maintenance-settle-next)))

(defun skg--maintenance-request-evidence ()
  (let* ((state skg--maintenance-client-incident)
         (incident-id (plist-get state :incident-id))
         (epoch (plist-get state :epoch))
         (server-sha (plist-get state :server-evidence-sha256)))
    (unless (and incident-id epoch
                 (string-match-p "\\`[0-9a-f]\\{64\\}\\'" server-sha))
      (error "Maintenance selection has no valid server evidence identity"))
    (setf (plist-get skg--maintenance-client-incident :phase)
          'requesting-evidence)
    (let ((tcp-proc (skg-tcp-connect-to-rust)))
      (skg-register-response-handler
       'maintenance-evidence #'skg--maintenance-handle-evidence t)
      (skg-set-request-failure-handler
       (lambda (reason)
         (when skg--maintenance-client-incident
           (setf (plist-get skg--maintenance-client-incident :phase)
                 'evidence-request-pending))
         (display-warning
          'skg (format "Maintenance evidence was not delivered: %s" reason)
          :warning)))
      (skg-submit-request
       tcp-proc
       (concat
        (prin1-to-string
         `((request . "maintenance evidence")
           (maintenance-epoch . ,epoch)
           (server-evidence-sha256 . ,server-sha)))
        "\n")
       nil incident-id))))

(defun skg--maintenance-handle-evidence (_tcp-proc payload opaque-bytes)
  (let* ((descriptor (read payload))
         (state skg--maintenance-client-incident)
         (incident-id (plist-get state :incident-id))
         (epoch (plist-get state :epoch)))
    (skg--maintenance-require-client-incident incident-id epoch)
    (unless (and (equal incident-id
                        (skg--maintenance-text descriptor 'incident-id))
                 (= epoch (skg--maintenance-field
                           descriptor 'maintenance-epoch)))
      (error "Maintenance evidence names another incident"))
    (condition-case error-data
        (let ((final
               (skg-recovery-archive-finalize
                (plist-get state :archive)
                descriptor
                opaque-bytes
                (plist-get state :settlements))))
          (setf (plist-get skg--maintenance-client-incident :evidence)
                descriptor
                (plist-get skg--maintenance-client-incident :final-archive)
                final
                (plist-get skg--maintenance-client-incident :phase)
                'archive-finalized-locally)
          (run-at-time 0 nil #'skg--maintenance-send-final-archive-ack))
      (error
       (setf (plist-get skg--maintenance-client-incident :phase)
             'archive-finalization-failed)
       (display-warning
        'skg
        (format "Maintenance recovery archive could not be finalized: %s"
                (error-message-string error-data))
        :error)
       (signal (car error-data) (cdr error-data))))))

(defun skg--maintenance-send-final-archive-ack ()
  (let* ((state skg--maintenance-client-incident)
         (incident-id (plist-get state :incident-id))
         (epoch (plist-get state :epoch))
         (final (plist-get state :final-archive))
         (tcp-proc (skg-tcp-connect-to-rust)))
    (unless final (error "Maintenance has no finalized client archive"))
    (skg-register-response-handler
     'maintenance-status #'skg--maintenance-handle-final-archive-ack t)
    (skg-set-request-failure-handler
     (lambda (reason)
       (when skg--maintenance-client-incident
         (setf (plist-get skg--maintenance-client-incident :phase)
               'archive-final-ack-pending))
       (display-warning
        'skg (format "Final archive ACK was not delivered: %s" reason)
        :warning)))
    (skg-submit-request
     tcp-proc
     (concat
      (prin1-to-string
       `((request . "maintenance archive finalized")
         (maintenance-epoch . ,epoch)
         (manifest-sha256 . ,(plist-get final :manifest-sha256))
         (transfer-manifest-sha256
          . ,(plist-get final :transfer-manifest-sha256))
         (artifact-bytes-sha256
          . ,(plist-get final :artifact-bytes-sha256))))
      "\n")
     nil incident-id)))

(defun skg--maintenance-handle-final-archive-ack (_tcp-proc payload)
  (let* ((response (read payload))
         (final (plist-get skg--maintenance-client-incident :final-archive)))
    (unless (and
             (equal (skg--maintenance-text response 'status)
                    "archive-finalized")
             (equal (skg--maintenance-text response 'manifest-sha256)
                    (plist-get final :manifest-sha256))
             (equal (skg--maintenance-text
                     response 'transfer-manifest-sha256)
                    (plist-get final :transfer-manifest-sha256))
             (equal (skg--maintenance-text response 'artifact-bytes-sha256)
                    (plist-get final :artifact-bytes-sha256)))
      (error "Server did not acknowledge the exact final archive"))
    (setf (plist-get skg--maintenance-client-incident :phase)
          'completing)
    (run-at-time 0 nil #'skg--maintenance-send-complete)))

(defun skg--maintenance-send-complete ()
  (let* ((state skg--maintenance-client-incident)
         (incident-id (plist-get state :incident-id))
         (epoch (plist-get state :epoch))
         (manifest-sha (plist-get (plist-get state :final-archive)
                                  :manifest-sha256))
         (tcp-proc (skg-tcp-connect-to-rust)))
    (skg-register-response-handler
     'maintenance-status #'skg--maintenance-handle-terminal t)
    (skg-set-request-failure-handler
     (lambda (reason)
       (when skg--maintenance-client-incident
         (setf (plist-get skg--maintenance-client-incident :phase)
               'completion-pending))
       (display-warning
        'skg (format "Maintenance completion reply was lost: %s" reason)
        :warning)))
    (skg-submit-request
     tcp-proc
     (concat
      (prin1-to-string
       `((request . "complete maintenance")
         (maintenance-epoch . ,epoch)
         (manifest-sha256 . ,manifest-sha)))
      "\n")
     nil incident-id)))

(defun skg--maintenance-handle-terminal (_tcp-proc payload)
  (let* ((response (read payload))
         (state skg--maintenance-client-incident)
         (incident-id (plist-get state :incident-id))
         (epoch (plist-get state :epoch))
         (manifest-sha (plist-get (plist-get state :final-archive)
                                  :manifest-sha256))
         (unlock-ids (skg--maintenance-string-list
                      response 'unlock-buffer-ids))
         (expected-ids (plist-get state :registered-buffer-ids)))
    (unless (and (equal (skg--maintenance-text response 'status) "terminal")
                 (equal (skg--maintenance-text response 'incident-id)
                        incident-id)
                 (= (skg--maintenance-field response 'maintenance-epoch)
                    epoch)
                 (equal (skg--maintenance-text response 'disposition)
                        "completed")
                 (equal (skg--maintenance-text response 'manifest-sha256)
                        manifest-sha)
                 (= (skg--maintenance-settlement-nat
                     response 'selected-graph-generation)
                    (plist-get state :g1-graph-generation))
                 (= (skg--maintenance-settlement-nat
                     response 'selected-manifest-revision)
                    (plist-get state :g1-manifest-revision))
                 (equal (sort (copy-sequence unlock-ids) #'string<)
                        (sort (copy-sequence expected-ids) #'string<)))
      (error "Maintenance terminal instruction changed its exact authority"))
    (dolist (buffer-id unlock-ids)
      (when-let ((buffer (skg-find-buffer-by-id buffer-id)))
        (skg-unlock-buffer-after-maintenance buffer epoch)))
    (setf (plist-get skg--maintenance-client-incident :phase)
          'terminal-received
          (plist-get skg--maintenance-client-incident :terminal) response)
    (skg--maintenance-set-handshake-summary 'terminal epoch)
    (when-let ((graph (skg--maintenance-field
                       response 'selected-graph-generation)))
      (setf (alist-get 'graph-generation skg--server-store-state) graph))
    (when-let ((revision (skg--maintenance-field
                          response 'selected-manifest-revision)))
      (setf (alist-get 'manifest-revision skg--server-store-state) revision))
    (when-let ((callback (plist-get state :terminal-callback)))
      (unless (plist-get state :terminal-callback-fired)
        (funcall callback response)
        (setf (plist-get state :terminal-callback-fired) t)))
    (run-at-time 0 nil #'skg--maintenance-send-terminal-ack)))

(defun skg--maintenance-send-terminal-ack ()
  (let* ((state skg--maintenance-client-incident)
         (incident-id (plist-get state :incident-id))
         (epoch (plist-get state :epoch))
         (tcp-proc (skg-tcp-connect-to-rust)))
    (skg-register-response-handler
     'maintenance-status #'skg--maintenance-handle-terminal-ack t)
    (skg-set-request-failure-handler
     (lambda (reason)
       (display-warning
        'skg (format "Terminal maintenance ACK was not delivered: %s" reason)
        :warning)))
    (skg-submit-request
     tcp-proc
     (concat
      (prin1-to-string
       `((request . "acknowledge terminal maintenance")
         (maintenance-epoch . ,epoch)))
      "\n")
     nil incident-id)))

(defun skg--maintenance-handle-terminal-ack (_tcp-proc payload)
  (let ((response (read payload)))
    (unless (equal (skg--maintenance-text response 'status) "idle")
      (error "Server did not enter idle after terminal maintenance ACK"))
    (skg--maintenance-finish-idle)))

(defun skg--maintenance-finish-idle ()
  "Forget a terminal incident only after the server durably reaches idle."
  (let* ((state skg--maintenance-client-incident)
         (final (and state (plist-get state :final-archive)))
         (path (and final (plist-get final :path))))
    (unless (and state (eq (plist-get state :phase) 'terminal-received))
      (error "Server became idle before the client received terminal authority"))
    (skg--maintenance-set-handshake-summary 'idle (plist-get state :epoch))
    (setq skg--maintenance-client-incident nil
          skg--pending-maintenance-offer nil)
    (message "Skg maintenance complete; recovery archive: %s" path)))

(defun skg--maintenance-inspect-retained-incident (response require-final)
  "Return the exact retained archive facts named by RESPONSE.
When REQUIRE-FINAL is non-nil, reject an archive which has not published its
checksummed final marker."
  (let* ((incident-id
          (or (skg--maintenance-text response 'active-incident-id)
              (skg--maintenance-text response 'incident-id)))
         (name (skg--maintenance-text response 'archive-directory-name))
         (path (and name skg--maintenance-archive-folder
                    (expand-file-name name skg--maintenance-archive-folder)))
         (summary (and path (skg-recovery-archive-inspect path)))
         (initial-sha
          (skg--maintenance-text response 'initial-manifest-sha256))
         (final-sha (skg--maintenance-text response 'manifest-sha256)))
    (unless (and summary
                 (equal incident-id (plist-get summary :incident-id))
                 (equal name (plist-get summary :name)))
      (error "Retained maintenance archive identity does not match the server"))
    (when (and initial-sha (not (equal initial-sha "none"))
               (not (equal initial-sha
                           (plist-get summary :initial-manifest-sha256))))
      (error "Retained maintenance initial checksum changed"))
    (when require-final
      (unless (and (eq (plist-get summary :status) 'finalized)
                   (equal final-sha
                          (plist-get summary :final-manifest-sha256)))
        (error "Retained maintenance final checksum changed")))
    summary))

(defun skg--maintenance-adopt-active (response)
  "Reconstruct client state for an archive-backed active RESPONSE."
  (unless skg--maintenance-client-incident
    (let* ((archive-status
            (skg--maintenance-text response 'archive-status))
           (_archive-ready
            (unless (member archive-status '("archive-ready" "finalized"))
              (error "A replacement editor cannot adopt maintenance before archive-ready")))
           (summary (skg--maintenance-inspect-retained-incident response nil))
           (incident-id
            (skg--maintenance-text response 'active-incident-id))
           (epoch (skg--maintenance-field response 'maintenance-epoch))
           (initial-sha (plist-get summary :initial-manifest-sha256))
           (finalized (eq (plist-get summary :status) 'finalized)))
      (when (and (equal archive-status "finalized")
                 (not (equal
                       (skg--maintenance-text
                        response 'archive-manifest-sha256)
                       (plist-get summary :final-manifest-sha256))))
        (error "Retained maintenance final checksum changed"))
      (setq skg--maintenance-client-incident
            (list
             :incident-id incident-id :epoch epoch
             :origin (skg--maintenance-text response 'origin)
             :requested-paths
             (skg--maintenance-string-list response 'requested-paths)
             :requested-ids
             (skg--maintenance-string-list response 'requested-ids)
             :phase 'adopting-retained-incident
             :offer
             (list :incident-id incident-id :epoch epoch
                   :origin (skg--maintenance-text response 'origin)
                   :started-at-utc
                   (skg--maintenance-text response 'started-at-utc)
                   :archive-name (plist-get summary :name)
                   :archive-folder skg--maintenance-archive-folder
                   :archive-identity skg--maintenance-archive-identity
                   :source-set (skg--maintenance-text response 'source-set)
                   :graph-generation
                   (skg--maintenance-field response 'g0-graph-generation)
                   :manifest-revision
                   (skg--maintenance-field response 'g0-manifest-revision))
             :registered-buffer-ids
             (skg--maintenance-string-list response 'registered-buffer-ids)
             :undo-waivers nil :locally-applied nil :settlements nil
             :pending-settlements nil :acknowledged-settlements nil
             :in-flight-settlement nil
             :archive (list :path (plist-get summary :path)
                            :manifest-sha256 initial-sha)
             :final-archive
             (and finalized
                  (list
                   :path (plist-get summary :path)
                   :manifest-sha256
                   (plist-get summary :final-manifest-sha256)
                   :transfer-manifest-sha256
                   (plist-get summary :transfer-manifest-sha256)
                   :artifact-bytes-sha256
                   (plist-get summary :artifact-bytes-sha256)))
             :adopted t :terminal-callback nil
             :terminal-callback-fired nil)))))

(defun skg--maintenance-adopt-terminal (response)
  "Reconstruct enough state to receive and ACK terminal RESPONSE."
  (unless skg--maintenance-client-incident
    (let ((summary (skg--maintenance-inspect-retained-incident response t)))
      (setq skg--maintenance-client-incident
            (list
             :incident-id (skg--maintenance-text response 'incident-id)
             :epoch (skg--maintenance-field response 'maintenance-epoch)
             :phase 'adopting-terminal
             :registered-buffer-ids
             (skg--maintenance-string-list response 'unlock-buffer-ids)
             :g1-graph-generation
             (skg--maintenance-field response 'selected-graph-generation)
             :g1-manifest-revision
             (skg--maintenance-field response 'selected-manifest-revision)
             :final-archive
             (list :path (plist-get summary :path)
                   :manifest-sha256
                   (plist-get summary :final-manifest-sha256)
                   :transfer-manifest-sha256
                   (plist-get summary :transfer-manifest-sha256)
                   :artifact-bytes-sha256
                   (plist-get summary :artifact-bytes-sha256))
             :adopted t :terminal-callback nil
             :terminal-callback-fired nil)))))

(defun skg--maintenance-status-challenge (response)
  (let ((challenge
         (list :operation (skg--maintenance-text response 'operation)
               :pids (skg--maintenance-string-list response 'pids)
               :prompt (skg--maintenance-text response 'prompt))))
    (unless (and (plist-get challenge :operation)
                 (plist-get challenge :pids)
                 (plist-get challenge :prompt))
      (error "Maintenance scalar challenge is incomplete"))
    challenge))

(defun skg--maintenance-resume-active (response)
  (let* ((incident-id (skg--maintenance-text response 'active-incident-id))
         (epoch (skg--maintenance-field response 'maintenance-epoch))
         (phase (skg--maintenance-text response 'phase))
         (_adopted (skg--maintenance-adopt-active response))
         (state (skg--maintenance-require-client-incident
                 incident-id epoch))
         (origin (skg--maintenance-text response 'origin))
         (paths (skg--maintenance-string-list response 'requested-paths))
         (ids (skg--maintenance-string-list response 'requested-ids))
         (settlements (skg--maintenance-field response 'view-settlements))
         (has-scalar (assoc 'scalar-approved response)))
    (skg--maintenance-refresh-presentation-buffer-ids response)
    (when (and (plist-get state :origin)
               (not (equal (plist-get state :origin) origin)))
      (error "Maintenance status changed its origin"))
    (when (and (plist-get state :requested-paths)
               (not (equal (plist-get state :requested-paths) paths)))
      (error "Maintenance status changed its requested paths"))
    (when (and (plist-get state :requested-ids)
               (not (equal (plist-get state :requested-ids) ids)))
      (error "Maintenance status changed its requested IDs"))
    (setf (plist-get state :origin) origin
          (plist-get state :requested-paths) paths
          (plist-get state :requested-ids) ids)
    (skg--maintenance-set-handshake-summary 'active epoch)
    (when (skg--maintenance-field response 'g1-graph-generation)
      (skg--maintenance-record-selection response))
    (cond
     (settlements
      (skg--maintenance-install-settlements settlements))
     ((and has-scalar
           (not (skg--maintenance-true-p response 'scalar-approved)))
      (let ((challenge (skg--maintenance-status-challenge response)))
        (setf (plist-get state :phase) 'awaiting-scalar-authorization
              (plist-get state :scalar-challenge) challenge)
        (run-at-time 0 nil #'skg--maintenance-prompt-scalar challenge)))
     ((and has-scalar
           (skg--maintenance-true-p response 'scalar-approved)
           (equal phase "presenting"))
      (setf (plist-get state :scalar-challenge)
            (skg--maintenance-status-challenge response)
            (plist-get state :phase) 'resuming-approved-scalar)
      (run-at-time 0 nil #'skg-approve-maintenance-scalar-release))
     ((equal phase "finalizing-archive")
      (setf (plist-get state :phase) 'finalizing-archive)
      (run-at-time 0 nil #'skg--maintenance-resume-finalization))
     ((equal phase "awaiting-locked-census")
      (setf (plist-get state :phase) 'awaiting-locked-census)
      (run-at-time 0 nil #'skg--maintenance-send-locked-census))
     ((equal phase "preparing-archive")
      (unless (plist-get state :lock-census-sha256)
        (setf (plist-get state :registered-buffer-ids)
              (skg--maintenance-string-list response 'registered-buffer-ids)
              (plist-get state :lock-census-sha256)
              (skg--maintenance-lock-offer response)))
      (setf (plist-get state :phase) 'preparing-archive)
      (if-let ((archive (plist-get state :archive)))
          (run-at-time
           0 nil #'skg--maintenance-send-archive-ready
           incident-id epoch
           (plist-get state :lock-census-sha256)
           (plist-get archive :manifest-sha256))
        (run-at-time 0 nil #'skg--maintenance-publish-initial)))
     ((member phase '("archive-ready" "running-external-mutation"
                      "final-observation"))
      (setf (plist-get state :phase)
            (if (equal phase "archive-ready")
                'origin-operation-required
              'waiting-for-origin-observation))
      (unless (skg--maintenance-dispatch-origin phase response)
        (message "Skg maintenance %s awaits its %s origin adapter"
                 incident-id origin)))
     ((member phase '("blocked-invalid-after-mutation"
                      "blocked-store-health"))
      (let ((reason (or (skg--maintenance-text response 'blocking-reason)
                        "unspecified")))
        (setf (plist-get state :phase) 'server-blocked
              (plist-get state :server-phase) phase
              (plist-get state :blocking-reason) reason)
        (setq skg--maintenance-client-incident state)
        (display-warning
         'skg
         (format
          (concat "Maintenance %s remains locked in server phase %s: %s. "
                  "Repair the reported problem, then run "
                  "M-x skg-retry-maintenance.")
         incident-id phase reason)
         :error)
        (when-let ((retirements
                    (skg--maintenance-field
                     response 'preselection-retirements)))
          (skg--maintenance-install-preselection-retirements retirements))))
     (t
      (setf (plist-get state :phase) 'waiting-for-server)
      (message "Skg maintenance %s is in server phase %s"
               incident-id phase)))))

(defun skg--maintenance-handle-status (_tcp-proc payload)
  "Resume or report the exact durable state in a status PAYLOAD."
  (let* ((response (read payload))
         (status (skg--maintenance-text response 'status)))
    (pcase status
      ("active" (skg--maintenance-resume-active response))
      ("terminal"
       (skg--maintenance-adopt-terminal response)
       (skg--maintenance-handle-terminal nil payload))
      ("idle"
       (if skg--maintenance-client-incident
           (skg--maintenance-finish-idle)
         (skg--maintenance-set-handshake-summary 'idle)
         (message "Skg maintenance is idle")))
      (_ (message "Skg maintenance: %s" payload)))))

(defun skg--maintenance-send-undo-failure
    (incident-id epoch buffer-key reason)
  (when skg--maintenance-client-incident
    (setf (plist-get skg--maintenance-client-incident :undo-failure)
          (list :buffer-key buffer-key :reason reason)))
  (let ((tcp-proc (skg-tcp-connect-to-rust)))
    (skg-register-response-handler
     'maintenance-status
     (lambda (_tcp payload)
       (let* ((response (read payload))
              (key (skg--maintenance-text response 'buffer-key))
              (exact-reason (skg--maintenance-text response 'reason)))
         (run-at-time
          0 nil
          (lambda ()
            (if (yes-or-no-p
                 (format
                  (concat "Native undo could not be archived for %s:\n%s\n"
                          "Continue only with exact text and diff recovery? ")
                  key exact-reason))
                (skg--maintenance-approve-undo-waiver
                 incident-id epoch key exact-reason)
              (skg-cancel-maintenance incident-id epoch))))))
     t)
    (skg-submit-request
     tcp-proc
     (concat
      (prin1-to-string
       `((request . "maintenance archive failed")
         (maintenance-epoch . ,epoch)
         (buffer-key . ,buffer-key)
         (reason . ,reason)))
      "\n")
     nil incident-id)))

(defun skg--maintenance-approve-undo-waiver
    (incident-id epoch buffer-key reason)
  (let ((tcp-proc (skg-tcp-connect-to-rust)))
    (skg-register-response-handler
     'maintenance-status
     (lambda (_tcp _payload)
       (push (cons buffer-key reason)
             (plist-get skg--maintenance-client-incident :undo-waivers))
       (run-at-time 0 nil #'skg--maintenance-publish-initial))
     t)
    (skg-submit-request
     tcp-proc
     (concat
      (prin1-to-string
       `((request . "approve undo waiver")
         (maintenance-epoch . ,epoch)
         (buffer-key . ,buffer-key)
         (reason . ,reason)))
      "\n")
     nil incident-id)))

(defun skg--maintenance-handle-bootstrap
    (_tcp-proc payload &optional terminal-callback origin-context)
  (let* ((response (read payload))
         (status (skg--maintenance-text response 'status))
         (incident-id (skg--maintenance-text response
                                             'allocated-incident-id))
         (epoch (skg--maintenance-field response 'maintenance-epoch))
         (offer (skg--maintenance-offer-for-writer response)))
    (pcase status
      ("install-maintenance-epoch-and-submit-locked-census"
       (when skg--maintenance-client-incident
         (error "Another client-known maintenance incident is active"))
       (setq skg--maintenance-client-incident
             (list :incident-id incident-id
                   :epoch epoch
                   :origin (skg--maintenance-text response 'origin)
                   :requested-paths
                   (skg--maintenance-string-list response 'requested-paths)
                   :requested-ids
                   (skg--maintenance-string-list response 'requested-ids)
                   :phase 'awaiting-locked-census
                   :offer offer
                   :undo-waivers nil
                   :undo-failure nil
                   :registered-buffer-ids nil
                   :g1-graph-generation nil
                   :g1-manifest-revision nil
                   :tantivy-generation nil
                   :server-evidence-sha256 nil
                   :scalar-challenge nil
                   :settlements nil
                   :pending-settlements nil
                   :acknowledged-settlements nil
                   :locally-applied nil
                   :in-flight-settlement nil
                   :archive nil
                   :evidence nil
                   :final-archive nil
                   :terminal nil
                   :origin-context origin-context
                   :terminal-callback terminal-callback
                   :terminal-callback-fired nil))
       (skg--maintenance-set-handshake-summary 'active epoch)
       (dolist (buffer (skg-registered-buffers))
         (skg-lock-buffer-for-maintenance buffer epoch))
       (skg--submit-buffer-census
        (skg-tcp-connect-to-rust) incident-id epoch))
      ("locked-census-accepted-publish-initial-archive"
       (skg--maintenance-require-client-incident incident-id epoch)
       (unless (and (equal offer
                           (plist-get skg--maintenance-client-incident :offer))
                    (equal (skg--maintenance-text response 'origin)
                           (plist-get skg--maintenance-client-incident :origin))
                    (equal (skg--maintenance-string-list
                            response 'requested-paths)
                           (plist-get skg--maintenance-client-incident
                                      :requested-paths))
                    (equal (skg--maintenance-string-list response 'requested-ids)
                           (plist-get skg--maintenance-client-incident
                                      :requested-ids)))
         (error "Maintenance locked-census offer changed bootstrap authority"))
       (setf (plist-get skg--maintenance-client-incident
                        :registered-buffer-ids)
             (skg--maintenance-string-list response 'registered-buffer-ids))
       (condition-case error-data
           (progn
             (setf (plist-get skg--maintenance-client-incident
                              :lock-census-sha256)
                   (skg--maintenance-lock-offer response)
                   (plist-get skg--maintenance-client-incident :phase)
                   'preparing-archive)
             (skg--maintenance-publish-initial))
         (error
          (display-warning 'skg (error-message-string error-data) :error)
          (run-at-time 0 nil #'skg-cancel-maintenance incident-id epoch))))
      (_ (error "Unexpected maintenance bootstrap status %S" status)))))

(defun skg-begin-maintenance
    (origin &optional candidate-id paths ids terminal-callback origin-context
            origin-fields)
  "Begin server-owned maintenance for ORIGIN.
CANDIDATE-ID accepts a pending observation.  PATHS and IDS are the exact
targets of an explicit partial reload.  TERMINAL-CALLBACK receives the parsed
terminal response only after the server completes the incident.
ORIGIN-CONTEXT is opaque client state retained across the origin adapter.
ORIGIN-FIELDS are adapter-specific fields included in the bootstrap request."
  (skg--maintenance-refuse-modified-raw-files)
  (let ((tcp-proc (skg-tcp-connect-to-rust)))
    (skg-register-response-handler
     'maintenance-offer
     (lambda (tcp payload)
       (skg--maintenance-handle-bootstrap
        tcp payload terminal-callback origin-context))
     t)
    (skg-submit-request
     tcp-proc
     (concat
      (prin1-to-string
       (append
        `((request . "begin maintenance")
          (origin . ,origin)
          (candidate-id . ,(or candidate-id "none")))
        (when paths `((paths ,@paths)))
        (when ids `((ids ,@ids)))
        origin-fields))
      "\n"))))

(defun skg-reconcile-pending-changes ()
  "Accept the latest server-owned valid disk candidate."
  (interactive)
  (unless skg--pending-maintenance-offer
    (user-error "Skg has no pending valid disk candidate in this client"))
  (let* ((candidate (plist-get skg--pending-maintenance-offer :candidate-id))
         (changed (plist-get skg--pending-maintenance-offer :changed-ids)))
    (when (yes-or-no-p
           (format "Archive dirty work and reconcile candidate %s (%s)? "
                   candidate
                   (if changed (string-join changed ", ") "semantic changes")))
      (skg-begin-maintenance "pending-reconciliation" candidate))))

(defun skg--maintenance-server-offer (_tcp-proc payload)
  (let* ((response (read payload))
         (candidate (skg--maintenance-text response 'candidate-id))
         (changed (skg--maintenance-string-list response
                                                'changed-primary-ids)))
    (setq skg--pending-maintenance-offer
          (list :candidate-id candidate :changed-ids changed))
    (run-at-time
     0 nil
     (lambda ()
       (when (yes-or-no-p
              (format
               "Skg observed disk changes to %s. Archive dirty work and reconcile now? "
               (if changed (string-join changed ", ") "the selected corpus")))
         (skg-begin-maintenance "pending-reconciliation" candidate))))))

(defun skg--maintenance-server-status (tcp-proc payload)
  "Dispatch one unsolicited durable maintenance status PAYLOAD."
  (let* ((response (read payload))
         (status (skg--maintenance-text response 'status)))
    (pcase status
      ((or "candidate-selected" "needs-scalar-authorization"
           "view-enrollment-pending")
       (unless (and (assoc 'incident-id response)
                    (assoc 'maintenance-epoch response))
         (error "Asynchronous maintenance selection has no exact envelope"))
       (skg--maintenance-handle-selection-response tcp-proc payload))
      ("origin-operation-failed"
       (skg--maintenance-require-client-incident
        (skg--maintenance-text response 'incident-id)
        (skg--maintenance-field response 'maintenance-epoch))
       (setf (plist-get skg--maintenance-client-incident :phase)
             'server-blocked
             (plist-get skg--maintenance-client-incident :server-phase)
             (skg--maintenance-text response 'phase)
             (plist-get skg--maintenance-client-incident :blocking-reason)
             (skg--maintenance-text response 'error))
       (display-warning
        'skg
        (format
         (concat "Maintenance remains locked in server phase %s: %s. "
                 "Repair the reported problem, then run "
                 "M-x skg-retry-maintenance.")
         (skg--maintenance-text response 'phase)
         (skg--maintenance-text response 'error))
        :error)
       (when-let ((retirements
                   (skg--maintenance-field
                    response 'preselection-retirements)))
         (skg--maintenance-install-preselection-retirements retirements)))
      ((or "active" "terminal" "idle")
       (skg--maintenance-handle-status tcp-proc payload))
      (_
       (if-let ((reason (skg--maintenance-text response 'pending-reason)))
           (display-warning
            'skg (format "Skg disk observation is pending: %s" reason)
            :warning)
         (message "Skg maintenance status: %s" payload))))))

(defun skg-cancel-maintenance (&optional incident-id epoch)
  "Cancel an incident which has not published ARCHIVE-READY."
  (interactive)
  (let* ((state skg--maintenance-client-incident)
         (incident-id (or incident-id (plist-get state :incident-id)))
         (epoch (or epoch (plist-get state :epoch))))
    (unless (and incident-id epoch)
      (user-error "No client-known maintenance incident to cancel"))
    (let ((tcp-proc (skg-tcp-connect-to-rust)))
      (skg-register-response-handler
       'maintenance-status
       (lambda (_tcp payload)
         (let* ((response (read payload))
                (unlock-epoch (skg--maintenance-field
                               response 'unlock-maintenance-epoch)))
           (dolist (buffer (skg-registered-buffers))
             (skg-unlock-buffer-after-maintenance buffer unlock-epoch))
           (setq skg--maintenance-client-incident nil)
           (message "Skg maintenance cancelled before archive publication.")))
       t)
      (skg-submit-request
       tcp-proc
       (concat
        (prin1-to-string
         `((request . "cancel maintenance")
           (maintenance-epoch . ,epoch)))
        "\n")
       nil incident-id))))

(defun skg--maintenance-handle-retry (_tcp-proc payload)
  "Resume asynchronous maintenance handling after a retry PAYLOAD."
  (let* ((response (read payload))
         (status (skg--maintenance-text response 'status))
         (incident-id (skg--maintenance-text response 'incident-id))
         (epoch (skg--maintenance-field response 'maintenance-epoch))
         (mode (skg--maintenance-text response 'recovery-mode)))
    (skg--maintenance-require-client-incident incident-id epoch)
    (unless (equal status "maintenance-retry-queued")
      (error "Server did not queue blocked maintenance recovery"))
    (setf (plist-get skg--maintenance-client-incident :phase)
          'waiting-for-origin-observation
          (plist-get skg--maintenance-client-incident :server-phase) nil
          (plist-get skg--maintenance-client-incident :blocking-reason) nil)
    (message "Skg queued %s maintenance recovery for %s"
             mode incident-id)))

(defun skg-retry-maintenance ()
  "Retry a repaired invalid-disk or store-health maintenance block.
The server preserves the incident and epoch, observes fresh disk, and never
repeats the incident's external origin operation."
  (interactive)
  (let* ((state skg--maintenance-client-incident)
         (incident-id (plist-get state :incident-id))
         (epoch (plist-get state :epoch)))
    (unless (and state incident-id epoch
                 (eq (plist-get state :phase) 'server-blocked))
      (user-error "Skg has no client-known blocked maintenance incident"))
    (setf (plist-get state :phase) 'maintenance-retry-pending)
    (let ((tcp-proc (skg-tcp-connect-to-rust)))
      (skg-register-response-handler
       'maintenance-status #'skg--maintenance-handle-retry t)
      (skg-set-request-failure-handler
       (lambda (reason)
         (when skg--maintenance-client-incident
           (setf (plist-get skg--maintenance-client-incident :phase)
                 'server-blocked))
         (display-warning
          'skg (format "Maintenance retry was not acknowledged: %s" reason)
          :warning)))
      (skg-submit-request
       tcp-proc
       (concat
        (prin1-to-string
         `((request . "retry maintenance")
           (maintenance-epoch . ,epoch)))
        "\n")
       nil incident-id))))

(defun skg-maintenance-status (&optional quiet)
  "Ask the server for its current durable maintenance state."
  (interactive)
  (let ((tcp-proc (skg-tcp-connect-to-rust)))
    (skg-register-response-handler
     'maintenance-status #'skg--maintenance-handle-status t)
    (when quiet
      (skg-set-request-failure-handler
       (lambda (reason)
         (display-warning
          'skg (format "Could not resume durable maintenance status: %s"
                       reason)
          :warning))))
    (skg-submit-request
     tcp-proc "((request . \"maintenance status\"))\n" nil
     (and skg--maintenance-client-incident
          (plist-get skg--maintenance-client-incident :incident-id)))))

(skg-register-server-push-handler
 'maintenance-offer #'skg--maintenance-server-offer)
(skg-register-server-push-handler
 'maintenance-status #'skg--maintenance-server-status)

(provide 'skg-maintenance)
