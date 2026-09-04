;;; skg-maintenance.el --- Maintenance epoch/archive protocol -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)
(require 'skg-buffer-registry)
(require 'skg-length-prefix)
(require 'skg-recovery-archive)
(require 'skg-request-save)
(require 'skg-state)

(defvar skg--maintenance-client-incident nil
  "Client facts for the active server-owned maintenance incident.")
(defvar skg--pending-maintenance-offer nil
  "Latest unsolicited valid disk candidate offered by the server.")

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

(defun skg-resume-maintenance-after-census ()
  "Resume durable maintenance only after reconnect census completes."
  (let ((state (and (listp skg--maintenance-state)
                    (format "%s"
                            (cdr (assq 'state skg--maintenance-state))))))
    (when (member state '("active" "terminal"))
      (skg-maintenance-status t))))

(defun skg--maintenance-registered-ids ()
  (sort (mapcar (lambda (buffer)
                  (with-current-buffer buffer
                    (skg--buffer-record-id skg--buffer-record)))
                (skg-registered-buffers))
        #'string<))

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
                offer :undo-waivers (plist-get state :undo-waivers))))
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
    (unless (and (natnump (plist-get values :g1-graph-generation))
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
    (setf (plist-get state :phase) 'presenting)
    (setq skg--maintenance-client-incident state)
    state))

(defun skg--maintenance-handle-selection-response (_tcp-proc payload)
  "Continue maintenance after selection or a scalar-approval retry."
  (let* ((response (read payload))
         (status (skg--maintenance-text response 'status))
         (state skg--maintenance-client-incident))
    (unless state (error "Maintenance selection arrived without client state"))
    (unless (equal status "archive-ready")
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
       (message "Skg maintenance archive is durable; its origin operation is next"))
      (_ (error "Unexpected maintenance selection status: %S" status)))))

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
            (required (skg--maintenance-text settlement 'required-ack)))
        (unless (and buffer-id
                     (member required '("retirement-ack" "release-ack"
                                        "application-ack" "close-ack"))
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
  (cl-remove-if (lambda (field) (eq (car-safe field) 'acknowledged))
                settlement))

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
        (unless (and prior
                     (equal
                      (skg--maintenance-settlement-without-ack prior)
                      (skg--maintenance-settlement-without-ack settlement)))
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
          (progn
            (unless (member (skg--maintenance-text settlement 'buffer-id)
                            (plist-get state :locally-applied))
              (error "Server acknowledged a settlement not applied locally"))
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
         (buffer-id (skg--maintenance-text settlement 'buffer-id)))
    (unless (and settlement
                 (equal buffer-id
                        (skg--maintenance-text response 'buffer-id))
                 (equal (skg--maintenance-text settlement 'required-ack)
                        (skg--maintenance-text response 'required-ack)))
      (error "Maintenance settlement ACK response changed identity"))
    (unless (equal buffer-id
                   (skg--maintenance-text
                    (car (plist-get state :pending-settlements)) 'buffer-id))
      (error "Maintenance settlement response arrived out of order"))
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
         (state (skg--maintenance-require-client-incident
                 incident-id epoch))
         (settlements (skg--maintenance-field response 'view-settlements))
         (has-scalar (assoc 'scalar-approved response)))
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
     ((equal phase "preparing-archive")
      (setf (plist-get state :phase) 'preparing-archive)
      (if-let ((archive (plist-get state :archive)))
          (run-at-time
           0 nil #'skg--maintenance-send-archive-ready
           incident-id epoch
           (plist-get state :lock-census-sha256)
           (plist-get archive :manifest-sha256))
        (run-at-time 0 nil #'skg--maintenance-publish-initial)))
     ((member phase '("blocked-invalid-after-mutation"
                      "blocked-store-health"))
      (setf (plist-get state :phase) 'server-blocked)
      (display-warning
       'skg
       (format "Maintenance %s remains locked in server phase %s"
               incident-id phase)
       :error))
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
       (unless skg--maintenance-client-incident
         (error "Server has terminal maintenance unknown to this client"))
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

(defun skg--maintenance-handle-bootstrap (_tcp-proc payload)
  (let* ((response (read payload))
         (incident-id (skg--maintenance-text response
                                             'allocated-incident-id))
         (epoch (skg--maintenance-field response 'maintenance-epoch))
         (offer (skg--maintenance-offer-for-writer response)))
    (setq skg--maintenance-client-incident
          (list :incident-id incident-id
                :epoch epoch
                :phase 'preparing-archive
                :offer offer
                :undo-waivers nil
                :undo-failure nil
                :registered-buffer-ids
                (skg--maintenance-string-list
                 response 'registered-buffer-ids)
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
                :terminal nil))
    (condition-case error-data
        (progn
          (setf (plist-get skg--maintenance-client-incident
                           :lock-census-sha256)
                (skg--maintenance-lock-offer response))
          (skg--maintenance-publish-initial))
      (error
       (display-warning 'skg (error-message-string error-data) :error)
       (run-at-time 0 nil #'skg-cancel-maintenance incident-id epoch)))))

(defun skg-begin-maintenance (origin &optional candidate-id)
  "Begin server-owned maintenance for ORIGIN and optional CANDIDATE-ID."
  (let ((tcp-proc (skg-tcp-connect-to-rust)))
    (skg-register-response-handler
     'maintenance-offer #'skg--maintenance-handle-bootstrap t)
    (skg-submit-request
     tcp-proc
     (concat
      (prin1-to-string
       `((request . "begin maintenance")
         (origin . ,origin)
         (candidate-id . ,(or candidate-id "none"))))
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

(provide 'skg-maintenance)
