;;; skg-maintenance.el --- Maintenance epoch/archive protocol -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)
(require 'skg-buffer-registry)
(require 'skg-length-prefix)
(require 'skg-recovery-archive)
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
     (lambda (_tcp payload)
       (let ((response (read payload)))
         (setf (plist-get skg--maintenance-client-incident :phase)
               'archive-ready)
         (message "Skg server verified recovery archive %s (%s bytes)."
                  (skg--maintenance-text response
                                         'verified-manifest-sha256)
                  (skg--maintenance-field response
                                          'archive-file-bytes))))
     t)
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

(defun skg--maintenance-send-undo-failure
    (incident-id epoch buffer-key reason)
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
                :undo-waivers nil))
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

(defun skg-maintenance-status ()
  "Ask the server for its current durable maintenance state."
  (interactive)
  (let ((tcp-proc (skg-tcp-connect-to-rust)))
    (skg-register-response-handler
     'maintenance-status
     (lambda (_tcp payload)
       (message "Skg maintenance: %s" payload))
     t)
    (skg-submit-request tcp-proc "((request . \"maintenance status\"))\n")))

(skg-register-server-push-handler
 'maintenance-offer #'skg--maintenance-server-offer)

(provide 'skg-maintenance)
