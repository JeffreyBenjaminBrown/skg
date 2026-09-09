;;; -*- lexical-binding: t; -*-
;;;
;;; Durable query-wait client state.  A wait keeps only its recipe and
;;; operation identity until the server publishes a result.

(require 'cl-lib)
(require 'org-id)
(require 'skg-state)
(require 'skg-buffer)
(require 'skg-buffer-registry)

(defvar skg--query-waits (make-hash-table :test #'equal)
  "Client query waits keyed by durable query-operation-id.")

(defvar-local skg--query-operation-id nil)
(put 'skg--query-operation-id 'permanent-local t)

(defconst skg-query-wait--known-statuses
  '(pending blocked executing ready delivered destination-rejected)
  "Statuses which may be retained for a durable query wait.")

(defun skg-query-wait-policy-choice ()
  "Choose current snapshot or a durable wait during reconciliation."
  (if (skg-query-wait-reconciliation-active-p)
      (if (y-or-n-p
           "Graph/search is rebuilding. Use the current snapshot? (No waits.) ")
          "current"
        "wait")
    "current"))

(defun skg-query-wait-reconciliation-active-p ()
  "Return non-nil while any coordinated graph transition is pending."
  (or skg--rebuilding
      (and (boundp 'skg--maintenance-client-incident)
           skg--maintenance-client-incident
           (not (eq (plist-get skg--maintenance-client-incident :phase)
                    'terminal)))
      (and skg--graph-transition-status
           (not (equal (format "%s" skg--graph-transition-status) "idle")))
      (eq skg--graph-write-admission 'closed)))

(defun skg-query-wait--field (response key)
  (let ((entry (assoc key response)))
    (when entry
      (let ((value (cadr entry)))
        (if (symbolp value) (symbol-name value) value)))))

(defun skg-query-wait--entry-field (entry key)
  "Read KEY from either proper or dotted alist ENTRY."
  (let ((field (assoc key entry)))
    (and field
         (let ((value (if (and (proper-list-p field) (= (length field) 2))
                          (cadr field)
                        (cdr field))))
           (if (symbolp value) (symbol-name value) value)))))

(defun skg-query-wait--target ()
  "Return the active incident or retained candidate-only wait target."
  (or (when (and (boundp 'skg--maintenance-client-incident)
                 skg--maintenance-client-incident
                 (not (eq (plist-get skg--maintenance-client-incident :phase)
                          'terminal)))
        (let ((incident skg--maintenance-client-incident))
          (list :incident-id (plist-get incident :incident-id)
                :maintenance-epoch (plist-get incident :epoch)
                :candidate-id (plist-get incident :candidate-id))))
      (cl-loop for entry in skg--pending-incidents
               for phase = (skg-query-wait--entry-field entry 'phase)
               ;; `pending-incidents' is report-only and may contain an
               ;; older incident.  Only an explicitly active ID is a target.
               for incident-id =
               (skg-query-wait--entry-field entry 'active-incident-id)
               for candidate-id =
               (skg-query-wait--entry-field entry 'candidate-id)
               when (and (not (equal (format "%s" phase) "terminal"))
                         (or incident-id candidate-id))
               return (list :incident-id incident-id
                            :maintenance-epoch
                            (skg-query-wait--entry-field
                             entry 'maintenance-epoch)
                            :candidate-id candidate-id))
      (when (and (boundp 'skg--pending-maintenance-offer)
                 skg--pending-maintenance-offer)
        (list :candidate-id
              (plist-get skg--pending-maintenance-offer :candidate-id)))))

(defun skg-query-wait--recipe (terms regex body operators ugly-choice)
  `((kind . "text-search")
    (terms . ,terms)
    (regex . ,(if regex "true" "false"))
    (body . ,(if body "true" "false"))
    (operators . ,(if operators "true" "false"))
    (ugly-telescopes . ,(or ugly-choice "default"))
    (source-set . ,skg--active-source-set-name)))

(defun skg-query-wait--recipe-text (recipe)
  ;; Query-wait's Rust parser accepts the canonical dotted-pair recipe
  ;; grammar.  The shared buffer census spelling intentionally uses proper
  ;; two-element lists, so convert only this wire boundary.
  (let ((normalized (skg--normalized-recipe-value recipe)))
    (prin1-to-string
     (mapcar (lambda (entry) (cons (car entry) (cadr entry))) normalized))))

(defun skg-query-wait--recipe-digest (recipe)
  (secure-hash 'sha256
               (encode-coding-string
                (skg-query-wait--recipe-text recipe) 'utf-8-unix)))

(defun skg-query-wait--destination (buffer)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((record skg--buffer-record))
        `((client-buffer-id
           . ,(skg--buffer-record-id record))
          (view-uri . ,(skg--buffer-record-view-uri record))
          (server-session-id
           . ,(skg--buffer-record-server-session-id record))
          (client-application-token
           . ,(skg--buffer-record-application-token record))
          (graph-generation
           . ,(skg--buffer-record-graph-generation record))
          (presentation-generation
           . ,(skg--buffer-record-presentation-generation record))
          (server-revision
           . ,(skg--buffer-record-server-revision record))
          (source-set . ,(skg--buffer-record-source-set record))
          (base-content-sha256
           . ,(skg--buffer-record-last-fetched-sha256 record))
          (destination-state
           . ,(if (buffer-modified-p) "dirty" "clean")))))))

(defun skg-query-wait--request-fields (record)
  (let* ((recipe (plist-get record :recipe))
         (target (plist-get record :target))
         (destination (plist-get record :destination)))
    `((request . "query wait")
      (query-operation-id . ,(plist-get record :query-operation-id))
      ,@(when (plist-get target :incident-id)
          `((incident-id . ,(plist-get target :incident-id))))
      ,@(when (plist-get target :maintenance-epoch)
          `((maintenance-epoch . ,(plist-get target :maintenance-epoch))))
      ,@(when (plist-get target :candidate-id)
          `((candidate-id . ,(plist-get target :candidate-id))))
      (outcome-kind . "graph-publication")
      (query-recipe . ,(skg-query-wait--recipe-text recipe))
      (query-recipe-digest . ,(plist-get record :recipe-digest))
      ,@(when destination destination))))

(defun skg-query-wait--status-fields (record)
  "Build a recovery-safe status request for RECORD.
Unknown or restarted waits may have no local recipe or destination yet."
  (let ((destination (skg-query-wait--destination (plist-get record :buffer))))
    `((request . "query wait status")
      (query-operation-id . ,(plist-get record :query-operation-id))
      ,@(when (and skg--server-session-id (null destination))
          `((server-session-id . ,skg--server-session-id)))
      ,@(when (plist-get record :recipe-digest)
          `((query-recipe-digest . ,(plist-get record :recipe-digest))))
      ,@destination)))

(defun skg-query-wait-ingest-pending (entries)
  "Ingest verified coarse server wait summaries without creating buffers.
Only durable operation identity and status are retained until an authenticated
status response supplies a verifiable recipe."
  (dolist (entry entries)
    (let* ((operation-id
            (skg-query-wait--entry-field entry 'query-operation-id))
           (status (intern (format "%s"
                                  (skg-query-wait--entry-field entry 'status))))
           (incident-id (skg-query-wait--entry-field entry 'incident-id))
           (epoch (skg-query-wait--entry-field entry 'maintenance-epoch))
           (candidate-id (skg-query-wait--entry-field entry 'candidate-id)))
      (when (and (stringp operation-id)
                 (memq status '(pending blocked executing ready)))
        (let ((record (or (gethash operation-id skg--query-waits)
                          (list :query-operation-id operation-id
                                :status status :target nil :recipe nil
                                :recipe-text nil :recipe-digest nil
                                :terms nil :result-digest nil :freshness nil
                                :reason nil :buffer nil :destination nil))))
          (setf (plist-get record :status) status)
          (when (or incident-id epoch candidate-id)
            (setf (plist-get record :target)
                  (list :incident-id incident-id
                        :maintenance-epoch epoch
                        :candidate-id candidate-id)))
          (puthash operation-id record skg--query-waits))))))

(defun skg-query-wait--ensure-record (operation-id &optional status)
  "Return OPERATION-ID's record, creating a coarse record when needed."
  (let* ((operation-id (format "%s" operation-id))
         (record (gethash operation-id skg--query-waits)))
    (or record
        (let ((new (list :query-operation-id operation-id
                         :status (or status 'pending) :target nil
                         :recipe nil :recipe-text nil :recipe-digest nil
                         :terms nil :result-digest nil :freshness nil
                         :reason nil :buffer nil :destination nil
                         :last-notified-state nil)))
          (puthash operation-id new skg--query-waits)
          new))))

(defun skg-query-wait--notify-status (record)
  "Show a status change once for RECORD, including unsolicited duplicates."
  (let ((state (list (plist-get record :status)
                     (plist-get record :reason))))
    (unless (equal state (plist-get record :last-notified-state))
      (setf (plist-get record :last-notified-state) state)
      (message "Skg query wait %s: %s%s"
               (plist-get record :query-operation-id)
               (plist-get record :status)
               (if (plist-get record :reason)
                   (format " (%s)" (plist-get record :reason))
                 "")))))

(defun skg-query-wait--record-status (response operation-id)
  (when operation-id
    (let* ((record (skg-query-wait--ensure-record operation-id))
           (recipe-text (skg-query-wait--field response 'query-recipe))
           (recipe-digest (skg-query-wait--field response
                                                  'query-recipe-digest))
           (known-digest (plist-get record :recipe-digest)))
      (when recipe-text
        (unless (and recipe-digest
                     (equal recipe-digest
                            (secure-hash
                             'sha256
                             (encode-coding-string recipe-text 'utf-8-unix))))
          (error "Skg query wait status recipe digest mismatch"))
        (setf (plist-get record :recipe-text) recipe-text
              (plist-get record :recipe-digest) recipe-digest))
      (when (and recipe-digest known-digest
                 (not (equal recipe-digest known-digest)))
        (error "Skg query wait status recipe identity changed"))
      (when (and recipe-digest (not recipe-text) (not known-digest))
        (setf (plist-get record :recipe-digest) recipe-digest))
      (let* ((status (format "%s"
                             (or (skg-query-wait--field response 'status)
                                 (skg-query-wait--field response
                                                         'query-wait-status)
                                 "pending")))
             (incident-id (skg-query-wait--field response 'incident-id))
             (epoch (skg-query-wait--field response 'maintenance-epoch))
             (candidate-id (skg-query-wait--field response 'candidate-id)))
        (setf (plist-get record :status) (intern status)
              (plist-get record :target)
              (if (or incident-id epoch candidate-id)
                  (list :incident-id incident-id :maintenance-epoch epoch
                        :candidate-id candidate-id)
                (plist-get record :target))
              (plist-get record :reason)
              (skg-query-wait--field response 'reason)
              (plist-get record :result-digest)
              (skg-query-wait--field response 'result-digest))
        record))))

(defun skg-query-wait--status-handler (operation-id)
  (lambda (_tcp payload)
    (let ((response (read payload)))
      (skg-require-current-server-session response)
      (skg-query-wait--notify-status
       (skg-query-wait--record-status response operation-id)))))

(defun skg-query-wait--status-push-handler (_tcp payload)
  "Handle an unsolicited durable query-wait status update."
  (let* ((response (read payload))
         (operation-id (skg-query-wait--field response
                                              'query-operation-id)))
    (skg-require-current-server-session response)
    (when operation-id
      (skg-query-wait--notify-status
       (skg-query-wait--record-status response operation-id)))))

(defun skg-query-wait--submit-record (record)
  (let* ((operation-id (plist-get record :query-operation-id))
         (target (plist-get record :target)))
    (skg-register-response-handler
     'query-wait-status (skg-query-wait--status-handler operation-id) t)
    (skg-submit-request
     (skg-tcp-connect-to-rust)
     (concat (prin1-to-string (skg-query-wait--request-fields record)) "\n")
     nil (plist-get target :incident-id))))

(defun skg-query-wait--placeholder (operation-id terms recipe)
  (let* ((uri (concat "search:wait:" operation-id))
         (content (format "* SKG search waiting\n\nTerms: %s\nWait status: pending\n"
                          terms))
         (buffer
          (skg-open-org-buffer-from-text
           nil content
           "*skg-search-wait*"
           uri 'search-view recipe
           (list :view-write-authority 'read-only
                 :server-session-id skg--server-session-id
                 :graph-generation
                 (or (cdr (assq 'graph-generation skg--server-store-state)) 0)
                 :presentation-generation 0 :server-revision 0
                 :application-token 1))))
    (with-current-buffer buffer
      (setq skg--query-operation-id operation-id)
      (setq buffer-read-only t))
    buffer))

(defun skg-query-wait-submit
    (terms regex body operators ugly-choice &optional operation-id)
  "Create a read-only placeholder and submit a durable query wait."
  (unless (skg-query-wait-reconciliation-active-p)
    (user-error "Skg has no pending reconciliation to wait for"))
    (let* ((target (or (skg-query-wait--target)
                     (user-error "Skg reconciliation has no incident target")))
         (operation-id (or operation-id (org-id-uuid)))
         (recipe (skg-query-wait--recipe terms regex body operators ugly-choice))
         (buffer (skg-query-wait--placeholder operation-id terms recipe))
         (record (list :query-operation-id operation-id
                       :status 'pending :target target :recipe recipe
                       :terms terms
                       :recipe-text (skg-query-wait--recipe-text recipe)
                       :recipe-digest (skg-query-wait--recipe-digest recipe)
                       :result-digest nil :freshness nil :reason nil
                       :buffer buffer :destination
                       (skg-query-wait--destination buffer))))
    (puthash operation-id record skg--query-waits)
    (skg-query-wait--submit-record record)
    operation-id))

(defun skg-query-wait--send-applied (tcp response record)
  (let ((operation-id (plist-get record :query-operation-id))
        (digest (or (skg-query-wait--field response 'result-digest)
                    (plist-get record :result-digest)))
        (buffer (plist-get record :buffer)))
    (skg-submit-request
     tcp
     (concat
      (prin1-to-string
       `((request . "query wait applied")
         (query-operation-id . ,operation-id)
         (result-digest . ,digest)
         (applied . "true")
         ,@(skg-query-wait--destination buffer)))
      "\n")
     nil (plist-get (plist-get record :target) :incident-id))))

(defun skg-query-wait--apply-result (tcp response record)
  (let* ((operation-id (skg-query-wait--field response 'query-operation-id))
         (buffer (plist-get record :buffer))
         (content (or (skg-query-wait--field response 'content) ""))
         (digest (skg-query-wait--field response 'result-digest))
         (uri (skg-query-wait--field response 'view-uri))
         (buffer-id (skg-query-wait--field response 'client-buffer-id))
         (recipe-digest
          (skg-query-wait--field response 'query-recipe-digest))
         (expected-token
          (skg-query-wait--field response 'expected-client-application-token))
         (result-token
          (skg-query-wait--field response 'resulting-client-application-token))
         (expected-graph
          (skg-query-wait--field response 'expected-graph-generation))
         (expected-presentation
          (skg-query-wait--field response 'expected-presentation-generation))
         (expected-revision
          (skg-query-wait--field response 'expected-server-revision))
         (result-graph (skg-query-wait--field response 'graph-generation))
         (result-presentation
          (skg-query-wait--field response 'presentation-generation))
         (result-revision
          (or (skg-query-wait--field response 'server-revision)
              (skg-query-wait--field response 'manifest-revision)))
         (result-source (skg-query-wait--field response 'source-set))
         (base-content-digest
          (skg-query-wait--field response 'base-content-sha256))
         (freshness (skg-query-wait--field response 'freshness)))
    (unless (and (buffer-live-p buffer) operation-id uri buffer-id
                 expected-token result-token recipe-digest expected-graph
                 expected-presentation expected-revision result-graph
                 result-presentation result-revision result-source
                 base-content-digest freshness digest
                 (equal operation-id
                        (buffer-local-value 'skg--query-operation-id buffer))
                 (equal uri (buffer-local-value 'skg-view-uri buffer))
                 (equal buffer-id
                        (skg--buffer-record-id
                         (buffer-local-value 'skg--buffer-record buffer)))
                 (equal expected-token
                        (skg--buffer-record-application-token
                         (buffer-local-value 'skg--buffer-record buffer)))
                 (equal recipe-digest (plist-get record :recipe-digest))
                 (equal expected-graph
                        (skg--buffer-record-graph-generation
                         (buffer-local-value 'skg--buffer-record buffer)))
                 (equal expected-presentation
                        (skg--buffer-record-presentation-generation
                         (buffer-local-value 'skg--buffer-record buffer)))
                 (equal expected-revision
                        (skg--buffer-record-server-revision
                         (buffer-local-value 'skg--buffer-record buffer)))
                 (equal result-source
                        (skg--buffer-record-source-set
                         (buffer-local-value 'skg--buffer-record buffer)))
                 (equal base-content-digest
                        (skg--buffer-record-last-fetched-sha256
                         (buffer-local-value 'skg--buffer-record buffer)))
                 (equal digest
                        (secure-hash 'sha256
                                     (encode-coding-string content 'utf-8-unix))))
      (setf (plist-get record :status) 'destination-rejected)
      (error "Skg query wait destination base or result digest changed"))
    (when (buffer-modified-p buffer)
      (setf (plist-get record :status) 'destination-rejected)
      (error "Skg query wait refuses to overwrite edited text"))
    (let ((installed-token
           (skg-apply-server-text
            buffer content uri expected-token expected-graph
            expected-presentation expected-revision)))
      (unless (equal installed-token result-token)
        (setf (plist-get record :status) 'destination-rejected)
        (error "Skg query wait result token changed during application"))
      (with-current-buffer buffer
        (setq buffer-read-only t)
        (setf (skg--buffer-record-graph-generation skg--buffer-record)
              result-graph
              (skg--buffer-record-presentation-generation skg--buffer-record)
              result-presentation
              (skg--buffer-record-server-revision skg--buffer-record)
              result-revision
              (skg--buffer-record-view-write-authority skg--buffer-record)
              'read-only
              (skg--buffer-record-source-set skg--buffer-record)
              result-source)))
    (setf (plist-get record :status) 'delivered
          (plist-get record :result-digest) digest
          (plist-get record :freshness) freshness)
    (skg-register-response-handler
     'query-wait-applied (skg-query-wait--status-handler operation-id) t)
    (skg-query-wait--send-applied tcp response record)))

(defun skg-query-wait-result-handler (_tcp payload)
  "Install one staged server result without replacing edited text."
  (let* ((response (read payload))
         (operation-id
          (skg-query-wait--field response 'query-operation-id))
         (record (and operation-id
                      (gethash operation-id skg--query-waits))))
    (unless record
      (message "Skg received an unknown query wait result %s" operation-id)
      (cl-return-from skg-query-wait-result-handler nil))
    (skg-require-current-server-session response)
    (let ((authority (skg-view-write-authority-from-response response)))
      (unless (eq authority 'read-only)
        (error "Skg query wait result must be read-only")))
    (if (and (eq (plist-get record :status) 'delivered)
             (equal (plist-get record :result-digest)
                    (skg-query-wait--field response 'result-digest)))
        (skg-query-wait--send-applied _tcp response record)
      (condition-case err
          (skg-query-wait--apply-result _tcp response record)
        (error
         (setf (plist-get record :reason) (error-message-string err))
         (message "Skg query wait result kept pending: %s"
                  (error-message-string err)))))))

(defun skg-query-wait-resume-all ()
  "Ask the server for every locally retained wait after census."
  (maphash
   (lambda (_operation-id record)
     (when (memq (plist-get record :status)
                 '(pending blocked ready delivered))
       (when-let ((buffer (plist-get record :buffer)))
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (setf (skg--buffer-record-server-session-id skg--buffer-record)
                   skg--server-session-id))))
       (skg-register-response-handler
        'query-wait-status
        (skg-query-wait--status-handler
         (plist-get record :query-operation-id)) t)
       (skg-submit-request
        (skg-tcp-connect-to-rust)
        (concat
         (prin1-to-string (skg-query-wait--status-fields record))
         "\n")
        nil (plist-get (plist-get record :target) :incident-id))))
   skg--query-waits))

(defun skg-query-wait-recover (operation-id &optional terms)
  "Query OPERATION-ID's status without creating a missing destination.
This is the explicit recovery path after an editor restart or buffer loss."
  (interactive
   (list (read-string "Query operation UUID: ")
         (read-string "Search terms (optional): ")))
  (let ((record (gethash operation-id skg--query-waits)))
    (unless record
      (setq record (list :query-operation-id operation-id :status 'pending
                         :terms (or terms "") :target nil :recipe nil
                         :recipe-digest nil :result-digest nil
                         :freshness nil :reason nil :buffer nil))
      (puthash operation-id record skg--query-waits))
    (skg-query-wait-status operation-id)))

(defun skg-query-wait-cancel (operation-id)
  "Cancel OPERATION-ID explicitly; waits never expire implicitly."
  (interactive
   (list (read-string "Query operation UUID: ")))
  (let ((record (gethash operation-id skg--query-waits)))
    (unless record (user-error "Unknown query operation %s" operation-id))
    (skg-register-response-handler
     'query-wait-status (skg-query-wait--status-handler operation-id) t)
    (skg-submit-request
     (skg-tcp-connect-to-rust)
     (concat
      (prin1-to-string
         `((request . "query wait cancel")
         (query-operation-id . ,operation-id)
         ,@(when skg--server-session-id
             `((server-session-id . ,skg--server-session-id)))
         ,@(when (plist-get (plist-get record :target) :incident-id)
             `((incident-id . ,(plist-get (plist-get record :target)
                                          :incident-id))))
         ,@(when (plist-get (plist-get record :target) :maintenance-epoch)
             `((maintenance-epoch . ,(plist-get (plist-get record :target)
                                                :maintenance-epoch))))
         ,@(when (plist-get (plist-get record :target) :candidate-id)
             `((candidate-id . ,(plist-get (plist-get record :target)
                                           :candidate-id)))))
      "\n")
     nil (plist-get (plist-get record :target) :incident-id)))))

(defun skg-query-wait-status (&optional operation-id)
  "Request durable status, or list retained operation IDs when nil."
  (interactive
   (list (let ((value (read-string "Query operation UUID (empty lists all): ")))
           (unless (string-empty-p value) value))))
  (if (null operation-id)
      (let (statuses)
        (maphash
         (lambda (id record)
           (push (format "%s: %s" id (plist-get record :status)) statuses))
         skg--query-waits)
        (message "Skg query waits: %s" (string-join (sort statuses #'string<) "; ")))
    (let ((record (gethash operation-id skg--query-waits)))
    (unless record (user-error "Unknown query operation %s" operation-id))
    (skg-register-response-handler
     'query-wait-status (skg-query-wait--status-handler operation-id) t)
    (skg-submit-request
     (skg-tcp-connect-to-rust)
      (concat
      (prin1-to-string (skg-query-wait--status-fields record))
      "\n")
     nil (plist-get (plist-get record :target) :incident-id)))))

(skg-register-server-push-handler
 'query-wait-result #'skg-query-wait-result-handler)
(skg-register-server-push-handler
 'query-wait-status #'skg-query-wait--status-push-handler)

(provide 'skg-query-wait)
