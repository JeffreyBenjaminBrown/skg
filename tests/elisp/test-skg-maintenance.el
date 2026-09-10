;;; test-skg-maintenance.el --- Durable maintenance client tests -*- lexical-binding: t; -*-

(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             (file-name-directory load-file-name)))
(require 'ert)
(require 'skg-maintenance)
(require 'skg-request-rebuild-dbs)

(defun skg-test-maintenance--settlement
    (buffer-id kind uri dirty requirement disposition)
  `((buffer-id ,buffer-id)
    (buffer-key "none")
    (kind ,kind)
    (view-uri ,uri)
    (dirty ,dirty)
    (impacted "true")
    (parse-uncertain "nil")
    (uncertainty-reason "none")
    (observed-ids ())
    (resolved-primary-ids ())
    (base-graph-generation 1)
    (base-presentation-generation 3)
    (base-server-revision 4)
    (base-application-token 7)
    (planned-disposition ,disposition)
    (required-ack ,requirement)
    (acknowledged "nil")))

(defmacro skg-test-maintenance--with-buffer (kind &rest body)
  (declare (indent 1))
  `(let ((buffer (generate-new-buffer " *skg-maintenance-test*"))
         (skg--buffer-registry (make-hash-table :test #'equal))
         (skg--server-store-state '((graph-generation . 1)))
         (skg--client-constructor-admission 'open))
     (unwind-protect
         (with-current-buffer buffer
           (org-mode)
           (insert "* Original\n")
           (setq skg-view-uri "view")
           (skg-register-buffer
            buffer ,kind :lifecycle 'live-view :disposable nil
            :view-uri "view" :last-fetched "* Original\n"
            :graph-generation 1 :presentation-generation 3
            :server-revision 4 :application-token 7)
           (skg-lock-buffer-for-maintenance buffer 9)
           (set-buffer-modified-p nil)
           ,@body)
       (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest test-skg-maintenance-retains-replaced-incident-records ()
  (let ((skg--maintenance-client-incident nil)
        (skg--maintenance-client-incidents nil)
        (incident-a '(:incident-id "incident-a" :phase settling
                      :archive (:path "/a") :locally-applied ("buffer-a")))
        (incident-b '(:incident-id "incident-b" :phase awaiting-census)))
    (skg--maintenance-replace-current-incident incident-a)
    ;; Extending the active plist may change its head without updating the index.
    (setq skg--maintenance-client-incident
          (append '(:settlements ("settlement-a")) incident-a))
    (should (equal '("settlement-a")
                   (plist-get (skg--maintenance-lookup-incident "incident-a")
                              :settlements)))
    (skg--maintenance-replace-current-incident incident-b)
    (should (equal '("settlement-a")
                   (plist-get (skg--maintenance-lookup-incident "incident-a")
                              :settlements)))
    (should (eq incident-b skg--maintenance-client-incident))
    ;; A later plist extension must be visible through current-ID lookup.
    (setq skg--maintenance-client-incident
          (append '(:settlements ("settlement-b")) incident-b))
    (should (equal '("settlement-b")
                   (plist-get (skg--maintenance-lookup-incident "incident-b")
                              :settlements)))
    (should (equal '("settlement-b")
                   (plist-get (car (skg--maintenance-list-incidents))
                              :settlements)))
    (skg--maintenance-clear-current-incident)
    (should-not skg--maintenance-client-incident)
    (should (equal '("settlement-a")
                   (plist-get (skg--maintenance-lookup-incident "incident-a")
                              :settlements)))
    (should (equal '("settlement-b")
                   (plist-get (skg--maintenance-lookup-incident "incident-b")
                              :settlements)))
    (should (= 2 (length (skg--maintenance-list-incidents))))))

(ert-deftest test-skg-maintenance-scopes-retained-callback-and-follow-on ()
  (let* ((incident-a (list :incident-id "incident-a" :phase 'waiting))
         (incident-b (list :incident-id "incident-b" :phase 'foreground))
         (scheduled-function nil)
         (scheduled-args nil)
         (skg--maintenance-client-incident nil)
         (skg--maintenance-client-incidents nil))
    (skg--maintenance-replace-current-incident incident-a)
    (skg--maintenance-replace-current-incident incident-b)
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (_seconds _repeat function &rest args)
                 (setq scheduled-function function
                       scheduled-args args)))
              ((symbol-function 'skg--maintenance-resume-active)
               (lambda (_response)
                 (setf (plist-get skg--maintenance-client-incident :phase)
                       'resumed)
                 (skg--maintenance-defer
                  "incident-a"
                  (lambda ()
                    (setf (plist-get skg--maintenance-client-incident
                                     :follow-on)
                          'ran))))))
      (skg--maintenance-handle-status
       nil "((status active) (active-incident-id incident-a)
              (maintenance-epoch 1))")
      (should (eq skg--maintenance-client-incident incident-b))
      (should (eq (plist-get incident-a :phase) 'resumed))
      (apply scheduled-function scheduled-args)
      (should (eq (plist-get (skg--maintenance-lookup-incident "incident-a")
                             :follow-on)
                 'ran))
      (should (eq skg--maintenance-client-incident incident-b)))))

(ert-deftest test-skg-maintenance-restores-foreground-after-historical-error ()
  (let ((incident-a '(:incident-id "incident-a" :phase waiting))
        (incident-b '(:incident-id "incident-b" :phase foreground))
        (skg--maintenance-client-incident nil)
        (skg--maintenance-client-incidents nil))
    (skg--maintenance-replace-current-incident incident-a)
    (skg--maintenance-replace-current-incident incident-b)
    (cl-letf (((symbol-function 'skg--maintenance-resume-active)
               (lambda (_response)
                 (setf (plist-get skg--maintenance-client-incident :phase)
                       'failed)
                 (error "historical callback failed"))))
      (should-error
       (skg--maintenance-handle-status
        nil "((status active) (active-incident-id incident-a))")))
    (should (eq skg--maintenance-client-incident incident-b))
    (should (eq (plist-get (skg--maintenance-lookup-incident "incident-a")
                           :phase)
                'failed))))

(ert-deftest test-skg-maintenance-historical-status-preserves-foreground-state ()
  (let ((incident-a '(:incident-id "incident-a" :phase terminal-received
                      :final-archive (:path "/a")))
        (incident-b '(:incident-id "incident-b" :phase foreground))
        (skg--maintenance-client-incident nil)
        (skg--maintenance-client-incidents nil)
        (skg--pending-maintenance-offer '(:candidate-id "newer")))
    (skg--maintenance-replace-current-incident incident-a)
    (skg--maintenance-replace-current-incident incident-b)
    (skg--maintenance-handle-status
     nil "((status idle) (incident-id incident-a))")
    (should (eq skg--maintenance-client-incident incident-b))
    (should (equal "newer"
                   (plist-get skg--pending-maintenance-offer :candidate-id)))
    (should (eq (plist-get incident-b :phase) 'foreground))))

(ert-deftest test-skg-begin-maintenance-sends-exact-explicit-targets ()
  (let (request registered-handler)
    (cl-letf (((symbol-function 'skg-tcp-connect-to-rust)
               (lambda () 'tcp))
              ((symbol-function 'skg-register-response-handler)
               (lambda (kind handler &optional _one-shot)
                 (should (eq kind 'maintenance-offer))
                 (setq registered-handler handler)))
              ((symbol-function 'skg-submit-request)
               (lambda (_tcp text &optional _content _incident)
                 (setq request text))))
      (skg-begin-maintenance
       "explicit-partial-reload" nil '("source/A.skg") '("alias" "B")
       #'ignore))
    (let ((parsed (read request)))
      (should (equal (cdr (assoc 'origin parsed))
                     "explicit-partial-reload"))
      (should (equal (cdr (assoc 'paths parsed)) '("source/A.skg")))
      (should (equal (cdr (assoc 'ids parsed)) '("alias" "B"))))
    (should (functionp registered-handler))))

(ert-deftest test-skg-maintenance-retains-independent-buffer-restrictions ()
  (skg-test-maintenance--with-buffer 'content-view
    (skg-unlock-buffer-after-maintenance buffer 9)
    (skg-lock-buffer-for-maintenance buffer 9 "incident-a")
    (skg-lock-buffer-for-maintenance buffer 10 "incident-b")
    (skg-lock-buffer-for-maintenance buffer 10 "incident-b")
    (should (= 2 (length skg--maintenance-restrictions)))
    (skg-unlock-buffer-after-maintenance buffer 10 "incident-b")
    (should (= 9 (skg--buffer-record-maintenance-epoch skg--buffer-record)))
    (should skg--maintenance-lock-overlay)
    (skg-unlock-buffer-after-maintenance buffer 9 "incident-a")
    (should-not (skg--buffer-record-maintenance-epoch skg--buffer-record))
    (should-not skg--maintenance-lock-overlay)
    (skg-lock-buffer-for-maintenance buffer 9 "incident-a")
    (skg-lock-buffer-for-maintenance buffer 10 "incident-b")
    (skg-unlock-buffer-after-maintenance buffer 9 "incident-a")
    (should (= 10 (skg--buffer-record-maintenance-epoch skg--buffer-record)))
    (skg-unlock-buffer-after-maintenance buffer 10 "incident-b")
    (should-not skg--maintenance-lock-overlay)))

(ert-deftest test-skg-maintenance-preserves-original-read-only-state ()
  (skg-test-maintenance--with-buffer 'content-view
    (setq buffer-read-only t)
    (skg-unlock-buffer-after-maintenance buffer 9)
    (skg-lock-buffer-for-maintenance buffer 9 "incident-read-only")
    (skg-unlock-buffer-after-maintenance buffer 9 "incident-read-only")
    (should buffer-read-only)
    (should-not skg--maintenance-lock-overlay)))

(ert-deftest test-skg-maintenance-old-api-finds-unique-epoch-obligation ()
  (skg-test-maintenance--with-buffer 'content-view
    (skg-unlock-buffer-after-maintenance buffer 9)
    (skg-lock-buffer-for-maintenance buffer 9 "incident-a")
    (skg-lock-buffer-for-maintenance buffer 10 "incident-b")
    ;; The old API supplies only an epoch while the current incident is B.
    (let ((skg--maintenance-client-incident '(:incident-id "incident-b")))
      (skg-unlock-buffer-after-maintenance buffer 9)
      (should (= 10 (skg--buffer-record-maintenance-epoch
                     skg--buffer-record))))
    (skg-unlock-buffer-after-maintenance buffer 10 "incident-b")
    (should-not skg--maintenance-lock-overlay)))

(ert-deftest test-skg-maintenance-migrates-legacy-epoch-key-on-relock ()
  (skg-test-maintenance--with-buffer 'content-view
    (skg-unlock-buffer-after-maintenance buffer 9)
    (skg-lock-buffer-for-maintenance buffer 11)
    (let ((skg--maintenance-client-incident '(:incident-id "incident-a")))
      (skg-lock-buffer-for-maintenance buffer 11)
      (should (= 1 (length skg--maintenance-restrictions)))
      (should (assoc '(incident "incident-a") skg--maintenance-restrictions))
      (skg-unlock-buffer-after-maintenance buffer 11 "incident-a")
      (should-not skg--maintenance-lock-overlay))))

(ert-deftest test-skg-idle-handshake-releases-obsolete-local-incident ()
  (skg-test-maintenance--with-buffer 'content-view
    (let ((skg--maintenance-client-incident
           '(:incident-id "old-incident" :epoch 9 :phase waiting-for-server))
          (skg--maintenance-state '((epoch . 10) (state . idle)))
          shown)
      (cl-letf (((symbol-function 'display-warning)
                 (lambda (type message level &rest _)
                   (setq shown (list type message level)))))
        (skg-maintenance-adopt-handshake-epoch))
      (should-not skg--maintenance-client-incident)
      (should-not (skg--buffer-record-maintenance-epoch skg--buffer-record))
      (should-not skg--maintenance-lock-overlay)
      (should (eq (car shown) 'skg))
      (should (string-match-p "old-incident" (cadr shown)))
      (should (eq (nth 2 shown) :warning)))))

(ert-deftest test-skg-new-allocation-retains-older-incident-and-restriction ()
  (skg-test-maintenance--with-buffer 'content-view
    (let ((skg--maintenance-client-incident
           '(:incident-id "old-incident" :epoch 9 :phase waiting-for-server))
          (skg--maintenance-state '((epoch . 9) (state . active)))
          submitted
          shown)
      (cl-letf (((symbol-function 'skg-tcp-connect-to-rust)
                 (lambda () 'tcp))
                ((symbol-function 'skg--submit-buffer-census)
                 (lambda (&rest arguments) (setq submitted arguments)))
                ((symbol-function 'display-warning)
                 (lambda (_type message _level &rest _)
                   (setq shown message))))
        (skg--maintenance-handle-bootstrap
         nil
         (concat
          "((status install-maintenance-epoch-and-submit-locked-census)"
          " (allocated-incident-id new-incident) (maintenance-epoch 10)"
          " (origin explicit-partial-reload) (started-at-utc now)"
          " (archive-directory-name archive) (source-set all)"
          " (g0-graph-generation 1) (g0-manifest-revision 2)"
          " (requested-paths ()) (requested-ids (node)))")))
      (should (equal (plist-get skg--maintenance-client-incident :incident-id)
                     "new-incident"))
      (should (= 10 (plist-get skg--maintenance-client-incident :epoch)))
      (should (= 10 (skg--buffer-record-maintenance-epoch
                     skg--buffer-record)))
      (should (equal (butlast submitted) '(tcp "new-incident" 10)))
      (should-not shown)
      (should (skg--maintenance-lookup-incident "old-incident"))
      (skg-unlock-buffer-after-maintenance buffer 10 "new-incident")
      (should (= 9 (skg--buffer-record-maintenance-epoch skg--buffer-record))))))

(ert-deftest test-skg-every-maintenance-origin-refuses-dirty-raw-files-first ()
  (let ((buffer (generate-new-buffer "raw-maintenance-preflight.skg"))
        (skg--buffer-registry (make-hash-table :test #'equal))
        connected)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (insert "pid: dirty\n")
            (skg-register-buffer
             buffer 'raw-skg-file
             :lifecycle 'ordinary-file :disposable nil)
            (set-buffer-modified-p t))
          (cl-letf (((symbol-function 'skg-tcp-connect-to-rust)
                     (lambda () (setq connected t) 'tcp)))
            (let ((error-data
                   (should-error
                    (skg-begin-maintenance "explicit-partial-reload")
                    :type 'user-error)))
              (should (string-match-p
                       "raw-maintenance-preflight\\.skg"
                       (error-message-string error-data)))))
          (should-not connected))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest test-skg-maintenance-locks-before-incident-census-and-archives-after-ack ()
  (skg-test-maintenance--with-buffer 'content-view
    (let* ((id (skg--buffer-record-id skg--buffer-record))
           (lock-sha (skg--maintenance-lock-census-sha256 (list id)))
           (base
            (format
             (concat " (allocated-incident-id incident)"
                     " (maintenance-epoch 9) (origin test-origin)"
                     " (started-at-utc now) (archive-directory-name archive)"
                     " (source-set all) (g0-graph-generation 1)"
                     " (g0-manifest-revision 2) (requested-paths ())"
                     " (requested-ids ())")))
           (skg--maintenance-client-incident nil)
           (skg--maintenance-state '((epoch . 0) (state . idle)))
           census-arguments published)
      (cl-letf (((symbol-function 'skg-tcp-connect-to-rust)
                 (lambda () 'tcp))
                ((symbol-function 'skg--submit-buffer-census)
                 (lambda (&rest arguments)
                   (setq census-arguments arguments)))
                ((symbol-function 'skg--maintenance-publish-initial)
                 (lambda () (setq published t))))
        (skg--maintenance-handle-bootstrap
         nil (concat "((status install-maintenance-epoch-and-submit-locked-census)"
                     base " (registered-buffer-ids ()))")
         #'ignore 'origin-context)
      (should (equal (butlast census-arguments)
                     '(tcp "incident" 9)))
        (should (eq (plist-get skg--maintenance-client-incident :phase)
                    'awaiting-locked-census))
        (should (= 9 (skg--buffer-record-maintenance-epoch
                      skg--buffer-record)))
        (should-not published)
        (skg--maintenance-handle-bootstrap
         nil
         (concat
          "((status locked-census-accepted-publish-initial-archive)"
          base " (registered-buffer-ids (" id "))"
          " (lock-census-sha256 " lock-sha "))"))
        (should published)
        (should (eq (plist-get skg--maintenance-client-incident :phase)
                    'preparing-archive))
        (should (equal (plist-get skg--maintenance-client-incident
                                  :registered-buffer-ids)
                       (list id)))))))

(ert-deftest test-skg-locked-census-rejection-cancels-the-prearchive-incident ()
  (let ((skg--maintenance-client-incident
         '(:incident-id "incident" :epoch 9 :phase awaiting-locked-census))
        handlers scheduled warning)
    (cl-letf (((symbol-function 'skg-tcp-connect-to-rust) (lambda () 'tcp))
              ((symbol-function 'skg-submit-priority-request)
               (lambda (_tcp _request supplied-handlers &rest _args)
                 (setq handlers supplied-handlers)))
              ((symbol-function 'display-warning)
               (lambda (_type message _level &rest _args)
                 (setq warning message)))
              ((symbol-function 'run-at-time)
               (lambda (_seconds _repeat function &rest arguments)
                 (setq scheduled (cons function arguments)))))
      (skg--maintenance-send-locked-census)
      (let ((handler (cadr (assoc 'error handlers))))
        (should (functionp handler))
        (funcall
         handler nil
         "((content \"dirty undo cannot be archived\") (terminal-status failed))"))
      (should (eq (plist-get skg--maintenance-client-incident :phase)
                  'cancelling-after-locked-census-refusal))
      (should (string-match-p "dirty undo cannot be archived" warning))
      (should (equal scheduled
                     '(skg--maintenance-run-deferred "incident"
                       skg-cancel-maintenance ("incident" 9)))))))

(ert-deftest test-skg-buffer-born-during-maintenance-is-outside-frozen-census ()
  (let ((buffer (generate-new-buffer " *skg-born-locked-test*"))
        (skg--buffer-registry (make-hash-table :test #'equal))
        (skg--server-store-state '((graph-generation . 1)))
        (skg--maintenance-state '((epoch . 12) (state . active))))
    (unwind-protect
        (with-current-buffer buffer
          (org-mode)
          (insert "* Born locked\n")
          (skg-register-buffer
           buffer 'content-view :lifecycle 'live-view :disposable nil
           :view-uri "new-view")
          (should-not (skg--buffer-record-maintenance-epoch
                       skg--buffer-record))
          (should-not skg--maintenance-lock-overlay))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest test-skg-buffer-census-emits-complete-normalized-descriptor ()
  (let ((buffer (generate-new-buffer " *skg-census-test*"))
        (skg--buffer-registry (make-hash-table :test #'equal))
        (skg--server-store-state '((graph-generation . 7)))
        (skg--active-source-set-name "private"))
    (unwind-protect
        (with-current-buffer buffer
          (org-mode)
          (insert "* Original\n")
          (setq skg-view-uri "search:dog")
          (skg-register-buffer
           buffer 'search-view :lifecycle 'live-view :disposable nil
           :view-uri "search:dog"
           :recipe '((kind . "search") (terms . "dog")
                     (regex . t) (body . nil) (operators . t))
           :root-ids '("z-root" "a-root" "z-root")
           :lifecycle 'live-view :continuation-id "continuation-1")
          (setf (skg--buffer-record-logical-dirty skg--buffer-record) t
                (skg--buffer-record-presentation-stale skg--buffer-record) t
                (skg--buffer-record-search-stale skg--buffer-record) t
                (skg--buffer-record-herald-bearing skg--buffer-record) t)
          (skg-lock-buffer-for-maintenance buffer 9)
          (let ((descriptor (car (skg-buffer-census))))
            (should (equal (cdr (assq 'lifecycle descriptor)) "live-view"))
            (should (equal (cdr (assq 'continuation-id descriptor))
                           "continuation-1"))
            (should (equal (cdr (assq 'source-set descriptor)) "private"))
            (should (equal (cdr (assq 'maintenance-epoch descriptor)) 9))
            (should (equal (cdr (assq 'dirty descriptor)) "true"))
            (should (equal (cdr (assq 'logical-dirty descriptor)) "true"))
            (should (equal (cdr (assq 'presentation-stale descriptor))
                           "true"))
            (should (equal (cdr (assq 'search-stale descriptor)) "true"))
            (should (equal (cdr (assq 'herald-bearing descriptor)) "true"))
            (should (equal (cadr (assq 'root-ids descriptor))
                           '("a-root" "z-root")))
            (should
             (equal (cdr (assq 'recipe descriptor))
                    "((body \"nil\") (kind \"search\") (operators \"true\") (regex \"true\") (terms \"dog\"))"))
            (should (natnump (cdr (assq 'modification-tick descriptor))))
            ;; An explicit empty census is a frozen empty membership, not a
            ;; request to enumerate the live registry again.
            (should-not (skg-buffer-census nil))
            (should (= 1 (length (skg-buffer-census))))))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest test-skg-attached-workflow-carries-origin-and-dirties-parent ()
  (let ((origin (generate-new-buffer " *skg-workflow-origin*"))
        (child (generate-new-buffer " *skg-workflow-child*"))
        (skg--buffer-registry (make-hash-table :test #'equal))
        (skg--server-store-state '((graph-generation . 7))))
    (unwind-protect
        (progn
          (with-current-buffer origin
            (insert "* Origin\n")
            (skg-register-buffer
             origin 'content-view :lifecycle 'live-view :disposable nil
             :view-uri "view:origin"
             :recipe '((kind . "single-root") (root-id . "origin"))
             :application-token 5)
            (set-buffer-modified-p nil))
          (with-current-buffer child
            (insert "* Draft metadata\n")
            (set-buffer-modified-p nil)
            (skg-register-buffer
             child 'metadata-editor :lifecycle 'attached-workflow
             :disposable nil
             :continuation-id "continuation-1" :origin-buffer origin
             :origin-location "((start 1) (end 9))"
             :recipe '((kind . "metadata-editor"))))
          (should (skg-buffer-logical-dirty-p origin))
          (should (skg-buffer-dirty-p child))
          (let* ((child-id (buffer-local-value
                            'skg--buffer-record child))
                 (descriptor
                  (seq-find
                   (lambda (entry)
                     (equal (cdr (assq 'buffer-id entry))
                            (skg--buffer-record-id child-id)))
                   (skg-buffer-census))))
            (should
             (equal (cdr (assq 'origin-buffer-id descriptor))
                    (skg--buffer-record-id
                     (buffer-local-value 'skg--buffer-record origin))))
            (should (equal (cdr (assq 'origin-view-uri descriptor))
                           "view:origin"))
            (should (equal (cdr (assq 'origin-application-token descriptor))
                           5))
            (should (equal (cdr (assq 'origin-location descriptor))
                           "((start 1) (end 9))")))
          (with-current-buffer origin
            (skg-register-buffer
             origin 'content-view :lifecycle 'live-view :disposable nil
             :view-uri "view:origin"
             :recipe '((kind . "single-root") (root-id . "origin"))
             :application-token 5))
          (should (skg-buffer-logical-dirty-p origin))
          (kill-buffer child)
          (should-not (skg-buffer-logical-dirty-p origin)))
      (when (buffer-live-p child) (kill-buffer child))
      (when (buffer-live-p origin) (kill-buffer origin)))))

(ert-deftest test-skg-buffer-registration-requires-explicit-constructor-policy ()
  (let ((buffer (generate-new-buffer " *skg-policy-audit*"))
        (skg--buffer-registry (make-hash-table :test #'equal)))
    (unwind-protect
        (progn
          (should-error
           (skg-register-buffer buffer 'derived-report :disposable t)
           :type 'error)
          (should-error
           (skg-register-buffer
            buffer 'derived-report :lifecycle 'client-local)
           :type 'error)
          (skg-register-buffer
           buffer 'derived-report
           :lifecycle 'client-local :disposable t)
          (should (eq 'derived-report
                      (skg--buffer-record-kind
                       (buffer-local-value 'skg--buffer-record buffer)))))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest test-skg-generated-buffer-reuse-requires-explicit-disposability ()
  (let ((durable (generate-new-buffer "*skg durable namesake*"))
        (disposable (generate-new-buffer "*skg disposable namesake*"))
        (skg--buffer-registry (make-hash-table :test #'equal))
        fresh)
    (unwind-protect
        (progn
          (with-current-buffer durable
            (insert "keep this report")
            (set-buffer-modified-p nil)
            (skg-register-buffer
             durable 'durable-report
             :lifecycle 'client-local :disposable nil
             :last-fetched (skg-buffer-raw-text durable)))
          (setq fresh
                (skg-acquire-generated-buffer "*skg durable namesake*"))
          (should-not (eq durable fresh))
          (should (equal "keep this report"
                         (with-current-buffer durable (buffer-string))))
          (with-current-buffer disposable
            (set-buffer-modified-p nil)
            (skg-register-buffer
             disposable 'derived-report
             :lifecycle 'client-local :disposable t
             :last-fetched ""))
          (should (eq disposable
                      (skg-acquire-generated-buffer
                       "*skg disposable namesake*"))))
      (dolist (buffer (list fresh disposable durable))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest test-skg-direct-buffer-constructor-audit-is-complete ()
  (let ((directory (file-name-directory
                    (locate-library "skg-buffer-registry")))
        owners)
    (dolist (file (directory-files directory t "\\.el\\'"))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (re-search-forward
                "(\\(?:get-buffer-create\\|generate-new-buffer\\)[[:space:]\n]"
                nil t)
          (let ((call (point)))
            (save-excursion
              (goto-char call)
              (if (re-search-backward
                   "^(\\(?:cl-\\)?defun[[:space:]]+\\([^[:space:]()]+\\)"
                   nil t)
                  (push (match-string-no-properties 1) owners)
                (push "<top-level>" owners)))))))
    (should
     (equal
      (sort (delete-dups owners) #'string<)
      (sort
       '("skg--display-search-phase1"
         "skg--generate-contentView-buffer"
         "skg--pull-diagnostic-buffer"
         "skg-acquire-generated-buffer"
         "skg-open-interrupted-view"
         "skg-sexp-edit--open-edit-buffer"
         "skg-undo-sidecar-save"
         "skg-view-id-stack")
       #'string<)))))

(ert-deftest test-skg-maintenance-census-is-incident-qualified ()
  (let (submitted)
    (cl-letf (((symbol-function 'skg-buffer-census) (lambda () nil))
              ((symbol-function 'skg-submit-priority-request)
               (lambda (&rest arguments) (setq submitted arguments))))
      (skg--submit-buffer-census 'tcp "incident" 9))
    (should (equal (nth 4 submitted) "incident"))
    (should (string-match-p "maintenance-epoch \\. 9" (nth 1 submitted)))))

(ert-deftest test-skg-maintenance-locked-census-serializes-only-frozen-ids ()
  (skg-test-maintenance--with-buffer 'content-view
    (let* ((id (skg--buffer-record-id skg--buffer-record))
           (readonly (generate-new-buffer " *skg-late-readonly-result*"))
           submitted)
      (unwind-protect
          (progn
            (with-current-buffer readonly
              (org-mode)
              (insert "* Late presentation\n")
              (skg-register-buffer
               readonly 'content-view :lifecycle 'live-view :disposable nil
               :view-uri "late" :view-write-authority 'read-only))
            (cl-letf (((symbol-function 'skg-submit-priority-request)
                       (lambda (&rest arguments) (setq submitted arguments))))
              (skg--submit-buffer-census
               'tcp "incident" 9 (list id "missing-former-member")))
            (let* ((serialized (nth 3 submitted))
                   (census (read serialized)))
              (should (= 1 (length census)))
              (should (equal id (cdr (assq 'buffer-id (car census)))))
              (should (equal 9
                             (cdr (assq 'maintenance-epoch (car census)))))
              (should (equal "editable"
                             (cdr (assq 'view-write-authority
                                        (car census)))))))
        (when (buffer-live-p readonly) (kill-buffer readonly))))))

(ert-deftest test-skg-maintenance-presentation-results-do-not-grow-frozen-census ()
  (let ((skg--maintenance-client-incident
         '(:registered-buffer-ids ("writable"))))
    (skg--maintenance-refresh-presentation-buffer-ids
     '((presentation-buffer-ids ("writable" "readonly-result"))))
    (should (equal '("writable")
                   (plist-get skg--maintenance-client-incident
                              :registered-buffer-ids)))
    (should (equal '("writable" "readonly-result")
                   (plist-get skg--maintenance-client-incident
                              :presentation-buffer-ids)))))

(ert-deftest test-skg-maintenance-locked-census-closes-constructor-admission ()
  (let ((skg--maintenance-client-incident
         '(:incident-id "incident" :epoch 9)) request)
    (cl-letf (((symbol-function 'skg-tcp-connect-to-rust)
               (lambda () 'tcp))
              ((symbol-function 'skg-submit-priority-request)
               (lambda (_tcp wire &rest _args) (setq request (read wire)))))
      (skg--maintenance-send-locked-census))
    (should (equal "incident" (cdr (assoc 'incident-id request))))
    (should (equal 'closed
                   (cdr (assoc 'client-constructor-admission request))))))

(ert-deftest test-skg-archive-ready-schedules-explicit-origin-worker ()
  (let ((skg--maintenance-client-incident
         '(:origin "explicit-partial-reload" :phase preparing-archive))
        scheduled)
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (_seconds _repeat function &rest arguments)
                 (setq scheduled (cons function arguments)))))
      (skg--maintenance-handle-selection-response
       nil "((status archive-ready))"))
    (should (eq (nth 2 scheduled) #'skg--maintenance-run-explicit-origin))
    (should (eq (plist-get skg--maintenance-client-incident :phase)
                'origin-operation-required))))

(ert-deftest test-skg-archive-ready-schedules-full-rebuild-worker ()
  (let ((skg--maintenance-client-incident
         '(:origin "full-rebuild" :phase preparing-archive))
        scheduled)
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (_seconds _repeat function &rest arguments)
                 (setq scheduled (cons function arguments)))))
      (skg--maintenance-handle-selection-response
       nil "((status archive-ready))"))
    (should (eq (nth 2 scheduled) #'skg--maintenance-run-explicit-origin))
    (should (eq (plist-get skg--maintenance-client-incident :phase)
                'origin-operation-required))))

(ert-deftest test-skg-selection-defers-for-new-view-enrollment ()
  (let ((skg--maintenance-client-incident
         '(:incident-id "incident" :epoch 9 :phase waiting-for-server)))
    (skg--maintenance-server-status
     nil
     (concat "((status view-enrollment-pending)"
             " (incident-id incident) (maintenance-epoch 9)"
             " (phase archive-ready) (pending-view-uris (view)))"))
    (should (eq (plist-get skg--maintenance-client-incident :phase)
                'waiting-for-view-enrollment))))

(ert-deftest test-skg-maintenance-selection-installs-replacement-sources ()
  (let ((skg--maintenance-client-incident '(:epoch 9))
        (skg--active-source-set-name "main")
        (skg--server-source-inventory nil))
    (skg--maintenance-record-selection
     `((g1-graph-generation 2)
       (g1-manifest-revision 6)
       (tantivy-generation 4)
       (server-evidence-sha256 ,(make-string 64 ?d))
       (source-set all)
       (maintenance-archive-folder replacement-archives)
       (maintenance-archive-identity /server/replacement-archives)
       (source-inventory
        (((name replacement)
          (abbreviation rep)
          (owned true)
          (position 0)
          (configured-path replacement-notes)
          (directory /data/replacement-notes)
          (directory-identity /data/replacement-notes))))))
    (should (equal skg--active-source-set-name "all"))
    (should (equal skg--maintenance-archive-folder
                   "replacement-archives"))
    (should (equal (plist-get (car skg--server-source-inventory) :name)
                   "replacement"))
    (should (equal
             (plist-get skg--maintenance-client-incident
                        :selected-source-set)
             "all"))))

(ert-deftest test-skg-rebuild-begins-maintenance-instead-of-raw-request ()
  (let ((skg--maintenance-client-incident nil)
        arguments)
    (cl-letf (((symbol-function 'skg-registered-buffers) (lambda () nil))
              ((symbol-function 'skg-begin-maintenance)
               (lambda (&rest values) (setq arguments values))))
      (skg-rebuild-dbs))
    (should (equal (car arguments) "full-rebuild"))
    (should (functionp (nth 4 arguments)))))

(ert-deftest test-skg-rebuild-allows-retained-report-after-open-publication ()
  (let ((skg--maintenance-client-incident
         '(:incident-id "old-report" :phase terminal))
        (skg--graph-write-admission 'open)
        arguments)
    (cl-letf (((symbol-function 'skg-registered-buffers) (lambda () nil))
              ((symbol-function 'skg-begin-maintenance)
               (lambda (&rest values) (setq arguments values))))
      (skg-rebuild-dbs))
    (should (equal (car arguments) "full-rebuild"))))

(ert-deftest test-skg-rebuild-refuses-during-an-active-incident ()
  (let ((skg--maintenance-client-incident
         '(:incident-id "active" :phase settling))
        (skg--graph-write-admission nil))
    (should-error (skg-rebuild-dbs) :type 'user-error)))

(ert-deftest test-skg-rebuild-refuses-a-dirty-raw-file-buffer ()
  (let ((buffer (generate-new-buffer " *skg-raw-rebuild-test*"))
        (skg--buffer-registry (make-hash-table :test #'equal))
        (skg--maintenance-client-incident nil)
        (skg--server-store-state '((graph-generation . 1))))
    (unwind-protect
        (with-current-buffer buffer
          (insert "pid: node\ntitle: dirty\n")
          (skg-register-buffer
           buffer 'raw-skg-file :lifecycle 'ordinary-file :disposable nil)
          (set-buffer-modified-p t)
          (should-error (skg-rebuild-dbs) :type 'user-error))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest test-skg-origin-failure-push-keeps-the-exact-incident-locked ()
  (let ((skg--maintenance-client-incident
         '(:incident-id "incident" :epoch 9 :phase waiting-for-origin-observation))
        warning)
    (cl-letf (((symbol-function 'display-warning)
               (lambda (_type message &rest _arguments)
                 (setq warning message))))
      (skg--maintenance-server-status
       nil
       (concat "((status origin-operation-failed)"
               " (incident-id incident) (maintenance-epoch 9)"
               " (phase blocked-invalid-after-mutation)"
               " (error \"malformed target\"))")))
    (should (eq (plist-get skg--maintenance-client-incident :phase)
                'server-blocked))
    (should (equal
             (plist-get skg--maintenance-client-incident :blocking-reason)
             "malformed target"))
    (should (string-match-p "skg-retry-maintenance" warning))))

(ert-deftest test-skg-blocked-status-retains-exact-recovery-instructions ()
  (let ((skg--maintenance-client-incident
         '(:incident-id "incident" :epoch 9 :origin "pull"
           :requested-paths nil :requested-ids nil :phase waiting-for-server))
        (skg--maintenance-state nil)
        warning)
    (cl-letf (((symbol-function 'display-warning)
               (lambda (_type message &rest _arguments)
                 (setq warning message))))
      (skg--maintenance-resume-active
       '((status active) (active-incident-id "incident")
         (maintenance-epoch 9) (phase "blocked-store-health")
         (origin "pull") (requested-paths ()) (requested-ids ())
         (blocking-reason "TypeDB is unavailable"))))
    (should (eq (plist-get skg--maintenance-client-incident :phase)
                'server-blocked))
    (should (equal
             (plist-get skg--maintenance-client-incident :server-phase)
             "blocked-store-health"))
    (should (equal
             (plist-get skg--maintenance-client-incident :blocking-reason)
             "TypeDB is unavailable"))
    (should (string-match-p "skg-retry-maintenance" warning))))

(ert-deftest test-skg-retry-maintenance-sends-exact-envelope-and-awaits-push ()
  (let ((skg--maintenance-client-incident
         '(:incident-id "incident" :epoch 9 :phase server-blocked
           :server-phase "blocked-invalid-after-mutation"
           :blocking-reason "malformed source"))
        request request-incident handler)
    (cl-letf (((symbol-function 'skg-tcp-connect-to-rust)
               (lambda () 'tcp))
              ((symbol-function 'skg-register-response-handler)
               (lambda (kind callback &optional _one-shot)
                 (should (eq kind 'maintenance-status))
                 (setq handler callback)))
              ((symbol-function 'skg-set-request-failure-handler) #'ignore)
              ((symbol-function 'skg-submit-request)
               (lambda (_tcp text &optional _content incident)
                 (setq request text request-incident incident))))
      (skg-retry-maintenance))
    (should (eq (plist-get skg--maintenance-client-incident :phase)
                'maintenance-retry-pending))
    (should (equal request-incident "incident"))
    (let ((parsed (read request)))
      (should (equal (cdr (assoc 'request parsed)) "retry maintenance"))
      (should (= (cdr (assoc 'maintenance-epoch parsed)) 9)))
    (funcall handler nil
             "((status maintenance-retry-queued) (incident-id incident) (maintenance-epoch 9) (recovery-mode targeted))")
    (should (eq (plist-get skg--maintenance-client-incident :phase)
                'waiting-for-origin-observation))
    (should-not (plist-get skg--maintenance-client-incident :server-phase))
    (should-not (plist-get skg--maintenance-client-incident
                           :blocking-reason))))

(ert-deftest test-skg-invalid-post-pull-retires-dirty-work-before-retry ()
  (let* ((retirement
          (skg-test-maintenance--settlement
           "dirty" "content-view" "view" "true"
           "retirement-ack" "interrupted"))
         (retirement (append retirement
                             '((settlement-resolution "pending"))))
         (skg--maintenance-client-incident
          '(:incident-id "incident" :epoch 9 :phase server-blocked
            :registered-buffer-ids ("dirty" "clean")
            :locally-applied nil))
         scheduled applied sent)
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (_seconds _repeat function &rest arguments)
                 (setq scheduled (cons function arguments))))
              ((symbol-function 'skg--maintenance-apply-settlement)
               (lambda (record) (setq applied record)))
              ((symbol-function
                'skg--maintenance-send-preselection-retirement-ack)
               (lambda (record) (setq sent record))))
      (skg--maintenance-install-preselection-retirements (list retirement))
      (apply (car scheduled) (cdr scheduled))
      (should (eq retirement applied))
      (should (eq retirement sent))
      (should (equal '("dirty")
                     (plist-get skg--maintenance-client-incident
                                :locally-applied)))
      (should-error
       (skg--maintenance-handle-preselection-retirement-ack
        nil
        "((status view-settlement-recorded) (buffer-id dirty) (required-ack retirement-ack))"))
      (skg--maintenance-handle-preselection-retirement-ack
       nil
       "((status all-invalid-dirty-buffers-retired) (buffer-id dirty) (required-ack retirement-ack))")
      (should
       (equal "client-acknowledged"
              (skg--maintenance-text
               (car (plist-get skg--maintenance-client-incident
                               :preselection-retirements))
               'settlement-resolution)))
      (apply (car scheduled) (cdr scheduled)))
    (should (eq (plist-get skg--maintenance-client-incident :phase)
                'server-blocked))
    (should-not (plist-get skg--maintenance-client-incident
                           :pending-preselection-retirements))))

(ert-deftest test-skg-maintenance-settlement-inventory-is-exact ()
  (let ((one (skg-test-maintenance--settlement
              "one" "content-view" "view-one" "nil"
              "release-ack" "retained-clean"))
        (two (skg-test-maintenance--settlement
              "two" "content-view" "view-two" "nil"
              "release-ack" "retained-clean")))
    (should (equal (skg--maintenance-validate-settlements
                    (list one two) '("two" "one"))
                   (list one two)))
    (should-error
     (skg--maintenance-validate-settlements (list one one) '("one" "two")))
    (should-error
     (skg--maintenance-validate-settlements (list one) '("one" "two")))))

(ert-deftest test-skg-maintenance-release-preserves-authored-bytes ()
  (skg-test-maintenance--with-buffer 'content-view
    (let* ((id (skg--buffer-record-id skg--buffer-record))
           (settlement (skg-test-maintenance--settlement
                        id "content-view" "view" "nil"
                        "release-ack" "retained-clean"))
           (before (skg-buffer-raw-text)))
      (skg-release-buffer-across-maintenance buffer settlement 9 2)
      (should (equal before (skg-buffer-raw-text)))
      (should (= 2 (skg--buffer-record-graph-generation skg--buffer-record)))
      (should (= 4 (skg--buffer-record-server-revision skg--buffer-record)))
      (should (= 7 (skg--buffer-record-application-token skg--buffer-record)))
      (should (skg--buffer-record-presentation-stale skg--buffer-record)))))

(ert-deftest test-skg-stale-and-pending-status-repeats-on-buffer-entry ()
  (skg-test-maintenance--with-buffer 'search-view
    (let ((skg--pending-maintenance-offer '((candidate-id . "candidate")))
          echoed)
      (setf (skg--buffer-record-presentation-stale skg--buffer-record) t
            (skg--buffer-record-search-stale skg--buffer-record) t
            (skg--buffer-record-herald-bearing skg--buffer-record) t)
      (should (string-match-p "pending-disk" (skg-buffer-status-indicator)))
      (should (string-match-p "presentation-stale"
                              (skg-buffer-status-indicator)))
      (should (string-match-p "search-stale" (skg-buffer-status-indicator)))
      (cl-letf (((symbol-function 'message)
                 (lambda (format-string &rest arguments)
                   (setq echoed (apply #'format format-string arguments)))))
        (skg-warn-buffer-status-on-entry))
      (should (string-match-p "maintenance-locked" echoed))
      (should (string-match-p "generated heralds" echoed))
      (should (string-match-p "Search membership and ranking" echoed)))))

(ert-deftest test-skg-rebuilding-status-is-explicit-orange-modeline-metadata ()
  (skg-test-maintenance--with-buffer 'content-view
    (let ((skg--rebuilding nil))
      (skg-update-rebuilding-status '((status active)))
      (should-not skg--rebuilding)
      (skg-update-rebuilding-status '((rebuilding true)))
      (let ((indicator (skg-buffer-status-indicator)))
        (let ((start (string-match "rebuilding" indicator)))
          (should start)
          (should-not (string-match "rebuilding" indicator
                                    (+ start (length "rebuilding"))))
          (should (eq (get-text-property start 'face indicator)
                      'skg-rebuilding-face))
          (should (equal (face-attribute
                          'skg-rebuilding-face :foreground nil t)
                         "orange")))
        (should (string-match-p "M:9" indicator)))
      (skg-update-rebuilding-status '((status active)))
      (should skg--rebuilding)
      (skg-update-rebuilding-status '((rebuilding nil)))
      (should-not (string-match-p "rebuilding"
                                  (skg-buffer-status-indicator))))))

(ert-deftest test-skg-known-rebuilding-refuses-save-before-transient-lock ()
  (skg-test-maintenance--with-buffer 'content-view
    (let ((skg--rebuilding t)
          (before (skg-buffer-raw-text))
          refusal)
      (condition-case error-data
          (skg-request-save-buffer)
        (user-error (setq refusal (error-message-string error-data))))
      (should (string-match-p "nothing was saved; try again" refusal))
      (should (equal before (skg-buffer-raw-text)))
      (should-not skg--save-lock-overlay)
      (should skg--maintenance-lock-overlay))))

(ert-deftest test-skg-raced-save-refusal-keeps-maintenance-lock ()
  (skg-test-maintenance--with-buffer 'content-view
    (skg-unlock-buffer-after-maintenance buffer 9)
    (skg--lock-for-save)
    (skg-lock-buffer-for-maintenance buffer 10)
    (skg--unlock-after-save)
    (should-not skg--save-lock-overlay)
    (should skg--maintenance-lock-overlay)
    (should (= 10 (skg--buffer-record-maintenance-epoch
                   skg--buffer-record)))))

(ert-deftest test-skg-maintenance-retirement-keeps-text-and-undo ()
  (skg-test-maintenance--with-buffer 'content-view
    (set-buffer-modified-p t)
    (setq buffer-undo-list '((1 . 2)))
    (let* ((id (skg--buffer-record-id skg--buffer-record))
           (settlement (skg-test-maintenance--settlement
                        id "content-view" "view" "true"
                        "retirement-ack" "interrupted"))
           (before (skg-buffer-raw-text))
           (undo-before buffer-undo-list))
      (skg-retire-buffer-for-maintenance
       buffer settlement 9 "12345678-1234-4234-8234-123456789abc")
      (should (equal before (skg-buffer-raw-text)))
      (should (equal undo-before buffer-undo-list))
      (should-not skg-view-uri)
      (should-not (skg--buffer-record-view-uri skg--buffer-record))
      (should (eq 'detached-recovery
                  (skg--buffer-record-lifecycle skg--buffer-record))))))

(ert-deftest test-skg-maintenance-application-checks-and-advances-authority ()
  (skg-test-maintenance--with-buffer 'content-view
    (let* ((id (skg--buffer-record-id skg--buffer-record))
           (content "* Rendered\n")
           (application
            `((content ,content)
              (content-sha256 ,(skg--sha256-text content))
              (resulting-graph-generation 2)
              (resulting-presentation-generation 8)
              (resulting-server-revision 5)
              (resulting-application-token 8)
              (warnings ())))
           (settlement
            (append
             (skg-test-maintenance--settlement
              id "content-view" "view" "nil"
              "application-ack" "refreshed")
             `((application ,application))))
           (state '(:epoch 9 :g1-graph-generation 2)))
      (skg--maintenance-apply-rendered-view
       buffer settlement application state)
      (should (equal content
                     (skg--buffer-record-last-fetched skg--buffer-record)))
      (should (= 2 (skg--buffer-record-graph-generation skg--buffer-record)))
      (should (= 8 (skg--buffer-record-presentation-generation
                    skg--buffer-record)))
      (should (= 5 (skg--buffer-record-server-revision skg--buffer-record)))
      (should (= 8 (skg--buffer-record-application-token skg--buffer-record)))
      (let ((changed (copy-tree application)))
        (setf (cadr (assoc 'content-sha256 changed)) (make-string 64 ?f))
        (should-error
         (skg--maintenance-apply-rendered-view
          buffer settlement changed state))))))

(ert-deftest test-skg-maintenance-application-ack-echoes-all-authority ()
  (let* ((settlement (skg-test-maintenance--settlement
                      "buffer" "content-view" "view" "nil"
                      "application-ack" "refreshed"))
         (application
          `((content "* Rendered\n")
            (content-sha256 ,(make-string 64 ?a))
            (resulting-graph-generation 2)
            (resulting-presentation-generation 8)
            (resulting-server-revision 5)
            (resulting-application-token 8)
            (warnings ())))
         (settlement (append settlement `((application ,application))))
         (fields (skg--maintenance-ack-fields settlement)))
    (dolist (key '(base-graph-generation base-presentation-generation
                   base-server-revision base-application-token
                   content-sha256 resulting-graph-generation
                   resulting-presentation-generation
                   resulting-server-revision resulting-application-token))
      (should (assoc key fields)))))

(ert-deftest test-skg-maintenance-status-filters-durable-acknowledgements ()
  (let* ((one (skg-test-maintenance--settlement
               "one" "content-view" "view-one" "nil"
               "release-ack" "retained-clean"))
         (two (skg-test-maintenance--settlement
               "two" "content-view" "view-two" "nil"
               "release-ack" "retained-clean"))
         (old (list (copy-tree one) (copy-tree two)))
         (scheduled nil)
         (skg--maintenance-client-incident
          (list :registered-buffer-ids '("one" "two")
                :settlements old
                :pending-settlements nil
                :acknowledged-settlements nil
                :locally-applied '("two")
                :in-flight-settlement two
                :phase 'view-settlement-ack-pending)))
    (setq two (cons '(acknowledged "true")
                    (assq-delete-all 'acknowledged two)))
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (_seconds _repeat function &rest _args)
                 (setq scheduled function))))
      (skg--maintenance-install-settlements (list one two)))
    (should (eq scheduled #'skg--maintenance-run-deferred))
    (should (equal '("one")
                   (mapcar (lambda (record)
                             (skg--maintenance-text record 'buffer-id))
                           (plist-get skg--maintenance-client-incident
                                      :pending-settlements))))
    (should (equal '("two")
                   (mapcar (lambda (record)
                             (skg--maintenance-text record 'buffer-id))
                           (plist-get skg--maintenance-client-incident
                                      :acknowledged-settlements))))
    (setf (plist-get skg--maintenance-client-incident :locally-applied) nil)
    (should-error
     (skg--maintenance-install-settlements (list one two)))))

(ert-deftest test-skg-maintenance-adopts-archive-backed-active-incident ()
  (let ((skg--maintenance-client-incident nil)
        (skg--maintenance-archive-folder "/archives")
        (skg--maintenance-archive-identity "/server/archives")
        (initial-sha (make-string 64 ?a)))
    (cl-letf (((symbol-function 'skg-recovery-archive-inspect)
               (lambda (path)
                 (should (equal path "/archives/archive"))
                 (list :incident-id
                       "12345678-1234-4234-8234-123456789abc"
                       :name "archive" :path path :status 'archive-ready
                       :initial-manifest-sha256 initial-sha))))
      (skg--maintenance-adopt-active
       `((active-incident-id "12345678-1234-4234-8234-123456789abc")
         (maintenance-epoch 9) (archive-status "archive-ready")
         (archive-directory-name "archive")
         (initial-manifest-sha256 ,initial-sha)
         (origin "explicit-partial-reload") (started-at-utc "now")
         (source-set "all") (g0-graph-generation 1)
         (g0-manifest-revision 2) (requested-paths ("one.skg"))
         (requested-ids ("node")) (registered-buffer-ids ("gone")))))
    (should (plist-get skg--maintenance-client-incident :adopted))
    (should (equal initial-sha
                   (plist-get
                    (plist-get skg--maintenance-client-incident :archive)
                    :manifest-sha256)))
    (should (equal '("gone")
                   (plist-get skg--maintenance-client-incident
                              :registered-buffer-ids)))
    (should (equal
             "explicit-partial-reload"
             (plist-get (plist-get skg--maintenance-client-incident :offer)
                        :origin)))))

(ert-deftest test-skg-maintenance-accepts-absent-census-resolution ()
  (let* ((old (skg-test-maintenance--settlement
               "gone" "content-view" "view" "nil"
               "application-ack" "refreshed"))
         (resolved (copy-tree old))
         scheduled
         (skg--buffer-registry (make-hash-table :test #'equal))
         (skg--maintenance-client-incident
          (list :registered-buffer-ids '("gone")
                :settlements (list old)
                :locally-applied nil)))
    (setq resolved (cons '(acknowledged "true")
                         (assq-delete-all 'acknowledged resolved))
          resolved (cons '(settlement-resolution "census-absent")
                         resolved))
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (_seconds _repeat function &rest _args)
                 (setq scheduled function))))
      (skg--maintenance-install-settlements (list resolved)))
    (should (eq scheduled #'skg--maintenance-run-deferred))
    (should (equal
             '("gone")
             (mapcar (lambda (record)
                       (skg--maintenance-text record 'buffer-id))
                     (plist-get skg--maintenance-client-incident
                                :acknowledged-settlements))))))

(ert-deftest test-skg-maintenance-retry-does-not-reapply-local-action ()
  (let* ((settlement (skg-test-maintenance--settlement
                      "one" "content-view" "view-one" "nil"
                      "release-ack" "retained-clean"))
         (applied 0)
         sent
         (skg--maintenance-client-incident
          (list :incident-id "incident"
                :epoch 9
                :phase 'settling-views
                :pending-settlements (list settlement)
                :locally-applied '("one")
                :in-flight-settlement nil)))
    (cl-letf (((symbol-function 'skg--maintenance-apply-settlement)
               (lambda (_settlement) (cl-incf applied)))
              ((symbol-function 'skg--maintenance-send-settlement-ack)
               (lambda (record) (setq sent record))))
      (skg--maintenance-settle-next))
    (should (= applied 0))
    (should (eq sent settlement))
    (should (eq settlement
                (plist-get skg--maintenance-client-incident
                           :in-flight-settlement)))))

(ert-deftest test-skg-maintenance-settlement-ack-updates-final-record ()
  (let* ((settlement (append
                      (skg-test-maintenance--settlement
                       "one" "content-view" "view-one" "nil"
                       "release-ack" "retained-clean")
                      '((settlement-resolution "pending"))))
         scheduled
         (skg--maintenance-client-incident
          (list :settlements (list settlement)
                :pending-settlements (list settlement)
                :acknowledged-settlements nil
                :in-flight-settlement settlement)))
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (_seconds _repeat function &rest _arguments)
                 (setq scheduled function))))
      (should-error
       (skg--maintenance-handle-settlement-ack
        nil
        "((status invalid-dirty-buffer-retired) (buffer-id one) (required-ack release-ack))"))
      (skg--maintenance-handle-settlement-ack
       nil
       "((status all-views-settled) (buffer-id one) (required-ack release-ack))"))
    (should (eq scheduled #'skg--maintenance-run-deferred))
    (let ((record (car (plist-get skg--maintenance-client-incident
                                  :settlements))))
      (should (equal "true"
                     (skg--maintenance-text record 'acknowledged)))
      (should (equal "client-acknowledged"
                     (skg--maintenance-text
                      record 'settlement-resolution))))))

(ert-deftest test-skg-maintenance-terminal-keeps-unsettled-census-locked ()
  (skg-test-maintenance--with-buffer 'content-view
    (let* ((id (skg--buffer-record-id skg--buffer-record))
           (manifest (make-string 64 ?a))
           (scheduled nil)
           (callback-count 0)
           (skg--maintenance-state '((epoch . 9) (state . active)))
           (skg--maintenance-client-incident
            (list :incident-id "12345678-1234-4234-8234-123456789abc"
                  :epoch 9
                  :phase 'completing
                  :registered-buffer-ids (list id)
                  :g1-graph-generation 2
                  :g1-manifest-revision 6
                  :final-archive (list :manifest-sha256 manifest
                                       :path "/archive")
                  :terminal-callback
                  (lambda (_response) (cl-incf callback-count))
                  :terminal-callback-fired nil))
           (payload
            (prin1-to-string
             `((status terminal)
               (incident-id "12345678-1234-4234-8234-123456789abc")
               (maintenance-epoch 9)
               (disposition completed)
               (manifest-sha256 ,manifest)
               (unlock-buffer-ids (,id))
               (selected-graph-generation 2)
               (selected-manifest-revision 6)))))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (_seconds _repeat function &rest _args)
                   (setq scheduled function))))
        (skg--maintenance-handle-terminal nil payload))
      (should (= 9 (skg--buffer-record-maintenance-epoch
                    skg--buffer-record)))
      (should (eq scheduled #'skg--maintenance-run-deferred))
      (should (= callback-count 1))
      (should (plist-get skg--maintenance-client-incident
                         :terminal-callback-fired))
      (should (eq (plist-get skg--maintenance-client-incident :phase)
                  'terminal-received))
      (should (equal (cdr (assq 'state skg--maintenance-state))
                     'terminal)))))

(ert-deftest test-skg-terminal-ack-keeps-newer-global-publication-state ()
  (let* ((skg--server-session-id
          "12345678-1234-4234-8234-123456789abc")
         (skg--owner-publication-revision 2)
         (skg--graph-write-admission 'open)
         (skg--rebuilding nil)
         (skg--graph-transition-status 'idle)
         (skg--pending-incidents nil)
         (skg--server-store-state '((graph-generation . 2)
                                    (manifest-revision . 6)))
         (skg--maintenance-client-incident
          '(:incident-id "incident" :epoch 9 :phase terminal-received
            :final-archive (:path "/archive"))))
    (skg--maintenance-handle-terminal-ack
     nil
     "((status terminal-acknowledged) (incident-id incident)
       (maintenance-epoch 9)
       (server-session-id \"12345678-1234-4234-8234-123456789abc\")
       (owner-publication-revision 1) (current-graph-generation 2)
       (current-manifest-revision 6) (graph-write-admission closed)
       (graph-transition-status transitioning) (rebuilding true)
       (pending-incidents ((incident-id newer))))")
    (should-not skg--maintenance-client-incident)
    (should (= 2 skg--owner-publication-revision))
    (should (eq 'open skg--graph-write-admission))
    (should-not skg--rebuilding)
    (should (eq 'idle skg--graph-transition-status))
    (should-not skg--pending-incidents)))

(ert-deftest test-skg-maintenance-census-stale-preserves-active-debt ()
  (skg-test-maintenance--with-buffer 'content-view
    (let ((id (skg--buffer-record-id skg--buffer-record))
          (skg--maintenance-state '((epoch . 9) (state . active)))
          skg--maintenance-client-incident)
      (setq skg--maintenance-client-incident
            (list :registered-buffer-ids (list id)))
      (skg-maintenance-handle-census-stale (list id))
      (should (equal "view" (skg--buffer-record-view-uri
                             skg--buffer-record)))
      (should (equal "view" skg-view-uri)))))

(ert-deftest test-skg-maintenance-late-terminal-and-ack-keep-newer-workflow ()
  (let* ((manifest (make-string 64 ?a))
         (skg--maintenance-client-incident nil)
         (skg--maintenance-client-incidents nil)
         (skg--owner-publication-revision 20)
         (skg--server-store-state '((graph-generation . 20) (manifest-revision . 30)))
         (skg--maintenance-state '((epoch . 10) (state . active)))
         (skg--pending-maintenance-offer '(:candidate-id "newer"))
         (skg--client-constructor-admission 'closed)
         (incident-a (list :incident-id "a" :epoch 9 :phase 'completing
                           :registered-buffer-ids nil :g1-graph-generation 2
                           :g1-manifest-revision 6
                           :final-archive (list :manifest-sha256 manifest :path "/a")))
         (incident-b (list :incident-id "b" :epoch 10 :phase 'preparing-archive))
         handler scheduled submitted-id)
    (cl-letf (((symbol-function 'skg-tcp-connect-to-rust) (lambda () 'tcp))
              ((symbol-function 'skg-register-response-handler)
               (lambda (_kind callback &rest _) (setq handler callback)))
              ((symbol-function 'skg-set-request-failure-handler) #'ignore)
              ((symbol-function 'skg-submit-request)
               (lambda (_tcp _request _handlers id) (setq submitted-id id)))
              ((symbol-function 'run-at-time)
               (lambda (_seconds _repeat function &rest args)
                 (setq scheduled (cons function args)))))
      (skg--maintenance-replace-current-incident incident-a)
      (skg--maintenance-send-complete)
      (skg--maintenance-replace-current-incident incident-b)
      (funcall handler nil
               (prin1-to-string
                `((status terminal) (incident-id a) (maintenance-epoch 9)
                  (disposition completed) (manifest-sha256 ,manifest)
                  (unlock-buffer-ids ()) (selected-graph-generation 2)
                  (selected-manifest-revision 6))))
      (should (eq incident-b skg--maintenance-client-incident))
      (should (= 20 (alist-get 'graph-generation skg--server-store-state)))
      (apply (car scheduled) (cdr scheduled))
      (should (equal submitted-id "a"))
      (funcall handler nil
               "((status terminal-acknowledged) (incident-id a) (maintenance-epoch 9))")
      (should (eq incident-b skg--maintenance-client-incident))
      (should (eq 'preparing-archive (plist-get incident-b :phase)))
      (should (plist-get (skg--maintenance-lookup-incident "a") :terminal-acknowledged))
      (should (eq 'closed skg--client-constructor-admission))
      (should (equal "newer" (plist-get skg--pending-maintenance-offer :candidate-id)))
      (should (= 10 (alist-get 'epoch skg--maintenance-state))))))

(ert-deftest test-skg-maintenance-status-selects-server-retained-incident ()
  (let* ((incident-b (list :incident-id "b" :epoch 10))
         (skg--maintenance-client-incident incident-b)
         (skg--maintenance-client-incidents nil)
         handler sent-id sent-request)
    (cl-letf (((symbol-function 'skg-tcp-connect-to-rust) (lambda () 'tcp))
              ((symbol-function 'skg-register-response-handler)
               (lambda (_kind callback &rest _) (setq handler callback)))
              ((symbol-function 'skg-submit-request)
               (lambda (_tcp request _handlers id)
                 (setq sent-id id sent-request (read request))))
              ((symbol-function 'skg--maintenance-resume-active)
               (lambda (response)
                 (should-not skg--maintenance-client-incident)
                 (skg--maintenance-replace-current-incident
                  (list :incident-id (skg--maintenance-text response 'active-incident-id)
                        :phase 'resumed)))))
      (skg-maintenance-status nil "a")
      (should (equal sent-id "a"))
      (should (equal (alist-get 'incident-id sent-request) "a"))
      (should-error (funcall handler nil "((status active) (active-incident-id b))"))
      (funcall handler nil "((status active) (active-incident-id a))")
      (should (eq incident-b skg--maintenance-client-incident))
      (should (eq 'resumed (plist-get (skg--maintenance-lookup-incident "a") :phase))))))

(ert-deftest test-skg-maintenance-idle-reconnect-preserves-pending-debt ()
  (skg-test-maintenance--with-buffer 'content-view
    (let* ((id (skg--buffer-record-id skg--buffer-record))
           (skg--maintenance-client-incident
            (list :incident-id "a" :epoch 9 :phase 'settling-views))
           (skg--maintenance-client-incidents nil)
           (skg--maintenance-state '((epoch . 9) (state . idle)))
           (skg--pending-incidents '(((incident-id a) (maintenance-epoch 9))))
           requested)
      (skg-maintenance-adopt-handshake-epoch)
      (skg-maintenance-handle-census-stale (list id))
      (should (equal "view" skg-view-uri))
      (should (= 9 (skg--buffer-record-maintenance-epoch skg--buffer-record)))
      (should (equal "a" (plist-get skg--maintenance-client-incident :incident-id)))
      (cl-letf (((symbol-function 'skg-maintenance-status)
                 (lambda (_quiet incident-id) (push incident-id requested))))
        (skg-resume-maintenance-after-census))
      (should (equal requested '("a"))))))

(ert-deftest test-skg-maintenance-new-handshake-keeps-old-restriction-and-resumes-both ()
  (skg-test-maintenance--with-buffer 'content-view
    (let ((skg--maintenance-client-incident
           (list :incident-id "a" :epoch 9 :phase 'settling-views))
          (skg--maintenance-client-incidents nil)
          (skg--maintenance-state '((epoch . 10) (state . active)))
          (skg--pending-incidents '(((incident-id a) (maintenance-epoch 9))
                                   ((incident-id b) (maintenance-epoch 10))))
          requested)
      (skg-maintenance-adopt-handshake-epoch)
      (should-not skg--maintenance-client-incident)
      (should (skg--maintenance-lookup-incident "a"))
      (should (= 10 (skg--buffer-record-maintenance-epoch skg--buffer-record)))
      (skg-unlock-buffer-after-maintenance buffer 10 "b")
      (should (= 9 (skg--buffer-record-maintenance-epoch skg--buffer-record)))
      (cl-letf (((symbol-function 'skg-maintenance-status)
                 (lambda (_quiet incident-id) (push incident-id requested))))
        (skg-resume-maintenance-after-census))
      (should (equal (nreverse requested) '("b" "a"))))))

(ert-deftest test-skg-maintenance-census-continuation-retains-own-incident ()
  (let ((skg--maintenance-client-incident nil)
        (skg--maintenance-client-incidents nil)
        (a (list :incident-id "a" :epoch 9 :phase 'awaiting-locked-census))
        (b (list :incident-id "b" :epoch 10 :phase 'settling-views))
        sent)
    (skg--maintenance-replace-current-incident a)
    (skg--maintenance-replace-current-incident b)
    (cl-letf (((symbol-function 'skg--maintenance-send-locked-census)
               (lambda () (setq sent skg--maintenance-client-incident))))
      (skg-resume-maintenance-after-census "a" 9))
    (should (eq a sent))
    (should (eq b skg--maintenance-client-incident))
    (should-error (skg-resume-maintenance-after-census "a" 10))))

(ert-deftest test-skg-maintenance-explicit-abandonment-only-releases-named-debt ()
  (skg-test-maintenance--with-buffer 'content-view
    (let ((skg--maintenance-client-incident nil)
          (skg--maintenance-client-incidents nil)
          (skg--maintenance-state '((epoch . 10) (state . active)))
          (skg--pending-incidents '(((incident-id b) (maintenance-epoch 10))))
          (skg--client-constructor-admission 'closed))
      (skg--maintenance-replace-current-incident
       (list :incident-id "a" :epoch 9 :phase 'waiting-for-server))
      (skg--maintenance-replace-current-incident
       (list :incident-id "b" :epoch 10 :phase 'preparing-archive))
      (skg-lock-buffer-for-maintenance buffer 10 "b")
      (skg-maintenance-adopt-handshake-epoch 'a)
      (should (equal "b" (plist-get skg--maintenance-client-incident :incident-id)))
      (should (eq 'closed skg--client-constructor-admission))
      (should (= 10 (skg--buffer-record-maintenance-epoch skg--buffer-record)))
      (skg-unlock-buffer-after-maintenance buffer 10 "b")
      (should-not (skg--buffer-record-maintenance-epoch skg--buffer-record)))))

(ert-deftest test-skg-maintenance-frozen-handshake-does-not-lock-later-buffer ()
  (skg-test-maintenance--with-buffer 'content-view
    (let* ((id (skg--buffer-record-id skg--buffer-record))
           (outside (generate-new-buffer " *skg-post-census*"))
           (skg--maintenance-client-incident nil)
           (skg--maintenance-client-incidents nil)
           (skg--pending-incidents nil))
      (unwind-protect
          (progn
            (with-current-buffer outside
              (org-mode)
              (skg-register-buffer outside 'content-view :view-uri "outside"
                                   :lifecycle 'live-view :disposable nil
                                   :view-write-authority 'editable))
            (dolist (ids (list nil (list id)))
              (skg-unlock-buffer-after-maintenance buffer 9)
              (let ((skg--maintenance-state
                     `((epoch . 9) (state . active) (incident-id . "a")
                       (census-buffer-ids . ,ids))))
                (skg-maintenance-adopt-handshake-epoch))
              (should (equal (and ids 9)
                             (skg--buffer-record-maintenance-epoch skg--buffer-record)))
              (should-not (buffer-local-value 'skg--maintenance-restrictions outside))))
        (when (buffer-live-p outside) (kill-buffer outside))))))

(provide 'test-skg-maintenance)
