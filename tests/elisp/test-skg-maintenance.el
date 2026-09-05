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
         (skg--server-store-state '((graph-generation . 1))))
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
        (should (equal census-arguments '(tcp "incident" 9)))
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

(ert-deftest test-skg-buffer-born-during-maintenance-inherits-epoch ()
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
          (should (= 12 (skg--buffer-record-maintenance-epoch
                         skg--buffer-record)))
          (should (overlayp skg--maintenance-lock-overlay)))
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
            (should (natnump (cdr (assq 'modification-tick descriptor))))))
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

(ert-deftest test-skg-archive-ready-schedules-explicit-origin-worker ()
  (let ((skg--maintenance-client-incident
         '(:origin "explicit-partial-reload" :phase preparing-archive))
        scheduled)
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (_seconds _repeat function &rest arguments)
                 (setq scheduled (cons function arguments)))))
      (skg--maintenance-handle-selection-response
       nil "((status archive-ready))"))
    (should (eq (car scheduled) #'skg--maintenance-run-explicit-origin))
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
    (should (eq (car scheduled) #'skg--maintenance-run-explicit-origin))
    (should (eq (plist-get skg--maintenance-client-incident :phase)
                'origin-operation-required))))

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
    (should (string-match-p "malformed target" warning))))

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
    (should (eq scheduled #'skg--maintenance-settle-next))
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
    (should (eq scheduled #'skg--maintenance-settle-next))
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

(ert-deftest test-skg-maintenance-terminal-unlocks-exact-census ()
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
      (should-not (skg--buffer-record-maintenance-epoch skg--buffer-record))
      (should (eq scheduled #'skg--maintenance-send-terminal-ack))
      (should (= callback-count 1))
      (should (plist-get skg--maintenance-client-incident
                         :terminal-callback-fired))
      (should (eq (plist-get skg--maintenance-client-incident :phase)
                  'terminal-received))
      (should (equal (cdr (assq 'state skg--maintenance-state))
                     'terminal)))))

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

(provide 'test-skg-maintenance)
