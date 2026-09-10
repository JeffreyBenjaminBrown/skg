;;; test-skg-pull.el --- Client-owned pull maintenance tests -*- lexical-binding: t; -*-

(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             (file-name-directory load-file-name)))
(require 'ert)
(require 'skg-pull)

(ert-deftest test-skg-pull-groups-local-sources-by-canonical-git-root ()
  (let ((skg--server-source-inventory
         '((:name "one" :configured-path "a/one")
           (:name "three" :configured-path "b/three")
           (:name "two" :configured-path "a/two"))))
    (cl-letf (((symbol-function 'skg-config-file)
               (lambda () "/client/skgconfig.toml"))
              ((symbol-function 'skg-source-paths-from-toml)
               (lambda (_file)
                 '(("three" . "/client/b/three")
                   ("one" . "/client/a/one")
                   ("two" . "/client/a/two"))))
              ((symbol-function 'file-directory-p) (lambda (_path) t))
              ((symbol-function 'vc-git-root)
               (lambda (path)
                 (if (string-prefix-p "/client/a/" path)
                     "/client/a"
                   "/client/b")))
              ((symbol-function 'file-truename) #'identity))
      (let ((repositories (skg--pull-local-repositories)))
        (should (equal (mapcar (lambda (repository)
                                (plist-get repository :root))
                              repositories)
                       '("/client/a/" "/client/b/")))
        (should (equal (mapcar (lambda (repository)
                                (plist-get repository :sources))
                              repositories)
                       '(("one" "two") ("three"))))
        (should
         (equal
          (mapcar (lambda (repository) (plist-get repository :key))
                  repositories)
          (mapcar
           (lambda (names)
             (skg--pull-repository-key names))
           '(("one" "two") ("three")))))))))

(ert-deftest test-skg-pull-requires-the-verified-server-inventory ()
  (let ((skg--server-source-inventory nil))
    (cl-letf (((symbol-function 'skg-config-file)
               (lambda () (ert-fail "read local config before preflight"))))
      (should-error (skg--pull-local-repositories) :type 'user-error))))

(ert-deftest test-skg-pull-refuses-a-limited-source-set-before-preflight ()
  (let ((skg--active-source-set-name "private")
        (skg--maintenance-client-incident nil))
    (cl-letf (((symbol-function 'skg--pull-local-repositories)
               (lambda () (ert-fail "computed repositories")))
              ((symbol-function 'skg-begin-maintenance)
               (lambda (&rest _arguments) (ert-fail "began maintenance"))))
      (should-error (skg-pull-all) :type 'user-error))))

(ert-deftest test-skg-pull-begins-one-maintenance-with-local-plan ()
  (let* ((key (make-string 64 ?a))
         (repositories
          `((:key ,key :root "/client/repo/" :sources ("one"))))
         (skg--active-source-set-name "all")
         (skg--maintenance-client-incident nil)
         submitted)
    (cl-letf (((symbol-function 'skg--pull-local-repositories)
               (lambda () repositories))
              ((symbol-function 'skg--pull-dirty-buffers) (lambda () nil))
              ((symbol-function 'skg-begin-maintenance)
               (lambda (&rest arguments) (setq submitted arguments))))
      (skg-pull-all))
    (should (equal (car submitted) "pull"))
    (should (eq (nth 4 submitted) #'skg--pull-terminal))
    (should (equal (plist-get (nth 5 submitted) :repositories)
                   repositories))
    (should
     (equal
      (nth 6 submitted)
      `((pull-repositories
         (((repository-key . ,key) (sources ("one"))))))))))

(ert-deftest test-skg-pull-allows-retained-report-after-open-publication ()
  (let* ((key (skg--pull-repository-key '("one")))
         (repositories `((:key ,key :root "/client/repository/"
                               :sources ("one"))))
         (skg--active-source-set-name "all")
         (skg--maintenance-client-incident
          '(:incident-id "old-report" :phase terminal))
         (skg--graph-write-admission 'open)
         submitted)
    (cl-letf (((symbol-function 'skg--pull-local-repositories)
               (lambda () repositories))
              ((symbol-function 'skg--pull-dirty-buffers) (lambda () nil))
              ((symbol-function 'skg-begin-maintenance)
               (lambda (&rest arguments) (setq submitted arguments))))
      (skg-pull-all))
    (should (equal (car submitted) "pull"))))

(ert-deftest test-skg-pull-refuses-during-an-active-incident ()
  (let ((skg--active-source-set-name "all")
        (skg--maintenance-client-incident
         '(:incident-id "active" :phase settling))
        (skg--graph-write-admission nil))
    (should-error (skg-pull-all) :type 'user-error)))

(ert-deftest test-skg-pull-bootstrap-sends-only-the-logical-repository-map ()
  (let* ((key (skg--pull-repository-key '("one")))
         (fields
          `((pull-repositories
             (((repository-key . ,key) (sources ("one")))))))
         request)
    (cl-letf (((symbol-function 'skg-tcp-connect-to-rust) (lambda () 'tcp))
              ((symbol-function 'skg-register-response-handler)
               (lambda (&rest _arguments) nil))
              ((symbol-function 'skg-submit-request)
               (lambda (_tcp wire &rest _arguments) (setq request wire))))
      (skg-begin-maintenance "pull" nil nil nil nil nil fields))
    (let* ((parsed (read request))
           (mapping (cadr (assoc 'pull-repositories parsed)))
           (record (car mapping)))
      (should (equal (cdr (assoc 'repository-key record)) key))
      (should (equal (cadr (assoc 'sources record)) '("one")))
      (should-not (string-match-p "/client/" request)))))

(ert-deftest test-skg-pull-refuses-a-dirty-raw-file-before-maintenance ()
  (let ((buffer (generate-new-buffer " *skg-pull-raw*"))
        (skg--active-source-set-name "all")
        (skg--maintenance-client-incident nil)
        began)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq skg--buffer-record
                  (make-skg--buffer-record :kind 'raw-skg-file)))
          (cl-letf (((symbol-function 'skg--pull-local-repositories)
                     (lambda () '((:key "repo" :root "/repo/"))))
                    ((symbol-function 'skg--pull-dirty-buffers)
                     (lambda () (list buffer)))
                    ((symbol-function 'skg-begin-maintenance)
                     (lambda (&rest _arguments) (setq began t))))
            (should-error (skg-pull-all) :type 'user-error)
            (should-not began)))
      (kill-buffer buffer))))

(ert-deftest test-skg-pull-starts-git-with-an-argument-vector ()
  (let* ((buffer (generate-new-buffer " *skg-pull-process*"))
         (marker (with-current-buffer buffer (point-marker)))
         (context
          (list :repositories nil
                :remaining '((:key "key" :root "/repo with space/"))
                :current-process nil :details nil :failures nil
                :diagnostic-buffer buffer))
         (skg--maintenance-client-incident
          (list :incident-id "incident" :epoch 4 :origin "pull"
                :origin-context context))
         process-arguments)
    (unwind-protect
        (cl-letf (((symbol-function 'make-process)
                   (lambda (&rest arguments)
                     (setq process-arguments arguments)
                     'fake-process))
                  ((symbol-function 'process-put)
                   (lambda (&rest _arguments) nil))
                  ((symbol-function 'process-mark) (lambda (_process) marker))
                  ((symbol-function 'display-buffer) (lambda (_buffer) nil)))
          (skg--pull-start-next)
          (should
           (equal (plist-get process-arguments :command)
                  '("git" "-C" "/repo with space/" "pull")))
          (should (functionp (plist-get process-arguments :sentinel)))
          (should (eq (plist-get process-arguments :filter)
                      #'skg--pull-process-filter))
          (should (eq (plist-get context :current-process) 'fake-process))
          (should-not (plist-get context :remaining)))
      (kill-buffer buffer))))

(ert-deftest test-skg-pull-generated-diagnostic-output-remains-clean ()
  (let* ((skg--buffer-registry (make-hash-table :test #'equal))
         (skg--maintenance-state nil)
         (context (list :diagnostic-buffer nil))
         (skg--maintenance-client-incident
          (list :incident-id "incident" :epoch 4 :origin "pull"
                :origin-context context))
         buffer)
    (unwind-protect
        (progn
          (setq buffer (skg--pull-diagnostic-buffer context))
          (skg--pull-append-diagnostic buffer "generated output\n")
          (with-current-buffer buffer
            (should-not (skg-buffer-dirty-p buffer))
            (should
             (equal (skg-buffer-raw-text buffer)
                    (skg--buffer-record-last-fetched skg--buffer-record)))
            (should
             (equal (skg--sha256-text (skg-buffer-raw-text buffer))
                    (skg--buffer-record-last-fetched-sha256
                     skg--buffer-record)))))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest test-skg-pull-replayed-authorization-reports-child-loss ()
  (let* ((context (list :current-process nil))
         (skg--maintenance-client-incident
          (list :incident-id "incident" :epoch 4 :origin "pull"
                :origin-context context))
         scheduled)
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (_seconds _repeat function &rest arguments)
                 (setq scheduled (cons function arguments)))))
      (skg--pull-handle-authorization
       nil
       (concat "((status external-mutation-authorized)"
               " (incident-id incident) (maintenance-epoch 4)"
               " (phase running-external-mutation) (replayed true))")))
    (should (eq (nth 2 scheduled) #'skg--pull-finish-origin))
    (should (equal (car (nth 3 scheduled)) "indeterminate"))
    (should (string-match-p "without a live owned Git child"
                            (car (cadr (nth 3 scheduled)))))))

(ert-deftest test-skg-pull-running-reconnect-resends-a-known-result ()
  (let* ((record '(:outcome "failed" :details ("repo failed")))
         (context (list :current-process nil :external-result record))
         (skg--maintenance-client-incident
          (list :incident-id "incident" :epoch 4 :origin "pull"
                :origin-context context))
         scheduled)
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (_seconds _repeat function &rest arguments)
                 (setq scheduled (cons function arguments)))))
      (skg--pull-resume-running "lost"))
    (should (eq (nth 2 scheduled) #'skg--pull-send-finish))
    (should (eq (car (nth 3 scheduled)) record))))

(ert-deftest test-skg-pull-running-reconnect-preserves-a-scheduled-hop ()
  (let* ((context (list :current-process nil :advance-timer 'pending))
         (skg--maintenance-client-incident
          (list :incident-id "incident" :epoch 4 :origin "pull"
                :origin-context context)))
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (&rest _arguments) (ert-fail "duplicated pull work"))))
      (should (skg--pull-resume-running "lost")))))

(ert-deftest test-skg-pull-final-observation-restores-server-result ()
  (let* ((sources '("one"))
         (key (skg--pull-repository-key sources))
         (repositories `((:key ,key :root "/repo/" :sources ,sources)))
         (context (list :repositories repositories
                        :server-repositories nil :external-result nil))
         (skg--maintenance-client-incident
          (list :incident-id "incident" :epoch 4 :origin "pull"
                :origin-context context))
         scheduled)
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (_seconds _repeat function &rest arguments)
                 (setq scheduled (cons function arguments)))))
      (skg--pull-origin-handler
       "final-observation"
       `((pull-repositories
          (((repository-key ,key) (sources ,sources))))
         (external-outcome failed)
         (external-details ("repo failed" "disk may differ")))))
    (should (equal (plist-get context :server-repositories)
                   `((:key ,key :sources ,sources))))
    (should (equal (plist-get context :external-result)
                   '(:outcome "failed"
                     :details ("repo failed" "disk may differ"))))
    (should (eq (nth 2 scheduled) #'skg--pull-send-finish))
    (should (equal (car (nth 3 scheduled))
                   (plist-get context :external-result)))))

(ert-deftest test-skg-pull-refuses-a-server-repository-topology-change ()
  (let* ((local-sources '("one" "two"))
         (local-key (skg--pull-repository-key local-sources))
         (server-key (skg--pull-repository-key '("one")))
         (context
          (list :repositories
                `((:key ,local-key :root "/repo/" :sources ,local-sources))))
         (skg--maintenance-client-incident
          (list :incident-id "incident" :epoch 4 :origin "pull"
                :origin-context context)))
    (should-error
     (skg--pull-install-server-repositories
      `((pull-repositories
         (((repository-key ,server-key) (sources ("one"))))))))))

(ert-deftest test-skg-pull-completion-reports-exact-external-outcome ()
  (let* ((context (list :external-result nil))
         (skg--maintenance-client-incident
          (list :incident-id "incident" :epoch 4 :origin "pull" :phase 'test
                :origin-context context))
         request request-incident registered-handler)
    (cl-letf (((symbol-function 'skg-tcp-connect-to-rust) (lambda () 'tcp))
              ((symbol-function 'skg-register-response-handler)
               (lambda (kind handler &optional _one-shot)
                 (should (eq kind 'maintenance-status))
                 (setq registered-handler handler)))
              ((symbol-function 'skg-set-request-failure-handler)
               (lambda (_handler) nil))
              ((symbol-function 'skg-submit-request)
               (lambda (_tcp wire &optional _content incident)
                 (setq request wire request-incident incident))))
      (skg--pull-finish-origin "failed" '("one failed" "disk may differ")))
    (let ((parsed (read request)))
      (should (equal (cdr (assoc 'request parsed))
                     "finish maintenance origin"))
      (should (equal (cdr (assoc 'maintenance-epoch parsed)) 4))
      (should (equal (cdr (assoc 'external-outcome parsed)) "failed"))
      (should (equal (cdr (assoc 'external-details parsed))
                     '("one failed" "disk may differ"))))
    (should (equal request-incident "incident"))
    (should (functionp registered-handler))
    (should (eq (plist-get skg--maintenance-client-incident :phase)
                'origin-completion-pending))))

(ert-deftest test-skg-pull-delayed-request-callbacks-retain-their-incident ()
  (let ((skg--maintenance-client-incident nil)
        (skg--maintenance-client-incidents nil)
        (a (list :incident-id "a" :epoch 9 :origin "pull"))
        (b (list :incident-id "b" :epoch 10 :phase 'foreground))
        handler failure)
    (skg--maintenance-replace-current-incident a)
    (cl-letf (((symbol-function 'skg-tcp-connect-to-rust) (lambda () 'tcp))
              ((symbol-function 'skg-register-response-handler)
               (lambda (_kind callback &rest _) (setq handler callback)))
              ((symbol-function 'skg-set-request-failure-handler)
               (lambda (callback) (setq failure callback)))
              ((symbol-function 'skg-submit-request) #'ignore)
              ((symbol-function 'display-warning) #'ignore))
      (skg--pull-send-finish '(:outcome "completed" :details nil))
      (skg--maintenance-replace-current-incident b)
      (funcall failure "lost reply")
      (should (eq 'origin-completion-pending
                  (plist-get (skg--maintenance-lookup-incident "a") :phase)))
      (should (eq b skg--maintenance-client-incident))
      (funcall handler nil
               "((status origin-operation-finished) (incident-id a)
                 (maintenance-epoch 9) (phase final-observation))")
      (should (eq 'waiting-for-origin-observation
                  (plist-get (skg--maintenance-lookup-incident "a") :phase)))
      (should (eq b skg--maintenance-client-incident))
      (should (eq 'foreground (plist-get b :phase))))))

(ert-deftest test-skg-pull-owned-process-exit-and-next-hop-preserve-foreground ()
  (let* ((buffer (generate-new-buffer " *skg-pull-scoping*"))
         (marker (with-current-buffer buffer (point-marker)))
         (context (list :repositories nil
                        :remaining '((:key "repo" :root "/repo/"))
                        :current-process nil :details nil :failures nil
                        :advance-timer nil :diagnostic-buffer buffer))
         (a (list :incident-id "a" :epoch 9 :origin "pull" :origin-context context))
         (b (list :incident-id "b" :epoch 10 :origin "pull"
                  :origin-context (list :current-process 'process-b)))
         (properties (make-hash-table))
         (skg--maintenance-client-incident nil)
         (skg--maintenance-client-incidents nil)
         sentinel scheduled finished-id)
    (unwind-protect
        (cl-letf (((symbol-function 'make-process)
                   (lambda (&rest args)
                     (setq sentinel (plist-get args :sentinel)) 'process-a))
                  ((symbol-function 'process-put)
                   (lambda (_process key value) (puthash key value properties)))
                  ((symbol-function 'process-get)
                   (lambda (_process key) (gethash key properties)))
                  ((symbol-function 'process-status) (lambda (_) 'exit))
                  ((symbol-function 'process-exit-status) (lambda (_) 0))
                  ((symbol-function 'process-buffer) (lambda (_) buffer))
                  ((symbol-function 'process-mark) (lambda (_) marker))
                  ((symbol-function 'display-buffer) #'ignore)
                  ((symbol-function 'run-at-time)
                   (lambda (_seconds _repeat function &rest args)
                     (setq scheduled (cons function args)) 'timer))
                  ((symbol-function 'skg--pull-send-finish)
                   (lambda (_record)
                     (setq finished-id
                           (plist-get skg--maintenance-client-incident :incident-id)))))
          (skg--maintenance-replace-current-incident a)
          (skg--pull-start-next)
          (skg--maintenance-replace-current-incident b)
          (funcall sentinel 'process-a "finished\n")
          (should (eq b skg--maintenance-client-incident))
          (should-not (plist-get context :current-process))
          (should (eq 'process-b (plist-get (plist-get b :origin-context) :current-process)))
          (apply (car scheduled) (cdr scheduled))
          (should (equal "a" finished-id))
          (should (eq b skg--maintenance-client-incident))
          (should (equal '("/repo/: finished") (plist-get context :details))))
      (kill-buffer buffer))))

(provide 'test-skg-pull)
