;;; test-skg-pull.el --- Client-owned pull maintenance tests -*- lexical-binding: t; -*-

(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             (file-name-directory load-file-name)))
(require 'ert)
(require 'skg-pull)

(ert-deftest test-skg-pull-groups-local-sources-by-canonical-git-root ()
  (let ((skg--server-source-inventory
         '((:name "one") (:name "three") (:name "two"))))
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
             (substring
              (secure-hash 'sha256
                           (mapconcat #'identity names (string 0)))
              0 16))
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
  (let* ((repositories
          '((:key "repo" :root "/client/repo/" :sources ("one"))))
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
                   repositories))))

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
          (should (eq (plist-get process-arguments :sentinel)
                      #'skg--pull-process-sentinel))
          (should (eq (plist-get context :current-process) 'fake-process))
          (should-not (plist-get context :remaining)))
      (kill-buffer buffer))))

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
    (should (eq (car scheduled) #'skg--pull-finish-origin))
    (should (equal (cadr scheduled) "indeterminate"))
    (should (string-match-p "without a live owned Git child"
                            (car (nth 2 scheduled))))))

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
    (should (eq (car scheduled) #'skg--pull-send-finish))
    (should (eq (cadr scheduled) record))))

(ert-deftest test-skg-pull-running-reconnect-preserves-a-scheduled-hop ()
  (let* ((context (list :current-process nil :advance-timer 'pending))
         (skg--maintenance-client-incident
          (list :incident-id "incident" :epoch 4 :origin "pull"
                :origin-context context)))
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (&rest _arguments) (ert-fail "duplicated pull work"))))
      (should (skg--pull-resume-running "lost")))))

(ert-deftest test-skg-pull-final-observation-restores-server-result ()
  (let* ((context (list :external-result nil))
         (skg--maintenance-client-incident
          (list :incident-id "incident" :epoch 4 :origin "pull"
                :origin-context context))
         scheduled)
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (_seconds _repeat function &rest arguments)
                 (setq scheduled (cons function arguments)))))
      (skg--pull-origin-handler
       "final-observation"
       '((external-outcome failed)
         (external-details ("repo failed" "disk may differ")))))
    (should (equal (plist-get context :external-result)
                   '(:outcome "failed"
                     :details ("repo failed" "disk may differ"))))
    (should (eq (car scheduled) #'skg--pull-send-finish))
    (should (equal (cadr scheduled)
                   (plist-get context :external-result)))))

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
    (should (eq registered-handler #'skg--pull-handle-finish))
    (should (eq (plist-get skg--maintenance-client-incident :phase)
                'origin-completion-pending))))

(provide 'test-skg-pull)
