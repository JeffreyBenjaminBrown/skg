;;; End-to-end full rebuild through durable maintenance, Emacs client. -*- lexical-binding: t; -*-

(load-file "../../../elisp/skg-init.el")
(load-file "../test-wait.el")

(defun rebuild-test-fail (format-string &rest arguments)
  (apply #'message (concat "FAIL: " format-string) arguments)
  (kill-emacs 1))

(defun rebuild-test-check (condition message)
  (unless condition (rebuild-test-fail "%s" message))
  (message "ok: %s" message))

(defun rebuild-test-buffer (id)
  (cl-find-if
   (lambda (buffer)
     (with-current-buffer buffer
       (and (derived-mode-p 'skg-content-view-mode)
            (string-match-p
             (format "(id %s)" id)
             (buffer-substring-no-properties (point-min) (point-max))))))
   (buffer-list)))

(defun rebuild-test-replace-config ()
  (let ((config (getenv "SKG_TEST_CONFIG")))
    (with-temp-buffer
      (insert-file-contents config)
      (dolist (replacement
               '(("default_source_set = \"main\""
                  "default_source_set = \"replacement\"")
                 ("maintenance_archive_folder = \"maintenance-archives\""
                  "maintenance_archive_folder = \"replacement-archives\"")
                 ("name = \"main\"" "name = \"replacement\"")
                 ("path = \"notes\"" "path = \"replacement-notes\"")))
        (goto-char (point-min))
        (unless (search-forward (car replacement) nil t)
          (rebuild-test-fail "config text was absent: %s" (car replacement)))
        (replace-match (cadr replacement) t t))
      (write-region (point-min) (point-max) config nil 'silent))))

(defun rebuild-test-break-config ()
  (let ((config (getenv "SKG_TEST_CONFIG")))
    (with-temp-buffer
      (insert-file-contents-literally config)
      (setq rebuild-test-valid-config (buffer-string)))
    (with-temp-file config
      (insert "this is not valid TOML = [\n"))))

(defvar rebuild-test-valid-config nil)

(defun rebuild-test-repair-config ()
  (unless rebuild-test-valid-config
    (rebuild-test-fail "no valid replacement config was retained"))
  (with-temp-file (getenv "SKG_TEST_CONFIG")
    (insert rebuild-test-valid-config)))

(defun rebuild-test-main ()
  (setq skg-port (string-to-number (getenv "SKG_TEST_PORT")))
  (setq skg-config-dir
        (file-name-directory (getenv "SKG_TEST_CONFIG")))

  (skg-request-single-root-content-view-from-id "x")
  (let* ((view
          (skg-test-wait-for
           (lambda ()
             (let ((buffer (rebuild-test-buffer "x")))
               (and buffer
                    (with-current-buffer buffer
                      (string-match-p
                       "title before rebuild"
                       (buffer-substring-no-properties
                        (point-min) (point-max))))
                    buffer)))))
         (ordinary-handler
          (alist-get "full-rebuild"
                     skg--maintenance-origin-operation-handlers
                     nil nil #'equal)))
    (rebuild-test-check view "the pre-rebuild view loaded")
    (rebuild-test-check
     (equal skg--active-source-set-name "main")
     "the initial restricted source-set is active")
    (skg-register-maintenance-origin-handler
     "full-rebuild"
     (lambda (phase response)
       (when (equal phase "archive-ready")
         (rebuild-test-replace-config))
       (funcall ordinary-handler phase response)))
    (skg-rebuild-dbs)
    (rebuild-test-check
     (skg-test-wait-for
      (lambda ()
        (and (null skg--maintenance-client-incident)
             (buffer-live-p view)
             (with-current-buffer view
               (string-match-p
                "title after rebuild"
                (buffer-substring-no-properties (point-min) (point-max))))) )
      60)
     "full rebuild returned idle with the same live view reconciled")
    (rebuild-test-check
     (> (or (alist-get 'graph-generation skg--server-store-state) 0) 1)
     "the selected graph generation advanced")
    (rebuild-test-check
     (equal skg--active-source-set-name "all")
     "the absent old source-set fell back exactly to all")
    (rebuild-test-check
     (equal (skg--source-names) '("replacement"))
     "the client installed the replacement source inventory")

    (skg-register-maintenance-origin-handler
     "full-rebuild"
     (lambda (phase response)
       (when (equal phase "archive-ready")
         (rebuild-test-break-config))
       (funcall ordinary-handler phase response)))
    (skg-rebuild-dbs)
    (rebuild-test-check
     (skg-test-wait-for
      (lambda ()
        (with-temp-buffer
          (insert-file-contents (getenv "SKG_TEST_CONFIG"))
          (string-prefix-p "this is not valid TOML" (buffer-string))))
      15)
     "the invalid replacement config reached the archive boundary")
    (rebuild-test-check
     (directory-files-recursively
      (expand-file-name "replacement-archives" skg-config-dir)
      "manifest\\.initial\\.sexp\\'")
     "the next incident used the replacement archive root")
    (accept-process-output nil 1)
    ;; Let the already-started server worker finish while this client is
    ;; deliberately not relying on its best-effort unsolicited failure frame.
    (sleep-for 1)
    (skg-maintenance-status t)
    (rebuild-test-check
     (skg-test-wait-for
      (lambda ()
        (eq (plist-get skg--maintenance-client-incident :phase)
            'server-blocked))
      30)
     "an invalid replacement config blocked before store mutation")
    (skg-request-single-root-content-view-from-id "y")
    (let ((blocked-view
           (skg-test-wait-for
            (lambda ()
              (let ((buffer (rebuild-test-buffer "y")))
                (and buffer
                     (with-current-buffer buffer
                       (string-match-p
                        "queryable after invalid preflight"
                        (buffer-substring-no-properties
                         (point-min) (point-max))))
                     buffer)))
            15)))
      (rebuild-test-check
       blocked-view
       "the selected graph remained queryable after invalid preflight")
      (rebuild-test-check
       (with-current-buffer blocked-view
         (and buffer-read-only
              (eq (skg--buffer-record-view-write-authority skg--buffer-record)
                  'read-only)
              (null (skg--buffer-record-maintenance-epoch skg--buffer-record))))
       "a new read-only query during blocked maintenance stays outside the census")
      (let* ((query-terms "title after rebuild")
             (query-id (skg-query-wait-submit
                        query-terms nil nil nil nil))
             (query-record (gethash query-id skg--query-waits))
             (query-buffer (plist-get query-record :buffer))
             (query-initial-token
              (with-current-buffer query-buffer
                (skg--buffer-record-application-token skg--buffer-record))))
        (rebuild-test-check query-id
                            "a query wait was accepted for the active incident")
        (rebuild-test-check
         (and (buffer-live-p query-buffer)
              (equal query-terms (plist-get query-record :terms))
              (eq (skg--buffer-record-view-write-authority
                   (buffer-local-value 'skg--buffer-record query-buffer))
                  'read-only))
         "the query wait placeholder retains its terms and read-only authority")
      (rebuild-test-repair-config)
      (skg-retry-maintenance)
      (rebuild-test-check
       (skg-test-wait-for
        (lambda ()
          (and (null skg--maintenance-client-incident)
               (eq (plist-get (gethash query-id skg--query-waits) :status)
                   'delivered)))
        60)
       "the repaired incident retried and delivered the query wait result")
      (rebuild-test-check
       (with-current-buffer query-buffer
         (and (string-match-p "title after rebuild" (buffer-string))
              (equal (plist-get (gethash query-id skg--query-waits) :terms)
                     query-terms)
              (= (skg--buffer-record-application-token skg--buffer-record)
                 (1+ query-initial-token))
              (eq (skg--buffer-record-view-write-authority skg--buffer-record)
                  'read-only)))
       "the query wait applied one exact read-only result with expected terms")
      (rebuild-test-check
       (with-current-buffer blocked-view
         (null (skg--buffer-record-maintenance-epoch skg--buffer-record)))
       "the mid-incident view joined terminal settlement and unlocked")))
      )
  (rebuild-test-check
   (directory-files-recursively
    (expand-file-name "maintenance-archives" skg-config-dir)
    "FINALIZED\\'")
   "a finalized recovery archive remains on disk")
  (message "PASS: full rebuild and invalid preflight completed through Emacs maintenance")
  (kill-emacs 0))

(run-at-time 75 nil (lambda () (rebuild-test-fail "full rebuild timed out")))
(rebuild-test-main)
