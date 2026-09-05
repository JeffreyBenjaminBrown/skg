;;; End-to-end full rebuild through durable maintenance, Emacs client. -*- lexical-binding: t; -*-

(load-file "../../../elisp/skg-init.el")
(load-file "../test-wait.el")

(defun rebuild-test-fail (format-string &rest arguments)
  (apply #'message (concat "FAIL: " format-string) arguments)
  (kill-emacs 1))

(defun rebuild-test-check (condition message)
  (unless condition (rebuild-test-fail "%s" message))
  (message "ok: %s" message))

(defun rebuild-test-buffer ()
  (cl-find-if
   (lambda (buffer)
     (with-current-buffer buffer
       (and (derived-mode-p 'skg-content-view-mode)
            (string-match-p
             "(id x)"
             (buffer-substring-no-properties (point-min) (point-max))))))
   (buffer-list)))

(defun rebuild-test-replace-config ()
  (let ((config (getenv "SKG_TEST_CONFIG")))
    (with-temp-buffer
      (insert-file-contents config)
      (dolist (replacement
               '(("default_source_set = \"main\""
                  "default_source_set = \"replacement\"")
                 ("name = \"main\"" "name = \"replacement\"")
                 ("path = \"notes\"" "path = \"replacement-notes\"")))
        (goto-char (point-min))
        (unless (search-forward (car replacement) nil t)
          (rebuild-test-fail "config text was absent: %s" (car replacement)))
        (replace-match (cadr replacement) t t))
      (write-region (point-min) (point-max) config nil 'silent))))

(defun rebuild-test-main ()
  (setq skg-port (string-to-number (getenv "SKG_TEST_PORT")))
  (setq skg-config-dir
        (file-name-directory (getenv "SKG_TEST_CONFIG")))

  (skg-request-single-root-content-view-from-id "x")
  (let ((view
         (skg-test-wait-for
          (lambda ()
            (let ((buffer (rebuild-test-buffer)))
              (and buffer
                   (with-current-buffer buffer
                     (string-match-p
                      "title before rebuild"
                      (buffer-substring-no-properties
                       (point-min) (point-max))))
                   buffer))))))
    (rebuild-test-check view "the pre-rebuild view loaded")
    (rebuild-test-check
     (equal skg--active-source-set-name "main")
     "the initial restricted source-set is active")
    (let ((ordinary-handler
           (alist-get "full-rebuild"
                      skg--maintenance-origin-operation-handlers
                      nil nil #'equal)))
      (skg-register-maintenance-origin-handler
       "full-rebuild"
       (lambda (phase response)
         (when (equal phase "archive-ready")
           (rebuild-test-replace-config))
         (funcall ordinary-handler phase response))))
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
     "the client installed the replacement source inventory"))
  (rebuild-test-check
   (directory-files-recursively
    (expand-file-name "maintenance-archives" skg-config-dir)
    "FINALIZED\\'")
   "a finalized recovery archive remains on disk")
  (message "PASS: full rebuild completed through Emacs maintenance")
  (kill-emacs 0))

(run-at-time 75 nil (lambda () (rebuild-test-fail "full rebuild timed out")))
(rebuild-test-main)
