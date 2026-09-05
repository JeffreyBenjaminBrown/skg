;;; End-to-end client-owned pull through durable maintenance, Emacs client.

(load-file "../../../elisp/skg-init.el")
(load-file "../test-wait.el")

(defun pull-test-fail (format-string &rest arguments)
  (apply #'message (concat "FAIL: " format-string) arguments)
  (kill-emacs 1))

(defun pull-test-check (condition message)
  (unless condition (pull-test-fail "%s" message))
  (message "ok: %s" message))

(defun pull-test-buffer ()
  (cl-find-if
   (lambda (buffer)
     (with-current-buffer buffer
       (and (derived-mode-p 'skg-content-view-mode)
            (string-match-p
             "(id x)"
             (buffer-substring-no-properties (point-min) (point-max))))))
   (buffer-list)))

(defun pull-test-idle-with-updated-view-p ()
  (let ((buffer (pull-test-buffer)))
    (and buffer
         (null skg--maintenance-client-incident)
         (with-current-buffer buffer
           (string-match-p
            "title after pull"
            (buffer-substring-no-properties (point-min) (point-max)))))))

(defun pull-test-git-status ()
  (with-temp-buffer
    (let ((exit (call-process
                 "git" nil t nil "-C" (getenv "SKG_PULL_REPO")
                 "status" "--porcelain")))
      (if (= exit 0)
          (buffer-substring-no-properties (point-min) (point-max))
        (format "git status exited %s: %s" exit (buffer-string))))))

(defun pull-test-main ()
  (setq skg-port (string-to-number (getenv "SKG_TEST_PORT")))
  (setq skg-config-dir
        (file-name-directory (getenv "SKG_TEST_CONFIG")))

  (skg-request-single-root-content-view-from-id "x")
  (pull-test-check
   (skg-test-wait-for
    (lambda ()
      (let ((buffer (pull-test-buffer)))
        (and buffer
             (with-current-buffer buffer
               (string-match-p
                "title before pull"
                (buffer-substring-no-properties
                 (point-min) (point-max))))))))
   "the pre-pull view loaded")
  (pull-test-check
   (equal skg--maintenance-archive-folder "maintenance-archives")
   (format "the handshake installed the archive folder (got %S)"
           skg--maintenance-archive-folder))

  (skg-pull-all)
  (pull-test-check
   (skg-test-wait-for #'pull-test-idle-with-updated-view-p 60)
   "pull maintenance returned idle with the live view updated")
  (let ((status (pull-test-git-status)))
    (pull-test-check
     (string-empty-p status)
     (format "the pulled worktree is clean (porcelain: %S)" status)))
  (pull-test-check
   (cl-find-if
    (lambda (buffer) (string-match-p "\\*Skg Pull " (buffer-name buffer)))
    (buffer-list))
   "the incident-qualified pull diagnostics remain available")
  (pull-test-check
   (directory-files-recursively
    (expand-file-name "maintenance-archives" skg-config-dir)
    "FINALIZED\\'")
   "a finalized recovery archive remains on disk")
  (message "PASS: client-owned pull completed through Emacs maintenance")
  (kill-emacs 0))

(run-at-time
 75 nil
 (lambda () (pull-test-fail "client-owned pull timed out")))
(pull-test-main)
