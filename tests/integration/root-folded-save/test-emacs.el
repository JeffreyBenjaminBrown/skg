;;; Integration test: save a single-root content view that is
;;; entirely folded (org-startup-folded=t, overview mode), point on
;;; root. Verifies the root line survives the save round-trip.

(load-file "../../../elisp/skg-init.el")
(load-file "../test-wait.el")

;; Critical: mirror the user's custom-set-variables setting.
(setq org-startup-folded t)

(defvar integration-test-phase "starting")
(defvar integration-test-completed nil)

(defun run-all-tests ()
  (message "=== SKG Root-Folded-Save Integration Test ===")
  (let ((test-port (getenv "SKG_TEST_PORT")))
    (when test-port
      (setq skg-port (string-to-number test-port))
      (message "Using test port: %d" skg-port)))
  (test-root-folded-save)
  (let ((timeout 0))
    (while (and (not integration-test-completed) (< timeout 100))
      (sleep-for 0.25)
      (setq timeout (1+ timeout))))
  (unless integration-test-completed
    (message "TIMEOUT: No complete response received!")
    (message "Last phase: %s" integration-test-phase)
    (kill-emacs 1)))

(defun fail (fmt &rest args)
  (message "✗ FAIL: %s" (apply 'format fmt args))
  (kill-emacs 1))

(defun test-root-folded-save ()
  (message "=== PHASE 1: request single-root view from server ===")
  ;; Mirror what skg-open-org-buffer-from-text does, but WITHOUT going
  ;; through the async dispatcher — we want the flow deterministic.
  ;; The text we insert is the text the server would return for the
  ;; initial view; we then activate skg-content-view-mode so that
  ;; org-startup-folded=t takes effect, exactly as in the real flow.
  (let ((buffer (get-buffer-create "*skg-root-folded-save*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)) (erase-buffer))
      (insert
       (concat
        "* (skg (node (id rfs-root) (source main))) rfs-root\n"
        "rfs-root body\n"
        "** (skg (node (id rfs-c1) (source main))) rfs-c1\n"
        "rfs-c1 body\n"
        "** (skg (node (id rfs-c2) (source main))) rfs-c2\n"
        "rfs-c2 body\n"
        "*** (skg (node (id rfs-g1) (source main))) rfs-g1\n"
        "rfs-g1 body\n"
        "** (skg (node (id rfs-c3) (source main))) rfs-c3\n"
        "rfs-c3 body\n"))
      (skg-content-view-mode)
      (setq skg-view-uri (org-id-uuid))
      (goto-char (point-min))

      (message "=== visibility after mode activation (org-startup-folded=t) ===")
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (when (looking-at org-heading-regexp)
            (message "  %s | invisible: %s"
                     (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position))
                     (invisible-p (point))))
          (forward-line 1)))

      (message "=== PHASE 2: save buffer ===")
      (setq integration-test-phase "saving")
      (condition-case err
          (skg-request-save-buffer)
        (error
         (message "CAUGHT ERROR during save: %S" err)
         (fail "skg-request-save-buffer raised: %S" err)))

      (skg-test-wait-for-response)

      (setq integration-test-phase "checking-result")
      (let* ((result (buffer-substring-no-properties
                      (point-min) (point-max)))
             (first-line
              (save-excursion
                (goto-char (point-min))
                (buffer-substring-no-properties
                 (line-beginning-position) (line-end-position)))))
        (message "=== PHASE 3: result ===")
        (message "%s" result)
        (message "First line: %s" first-line)

        (unless (string-match-p "(id rfs-root)" result)
          (fail "Result lost the root headline. First line: %s" first-line))
        (unless (string-match-p "^\\* " first-line)
          (fail "First line is not a level-1 headline: %s" first-line))

        (message "✓ PASS: root survived save round-trip")
        (setq integration-test-completed t)
        (kill-emacs 0)))))

(progn
  (run-at-time
   30 nil
   (lambda ()
     (message "TIMEOUT: Integration test timed out!")
     (message "Last phase: %s" integration-test-phase)
     (kill-emacs 1)))
  (run-all-tests))
