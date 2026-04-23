;;; Integration test: skg-goto-in-magit on a phantom whose file is
;;; gone from disk.
;;;
;;; HEAD:       a contains [b]; a.skg and b.skg both committed.
;;; Worktree:   a no longer contains b; b.skg deleted from disk.
;;;
;;; Phase 1: Open content view from a.
;;; Phase 2: Toggle diff mode on. b appears as a phantom under a.
;;; Phase 3: Call skg-goto-in-magit on the phantom b.
;;; Phase 4: Verify magit opened on the skg-data repo (NOT the outer
;;;          skg project repo), and that point is on b.skg.

(load-file "../../../elisp/skg-init.el")
(load-file "../test-wait.el")

(unless (require 'magit nil t)
  (message "SKIP: magit not available in this Emacs")
  (kill-emacs 0))

(defvar integration-test-phase "starting")

(defun test-fail (message &rest args)
  (apply #'message (concat "✗ FAIL: " message) args)
  (kill-emacs 1))

(defun test-pass (message &rest args)
  (apply #'message (concat "✓ PASS: " message) args))

(defun find-content-view-buffer ()
  (cl-find-if
   (lambda (b)
     (with-current-buffer b
       (derived-mode-p 'skg-content-view-mode)))
   (buffer-list)))

(defun phase-1-open-content-view-from-a ()
  (setq integration-test-phase "phase-1")
  (let ((test-port (getenv "SKG_TEST_PORT")))
    (when test-port
      (setq skg-port (string-to-number test-port))))
  ;; skg-config-dir must match the directory containing the config
  ;; file that the server was launched with. Here that's
  ;; $TEST_DIR/data/skgconfig.toml.test, so skg-config-dir is
  ;; $TEST_DIR/data/.
  (setq skg-config-dir
        (expand-file-name "data/" (file-name-directory load-file-name)))
  (skg-request-single-root-content-view-from-id "a")
  (unless (skg-test-wait-for
           (lambda ()
             (let ((buf (find-content-view-buffer)))
               (when buf
                 (with-current-buffer buf
                   (string-match-p "(id a)"
                                   (buffer-substring-no-properties
                                    (point-min) (point-max))))))))
    (test-fail "Timeout waiting for content view of a"))
  (test-pass "Content view of a loaded"))

(defun phase-2-toggle-diff-and-verify-phantom-b ()
  (setq integration-test-phase "phase-2")
  (let ((buf (find-content-view-buffer)))
    (switch-to-buffer buf)
    (skg-view-diff-mode)
    (skg-test-wait-for-response 20)
    (let ((content (buffer-substring-no-properties
                    (point-min) (point-max))))
      ;; Phantom b shows up as a child of a with 'removedM on the
      ;; unstaged side (removed from contains) and 'removedX on the
      ;; unstaged side (b.skg deleted).
      (unless (string-match-p "(id b)" content)
        (test-fail "No (id b) in buffer. Content:\n%s" content))
      (unless (string-match-p "removedX" content)
        (test-fail "No removedX axis on phantom. Content:\n%s" content))
      (test-pass "Phantom b present with removedX axis"))))

(defun phase-3-call-view-magit-on-phantom-b ()
  (setq integration-test-phase "phase-3")
  (let ((buf (find-content-view-buffer)))
    (switch-to-buffer buf)
    (goto-char (point-min))
    (unless (re-search-forward "(id b)" nil t)
      (test-fail "Could not find phantom b line"))
    (beginning-of-line)
    (message "  Positioned on: %s"
             (buffer-substring-no-properties
              (line-beginning-position) (line-end-position)))
    (skg-goto-in-magit)
    (unless (skg-test-wait-for-response 20)
      (test-fail "Timeout waiting for skg-goto-in-magit response"))
    (sleep-for 1)
    (test-pass "skg-goto-in-magit request completed")))

(defun phase-4-verify-magit-root-folder-and-point-position ()
  (setq integration-test-phase "phase-4")
  (unless (skg-test-wait-for
           (lambda ()
             (cl-find-if
              (lambda (b) (string-match-p "magit:" (buffer-name b)))
              (buffer-list)))
           5)
    (test-fail "No magit buffer appeared"))
  (test-pass "Magit buffer opened")
  (let* ((magit-buf
          (cl-find-if
           (lambda (b) (string-match-p "magit:" (buffer-name b)))
           (buffer-list)))
         (expected-repo
          (file-truename
           (directory-file-name
            (expand-file-name
             "data/skg-data"
             (file-name-directory load-file-name))))))
    ;; In batch mode, magit-status-setup-buffer creates the buffer
    ;; but doesn't make it current; switch explicitly.
    (switch-to-buffer magit-buf)
    (let ((actual-repo
           (file-truename
            (directory-file-name (magit-toplevel)))))
      (unless (string-equal expected-repo actual-repo)
        (test-fail
         "magit opened on wrong repo.\n  Expected: %s\n  Got:      %s"
         expected-repo actual-repo))
      (test-pass "magit rooted at the skg-data repo (%s)" actual-repo))
    ;; Point should have been moved to the b.skg section. Magit's
    ;; rendering of a deleted file section includes "b.skg" as the
    ;; filename; we match that substring on the current line.
    (let ((line (buffer-substring-no-properties
                 (line-beginning-position) (line-end-position))))
      (message "  Magit line at point: %s" line)
      (unless (string-match-p "b\\.skg" line)
        (test-fail "Expected point on b.skg line, got: %s" line))
      (test-pass "Point is on the b.skg line in magit"))))

(defun run-all-tests ()
  (message "=== SKG View-Magit-Phantom-Deleted-File Integration Test ===")
  (phase-1-open-content-view-from-a)
  (phase-2-toggle-diff-and-verify-phantom-b)
  (phase-3-call-view-magit-on-phantom-b)
  (phase-4-verify-magit-root-folder-and-point-position)
  (message "")
  (test-pass "All view-magit-phantom-deleted-file tests passed!")
  (kill-emacs 0))

(progn
  (run-at-time
   45 nil
   (lambda ()
     (message "TIMEOUT: Integration test timed out!")
     (message "Last phase: %s" integration-test-phase)
     (kill-emacs 1)))
  (run-all-tests))
