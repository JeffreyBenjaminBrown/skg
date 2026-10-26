;;; Integration test: skg-goto-in-magit on a ghost whose file is
;;; gone from disk.
;;;
;;; HEAD:       a contains [b]; a.skg and b.skg both committed.
;;; Worktree:   a no longer contains b; b.skg deleted from disk.
;;;
;;; Phase 1: Open the diff view.
;;; Phase 2: In its "all changes" buffer, b appears as a ghost.
;;; Phase 3: Call skg-goto-in-magit on the ghost b.
;;; Phase 4: Verify magit opened on the skg-data skgrepo (not the outer
;;;          skg project skgrepo), and that point is on b.skg.

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
  "The diff view's \"all changes\" buffer."
  (cl-find-if
   (lambda (b) (string-match-p "diff-view: all changes" (buffer-name b)))
   (buffer-list)))

(defun phase-1-open-the-diff-view ()
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
  (skg-diffView-open)
  (unless (skg-test-wait-for
           (lambda ()
             (let ((buf (find-content-view-buffer)))
               (when buf
                 (with-current-buffer buf
                   (string-match-p "(id a)"
                                   (buffer-substring-no-properties
                                    (point-min) (point-max))))))))
    (test-fail "Timeout waiting for the \"all changes\" buffer"))
  (test-pass "The \"all changes\" buffer loaded"))

(defun phase-2-verify-phantom-b ()
  (setq integration-test-phase "phase-2")
  (let ((content (with-current-buffer (find-content-view-buffer)
                   (buffer-substring-no-properties
                    (point-min) (point-max)))))
    (unless (string-match-p "(ghost (gnid b)" content)
      (test-fail "No ghost b in buffer. Content:\n%s" content))
    (test-pass "Ghost b present")))

(defun phase-3-call-view-magit-on-phantom-b ()
  (setq integration-test-phase "phase-3")
  (let ((buf (find-content-view-buffer)))
    (switch-to-buffer buf)
    (goto-char (point-min))
    (unless (re-search-forward "(ghost (gnid b)" nil t)
      (test-fail "Could not find ghost b line"))
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
         (expected-skgrepo
          (file-truename
           (directory-file-name
            (expand-file-name
             "data/owned/skg-data"
             (file-name-directory load-file-name))))))
    ;; In batch mode, magit-status-setup-buffer creates the buffer
    ;; but doesn't make it current; switch explicitly.
    (switch-to-buffer magit-buf)
    (let ((actual-skgrepo
           (file-truename
            (directory-file-name (magit-toplevel)))))
      (unless (string-equal expected-skgrepo actual-skgrepo)
        (test-fail
         "magit opened on wrong repo.\n  Expected: %s\n  Got:      %s"
         expected-skgrepo actual-skgrepo))
      (test-pass "magit rooted at the skg-data repo (%s)" actual-skgrepo))
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
  (phase-1-open-the-diff-view)
  (phase-2-verify-phantom-b)
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
