;;; Integration test: skg-view-magit on a removed-here phantom.
;;;
;;; Graph at HEAD: a contains [b].
;;; Worktree:      a contains [].
;;;
;;; Phase 1: Open content view from a.
;;; Phase 2: Toggle diff mode on.
;;;          b appears as a removed-here phantom under a.
;;; Phase 3: Call skg-view-magit on the phantom b.
;;; Phase 4: Verify magit opened on a.skg's diff,
;;;          point on the line showing deletion of b's ID.

(load-file "../../../elisp/skg-init.el")
(load-file "../test-wait.el")

(unless (require 'magit nil t)
  (message "SKIP: magit not available in this Emacs")
  (kill-emacs 0))

(defvar integration-test-phase "starting")

(defun test-fail (message &rest args)
  "Report test failure and exit."
  (apply #'message (concat "✗ FAIL: " message) args)
  (kill-emacs 1))

(defun test-pass (message &rest args)
  "Report test success."
  (apply #'message (concat "✓ PASS: " message) args))

(defun phase-1-open-content-view-from-a ()
  (setq integration-test-phase "phase-1")
  (let ((test-port (getenv "SKG_TEST_PORT")))
    (when test-port
      (setq skg-port (string-to-number test-port))))
  (setq skg-config-dir
        (expand-file-name "data/" (file-name-directory load-file-name)))
  (skg-request-single-root-content-view-from-id "a")
  (unless (skg-test-wait-for
           (lambda ()
             (let ((buf (cl-find-if
                          (lambda (b)
                            (with-current-buffer b
                              (derived-mode-p 'skg-content-view-mode)))
                          (buffer-list))))
               (when buf
                 (with-current-buffer buf
                   (string-match-p "(id a)"
                                   (buffer-substring-no-properties
                                    (point-min) (point-max))))))))
    (test-fail "Timeout waiting for content view of a"))
  (test-pass "Content view of a loaded"))

(defun phase-2-toggle-diff-and-verify-phantom-b ()
  (setq integration-test-phase "phase-2")
  (let ((buf (cl-find-if
               (lambda (b) (with-current-buffer b
                             (derived-mode-p 'skg-content-view-mode)))
               (buffer-list))))
    (switch-to-buffer buf)
    (skg-view-diff-mode)
    (skg-test-wait-for-response 20)
    (let ((content (buffer-substring-no-properties
                    (point-min) (point-max))))
      (unless (string-match-p "removed-here" content)
        (test-fail "No removed-here phantom found. Buffer: %S" content))
      (test-pass "removed-here phantom b present"))))

(defun phase-3-call-view-magit-on-phantom-b ()
  (setq integration-test-phase "phase-3")
  (let ((buf (cl-find-if
               (lambda (b) (with-current-buffer b
                             (derived-mode-p 'skg-content-view-mode)))
               (buffer-list))))
    (switch-to-buffer buf)
    (goto-char (point-min))
    (unless (re-search-forward "removed-here" nil t)
      (test-fail "Could not find removed-here line"))
    (beginning-of-line)
    (message "  Positioned on: %s"
             (buffer-substring-no-properties
              (line-beginning-position) (line-end-position)))
    (skg-view-magit)
    (unless (skg-test-wait-for-response 20)
      (test-fail "Timeout waiting for skg-view-magit response"))
    (sleep-for 1)
    (test-pass "skg-view-magit completed on phantom")))

(defun phase-4-verify-magit-points-to-deletion-of-b-in-a ()
  (setq integration-test-phase "phase-4")
  ;; Check that a magit buffer appeared
  (unless (skg-test-wait-for
           (lambda ()
             (cl-find-if
              (lambda (b) (string-match-p "magit:" (buffer-name b)))
              (buffer-list)))
           5)
    (test-fail "No magit buffer appeared"))
  (test-pass "Magit buffer opened")
  (let ((magit-buf (cl-find-if
                    (lambda (b)
                      (string-match-p "magit:" (buffer-name b)))
                    (buffer-list))))
    ;; Why this needs doing:
    ;; In batch mode, `magit-status-setup-buffer' creates the magit
    ;; buffer but does not make it current in the calling buffer's
    ;; context (process filters restore the previous buffer). Switch
    ;; to it explicitly before reading the line at point.
    (switch-to-buffer magit-buf))
  ;; Verify point is on the deletion line for b's ID
  ;; inside a.skg's diff.  The line should look like:
  ;;   -- b
  ;; i.e. git's "-" (deleted line), then YAML's "- b" (list item).
  (let ((line (buffer-substring-no-properties
               (line-beginning-position) (line-end-position))))
    (message "  Magit line at point: %s" line)
    (unless (string-match-p "^-- b$" line)
      (test-fail "Expected point on deletion line for b, got: %s" line))
    (test-pass "Point is on the deletion line for b in a.skg's diff")))

(defun run-all-tests ()
  (message "=== SKG View-Magit-Phantom Integration Test ===")
  (phase-1-open-content-view-from-a)
  (phase-2-toggle-diff-and-verify-phantom-b)
  (phase-3-call-view-magit-on-phantom-b)
  (phase-4-verify-magit-points-to-deletion-of-b-in-a)
  (message "")
  (test-pass "All view-magit-phantom tests passed!")
  (kill-emacs 0))

(progn
  (run-at-time
   45 nil
   (lambda ()
     (message "TIMEOUT: Integration test timed out!")
     (message "Last phase: %s" integration-test-phase)
     (kill-emacs 1)))
  (run-all-tests))
