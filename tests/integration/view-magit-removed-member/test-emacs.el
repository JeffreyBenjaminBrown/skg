;;; Integration test: skg-goto-in-magit-parent on a removed member.
;;;
;;; Graph at HEAD: a contains [b].
;;; Worktree:      a contains [].
;;;
;;; Phase 1: Open the diff view.
;;; Phase 2: In its "all changes" buffer, b appears under a as a
;;;          removed member: write-protected, tagged (diff (presence rel-)).
;;; Phase 3: Call skg-goto-in-magit-parent on b.
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

;;; The line of the diff view's "all changes" buffer that shows
;;; b as a removed member of a.
(defconst test-removed-member-line-regexp
  "(id b).*(diff (presence rel-)")

(defun find-all-changes-buffer ()
  (cl-find-if
   (lambda (b) (string-match-p "diff-view: all changes" (buffer-name b)))
   (buffer-list)))

(defun phase-1-open-the-diff-view ()
  (setq integration-test-phase "phase-1")
  (let ((test-port (getenv "SKG_TEST_PORT")))
    (when test-port
      (setq skg-port (string-to-number test-port))))
  (setq skg-config-dir
        (expand-file-name "data/" (file-name-directory load-file-name)))
  (skg-diffView-open)
  (unless (skg-test-wait-for
           (lambda ()
             (let ((buf (find-all-changes-buffer)))
               (when buf
                 (with-current-buffer buf
                   (string-match-p "(id a)"
                                   (buffer-substring-no-properties
                                    (point-min) (point-max))))))))
    (test-fail "Timeout waiting for the \"all changes\" buffer"))
  (test-pass "The \"all changes\" buffer loaded"))

(defun phase-2-verify-removed-member-b ()
  (setq integration-test-phase "phase-2")
  (let ((content (with-current-buffer (find-all-changes-buffer)
                   (buffer-substring-no-properties
                    (point-min) (point-max)))))
    (unless (string-match-p test-removed-member-line-regexp content)
      (test-fail "No removed member b found. Buffer: %S" content))
    (test-pass "removed member b present")))

(defun phase-3-call-view-magit-on-removed-member-b ()
  (setq integration-test-phase "phase-3")
  (switch-to-buffer (find-all-changes-buffer))
  (goto-char (point-min))
  (unless (re-search-forward test-removed-member-line-regexp nil t)
    (test-fail "Could not find the removed member's line"))
  (beginning-of-line)
  (message "  Positioned on: %s"
           (buffer-substring-no-properties
            (line-beginning-position) (line-end-position)))
  (skg-goto-in-magit-parent)
  (unless (skg-test-wait-for-response 20)
    (test-fail "Timeout waiting for skg-goto-in-magit-parent response"))
  (sleep-for 1)
  (test-pass "skg-goto-in-magit-parent completed on the removed member"))

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
  (message "=== SKG View-Magit-Removed-Member Integration Test ===")
  (phase-1-open-the-diff-view)
  (phase-2-verify-removed-member-b)
  (phase-3-call-view-magit-on-removed-member-b)
  (phase-4-verify-magit-points-to-deletion-of-b-in-a)
  (message "")
  (test-pass "All view-magit-removed-member tests passed!")
  (kill-emacs 0))

(progn
  (run-at-time
   45 nil
   (lambda ()
     (message "TIMEOUT: Integration test timed out!")
     (message "Last phase: %s" integration-test-phase)
     (kill-emacs 1)))
  (run-all-tests))
