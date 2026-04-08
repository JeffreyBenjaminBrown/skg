;;; Integration test for skg-view-magit
;;; Tests that skg-view-magit opens magit-status and reports staging state.

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

(defun phase-0-init ()
  "Connect to skg server, request content view of x."
  (setq integration-test-phase "init")
  (message "=== PHASE 0: Init and request content view ===")
  (let (( test-port (getenv "SKG_TEST_PORT") ))
    (when test-port
      (setq skg-port (string-to-number test-port))
      (message "Using test port: %d" skg-port)))
  (setq skg-config-dir
        (expand-file-name "data/" (file-name-directory load-file-name)))
  (skg-request-single-root-content-view-from-id "x")
  (unless (skg-test-wait-for
           (lambda ()
             (let (( buf (cl-find-if
                          (lambda (b)
                            (with-current-buffer b
                            (derived-mode-p 'skg-content-view-mode)))
                          (buffer-list)) ))
               (when buf
                 (with-current-buffer buf
                   (string-match-p "(id x)"
                                   (buffer-substring-no-properties
                                    (point-min) (point-max)) ))))) )
    (test-fail "Timeout waiting for content view of x"))
  (test-pass "Content view of x loaded"))

(defun phase-1-magit-from-x ()
  "From line 1 (node x), skg-view-magit should show unstaged changes."
  (setq integration-test-phase "magit-from-x")
  (message "=== PHASE 1: skg-view-magit from x (modified file) ===")
  (let (( buf (cl-find-if
               (lambda (b) (with-current-buffer b
                            (derived-mode-p 'skg-content-view-mode)))
               (buffer-list)) ))
    (switch-to-buffer buf)
    (goto-char (point-min))
    (message "Buffer content: %s"
             (buffer-substring-no-properties (point-min) (point-max)))
    (skg-view-magit)
    (unless (skg-test-wait-for-response)
      (test-fail "Timeout waiting for file path response"))
    ;; Give magit a moment to render
    (sleep-for 1)
    ;; Check that we're in a magit buffer
    (unless (skg-test-wait-for
             (lambda ()
               (cl-find-if
                (lambda (b) (string-match-p "magit:" (buffer-name b)))
                (buffer-list)))
             5)
      (test-fail "No magit buffer appeared"))
    (test-pass "Magit buffer opened for x")
    ;; Verify point is on the x.skg line
    (let (( line (buffer-substring-no-properties
                  (line-beginning-position) (line-end-position)) ))
      (message "Magit line at point: %s" line)
      (unless (string-match-p "x\\.skg" line)
        (test-fail "Expected point on x.skg line, got: %s" line))
      (unless (string-match-p "modified" line)
        (test-fail "Expected 'modified' on x.skg line, got: %s" line))
      (test-pass "Point is on 'modified x.skg' line"))))

(defun phase-2-magit-from-y ()
  "From line 2 (node y), skg-view-magit should report no changes."
  (setq integration-test-phase "magit-from-y")
  (message "=== PHASE 2: skg-view-magit from y (unchanged file) ===")
  ;; Switch back to the skg content view
  (let (( buf (cl-find-if
               (lambda (b) (with-current-buffer b
                            (derived-mode-p 'skg-content-view-mode)))
               (buffer-list)) ))
    (switch-to-buffer buf)
    ;; Go to line 2 (node y)
    (goto-char (point-min))
    (forward-line 1)
    (message "Line 2: %s"
             (buffer-substring-no-properties
              (line-beginning-position) (line-end-position)))
    (skg-view-magit)
    (unless (skg-test-wait-for-response)
      (test-fail "Timeout waiting for file path response for y"))
    ;; Give magit a moment
    (sleep-for 1)
    (test-pass "skg-view-magit completed for y")))

(defun integration-test-main ()
  "Main test orchestrator."
  (message "=== SKG View-Magit Integration Test ===")
  (phase-0-init)
  (phase-1-magit-from-x)
  (phase-2-magit-from-y)
  (message "")
  (test-pass "All view-magit tests passed!")
  (kill-emacs 0))

(progn
  (run-at-time
   30 nil
   (lambda ()
     (message "TIMEOUT: Integration test timed out!")
     (message "Last phase: %s" integration-test-phase)
     (kill-emacs 1)))
  (integration-test-main))
