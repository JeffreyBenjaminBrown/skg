;;; Integration test for collateral buffer updates via save pipeline
;;;
;;; Opens two buffers over a containment cycle (a contains b, b contains a),
;;; removes 'a' from b's children and saves. The collateral a-view should
;;; lose the now-stale indefinitive 'a' underneath 'b', because
;;; complete_relevant_children discards children not in the parent's goal_list.
;;;
;;; This test does NOT exercise DeletedNode — no nodes are deleted from disk,
;;; only a containment relationship is modified.

;; Load the project elisp configuration
(load-file "../../../elisp/skg-init.el")
(load-file "test-helpers.el")

;; Test result tracking
(defvar integration-test-phase "starting")
(defvar integration-test-completed nil)

(defun phase-1-open-buffer-a ()
  "Open buffer A and verify its initial structure."
  (message "=== PHASE 1: Open buffer A ===")
  (setq integration-test-phase "phase-1-open-buffer-a")
  (skg-request-single-root-content-view-from-id "a")
  (sleep-for 0.5)
  (let ((buf (get-buffer "*skg: a*")))
    (unless buf
      (message "✗ FAIL [phase 1]: Buffer *skg: a* was not created")
      (kill-emacs 1))
    (assert-headline-structure
     buf
     '((1 . "a") (2 . "b") (3 . "a"))
     "phase 1: buffer A initial")))

(defun phase-2-open-buffer-b ()
  "Open buffer B and verify its initial structure."
  (message "=== PHASE 2: Open buffer B ===")
  (setq integration-test-phase "phase-2-open-buffer-b")
  (skg-request-single-root-content-view-from-id "b")
  (sleep-for 0.5)
  (let ((buf (get-buffer "*skg: b*")))
    (unless buf
      (message "✗ FAIL [phase 2]: Buffer *skg: b* was not created")
      (kill-emacs 1))
    (assert-headline-structure
     buf
     '((1 . "b") (2 . "a") (3 . "b"))
     "phase 2: buffer B initial")))

(defun phase-3-remove-and-save ()
  "Remove 'a' from b's children and save buffer B."
  (message "=== PHASE 3: Remove a from b's children, save ===")
  (setq integration-test-phase "phase-3-remove-and-save")
  (with-current-buffer "*skg: b*"
    (goto-char (point-min))
    (forward-line 1)
    (delete-region (point) (point-max))
    (message "Buffer B after edit: %S"
             (buffer-substring-no-properties (point-min) (point-max)))
    (skg-request-save-buffer))
  (sleep-for 1.0))

(defun phase-4-verify-collateral ()
  "Verify that collateral buffer A was updated."
  (message "=== PHASE 4: Verify collateral buffer A ===")
  (setq integration-test-phase "phase-4-verify-collateral")
  (let ((buf (get-buffer "*skg: a*")))
    (unless buf
      (message "✗ FAIL [phase 4]: Buffer *skg: a* no longer exists")
      (kill-emacs 1))
    (message "Buffer A content after collateral update: %S"
             (with-current-buffer buf
               (buffer-substring-no-properties (point-min) (point-max))))
    (assert-headline-structure
     buf
     '((1 . "a") (2 . "b"))
     "phase 4: buffer A after collateral update")))

(defun run-all-tests ()
  "Main orchestrator function that runs all integration tests."
  (message "=== SKG Save Collateral Buffers Integration Test ===")

  ;; Set the port from environment variable if available
  (let ((test-port (getenv "SKG_TEST_PORT")))
    (when test-port
      (setq skg-port (string-to-number test-port))
      (message "Using test port: %d" skg-port)))

  ;; Wait a moment for server to be fully ready
  (sleep-for 0.25)

  (phase-1-open-buffer-a)
  (phase-2-open-buffer-b)
  (phase-3-remove-and-save)
  (phase-4-verify-collateral)

  (message "✓ PASS: All phases completed successfully!")
  (setq integration-test-completed t)
  (kill-emacs 0))

(progn ;; Run the test with a timeout in case things hang.
  (run-at-time
   20 nil
   (lambda ()
     (message "TIMEOUT: Integration test timed out!")
     (message "Last phase: %s" integration-test-phase)
     (kill-emacs 1)))
  ( ;; the test
   run-all-tests))
