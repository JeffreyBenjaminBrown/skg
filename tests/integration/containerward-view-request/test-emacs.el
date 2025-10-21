;;; Integration test for containerward-view request functionality
;;; This script tests:
;;; 1. Creating a content view for node "1"
;;; 2. Adding (requests containerward-view) to node "12"
;;; 3. Saving and verifying containerward path is integrated
;;;
;;; NOTE: File system operations (backup/cleanup) are handled by run-test.sh

;; Load the project elisp configuration
(load-file "../../../elisp/skg-init.el")

;; Test result tracking
(defvar integration-test-phase "starting")
(defvar integration-test-completed nil)

(defun run-all-tests ()
  "Main orchestrator function that runs all integration tests."
  (message "=== SKG Containerward View Request Integration Test ===")

  ;; Set the port from environment variable if available
  (let ((test-port (getenv "SKG_TEST_PORT")))
    (when test-port
      (setq skg-port (string-to-number test-port))
      (message "Using test port: %d" skg-port)))

  ;; Wait a moment for server to be fully ready
  (sleep-for 0.25)

  ;; Phase 1: Get initial content view
  (test-initial-content-view)

  ;; Wait for completion with timeout
  (let ((timeout 0))
    (while (and (not integration-test-completed) (< timeout 100))
      (sleep-for 0.25)
      (setq timeout (1+ timeout))))

  ;; If we got here without completion, it's a timeout
  (unless integration-test-completed
    (message "TIMEOUT: No complete response received!")
    (message "Last phase: %s" integration-test-phase)
    (kill-emacs 1)))

(defun test-initial-content-view ()
  "Request initial content view for node 1."
  (message "=== PHASE 1: Requesting content view for node '1' ===")
  (skg-request-single-root-content-view-from-node "1")

  ;; Wait for the response
  (sleep-for 0.25)

  (let ((content-buffer (get-buffer "*skg-content-view*")))
    (if content-buffer
        (with-current-buffer content-buffer
          (let ((content (buffer-substring-no-properties (point-min) (point-max))))
            (message "Content view received")
            (message "Content: %s" content)

            ;; The content should show node 1 with its children 11, 12->121, 13
            (if (string-match-p "\\* (skg (id 1)" content)
                (progn
                  (message "✓ PASS: Found node 1 in content view")
                  (setq integration-test-phase "initial-view-complete")
                  (test-add-containerward-request))
              (progn
                (message "✗ FAIL: Node 1 not found in content view")
                (kill-emacs 1)))))
      (progn
        (message "✗ FAIL: No *skg-content-view* buffer was created")
        (kill-emacs 1)))))

(defun test-add-containerward-request ()
  "Add containerward-view request to node 12 and save."
  (message "=== PHASE 2: Adding containerward-view request to node 12 ===")

  (with-current-buffer "*skg-content-view*"
    ;; Find the line with "** (skg (id 12)" and position on it
    (goto-char (point-min))
    (if (re-search-forward "^\\*\\* (skg (id 12)" nil t)
        (progn
          (beginning-of-line)
          (message "✓ Positioned on node 12")

          ;; Insert containerward path request and save
          (message "Calling skg-request-containerward-view2...")
          (skg-request-containerward-view2)

          ;; Wait for response
          (sleep-for 0.25)

          (setq integration-test-phase "save-with-request-complete")
          (test-verify-containerward-path))
      (progn
        (message "✗ FAIL: Could not find node 12 in buffer")
        (kill-emacs 1)))))

(defun test-verify-containerward-path ()
  "Verify that the containerward path was integrated."
  (message "=== PHASE 3: Verifying containerward path integration ===")

  (with-current-buffer "*skg-content-view*"
    (let ((content (buffer-substring-no-properties (point-min) (point-max))))
      (message "Updated buffer content: %s" content)

      ;; The containerward path should show:
      ;; ** 12
      ;; *** 1 (with parentIgnores and indefinitive)
      ;; **** 0 (with parentIgnores and indefinitive)
      ;; *** 121 (the original child of 12)

      ;; Check for node 1 under node 12
      (if (string-match-p "\\*\\*\\* (skg (id 1).*indefinitive.*1\n\\*\\*\\*\\* (skg (id 0)" content)
          (progn
            (message "✓ PASS: Found containerward path (1->0) under node 12")

            ;; Verify the request was stripped
            (if (not (string-match-p "requests containerwardView" content))
                (progn
                  (message "✓ PASS: containerward-view request was stripped from metadata")
                  (message "✓ PASS: Integration test successful!")
                  (setq integration-test-completed t)
                  (kill-emacs 0))
              (progn
                (message "✗ FAIL: containerward-view request was not stripped")
                (kill-emacs 1))))
        (progn
          (message "✗ FAIL: Containerward path not found in expected location")
          (message "Expected to find node 1 (indefinitive) and node 0 under node 12")
          (kill-emacs 1))))))

(progn ;; Run the test with a timeout in case things hang.
  (run-at-time
   30 nil
   (lambda ()
     (message "TIMEOUT: Integration test timed out!")
     (message "Last phase: %s" integration-test-phase)
     (kill-emacs 1)))
  ( ;; the test
   run-all-tests))
