;;; Integration test for skg content view and save functionality
;;; This script tests:
;;; 1. skg-request-single-root-content-view-from-id "1"
;;; 2. Adding content and skg-request-save-buffer
;;; 3. Verifying UUID generation and content persistence
;;;
;;; NOTE: File system operations (backup/cleanup) are handled by run-test.sh


;; Load the project elisp configuration
(load-file "../../../elisp/skg-init.el")

;; Test result tracking
(defvar integration-test-phase "starting")
(defvar integration-test-completed nil)
(defvar integration-test-new-uuid nil) ;; Track created UUID for verification

(defun run-all-tests ()
  "Main orchestrator function that runs all integration tests."
  (message "=== SKG Content View and Save Integration Test ===")

  ;; Set the port from environment variable if available
  (let ((test-port (getenv "SKG_TEST_PORT")))
    (when test-port
      (setq skg-port (string-to-number test-port))
      (message "Using test port: %d" skg-port)))

  ;; Wait a moment for server to be fully ready
  (sleep-for 0.25)

  ;; Phase 1: Test content view creation
  (test-content-view)

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

(defun test-content-view ()
  "Test content view creation and proceed to save test if successful."
  (message "=== PHASE 1: Requesting content view for node '1' ===")
  (skg-request-single-root-content-view-from-id "1")
  (message "Called skg-request-single-root-content-view-from-id")

  ;; Wait for the response
  (sleep-for 0.25)

  ;; Check if the content view buffer was created with expected content
  (let ((content-buffer ;; The buffer name is based on the extracted title, so it will be "*skg: 1*"
         (get-buffer "*skg: 1*")))
    (if content-buffer
        (with-current-buffer content-buffer
          (let ((content (buffer-substring-no-properties (point-min) (point-max))))
            (message "Content view received")
            (message "Content: %s" content)

            ;; Verify exact content: should include source metadata
            (let ((expected-content "* (skg (node (id 1) (source main) (graphStats (containers 0)))) 1\n"))
              (if (string= content expected-content)
                  (progn
                    (message "✓ PASS: Buffer content exactly matches expected")
                    (setq integration-test-phase "content-view-complete")
                    ;; Proceed to save test
                    (test-save-content))
                (progn
                  (message "✗ FAIL: Buffer content does not exactly match expected")
                  (message "Expected exactly: %S" expected-content)
                  (message "Got: %S" content)
                  (message "Expected length: %d, Got length: %d"
                           (length expected-content) (length content))
                  (kill-emacs 1))))))
      (progn
        (message "✗ FAIL: No *skg-content-view* buffer was created")
        (kill-emacs 1)))))

(defun test-save-content ()
  "Test saving modified buffer content."
  (message "=== PHASE 2: Testing save buffer ===")

  ;; Get the content view buffer
  (let ((content-buffer (get-buffer "*skg: 1*")))
    (if content-buffer
        (with-current-buffer content-buffer
          ;; Save buffer state after fetch but before editing
          (let ((fetch-content (buffer-substring-no-properties (point-min) (point-max))))
            (with-temp-file "fetch.log"
              (insert fetch-content))
            (message "✓ Saved buffer state to fetch.log"))

          (goto-char (point-max))
          ;; Add a new line
          (insert "\n** 2")
          (message "✓ Added new content line: ** 2")

          ;; Save buffer state after editing but before saving
          (let ((edited-content (buffer-substring-no-properties (point-min) (point-max))))
            (with-temp-file "edited-unsaved.log"
              (insert edited-content))
            (message "✓ Saved edited buffer state to edited-unsaved.log"))

          ;; Save the buffer
          (message "Calling skg-request-save-buffer...")
          (skg-request-save-buffer)

          ;; Wait for save response
          (sleep-for 0.5)

          ;; Check the updated content
          (let ((updated-content (buffer-substring-no-properties (point-min) (point-max))))
            ;; Save buffer state after saving and rebuilding
            (with-temp-file "saved-rebuilt.log"
              (insert updated-content))
            (message "✓ Saved rebuilt buffer state to saved-rebuilt.log")
            (message "Updated buffer content: %s" updated-content)

            ;; Verify the structure: should have original line and new line with UUID
            (if (and (string-match-p "\\* (skg (node (id 1) (source main)" updated-content)
                     (string-match "\\*\\* (skg (node (id \\([^)]+\\)) (source main).*) 2" updated-content))
                (progn
                  ;; Extract the new UUID for verification
                  (when (string-match "\\*\\* (skg (node (id \\([^)]+\\)).*) 2" updated-content)
                    (setq integration-test-new-uuid (match-string 1 updated-content))
                    (message "✓ Extracted new UUID: %s" integration-test-new-uuid))

                  (message "✓ PASS: Buffer shows correct structure after save")
                  (message "✓ PASS: Found new UUID in saved content")
                  (message "✓ PASS: Integration test successful!")
                  (setq integration-test-completed t)
                  (kill-emacs 0))
              (progn
                (message "✗ FAIL: Expected content structure not found after save")
                (message "Expected: * (skg (node (id 1) (source main) ...)) 1 and ** (skg (node (id UUID) (source main) ...)) 2")
                (message "Got: %s" updated-content)
                (kill-emacs 1)))))
      (progn
        (message "✗ FAIL: Content view buffer not found for save test")
        (kill-emacs 1)))))

(progn ;; Run the test with a timeout in case things hang.
  (run-at-time
   20 nil
   (lambda ()
     (message "TIMEOUT: Integration test timed out!")
     (message "Last phase: %s" integration-test-phase)
     (kill-emacs 1)))
  ( ;; the test
   run-all-tests))
