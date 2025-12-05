;;; Integration test for skg save error handling
;;; This script tests:
;;; 1. Invalid save (duplicate ID without indefinitive) should show error buffer
;;; 2. Valid save (with indefinitive) should work normally
;;;
;;; NOTE: File system operations (backup/cleanup) are handled by run-test.sh

;; Load the project elisp configuration
(load-file "../../../elisp/skg-init.el")

;; Test result tracking
(defvar integration-test-phase "starting")
(defvar integration-test-completed nil)

(defun run-all-tests ()
  "Main orchestrator function that runs all integration tests."
  (message "=== SKG Valid and Invalid Saves Integration Test ===")

  ;; Set the port from environment variable if available
  (let ((test-port (getenv "SKG_TEST_PORT")))
    (when test-port
      (setq skg-port (string-to-number test-port))
      (message "Using test port: %d" skg-port)))

  ;; Wait a moment for server to be fully ready
  (sleep-for 0.25)

  ;; Phase 1: Test invalid save (should create error buffer)
  (test-invalid-save)

  ;; Wait for completion with timeout
  (let ((timeout 0))
    (while (and (not integration-test-completed) (< timeout 200))
      (sleep-for 0.25)
      (setq timeout (1+ timeout))))

  ;; If we got here without completion, it's a timeout
  (unless integration-test-completed
    (message "TIMEOUT: No complete response received!")
    (message "Last phase: %s" integration-test-phase)
    (kill-emacs 1)))

(defun test-invalid-save ()
  "Test invalid save that should create error buffer."
  (message "=== PHASE 1: Testing invalid save (duplicate ID without indefinitive) ===")

  ;; Create the *skg-content-view* buffer with problematic content
  (with-current-buffer (get-buffer-create "*skg-content-view*")
    (erase-buffer)
    (insert "* (skg (id 1) (source main)) 1\n** (skg (id 1)) 1")
    (org-mode)
    (goto-char (point-min))
    (message "✓ Created *skg-content-view* buffer with invalid content"))

  ;; Switch to the buffer to make it current
  (switch-to-buffer "*skg-content-view*")

  ;; Attempt to save - this should fail and create error buffer
  (message "Calling skg-request-save-buffer with invalid content...")
  (skg-request-save-buffer)

  ;; Wait for response
  (sleep-for 0.5)

  ;; Check if error buffer was created
  (let ((error-buffer (get-buffer "*SKG Save Errors - Inconsistencies Found*"))
        (content-buffer (get-buffer "*skg-content-view*")))

    (if error-buffer
        (progn
          (message "✓ PASS: Error buffer was created")
          (with-current-buffer error-buffer
            (let ((error-content (buffer-substring-no-properties (point-min) (point-max))))
              (message "Error buffer content: %s" error-content)
              (if (string-match-p "NOTHING WAS SAVED" error-content)
                  (message "✓ PASS: Error buffer contains expected error message")
                (progn
                  (message "✗ FAIL: Error buffer does not contain expected content")
                  (kill-emacs 1)))))

          ;; Check that buffer has focused marker added (from point position)
          (if content-buffer
              (with-current-buffer content-buffer
                (let ((content (buffer-substring-no-properties (point-min) (point-max))))
                  (if (string= content "* (skg (id 1) (source main) (view focused)) 1\n** (skg (id 1)) 1")
                      (progn
                        (message "✓ PASS: Buffer has focused marker as expected")
                        (setq integration-test-phase "invalid-save-complete")
                        ;; Proceed to valid save test
                        (test-valid-save))
                    (progn
                      (message "✗ FAIL: Buffer content does not match expected")
                      (message "Expected: %S" "* (skg (id 1) (source main) (view focused)) 1\n** (skg (id 1)) 1")
                      (message "Got: %S" content)
                      (kill-emacs 1)))))
            (progn
              (message "✗ FAIL: Original *skg-content-view* buffer was lost")
              (kill-emacs 1))))
      (progn
        (message "✗ FAIL: No error buffer was created")
        (message "Available buffers:")
        (dolist (buf (buffer-list))
          (message "  - %s" (buffer-name buf)))
        (kill-emacs 1)))))

(defun test-valid-save ()
  "Test valid save after fixing the content."
  (message "=== PHASE 2: Testing valid save (with indefinitive) ===")

  ;; Switch back to content view buffer and fix the content
  (with-current-buffer "*skg-content-view*"
    (goto-char (point-min))
    (search-forward "** (skg (id 1)) 1")
    (replace-match "** (skg (id 1) (code indefinitive)) 1")
    (message "✓ Amended previously invalid content to use indefinitive, so it is now valid"))

  ;; Switch to buffer to make it current
  (switch-to-buffer "*skg-content-view*")

  ;; Save the corrected content
  (message "Calling skg-request-save-buffer with valid content...")
  (skg-request-save-buffer)

  ;; Wait for response
  (sleep-for 0.5)

  ;; Check the updated content
  (with-current-buffer "*skg-content-view*"
    (let ((updated-content (buffer-substring-no-properties (point-min) (point-max))))
      (message "Updated buffer content: %s" updated-content)

      ;; Should contain cycle and indefinitive markers
      ;; Note: cycle appears within (view ...), indefinitive within (code ...)
      (if (and (string-match-p "(view.*cycle" updated-content)
               (string-match-p "(code.*indefinitive" updated-content))
          (progn
            (message "✓ PASS: Valid save worked and showed cycle indefinitive")
            (message "✓ PASS: Integration test successful!")
            (setq integration-test-completed t)
            (kill-emacs 0))
        (progn
          (message "✗ FAIL: Expected cycle and indefinitive markers not found")
          (message "Expected to contain: '(view cycle)' and '(code indefinitive)'")
          (message "Got: %s" updated-content)
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
