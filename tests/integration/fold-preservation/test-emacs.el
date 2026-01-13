;;; Integration test for fold preservation functionality
;;; This script tests:
;;; 1. Creating a buffer with content
;;; 2. Folding some headlines
;;; 3. Saving the buffer
;;; 4. Verifying folding is preserved and point is at the right location

;; Load the project elisp configuration
(load-file "../../../elisp/skg-init.el")

;; Test result tracking
(defvar integration-test-phase "starting")
(defvar integration-test-completed nil)

(defun run-all-tests ()
  "Main orchestrator function that runs all integration tests."
  (message "=== SKG Fold Preservation Integration Test ===")

  ;; Set the port from environment variable if available
  (let ((test-port (getenv "SKG_TEST_PORT")))
    (when test-port
      (setq skg-port (string-to-number test-port))
      (message "Using test port: %d" skg-port)))

  ;; Wait a moment for server to be fully ready
  (sleep-for 0.25)

  ;; Run the test
  (test-fold-preservation)

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

(defun test-fold-preservation ()
  "Test that folding is preserved after save."
  (message "=== PHASE 1: Creating buffer with content ===")

  ;; Create buffer with content
  (let ((buffer (get-buffer-create "*skg-content-view*")))
    (with-current-buffer buffer
      (erase-buffer)
      (org-mode)
      (insert (concat "* (skg (node (source main))) a\n"
                      "* (skg (node (source main))) b\n"
                      "** c\n"
                      "** d\n"
                      "*** d1\n"
                      "*** d2\n"
                      "** e\n"))
      (message "✓ Created buffer with content")

      ;; Position on line 4 (** d)
      (goto-char (point-min))
      (forward-line 3)
      (message "✓ Positioned on line 4")

      ;; Verify we're on the right line
      (let ((current-line (buffer-substring-no-properties
                           (line-beginning-position)
                           (line-end-position))))
        (if (string= current-line "** d")
            (message "✓ Confirmed on line with '** d'")
          (progn
            (message "✗ FAIL: Expected to be on '** d', but on: %s" current-line)
            (kill-emacs 1))))

      ;; Fold the headline (this should hide d1 and d2)
      (org-cycle)
      (message "✓ Folded headline d")

      ;; Verify that d1 is now invisible
      (save-excursion
        (forward-line 1)
        (if (invisible-p (point))
            (message "✓ Confirmed that d1 is invisible after folding")
          (progn
            (message "✗ FAIL: d1 should be invisible but is visible")
            (kill-emacs 1))))

      (setq integration-test-phase "buffer-created-and-folded")

      ;; Save the buffer
      (message "=== PHASE 2: Saving buffer ===")
      (message "Calling skg-request-save-buffer...")
      (skg-request-save-buffer)

      ;; Wait for response
      (sleep-for 0.5)

      (setq integration-test-phase "buffer-saved")

      ;; Verify point is still on line 4
      (message "=== PHASE 3: Verifying point position ===")
      (let ((current-line-num (line-number-at-pos)))
        (if (= current-line-num 4)
            (message "✓ PASS: Point is on line 4 after save")
          (progn
            (message "✗ FAIL: Point should be on line 4 but is on line %d" current-line-num)
            (kill-emacs 1))))

      ;; Verify we're still on ** d (may have skg metadata added by server)
      (let ((current-line (buffer-substring-no-properties
                           (line-beginning-position)
                           (line-end-position))))
        (if (string-match-p "\\*\\* .* d" current-line)
            (message "✓ PASS: Still on line with '** d'")
          (progn
            (message "✗ FAIL: Expected to be on line with '** d', but on: %s" current-line)
            (kill-emacs 1))))

      ;; Move to next line
      (message "=== PHASE 4: Testing next-line behavior ===")
      (next-line)

      ;; Verify point is now on line 7 (** e)
      (let ((current-line-num (line-number-at-pos)))
        (if (= current-line-num 7)
            (message "✓ PASS: Point is on line 7 after next-line")
          (progn
            (message "✗ FAIL: Point should be on line 7 but is on line %d" current-line-num)
            (message "Current line content: %s"
                     (buffer-substring-no-properties
                      (line-beginning-position)
                      (line-end-position)))
            (kill-emacs 1))))

      ;; Verify we're on ** e (may have skg metadata added by server)
      (let ((current-line (buffer-substring-no-properties
                           (line-beginning-position)
                           (line-end-position))))
        (if (string-match-p "\\*\\* .* e" current-line)
            (message "✓ PASS: Now on line with '** e'")
          (progn
            (message "✗ FAIL: Expected to be on line with '** e', but on: %s" current-line)
            (kill-emacs 1))))

      (message "✓ PASS: Integration test successful!")
      (setq integration-test-completed t)
      (kill-emacs 0))))

(progn ;; Run the test with a timeout in case things hang.
  (run-at-time
   30 nil
   (lambda ()
     (message "TIMEOUT: Integration test timed out!")
     (message "Last phase: %s" integration-test-phase)
     (kill-emacs 1)))
  ( ;; the test
   run-all-tests))
