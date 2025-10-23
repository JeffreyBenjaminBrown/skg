;;; Integration test for focus and folded markers functionality
;;; This script tests, in order:
;;; - Creating a buffer with 6 headlines
;;; - Folding headline 3 (which contains headlines 4 and 5)
;;; - Positioning point on headline 2
;;; - Saving with skg-request-save-buffer
;;; - Verifying that after save, the markers are removed but the folding is preserved
;;;   and point is on the focused headline
;;;
;;; NOTE: File system operations (backup/cleanup) are handled by run-test.sh

;; Load the project elisp configuration
(load-file "../../../elisp/skg-init.el")

;; Test result tracking
(defvar integration-test-phase "starting")
(defvar integration-test-completed nil)

(defun run-all-tests ()
  "Main orchestrator function that runs all integration tests."
  (message "=== SKG Focus and Folded Markers Integration Test ===")

  ;; Set the port from environment variable if available
  (let ((test-port (getenv "SKG_TEST_PORT")))
    (when test-port
      (setq skg-port (string-to-number test-port))
      (message "Using test port: %d" skg-port)))

  ;; Wait a moment for server to be fully ready
  (sleep-for 0.25)

  ;; Run the test
  (test-focus-and-folded-markers)

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

(defun test-focus-and-folded-markers ()
  "Test focus and folded markers during save."
  (message "=== PHASE 1: Creating test buffer ===")

  ;; Create a new buffer with the test content
  (let ((test-buffer (get-buffer-create "*skg-content-view*")))
    (with-current-buffer test-buffer
      (erase-buffer)
      (org-mode)

      ;; Insert the 6 headlines
      (insert "* (skg (id 1)) 1\n")
      (insert "** (skg (id 2)) 2\n")
      (insert "** (skg (id 3)) 3\n")
      (insert "*** (skg (id 4)) 4\n")
      (insert "*** (skg (id 5)) 5\n")
      (insert "** (skg (id 6)) 6\n")

      (message "✓ Created buffer with 6 headlines")

      ;; Save the initial state
      (let ((initial-content (buffer-substring-no-properties (point-min) (point-max))))
        (with-temp-file "initial.log"
          (insert initial-content))
        (message "✓ Saved initial buffer state to initial.log"))

      ;; Position point on headline 3 (third headline)
      (goto-char (point-min))
      (re-search-forward "^\\*\\* (skg (id 3))")
      (beginning-of-line)
      (message "✓ Positioned point on headline 3")

      ;; Fold headline 3 using org-cycle
      (org-cycle)
      (message "✓ Folded headline 3")

      ;; Move point up to headline 2
      (forward-line -1)
      (message "✓ Moved point to headline 2")

      ;; Verify we're on the right line
      (let ((current-line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
        (message "Current line: %s" current-line)
        (unless (string-match-p "(id 2)" current-line)
          (message "✗ FAIL: Not on headline 2")
          (kill-emacs 1)))

      ;; Debug: Check invisibility of each headline
      (message "=== Checking invisibility status ===")
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (when (org-at-heading-p)
            (let ((headline (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
                  (is-invisible (invisible-p (point))))
              (message "Line: %s | Invisible: %s" headline is-invisible)))
          (forward-line 1)))

      (setq integration-test-phase "ready-to-save")

      ;; Debug: Manually test skg-add-folded-markers
      (message "=== Testing skg-add-folded-markers ===")
      (skg-add-folded-markers)
      (let ((after-folded-markers (buffer-substring-no-properties (point-min) (point-max))))
        (with-temp-file "after-folded-markers.log"
          (insert after-folded-markers))
        (message "Buffer after skg-add-folded-markers:\n%s" after-folded-markers))

      ;; Save the buffer
      (message "=== PHASE 2: Saving buffer ===")
      (skg-request-save-buffer)

      ;; Wait for save response
      (sleep-for 0.5)

      ;; Check the result
      (setq integration-test-phase "checking-result")
      (let ((result-content (buffer-substring-no-properties (point-min) (point-max))))
        ;; Save the result for debugging
        (with-temp-file "result.log"
          (insert result-content))
        (message "✓ Saved result buffer state to result.log")
        (message "Result content:\n%s" result-content)

        ;; Verify the markers are REMOVED (not present in buffer)
        (let ((has-focused-marker (string-match-p "\\<focused\\>" result-content))
              (has-folded-marker (string-match-p "\\<folded\\>" result-content))
              (current-line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
              (line-4-invisible (save-excursion
                                  (goto-char (point-min))
                                  (forward-line 3)
                                  (invisible-p (point))))
              (line-5-invisible (save-excursion
                                  (goto-char (point-min))
                                  (forward-line 4)
                                  (invisible-p (point)))))

          (if (and (not has-focused-marker)
                   (not has-folded-marker)
                   (string-match-p "(id 2)" current-line)
                   line-4-invisible
                   line-5-invisible)
              (progn
                (message "✓ PASS: Focused marker removed from buffer")
                (message "✓ PASS: Folded markers removed from buffer")
                (message "✓ PASS: Point is on headline 2 (focused headline)")
                (message "✓ PASS: Headline 4 is invisible (folded)")
                (message "✓ PASS: Headline 5 is invisible (folded)")
                (message "✓ PASS: Integration test successful!")
                (setq integration-test-completed t)
                (kill-emacs 0))
            (progn
              (message "✗ FAIL: Expected behavior not found")
              (message "  has-focused-marker (should be nil): %s" has-focused-marker)
              (message "  has-folded-marker (should be nil): %s" has-folded-marker)
              (message "  current-line: %s" current-line)
              (message "  line-4-invisible (should be non-nil): %s" line-4-invisible)
              (message "  line-5-invisible (should be non-nil): %s" line-5-invisible)
              (kill-emacs 1))))))))

(progn ;; Run the test with a timeout in case things hang.
  (run-at-time
   20 nil
   (lambda ()
     (message "TIMEOUT: Integration test timed out!")
     (message "Last phase: %s" integration-test-phase)
     (kill-emacs 1)))
  ( ;; the test
   run-all-tests))
