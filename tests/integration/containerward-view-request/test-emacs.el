;;; Integration test for containerward-view request functionality
;;; This script tests:
;;; 1. Creating initial buffer structure and saving to establish relationships
;;; 2. Creating new buffer without root node
;;; 3. Adding containerward-view request to node 12
;;; 4. Verifying containerward path [1, 0] is integrated under node 12
;;;
;;; NOTE: File system operations (backup/cleanup) are handled by run-test.sh

;; Load the project elisp configuration
(load-file "../../../elisp/skg-init.el")

;; Test result tracking
(defvar integration-test-phase "starting")
(defvar integration-test-completed nil)

(defun strip-all-metadata (text)
  "Remove all (skg ...) metadata from TEXT, leaving only stars and titles."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (let ((result ""))
      ;; Process each line
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position)
                     (line-end-position))))
          ;; Extract stars and title from each line
          (when (string-match "^\\(\\*+\\) .*?\\([^ ]+\\)$" line)
            (let ((stars (match-string 1 line))
                  (title (match-string 2 line)))
              (setq result (concat result stars " " title "\n")))))
        (forward-line 1))
      result)))

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

  ;; Phase 1: Create initial buffer and save to establish relationships
  (test-establish-relationships)

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

(defun test-establish-relationships ()
  "Create initial buffer with full structure and save to establish relationships."
  (message "=== PHASE 1: Establishing relationships on disk ===")

  ;; Create buffer with structure including node 0
  (let ((buffer (get-buffer-create "*skg-content-view*")))
    (with-current-buffer buffer
      (erase-buffer)
      (org-mode)
      (insert "* (skg (id 0)) 0\n")
      (insert "** (skg (id 1)) 1\n")
      (insert "*** (skg (id 11)) 11\n")
      (insert "*** (skg (id 12)) 12\n")
      (insert "**** (skg (id 121)) 121\n")
      (insert "*** (skg (id 13)) 13\n")
      (message "✓ Created initial buffer with full structure")

      ;; Save to establish relationships
      (message "Saving initial buffer to establish relationships...")
      (skg-request-save-buffer)

      ;; Wait for save to complete
      (sleep-for 0.5)

      (message "✓ Relationships established on disk")
      (setq integration-test-phase "relationships-established")

      ;; Now create new buffer without node 0
      (test-create-new-buffer))))

(defun test-create-new-buffer ()
  "Obliterate buffer and create new one without node 0."
  (message "=== PHASE 2: Creating new buffer without node 0 ===")

  (let ((buffer (get-buffer "*skg-content-view*")))
    (when buffer
      (with-current-buffer buffer
        (erase-buffer)
        (insert "* (skg (id 1)) 1\n")
        (insert "** (skg (id 11)) 11\n")
        (insert "** (skg (id 12)) 12\n")
        (insert "*** (skg (id 121)) 121\n")
        (insert "** (skg (id 13)) 13\n")
        (message "✓ Created new buffer without node 0")

        ;; Position on line 3 (** (skg (id 12)) 12)
        (goto-char (point-min))
        (forward-line 2)
        (message "✓ Positioned on node 12 (line 3)")

        (setq integration-test-phase "new-buffer-created")

        ;; Call containerward view request
        (test-request-containerward-view)))))

(defun test-request-containerward-view ()
  "Call skg-request-containerward-view2 on node 12."
  (message "=== PHASE 3: Requesting containerward view for node 12 ===")

  (with-current-buffer "*skg-content-view*"
    (message "Calling skg-request-containerward-view2...")
    (skg-request-containerward-view2)

    ;; Wait for response
    (sleep-for 0.5)

    (setq integration-test-phase "containerward-request-complete")
    (test-verify-result)))

(defun test-verify-result ()
  "Strip metadata and verify result matches expected structure."
  (message "=== PHASE 4: Verifying result structure ===")

  (with-current-buffer "*skg-content-view*"
    (let* ((buffer-content (buffer-substring-no-properties
                     (point-min) (point-max)))
           (stripped-buffer-content
            (strip-all-metadata buffer-content))
           (expected-without-metadata
            ;; It's easier to read this way, without metadata.
            (concat "* 1\n"
                    "** 11\n"
                    "** 12\n" ;; backpath was requested here
                    "*** 1\n" ;; its first element
                    "**** 0\n" ;; its second element
                    "*** 121\n"
                    "** 13\n"))
           (expected
            (concat "* (skg (id 1) (view (rels (contents 3)))) 1\n"
                    "** (skg (id 11)) 11\n"
                    "** (skg (id 12) (view focused (rels (contents 1)))) 12\n"
                    "*** (skg (id 1) (view (rels notInParent containsParent (contents 3))) (code (relToParent parentIgnores) indefinitive)) 1\n"
                    "**** (skg (id 0) (view (rels notInParent containsParent (containers 0) (contents 1))) (code (relToParent parentIgnores) indefinitive)) 0\n"
                    "*** (skg (id 121)) 121\n"
                    "** (skg (id 13)) 13\n")))

      (message "Buffer-Content with metadata: %s" buffer-content)
      (message "Expected buffer-content with metadata: %s" expected)
      (message "Stripped buffer-content: %s" stripped-buffer-content)
      (message "Expected buffer-content without metadata: %s"
               expected-without-metadata)

      (if (string= buffer-content expected)
          (progn
            (message "✓ PASS: Buffer-Content matches expected (with metadata)")
            (if (string= stripped-buffer-content expected-without-metadata)
                (progn
                  (message "✓ PASS: Stripped buffer-content matches expected structure")
                  (message "✓ PASS: Containerward path [1, 0] was correctly integrated under node 12")
                  (message "✓ PASS: Integration test successful!")
                  (setq integration-test-completed t)
                  (kill-emacs 0))
              (progn
                (message "✗ FAIL: Stripped buffer-content does not match expected without metadata")
                (kill-emacs 1))))
        (progn
          (message "✗ FAIL: Buffer-Content does not match expected (with metadata)")
          (message "Expected containerward path [1, 0] under node 12")
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
