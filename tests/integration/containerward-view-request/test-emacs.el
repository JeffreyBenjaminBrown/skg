;;; Integration test for the containers folder request
;;; This script tests:
;;; 1. Creating initial buffer structure and saving to establish relationships
;;; 2. Creating new buffer without root node
;;; 3. Requesting node 12's containers folder
;;; 4. Verifying the folder holds 1, with the containerward chain [0] under it
;;;
;;; NOTE: File system operations (backup/cleanup) are handled by run-test.sh

;; Load the project elisp configuration
(load-file "../../../elisp/skg-init.el")
(load-file "../test-wait.el")

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
          ;; A headline without a title (a folder) is skipped.
          (when (string-match "^\\(\\*+\\) .*) \\([^ )]+\\)$" line)
            (let ((stars (match-string 1 line))
                  (title (match-string 2 line)))
              (setq result (concat result stars " " title "\n")))))
        (forward-line 1))
      result)))

(defun test-establish-relationships ()
  "Create initial buffer with full structure and save to establish relationships."
  (message "=== PHASE 1: Establishing relationships on disk ===")

  ;; Create buffer with structure including node 0
  (let ((buffer (get-buffer-create "*skg-content-view*")))
    (with-current-buffer buffer
      (erase-buffer)
      (org-mode)
      (setq skg-view-id (org-id-uuid))
      (insert "* (skg node (gnid 0) (skgRepos (nodeRepo main))) 0\n")
      (insert "** (skg node (gnid 1)) 1\n")
      (insert "*** (skg node (gnid 11)) 11\n")
      (insert "*** (skg node (gnid 12)) 12\n")
      (insert "**** (skg node (gnid 121)) 121\n")
      (insert "*** (skg node (gnid 13)) 13\n")
      (message "✓ Created initial buffer with full structure")

      ;; Save to establish relationships
      (message "Saving initial buffer to establish relationships...")
      (skg-request-save-buffer)

      ;; Wait for save to complete
      (skg-test-wait-for-response)

      (message "✓ Relationships established on disk")
      (setq integration-test-phase "relationships-established"))))

(defun test-create-new-buffer ()
  "Obliterate buffer and create new one without node 0."
  (message "=== PHASE 2: Creating new buffer without node 0 ===")

  (let ((buffer (get-buffer "*skg-content-view*")))
    (when buffer
      (with-current-buffer buffer
        (erase-buffer)
        (insert "* (skg node (gnid 1) (skgRepos (nodeRepo main))) 1\n")
        (insert "** (skg node (gnid 11)) 11\n")
        (insert "** (skg node (gnid 12)) 12\n")
        (insert "*** (skg node (gnid 121)) 121\n")
        (insert "** (skg node (gnid 13)) 13\n")
        (message "✓ Created new buffer without node 0")

        ;; Position on line 3 (** (skg (gnid 12)) 12)
        (goto-char (point-min))
        (forward-line 2)
        (message "✓ Positioned on node 12 (line 3)")

        (setq integration-test-phase "new-buffer-created")))))

(defun test-request-containerward-view ()
  "Call skg-show-folderOf-containers on node 12."
  (message "=== PHASE 3: Requesting the containers folder of node 12 ===")

  (with-current-buffer "*skg-content-view*"
    (message "Calling skg-show-folderOf-containers...")
    (skg-show-folderOf-containers) ;; this also saves the buffer

    ;; Wait for response
    (skg-test-wait-for-response)

    (setq integration-test-phase "containerward-request-complete")))

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
                    "** 12\n" ;; the folder was requested here
                    "**** 1\n" ;; in its containers folder
                    "***** 0\n" ;; the chain under 1
                    "*** 121\n"
                    "** 13\n"))
           (expected
            (concat "* (skg node (gnid 1) (skgRepos (nodeRepo main)) (view (rels (contains (in 1) (out 3))))) 1\n"
                    "** (skg node (gnid 11) (skgRepos (nodeRepo main)) (view (rels (contains (in 1 (ancestors 1)))) (birth (contains in 1)))) 11\n"
                    "** (skg node (gnid 12) (skgRepos (nodeRepo main)) (view (rels (contains (in 1 (ancestors 1)) (out 1))) (birth (contains in 1)))) 12\n"
                    "*** (skg containerFolder)\n"
                    "**** (skg node (gnid 1) (skgRepos (nodeRepo main)) writeProtected (view (rels (contains (in 1) (out 3 (ancestors 2)))) (birth (contains out 2)) cycle)) 1\n"
                    "***** (skg node (gnid 0) (skgRepos (nodeRepo main)) noClaim writeProtected (view (rels (contains (out 1 (ancestors 1)))))) 0\n"
                    "*** (skg node (gnid 121) (skgRepos (nodeRepo main)) (view (rels (contains (in 1 (ancestors 1)))) (birth (contains in 1)))) 121\n"
                    "** (skg node (gnid 13) (skgRepos (nodeRepo main)) (view (rels (contains (in 1 (ancestors 1)))) (birth (contains in 1)))) 13\n")))

      (message "Buffer-Content with metadata: %s" buffer-content)
      (message "Expected buffer-content with metadata: %s" expected)
      (message "Stripped buffer-content: %s" stripped-buffer-content)
      (message "Expected buffer-content without metadata: %s"
               expected-without-metadata)

      (if (string= (skg-test-without-viewnode-ids buffer-content) expected)
          (progn
            (message "✓ PASS: Buffer-Content matches expected (with metadata)")
            (if (string= stripped-buffer-content expected-without-metadata)
                (progn
                  (message "✓ PASS: Stripped buffer-content matches expected structure")
                  (message "✓ PASS: The containers folder of node 12 holds 1, with the chain [0] under it"))
              (progn
                (message "✗ FAIL: Stripped buffer-content does not match expected without metadata")
                (kill-emacs 1))))
        (progn
          (message "✗ FAIL: Buffer-Content does not match expected (with metadata)")
          (message "Expected the containers folder of node 12 to hold 1, with the chain [0]")
          (kill-emacs 1))))))

(defun run-all-tests ()
  "Main orchestrator function that runs all integration tests."
  (message "=== SKG Containerward View Request Integration Test ===")

  ;; Set the port from environment variable if available
  (let ((test-port (getenv "SKG_TEST_PORT")))
    (when test-port
      (setq skg-port (string-to-number test-port))
      (message "Using test port: %d" skg-port)))

  (test-establish-relationships)
  (test-create-new-buffer)
  (test-request-containerward-view)
  (test-verify-result)

  (message "✓ PASS: Integration test successful!")
  (setq integration-test-completed t)
  (kill-emacs 0))

(progn ;; Run the test with a timeout in case things hang.
  (run-at-time
   30 nil
   (lambda ()
     (message "TIMEOUT: Integration test timed out!")
     (message "Last phase: %s" integration-test-phase)
     (kill-emacs 1)))
  ( ;; the test
   run-all-tests))
