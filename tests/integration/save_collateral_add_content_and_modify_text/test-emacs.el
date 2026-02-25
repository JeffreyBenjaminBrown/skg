;;; Integration test for collateral buffer updates: title edit + add child
;;;
;;; Opens two buffers over a containment cycle (a contains b, b contains a).
;;; From buffer B, edits a's title and adds a new child "c" under a (no
;;; metadata — the server reads it as a TrueNode with a random UUID).
;;; Verifies that buffer B's save response reflects the changes (title,
;;; new node with UUID). Then checks collateral buffer A.
;;;
;;; Known limitation: collateral completion currently fails when the saved
;;; buffer introduced a brand-new node, because build_child_creation_data
;;; can't find a source for the new UUID. So buffer A is NOT updated.

;; Load the project elisp configuration
(load-file "../../../elisp/skg-init.el")
(load-file "../save_collateral_break_cycle/test-helpers.el")

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

(defun phase-3-edit-and-save ()
  "Edit a's title, add child c under a, and save buffer B."
  (message "=== PHASE 3: Edit a's title, add child c, save ===")
  (setq integration-test-phase "phase-3-edit-and-save")
  (let ((buf (get-buffer "*skg: b*")))
    ;; Verify pre-edit structure
    (assert-headline-titles
     buf
     '((1 . "b") (2 . "a") (3 . "b"))
     "phase 3: buffer B before edit")
    (with-current-buffer buf
      ;; Change a's title on line 2: replace trailing "a" with new title
      (goto-char (point-min))
      (forward-line 1)
      (end-of-line)
      (delete-char -1)
      (insert "Node a was given this longer title")
      ;; Add a new child of a with no metadata
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert "*** c"))
    ;; Verify post-edit structure
    (assert-headline-titles
     buf
     '((1 . "b")
       (2 . "Node a was given this longer title")
       (3 . "b")
       (3 . "c"))
     "phase 3: buffer B after edit")
    (with-current-buffer buf
      (skg-request-save-buffer))
    (sleep-for 1.0)))

(defun phase-4-verify-saved-buffer ()
  "Verify that buffer B's save response reflects the edits."
  (message "=== PHASE 4: Verify buffer B after save ===")
  (setq integration-test-phase "phase-4-verify-saved-buffer")
  (let ((buf (get-buffer "*skg: b*")))
    (unless buf
      (message "✗ FAIL [phase 4]: Buffer *skg: b* no longer exists")
      (kill-emacs 1))
    (assert-headline-titles
     buf
     '((1 . "b")
       (2 . "Node a was given this longer title")
       (3 . "b")
       (3 . "c"))
     "phase 4: buffer B after save")
    (with-current-buffer buf ;; Verify line 4 has title "c" and an ID
      (goto-char (point-min))
      (forward-line 3)
      (let* ((line (buffer-substring-no-properties
                    (line-beginning-position) (line-end-position)))
             (parts (skg-split-as-stars-metadata-title line))
             (metadata-str (nth 1 parts))
             (title (nth 2 parts))
             (sexp (condition-case nil
                       (car (read-from-string metadata-str))
                     (error nil)))
             (id (skg--extract-id-from-metadata-sexp sexp)))
        (unless (equal title "c")
          (message "✗ FAIL [phase 4]: Line 4 title is %S, expected \"c\"" title)
          (kill-emacs 1))
        (unless id
          (message "✗ FAIL [phase 4]: Line 4 has no node id in metadata")
          (message "  Metadata: %S" metadata-str)
          (kill-emacs 1))
        (message "✓ PASS [phase 4]: c was assigned UUID %s" id)))))

(defun phase-5-verify-collateral ()
  "Check collateral buffer A.
Collateral completion currently fails when a new node was created
\(build_child_creation_data can't find the new UUID), so buffer A
is NOT updated. Assert that it's unchanged from the initial view."
  (message "=== PHASE 5: Verify collateral buffer A (known limitation) ===")
  (setq integration-test-phase "phase-5-verify-collateral")
  (let ((buf (get-buffer "*skg: a*")))
    (unless buf
      (message "✗ FAIL [phase 5]: Buffer *skg: a* no longer exists")
      (kill-emacs 1))
    (message "Buffer A content (should be unchanged): %S"
             (with-current-buffer buf
               (buffer-substring-no-properties (point-min) (point-max))))
    ;; Buffer A is unchanged because collateral completion failed
    (assert-headline-structure
     buf
     '((1 . "a") (2 . "b") (3 . "a"))
     "phase 5: buffer A unchanged (collateral failed on new UUID)")))

(defun run-all-tests ()
  "Main orchestrator function that runs all integration tests."
  (message "=== SKG Collateral Add Content Integration Test ===")

  ;; Set the port from environment variable if available
  (let ((test-port (getenv "SKG_TEST_PORT")))
    (when test-port
      (setq skg-port (string-to-number test-port))
      (message "Using test port: %d" skg-port)))

  ;; Wait a moment for server to be fully ready
  (sleep-for 0.25)

  (phase-1-open-buffer-a)
  (phase-2-open-buffer-b)
  (phase-3-edit-and-save)
  (phase-4-verify-saved-buffer)
  (phase-5-verify-collateral)

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
