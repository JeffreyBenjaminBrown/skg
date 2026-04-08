;;; Integration test for git diff view updates.
;;; For an overview, see
;;;   tests/integration/git-diff-view-updates/data/skg-data/README.org

(load-file "../../../elisp/skg-init.el")
(load-file "../save_collateral_break_cycle/test-helpers.el")
(load-file "../test-wait.el")

(defvar integration-test-phase "starting")

;; ─── Phase 1: Open view-a ───────────────────────────────────

(defun phase-1-open-view-a ()
  (message "=== PHASE 1: Open view from a ===")
  (setq integration-test-phase "phase-1")
  (skg-request-single-root-content-view-from-id "a")
  (skg-test-wait-for-buffer "*a*")
  (let ((buf (get-buffer "*a*")))
    (unless buf
      (message "✗ FAIL [phase 1]: buffer *a* not created")
      (kill-emacs 1))
    (assert-headline-titles
     buf
     '((1 . "a") (2 . "b") (3 . "c") (3 . "d") (3 . "e"))
     "phase 1: view-a initial")))

;; ─── Phase 2: Open view-b ───────────────────────────────────

(defun phase-2-open-view-b ()
  (message "=== PHASE 2: Open view from b ===")
  (setq integration-test-phase "phase-2")
  (skg-request-single-root-content-view-from-id "b")
  (skg-test-wait-for-buffer "*b*")
  (let ((buf (get-buffer "*b*")))
    (unless buf
      (message "✗ FAIL [phase 2]: buffer *b* not created")
      (kill-emacs 1))
    (assert-headline-titles
     buf
     '((1 . "b") (2 . "c") (2 . "d") (2 . "e"))
     "phase 2: view-b initial")))

;; ─── Phase 3: Edit view-b and save ─────────────────────────

(defun phase-3-edit-and-save ()
  (message "=== PHASE 3: Edit view-b and save ===")
  (setq integration-test-phase "phase-3")
  (let ((buf (get-buffer "*b*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t)
            content)
        (setq content (buffer-substring-no-properties (point-min) (point-max)))
        (message "Buffer-b before edit: %S" content)
        ;; Replace the entire buffer with the desired post-edit state.
        ;; c gets editRequest delete; d is removed from b's children;
        ;; e is renamed; f added with d as child.
        (erase-buffer)
        (insert "* (skg (node (id b) (source main))) b\n")
        (insert "** (skg (node (id c) (source main) (editRequest delete))) c\n")
        (insert "** (skg (node (id e) (source main))) e, edited\n")
        (insert "** (skg (node (id f) (source main))) f\n")
        (insert "*** (skg (node (id d) (source main))) d\n")))
    (with-current-buffer buf
      (skg-request-save-buffer))
    (skg-test-wait-for-response)))

;; ─── Phase 4: Verify view-b after save ─────────────────────

(defun phase-4-verify-after-save ()
  (message "=== PHASE 4: Verify view-b after save ===")
  (setq integration-test-phase "phase-4")
  (let ((buf (get-buffer "*b*")))
    (message "Buffer-b after save: %S"
             (with-current-buffer buf
               (buffer-substring-no-properties (point-min) (point-max))))
    ;; c should be gone (deleted). d should appear under f.
    (assert-headline-titles
     buf
     '((1 . "b") (2 . "e, edited") (2 . "f") (3 . "d"))
     "phase 4: view-b after save")))

;; ─── Phase 5: Toggle diff mode on ──────────────────────────

(defun phase-5-toggle-diff-on ()
  (message "=== PHASE 5: Toggle git diff mode ON ===")
  (setq integration-test-phase "phase-5")
  ;; skg-view-diff-mode triggers a save which re-renders.
  ;; We need to be in a content-view buffer for the save to work.
  (switch-to-buffer "*b*")
  (skg-view-diff-mode)
  (skg-test-wait-for-response 20))

;; ─── Phase 6: Verify diff markers in view-b ────────────────

(defun phase-6-verify-diff-view-b ()
  (message "=== PHASE 6: Verify diff markers in view-b ===")
  (setq integration-test-phase "phase-6")
  (let* ((buf (get-buffer "*b*"))
         (content (with-current-buffer buf
                    (buffer-substring-no-properties
                     (point-min) (point-max)))))
    (message "Buffer-b with diff: %S" content)
    ;; Check that key diff markers appear somewhere in the buffer.
    ;; We check for the presence of diff-related strings rather
    ;; than exact line-by-line match, because graphStats etc. may vary.
    (dolist (expected '(("removed" . "c")     ;; c was deleted
                        ("e, edited" . nil)    ;; e's new title present
                        ("f" . nil)))          ;; f present
      (let ((marker (car expected)))
        (unless (string-match-p (regexp-quote marker) content)
          (message "✗ FAIL [phase 6]: expected %S in view-b" marker)
          (kill-emacs 1))))
    (message "✓ PASS [phase 6]: diff markers present in view-b")))

;; ─── Phase 7: Verify diff markers in view-a ────────────────

(defun phase-7-verify-diff-view-a ()
  (message "=== PHASE 7: Verify diff markers in view-a ===")
  (setq integration-test-phase "phase-7")
  (let* ((buf (get-buffer "*a*"))
         (content (with-current-buffer buf
                    (buffer-substring-no-properties
                     (point-min) (point-max)))))
    (message "Buffer-a with diff: %S" content)
    ;; a's view should show the same diff markers under b.
    (dolist (expected '(("removed" . "c")
                        ("e, edited" . nil)
                        ("f" . nil)))
      (let ((marker (car expected)))
        (unless (string-match-p (regexp-quote marker) content)
          (message "✗ FAIL [phase 7]: expected %S in view-a" marker)
          (kill-emacs 1))))
    (message "✓ PASS [phase 7]: diff markers present in view-a")))

;; ─── Phase 8: Commit e's title change, re-save ─────────────

(defun phase-8-commit-e-and-resave ()
  (message "=== PHASE 8: Commit e.skg, re-save to refresh diff ===")
  (setq integration-test-phase "phase-8")
  ;; Shell out to git to commit e.skg
  (let* ((skg-data (getenv "SKG_DATA_DIR"))
         (default-directory (or skg-data
                                (expand-file-name "data/skg-data"
                                                  (file-name-directory load-file-name)))))
    (message "Committing e.skg in %s" default-directory)
    (shell-command "git add e.skg && git commit -q -m 'update e title'"))
  ;; Re-save to trigger diff recomputation
  (with-current-buffer "*b*"
    (skg-request-save-buffer))
  (skg-test-wait-for-response 20))

;; ─── Phase 9: Verify textChanged gone ───────────────────────

(defun phase-9-verify-no-text-changed ()
  (message "=== PHASE 9: Verify textChanged gone after commit ===")
  (setq integration-test-phase "phase-9")
  (let* ((buf (get-buffer "*b*"))
         (content (with-current-buffer buf
                    (buffer-substring-no-properties
                     (point-min) (point-max)))))
    (message "Buffer-b after commit: %S" content)
    (when (string-match-p "textChanged" content)
      (message "✗ FAIL [phase 9]: textChanged still in view-b after committing e.skg")
      (kill-emacs 1))
    (message "✓ PASS [phase 9]: textChanged gone from view-b")))

;; ─── Phase 10: Toggle diff mode off ─────────────────────────

(defun phase-10-toggle-diff-off ()
  (message "=== PHASE 10: Toggle git diff mode OFF ===")
  (setq integration-test-phase "phase-10")
  (switch-to-buffer "*b*")
  (skg-view-diff-mode)
  (skg-test-wait-for-response 20))

;; ─── Phase 11: Verify clean views ───────────────────────────

(defun phase-11-verify-clean-views ()
  (message "=== PHASE 11: Verify views are clean (no diff markers) ===")
  (setq integration-test-phase "phase-11")
  ;; view-b should have no diff/phantom nodes
  (let ((buf-b (get-buffer "*b*")))
    (assert-headline-titles
     buf-b
     '((1 . "b") (2 . "e, edited") (2 . "f") (3 . "d"))
     "phase 11: view-b clean"))
  ;; view-a should have no diff/phantom nodes
  (let* ((buf-a (get-buffer "*a*"))
         (titles (headline-titles buf-a)))
    (message "Buffer-a titles after diff-off: %S" titles)
    ;; a should contain b, b should contain e,f; f contains d.
    ;; The exact indefinitive markers vary, so just check titles.
    (dolist (expected-title '("a" "b" "e, edited" "f" "d"))
      (unless (cl-find expected-title titles :key #'cdr :test #'equal)
        (message "✗ FAIL [phase 11]: expected title %S in view-a" expected-title)
        (kill-emacs 1)))
    ;; Should NOT have diff markers
    (let ((content (with-current-buffer buf-a
                     (buffer-substring-no-properties (point-min) (point-max)))))
      (when (string-match-p "diff:" content)
        (message "✗ FAIL [phase 11]: diff markers remain in view-a")
        (kill-emacs 1))
      (when (string-match-p "textChanged" content)
        (message "✗ FAIL [phase 11]: textChanged remains in view-a")
        (kill-emacs 1))))
  (message "✓ PASS [phase 11]: both views clean"))

;; ─── Main ────────────────────────────────────────────────────

(defun run-all-tests ()
  (message "=== SKG Git Diff View Updates Integration Test ===")
  (let ((test-port (getenv "SKG_TEST_PORT")))
    (when test-port
      (setq skg-port (string-to-number test-port))))

  (phase-1-open-view-a)
  (phase-2-open-view-b)
  (phase-3-edit-and-save)
  (phase-4-verify-after-save)
  (phase-5-toggle-diff-on)
  (phase-6-verify-diff-view-b)
  (phase-7-verify-diff-view-a)
  (phase-8-commit-e-and-resave)
  (phase-9-verify-no-text-changed)
  (phase-10-toggle-diff-off)
  (phase-11-verify-clean-views)

  (message "✓ PASS: All phases completed!")
  (kill-emacs 0))

(progn
  (run-at-time
   45 nil
   (lambda ()
     (message "TIMEOUT: Integration test timed out!")
     (message "Last phase: %s" integration-test-phase)
     (kill-emacs 1)))
  (run-all-tests))
