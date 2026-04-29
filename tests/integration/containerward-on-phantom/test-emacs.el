;;; Integration test: auto-inserted containerward ancestry on removed-here phantoms.
;;;
;;; Graph (at HEAD):  a contains [b, c],  b contains [c].
;;;
;;; Phase 1: Open view from a.  Shows a -> {b -> c(indef), c}.
;;; Phase 2: Delete the indefinitive c from under b and save.
;;;          b.skg is updated (contains: []).
;;; Phase 3: Toggle diff mode on.
;;;          Under b, c appears as a removed-here phantom
;;;          with containerward ancestry auto-inserted.
;;; Phase 4: Verify that the containerward path (a) was integrated.

(load-file "../../../elisp/skg-init.el")
(load-file "../save_collateral_break_cycle/test-helpers.el")
(load-file "../test-wait.el")

(defvar integration-test-phase "starting")

(defun phase-1-open-view-from-a ()
  (message "=== PHASE 1: Open view from a ===")
  (setq integration-test-phase "phase-1")
  (skg-request-single-root-content-view-from-id "a")
  (skg-test-wait-for-buffer "*a*")
  (let ((buf (get-buffer "*a*")))
    (unless buf
      (message "✗ FAIL [phase 1]: buffer *a* not created")
      (kill-emacs 1))
    ;; a -> {b -> c(indef), c}.
    (assert-headline-titles
     buf
     '((1 independent "a")
       (2 contentOf   "b")
       (3 contentOf   "c")
       (2 contentOf   "c"))
     "phase 1: initial view")))

(defun phase-2-remove-c-from-under-b-and-save ()
  (message "=== PHASE 2: Remove c from under b and save ===")
  (setq integration-test-phase "phase-2")
  (with-current-buffer "*a*"
    (let ((inhibit-read-only t))
      ;; Find the indef c under b (the *** line)
      (goto-char (point-min))
      (unless (re-search-forward "^\\*\\*\\* .*(skg.* indef\\b" nil t)
        (message "✗ FAIL [phase 2]: could not find indef c under b")
        (message "  Buffer: %S" (buffer-substring-no-properties
                                  (point-min) (point-max)))
        (kill-emacs 1))
      (beginning-of-line)
      (let ((beg (point))
            (end (progn (forward-line 1) (point))))
        (delete-region beg end)))
    ;; Buffer should now be: a -> {b, c}
    (assert-headline-titles
     (current-buffer)
     '((1 independent "a")
       (2 contentOf   "b")
       (2 contentOf   "c"))
     "phase 2: after removing c from b")
    (skg-request-save-buffer)
    (skg-test-wait-for-response)
    (message "✓ PASS [phase 2]: saved")))

(defun phase-3-toggle-diff-on ()
  (message "=== PHASE 3: Toggle diff mode ON ===")
  (setq integration-test-phase "phase-3")
  (switch-to-buffer "*a*")
  (skg-view-diff-mode)
  (skg-test-wait-for-response 20)
  ;; Verify that a removed-here phantom c appears under b
  (let* ((content (with-current-buffer "*a*"
                    (buffer-substring-no-properties
                     (point-min) (point-max)))))
    (unless (string-match-p "(unstaged removedM)" content)
      (message "✗ FAIL [phase 3]: no removed-here phantom found")
      (message "  Buffer: %S" content)
      (kill-emacs 1))
    (message "✓ PASS [phase 3]: removed-here phantom present")))

(defun phase-4-verify-containerward-path-was-inserted ()
  (message "=== PHASE 4: Verify containerward path ===")
  (setq integration-test-phase "phase-4")
  (let* ((buf (get-buffer "*a*"))
         (content (with-current-buffer buf
                    (buffer-substring-no-properties
                     (point-min) (point-max)))))
    (message "  Final buffer: %S" content)
    ;; The containerward path for c should show a as container.
    ;; Look for a node under the phantom c
    ;; that has "containerOf" in its metadata.
    (unless (string-match-p "containerOf" content)
      (message "✗ FAIL [phase 4]: no containerOf node found in buffer")
      (message "  Expected containerward ancestry under phantom c")
      (kill-emacs 1))
    (message "✓ PASS [phase 4]: containerward ancestry auto-inserted")))

(defun run-all-tests ()
  (message "=== SKG Containerward-on-Phantom Integration Test ===")
  (let ((test-port (getenv "SKG_TEST_PORT")))
    (when test-port
      (setq skg-port (string-to-number test-port))))

  (phase-1-open-view-from-a)
  (phase-2-remove-c-from-under-b-and-save)
  (phase-3-toggle-diff-on)
  (phase-4-verify-containerward-path-was-inserted)

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
