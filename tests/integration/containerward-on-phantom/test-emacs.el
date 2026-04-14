;;; Integration test: containerward view request on a removed-here phantom.
;;;
;;; Graph (at HEAD):  a contains [b, c],  b contains [c].
;;;
;;; Phase 1: Open view from a.  Shows a -> {b -> c(indef), c}.
;;; Phase 2: Delete the indefinitive c from under b and save.
;;;          b.skg is updated (contains: []).
;;; Phase 3: Toggle diff mode on.
;;;          Under b, c appears as a removed-here phantom.
;;; Phase 4: Request containerward on the phantom c.
;;; Phase 5: Verify that the containerward path (a) was integrated.

(load-file "../../../elisp/skg-init.el")
(load-file "../save_collateral_break_cycle/test-helpers.el")
(load-file "../test-wait.el")

(defvar integration-test-phase "starting")

;; ─── Phase 1: Open view from a ──────────────────────────────

(defun phase-1-open-view ()
  (message "=== PHASE 1: Open view from a ===")
  (setq integration-test-phase "phase-1")
  (skg-request-single-root-content-view-from-id "a")
  (skg-test-wait-for-buffer "*a*")
  (let ((buf (get-buffer "*a*")))
    (unless buf
      (message "✗ FAIL [phase 1]: buffer *a* not created")
      (kill-emacs 1))
    ;; a -> {b -> c(indef), c}
    (assert-headline-titles
     buf
     '((1 . "a") (2 . "b") (3 . "c") (2 . "c"))
     "phase 1: initial view")))

;; ─── Phase 2: Remove c from under b, save ───────────────────

(defun phase-2-remove-c-from-b-and-save ()
  (message "=== PHASE 2: Remove c from under b and save ===")
  (setq integration-test-phase "phase-2")
  (with-current-buffer "*a*"
    (let ((inhibit-read-only t))
      ;; Find the indefinitive c under b (the *** line)
      (goto-char (point-min))
      (unless (re-search-forward "^\\*\\*\\* .*(skg.*indefinitive" nil t)
        (message "✗ FAIL [phase 2]: could not find indefinitive c under b")
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
     '((1 . "a") (2 . "b") (2 . "c"))
     "phase 2: after removing c from b")
    (skg-request-save-buffer)
    (skg-test-wait-for-response)
    (message "✓ PASS [phase 2]: saved")))

;; ─── Phase 3: Toggle diff mode on ───────────────────────────

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
    (unless (string-match-p "removed-here" content)
      (message "✗ FAIL [phase 3]: no removed-here phantom found")
      (message "  Buffer: %S" content)
      (kill-emacs 1))
    (message "✓ PASS [phase 3]: removed-here phantom present")))

;; ─── Phase 4: Request containerward on phantom c ─────────────

(defun phase-4-request-containerward ()
  (message "=== PHASE 4: Request containerward on phantom c ===")
  (setq integration-test-phase "phase-4")
  (with-current-buffer "*a*"
    ;; Position on the removed-here phantom c (under b)
    (goto-char (point-min))
    (unless (re-search-forward "^\\*\\*\\* .*(skg.*removed-here" nil t)
      (message "✗ FAIL [phase 4]: could not find removed-here phantom line")
      (message "  Buffer: %S" (buffer-substring-no-properties
                                (point-min) (point-max)))
      (kill-emacs 1))
    (beginning-of-line)
    (message "  Positioned on: %s"
             (buffer-substring-no-properties
              (line-beginning-position) (line-end-position)))
    ;; Request containerward view
    (skg-request-containerward-view)
    (skg-test-wait-for-response 20)
    (message "✓ PASS [phase 4]: containerward request completed")))

;; ─── Phase 5: Verify containerward path was integrated ───────

(defun phase-5-verify-result ()
  (message "=== PHASE 5: Verify containerward path ===")
  (setq integration-test-phase "phase-5")
  (let* ((buf (get-buffer "*a*"))
         (content (with-current-buffer buf
                    (buffer-substring-no-properties
                     (point-min) (point-max)))))
    (message "  Final buffer: %S" content)
    ;; The containerward path for c should show a as container.
    ;; Look for a level-4 node under the phantom c (level 3)
    ;; that has "containerOf" in its metadata.
    (unless (string-match-p "containerOf" content)
      (message "✗ FAIL [phase 5]: no containerOf node found in buffer")
      (message "  Expected a containerward path under phantom c")
      (kill-emacs 1))
    ;; Also verify that viewRequests was consumed (not still in the metadata)
    (when (string-match-p "viewRequests" content)
      (message "✗ FAIL [phase 5]: viewRequests not consumed")
      (kill-emacs 1))
    (message "✓ PASS [phase 5]: containerward path integrated")))

;; ─── Main ────────────────────────────────────────────────────

(defun run-all-tests ()
  (message "=== SKG Containerward-on-Phantom Integration Test ===")
  (let ((test-port (getenv "SKG_TEST_PORT")))
    (when test-port
      (setq skg-port (string-to-number test-port))))

  (phase-1-open-view)
  (phase-2-remove-c-from-b-and-save)
  (phase-3-toggle-diff-on)
  (phase-4-request-containerward)
  (phase-5-verify-result)

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
