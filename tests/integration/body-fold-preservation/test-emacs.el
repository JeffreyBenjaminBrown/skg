;;; Integration test: body folding survives a save round-trip.
;;;
;;; Two scenarios are tested back-to-back, each in its own buffer:
;;;
;;; A) Subtree-fold on a node with body + child:
;;;    `org-cycle' once → entire subtree hidden (body + child).
;;;    After save: body and child should stay hidden.
;;;
;;; B) CHILDREN-state on a node with body + child:
;;;    `org-cycle' twice → child *headline* visible, parent body
;;;    visible, but the *child's* body is hidden.
;;;    After save: the child's body should still be hidden. The
;;;    current fold-marker mechanism only marks invisible *headlines*.
;;;    The child isn't hidden, and a body line isn't a headline, so
;;;    nothing gets marked — the child's body comes back visible.
;;;    That's the bug this scenario exposes.
;;;
;;; The assertion compares before/after fold "snapshots". A snapshot
;;; is a string with one line per buffer line, prefixed with `[V]`
;;; (visible) or `[H]` (hidden), with skg metadata stripped from
;;; headlines so the comparison shows only the structure that should
;;; round-trip.

(load-file "../../../elisp/skg-init.el")
(load-file "../test-wait.el")

(defvar integration-test-phase "starting")
(defvar integration-test-completed nil)

(defun fail (fmt &rest args)
  (message "✗ FAIL: %s" (apply 'format fmt args))
  (kill-emacs 1))

(defun pass (fmt &rest args)
  (message "✓ PASS: %s" (apply 'format fmt args)))

(defun bfp-strip-metadata (line)
  "Drop the `(skg ...)' sexp from LINE, keeping `* title' shape."
  (let ((parts (skg-split-as-stars-metadata-title line)))
    (if parts
        (concat (car parts) (nth 2 parts))
      line)))

(defun bfp-snapshot ()
  "Return a multi-line snapshot of the buffer:
each line prefixed with `[V]' or `[H]' for visibility, with skg
metadata stripped from headlines so headlines read like `* title'."
  (save-excursion
    (goto-char (point-min))
    (let ((acc ""))
      (while (not (eobp))
        (let* ((bol (line-beginning-position))
               (eol (line-end-position))
               (line (buffer-substring-no-properties bol eol))
               (vis (if (invisible-p bol) "[H]" "[V]"))
               (clean (bfp-strip-metadata line)))
          (setq acc (concat acc vis " " clean "\n")))
        (forward-line 1))
      acc)))

(defun bfp-build-buffer (name)
  "Create a content-view buffer named NAME with our test fixture."
  (let ((buf (get-buffer-create name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t)) (erase-buffer))
      (insert
       (concat
        "* (skg (node (source main))) root\n"
        "root body line one\n"
        "root body line two\n"
        "** (skg (node (source main))) child\n"
        "child body line\n"))
      (skg-content-view-mode)
      (setq skg-view-uri (org-id-uuid))
      (goto-char (point-min)))
    buf))

(defun bfp-run-scenario (name expected-snapshot-before cycles)
  "Run one save-round-trip scenario.
NAME is a label; CYCLES is how many times to call `org-cycle' on root.
EXPECTED-SNAPSHOT-BEFORE is the exact multi-line snapshot we expect
*before* save — a sanity check that this emacs actually folds the way
we think it does.
After save we assert the snapshot is unchanged.
Returns a short diagnostic string on failure, nil on success."
  (message "")
  (message "=== Scenario %s : %d org-cycle(s) on root ===" name cycles)
  (let ((buf (bfp-build-buffer (format "*skg-bfp-%s*" name))))
    (switch-to-buffer buf)
    (goto-char (point-min))
    (dotimes (_ cycles) (org-cycle))

    (let ((before (bfp-snapshot)))
      (message "--- snapshot BEFORE save ---\n%s" before)
      (unless (equal before expected-snapshot-before)
        (fail
         "Scenario %s: BEFORE snapshot did not match expectation.
EXPECTED:
%s
ACTUAL:
%s"
         name expected-snapshot-before before))

      (setq integration-test-phase (format "%s-saving" name))
      (condition-case err
          (skg-request-save-buffer)
        (error
         (fail "Scenario %s: save raised: %S" name err)))
      (skg-test-wait-for-response)

      (let ((after (bfp-snapshot)))
        (message "--- snapshot AFTER save ---\n%s" after)
        (if (equal before after)
            (progn (pass "Scenario %s: fold state preserved" name) nil)
          (message "DIFF in scenario %s.\nBEFORE:\n%s\nAFTER:\n%s"
                   name before after)
          ;; Don't kill emacs yet — we want all scenarios to report.
          (format "Scenario %s: BEFORE/AFTER differ" name))))))

(defun run-all-tests ()
  (message "=== SKG Body-Fold-Preservation Integration Test ===")
  (let ((test-port (getenv "SKG_TEST_PORT")))
    (when test-port
      (setq skg-port (string-to-number test-port))))

  (let* ((diff-a (bfp-run-scenario
                  "A-subtree"
                  (concat
                   "[V] * root\n"
                   "[H] root body line one\n"
                   "[H] root body line two\n"
                   "[H] ** child\n"
                   "[H] child body line\n")
                  1))
         (diff-b (bfp-run-scenario
                  "B-children"
                  (concat
                   "[V] * root\n"
                   "[V] root body line one\n"
                   "[V] root body line two\n"
                   "[V] ** child\n"
                   "[H] child body line\n")
                  2))
         (diffs  (delq nil (list diff-a diff-b))))
    (if diffs
        (fail "Scenarios with broken fold-preservation: %s"
              (mapconcat #'identity diffs "; "))
      (pass "All fold-preservation scenarios passed"))
    (setq integration-test-completed t)
    (kill-emacs 0)))

(progn
  (run-at-time
   60 nil
   (lambda ()
     (message "TIMEOUT after phase: %s" integration-test-phase)
     (kill-emacs 1)))
  (run-all-tests))
