;;; Integration test for skg-search-make-link.
;;;
;;; With point between headlines a and b,
;;; upon running skg-search-make-link, buffer goes from
;;;     * a
;;;
;;;     * b
;;;   to
;;;     * a
;;;     [[id:X][LABEL]]
;;;     * b
;;;
;;; Three fixtures: ~parent~ (the node we open a content view of)
;;; contains ~child~; ~target~ is the node we'll search for.

(load-file "../../../elisp/skg-init.el")
(load-file "../../../elisp/skg-test-utils.el")
(load-file "../test-wait.el")

(defvar integration-test-phase "starting")

(defun fail (fmt &rest args)
  (message "✗ FAIL: %s" (apply #'format fmt args))
  (kill-emacs 1))

(defun open-parent-content-view ()
  "Open a content view of the parent node and return its buffer."
  (message "=== PHASE 1: Opening content view for parent ===")
  (skg-request-single-root-content-view-from-id "parent")
  (let ((buf (skg-test-wait-for-buffer "*parent node*" 15)))
    (unless buf
      ;; The buffer name is derived from the title; fall back to a
      ;; broader search for any non-search skg content view buffer.
      (setq buf (find-skg-content-buffer)))
    (unless buf
      (fail "no content view buffer was created"))
    (with-current-buffer buf
      (unless (derived-mode-p 'skg-content-view-mode)
        (fail "buffer %s is not in skg-content-view-mode"
              (buffer-name buf)))
      (when (buffer-file-name)
        (fail "buffer %s unexpectedly visits a file: %s"
              (buffer-name buf) (buffer-file-name)))
      (message "✓ content view buffer %s open and not file-visiting"
               (buffer-name buf))
      buf)))

(defun park-point-on-blank-line-between-headlines (buf)
  "In BUF, find the parent's headline, insert a blank line below it,
park point on that blank line. Return the point position."
  (switch-to-buffer buf)
  (goto-char (point-min))
  ;; Find the line ending the parent's headline (level-1).
  (unless (re-search-forward "^\\* " nil t)
    (fail "no level-1 headline found in source buffer"))
  (end-of-line)
  ;; Sanity-check the next line is the child's headline.
  (save-excursion
    (forward-line 1)
    (unless (looking-at "^\\*\\* ")
      (fail "expected level-2 child headline after parent, got: %S"
            (buffer-substring-no-properties
             (point) (line-end-position)))))
  ;; Insert a fresh blank line between the parent's headline and the
  ;; child's headline, then park point on that blank line.
  (insert "\n")
  (let ((pos (point)))
    (message "✓ inserted blank line; point at %d" pos)
    (message "  buffer now:\n%s"
             (buffer-substring-no-properties
              (point-min) (point-max)))
    pos))

(defun invoke-search-make-link (search-terms)
  (message "=== PHASE 2: Invoking skg-search-make-link ===")
  (setq integration-test-phase "invoking-search-make-link")
  (condition-case err
      (skg-search-make-link search-terms)
    (error (fail "skg-search-make-link signalled: %S" err))))

(defun pick-target-and-finish (search-terms)
  (message "=== PHASE 3: Picking target result ===")
  (setq integration-test-phase "waiting-for-search-buffer")
  (let* ((search-buf-name (skg-search-buffer-name search-terms))
         (search-buf (skg-test-wait-for-buffer search-buf-name 15)))
    (unless search-buf
      (fail "search buffer %S never appeared" search-buf-name))
    (with-current-buffer search-buf
      (unless skg-search-make-link-mode
        (fail "search buffer was not in skg-search-make-link-mode"))
      (goto-char (point-min))
      (unless (re-search-forward "(id target)" nil t)
        (fail "target result not present in search buffer; got:\n%s"
              (buffer-substring-no-properties
               (point-min) (point-max))))
      (goto-char (match-beginning 0))
      (message "✓ point on target result; calling finish")
      (setq integration-test-phase "calling-finish")
      (skg-search-make-link-finish))))

(defun verify-link-on-its-own-line (source-buf)
  "Verify that SOURCE-BUF has a [[id:target][...]] link followed by
its own newline, with the child's `** child node' headline still
intact on the next line — i.e. the link did NOT eat the blank
line's trailing newline."
  (message "=== PHASE 4: Verifying link landed on the blank line ===")
  (unless (buffer-live-p source-buf)
    (fail "source buffer was killed"))
  (with-current-buffer source-buf
    (let ((text (buffer-string)))
      (message "Final source buffer:\n%s" text)
      (let ((link-rx "\\[\\[id:target\\]\\[\\([^]]+\\)\\]\\]"))
        (unless (string-match link-rx text)
          (fail "no [[id:target][LABEL]] link found")))
      (let ((bug-rx "\\[\\[id:target\\]\\[[^]]+\\]\\]\\*"))
        (when (string-match bug-rx text)
          (fail "link is glued to the next headline (the reported bug):\n%s"
                text)))
      (let ((good-rx "\\[\\[id:target\\]\\[[^]]+\\]\\]\n\\*\\*"))
        (unless (string-match good-rx text)
          (fail "link not followed by newline + level-2 headline; got:\n%s"
                text))))
    (message "✓ link is on its own line; ** child node intact on next line")))

(defun run-test ()
  (message "=== SKG search-make-link blank-line Integration Test ===")
  (let ((test-port (getenv "SKG_TEST_PORT")))
    (when test-port
      (setq skg-port (string-to-number test-port))
      (message "Using test port: %d" skg-port)))
  (let* ((source-buf (open-parent-content-view))
         (_pos (park-point-on-blank-line-between-headlines source-buf)))
    (invoke-search-make-link "target")
    (pick-target-and-finish "target")
    (skg-test-wait-for
     (lambda ()
       (with-current-buffer source-buf
         (string-match-p "\\[\\[id:target\\]" (buffer-string))))
     5)
    (verify-link-on-its-own-line source-buf))
  (message "✓ PASS: Integration test successful!")
  (kill-emacs 0))

(progn
  (run-at-time
   30 nil
   (lambda ()
     (message "TIMEOUT: integration test timed out at phase: %s"
              integration-test-phase)
     (kill-emacs 1)))
  (run-test))
