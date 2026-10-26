;;; Integration test: opening a content view of `child', whose
;;; parent contains it, should render the buffer with `child''s
;;; containers folder as its first child, holding `parent',
;;; write-protected. The view-root itself is a member, which emitted
;;; metadata leaves implicit.

(load-file "../../../elisp/skg-init.el")
(load-file "../../../elisp/skg-test-utils.el")
(load-file "../test-wait.el")

(defvar integration-test-phase "starting")

(defun fail (fmt &rest args)
  (message "✗ FAIL: %s" (apply #'format fmt args))
  (kill-emacs 1))

(defun open-child-content-view ()
  (message "=== PHASE 1: Requesting content view of `child' ===")
  (skg-request-single-root-content-view-from-id "child")
  (let ((buf (skg-test-wait-for-buffer "*the child*" 15)))
    (unless buf (setq buf (find-skg-content-buffer)))
    (unless buf (fail "no content view buffer was created"))
    buf))

(defun verify-ancestry-prepended (buf)
  (message "=== PHASE 2: Verifying the containers folder holding parent ===")
  (with-current-buffer buf
    (let ((text (skg-test-without-viewnode-ids (buffer-string))))
      (message "Buffer:\n%s" text)
      (unless (string-match-p "^\\* (skg node (gnid child)" text)
        (fail "view-root not as expected; got:\n%s" text))
      (let ((root-line (and (string-match "^\\* (skg node (gnid child) .*$" text)
                            (match-string 0 text))))
        (unless root-line
          (fail "no view-root headline for child; buffer:\n%s" text))
        (when (string-match-p "noClaim" root-line)
          (fail "contained view-root should be content, not false; line: %S"
                root-line))
        (when (string-match-p "noClaim writeProtected (view (rels (contains (out 1 (ancestors 1)))) (birth (contains out 1)))" root-line)
          (fail "contained view-root should be content, not content; line: %S"
                root-line))
        (when (string-match-p "noClaim writeProtected (view (rels (linksTo (out (ancestors 1)))) (birth (linksTo out 1)))" root-line)
          (fail "contained view-root should be content, not line: %S"
                root-line)))
      (unless (string-match-p "^\\* (skg node (gnid child) .*\n\\*\\* (skg containerFolder" text)
        (fail "the root's first child is not its containers folder; buffer:\n%s" text))
      (let ((line (and (string-match
                        "^\\*\\*\\* (skg node (gnid parent) .*$" text)
                       (match-string 0 text))))
        (unless line
          (fail "no level-3 headline for parent; buffer:\n%s" text))
        (unless (string-match-p "(gnid parent) (skgRepos (nodeRepo main)) writeProtected (view (rels (contains (out 1 (ancestors 2)))) (birth (contains out 2)))" line)
          (fail "parent is not a write-protected member of the folder; line: %S" line))
        (unless (string-match-p " writeProtected\\b" line)
          (fail "parent is not writeProtected; line: %S" line))))))

(defun run-test ()
  (message "=== SKG content-view-from-non-root Integration Test ===")
  (let ((test-port (getenv "SKG_TEST_PORT")))
    (when test-port
      (setq skg-port (string-to-number test-port))
      (message "Using test port: %d" skg-port)))
  (let ((buf (open-child-content-view)))
    (verify-ancestry-prepended buf))
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
