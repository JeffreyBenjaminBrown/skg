;;; Integration test: opening a content view of `child', whose
;;; parent contains it, should render the buffer with `parent'
;;; prepended as the first child of `child' (Birth::ContainsParent
;;; indefinitive). Because `child' is contained in the graph, the
;;; view-root is born as container; that default is implicit in
;;; emitted metadata.

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
  (message "=== PHASE 2: Verifying parent prepended as first child ===")
  (with-current-buffer buf
    (let ((text (buffer-string)))
      (message "Buffer:\n%s" text)
      (unless (string-match-p "^\\* (skg (node (id child)" text)
        (fail "view-root not as expected; got:\n%s" text))
      (let ((root-line (and (string-match "^\\* (skg (node (id child).*$" text)
                            (match-string 0 text))))
        (unless root-line
          (fail "no view-root headline for child; buffer:\n%s" text))
        (when (string-match-p "(parentIs independent)" root-line)
          (fail "contained view-root should be content, not independent; line: %S"
                root-line))
        (when (string-match-p "(parentIs independent) (birth backpath container)" root-line)
          (fail "contained view-root should be content, not content; line: %S"
                root-line))
        (when (string-match-p "(parentIs independent) (birth backpath linkSource)" root-line)
          (fail "contained view-root should be content, not line: %S"
                root-line)))
      (let ((line (and (string-match
                        "^\\*\\* (skg (node (id parent).*$" text)
                       (match-string 0 text))))
        (unless line
          (fail "no level-2 headline for parent; buffer:\n%s" text))
        (unless (string-match-p "(parentIs independent) (birth backpath container)" line)
          (fail "parent is not (parentIs independent) (birth backpath container); line: %S" line))
        (unless (string-match-p " indef\\b" line)
          (fail "parent is not indefinitive; line: %S" line))))))

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
