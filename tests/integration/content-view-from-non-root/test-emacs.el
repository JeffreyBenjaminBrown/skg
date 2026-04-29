;;; Integration test: opening a content view of `child', whose
;;; parent contains it, should render the buffer with `parent'
;;; prepended as the first child of `child' (Birth::ContainerOf
;;; indefinitive). The view-root's metadata
;;; `(graphStats (containers 1))' makes the count visible to the
;;; user; no separate notice is sent.

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
      ;; The view-root's graphStats should expose the container count
      ;; — that's the "explanation" the user sees, in lieu of any
      ;; separate notice.
      (unless (string-match-p
               "^\\* (skg (node (id child)[^)]*)[^)]*)[^)]*(graphStats[^)]*(containers 1)"
               text)
        (fail "view-root metadata does not show (containers 1); got:\n%s"
              text))
      (let ((line (and (string-match
                        "^\\*\\* (skg (node (id parent).*$" text)
                       (match-string 0 text))))
        (unless line
          (fail "no level-2 headline for parent; buffer:\n%s" text))
        (unless (string-match-p "(birth containerOf)" line)
          (fail "parent is not (birth containerOf); line: %S" line))
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
