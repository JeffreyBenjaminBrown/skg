;;; Integration test: save a content view whose body contains
;;; tabs + newlines, then inspect the on-disk .skg file and verify
;;; the body was written as a YAML block literal (|…) rather than
;;; a double-quoted one-line string full of \n escapes.

(load-file "../../../elisp/skg-init.el")
(load-file "../test-wait.el")

(defvar integration-test-phase "starting")
(defvar integration-test-completed nil)

(defun fail (fmt &rest args)
  (message "✗ FAIL: %s" (apply 'format fmt args))
  (kill-emacs 1))

(defun pass (fmt &rest args)
  (message "✓ PASS: %s" (apply 'format fmt args)))

(defun run-all-tests ()
  (message "=== SKG Save-Body-With-Tabs Integration Test ===")
  (let ((test-port (getenv "SKG_TEST_PORT")))
    (when test-port
      (setq skg-port (string-to-number test-port))
      (message "Using test port: %d" skg-port)))
  (test-save-body-with-tabs)
  (let ((timeout 0))
    (while (and (not integration-test-completed) (< timeout 120))
      (sleep-for 0.25)
      (setq timeout (1+ timeout))))
  (unless integration-test-completed
    (message "TIMEOUT: No complete response received!")
    (message "Last phase: %s" integration-test-phase)
    (kill-emacs 1)))

(defun test-save-body-with-tabs ()
  (message "=== PHASE 1: construct content view with tab-bearing body ===")
  (let ((buffer (get-buffer-create "*skg-body-with-tabs*"))
        ;; Real tabs and real newlines inside the body lines:
        (org-text
         (concat
          "* (skg (node (id bwt-root) (source main))) bwt-root\n"
          "line one\n"
          "\t(elisp-code)\n"
          "line three\n")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)) (erase-buffer))
      (insert org-text)
      (skg-content-view-mode)
      (setq skg-view-uri (org-id-uuid))
      (goto-char (point-min))

      (message "=== PHASE 2: save buffer ===")
      (setq integration-test-phase "saving")
      (condition-case err
          (skg-request-save-buffer)
        (error
         (fail "skg-request-save-buffer raised: %S" err)))

      (skg-test-wait-for-response)

      (setq integration-test-phase "checking-disk")
      (let* ((skg-file
              (expand-file-name
               "data/skg-data/bwt-root.skg"
               (file-name-directory load-file-name)))
             (disk-contents
              (with-temp-buffer
                (insert-file-contents skg-file)
                (buffer-string))))
        (message "=== PHASE 3: file contents ===")
        (message "%s" disk-contents)

        ;; Assertion 1: body field must be present.
        (unless (string-match-p "^body:" disk-contents)
          (fail "body field missing from %s" skg-file))

        ;; Assertion 2: must use block-literal style, not double-quoted.
        (unless (string-match-p "^body: |" disk-contents)
          (fail "body is not a block literal. Got:\n%s" disk-contents))

        ;; Assertion 3: no literal backslash-n escape sequences anywhere.
        (when (string-match-p "\\\\n" disk-contents)
          (fail "file contains backslash-n escape(s). Got:\n%s" disk-contents))

        ;; Assertion 4: the tab char from the body must be preserved on disk.
        (unless (string-match-p "\t" disk-contents)
          (fail "expected tab preserved in block-literal body. Got:\n%s" disk-contents))

        (pass "body written as block literal; no \\n escapes; tab preserved")
        (setq integration-test-completed t)
        (kill-emacs 0)))))

(progn
  (run-at-time
   45 nil
   (lambda ()
     (message "TIMEOUT: Integration test timed out!")
     (message "Last phase: %s" integration-test-phase)
     (kill-emacs 1)))
  (run-all-tests))
