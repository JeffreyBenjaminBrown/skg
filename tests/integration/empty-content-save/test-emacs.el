;;; If we open an empty skg buffer,
;;; enter the line '* (skg (node (id 1))) 1', and save,
;;; the result should be saved to 'data/skg/1.skg'.

(load-file "../../../elisp/skg-init.el")

(defvar integration-test-phase "starting")

(defun fail-test (message-text)
  (message "✗ FAIL (%s): %s" integration-test-phase message-text)
  (kill-emacs 1))

(defun run-empty-content-save-test ()
  (let ((test-port (getenv "SKG_TEST_PORT")))
    (when test-port
      (setq skg-port (string-to-number test-port))))

  ;; Ensure we start from a clean state
  (setq integration-test-phase "open-empty-buffer")
  (skg-open-empty-content-view)

  (let ((content-buffer (get-buffer skg-content-view-buffer-name)))
    (unless content-buffer
      (fail-test "skg content buffer was not created")))

  (with-current-buffer skg-content-view-buffer-name
    (erase-buffer)
    (insert "* (skg (node (id 1) (source main))) 1\n")
    (goto-char (point-min))
    (setq integration-test-phase "save-buffer")
    (skg-request-save-buffer))

  ;; Wait for the file to be written by the server
  (let* ((skg-dir (expand-file-name "data/skg" default-directory))
         (target-file (expand-file-name "1.skg" skg-dir))
         (attempts 0))
    (while (and (< attempts 40)
                (not (file-exists-p target-file)))
      (sleep-for 0.25)
      (setq attempts (1+ attempts)))
    (unless (file-exists-p target-file)
      (fail-test "1.skg was not created")))

  ;; Allow a short moment for the file contents to settle
  (sleep-for 0.1)

  (setq integration-test-phase "verify-file")
  (let ((file-contents (with-temp-buffer
                         (insert-file-contents (expand-file-name "data/skg/1.skg" default-directory))
                         (buffer-string))))
    (unless (string-match-p "title:[[:space:]]*['\"]1['\"]" file-contents)
      (message "File contents:\n%s" file-contents)
      (fail-test "title field does not match expected value"))
    (unless (string-match-p "ids:[[:space:]]*\n-[[:space:]]*['\"]1['\"]" file-contents)
      (message "File contents:\n%s" file-contents)
      (fail-test "ids field does not list \"1\"")))

  (setq integration-test-phase "success")
  (message "✓ PASS: Empty buffer save produced 1.skg with expected content")
  (kill-emacs 0))

(run-empty-content-save-test)
