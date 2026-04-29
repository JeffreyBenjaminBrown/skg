;;; Integration test: literal search of titles with Tantivy operator chars.

(load-file "../../../elisp/skg-init.el")
(load-file "../../../elisp/skg-test-utils.el")
(load-file "../test-wait.el")

(defvar integration-test-phase "starting")

(defun test-one-escape-query (query expected-id)
  "Run QUERY and verify the search buffer contains (id EXPECTED-ID)."
  (message "=== Searching literal: %s ===" query)
  (setq integration-test-phase (format "searching %s" query))
  (skg--request-text-search query nil nil nil)
  (skg-test-wait-for-buffer (skg-search-buffer-name query))
  (let ((search-buffer (get-buffer (skg-search-buffer-name query))))
    (if search-buffer
        (with-current-buffer search-buffer
          (let* ((content (buffer-substring-no-properties
                           (point-min) (point-max)))
                 (needle (format "(id %s)" expected-id)))
            (if (string-match-p (regexp-quote needle) content)
                (message "✓ PASS: found %s for query %S" needle query)
              (progn
                (message "✗ FAIL: expected %s not found for query %S"
                         needle query)
                (message "Content: %s" content)
                (kill-emacs 1)))))
      (progn
        (message "✗ FAIL: no search buffer for query %S" query)
        (kill-emacs 1)))))

(defun integration-test-escape-chars ()
  "Run each of the three escape-character literal searches."
  (message "=== SKG Search Escape-Chars Integration Test ===")
  (let ((test-port (getenv "SKG_TEST_PORT")))
    (when test-port
      (setq skg-port (string-to-number test-port))
      (message "Using test port: %d" skg-port)))
  (test-one-escape-query "C++ tips"     "c_plus")
  (test-one-escape-query "[draft] plan" "brackets")
  (test-one-escape-query "cat:dog"      "colons")
  (message "✓ PASS: all escape-char searches succeeded")
  (kill-emacs 0))

(progn
  (run-at-time
   20 nil
   (lambda ()
     (message "TIMEOUT: integration test timed out at phase: %s"
              integration-test-phase)
     (kill-emacs 1)))
  (integration-test-escape-chars))
