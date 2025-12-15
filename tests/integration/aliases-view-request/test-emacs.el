;;; Integration test for aliases-view request functionality

(load-file "../../../elisp/skg-init.el")

(defvar integration-test-phase "starting")
(defvar integration-test-completed nil)

(defconst skg-aliases-base-buffer
  "* (skg (id test-node)) Test Node
")

(defun strip-metadata-and-bodies (text)
  "Return TEXT with metadata and body content removed for comparison."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (let ((result ""))
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position)
                     (line-end-position))))
          (when (string-match "^\\(\\*+\\) \\((skg .*)\\) \\(.*\\)$" line)
            (setq result (concat result (match-string 1 line)
                                 " "
                                 (match-string 3 line)
                                 "\n"))))
        (forward-line 1))
      result)))

(defun skg-aliases--request-on-line (line-number)
  "Return full buffer text after requesting aliases view at LINE-NUMBER.
LINE-NUMBER is zero-based."
  ;; Load the node from server using single-root-content-view
  (message "Loading node from server...")
  (skg-request-single-root-content-view-from-id "test-node")
  (sleep-for 0.5)

  (let ((buffer (get-buffer "*skg-content-view*")))
    (unless buffer
      (error "Content buffer not created"))
    (with-current-buffer buffer
      ;; Request aliases view
      (goto-char (point-min))
      (forward-line line-number)
      (setq integration-test-phase
            (format "requesting-aliases-view-line-%d" line-number))
      (skg-request-aliases-view)
      (skg-request-save-buffer) ;; save to send request to server
      (sleep-for 0.5)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun skg-aliases--verify-view (line-number expected-full expected-stripped)
  "Run request at LINE-NUMBER and assert resulting text/stripped strings."
  (let* ((buffer-content (skg-aliases--request-on-line line-number))
         (stripped (strip-metadata-and-bodies buffer-content)))
    (message "Line %d buffer content: %s" line-number buffer-content)
    (unless (string= buffer-content expected-full)
      (message "✗ FAIL: Unexpected buffer content for line %d" line-number)
      (message "Expected: %s" expected-full)
      (message "Actual:   %s" buffer-content)
      (kill-emacs 1))
    (unless (string= stripped expected-stripped)
      (message "✗ FAIL: Unexpected stripped content for line %d" line-number)
      (message "Expected: %s" expected-stripped)
      (message "Actual:   %s" stripped)
      (kill-emacs 1))))

(defun run-aliases-view-test ()
  (message "=== SKG Aliases View Request Integration Test ===")
  (let ((test-port (getenv "SKG_TEST_PORT")))
    (when test-port
      (setq skg-port (string-to-number test-port))))

  (let ((expected-with-aliases
         (concat "* (skg (id test-node) (source main) (view (rels (containers 0)))) Test Node\n"
                 "** (skg (code (interp aliasCol)))\n"
                 "*** (skg (code (interp alias))) first alias\n"
                 "*** (skg (code (interp alias))) second alias\n"))
        (expected-stripped
         (concat "* Test Node\n"
                 "*** first alias\n"
                 "*** second alias\n")))

    (setq integration-test-phase "testing-line-0")
    (skg-aliases--verify-view 0 expected-with-aliases expected-stripped)

    (message "✓ PASS: Aliases view verified")
    (setq integration-test-completed t)
    (kill-emacs 0)))

(progn
  (run-at-time
   30 nil
   (lambda ()
     (message "TIMEOUT: Integration test timed out!")
     (message "Last phase: %s" integration-test-phase)
     (kill-emacs 1)))
  (run-aliases-view-test))
