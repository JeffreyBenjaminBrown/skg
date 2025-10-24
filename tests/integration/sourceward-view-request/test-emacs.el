;;; Integration test for sourceward-view request functionality

(load-file "../../../elisp/skg-init.el")

(defvar integration-test-phase "starting")
(defvar integration-test-completed nil)

(defconst skg-sourceward-base-buffer
  "* (skg (id 1)) 1
** (skg (id 11)) 11
** (skg (id 12)) 12
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

(defun skg-sourceward--request-on-line (line-number)
  "Return full buffer text after requesting sourceward view at LINE-NUMBER.
LINE-NUMBER is zero-based."
  (let ((buffer (get-buffer-create "*skg-content-view*")))
    (with-current-buffer buffer
      (erase-buffer)
      (org-mode)
      (insert skg-sourceward-base-buffer)
      (goto-char (point-min))
      (forward-line line-number)
      (setq integration-test-phase
            (format "requesting-sourceward-view-line-%d" line-number))
      (skg-request-sourceward-view)
      (sleep-for 0.25)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun skg-sourceward--verify-view (line-number expected-full expected-stripped)
  "Run request at LINE-NUMBER and assert resulting text/stripped strings."
  (let* ((buffer-content (skg-sourceward--request-on-line line-number))
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

(defun run-sourceward-view-test ()
  (message "=== SKG Sourceward View Request Integration Test ===")
  (let ((test-port (getenv "SKG_TEST_PORT")))
    (when test-port
      (setq skg-port (string-to-number test-port))))

  (let ((expected-line0
         (concat "* (skg (id 1) (view (rels (containers 0) (contents 2)))) 1\n"
                 "** (skg (id 11) (view (rels (linksIn 1)))) 11\n"
                 "** (skg (id 12)) 12\n"))
        (expected-line2
         (concat "* (skg (id 1) (view (rels (containers 0) (contents 2)))) 1\n"
                 "** (skg (id 11) (view (rels (linksIn 1)))) 11\n"
                 "** (skg (id 12) (view)) 12\n"))
        (expected-changed
         (concat "* (skg (id 1) (view (rels (containers 0) (contents 2)))) 1\n"
                 "** (skg (id 11) (view (rels (linksIn 1)))) 11\n"
                 "*** (skg (id l-11) (view (rels notInParent (containers 0))) (code (relToParent parentIgnores) indefinitive)) [[id:11][a link to 11]]\n"
                 "** (skg (id 12)) 12\n"))
        (expected-no-link (concat "* 1\n** 11\n** 12\n"))
        (expected-with-link (concat "* 1\n** 11\n*** [[id:11][a link to 11]]\n** 12\n")))

    (setq integration-test-phase "testing-line-0")
    (skg-sourceward--verify-view 0 expected-line0 expected-no-link)

    (setq integration-test-phase "testing-line-1")
    (skg-sourceward--verify-view 1 expected-changed expected-with-link)

    (setq integration-test-phase "testing-line-2")
    (skg-sourceward--verify-view 2 expected-line2 expected-no-link)

    (message "✓ PASS: Sourceward view scenarios verified")
    (setq integration-test-completed t)
    (kill-emacs 0)))

(progn
  (run-at-time
   30 nil
   (lambda ()
     (message "TIMEOUT: Integration test timed out!")
     (message "Last phase: %s" integration-test-phase)
     (kill-emacs 1)))
  (run-sourceward-view-test))
