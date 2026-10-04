;;; Integration test for mentionerward-view request functionality

(load-file "../../../elisp/skg-init.el")
(load-file "../test-wait.el")

(defvar integration-test-phase "starting")
(defvar integration-test-completed nil)

(defconst skg-mentionerward-base-buffer
  "* (skg (node (id 1) (repo main))) 1
** (skg (node (id 11))) 11
** (skg (node (id 12))) 12
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

(defun skg-mentionerward--request-on-line (line-number)
  "Return full buffer text after requesting mentionerward view at LINE-NUMBER.
LINE-NUMBER is zero-based."
  (let ((buffer (get-buffer-create "*skg-content-view*")))
    (with-current-buffer buffer
      (erase-buffer)
      (org-mode)
      (setq skg-view-uri (org-id-uuid))
      (insert skg-mentionerward-base-buffer)
      (goto-char (point-min))
      (forward-line line-number)
      (setq integration-test-phase
            (format "requesting-mentionerward-view-line-%d" line-number))
      (skg-show-paths-through-mentioners) ;; this also saves the buffer
      (skg-test-wait-for-response)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun skg-mentionerward--verify-view (line-number expected-full expected-stripped)
  "Run request at LINE-NUMBER and assert resulting text/stripped strings."
  (let* ((buffer-content (skg-mentionerward--request-on-line line-number))
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

(defun run-mentionerward-view-test ()
  (message "=== SKG Mentionerward View Request Integration Test ===")
  (let ((test-port (getenv "SKG_TEST_PORT")))
    (when test-port
      (setq skg-port (string-to-number test-port))))

  (let ((expected-line0
         (concat "* (skg (node (id 1) (repo main) (affectsParent na) (rels (contains (out 2))))) 1\n"
                 "** (skg (node (id 11) (repo main) (rels (contains (in 1 (ancestors 1))) (links_to (in 1 (substantive 0))) (birth contains)))) 11\n"
                 "** (skg (node (id 12) (repo main) (rels (contains (in 1 (ancestors 1))) (birth contains)))) 12\n"))
        (expected-line2
         (concat "* (skg (node (id 1) (repo main) (affectsParent na) (rels (contains (out 2))))) 1\n"
                 "** (skg (node (id 11) (repo main) (rels (contains (in 1 (ancestors 1))) (links_to (in 1 (substantive 0))) (birth contains)))) 11\n"
                 "** (skg (node (id 12) (repo main) (rels (contains (in 1 (ancestors 1))) (birth contains)))) 12\n"))
        (expected-changed
         (concat "* (skg (node (id 1) (repo main) (affectsParent na) (rels (contains (out 2))))) 1\n"
                 "** (skg (node (id 11) (repo main) (rels (contains (in 1 (ancestors 1))) (links_to (in 1 (substantive 0))) (birth contains)))) 11\n"
                 "*** (skg (node (id l-11) (repo main) (affectsParent false) writeProtected (rels (links_to (out 1 (ancestors 1))) (birth links_to)))) [[id:11][a link to 11]]\n"
                 "** (skg (node (id 12) (repo main) (rels (contains (in 1 (ancestors 1))) (birth contains)))) 12\n"))
        (expected-no-link (concat "* 1\n** 11\n** 12\n"))
        (expected-with-link (concat "* 1\n** 11\n*** [[id:11][a link to 11]]\n** 12\n")))

    (setq integration-test-phase "testing-line-0")
    (skg-mentionerward--verify-view 0 expected-line0 expected-no-link)

    (setq integration-test-phase "testing-line-1")
    (skg-mentionerward--verify-view 1 expected-changed expected-with-link)

    (setq integration-test-phase "testing-line-2")
    (skg-mentionerward--verify-view 2 expected-line2 expected-no-link)

    (message "✓ PASS: Mentionerward view scenarios verified")
    (setq integration-test-completed t)
    (kill-emacs 0)))

(progn
  (run-at-time
   30 nil
   (lambda ()
     (message "TIMEOUT: Integration test timed out!")
     (message "Last phase: %s" integration-test-phase)
     (kill-emacs 1)))
  (run-mentionerward-view-test))
