;;; Integration test: a member of a forks folder that restates its
;;; note, in Emacs (run-test.sh gives the data: F forks G, both titled
;;; "Same words", with one body).
;;; 1. F's view: G's title is covered by '<b's title, unchanged>' and
;;;    its body by '<b's body, unchanged>', in the message face; the
;;;    buffer holds G's real title and body.
;;; 2. C-c v u uncovers them; again, covers them.
;;; 3. The real title, uncovered, edited to "G retitled", and the view
;;;    saved: G's file has the new title and its own body; redrawn, G's
;;;    title is no longer covered, its body still is.

(load-file "../../../elisp/skg-init.el")
(load-file "../test-wait.el")

(defun test-fail (message &rest args)
  "Report test failure and exit."
  (apply #'message (concat "✗ FAIL: " message) args)
  (message "Buffer:\n%s" (buffer-substring-no-properties (point-min) (point-max)))
  (kill-emacs 1))

(defun restated-goto-line-showing (gnid)
  "Move point to the start of the line showing GNID."
  (goto-char (point-min))
  (unless (re-search-forward
           (concat "^\\*+ (skg .*" (regexp-quote (format "(gnid %s)" gnid)))
           nil t)
    (test-fail "no line showing %s" gnid))
  (beginning-of-line))

(defun restated-covers ()
  "Each cover in the buffer, in order, as (COVERED-TEXT . SHOWN)."
  (mapcar (lambda (ov)
            (cons (buffer-substring-no-properties (overlay-start ov) (overlay-end ov))
                  (substring-no-properties (overlay-get ov 'display))))
          (sort (seq-filter (lambda (ov) (overlay-get ov 'skg-fork-dedup))
                            (overlays-in (point-min) (point-max)))
                (lambda (a b) (< (overlay-start a) (overlay-start b))))))

(defun restated-file (name)
  (with-temp-buffer
    (insert-file-contents (expand-file-name (concat "data/main/" name)
                                            default-directory))
    (buffer-string)))

(defconst restated-body "A long body\nthat its forks restate.")

(defun run-test ()
  (let ((test-port (getenv "SKG_TEST_PORT")))
    (when test-port (setq skg-port (string-to-number test-port))))
  (message "=== PHASE 1: F's view ===")
  (skg-request-single-root-content-view-from-id "F")
  (let ((buffer (skg-test-wait-for-buffer "*Same words*" 15)))
    (unless buffer (test-fail "no buffer for F"))
    (skg-test-wait-for-response 15)
    (with-current-buffer buffer
      (message "F's view:\n%s" (buffer-string))
      (let ((covers (restated-covers)))
        (unless (equal covers
                       `(("Same words" . "<b's title, unchanged>")
                         (,restated-body . "<b's body, unchanged>")))
          (test-fail "G is not covered as expected: %S" covers)))
      (restated-goto-line-showing "G")
      (unless (string-suffix-p " Same words"
                               (buffer-substring-no-properties
                                (point) (line-end-position)))
        (test-fail "the buffer lost G's real title"))
      (message "=== PHASE 2: toggle ===")
      (skg-toggle-fork-dedup-overlay)
      (when (restated-covers) (test-fail "C-c v u left covers"))
      (skg-toggle-fork-dedup-overlay)
      (unless (= 2 (length (restated-covers))) (test-fail "C-c v u did not restore the covers"))
      (message "=== PHASE 3: retitle G, save ===")
      (restated-goto-line-showing "G")
      (end-of-line)
      (search-backward "Same words")
      (replace-match "G retitled" t t)
      (skg-request-save-buffer)
      (unless (skg-test-wait-for-response 20)
        (test-fail "the save got no response"))
      (message "F's view after the save:\n%s" (buffer-string))
      (let ((covers (restated-covers)))
        (unless (equal covers `((,restated-body . "<b's body, unchanged>")))
          (test-fail "after the save, G is not covered as expected: %S" covers)))))
  (let ((g (restated-file "G.skg"))
        (f (restated-file "F.skg")))
    (unless (string-match-p "title: G retitled" g)
      (test-fail "G's file lacks its new title:\n%s" g))
    (unless (string-match-p "A long body\n *that its forks restate\\." g)
      (test-fail "G's file lost its body:\n%s" g))
    (when (string-match-p "unchanged>" (concat f g))
      (test-fail "a cover's text reached a file:\n%s\n%s" f g))
    (unless (string-match-p "title: Same words" f)
      (test-fail "F's title changed:\n%s" f)))
  (message "✓ PASS: Integration test successful!")
  (kill-emacs 0))

(progn
  (run-at-time 40 nil (lambda ()
                        (message "TIMEOUT: integration test timed out")
                        (kill-emacs 1)))
  (run-test))
