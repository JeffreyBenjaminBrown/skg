;;; Integration test: cycling a view's perspective, whose overrides it
;;; follows ('skg-override-perspective'). See run-test.sh for the data.

(load-file "../../../elisp/skg-init.el")
(load-file "../test-wait.el")

(defun test-fail (message &rest args)
  "Report test failure and exit."
  (apply #'message (concat "✗ FAIL: " message) args)
  (kill-emacs 1))

(defun test--text ()
  (buffer-substring-no-properties (point-min) (point-max)))

(defun test--line-of-gnid (gnid)
  "The start of the line showing GNID in the current buffer, or nil."
  (save-excursion
    (goto-char (point-min))
    (when (search-forward (format "(gnid %s)" gnid) nil t)
      (line-beginning-position))))

(defun integration-test-override-perspective ()
  (let ((test-port (getenv "SKG_TEST_PORT")))
    (when test-port (setq skg-port (string-to-number test-port))))
  (skg-request-single-root-content-view-from-id "R")
  (let ((buf (skg-test-wait-for
              (lambda ()
                (seq-find (lambda (b)
                            (with-current-buffer b
                              (and (boundp 'skg-view-id) skg-view-id
                                   (test--line-of-gnid "A"))))
                          (buffer-list)))
              10)))
    (unless buf (test-fail "the view of R, showing A, did not open"))
    (switch-to-buffer buf)
    (with-current-buffer buf
      (when skg-overrides-followed
        (test-fail "a fresh view follows its root authors' overrides"))
      ;; Point on A's headline; the switch must leave it on U's, which
      ;; stands in the same place.
      (goto-char (test--line-of-gnid "A"))
      (skg-override-perspective)
      (unless (skg-test-wait-for
               (lambda () (and (equal skg-overrides-followed "user")
                               (test--line-of-gnid "U")
                               (not skg--stream-in-progress)))
               10)
        (test-fail "switched, the view does not show U:\n%s" (test--text)))
      (when (test--line-of-gnid "A")
        (test-fail "switched, the view still shows A:\n%s" (test--text)))
      (unless (equal (line-beginning-position) (test--line-of-gnid "U"))
        (test-fail "point left the headline M's overrider stands on"))
      (unless (equal (eval (cadr mode-line-process) t) ":your overrides")
        (test-fail "the mode line does not say the view follows your overrides"))
      (message "✓ switched to your overrides; U stands in M's place")
      ;; Unsaved edits refuse the switch.
      (goto-char (test--line-of-gnid "U"))
      (forward-line 1)
      (insert "a body line for U\n")
      (forward-line -1)
      (unless (condition-case nil
                  (progn (skg-override-perspective) nil)
                (user-error t))
        (test-fail "a buffer with unsaved edits was switched"))
      (message "✓ refused while dirty")
      ;; A save keeps the choice: it names it, and its redraw follows it.
      (skg-request-save-buffer)
      (unless (skg-test-wait-for
               (lambda () (and (not (buffer-modified-p))
                               (string-match-p "a body line for U" (test--text))
                               (not skg--stream-in-progress)))
               20)
        (test-fail "the save did not finish:\n%s" (test--text)))
      (unless (and (test--line-of-gnid "U") (not (test--line-of-gnid "A"))
                   (equal skg-overrides-followed "user"))
        (test-fail "after the save the view no longer follows your overrides:\n%s"
                   (test--text)))
      (message "✓ a save keeps the choice")
      ;; The next perspective is third's, which shows B.
      (skg-override-perspective)
      (unless (skg-test-wait-for
               (lambda () (and (equal skg-overrides-followed "owner of third")
                               (test--line-of-gnid "B")
                               (not skg--stream-in-progress)))
               10)
        (test-fail "the next perspective does not show B:\n%s" (test--text)))
      (unless (equal (eval (cadr mode-line-process) t) ":third's overrides")
        (test-fail "the mode line does not name third's overrides"))
      (message "✓ switched to third's overrides; B stands in M's place")
      ;; After the last, the root authors' again, showing A.
      (skg-override-perspective)
      (unless (skg-test-wait-for
               (lambda () (and (null skg-overrides-followed)
                               (test--line-of-gnid "A")
                               (not skg--stream-in-progress)))
               10)
        (test-fail "switched back, the view does not show A:\n%s" (test--text)))
      (when (eval (cadr mode-line-process) t)
        (test-fail "the root authors' perspective is marked in the mode line"))
      (message "✓ switched back to the root authors' overrides")))
  (integration-test-the-root-too)
  (message "PASS: Integration test successful!")
  (kill-emacs 0))

;; The root too: a view of M, which A (M's own author's), U (yours)
;; and B (third's) each override, shows M as its root from the root
;; authors' perspective and each override from its author's, re-pointed
;; where the root stands, so point stays on it; the buffer keeps its name.
(defun test--root-line ()
  (save-excursion
    (goto-char (point-min))
    (buffer-substring-no-properties (line-beginning-position) (line-end-position))))

(defun integration-test-the-root-too ()
  (skg-request-single-root-content-view-from-id "M")
  (let ((buf (skg-test-wait-for
              (lambda ()
                (seq-find (lambda (b)
                            (with-current-buffer b
                              (and (boundp 'skg-view-id) skg-view-id
                                   (string-match-p "(gnid M)" (test--root-line)))))
                          (buffer-list)))
              10)))
    (unless buf (test-fail "the view of M did not open"))
    (switch-to-buffer buf)
    (with-current-buffer buf
      (let ((name (buffer-name)))
        (dolist (step '(("user" "U" ":your overrides")
                        ("owner of foreign" "A" ":foreign's overrides")
                        ("owner of third" "B" ":third's overrides")
                        (nil "M" nil)))
          (goto-char (point-min))
          (end-of-line)
          (let ((followed (nth 0 step)) (shown (nth 1 step)))
            (skg-override-perspective)
            (unless (skg-test-wait-for
                     (lambda () (and (equal skg-overrides-followed followed)
                                     (string-match-p (format "(gnid %s)" shown)
                                                     (test--root-line))
                                     (not skg--stream-in-progress)))
                     10)
              (test-fail "the root is not %s from %s:\n%s" shown followed (test--text)))
            (unless (eq (equal shown "M")
                        (not (string-match-p "(overridesHere M)" (test--root-line))))
              (test-fail "the root %s does not stand for M as it should:\n%s"
                         shown (test--text)))
            (unless (equal (line-number-at-pos) 1)
              (test-fail "point left the root when it became %s" shown))
            (unless (equal (eval (cadr mode-line-process) t) (nth 2 step))
              (test-fail "the mode line is wrong for %s" followed))
            (unless (equal (buffer-name) name)
              (test-fail "the buffer was renamed: %s" (buffer-name)))
            (message "✓ the root is %s" shown)))))))

(run-at-time 60 nil (lambda ()
                      (message "TIMEOUT: Integration test timed out!")
                      (kill-emacs 1)))

(integration-test-override-perspective)
