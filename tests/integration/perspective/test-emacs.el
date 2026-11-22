;;; Integration test: cycling a view's perspective
;;; ('skg-cycle-perspective'). See run-test.sh for the data.

(load-file "../../../elisp/skg-init.el")
(load-file "../test-wait.el")

(defun test-fail (message &rest args)
  "Report test failure and exit."
  (apply #'message (concat "✗ FAIL: " message) args)
  (kill-emacs 1))

(defun test--text ()
  (buffer-substring-no-properties (point-min) (point-max)))

(defun test--line-in-ms-place (gnid)
  "The start of the line showing GNID in M's place (marked
'(inPlaceOf M)') in the current buffer, or nil. M, drawn editable in
the forks folder of the mirror standing in its place, shows the other
mirrors there too, as forkers of M."
  (save-excursion
    (goto-char (point-min))
    (let ((found nil))
      (while (and (not found)
                  (search-forward (format "(gnid %s)" gnid) nil t))
        (when (string-match-p
               "(inPlaceOf M)"
               (buffer-substring-no-properties
                (line-beginning-position) (line-end-position)))
          (setq found (line-beginning-position))))
      found)))

(defun integration-test-perspective ()
  (skg-test-use-test-port)
  (skg-request-single-root-content-view-from-id "R")
  (let ((buf (skg-test-wait-for
              (lambda ()
                (seq-find (lambda (b)
                            (with-current-buffer b
                              (and (boundp 'skg-view-id) skg-view-id
                                   (test--line-in-ms-place "A"))))
                          (buffer-list)))
              10)))
    (unless buf (test-fail "the view of R, showing A, did not open"))
    (switch-to-buffer buf)
    (with-current-buffer buf
      (when skg-perspective
        (test-fail "a fresh view is not drawn from its root homes"))
      ;; Point on A's headline; the switch must leave it on U's, which
      ;; stands in the same place.
      (goto-char (test--line-in-ms-place "A"))
      (skg-cycle-perspective)
      (unless (skg-test-wait-for
               (lambda () (and (equal skg-perspective "skgrepo main")
                               (test--line-in-ms-place "U")
                               (not skg--stream-in-progress)))
               10)
        (test-fail "switched, the view does not show U:\n%s" (test--text)))
      (when (test--line-in-ms-place "A")
        (test-fail "switched, the view still shows A:\n%s" (test--text)))
      (unless (equal (line-beginning-position) (test--line-in-ms-place "U"))
        (test-fail "point left the headline M's mirror stands on"))
      (unless (equal (eval (cadr mode-line-process) t) ":the perspective of main")
        (test-fail "the mode line does not say the view shows main's perspective"))
      (message "✓ switched to main's perspective; U stands in M's place")
      ;; Unsaved edits refuse the switch.
      (goto-char (test--line-in-ms-place "U"))
      (forward-line 1)
      (insert "a body line for U\n")
      (forward-line -1)
      (unless (condition-case nil
                  (progn (skg-cycle-perspective) nil)
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
      (unless (and (test--line-in-ms-place "U") (not (test--line-in-ms-place "A"))
                   (equal skg-perspective "skgrepo main"))
        (test-fail "after the save the view no longer shows main's perspective:\n%s"
                   (test--text)))
      (message "✓ a save keeps the choice")
      ;; The next perspective is third's, which shows B.
      (skg-cycle-perspective)
      (unless (skg-test-wait-for
               (lambda () (and (equal skg-perspective "skgrepo third")
                               (test--line-in-ms-place "B")
                               (not skg--stream-in-progress)))
               10)
        (test-fail "the next perspective does not show B:\n%s" (test--text)))
      (unless (equal (eval (cadr mode-line-process) t) ":the perspective of third")
        (test-fail "the mode line does not name third's perspective"))
      (message "✓ switched to third's perspective; B stands in M's place")
      ;; The next is origin's, M's own home, which shows M itself.
      (skg-cycle-perspective)
      (unless (skg-test-wait-for
               (lambda () (and (equal skg-perspective "skgrepo origin")
                               (not (test--line-in-ms-place "A"))
                               (not (test--line-in-ms-place "B"))
                               (string-match-p "^\\*\\* (skg [^\n]*(gnid M)" (test--text))
                               (not skg--stream-in-progress)))
               10)
        (test-fail "the next perspective does not show M itself:\n%s" (test--text)))
      (message "✓ switched to origin's perspective; M stands as itself")
      ;; After the last, the root homes again, showing A.
      (skg-cycle-perspective)
      (unless (skg-test-wait-for
               (lambda () (and (null skg-perspective)
                               (test--line-in-ms-place "A")
                               (not skg--stream-in-progress)))
               10)
        (test-fail "switched back, the view does not show A:\n%s" (test--text)))
      (when (eval (cadr mode-line-process) t)
        (test-fail "the root homes are marked in the mode line"))
      (message "✓ switched back to the root homes")))
  (integration-test-the-root-too)
  (message "PASS: Integration test successful!")
  (kill-emacs 0))

;; The root too: a view of M, which A (foreign's), U (main's) and B
;; (third's) each fork, shows M as its root from the root homes (M's
;; home is origin) and each fork from its skgrepo's perspective, in
;; declaration order, re-pointed where the root stands, so point stays
;; on it; the buffer keeps its name.
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
        (dolist (step '(("skgrepo foreign" "A" ":the perspective of foreign")
                        ("skgrepo main" "U" ":the perspective of main")
                        ("skgrepo third" "B" ":the perspective of third")
                        (nil "M" nil)))
          (goto-char (point-min))
          (end-of-line)
          (let ((followed (nth 0 step)) (shown (nth 1 step)))
            (skg-cycle-perspective)
            (unless (skg-test-wait-for
                     (lambda () (and (equal skg-perspective followed)
                                     (string-match-p (format "(gnid %s)" shown)
                                                     (test--root-line))
                                     (not skg--stream-in-progress)))
                     10)
              (test-fail "the root is not %s from %s:\n%s" shown followed (test--text)))
            (unless (eq (equal shown "M")
                        (not (string-match-p "(inPlaceOf M)" (test--root-line))))
              (test-fail "the root %s does not stand for M as it should:\n%s"
                         shown (test--text)))
            (unless (equal (line-number-at-pos) 1)
              (test-fail "point left the root when it became %s" shown))
            (unless (equal (eval (cadr mode-line-process) t) (nth 2 step))
              (test-fail "the mode line is wrong for %s" followed))
            (unless (equal (buffer-name) name)
              (test-fail "the buffer was renamed: %s" (buffer-name)))
            (message "✓ the root is %s" shown)))))))

(skg-test-arm-timeout 60)

(integration-test-perspective)
