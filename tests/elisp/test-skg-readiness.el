;;; test-skg-readiness.el --- Tests for the commit-readiness gestures and the readiness summary.  -*- lexical-binding: t; -*-

(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             (file-name-directory load-file-name)))
(require 'cl-lib)
(require 'ert)
(require 'org)
(require 'skg-readiness)

(defun test-skg-readiness--run (lines line-number command &optional prefix axis)
  "Insert LINES, run COMMAND (with PREFIX) on line LINE-NUMBER, and
return the buffer's lines. The save is stubbed; it must be called
once. AXIS answers the prompt for a member's axis; without it, the
prompt must not be shown. A rewritten headline without a title ends
in a space, as 'skg-format-headline' writes it."
  (with-temp-buffer
    (org-mode)
    (insert (mapconcat #'identity lines "\n") "\n")
    (goto-char (point-min))
    (forward-line (1- line-number))
    (let ((saves 0))
      (cl-letf (((symbol-function 'skg-request-save-buffer)
                 (lambda (&rest _) (setq saves (1+ saves))))
                ((symbol-function 'completing-read)
                 (lambda (&rest _)
                   (should axis)
                   axis)))
        (funcall command prefix))
      (should (= saves 1)))
    (split-string (buffer-string) "\n" t)))

(ert-deftest test-skg-cycle-commit-ready-toggles-a-leaf ()
  (should (equal (test-skg-readiness--run
                  '("** (skg removedAlias (diff rel-)) a1")
                  1 #'skg-cycle-commit-ready)
                 '("** (skg removedAlias (diff rel-) (readyRequest)) a1")))
  (should (equal (test-skg-readiness--run
                  '("** (skg (oldText title) (ready)) txt")
                  1 #'skg-cycle-commit-ready)
                 '("** (skg (oldText title) (ready) (unreadyRequest)) txt"))))

(ert-deftest test-skg-cycle-commit-ready-asks-which-axis-of-a-member ()
  (should (equal (test-skg-readiness--run
                  '("* (skg (node (id k) (repo r) (diff node+ rel+) (ready rel))) k")
                  1 #'skg-cycle-commit-ready nil "rel")
                 '("* (skg (node (id k) (repo r) (diff node+ rel+) (ready rel) (unreadyRequest rel))) k")))
  ;; With one axis, nothing is asked.
  (should (equal (test-skg-readiness--run
                  '("* (skg (historical (id d) (repo r) (diff node-))) d")
                  1 #'skg-cycle-commit-ready)
                 '("* (skg (historical (id d) (repo r) (diff node-) (readyRequest node))) d"))))

(ert-deftest test-skg-cycle-commit-ready-on-an-aggregate-writes-one-level-down ()
  ;; A changed folder: a request on each leaf just below it.
  (should (equal (test-skg-readiness--run
                  '("* (skg (node (id ali) (repo r))) ali"
                    "** (skg (aliasFolder changed))"
                    "*** (skg removedAlias (diff rel-) (ready)) a1"
                    "*** (skg alias) a2"
                    "*** (skg alias (diff rel+)) a3")
                  2 #'skg-cycle-commit-ready)
                 '("* (skg (node (id ali) (repo r))) ali"
                   "** (skg (aliasFolder changed))"
                   "*** (skg removedAlias (diff rel-) (ready) (readyRequest)) a1"
                   "*** (skg alias) a2"
                   "*** (skg alias (diff rel+) (readyRequest)) a3")))
  ;; A changed node: one request naming every leaf it owns.
  (should (equal (test-skg-readiness--run
                  '("* (skg (node (id txt) (repo r))) txt"
                    "** (skg (oldText title)) txt")
                  1 #'skg-cycle-commit-ready)
                 '("* (skg (node (id txt) (repo r) (readyRequest all))) txt"
                   "** (skg (oldText title)) txt"))))

(ert-deftest test-skg-cycle-commit-ready-recursively-touches-only-leaves ()
  (should (equal (test-skg-readiness--run
                  '("* (skg (node (id r) (repo r))) r"
                    "** (skg (changesetMarker content))"
                    "** (skg (historical (id d) (repo r) (diff node- rel-))) d"
                    "*** (skg (node (id k) (repo r) writeProtected)) k"
                    "** (skg (node (id c) (repo r) writeProtected (diff rel+))) c"
                    "* (skg (node (id other) (repo r))) other"
                    "** (skg (oldText title)) other")
                  1 #'skg-cycle-commit-ready '(4))
                 '("* (skg (node (id r) (repo r) (readyRequest all))) r"
                   "** (skg (changesetMarker content) (readyRequest)) "
                   "** (skg (historical (id d) (repo r) (diff node- rel-) (readyRequest node rel))) d"
                   "*** (skg (node (id k) (repo r) writeProtected)) k"
                   "** (skg (node (id c) (repo r) writeProtected (diff rel+) (readyRequest rel))) c"
                   "* (skg (node (id other) (repo r))) other"
                   "** (skg (oldText title)) other"))))

(ert-deftest test-skg-mark-gnode-skgrepo-changes-commitReady ()
  (let ((lines '("* (skg (node (id a) (repo b))) a"
                 "** (skg (homeMove (old x) (new b)))"
                 "** (skg (node (id c) (repo b) writeProtected)) c"
                 "*** (skg (homeMove (old x) (new b)))"
                 "** (skg (node (id e) (repo b))) e"
                 "*** (skg (homeMove (old y) (new b)))")))
    (should (equal (test-skg-readiness--run
                    lines 1 #'skg-mark-gnode-skgrepo-changes-commitReady)
                   '("* (skg (node (id a) (repo b))) a"
                     "** (skg (homeMove (old x) (new b)) (readyRequest)) "
                     "** (skg (node (id c) (repo b) writeProtected)) c"
                     "*** (skg (homeMove (old x) (new b)))"
                     "** (skg (node (id e) (repo b))) e"
                     "*** (skg (homeMove (old y) (new b)))")))
    ;; Recursively: only the moves between the same two skgrepos.
    (should (equal (test-skg-readiness--run
                    lines 1 #'skg-mark-gnode-skgrepo-changes-commitReady '(4))
                   '("* (skg (node (id a) (repo b))) a"
                     "** (skg (homeMove (old x) (new b)) (readyRequest)) "
                     "** (skg (node (id c) (repo b) writeProtected)) c"
                     "*** (skg (homeMove (old x) (new b)) (readyRequest)) "
                     "** (skg (node (id e) (repo b))) e"
                     "*** (skg (homeMove (old y) (new b)))")))))

(ert-deftest test-skg-readiness-summary-is-shown-after-a-save ()
  (let ((shown '()))
    (cl-letf (((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) shown))))
      (with-temp-buffer
        (skg-handle-save-sexp
         (concat "((content \"* (skg (node (id a))) a\n\") (errors ()) (warnings ())"
                 " (readiness-summary (ready 12) (changed 14) (foreign 2)"
                 " (order-only 1) (unshown 3)))")))
      (should (equal (car shown)
                     "12 of 14 changed files are ready, 3 changes are not shown in this view; reopen it, 2 files changed in foreign skgrepos"))
      (with-temp-buffer
        (skg-handle-save-sexp
         (concat "((content \"* (skg (node (id a))) a\n\") (errors ()) (warnings ())"
                 " (readiness-summary (ready 14) (changed 14) (foreign 0)"
                 " (order-only 0) (unshown 0)))")))
      (should (equal (car shown)
                     "COMMIT READY. All 14 of 14 changed files are ready."))
      (setq shown '())
      (with-temp-buffer
        (skg-handle-save-sexp
         "((content \"* (skg (node (id a))) a\n\") (errors ()) (warnings ()))"))
      (should-not (seq-some (lambda (line) (string-match-p "changed files" line))
                            shown)))))
