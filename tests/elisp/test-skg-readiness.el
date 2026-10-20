;;; test-skg-readiness.el --- Tests for the commit-readiness gestures and the readiness summary.  -*- lexical-binding: t; -*-

(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             (file-name-directory load-file-name)))
(require 'cl-lib)
(require 'ert)
(require 'org)
(require 'skg-readiness)
(require 'skg-request-save) ; for skg-handle-save-sexp

(defun test-skg-readiness--run (lines line-number command
                                       &optional prefix object answer)
  "Insert LINES, run COMMAND (with PREFIX) on line LINE-NUMBER, and
return the buffer's lines. The save is stubbed; it must be called
once, or never when ANSWER is 'no. OBJECT answers the prompt for a
member's object, and ANSWER ('yes or 'no) the question before a wide
unmark; without them, those must not be asked. A rewritten headline
without a title ends in a space, as 'skg-format-headline' writes it."
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
                   (should object)
                   object))
                ((symbol-function 'yes-or-no-p)
                 (lambda (prompt)
                   (should answer)
                   (should (string-prefix-p "Unmarking this node commit-ready will unmark every contributing decision."
                                            prompt))
                   (eq answer 'yes))))
        (funcall command prefix))
      (should (= saves (if (eq answer 'no) 0 1))))
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

(ert-deftest test-skg-cycle-commit-ready-asks-which-object-of-a-member ()
  (should (equal (test-skg-readiness--run
                  '("* (skg (node (id k) (repo r) (diff node+ rel+) (ready rel))) k")
                  1 #'skg-cycle-commit-ready nil "rel")
                 '("* (skg (node (id k) (repo r) (diff node+ rel+) (ready rel) (unreadyRequest rel))) k")))
  ;; With one object, nothing is asked.
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

(ert-deftest test-skg-toggle-homeSkgrepo-move-commitReady ()
  (let ((lines '("* (skg (node (id a) (repo b))) a"
                 "** (skg (homeSkgrepoMove (old x) (new b)))"
                 "** (skg (node (id c) (repo b) writeProtected)) c"
                 "*** (skg (homeSkgrepoMove (old x) (new b)))"
                 "** (skg (node (id e) (repo b))) e"
                 "*** (skg (homeSkgrepoMove (old y) (new b)))")))
    (should (equal (test-skg-readiness--run
                    lines 1 #'skg-toggle-homeSkgrepo-move-commitReady)
                   '("* (skg (node (id a) (repo b))) a"
                     "** (skg (homeSkgrepoMove (old x) (new b)) (readyRequest)) "
                     "** (skg (node (id c) (repo b) writeProtected)) c"
                     "*** (skg (homeSkgrepoMove (old x) (new b)))"
                     "** (skg (node (id e) (repo b))) e"
                     "*** (skg (homeSkgrepoMove (old y) (new b)))")))
    ;; Recursively: only the moves between the same two skgrepos.
    (should (equal (test-skg-readiness--run
                    lines 1 #'skg-toggle-homeSkgrepo-move-commitReady '(4))
                   '("* (skg (node (id a) (repo b))) a"
                     "** (skg (homeSkgrepoMove (old x) (new b)) (readyRequest)) "
                     "** (skg (node (id c) (repo b) writeProtected)) c"
                     "*** (skg (homeSkgrepoMove (old x) (new b)) (readyRequest)) "
                     "** (skg (node (id e) (repo b))) e"
                     "*** (skg (homeSkgrepoMove (old y) (new b)))")))))

(ert-deftest test-skg-readiness-a-single-leaf-unmark-asks-nothing ()
  ;; The folder's one leaf is its only unmark: nothing is asked.
  (should (equal (test-skg-readiness--run
                  '("** (skg (aliasFolder changed) (ready))"
                    "*** (skg removedAlias (diff rel-) (ready)) a1"
                    "*** (skg alias) a2")
                  1 #'skg-cycle-commit-ready)
                 '("** (skg (aliasFolder changed) (ready))"
                   "*** (skg removedAlias (diff rel-) (ready) (unreadyRequest)) a1"
                   "*** (skg alias) a2"))))

(ert-deftest test-skg-readiness-a-wide-unmark-asks-first ()
  (let ((lines '("* (skg (historical (id d) (repo r) (diff node- rel-) (ready node rel))) d"
                 "** (skg (oldText title) (ready)) d")))
    ;; Refused: nothing is written, nothing saved.
    (should (equal (test-skg-readiness--run
                    lines 1 #'skg-cycle-commit-ready '(4) nil 'no)
                   lines))
    (should (equal (test-skg-readiness--run
                    lines 1 #'skg-cycle-commit-ready '(4) nil 'yes)
                   '("* (skg (historical (id d) (repo r) (diff node- rel-) (ready node rel) (unreadyRequest node rel))) d"
                     "** (skg (oldText title) (ready) (unreadyRequest)) d"))))
  ;; Unmarking a changed node unmarks every leaf it owns.
  (should (equal (test-skg-readiness--run
                  '("* (skg (node (id txt) (repo r))) txt"
                    "** (skg (oldText title) (ready)) txt")
                  1 #'skg-cycle-commit-ready nil nil 'no)
                 '("* (skg (node (id txt) (repo r))) txt"
                   "** (skg (oldText title) (ready)) txt"))))

(ert-deftest test-skg-toggle-homeSkgrepo-move-commitReady-unmarks ()
  (let ((lines '("* (skg (node (id a) (repo b))) a"
                 "** (skg (homeSkgrepoMove (old x) (new b)) (ready))"
                 "** (skg (node (id c) (repo b) writeProtected)) c"
                 "*** (skg (homeSkgrepoMove (old x) (new b)) (ready))")))
    ;; One move, from the node or the move: nothing is asked.
    (dolist (line-number '(1 2))
      (should (equal (test-skg-readiness--run
                      lines line-number
                      #'skg-toggle-homeSkgrepo-move-commitReady)
                     '("* (skg (node (id a) (repo b))) a"
                       "** (skg (homeSkgrepoMove (old x) (new b)) (ready) (unreadyRequest)) "
                       "** (skg (node (id c) (repo b) writeProtected)) c"
                       "*** (skg (homeSkgrepoMove (old x) (new b)) (ready))"))))
    ;; Recursively, two moves: asked first.
    (should (equal (test-skg-readiness--run
                    lines 1 #'skg-toggle-homeSkgrepo-move-commitReady
                    '(4) nil 'yes)
                   '("* (skg (node (id a) (repo b))) a"
                     "** (skg (homeSkgrepoMove (old x) (new b)) (ready) (unreadyRequest)) "
                     "** (skg (node (id c) (repo b) writeProtected)) c"
                     "*** (skg (homeSkgrepoMove (old x) (new b)) (ready) (unreadyRequest)) ")))))

(ert-deftest test-skg-readiness-do-children-visits-exactly-one-level-below-point ()
  "The children of the headline at point, not its grandchildren, its
siblings, or a child without metadata."
  (with-temp-buffer
    (org-mode)
    (insert "* (skg (node (id before))) before\n"
            "* (skg (node (id parent))) parent\n"
            "body of parent\n"
            "** (skg (node (id child1))) child1\n"
            "*** (skg (node (id grandchild))) grandchild\n"
            "** no metadata\n"
            "** (skg (node (id child2))) child2\n"
            "* (skg (node (id after))) after\n"
            "** (skg (node (id nephew))) nephew\n")
    (goto-char (point-min))
    (forward-line 1)
    (let ((visited nil))
      (should (equal (skg--readiness-do-children
                      (lambda (metadata-sexp)
                        (push (skg--node-id metadata-sexp) visited)
                        (skg--node-id metadata-sexp)))
                     '("child1" "child2")))
      (should (equal (nreverse visited) '("child1" "child2"))))))

(ert-deftest test-skg-readiness-summary-is-shown-after-a-save ()
  (let ((shown '()))
    (cl-letf (((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) shown))))
      (with-temp-buffer
        (skg-handle-save-sexp
         (concat "((content \"* (skg (node (id a))) a\n\") (errors ()) (warnings ())"
                 " (readiness-summary (ready 12) (changed 14) (foreign 2)"
                 " (order-only 1) (notEditable 3)))")))
      (should (equal (car shown)
                     "12 of 14 changed files are ready, 3 changes are not editable in this view; C-c r u integrates them, 2 files changed in foreign skgrepos"))
      (with-temp-buffer
        (skg-handle-save-sexp
         (concat "((content \"* (skg (node (id a))) a\n\") (errors ()) (warnings ())"
                 " (readiness-summary (ready 14) (changed 14) (foreign 0)"
                 " (order-only 0) (notEditable 0)))")))
      (should (equal (car shown)
                     "COMMIT READY. All 14 of 14 changed files are ready."))
      (setq shown '())
      (with-temp-buffer
        (skg-handle-save-sexp
         "((content \"* (skg (node (id a))) a\n\") (errors ()) (warnings ()))"))
      (should-not (seq-some (lambda (line) (string-match-p "changed files" line))
                            shown)))))
