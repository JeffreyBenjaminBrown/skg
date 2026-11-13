;;; test-skg-processed.el --- Tests for the processed gestures and the processed summary.  -*- lexical-binding: t; -*-

(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             (file-name-directory load-file-name)))
(require 'cl-lib)
(require 'ert)
(require 'org)
(require 'skg-processed)
(require 'skg-request-save) ; for skg-handle-save-sexp
(require 'skg-keymaps-and-aliases)

(defun test-skg-processed--run (lines line-number command
                                       &optional prefix answer)
  "Insert LINES, run COMMAND (with PREFIX) on line LINE-NUMBER, and
return the buffer's lines. The save is stubbed; it must be called
once, or never when ANSWER is 'no. ANSWER ('yes or 'no) answers the
question before a wide unmark; without it, that must not be asked.
No gesture prompts for anything else. A rewritten headline
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
                   (ert-fail "a gesture asked which object")))
                ((symbol-function 'yes-or-no-p)
                 (lambda (prompt)
                   (should answer)
                   (should (string-prefix-p "Unmarking this node processed will unmark every contributing decision."
                                            prompt))
                   (eq answer 'yes))))
        (funcall command prefix))
      (should (= saves (if (eq answer 'no) 0 1))))
    (split-string (buffer-string) "\n" t)))

(ert-deftest test-skg-cycle-processed-toggles-a-leaf ()
  (should (equal (test-skg-processed--run
                  '("** (skg alias noClaim (view ghostWarrant) (diff (rel removed) (leaves 1))) a1")
                  1 #'skg-cycle-processed)
                 '("** (skg alias noClaim (editRequest (processed)) (view ghostWarrant) (diff (rel removed) (leaves 1))) a1")))
  (should (equal (test-skg-processed--run
                  '("** (skg (oldText title) (diff (leaves 1) (processed))) txt")
                  1 #'skg-cycle-processed)
                 '("** (skg (oldText title) (editRequest (unprocessed)) (diff (leaves 1) (processed))) txt")))
  ;; Without the server's '(diff (leaves ...))' fact, nothing is named.
  (with-temp-buffer
    (org-mode)
    (insert "** (skg (oldText title)) txt\n")
    (goto-char (point-min))
    (should-error (skg-cycle-processed) :type 'user-error)))

(ert-deftest test-skg-write-processed-request-places-it-in-the-editRequest ()
  "The request replaces any other processed request as the last part of
the headline's '(editRequest ...)', keeping the other request there; a
headline without the form gets one where the server writes it, before
its view requests and its derived facts."
  (cl-flet ((write (line processed objects)
              (with-temp-buffer
                (org-mode)
                (insert line "\n")
                (goto-char (point-min))
                (skg--write-processed-request processed objects)
                (car (split-string (buffer-string) "\n" t)))))
    (should (equal (write "* (skg node (gnid k) (skgRepos (nodeRepo r)) (editRequest (processed rel)) (diff (rel added) (leaves 1 rel))) k"
                          nil '(rel))
                   "* (skg node (gnid k) (skgRepos (nodeRepo r)) (editRequest (unprocessed rel)) (diff (rel added) (leaves 1 rel))) k"))
    (should (equal (write "* (skg node (gnid k) (skgRepos (nodeRepo r)) (editRequest (processed node) delete) (viewRequests editableView) (diff (node added) (leaves 1 node))) k"
                          nil '(node))
                   "* (skg node (gnid k) (skgRepos (nodeRepo r)) (editRequest delete (unprocessed node)) (viewRequests editableView) (diff (node added) (leaves 1 node))) k"))
    (should (equal (write "* (skg node (gnid k) (skgRepos (nodeRepo r)) (viewRequests editableView) (view cycle) (diff (node added) (leaves 1 node))) k"
                          t '(node))
                   "* (skg node (gnid k) (skgRepos (nodeRepo r)) (editRequest (processed node)) (viewRequests editableView) (view cycle) (diff (node added) (leaves 1 node))) k"))
    (should (equal (write "** (skg alias (diff (rel added) (leaves 1))) a"
                          t nil)
                   "** (skg alias (editRequest (processed)) (diff (rel added) (leaves 1))) a"))
    (should (equal (write "** (skg (homeSkgrepoMove (old x) (new b))) "
                          t nil)
                   "** (skg (homeSkgrepoMove (old x) (new b)) (editRequest (processed))) "))))

(ert-deftest test-skg-cycle-processed-names-every-listed-object ()
  ;; A new member lists two objects, one of them processed: the gesture
  ;; names both, without asking, and marks them.
  (should (equal (test-skg-processed--run
                  '("* (skg node (gnid k) (skgRepos (nodeRepo r)) (diff (node added) (rel added) (leaves 2 node rel) (processed rel))) k")
                  1 #'skg-cycle-processed)
                 '("* (skg node (gnid k) (skgRepos (nodeRepo r)) (editRequest (processed node rel)) (diff (node added) (rel added) (leaves 2 node rel) (processed rel))) k")))
  ;; A modified node in a new relationship: one gesture marks its file
  ;; and the relationship.
  (should (equal (test-skg-processed--run
                  '("* (skg node (gnid m) (skgRepos (nodeRepo r)) (diff (rel added) (properties changed) (leaves 3 rel all) (processed rel))) m")
                  1 #'skg-cycle-processed)
                 '("* (skg node (gnid m) (skgRepos (nodeRepo r)) (editRequest (processed rel all)) (diff (rel added) (properties changed) (leaves 3 rel all) (processed rel))) m")))
  ;; With one object, likewise.
  (should (equal (test-skg-processed--run
                  '("* (skg ghost (gnid d) (skgRepos (nodeRepo r)) (diff (node removed) (leaves 1 node))) d")
                  1 #'skg-cycle-processed)
                 '("* (skg ghost (gnid d) (skgRepos (nodeRepo r)) (editRequest (processed node)) (diff (node removed) (leaves 1 node))) d"))))

(ert-deftest test-skg-cycle-processed-a-modified-node-toggles-by-its-own-processed ()
  ;; '(processed all)': processed, so the gesture unmarks.
  (should (equal (test-skg-processed--run
                  '("* (skg node (gnid txt) (skgRepos (nodeRepo r)) (diff (properties changed) (leaves 1 all) (processed all))) txt"
                    "** (skg (oldText title) (diff (leaves 1) (processed))) txt")
                  1 #'skg-cycle-processed)
                 '("* (skg node (gnid txt) (skgRepos (nodeRepo r)) (editRequest (unprocessed all)) (diff (properties changed) (leaves 1 all) (processed all))) txt"
                   "** (skg (oldText title) (diff (leaves 1) (processed))) txt")))
  ;; Without it, unprocessed, whatever its children say: the gesture marks.
  (should (equal (test-skg-processed--run
                  '("* (skg node (gnid txt) (skgRepos (nodeRepo r)) (diff (properties changed) (leaves 1 all))) txt"
                    "** (skg (oldText title) (diff (leaves 1) (processed))) txt")
                  1 #'skg-cycle-processed)
                 '("* (skg node (gnid txt) (skgRepos (nodeRepo r)) (editRequest (processed all)) (diff (properties changed) (leaves 1 all))) txt"
                   "** (skg (oldText title) (diff (leaves 1) (processed))) txt")))
  ;; A '(processed rel)' does not make 'all' processed.
  (should (equal (test-skg-processed--run
                  '("* (skg node (gnid m) (skgRepos (nodeRepo r)) (diff (rel added) (properties changed) (leaves 2 rel all) (processed rel))) m")
                  1 #'skg-cycle-processed)
                 '("* (skg node (gnid m) (skgRepos (nodeRepo r)) (editRequest (processed rel all)) (diff (rel added) (properties changed) (leaves 2 rel all) (processed rel))) m"))))

(ert-deftest test-skg-cycle-processed-on-a-processed-aggregate-writes-one-level-down ()
  ;; A changed folder: a request on each leaf just below it.
  (should (equal (test-skg-processed--run
                  '("* (skg node (gnid ali) (skgRepos (nodeRepo r)) (diff (leaves 2 all))) ali"
                    "** (skg aliasFolder)"
                    "*** (skg alias noClaim (view ghostWarrant) (diff (rel removed) (leaves 1) (processed))) a1"
                    "*** (skg alias) a2"
                    "*** (skg alias (diff (rel added) (leaves 1))) a3")
                  2 #'skg-cycle-processed)
                 '("* (skg node (gnid ali) (skgRepos (nodeRepo r)) (diff (leaves 2 all))) ali"
                   "** (skg aliasFolder)"
                   "*** (skg alias noClaim (editRequest (processed)) (view ghostWarrant) (diff (rel removed) (leaves 1) (processed))) a1"
                   "*** (skg alias) a2"
                   "*** (skg alias (editRequest (processed)) (diff (rel added) (leaves 1))) a3")))
  ;; A changed node: one request naming every leaf it records.
  (should (equal (test-skg-processed--run
                  '("* (skg node (gnid txt) (skgRepos (nodeRepo r)) (diff (leaves 1 all))) txt"
                    "** (skg (oldText title) (diff (leaves 1))) txt")
                  1 #'skg-cycle-processed)
                 '("* (skg node (gnid txt) (skgRepos (nodeRepo r)) (editRequest (processed all)) (diff (leaves 1 all))) txt"
                   "** (skg (oldText title) (diff (leaves 1))) txt"))))

(ert-deftest test-skg-cycle-processed-recursively-touches-only-leaves ()
  ;; Below the headline at point, no 'all': the changeset marker's
  ;; leaves are its siblings', and so are untouched there, and of the
  ;; modified m only the relationship is named.
  (should (equal (test-skg-processed--run
                  '("* (skg node (gnid r) (skgRepos (nodeRepo r)) (diff (leaves 3 all))) r"
                    "** (skg (changesetMarker content) (diff (leaves 2 all)))"
                    "** (skg ghost (gnid d) (skgRepos (nodeRepo r)) (diff (node removed) (rel removed) (leaves 2 node rel))) d"
                    "*** (skg node (gnid k) (skgRepos (nodeRepo r)) writeProtected) k"
                    "** (skg node (gnid c) (skgRepos (nodeRepo r)) writeProtected (diff (rel added) (leaves 1 rel))) c"
                    "** (skg node (gnid m) (skgRepos (nodeRepo r)) writeProtected (diff (rel added) (properties changed) (leaves 2 rel all))) m"
                    "* (skg node (gnid other) (skgRepos (nodeRepo r)) (diff (leaves 1 all))) other"
                    "** (skg (oldText title) (diff (leaves 1))) other")
                  1 #'skg-cycle-processed '(4))
                 '("* (skg node (gnid r) (skgRepos (nodeRepo r)) (editRequest (processed all)) (diff (leaves 3 all))) r"
                   "** (skg (changesetMarker content) (diff (leaves 2 all)))"
                   "** (skg ghost (gnid d) (skgRepos (nodeRepo r)) (editRequest (processed node rel)) (diff (node removed) (rel removed) (leaves 2 node rel))) d"
                   "*** (skg node (gnid k) (skgRepos (nodeRepo r)) writeProtected) k"
                   "** (skg node (gnid c) (skgRepos (nodeRepo r)) writeProtected (editRequest (processed rel)) (diff (rel added) (leaves 1 rel))) c"
                   "** (skg node (gnid m) (skgRepos (nodeRepo r)) writeProtected (editRequest (processed rel)) (diff (rel added) (properties changed) (leaves 2 rel all))) m"
                   "* (skg node (gnid other) (skgRepos (nodeRepo r)) (diff (leaves 1 all))) other"
                   "** (skg (oldText title) (diff (leaves 1))) other")))
  ;; At point, a changeset marker names its processed aggregate.
  (should (equal (test-skg-processed--run
                  '("** (skg (changesetMarker content) (diff (leaves 2 all)))")
                  1 #'skg-cycle-processed '(4))
                 '("** (skg (changesetMarker content) (editRequest (processed all)) (diff (leaves 2 all))) "))))

;; At a new node in content position, the recursive gesture names
;; 'newContent' (one C-u) or 'newOutward' (two), which the server
;; expands from the graph, and skips the rest of that node's subtree;
;; it walks on past it. A new node in a folder is no content; the walk
;; treats it as any other headline.
(ert-deftest test-skg-cycle-processed-recursively-hands-new-nodes-to-the-server ()
  (let ((lines '("* (skg node (gnid r) (skgRepos (nodeRepo r)) (diff (leaves 1 all))) r"
                 "** (skg node (gnid n) (skgRepos (nodeRepo r)) (diff (node added) (rel added) (leaves 2 node rel))) n"
                 "*** (skg node (gnid n2) (skgRepos (nodeRepo r)) (diff (node added) (rel added) (leaves 2 node rel))) n2"
                 "*** (skg node (gnid m) (skgRepos (nodeRepo r)) writeProtected (diff (rel added) (properties changed) (leaves 2 rel all))) m"
                 "** (skg subscriberFolder)"
                 "*** (skg node (gnid s) (skgRepos (nodeRepo r)) writeProtected (diff (node added) (rel added) (leaves 2 node rel))) s"
                 "** (skg node (gnid c) (skgRepos (nodeRepo r)) (diff (rel added) (leaves 1 rel))) c")))
    (dolist (case '(((4) . "newContent") ((16) . "newOutward")))
      (should (equal (test-skg-processed--run lines 1 #'skg-cycle-processed (car case))
                     (list
                      "* (skg node (gnid r) (skgRepos (nodeRepo r)) (editRequest (processed all)) (diff (leaves 1 all))) r"
                      (format "** (skg node (gnid n) (skgRepos (nodeRepo r)) (editRequest (processed node rel %s)) (diff (node added) (rel added) (leaves 2 node rel))) n"
                              (cdr case))
                      (nth 2 lines)
                      (nth 3 lines)
                      (nth 4 lines)
                      "*** (skg node (gnid s) (skgRepos (nodeRepo r)) writeProtected (editRequest (processed node rel)) (diff (node added) (rel added) (leaves 2 node rel))) s"
                      "** (skg node (gnid c) (skgRepos (nodeRepo r)) (editRequest (processed rel)) (diff (rel added) (leaves 1 rel))) c"))))
    ;; At a new start: the walk object alone, not the relationship to
    ;; its viewparent, which is no part of what the walk reaches.
    (should (equal (test-skg-processed--run (cdr lines) 1 #'skg-cycle-processed '(4))
                   (cons "** (skg node (gnid n) (skgRepos (nodeRepo r)) (editRequest (processed newContent)) (diff (node added) (rel added) (leaves 2 node rel))) n"
                         (cddr lines))))
    ;; A new start toggles by its existence alone, and unmarking asks
    ;; first, since the client cannot count what the server will reach.
    (should (equal (test-skg-processed--run
                    '("* (skg node (gnid n) (skgRepos (nodeRepo r)) (diff (node added) (rel added) (leaves 2 node rel) (processed node))) n")
                    1 #'skg-cycle-processed '(4) 'yes)
                   '("* (skg node (gnid n) (skgRepos (nodeRepo r)) (editRequest (unprocessed newContent)) (diff (node added) (rel added) (leaves 2 node rel) (processed node))) n")))))

(ert-deftest test-skg-toggle-homeSkgrepo-move-processed ()
  (let ((lines '("* (skg node (gnid a) (skgRepos (nodeRepo b)) (diff (leaves 1 all))) a"
                 "** (skg (homeSkgrepoMove (old x) (new b)) (diff (leaves 1)))"
                 "** (skg node (gnid c) (skgRepos (nodeRepo b)) writeProtected (diff (leaves 1 all))) c"
                 "*** (skg (homeSkgrepoMove (old x) (new b)) (diff (leaves 1)))"
                 "** (skg node (gnid e) (skgRepos (nodeRepo b)) (diff (leaves 1 all))) e"
                 "*** (skg (homeSkgrepoMove (old y) (new b)) (diff (leaves 1)))")))
    (should (equal (test-skg-processed--run
                    lines 1 #'skg-toggle-homeSkgrepo-move-processed)
                   '("* (skg node (gnid a) (skgRepos (nodeRepo b)) (diff (leaves 1 all))) a"
                     "** (skg (homeSkgrepoMove (old x) (new b)) (editRequest (processed)) (diff (leaves 1))) "
                     "** (skg node (gnid c) (skgRepos (nodeRepo b)) writeProtected (diff (leaves 1 all))) c"
                     "*** (skg (homeSkgrepoMove (old x) (new b)) (diff (leaves 1)))"
                     "** (skg node (gnid e) (skgRepos (nodeRepo b)) (diff (leaves 1 all))) e"
                     "*** (skg (homeSkgrepoMove (old y) (new b)) (diff (leaves 1)))")))
    ;; Recursively: only the moves between the same two skgrepos.
    (should (equal (test-skg-processed--run
                    lines 1 #'skg-toggle-homeSkgrepo-move-processed '(4))
                   '("* (skg node (gnid a) (skgRepos (nodeRepo b)) (diff (leaves 1 all))) a"
                     "** (skg (homeSkgrepoMove (old x) (new b)) (editRequest (processed)) (diff (leaves 1))) "
                     "** (skg node (gnid c) (skgRepos (nodeRepo b)) writeProtected (diff (leaves 1 all))) c"
                     "*** (skg (homeSkgrepoMove (old x) (new b)) (editRequest (processed)) (diff (leaves 1))) "
                     "** (skg node (gnid e) (skgRepos (nodeRepo b)) (diff (leaves 1 all))) e"
                     "*** (skg (homeSkgrepoMove (old y) (new b)) (diff (leaves 1)))")))))

(ert-deftest test-skg-processed-a-single-leaf-unmark-asks-nothing ()
  ;; The folder's one leaf is its only unmark: nothing is asked.
  (should (equal (test-skg-processed--run
                  '("** (skg aliasFolder)"
                    "*** (skg alias noClaim (view ghostWarrant) (diff (rel removed) (leaves 1) (processed))) a1"
                    "*** (skg alias) a2")
                  1 #'skg-cycle-processed)
                 '("** (skg aliasFolder)"
                   "*** (skg alias noClaim (editRequest (unprocessed)) (view ghostWarrant) (diff (rel removed) (leaves 1) (processed))) a1"
                   "*** (skg alias) a2")))
  ;; A modified node whose only leaf is processed: one leaf, as the server
  ;; counts, so nothing is asked.
  (should (equal (test-skg-processed--run
                  '("* (skg node (gnid txt) (skgRepos (nodeRepo r)) (diff (properties changed) (leaves 1 all) (processed all))) txt"
                    "** (skg (oldText title) (diff (leaves 1) (processed))) txt")
                  1 #'skg-cycle-processed)
                 '("* (skg node (gnid txt) (skgRepos (nodeRepo r)) (editRequest (unprocessed all)) (diff (properties changed) (leaves 1 all) (processed all))) txt"
                   "** (skg (oldText title) (diff (leaves 1) (processed))) txt"))))

(ert-deftest test-skg-processed-a-wide-unmark-asks-first ()
  (let ((lines '("* (skg ghost (gnid d) (skgRepos (nodeRepo r)) (diff (node removed) (rel removed) (leaves 2 node rel) (processed node rel))) d"
                 "** (skg (oldText title) (diff (leaves 1) (processed))) d")))
    ;; Refused: nothing is written, nothing saved.
    (should (equal (test-skg-processed--run
                    lines 1 #'skg-cycle-processed '(4) 'no)
                   lines))
    (should (equal (test-skg-processed--run
                    lines 1 #'skg-cycle-processed '(4) 'yes)
                   '("* (skg ghost (gnid d) (skgRepos (nodeRepo r)) (editRequest (unprocessed node rel)) (diff (node removed) (rel removed) (leaves 2 node rel) (processed node rel))) d"
                     "** (skg (oldText title) (editRequest (unprocessed)) (diff (leaves 1) (processed))) d"))))
  ;; Unmarking a modified node unmarks every leaf it records: two here.
  (should (equal (test-skg-processed--run
                  '("* (skg node (gnid txt) (skgRepos (nodeRepo r)) (diff (properties changed) (leaves 2 all) (processed all))) txt"
                    "** (skg (oldText title) (diff (leaves 1) (processed))) txt"
                    "** (skg (homeSkgrepoMove (old x) (new r)) (diff (leaves 1) (processed)))")
                  1 #'skg-cycle-processed nil 'no)
                 '("* (skg node (gnid txt) (skgRepos (nodeRepo r)) (diff (properties changed) (leaves 2 all) (processed all))) txt"
                   "** (skg (oldText title) (diff (leaves 1) (processed))) txt"
                   "** (skg (homeSkgrepoMove (old x) (new r)) (diff (leaves 1) (processed)))"))))

(ert-deftest test-skg-processed-the-wide-unmark-warning-uses-the-servers-count ()
  ;; A processed changeset marker over three leaves: asked first.
  (let ((lines '("** (skg (changesetMarker content) (diff (leaves 3 all) (processed all)))")))
    (should (equal (test-skg-processed--run
                    lines 1 #'skg-cycle-processed nil 'no)
                   lines)))
  ;; A processed single-leaf headline: nothing is asked.
  (should (equal (test-skg-processed--run
                  '("** (skg (oldText title) (diff (leaves 1) (processed))) txt")
                  1 #'skg-cycle-processed)
                 '("** (skg (oldText title) (editRequest (unprocessed)) (diff (leaves 1) (processed))) txt"))))

(ert-deftest test-skg-toggle-homeSkgrepo-move-processed-unmarks ()
  (let ((lines '("* (skg node (gnid a) (skgRepos (nodeRepo b)) (diff (leaves 1 all))) a"
                 "** (skg (homeSkgrepoMove (old x) (new b)) (diff (leaves 1) (processed)))"
                 "** (skg node (gnid c) (skgRepos (nodeRepo b)) writeProtected (diff (leaves 1 all))) c"
                 "*** (skg (homeSkgrepoMove (old x) (new b)) (diff (leaves 1) (processed)))")))
    ;; One move, from the node or the move: nothing is asked.
    (dolist (line-number '(1 2))
      (should (equal (test-skg-processed--run
                      lines line-number
                      #'skg-toggle-homeSkgrepo-move-processed)
                     '("* (skg node (gnid a) (skgRepos (nodeRepo b)) (diff (leaves 1 all))) a"
                       "** (skg (homeSkgrepoMove (old x) (new b)) (editRequest (unprocessed)) (diff (leaves 1) (processed))) "
                       "** (skg node (gnid c) (skgRepos (nodeRepo b)) writeProtected (diff (leaves 1 all))) c"
                       "*** (skg (homeSkgrepoMove (old x) (new b)) (diff (leaves 1) (processed)))"))))
    ;; Recursively, two moves: asked first.
    (should (equal (test-skg-processed--run
                    lines 1 #'skg-toggle-homeSkgrepo-move-processed
                    '(4) 'yes)
                   '("* (skg node (gnid a) (skgRepos (nodeRepo b)) (diff (leaves 1 all))) a"
                     "** (skg (homeSkgrepoMove (old x) (new b)) (editRequest (unprocessed)) (diff (leaves 1) (processed))) "
                     "** (skg node (gnid c) (skgRepos (nodeRepo b)) writeProtected (diff (leaves 1 all))) c"
                     "*** (skg (homeSkgrepoMove (old x) (new b)) (editRequest (unprocessed)) (diff (leaves 1) (processed))) ")))))

(ert-deftest test-skg-processed-do-children-visits-exactly-one-level-below-point ()
  "The children of the headline at point, not its grandchildren, its
siblings, or a child without metadata."
  (with-temp-buffer
    (org-mode)
    (insert "* (skg node (gnid before)) before\n"
            "* (skg node (gnid parent)) parent\n"
            "body of parent\n"
            "** (skg node (gnid child1)) child1\n"
            "*** (skg node (gnid grandchild)) grandchild\n"
            "** no metadata\n"
            "** (skg node (gnid child2)) child2\n"
            "* (skg node (gnid after)) after\n"
            "** (skg node (gnid nephew)) nephew\n")
    (goto-char (point-min))
    (forward-line 1)
    (let ((visited nil))
      (should (equal (skg--processed-do-children
                      (lambda (metadata-sexp)
                        (push (skg--node-id metadata-sexp) visited)
                        (skg--node-id metadata-sexp)))
                     '("child1" "child2")))
      (should (equal (nreverse visited) '("child1" "child2"))))))

(ert-deftest test-skg-processed-summary-is-shown-after-a-save ()
  (let ((shown '()))
    (cl-letf (((symbol-function 'message)
               (lambda (format-string &rest args)
                 (push (apply #'format format-string args) shown))))
      (with-temp-buffer
        (skg-handle-save-sexp
         (concat "((content \"* (skg node (gnid a)) a\n\") (errors ()) (warnings ())"
                 " (processed-summary (processed 12) (changed 14) (foreign 2)"
                 " (order-only 1) (notEditable 3)))")))
      (should (equal (car shown)
                     "12 of 14 changed files are processed, 3 changes are not editable in this view; C-c r u extends the diff view to them, 2 files changed in foreign skgrepos"))
      (with-temp-buffer
        (skg-handle-save-sexp
         (concat "((content \"* (skg node (gnid a)) a\n\") (errors ()) (warnings ())"
                 " (processed-summary (processed 14) (changed 14) (foreign 0)"
                 " (order-only 0) (notEditable 0)))")))
      (should (equal (car shown)
                     "Commit ready. All 14 of 14 changed files are processed."))
      (setq shown '())
      (with-temp-buffer
        (skg-handle-save-sexp
         "((content \"* (skg node (gnid a)) a\n\") (errors ()) (warnings ()))"))
      (should-not (seq-some (lambda (line) (string-match-p "changed files" line))
                            shown)))))

;;; Moving between headlines with change facts

(defconst test-skg-processed--headlines-with-change-facts
  '("* (skg node (gnid r) (skgRepos (nodeRepo main))) r"
    "** (skg node (gnid a) (skgRepos (nodeRepo main)) (diff (node added) (leaves 1 node))) a"
    "*** (skg node (gnid b) (skgRepos (nodeRepo main))) b"
    "** (skg node (gnid c) (skgRepos (nodeRepo main)) (diff (properties changed) (leaves 2 all) (processed all))) c"
    "** (skg containerFolder) its containers"
    "*** (skg alias noClaim (view ghostWarrant) (diff (rel removed) (leaves 1))) a1")
  "Three headlines with change facts (lines 2, 4 and 6), the middle one processed,
among headlines that carry no leaves fact, one of them a changed folder.")

(defun test-skg-processed--goto-changes (lines from command times)
  "Insert LINES, put point at the start of line FROM (or at the
buffer's end when FROM is nil), and run COMMAND TIMES times. Return
the line each run left point on, or the message of a run that did
not move it. A run that moves leaves point on '(skg'."
  (with-temp-buffer
    (org-mode)
    (insert (mapconcat #'identity lines "\n") "\n")
    (if from
        (progn (goto-char (point-min)) (forward-line (1- from)))
      (goto-char (point-max)))
    (let ((results '()))
      (dotimes (_ times)
        (let* ((before (point))
               (shown nil))
          (cl-letf (((symbol-function 'message)
                     (lambda (fmt &rest args)
                       (setq shown (apply #'format fmt args)))))
            (funcall command))
          (if (= before (point))
              (push shown results)
            (should (looking-at-p "(skg"))
            (push (line-number-at-pos) results))))
      (nreverse results))))

(ert-deftest test-skg-diffView-goto-change-commands ()
  "Each of the six commands visits the headlines with change facts of its kind in
buffer order and reports the end without wrapping."
  (let ((lines test-skg-processed--headlines-with-change-facts))
    (should (equal (test-skg-processed--goto-changes
                    lines 1 #'skg-diffView-goto-next-change 4)
                   '(2 4 6 "No further change")))
    (should (equal (test-skg-processed--goto-changes
                    lines 1 #'skg-diffView-goto-next-unprocessed-change 3)
                   '(2 6 "No further unprocessed change")))
    (should (equal (test-skg-processed--goto-changes
                    lines 1 #'skg-diffView-goto-next-processed-change 2)
                   '(4 "No further processed change")))
    (should (equal (test-skg-processed--goto-changes
                    lines nil #'skg-diffView-goto-previous-change 4)
                   '(6 4 2 "No earlier change")))
    (should (equal (test-skg-processed--goto-changes
                    lines nil #'skg-diffView-goto-previous-unprocessed-change 3)
                   '(6 2 "No earlier unprocessed change")))
    (should (equal (test-skg-processed--goto-changes
                    lines nil #'skg-diffView-goto-previous-processed-change 2)
                   '(4 "No earlier processed change")))
    ;; From the middle, each direction starts at point.
    (should (equal (test-skg-processed--goto-changes
                    lines 3 #'skg-diffView-goto-previous-change 1)
                   '(2)))
    (should (equal (test-skg-processed--goto-changes
                    lines 3 #'skg-diffView-goto-next-unprocessed-change 1)
                   '(6)))))

(ert-deftest test-skg-diffView-goto-change-outside-the-diff-view ()
  "Where no headline carries a leaves fact, there is no change."
  (should (equal (test-skg-processed--goto-changes
                  '("* (skg node (gnid r) (skgRepos (nodeRepo main))) r"
                    "** (skg node (gnid a) (skgRepos (nodeRepo main))) a")
                  1 #'skg-diffView-goto-next-change 1)
                 '("No further change"))))

(ert-deftest test-skg-diffView-goto-change-keybindings ()
  (dolist (pair '(("C-c r n c" . skg-diffView-goto-next-change)
                  ("C-c r n u" . skg-diffView-goto-next-unprocessed-change)
                  ("C-c r n r" . skg-diffView-goto-next-processed-change)
                  ("C-c r p c" . skg-diffView-goto-previous-change)
                  ("C-c r p u" . skg-diffView-goto-previous-unprocessed-change)
                  ("C-c r p r" . skg-diffView-goto-previous-processed-change)))
    (should (eq (lookup-key skg-content-view-mode-map (kbd (car pair)))
                (cdr pair)))))

(ert-deftest test-skg-membership-change-processed-checks-every-relRepo-move ()
  "A folder member's change is processed only when every '(relRepoMove ...)'
child is, not the first alone."
  (let ((member '(skg node (gnid m)))
        (processed-move '(skg (relRepoMove (old pub) (new priv)) (diff (processed))))
        (unprocessed-move '(skg (relRepoMove (old priv) (new pub)) (diff))))
    (should (eq (skg--membership-change-processed
                 member (list processed-move unprocessed-move))
                'unprocessed))
    (should (eq (skg--membership-change-processed
                 member (list processed-move processed-move))
                'processed))
    (should (eq (skg--membership-change-processed
                 '(skg node (gnid m) (diff (rel added) (leaves 1 rel) (processed rel)))
                 (list processed-move))
                'processed))
    (should-not (skg--membership-change-processed member nil))))
