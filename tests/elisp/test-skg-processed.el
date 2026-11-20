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

(defun test-skg-processed--refused (lines line-number command &optional prefix)
  "Run COMMAND (with PREFIX) on line LINE-NUMBER of LINES; it must
refuse with a user error, writing nothing and saving nothing."
  (with-temp-buffer
    (org-mode)
    (insert (mapconcat #'identity lines "\n") "\n")
    (goto-char (point-min))
    (forward-line (1- line-number))
    (cl-letf (((symbol-function 'skg-request-save-buffer)
               (lambda (&rest _) (ert-fail "a refused gesture saved"))))
      (should-error (funcall command prefix) :type 'user-error))
    (should (equal (split-string (buffer-string) "\n" t) lines))))

(ert-deftest test-skg-mark-processed-a-single-leaf ()
  "A headline naming one leaf gets the bare request from a letter that
names its kind: a relRepo move is a relationship's, any other the
node's own."
  (should (equal (test-skg-processed--run
                  '("** (skg alias noClaim (view ghostWarrant) (diff (rel removed) (leaves 1))) a1")
                  1 #'skg-mark-node-processed)
                 '("** (skg alias noClaim (editRequest (processed)) (view ghostWarrant) (diff (rel removed) (leaves 1))) a1")))
  (should (equal (test-skg-processed--run
                  '("** (skg (oldText title) (diff (leaves 1) (processed))) txt")
                  1 #'skg-mark-both-unprocessed)
                 '("** (skg (oldText title) (editRequest (unprocessed)) (diff (leaves 1) (processed))) txt")))
  (should (equal (test-skg-processed--run
                  '("** (skg (relRepoMove (old pub) (new priv)) (diff (leaves 1)))")
                  1 #'skg-mark-rel-processed)
                 '("** (skg (relRepoMove (old pub) (new priv)) (editRequest (processed)) (diff (leaves 1))) ")))
  (test-skg-processed--refused
   '("** (skg (relRepoMove (old pub) (new priv)) (diff (leaves 1)))")
   1 #'skg-mark-node-processed)
  ;; A node's other changes, a reorder among them, are the node's own.
  (should (equal (test-skg-processed--run
                  '("** (skg (otherChanges contentOrder) (diff (leaves 1)))"
                    "The order of its content (- at HEAD, + now):"
                    "+b"
                    " a"
                    "-b")
                  1 #'skg-mark-node-processed)
                 '("** (skg (otherChanges contentOrder) (editRequest (processed)) (diff (leaves 1))) "
                   "The order of its content (- at HEAD, + now):"
                   "+b"
                   " a"
                   "-b")))
  (test-skg-processed--refused
   '("** (skg (otherChanges attributionChange) (diff (leaves 1)))")
   1 #'skg-mark-rel-processed)
  ;; An unknown viewnode shows no node, only its relationship.
  (should (equal (test-skg-processed--run
                  '("** (skg unknown (gnid k) (diff (rel added) (leaves 1))) k")
                  1 #'skg-mark-rel-processed)
                 '("** (skg unknown (gnid k) (editRequest (processed)) (diff (rel added) (leaves 1))) k")))
  (test-skg-processed--refused
   '("** (skg unknown (gnid k) (diff (rel added) (leaves 1))) k")
   1 #'skg-mark-node-processed)
  (test-skg-processed--refused
   '("** (skg alias (diff (rel added) (leaves 1))) a")
   1 #'skg-mark-rel-processed)
  ;; Without the server's '(diff (leaves ...))' fact, nothing is named.
  (test-skg-processed--refused '("** (skg (oldText title)) txt")
                               1 #'skg-mark-both-processed))

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

(ert-deftest test-skg-mark-processed-names-the-letters-among-the-listed-objects ()
  ;; A new member lists two objects, one of them processed: each letter
  ;; names its own, without asking.
  (let ((line "* (skg node (gnid k) (skgRepos (nodeRepo r)) (diff (node added) (rel added) (leaves 2 node rel) (processed rel))) k"))
    (dolist (case '((skg-mark-node-processed . "(processed node)")
                    (skg-mark-rel-processed . "(processed rel)")
                    (skg-mark-both-processed . "(processed node rel)")
                    (skg-mark-rel-unprocessed . "(unprocessed rel)")
                    (skg-mark-node-unprocessed . "(unprocessed node)")))
      (should (equal (test-skg-processed--run (list line) 1 (car case))
                     (list (format "* (skg node (gnid k) (skgRepos (nodeRepo r)) (editRequest %s) (diff (node added) (rel added) (leaves 2 node rel) (processed rel))) k"
                                   (cdr case)))))))
  ;; 'b' on a headline that lists one object names that one.
  (should (equal (test-skg-processed--run
                  '("* (skg ghost (gnid d) (skgRepos (nodeRepo r)) (diff (node removed) (leaves 1 node))) d")
                  1 #'skg-mark-both-processed)
                 '("* (skg ghost (gnid d) (skgRepos (nodeRepo r)) (editRequest (processed node)) (diff (node removed) (leaves 1 node))) d")))
  ;; A letter naming nothing listed refuses.
  (test-skg-processed--refused
   '("* (skg ghost (gnid d) (skgRepos (nodeRepo r)) (diff (node removed) (leaves 1 node))) d")
   1 #'skg-mark-rel-processed)
  (test-skg-processed--refused
   '("* (skg node (gnid c) (skgRepos (nodeRepo r)) (diff (rel added) (leaves 1 rel))) c")
   1 #'skg-mark-node-unprocessed))

(ert-deftest test-skg-mark-processed-on-a-changed-folder-writes-one-level-down ()
  ;; The letter filters the members: an alias is its node's own leaf.
  (let ((lines '("* (skg node (gnid ali) (skgRepos (nodeRepo r)) (diff (leaves 2 node))) ali"
                 "** (skg aliasFolder)"
                 "*** (skg alias noClaim (view ghostWarrant) (diff (rel removed) (leaves 1) (processed))) a1"
                 "*** (skg alias) a2"
                 "*** (skg alias (diff (rel added) (leaves 1))) a3")))
    (should (equal (test-skg-processed--run lines 2 #'skg-mark-node-processed)
                   '("* (skg node (gnid ali) (skgRepos (nodeRepo r)) (diff (leaves 2 node))) ali"
                     "** (skg aliasFolder)"
                     "*** (skg alias noClaim (editRequest (processed)) (view ghostWarrant) (diff (rel removed) (leaves 1) (processed))) a1"
                     "*** (skg alias) a2"
                     "*** (skg alias (editRequest (processed)) (diff (rel added) (leaves 1))) a3")))
    (test-skg-processed--refused lines 2 #'skg-mark-rel-processed))
  ;; A partner folder: 'r' names each member's relationship, 'n' only
  ;; the members whose node changed.
  (let ((lines '("** (skg hiderFolder)"
                 "*** (skg node (gnid s1) (skgRepos (nodeRepo r)) (diff (rel added) (leaves 1 rel))) s1"
                 "*** (skg node (gnid s2) (skgRepos (nodeRepo r)) (diff (node added) (rel added) (leaves 2 node rel))) s2")))
    (should (equal (test-skg-processed--run lines 1 #'skg-mark-rel-processed)
                   '("** (skg hiderFolder)"
                     "*** (skg node (gnid s1) (skgRepos (nodeRepo r)) (editRequest (processed rel)) (diff (rel added) (leaves 1 rel))) s1"
                     "*** (skg node (gnid s2) (skgRepos (nodeRepo r)) (editRequest (processed rel)) (diff (node added) (rel added) (leaves 2 node rel))) s2")))
    (should (equal (test-skg-processed--run lines 1 #'skg-mark-node-processed)
                   '("** (skg hiderFolder)"
                     "*** (skg node (gnid s1) (skgRepos (nodeRepo r)) (diff (rel added) (leaves 1 rel))) s1"
                     "*** (skg node (gnid s2) (skgRepos (nodeRepo r)) (editRequest (processed node)) (diff (node added) (rel added) (leaves 2 node rel))) s2")))))

(ert-deftest test-skg-mark-processed-on-a-changeset-marker ()
  "At point a changeset marker takes the letters as they are: its
'node' names its members' own leaves, which its '(leaves ...)' does
not list nor count, so unmarking them asks first."
  (let ((lines '("** (skg (changesetMarker content) (diff (leaves 1 rel) (processed rel)))")))
    (should (equal (test-skg-processed--run lines 1 #'skg-mark-node-processed)
                   '("** (skg (changesetMarker content) (editRequest (processed node)) (diff (leaves 1 rel) (processed rel))) ")))
    (should (equal (test-skg-processed--run lines 1 #'skg-mark-both-processed)
                   '("** (skg (changesetMarker content) (editRequest (processed node rel)) (diff (leaves 1 rel) (processed rel))) ")))
    (should (equal (test-skg-processed--run lines 1 #'skg-mark-rel-unprocessed)
                   '("** (skg (changesetMarker content) (editRequest (unprocessed rel)) (diff (leaves 1 rel) (processed rel))) ")))
    (should (equal (test-skg-processed--run lines 1 #'skg-mark-node-unprocessed nil 'no)
                   lines))))

(ert-deftest test-skg-mark-processed-recursively-honours-the-letter ()
  "With C-u, every headline in the subtree gets what the letter names
there. Below the headline at point a changeset marker names only what
it lists, its members naming their own leaves."
  (let ((lines '("* (skg node (gnid r) (skgRepos (nodeRepo r)) (diff (properties changed) (leaves 1 node))) r"
                 "** (skg (changesetMarker content) (diff (leaves 2 rel)))"
                 "** (skg ghost (gnid d) (skgRepos (nodeRepo r)) (diff (node removed) (rel removed) (leaves 2 node rel))) d"
                 "*** (skg node (gnid k) (skgRepos (nodeRepo r)) writeProtected) k"
                 "** (skg node (gnid c) (skgRepos (nodeRepo r)) writeProtected (diff (rel added) (leaves 1 rel))) c"
                 "** (skg node (gnid m) (skgRepos (nodeRepo r)) writeProtected (diff (rel added) (properties changed) (leaves 3 node rel))) m"
                 "*** (skg (oldText title) (diff (leaves 1))) m"
                 "* (skg node (gnid other) (skgRepos (nodeRepo r)) (diff (leaves 1 node))) other")))
    (should (equal (test-skg-processed--run lines 1 #'skg-mark-rel-processed '(4))
                   '("* (skg node (gnid r) (skgRepos (nodeRepo r)) (diff (properties changed) (leaves 1 node))) r"
                     "** (skg (changesetMarker content) (editRequest (processed rel)) (diff (leaves 2 rel))) "
                     "** (skg ghost (gnid d) (skgRepos (nodeRepo r)) (editRequest (processed rel)) (diff (node removed) (rel removed) (leaves 2 node rel))) d"
                     "*** (skg node (gnid k) (skgRepos (nodeRepo r)) writeProtected) k"
                     "** (skg node (gnid c) (skgRepos (nodeRepo r)) writeProtected (editRequest (processed rel)) (diff (rel added) (leaves 1 rel))) c"
                     "** (skg node (gnid m) (skgRepos (nodeRepo r)) writeProtected (editRequest (processed rel)) (diff (rel added) (properties changed) (leaves 3 node rel))) m"
                     "*** (skg (oldText title) (diff (leaves 1))) m"
                     "* (skg node (gnid other) (skgRepos (nodeRepo r)) (diff (leaves 1 node))) other")))
    (should (equal (test-skg-processed--run lines 1 #'skg-mark-node-processed '(4))
                   '("* (skg node (gnid r) (skgRepos (nodeRepo r)) (editRequest (processed node)) (diff (properties changed) (leaves 1 node))) r"
                     "** (skg (changesetMarker content) (diff (leaves 2 rel)))"
                     "** (skg ghost (gnid d) (skgRepos (nodeRepo r)) (editRequest (processed node)) (diff (node removed) (rel removed) (leaves 2 node rel))) d"
                     "*** (skg node (gnid k) (skgRepos (nodeRepo r)) writeProtected) k"
                     "** (skg node (gnid c) (skgRepos (nodeRepo r)) writeProtected (diff (rel added) (leaves 1 rel))) c"
                     "** (skg node (gnid m) (skgRepos (nodeRepo r)) writeProtected (editRequest (processed node)) (diff (rel added) (properties changed) (leaves 3 node rel))) m"
                     "*** (skg (oldText title) (editRequest (processed)) (diff (leaves 1))) m"
                     "* (skg node (gnid other) (skgRepos (nodeRepo r)) (diff (leaves 1 node))) other")))
    (should (equal (test-skg-processed--run lines 1 #'skg-mark-both-processed '(4))
                   '("* (skg node (gnid r) (skgRepos (nodeRepo r)) (editRequest (processed node)) (diff (properties changed) (leaves 1 node))) r"
                     "** (skg (changesetMarker content) (editRequest (processed rel)) (diff (leaves 2 rel))) "
                     "** (skg ghost (gnid d) (skgRepos (nodeRepo r)) (editRequest (processed node rel)) (diff (node removed) (rel removed) (leaves 2 node rel))) d"
                     "*** (skg node (gnid k) (skgRepos (nodeRepo r)) writeProtected) k"
                     "** (skg node (gnid c) (skgRepos (nodeRepo r)) writeProtected (editRequest (processed rel)) (diff (rel added) (leaves 1 rel))) c"
                     "** (skg node (gnid m) (skgRepos (nodeRepo r)) writeProtected (editRequest (processed node rel)) (diff (rel added) (properties changed) (leaves 3 node rel))) m"
                     "*** (skg (oldText title) (editRequest (processed)) (diff (leaves 1))) m"
                     "* (skg node (gnid other) (skgRepos (nodeRepo r)) (diff (leaves 1 node))) other")))
    ;; At point, a changeset marker takes the letters as they are.
    (should (equal (test-skg-processed--run (list (nth 1 lines)) 1 #'skg-mark-both-processed '(4))
                   '("** (skg (changesetMarker content) (editRequest (processed node rel)) (diff (leaves 2 rel))) ")))))

;; At a new node in content position, the recursive gesture names the
;; letters and 'newContent' (one C-u) or 'newOutward' (two), which the
;; server expands from the graph, and skips the rest of that node's
;; subtree; it walks on past it. A new node in a folder is no content;
;; the walk treats it as any other headline.
(ert-deftest test-skg-mark-processed-recursively-hands-new-nodes-to-the-server ()
  (let ((lines '("* (skg node (gnid r) (skgRepos (nodeRepo r)) (diff (properties changed) (leaves 1 node))) r"
                 "** (skg node (gnid n) (skgRepos (nodeRepo r)) (diff (node added) (rel added) (leaves 2 node rel))) n"
                 "*** (skg node (gnid n2) (skgRepos (nodeRepo r)) (diff (node added) (rel added) (leaves 2 node rel))) n2"
                 "*** (skg node (gnid m) (skgRepos (nodeRepo r)) writeProtected (diff (rel added) (properties changed) (leaves 3 node rel))) m"
                 "** (skg hiderFolder)"
                 "*** (skg node (gnid s) (skgRepos (nodeRepo r)) writeProtected (diff (node added) (rel added) (leaves 2 node rel))) s"
                 "** (skg node (gnid c) (skgRepos (nodeRepo r)) (diff (rel added) (leaves 1 rel))) c")))
    (dolist (case '(((4) . "newContent") ((16) . "newOutward")))
      (should (equal (test-skg-processed--run lines 1 #'skg-mark-both-processed (car case))
                     (list
                      "* (skg node (gnid r) (skgRepos (nodeRepo r)) (editRequest (processed node)) (diff (properties changed) (leaves 1 node))) r"
                      (format "** (skg node (gnid n) (skgRepos (nodeRepo r)) (editRequest (processed node rel %s)) (diff (node added) (rel added) (leaves 2 node rel))) n"
                              (cdr case))
                      (nth 2 lines)
                      (nth 3 lines)
                      (nth 4 lines)
                      "*** (skg node (gnid s) (skgRepos (nodeRepo r)) writeProtected (editRequest (processed node rel)) (diff (node added) (rel added) (leaves 2 node rel))) s"
                      "** (skg node (gnid c) (skgRepos (nodeRepo r)) (editRequest (processed rel)) (diff (rel added) (leaves 1 rel))) c"))))
    ;; The letter filters the walk, and the headlines walked in the buffer.
    (should (equal (test-skg-processed--run lines 1 #'skg-mark-rel-processed '(4))
                   (list
                    (nth 0 lines)
                    "** (skg node (gnid n) (skgRepos (nodeRepo r)) (editRequest (processed rel newContent)) (diff (node added) (rel added) (leaves 2 node rel))) n"
                    (nth 2 lines)
                    (nth 3 lines)
                    (nth 4 lines)
                    "*** (skg node (gnid s) (skgRepos (nodeRepo r)) writeProtected (editRequest (processed rel)) (diff (node added) (rel added) (leaves 2 node rel))) s"
                    "** (skg node (gnid c) (skgRepos (nodeRepo r)) (editRequest (processed rel)) (diff (rel added) (leaves 1 rel))) c")))
    ;; At a new start, the letter names the headline's own objects too.
    (should (equal (test-skg-processed--run (cdr lines) 1 #'skg-mark-node-processed '(4))
                   (cons "** (skg node (gnid n) (skgRepos (nodeRepo r)) (editRequest (processed node newContent)) (diff (node added) (rel added) (leaves 2 node rel))) n"
                         (cddr lines))))
    ;; The letter goes whole beside the walk, even where the headline
    ;; does not list it: without it the walk would name every kind.
    (should (equal (test-skg-processed--run
                    '("* (skg node (gnid n) (skgRepos (nodeRepo r)) (diff (node added) (leaves 1 node))) n")
                    1 #'skg-mark-rel-processed '(4))
                   '("* (skg node (gnid n) (skgRepos (nodeRepo r)) (editRequest (processed rel newContent)) (diff (node added) (leaves 1 node))) n")))
    ;; Unmarking at a new start asks first, since the client cannot
    ;; count what the server will reach.
    (should (equal (test-skg-processed--run
                    '("* (skg node (gnid n) (skgRepos (nodeRepo r)) (diff (node added) (rel added) (leaves 2 node rel) (processed node))) n")
                    1 #'skg-mark-node-unprocessed '(4) 'yes)
                   '("* (skg node (gnid n) (skgRepos (nodeRepo r)) (editRequest (unprocessed node newContent)) (diff (node added) (rel added) (leaves 2 node rel) (processed node))) n")))))

(ert-deftest test-skg-mark-homeSkgrepo-move-processed ()
  (let ((lines '("* (skg node (gnid a) (skgRepos (nodeRepo b)) (diff (leaves 1 node))) a"
                 "** (skg (homeSkgrepoMove (old x) (new b)) (diff (leaves 1)))"
                 "** (skg node (gnid c) (skgRepos (nodeRepo b)) writeProtected (diff (leaves 1 node))) c"
                 "*** (skg (homeSkgrepoMove (old x) (new b)) (diff (leaves 1)))"
                 "** (skg node (gnid e) (skgRepos (nodeRepo b)) (diff (leaves 1 node))) e"
                 "*** (skg (homeSkgrepoMove (old y) (new b)) (diff (leaves 1)))")))
    (should (equal (test-skg-processed--run
                    lines 1 #'skg-mark-homeSkgrepo-move-processed)
                   '("* (skg node (gnid a) (skgRepos (nodeRepo b)) (diff (leaves 1 node))) a"
                     "** (skg (homeSkgrepoMove (old x) (new b)) (editRequest (processed)) (diff (leaves 1))) "
                     "** (skg node (gnid c) (skgRepos (nodeRepo b)) writeProtected (diff (leaves 1 node))) c"
                     "*** (skg (homeSkgrepoMove (old x) (new b)) (diff (leaves 1)))"
                     "** (skg node (gnid e) (skgRepos (nodeRepo b)) (diff (leaves 1 node))) e"
                     "*** (skg (homeSkgrepoMove (old y) (new b)) (diff (leaves 1)))")))
    ;; Recursively: only the moves between the same two skgrepos.
    (should (equal (test-skg-processed--run
                    lines 1 #'skg-mark-homeSkgrepo-move-processed '(4))
                   '("* (skg node (gnid a) (skgRepos (nodeRepo b)) (diff (leaves 1 node))) a"
                     "** (skg (homeSkgrepoMove (old x) (new b)) (editRequest (processed)) (diff (leaves 1))) "
                     "** (skg node (gnid c) (skgRepos (nodeRepo b)) writeProtected (diff (leaves 1 node))) c"
                     "*** (skg (homeSkgrepoMove (old x) (new b)) (editRequest (processed)) (diff (leaves 1))) "
                     "** (skg node (gnid e) (skgRepos (nodeRepo b)) (diff (leaves 1 node))) e"
                     "*** (skg (homeSkgrepoMove (old y) (new b)) (diff (leaves 1)))"))))
  (test-skg-processed--refused
   '("* (skg node (gnid a) (skgRepos (nodeRepo b)) (diff (leaves 1 node))) a")
   1 #'skg-mark-homeSkgrepo-move-processed))

(ert-deftest test-skg-processed-a-single-leaf-unmark-asks-nothing ()
  ;; The folder's one leaf is its only unmark: nothing is asked.
  (should (equal (test-skg-processed--run
                  '("** (skg aliasFolder)"
                    "*** (skg alias noClaim (view ghostWarrant) (diff (rel removed) (leaves 1) (processed))) a1"
                    "*** (skg alias) a2")
                  1 #'skg-mark-node-unprocessed)
                 '("** (skg aliasFolder)"
                   "*** (skg alias noClaim (editRequest (unprocessed)) (view ghostWarrant) (diff (rel removed) (leaves 1) (processed))) a1"
                   "*** (skg alias) a2")))
  ;; A modified node whose only leaf is processed: one leaf, as the server
  ;; counts, so nothing is asked.
  (should (equal (test-skg-processed--run
                  '("* (skg node (gnid txt) (skgRepos (nodeRepo r)) (diff (properties changed) (leaves 1 node) (processed node))) txt"
                    "** (skg (oldText title) (diff (leaves 1) (processed))) txt")
                  1 #'skg-mark-node-unprocessed)
                 '("* (skg node (gnid txt) (skgRepos (nodeRepo r)) (editRequest (unprocessed node)) (diff (properties changed) (leaves 1 node) (processed node))) txt"
                   "** (skg (oldText title) (diff (leaves 1) (processed))) txt")))
  ;; Of two leaves, 'n' or 'r' alone unmarks one.
  (dolist (case '((skg-mark-node-unprocessed . "(unprocessed node)")
                  (skg-mark-rel-unprocessed . "(unprocessed rel)")))
    (should (equal (test-skg-processed--run
                    '("* (skg ghost (gnid d) (skgRepos (nodeRepo r)) (diff (node removed) (rel removed) (leaves 2 node rel) (processed node rel))) d")
                    1 (car case))
                   (list (format "* (skg ghost (gnid d) (skgRepos (nodeRepo r)) (editRequest %s) (diff (node removed) (rel removed) (leaves 2 node rel) (processed node rel))) d"
                                 (cdr case)))))))

(ert-deftest test-skg-processed-a-wide-unmark-asks-first ()
  (let ((lines '("* (skg ghost (gnid d) (skgRepos (nodeRepo r)) (diff (node removed) (rel removed) (leaves 2 node rel) (processed node rel))) d"
                 "** (skg (oldText title) (diff (leaves 1) (processed))) d")))
    ;; Refused: nothing is written, nothing saved.
    (should (equal (test-skg-processed--run
                    lines 1 #'skg-mark-both-unprocessed nil 'no)
                   lines))
    (should (equal (test-skg-processed--run
                    lines 1 #'skg-mark-both-unprocessed '(4) 'yes)
                   '("* (skg ghost (gnid d) (skgRepos (nodeRepo r)) (editRequest (unprocessed node rel)) (diff (node removed) (rel removed) (leaves 2 node rel) (processed node rel))) d"
                     "** (skg (oldText title) (editRequest (unprocessed)) (diff (leaves 1) (processed))) d"))))
  ;; Unmarking a modified node unmarks every leaf it names: two here.
  (should (equal (test-skg-processed--run
                  '("* (skg node (gnid txt) (skgRepos (nodeRepo r)) (diff (properties changed) (leaves 2 node) (processed node))) txt"
                    "** (skg (oldText title) (diff (leaves 1) (processed))) txt"
                    "** (skg (homeSkgrepoMove (old x) (new r)) (diff (leaves 1) (processed)))")
                  1 #'skg-mark-node-unprocessed nil 'no)
                 '("* (skg node (gnid txt) (skgRepos (nodeRepo r)) (diff (properties changed) (leaves 2 node) (processed node))) txt"
                   "** (skg (oldText title) (diff (leaves 1) (processed))) txt"
                   "** (skg (homeSkgrepoMove (old x) (new r)) (diff (leaves 1) (processed)))"))))

(ert-deftest test-skg-processed-leaves-touched ()
  "How many leaves a request touches, as the server counts them; an
object the headline does not list counts two, so that unmarking it asks."
  (dolist (case '((((3 node rel)) (node rel) 3)
                  (((3 node rel)) (node) 2)
                  (((3 node rel)) (rel) 1)
                  (((2 node)) (node) 2)
                  (((1 rel)) (node rel) 3) ; a changeset marker's 'node'
                  (((1 rel)) (node) 2)
                  (((2 node rel)) (node rel newContent) 4)
                  (((1 node)) (rel newOutward) 4)
                  (((1)) nil 1)
                  ((nil) nil 1)))
    (should (equal (list (car case) (cadr case)
                         (skg--processed-leaves-touched (caar case) (cadr case)))
                   case))))

(ert-deftest test-skg-mark-processed-keybindings ()
  "Each mark is 'C-c m', what, which way; the toggles are gone."
  (dolist (pair '(("C-c m n p" . skg-mark-node-processed)
                  ("C-c m n u" . skg-mark-node-unprocessed)
                  ("C-c m r p" . skg-mark-rel-processed)
                  ("C-c m r u" . skg-mark-rel-unprocessed)
                  ("C-c m b p" . skg-mark-both-processed)
                  ("C-c m b u" . skg-mark-both-unprocessed)
                  ("C-c m h p" . skg-mark-homeSkgrepo-move-processed)
                  ("C-c m h u" . skg-mark-homeSkgrepo-move-unprocessed)))
    (should (eq (lookup-key skg-content-view-mode-map (kbd (car pair)))
                (cdr pair))))
  (dolist (key '("C-c r r" "C-c r h"))
    (should-not (lookup-key skg-content-view-mode-map (kbd key))))
  (should-not (fboundp 'skg-cycle-processed))
  (should-not (fboundp 'skg-toggle-homeSkgrepo-move-processed)))

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
                 " (order-only 1) (waiting 1) (notEditable 3)))")))
      (should (equal (car shown)
                     "12 of 14 changed nodes are processed (1 of them waits), 3 changes are not editable in this view; C-c r u extends the diff view to them, 2 nodes changed in foreign skgrepos"))
      (with-temp-buffer
        (skg-handle-save-sexp
         (concat "((content \"* (skg node (gnid a)) a\n\") (errors ()) (warnings ())"
                 " (processed-summary (processed 14) (changed 14) (foreign 1)"
                 " (order-only 0) (waiting 0) (notEditable 0)))")))
      (should (equal (car shown)
                     "Commit ready. All 14 of 14 changed nodes are processed, 1 node changed in foreign skgrepos."))
      (with-temp-buffer
        (skg-handle-save-sexp
         (concat "((content \"* (skg node (gnid a)) a\n\") (errors ()) (warnings ())"
                 " (processed-summary (processed 5) (changed 6) (foreign 0)"
                 " (order-only 0) (waiting 2) (notEditable 0)))")))
      (should (equal (car shown)
                     "5 of 6 changed nodes are processed (2 of them wait)"))
      (setq shown '())
      (with-temp-buffer
        (skg-handle-save-sexp
         "((content \"* (skg node (gnid a)) a\n\") (errors ()) (warnings ()))"))
      (should-not (seq-some (lambda (line) (string-match-p "changed nodes" line))
                            shown)))))

;;; Moving between headlines with change facts

(defconst test-skg-processed--headlines-with-change-facts
  '("* (skg node (gnid r) (skgRepos (nodeRepo main))) r"
    "** (skg node (gnid a) (skgRepos (nodeRepo main)) (diff (node added) (leaves 1 node))) a"
    "*** (skg node (gnid b) (skgRepos (nodeRepo main))) b"
    "** (skg node (gnid c) (skgRepos (nodeRepo main)) (diff (properties changed) (leaves 2 node) (processed node))) c"
    "** (skg containerFolder) its containers"
    "*** (skg alias noClaim (view ghostWarrant) (diff (rel removed) (leaves 1))) a1")
  "Three headlines with change facts (lines 2, 4 and 6), the middle one processed,
among headlines that carry no leaves fact, one of them a changed folder.")

(defun test-skg-processed--goto-changes (lines from command times)
  "Insert LINES, put point at the start of line FROM (or at the
buffer's end when FROM is nil), and run COMMAND TIMES times. Return
the line each run left point on, or the message of a run that did
not move it. A run that moves leaves point at the start of a line."
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
            (should (bolp))
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
                  ("C-c r n p" . skg-diffView-goto-next-processed-change)
                  ("C-c r p c" . skg-diffView-goto-previous-change)
                  ("C-c r p u" . skg-diffView-goto-previous-unprocessed-change)
                  ("C-c r p p" . skg-diffView-goto-previous-processed-change)))
    (should (eq (lookup-key skg-content-view-mode-map (kbd (car pair)))
                (cdr pair)))))

;;; The plain motions skip a ☮ member of a folder of other files

(defconst test-skg-processed--complete-diff-view
  '("* (skg (vnid 0a) ghost (gnid m) (skgRepos (nodeRepo main)) (diff (node removed) (leaves 1 node))) m"
    "** (skg (vnid 0b) node (gnid a) (skgRepos (nodeRepo main)) (view (rels (contains (in 2)) (linksTo (in 1 (substantive 1))) (forks (in 1))) ghostWarrant) (diff (rel removed) (leaves 1 rel))) a"
    "*** (skg (vnid 0c) containerFolder)"
    "**** (skg (vnid 0d) node (gnid r2) (skgRepos (nodeRepo main)) writeProtected (view (rels (contains (out 1 (ancestors 2)))) (birth (contains out 2))) (diff (rel added) (leaves 1 rel))) r2"
    "**** (skg (vnid 0e) node (gnid x) (skgRepos (nodeRepo main)) writeProtected (view (rels (contains (out 1 (ancestors 2)))) (birth (contains out 2)))) x"
    "*** (skg (vnid 0f) mentionerFolder)"
    "**** (skg (vnid 0g) node (gnid y) (skgRepos (nodeRepo main)) writeProtected (view omittedBody (rels (linksTo (out 1 (ancestors 2)))) (birth (linksTo out 2))) (diff (rel added) (properties changed) (leaves 1 node))) y2"
    "*** (skg (vnid 0h) forksFolder)"
    "**** (skg (vnid 0i) node (gnid s) (skgRepos (nodeRepo main)) writeProtected (view (rels (forks (out 1 (ancestors 2)))) (birth (forks out 2))) (diff (rel added) (leaves 1 rel))) s"
    "* (skg (vnid 0j) node (gnid r2) (skgRepos (nodeRepo main)) (view (rels (contains (out 1))))) r2"
    "** (skg (vnid 0k) (changesetMarker content) (diff (leaves 1 rel)))"
    "** (skg (vnid 0l) node (gnid a) (skgRepos (nodeRepo main)) writeProtected (view (rels (contains (in 2 (ancestors 1))) (linksTo (in 1 (substantive 1))) (forks (in 1))) (birth (contains in 1))) (diff (rel added) (leaves 1 rel))) a"
    "* (skg (vnid 0m) node (gnid s) (skgRepos (nodeRepo main)) (view (rels (forks (out 1))))) s"
    "** (skg (vnid 0n) forksFolder)"
    "*** (skg (vnid 0o) node (gnid a) (skgRepos (nodeRepo main)) writeProtected (view (rels (contains (in 2) (out 0 (unintegrated 0))) (linksTo (in 1 (substantive 1))) (forks (in 1 (ancestors 2)))) (birth (forks in 2))) (diff (rel added) (leaves 1 rel))) a"
    "**** (skg (vnid 0p) ghost (gnid m) (skgRepos (nodeRepo main)) (diff (node removed) (rel removed))) m"
    "***** (skg (vnid 0q) node (gnid a) (skgRepos (nodeRepo main)) writeProtected (view (rels (contains (in 2)) (linksTo (in 1 (substantive 1))) (forks (in 1))) ghostWarrant cycle) (diff (rel removed) (leaves 1 rel))) a"
    "**** (skg (vnid 0r) node (gnid r2) (skgRepos (nodeRepo main)) writeProtected (view (rels (contains (out 1 (ancestors 1)))))) r2"
    "**** (skg (vnid 0s) node (gnid x) (skgRepos (nodeRepo main)) writeProtected (view (rels (contains (out 1 (ancestors 1)))))) x"
    "* (skg (vnid 0t) node (gnid y) (skgRepos (nodeRepo main)) (view (rels (linksTo (out 1)))) (diff (properties changed) (leaves 1 node))) y2"
    "see [[id:a][a]]"
    "** (skg (vnid 0u) (oldText titleAndBody) (diff (leaves 1))) y"
    "*** (skg (vnid 0v) bodyDiff)"
    "+see [[id:a][a]]")
  "A diff view the server drew, every change in it editable. At HEAD m
contains a; in the worktree m is deleted, r2 contains a, s forks a,
and y, retitled, mentions a. So a's containers (line 4), mentioners
\(7) and forks (9) changed, each member write-protected, while each
member's change is drawn where its node stands editable: r2's at 11
and 12, y's at 20 and 22, s's at 15 and 17. In a's forks folder s,
born only of forking a, records that fork in its own file, so the
plain motions skip it, as a member of a folder of other files.")

(defconst test-skg-processed--diff-view-with-an-editable-member
  '("* (skg (vnid 0a) ghost (gnid m) (skgRepos (nodeRepo main)) (diff (node removed) (leaves 1 node))) m"
    "** (skg (vnid 0b) node (gnid a) (skgRepos (nodeRepo main)) (view (rels (contains (in 2)) (linksTo (in 1 (substantive 1))) (forks (in 1))) ghostWarrant) (diff (rel removed) (leaves 1 rel))) a"
    "*** (skg (vnid 0c) containerFolder)"
    "**** (skg (vnid 0d) node (gnid r2) (skgRepos (nodeRepo main)) writeProtected (view (rels (contains (out 1 (ancestors 2)))) (birth (contains out 2))) (diff (rel added) (leaves 1 rel))) r2"
    "**** (skg (vnid 0e) node (gnid x) (skgRepos (nodeRepo main)) writeProtected (view (rels (contains (out 1 (ancestors 2)))) (birth (contains out 2)))) x"
    "*** (skg (vnid 0f) mentionerFolder)"
    "**** (skg (vnid 0g) node (gnid y) (skgRepos (nodeRepo main)) (view (rels (linksTo (out 1 (ancestors 2)))) (birth (linksTo out 2))) (diff (rel added) (properties changed) (leaves 1 node))) y2"
    "see [[id:a][a]]"
    "***** (skg (vnid 0w) (oldText titleAndBody) (diff (leaves 1))) y"
    "****** (skg (vnid 0x) bodyDiff)"
    "+see [[id:a][a]]"
    "*** (skg (vnid 0h) forksFolder)"
    "**** (skg (vnid 0i) node (gnid s) (skgRepos (nodeRepo main)) writeProtected (view (rels (forks (out 1 (ancestors 2)))) (birth (forks out 2))) (diff (rel added) (leaves 1 rel))) s"
    "* (skg (vnid 0j) node (gnid r2) (skgRepos (nodeRepo main)) (view (rels (contains (out 1))))) r2"
    "** (skg (vnid 0k) (changesetMarker content) (diff (leaves 1 rel)))"
    "** (skg (vnid 0l) node (gnid a) (skgRepos (nodeRepo main)) writeProtected (view (rels (contains (in 2 (ancestors 1))) (linksTo (in 1 (substantive 1))) (forks (in 1))) (birth (contains in 1))) (diff (rel added) (leaves 1 rel))) a"
    "* (skg (vnid 0m) node (gnid s) (skgRepos (nodeRepo main)) (view (rels (forks (out 1))))) s"
    "** (skg (vnid 0n) forksFolder)"
    "*** (skg (vnid 0o) node (gnid a) (skgRepos (nodeRepo main)) writeProtected (view (rels (contains (in 2) (out 0 (unintegrated 0))) (linksTo (in 1 (substantive 1))) (forks (in 1 (ancestors 2)))) (birth (forks in 2))) (diff (rel added) (leaves 1 rel))) a"
    "**** (skg (vnid 0p) ghost (gnid m) (skgRepos (nodeRepo main)) (diff (node removed) (rel removed))) m"
    "***** (skg (vnid 0q) node (gnid a) (skgRepos (nodeRepo main)) writeProtected (view (rels (contains (in 2)) (linksTo (in 1 (substantive 1))) (forks (in 1))) ghostWarrant cycle) (diff (rel removed) (leaves 1 rel))) a"
    "**** (skg (vnid 0r) node (gnid r2) (skgRepos (nodeRepo main)) writeProtected (view (rels (contains (out 1 (ancestors 1)))))) r2"
    "**** (skg (vnid 0s) node (gnid x) (skgRepos (nodeRepo main)) writeProtected (view (rels (contains (out 1 (ancestors 1)))))) x"
    "* (skg (vnid 0t) node (gnid y) (skgRepos (nodeRepo main)) writeProtected (view omittedBody (rels (linksTo (out 1)))) (diff (properties changed) (leaves 1 node))) y2")
  "The same view after C-c s e on y in a's mentioners folder (line 7),
as the server drew it: y's old text (line 9) now stands only there,
inside the folder, and y's root (line 24) is write-protected.")

(defun test-skg-processed--visited-lines (lines anywhere)
  "The lines of LINES a motion between changes visits: every one, when
ANYWHERE, a capital's, and otherwise a plain one's."
  (with-temp-buffer
    (org-mode)
    (insert (mapconcat #'identity lines "\n") "\n")
    (goto-char (point-min))
    (let ((visited '()))
      (while (not (eobp))
        (when (skg--diffView-change-start-on-line nil anywhere)
          (push (line-number-at-pos) visited))
        (forward-line 1))
      (nreverse visited))))

(defun test-skg-processed--skipped-changes-are-drawn-editable-p (lines)
  "Check, on the diff view LINES, the property the plain motions
promise in a view extended to every change: each headline with change
facts that C-c r n c skips is a ☮ member of a folder of other files'
relationships, and its node stands editable elsewhere in the buffer,
where the plain motion visits what it names: when it names 'node', a
headline there or under it; when it names 'rel', a headline under it
standing for the folder's viewparent, the relationship's other end.
Returns the skipped lines."
  (let* ((visited (test-skg-processed--visited-lines lines nil))
         (skipped (seq-difference
                   (test-skg-processed--visited-lines lines t) visited)))
    (with-temp-buffer
      (org-mode)
      (insert (mapconcat #'identity lines "\n") "\n")
      (dolist (line skipped)
        (goto-char (point-min)) (forward-line (1- line))
        (let* ((sexp (skg--metadata-sexp-at-point-or-nil))
               (gnid (format "%s" (cadr (assq 'gnid (cdr sexp)))))
               (objects (cdr (skg--processed-leaves sexp)))
               (partner (save-excursion
                          (org-up-heading-safe) (org-up-heading-safe)
                          (format "%s" (cadr (assq 'gnid (cdr (skg--metadata-sexp-at-point-or-nil))))))))
          (should (skg--diffView-change-of-other-files-p sexp))
          (goto-char (point-min))
          (should
           (cl-loop
            while (re-search-forward
                   (format "^\\*+ (skg .*(gnid %s)" (regexp-quote gnid)) nil t)
            thereis
            (save-excursion
              (beginning-of-line)
              (let ((occurrence (skg--metadata-sexp-at-point-or-nil))
                    (start (line-number-at-pos))
                    (end (save-excursion (org-end-of-subtree t t)
                                         (line-number-at-pos))))
                (and (skg--occurrence-sexp-p occurrence)
                     (not (skg--node-write-protected-p occurrence))
                     (or (not (memq 'node objects))
                         (seq-some (lambda (v) (and (>= v start) (< v end)))
                                   visited))
                     (or (not (memq 'rel objects))
                         (seq-some
                          (lambda (v)
                            (and (> v start) (< v end)
                                 (string-match-p
                                  (format "(gnid %s)" (regexp-quote partner))
                                  (nth (1- v) lines))))
                          visited)))))))))
      skipped)))

(ert-deftest test-skg-diffView-plain-motions-skip-members-of-other-files-folders ()
  "C-c r n c and C-c r p c skip the ☮ members of a's containers,
mentioners and forks folders (s there forking a alone); the capitals visit them."
  (let ((lines test-skg-processed--complete-diff-view))
    (should (equal (test-skg-processed--goto-changes
                    lines 1 #'skg-diffView-goto-next-change 8)
                   '(2 11 12 15 17 20 22 "No further change")))
    (should (equal (test-skg-processed--goto-changes
                    lines 1 #'skg-diffView-goto-next-change-anywhere 11)
                   '(2 4 7 9 11 12 15 17 20 22 "No further change")))
    (should (equal (test-skg-processed--goto-changes
                    lines nil #'skg-diffView-goto-previous-change 8)
                   '(22 20 17 15 12 11 2 1)))
    (should (equal (test-skg-processed--goto-changes
                    lines nil #'skg-diffView-goto-previous-change-anywhere 4)
                   '(22 20 17 15)))))

(ert-deftest test-skg-diffView-plain-motions-reach-every-change-of-a-complete-view ()
  "Jeff's requirement: in a view extended to every change, the plain
motion reaches each change. Where a member stands write-protected in a
folder of other files, its change is drawn where its node is editable;
where a member was made editable there (C-c s e), it and its old text
are visited inside the folder."
  (should (equal (test-skg-processed--skipped-changes-are-drawn-editable-p
                  test-skg-processed--complete-diff-view)
                 '(4 7 9)))
  (let ((lines test-skg-processed--diff-view-with-an-editable-member))
    (should (equal (test-skg-processed--skipped-changes-are-drawn-editable-p
                    lines)
                   '(4 13)))
    (should (equal (test-skg-processed--visited-lines lines nil)
                   '(1 2 7 9 15 16 19 21 24)))))

(ert-deftest test-skg-diffView-plain-motions-visit-ghosts-and-restricted-members ()
  "Only a write-protected occurrence is skipped. A ghost in a folder of
other files is visited: a deleted hider's hide stands only there. A
restricted member is visited: its relationship's recorder cannot be
drawn editable. A member of the forks folder that the viewparent
forks (no birth of forking it alone) is visited."
  (let ((lines
         '("* (skg node (gnid x) (skgRepos (nodeRepo main))) x"
           "** (skg hiderFolder)"
           "*** (skg ghost (gnid g) (skgRepos (nodeRepo main)) (diff (node removed) (rel removed) (leaves 2 node rel))) g"
           "** (skg containerFolder)"
           "*** (skg restricted (gnid h) (diff (rel added) (leaves 1 rel)))"
           "*** (skg node (gnid c) (skgRepos (nodeRepo main)) writeProtected (diff (rel added) (leaves 1 rel))) c"
           "** (skg forksFolder)"
           "*** (skg node (gnid e) (skgRepos (nodeRepo main)) writeProtected (diff (rel added) (leaves 1 rel))) e")))
    (should (equal (test-skg-processed--goto-changes
                    lines 1 #'skg-diffView-goto-next-change 4)
                   '(3 5 8 "No further change")))
    (should (equal (test-skg-processed--goto-changes
                    lines 1 #'skg-diffView-goto-next-change-anywhere 4)
                   '(3 5 6 8)))))

(ert-deftest test-skg-diffView-no-change-message-names-the-capital-key ()
  "When a plain motion finds nothing but the capital would, it says so
and names the capital's key."
  (let ((lines (seq-take test-skg-processed--complete-diff-view 9)))
    (should (equal (test-skg-processed--goto-changes
                    lines 3 #'skg-diffView-goto-next-change 1)
                   '("No further change but ☮ ones in \"(other files)\" folders; C-c r n C visits them")))
    (should (equal (test-skg-processed--goto-changes
                    lines 3 #'skg-diffView-goto-next-unprocessed-change 1)
                   '("No further unprocessed change but ☮ ones in \"(other files)\" folders; C-c r n U visits them")))
    (should (equal (test-skg-processed--goto-changes
                    lines 3 #'skg-diffView-goto-next-processed-change 1)
                   '("No further processed change")))
    (should (equal (test-skg-processed--goto-changes
                    (list (nth 2 lines) (nth 3 lines)) nil
                    #'skg-diffView-goto-previous-change 1)
                   '("No earlier change but ☮ ones in \"(other files)\" folders; C-c r p C visits them")))))

(ert-deftest test-skg-diffView-other-files-folders-are-those-labelled-so ()
  "'folder_heralds.other_files' names exactly the folders whose
changed label says \"(other files)\"."
  (let* ((folder-heralds (alist-get 'folder_heralds skg-shared-herald-styles))
         (labelled (seq-filter
                    (lambda (entry)
                      (string-suffix-p "(other files)" (cdr entry)))
                    (alist-get 'changed_labels folder-heralds))))
    (should (equal (sort (mapcar (lambda (e) (symbol-name (car e))) labelled)
                         #'string<)
                   (sort (copy-sequence (alist-get 'other_files folder-heralds))
                         #'string<)))))

(ert-deftest test-skg-diffView-capital-motion-keybindings ()
  (dolist (pair '(("C-c r n C" . skg-diffView-goto-next-change-anywhere)
                  ("C-c r n U" . skg-diffView-goto-next-unprocessed-change-anywhere)
                  ("C-c r n P" . skg-diffView-goto-next-processed-change-anywhere)
                  ("C-c r p C" . skg-diffView-goto-previous-change-anywhere)
                  ("C-c r p U" . skg-diffView-goto-previous-unprocessed-change-anywhere)
                  ("C-c r p P" . skg-diffView-goto-previous-processed-change-anywhere)))
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
