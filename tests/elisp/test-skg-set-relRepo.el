;;; test-skg-set-relRepo.el --- Tests for skg-set-relRepo
;;;
;;; skg-set-relRepo (C-c s r; formerly
;;; skg-privatize-relationship, see
;;; BUG-and-fix_make-edge-more-public.org) classifies the relationship the
;;; headline at point represents, asks the server for that relationship's
;;; (default, current) relRepos, and offers the skgrepos at least
;;; as private as the default plus a no-override choice. These tests
;;; cover the pure pieces (relationship classification, menu slicing, choice
;;; application) and the response handler with the network and
;;; minibuffer stubbed out.

(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             (file-name-directory load-file-name)))
(require 'ert)
(require 'org)
(require 'skg-buffer)
(require 'skg-metadata)
(require 'skg-config)
(require 'skg-request-relRepo-info)

(defvar test--config-public-private-trusted
  (concat "[[repos]]\n"
          "name = \"public\"\n"
          "path = \"owned/public\"\n"
          "\n"
          "[[repos]]\n"
          "name = \"private\"\n"
          "path = \"owned/private\"\n"
          "\n"
          "[[repos]]\n"
          "name = \"trusted\"\n"
          "path = \"owned/trusted\"\n"
          "")
  "Config text with three owned repos, in this declared order:
public, private, trusted. (The names don't need to reflect an actual
privacy order for these tests -- only that `skg--repo-names'
returns them in this config order, which is all the client-side menu
depends on; the server enforces the real floor at save.)")

(defun test--with-skg-content-view (org-text config-text body-fn)
  "Run BODY-FN in a temp skg content-view buffer with ORG-TEXT.
CONFIG-TEXT is written to a temporary skgconfig.toml so that
skg-config-dir is set and `skg--repo-names' works."
  (let* ((config-dir (make-temp-file "skg-test-config" t))
         (config-file (expand-file-name "skgconfig.toml" config-dir))
         (skg-config-dir (file-name-as-directory config-dir)))
    (with-temp-file config-file
      (insert config-text))
    (unwind-protect
        (with-temp-buffer
          (insert org-text)
          (skg-content-view-mode)
          (goto-char (point-min))
          (funcall body-fn))
      (delete-file config-file)
      (delete-directory config-dir))))

(defun test--buffer-line (n)
  "Return the text of line N (1-indexed) of the current buffer."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- n))
    (buffer-substring-no-properties
     (line-beginning-position) (line-end-position))))

;; --- Relationship classification: skg--rel-at-point ---

(ert-deftest test-rel-content-child ()
  "A content child's relationship: recorder = viewparent, relation = contains."
  (test--with-skg-content-view
   (concat
    "* (skg (node (id recorder) (repo public))) recorder\n"
    "** (skg (node (id kid) (repo public))) kid\n")
   test--config-public-private-trusted
   (lambda ()
     (goto-char (point-min))
     (search-forward "(id kid)" nil t)
     (beginning-of-line)
     (should (equal (skg--rel-at-point)
                    '(:recorder "recorder" :member "kid"
                      :relation "contains"))))))

(ert-deftest test-rel-writable-folder-member ()
  "A subscribeeFolder member's relationship: recorder = the folder's ANCHOR (its
viewparent), relation = the folder's relation."
  (test--with-skg-content-view
   (concat
    "* (skg (node (id anchor) (repo public))) anchor\n"
    "** (skg subscribeeFolder)\n"
    "*** (skg (node (id seen) (repo public) writeProtected)) seen\n")
   test--config-public-private-trusted
   (lambda ()
     (goto-char (point-min))
     (search-forward "(id seen)" nil t)
     (beginning-of-line)
     (should (equal (skg--rel-at-point)
                    '(:recorder "anchor" :member "seen"
                      :relation "subscribesTo"))))))

(ert-deftest test-rel-write-protected-child-of-editable-parent ()
  "A write-protected child's relationship belongs to its editable parent,
so it can be set from the child."
  (test--with-skg-content-view
   (concat
    "* (skg (node (id recorder) (repo public))) recorder\n"
    "** (skg (node (id kid) (repo public) writeProtected)) kid\n")
   test--config-public-private-trusted
   (lambda ()
     (goto-char (point-min))
     (search-forward "(id kid)" nil t)
     (beginning-of-line)
     (should (equal (skg--rel-at-point)
                    '(:recorder "recorder" :member "kid"
                      :relation "contains"))))))

(ert-deftest test-rel-refuses-under-write-protected-parent ()
  "Refuses (user-error) under a write-protected parent, which the save
would not let write the relationship."
  (test--with-skg-content-view
   (concat
    "* (skg (node (id recorder) (repo public) writeProtected)) recorder\n"
    "** (skg (node (id kid) (repo public))) kid\n")
   test--config-public-private-trusted
   (lambda ()
     (goto-char (point-min))
     (search-forward "(id kid)" nil t)
     (beginning-of-line)
     (let ((err (should-error (skg--rel-at-point)
                              :type 'user-error)))
       (should (string-match-p "write-protected" (cadr err)))))))

(ert-deftest test-rel-refuses-on-write-protected-folder-member ()
  "Refuses (user-error) on a member of a write-protected folder."
  (test--with-skg-content-view
   (concat
    "* (skg (node (id recorder) (repo public))) recorder\n"
    "** (skg subscriberFolder)\n"
    "*** (skg (node (id sub) (repo public))) sub\n")
   test--config-public-private-trusted
   (lambda ()
     (goto-char (point-min))
     (search-forward "(id sub)" nil t)
     (beginning-of-line)
     (let ((err (should-error (skg--rel-at-point)
                              :type 'user-error)))
       (should (string-match-p "write-protected" (cadr err)))
       (should (string-match-p "subscriberFolder" (cadr err)))))))

(ert-deftest test-rel-refuses-on-root ()
  "Refuses on a root headline: with no viewparent there is no relationship."
  (test--with-skg-content-view
   "* (skg (node (id x) (repo public))) x\n"
   test--config-public-private-trusted
   (lambda ()
     (goto-char (point-min))
     (let ((err (should-error (skg--rel-at-point)
                              :type 'user-error)))
       (should (string-match-p "Root headline" (cadr err)))))))

(ert-deftest test-rel-refuses-off-unrestrictedNode ()
  "Refuses on a non-vognode headline (no (node ...) form)."
  (test--with-skg-content-view
   (concat
    "* (skg (node (id recorder) (repo public))) recorder\n"
    "** (skg aliasFolder) aliases\n")
   test--config-public-private-trusted
   (lambda ()
     (goto-char (point-min))
     (search-forward "(skg aliasFolder)" nil t)
     (beginning-of-line)
     (should-error (skg--rel-at-point)
                   :type 'user-error))))

;; --- Menu slicing: skg--relRepo-choices ---

(ert-deftest test-relRepo-choices-slices-at-default ()
  "The menu is the ladder's tail from the default onward."
  (should (equal (skg--relRepo-choices
                  '("public" "private" "trusted") "private")
                 '("private" "trusted")))
  (should (equal (skg--relRepo-choices
                  '("public" "private" "trusted") "public")
                 '("public" "private" "trusted")))
  (should (equal (skg--relRepo-choices
                  '("public" "private" "trusted") "trusted")
                 '("trusted"))))

(ert-deftest test-relRepo-choices-full-ladder-fallback ()
  "With no default (or one na from the ladder), the whole ladder
is offered; the server's save-time floor check backstops."
  (should (equal (skg--relRepo-choices
                  '("public" "private") nil)
                 '("public" "private")))
  (should (equal (skg--relRepo-choices
                  '("public" "private") "unknown")
                 '("public" "private"))))

;; --- Choice application: skg--apply-relRepo-choice ---

(ert-deftest test-apply-relRepo-sets-atom ()
  "Choosing a repo writes the (relRepo NAME) atom."
  (test--with-skg-content-view
   "* (skg (node (id x) (repo public))) x\n"
   test--config-public-private-trusted
   (lambda ()
     (goto-char (point-min))
     (skg--apply-relRepo-choice "trusted")
     (should (string-match-p "(relRepo trusted)"
                             (test--buffer-line 1))))))

(ert-deftest test-apply-relRepo-preserves-existing-viewstats ()
  "Setting relRepo must not disturb a pre-existing viewStats sibling."
  (test--with-skg-content-view
   "* (skg (node (id x) (repo public) (viewStats cycle))) x\n"
   test--config-public-private-trusted
   (lambda ()
     (goto-char (point-min))
     (skg--apply-relRepo-choice "public")
     (should (string-match-p "cycle" (test--buffer-line 1)))
     (should (string-match-p "(relRepo public)"
                             (test--buffer-line 1))))))

(ert-deftest test-apply-relRepo-removes-override ()
  "The no-override choice removes only a pending request, preserving
the displayed relRepo fact; its message says the SAVED skgrepo survives
(sticky), not that anything resets to the default."
  (test--with-skg-content-view
   "* (skg (node (id x) (repo public) (viewStats (relRepo private)) (editRequest (relRepo trusted)))) x\n"
   test--config-public-private-trusted
   (lambda ()
     (goto-char (point-min))
     (let ((msg (skg--apply-relRepo-choice
                 skg--relRepo-no-override)))
       (should (string-match-p "sticky" msg))
       (should-not (string-match-p "editRequest" (test--buffer-line 1)))
       (should (string-match-p "(viewStats (relRepo private))"
                               (test--buffer-line 1)))))))

(ert-deftest test-apply-relRepo-remove-without-atom-is-noop ()
  "The no-override choice without an atom changes nothing."
  (test--with-skg-content-view
   "* (skg (node (id x) (repo public))) x\n"
   test--config-public-private-trusted
   (lambda ()
     (goto-char (point-min))
     (let ((before (test--buffer-line 1))
           (msg (skg--apply-relRepo-choice
                 skg--relRepo-no-override)))
       (should (string-match-p "nothing to remove" msg))
       (should (equal (test--buffer-line 1) before))))))

(ert-deftest test-apply-relRepo-on-alias-uses-flat-metadata ()
  "Alias relRepo intent is a flat non-vognode editRequest, not node viewStats."
  (test--with-skg-content-view
   (concat
    "* (skg (node (id recorder) (repo public))) recorder\n"
    "** (skg aliasFolder)\n"
    "*** (skg alias) nickname\n")
   test--config-public-private-trusted
   (lambda ()
     (goto-char (point-min))
     (forward-line 2)
     (skg--apply-relRepo-choice "trusted")
     (should (string-match-p
              "(skg alias (editRequest (relRepo trusted)))"
              (test--buffer-line 3)))
     (should-not (string-match-p "viewStats" (test--buffer-line 3)))
     (should (equal (skg--relRepo-requested-value) "trusted"))
     (skg--apply-relRepo-choice
      skg--relRepo-no-override)
     (should (equal (test--buffer-line 3)
                    "*** (skg alias) nickname")))))

(ert-deftest test-apply-relRepo-on-unknown-keeps-fact-separate ()
  "An Unknown content member stores intent under its own editRequest."
  (test--with-skg-content-view
   (concat
    "* (skg (node (id recorder) (repo public))) recorder\n"
    "** (skg (unknown (id na) (viewStats (relRepo private))))\n")
   test--config-public-private-trusted
   (lambda ()
     (goto-char (point-min))
     (forward-line 1)
     (skg--apply-relRepo-choice "trusted")
     (should (string-match-p
              "(unknown (id na) (viewStats (relRepo private)) (editRequest (relRepo trusted)))"
              (test--buffer-line 2)))
     (skg--apply-relRepo-choice
      skg--relRepo-no-override)
     (should (string-match-p "(viewStats (relRepo private))"
                             (test--buffer-line 2)))
     (should-not (string-match-p "editRequest" (test--buffer-line 2))))))

(ert-deftest test-recursive-relRepo-preflights-edit-conflicts ()
  "A delete/merge target aborts the recursive operation before any write."
  (test--with-skg-content-view
   (concat
    "* (skg (node (id recorder) (repo public))) recorder\n"
    "** (skg (node (id a) (repo public))) a\n"
    "** (skg (node (id b) (repo public) (editRequest delete))) b\n")
   test--config-public-private-trusted
   (lambda ()
     (goto-char (point-min))
     (should-error
      (skg--set-relRepo-recursive-walk 'content "trusted"))
     (should-not (string-match-p "relRepo" (test--buffer-line 2))))))

(ert-deftest test-alias-command-derives-default-locally ()
  "The alias gesture uses its owning node's home without an relationship-info request."
  (test--with-skg-content-view
   (concat
    "* (skg (node (id recorder) (repo private))) recorder\n"
    "** (skg aliasFolder)\n"
    "*** (skg alias) nickname\n")
   test--config-public-private-trusted
   (lambda ()
     (goto-char (point-min))
     (forward-line 2)
     (let (seen-choices)
       (cl-letf (((symbol-function 'run-at-time)
                  (lambda (_secs _repeat fn &rest args) (apply fn args)))
                 ((symbol-function 'completing-read)
                  (lambda (_prompt choices &rest _)
                    (setq seen-choices choices)
                    "trusted"))
                 ((symbol-function 'process-send-string)
                  (lambda (&rest _)
                    (ert-fail "alias command must not contact relationship endpoint"))))
         (skg--set-relRepo-at-point))
       (should (equal seen-choices
                      (list "private" "trusted"
                            skg--relRepo-no-override)))
       (should (string-match-p "(relRepo trusted)"
                               (test--buffer-line 3)))))))

;; --- The response handler, network and minibuffer stubbed ---

(defun test--run-info-handler (payload choice-fn)
  "Run `skg--set-relRepo-from-info' on PAYLOAD against
the buffer at point, with `run-at-time' made synchronous and
`completing-read' (which `skg--completing-read-with-cycle' wraps)
stubbed by CHOICE-FN, which receives (PROMPT CHOICES PREFILL) --
PREFILL being the initial minibuffer contents -- and returns the
choice."
  (let ((buffer (current-buffer))
        (marker (point-marker)))
    (cl-letf (((symbol-function 'run-at-time)
               (lambda (_secs _repeat fn &rest args) (apply fn args)))
              ((symbol-function 'completing-read)
               (lambda (prompt choices &optional _pred _req init
                        _hist _def _inherit)
                 (funcall choice-fn prompt choices init))))
      (skg--set-relRepo-from-info buffer marker payload))))

(ert-deftest test-relRepo-handler-slices-and-applies ()
  "A (default, current) reply offers the slice from the default plus
the no-override entry, pre-fills the minibuffer with the current
repo, and applies the selection."
  (test--with-skg-content-view
   (concat
    "* (skg (node (id recorder) (repo public))) recorder\n"
    "** (skg (node (id kid) (repo public))) kid\n")
   test--config-public-private-trusted
   (lambda ()
     (goto-char (point-min))
     (search-forward "(id kid)" nil t)
     (beginning-of-line)
     (let (seen-choices seen-prefill)
       (test--run-info-handler
        "((response-type relRepo-info) (default \"private\") (current \"trusted\"))"
        (lambda (_prompt choices prefill)
          (setq seen-choices choices
                seen-prefill prefill)
          "private"))
       (should (equal seen-choices
                      (list "private" "trusted"
                            skg--relRepo-no-override)))
       (should (equal seen-prefill "trusted"))
       (should (string-match-p "(relRepo private)"
                               (test--buffer-line 2)))))))

(ert-deftest test-relRepo-handler-more-public-current-prefills-default ()
  "A legacy CURRENT more public than the default is not among the
choices, so the prompt pre-fills with the default instead."
  (test--with-skg-content-view
   (concat
    "* (skg (node (id recorder) (repo public))) recorder\n"
    "** (skg (node (id kid) (repo public))) kid\n")
   test--config-public-private-trusted
   (lambda ()
     (goto-char (point-min))
     (search-forward "(id kid)" nil t)
     (beginning-of-line)
     (let (seen-prefill)
       (test--run-info-handler
        "((response-type relRepo-info) (default \"private\") (current \"public\"))"
        (lambda (_prompt _choices prefill)
          (setq seen-prefill prefill)
          skg--relRepo-no-override))
       (should (equal seen-prefill "private"))))))

(ert-deftest test-relRepo-handler-error-offers-full-ladder ()
  "An error reply falls back to the full ladder (plus no-override)."
  (test--with-skg-content-view
   (concat
    "* (skg (node (id recorder) (repo public))) recorder\n"
    "** (skg (node (id kid) (repo public))) kid\n")
   test--config-public-private-trusted
   (lambda ()
     (goto-char (point-min))
     (search-forward "(id kid)" nil t)
     (beginning-of-line)
     (let (seen-choices)
       (test--run-info-handler
        "((response-type relRepo-info) (error \"member 'kid' is not in the graph\"))"
        (lambda (_prompt choices _def)
          (setq seen-choices choices)
          skg--relRepo-no-override))
       (should (equal seen-choices
                      (list "public" "private" "trusted"
                            skg--relRepo-no-override)))
       (should-not (string-match-p "relRepo"
                                   (test--buffer-line 2)))))))

;; --- The recursive walk: skg--set-relRepo-recursive-walk ---

(defvar test--recursive-content-tree
  (concat
   "* (skg (node (id r) (repo public))) r\n"
   "** (skg (node (id a) (repo public))) a\n"
   "*** (skg (node (id b) (repo public))) b\n"
   "** (skg (node (id c) (repo public) (affectsParent false))) c\n"
   "*** (skg (node (id d) (repo public))) d\n"
   "** (skg (node (id e) (repo public) writeProtected)) e\n"
   "*** (skg (node (id f) (repo public))) f\n"
   "** (skg subscribeeFolder)\n"
   "*** (skg (node (id g) (repo public))) g\n"
   "**** (skg (node (id h) (repo public))) h\n"
   "** (skg aliasFolder) aliases\n")
  "A view-root tree exercising the walk's qualification and pruning:
true content (a, b), an false branch (c, d), an
write-protected-but-true member (e) over content (f), a
subscribeeFolder member (g) over subscribee-as-such content (h), and an
aliasFolder.")

(defun test--line-of-id (skgid)
  "Return the text of the buffer line whose metadata carries ID."
  (save-excursion
    (goto-char (point-min))
    (search-forward (format "(id %s)" skgid))
    (buffer-substring-no-properties
     (line-beginning-position) (line-end-position))))

(ert-deftest test-recursive-walk-content ()
  "Kind `content' hits true content children of editable
unrestrictedNode parents only: the root's own (na) relationship is skipped, the
false branch and everything below the write-protected node and the
subscribee-as-such member are pruned, and folder members are untouched."
  (test--with-skg-content-view
   test--recursive-content-tree
   test--config-public-private-trusted
   (lambda ()
     (goto-char (point-min))
     (let ((count (skg--set-relRepo-recursive-walk
                   'content "trusted")))
       (should (= count 3)) ;; a, b, e
       (dolist (skgid '("a" "b" "e"))
         (should (string-match-p "(relRepo trusted)"
                                 (test--line-of-id skgid))))
       (dolist (skgid '("r" "c" "d" "f" "g" "h"))
         (should-not (string-match-p "relRepo"
                                     (test--line-of-id skgid))))))))

(ert-deftest test-recursive-walk-subscribee ()
  "Kind `subscribee' hits only subscribeeFolder members, leaving content
children and subscribee-as-such content untouched."
  (test--with-skg-content-view
   test--recursive-content-tree
   test--config-public-private-trusted
   (lambda ()
     (goto-char (point-min))
     (let ((count (skg--set-relRepo-recursive-walk
                   'subscribee "private")))
       (should (= count 1)) ;; g
       (should (string-match-p "(relRepo private)"
                               (test--line-of-id "g")))
       (dolist (skgid '("r" "a" "b" "c" "d" "e" "f" "h"))
         (should-not (string-match-p "relRepo"
                                     (test--line-of-id skgid))))))))

(ert-deftest test-recursive-walk-root-relationship-inclusive ()
  "Starting the walk below the view root includes the start node's
own relationship to its view-parent."
  (test--with-skg-content-view
   test--recursive-content-tree
   test--config-public-private-trusted
   (lambda ()
     (goto-char (point-min))
     (search-forward "(id a)")
     (beginning-of-line)
     (let ((count (skg--set-relRepo-recursive-walk
                   'content "trusted")))
       (should (= count 2)) ;; a and b
       (should (string-match-p "(relRepo trusted)"
                               (test--line-of-id "a")))
       (should (string-match-p "(relRepo trusted)"
                               (test--line-of-id "b")))))))

(ert-deftest test-recursive-walk-overridden-and-member-content ()
  "An overriddenFolder member matches kind `overridden'; the member's
own content children (the member being editable) match kind
`content' through the folder."
  (let ((tree (concat
               "* (skg (node (id anchor) (repo public))) anchor\n"
               "** (skg overriddenFolder)\n"
               "*** (skg (node (id o) (repo public))) o\n"
               "**** (skg (node (id oc) (repo public))) oc\n")))
    (test--with-skg-content-view
     tree test--config-public-private-trusted
     (lambda ()
       (goto-char (point-min))
       (should (= 1 (skg--set-relRepo-recursive-walk
                     'overridden "private")))
       (should (string-match-p "(relRepo private)"
                               (test--line-of-id "o")))
       (should-not (string-match-p "relRepo"
                                   (test--line-of-id "oc")))))
    (test--with-skg-content-view
     tree test--config-public-private-trusted
     (lambda ()
       (goto-char (point-min))
       (should (= 1 (skg--set-relRepo-recursive-walk
                     'content "private")))
       (should (string-match-p "(relRepo private)"
                               (test--line-of-id "oc")))
       (should-not (string-match-p "relRepo"
                                   (test--line-of-id "o")))))))

(ert-deftest test-recursive-walk-prunes-write-protected-folder ()
  "A write-protected folder's whole branch is pruned: even a editable
member's content children are not reached."
  (test--with-skg-content-view
   (concat
    "* (skg (node (id recorder) (repo public))) recorder\n"
    "** (skg subscriberFolder)\n"
    "*** (skg (node (id s) (repo public))) s\n"
    "**** (skg (node (id sc) (repo public))) sc\n")
   test--config-public-private-trusted
   (lambda ()
     (goto-char (point-min))
     (should (= 0 (skg--set-relRepo-recursive-walk
                   'content "trusted")))
     (dolist (skgid '("s" "sc"))
       (should-not (string-match-p "relRepo"
                                   (test--line-of-id skgid)))))))

(ert-deftest test-recursive-walk-write-protected-folder-anchor ()
  "A writable folder under an WRITE_PROTECTED anchor is not collected at
save, so its members do not match -- whether the walk starts at the
anchor (pruned below the write-protected node) or at the folder itself
(refused by the anchor-editableness check)."
  (let ((tree (concat
               "* (skg (node (id anchor) (repo public) writeProtected)) anchor\n"
               "** (skg subscribeeFolder)\n"
               "*** (skg (node (id g) (repo public))) g\n")))
    (test--with-skg-content-view
     tree test--config-public-private-trusted
     (lambda ()
       (goto-char (point-min))
       (should (= 0 (skg--set-relRepo-recursive-walk
                     'subscribee "trusted")))
       (should-not (string-match-p "relRepo"
                                   (test--line-of-id "g")))))
    (test--with-skg-content-view
     tree test--config-public-private-trusted
     (lambda ()
       (goto-char (point-min))
       (search-forward "subscribeeFolder")
       (beginning-of-line)
       (should (= 0 (skg--set-relRepo-recursive-walk
                     'subscribee "trusted")))
       (should-not (string-match-p "relRepo"
                                   (test--line-of-id "g")))))))

(ert-deftest test-recursive-walk-removes-overrides ()
  "The no-override choice removes pending repo requests throughout
the subtree, while preserving display facts."
  (test--with-skg-content-view
   (concat
    "* (skg (node (id r) (repo public))) r\n"
    "** (skg (node (id a) (repo public) (viewStats (relRepo trusted)) (editRequest (relRepo trusted)))) a\n"
    "*** (skg (node (id b) (repo public) (viewStats (relRepo private)) (editRequest (relRepo private)))) b\n")
   test--config-public-private-trusted
   (lambda ()
     (goto-char (point-min))
     (should (= 2 (skg--set-relRepo-recursive-walk
                   'content skg--relRepo-no-override)))
     (dolist (skgid '("a" "b"))
       (should-not (string-match-p "editRequest"
                                   (test--line-of-id skgid)))))))

;; --- The kind menu: skg--select-relationship-kind ---

(ert-deftest test-relationship-kind-menu-settable-roles ()
  "The menu tree offers exactly the three writable kinds, one per
writable position, and covers all five schema relations."
  (should (equal (mapcar #'car (skg--relationship-kind-menu-tree))
                 '("contains" "linksTo" "subscribesTo"
                   "hidesFromSubs" "overrides")))
  (should (equal (delq nil
                       (mapcar (lambda (role) (nth 1 role))
                               (apply #'append
                                      (mapcar #'cdr
                                              (skg--relationship-kind-menu-tree)))))
                 '(content subscribee overridden))))

(defun test--choose-menu-role (role-line)
  "In the relationship-kind menu buffer, move to ROLE-LINE and choose it."
  (with-current-buffer "*skg-relationship-kinds*"
    (goto-char (point-min))
    (search-forward role-line)
    (beginning-of-line)
    (skg--relationship-kind-menu-choose)))

(ert-deftest test-relationship-kind-menu-choose ()
  "RET on a settable role calls the continuation with its kind; RET
on a write-protected role refuses; RET on a relation (level-1) headline
does neither."
  (unwind-protect
      (progn
        (let (chosen)
          (cl-letf (((symbol-function 'pop-to-buffer)
                     (lambda (buffer &rest _) (set-buffer buffer))))
            (skg--select-relationship-kind
             (lambda (kind) (setq chosen kind))))
          (with-current-buffer "*skg-relationship-kinds*"
            (goto-char (point-min))
            (search-forward "* subscribes")
            (beginning-of-line)
            (skg--relationship-kind-menu-choose) ;; level-1: a no-op
            (should-not chosen)
            (should-error (test--choose-menu-role "** subscriber")
                          :type 'user-error))
          (test--choose-menu-role "** content")
          (should (eq chosen 'content))
          ;; Choosing killed the menu buffer.
          (should-not (get-buffer "*skg-relationship-kinds*"))))
    (when (get-buffer "*skg-relationship-kinds*")
      (kill-buffer "*skg-relationship-kinds*"))))

(ert-deftest test-set-relRepo-recursive-end-to-end ()
  "The full command: menu choice, repo prompt, walk. Point and
window plumbing are stubbed as in the other handler tests."
  (test--with-skg-content-view
   test--recursive-content-tree
   test--config-public-private-trusted
   (lambda ()
     (goto-char (point-min))
     (let ((view-buffer (current-buffer)))
       (unwind-protect
           (progn
             (cl-letf (((symbol-function 'pop-to-buffer)
                        (lambda (buffer &rest _) (set-buffer buffer)))
                       ((symbol-function 'completing-read)
                        (lambda (&rest _) "trusted")))
               (skg-set-relRepo-recursive)
               (test--choose-menu-role "** content"))
             (with-current-buffer view-buffer
               (dolist (skgid '("a" "b" "e"))
                 (should (string-match-p "(relRepo trusted)"
                                         (test--line-of-id skgid))))
               (should-not (string-match-p "relRepo"
                                           (test--line-of-id "g")))))
         (when (get-buffer "*skg-relationship-kinds*")
           (kill-buffer "*skg-relationship-kinds*")))))))

;; --- set-repo stuck-relationship offer and write-protected warning ---
;; (Here rather than in test-skg-metadata.el because these need the
;; config harness: the stuck-relationship analysis reads the privacy ladder.
;; In test--config-public-private-trusted the order is public,
;; private, trusted -- so private -> public publicizes.)

(defun test--messages-during (fn)
  "Run FN with `message' captured; return (RESULT . MESSAGES)."
  (let (messages)
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args)
                 (when fmt
                   (push (apply #'format fmt args) messages))
                 nil)))
      (cons (funcall fn) (nreverse messages)))))

(ert-deftest test-set-repo-recursive-offers-stuck-relationship-fix ()
  "A publicizing recursive move detects the content relationships it would
leave behind, and on acceptance writes (relRepo NEW) atoms on them."
  (test--with-skg-content-view
   (concat
    "* (skg (node (id r) (repo private))) r\n"
    "** (skg (node (id a) (repo private))) a\n"
    "*** (skg (node (id b) (repo private))) b\n")
   test--config-public-private-trusted
   (lambda ()
     (goto-char (point-min))
     (let (offer-prompt)
       (cl-letf (((symbol-function 'skg--prompt-for-repo-change)
                  (lambda (_current) "public"))
                 ((symbol-function 'y-or-n-p)
                  (lambda (prompt) (setq offer-prompt prompt) t)))
         (let ((msgs (cdr (test--messages-during
                           (lambda () (skg-set-repo t))))))
           (should (string-match-p "2 content relationships" offer-prompt))
           (should (seq-find (lambda (m)
                               (string-match-p "Also publicized 2" m))
                             msgs))))
       (dolist (skgid '("r" "a" "b"))
         (should (string-match-p "(repo public)"
                                 (test--line-of-id skgid))))
       (dolist (skgid '("a" "b"))
         (should (string-match-p "(relRepo public)"
                                 (test--line-of-id skgid))))
       (should-not (string-match-p "relRepo"
                                   (test--line-of-id "r")))))))

(ert-deftest test-set-repo-recursive-decline-mentions-recursive-relrepo ()
  "Declining the stuck-relationship offer leaves the relationships alone and points
at C-c s R."
  (test--with-skg-content-view
   (concat
    "* (skg (node (id r) (repo private))) r\n"
    "** (skg (node (id a) (repo private))) a\n")
   test--config-public-private-trusted
   (lambda ()
     (goto-char (point-min))
     (cl-letf (((symbol-function 'skg--prompt-for-repo-change)
                (lambda (_current) "public"))
               ((symbol-function 'y-or-n-p)
                (lambda (_prompt) nil)))
       (let ((msgs (cdr (test--messages-during
                         (lambda () (skg-set-repo t))))))
         (should (seq-find (lambda (m) (string-match-p "C-c s R" m))
                           msgs))))
     (should-not (string-match-p "relRepo" (test--line-of-id "a"))))))

(ert-deftest test-set-repo-recursive-no-offer-when-privatizing ()
  "A privatizing move strands nothing (relationships rise automatically), so
no offer is made."
  (test--with-skg-content-view
   (concat
    "* (skg (node (id r) (repo public))) r\n"
    "** (skg (node (id a) (repo public))) a\n")
   test--config-public-private-trusted
   (lambda ()
     (goto-char (point-min))
     (cl-letf (((symbol-function 'skg--prompt-for-repo-change)
                (lambda (_current) "trusted"))
               ((symbol-function 'y-or-n-p)
                (lambda (_prompt)
                  (error "Should not offer a stuck-relationship fix"))))
       (test--messages-during (lambda () (skg-set-repo t))))
     (should (string-match-p "(repo trusted)" (test--line-of-id "a")))
     (should-not (string-match-p "relRepo" (test--line-of-id "a"))))))

(ert-deftest test-set-repo-recursive-warns-about-write-protected ()
  "A write-protected matching instance is NOT edited; its ID goes to
*Messages* and the summary carries a loud WARNING. Relationships touching
it are not offered (they cannot actually publicize)."
  (test--with-skg-content-view
   (concat
    "* (skg (node (id r) (repo private))) r\n"
    "** (skg (node (id e) (repo private) writeProtected)) e\n"
    "*** (skg (node (id f) (repo private))) f\n")
   test--config-public-private-trusted
   (lambda ()
     (goto-char (point-min))
     (cl-letf (((symbol-function 'skg--prompt-for-repo-change)
                (lambda (_current) "public"))
               ((symbol-function 'y-or-n-p)
                (lambda (_prompt)
                  (error "Should not offer: both relationships touch the write-protected node"))))
       (let ((msgs (cdr (test--messages-during
                         (lambda () (skg-set-repo t))))))
         (should (seq-find (lambda (m)
                             (and (string-match-p "NOT changed" m)
                                  (string-match-p ": e" m)))
                           msgs))
         (should (seq-find (lambda (m)
                             (and (string-match-p "WARNING" m)
                                  (string-match-p "\\*Messages\\*" m)))
                           msgs))))
     (should (string-match-p "(repo private)" (test--line-of-id "e")))
     (dolist (skgid '("r" "f"))
       (should (string-match-p "(repo public)"
                               (test--line-of-id skgid)))))))

(ert-deftest test-set-repo-recursive-repeat-follows-its-editable-occurrence ()
  "A write-protected repeat of a node whose editable occurrence the move
changes is left as rendered (the save ignores it) and draws no warning:
the node does move."
  (test--with-skg-content-view
   (concat
    "* (skg (node (id r) (repo private))) r\n"
    "** (skg (node (id a) (repo private))) a\n"
    "*** (skg (node (id x) (repo private))) x\n"
    "** (skg (node (id b) (repo private))) b\n"
    "*** (skg (node (id x) (repo private) writeProtected)) x again\n")
   test--config-public-private-trusted
   (lambda ()
     (goto-char (point-min))
     (cl-letf (((symbol-function 'skg--prompt-for-repo-change)
                (lambda (_current) "trusted")))
       (let ((msgs (cdr (test--messages-during
                         (lambda () (skg-set-repo t))))))
         (should-not (seq-find (lambda (m)
                                 (or (string-match-p "NOT changed" m)
                                     (string-match-p "WARNING" m)))
                               msgs))))
     (should (string-match-p "(repo trusted)" (test--line-of-id "x")))
     (should (string-match-p
              "(repo private) writeProtected"
              (save-excursion
                (goto-char (point-min))
                (search-forward "x again")
                (buffer-substring (line-beginning-position)
                                  (line-end-position))))))))

(ert-deftest test-set-repo-single-publicizes-a-public-parents-child-relationship ()
  "Moving a private child into public offers its public parent's relationship too."
  (test--with-skg-content-view
   (concat
    "* (skg (node (id parent) (repo public))) parent\n"
    "** (skg (node (id child) (repo private))) child\n")
   test--config-public-private-trusted
   (lambda ()
     (goto-char (point-min))
     (search-forward "(id child)")
     (beginning-of-line)
     (cl-letf (((symbol-function 'skg--prompt-for-repo-change)
                (lambda (_current) "public"))
               ((symbol-function 'y-or-n-p)
                (lambda (prompt)
                  (should (string-match-p "1 content relationship " prompt))
                  t)))
       (test--messages-during (lambda () (skg-set-repo))))
     (should (string-match-p "(repo public)" (test--line-of-id "child")))
     (should (string-match-p "(relRepo public)"
                             (test--line-of-id "child"))))))

(ert-deftest test-set-repo-single-offers-direct-child-relationships ()
  "A single (non-recursive) publicizing move offers only the relationships it
actually changes: its direct children's inbound relationships whose default
rises. A child still more private than the new repo is left alone."
  (test--with-skg-content-view
   (concat
    "* (skg (node (id r) (repo private))) r\n"
    "** (skg (node (id a) (repo public))) a\n"
    "** (skg (node (id c) (repo trusted))) c\n")
   test--config-public-private-trusted
   (lambda ()
     (goto-char (point-min))
     (cl-letf (((symbol-function 'skg--prompt-for-repo-change)
                (lambda (_current) "public"))
               ((symbol-function 'y-or-n-p)
                (lambda (prompt)
                  (should (string-match-p "1 content relationship "
                                          prompt))
                  t)))
       (test--messages-during (lambda () (skg-set-repo))))
     (should (string-match-p "(repo public)" (test--line-of-id "r")))
     ;; a's relationship default rose private->public; c's stayed trusted.
     (should (string-match-p "(relRepo public)" (test--line-of-id "a")))
     (should-not (string-match-p "relRepo" (test--line-of-id "c")))
     (should (string-match-p "(repo trusted)" (test--line-of-id "c"))))))

(ert-deftest test-set-repo-single-write-protected-warns-and-skips ()
  "A single move of a write-protected instance edits nothing and warns."
  (test--with-skg-content-view
   "* (skg (node (id x) (repo private) writeProtected)) x\n"
   test--config-public-private-trusted
   (lambda ()
     (goto-char (point-min))
     (cl-letf (((symbol-function 'skg--prompt-for-repo-change)
                (lambda (_current) "public")))
       (let ((msgs (cdr (test--messages-during
                         (lambda () (skg-set-repo))))))
         (should (seq-find (lambda (m) (string-match-p "WARNING" m))
                           msgs))))
     (should (string-match-p "(repo private)" (test--line-of-id "x")))
     (should-not (string-match-p "(repo public)"
                                 (test--line-of-id "x"))))))

(provide 'test-skg-set-relRepo)
