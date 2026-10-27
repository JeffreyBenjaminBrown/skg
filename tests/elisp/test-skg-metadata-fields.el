;;; test-skg-metadata-fields.el --- Tests for skg-metadata-fields

(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             (file-name-directory load-file-name)))
(require 'ert)
(require 'skg-metadata-fields)
(require 'skg-shared)

(defconst test--node-kind (skg-shared-metadata-kind "node")
  "The occurrence's kind from 'shared/metadata-fields.json'.")

;;
;; skg-metadata-fields-kind-of-sexp
;;

(ert-deftest test-kind-of-sexp ()
  "A metadata sexp's kind is that of its first atom the table names,
bare or heading a list, after any marker; the occurrence's when it
names none; nil for what is no '(skg ...)'."
  (dolist (case '(((skg node (gnid abc) (skgRepos (nodeRepo jeff))) . "occurrence")
                  ((skg focused node (gnid abc)) . "occurrence")
                  ((skg ghost (gnid g) (skgRepos (nodeRepo r))) . "ghost")
                  ((skg alias (skgRepos (relRepo r))) . "alias")
                  ((skg (flag noSearchMatching)) . "flag")
                  ((skg subscribeeFolder) . "folder")
                  ((skg folded tombstone) . "tombstone")
                  ((skg (relRepo r)) . "occurrence")
                  ((skg) . "occurrence")
                  ((foo) . nil)))
    (should (equal (alist-get 'kind (skg-metadata-fields-kind-of-sexp
                                     (car case)))
                   (cdr case)))))

;;
;; skg-headlines-to-org
;;

(ert-deftest test-headlines-to-org ()
  "Converts headline list to org text."
  (let ((headlines '((1 . "skg") (2 . "node") (3 . "id") (4 . "abc"))))
    (should (equal (skg-headlines-to-org headlines)
                   "* skg\n** node\n*** id\n**** abc"))))

;;
;; Fill in: minimal sexp (id + repo only) -> all defaults appear
;;

(ert-deftest test-fillIn-minimal-sexp ()
  "Filling in a minimal occurrence inserts all default fields."
  (let* ((sexp '(skg node (gnid abc) (skgRepos (nodeRepo jeff))))
         (org-text (sexp-to-org sexp))
         (filledIn (skg-metadata-fields-fillIn-defaults-in-org org-text test--node-kind))
         (lines (split-string filledIn "\n"))
         (headlines (org-to-sexp--extract-headlines lines)))
    ;; Should have: skg, node, gnid : abc, skgRepos : nodeRepo : jeff,
    ;; write-protected : false (default), noClaim : false (default),
    ;; editRequest : none (default), viewRequests : none (default)
    (should (= (length headlines) 15)) ;; each key *and each value* is a separate headline
    ;; Check default fields are present
    (should (cl-find "writeProtected" headlines
                     :key #'cdr :test #'string=))
    (should (cl-find "false (default)" headlines
                     :key #'cdr :test #'string=))
    (should (cl-find "editRequest" headlines
                     :key #'cdr :test #'string=))
    (should (cl-find "none (default)" headlines
                     :key #'cdr :test #'string=))))

;;
;; Fill in: sexp with 'writeProtected' present -> shows 'true' child
;;

(ert-deftest test-fillIn-with-write-protected ()
  "Filling in a sexp with bare writeProtected shows 'true' child."
  (let* ((sexp '(skg node (gnid abc) (skgRepos (nodeRepo jeff)) writeProtected))
         (org-text (sexp-to-org sexp))
         (filledIn (skg-metadata-fields-fillIn-defaults-in-org org-text test--node-kind))
         (lines (split-string filledIn "\n"))
         (headlines (org-to-sexp--extract-headlines lines))
         (write-protected-idx (cl-position "writeProtected" headlines
                                 :key #'cdr :test #'string=)))
    ;; The next headline after write-protected should be "true"
    (should write-protected-idx)
    (should (string= (cdr (nth (1+ write-protected-idx) headlines)) "true"))))

;;
;; Fill in: a member (no noClaim atom) -> inserts 'false (default)';
;; a child claiming nothing has the bare atom -> shows 'true'
;;

(ert-deftest test-fillIn-noClaim ()
  "Filling in offers noClaim as a bare atom: 'false (default)' on a
member, 'true' on a child claiming nothing."
  (dolist (case '(((skg node (gnid abc) (skgRepos (nodeRepo jeff))) . "false (default)")
                  ((skg node (gnid abc) (skgRepos (nodeRepo jeff)) noClaim) . "true")))
    (let* ((org-text (sexp-to-org (car case)))
           (filledIn (skg-metadata-fields-fillIn-defaults-in-org org-text test--node-kind))
           (lines (split-string filledIn "\n"))
           (headlines (org-to-sexp--extract-headlines lines))
           (noClaim-idx (cl-position "noClaim" headlines
                                       :key #'cdr :test #'string=)))
      (should noClaim-idx)
      (should (string= (cdr (nth (1+ noClaim-idx) headlines))
                       (cdr case))))))

;;
;; Strip: unmodified filled-in org -> returns original sexp
;;

(ert-deftest test-strip-unmodified-returns-original ()
  "Stripping an unmodified filled-in org returns the original sexp."
  (let* ((sexp '(skg node (gnid abc) (skgRepos (nodeRepo jeff))))
         (org-text (sexp-to-org sexp))
         (filledIn (skg-metadata-fields-fillIn-defaults-in-org org-text test--node-kind))
         (stripped (skg-metadata-fields-strip-defaults-from-org filledIn test--node-kind))
         (result (org-to-sexp stripped)))
    (should (equal result sexp))))

;;
;; Strip: after cycling write-protected to true -> sexp has bare 'writeProtected'
;;

(ert-deftest test-strip-write-protected-true ()
  "Stripping writeProtected=true produces bare atom in sexp."
  (let* ((org-text (concat "* skg\n"
                           "** node\n"
                           "** gnid\n"
                           "*** abc\n"
                           "** skgRepos\n"
                           "*** nodeRepo\n"
                           "**** jeff\n"
                           "** writeProtected\n"
                           "*** true\n"
                           "** noClaim\n"
                           "*** false (default)\n"
                           "** editRequest\n"
                           "*** none (default)\n"
                           "** viewRequests\n"
                           "*** none (default)"))
         (stripped (skg-metadata-fields-strip-defaults-from-org org-text test--node-kind))
         (result (org-to-sexp stripped)))
    (should (equal result '(skg node (gnid abc) (skgRepos (nodeRepo jeff)) writeProtected)))))

;;
;; Strip: accepts bare 'false' (without '(default)') as the default value
;;

(ert-deftest test-strip-bare-false ()
  "Stripping accepts bare 'false' as the default value."
  (let* ((org-text (concat "* skg\n"
                           "** node\n"
                           "** gnid\n"
                           "*** abc\n"
                           "** skgRepos\n"
                           "*** nodeRepo\n"
                           "**** jeff\n"
                           "** writeProtected\n"
                           "*** false\n"
                           "** noClaim\n"
                           "*** false\n"
                           "** editRequest\n"
                           "*** none\n"
                           "** viewRequests\n"
                           "*** none"))
         (stripped (skg-metadata-fields-strip-defaults-from-org org-text test--node-kind))
         (result (org-to-sexp stripped)))
    (should (equal result '(skg node (gnid abc) (skgRepos (nodeRepo jeff)))))))

;;
;; Round-trip: fill in then strip with no edits = identity
;;

(ert-deftest test-round-trip-minimal ()
  "Round-trip: fill in then strip on minimal sexp is identity."
  (let* ((sexp '(skg node (gnid abc) (skgRepos (nodeRepo jeff))))
         (org-text (sexp-to-org sexp))
         (filledIn (skg-metadata-fields-fillIn-defaults-in-org org-text test--node-kind))
         (stripped (skg-metadata-fields-strip-defaults-from-org filledIn test--node-kind))
         (result (org-to-sexp stripped)))
    (should (equal result sexp))))

(ert-deftest test-round-trip-with-write-protected ()
  "Round-trip: fill in then strip preserves bare writeProtected."
  (let* ((sexp '(skg node (gnid abc) (skgRepos (nodeRepo jeff)) writeProtected))
         (org-text (sexp-to-org sexp))
         (filledIn (skg-metadata-fields-fillIn-defaults-in-org org-text test--node-kind))
         (stripped (skg-metadata-fields-strip-defaults-from-org filledIn test--node-kind))
         (result (org-to-sexp stripped)))
    (should (equal result sexp))))

(ert-deftest test-round-trip-with-editrequest-delete ()
  "Round-trip: fill in then strip preserves (editRequest delete)."
  (let* ((sexp '(skg node (gnid abc) (skgRepos (nodeRepo jeff)) (editRequest delete)))
         (org-text (sexp-to-org sexp))
         (filledIn (skg-metadata-fields-fillIn-defaults-in-org org-text test--node-kind))
         (stripped (skg-metadata-fields-strip-defaults-from-org filledIn test--node-kind))
         (result (org-to-sexp stripped)))
    (should (equal result sexp))))

(ert-deftest test-round-trip-with-editrequest-merge ()
  "Round-trip: fill in then strip preserves (editRequest (merge XYZ))."
  (let* ((sexp '(skg node (gnid abc) (skgRepos (nodeRepo jeff)) (editRequest (merge XYZ))))
         (org-text (sexp-to-org sexp))
         (filledIn (skg-metadata-fields-fillIn-defaults-in-org org-text test--node-kind))
         (stripped (skg-metadata-fields-strip-defaults-from-org filledIn test--node-kind))
         (result (org-to-sexp stripped)))
    (should (equal result sexp))))

;;
;; Ordering: every part where the server writes it ('parts' in
;; 'shared/metadata-fields.json'), a missing field among them
;;

(ert-deftest test-canonical-ordering ()
  "Parts appear in the order the server writes them after filling in."
  (let* ((sexp '(skg node (skgRepos (nodeRepo jeff)) (view cycle) (gnid abc) writeProtected))
         (org-text (sexp-to-org sexp))
         (filledIn (skg-metadata-fields-fillIn-defaults-in-org org-text test--node-kind))
         (lines (split-string filledIn "\n"))
         (headlines (org-to-sexp--extract-headlines lines))
         (level-2 (mapcar #'cdr
                          (cl-remove-if-not
                           (lambda (hl) (= (car hl) 2))
                           headlines))))
    (should (equal level-2
                   '("node" "gnid" "skgRepos" "noClaim" "writeProtected"
                     "editRequest" "viewRequests" "view")))))

;;
;; Strip: editRequest merge with org link
;;

(ert-deftest test-strip-editrequest-merge-with-link ()
  "Strip handles merge with org link [[id:XYZ][label]]."
  (let* ((org-text (concat "* skg\n"
                           "** node\n"
                           "** gnid\n"
                           "*** abc\n"
                           "** skgRepos\n"
                           "*** nodeRepo\n"
                           "**** jeff\n"
                           "** editRequest\n"
                           "*** merge [[id:XYZ][some label]]"))
         (stripped (skg-metadata-fields-strip-defaults-from-org org-text test--node-kind))
         (result (org-to-sexp stripped)))
    (should (equal result
                   '(skg node (gnid abc) (skgRepos (nodeRepo jeff))
                         (editRequest (merge XYZ)))))))

;;
;; Strip: noClaim true -> the bare atom
;;

(ert-deftest test-strip-noClaim-true ()
  "Stripping noClaim=true produces the bare atom in the sexp."
  (let* ((org-text (concat "* skg\n"
                           "** node\n"
                           "** gnid\n"
                           "*** abc\n"
                           "** skgRepos\n"
                           "*** nodeRepo\n"
                           "**** jeff\n"
                           "** noClaim\n"
                           "*** true"))
         (stripped (skg-metadata-fields-strip-defaults-from-org org-text test--node-kind))
         (result (org-to-sexp stripped)))
    (should (equal result
                   '(skg node (gnid abc) (skgRepos (nodeRepo jeff)) noClaim)))))

;;
;; Strip: viewRequests with actual values -> kept as-is
;;

(ert-deftest test-strip-viewrequests-kept ()
  "Stripping viewRequests with actual values keeps them."
  (let* ((org-text (concat "* skg\n"
                           "** node\n"
                           "** gnid\n"
                           "*** abc\n"
                           "** skgRepos\n"
                           "*** nodeRepo\n"
                           "**** jeff\n"
                           "** viewRequests\n"
                           "*** folder\n"
                           "**** aliases\n"
                           "*** folder\n"
                           "**** containers"))
         (stripped (skg-metadata-fields-strip-defaults-from-org org-text test--node-kind))
         (result (org-to-sexp stripped)))
    (should (equal result
                   '(skg node (gnid abc) (skgRepos (nodeRepo jeff))
                         (viewRequests (folder aliases) (folder containers)))))))

;;
;; Strip: the empty-node view skeleton (childless editable fields)
;; drops every unpopulated field, keeping only the pre-filled skgrepo.
;;

(ert-deftest test-strip-empty-node-skeleton ()
  "Stripping the all-childless skeleton keeps only the repo field.
Every editable field -- including viewRequests -- is childless, so each
is dropped key-and-all, leaving (skg node (skgRepos (nodeRepo only)))."
  (let* ((org-text (concat "* skg\n"
                           "** node\n"
                           "** skgRepos\n"
                           "*** nodeRepo\n"
                           "**** only\n"
                           "** writeProtected\n"
                           "** noClaim\n"
                           "** editRequest\n"
                           "** viewRequests"))
         (stripped (skg-metadata-fields-strip-defaults-from-org org-text test--node-kind))
         (result (org-to-sexp stripped)))
    (should (equal result '(skg node (skgRepos (nodeRepo only)))))))

(ert-deftest test-strip-viewrequests-childless-dropped ()
  "A childless viewRequests field is dropped, not kept as a bare atom."
  (let* ((org-text (concat "* skg\n"
                           "** node\n"
                           "** skgRepos\n"
                           "*** nodeRepo\n"
                           "**** only\n"
                           "** viewRequests"))
         (stripped (skg-metadata-fields-strip-defaults-from-org org-text test--node-kind))
         (result (org-to-sexp stripped)))
    (should (equal result '(skg node (skgRepos (nodeRepo only)))))))

(ert-deftest test-strip-drops-a-repo-without-a-value ()
  "Stripping drops a skgrepo given no value, and a skgRepos left empty."
  (dolist (case '(("**** bob\n*** relRepo" . (skg node (gnid abc) (skgRepos (nodeRepo bob))))
                  ("" . (skg node (gnid abc)))))
    (should (equal (org-to-sexp
                    (skg-metadata-fields-strip-defaults-from-org
                     (concat "* skg\n** node\n** gnid\n*** abc\n"
                             "** skgRepos\n*** nodeRepo\n" (car case))
                     test--node-kind))
                   (cdr case)))))

(ert-deftest test-strip-repo-bare-value ()
  "Stripping repo without '(default)' keeps the value as-is."
  (let* ((org-text (concat "* skg\n"
                           "** node\n"
                           "** gnid\n"
                           "*** abc\n"
                           "** skgRepos\n"
                           "*** nodeRepo\n"
                           "**** bob"))
         (stripped (skg-metadata-fields-strip-defaults-from-org org-text test--node-kind))
         (result (org-to-sexp stripped)))
    (should (equal result '(skg node (gnid abc) (skgRepos (nodeRepo bob)))))))

;;
;; Bug: skg-view-metadata was passing hardcoded "* skg\n** node"
;; instead of the actual sexp-to-org output, discarding all fields.
;; These tests verify filling in works correctly on real metadata sexps.
;;

(ert-deftest test-fillIn-preserves-existing-repo ()
  "Filling in a sexp that already has repo preserves it."
  (let* ((sexp '(skg node (gnid abc) (skgRepos (nodeRepo public))
                     (rels "C5")))
         (org-text (sexp-to-org sexp))
         (filledIn (skg-metadata-fields-fillIn-defaults-in-org org-text test--node-kind))
         (lines (split-string filledIn "\n"))
         (headlines (org-to-sexp--extract-headlines lines)))
    ;; Repo field must be present
    (should (cl-find "nodeRepo" headlines :key #'cdr :test #'string=))
    ;; Repo value must be "public" (not missing)
    (should (cl-find "public" headlines :key #'cdr :test #'string=))
    ;; ID must be preserved
    (should (cl-find "gnid" headlines :key #'cdr :test #'string=))
    (should (cl-find "abc" headlines :key #'cdr :test #'string=))
    ;; the rels herald must be preserved (as non-canonical, appended at end)
    (should (cl-find "rels" headlines :key #'cdr :test #'string=))))

(ert-deftest test-fillIn-preserves-repo-round-trip ()
  "Filling in then stripping a sexp with id + repo is identity. (The
sexp<->org bijection is symbol-based, so the display-only herald strings
-- which are stripped before this path anyway -- are not round-tripped
here; see test-round-trip-with-links-in-herald's removal.)"
  (let* ((sexp '(skg node (gnid abc) (skgRepos (nodeRepo public))))
         (org-text (sexp-to-org sexp))
         (filledIn (skg-metadata-fields-fillIn-defaults-in-org org-text test--node-kind))
         (stripped (skg-metadata-fields-strip-defaults-from-org filledIn test--node-kind))
         (result (org-to-sexp stripped)))
    (should (equal result sexp))))

(ert-deftest test-fillIn-real-world-metadata ()
  "Filling in a real-world metadata sexp (from a bug report)
preserves repo and all fields."
  (let* ((sexp '(skg node (gnid 6972d099)
                     (skgRepos (nodeRepo public))
                     (rels "C5 4(1,1)L")))
         (org-text (sexp-to-org sexp))
         (filledIn (skg-metadata-fields-fillIn-defaults-in-org org-text test--node-kind))
         (lines (split-string filledIn "\n"))
         (headlines (org-to-sexp--extract-headlines lines)))
    ;; Repo must be present
    (should (cl-find "nodeRepo" headlines :key #'cdr :test #'string=))
    (should (cl-find "public" headlines :key #'cdr :test #'string=))
    ;; All editable defaults must be present
    (should (cl-find "writeProtected" headlines :key #'cdr :test #'string=))
    (should (cl-find "noClaim" headlines :key #'cdr :test #'string=))
    (should (cl-find "editRequest" headlines :key #'cdr :test #'string=))
    ;; the rels herald must be preserved
    (should (cl-find "rels" headlines :key #'cdr :test #'string=))))

;; (Removed test-round-trip-with-links-in-herald: it checked that a
;; herald string survives the sexp<->org bijection. The new herald
;; tokens are quoted strings -- often with parens/spaces, e.g. "3(1)L" --
;; which the symbol-based bijection cannot represent. Heralds are
;; display-only and stripped (skg-strip-heralds-from-sexp) before any
;; save/edit path that uses the bijection, so this round-trip no longer
;; applies.)

;;
;; Display-only title group
;;

(ert-deftest test-fillIn-prepends-display-title ()
  "Filling in with a display title prepends the title group."
  (let* ((sexp '(skg node (gnid abc) (skgRepos (nodeRepo jeff))))
         (org-text (sexp-to-org sexp))
         (filledIn
          (skg-metadata-fields-fillIn-defaults-in-org
           org-text test--node-kind "actual title"))
         (lines (split-string filledIn "\n"))
         (headlines (org-to-sexp--extract-headlines lines)))
    (should (equal (cl-subseq headlines 0 4)
                   '((1 . "title")
                     (2 . "actual title")
                     (1 . "skg")
                     (2 . "node"))))))

(ert-deftest test-fillIn-with-empty-display-title-does-not-prepend ()
  "Filling in with an empty display title leaves metadata first."
  (let* ((sexp '(skg node (gnid abc) (skgRepos (nodeRepo jeff))))
         (org-text (sexp-to-org sexp))
         (filledIn
          (skg-metadata-fields-fillIn-defaults-in-org org-text test--node-kind ""))
         (lines (split-string filledIn "\n"))
         (headlines (org-to-sexp--extract-headlines lines)))
    (should (equal (car headlines) '(1 . "skg")))))

(ert-deftest test-strip-removes-display-title ()
  "Stripping removes the display-only title group before conversion."
  (let* ((sexp '(skg node (gnid abc) (skgRepos (nodeRepo jeff))))
         (org-text (sexp-to-org sexp))
         (filledIn
          (skg-metadata-fields-fillIn-defaults-in-org
           org-text test--node-kind "actual title"))
         (stripped (skg-metadata-fields-strip-defaults-from-org filledIn test--node-kind))
         (result (org-to-sexp stripped)))
    (should (equal result sexp))))

;;
;; Other kinds, values with arguments, conditions
;;

(ert-deftest test-fillIn-and-strip-a-top-level-field ()
  "An alias's field sits beside its atom, after the markers and the
atom; filling in shows it at its default and stripping drops it."
  (let* ((sexp '(skg focused alias))
         (kind (skg-metadata-fields-kind-of-sexp sexp))
         (filledIn (skg-metadata-fields-fillIn-defaults-in-org
                    (sexp-to-org sexp) kind)))
    (should (equal (org-to-sexp--extract-headlines
                    (split-string filledIn "\n"))
                   '((1 . "skg") (2 . "focused") (2 . "alias")
                     (2 . "noClaim") (3 . "false (default)")
                     (2 . "editRequest") (3 . "none (default)"))))
    (should (equal (org-to-sexp (skg-metadata-fields-strip-defaults-from-org
                                 filledIn kind))
                   sexp))))

(ert-deftest test-fillIn-leaves-a-kind-without-fields-alone ()
  "A tombstone has no field a user may set: filling in and stripping
change nothing."
  (let* ((sexp '(skg folded tombstone))
         (kind (skg-metadata-fields-kind-of-sexp sexp))
         (org-text (sexp-to-org sexp)))
    (should (equal (skg-metadata-fields-fillIn-defaults-in-org org-text kind)
                   org-text))
    (should (equal (org-to-sexp (skg-metadata-fields-strip-defaults-from-org
                                 org-text kind))
                   sexp))))

(ert-deftest test-strip-writes-values-with-arguments-as-lists ()
  "A value typed with its arguments on one line saves as a list; a
link typed as an id saves as its id; a default among several values
is dropped."
  (let* ((org-text (concat "* skg\n"
                           "** node\n"
                           "** gnid\n"
                           "*** abc\n"
                           "** editRequest\n"
                           "*** relRepo r\n"
                           "** viewRequests\n"
                           "*** editableView\n"
                           "*** none (default)\n"
                           "*** folder containers"))
         (stripped (skg-metadata-fields-strip-defaults-from-org
                    org-text test--node-kind)))
    (should (equal (org-to-sexp stripped)
                   '(skg node (gnid abc)
                         (editRequest (relRepo r))
                         (viewRequests editableView
                                       (folder containers)))))
    (should (equal (org-to-sexp
                    (skg-metadata-fields-strip-defaults-from-org
                     "* skg\n** node\n** editRequest\n*** merge [[id:x][a title]]"
                     test--node-kind))
                   '(skg node (editRequest (merge x)))))
    (should (equal (org-to-sexp
                    (skg-metadata-fields-strip-defaults-from-org
                     "* skg\n** node\n** editRequest\n*** flag noSearchMatching false"
                     test--node-kind))
                   '(skg node (editRequest (flag noSearchMatching false)))))))

(ert-deftest test-cycle-values-honor-the-conditions ()
  "A value offered only to an editable headline, or only to a removed
member, is left out elsewhere."
  (should (equal (skg-metadata-fields-cycle-values
                  test--node-kind "editRequest" nil nil)
                 '("none (default)" "relRepo")))
  (should (equal (skg-metadata-fields-cycle-values
                  test--node-kind "editRequest" t t)
                 '("none (default)" "delete" "merge" "relRepo" "flag"
                   "restoreRel")))
  (should (equal (skg-metadata-fields-cycle-values
                  test--node-kind "writeProtected" nil nil)
                 '("false (default)" "true")))
  (should-not (skg-metadata-fields-cycle-values
               test--node-kind "repo" t nil)))

(provide 'test-skg-metadata-fields)
