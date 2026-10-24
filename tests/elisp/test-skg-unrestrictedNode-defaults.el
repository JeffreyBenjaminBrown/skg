;;; test-skg-unrestrictedNode-defaults.el --- Tests for skg-unrestrictedNode-defaults

(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             (file-name-directory load-file-name)))
(require 'ert)
(require 'skg-unrestrictedNode-defaults)

;;
;; skg-unrestrictedNode-sexp-p
;;

(ert-deftest test-unrestrictedNode-sexp-p-positive ()
  "Recognizes an UnrestrictedVognode sexp."
  (should (skg-unrestrictedNode-sexp-p '(skg (node (id abc) (repo jeff))))))

(ert-deftest test-unrestrictedNode-sexp-p-negative-not-skg ()
  "Rejects non-skg sexp."
  (should-not (skg-unrestrictedNode-sexp-p '(foo (node (id abc))))))

(ert-deftest test-unrestrictedNode-sexp-p-negative-not-node ()
  "Rejects skg sexp without node."
  (should-not (skg-unrestrictedNode-sexp-p '(skg (alias (id abc))))))

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
  "Filling in a minimal UnrestrictedVognode inserts all default fields."
  (let* ((sexp '(skg (node (id abc) (repo jeff))))
         (org-text (sexp-to-org sexp))
         (filledIn (skg-unrestrictedNode-fillIn-defaults-in-org org-text))
         (lines (split-string filledIn "\n"))
         (headlines (org-to-sexp--extract-headlines lines)))
    ;; Should have: skg, node, id : abc, repo : jeff,
    ;; write-protected : false (default), nonMember : false (default),
    ;; editRequest : none (default), viewRequests : none (default)
    (should (= (length headlines) 14)) ;; each key *and each value* is a separate headline
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
  (let* ((sexp '(skg (node (id abc) (repo jeff) writeProtected)))
         (org-text (sexp-to-org sexp))
         (filledIn (skg-unrestrictedNode-fillIn-defaults-in-org org-text))
         (lines (split-string filledIn "\n"))
         (headlines (org-to-sexp--extract-headlines lines))
         (write-protected-idx (cl-position "writeProtected" headlines
                                 :key #'cdr :test #'string=)))
    ;; The next headline after write-protected should be "true"
    (should write-protected-idx)
    (should (string= (cdr (nth (1+ write-protected-idx) headlines)) "true"))))

;;
;; Fill in: a member (no nonMember atom) -> inserts 'false (default)';
;; a non-member's bare atom -> shows 'true'
;;

(ert-deftest test-fillIn-nonMember ()
  "Filling in offers nonMember as a bare atom: 'false (default)' on a
member, 'true' on a non-member."
  (dolist (case '(((skg (node (id abc) (repo jeff))) . "false (default)")
                  ((skg (node (id abc) (repo jeff) nonMember)) . "true")))
    (let* ((org-text (sexp-to-org (car case)))
           (filledIn (skg-unrestrictedNode-fillIn-defaults-in-org org-text))
           (lines (split-string filledIn "\n"))
           (headlines (org-to-sexp--extract-headlines lines))
           (nonMember-idx (cl-position "nonMember" headlines
                                       :key #'cdr :test #'string=)))
      (should nonMember-idx)
      (should (string= (cdr (nth (1+ nonMember-idx) headlines))
                       (cdr case))))))

;;
;; Strip: unmodified filled-in org -> returns original sexp
;;

(ert-deftest test-strip-unmodified-returns-original ()
  "Stripping an unmodified filled-in org returns the original sexp."
  (let* ((sexp '(skg (node (id abc) (repo jeff))))
         (org-text (sexp-to-org sexp))
         (filledIn (skg-unrestrictedNode-fillIn-defaults-in-org org-text))
         (stripped (skg-unrestrictedNode-strip-defaults-from-org filledIn))
         (result (org-to-sexp stripped)))
    (should (equal result sexp))))

;;
;; Strip: after cycling write-protected to true -> sexp has bare 'writeProtected'
;;

(ert-deftest test-strip-write-protected-true ()
  "Stripping writeProtected=true produces bare atom in sexp."
  (let* ((org-text (concat "* skg\n"
                           "** node\n"
                           "*** id\n"
                           "**** abc\n"
                           "*** repo\n"
                           "**** jeff\n"
                           "*** writeProtected\n"
                           "**** true\n"
                           "*** nonMember\n"
                           "**** false (default)\n"
                           "*** editRequest\n"
                           "**** none (default)\n"
                           "*** viewRequests\n"
                           "**** none (default)"))
         (stripped (skg-unrestrictedNode-strip-defaults-from-org org-text))
         (result (org-to-sexp stripped)))
    (should (equal result '(skg (node (id abc) (repo jeff) writeProtected))))))

;;
;; Strip: accepts bare 'false' (without '(default)') as the default value
;;

(ert-deftest test-strip-bare-false ()
  "Stripping accepts bare 'false' as the default value."
  (let* ((org-text (concat "* skg\n"
                           "** node\n"
                           "*** id\n"
                           "**** abc\n"
                           "*** repo\n"
                           "**** jeff\n"
                           "*** writeProtected\n"
                           "**** false\n"
                           "*** nonMember\n"
                           "**** false\n"
                           "*** editRequest\n"
                           "**** none\n"
                           "*** viewRequests\n"
                           "**** none"))
         (stripped (skg-unrestrictedNode-strip-defaults-from-org org-text))
         (result (org-to-sexp stripped)))
    (should (equal result '(skg (node (id abc) (repo jeff)))))))

;;
;; Round-trip: fill in then strip with no edits = identity
;;

(ert-deftest test-round-trip-minimal ()
  "Round-trip: fill in then strip on minimal sexp is identity."
  (let* ((sexp '(skg (node (id abc) (repo jeff))))
         (org-text (sexp-to-org sexp))
         (filledIn (skg-unrestrictedNode-fillIn-defaults-in-org org-text))
         (stripped (skg-unrestrictedNode-strip-defaults-from-org filledIn))
         (result (org-to-sexp stripped)))
    (should (equal result sexp))))

(ert-deftest test-round-trip-with-write-protected ()
  "Round-trip: fill in then strip preserves bare writeProtected."
  (let* ((sexp '(skg (node (id abc) (repo jeff) writeProtected)))
         (org-text (sexp-to-org sexp))
         (filledIn (skg-unrestrictedNode-fillIn-defaults-in-org org-text))
         (stripped (skg-unrestrictedNode-strip-defaults-from-org filledIn))
         (result (org-to-sexp stripped)))
    (should (equal result sexp))))

(ert-deftest test-round-trip-with-editrequest-delete ()
  "Round-trip: fill in then strip preserves (editRequest delete)."
  (let* ((sexp '(skg (node (id abc) (repo jeff) (editRequest delete))))
         (org-text (sexp-to-org sexp))
         (filledIn (skg-unrestrictedNode-fillIn-defaults-in-org org-text))
         (stripped (skg-unrestrictedNode-strip-defaults-from-org filledIn))
         (result (org-to-sexp stripped)))
    (should (equal result sexp))))

(ert-deftest test-round-trip-with-editrequest-merge ()
  "Round-trip: fill in then strip preserves (editRequest (merge XYZ))."
  (let* ((sexp '(skg (node (id abc) (repo jeff) (editRequest (merge XYZ)))))
         (org-text (sexp-to-org sexp))
         (filledIn (skg-unrestrictedNode-fillIn-defaults-in-org org-text))
         (stripped (skg-unrestrictedNode-strip-defaults-from-org filledIn))
         (result (org-to-sexp stripped)))
    (should (equal result sexp))))

;;
;; Canonical ordering: id and repo first, editable fields, write-protected stats last
;;

(ert-deftest test-canonical-ordering ()
  "Fields appear in canonical order after filling in."
  (let* ((sexp '(skg (node (repo jeff) (graphStats 42) (id abc) writeProtected)))
         (org-text (sexp-to-org sexp))
         (filledIn (skg-unrestrictedNode-fillIn-defaults-in-org org-text))
         (lines (split-string filledIn "\n"))
         (headlines (org-to-sexp--extract-headlines lines))
         (level-3 (mapcar #'cdr
                          (cl-remove-if-not
                           (lambda (hl) (= (car hl) 3))
                           headlines))))
    ;; Order should be: id, repo, write-protected, nonMember,
    ;; editRequest, viewRequests, graphStats
    (should (equal level-3
                   '("id" "repo" "writeProtected" "nonMember"
                     "editRequest" "viewRequests" "graphStats")))))

;;
;; Strip: editRequest merge with org link
;;

(ert-deftest test-strip-editrequest-merge-with-link ()
  "Strip handles merge with org link [[id:XYZ][label]]."
  (let* ((org-text (concat "* skg\n"
                           "** node\n"
                           "*** id\n"
                           "**** abc\n"
                           "*** repo\n"
                           "**** jeff\n"
                           "*** editRequest\n"
                           "**** merge [[id:XYZ][some label]]"))
         (stripped (skg-unrestrictedNode-strip-defaults-from-org org-text))
         (result (org-to-sexp stripped)))
    (should (equal result
                   '(skg (node (id abc) (repo jeff)
                               (editRequest (merge XYZ))))))))

;;
;; Strip: nonMember true -> the bare atom
;;

(ert-deftest test-strip-nonMember-true ()
  "Stripping nonMember=true produces the bare atom in the sexp."
  (let* ((org-text (concat "* skg\n"
                           "** node\n"
                           "*** id\n"
                           "**** abc\n"
                           "*** repo\n"
                           "**** jeff\n"
                           "*** nonMember\n"
                           "**** true"))
         (stripped (skg-unrestrictedNode-strip-defaults-from-org org-text))
         (result (org-to-sexp stripped)))
    (should (equal result
                   '(skg (node (id abc) (repo jeff) nonMember))))))

;;
;; Strip: viewRequests with actual values -> kept as-is
;;

(ert-deftest test-strip-viewrequests-kept ()
  "Stripping viewRequests with actual values keeps them."
  (let* ((org-text (concat "* skg\n"
                           "** node\n"
                           "*** id\n"
                           "**** abc\n"
                           "*** repo\n"
                           "**** jeff\n"
                           "*** viewRequests\n"
                           "**** folder\n"
                           "***** aliases\n"
                           "**** folder\n"
                           "***** containers"))
         (stripped (skg-unrestrictedNode-strip-defaults-from-org org-text))
         (result (org-to-sexp stripped)))
    (should (equal result
                   '(skg (node (id abc) (repo jeff)
                               (viewRequests (folder aliases) (folder containers))))))))

;;
;; Strip: the empty-node view skeleton (childless editable fields)
;; drops every unpopulated field, keeping only the pre-filled skgrepo.
;;

(ert-deftest test-strip-empty-node-skeleton ()
  "Stripping the all-childless skeleton keeps only the repo field.
Every editable field -- including viewRequests -- is childless, so each
is dropped key-and-all, leaving (skg (node (repo only)))."
  (let* ((org-text (concat "* skg\n"
                           "** node\n"
                           "*** repo\n"
                           "**** only\n"
                           "*** writeProtected\n"
                           "*** nonMember\n"
                           "*** editRequest\n"
                           "*** viewRequests"))
         (stripped (skg-unrestrictedNode-strip-defaults-from-org org-text))
         (result (org-to-sexp stripped)))
    (should (equal result '(skg (node (repo only)))))))

(ert-deftest test-strip-viewrequests-childless-dropped ()
  "A childless viewRequests field is dropped, not kept as a bare atom."
  (let* ((org-text (concat "* skg\n"
                           "** node\n"
                           "*** repo\n"
                           "**** only\n"
                           "*** viewRequests"))
         (stripped (skg-unrestrictedNode-strip-defaults-from-org org-text))
         (result (org-to-sexp stripped)))
    (should (equal result '(skg (node (repo only)))))))

;;
;; Repo defaults
;;

(ert-deftest test-fillIn-with-default-repo-marks-matching ()
  "Filling in with default-repo marks matching repo value."
  (let* ((sexp '(skg (node (id abc) (repo jeff))))
         (org-text (sexp-to-org sexp))
         (filledIn (skg-unrestrictedNode-fillIn-defaults-in-org org-text "jeff"))
         (lines (split-string filledIn "\n"))
         (headlines (org-to-sexp--extract-headlines lines)))
    (should (cl-find "jeff (default)" headlines
                     :key #'cdr :test #'string=))))

(ert-deftest test-fillIn-with-default-repo-leaves-nonmatching ()
  "Filling in with default-repo leaves non-matching repo value bare."
  (let* ((sexp '(skg (node (id abc) (repo bob))))
         (org-text (sexp-to-org sexp))
         (filledIn (skg-unrestrictedNode-fillIn-defaults-in-org org-text "jeff"))
         (lines (split-string filledIn "\n"))
         (headlines (org-to-sexp--extract-headlines lines)))
    (should (cl-find "bob" headlines :key #'cdr :test #'string=))
    (should-not (cl-find "bob (default)" headlines
                         :key #'cdr :test #'string=))))

(ert-deftest test-fillIn-inserts-default-repo-when-missing ()
  "Filling in a node with no repo inserts default repo."
  (let* ((sexp '(skg (node (id abc))))
         (org-text (sexp-to-org sexp))
         (filledIn (skg-unrestrictedNode-fillIn-defaults-in-org org-text "jeff"))
         (lines (split-string filledIn "\n"))
         (headlines (org-to-sexp--extract-headlines lines)))
    (should (cl-find "repo" headlines :key #'cdr :test #'string=))
    (should (cl-find "jeff (default)" headlines
                     :key #'cdr :test #'string=))))

(ert-deftest test-strip-repo-with-default-suffix ()
  "Stripping repo with '(default)' suffix removes the suffix."
  (let* ((org-text (concat "* skg\n"
                           "** node\n"
                           "*** id\n"
                           "**** abc\n"
                           "*** repo\n"
                           "**** jeff (default)"))
         (stripped (skg-unrestrictedNode-strip-defaults-from-org org-text))
         (result (org-to-sexp stripped)))
    (should (equal result '(skg (node (id abc) (repo jeff)))))))

(ert-deftest test-strip-repo-bare-value ()
  "Stripping repo without '(default)' keeps the value as-is."
  (let* ((org-text (concat "* skg\n"
                           "** node\n"
                           "*** id\n"
                           "**** abc\n"
                           "*** repo\n"
                           "**** bob"))
         (stripped (skg-unrestrictedNode-strip-defaults-from-org org-text))
         (result (org-to-sexp stripped)))
    (should (equal result '(skg (node (id abc) (repo bob)))))))

(ert-deftest test-round-trip-with-default-repo ()
  "Round-trip with default-repo: fill in then strip is identity."
  (let* ((sexp '(skg (node (id abc) (repo jeff))))
         (org-text (sexp-to-org sexp))
         (filledIn (skg-unrestrictedNode-fillIn-defaults-in-org org-text "jeff"))
         (stripped (skg-unrestrictedNode-strip-defaults-from-org filledIn))
         (result (org-to-sexp stripped)))
    (should (equal result sexp))))

(ert-deftest test-round-trip-new-node-no-repo ()
  "Round-trip for new node: fill in with default, strip keeps repo."
  (let* ((org-text "* skg\n** node")
         (filledIn (skg-unrestrictedNode-fillIn-defaults-in-org org-text "jeff"))
         (stripped (skg-unrestrictedNode-strip-defaults-from-org filledIn))
         (result (org-to-sexp stripped)))
    (should (equal result '(skg (node (repo jeff)))))))

;;
;; Bug: skg-view-metadata was passing hardcoded "* skg\n** node"
;; instead of the actual sexp-to-org output, discarding all fields.
;; These tests verify filling in works correctly on real metadata sexps.
;;

(ert-deftest test-fillIn-preserves-existing-repo ()
  "Filling in a sexp that already has repo preserves it."
  (let* ((sexp '(skg (node (id abc) (repo public)
                           (rels "C5"))))
         (org-text (sexp-to-org sexp))
         (filledIn (skg-unrestrictedNode-fillIn-defaults-in-org org-text))
         (lines (split-string filledIn "\n"))
         (headlines (org-to-sexp--extract-headlines lines)))
    ;; Repo field must be present
    (should (cl-find "repo" headlines :key #'cdr :test #'string=))
    ;; Repo value must be "public" (not missing)
    (should (cl-find "public" headlines :key #'cdr :test #'string=))
    ;; ID must be preserved
    (should (cl-find "id" headlines :key #'cdr :test #'string=))
    (should (cl-find "abc" headlines :key #'cdr :test #'string=))
    ;; the rels herald must be preserved (as non-canonical, appended at end)
    (should (cl-find "rels" headlines :key #'cdr :test #'string=))))

(ert-deftest test-fillIn-preserves-repo-round-trip ()
  "Filling in then stripping a sexp with id + repo is identity. (The
sexp<->org bijection is symbol-based, so the display-only herald strings
-- which are stripped before this path anyway -- are not round-tripped
here; see test-round-trip-with-links-in-herald's removal.)"
  (let* ((sexp '(skg (node (id abc) (repo public))))
         (org-text (sexp-to-org sexp))
         (filledIn (skg-unrestrictedNode-fillIn-defaults-in-org org-text))
         (stripped (skg-unrestrictedNode-strip-defaults-from-org filledIn))
         (result (org-to-sexp stripped)))
    (should (equal result sexp))))

(ert-deftest test-fillIn-real-world-metadata ()
  "Filling in a real-world metadata sexp (from a bug report)
preserves repo and all fields."
  (let* ((sexp '(skg (node (id 6972d099)
                           (repo public)
                           (rels "C5 4(1,1)L"))))
         (org-text (sexp-to-org sexp))
         (filledIn (skg-unrestrictedNode-fillIn-defaults-in-org org-text))
         (lines (split-string filledIn "\n"))
         (headlines (org-to-sexp--extract-headlines lines)))
    ;; Repo must be present
    (should (cl-find "repo" headlines :key #'cdr :test #'string=))
    (should (cl-find "public" headlines :key #'cdr :test #'string=))
    ;; All editable defaults must be present
    (should (cl-find "writeProtected" headlines :key #'cdr :test #'string=))
    (should (cl-find "nonMember" headlines :key #'cdr :test #'string=))
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
  (let* ((sexp '(skg (node (id abc) (repo jeff))))
         (org-text (sexp-to-org sexp))
         (filledIn
          (skg-unrestrictedNode-fillIn-defaults-in-org
           org-text nil "actual title"))
         (lines (split-string filledIn "\n"))
         (headlines (org-to-sexp--extract-headlines lines)))
    (should (equal (cl-subseq headlines 0 4)
                   '((1 . "title")
                     (2 . "actual title")
                     (1 . "skg")
                     (2 . "node"))))))

(ert-deftest test-fillIn-with-empty-display-title-does-not-prepend ()
  "Filling in with an empty display title leaves metadata first."
  (let* ((sexp '(skg (node (id abc) (repo jeff))))
         (org-text (sexp-to-org sexp))
         (filledIn
          (skg-unrestrictedNode-fillIn-defaults-in-org org-text nil ""))
         (lines (split-string filledIn "\n"))
         (headlines (org-to-sexp--extract-headlines lines)))
    (should (equal (car headlines) '(1 . "skg")))))

(ert-deftest test-strip-removes-display-title ()
  "Stripping removes the display-only title group before conversion."
  (let* ((sexp '(skg (node (id abc) (repo jeff))))
         (org-text (sexp-to-org sexp))
         (filledIn
          (skg-unrestrictedNode-fillIn-defaults-in-org
           org-text nil "actual title"))
         (stripped (skg-unrestrictedNode-strip-defaults-from-org filledIn))
         (result (org-to-sexp stripped)))
    (should (equal result sexp))))

(provide 'test-skg-unrestrictedNode-defaults)
