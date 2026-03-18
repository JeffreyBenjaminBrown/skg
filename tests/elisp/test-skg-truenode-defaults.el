;;; test-skg-truenode-defaults.el --- Tests for skg-truenode-defaults

(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             (file-name-directory load-file-name)))
(require 'ert)
(require 'skg-truenode-defaults)

;;
;; skg-truenode-sexp-p
;;

(ert-deftest test-truenode-sexp-p-positive ()
  "Recognizes a TrueNode sexp."
  (should (skg-truenode-sexp-p '(skg (node (id abc) (source jeff))))))

(ert-deftest test-truenode-sexp-p-negative-not-skg ()
  "Rejects non-skg sexp."
  (should-not (skg-truenode-sexp-p '(foo (node (id abc))))))

(ert-deftest test-truenode-sexp-p-negative-not-node ()
  "Rejects skg sexp without node."
  (should-not (skg-truenode-sexp-p '(skg (alias (id abc))))))

;;
;; skg-headlines-to-org
;;

(ert-deftest test-headlines-to-org ()
  "Converts headline list to org text."
  (let ((headlines '((1 . "skg") (2 . "node") (3 . "id") (4 . "abc"))))
    (should (equal (skg-headlines-to-org headlines)
                   "* skg\n** node\n*** id\n**** abc"))))

;;
;; Expand: minimal sexp (id + source only) -> all defaults appear
;;

(ert-deftest test-expand-minimal-sexp ()
  "Expanding a minimal TrueNode inserts all default fields."
  (let* ((sexp '(skg (node (id abc) (source jeff))))
         (org-text (sexp-to-org sexp))
         (expanded (skg-truenode-expand-defaults-in-org org-text))
         (lines (split-string expanded "\n"))
         (headlines (org-to-sexp--extract-headlines lines)))
    ;; Should have: skg, node, id, abc, source, jeff,
    ;; indefinitive, false (default), parentIgnores, false (default),
    ;; editRequest, none (default), viewRequests, none (default)
    (should (= (length headlines) 14))
    ;; Check default fields are present
    (should (cl-find "indefinitive" headlines
                     :key #'cdr :test #'string=))
    (should (cl-find "false (default)" headlines
                     :key #'cdr :test #'string=))
    (should (cl-find "editRequest" headlines
                     :key #'cdr :test #'string=))
    (should (cl-find "none (default)" headlines
                     :key #'cdr :test #'string=))))

;;
;; Expand: sexp with 'indefinitive' present -> shows 'true' child
;;

(ert-deftest test-expand-with-indefinitive ()
  "Expanding a sexp with bare indefinitive shows 'true' child."
  (let* ((sexp '(skg (node (id abc) (source jeff) indefinitive)))
         (org-text (sexp-to-org sexp))
         (expanded (skg-truenode-expand-defaults-in-org org-text))
         (lines (split-string expanded "\n"))
         (headlines (org-to-sexp--extract-headlines lines))
         (indef-idx (cl-position "indefinitive" headlines
                                 :key #'cdr :test #'string=)))
    ;; The next headline after indefinitive should be "true"
    (should indef-idx)
    (should (string= (cdr (nth (1+ indef-idx) headlines)) "true"))))

;;
;; Expand: sexp with 'parentIgnores' present -> shows 'true' child
;;

(ert-deftest test-expand-with-parentIgnores ()
  "Expanding a sexp with bare parentIgnores shows 'true' child."
  (let* ((sexp '(skg (node (id abc) (source jeff) parentIgnores)))
         (org-text (sexp-to-org sexp))
         (expanded (skg-truenode-expand-defaults-in-org org-text))
         (lines (split-string expanded "\n"))
         (headlines (org-to-sexp--extract-headlines lines))
         (pi-idx (cl-position "parentIgnores" headlines
                              :key #'cdr :test #'string=)))
    (should pi-idx)
    (should (string= (cdr (nth (1+ pi-idx) headlines)) "true"))))

;;
;; Strip: unmodified expanded org -> returns original sexp
;;

(ert-deftest test-strip-unmodified-returns-original ()
  "Stripping an unmodified expanded org returns the original sexp."
  (let* ((sexp '(skg (node (id abc) (source jeff))))
         (org-text (sexp-to-org sexp))
         (expanded (skg-truenode-expand-defaults-in-org org-text))
         (stripped (skg-truenode-strip-defaults-from-org expanded))
         (result (org-to-sexp stripped)))
    (should (equal result sexp))))

;;
;; Strip: after cycling indefinitive to true -> sexp has bare 'indefinitive'
;;

(ert-deftest test-strip-indefinitive-true ()
  "Stripping indefinitive=true produces bare atom in sexp."
  (let* ((org-text (concat "* skg\n"
                           "** node\n"
                           "*** id\n"
                           "**** abc\n"
                           "*** source\n"
                           "**** jeff\n"
                           "*** indefinitive\n"
                           "**** true\n"
                           "*** parentIgnores\n"
                           "**** false (default)\n"
                           "*** editRequest\n"
                           "**** none (default)\n"
                           "*** viewRequests\n"
                           "**** none (default)"))
         (stripped (skg-truenode-strip-defaults-from-org org-text))
         (result (org-to-sexp stripped)))
    (should (equal result '(skg (node (id abc) (source jeff) indefinitive))))))

;;
;; Strip: accepts bare 'false' (without '(default)') as the default value
;;

(ert-deftest test-strip-bare-false ()
  "Stripping accepts bare 'false' as the default value."
  (let* ((org-text (concat "* skg\n"
                           "** node\n"
                           "*** id\n"
                           "**** abc\n"
                           "*** source\n"
                           "**** jeff\n"
                           "*** indefinitive\n"
                           "**** false\n"
                           "*** parentIgnores\n"
                           "**** false\n"
                           "*** editRequest\n"
                           "**** none\n"
                           "*** viewRequests\n"
                           "**** none"))
         (stripped (skg-truenode-strip-defaults-from-org org-text))
         (result (org-to-sexp stripped)))
    (should (equal result '(skg (node (id abc) (source jeff)))))))

;;
;; Round-trip: expand then strip with no edits = identity
;;

(ert-deftest test-round-trip-minimal ()
  "Round-trip: expand then strip on minimal sexp is identity."
  (let* ((sexp '(skg (node (id abc) (source jeff))))
         (org-text (sexp-to-org sexp))
         (expanded (skg-truenode-expand-defaults-in-org org-text))
         (stripped (skg-truenode-strip-defaults-from-org expanded))
         (result (org-to-sexp stripped)))
    (should (equal result sexp))))

(ert-deftest test-round-trip-with-indefinitive ()
  "Round-trip: expand then strip preserves bare indefinitive."
  (let* ((sexp '(skg (node (id abc) (source jeff) indefinitive)))
         (org-text (sexp-to-org sexp))
         (expanded (skg-truenode-expand-defaults-in-org org-text))
         (stripped (skg-truenode-strip-defaults-from-org expanded))
         (result (org-to-sexp stripped)))
    (should (equal result sexp))))

(ert-deftest test-round-trip-with-editrequest-delete ()
  "Round-trip: expand then strip preserves (editRequest delete)."
  (let* ((sexp '(skg (node (id abc) (source jeff) (editRequest delete))))
         (org-text (sexp-to-org sexp))
         (expanded (skg-truenode-expand-defaults-in-org org-text))
         (stripped (skg-truenode-strip-defaults-from-org expanded))
         (result (org-to-sexp stripped)))
    (should (equal result sexp))))

(ert-deftest test-round-trip-with-editrequest-merge ()
  "Round-trip: expand then strip preserves (editRequest (merge XYZ))."
  (let* ((sexp '(skg (node (id abc) (source jeff) (editRequest (merge XYZ)))))
         (org-text (sexp-to-org sexp))
         (expanded (skg-truenode-expand-defaults-in-org org-text))
         (stripped (skg-truenode-strip-defaults-from-org expanded))
         (result (org-to-sexp stripped)))
    (should (equal result sexp))))

;;
;; Canonical ordering: id and source first, editable fields, readonly stats last
;;

(ert-deftest test-canonical-ordering ()
  "Fields appear in canonical order after expansion."
  (let* ((sexp '(skg (node (source jeff) (graphStats 42) (id abc) indefinitive)))
         (org-text (sexp-to-org sexp))
         (expanded (skg-truenode-expand-defaults-in-org org-text))
         (lines (split-string expanded "\n"))
         (headlines (org-to-sexp--extract-headlines lines))
         (level-3 (mapcar #'cdr
                          (cl-remove-if-not
                           (lambda (hl) (= (car hl) 3))
                           headlines))))
    ;; Order should be: id, source, indefinitive, parentIgnores,
    ;; editRequest, viewRequests, graphStats
    (should (equal level-3
                   '("id" "source" "indefinitive" "parentIgnores"
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
                           "*** source\n"
                           "**** jeff\n"
                           "*** editRequest\n"
                           "**** merge [[id:XYZ][some label]]"))
         (stripped (skg-truenode-strip-defaults-from-org org-text))
         (result (org-to-sexp stripped)))
    (should (equal result
                   '(skg (node (id abc) (source jeff)
                               (editRequest (merge XYZ))))))))

;;
;; Strip: parentIgnores true -> bare atom
;;

(ert-deftest test-strip-parentIgnores-true ()
  "Stripping parentIgnores=true produces bare atom in sexp."
  (let* ((org-text (concat "* skg\n"
                           "** node\n"
                           "*** id\n"
                           "**** abc\n"
                           "*** source\n"
                           "**** jeff\n"
                           "*** parentIgnores\n"
                           "**** true"))
         (stripped (skg-truenode-strip-defaults-from-org org-text))
         (result (org-to-sexp stripped)))
    (should (equal result
                   '(skg (node (id abc) (source jeff) parentIgnores))))))

;;
;; Strip: viewRequests with actual values -> kept as-is
;;

(ert-deftest test-strip-viewrequests-kept ()
  "Stripping viewRequests with actual values keeps them."
  (let* ((org-text (concat "* skg\n"
                           "** node\n"
                           "*** id\n"
                           "**** abc\n"
                           "*** source\n"
                           "**** jeff\n"
                           "*** viewRequests\n"
                           "**** aliases\n"
                           "**** containerwardView"))
         (stripped (skg-truenode-strip-defaults-from-org org-text))
         (result (org-to-sexp stripped)))
    (should (equal result
                   '(skg (node (id abc) (source jeff)
                               (viewRequests aliases containerwardView)))))))

;;
;; Source defaults
;;

(ert-deftest test-expand-with-default-source-marks-matching ()
  "Expanding with default-source marks matching source value."
  (let* ((sexp '(skg (node (id abc) (source jeff))))
         (org-text (sexp-to-org sexp))
         (expanded (skg-truenode-expand-defaults-in-org org-text "jeff"))
         (lines (split-string expanded "\n"))
         (headlines (org-to-sexp--extract-headlines lines)))
    (should (cl-find "jeff (default)" headlines
                     :key #'cdr :test #'string=))))

(ert-deftest test-expand-with-default-source-leaves-nonmatching ()
  "Expanding with default-source leaves non-matching source value bare."
  (let* ((sexp '(skg (node (id abc) (source bob))))
         (org-text (sexp-to-org sexp))
         (expanded (skg-truenode-expand-defaults-in-org org-text "jeff"))
         (lines (split-string expanded "\n"))
         (headlines (org-to-sexp--extract-headlines lines)))
    (should (cl-find "bob" headlines :key #'cdr :test #'string=))
    (should-not (cl-find "bob (default)" headlines
                         :key #'cdr :test #'string=))))

(ert-deftest test-expand-inserts-default-source-when-missing ()
  "Expanding a node with no source inserts default source."
  (let* ((sexp '(skg (node (id abc))))
         (org-text (sexp-to-org sexp))
         (expanded (skg-truenode-expand-defaults-in-org org-text "jeff"))
         (lines (split-string expanded "\n"))
         (headlines (org-to-sexp--extract-headlines lines)))
    (should (cl-find "source" headlines :key #'cdr :test #'string=))
    (should (cl-find "jeff (default)" headlines
                     :key #'cdr :test #'string=))))

(ert-deftest test-strip-source-with-default-suffix ()
  "Stripping source with '(default)' suffix removes the suffix."
  (let* ((org-text (concat "* skg\n"
                           "** node\n"
                           "*** id\n"
                           "**** abc\n"
                           "*** source\n"
                           "**** jeff (default)"))
         (stripped (skg-truenode-strip-defaults-from-org org-text))
         (result (org-to-sexp stripped)))
    (should (equal result '(skg (node (id abc) (source jeff)))))))

(ert-deftest test-strip-source-bare-value ()
  "Stripping source without '(default)' keeps the value as-is."
  (let* ((org-text (concat "* skg\n"
                           "** node\n"
                           "*** id\n"
                           "**** abc\n"
                           "*** source\n"
                           "**** bob"))
         (stripped (skg-truenode-strip-defaults-from-org org-text))
         (result (org-to-sexp stripped)))
    (should (equal result '(skg (node (id abc) (source bob)))))))

(ert-deftest test-round-trip-with-default-source ()
  "Round-trip with default-source: expand then strip is identity."
  (let* ((sexp '(skg (node (id abc) (source jeff))))
         (org-text (sexp-to-org sexp))
         (expanded (skg-truenode-expand-defaults-in-org org-text "jeff"))
         (stripped (skg-truenode-strip-defaults-from-org expanded))
         (result (org-to-sexp stripped)))
    (should (equal result sexp))))

(ert-deftest test-round-trip-new-node-no-source ()
  "Round-trip for new node: expand with default, strip keeps source."
  (let* ((org-text "* skg\n** node")
         (expanded (skg-truenode-expand-defaults-in-org org-text "jeff"))
         (stripped (skg-truenode-strip-defaults-from-org expanded))
         (result (org-to-sexp stripped)))
    (should (equal result '(skg (node (source jeff)))))))

;;
;; Bug: skg-edit-metadata was passing hardcoded "* skg\n** node"
;; instead of the actual sexp-to-org output, discarding all fields.
;; These tests verify expand works correctly on real metadata sexps.
;;

(ert-deftest test-expand-preserves-existing-source ()
  "Expanding a sexp that already has source preserves it."
  (let* ((sexp '(skg (node (id abc) (source public)
                           (graphStats (containers 0) (contents 5)))))
         (org-text (sexp-to-org sexp))
         (expanded (skg-truenode-expand-defaults-in-org org-text))
         (lines (split-string expanded "\n"))
         (headlines (org-to-sexp--extract-headlines lines)))
    ;; Source field must be present
    (should (cl-find "source" headlines :key #'cdr :test #'string=))
    ;; Source value must be "public" (not missing)
    (should (cl-find "public" headlines :key #'cdr :test #'string=))
    ;; ID must be preserved
    (should (cl-find "id" headlines :key #'cdr :test #'string=))
    (should (cl-find "abc" headlines :key #'cdr :test #'string=))
    ;; graphStats must be preserved (as non-canonical, appended at end)
    (should (cl-find "graphStats" headlines :key #'cdr :test #'string=))))

(ert-deftest test-expand-preserves-source-round-trip ()
  "Expanding then stripping a sexp with source + graphStats is identity
for the editable fields (graphStats is readonly and preserved too)."
  (let* ((sexp '(skg (node (id abc) (source public)
                           (graphStats (containers 0) (contents 5)))))
         (org-text (sexp-to-org sexp))
         (expanded (skg-truenode-expand-defaults-in-org org-text))
         (stripped (skg-truenode-strip-defaults-from-org expanded))
         (result (org-to-sexp stripped)))
    (should (equal result sexp))))

(ert-deftest test-expand-real-world-metadata ()
  "Expanding a real-world metadata sexp (like from next.org bug report)
preserves source and all fields."
  (let* ((sexp '(skg (node (id 6972d099)
                           (source public)
                           (graphStats (containers 0)
                                       (contents 5)
                                       (linksIn 4→)))))
         (org-text (sexp-to-org sexp))
         (expanded (skg-truenode-expand-defaults-in-org org-text))
         (lines (split-string expanded "\n"))
         (headlines (org-to-sexp--extract-headlines lines)))
    ;; Source must be present
    (should (cl-find "source" headlines :key #'cdr :test #'string=))
    (should (cl-find "public" headlines :key #'cdr :test #'string=))
    ;; All editable defaults must be present
    (should (cl-find "indefinitive" headlines :key #'cdr :test #'string=))
    (should (cl-find "parentIgnores" headlines :key #'cdr :test #'string=))
    (should (cl-find "editRequest" headlines :key #'cdr :test #'string=))
    ;; graphStats must be preserved
    (should (cl-find "graphStats" headlines :key #'cdr :test #'string=))))

(ert-deftest test-round-trip-with-links-in-herald ()
  "Round-trip: expand then strip preserves linksIn herald string (e.g. 4→)."
  (let* ((sexp '(skg (node (id abc) (source public)
                           (graphStats (containers 0)
                                       (contents 5)
                                       (linksIn 3→1)))))
         (org-text (sexp-to-org sexp))
         (expanded (skg-truenode-expand-defaults-in-org org-text))
         (stripped (skg-truenode-strip-defaults-from-org expanded))
         (result (org-to-sexp stripped)))
    (should (equal result sexp))))

(provide 'test-skg-truenode-defaults)
