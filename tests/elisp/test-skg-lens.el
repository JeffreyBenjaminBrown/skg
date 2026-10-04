;;; test-skg-lens.el

(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             (file-name-directory load-file-name)))
(require 'ert)
(require 'skg-lens)

(ert-deftest test-skg-transform-sexp-flat-one-flat-rule-applies ()
  "One flat rule that applies: (a b) with rule (a (b c)) -> (\"c\")."
  (should (equal (skg-transform-sexp-flat '(a b)
                                          '(a (b c)))
                 '("c"))))

(ert-deftest test-skg-transform-sexp-flat-one-flat-rule-does-not-apply ()
  "One flat rule that does not apply: (a c) with rule (a (b c)) -> ()."
  (should (equal (skg-transform-sexp-flat '(a c)
                                          '(a (b c)))
                 '())))

(ert-deftest test-skg-transform-sexp-flat-one-nested-rule-applies ()
  "One nested rule that applies: (a (b c)) with rule (a (b (c d))) -> (\"d\")."
  (should (equal (skg-transform-sexp-flat '(a (b c))
                                          '(a (b (c d))))
                 '("d"))))

(ert-deftest test-skg-transform-sexp-flat-one-nested-rule-does-not-apply ()
  "One nested rule that does not apply: (a (b c)) with rule (a (b (c d))) -> (\"d\").
Note: This test appears identical to the 'applies' version - may need clarification."
  (should (equal (skg-transform-sexp-flat '(a (b c))
                                          '(a (b (c d))))
                 '("d"))))

(ert-deftest test-skg-transform-sexp-flat-two-flat-rules-apply-not-in-order ()
  "Two flat rules that apply, not in order: (a b c) with rules (a (c cc) (b bb)) -> (\"cc\" \"bb\")."
  (should (equal (skg-transform-sexp-flat '(a b c)
                                          '(a (c cc) (b bb)))
                 '("cc" "bb"))))

(ert-deftest test-skg-transform-sexp-flat-complex-rules ()
  "Complex rules with multiple nested transformations.
Any rule below without a 'does not apply' comment applies."
  (should
   (equal
    (skg-transform-sexp-flat
     '(a (b b1 b2 b3)
         (c c1
            (d d1 d2 d3)
            c2 c3))
     '(a (b c) ;; does not apply; b is a label in 'object'
         (b (b2 b22))
         (c (b (b3 b33)) ;; does not apply; that's not where b3 is
            (d (d3 d33))
            (c1 c11))))
    '("b22" "d33" "c11")) ))

(ert-deftest test-skg-transform-sexp-flat-one-flat-ANY-rule-applies ()
  "One flat ANY rule that applies: (a b) with rule (a (ANY c)) -> (\"c\")."
  (should (equal (skg-transform-sexp-flat '(a b)
                                          '(a (ANY c)))
                 '("c")) ))

(ert-deftest test-skg-transform-sexp-flat-one-flat-ANY-rule-does-not-apply ()
  "One flat ANY rule that does not apply: (a b) with rule (c (ANY d)) -> ()."
  (should (equal (skg-transform-sexp-flat '(a b)
                                          '(c (ANY d)) )
                 '() )) )

(ert-deftest test-skg-transform-sexp-flat-one-nested-ANY-rule-applies-even-if-the-label-is-bare ()
  "One nested ANY rule that applies: (a (b)) with rule (a (b (ANY d))) -> (\"d\"). Note that 'b' in 'object' is a label in a singleton sexp -- even in that case, the rule applies."
  (should (equal (skg-transform-sexp-flat '(a (b))
                                          '(a (b (ANY d))))
                 '("d")) ))

(ert-deftest test-skg-transform-sexp-flat-ANY-considers-only-chain-of-labels ()
  "ANY considers only the chain of labels.
Does not apply because c is a label in the rule and not in the object."
  (should (equal (skg-transform-sexp-flat '(a (b c))
                                          '(a (b (c (ANY d)))))
                 '() )) )

(ert-deftest test-skg-transform-sexp-flat-two-flat-rules-one-ANY-both-apply ()
  "Two flat rules, one of them ANY, both apply: (a b c) with rules (a (ANY x) (c cc)) -> (\"x\" \"cc\")."
  (should (equal (skg-transform-sexp-flat '(a b c)
                                          '(a (ANY x) (c cc)) )
                 '("x" "cc")) ))

(ert-deftest test-skg-transform-sexp-flat ()
  (should (equal (skg-transform-sexp-flat '(a (b c))
                                          '(a (b (ANY x IT y)) ))
                 '("x:c:y") )))

(ert-deftest test-skg-transform-sexp-flat-string-prefix-prepends-to-child-outputs ()
  "A string literal in a non-leaf rule is concatenated as a prefix
before each output that the rule's list children produce."
  (should
    (equal
      (skg-transform-sexp-flat
        '(a (b c))
        '(a (b "B: " (c "cc"))))
      '("B: cc"))))

(ert-deftest test-skg-transform-sexp-flat-string-prefix-no-children-match-emits-prefix ()
  "If no list child fires, a string-literal prefix is emitted alone.
This lets rules like (STOP deleted \"DELETED\" (id) (repo)) serve
as a label even when their sub-rules are vacuous."
  (should
    (equal
      (skg-transform-sexp-flat
        '(a (b c))
        '(a (b "B: " (d "dd")))) ;; d absent in object
      '("B: "))))

(ert-deftest test-skg-transform-sexp-flat-string-prefix-applies-to-each-match ()
  "The string-literal prefix is prepended to every output of every
matching list child, not just the first."
  (should
    (equal
      (skg-transform-sexp-flat
        '(a (b c d))
        '(a (b "B: " (c "cc") (d "dd"))))
      '("B: cc" "B: dd"))))

;;
;; HIGH style keyword
;;

(ert-deftest test-skg-transform-sexp-flat-orange-is-a-style ()
  "HIGH is accepted alongside STOP/GO/NORMAL/MEDIUM."
  (let ((out (skg-transform-sexp-flat
              '(a b) '(a (HIGH b "bb")))))
    (should (equal out '("bb")))
    (should (eq (get-text-property 0 'skg-style (car out))
                'HIGH))))

;;
;; ABUT marker
;;

(ert-deftest test-skg-transform-sexp-flat-abut-marks-first-char ()
  "A rule with ABUT sets `skg-abut' on position 0 of its emitted
token so renderers can join without a separator."
  (let ((out (skg-transform-sexp-flat
              '(a b) '(a (b ABUT "bb")))))
    (should (equal out '("bb")))
    (should (eq (get-text-property 0 'skg-abut (car out)) t))))

(ert-deftest test-skg-transform-sexp-flat-abut-without-marker-is-nil ()
  "Without ABUT, `skg-abut' is nil."
  (let ((out (skg-transform-sexp-flat
              '(a b) '(a (b "bb")))))
    (should (null (get-text-property 0 'skg-abut (car out))))))

;;
;; INTERC directive
;;

(ert-deftest test-skg-transform-sexp-flat-interc-joins-slots-with-separator ()
  "An INTERC rule emits one token per matching object child:
each sub-rule contributes one slot, slots are joined with the
rule's SEP, and a literal-string prefix is prepended."
  (should (equal (skg-transform-sexp-flat
                  '(a (pair (left 3) (right 8)))
                  '(a (INTERC "{" pair
                        (left  (ANY IT))
                        (right (ANY IT)))))
                 '("3{8"))))

(ert-deftest test-skg-transform-sexp-flat-interc-missing-slot-still-emits-separator ()
  "When one slot is empty but another is non-empty, the separator
is still emitted between them. A solitary containsHerald-style
`3{' comes out correctly when only the left slot fires."
  (should (equal (skg-transform-sexp-flat
                  '(a (pair (left 3)))
                  '(a (INTERC "{" pair
                        (left  (ANY IT))
                        (right (ANY IT)))))
                 '("3{")))
  (should (equal (skg-transform-sexp-flat
                  '(a (pair (right 8)))
                  '(a (INTERC "{" pair
                        (left  (ANY IT))
                        (right (ANY IT)))))
                 '("{8"))))

(ert-deftest test-skg-transform-sexp-flat-interc-all-slots-empty-suppresses ()
  "When every slot is empty for a given match, INTERC emits
nothing for that match -- avoids displaying a naked separator."
  (should (equal (skg-transform-sexp-flat
                  '(a (pair (other 99)))
                  '(a (INTERC "{" pair
                        (left  (ANY IT))
                        (right (ANY IT)))))
                 '())))

(ert-deftest test-skg-transform-sexp-flat-interc-skipped-if-label-absent ()
  "If the object has no child with the INTERC's label, nothing is emitted."
  (should (equal (skg-transform-sexp-flat
                  '(a (otherPair))
                  '(a (INTERC "{" pair (left (ANY IT)))))
                 '())))

(ert-deftest test-skg-transform-sexp-flat-interc-preserves-per-subrule-styles ()
  "Colors from INTERC sub-rules are preserved per-segment on the
output token. The separator takes the INTERC rule's own style
context (nil here, since the outer INTERC has no style directive)."
  (let* ((out (skg-transform-sexp-flat
               '(a (pair (left 3) (right 8)))
               '(a (INTERC "{" pair
                     (MEDIUM left  (ANY IT))
                     (NORMAL   right (ANY IT))))))
         (s   (car out)))
    (should (equal out '("3{8")))
    (should (eq  (get-text-property 0 'skg-style s) 'MEDIUM))
    (should (null (get-text-property 1 'skg-style s))) ;; separator: no style
    (should (eq  (get-text-property 2 'skg-style s) 'NORMAL))))

(ert-deftest test-skg-transform-sexp-flat-interc-with-own-style-styles-separator-and-prefix ()
  "When INTERC carries its own style, the separator and any
literal prefix inherit it; sub-rule styles still override."
  (let* ((out (skg-transform-sexp-flat
               '(a (pair (left 3) (right 8)))
               '(a (NORMAL INTERC "{" pair "P:"
                     (MEDIUM left  (ANY IT))
                     (right        (ANY IT))))))
         (s   (car out)))
    (should (equal out '("P:3{8")))
    (should (eq (get-text-property 0 'skg-style s) 'NORMAL))   ;; prefix "P"
    (should (eq (get-text-property 1 'skg-style s) 'NORMAL))   ;; prefix ":"
    (should (eq (get-text-property 2 'skg-style s) 'MEDIUM)) ;; "3"
    (should (eq (get-text-property 3 'skg-style s) 'NORMAL))   ;; separator
    (should (eq (get-text-property 4 'skg-style s) 'NORMAL)))) ;; "8" via inherited

(ert-deftest test-skg-transform-sexp-flat-interc-empty-separator-concatenates-slots ()
  "An empty-string separator makes INTERC concatenate slots with
nothing between them -- the pattern the stage-axis rules use."
  (should (equal (skg-transform-sexp-flat
                  '(a (stage deletedN removedR))
                  '(a (INTERC "" stage "stage:"
                        (deletedN "-N")
                        (removedR "-R"))))
                 '("stage:-N-R"))))

(ert-deftest test-skg-transform-sexp-flat-interc-emits-per-matching-child ()
  "If OBJECT has multiple matching children, INTERC emits one
token per match."
  (should (equal (skg-transform-sexp-flat
                  '(a (stage deletedN) (stage addedR))
                  '(a (INTERC "" stage "stage:"
                        (deletedN "-N")
                        (addedR "R"))))
                 '("stage:-N" "stage:R"))))

(provide 'test-skg-lens)
