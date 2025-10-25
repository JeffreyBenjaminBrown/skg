;;; test-skg-lens.el

(add-to-list
 'load-path
 (expand-file-name "../../elisp/skg-sexpr"
                   (file-name-directory load-file-name)))

(require 'ert)
(require 'skg-lens)

(ert-deftest test-skg-transform-sexp-flat-one-flat-rule-applies ()
  "One flat rule that applies: (a b) with rule (a (b c)) -> (c)."
  (should (equal (skg-transform-sexp-flat '(a b)
                                          '(a (b c)))
                 '(c))))

(ert-deftest test-skg-transform-sexp-flat-one-flat-rule-does-not-apply ()
  "One flat rule that does not apply: (a c) with rule (a (b c)) -> ()."
  (should (equal (skg-transform-sexp-flat '(a c)
                                          '(a (b c)))
                 '())))

(ert-deftest test-skg-transform-sexp-flat-one-nested-rule-applies ()
  "One nested rule that applies: (a (b c)) with rule (a (b (c d))) -> (d)."
  (should (equal (skg-transform-sexp-flat '(a (b c))
                                          '(a (b (c d))))
                 '(d))))

(ert-deftest test-skg-transform-sexp-flat-one-nested-rule-does-not-apply ()
  "One nested rule that does not apply: (a (b c)) with rule (a (b (c d))) -> (d).
Note: This test appears identical to the 'applies' version - may need clarification."
  (should (equal (skg-transform-sexp-flat '(a (b c))
                                          '(a (b (c d))))
                 '(d))))

(ert-deftest test-skg-transform-sexp-flat-two-flat-rules-apply-not-in-order ()
  "Two flat rules that apply, not in order: (a b c) with rules (a (c cc) (b bb)) -> (a bb cc)."
  (should (equal (skg-transform-sexp-flat '(a b c)
                                          '(a (c cc) (b bb)))
                 '(cc bb))))

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
    '(b22 d33 c11)) ))

(ert-deftest test-skg-transform-sexp-flat-one-flat-ANY-rule-applies ()
  "One flat ANY rule that applies: (a b) with rule (a (ANY c)) -> (c)."
  (should (equal (skg-transform-sexp-flat '(a b)
                                          '(a (ANY c)))
                 '(c)) ))

(ert-deftest test-skg-transform-sexp-flat-one-flat-ANY-rule-does-not-apply ()
  "One flat ANY rule that does not apply: (a b) with rule (c (ANY d)) -> ()."
  (should (equal (skg-transform-sexp-flat '(a b)
                                          '(c (ANY d)) )
                 '() )) )

(ert-deftest test-skg-transform-sexp-flat-one-nested-ANY-rule-applies-even-if-the-label-is-bare ()
  "One nested ANY rule that applies: (a (b)) with rule (a (b (ANY d))) -> (d). Note that 'b' in 'object' is a label in a singleton sexp -- even in that case, the rule applies."
  (should (equal (skg-transform-sexp-flat '(a (b))
                                          '(a (b (ANY d))))
                 '(d)) ))

(ert-deftest test-skg-transform-sexp-flat-ANY-considers-only-chain-of-labels ()
  "ANY considers only the chain of labels.
Does not apply because c is a label in the rule and not in the object."
  (should (equal (skg-transform-sexp-flat '(a (b c))
                                          '(a (b (c (ANY d)))))
                 '() )) )

(ert-deftest test-skg-transform-sexp-flat-two-flat-rules-one-ANY-both-apply ()
  "Two flat rules, one of them ANY, both apply: (a b c) with rules (a (ANY x) (c cc)) -> (x cc)."
  (should (equal (skg-transform-sexp-flat '(a b c)
                                          '(a (ANY x) (c cc)) )
                 '(x cc)) ))

(ert-deftest test-skg-transform-sexp-flat ()
  (should (equal (skg-transform-sexp-flat '(a (b c))
                                          '(a (b (ANY x IT y)) ))
                 '(x:c:y) )))

(provide 'test-skg-lens)
