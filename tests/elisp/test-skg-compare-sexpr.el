;;; test-skg-compare-sexpr.el --- Tests for skg-compare-sexpr

(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             (file-name-directory load-file-name)))
(require 'ert)
(require 'skg-sexpr-search)

(ert-deftest test-sexp-subtree-simple-match ()
  "Simple subtree match: (a b c) contains (a)."
  (should (skg-sexp-subtree-p '(a b c) '(a))))

(ert-deftest test-sexp-subtree-unordered-match ()
  "Unordered elements match: (a b c) contains (a c b)."
  (should (skg-sexp-subtree-p '(a b c) '(a c b))))

(ert-deftest test-sexp-subtree-partial-match ()
  "Partial element match: (a b c) contains (a c)."
  (should (skg-sexp-subtree-p '(a b c) '(a c))))

(ert-deftest test-sexp-subtree-wrong-head ()
  "Wrong head fails: (a b c) does not contain (c a b)."
  (should-not (skg-sexp-subtree-p '(a b c) '(c a b))))

(ert-deftest test-sexp-subtree-nested-simple ()
  "Nested simple match: (a (b c d) e) contains (a e)."
  (should (skg-sexp-subtree-p '(a (b c d) e) '(a e))))

(ert-deftest test-sexp-subtree-nested-with-sublist ()
  "Nested with sublist: (a (b c d) e) contains (a (b))."
  (should (skg-sexp-subtree-p '(a (b c d) e) '(a (b)))))

(ert-deftest test-sexp-subtree-nested-partial-sublist ()
  "Nested partial sublist: (a (b c d) e) contains (a (b c))."
  (should (skg-sexp-subtree-p '(a (b c d) e) '(a (b c)))))

(ert-deftest test-sexp-subtree-nested-wrong-order-in-sublist ()
  "Nested wrong order in sublist: (a (b c d) e) does not contain (a (c b))."
  (should-not (skg-sexp-subtree-p '(a (b c d) e) '(a (c b)))))

(ert-deftest test-sexp-subtree-atom-match ()
  "Atoms match when equal."
  (should (skg-sexp-subtree-p 'foo 'foo)))

(ert-deftest test-sexp-subtree-atom-mismatch ()
  "Atoms don't match when different."
  (should-not (skg-sexp-subtree-p 'foo 'bar)))

(ert-deftest test-sexp-subtree-list-vs-atom ()
  "List doesn't match atom."
  (should-not (skg-sexp-subtree-p '(a b) 'a)))

(provide 'test-skg-compare-sexpr)
;;; test-skg-compare-sexpr.el ends here
