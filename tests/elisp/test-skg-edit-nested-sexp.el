;;; test-skg-edit-nested-sexp.el --- Tests for skg-edit-nested-sexp

(add-to-list
 'load-path
 (expand-file-name "../../elisp"
                   (file-name-directory load-file-name)))

(require 'ert)
(require 'skg-sexpr)

(ert-deftest test-edit-sexp-delete-from-top ()
  "Delete from top: (skg a b c d) + (skg (DELETE b c)) -> (skg a d)."
  (let ((target '(skg a b c d))
        (instructions '(skg (DELETE b c)))
        (expected '(skg a d)))
    (should (equal (skg-edit-nested-sexp target instructions)
                   expected))))

(ert-deftest test-edit-sexp-delete-nesting-matters ()
  "For delete, nesting and parens matter:
(skg a (b c) (d e) f) + (skg (DELETE (b) d e f)) -> (skg a (d e))."
  (let ((target '(skg a (b c) (d e) f))
        (instructions '(skg (DELETE (b) d e f)))
        (expected '(skg a (d e))))
    (should (equal (skg-edit-nested-sexp target instructions)
                   expected))))

(ert-deftest test-edit-sexp-delete-and-merge ()
  "Delete and merge something at top:
(skg a b (c d)) + (skg (DELETE b) (c e)) -> (skg a (c d e))."
  (let ((target '(skg a b (c d)))
        (instructions '(skg (DELETE b) (c e)))
        (expected '(skg a (c d e))))
    (should (equal (skg-edit-nested-sexp target instructions)
                   expected))))

(ert-deftest test-edit-sexp-replace-bare ()
  "Replace something bare:
(skg a b c) + (skg (REPLACE b x)) -> (skg a x c)."
  (let ((target '(skg a b c))
        (instructions '(skg (REPLACE b x)))
        (expected '(skg a x c)))
    (should (equal (skg-edit-nested-sexp target instructions)
                   expected))))

(ert-deftest test-edit-sexp-replace-nested ()
  "Replace something nested:
(skg (a b) (c d) (e f)) + (skg (REPLACE (c) (c x))) -> (skg (a b) (c x) (e f))."
  (let ((target '(skg (a b) (c d) (e f)))
        (instructions '(skg (REPLACE (c) (c x))))
        (expected '(skg (a b) (c x) (e f))))
    (should (equal (skg-edit-nested-sexp target instructions)
                   expected))))

(ert-deftest test-edit-sexp-multiple-operations ()
  "Multiple deletes, replaces, ensures and merges."
  (let ((target '(skg (a a1 a2 (a3 a31 a32) (a4 a41 a42))
                      (b b1 b2 (b3 b31 b32) (b4 b41 b42))
                      (c c1 c2 (c3 c31 c32) (c4 c41 c42))
                      (d d1 d2 (d3 d31 d32) (d4 d41 d42))))
        (instructions '(skg (a (DELETE a1 (a4)) (a3 (DELETE a31)))
                            (b (REPLACE b1 x)
                               (REPLACE (b4) (b4 z))
                               (b3 (REPLACE b31 y)))
                            (c NOW (c3 HEY) (c4 THERE))
                            (d (ENSURE d5) (ENSURE (d6 y)) (ENSURE (d3 x)))))
        (expected '(skg (a a2 (a3 a32))
                        (b x b2 (b3 y b32) (b4 z))
                        (c c1 c2 (c3 c31 c32 HEY) (c4 c41 c42 THERE) NOW)
                        (d d1 d2 (d3 x) (d4 d41 d42) d5 (d6 y)))))
    (should (equal (skg-edit-nested-sexp target instructions)
                   expected))))

(ert-deftest test-edit-sexp-nil-target ()
  "When target is nil, ignore DELETE/REPLACE, unwrap ENSURE, and merge the rest."
  (let ((target nil)
        (instructions '(skg (DELETE c) (REPLACE (a) (a x))
                            (ENSURE (a a1))
                            (ENSURE (d d1))
                            (a a2) (c c2)
                            (ENSURE (c c3))))
        (expected '(skg (a a1 a2) (d d1) (c c3))))
    (should (equal (skg-edit-nested-sexp target instructions)
                   expected))))

(ert-deftest test-edit-sexp-different-starting-symbols ()
  "When target and instructions start with different symbols, return target unchanged."
  (let ((target '(a))
        (instructions '(b c))
        (expected '(a)))
    (should (equal (skg-edit-nested-sexp target instructions)
                   expected))))

(ert-deftest test-edit-sexp-empty-target ()
  "An empty list target accepts any merge, unwrapping ENSURE."
  (let ((target '())
        (instructions '(b (c d) (ENSURE e) (ENSURE (f g))))
        (expected '(b (c d) e (f g))))
    (should (equal (skg-edit-nested-sexp target instructions)
                   expected))))

(provide 'test-skg-edit-nested-sexp)
;;; test-skg-edit-nested-sexp.el ends here
