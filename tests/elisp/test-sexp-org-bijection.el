;;; test-sexp-org-bijection.el --- Tests for sexp-org-bijection

(load-file (expand-file-name "../../elisp/skg-test-utils.el"
                             (file-name-directory load-file-name)))
(require 'ert)
(require 'skg-sexpr-org-bijection)

;;
;; Test case data: (sexp org-text description)
;;

(defvar bijective-cases
  ;; Each triple is: (sexp, corresponding org text, error message)
  `(((a)
     "* a"
     "simplest case")

    ((a b)
"* a
** b"
     "two elements")

    ((a (b a))
"* a
** b
*** a"
     "nested list")

    ((a (b a) c)
"* a
** b
*** a
** c"
     "nested with sibling")

    ((a ,(intern "b c") d)
"* a
** b c
** d"
     "whitespace in symbol")

    ((a (b c) d)
"* a
** b
*** c
** d"
     "standard nested structure")

    ((root (child1 grandchild1 grandchild2) child2 (child3 (deep nested)))
"* root
** child1
*** grandchild1
*** grandchild2
** child2
** child3
*** deep
**** nested"
     "complex structure"))
  "Test cases where sexp <-> org conversion is bijective.
Each element is (SEXP ORG-TEXT DESCRIPTION).")

(defvar org-to-sexp-cases
  '(("* a
some body text
** b
more body"
     (a b)
     "body text discarded")

    ("* a

** b

"
     (a b)
     "blank lines discarded"))
  "Test cases for org-to-sexp only (not bijective due to info loss).
Each element is (ORG-TEXT SEXP DESCRIPTION).")

(defvar sexp-to-org-cases
  '()
  "Test cases for sexp-to-org only (not bijective).
Each element is (SEXP ORG-TEXT DESCRIPTION).
Currently empty - all valid sexp-to-org conversions are bijective.")

;;
;; Test runner helpers
;;

(defun test-sexp-to-org (sexp expected-org description)
  "Test that (sexp-to-org SEXP) equals EXPECTED-ORG.
DESCRIPTION is used in error messages."
  (let ((result (condition-case err
                    (sexp-to-org sexp)
                  (error (error "sexp-to-org failed for '%s': %s"
                                description (error-message-string err))))))
    (unless (equal result expected-org)
      (error "sexp-to-org failed for '%s': expected %S, got %S"
             description expected-org result))))

(defun test-org-to-sexp (org-text expected-sexp description)
  "Test that (org-to-sexp ORG-TEXT) equals EXPECTED-SEXP.
DESCRIPTION is used in error messages."
  (let ((result (condition-case err
                    (org-to-sexp org-text)
                  (error (error "org-to-sexp failed for '%s': %s"
                                description (error-message-string err))))))
    (unless (equal result expected-sexp)
      (error "org-to-sexp failed for '%s': expected %S, got %S"
             description expected-sexp result))))

(defun test-bijective (sexp org-text description)
  "Test that SEXP and ORG-TEXT convert to each other bidirectionally.
DESCRIPTION is used in error messages."
  (test-sexp-to-org sexp org-text description)
  (test-org-to-sexp org-text sexp description))

;;
;; Bijective conversion tests
;;

(ert-deftest test-bijective-cases ()
  "Test all bijective sexp <-> org conversions."
  (dolist (case bijective-cases)
    (let ((sexp (nth 0 case))
          (org-text (nth 1 case))
          (description (nth 2 case)))
      (test-bijective sexp org-text description))))

;;
;; One-way org-to-sexp tests (info loss prevents round-trip)
;;

(ert-deftest test-org-to-sexp-cases ()
  "Test org-to-sexp conversions that are not bijective."
  (dolist (case org-to-sexp-cases)
    (let ((org-text (nth 0 case))
          (expected-sexp (nth 1 case))
          (description (nth 2 case)))
      (test-org-to-sexp org-text expected-sexp description))))

;;
;; One-way sexp-to-org tests (if any)
;;

(ert-deftest test-sexp-to-org-cases ()
  "Test sexp-to-org conversions that are not bijective."
  (dolist (case sexp-to-org-cases)
    (let ((sexp (nth 0 case))
          (expected-org (nth 1 case))
          (description (nth 2 case)))
      (test-sexp-to-org sexp expected-org description))))

;;
;; Error cases for sexp-to-org
;;

(ert-deftest test-sexp-to-org-rejects-bare-atom ()
  "Bare atom input should signal an error."
  (should-error (sexp-to-org 'a) :type 'error))

(ert-deftest test-sexp-to-org-rejects-empty-list ()
  "Empty list input should signal an error."
  (should-error (sexp-to-org '()) :type 'error))

(ert-deftest test-sexp-to-org-rejects-non-atom-headed ()
  "Non-atom-headed input should signal an error: ((a) b)"
  (should-error (sexp-to-org '((a) b)) :type 'error))

(ert-deftest test-sexp-to-org-rejects-non-paren-terse ()
  "Non-paren-terse input should signal an error: (a (b) c)"
  (should-error (sexp-to-org '(a (b) c)) :type 'error))

;;
;; Error cases for org-to-sexp
;;

(ert-deftest test-org-to-sexp-rejects-no-headlines ()
  "Input with no headlines should signal an error."
  (should-error (org-to-sexp "just some text") :type 'error))

(ert-deftest test-org-to-sexp-rejects-empty-input ()
  "Empty input should signal an error."
  (should-error (org-to-sexp "") :type 'error))

(ert-deftest test-org-to-sexp-rejects-multiple-roots ()
  "Multiple root headlines should signal an error."
  (should-error (org-to-sexp "* a
** b
* c") :type 'error))

(ert-deftest test-org-to-sexp-rejects-level-jump ()
  "Level jump > 1 should signal an error."
  (should-error (org-to-sexp "* a
*** b") :type 'error))

(ert-deftest test-org-to-sexp-rejects-level-jump-nested ()
  "Nested level jump > 1 should signal an error."
  (should-error (org-to-sexp "* a
** b
**** c") :type 'error))

(ert-deftest test-org-to-sexp-rejects-empty-headline ()
  "Empty headline text should signal an error."
  (should-error (org-to-sexp "* ") :type 'error))

;;
;; Validation helper tests
;;

(ert-deftest test-atom-headed-p-simple ()
  "(a) is atom-headed."
  (should (sexp-atom-headed-p '(a))))

(ert-deftest test-atom-headed-p-nested ()
  "(a (a (a (a)))) is atom-headed."
  (should (sexp-atom-headed-p '(a (a (a (a)))))))

(ert-deftest test-atom-headed-p-with-atoms ()
  "(a b (a b (a b)) a b) is atom-headed."
  (should (sexp-atom-headed-p '(a b (a b (a b)) a b))))

(ert-deftest test-atom-headed-p-fails-list-head ()
  "((a)) is not atom-headed."
  (should-not (sexp-atom-headed-p '((a)))))

(ert-deftest test-atom-headed-p-fails-list-head-with-tail ()
  "((a) b) is not atom-headed."
  (should-not (sexp-atom-headed-p '((a) b))))

(ert-deftest test-atom-headed-p-fails-nested-list-head ()
  "(a ((b) a)) is not atom-headed."
  (should-not (sexp-atom-headed-p '(a ((b) a)))))

(ert-deftest test-paren-terse-p-simple ()
  "(a) is paren-terse."
  (should (sexp-paren-terse-p '(a))))

(ert-deftest test-paren-terse-p-two-elements ()
  "(a b) is paren-terse."
  (should (sexp-paren-terse-p '(a b))))

(ert-deftest test-paren-terse-p-nested ()
  "(a (b c) d) is paren-terse."
  (should (sexp-paren-terse-p '(a (b c) d))))

(ert-deftest test-paren-terse-p-complex ()
  "(a (b (c d)) e (f g)) is paren-terse."
  (should (sexp-paren-terse-p '(a (b (c d)) e (f g)))))

(ert-deftest test-paren-terse-p-fails-single-atom-sublist ()
  "(a (b) c) is not paren-terse because (b) wraps single atom."
  (should-not (sexp-paren-terse-p '(a (b) c))))

(ert-deftest test-paren-terse-p-fails-deeply-nested-single-atom ()
  "(a (b (c))) is not paren-terse because (c) wraps single atom."
  (should-not (sexp-paren-terse-p '(a (b (c))))))

(provide 'test-sexp-org-bijection)
;;; test-sexp-org-bijection.el ends here
