;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: Transform s-expressions using pattern-matching rules

(require 'cl-lib)

(defun skg-transform-sexp-flat
  (object rules)
  "Applies 'rules' to 'object' to generate an output expression.
Has no side effects.
The test suite demonstrates how rules are interpreted,
perhaps better than the following explanation.
.
Both 'object' and 'rules' can be nested s-exps.
The first element of each s-expr at any level should be an atom,
which we'll call the 'label'. The rest can be anything.
The leaf-lists (lists with no sublists) of a rule
contain, after the label, a series of symbols,
 which will be concatenated in the result with ':' between them.
.
Consider such a leaf-list in a rule, with two elts, 'a' and 'b'.
The rules indicates how to transform 'a' into 'b'.
('a' is a label in the rule but is an atom in the object.)
If the transformation applies, it is appended to the output expression.
The transformation only applies if the atom 'a' from 'rules'
can be found in 'object', and both are nested such that
the sequence of 'label's leading to each is the same.
  So for instance if object=(a (b c)) and rules=(a (b (c d))),
  the output would be 'd', but if rules had been (b (a (c d)))
  the output would instead be the empty list.
  (In the first case if the rule were instead '(a (b (c d e)))',
  the output would be 'd:e'.)
.
There are two keywords that receive special treatment:
ANY and IT.
.
ANY indicates that the path of labels exists.
Regardless of what the leaf sexp in the object at that path contains,
the elements listed after ANY in the corresponding leaf-sexp
of the rule will be appended to the output. This is true
even if the object leaf-sexp is a singleton,
containing only the label.
  So for instance, if our rule is (a (b (ANY x))),
  the output would be (x) for any of the following objects:
    (a (b))
    (a (b c))
    (a c (b c))
  whereas for these objects, the output would be ():
    (b a)
    (a b)
    (a)
.
In an ANY leaf-sexp in a rule,
IT refers to any value found after the label
of the corresponding leaf-sexp in 'object'.
Outside of an ANY leaf-sexp in a rule, IT is meaningless.
  So for instance:
    object = (a (b c))
    rule = (a (b (ANY x IT y)))
    output = (x:c:y)
  Note that a single IT can match multiple values:
    object = (a (b c d))
    rule = (a (b (ANY x IT y)))
    output = (x:c:y x:d:y)
.
A leaf-list in a rule can be vacuous, a singleton label.
This is just to demonstrate that the possibility has been considered,
and does not affect the output.
Such singleton leaf-lists can be omitted from the rule to no effect."
  (if
      (or
        (not (listp object))
        (not (listp rules))
        (not (equal (car object) (car rules))))
      '()
    (skg--transform-sexp-flat-from object rules)))

(defun skg--transform-sexp-flat-from
  (object
    rule)
  (let
      ((results '()))
    (dolist
        (rule-child (cdr rule))
      (setq results
            (nconc results
                   (skg--transform-sexp-flat-dispatch
                    object rule-child))))
    results))

(defun skg--transform-sexp-flat-dispatch
  (object
    rule-child)
  (cond
    ((not (listp rule-child)) '())
    ((skg--rule-any-leaf-p rule-child)
      (skg--apply-any-leaf object rule-child))
    ((skg--rule-leaf-p rule-child)
      (skg--apply-ordinary-leaf object rule-child))
    (t
      (skg--apply-non-leaf object rule-child))))

(defun skg--rule-leaf-p
  (rule-child)
  (not
    (cl-some #'listp (cdr rule-child))))

(defun skg--rule-any-leaf-p
  (rule-child)
  (and
    (eq (car rule-child) 'ANY)
    (skg--rule-leaf-p rule-child)))

(defun skg--apply-non-leaf
  (object
    rule-child)
  (let
      ((matches
          (skg--object-children-with-label object (car rule-child)))
        (results '()))
    (dolist
        (match matches)
      (setq results
            (nconc results
                   (skg--transform-sexp-flat-from
                    match rule-child))))
    results))

(defun skg--apply-ordinary-leaf
  (object
    rule-child)
  (let
      ((label (car rule-child))
        (tokens (cdr rule-child))
        (matches
          (skg--object-atomic-matches object (car rule-child)))
        (results '()))
    (when tokens
      (dolist
          (_ matches)
        (setq results
              (nconc results
                      (list (skg--tokens->symbol tokens))))))
    results))

(defun skg--apply-any-leaf
  (object
    rule-child)
  (let
      ((tokens (cdr rule-child))
        (tail-values (cdr object))
        (results '()))
    (cond
      ((null tokens) '())
      ((memq 'IT tokens)
        (dolist
            (tail-value tail-values)
          (setq results
                (nconc results
                        (list
                          (skg--tokens->symbol
                            (skg--replace-it tokens tail-value))))))
        results)
      (t
        (list (skg--tokens->symbol tokens))))))

(defun skg--replace-it
  (tokens
    tail-value)
  (mapcar
    (lambda
      (token)
      (if
          (eq token 'IT)
          tail-value
        token))
    tokens))

(defun skg--object-children-with-label
  (object
    label)
  (cl-loop
    for element in (cdr object)
    when
    (and
      (listp element)
      (equal (car element) label))
    collect element))

(defun skg--object-atomic-matches
  (object
    label)
  (cl-loop
    for element in (cdr object)
    when
    (and
      (not (listp element))
      (equal element label))
    collect element))

(defun skg--tokens->symbol
  (tokens)
  (intern
    (mapconcat #'skg--token->string tokens ":")))

(defun skg--token->string
  (token)
  (cond
    ((symbolp token) (symbol-name token))
    ((stringp token) token)
    (t (format "%s" token))))

(provide 'skg-lens)
