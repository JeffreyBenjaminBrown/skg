;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: Compare s-expressions.
;;;
;;; USER-FACING FUNCTIONS
;;;   (none)

(require 'cl-lib)

(defun skg-sexp-subtree-p
  ( object structure )
  "Check if STRUCTURE is a semantic subtree of OBJECT.

The comparison treats the first element of each list as a 'head' that
must match exactly, while remaining elements are treated as an unordered
set with recursive matching for sublists.

Examples:
  (skg-sexp-subtree-p '(a b c) '(a))             => t
  (skg-sexp-subtree-p '(a b c) '(a c b))         => t
  (skg-sexp-subtree-p '(a b c) '(a c))           => t
  (skg-sexp-subtree-p '(a b c) '(c a b))         => nil
  (skg-sexp-subtree-p '(a (b c d) e) '(a e))     => t
  (skg-sexp-subtree-p '(a (b c d) e) '(a (b)))   => t
  (skg-sexp-subtree-p '(a (b c d) e) '(a (b c))) => t
  (skg-sexp-subtree-p '(a (b c d) e) '(a (c b))) => nil"
  ( cond
    ;; Atoms must match exactly
    ( ( atom structure ) ( equal object structure ) )

    ;; Both must be lists
    ( ( not ( listp object ) ) nil )

    ;; Heads must match exactly
    ( ( not ( equal ( car object ) ( car structure ) ) ) nil )

    ;; Each element in structure's tail must match some element in object's tail
    ( t
      ( cl-every
        ( lambda
          ( struct-elem )
          ( cl-some
            ( lambda
              ( obj-elem )
              ( skg-sexp-subtree-p obj-elem struct-elem ) )
            ( cdr object ) ) )
        ( cdr structure ) ) ) ) )

(provide 'skg-compare-sexpr)

;;; skg-compare-sexpr.el ends here
