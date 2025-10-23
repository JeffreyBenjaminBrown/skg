;;; -*- lexical-binding: t; -*-
;; Utilities for finding s-expression boundaries in text

(require 'skg-edit-sexpr)
(require 'skg-compare-sexpr)

(defun skg-find-sexp-end (text &optional start-pos)
  "Find the position after the closing paren of the first s-expression in TEXT.
START-POS defaults to 0. Returns the position (1-based index) after the closing paren,
or nil if parentheses are unbalanced or no complete s-expression is found."
  (let ((pos (or start-pos 0))
        (len (length text))
        (depth 0)
        (end nil))
    (while (and (< pos len) (or (= depth 0) (not end)))
      (cond ((eq (aref text pos) ?\()
             (setq depth (1+ depth)))
            ((eq (aref text pos) ?\))
             (setq depth (1- depth))
             (when (= depth 0)
               (setq end pos))))
      (setq pos (1+ pos)))
    (when end
      (1+ end))))

(provide 'skg-sexpr)
