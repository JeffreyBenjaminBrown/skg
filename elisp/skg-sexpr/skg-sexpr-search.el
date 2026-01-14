;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: Generic utilities that
;;; read, compare, edit and transform s-exps.
;;;
;;; USER-FACING FUNCTIONS
;;;   (none)

(require 'skg-sexpr-dsl-for-edits)
(require 'skg-compare-sexpr)
(require 'skg-lens)

(defun skg-first-sexpr-on-line ()
  "Find and return the first s-expression on the current line.
Moves point to the beginning of the line, searches forward for an
open paren, positions point on it, then reads the complete sexp.
Returns the sexp as a Lisp object, or nil if no sexp found on line.
Signals an error if a sexp is found but does not end on this line."
  (beginning-of-line)
  (let ((line-end (line-end-position)))
    (when (search-forward "(" line-end t)
      (backward-char 1)
      (let ((start (point)))
        (forward-sexp 1)
        (when (> (point) line-end)
          (error "Sexp does not end on this line"))
        (read (buffer-substring-no-properties start (point)))))))

(defun skg-sexp-at-or-after-point ()
  "Return the sexp at point or the first sexp after point on this line.
If point is inside a sexp that starts on this line, returns that sexp
\(the outermost one starting on this line if nested).
If point is not inside such a sexp, searches forward on this line.
Signals an error if:
- No sexp is found on this line at or after point
- A sexp is found but does not end on this line."
  (let ((line-start (line-beginning-position))
        (line-end (line-end-position))
        (found-start nil))
    (when (> (nth 0 (syntax-ppss)) 0)
      ;; If inside parens, find outermost opening paren on this line
      (save-excursion
        (condition-case nil
            (while t
              (backward-up-list 1)
              (if (>= (point) line-start)
                  (setq found-start (point))
                (signal 'scan-error nil)))
          (scan-error nil))))
    (unless found-start
      ;; If not inside a sexp on this line, search forward
      (if (search-forward "(" line-end t)
          (setq found-start (1- (point)))
        (error "No sexp found on this line at or after point")))
    (progn ;; Move to start and verify sexp ends on this line
      (goto-char found-start)
      (forward-sexp 1)
      (when (> (point) line-end)
        (error "Sexp does not end on this line"))
      (read (buffer-substring-no-properties found-start (point))))))

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

(provide 'skg-sexpr-search)
