;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: Roughly-bijective transformations between
;;; atom-headed paren-terse sexps and org text.
;;;
;;; INPUT REQUIREMENTS
;;; - An input sexp must be:
;;;   - A non-empty list
;;;   - "Atom-headed": every list (and sublist) has a bare atom
;;;     as its first member, including at the top-level.
;;;   - "Paren-terse": no proper sub-sexp
;;;     wraps a single atom in parens.
;;; - Input org text must have:
;;;   - At least one headline
;;;   - Exactly one root (level-1) headline
;;;   - No empty headline text
;;;   - "Child-homogeneity": The first child cannot be indented
;;;     more than one level deeper than its parent.
;;;
;;; TRANSFORMATION NOTES
;;; - Body text in org is discarded (hence "roughly" bijective).
;;; - Multi-word headlines become symbols with spaces.
;;; - Childless non-root headlines become bare atoms, not lists.
;;;
;;; User-facing functions:
;;;   NONE
;;;
;;; ENTRY POINTS:
;;;   sexp-to-org
;;;   org-to-sexp

(require 'cl-lib)

;;
;; sexp-to-org
;;

(defun sexp-to-org (sexp)
  "Convert SEXP to org-mode text.
SEXP must be a list that is both atom-headed and paren-terse.
Signals an error if validation fails."
  (unless (listp sexp)
    (error "sexp-to-org: input must be a list, got %S" sexp))
  (when (null sexp)
    (error "sexp-to-org: input must be a non-empty list"))
  (unless (sexp-atom-headed-p sexp)
    (error "sexp-to-org: input is not atom-headed: %S" sexp))
  (unless (sexp-paren-terse-p sexp)
    (error "sexp-to-org: input is not paren-terse: %S" sexp))
  (sexp-to-org--convert sexp 1))

(defun sexp-atom-headed-p (sexp)
  "Return t if SEXP is atom-headed.
A sexp is atom-headed if it, and every sublist at any level,
has a bare atom as its first member."
  (cond
   ((atom sexp) t)
   ((null sexp) nil)
   ((not (atom (car sexp))) nil)
   (t (cl-every #'sexp-atom-headed-p (cdr sexp)))))

(defun sexp-paren-terse-p (sexp)
  "Return t if SEXP is paren-terse.
No proper sub-sexp may put parens around a single atom."
  (cond
   ((atom sexp) t)
   (t (cl-every #'sexp-paren-terse-p--proper-subsexp (cdr sexp)))))

(defun sexp-paren-terse-p--proper-subsexp (elem)
  "Check paren-terseness for ELEM as a proper subsexp.
A proper subsexp cannot be a list containing a single atom."
  (cond
   ((atom elem) t)
   ((and (= (length elem) 1) (atom (car elem))) nil)
   (t (cl-every #'sexp-paren-terse-p--proper-subsexp (cdr elem)))))

;;
;; org-to-sexp
;;

(defun org-to-sexp (org-text)
  "Convert ORG-TEXT to a sexp.
Discards all non-headline lines before transforming.
Signals an error if:
- There are no headlines
- There are multiple root headlines
- There is a level jump greater than 1
- Any headline has empty text."
  (let* ((lines (split-string org-text "\n"))
         (headlines (org-to-sexp--extract-headlines lines)))
    (when (null headlines)
      (error "org-to-sexp: no headlines found in input"))
    (org-to-sexp--validate-single-root headlines)
    (org-to-sexp--validate-no-empty-headlines headlines)
    (org-to-sexp--validate-child-homogeneity headlines)
    (car (org-to-sexp--build-tree headlines 1))))

(defun org-to-sexp--extract-headlines (lines)
  "Extract headlines from LINES, discarding non-headline content.
Returns a list of (LEVEL . TEXT) pairs."
  (let (result)
    (dolist (line lines)
      (when (string-match "^\\(\\*+\\) +\\(.+\\)$" line)
        (let ((level (length (match-string 1 line)))
              (text (match-string 2 line)))
          (push (cons level text) result))))
    (nreverse result)))

(defun org-to-sexp--validate-single-root (headlines)
  "Validate that HEADLINES has exactly one root (level-1) headline.
Signals an error if the first headline is not level 1,
or if there are multiple level-1 headlines."
  (let ((first-level (caar headlines)))
    (unless (= first-level 1)
      (error "org-to-sexp: first headline must be at level 1, got level %d"
             first-level)))
  (when (cl-some (lambda (hl) (= (car hl) 1))
                 (cdr headlines))
    (error "org-to-sexp: multiple root headlines found")))

(defun org-to-sexp--validate-no-empty-headlines (headlines)
  "Validate that no headline in HEADLINES has empty text.
Signals an error if any headline text is empty or whitespace-only."
  (dolist (hl headlines)
    (let ((level (car hl))
          (text (cdr hl)))
      (when (string-empty-p (string-trim text))
        (error "org-to-sexp: empty headline text at level %d" level)))))

(defun org-to-sexp--validate-child-homogeneity (headlines)
  "Validate that no child is more than one level deeper than its parent.
Signals an error if any headline jumps more than one level."
  (let ((prev-level (caar headlines)))
    (dolist (hl (cdr headlines))
      (let ((level (car hl)))
        (when (> level (1+ prev-level))
          (error "org-to-sexp: level jump from %d to %d (max allowed is 1)"
                 prev-level level))
        (setq prev-level level)))))

(defun sexp-to-org--convert (sexp level)
  "Convert SEXP to org text starting at headline LEVEL.
Assumes SEXP has already been validated."
  (let* ((head (car sexp))
         (tail (cdr sexp))
         (headline (sexp-to-org--make-headline level head))
         (children (mapcar
                    (lambda (elem)
                      (if (listp elem)
                          (sexp-to-org--convert elem (1+ level))
                        (sexp-to-org--make-headline (1+ level) elem)))
                    tail)))
    (mapconcat #'identity (cons headline children) "\n")))

(defun sexp-to-org--make-headline (level text)
  "Create an org headline at LEVEL with TEXT.
TEXT can be a symbol, string, or number."
  (let ((text-str (cond
                   ((stringp text) text)
                   ((numberp text) (number-to-string text))
                   (t (symbol-name text)))))
    (concat (make-string level ?*) " " text-str)))

(defun org-to-sexp--build-tree (headlines expected-level)
  "Build a sexp tree from HEADLINES starting at EXPECTED-LEVEL.
Returns (SEXP . REMAINING-HEADLINES).
For non-root headlines with no children, returns just the symbol.
For root or headlines with children, returns a list."
  (when (and headlines (= (caar headlines) expected-level))
    (let* ((head-hl (car headlines))
           (head-text (cdr head-hl))
           (head-symbol (intern head-text))
           (rest (cdr headlines))
           (children nil)
           (child-level (1+ expected-level)))
      (while (and rest (>= (caar rest) child-level))
        (if (= (caar rest) child-level)
            (let ((child-result (org-to-sexp--build-tree rest child-level)))
              (push (car child-result) children)
              (setq rest (cdr child-result)))
          (setq rest (cdr rest))))
      (let ((sexp (cond
                   (children (cons head-symbol (nreverse children)))
                   ((= expected-level 1) (list head-symbol))
                   (t head-symbol))))
        (cons sexp rest)))))

(provide 'skg-sexpr-org-bijection)
