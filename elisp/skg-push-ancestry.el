;;; skg-push-ancestry.el --- Push the ancestry at point to the linkstack  -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;;
;;; 'skg-push-ancestry' (C-c U) pushes the view-ancestry of the
;;; headline at point to 'skg-linkstack', as a tree of the notes above
;;; it, each joined to the next by the graph edge that justifies its
;;; place in the view ('skg--ancestry-edge', read from the headline's
;;; (view (birth ...) (rels ...)) metadata). Folder headlines are
;;; skipped; it refuses, naming the two notes, when a note has no edge
;;; to the note above it. 'skg-paste-link' (C-c o l) then pastes the
;;; tree as a chain of links joined by those edges. The linkstack's
;;; trees, and pasting them, are 'skg-linkstack.el's
;;; ('skg--linkstack-tree'). The pushed tree records the graph when it
;;; was pushed; it is not kept current.
;;;
;;; DATA USED/ASSUMED: See docs/api-and-formats.org, "Stats metadata".
;;;
;;; Code:

(require 'cl-lib)
(require 'org)
(require 'skg-id-search)
(require 'skg-linkstack)
(require 'skg-metadata)
(require 'skg-sexpr-search)
(require 'skg-state)

(defconst skg--ancestry-relations '(contains forks linksTo hides)
  "The relations an ancestry edge can be, in the order
`skg--ancestry-edge' prefers them when no birth fact chooses one.")

(defun skg-push-ancestry ()
  "Push to `skg-linkstack' the view-ancestry of the headline at point:
a tree whose root is the view's top-level note above it, and whose
chain runs down through each note above it to the headline's own,
folder headlines skipped. Each note is joined to the one above it by
the graph edge its metadata records between them
\(`skg--ancestry-edge'). Refuses, naming the two notes, when some
note has no edge to the note above it. Paste it with
`skg-paste-link': links joined by those edges, as
\"[[id:A][a]] / [[id:B][b]] ~ [[id:C][c]]\".
The pushed tree records the graph when it was pushed; it is not
kept current."
  (interactive)
  (let (( tree (skg--ancestry-tree-at-point) ))
    (push tree skg-linkstack)
    (message "pushed to stack: %s%s"
             (skg--linkstack-tree-prefix-for-prompt tree)
             (cadr (skg--linkstack-tree-leaf tree))) ))

(defun skg--ancestry-tree-at-point ()
  "The tree `skg-push-ancestry' pushes from the headline at point."
  (save-excursion
    (condition-case nil (org-back-to-heading t)
      (error (user-error "Not on a headline")))
    (let (( note (skg--ancestry-note-at-point) ))
      (unless note
        (user-error "No ID in metadata on this headline"))
      (let (( chain (list (cons note nil)) ))
        ;; CHAIN: (NOTE . EDGE) pairs, the root's first once complete;
        ;; each EDGE joins its NOTE to the note before it.
        (while (let (( found (skg--ancestry-nearest-note-above) ))
                 (when found
                   (let* (( above (car found) )
                          ( below (caar chain) )
                          ( edge (skg--ancestry-edge
                                  (nth 2 below) (cdr found)) ))
                     (unless edge
                       (user-error "Cannot push this ancestry: no edge of the graph joins \"%s\" to \"%s\", the note above it in this view"
                                   (cadr below) (cadr above)))
                     (setcdr (car chain) edge)
                     (push (cons above nil) chain) ))))
        (skg--ancestry-tree-of-chain chain) ))))

(defun skg--ancestry-note-at-point ()
  "(ID TITLE METADATA-SEXP) of the headline at point, or nil if its
metadata names no node."
  (let (( sexp (skg--metadata-sexp-at-point-or-nil) ))
    (when (and sexp (skg--metadata-sexp-contains-id-p sexp))
      (list (skg--extract-id-from-metadata-sexp sexp)
            (caddr (skg-split-as-stars-metadata-title
                    (skg-get-current-headline-text)))
            sexp)) ))

(defun skg--ancestry-nearest-note-above ()
  "Move up from the headline at point to the nearest ancestor headline
that names a node, and return (NOTE . GENERATION): NOTE as
`skg--ancestry-note-at-point' gives it, GENERATION how many headline
levels up it is (folders and other headlines that name no node
count, as ancestor generations do in metadata). Nil at the top."
  (let (( generation 0 )
        ( note nil ))
    (while (and (not note) (org-up-heading-safe))
      (setq generation (1+ generation))
      (setq note (skg--ancestry-note-at-point)))
    (when note (cons note generation)) ))

(defun skg--ancestry-tree-of-chain (chain)
  "The linkstack tree (`skg--linkstack-tree') of CHAIN, a list of
\(NOTE . EDGE) pairs, the root's first."
  (let* (( pair (car chain) )
         ( note (car pair) ))
    (skg--linkstack-tree
     (car note) (cadr note) (cdr pair)
     (when (cdr chain)
       (list (skg--ancestry-tree-of-chain (cdr chain))))) ))

(defun skg--ancestry-edge (sexp generation)
  "The edge (RELATION SIDE...) that joins the headline whose metadata
is SEXP to its ancestor GENERATION levels up, read from its
`(view (birth ...) (rels ...))' (docs/api-and-formats.org, \"Stats
metadata\"); nil if none does. SIDE is \\='in where the ancestor
relates to the headline's note (it contains, forks, links to or hides
it) and \\='out where the note relates to the ancestor. A birth fact
for that GENERATION, the relation that put the note there, chooses the
relation; otherwise the first of `skg--ancestry-relations' whose
`(rels ...)' sides name GENERATION among their ancestors. Only forks
keeps both sides, a mutual fork; another relation holding both ways
keeps the side its birth fact names, else \\='in."
  (let* (( view (skg-sexp-cdr-at-path sexp '(skg view)) )
         ( born (cl-find-if (lambda (fact) (eql (nth 2 fact) generation))
                            (cdr (assq 'birth view))) )
         ( relation (or (car born)
                        (cl-find-if (lambda (relation)
                                      (skg--ancestry-sides
                                       view relation generation))
                                    skg--ancestry-relations)) )
         ( sides (and relation
                      (or (skg--ancestry-sides view relation generation)
                          (and born (list (cadr born))))) ))
    (when sides
      (cons relation
            (cond ((eq relation 'forks) sides)
                  (born (list (cadr born)))
                  (t (list (car sides))))) )))

(defun skg--ancestry-sides (view relation generation)
  "The sides, of \\='in and \\='out, of RELATION in VIEW's `(rels ...)'
that name GENERATION among their ancestors."
  (let (( relation-form (assq relation (cdr (assq 'rels view))) ))
    (cl-remove-if-not
     (lambda (side)
       (memql generation
              (cdr (assq 'ancestors (cdr (assq side (cdr relation-form)))))))
     '(in out)) ))

(provide 'skg-push-ancestry)
