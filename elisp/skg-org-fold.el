;;; -*- lexical-binding: t; -*-
;;
;; PURPOSE: Read and restore org-fold state, and the 'folded' and
;; 'bodyFolded' atoms that carry it to and from the server.
;;
;; BACKGROUND:
;; If a node's branches are folded,
;; they are marked folded, but the node they belong to is not.
;; For other discoveries re. folding (so far none esp. relevant), see
;;   not_using/experiments/org-folding/FINDINGS.org
;;
;; TODO ? Distinguish between foldedness and other invisibility.
;; Currently foldedness is recognized via '(invisible-p (point))'.

(require 'org)
(require 'skg-metadata)
(require 'skg-sexpr-search)

(defun skg-org-body-toggle ()
  "Toggle the visibility of the body of the headline at point.
If the headline's subtree is folded, show its body and immediate
children without opening the children's entries. Otherwise, hide or
show the body without changing the children's visibility. Works from
the headline or from inside its body; when called from the body, point
moves to the start of the headline. A bodyless headline is left alone,
with a message, unless its children need to be revealed."
  (interactive)
  (unless (org-at-heading-p)
    (org-back-to-heading t))
  (let ((body-state ; one of 'absent, 'hidden, 'visible
         (save-excursion
           (forward-line 1)
           (cond ((or (eobp) (org-at-heading-p)) 'absent)
                 ((invisible-p (point)) 'hidden)
                 (t 'visible))))
        (immediate-children-hidden-p
         (save-excursion
           (and (org-goto-first-child)
                (invisible-p (point))))))
    (cond
     (immediate-children-hidden-p
      (org-fold-show-entry)
      (org-fold-show-children))
     ((eq body-state 'absent)
      (message "This headline has no body."))
     ((eq body-state 'hidden)
      (org-fold-show-entry))
     (t (org-fold-hide-entry)))))

(defun skg-fold-headlines-by-atoms (atom-headlines)
  "PURPOSE:
Restore the user's fold state from the fold atoms the server drew.
ATOM-HEADLINES lists, in buffer order, each headline that had atoms,
as (START . ATOMS): START where the headline starts in the current
buffer and ATOMS its atoms, as strings
\(`skg--stripped-text-and-atom-headlines').
.
Two atoms are recognised:
- 'folded'      : this headline is hidden inside a folded ancestor.
                  We restore that by folding its parent's subtree.
- 'bodyFolded'  : this headline is visible but its body is hidden
                  (e.g. via `org-fold-hide-entry'). We restore that
                  by hiding this headline's entry directly.
.
Hides the entries first, the last in the buffer first, and then the
parents' subtrees, each parent once, the last-found first. A top-level
`folded' headline has no parent to fold, and stays visible.
Assumes no fold hides anything yet."
  (let ((parent-starts nil) ;; the last-found first
        (parent-start-set (make-hash-table)))
    (save-excursion
      (dolist (headline atom-headlines)
        (when (member "folded" (cdr headline))
          (goto-char (car headline))
          (when (and (org-up-heading-safe)
                     (not (gethash (point) parent-start-set)))
            (puthash (point) t parent-start-set)
            (push (point) parent-starts))))
      (dolist (headline (reverse atom-headlines))
        (when (member "bodyFolded" (cdr headline))
          (goto-char (car headline))
          (org-fold-hide-entry)))
      (dolist (parent-start parent-starts)
        (goto-char parent-start)
        (org-fold-hide-subtree)))))

(defun skg--fold-state-of-headline-at-point ()
  "The fold state of the headline at point, which point is at the start
of: `hidden' inside a folded ancestor; `collapsed', its children hidden
by its own fold; `bodyHidden', only its body hidden; else `open'.
A childless headline whose fold hides its body is `bodyHidden'. A
hidden headline's own fold is not looked for, as a save does not."
  (cond ((invisible-p (point)) 'hidden)
        ((save-excursion (and (org-goto-first-child)
                              (invisible-p (point))))
         'collapsed)
        ((skg-headline-body-hidden-p) 'bodyHidden)
        (t 'open)))

(defun skg-headline-body-hidden-p ()
  "Return t when point is at a visible headline whose body is hidden
*by an entry fold* rather than by a subtree fold above it.
.
Operationally:
- The first line after this headline must exist, must not itself be
  a headline, and must be invisible.
- and the next headline in the buffer (this headline's first child
  or its next sibling) must be visible — or absent. If the next
  headline is also hidden, the body's hiddenness is a consequence of
  this headline's subtree being folded, which the existing `folded'
  atom mechanism already restores via the children's atoms; we
  must not double-record it."
  (save-excursion
    (forward-line 1)
    (and (not (eobp))
         (not (org-at-heading-p))
         (invisible-p (point))
         (let ((next-headline
                (save-excursion
                  (when (outline-next-heading)
                    (point)))))
           (or (null next-headline)
               (not (invisible-p next-headline)))))))

(provide 'skg-org-fold)
