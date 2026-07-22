;;; -*- lexical-binding: t; -*-
;;
;; PURPOSE: Read, act on, and edit 'folded' metadata.
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
Checks whether the body is visible, then delegates to
`outline-hide-entry' or `outline-show-entry'. Works from the
headline or from inside its body; when called from the body, point
moves to the start of the headline. A bodyless headline is left
alone, with a message."
  (interactive)
  (unless (org-at-heading-p)
    (org-back-to-heading t))
  (let ((body-state ;; one of 'absent, 'hidden, 'visible
         (save-excursion
           (forward-line 1)
           (cond ((or (eobp) (org-at-heading-p)) 'absent)
                 ((invisible-p (point)) 'hidden)
                 (t 'visible)))))
    (cond
     ((eq body-state 'absent)
      (message "This headline has no body."))
     ((eq body-state 'hidden)
      (outline-show-entry))
     (t (outline-hide-entry)))))

(defun skg-fold-marked-headlines ()
  "PURPOSE:
Restore the user's fold state from per-headline metadata markers.
.
Two markers are recognised:
- 'folded'      : THIS headline is hidden inside a folded ancestor.
                  We restore that by folding its parent's subtree.
- 'bodyFolded'  : THIS headline is visible but its body is hidden
                  (e.g. via `org-fold-hide-entry'). We restore that
                  by hiding this headline's entry directly.
.
METHOD:
- First unfold everything.
- Pass 1: find every headline with `bodyFolded' and hide its entry.
- Pass 2: iteratively find a visible `folded'-marked headline and
  fold its parent. We loop because folding a parent makes more
  `folded' headlines invisible, removing them from the search."
  (org-show-all)
  (skg--fold-bodies-of-bodyfolded-headlines)
  (goto-char (point-min))
  (let ((found t))
    (while found
      (setq found nil)
      (save-excursion
        ;; Search for visible headline with 'folded' in metadata.
        (goto-char (point-min))
        (while (and (not found) (not (eobp)))
          (beginning-of-line)
          (when (and (looking-at org-heading-regexp)
                     (not (invisible-p (point)))
                     (skg-headline-has-folded-in-view-p))
            (setq found (point)))
          (forward-line 1)))
      (when found
        ;; If we found one, go to its parent and fold.
        (goto-char found)
        (when (org-up-heading-safe)
          (org-cycle)) )) ))

(defun skg--fold-bodies-of-bodyfolded-headlines ()
  "Hide the entry of every headline tagged `bodyFolded' in metadata.
Assumes the buffer is currently fully shown — we only need to add the
fold; we don't have to undo any prior fold."
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (beginning-of-line)
      (when (and (org-at-heading-p)
                 (skg-headline-has-bodyfolded-in-view-p))
        (org-fold-hide-entry))
      (forward-line 1))))

(defun skg-remove-folded-markers ()
  "Remove 'folded' and 'bodyFolded' markers from all (skg ...) metadata.
Re-folds headlines that had folded children using org-mode functions,
and re-hides entries that had `bodyFolded'."
  (save-excursion
    (let ((inhibit-read-only t)
          (parent-markers
           ;; This processes the entire buffer.
           (skg-collect-parent-markers-of-folded-headlines))
          (bodyfolded-markers
           (skg-collect-markers-of-bodyfolded-headlines)))
      (org-show-all) ;; so that text editing works
      (progn ;; Remove "folded" from metadata using DELETE operation.
        ;; This also processes the entire buffer.
        (goto-char (point-min))
        (while (not (eobp))
          (beginning-of-line)
          (when (org-at-heading-p)
            (when (skg-headline-has-folded-in-view-p)
              (skg-edit-metadata-at-point '(skg (DELETE folded))))
            (when (skg-headline-has-bodyfolded-in-view-p)
              (skg-edit-metadata-at-point '(skg (DELETE bodyFolded)))))
          (forward-line 1)))
      (progn ;; Re-hide entries that had `bodyFolded'.
        (dolist (m bodyfolded-markers)
          (goto-char m)
          (when (org-at-heading-p)
            (org-fold-hide-entry))
          (set-marker m nil)))
      (progn;; This is a third pass through the entire buffer.
        (dolist (parent-marker parent-markers)
          (goto-char parent-marker)
          (when (org-at-heading-p)
            (org-fold-hide-subtree))
          (set-marker parent-marker nil))))))

(defun skg-collect-markers-of-bodyfolded-headlines ()
  "Collect markers pointing to each headline tagged `bodyFolded'.
Caller must `set-marker' each to nil when done."
  (let ((markers '()))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (beginning-of-line)
        (when (and (org-at-heading-p)
                   (skg-headline-has-bodyfolded-in-view-p))
          (push (point-marker) markers))
        (forward-line 1)))
    markers))

(defun skg-add-folded-markers ()
  "Annotate the headline at each fold-relevant position.
- For each *invisible* headline (hidden inside a folded ancestor),
  merge `(skg folded)' into its metadata.
- For each *visible* headline whose body content is hidden (e.g.
  via `org-fold-hide-entry'), merge `(skg bodyFolded)' instead.
The two flags are mutually exclusive in this pass — a hidden
headline doesn't get to record its own body state, since it's
already covered by the ancestor's subtree fold."
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (beginning-of-line)
      (when (org-at-heading-p)
        (cond
         ((invisible-p (point))
          (skg-edit-metadata-at-point '(skg folded)))
         ((skg-headline-body-hidden-p)
          (skg-edit-metadata-at-point '(skg bodyFolded)))))
      (forward-line 1))))

(defun skg-headline-body-hidden-p ()
  "Return t when point is at a visible headline whose body is hidden
*by an entry fold* rather than by a subtree fold above it.
.
Operationally:
- The first line after this headline must exist, must not itself be
  a headline, and must be invisible.
- AND the next headline in the buffer (this headline's first child
  or its next sibling) must be visible — or absent. If the next
  headline is also hidden, the body's hiddenness is a consequence of
  this headline's subtree being folded, which the existing `folded'
  marker mechanism already restores via the children's markers; we
  must not double-record it."
  (save-excursion
    (forward-line 1)
    (and (not (eobp))
         (not (org-at-heading-p))
         (invisible-p (point))
         (let ((next-heading
                (save-excursion
                  (when (outline-next-heading)
                    (point)))))
           (or (null next-heading)
               (not (invisible-p next-heading)))))))

(defun skg-collect-parent-markers-of-folded-headlines ()
  "Collect markers for parent headlines of all headlines with folded markers.
Returns a list of markers pointing to parent headlines.
Duplicates are removed - each parent appears only once in the list.
Caller is responsible for freeing the markers with set-marker."
  (let ((parent-markers '()))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (beginning-of-line)
        (when (and (org-at-heading-p)
                   (skg-headline-has-folded-in-view-p))
          ;; This headline has "folded" marker, so record its parent
          (save-excursion
            (when (org-up-heading-safe)
              (let ((parent-pos (point)))
                ;; Check if already in list by position (deduplicate)
                (unless (cl-some (lambda (m) (= (marker-position m) parent-pos))
                                 parent-markers)
                  (push (point-marker) parent-markers))))))
        (forward-line 1)))
    parent-markers))

(defun skg-headline-has-folded-in-view-p ()
  "Return t if the current headline has 'folded' in its metadata.
Assumes point is at the beginning of a headline.
Verifies the structure is (skg ... folded ...)."
  (skg--headline-has-bare-marker-p 'folded))

(defun skg-headline-has-bodyfolded-in-view-p ()
  "Return t if the current headline has 'bodyFolded' in its metadata.
Assumes point is at the beginning of a headline.
Verifies the structure is (skg ... bodyFolded ...)."
  (skg--headline-has-bare-marker-p 'bodyFolded))

(defun skg--headline-has-bare-marker-p (marker)
  "Return t if the current headline's metadata contains the bare atom MARKER.
Assumes point is at the beginning of a headline."
  (when (org-at-heading-p)
    (let* ((headline-text (skg-get-current-headline-text))
           (match-result (skg-split-as-stars-metadata-title
                          headline-text)))
      (when match-result
        (let ((metadata-sexp (nth 1 match-result)))
          (when (and metadata-sexp
                     (not (string-empty-p metadata-sexp)))
            (skg-sexp-subtree-p (read metadata-sexp)
                                (list 'skg marker))))))))

(provide 'skg-org-fold)
