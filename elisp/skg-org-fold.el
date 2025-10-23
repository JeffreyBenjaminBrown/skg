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
(require 'skg-sexpr)

(defun skg-fold-marked-headlines ()
  "PURPOSE:
Hide all headlines marked with 'folded' metadata,
by folding their parents.
.
METHOD:
First unfold everything.
Then process the buffer from the top, via this loop:
- Find the next visible node with 'folded' in its metadata
- Navigate to its parent
- Fold that parent"
  (org-show-all)
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

(defun skg-remove-folded-markers ()
  "Remove 'folded' from all (skg ...) metadata markers.
Re-folds headlines that had folded children using org-mode functions."
  (save-excursion
    (let ((inhibit-read-only t)
          (parent-markers
           ;; This processes the entire buffer.
           (skg-collect-parent-markers-of-folded-headlines)))
      (org-show-all) ;; so that text editing works
      (progn ;; Remove "folded" from metadata using DELETE operation.
        ;; This also processes the entire buffer.
        (goto-char (point-min))
        (while (not (eobp))
          (beginning-of-line)
          (when (and (org-at-heading-p)
                     (skg-headline-has-folded-in-view-p))
            (skg-edit-metadata-at-point
             '(skg (view (DELETE folded)))))
          (forward-line 1)))
      (progn;; This is a third pass through the entire buffer.
        (dolist (parent-marker parent-markers)
          (goto-char parent-marker)
          (when (org-at-heading-p)
            (org-fold-hide-subtree))
          (set-marker parent-marker nil))))))

(defun skg-add-folded-markers ()
  "Add 'folded' to (view ...) metadata of all invisible headlines in the buffer.
Merges '(skg (view folded))' into existing metadata, creating nested structure
if needed. If metadata already contains 'folded' in the view section, no change."
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (beginning-of-line)
      (when (and (org-at-heading-p)
                 (invisible-p (point)))
        (skg-edit-metadata-at-point '(skg (view folded))))
      (forward-line 1))))

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
  "Return t if the current headline has 'folded' in its (view ...) metadata.
Assumes point is at the beginning of a headline.
Verifies the structure is (skg ... (view ... folded ...) ...)."
  (when (org-at-heading-p)
    (let* ((headline-text (skg-get-current-headline-text))
           (match-result (skg-split-as-stars-metadata-title headline-text)))
      (when match-result
        (let* ((inner (nth 1 match-result))
               ;; Parse all s-expressions from inner by wrapping in parens
               (all-sexps (read (concat "(" inner ")"))))
          ;; Search for (view ... folded ...) in the list of s-expressions
          (cl-some (lambda (sexp)
                     (skg-sexp-subtree-p sexp '(view folded)))
                   all-sexps))))))

(provide 'skg-org-fold)
