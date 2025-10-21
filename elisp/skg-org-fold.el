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

(defun skg-create-org-buffer-with-folds (content)
  "Create an org-mode buffer from CONTENT string.
First inserts all content unchanged, then folds headlines marked with
'folded' by backtracking to their parents, then removes the fold markers.
Returns the newly created buffer."
  (let ((buf (generate-new-buffer "*org-backtracking-fold*")))
    (with-current-buffer buf
      (org-mode)
      (insert content)
      (goto-char (point-min))
      (skg-fold-marked-headlines)
      (skg-remove-folded-markers)
      (goto-char (point-min)))
    buf))

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
                     (save-excursion
                       (beginning-of-line)
                       (looking-at ".*(skg.*\\<folded\\>.*)")))
            (setq found (point)))
          (forward-line 1)))
      (when found
        ;; If we found one, go to its parent and fold.
        (goto-char found)
        (when (org-up-heading-safe)
          (org-cycle)) )) ))

(defun skg-remove-folded-markers ()
  "Remove 'folded' from all (skg ...) metadata markers.
Handles all cases:
folded as only value, first value, middle value, or last value.
Works on all text including invisible regions."
  (save-excursion
    (goto-char (point-min))
    (let ((inhibit-read-only t)
          (saved-invisibility-spec buffer-invisibility-spec)
          (invisible-markers '()))
      ;; First pass: record which headlines are invisible
      (while (not (eobp))
        (beginning-of-line)
        (when (and (org-at-heading-p)
                   (invisible-p (point)))
          (push (point-marker) invisible-markers))
        (forward-line 1))
      ;; Second pass: remove folded markers, making text temporarily visible
      (setq buffer-invisibility-spec nil)
      (goto-char (point-min))
      (while (not (eobp))
        (beginning-of-line)
        (when (org-at-heading-p)
          (let* ((bol (line-beginning-position))
                 (eol (line-end-position))
                 (headline-text (buffer-substring-no-properties bol eol)))
            (when (string-match-p "(skg" headline-text)
              (let* ((match-result (skg-split-as-stars-metadata-title
                                    headline-text)))
                (when match-result
                  (let* ((stars (nth 0 match-result))
                         (inner (nth 1 match-result))
                         (title (nth 2 match-result))
                         (parsed (skg-parse-metadata-inner inner))
                         (alist (car parsed))
                         (bare-values (cadr parsed))
                         (filtered-values
                          (seq-filter (lambda (v)
                                        (not (string-equal v "folded")))
                                      bare-values))
                         (new-inner (skg-reconstruct-metadata-inner
                                     alist filtered-values))
                         (new-headline (skg-format-headline stars new-inner title)))
                    (delete-region bol eol)
                    (goto-char bol)
                    (insert new-headline)))))))
        (forward-line 1))
      ;; Third pass: restore invisibility for marked headlines
      (setq buffer-invisibility-spec saved-invisibility-spec)
      (dolist (marker invisible-markers)
        (goto-char marker)
        (when (org-at-heading-p)
          ;; Make this headline invisible again using an overlay
          (let ((ov (make-overlay (line-beginning-position) (line-end-position))))
            (overlay-put ov 'invisible 'outline)))
        (set-marker marker nil)))))

(provide 'skg-org-fold)
