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

(defun skg-add-folded-markers ()
  "Add 'folded' to metadata of all invisible headlines in the buffer.
- If a headline has no <skg<...>> metadata,
  prefix the title with '<skg<folded>>'.
- If it has empty metadata (e.g. '<skg<>>' or '<skg< >>'),
  replace with '<skg<folded>>'.
- If it has metadata without 'folded',
  append ',folded' to the existing metadata.
- If metadata already contains 'folded', do nothing."
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (beginning-of-line)
      (when (and (org-at-heading-p)
                 (invisible-p (point)))
        (skg-insert-bare-value-into-metadata "folded"))
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
Works on all text including invisible regions.
.
PITFALL: Does not use skg-metadata.el, even though that module
defines functions for modifying metadata,
because those functions change the buffer's folding state.
.
SOLUTION: If this ever gives me problems, this can be deleted,
PROVIDED that (just) before it sends the buffer to Rust,
Emacs recomputes all folded metadata."
  (save-excursion
    (goto-char (point-min))
    (let ((inhibit-read-only t)
          (buffer-invisibility-spec buffer-invisibility-spec))
      (setq ;; Temporarily make all text visible for replacement
       buffer-invisibility-spec nil)
      (progn ;; folded is the only value: (skg folded) → (skg)
        (goto-char (point-min))
        (while (re-search-forward "(skg +folded *)" nil t)
          (replace-match "(skg)" nil nil)))
      (progn ;; folded is first with whitespace after: (skg folded other) → (skg other)
        (goto-char (point-min))
        (while (re-search-forward "(skg +folded +\\([^)]+\\))" nil t)
          (replace-match "(skg \\1)" nil nil)))
      (progn ;; folded is in middle or last within (skg ...): → remove it
        (goto-char (point-min))
        (while (re-search-forward "(skg\\([^)]*\\) +\\<folded\\>\\([^)]*\\))" nil t)
          (replace-match "(skg\\1\\2)" nil nil)))
      (setq ;; Restore invisibility
       buffer-invisibility-spec buffer-invisibility-spec))))

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

(provide 'skg-org-fold)
