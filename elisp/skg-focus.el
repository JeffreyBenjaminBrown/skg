;; Utilities for managing focus metadata on org headlines

(require 'org)
(require 'skg-metadata)
(require 'skg-sexpr)

(defun skg-headline-has-focused-in-view-p ()
  "Return t if the current headline has 'focused' in its (view ...) metadata.
Assumes point is at the beginning of a headline.
Verifies the structure is (skg ... (view ... focused ...) ...)."
  (when (org-at-heading-p)
    (let* ((headline-text (skg-get-current-headline-text))
           (match-result (skg-split-as-stars-metadata-title headline-text)))
      (when match-result
        (let* ((inner (nth 1 match-result))
               ;; Parse all s-expressions from inner by wrapping in parens
               (all-sexps (read (concat "(" inner ")"))))
          ;; Search for (view ... focused ...) in the list of s-expressions
          (cl-some (lambda (sexp)
                     (skg-sexp-subtree-p sexp '(view focused)))
                   all-sexps))))))

(defun skg-add-focused-marker ()
  "Add 'focused' to (view ...) metadata of the current headline.
If point is in a headline body, navigates to the owning headline.
Merges '(skg (view focused))' into existing metadata, creating nested structure
if needed. If metadata already contains 'focused' in the view section, no change."
  (save-excursion
    (org-back-to-heading t)
    (skg-edit-metadata-at-point '(skg (view focused)))))

(defun skg-goto-focused-headline ()
  "Move point to the first headline with 'focused' in its (view ...) metadata.
If no focused headline is found, move point to the beginning of buffer.
TODO: This could be faster if we found 'focused' in the course of dealing
with 'folded' markers, rather than searching for it separately afterward."
  (goto-char (point-min))
  (let ((found-position nil))
    (save-excursion
      (while (and (not found-position) (not (eobp)))
        (beginning-of-line)
        (when (and (org-at-heading-p)
                   (looking-at ".*\\<focused\\>")
                   (skg-headline-has-focused-in-view-p))
          (setq found-position (point)))
        (forward-line 1)))
    (if found-position
        (goto-char found-position)
      (goto-char (point-min)))))

(defun skg-remove-focused-marker ()
  "Remove 'focused' from (view ...) metadata of the focused headline.
Searches for the first headline with 'focused' in its metadata and removes it.
If no focused headline is found, does nothing."
  (save-excursion
    (goto-char (point-min))
    (let ((found-position nil))
      (while (and (not found-position) (not (eobp)))
        (beginning-of-line)
        (when (and (org-at-heading-p)
                   (looking-at ".*\\<focused\\>")
                   (skg-headline-has-focused-in-view-p))
          (setq found-position (point)))
        (forward-line 1))
      (when found-position
        (goto-char found-position)
        (skg-edit-metadata-at-point
         '(skg (view (DELETE focused))))))))

(provide 'skg-focus)
