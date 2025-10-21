;; Utilities for managing focus metadata on org headlines

(require 'org)
(require 'skg-metadata)

(defun skg-add-focused-marker ()
  "Add 'focused' to (view ...) metadata of the current headline.
If point is in a headline body, navigates to the owning headline.
Merges '(skg (view focused))' into existing metadata, creating nested structure
if needed. If metadata already contains 'focused' in the view section, no change."
  (save-excursion
    (org-back-to-heading t)
    (skg-edit-metadata-at-point '(skg (view focused)))))

(provide 'skg-focus)
