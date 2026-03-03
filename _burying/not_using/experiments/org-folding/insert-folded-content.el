;;; insert-folded-content.el --- Insert org content with folding markers

;; This demonstrates how to insert org-mode content line-by-line,
;; immediately folding lines marked with <skg<folded>>,
;; without needing to backtrack to parent nodes.

(require 'org)

(defun skg-insert-and-fold-line (line)
  "Insert LINE into current buffer and fold it if marked with <skg<folded>>.
The fold marker is removed from the inserted text.
Returns t if line was folded, nil otherwise."
  (let* ((has-fold-marker (string-match "<skg<folded>>" line))
         (clean-line (replace-regexp-in-string "<skg<folded>>" "" line)))

    ;; Insert the clean line (without fold marker)
    (insert clean-line)
    (when (not (string-suffix-p "\n" clean-line))
      (insert "\n"))

    ;; If marked as folded, immediately hide this line
    (when has-fold-marker
      (save-excursion
        (forward-line -1)  ; Go back to the line we just inserted
        (beginning-of-line)
        (when (looking-at org-heading-regexp)
          (let ((line-start (point))
                (line-end (progn (end-of-line) (point))))
            ;; Use outline-flag-region to hide just this headline
            ;; This works without needing fold specs to be registered
            (outline-flag-region line-start line-end t)))))

    has-fold-marker))

(defun skg-create-buffer-from-string (content)
  "Create an org-mode buffer from CONTENT string.
Lines containing <skg<folded>> will be immediately hidden as they're inserted.
The fold markers are removed from the final buffer content.
Returns the newly created buffer."
  (let ((buf (generate-new-buffer "*org-with-folding*")))
    (with-current-buffer buf
      (org-mode)
      ;; Process each line in order, folding as we go
      (dolist (line (split-string content "\n" t))
        (skg-insert-and-fold-line line))
      (goto-char (point-min)))
    buf))

;; Example usage and test
(defun demo-folded-insertion ()
  "Demonstrate inserting org content with pre-specified folding."
  (interactive)
  (let* ((test-content "* 1
** 11
*** <skg<folded>> 111
*** <skg<folded>> 112
** 12")
         (buf (skg-create-buffer-from-string test-content)))

    (message "\n=== Demo: Inserting content with fold markers ===")
    (message "Input content (with markers):\n%s\n" test-content)

    (with-current-buffer buf
      (message "=== Result ===")
      (message "Buffer content (fold markers removed):\n%s" (buffer-string))

      (message "\n=== Folding states ===")
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (beginning-of-line)
          (when (looking-at org-heading-regexp)
            (let* ((level (org-current-level))
                   (text (org-get-heading t t t t))
                   (invisible (invisible-p (point))))
              (message "  %s %s - Invisible: %s"
                       (make-string level ?*)
                       text
                       (if invisible "YES" "NO"))))
          (forward-line 1)))

      (switch-to-buffer buf)
      (message "\nBuffer opened. Lines 111 and 112 should be invisible."))))

;; Provide the feature
(provide 'insert-folded-content)

;;; insert-folded-content.el ends here
