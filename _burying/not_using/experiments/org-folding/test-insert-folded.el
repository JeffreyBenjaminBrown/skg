;;; test-insert-folded.el --- Experiment with inserting pre-folded content

(defun insert-and-fold-line (line)
  "Insert LINE into buffer and fold it if it contains <skg<folded>>."
  (let* ((folded (string-match "<skg<folded>>" line))
         (clean-line (replace-regexp-in-string "<skg<folded>>" "" line)))
    ;; Insert the clean line
    (insert clean-line)
    (when (not (string-suffix-p "\n" clean-line))
      (insert "\n"))

    ;; If this line should be folded, hide it immediately
    (when folded
      (save-excursion
        (forward-line -1)  ; Go back to the line we just inserted
        (beginning-of-line)
        ;; Hide this entire line using org-fold
        (when (looking-at org-heading-regexp)
          (let ((end (save-excursion
                       (end-of-line)
                       (point))))
            (org-fold-region (point) end t 'org-fold-outline)))))))

(defun create-buffer-from-string (content)
  "Create an org buffer from CONTENT string, respecting fold markers."
  (let ((buf (generate-new-buffer "*org-fold-insertion-test*")))
    (with-current-buffer buf
      (org-mode)
      ;; Process line by line
      (dolist (line (split-string content "\n" t))
        (insert-and-fold-line line))
      (goto-char (point-min)))
    buf))

(defun test-insert-folded ()
  "Test inserting content with fold markers."
  (interactive)
  (let* ((test-content "* 1
** 11
*** <skg<folded>> 111
*** <skg<folded>> 112
** 12")
         (buf (create-buffer-from-string test-content)))

    (with-current-buffer buf
      (message "\n=== Buffer after insertion ===")
      (message "Full buffer content:\n%s" (buffer-string))

      (message "\n=== Headline folding states ===")
      (let ((info (save-excursion
                    (goto-char (point-min))
                    (let ((results '()))
                      (while (not (eobp))
                        (beginning-of-line)
                        (when (looking-at org-heading-regexp)
                          (let* ((level (org-current-level))
                                 (text (org-get-heading t t t t))
                                 (folded (org-fold-folded-p)))
                            (push (list level folded text) results)))
                        (forward-line 1))
                      (nreverse results)))))
        (dolist (item info)
          (message "Level: %d, Folded: %s, Text: %s"
                   (nth 0 item) (nth 1 item) (nth 2 item))))

      (message "\n=== Checking visibility ===")
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (let* ((line-start (line-beginning-position))
                 (line-end (line-end-position))
                 (invisible (invisible-p line-start)))
            (message "Line %d: %s (invisible: %s)"
                     (line-number-at-pos)
                     (buffer-substring-no-properties line-start line-end)
                     invisible))
          (forward-line 1)))

      (switch-to-buffer buf)
      (message "\nBuffer created and displayed."))))

;; Run the test
(test-insert-folded)
