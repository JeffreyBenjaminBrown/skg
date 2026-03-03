;;; test-folding.el --- Experiment with org-mode folding properties

;; Create a test buffer with the specified content
(defun create-test-buffer ()
  "Create a test buffer with the specified org content."
  (let ((buf (generate-new-buffer "*org-fold-test*")))
    (with-current-buffer buf
      (org-mode)
      (insert "* 1\n")
      (insert "** 11\n")
      (insert "** 12\n")
      (insert "*** 121\n")
      (insert "*** 122\n")
      (insert "** 13\n")
      ;; Move to line 3 (** 12)
      (goto-char (point-min))
      (forward-line 2))
    buf))

;; Collect folding information for all headlines
(defun collect-headline-info ()
  "Collect level, folded state, and text for each headline in current buffer.
Returns a list of (level folded text) for each headline."
  (save-excursion
    (goto-char (point-min))
    (let ((results '()))
      (while (not (eobp))
        (beginning-of-line)
        ;; Check if this line is a headline
        (when (looking-at org-heading-regexp)
          (let* ((level (org-current-level))
                 (text (org-get-heading t t t t))
                 ;; Check for folded/invisible property
                 (folded (org-fold-folded-p)))
            (push (list level folded text) results)))
        (forward-line 1))
      (nreverse results))))

;; Main experiment
(defun run-folding-experiment ()
  "Run the folding experiment and report results."
  (interactive)
  (let ((buf (create-test-buffer)))
    (with-current-buffer buf
      (message "\n=== Initial buffer state ===")
      (message "Buffer content:\n%s" (buffer-string))

      ;; Collect info before folding
      (message "\n=== Before folding (first collection) ===")
      (let ((before-1 (collect-headline-info)))
        (dolist (item before-1)
          (message "Level: %d, Folded: %s, Text: %s"
                   (nth 0 item) (nth 1 item) (nth 2 item))))

      ;; Collect again to check if collection modifies buffer
      (message "\n=== Before folding (second collection) ===")
      (let ((before-2 (collect-headline-info)))
        (dolist (item before-2)
          (message "Level: %d, Folded: %s, Text: %s"
                   (nth 0 item) (nth 1 item) (nth 2 item))))

      ;; Now fold the "** 12" headline
      (goto-char (point-min))
      (forward-line 2)  ; Move to ** 12
      (org-cycle)  ; Fold it

      (message "\n=== After folding ** 12 ===")
      (message "Buffer content (visible):\n%s"
               (buffer-substring-no-properties (point-min) (point-max)))

      ;; Collect info after folding (first time)
      (message "\n=== After folding (first collection) ===")
      (let ((after-1 (collect-headline-info)))
        (dolist (item after-1)
          (message "Level: %d, Folded: %s, Text: %s"
                   (nth 0 item) (nth 1 item) (nth 2 item))))

      ;; Collect again to check if collection modifies buffer
      (message "\n=== After folding (second collection) ===")
      (let ((after-2 (collect-headline-info)))
        (dolist (item after-2)
          (message "Level: %d, Folded: %s, Text: %s"
                   (nth 0 item) (nth 1 item) (nth 2 item))))

      ;; Keep buffer open for inspection
      (switch-to-buffer buf)
      (message "\nExperiment complete. Buffer kept open for inspection."))))

;; Run the experiment
(run-folding-experiment)
