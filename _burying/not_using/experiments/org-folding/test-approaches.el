;;; test-approaches.el --- Test different approaches to marking lines invisible

(require 'org)
(require 'org-fold)

(defun test-approach-1-outline-flag ()
  "Test using outline-flag-region."
  (let ((buf (generate-new-buffer "*test-outline-flag*")))
    (with-current-buffer buf
      (org-mode)
      (insert "* 1\n")
      (insert "** 11\n")
      (insert "*** 111\n")
      (save-excursion
        (goto-char (point-min))
        (forward-line 2)  ; Go to *** 111
        (beginning-of-line)
        (let ((start (point))
              (end (progn (end-of-line) (point))))
          (outline-flag-region start end t)))
      (goto-char (point-min)))
    (switch-to-buffer buf)
    (message "Approach 1: outline-flag-region")))

(defun test-approach-2-text-property ()
  "Test using text properties directly."
  (let ((buf (generate-new-buffer "*test-text-property*")))
    (with-current-buffer buf
      (org-mode)
      (insert "* 1\n")
      (insert "** 11\n")
      (insert "*** 111\n")
      (save-excursion
        (goto-char (point-min))
        (forward-line 2)  ; Go to *** 111
        (beginning-of-line)
        (let ((start (point))
              (end (progn (end-of-line) (point))))
          (put-text-property start end 'invisible 'outline)))
      (goto-char (point-min)))
    (switch-to-buffer buf)
    (message "Approach 2: put-text-property")))

(defun test-approach-3-org-fold-core ()
  "Test using org-fold-core-region with proper setup."
  (let ((buf (generate-new-buffer "*test-org-fold-core*")))
    (with-current-buffer buf
      (org-mode)
      (insert "* 1\n")
      (insert "** 11\n")
      (insert "*** 111\n")
      (save-excursion
        (goto-char (point-min))
        (forward-line 2)  ; Go to *** 111
        (beginning-of-line)
        (let ((start (point))
              (end (progn (end-of-line) (point))))
          ;; Use org-fold-core-region with the heading spec
          (org-fold-core-region start end t 'headline)))
      (goto-char (point-min)))
    (switch-to-buffer buf)
    (message "Approach 3: org-fold-core-region with 'headline spec")))

(defun run-all-tests ()
  "Run all three test approaches."
  (interactive)
  (message "\n=== Testing Three Approaches ===\n")

  (message "Test 1: outline-flag-region")
  (condition-case err
      (test-approach-1-outline-flag)
    (error (message "  ERROR: %s" err)))

  (sit-for 2)

  (message "\nTest 2: put-text-property")
  (condition-case err
      (test-approach-2-text-property)
    (error (message "  ERROR: %s" err)))

  (sit-for 2)

  (message "\nTest 3: org-fold-core-region")
  (condition-case err
      (test-approach-3-org-fold-core)
    (error (message "  ERROR: %s" err))))

;; Run the tests
(run-all-tests)
