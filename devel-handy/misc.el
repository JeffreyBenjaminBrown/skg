;; `M-x pp-28` pretty-prints a highlighted s-exp.

(defun props ()
  (interactive)
  (let ((props (text-properties-at (point))))
    (message "Properties at point %d: %S" (point) props)
    props))

(defun xxx ()
  (interactive)
  (message "%S"
           (org-sexp-parse-all-branches)))

(defun 2id-insert ()
  "If at a heading, add a random 3-digit ID to it, both visibly and as a property of the bullet."
  (interactive)
  (when (org-at-heading-p)
    (let* ((random-id (number-to-string (+ 100 (random 900))))
           (current-point (point))
           (heading-start (line-beginning-position))
           (stars-end (save-excursion
                        (goto-char heading-start)
                        (looking-at "\\*+\\s-+")
                        (match-end 0))))
      (add-text-properties heading-start stars-end
                           `(id ,random-id))
      (goto-char stars-end)
      (insert (concat random-id " "))
      (goto-char (+ current-point (length random-id) 1)))))
