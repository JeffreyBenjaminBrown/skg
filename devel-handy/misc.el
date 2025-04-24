;; `M-x pp-28` pretty-prints a highlighted s-exp.

(defun props ()
  (interactive)
  (let ((props (text-properties-at (point))))
    (message "Properties at point %d: %S" (point) props)
    props))

(defun xxx ()
  (interactive)
  (message "%S" (org-sexp-parse-all-branches)))
