;;; -*- lexical-binding: t; -*-
;; PURPOSE: Mostly for interactive experimentation.
;; The test suite uses one of these functions.
;; The library uses none of them.


;; `M-x pp-28` pretty-prints a highlighted s-exp.

(defun skg-props ()
  "Print the properties at point to the minibuffer."
  (interactive)
  (let (( props ( text-properties-at ( point )) ))
    (message "Properties at point %d: %S" (point) props)
    props ))

(defun skg-2id-insert ()
  "If at a headline, add a random 3-digit ID to it, both visibly and as a property of the bullet."
  (interactive)
  (when (org-at-heading-p)
    (let* ((random-id (number-to-string
                       (random 1000)) )
           (current-point (point))
           (headline-start (line-beginning-position))
           (stars-end (save-excursion
                        (goto-char headline-start)
                        (looking-at "\\*+\\s-+")
                        (match-end 0)) ))
      (add-text-properties headline-start stars-end
                           `(id ,random-id))
      (goto-char stars-end)
      (insert (concat random-id " "))
      (goto-char (+ current-point (length random-id) 1 )) )) )

(defun skg-add-id-properties-to-all-headlines ()
  "Add an 'id' text property to each headline in the current buffer.
The 'id' property value equals the headline text.
The property is applied to the entire headline line (asterisks and text, even trailing whitespace).
Preserves folding state by using save-excursion and save-restriction."

  (interactive)
  (save-excursion
    (save-restriction
      (widen)  ; Ensure we can see the whole buffer
      (goto-char (point-min))
      (while (re-search-forward "^\\*+ \\(.+\\)$" nil t)
        (let ((headline-text (match-string-no-properties 1))
              (line-start (line-beginning-position))
              (line-end   (line-end-position)) )
          (put-text-property
           line-start  line-end
           'id  headline-text )) )) ))

(defun skg-alist-diff (expected actual &optional path)
  "Compare two recursive alists and show differences, ignoring order."
  (let ((path (or path "root")) )
    (cond
     ((and (consp expected) (consp actual))
      (let ((exp-keys (mapcar #'car expected))
            (act-keys (mapcar #'car actual)) )
        ;; Check for missing/extra keys - use while loops instead of dolist
        (let ((keys exp-keys))
          (while keys
            (let ((current-key (car keys)) )
              (unless (assoc current-key actual)
                (message "Missing key at %s: %s" path current-key))
              (setq keys (cdr keys)) )) )

        (let ((keys act-keys))
          (while keys
            (let ((current-key (car keys)) )
              (unless (assoc current-key expected)
                (message "Extra key at %s: %s" path current-key))
              (setq keys (cdr keys)) )) )

        ;; Recursively check matching keys
        (let ((pairs expected))
          (while pairs
            (let* ((pair (car pairs))
                   (current-key (car pair))
                   (exp-val (cdr pair))
                   (act-val (cdr (assoc current-key actual)) ))
              (skg-alist-diff exp-val act-val
                          (format "%s.%s" path current-key))
              (setq pairs (cdr pairs)) )) )) )
     ((not (equal expected actual))
      (message "Value mismatch at %s: expected %S, got %S"
               path expected actual)) )) )

(defun skg-doc-get-property (node property-key)
  "Get PROPERTY from NODE."
  (when (and node (consp node))
    (if (and (symbolp (car node))
             (eq (car node) property-key))
        (cdr node)
      (cdr (assq property-key node)))))

(provide 'skg-util)
