(defun alist-diff (expected actual &optional path)
  "Compare two recursive alists and show differences, ignoring order."
  (let ((path (or path "root")))
    (cond
     ((and (consp expected) (consp actual))
      (let ((exp-keys (mapcar #'car expected))
            (act-keys (mapcar #'car actual)))
        ;; Check for missing/extra keys - use while loops instead of dolist
        (let ((keys exp-keys))
          (while keys
            (let ((current-key (car keys)))
              (unless (assoc current-key actual)
                (message "Missing key at %s: %s" path current-key))
              (setq keys (cdr keys)))))

        (let ((keys act-keys))
          (while keys
            (let ((current-key (car keys)))
              (unless (assoc current-key expected)
                (message "Extra key at %s: %s" path current-key))
              (setq keys (cdr keys)))))

        ;; Recursively check matching keys
        (let ((pairs expected))
          (while pairs
            (let* ((pair (car pairs))
                   (current-key (car pair))
                   (exp-val (cdr pair))
                   (act-val (cdr (assoc current-key actual))))
              (alist-diff exp-val act-val
                          (format "%s.%s" path current-key))
              (setq pairs (cdr pairs)))))))
     ((not (equal expected actual))
      (message "Value mismatch at %s: expected %S, got %S"
               path expected actual)))))

(provide 'skg-util)
