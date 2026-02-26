;;; Shared helpers for save_collateral integration tests.
;;; Load with (load-file "path/to/test-helpers.el").

(defun headline-structure (buffer)
  "Extract (depth . id) pairs from every headline in BUFFER.
Depth is the number of asterisks. ID is extracted from the (skg (node (id X) ...))
metadata via skg-sexp-cdr-at-path. Headlines without metadata are skipped."
  (with-current-buffer buffer
    (let ((result '()))
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (let* ((line (buffer-substring-no-properties
                        (line-beginning-position) (line-end-position)))
                 (parts (skg-split-as-stars-metadata-title line)))
            (when parts
              (let* ((stars (nth 0 parts))
                     (metadata-str (nth 1 parts))
                     (depth (length (string-trim-right stars)))
                     (sexp (condition-case nil
                               (car (read-from-string metadata-str))
                             (error nil)))
                     (id-list (when sexp
                                (skg-sexp-cdr-at-path sexp '(skg node id))))
                     (id (when id-list
                           (format "%s" (car id-list)))))
                (when id
                  (push (cons depth id) result)))))
          (forward-line 1)))
      (nreverse result))))

(defun headline-titles (buffer)
  "Extract (depth . title) pairs from every headline in BUFFER.
Depth is the number of asterisks. Title is the text after metadata."
  (with-current-buffer buffer
    (let ((result '()))
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (let* ((line (buffer-substring-no-properties
                        (line-beginning-position) (line-end-position)))
                 (parts (skg-split-as-stars-metadata-title line)))
            (when parts
              (let* ((stars (nth 0 parts))
                     (title (nth 2 parts))
                     (depth (length (string-trim-right stars))))
                (push (cons depth title) result))))
          (forward-line 1)))
      (nreverse result))))

(defun format-pairs (pairs)
  "Format a list of (int . string) pairs for display."
  (mapconcat
   (lambda (pair)
     (format "(%d . %S)" (car pair) (cdr pair)))
   pairs ", "))

(defun assert-headline-structure (buffer expected phase-label)
  "Assert that BUFFER's headline structure matches EXPECTED.
EXPECTED is a list of (depth . id) pairs.
PHASE-LABEL is used in log messages. Kills emacs with exit 1 on failure."
  (let ((actual (headline-structure buffer)))
    (if (equal actual expected)
        (message "✓ PASS [%s]: headline-structure is ((%s))"
                 phase-label (format-pairs actual))
      (progn
        (message "✗ FAIL [%s]: headline-structure mismatch" phase-label)
        (message "  Expected: ((%s))" (format-pairs expected))
        (message "  Got:      ((%s))" (format-pairs actual))
        (message "  Buffer content: %S"
                 (with-current-buffer buffer
                   (buffer-substring-no-properties (point-min) (point-max))))
        (kill-emacs 1)))))

(defun assert-headline-titles (buffer expected phase-label)
  "Assert that BUFFER's headline titles match EXPECTED exactly.
EXPECTED is a list of (depth . title) pairs.
PHASE-LABEL is used in log messages. Kills emacs with exit 1 on failure."
  (let ((actual (headline-titles buffer)))
    (if (equal actual expected)
        (message "✓ PASS [%s]: headline-titles is ((%s))"
                 phase-label (format-pairs actual))
      (progn
        (message "✗ FAIL [%s]: headline-titles mismatch" phase-label)
        (message "  Expected: ((%s))" (format-pairs expected))
        (message "  Got:      ((%s))" (format-pairs actual))
        (message "  Buffer content: %S"
                 (with-current-buffer buffer
                   (buffer-substring-no-properties (point-min) (point-max))))
        (kill-emacs 1)))))

(defun headline-types-and-titles (buffer)
  "Extract (depth type title) triples from every headline in BUFFER.
Type is a symbol derived from the metadata:
  node, deleted, deletedScaffold, subscribeeCol, etc.
For (skg (node ...)) the type is node; for (skg (deleted ...)) it is
deleted; for (skg deletedScaffold) it is deletedScaffold; and so on."
  (with-current-buffer buffer
    (let ((result '()))
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (let* ((line (buffer-substring-no-properties
                        (line-beginning-position) (line-end-position)))
                 (parts (skg-split-as-stars-metadata-title line)))
            (when parts
              (let* ((stars (nth 0 parts))
                     (title (nth 2 parts))
                     (depth (length (string-trim-right stars)))
                     (metadata-str (nth 1 parts))
                     (sexp (condition-case nil
                               (car (read-from-string metadata-str))
                             (error nil)))
                     (type-elem (when (and sexp (listp sexp))
                                  (cadr sexp)))
                     (type-sym (cond
                                ((listp type-elem) (car type-elem))
                                ((symbolp type-elem) type-elem)
                                (t nil))))
                (when type-sym
                  (push (list depth type-sym title) result)))))
          (forward-line 1)))
      (nreverse result))))

(defun format-types-and-titles (triples)
  "Format a list of (depth type title) triples for display."
  (mapconcat
   (lambda (triple)
     (format "(%d %s %S)" (nth 0 triple) (nth 1 triple) (nth 2 triple)))
   triples "\n    "))

(defun assert-headline-types-and-titles (buffer expected phase-label)
  "Assert that BUFFER's (depth type title) triples match EXPECTED.
EXPECTED is a list of (depth type-symbol title-string) triples.
PHASE-LABEL is used in log messages. Kills emacs with exit 1 on failure."
  (let ((actual (headline-types-and-titles buffer)))
    (if (equal actual expected)
        (message "✓ PASS [%s]: headline-types-and-titles match"
                 phase-label)
      (progn
        (message "✗ FAIL [%s]: headline-types-and-titles mismatch"
                 phase-label)
        (message "  Expected:\n    %s" (format-types-and-titles expected))
        (message "  Got:\n    %s" (format-types-and-titles actual))
        (message "  Buffer content: %S"
                 (with-current-buffer buffer
                   (buffer-substring-no-properties (point-min) (point-max))))
        (kill-emacs 1)))))

(provide 'test-helpers)
