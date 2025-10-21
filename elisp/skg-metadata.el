;; Utilities to parse and edit skg headline metadata

(require 'org)
(require 'skg-sexpr)

(defun skg-parse-headline-metadata (headline-text)
  "Parse skg metadata from HEADLINE-TEXT after org bullets.
Returns (METADATA-ALIST BARE-VALUES-SET TITLE-TEXT) or nil if no metadata found.
METADATA-ALIST contains key-value pairs, BARE-VALUES-SET contains standalone values."
  (let ((trimmed (string-trim-left headline-text)))
    (when (string-prefix-p "(skg" trimmed)
      (let ((sexp-end-pos (skg-find-sexp-end trimmed)))
        (when sexp-end-pos
          (let* ((skg-sexp (substring trimmed 0 sexp-end-pos))
                 (title-start sexp-end-pos)
                 (len (length trimmed))
                 (title (string-trim (if (< title-start len)
                                         (substring trimmed title-start)
                                       "")))
                 ;; Extract content between "(skg" and final ")"
                 (inner (string-trim (substring skg-sexp 4 (1- (length skg-sexp)))))
                 (parsed (skg-parse-metadata-inner inner)))
            (list (car parsed) (cadr parsed) title)))))))

(defun skg-delete-kv-pair-from-metadata-by-key
    (key)
  "Delete all kv-pairs with KEY from the metadata of the headline at point.
If the current line is not a headline, or has no metadata, no effect."
  (when (org-at-heading-p)
    (let* ((headline-text (skg-get-current-headline-text))
           (match-result (skg-split-as-stars-metadata-title
                          headline-text)))
      (when (and match-result
                 (string-match-p "(skg" headline-text))
        (let* ((stars (nth 0 match-result))
               (inner (nth 1 match-result))
               (title (nth 2 match-result))
               (parsed (skg-parse-metadata-inner inner))
               (alist (car parsed))
               (bare-values (cadr parsed))
               (filtered-alist (seq-filter
                                (lambda (kv)
                                  (not (string-equal (car kv) key)))
                                alist))
               (new-inner (skg-reconstruct-metadata-inner
                           filtered-alist bare-values)))
          (beginning-of-line)
          (delete-region (line-beginning-position)
                         (line-end-position))
          (insert (skg-format-headline stars new-inner title)))))))

(defun skg-delete-value-from-metadata
    (value)
  "Delete all instances of VALUE from the metadata of the headline at point.
If the current line is not a headline, or has no metadata, no effect."
  (when (org-at-heading-p)
    (let* ((headline-text (skg-get-current-headline-text))
           (match-result (skg-split-as-stars-metadata-title
                          headline-text)))
      (when (and match-result
                 (string-match-p "(skg" headline-text))
        (let* ((stars (nth 0 match-result))
               (inner (nth 1 match-result))
               (title (nth 2 match-result))
               (parsed (skg-parse-metadata-inner inner))
               (alist (car parsed))
               (bare-values (cadr parsed))
               (filtered-values
                (seq-filter (lambda (v)
                              (not (string-equal v value)))
                            bare-values))
               (new-inner (skg-reconstruct-metadata-inner
                           alist filtered-values)))
          (beginning-of-line)
          (delete-region (line-beginning-position)
                         (line-end-position))
          (insert (skg-format-headline stars new-inner title)))))))

(defun skg-replace-current-line (new-content)
  "Replace the current line with NEW-CONTENT.
Moves to beginning of line, deletes the line, and inserts NEW-CONTENT."
  (beginning-of-line)
  (delete-region (line-beginning-position)
                 (line-end-position))
  (insert new-content))

(defun skg-merge-metadata-at-point (new-content)
  "Merge NEW-CONTENT into the metadata of the headline at point.
If there is metadata, merges it with existing metadata.
If there is no metadata, creates new metadata from NEW-CONTENT.
If the current line is not a headline, no effect."
  (when (org-at-heading-p) ;; otherwise this does nothing
    (let* ((headline-text (skg-get-current-headline-text))
           (match-result ;; could be nil
            (skg-split-as-stars-metadata-title
             headline-text)))
      (if match-result
          (let* ((stars (nth 0 match-result))
                 (inner (nth 1 match-result))
                 (title (nth 2 match-result))
                 (host-sexp (read (concat "(skg " inner ")")))
                 (merged-sexp (skg-edit-nested-sexp
                               host-sexp new-content)))
            (let ((new-inner (substring-no-properties
                              (format "%S" merged-sexp)
                              5 -1)))
              (skg-replace-current-line
               (skg-format-headline stars new-inner title))))
        (progn ;; Headline has no metadata.
          (when (string-match "^\\(\\*+\\s-+\\)\\(.*\\)" headline-text)
          (let* ((stars (match-string 1 headline-text))
                 (title (match-string 2 headline-text))
                 (metadata (substring-no-properties
                               (format "%S" new-content)
                               5 -1)))
            (skg-replace-current-line
             (skg-format-headline stars metadata title)))))))))

(defun skg-split-as-stars-metadata-title (headline-text)
  "Match HEADLINE-TEXT and extract stars, metadata inner, and title.
Returns (STARS INNER TITLE) or nil if no match.
Handles nested parentheses in metadata correctly."
  (let ((trimmed (string-trim-left headline-text)))
    (when (string-match "^\\(\\*+\\s-+\\)" trimmed)
      (let* ((stars (match-string 1 trimmed))
             (after-stars (substring trimmed (match-end 1))))
        (if (string-prefix-p "(skg" after-stars)
            ;; Has metadata - find matching close paren
            (let ((sexp-end-pos (skg-find-sexp-end after-stars)))
              (when sexp-end-pos
                (let* ((skg-sexp (substring after-stars 0 sexp-end-pos))
                       (title-start sexp-end-pos)
                       (len (length after-stars))
                       (title (string-trim (if (< title-start len)
                                               (substring after-stars title-start)
                                             "")))
                       (inner (string-trim (substring skg-sexp 4 (1- (length skg-sexp))))))
                  (list stars inner title))))
          ;; No metadata
          (list stars "" after-stars))))))

(defun skg-get-current-headline-text ()
  "ASSUMES
point is already on a headline - does not move point.
.
Returns the current headline in its entirety,
including asterisks and metadata, but not the trailing newline."
  (save-excursion
    (beginning-of-line)
    (let ((start (point)))
      (end-of-line)
      (buffer-substring-no-properties start (point)))))

(defun skg-parse-metadata-inner (inner)
  "Parse metadata INNER string containing s-expressions and bare values.
Returns (ALIST SET) where ALIST contains (key value) pairs and SET contains bare values."
  (let ((alist '())
        (set '())
        (input (concat "(" inner ")"))
        (pos 0))
    (with-temp-buffer
      (insert input)
      (goto-char (point-min))
      (condition-case nil
          (let ((elements (read (current-buffer))))
            (dolist (element elements)
              (cond
               (;; (key value) pair
                (and (listp element)
                     (= (length element) 2))
                (let ((key (format "%s" (car element)))
                      (val (format "%s" (cadr element))))
                  (push (cons key val) alist)))
               (;; Special case: (rels ...) sub-s-expr
                (and (listp element)
                     (> (length element) 2)
                     (eq (car element) 'rels))
                ;; Store as a special entry with the inner content as a string
                (let ((rel-inner (mapconcat
                                  (lambda (x) (format "%s" x))
                                  (cdr element)
                                  " ")))
                  (push (cons "rels" rel-inner) alist)))
               (t ;; Bare value (symbol or other atom)
                (let ((bare-val (format "%s" element)))
                  (push bare-val set))))))
        (error nil)))
    (list (nreverse alist) (nreverse set))))

(defun skg-reconstruct-metadata-inner
    (alist bare-values)
  "Reconstruct metadata inner string from ALIST and BARE-VALUES.
Returns a whitespace-separated string suitable for use inside (skg ...).
Key-value pairs are formatted as (key value)."
  (let ((parts '()))
    (dolist (kv alist)
      (push (format "(%s %s)" (car kv) (cdr kv))
            parts))
    (dolist (val bare-values)
      (push val parts))
    (mapconcat #'identity (nreverse parts) " ")))

(defun skg-format-headline
    (stars metadata-inner title)
  "Format a headline with STARS, METADATA-INNER, and TITLE.
Handles empty metadata correctly."
  (if (string-empty-p metadata-inner)
      (format "%s(skg) %s" stars title)
    (format "%s(skg %s) %s" stars metadata-inner title)))

(provide 'skg-metadata)
