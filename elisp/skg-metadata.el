;; Utilities to parse and edit skg headline metadata

(require 'org)

(defun skg-parse-headline-metadata (headline-text)
  "Parse skg metadata from HEADLINE-TEXT after org bullets.
Returns (METADATA-ALIST BARE-VALUES-SET TITLE-TEXT) or nil if no metadata found.
METADATA-ALIST contains key-value pairs, BARE-VALUES-SET contains standalone values."
  (let ((trimmed (string-trim-left headline-text)))
    (when (string-match "^<skg<\\([^<>]*\\)>>\\s-*\\(.*\\)" trimmed)
      (let* ((inner (match-string 1 trimmed))
             (title (match-string 2 trimmed))
             (parsed (skg-parse-metadata-inner inner)))
        (list (car parsed) (cadr parsed) title)))))

(defun skg-parse-metadata-inner (inner)
  "Parse comma-separated metadata INNER string.
Returns (ALIST SET) where ALIST contains key:value pairs and SET contains bare values."
  (let ((alist '())
        (set '()))
    (dolist (raw (split-string inner "," t))
      (let ((tok (string-trim raw)))
        (unless (string-empty-p tok)
          (if (string-match "\\([^:]+\\):\\(.*\\)" tok)
              ;; Key-value pair
              (let ((key (string-trim (match-string 1 tok)))
                    (val (string-trim (match-string 2 tok))))
                (push (cons key val) alist))
            ;; Bare value
            (push tok set)))))
    (list (nreverse alist) (nreverse set))))

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

(defun skg-delete-kv-pair-from-metadata-by-key
    (key)
  "Delete all kv-pairs with KEY from the metadata of the headline at point.
If the current line is not a headline, or has no metadata, no effect."
  (when (org-at-heading-p)
    (let* ((headline-text (skg-get-current-headline-text))
           (trimmed (string-trim-left headline-text))
           (has-metadata (string-match "^\\(\\*+\\s-+\\)<skg<\\([^<>]*\\)>>\\s-*\\(.*\\)" trimmed)))
      (when has-metadata
        (let* ((stars (match-string 1 trimmed))
               (inner (match-string 2 trimmed))
               (title (match-string 3 trimmed))
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
          (insert (format "%s<skg<%s>> %s"
                          stars new-inner title)))))))

(defun skg-delete-value-from-metadata
    (value)
  "Delete all instances of VALUE from the metadata of the headline at point.
If the current line is not a headline, or has no metadata, no effect."
  (when (org-at-heading-p)
    (let*
        ((headline-text (skg-get-current-headline-text))
         (trimmed (string-trim-left headline-text))
         (has-metadata
          (string-match "^\\(\\*+\\s-+\\)<skg<\\([^<>]*\\)>>\\s-*\\(.*\\)" trimmed)))
      (when has-metadata
        (let* ((stars (match-string 1 trimmed))
               (inner (match-string 2 trimmed))
               (title (match-string 3 trimmed))
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
          (insert (format "%s<skg<%s>> %s"
                          stars new-inner title)))))))

(defun skg-insert-bare-value-into-metadata
    (value)
  "Insert VALUE into the metadata of the headline at point.
If there is metadata, appends it at the end unless already present.
If there is no metadata, creates '<skg<VALUE>>'.
If the current line is not a headline, no effect.
Does nothing if VALUE already exists in metadata."
  (when (org-at-heading-p)
    (let* ((headline-text (skg-get-current-headline-text))
           (trimmed (string-trim-left headline-text))
           (has-metadata
            (string-match "^\\(\\*+\\s-+\\)<skg<\\([^<>]*\\)>>\\s-*\\(.*\\)" trimmed)))
      (if has-metadata
          (let* ((stars (match-string 1 trimmed))
                 (inner (match-string 2 trimmed))
                 (title (match-string 3 trimmed))
                 (parsed (skg-parse-metadata-inner inner))
                 (alist (car parsed))
                 (bare-values (cadr parsed)))
            (unless (member value bare-values)
              (let* ((new-bare-values
                      (append bare-values (list value)))
                     (new-inner (skg-reconstruct-metadata-inner
                                 alist new-bare-values)))
                (beginning-of-line)
                (delete-region (line-beginning-position)
                               (line-end-position))
                (insert (format "%s<skg<%s>> %s"
                                stars new-inner title)))))
        (when (string-match "^\\(\\*+\\s-+\\)\\(.*\\)" trimmed)
          (let* ((stars (match-string 1 trimmed))
                 (title (match-string 2 trimmed)))
            (beginning-of-line)
            (delete-region (line-beginning-position)
                           (line-end-position))
            (insert (format "%s<skg<%s>> %s"
                            stars value title))))))))

(defun skg-insert-kv-pair-into-metadata
    (key value)
  "Insert KEY:VALUE into the metadata of the headline at point.
If there is metadata, appends it at the end unless KEY already exists.
If there is no metadata, creates '<skg<KEY:VALUE>>'.
If the current line is not a headline, no effect.
Does nothing if KEY already exists in metadata."
  (when (org-at-heading-p)
    (let* ((headline-text (skg-get-current-headline-text))
           (trimmed (string-trim-left headline-text))
           (has-metadata
            (string-match "^\\(\\*+\\s-+\\)<skg<\\([^<>]*\\)>>\\s-*\\(.*\\)" trimmed)))
      (if has-metadata
          (let* ((stars (match-string 1 trimmed))
                 (inner (match-string 2 trimmed))
                 (title (match-string 3 trimmed))
                 (parsed (skg-parse-metadata-inner inner))
                 (alist (car parsed))
                 (bare-values (cadr parsed)))
            (unless (assoc key alist)
              (let* ((new-alist
                      (append alist (list (cons key value))))
                     (new-inner
                      (skg-reconstruct-metadata-inner
                       new-alist bare-values)))
                (beginning-of-line)
                (delete-region (line-beginning-position)
                               (line-end-position))
                (insert (format "%s<skg<%s>> %s"
                                stars new-inner title)))))
        (when (string-match "^\\(\\*+\\s-+\\)\\(.*\\)" trimmed)
          (let* ((stars (match-string 1 trimmed))
                 (title (match-string 2 trimmed)))
            (beginning-of-line)
            (delete-region (line-beginning-position)
                           (line-end-position))
            (insert (format "%s<skg<%s:%s>> %s"
                            stars key value title))))))))

(defun skg-reconstruct-metadata-inner
    (alist bare-values)
  "Reconstruct metadata inner string from ALIST and BARE-VALUES.
Returns a comma-separated string suitable for use inside <skg<...>>."
  (let ((parts '()))
    (dolist (kv alist)
      (push (format "%s:%s" (car kv) (cdr kv))
            parts))
    (dolist (val bare-values)
      (push val parts))
    (mapconcat #'identity (nreverse parts) ",")))

(provide 'skg-metadata)
