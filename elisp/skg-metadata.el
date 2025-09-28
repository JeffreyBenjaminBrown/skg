;; Metadata parsing utilities for skg headlines

;; TODO: Does this duplicate functionality in heralds-minor-mode.el?

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
  "Returns the current headline in its entirety,
including asterisks and metadata, but not the trailing newline."
  (save-excursion
    (org-back-to-heading)
    (let ((start (point)))
      (end-of-line)
      (buffer-substring-no-properties start (point)))))

(provide 'skg-metadata)
