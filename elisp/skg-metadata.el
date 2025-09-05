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

(defun skg-get-current-headline-metadata ()
  "Extract metadata from current org headline.
Returns (ID LEVEL TITLE) or signals error if not on headline or no ID found."
  ;; TODO: This could be simplified by using the org function that returns the headline without the bullet.
  (unless (org-at-heading-p)
    (error "Not on an org headline"))

  (let* ((headline-text (buffer-substring-no-properties
                         (line-beginning-position)
                         (line-end-position)))
         ;; Remove org bullets (asterisks and space)
         (after-bullet (if (string-match "^\\*+\\s-+\\(.*\\)" headline-text)
                          (match-string 1 headline-text)
                        headline-text))
         (metadata-parsed (skg-parse-headline-metadata after-bullet))
         (level (org-current-level)))

    (unless metadata-parsed
      (error "No skg metadata found on current headline"))

    (let* ((alist (car metadata-parsed))
           (title (caddr metadata-parsed))
           (id (cdr (assoc "id" alist))))

      (unless id
        (error "No ID found in headline metadata"))

      (list id level title))))

(provide 'skg-metadata)
