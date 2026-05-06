;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: Utilities to parse and edit skg headline metadata.

(require 'org)
(require 'org-fold-core)
(require 'skg-config)
(require 'skg-sexpr-search)

(defun skg-delete (&optional recursive)
  "Mark the headline at point for deletion.
With a prefix argument RECURSIVE, also mark every truenode
org-descendent (equivalent to `skg-delete-recursive').
Edits the metadata to include `delete` in the `editRequest` section.
Does NOT save; call `skg-request-save-buffer' afterward."
  (interactive "P")
  (if recursive
      (skg-delete-recursive)
    (skg-edit-metadata-at-point '(skg (node (editRequest delete))))
    (message "This change will only be applied when you save the buffer.")))

(defun skg-delete-recursive ()
  "Mark the headline at point, and every truenode org-descendent of it,
for deletion. Descendent headlines that are not truenodes (phantoms,
aliascol, id-col, etc.) are skipped. Does NOT save;
call `skg-request-save-buffer' afterward."
  (interactive)
  (unless (org-at-heading-p)
    (user-error "Not on a headline"))
  (save-excursion
    (let ((start-level (org-outline-level)))
      (skg-edit-metadata-at-point
       '(skg (node (editRequest delete)))) ;; mark this one
      (outline-next-heading)
      (while (and (not (eobp))
                  (> (org-outline-level) start-level))
        (let* ((parts (skg-split-as-stars-metadata-title
                       (skg-get-current-headline-text)))
               (meta (and parts (nth 1 parts))))
          (when (and meta (not (string-empty-p meta))
                     (skg-sexp-subtree-p (read meta) '(skg (node))))
            (skg-edit-metadata-at-point ;; mark a descendent
             '(skg (node (editRequest delete))))))
        (outline-next-heading))))
  (message "This change will only be applied when you save the buffer."))

(defun skg-make-indefinitive ()
  "Mark the headline at point as indefinitive.
Edits the metadata to include `indef` in the `node` section.
Does NOT save; call `skg-request-save-buffer' afterward."
  (interactive)
  (skg-edit-metadata-at-point '(skg (node indef))))

(defun skg--current-headline-metadata-sexp ()
  "Return the parsed skg metadata sexp for the headline at point."
  (unless (org-at-heading-p)
    (user-error "Not on a headline"))
  (let* ((headline (skg-get-current-headline-text))
         (split (skg-split-as-stars-metadata-title headline))
         (metadata-str (and split (cadr split))))
    (unless (and metadata-str
                 (not (string-empty-p metadata-str)))
      (user-error "Headline has no skg metadata"))
    (read metadata-str)))

(defun skg--current-node-source ()
  "Return the source string for the TrueNode headline at point."
  (let* ((sexp (skg--current-headline-metadata-sexp))
         (source-values (skg-sexp-cdr-at-path sexp '(skg node source))))
    (unless source-values
      (user-error "Node has no source"))
    (format "%s" (car source-values))))

(defun skg-change-source ()
  "Prompt for and change the source of the node at point.
Starts with the current source as minibuffer text.  S-left/S-right cycle
through owned sources, C-? displays all configured sources and
their paths, and typed source names are accepted directly.
Does NOT save; call `skg-request-save-buffer' afterward."
  (interactive)
  (let* ((current-source (skg--current-node-source))
         (new-source (string-trim
                      (skg--prompt-for-source-change current-source))))
    (unless (string-empty-p new-source)
      (when (string-match-p "[[:space:]]" new-source)
        (user-error "Source names cannot contain whitespace"))
      (if (string= current-source new-source)
          (message "Source unchanged: %s" current-source)
        (skg-edit-metadata-at-point
         `(skg (node (ENSURE (source ,(intern new-source)))
                     (viewStats))))
        (skg-edit-metadata-at-point
         `(skg (node (viewStats
                      (ENSURE
                       (sourceHerald ,(intern
                                        (format "⌂:%s" new-source))))))))
        (message "Source changed from %s to %s. Save to apply."
                 current-source new-source)))))

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
                 (parsed (skg-parse-metadata-sexp skg-sexp)))
            (list (car parsed) (cadr parsed) title)))))))

(defun skg-delete-kv-pair-from-metadata-by-key
    (key)
  "Delete all kv-pairs with KEY from the metadata of the headline at point.
If the current line is not a headline, or has no metadata, no effect.
Routes the rewrite through `skg-replace-current-line' so that editing
a folded heading does not fire org-fold's fragility check — see the
\"Programmatic metadata edits must be performed ignoring fragility
checks\" entry in PITFALLs.org."
  (when (org-at-heading-p)
    (let* ((headline-text (skg-get-current-headline-text))
           (match-result (skg-split-as-stars-metadata-title
                          headline-text)))
      (when (and match-result
                 (string-match-p "(skg" headline-text))
        (let* ((stars (nth 0 match-result))
               (metadata-sexp (nth 1 match-result))
               (title (nth 2 match-result))
               (parsed (skg-parse-metadata-sexp metadata-sexp))
               (alist (car parsed))
               (bare-values (cadr parsed))
               (filtered-alist (seq-filter
                                (lambda (kv)
                                  (not (string-equal (car kv) key)))
                                alist))
               (new-metadata-sexp (skg-reconstruct-metadata-sexp
                                   filtered-alist bare-values)))
          (skg-replace-current-line
           (skg-format-headline stars new-metadata-sexp title)))))))

(defun skg-delete-value-from-metadata
    (value)
  "Delete all instances of VALUE from the metadata of the headline at point.
If the current line is not a headline, or has no metadata, no effect.
Routes the rewrite through `skg-replace-current-line' so that editing
a folded heading does not fire org-fold's fragility check — see the
\"Programmatic metadata edits must be performed ignoring fragility
checks\" entry in PITFALLs.org."
  (when (org-at-heading-p)
    (let* ((headline-text (skg-get-current-headline-text))
           (match-result (skg-split-as-stars-metadata-title
                          headline-text)))
      (when (and match-result
                 (string-match-p "(skg" headline-text))
        (let* ((stars (nth 0 match-result))
               (metadata-sexp (nth 1 match-result))
               (title (nth 2 match-result))
               (parsed (skg-parse-metadata-sexp metadata-sexp))
               (alist (car parsed))
               (bare-values (cadr parsed))
               (filtered-values
                (seq-filter (lambda (v)
                              (not (string-equal v value)))
                            bare-values))
               (new-metadata-sexp (skg-reconstruct-metadata-sexp
                                   alist filtered-values)))
          (skg-replace-current-line
           (skg-format-headline stars new-metadata-sexp title)))))))

(defun skg-edit-metadata-at-point (edits)
  "Use EDITS to edit the metadata of the headline at point.
If there is metadata, merges it with existing metadata.
If there is no metadata, creates new metadata from EDITS.
If the current line is not a headline, no effect.
Routes the rewrite through `skg-replace-current-line' so that editing
a folded heading does not fire org-fold's fragility check — see the
\"Programmatic metadata edits must be performed ignoring fragility
checks\" entry in PITFALLs.org."
  (when (org-at-heading-p) ;; otherwise this does nothing
    (let* ((headline-text (skg-get-current-headline-text))
           (match-result ;; could be nil
            (skg-split-as-stars-metadata-title
             headline-text)))
      (if (and match-result
               (not (string-empty-p (nth 1 match-result))))
          (let* ((stars (nth 0 match-result))
                 (metadata-sexp (nth 1 match-result))
                 (title (nth 2 match-result))
                 (host-sexp (read metadata-sexp))
                 (merged-sexp (skg-edit-nested-sexp
                               host-sexp edits)))
            (let ((new-metadata-sexp (substring-no-properties
                                      (format "%S" merged-sexp))))
              (skg-replace-current-line
               (skg-format-headline stars new-metadata-sexp title))))
        (progn ;; Headline has no metadata.
          (when (string-match "^\\(\\*+\\s-+\\)\\(.*\\)" headline-text)
          (let* ((stars (match-string 1 headline-text))
                 (title (match-string 2 headline-text))
                 (metadata-sexp (substring-no-properties
                                 (format "%S" edits))))
            (skg-replace-current-line
             (skg-format-headline stars metadata-sexp title)))))))))

(defun skg-replace-current-line (new-content)
  "Replace the current line with NEW-CONTENT.
Moves to beginning of line, deletes the line, and inserts NEW-CONTENT.
The `delete-region' + `insert' pair below runs inside
`org-fold-core-ignore-fragility-checks' to protect against corrupting
a folded subtree beneath the edited heading line — see the
\"Programmatic metadata edits must be performed ignoring fragility
checks\" entry in PITFALLs.org for the full explanation. Do not
bypass this helper when rewriting a heading line in place; call it
instead of raw `delete-region' + `insert'."
  (beginning-of-line)
  (org-fold-core-ignore-fragility-checks
    (delete-region (line-beginning-position)
                   (line-end-position))
    (insert new-content)))

(defun skg-split-as-stars-metadata-title (headline-text)
  "Match HEADLINE-TEXT and extract stars, metadata sexp, and title.
Returns (STARS METADATA-SEXP TITLE) or nil if no match.
METADATA-SEXP is the complete (skg ...) s-expression, or empty string if no metadata.
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
                                             ""))))
                  (list stars skg-sexp title))))
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

(defun skg-parse-metadata-sexp (metadata-sexp)
  "Parse METADATA-SEXP string containing (skg ...) s-expression.
Returns (ALIST SET) where ALIST contains (key value) pairs and SET contains bare values."
  (let ((alist '())
        (set '()))
    (when (and metadata-sexp
               (not (string-empty-p metadata-sexp)))
      (with-temp-buffer
        (insert metadata-sexp)
        (goto-char (point-min))
        (condition-case nil
            (let* ((sexp (read (current-buffer)))
                   (elements (cdr sexp))) ;; Skip 'skg symbol
              (dolist (element elements)
                (cond
                 (;; (key value) pair
                  (and (listp element)
                       (= (length element) 2))
                  (let ((key (format "%s" (car element)))
                        (val (format "%s" (cadr element))))
                    (push (cons key val) alist)))
                 (;; Special case: (graphStats ...) sub-s-expr
                  (and (listp element)
                       (> (length element) 1)
                       (eq (car element) 'graphStats))
                  (let ((graphstats-sexp (format "%S" element)))
                    (push (cons "graphStats" graphstats-sexp) alist)))
                 (;; Special case: (viewStats ...) sub-s-expr
                  (and (listp element)
                       (> (length element) 1)
                       (eq (car element) 'viewStats))
                  (let ((viewstats-sexp (format "%S" element)))
                    (push (cons "viewStats" viewstats-sexp) alist)))
                 (t ;; Bare value (symbol or other atom)
                  (let ((bare-val (format "%s" element)))
                    (push bare-val set))))))
          (error nil))))
    (list (nreverse alist) (nreverse set))))

(defun skg-reconstruct-metadata-sexp
    (alist bare-values)
  "Reconstruct complete (skg ...) metadata s-expression from ALIST and BARE-VALUES.
Returns a string containing the complete s-expression.
Key-value pairs are formatted as (key value),
except 'graphStats' and 'viewStats' which are already complete s-expressions."
  (let ((parts '()))
    (dolist (kv alist)
      (if (or (string-equal (car kv) "graphStats")
              (string-equal (car kv) "viewStats"))
          ;; graphStats/viewStats value is already a complete sexp string
          (push (cdr kv) parts)
        ;; Regular key-value pair
        (push (format "(%s %s)" (car kv) (cdr kv))
              parts)))
    (dolist (val bare-values)
      (push val parts))
    (if (null parts)
        "(skg)"
      (format "(skg %s)"
              (mapconcat #'identity (nreverse parts) " ")))))

(defun skg-format-headline
    (stars metadata-sexp title)
  "Format a headline with STARS, METADATA-SEXP, and TITLE.
METADATA-SEXP should be the complete (skg ...) s-expression,
OR the empty string.
Handles empty metadata correctly."
  (if (string-empty-p metadata-sexp)
      (format "%s(skg) %s" stars title)
    (format "%s%s %s" stars metadata-sexp title)))

(defun skg--around-org-todo (orig-fn &rest args)
  "Around advice for `org-todo'.
Strip (skg ...) metadata before cycling, re-insert after."
  (let* ((on-heading (org-at-heading-p))
         (parts (when on-heading
                  (save-excursion
                    (beginning-of-line)
                    (skg-split-as-stars-metadata-title
                     (buffer-substring-no-properties
                      (line-beginning-position)
                      (line-end-position))))))
         (has-metadata (and parts
                            (not (string-empty-p (nth 1 parts))))))
    (when has-metadata
      ;; Remove metadata from the line so org sees a plain heading.
      (save-excursion
        (skg-replace-current-line
         (concat (nth 0 parts) (nth 2 parts)))))
    (apply orig-fn args)
    (when has-metadata
      ;; Re-insert metadata after stars (and any new TODO keyword).
      (save-excursion
        (beginning-of-line)
        (let* ((new-line (buffer-substring-no-properties
                          (line-beginning-position)
                          (line-end-position)))
               (new-parts (when (string-match
                                 "^\\(\\*+\\s-+\\)\\(.*\\)" new-line)
                            (list (match-string 1 new-line)
                                  (match-string 2 new-line)))))
          (when new-parts
            (skg-replace-current-line
             (concat (nth 0 new-parts)
                     (nth 1 parts)
                     " "
                     (nth 1 new-parts)))))))))

(advice-add 'org-todo :around #'skg--around-org-todo)

(provide 'skg-metadata)
