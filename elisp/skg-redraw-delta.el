;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: Redraw a view by its delta: rewrite only the headline
;;; lines a response names in '(changed-headlines (V ...))', instead of
;;; erasing and reinserting the whole buffer ('docs/api-and-formats.org',
;;; "The redraw delta"). The new lines are the server's lines in their
;;; stripped form, without the fold and focus atoms, which the buffer
;;; keeps as org folds and point. After the rewrite the buffer must
;;; equal the stripped form of the new text; when it does not, the
;;; caller redraws the whole buffer.
;;;
;;; The stripped form is one rule the server and both clients
;;; implement. All three test against
;;; 'tests/shared/fold-and-focus-atom-strip-cases.txt', whose header
;;; states the rule.
;;;
;;; PITFALL: With 'org-fold-core-style' 'text-properties', a rewritten
;;; headline whose body is folded takes the fold's property and hides
;;; itself, so a patch needs 'overlays', under which every fold around
;;; and below a rewritten line survives.

(require 'cl-lib)
(require 'org-fold-core)
(require 'skg-buffer) ; for skg--inhibit-dirty-view-confirmation
(require 'skg-log)
(require 'skg-metadata) ; for the headline levels, ancestors and folders
(require 'heralds-minor-mode)

(defconst skg--fold-and-focus-atoms '("focused" "folded" "bodyFolded")
  "The bare atoms the stripped form of a headline line removes.")

(defconst skg--viewnode-id-at-headline-start-regexp
  "^\\*+ (skg (vnid \\([^)\n]*\\))"
  "A headline line beginning with its viewnode ID, as the server renders
it ('viewnode_to_string_where_it_stands'); group 1 is the ID.")

;;; Patching

(defun skg--patch-buffer-with-delta (new-content viewnode-ids)
  "Rewrite in the current buffer each headline line VIEWNODE-IDS names,
by that viewnode ID's line in the stripped form of NEW-CONTENT, then
re-herald those lines and what depends on them
\(`skg--reherald-after-rewrites'). Return non-nil when the buffer then
equals the stripped form of NEW-CONTENT; the caller redraws the whole
buffer otherwise. Return nil without editing when
the fold style is not 'overlays' or some viewnode ID is missing from
the buffer or from NEW-CONTENT; return nil after editing when the
buffer differs from the stripped form, or on an error, which it logs.
Leaves folds and point as they are."
  (condition-case err
      (skg--patch-buffer-with-delta-or-signal new-content viewnode-ids)
    (error (skg-log 'error 'save "redraw patch error: %S" err)
           nil)))

(defun skg--patch-buffer-with-delta-or-signal (new-content viewnode-ids)
  "`skg--patch-buffer-with-delta', without its handling of errors."
  (when (eq org-fold-core-style 'overlays)
    (let* ((wanted-viewnode-ids
            (mapcar (lambda (viewnode-id) (format "%s" viewnode-id))
                    viewnode-ids))
           (stripped-content (skg--stripped-text new-content))
           (new-lines (skg--headline-lines-by-viewnode-id
                       stripped-content wanted-viewnode-ids))
           (headline-starts (skg--headline-starts-by-viewnode-id
                             wanted-viewnode-ids)))
      (when (cl-every (lambda (viewnode-id)
                        (and (gethash viewnode-id new-lines)
                             (gethash viewnode-id headline-starts)))
                      wanted-viewnode-ids)
        (let* ((inhibit-read-only t)
               (skg--inhibit-dirty-view-confirmation t)
               (rewritten-line-starts
                (skg--rewrite-headline-lines
                 (mapcar (lambda (viewnode-id)
                           (cons (gethash viewnode-id headline-starts)
                                 (gethash viewnode-id new-lines)))
                         wanted-viewnode-ids))))
          (when heralds-minor-mode
            (skg--reherald-after-rewrites rewritten-line-starts)
            (skg-fork-dedup-refresh-lines rewritten-line-starts))
          (mapc (lambda (marker) (set-marker marker nil))
                rewritten-line-starts))
        (string= (buffer-substring-no-properties (point-min) (point-max))
                 stripped-content)))))

(defun skg--headline-lines-by-viewnode-id (text viewnode-ids)
  "A table from each of VIEWNODE-IDS to the headline line of TEXT that
begins with it, read in one pass over TEXT."
  (let ((wanted (skg--string-set viewnode-ids))
        (lines (make-hash-table :test 'equal))
        (case-fold-search nil)
        (start 0))
    (while (string-match skg--viewnode-id-at-headline-start-regexp
                         text start)
      (let ((viewnode-id (match-string 1 text))
            (line-end (or (string-search "\n" text (match-end 0))
                          (length text))))
        (when (gethash viewnode-id wanted)
          (puthash viewnode-id
                   (substring text (match-beginning 0) line-end)
                   lines))
        (setq start line-end)))
    lines))

(defun skg--headline-starts-by-viewnode-id (viewnode-ids)
  "A table from each of VIEWNODE-IDS to where the headline line of the
current buffer that begins with it starts, read in one pass."
  (let ((wanted (skg--string-set viewnode-ids))
        (starts (make-hash-table :test 'equal)))
    (dolist (headline (skg--viewnode-id-headlines) starts)
      (when (gethash (car headline) wanted)
        (puthash (car headline) (cadr headline) starts)))))

(defun skg--viewnode-id-headlines ()
  "Each headline line of the current buffer that begins with a viewnode
ID, as (ID START LINE), START where the line starts and LINE its line
number, in buffer order."
  (let ((case-fold-search nil)
        (headlines nil)
        (line 1)
        (previous-start (point-min)))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward skg--viewnode-id-at-headline-start-regexp
                                nil t)
        (let ((start (match-beginning 0)))
          (setq line (+ line (count-lines previous-start start))
                previous-start start)
          (push (list (match-string-no-properties 1) start line)
                headlines))))
    (nreverse headlines)))

(defun skg--string-set (strings)
  "A hash table holding each of STRINGS."
  (let ((set (make-hash-table :test 'equal)))
    (dolist (string strings set)
      (puthash string t set))))

(defun skg--rewrite-headline-lines (starts-and-new-lines)
  "Rewrite each line starting at the car of an element of
STARTS-AND-NEW-LINES as its cdr, from the end of the buffer backwards,
so that the positions not yet reached stay valid. Return a marker at
each rewritten line's start. The herald hook does not run: its
per-edit refresh would re-herald a folder once per rewritten member,
reading all its members each time; `skg--reherald-after-rewrites'
re-heralds each line once instead. The other change hooks run."
  (let ((descending
         (sort (copy-sequence starts-and-new-lines)
               (lambda (a b) (> (car a) (car b)))))
        (after-change-functions
         (remq #'heralds-after-change after-change-functions)))
    (save-excursion
      (mapcar (lambda (start-and-new-line)
                (goto-char (car start-and-new-line))
                (skg--rewrite-line-at-point (cdr start-and-new-line))
                (copy-marker (line-beginning-position)))
              descending))))

(defun skg--rewrite-line-at-point (new-line)
  "Make the line at point read NEW-LINE, replacing only the span
between the prefix and the suffix the two share, so that point and
markers in the shared parts keep their place in the text. Edits inside
`org-fold-core-ignore-fragility-checks', as `skg-replace-current-line'
does."
  (let* ((line-start (line-beginning-position))
         (old-line (buffer-substring-no-properties
                    line-start (line-end-position))))
    (unless (string= old-line new-line)
      (let* ((prefix-length (skg--common-prefix-length old-line new-line))
             (suffix-length
              (min (skg--common-prefix-length (reverse old-line)
                                              (reverse new-line))
                   (- (length old-line) prefix-length)
                   (- (length new-line) prefix-length))))
        (org-fold-core-ignore-fragility-checks
          (goto-char (+ line-start prefix-length))
          (delete-region (point)
                         (+ line-start (- (length old-line) suffix-length)))
          (insert (substring new-line
                             prefix-length
                             (- (length new-line) suffix-length))))))))

(defun skg--common-prefix-length (string-a string-b)
  "How many characters STRING-A and STRING-B share at their start."
  (let ((comparison (compare-strings string-a nil nil string-b nil nil)))
    (if (eq comparison t)
        (length string-a)
      (1- (abs comparison)))))

(defun skg--reherald-after-rewrites (line-starts)
  "Re-apply, once each, the herald of each headline starting at one of
LINE-STARTS and of each headline whose herald reads one of them: the
folders among its two nearest ancestors, which read their members
\(`heralds--refresh-folders-above' does this for an edit), and the
headlines one or two levels below it, since a pending detachment reads
its parent and grandparent (`skg--pending-detachment-at-point-p')."
  (let ((reherald-starts (make-hash-table)))
    (save-excursion
      (dolist (line-start line-starts)
        (goto-char line-start)
        (puthash (line-beginning-position) t reherald-starts)
        (dolist (ancestor-start (skg--folder-ancestor-starts-on-line))
          (puthash ancestor-start t reherald-starts))
        (dolist (descendant-start
                 (skg--children-and-grandchildren-starts-on-line))
          (puthash descendant-start t reherald-starts)))
      (maphash (lambda (reherald-start _)
                 (goto-char reherald-start)
                 (heralds-clear-overlays-in-region
                  (line-beginning-position) (line-end-position))
                 (heralds-apply-to-line))
               reherald-starts))))

(defun skg--folder-ancestor-starts-on-line ()
  "Where the folders among the two nearest ancestor headlines of the
headline on the line at point begin."
  (save-excursion
    (cl-remove-if-not
     (lambda (ancestor-start)
       (and ancestor-start
            (progn (goto-char ancestor-start)
                   (skg--folder-atom (skg--metadata-sexp-at-point-or-nil)))))
     (skg--ancestor-headline-starts-on-line 2))))

(defun skg--children-and-grandchildren-starts-on-line ()
  "Where the headlines one or two levels below the headline on the line
at point begin."
  (save-excursion
    (let ((level (skg--headline-level-on-line))
          (case-fold-search nil)
          (starts nil)
          (done nil))
      (when level
        (end-of-line)
        (while (and (not done)
                    (re-search-forward
                     (format "^\\*\\{1,%d\\}[ \t]" (+ level 2)) nil t))
          (if (<= (skg--headline-level-on-line) level)
              (setq done t)
            (push (line-beginning-position) starts)
            (end-of-line))))
      starts)))

;;; The stripped form

(defun skg--stripped-text (text)
  "The stripped form of TEXT: each headline line stripped, the other
lines kept."
  (car (skg--stripped-text-and-atom-headlines text)))

(defun skg--stripped-text-and-atom-headlines (text)
  "The stripped form of TEXT, and what its stripping removed, as
\(STRIPPED . ATOM-HEADLINES). ATOM-HEADLINES lists, in text order, each
headline line that lost atoms, as (OFFSET . ATOMS): OFFSET is where the
line starts in STRIPPED, counting from 0, and ATOMS are the atoms it
lost, as strings, in the order they stood."
  (let ((offset 0)
        (stripped-lines nil)
        (atom-headlines nil))
    (dolist (line (split-string text "\n"))
      (let ((stripped-and-atoms (skg--stripped-line-and-atoms line)))
        (when (cdr stripped-and-atoms)
          (push (cons offset (cdr stripped-and-atoms)) atom-headlines))
        (push (car stripped-and-atoms) stripped-lines)
        (setq offset (+ offset (length (car stripped-and-atoms)) 1))))
    (cons (mapconcat #'identity (nreverse stripped-lines) "\n")
          (nreverse atom-headlines))))

(defun skg--stripped-line (line)
  "The stripped form of LINE: a headline line without the fold and focus
atoms at depth 1 of its '(skg ...)' form, each removed with the one
space before it. Any other line is its own stripped form."
  (car (skg--stripped-line-and-atoms line)))

(defun skg--stripped-line-and-atoms (line)
  "The stripped form of LINE (`skg--stripped-line'), and the atoms its
stripping removed, as (STRIPPED . ATOMS), ATOMS strings in the order
they stood."
  (let* ((case-fold-search nil)
         (form-start
          (and (string-match-p "focused\\|olded" line) ; most lines hold none
               (string-match "\\`\\*+ \\((skg\\)[ )]" line)
               (match-beginning 1))))
    (if form-start
        (skg--fold-and-focus-atoms-split-from-line line form-start)
      (list line))))

(defun skg--without-fold-and-focus-atoms (line form-start &optional atoms)
  "LINE without each fold and focus atom, and the space before it, at
depth 1 of the form starting at FORM-START. Depth counts the parentheses
outside string literals. The text after the form is kept as it is.
ATOMS, when given, names the atoms to remove instead
\(default `skg--fold-and-focus-atoms')."
  (car (skg--fold-and-focus-atoms-split-from-line line form-start atoms)))

(defun skg--fold-and-focus-atoms-split-from-line (line form-start
                                                       &optional atoms)
  "`skg--without-fold-and-focus-atoms' of LINE, FORM-START and ATOMS,
and the atoms it removed, as (LINE-WITHOUT-THEM . REMOVED-ATOMS),
REMOVED-ATOMS strings in the order they stood."
  (let ((atoms (or atoms skg--fold-and-focus-atoms))
        (removed-atoms nil)
        (line-length (length line))
        (kept-pieces nil)
        (copied-up-to 0)
        (depth 0)
        (i form-start)
        (form-closed nil))
    (while (and (not form-closed) (< i line-length))
      (let ((char (aref line i)))
        (cond
         ((eq char ?\")
          (setq i (skg--end-of-string-literal line i)))
         ((eq char ?\()
          (setq depth (1+ depth)
                i (1+ i)))
         ((eq char ?\))
          (setq depth (1- depth))
          (if (zerop depth)
              (setq form-closed t)
            (setq i (1+ i))))
         ((and (eq char ?\s) (= depth 1))
          (let ((atom-end (skg--end-of-bare-atom line (1+ i))))
            (if (member (substring line (1+ i) atom-end)
                        atoms)
                (progn (push (substring line copied-up-to i) kept-pieces)
                       (push (substring line (1+ i) atom-end) removed-atoms)
                       (setq copied-up-to atom-end
                             i atom-end))
              (setq i (1+ i)))))
         (t (setq i (1+ i))))))
    (push (substring line copied-up-to) kept-pieces)
    (cons (apply #'concat (nreverse kept-pieces))
          (nreverse removed-atoms))))

(defun skg--end-of-string-literal (line quote)
  "The index just past the string literal of LINE whose opening quote is
at QUOTE, or the end of LINE if it is not closed. A backslash escapes the
character after it."
  (let ((line-length (length line))
        (i (1+ quote))
        (end nil))
    (while (and (not end) (< i line-length))
      (pcase (aref line i)
        (?\\ (setq i (+ i 2)))
        (?\" (setq end (1+ i)))
        (_ (setq i (1+ i)))))
    (or end line-length)))

(defun skg--end-of-bare-atom (line start)
  "The index just past the bare atom of LINE starting at START: the run
of characters that are not whitespace, parentheses or double quotes.
Whitespace is the server's, ASCII's: space, tab, newline, form feed and
carriage return."
  (let ((line-length (length line))
        (i start))
    (while (and (< i line-length)
                (not (memq (aref line i) '(?\s ?\t ?\n ?\f ?\r ?\( ?\) ?\"))))
      (setq i (1+ i)))
    i))

(provide 'skg-redraw-delta)
