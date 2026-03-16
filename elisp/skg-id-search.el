;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: Extract, search for, and visit (in a new view) IDs.
;;;
;;; USER-FACING FUNCTIONS
;;;   skg-id-next
;;;   skg-id-prev
;;;   skg-id-push
;;;   skg-validate-id-stack-buffer
;;;   skg-view
;;;   skg-id-stack

;; todo ? speed : This could be more efficient.
;; It re-reads the same text a few times.

(require 'cl-lib)
(require 'skg-metadata)
(require 'skg-request-single-root-content-view)
(require 'skg-sexpr-search)
(require 'skg-state)

(defconst skg-link-regex
  "\\[\\[id:[^]]+\\]\\[[^]]+\\]\\]"
  "Regex matching org-mode links of the form [[id:X][label]].")

(defun skg-visit-link ()
  "If point is on an org-mode link like [[id:THE_ID][label]],
request a single root content view from that ID.
If that ID is already a root of an open view,
switch to that view's buffer instead of opening a new one.
(The server figures that out.)
If point is not on a link, print a message and do nothing."
  (let* ( ( pos ( point ))
          ( line-start ( line-beginning-position ))
          ( line-end ( line-end-position ))
          ( line-text ( buffer-substring-no-properties
                        line-start line-end ))
          ( link-regex "\\[\\[id:\\([^]]+\\)\\]\\[\\([^]]+\\)\\]\\]" )
          ( found-link nil ) ;; mutates
          ( link-id nil ))   ;; mutates
    (save-excursion
      ;; Search for links in the current line
      (goto-char line-start)
      (while ( and ( not found-link )
                   ( re-search-forward link-regex line-end t ))
        (let ( ( match-start ( match-beginning 0 ))
               ( match-end ( match-end 0 ))
               ( id ( match-string-no-properties 1 )) )
          (when ( and ;; Check if point is within this link
                  ( >= pos match-start )
                  ( <= pos match-end ))
            (setq found-link t)
            (setq link-id id )) )) )
    (if found-link
        (progn
          (message "Visiting node: %s" link-id)
          (skg-request-single-root-content-view-from-id link-id))
      (message "Point not on a link")) ))

(defun skg--id-at-point ()
  "Return (id . label) if point is on a UUID, metadata, link, or .skg filename.
Otherwise nil."
  (or (skg--point-on-uuid-v4-p)
      (skg--point-in-metadata-p)
      (skg--point-in-link-p)
      (skg--point-on-skg-filename-p)) )

(defun skg-nearest-id ()
  "Return (id . label) for the nearest ID on the current line, or nil.
First checks whether point is directly on an ID.
If not, tries skg-id-next (only if it stays on this line),
then skg-id-prev (only if it stays on this line).
Point is always restored."
  (let (( result (skg--id-at-point) ))
    (unless result
      (let (( start-pos (point) )
            ( current-line (line-number-at-pos) ))
        (save-excursion ;; Is there an ID after point on this line?
          (skg-id-next)
          (when (and (/= (point) start-pos)
                     (= (line-number-at-pos) current-line))
            (setq result (skg--id-at-point)) ))
        (unless result
          (save-excursion ;; There's definitely an ID before point.
            (skg-id-prev)
            (when (= (line-number-at-pos) current-line)
              (setq result (skg--id-at-point)) )) )) )
    result ))

(defun skg-view ()
  "Open a content view for the nearest ID on the current line."
  (interactive)
  (let (( result (skg-nearest-id) ))
    (if result
        (let (( id (car result) ))
          (message "Visiting node: %s" id)
          (skg-request-single-root-content-view-from-id id))
      (message "No ID found on this line")) ))

(defun skg-view-id (id)
  "Open a content view for ID, prompting when called interactively. (skg-view is usually more convenient.)"
  (interactive "sNode ID: ")
  (message "Visiting node: %s" id)
  (skg-request-single-root-content-view-from-id id))

(defun skg-id-next ()
  "Move point to the next ID occurrence.
An ID can appear in metadata like (skg (node (id X)) ...),
  in which case point moves to the opening paren of (skg ...),
or in a link like [[id:X][label]],
  in which case point moves to the opening bracket of [[id:..."
  (interactive)
  (let ( ( start-pos (point) )
         ( next-link nil )
         ( next-metadata nil ))
    (save-excursion ;; Find next link
      (forward-char 1)
      (when (re-search-forward skg-link-regex nil t)
        (setq next-link (match-beginning 0)) ))
    (save-excursion ;; Find next metadata
      (beginning-of-line)
      (cl-loop while (not (eobp))
               for md-pos = (skg--find-metadata-start-on-line)
               when (and md-pos (> md-pos start-pos))
               return (setq next-metadata md-pos)
               do (forward-line 1) ))
    (let (( candidates (delq nil (list next-link next-metadata)) ))
      (when candidates
        (goto-char (apply #'min candidates)) )) ))

(defun skg-id-prev ()
  "Move point to the previous ID occurrence.
An ID can appear in metadata like (skg (node (id X)) ...),
  in which case point moves to the opening paren of (skg ...),
or in a link like [[id:X][label]],
  in which case point moves to the opening bracket of [[id:..."
  (interactive)
  (let ( ( start-pos (point) )
         ( prev-link nil )
         ( prev-metadata nil ))
    (save-excursion ;; Find previous link
      (backward-char 1)
      (when (re-search-backward skg-link-regex nil t)
        (setq prev-link (match-beginning 0)) ))
    (save-excursion ;; Find previous metadata
      (beginning-of-line)
      (cl-loop for md-pos = (skg--find-metadata-start-on-line)
               when (and md-pos (< md-pos start-pos))
               return (setq prev-metadata md-pos)
               while (zerop (forward-line -1)) ))
    (let (( candidates (delq nil (list prev-link prev-metadata)) ))
      (when candidates
        (goto-char (apply #'max candidates)) )) ))

(defun skg--find-metadata-start-on-line ()
  "If current line has metadata with id, return position of its opening paren.
Otherwise return nil."
  ;; todo ? speed : Is verifying that it has an ID over-cautious?
  (save-excursion
    (beginning-of-line)
    (when (looking-at "^\\*+ (skg")
      (let* ( ( skg-start (- (match-end 0) 4) )
              ( after-bullet (buffer-substring-no-properties
                              skg-start (line-end-position) )))
        (condition-case nil
            (let (( sexp (read after-bullet) ))
              (when (skg--metadata-sexp-contains-id-p sexp)
                skg-start ))
          (error nil) )) )))

(defun skg-id-push ()
  "Push the metadata ID from the current line to `skg-id-stack'."
  (interactive)
  (let* (( headline (skg-get-current-headline-text) )
         ( split (skg-split-as-stars-metadata-title headline) )
         ( metadata-sexp (when split (cadr split)) )
         ( title (when split (caddr split)) ))
    (if (and metadata-sexp
             (not (string-empty-p metadata-sexp)) )
        (let (( sexp (read metadata-sexp) ))
          (if (skg--metadata-sexp-contains-id-p sexp)
              (let (( id (skg--extract-id-from-metadata-sexp sexp) ))
                (push (list id title) skg-id-stack)
                (message "pushed to stack: %s" title) )
            (message "No ID in metadata on this line") ))
      (message "No metadata on this line") )))

(defun skg-id-pop ()
  "Pop the top of `skg-id-stack' and insert an org link at point.
Prompts for the link label, defaulting to the title from the stack."
  (interactive)
  (if (null skg-id-stack)
      (message "ID stack is empty")
    (let* (( entry (pop skg-id-stack) )
           ( id (car entry) )
           ( title (cadr entry) )
           ( label (read-string "Link label: " title) ))
      (insert (format "[[id:%s][%s]]" id label)) )))

(defun skg--point-in-metadata-p ()
  "If point is within metadata, return (id . title). Otherwise nil."
  (save-excursion
    (let (( pos (point) ))
      (beginning-of-line)
      (when (looking-at "^\\*+ (skg")
        (let* ( ( skg-start (- (match-end 0) 4))
                ( after-bullet (buffer-substring-no-properties
                                skg-start (line-end-position)) ))
          (condition-case nil
              (let* ( ( sexp (read after-bullet) )
                      ( sexp-len (length (format "%S" sexp)) )
                      ( skg-end (+ skg-start sexp-len) ))
                (when (and (>= pos skg-start)
                           (< pos skg-end)
                           (skg--metadata-sexp-contains-id-p sexp) )
                  (let ( (id (skg--extract-id-from-metadata-sexp sexp))
                         (title (string-trim
                                 (buffer-substring-no-properties
                                  skg-end (line-end-position)) )) )
                    (cons id title)) ))
            (error nil) )) )) ))

(defun skg--metadata-sexp-contains-id-p
    (sexp)
  "Return t if SEXP contains an id in the structure (skg (node (id ...)))."
  (skg-sexp-subtree-p sexp '(skg (node (id)))))

(defun skg--extract-id-from-metadata-sexp
    (sexp)
  "Extract the id value from SEXP, a list like (skg (node (id X) ...)).
Returns the id as a string, or nil if not found."
  (let ((val (car (skg-sexp-cdr-at-path sexp '(skg node id)))))
    (when val (format "%s" val))))

(defun skg--extract-source-from-metadata-sexp (sexp)
  "Extract the source value from SEXP, a list like (skg (node (source X) ...)).
Returns the source as a string, or nil if not found."
  (let ((val (car (skg-sexp-cdr-at-path sexp '(skg node source)))))
    (when val (format "%s" val))))

(defun skg--point-in-link-p ()
  "If point is within a link, return (id . label). Otherwise nil."
  (let ( ( pos (point) )
         ( line-start (line-beginning-position) )
         ( line-end (line-end-position) )
         ( link-regex "\\[\\[id:\\([^]]+\\)\\]\\[\\([^]]+\\)\\]\\]" )
         ( result nil ))
    (save-excursion
      (goto-char line-start)
      (while (and (not result)
                  (re-search-forward link-regex line-end t) )
        (when (and (>= pos (match-beginning 0))
                   (< pos (match-end 0)) )
          (setq result (cons (match-string 1)
                             (match-string 2) )) )) )
    result ))

(defconst skg--uuid-v4-regex
  "[0-9a-f]\\{8\\}-[0-9a-f]\\{4\\}-4[0-9a-f]\\{3\\}-[89ab][0-9a-f]\\{3\\}-[0-9a-f]\\{12\\}"
  "Regex matching a UUID v4 (lowercase hex).")

(defun skg--point-on-uuid-v4-p ()
  "If point is on a UUID v4 string, return (uuid . uuid). Otherwise nil."
  (let (( case-fold-search t )
        ( line-start (line-beginning-position) )
        ( line-end (line-end-position) )
        ( pos (point) )
        ( result nil ))
    (save-excursion
      (goto-char line-start)
      (while (and (not result)
                  (re-search-forward skg--uuid-v4-regex line-end t) )
        (when (and (>= pos (match-beginning 0))
                   (<= pos (match-end 0)) )
          (setq result (cons (match-string-no-properties 0)
                             (match-string-no-properties 0) )) )) )
    result ))

(defun skg--point-on-skg-filename-p ()
  "If point is on a .skg filename, return (basename . filename).
The basename (filename without .skg extension) is the ID.
A .skg filename is a word-boundary-delimited string ending in .skg,
e.g. \"557a869b-02ba-4c59-a5d3-5fb469a12353.skg\" or \"a.skg\"."
  (let (( line-start (line-beginning-position) )
        ( line-end (line-end-position) )
        ( pos (point) )
        ;; Match valid ID characters (alphanum, dash, underscore) ending in .skg
        ( skg-file-regex "\\([a-zA-Z0-9_-]+\\)\\.skg\\b" )
        ( result nil ))
    (save-excursion
      (goto-char line-start)
      (while (and (not result)
                  (re-search-forward skg-file-regex line-end t) )
        (when (and (>= pos (match-beginning 0))
                   (<= pos (match-end 0)) )
          (setq result (cons (match-string-no-properties 1)
                             (match-string-no-properties 0) )) )) )
    result ))

(defun skg--truncate-id
    (id)
  "Return first 6 characters of ID followed by ellipsis."
  (if (> (length id) 6)
      (concat (substring id 0 6) "...")
    id ))

(defun skg-replace-id-stack-from-buffer ()
  "Replace `skg-id-stack' with contents parsed from current buffer.
Uses `skg-validate-id-stack-buffer' to parse and validate.
On success, sets `skg-id-stack' to the parsed result.
On failure, prints error message and leaves `skg-id-stack' unchanged."
  (let (( validation-result (skg-validate-id-stack-buffer) ))
    (if (eq (car validation-result) 'success)
        (setq skg-id-stack (cadr validation-result))
      (message "Invalid ID buffer: %s" (cadr validation-result)) )))

(defun skg--save-id-stack-buffer ()
  "Save handler for the id-stack buffer.
Validates and replaces `skg-id-stack' with buffer contents.
Rather than make this function 'interactive'
(which would interfere with tab-completing
'skg-save' to become 'skg-request-save-buffer'),
it is bound to C-x C-s (which normally calls 'save-buffer')
by 'skg-id-stack-mode'."
  (let (( validation-result (skg-validate-id-stack-buffer) ))
    (if (eq (car validation-result) 'success)
        (progn
          (setq skg-id-stack (cadr validation-result))
          (set-buffer-modified-p nil)
          (message "ID stack updated (%d items)"
                   (length skg-id-stack)) )
      (message "Invalid ID buffer: %s" (cadr validation-result)) )))

(defun skg-validate-id-stack-buffer ()
  "Validate current buffer as an id-stack buffer and return the stack.
Each headline must have a non-empty title and exactly one body line (the ID).
Returns (success RESULT) where RESULT is a list of (id label) pairs,
with the first headline last in the list.
Returns (error MESSAGE) if validation fails."
  (save-excursion
    (goto-char (point-min))
    (let ( ( result nil )
           ( error-msg nil )
           ( headline-regex "^\\*+ +\\([^ \n].*\\)$" ))
      (progn ;; Check no content before first headline
        (skip-chars-forward " \t\n")
        (when (and (not (eobp))
                   (not (looking-at "^\\*")) )
          (setq error-msg "content before first headline") ))
      (while (and (not error-msg) (not (eobp)))
        (if (looking-at headline-regex)
            ;; Current line is a headline with non-empty title.
            (let ( ( label (match-string 1) )
                   ( body-start (1+ (line-end-position)) )
                   ( body-end nil ))
              (progn ;; Find where the body ends:
                ;; either at the next headline or at end of buffer.
                (forward-line 1)
                (setq body-end
                      (if (re-search-forward "^\\*" nil t)
                          (progn (beginning-of-line) (point))
                        (point-max) )))
              (let* ( ( body-text (buffer-substring-no-properties
                                   body-start body-end) )
                      ( body-lines (seq-filter
                                    (lambda (line)
                                      (not (string-empty-p
                                            (string-trim line) )))
                                    (split-string body-text "\n") )))
                (if (= (length body-lines) 1)
                    (push (list
                           (string-trim (car body-lines)) ;; the id
                           label)
                          result)
                  (setq error-msg
                        (format "headline '%s' must have exactly one body line"
                                label) )) ))
          (;; Not a headline with a valid title.
           ;; Could be a blank line, or a headline with empty title.
           if (looking-at "^\\*")
              ;; Looks like headline but has no title - that's an error.
              (setq error-msg "headline with empty title")
            ( ;; Not a headline at all (blank line), so skip it.
             forward-line 1) ) ))
      (if error-msg
          (list 'error error-msg)
        ;; nreverse so first headline in buffer = head of stack
        (list 'success (nreverse result)) )) ))

(defvar skg-id-stack-mode-map
  (let (( map (make-sparse-keymap) ))
    (define-key map (kbd "C-x C-s") #'skg--save-id-stack-buffer)
    map )
  "Keymap for `skg-id-stack-mode'.")

(defun skg-id-stack ()
  "Open a buffer to edit `skg-id-stack'.
The buffer displays the stack in org format
(label headlines and ID bodies).
Saving (C-x C-s) validates and updates `skg-id-stack'
without writing to disk."
  (interactive)
  (let (( buf (get-buffer-create "*skg-id-stack*") ))
    (switch-to-buffer buf)
    (erase-buffer)
    (insert (skg--format-id-stack-as-org))
    (goto-char (point-min))
    (org-mode)
    (skg-id-stack-mode 1)
    (set-buffer-modified-p nil)
    (message "Edit ID stack. C-x C-s to save changes.") ))

(defun skg--format-id-stack-as-org ()
  "Format `skg-id-stack' as org-mode text.
Each (id label) pair becomes a headline with label as title and id as body.
Head of stack (most recently pushed) appears at top of buffer."
  (mapconcat
   (lambda (entry)
     (let (( id (car entry) )
           ( label (cadr entry) ))
       (format "* %s\n%s" label id) ))
   skg-id-stack
   "\n" ))

(define-minor-mode skg-id-stack-mode
  "Minor mode for editing the skg ID stack.
Overrides save to update `skg-id-stack' instead of writing to a file."
  :lighter " ID-Stack"
  :keymap skg-id-stack-mode-map)

;; Hide from M-x: this mode is only activated
;; programmatically by 'skg-id-stack'.
(put 'skg-id-stack-mode 'completion-predicate #'ignore)

(provide 'skg-id-search)
