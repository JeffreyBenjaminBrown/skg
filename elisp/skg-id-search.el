;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: Extract, search for, and visit (in a new view) IDs.

;; todo ? speed : This could be more efficient.
;; It re-reads the same text a few times.

(require 'cl-lib)
(require 'subr-x)
(require 'skg-buffer)
(require 'skg-metadata)
(require 'skg-request-single-root-content-view)
(require 'skg-sexpr-search)
(require 'skg-state)

(defconst skg-link-regex
  "\\[\\[id:[^]]+\\]\\[[^]]+\\]\\]"
  "Regex matching org-mode links of the form [[id:X][label]].")

(defconst skg--link-with-label-group-regex
  "\\[\\[id:[^]]+\\]\\[\\([^]]+\\)\\]\\]"
  "Like `skg-link-regex' but with the label captured as group 1.
Used by `skg-replace-links-with-labels' to replace each link with its label.")

(defun skg-replace-links-with-labels (text)
  "Return TEXT with each [[id:X][LABEL]] replaced by LABEL.
Mirrors the Rust-side `replace_each_link_with_its_label'."
  (replace-regexp-in-string
   skg--link-with-label-group-regex "\\1" text))

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

(defun skg--bypass-override-here-p ()
  "Whether a goto from the current buffer should bypass the
override-choice menu. True in magit buffers: a readable-ID jump
from magit should land on the original raw node
\(TODO/full-schema/11_override-rendering-and-navigation.org).
Detected by major-mode name so magit need not be loaded."
  (string-prefix-p "magit-" (symbol-name major-mode)))

(defun skg-goto ()
  "Open a content view for the nearest ID on the current line.
From a magit buffer this bypasses the override-choice menu (the
jump lands on the original raw node); elsewhere, visiting an
overridden node offers the menu."
  (interactive)
  (let (( result (skg-nearest-id) ))
    (if result
        (let (( id (car result) ))
          (message "Visiting node: %s" id)
          (skg-request-single-root-content-view-from-id
           id nil (skg--bypass-override-here-p)))
      (message "No ID found on this line")) ))

(defun skg-goto-bypassOverride ()
  "Like `skg-goto', but always bypass the override-choice menu:
open the nearest ID itself, even if it is overridden. The escape
hatch from the menu (and from anywhere) to the original node."
  (interactive)
  (let (( result (skg-nearest-id) ))
    (if result
        (let (( id (car result) ))
          (message "Visiting node (bypassing overriders): %s" id)
          (skg-request-single-root-content-view-from-id id nil t))
      (message "No ID found on this line")) ))

(defun skg-goto-by-id (id)
  "Open a content view for ID, prompting when called interactively. (skg-goto is usually more convenient.)"
  (interactive "sNode ID: ")
  (message "Visiting node: %s" id)
  (skg-request-single-root-content-view-from-id id))

(defun skg-goto-and-close-this ()
  "Like `skg-goto', but also kill the buffer it was called from
once the goto request has been issued."
  (interactive)
  (let ((buf (current-buffer)))
    (skg-goto)
    (when (buffer-live-p buf)
      (kill-buffer buf))))

(defun skg-goto-by-id-and-close-this (id)
  "Like `skg-goto-by-id', but also kill the buffer it was called
from once the goto request has been issued."
  (interactive "sNode ID: ")
  (let ((buf (current-buffer)))
    (skg-goto-by-id id)
    (when (buffer-live-p buf)
      (kill-buffer buf))))

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
  "Push an ID and its title/label to `skg-id-stack'.
If point is on an inline [[id:X][label]] link, push that link's
ID and label. Otherwise push the headline's metadata ID and title."
  (interactive)
  (let (( link-hit (skg--point-in-link-p) ))
    (if link-hit
        (let (( id    (car link-hit) )
              ( label (cdr link-hit) ))
          (push (list id label) skg-id-stack)
          (message "pushed to stack: %s" label) )
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
          (message "No metadata on this line") )) )))

(defun skg--id-stack-top-or-message ()
  "If possible, return the top entry (id title) of `skg-id-stack'.
Othewrise (because the stack is empty) print a message and return nil."
  (if (null skg-id-stack)
      (progn (message "ID stack is empty") nil)
    (car skg-id-stack) ))

(defun skg--insert-link-from-entry (entry)
  "Insert an org link at point, using ENTRY, a (id title) pair.
Prompts for the link label, defaulting to the title."
  (let* (( id (car entry) )
         ( title (cadr entry) )
         ( label (if (minibufferp)
                     title
                   (read-string "Link label: " title))) )
    (insert (format "[[id:%s][%s]]" id label)) ))

(defun skg--org-stars-for-node-insertion ()
  "Return headline stars for inserting a node at point."
  (if (derived-mode-p 'org-mode)
      (save-excursion
        (condition-case nil
            (progn
              (org-back-to-heading t)
              (make-string (org-outline-level) ?*))
          (error "*")))
    "*"))

(defun skg--insert-node-from-entry (entry)
  "Insert an indefinitive ActiveNode headline from ENTRY, an (id title) pair.
If point is already after headline stars at the start of a line,
insert only the metadata and title.  Otherwise insert a full same-level
headline."
  (let* (( id (car entry) )
         ( title (cadr entry) )
         ( node-text (format "(skg (node (id %s) indef)) %s" id title)) )
    (if (save-excursion
          (let ((pos (point)))
            (beginning-of-line)
            (and (looking-at "\\*+[ \t]*")
                 (<= (match-end 0) pos)
                 (string-blank-p
                  (buffer-substring-no-properties (match-end 0) pos)))))
        (insert node-text)
      (insert (format "%s %s\n"
                      (skg--org-stars-for-node-insertion)
                      node-text)))))

(defun skg-paste-id ()
  "Insert the ID at the top of `skg-id-stack' at point.
Does not modify the stack."
  (interactive)
  (let (( entry (skg--id-stack-top-or-message) ))
    (when entry
      (insert (car entry)) )))

(defun skg-paste-link ()
  "Insert an org link at point, using the top of `skg-id-stack'.
Does not modify the stack.
Prompts for the link label, defaulting to the title from the stack."
  (interactive)
  (let (( entry (skg--id-stack-top-or-message) ))
    (when entry
      (skg--insert-link-from-entry entry) )))

(defun skg-paste-node ()
  "Insert an indefinitive ActiveNode headline from the top of `skg-id-stack'.
Does not modify the stack.  The inserted metadata contains the node ID
and `indef`; the headline title comes from the stack entry."
  (interactive)
  (let (( entry (skg--id-stack-top-or-message) ))
    (when entry
      (skg--insert-node-from-entry entry) )))

(defun skg-pop-id ()
  "Pop the top of `skg-id-stack' and insert the ID at point."
  (interactive)
  (let (( entry (skg--id-stack-top-or-message) ))
    (when entry
      (pop skg-id-stack)
      (insert (car entry)) )))

(defun skg-pop-link ()
  "Pop the top of `skg-id-stack' and insert an org link at point.
Prompts for the link label, defaulting to the title from the stack."
  (interactive)
  (let (( entry (skg--id-stack-top-or-message) ))
    (when entry
      (pop skg-id-stack)
      (skg--insert-link-from-entry entry) )))

(defun skg-pop-node ()
  "Pop the top of `skg-id-stack' and insert an indefinitive ActiveNode headline.
The inserted metadata contains the node ID and `indef`; the headline
title comes from the stack entry."
  (interactive)
  (let (( entry (skg--id-stack-top-or-message) ))
    (when entry
      (pop skg-id-stack)
      (skg--insert-node-from-entry entry) )))

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
  "Return t if SEXP contains an id under the ActiveNode shape
(skg (node (id ...))) or the Deleted-phantom shape
(skg (deleted (id ...)))."
  (or (skg-sexp-subtree-p sexp '(skg (node    (id))))
      (skg-sexp-subtree-p sexp '(skg (deleted (id))))))

(defun skg--extract-id-from-metadata-sexp
    (sexp)
  "Extract the id value from SEXP. Accepts either the ActiveNode shape
(skg (node (id X) ...)) or the Deleted-phantom shape
(skg (deleted (id X) ...)). Returns the id as a string, or nil."
  (let ((val (or (car (skg-sexp-cdr-at-path sexp '(skg node    id)))
                 (car (skg-sexp-cdr-at-path sexp '(skg deleted id))))))
    (when val (format "%s" val))))

(defun skg--extract-source-from-metadata-sexp (sexp)
  "Extract the source value from SEXP. Accepts either the ActiveNode shape
(skg (node (source X) ...)) or the Deleted-phantom shape
(skg (deleted (source X) ...)). Returns the source as a string, or nil."
  (let ((val (or (car (skg-sexp-cdr-at-path sexp '(skg node    source)))
                 (car (skg-sexp-cdr-at-path sexp '(skg deleted source))))))
    (when val (format "%s" val))))

(defun skg--metadata-is-removed-here-phantom-p (sexp)
  "Return t if SEXP describes a 'removed-here' phantom: a node whose
membership was removed in some stage but whose file existence is
unchanged. SEXP is shaped like (skg (node ... (staged ATOMS) (unstaged ATOMS))).
A removed-here phantom has at least one removedM atom under a stage
form, and no removedX/newX atoms."
  (let ((staged-atoms   (skg-sexp-cdr-at-path sexp '(skg node staged)))
        (unstaged-atoms (skg-sexp-cdr-at-path sexp '(skg node unstaged))))
    (let ((all-atoms (append staged-atoms unstaged-atoms)))
      (and (memq 'removedM all-atoms)
           (not (memq 'newX     all-atoms))
           (not (memq 'removedX all-atoms))))))

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
Bound to C-x C-s (which normally calls `save-buffer')
by `skg-id-stack-mode'."
  (interactive)
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

;; skg-id-stack-mode-map is defined in skg-keymaps-and-aliases.el.

(defun skg-view-id-stack ()
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
    (skg--org-mode-with-options)
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
;; programmatically by 'skg-view-id-stack'.
(put 'skg-id-stack-mode 'completion-predicate #'ignore)

(provide 'skg-id-search)
