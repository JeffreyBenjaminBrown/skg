;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: Extract, search for, and visit (in a new view) IDs.
;;;
;;; USER-FACING FUNCTIONS
;;;   skg-visit-link
;;;   skg-next-id
;;;   skg-previous-id
;;;   skg-push-id-to-stack
;;;   skg-validate-id-stack-buffer
;;;   skg-replace-id-stack-from-buffer
;;;   skg-edit-id-stack

;; todo ? speed : This could be more efficient.
;; It re-reads the same text a few times.

(require 'cl-lib)
(require 'skg-request-single-root-content-view)
(require 'skg-sexpr-search)
(require 'skg-state)

(defconst skg-link-regex
  "\\[\\[id:[^]]+\\]\\[[^]]+\\]\\]"
  "Regex matching org-mode links of the form [[id:X][label]].")

(defun skg-visit-link ()
  "If point is on an org-mode link like [[id:THE_ID][label]],
request a single root content view from that ID.
If point is not on a link, print a message and do nothing."
  (interactive)
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

(defun skg-next-id ()
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

(defun skg-previous-id ()
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

(defun skg-push-id-to-stack ()
  "Push (id label) pair to `skg-id-stack' if point is on metadata or a link.
For metadata, id comes from (id X) and label is the headline title.
For links, id and label come from [[id:X][label]].
Does nothing if point is not within metadata or a link."
  (interactive)
  (let ( ( md-result (skg--point-in-metadata-p) )
         ( result nil ))
    (if md-result
        (setq result md-result)
      (setq result (skg--point-in-link-p)) )
    (if result
        (let ( ( id (car result) )
               ( label (cdr result) ))
          (push (list id label) skg-id-stack)
          (message "pushed (%s, %s) to skg-id-stack"
                   (skg--truncate-id id) label) )
      (message "Nothing pushed to skg-id-stack; point on neither a link nor a metadata sexp.") )))

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

(defun skg-edit-id-stack ()
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

(provide 'skg-id-search)
