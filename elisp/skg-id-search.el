;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: Extract, search for, and visit (in a new view) IDs.
;;;
;;; USER-FACING FUNCTIONS
;;;   skg-visit-link
;;;   skg-next-id
;;;   skg-previous-id
;;;   push-sides-of-line-to-id-stack

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
               ( id ( match-string 1 )) )
          (when ( and ;; Check if point is within this link
                  ( >= pos match-start )
                  ( <= pos match-end ))
            (setq found-link t)
            (setq link-id id )) )) )
    (if found-link
        (progn
          (message "Visiting node: %s" link-id)
          (require 'skg-request-single-root-content-view)
          (skg-request-single-root-content-view-from-id link-id))
      (message "Point not on a link")) ))

(defun skg--metadata-sexp-contains-id-p
    (sexp)
  "Return t if SEXP (a list starting with 'skg) contains an (id ...) pair."
  (and (listp sexp)
       (seq-some (lambda (elem)
                   (and (listp elem)
                        (eq (car elem) 'id) ))
                 sexp) ))

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

(defun skg-next-id ()
  "Move point to the next ID occurrence.
An ID can appear in metadata like (skg (id X) ...) or in a link like [[id:X][label]].
For metadata, point moves to the opening paren of (skg ...).
For links, point moves to the opening bracket of [[id:..."
  (interactive)
  (let ( ( start-pos (point) )
         ( next-link nil )
         ( next-metadata nil ))
    (progn ;; Find next link
      (save-excursion
        (forward-char 1)
        (when (re-search-forward skg-link-regex nil t)
          (setq next-link (match-beginning 0)) )) )
    (progn ;; Find next metadata
      (save-excursion
        (forward-line 1)
        (while (and (not next-metadata)
                    (not (eobp)) )
          (let (( md-pos (skg--find-metadata-start-on-line) ))
            (if md-pos
                (setq next-metadata md-pos)
              (forward-line 1) )) )) )
    (cond
     ( (and next-link next-metadata)
       (goto-char (min next-link next-metadata)) )
     ( next-link
       (goto-char next-link) )
     ( next-metadata
       (goto-char next-metadata) )) ))

(defun skg-previous-id ()
  "Move point to the previous ID occurrence.
An ID can appear in metadata like (skg (id X) ...) or in a link like [[id:X][label]].
For metadata, point moves to the opening paren of (skg ...).
For links, point moves to the opening bracket of [[id:..."
  (interactive)
  (let ( ( start-pos (point) )
         ( prev-link nil )
         ( prev-metadata nil ))
    (progn ;; Find previous link
      (save-excursion
        (backward-char 1)
        (when (re-search-backward skg-link-regex nil t)
          (setq prev-link (match-beginning 0)) )) )
    (progn ;; Find previous metadata
      (save-excursion
        (beginning-of-line)
        (let (( md-pos (skg--find-metadata-start-on-line) ))
          (when (and md-pos (< md-pos start-pos))
            (setq prev-metadata md-pos) ))
        (unless prev-metadata
          (forward-line -1)
          (while (and (not prev-metadata)
                      (not (bobp)) )
            (let (( md-pos (skg--find-metadata-start-on-line) ))
              (if md-pos
                  (setq prev-metadata md-pos)
                (forward-line -1) )) )) ))
    (cond
     ( (and prev-link prev-metadata)
       (goto-char (max prev-link prev-metadata)) )
     ( prev-link
       (goto-char prev-link) )
     ( prev-metadata
       (goto-char prev-metadata) )) ))

(defun push-sides-of-line-to-id-stack ()
  "TEMPORARY CODE.
This is just a scaffold, an easier function to implement,
before the implementation of 'skg-push-id-to-stack'.

PURPOSE: Push a pair of strings to `skg-id-stack'.
The first string is text from line start to point.
The second string is text from point to line end."
  (interactive)
  (let ( ( left (buffer-substring-no-properties
                 (line-beginning-position) (point) ))
         ( right (buffer-substring-no-properties
                  (point) (line-end-position) )))
    (push (list left right) skg-id-stack) ))

(provide 'skg-id-search)
