;;; skg-id-search.el --- Find and visit the ID near point  -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;;
;;; Find the ID at or near point and open a view of it. 'skg-goto'
;;; (C-c g RET, or C-c C-o in a view) visits the nearest ID on the
;;; line ('skg-nearest-id': a UUID, headline metadata, an
;;; [[id:..]] link or a .skg filename); 'skg-goto-by-id' (C-c g i)
;;; visits a typed ID; with G for g ('skg-goto-and-close-this',
;;; 'skg-goto-by-id-and-close-this') each also kills the buffer left
;;; behind. 'skg-id-next' and 'skg-id-prev' (C-c i n, C-c i p) hop
;;; between IDs; 'skg-goto-viewnode-id' (C-c g v) moves to the
;;; headline a message names by its viewnode ID. Requesting the view
;;; is 'skg-request-single-root-content-view.el's
;;; ('skg-request-single-root-content-view-from-id').
;;;
;;; Also here: 'skg-replace-links-with-labels', used by
;;; 'skg-search-make-link.el'. The linkstack, which pushes and pastes
;;; the IDs found here, is 'skg-linkstack.el's.
;;;
;;; todo ? speed : This could be more efficient.
;;; It re-reads the same text a few times.
;;;
;;; Code:

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
          ( link-regex "\\[\\[id:\\([^]]+\\)\\]\\[\\([^]]+\\)\\]\\]" )
          ( found-link nil ) ;; mutates
          ( link-gnid nil ))   ;; mutates
    (save-excursion
      ;; Search for links in the current line
      (goto-char line-start)
      (while ( and ( not found-link )
                   ( re-search-forward link-regex line-end t ))
        (let ( ( match-start ( match-beginning 0 ))
               ( match-end ( match-end 0 ))
               ( gnid ( match-string-no-properties 1 )) )
          (when ( and ;; Check if point is within this link
                  ( >= pos match-start )
                  ( <= pos match-end ))
            (setq found-link t)
            (setq link-gnid gnid )) )) )
    (if found-link
        (progn
          (message "Visiting node: %s" link-gnid)
          (skg-request-single-root-content-view-from-id link-gnid))
      (message "Point not on a link")) ))

(defun skg--id-at-point ()
  "Return (id . label) if point is on a UUID, metadata, link, or .skg filename.
Otherwise nil."
  (or (skg--point-on-uuid-v4-p)
      (skg--point-in-metadata-p)
      (skg--point-in-link-p)
      (skg--point-on-skg-filename-p)) )

(defun skg-nearest-id ()
  "Return (id . label) for the nearest ID on the current line, or nil,
or the symbol `ambiguous'.
In a buffer visiting a raw .skg file (`skg-file-minor-mode'), IDs are
the line's bare UUIDs and nearest means literally nearest:
point on an ID wins; a single-ID line needs no proximity; when every
ID sits on one side of point, the closest one; IDs on both sides are
`ambiguous' -- the caller should say so and visit nothing.
Elsewhere (content views, magit, search buffers) the prior behavior
stands: point on an ID, else the first ID after point on this line,
else the nearest one before it. Point is always restored."
  (if (bound-and-true-p skg-file-minor-mode)
      (skg--nearest-uuid-on-line-by-proximity)
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
      result )))

(defun skg--uuid-occurrences-on-line ()
  "Return each v4 UUID on the current line as (ID START END), in order."
  (let (( case-fold-search t )
        ( line-start (line-beginning-position) )
        ( line-end (line-end-position) )
        ( occurrences nil ))
    (save-excursion
      (goto-char line-start)
      (while (re-search-forward skg--uuid-v4-regex line-end t)
        (push (list (match-string-no-properties 0)
                    (match-beginning 0)
                    (match-end 0))
              occurrences)))
    (nreverse occurrences)))

(defun skg--nearest-uuid-on-line-by-proximity ()
  "The `skg-nearest-id' rule for raw .skg files. Return (id . id),
nil (no ID on the line), or `ambiguous' (IDs on both sides of point)."
  (or (skg--id-at-point) ;; point directly on an ID always wins
      (let* ((occurrences (skg--uuid-occurrences-on-line))
             (pos (point))
             (left (cl-remove-if-not
                    (lambda (o) (<= (nth 2 o) pos)) occurrences))
             (right (cl-remove-if-not
                     (lambda (o) (>= (nth 1 o) pos)) occurrences))
             (id-of (lambda (o) (cons (nth 0 o) (nth 0 o)))))
        (cond
         ((null occurrences) nil)
         ((null (cdr occurrences)) ;; exactly one: proximity is moot
          (funcall id-of (car occurrences)))
         ((null right) ;; all left of point: the last is the closest
          (funcall id-of (car (last left))))
         ((null left) ;; all right of point: the first is the closest
          (funcall id-of (car right)))
         (t 'ambiguous)))))

(defun skg-goto ()
  "Open a content view for the nearest ID on the current line.
The requested node itself is the raw root. Recursive content may
still use mirror substitution. In a raw .skg file, IDs on both
sides of point are ambiguous: it says so and visits nothing."
  (interactive)
  (let (( result (skg-nearest-id) ))
    (cond
     ((eq result 'ambiguous)
      (message "Point sits between IDs; move onto (or nearer to) one."))
     (result
      (let (( gnid (car result) ))
        (message "Visiting node: %s" gnid)
        (skg-request-single-root-content-view-from-id gnid)))
     (t (message "No ID found on this line")) )))

(defun skg-goto-by-id (gnid)
  "Open a content view for ID, prompting when called interactively. (skg-goto is usually more convenient.)"
  (interactive "sNode ID: ")
  (message "Visiting node: %s" gnid)
  (skg-request-single-root-content-view-from-id gnid))

;; Find a headline by its viewnode ID

(defun skg--viewnode-id-headline-regex (viewnode-id)
  "Regex matching a headline whose metadata carries (vnid VIEWNODE-ID)."
  (concat "^\\*+ (skg .*(vnid " (regexp-quote viewnode-id) ")"))

(defun skg-goto-viewnode-id (viewnode-id)
  "Move point to the headline carrying VIEWNODE-ID, prompting for it.
A message names a headline by its viewnode ID, which the headline's
metadata carries as (vnid X). The search reads the buffer text, so it
finds the ID while heralds hide the metadata. Returns the headline's
position, or nil (with a message) when no headline carries the ID."
  (interactive "sViewnode ID: ")
  (let* (( viewnode-id (string-trim viewnode-id) )
         ( headline-start
           (save-excursion
             (goto-char (point-min))
             (when (re-search-forward
                    (skg--viewnode-id-headline-regex viewnode-id) nil t)
               (line-beginning-position)) )))
    (if (not headline-start)
        (progn (message "No headline here carries viewnode ID %s"
                        viewnode-id)
               nil)
      (push-mark)
      (goto-char headline-start)
      (when (derived-mode-p 'org-mode)
        (org-reveal)) ;; open the folds hiding it
      headline-start)))

(defun skg-goto-and-close-this ()
  "Like `skg-goto', but also kill the buffer it was called from
once the goto request has been issued."
  (interactive)
  (let ((buf (current-buffer)))
    (skg-goto)
    (when (buffer-live-p buf)
      (kill-buffer buf))))

(defun skg-goto-by-id-and-close-this (gnid)
  "Like `skg-goto-by-id', but also kill the buffer it was called
from once the goto request has been issued."
  (interactive "sNode ID: ")
  (let ((buf (current-buffer)))
    (skg-goto-by-id gnid)
    (when (buffer-live-p buf)
      (kill-buffer buf))))

(defun skg-id-next ()
  "Move point to the next ID occurrence.
An ID can appear in metadata like (skg node (gnid X) ...),
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
An ID can appear in metadata like (skg node (gnid X) ...),
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
  (let (( metadata (skg--metadata-on-line) ))
    (when (and metadata
               (skg--metadata-sexp-contains-id-p (nth 2 metadata)) )
      (nth 0 metadata) )))

(defun skg--point-in-metadata-p ()
  "If point is within metadata, return (id . title). Otherwise nil."
  (let* (( pos (point) )
         ( metadata (skg--metadata-on-line) )
         ( skg-start (nth 0 metadata) )
         ( skg-end (nth 1 metadata) )
         ( sexp (nth 2 metadata) ))
    (when (and metadata
               (>= pos skg-start)
               (< pos skg-end)
               (skg--metadata-sexp-contains-id-p sexp) )
      (cons (skg--extract-id-from-metadata-sexp sexp)
            (string-trim
             (buffer-substring-no-properties
              skg-end (line-end-position) )) )) ))

(defun skg--metadata-sexp-contains-id-p
    (sexp)
  "Return t if SEXP is an occurrence's or a ghost's carrying
an id: (skg node (gnid ...) ...) or (skg ghost (gnid ...) ...)."
  (or (skg-sexp-subtree-p sexp '(skg node (gnid)))
      (skg-sexp-subtree-p sexp '(skg ghost (gnid)))))

(defun skg--extract-id-from-metadata-sexp
    (sexp)
  "Extract the id value from SEXP, an occurrence's
\(skg node (gnid X) ...) or a ghost's (skg ghost (gnid X) ...).
Returns the id as a string, or nil."
  (let ((val (and (skg--metadata-sexp-contains-id-p sexp)
                  (car (skg-sexp-cdr-at-path sexp '(skg gnid))))))
    (when val (format "%s" val))))

(defun skg--extract-repo-from-metadata-sexp (sexp)
  "Extract the repo value from SEXP, an occurrence's
\(skg node (skgRepos (nodeRepo X)) ...) or a ghost's
\(skg ghost (skgRepos (nodeRepo X)) ...). Returns the repo as a
string, or nil."
  (let ((val (car (skg-sexp-cdr-at-path sexp '(skg skgRepos nodeRepo)))))
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

(provide 'skg-id-search)
