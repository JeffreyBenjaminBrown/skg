;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: The processed gestures of the diff view
;;; ('docs/diff-view.org', "Processed requests and gestures"). Each says
;;; what it marks and which way: 'C-c m', then 'n' (the node), 'r' (its
;;; relationship to its viewparent) or 'b' (both), then 'p' (processed)
;;; or 'u' (unprocessed). It writes processed requests, '(processed
;;; OBJECTS)' or '(unprocessed OBJECTS)', into the '(editRequest ...)'
;;; of headline metadata, then saves. The save consumes them, setting
;;; or clearing the marks, and answers with the buffer rerendered and
;;; the processed summary.
;;;
;;; The server says which leaves (single changes) a headline names:
;;; it writes '(leaves N OBJECT...)' in the headline's '(diff ...)'
;;; form, beside '(processed ...)'. The OBJECTs are those of 'node'
;;; and 'rel' that name some leaf there; N is how many distinct leaves
;;; a request naming every listed object touches. A headline that
;;; names exactly one leaf (a property member, an old text, a home
;;; skgrepo or relRepo move) lists no object, and a bare request marks
;;; it. A changeset marker lists only 'rel', though 'node' there names
;;; its members' own leaves. A folder shown because of a change names
;;; no leaf itself; a gesture on it acts on its children. The server
;;; writes such a folder as its bare atom; whether it is changed, and
;;; processed, follows from its members
;;; (`skg--folder-change-processed-at-point').
;;;
;;; The letter's objects are what a gesture names on each headline,
;;; filtered by what the headline lists
;;; (`skg--processed-objects-named-by-letters').
;;;
;;; It also formats the processed summary, which the save, stage-and-
;;; commit and extend-diff-view show.
;;;
;;; DATA USED/ASSUMED: See docs/api-and-formats.org.

(require 'cl-lib)
(require 'org)
(require 'seq)
(require 'skg-metadata)
(require 'skg-modify-graph) ; for skg--immediate-viewchildren

;; The save module shows the processed summary formatted here, so it
;; requires this module; the gestures load it only when they save.
(autoload 'skg-request-save-buffer "skg-request-save" nil t)

(defconst skg--processed-wide-unmark-question
  "Unmarking this node processed will unmark every contributing decision. You might discard a lot of hard-won information. Continue? "
  "Asked before a gesture unmarks more than one leaf.
`yes-or-no-p' appends the \"(yes or no)\".")

(defun skg-mark-node-processed (&optional recursive)
  "Mark the node at point processed, then save: every leaf of the node
itself (its existence; on a modified node its text, home skgrepo,
aliases, extra IDs and flags), not its relationship to its viewparent.
With a prefix argument RECURSIVE, mark the node of every headline in
the subtree (`skg--mark-processed')."
  (interactive "P")
  (skg--mark-processed '(node) t recursive))

(defun skg-mark-node-unprocessed (&optional recursive)
  "As `skg-mark-node-processed', but unmark."
  (interactive "P")
  (skg--mark-processed '(node) nil recursive))

(defun skg-mark-rel-processed (&optional recursive)
  "Mark processed the relationship of the headline at point to its
viewparent, then save. With a prefix argument RECURSIVE, mark the
relationship of every headline in the subtree (`skg--mark-processed')."
  (interactive "P")
  (skg--mark-processed '(rel) t recursive))

(defun skg-mark-rel-unprocessed (&optional recursive)
  "As `skg-mark-rel-processed', but unmark."
  (interactive "P")
  (skg--mark-processed '(rel) nil recursive))

(defun skg-mark-both-processed (&optional recursive)
  "Mark processed both the node at point and its relationship to its
viewparent, then save. With a prefix argument RECURSIVE, mark both on
every headline in the subtree (`skg--mark-processed')."
  (interactive "P")
  (skg--mark-processed '(node rel) t recursive))

(defun skg-mark-both-unprocessed (&optional recursive)
  "As `skg-mark-both-processed', but unmark."
  (interactive "P")
  (skg--mark-processed '(node rel) nil recursive))

(defun skg--mark-processed (letter-objects processed recursive)
  "Mark (PROCESSED non-nil) or unmark what LETTER-OBJECTS, '(node),
'(rel) or '(node rel), name on the headline at point, then save. On a
changed folder, act on each headline one level down. With a prefix
argument RECURSIVE, act on every headline in the subtree at point,
except that at a new node in content position the server walks the
graph instead of the buffer: with one C-u contentward, with two along
every relationship each new node records, the letters filtering what
the walk marks (`skg--processed-requests-for-subtree'). Before
unmarking more than one leaf, ask. Works on write-protected
occurrences too."
  (unless (org-at-heading-p)
    (user-error "Not on a headline"))
  (let ((requests (save-excursion
                    (if recursive
                        (skg--processed-requests-for-subtree
                         letter-objects processed
                         (if (equal recursive '(16)) 'newOutward 'newContent))
                      (skg--processed-requests-at-point
                       letter-objects processed)))))
    (unless requests
      (user-error "%s" (skg--processed-nothing-to-mark-text letter-objects)))
    (when (skg--processed-write-requests-if-confirmed requests)
      (skg-request-save-buffer))))

(defun skg--processed-nothing-to-mark-text (letter-objects)
  "What a gesture with LETTER-OBJECTS says when it names nothing."
  (pcase letter-objects
    ('(node) "Its node has no change to mark here")
    ('(rel) "Its relationship to its viewparent has no change to mark here")
    (_ "Neither its node nor its relationship has a change to mark here")))

(defun skg-mark-homeSkgrepo-move-processed (&optional recursive)
  "Mark the home-skgrepo move of the node at point processed, then save.
With a prefix argument RECURSIVE, mark too the move of every
descendant that moved between the same two skgrepos
\(`skg--mark-homeSkgrepo-move')."
  (interactive "P")
  (skg--mark-homeSkgrepo-move t recursive))

(defun skg-mark-homeSkgrepo-move-unprocessed (&optional recursive)
  "As `skg-mark-homeSkgrepo-move-processed', but unmark."
  (interactive "P")
  (skg--mark-homeSkgrepo-move nil recursive))

(defun skg--mark-homeSkgrepo-move (processed recursive)
  "Mark (PROCESSED non-nil) or unmark the home-skgrepo move of the node
at point, then save. With RECURSIVE, do the same to the move of every
descendant that moved between the same two skgrepos. Before unmarking
more than one move, ask. Point may be on the node or on its move.
Works on write-protected occurrences too."
  (unless (org-at-heading-p)
    (user-error "Not on a headline"))
  (let* ((move (or (skg--processed-homeSkgrepo-move-at-point)
                   (user-error "This node's home skgrepo did not change")))
         (requests
          (save-excursion
            (goto-char (car move))
            (if (not recursive)
                (list (skg--processed-request-at-point processed nil))
              (org-up-heading-safe)
              (skg--processed-walk-subtree
               (lambda (metadata-sexp)
                 (when (equal (skg--processed-homeSkgrepo-move metadata-sexp)
                              (cdr move))
                   (skg--processed-request-at-point processed nil))))))))
    (when (skg--processed-write-requests-if-confirmed requests)
      (if processed
          (message "Marked %d home skgrepo move(s) processed." (length requests))
        (message "Unmarked %d home skgrepo move(s)." (length requests)))
      (skg-request-save-buffer))))

;;; Reading a headline's change facts

(defun skg--processed-objects-named-by-letters
    (metadata-sexp letter-objects &optional at-gesture)
  "What a gesture with LETTER-OBJECTS names on the headline with
METADATA-SEXP, at point: nil when it names nothing there, else
\(OBJECTS), OBJECTS the request's, nil for a bare request.
On a headline that lists objects, the letters among them. On one that
names a single leaf, the bare request, when the leaf is of a kind a
letter names: a relRepo move is a relationship's, any other the
node's own (`skg--processed-single-leaf-kind'). On a changeset marker
AT-GESTURE (the headline the gesture acts on), the letters
themselves: its 'node' names its members' own leaves, which its
'(leaves ...)' does not list; below the gesture those members name
them under their own headlines."
  (let ((leaves (skg--processed-leaves metadata-sexp)))
    (cond
     ((null leaves) nil)
     ((null (cdr leaves))
      (when (memq (skg--processed-single-leaf-kind metadata-sexp)
                  letter-objects)
        (list nil)))
     ((and at-gesture (assq 'changesetMarker (cdr metadata-sexp)))
      (list letter-objects))
     (t (let ((objects (seq-filter (lambda (object) (memq object (cdr leaves)))
                                   letter-objects)))
          (when objects (list objects)))))))

(defun skg--processed-single-leaf-kind (metadata-sexp)
  "The letter object naming the one leaf of a headline that names one:
`rel' for a relRepo move, else `node' (an alias, extra ID or flag of
its folder's viewparent, an old text, a home skgrepo move)."
  (if (assq 'relRepoMove (cdr metadata-sexp)) 'rel 'node))

(defun skg--processed-p (metadata-sexp)
  "Non-nil when the headline with METADATA-SEXP, at point, is processed: a
headline that names leaves, on all the objects it lists, or a changed
folder when every change of its members' relationships is processed."
  (let ((leaves (skg--processed-leaves metadata-sexp)))
    (if leaves
        (skg--objects-processed-p metadata-sexp (cdr leaves))
      (eq (skg--folder-change-processed-at-point) 'processed))))

(defun skg--processed-homeSkgrepo-move (metadata-sexp)
  "The (OLD NEW) skgrepos of METADATA-SEXP when it is a home skgrepo move."
  (let ((move (assq 'homeSkgrepoMove (cdr metadata-sexp))))
    (when move
      (list (cadr (assq 'old (cdr move)))
            (cadr (assq 'new (cdr move)))))))

(defun skg--processed-homeSkgrepo-move-at-point ()
  "(POSITION . (OLD NEW)) of the home skgrepo move at point: the headline at
point, or the child of the node at point that is its home skgrepo move."
  (let* ((metadata-sexp (skg--metadata-sexp-at-point-or-nil))
         (pair (and metadata-sexp (skg--processed-homeSkgrepo-move metadata-sexp))))
    (if pair
        (cons (line-beginning-position) pair)
      (save-excursion
        (let ((found nil))
          (skg--processed-do-children
           (lambda (child-sexp)
             (let ((pair (skg--processed-homeSkgrepo-move child-sexp)))
               (when (and pair (not found))
                 (setq found (cons (line-beginning-position) pair))))))
          found)))))

;;; Walking headlines

(defun skg--processed-do-children (fn)
  "Call FN, with point on each, on the metadata of the headlines one
level below the headline at point. Returns the non-nil results, in
buffer order. Leaves point on the last of those headlines."
  (delq nil
        (mapcar (lambda (position)
                  (goto-char position)
                  (let ((metadata-sexp (skg--metadata-sexp-at-point-or-nil)))
                    (and metadata-sexp
                         (save-excursion (funcall fn metadata-sexp)))))
                (skg--immediate-viewchildren))))

(defun skg--processed-walk-subtree (fn)
  "Call FN, with point on each, on the metadata of the headline at
point and of every headline below it. Returns the non-nil results,
in buffer order."
  (let ((level (org-outline-level))
        (results '())
        (first t))
    (while (and (not (eobp))
                (or first (> (org-outline-level) level)))
      (setq first nil)
      (let* ((metadata-sexp (skg--metadata-sexp-at-point-or-nil))
             (result (and metadata-sexp
                          (save-excursion (funcall fn metadata-sexp)))))
        (when result
          (push result results)))
      (outline-next-heading))
    (nreverse results)))

;;; The gestures
;;; A gesture first gathers its requests, each '(MARKER PROCESSED OBJECTS
;;; COUNT)' for the headline at MARKER, COUNT being how many leaves it
;;; touches, then writes them all or none.

(defun skg--processed-requests-at-point (letter-objects processed)
  "The requests of the one-level gesture with LETTER-OBJECTS on the
headline at point, marking (PROCESSED non-nil) or unmarking: on a
headline that names leaves, one, naming what
`skg--processed-objects-named-by-letters' says; on a changed folder,
one on each child that names some of them."
  (let* ((metadata-sexp (skg--current-headline-metadata-sexp))
         (named (skg--processed-objects-named-by-letters
                 metadata-sexp letter-objects t)))
    (cond
     (named
      (list (skg--processed-request-at-point processed (car named))))
     ((and (not (skg--processed-leaves metadata-sexp))
           (skg--folder-change-processed-at-point))
      (skg--processed-do-children
       (lambda (child-sexp)
         (let ((child-named (skg--processed-objects-named-by-letters
                             child-sexp letter-objects)))
           (when child-named
             (skg--processed-request-at-point processed (car child-named))))))))))

(defun skg--processed-requests-for-subtree (letter-objects processed walk-object)
  "The requests of the recursive gesture with LETTER-OBJECTS on the
headline at point, marking (PROCESSED non-nil) or unmarking: one on
every headline in the subtree that names some of them. At a new node
in content position (`skg--processed-hands-to-the-server-p'), the
request names LETTER-OBJECTS and WALK-OBJECT, `newContent' or
`newOutward', which the server expands from the graph, through new
nodes only, whatever the buffer shows or write-protects, the letters
naming both the headline's own objects and the kinds of leaf the walk
marks; so the walk skips the rest of that node's subtree. The letters
are sent whole there even where the headline lists fewer, since
without them the walk would name every kind."
  (org-back-to-heading t)
  (let ((root (point))
        (end (save-excursion (org-end-of-subtree t t) (point)))
        (requests '()))
    (while (< (point) end)
      (let* ((headline-sexp (skg--metadata-sexp-at-point-or-nil))
             (at-root (= (point) root))
             (handed-over (and headline-sexp
                               (if at-root
                                   (skg--processed-node-added-p headline-sexp)
                                 (skg--processed-hands-to-the-server-p
                                  headline-sexp))))
             (named (cond (handed-over
                           (list (append letter-objects (list walk-object))))
                          (headline-sexp
                           (skg--processed-objects-named-by-letters
                            headline-sexp letter-objects at-root)))))
        (when named
          (push (save-excursion
                  (skg--processed-request-at-point processed (car named)))
                requests))
        (if handed-over
            (org-end-of-subtree t t)
          (outline-next-heading))))
    (nreverse requests)))

(defun skg--processed-hands-to-the-server-p (metadata-sexp)
  "Non-nil when the recursive gesture hands the headline at point, with
METADATA-SEXP, to the server: a new node whose viewparent is not a
folder, so that it stands in its viewparent's content."
  (and (skg--processed-node-added-p metadata-sexp)
       (save-excursion
         (and (org-up-heading-safe)
              (not (skg--folder-atom
                    (skg--metadata-sexp-at-point-or-nil)))))))

(defun skg--processed-node-added-p (metadata-sexp)
  "Non-nil when METADATA-SEXP's headline is a new node: its '(diff ...)'
says '(node added)'."
  (eq (cadr (skg--processed-form metadata-sexp 'node)) 'added))

(defun skg--processed-request-at-point (processed objects)
  "A request that marks (PROCESSED non-nil) or unmarks the leaves on
OBJECTS of the headline at point, with how many leaves it touches
\(`skg--processed-leaves-touched')."
  (list (copy-marker (line-beginning-position)) processed objects
        (skg--processed-leaves-touched
         (skg--processed-leaves (skg--current-headline-metadata-sexp))
         objects)))

(defun skg--processed-leaves-touched (leaves objects)
  "How many leaves a request naming OBJECTS touches on a headline whose
'(leaves N OBJECT...)' is LEAVES, (N . LISTED), as the server counts
them. Of the objects listed there: N when OBJECTS name them all; one
for 'rel' alone, since it names the one relationship to the viewparent;
N less that one for 'node' alone. An object the headline does not list
\('node' on a changeset marker, 'newContent', 'newOutward') counts two,
so that unmarking it always asks first: the client cannot count what
the server will reach. A headline without the fact, which only the
home skgrepo move gesture can reach, and a bare request count one."
  (let* ((listed (cdr leaves))
         (named-listed (seq-filter (lambda (object) (memq object listed))
                                   objects))
         (unlisted (seq-difference objects listed)))
    (+ (* 2 (length unlisted))
       (cond ((or (null leaves) (null objects)) 1)
             ((null named-listed) 0)
             ((= (length named-listed) (length listed)) (car leaves))
             ((equal named-listed '(rel)) 1)
             (t (1- (car leaves)))))))

(defun skg--processed-unmarks-several-leaves-p (requests)
  "Non-nil when the unmark requests among REQUESTS touch more than one
leaf."
  (> (apply #'+ (mapcar (lambda (request) (nth 3 request))
                        (seq-remove #'cadr requests)))
     1))

(defun skg--processed-write-requests-if-confirmed (requests)
  "Write REQUESTS, first asking when they unmark more than one leaf.
On refusal, write nothing and return nil."
  (let ((confirmed (or (not (skg--processed-unmarks-several-leaves-p requests))
                       (yes-or-no-p skg--processed-wide-unmark-question))))
    (save-excursion
      (dolist (request requests)
        (when confirmed
          (goto-char (car request))
          (skg--write-processed-request (nth 1 request) (nth 2 request)))
        (set-marker (car request) nil)))
    confirmed))

(defun skg--write-processed-request (processed objects)
  "Replace the processed request of the headline at point with one
that marks (PROCESSED non-nil) or unmarks the leaves on OBJECTS: '(processed
OBJECTS)' or '(unprocessed OBJECTS)', the last part of the headline's
'(editRequest ...)', which stands at the top level of its metadata,
whatever its kind. The headline's other edit request stays. A
headline without '(editRequest ...)' gets one where the server writes
it (`skg--metadata-with-processed-request')."
  (skg--current-headline-metadata-sexp) ;; refuses a headline without
  (skg--rewrite-metadata-at-point
   (lambda (metadata-sexp)
     (skg--metadata-with-processed-request
      metadata-sexp (cons (if processed 'processed 'unprocessed) objects)))))

;;; Moving between headlines with change facts
;;; A headline with change facts is one whose metadata carries a
;;; '(leaves ...)' fact. It is processed when its '(processed ...)' names every
;;; object that fact lists, and otherwise unprocessed. Outside the diff view no
;;; headline carries one, so these commands report no change there.

(defun skg-diffView-goto-next-change ()
  "Move point to the next headline with change facts after point."
  (interactive)
  (skg--diffView-goto-change 'next nil))

(defun skg-diffView-goto-next-unprocessed-change ()
  "Move point to the next unprocessed headline with change facts after point."
  (interactive)
  (skg--diffView-goto-change 'next 'unprocessed))

(defun skg-diffView-goto-next-processed-change ()
  "Move point to the next processed headline with change facts after point."
  (interactive)
  (skg--diffView-goto-change 'next 'processed))

(defun skg-diffView-goto-previous-change ()
  "Move point to the previous headline with change facts before point."
  (interactive)
  (skg--diffView-goto-change 'previous nil))

(defun skg-diffView-goto-previous-unprocessed-change ()
  "Move point to the previous unprocessed headline with change facts before point."
  (interactive)
  (skg--diffView-goto-change 'previous 'unprocessed))

(defun skg-diffView-goto-previous-processed-change ()
  "Move point to the previous processed headline with change facts before point."
  (interactive)
  (skg--diffView-goto-change 'previous 'processed))

(defun skg--diffView-goto-change (direction processed)
  "Move point to the nearest headline with change facts after point (DIRECTION
'next) or before it (DIRECTION 'previous), in buffer order, without
wrapping: one that is PROCESSED ('processed or 'unprocessed), or
any headline with change facts when PROCESSED is nil. Point lands where
`skg-id-next' puts it on a headline, the opening paren of its
metadata. When there is none, say so and leave point."
  (let ((target (skg--diffView-change-position direction processed)))
    (if target
        (goto-char target)
      (message "%s"
               (concat "No "
                       (if (eq direction 'next) "further" "earlier")
                       (when processed
                         (concat " " (symbol-name processed)))
                       " change")))))

(defun skg--diffView-change-position (direction processed)
  "Where `skg--diffView-goto-change' would move point, or nil."
  (let ((start-pos (point))
        (step (if (eq direction 'next) 1 -1)))
    (save-excursion
      (beginning-of-line)
      (cl-loop for change-pos = (skg--diffView-change-start-on-line
                                 processed)
               when (and change-pos
                         (if (eq direction 'next)
                             (> change-pos start-pos)
                           (< change-pos start-pos)))
               return change-pos
               while (and (zerop (forward-line step))
                          (not (eobp)))))))

(defun skg--diffView-change-start-on-line (processed)
  "On a headline with change facts that is PROCESSED (any when nil),
the position of its metadata's opening paren; otherwise nil."
  (when (looking-at "^\\*+ (skg")
    (let ((metadata-start (- (match-end 0) 4))
          (metadata-sexp (ignore-errors
                           (skg--metadata-sexp-at-point-or-nil))))
      (when (and metadata-sexp
                 (skg--processed-leaves metadata-sexp)
                 (pcase processed
                   ('processed (skg--processed-p metadata-sexp))
                   ('unprocessed (not (skg--processed-p metadata-sexp)))
                   (_ t)))
        metadata-start))))

;;; The processed summary

(defun skg--processed-summary-message (summary)
  "The line shown after a save of a buffer of the diff view,
from SUMMARY, the save response's
'(processed-summary (processed N) (changed M) (foreign F) (order-only O)
\(notEditable K))'."
  (let* ((count (lambda (key)
                  (or (cadr (assq key (cdr summary))) 0)))
         (processed (funcall count 'processed))
         (changed (funcall count 'changed))
         (foreign (funcall count 'foreign))
         (not-editable (funcall count 'notEditable))
         (complete (= processed changed)))
    (concat
     (if complete
         (format "Commit ready. All %d of %d changed files are processed"
                 processed changed)
       (format "%d of %d changed files are processed" processed changed))
     (if (> not-editable 0)
         (format ", %d changes are not editable in this view; C-c r u extends the diff view to them"
                 not-editable)
       "")
     (if (> foreign 0)
         (format ", %d files changed in foreign skgrepos" foreign)
       "")
     (if complete "." ""))))

(provide 'skg-processed)
