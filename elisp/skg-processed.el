;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: The processed gestures of the diff view
;;; ('docs/diff-view.org', "Processed requests and gestures"). Each writes
;;; processed requests, '(processed OBJECTS)' or '(unprocessed OBJECTS)', into
;;; the '(editRequest ...)' of headline metadata, then saves. The save
;;; consumes them, setting or clearing the marks, and answers with the
;;; buffer rerendered and the processed summary.
;;;
;;; The server says which leaves (single changes) a headline names:
;;; it writes '(leaves N OBJECT...)' in the headline's '(diff ...)'
;;; form, beside '(processed ...)'. The OBJECTs
;;; are those a request on the headline may name: 'node' (∃, its
;;; existence) and/or 'rel' (~, its relationship to its viewparent) on
;;; a vognode (a ghost among them), each worth one leaf; 'all' on a modified node
;;; (M, every leaf its file records, beside 'rel' when that names a
;;; leaf) and on a processed aggregate (a changeset marker); none
;;; on a headline that names one leaf. N is how many distinct leaves a
;;; request naming every listed object touches. A gesture names every
;;; object the headline lists, and toggles by the headline's own '(processed
;;; ...)': it is processed when that names every listed object. A folder
;;; shown because of a change names no leaf itself; a gesture on it acts
;;; on its children. The server writes such a folder as its bare atom;
;;; whether it is changed, and processed, follows from its members
;;; (`skg--folder-change-processed-at-point').
;;;
;;; Below the headline a gesture acts on, it names no 'all': the leaves
;;; of a modified node or a processed aggregate there are named
;;; by the headlines under it, or are not shown there.
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

(defun skg-cycle-processed (&optional recursive)
  "Toggle whether the change at point is processed, then save.
On a headline that names leaves, toggle every object it lists: on a
new member its existence and its relationship (∃ and ~), on a
modified node (M) every leaf its file records, beside its
relationship when that changed. On a changed folder, toggle each
leaf one level down. With a prefix argument RECURSIVE, mark (or, if
the headline at point is processed, unmark) every leaf named in the
subtree at point, except that at a new node the server walks the
graph instead of the buffer: with one C-u, contentward, marking each
new node's text and content; with two, along every relationship each
new node records, marking everything it records
\(`skg--processed-requests-for-subtree'). Before unmarking more than
one leaf, ask. Works on write-protected occurrences too."
  (interactive "P")
  (unless (org-at-heading-p)
    (user-error "Not on a headline"))
  (let ((requests (save-excursion
                    (if recursive
                        (skg--processed-requests-for-subtree
                         (if (equal recursive '(16)) 'newOutward 'newContent))
                      (skg--processed-requests-at-point)))))
    (unless requests
      (user-error "Nothing here can be marked processed"))
    (when (skg--processed-write-requests-if-confirmed requests)
      (skg-request-save-buffer))))

(defun skg-toggle-homeSkgrepo-move-processed (&optional recursive)
  "Toggle whether the home-skgrepo move of the node at point is
processed, then save: unmark it if it is processed, else mark it. With a
prefix argument RECURSIVE, do the same to the move of every
descendant that moved between the same two skgrepos. Before
unmarking more than one move, ask. Point may be on the node or on
its move. Works on write-protected occurrences too."
  (interactive "P")
  (unless (org-at-heading-p)
    (user-error "Not on a headline"))
  (let* ((move (or (skg--processed-homeSkgrepo-move-at-point)
                   (user-error "This node's home skgrepo did not change")))
         (processed (save-excursion
                  (goto-char (car move))
                  (not (skg--processed-p
                        (skg--current-headline-metadata-sexp)))))
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

(defun skg--processed-leaves-below (metadata-sexp)
  "`skg--processed-leaves' of METADATA-SEXP, for a headline below the
one a gesture acts on, without `all': nil when `all' was its only
object."
  (let ((leaves (skg--processed-leaves metadata-sexp)))
    (if (not (memq 'all (cdr leaves)))
        leaves
      (let ((objects (remq 'all (cdr leaves))))
        (when objects
          (cons (length objects) objects))))))

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

(defun skg--processed-requests-at-point ()
  "The requests of the one-level gesture on the headline at point."
  (let* ((metadata-sexp (skg--current-headline-metadata-sexp))
         (leaves (skg--processed-leaves metadata-sexp)))
    (cond
     (leaves
      (list (skg--processed-request-at-point
             (not (skg--objects-processed-p metadata-sexp (cdr leaves)))
             (cdr leaves))))
     ((skg--folder-change-processed-at-point)
      (let ((processed (not (skg--processed-p metadata-sexp))))
        (skg--processed-do-children
         (lambda (child-sexp)
           (let ((child-leaves (skg--processed-leaves-below child-sexp)))
             (when child-leaves
               (skg--processed-request-at-point processed (cdr child-leaves)))))))))))

(defun skg--processed-requests-for-subtree (walk-object)
  "The requests of the recursive gesture on the headline at point: one
on every headline in the subtree that names leaves, marking them processed
unless the headline at point is processed. At a new node in content
position (`skg--processed-hands-to-the-server-p'), the request names
WALK-OBJECT, `newContent' or `newOutward', which the server expands
from the graph, through new nodes only, whatever the buffer shows or
write-protects; so the walk skips the rest of that node's subtree.
There the start names WALK-OBJECT alone, leaving the relationship to
its viewparent, which is no part of what it reaches, and is processed
when its existence is."
  (org-back-to-heading t)
  (let* ((metadata-sexp (skg--current-headline-metadata-sexp))
         (root (point))
         (end (save-excursion (org-end-of-subtree t t) (point)))
         (root-is-handed-over (skg--processed-node-added-p metadata-sexp))
         (processed (not (if root-is-handed-over
                         (skg--objects-processed-p metadata-sexp '(node))
                       (skg--processed-p metadata-sexp))))
         (requests '()))
    (while (< (point) end)
      (let* ((headline-sexp (skg--metadata-sexp-at-point-or-nil))
             (at-root (= (point) root))
             (handed-over (if at-root
                              root-is-handed-over
                            (and headline-sexp
                                 (skg--processed-hands-to-the-server-p
                                  headline-sexp))))
             (leaves
              (and headline-sexp
                   (not (and at-root handed-over)) ; the start names the walk alone
                   (if at-root
                       (skg--processed-leaves headline-sexp)
                     (skg--processed-leaves-below headline-sexp)))))
        (when (or leaves handed-over)
          (push (save-excursion
                  (skg--processed-request-at-point
                   processed (append (cdr leaves)
                                 (and handed-over (list walk-object)))))
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
  "A request that marks (PROCESSED non-nil) or unmarks the leaves on OBJECTS
of the headline at point, with how many leaves it touches as the
server counts them: N of the headline's '(leaves N OBJECT...)' when
OBJECTS are the ones it lists, else one for each object, and two for
`newContent' or `newOutward', so that unmarking new nodes always asks
first: the client cannot count what the server will reach. A
headline without the fact, which only the home skgrepo move gesture
can reach, counts one."
  (let ((leaves (skg--processed-leaves (skg--current-headline-metadata-sexp))))
    (list (copy-marker (line-beginning-position)) processed objects
          (if (equal objects (cdr leaves))
              (or (car leaves) 1)
            (+ (length objects)
               (seq-count (lambda (object) (memq object '(newContent newOutward)))
                          objects))))))

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
