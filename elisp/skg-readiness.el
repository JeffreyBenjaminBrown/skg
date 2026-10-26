;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: The commit-readiness gestures of the diff view
;;; ('docs/diff-view.org', "Readiness requests and gestures"). Each writes
;;; readiness requests, '(readyRequest OBJECTS)' or '(unreadyRequest
;;; OBJECTS)', into headline metadata, then saves. The save consumes
;;; them, setting or clearing the marks, and answers with the buffer
;;; rerendered and the readiness summary.
;;;
;;; The server says which leaves (single changes) a headline names:
;;; it writes '(leaves N OBJECT...)' in the headline's '(diff ...)'
;;; form, beside '(ready ...)'. The OBJECTs
;;; are those a request on the headline may name: 'node' (∃, its
;;; existence) and/or 'rel' (~, its relationship to its viewparent) on
;;; a vognode (a ghost among them), each worth one leaf; 'all' on a modified node
;;; (M, every leaf its file records, beside 'rel' when that names a
;;; leaf) and on a commit readiness aggregate (a changeset marker); none
;;; on a headline that names one leaf. N is how many distinct leaves a
;;; request naming every listed object touches. A gesture names every
;;; object the headline lists, and toggles by the headline's own '(ready
;;; ...)': it is ready when that names every listed object. A folder
;;; shown because of a change names no leaf itself; a gesture on it acts
;;; on its children.
;;;
;;; Below the headline a gesture acts on, it names no 'all': the leaves
;;; of a modified node or a commit readiness aggregate there are named
;;; by the headlines under it, or are not shown there.
;;;
;;; It also formats the readiness summary, which the save, stage-and-
;;; commit and integrate-unshown-changes show.
;;;
;;; DATA USED/ASSUMED: See docs/api-and-formats.org.

(require 'cl-lib)
(require 'org)
(require 'seq)
(require 'skg-metadata)
(require 'skg-modify-graph) ; for skg--immediate-viewchildren

;; The save module shows the readiness summary formatted here, so it
;; requires this module; the gestures load it only when they save.
(autoload 'skg-request-save-buffer "skg-request-save" nil t)

(defconst skg--readiness-wide-unmark-question
  "Unmarking this node commit-ready will unmark every contributing decision. You might discard a lot of hard-won information. Continue? "
  "Asked before a gesture unmarks more than one leaf.
`yes-or-no-p' appends the \"(yes or no)\".")

(defun skg-cycle-commit-ready (&optional recursive)
  "Toggle the commit-readiness of the change at point, then save.
On a headline that names leaves, toggle every object it lists: on a
new member its existence and its relationship (∃ and ~), on a
modified node (M) every leaf its file records, beside its
relationship when that changed. On a changed folder, toggle each
leaf one level down. With a prefix argument RECURSIVE, mark (or, if
the headline at point is ready, unmark) every leaf named in the
subtree at point. Before unmarking more than one leaf, ask. Works on
write-protected occurrences too."
  (interactive "P")
  (unless (org-at-heading-p)
    (user-error "Not on a headline"))
  (let ((requests (save-excursion
                    (if recursive
                        (skg--readiness-requests-for-subtree)
                      (skg--readiness-requests-at-point)))))
    (unless requests
      (user-error "Nothing here can be marked commit-ready"))
    (when (skg--readiness-write-requests-if-confirmed requests)
      (skg-request-save-buffer))))

(defun skg-toggle-homeSkgrepo-move-commitReady (&optional recursive)
  "Toggle the commit-readiness of the home-skgrepo move of the node at
point, then save: unmark it if it is ready, else mark it. With a
prefix argument RECURSIVE, do the same to the move of every
descendant that moved between the same two skgrepos. Before
unmarking more than one move, ask. Point may be on the node or on
its move. Works on write-protected occurrences too."
  (interactive "P")
  (unless (org-at-heading-p)
    (user-error "Not on a headline"))
  (let* ((move (or (skg--readiness-homeSkgrepo-move-at-point)
                   (user-error "This node's home skgrepo did not change")))
         (ready (save-excursion
                  (goto-char (car move))
                  (not (skg--readiness-ready-p
                        (skg--current-headline-metadata-sexp)))))
         (requests
          (save-excursion
            (goto-char (car move))
            (if (not recursive)
                (list (skg--readiness-request-here ready nil))
              (org-up-heading-safe)
              (skg--readiness-walk-subtree
               (lambda (metadata-sexp)
                 (when (equal (skg--readiness-homeSkgrepo-move metadata-sexp)
                              (cdr move))
                   (skg--readiness-request-here ready nil))))))))
    (when (skg--readiness-write-requests-if-confirmed requests)
      (if ready
          (message "Marked %d home skgrepo move(s) ready." (length requests))
        (message "Unmarked %d home skgrepo move(s)." (length requests)))
      (skg-request-save-buffer))))

;;; Reading a headline's change facts

(defun skg--readiness-host (metadata-sexp)
  "The list in METADATA-SEXP that holds its '(diff ...)' form: the
form among its elements that holds one, as a vognode's (a ghost's
among them) does, if one does, else the whole sexp, as a property's
or a changed folder's."
  (or (seq-find (lambda (element)
                  (and (consp element)
                       (assq 'diff (cdr element))))
                (cdr metadata-sexp))
      metadata-sexp))

(defun skg--readiness-diff-form (metadata-sexp)
  "The '(diff ...)' form of METADATA-SEXP, or nil."
  (assq 'diff (cdr (skg--readiness-host metadata-sexp))))

(defun skg--readiness-form (metadata-sexp key)
  "The (KEY ...) part of METADATA-SEXP's '(diff ...)' form, or nil."
  (assq key (cdr (skg--readiness-diff-form metadata-sexp))))

(defun skg--readiness-leaves (metadata-sexp)
  "The '(leaves N OBJECT...)' fact of the headline with METADATA-SEXP,
as (N . OBJECTS), or nil when it names no leaf."
  (cdr (skg--readiness-form metadata-sexp 'leaves)))

(defun skg--readiness-leaves-below (metadata-sexp)
  "The same, for a headline below the one a gesture acts on, without
`all': nil when `all' was its only object."
  (let ((leaves (skg--readiness-leaves metadata-sexp)))
    (if (not (memq 'all (cdr leaves)))
        leaves
      (let ((objects (remq 'all (cdr leaves))))
        (when objects
          (cons (length objects) objects))))))

(defun skg--readiness-objects-ready-p (metadata-sexp objects)
  "Non-nil when every one of OBJECTS is ready on METADATA-SEXP's headline:
when it carries a '(ready ...)' that names each of them. A headline
that lists no object (one naming a single leaf) is ready when it
carries any '(ready ...)'."
  (let ((ready (skg--readiness-form metadata-sexp 'ready)))
    (and ready
         (seq-every-p (lambda (object) (memq object (cdr ready)))
                      objects))))

(defun skg--readiness-changed-folder-p (metadata-sexp)
  "Non-nil when METADATA-SEXP is a folder shown because of a change."
  (seq-some (lambda (element)
              (and (consp element)
                   (string-suffix-p "Folder" (symbol-name (car element)))
                   (memq 'changed (cdr element))))
            (cdr metadata-sexp)))

(defun skg--readiness-ready-p (metadata-sexp)
  "Non-nil when the headline with METADATA-SEXP is ready: a headline
that names leaves, on all the objects it lists, or a changed folder
whose '(diff ...)' carries its aggregate, the objectless '(ready)'."
  (let ((leaves (skg--readiness-leaves metadata-sexp)))
    (cond (leaves (skg--readiness-objects-ready-p metadata-sexp (cdr leaves)))
          ((skg--readiness-changed-folder-p metadata-sexp)
           (skg--readiness-form metadata-sexp 'ready)))))

(defun skg--readiness-homeSkgrepo-move (metadata-sexp)
  "The (OLD NEW) skgrepos of METADATA-SEXP when it is a home skgrepo move."
  (let ((move (assq 'homeSkgrepoMove (cdr metadata-sexp))))
    (when move
      (list (cadr (assq 'old (cdr move)))
            (cadr (assq 'new (cdr move)))))))

(defun skg--readiness-homeSkgrepo-move-at-point ()
  "(POSITION . (OLD NEW)) of the home skgrepo move at point: the headline at
point, or the child of the node at point that is its home skgrepo move."
  (let* ((metadata-sexp (skg--metadata-sexp-at-point-or-nil))
         (pair (and metadata-sexp (skg--readiness-homeSkgrepo-move metadata-sexp))))
    (if pair
        (cons (line-beginning-position) pair)
      (save-excursion
        (let ((found nil))
          (skg--readiness-do-children
           (lambda (child-sexp)
             (let ((pair (skg--readiness-homeSkgrepo-move child-sexp)))
               (when (and pair (not found))
                 (setq found (cons (line-beginning-position) pair))))))
          found)))))

;;; Walking headlines

(defun skg--readiness-do-children (fn)
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

(defun skg--readiness-walk-subtree (fn)
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
;;; A gesture first gathers its requests, each '(MARKER READY OBJECTS
;;; COUNT)' for the headline at MARKER, COUNT being how many leaves it
;;; touches, then writes them all or none.

(defun skg--readiness-requests-at-point ()
  "The requests of the one-level gesture on the headline at point."
  (let* ((metadata-sexp (skg--current-headline-metadata-sexp))
         (leaves (skg--readiness-leaves metadata-sexp)))
    (cond
     (leaves
      (list (skg--readiness-request-here
             (not (skg--readiness-objects-ready-p metadata-sexp (cdr leaves)))
             (cdr leaves))))
     ((skg--readiness-changed-folder-p metadata-sexp)
      (let ((ready (not (skg--readiness-ready-p metadata-sexp))))
        (skg--readiness-do-children
         (lambda (child-sexp)
           (let ((child-leaves (skg--readiness-leaves-below child-sexp)))
             (when child-leaves
               (skg--readiness-request-here ready (cdr child-leaves)))))))))))

(defun skg--readiness-requests-for-subtree ()
  "The requests of the recursive gesture on the headline at point: one
on every headline in the subtree that names leaves, readying them
unless the headline at point is ready."
  (let* ((metadata-sexp (skg--current-headline-metadata-sexp))
         (root (line-beginning-position))
         (ready (not (skg--readiness-ready-p metadata-sexp))))
    (skg--readiness-walk-subtree
     (lambda (headline-sexp)
       (let ((leaves (if (= (line-beginning-position) root)
                         (skg--readiness-leaves headline-sexp)
                       (skg--readiness-leaves-below headline-sexp))))
         (when leaves
           (skg--readiness-request-here ready (cdr leaves))))))))

(defun skg--readiness-request-here (ready objects)
  "A request that marks (READY non-nil) or unmarks the leaves on OBJECTS
of the headline at point, with how many leaves it touches as the
server counts them: N of the headline's '(leaves N OBJECT...)' when
OBJECTS are the ones it lists, else one for each of `node' and `rel'.
A headline without the fact, which only the home skgrepo move gesture can
reach, counts one."
  (let ((leaves (skg--readiness-leaves (skg--current-headline-metadata-sexp))))
    (list (copy-marker (line-beginning-position)) ready objects
          (if (equal objects (cdr leaves))
              (or (car leaves) 1)
            (length objects)))))

(defun skg--readiness-unmarks-several-leaves-p (requests)
  "Non-nil when the unmark requests among REQUESTS touch more than one
leaf."
  (> (apply #'+ (mapcar (lambda (request) (nth 3 request))
                        (seq-remove #'cadr requests)))
     1))

(defun skg--readiness-write-requests-if-confirmed (requests)
  "Write REQUESTS, first asking when they unmark more than one leaf.
On refusal, write nothing and return nil."
  (let ((confirmed (or (not (skg--readiness-unmarks-several-leaves-p requests))
                       (yes-or-no-p skg--readiness-wide-unmark-question))))
    (save-excursion
      (dolist (request requests)
        (when confirmed
          (goto-char (car request))
          (skg--write-readiness-request (nth 1 request) (nth 2 request)))
        (set-marker (car request) nil)))
    confirmed))

(defun skg--write-readiness-request (ready objects)
  "Replace the readiness request of the headline at point with one
that marks (READY non-nil) or unmarks the leaves on OBJECTS. The
request is the last part of the headline's '(diff ...)' form, which
lies under '(node ...)', '(ghost ...)' or another vognode's form, or
at top level on a property. A headline without the form gets one."
  (let* ((metadata-sexp (skg--current-headline-metadata-sexp))
         (host (skg--readiness-host metadata-sexp))
         (request (cons (if ready 'readyRequest 'unreadyRequest) objects))
         (diff-edit
          (if (skg--readiness-diff-form metadata-sexp)
              `(diff (DELETE (readyRequest) (unreadyRequest)) ,request)
            ;; A missing form is appended whole, so it holds no DELETE.
            `(diff ,request))))
    (skg-edit-metadata-at-point
     (if (eq host metadata-sexp)
         `(skg ,diff-edit)
       `(skg (,(car host) ,diff-edit))))))

;;; Moving between change headlines
;;; A change headline is one whose metadata carries a '(leaves ...)'
;;; fact. It is ready when its '(ready ...)' names every object that
;;; fact lists, and otherwise unready. Outside the diff view no
;;; headline carries one, so these commands report no change there.

(defun skg-diffView-goto-next-change ()
  "Move point to the next change headline after point."
  (interactive)
  (skg--diffView-goto-change 'next nil))

(defun skg-diffView-goto-next-unready-change ()
  "Move point to the next unready change headline after point."
  (interactive)
  (skg--diffView-goto-change 'next 'unready))

(defun skg-diffView-goto-next-ready-change ()
  "Move point to the next ready change headline after point."
  (interactive)
  (skg--diffView-goto-change 'next 'ready))

(defun skg-diffView-goto-previous-change ()
  "Move point to the previous change headline before point."
  (interactive)
  (skg--diffView-goto-change 'previous nil))

(defun skg-diffView-goto-previous-unready-change ()
  "Move point to the previous unready change headline before point."
  (interactive)
  (skg--diffView-goto-change 'previous 'unready))

(defun skg-diffView-goto-previous-ready-change ()
  "Move point to the previous ready change headline before point."
  (interactive)
  (skg--diffView-goto-change 'previous 'ready))

(defun skg--diffView-goto-change (direction readiness)
  "Move point to the nearest change headline after point (DIRECTION
'next) or before it (DIRECTION 'previous), in buffer order, without
wrapping: one whose readiness is READINESS ('ready or 'unready), or
any change headline when READINESS is nil. Point lands where
`skg-id-next' puts it on a headline, the opening paren of its
metadata. When there is none, say so and leave point."
  (let ((target (skg--diffView-change-position direction readiness)))
    (if target
        (goto-char target)
      (message "%s"
               (concat "No "
                       (if (eq direction 'next) "further" "earlier")
                       (when readiness
                         (concat " " (symbol-name readiness)))
                       " change")))))

(defun skg--diffView-change-position (direction readiness)
  "Where `skg--diffView-goto-change' would move point, or nil."
  (let ((start-pos (point))
        (step (if (eq direction 'next) 1 -1)))
    (save-excursion
      (beginning-of-line)
      (cl-loop for change-pos = (skg--diffView-change-start-on-line
                                 readiness)
               when (and change-pos
                         (if (eq direction 'next)
                             (> change-pos start-pos)
                           (< change-pos start-pos)))
               return change-pos
               while (and (zerop (forward-line step))
                          (not (eobp)))))))

(defun skg--diffView-change-start-on-line (readiness)
  "On a change headline whose readiness is READINESS (any when nil),
the position of its metadata's opening paren; otherwise nil."
  (when (looking-at "^\\*+ (skg")
    (let ((metadata-start (- (match-end 0) 4))
          (metadata-sexp (ignore-errors
                           (skg--metadata-sexp-at-point-or-nil))))
      (when (and metadata-sexp
                 (skg--readiness-leaves metadata-sexp)
                 (pcase readiness
                   ('ready (skg--readiness-ready-p metadata-sexp))
                   ('unready (not (skg--readiness-ready-p metadata-sexp)))
                   (_ t)))
        metadata-start))))

;;; The readiness summary

(defun skg--readiness-summary-message (summary)
  "The line shown after a save of a buffer of the diff view,
from SUMMARY, the save response's
'(readiness-summary (ready N) (changed M) (foreign F) (order-only O)
\(notEditable K))'."
  (let* ((count (lambda (key)
                  (or (cadr (assq key (cdr summary))) 0)))
         (ready (funcall count 'ready))
         (changed (funcall count 'changed))
         (foreign (funcall count 'foreign))
         (not-editable (funcall count 'notEditable))
         (complete (= ready changed)))
    (concat
     (if complete
         (format "Commit ready. All %d of %d changed files are ready"
                 ready changed)
       (format "%d of %d changed files are ready" ready changed))
     (if (> not-editable 0)
         (format ", %d changes are not editable in this view; C-c r u integrates them"
                 not-editable)
       "")
     (if (> foreign 0)
         (format ", %d files changed in foreign skgrepos" foreign)
       "")
     (if complete "." ""))))

(provide 'skg-readiness)
