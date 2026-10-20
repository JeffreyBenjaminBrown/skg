;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: The commit-readiness gestures of the total diff view
;;; ('TODO/total-diff-view/02_plan.org', "gestures"). Each writes
;;; readiness requests, '(readyRequest AXES)' or '(unreadyRequest
;;; AXES)', into headline metadata, then saves. The save consumes
;;; them, setting or clearing the marks, and answers with the buffer
;;; redrawn and the readiness summary.
;;;
;;; A headline names leaves (single changes) when it carries a
;;; '(diff ...)' fact or is one of the total diff view's properties.
;;; A vognode or phantom names a leaf on each axis its '(diff ...)'
;;; signs: 'node' (∃, its existence) and 'rel' (~, its relationship
;;; to its viewparent). A property names one leaf, and its request
;;; names no axis. A folder drawn because of a change, and a changed
;;; node without a '(diff ...)' fact, are aggregates.
;;;
;;; DATA USED/ASSUMED: See docs/api-and-formats.org.

(require 'org)
(require 'seq)
(require 'skg-metadata)
(require 'skg-request-save)

(defconst skg--readiness-vognode-kinds '(node deleted unknown historical)
  "The metadata forms that hold a vognode's or phantom's change facts.")

(defconst skg--readiness-property-heads '(oldText homeMove relRepoMove)
  "The heads of the properties that name one leaf without a diff fact.")

(defconst skg--readiness-wide-unmark-question
  "Unmarking this node commit-ready will unmark every contributing decision. You might discard a lot of hard-won information. Continue? "
  "Asked before a gesture unmarks more than one leaf.
`yes-or-no-p' appends the \"(yes or no)\".")

(defun skg-cycle-commit-ready (&optional recursive)
  "Toggle the commit-readiness of the change at point, then save.
On a headline that names one leaf, toggle it; on a content member
that names two (∃ and ~), ask which. On a changed folder, toggle
each leaf one level down; on a changed node, every leaf it owns.
With a prefix argument RECURSIVE, mark (or, if the headline at point
is ready, unmark) every leaf named in the subtree at point. Before
unmarking more than one leaf, ask. Works on write-protected
occurrences too."
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

(defun skg-mark-gnode-skgrepo-changes-commitReady (&optional recursive)
  "Toggle the commit-readiness of the home-skgrepo move of the node at
point, then save: unmark it if it is ready, else mark it. With a
prefix argument RECURSIVE, do the same to the move of every
descendant that moved between the same two skgrepos. Before
unmarking more than one move, ask. Point may be on the node or on
its move. Works on write-protected occurrences too."
  (interactive "P")
  (unless (org-at-heading-p)
    (user-error "Not on a headline"))
  (let* ((move (or (skg--readiness-home-move-at-point)
                   (user-error "This node's home skgrepo did not change")))
         (ready (save-excursion
                  (goto-char (car move))
                  (not (skg--readiness-ready-p
                        (skg--current-headline-metadata-sexp)))))
         (requests
          (save-excursion
            (goto-char (car move))
            (if (not recursive)
                (list (skg--readiness-request-here ready '(single)))
              (org-up-heading-safe)
              (skg--readiness-walk-subtree
               (lambda (metadata-sexp)
                 (when (equal (skg--readiness-home-move metadata-sexp)
                              (cdr move))
                   (skg--readiness-request-here ready '(single)))))))))
    (when (skg--readiness-write-requests-if-confirmed requests)
      (if ready
          (message "Marked %d home move(s) ready." (length requests))
        (message "Unmarked %d home move(s)." (length requests)))
      (skg-request-save-buffer))))

;;; Reading a headline's change facts

(defun skg--readiness-host (metadata-sexp)
  "The list in METADATA-SEXP that holds its change facts: the kind
form of a vognode or phantom, else the whole sexp."
  (or (seq-find (lambda (element)
                  (and (consp element)
                       (memq (car element) skg--readiness-vognode-kinds)))
                (cdr metadata-sexp))
      metadata-sexp))

(defun skg--readiness-vognode-p (metadata-sexp)
  "Non-nil when METADATA-SEXP is a vognode's or a phantom's."
  (not (eq (skg--readiness-host metadata-sexp) metadata-sexp)))

(defun skg--readiness-form (metadata-sexp key)
  "The (KEY ...) form among METADATA-SEXP's change facts, or nil."
  (seq-find (lambda (element) (eq (car-safe element) key))
            (cdr (skg--readiness-host metadata-sexp))))

(defun skg--readiness-leaf-axes (metadata-sexp)
  "The axes on which the headline with METADATA-SEXP names a leaf:
a sublist of (node rel) for a vognode or phantom, (single) for a
property that names one, nil when it names none."
  (let ((diff (cdr (skg--readiness-form metadata-sexp 'diff))))
    (if (skg--readiness-vognode-p metadata-sexp)
        (seq-filter
         (lambda (axis)
           (seq-some (lambda (atom)
                       (string-prefix-p (symbol-name axis) (symbol-name atom)))
                     diff))
         '(node rel))
      (when (or diff
                (seq-some (lambda (element)
                            (memq (car-safe element)
                                  skg--readiness-property-heads))
                          (cdr metadata-sexp))
                (memq (cadr (assq 'changesetMarker (cdr metadata-sexp)))
                      '(content relRepo)))
        '(single)))))

(defun skg--readiness-axes-ready-p (metadata-sexp axes)
  "Non-nil when every one of AXES is ready on METADATA-SEXP's headline."
  (let ((ready (skg--readiness-form metadata-sexp 'ready)))
    (and ready
         (seq-every-p (lambda (axis)
                        (or (eq axis 'single) (memq axis (cdr ready))))
                      axes))))

(defun skg--readiness-changed-folder-p (metadata-sexp)
  "Non-nil when METADATA-SEXP is a folder drawn because of a change."
  (seq-some (lambda (element)
              (and (consp element)
                   (string-suffix-p "Folder" (symbol-name (car element)))
                   (memq 'changed (cdr element))))
            (cdr metadata-sexp)))

(defun skg--readiness-ready-p (metadata-sexp)
  "Non-nil when the headline with METADATA-SEXP is ready: a leaf on
all its axes, or a folder that carries (ready). Nil for an aggregate
node, whose readiness is not drawn."
  (let ((axes (skg--readiness-leaf-axes metadata-sexp)))
    (cond (axes (skg--readiness-axes-ready-p metadata-sexp axes))
          ((skg--readiness-changed-folder-p metadata-sexp)
           (skg--readiness-form metadata-sexp 'ready)))))

(defun skg--readiness-home-move (metadata-sexp)
  "The (OLD NEW) skgrepos of METADATA-SEXP when it is a home move."
  (let ((move (assq 'homeMove (cdr metadata-sexp))))
    (when move
      (list (cadr (assq 'old (cdr move)))
            (cadr (assq 'new (cdr move)))))))

(defun skg--readiness-home-move-at-point ()
  "(POSITION . (OLD NEW)) of the home move at point: the headline at
point, or the child of the node at point that is its home move."
  (let* ((metadata-sexp (skg--metadata-sexp-at-point-or-nil))
         (pair (and metadata-sexp (skg--readiness-home-move metadata-sexp))))
    (if pair
        (cons (line-beginning-position) pair)
      (save-excursion
        (let ((found nil))
          (skg--readiness-do-children
           (lambda (child-sexp)
             (let ((pair (skg--readiness-home-move child-sexp)))
               (when (and pair (not found))
                 (setq found (cons (line-beginning-position) pair))))))
          found)))))

;;; Walking headlines

(defun skg--readiness-do-children (fn)
  "Call FN, with point on each, on the metadata of the headlines one
level below the headline at point. Returns the non-nil results, in
buffer order."
  (let ((level (org-outline-level))
        (results '()))
    (outline-next-heading)
    (while (and (not (eobp)) (> (org-outline-level) level))
      (let* ((metadata-sexp (skg--metadata-sexp-at-point-or-nil))
             (result (and metadata-sexp (= (org-outline-level) (1+ level))
                          (save-excursion (funcall fn metadata-sexp)))))
        (when result
          (push result results)))
      (outline-next-heading))
    (nreverse results)))

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
;;; A gesture first gathers its requests, each '(MARKER READY AXES)'
;;; for the headline at MARKER, then writes them all or none.

(defun skg--readiness-requests-at-point ()
  "The requests of the one-level gesture on the headline at point."
  (let* ((metadata-sexp (skg--current-headline-metadata-sexp))
         (axes (skg--readiness-leaf-axes metadata-sexp)))
    (cond
     (axes
      (let ((chosen (if (cdr axes)
                        (list (skg--readiness-ask-axis))
                      axes)))
        (list (skg--readiness-request-here
               (not (skg--readiness-axes-ready-p metadata-sexp chosen))
               chosen))))
     ((skg--readiness-changed-folder-p metadata-sexp)
      (let ((ready (not (skg--readiness-ready-p metadata-sexp))))
        (skg--readiness-do-children
         (lambda (child-sexp)
           (let ((child-axes (skg--readiness-leaf-axes child-sexp)))
             (when child-axes
               (skg--readiness-request-here ready child-axes)))))))
     ((skg--readiness-vognode-p metadata-sexp)
      (list (skg--readiness-request-here
             (not (skg--readiness-children-ready-p)) '(all)))))))

(defun skg--readiness-requests-for-subtree ()
  "The requests of the recursive gesture on the headline at point: one
on every leaf named in the subtree, readying them unless the headline
at point is ready. On a changed node, also one naming every leaf it
owns."
  (let* ((metadata-sexp (skg--current-headline-metadata-sexp))
         (aggregate-node (and (skg--readiness-vognode-p metadata-sexp)
                              (not (skg--readiness-leaf-axes metadata-sexp))))
         (ready (not (if aggregate-node
                         (save-excursion (skg--readiness-children-ready-p))
                       (skg--readiness-ready-p metadata-sexp)))))
    (append
     (when aggregate-node
       (list (skg--readiness-request-here ready '(all))))
     (skg--readiness-walk-subtree
      (lambda (headline-sexp)
        (let ((axes (skg--readiness-leaf-axes headline-sexp)))
          (when axes
            (skg--readiness-request-here ready axes))))))))

(defun skg--readiness-request-here (ready axes)
  "A request that marks (READY non-nil) or unmarks the leaves on AXES
of the headline at point."
  (list (copy-marker (line-beginning-position)) ready axes))

(defun skg--readiness-unmarks-several-leaves-p (requests)
  "Non-nil when the unmark requests among REQUESTS name more than one
leaf. A request naming `all' names every leaf its node owns."
  (let ((unmarks (seq-remove #'cadr requests)))
    (or (seq-some (lambda (request) (memq 'all (nth 2 request)))
                  unmarks)
        (> (apply #'+ (mapcar (lambda (request) (length (nth 2 request)))
                              unmarks))
           1))))

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

(defun skg--readiness-children-ready-p ()
  "Non-nil when every child of the headline at point that names a
leaf, or is a changed folder, is ready, and at least one does."
  (let ((some nil)
        (all t))
    (save-excursion
      (skg--readiness-do-children
       (lambda (child-sexp)
         (when (or (skg--readiness-leaf-axes child-sexp)
                   (skg--readiness-changed-folder-p child-sexp))
           (setq some t)
           (unless (skg--readiness-ready-p child-sexp)
             (setq all nil))))))
    (and some all)))

(defun skg--readiness-ask-axis ()
  "Ask which of a content member's two changes to toggle."
  (intern (completing-read
           "Which change (node = ∃ its existence, rel = ~ its relationship here)? "
           '("node" "rel") nil t)))

(defun skg--write-readiness-request (ready axes)
  "Replace the readiness request of the headline at point with one
that marks (READY non-nil) or unmarks the leaves on AXES."
  (let* ((metadata-sexp (skg--current-headline-metadata-sexp))
         (host (skg--readiness-host metadata-sexp))
         (request (cons (if ready 'readyRequest 'unreadyRequest)
                        (remq 'single axes)))
         (edits `((DELETE (readyRequest) (unreadyRequest)) ,request)))
    (skg-edit-metadata-at-point
     (if (eq host metadata-sexp)
         `(skg ,@edits)
       `(skg (,(car host) ,@edits))))))

(provide 'skg-readiness)
