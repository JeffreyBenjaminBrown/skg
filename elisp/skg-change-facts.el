;;; skg-change-facts.el --- Read a diff-view headline's change facts  -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;;
;;; Say what the diff view shows changed at a headline, and whether
;;; the user marked that change processed, by reading its change
;;; facts, the '(diff ...)' part of its metadata: what changed
;;; ('skg--processed-form', 'skg--processed-leaves') and whether it is
;;; processed ('skg--objects-processed-p'); and whether a folder is
;;; changed, by its members' relationships, and processed
;;; ('skg--folder-change-processed',
;;; 'skg--folder-change-processed-at-point'). The heralds
;;; ('heralds-minor-mode.el'), the herald descriptions
;;; ('skg-request-describe-heralds.el') and the processed gestures
;;; ('skg-processed.el') read them. The gestures themselves are
;;; 'skg-processed.el's; writing a processed request is
;;; 'skg-metadata.el's ('skg--metadata-with-processed-request'). Its
;;; Neovim twin is 'nvim/lua/skg/change_facts.lua'.
;;;
;;; Code:

(require 'seq)
(require 'org)
(require 'skg-metadata)

;;; A headline's change facts

(defun skg--processed-diff-form (metadata-sexp)
  "The '(diff ...)' form of METADATA-SEXP, or nil. Every kind's
'(diff ...)' stands at the top level of '(skg ...)', beside its kind
atom."
  (assq 'diff (cdr metadata-sexp)))

(defun skg--processed-form (metadata-sexp key)
  "The (KEY ...) part of METADATA-SEXP's '(diff ...)' form, or nil."
  (assq key (cdr (skg--processed-diff-form metadata-sexp))))

(defun skg--processed-leaves (metadata-sexp)
  "The '(leaves N OBJECT...)' fact of the headline with METADATA-SEXP,
as (N . OBJECTS), or nil when it names no leaf."
  (cdr (skg--processed-form metadata-sexp 'leaves)))

(defun skg--objects-processed-p (metadata-sexp objects)
  "Non-nil when every one of OBJECTS is processed on METADATA-SEXP's headline:
when it carries a '(processed ...)' that names each of them. A headline
that lists no object (one naming a single leaf) is processed when it
carries any '(processed ...)'."
  (let ((processed (skg--processed-form metadata-sexp 'processed)))
    (and processed
         (seq-every-p (lambda (object) (memq object (cdr processed)))
                      objects))))

;;; A changed folder
;;; The server writes every folder as its bare atom. Whether the diff
;;; view shows it because of a change, and whether that change is
;;; processed, follows from its members (its child headlines), as computed
;;; here ('folder_change_facts' did it on the server before).


(defun skg--membership-change-processed (member-sexp child-sexps)
  "Whether the folder member with metadata MEMBER-SEXP, whose child
headlines carry CHILD-SEXPS, carries a change of its relationship to
the folder's viewparent, and whether that change is processed: nil when
it carries none, else `processed' or `unprocessed'. It carries one when its
'(diff ...)' has a '(rel ...)', or when a child is a
'(relRepoMove ...)'. The change is processed when both hold: a '(rel ...)'
comes with a '(processed ...)' that names 'rel' if the member's
'(leaves ...)' lists 'rel'; every '(relRepoMove ...)' child carries
'(diff (processed ...))'."
  (let ((rel-change (skg--processed-form member-sexp 'rel))
        (relRepo-moves
         (seq-filter (lambda (child-sexp)
                       (assq 'relRepoMove (cdr child-sexp)))
                     child-sexps)))
    (when (or rel-change relRepo-moves)
      (if (and (or (not rel-change)
                   (skg--objects-processed-p
                    member-sexp
                    (and (memq 'rel (cdr (skg--processed-leaves member-sexp)))
                         '(rel))))
               (seq-every-p (lambda (relRepo-move)
                              (skg--processed-form relRepo-move 'processed))
                            relRepo-moves))
          'processed
        'unprocessed))))

(defun skg--folder-change-processed (members)
  "Whether a folder whose MEMBERS are as given is changed, and whether
that change is processed: nil when no member carries a change of its
relationship to the folder's viewparent, `processed' when every such
change is processed, else `unprocessed'. Each of MEMBERS is (MEMBER-SEXP
. CHILD-SEXPS), as `skg--membership-change-processed' takes them."
  (let ((changes (delq nil (mapcar (lambda (member)
                                     (skg--membership-change-processed
                                      (car member) (cdr member)))
                                   members))))
    (when changes
      (if (memq 'unprocessed changes) 'unprocessed 'processed))))

(defun skg--folder-change-processed-at-point ()
  "`skg--folder-change-processed' of the folder headline on the line
at point, or nil when that line is no folder's headline. Reads
headline levels by their stars, so it works in any buffer."
  (save-excursion
    (beginning-of-line)
    (let ((level (skg--headline-level-on-line)))
      (when (and level
                 (skg--folder-atom (skg--metadata-sexp-at-point-or-nil)))
        (skg--folder-change-processed
         (skg--folder-members-below level))))))

(defun skg--folder-members-below (folder-level)
  "The members of the folder whose headline, at FOLDER-LEVEL, is on the
line at point: for each headline one level below it, (MEMBER-SEXP
. CHILD-SEXPS), the metadata of the member and of its own child
headlines, nil where a headline carries none. Moves point."
  (let ((members '())
        (done nil))
    (while (and (not done) (zerop (forward-line 1)) (not (eobp)))
      (let ((level (skg--headline-level-on-line)))
        (cond ((null level))
              ((<= level folder-level) (setq done t))
              ((= level (1+ folder-level))
               (push (list (skg--metadata-sexp-at-point-or-nil)) members))
              ((and members (= level (+ 2 folder-level)))
               (setcdr (car members)
                       (append (cdar members)
                               (list (skg--metadata-sexp-at-point-or-nil))))))))
    (nreverse members)))

(provide 'skg-change-facts)
