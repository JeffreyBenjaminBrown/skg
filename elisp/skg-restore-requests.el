;;; skg-restore-requests.el --- Ask a save to restore what was removed  -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;;
;;; In the diff view, ask the next save to bring back what a change
;;; removed ('docs/diff-view.org', "Restoring what a change removed"):
;;; 'skg-restore-deleted-node' (C-c s n) writes back the deleted node
;;; at point (a ghost), and 'skg-restore-removed-membership' (C-c s l)
;;; re-adds the removed relationship at point; neither saves.
;;; 'skg--restoreRel-position-at-point-p' says where a restoreRel may
;;; stand, as the save checks it, and 'skg--removed-membership-p'
;;; whether a headline carries a removed membership, so that the
;;; metadata editor's cycle ('skg-sexpr-edit/skg-sexpr-cycling.el')
;;; offers restoreRel. Writing the request into the metadata is
;;; 'skg-metadata.el's ('skg--write-edit-request'). Its Neovim twin is
;;; 'nvim/lua/skg/restore_requests.lua'.
;;;
;;; The server refuses the requests on the wrong kind of headline with
;;; the same texts as the refusals below, except that its refusal of a
;;; restoreRel without a removed membership goes on to say where a
;;; restoreRel belongs.
;;;
;;; Code:

(require 'org)
(require 'skg-metadata)

(defconst skg--restore-rel-on-a-deleted-node
  "This node is deleted; restore it first (skg-restore-deleted-node)"
  "Why a removed membership cannot be restored on a ghost.")

(defconst skg--restore-node-on-a-node-not-deleted
  "Only a node deleted since HEAD can be restored; restoreNode belongs on a ghost (skg-restore-deleted-node)"
  "Why `skg-restore-deleted-node' refuses anything but a ghost.")

(defconst skg--restore-rel-without-a-removed-membership
  "Only a removed membership can be restored"
  "Why `skg-restore-removed-membership' refuses a headline.")

(defun skg-restore-deleted-node ()
  "Ask the next save to write back the deleted node at point.
The headline must be a ghost: a node of the diff view deleted since
HEAD. The save writes its file as HEAD had it, and
re-adds it to the lists of the nodes that recorded it at HEAD.
Does not save; call `skg-request-save-buffer' afterward."
  (interactive)
  (unless (skg--ghost-sexp-p (skg--metadata-sexp-at-point-or-nil))
    (user-error "%s" skg--restore-node-on-a-node-not-deleted))
  (skg--write-edit-request 'restoreNode 'ghost)
  (message "Restore request set. Save to apply."))

(defun skg-restore-removed-membership ()
  "Ask the next save to re-add the removed relationship at point.
The headline must be a removed member, or a member of a partner
folder carrying '(rel removed)': its relationship to its viewparent
(in a folder, to the folder's viewparent) existed at HEAD and is gone.
Which of the two it is, and whether it is either, depends on where it
stands (`skg--restoreRel-position-at-point-p').
The save re-adds it as HEAD recorded it.
Does not save; call `skg-request-save-buffer' afterward."
  (interactive)
  (let ((metadata (skg--metadata-sexp-at-point-or-nil)))
    (cond ((skg--ghost-sexp-p metadata)
           (user-error "%s" skg--restore-rel-on-a-deleted-node))
          ((not (and (skg--removed-membership-p metadata)
                     (skg--restoreRel-position-at-point-p)))
           (user-error "%s" skg--restore-rel-without-a-removed-membership)))
    (skg--write-edit-request 'restoreRel 'node)
    (message "Restore request set. Save to apply.")))

(defun skg--removed-membership-p (metadata-sexp)
  "Non-nil when METADATA-SEXP is an occurrence whose
'(diff ...)' says its relationship was removed. Whether a restoreRel
may sit on it depends also on where it stands
(`skg--restoreRel-position-at-point-p')."
  (and (skg--occurrence-sexp-p metadata-sexp)
       (skg--relationship-removed-p metadata-sexp)))

(defun skg--restoreRel-position-at-point-p ()
  "Non-nil iff the headline at point stands where a restoreRel may:
under a partner folder (whose removed members claim nothing), or as
a removed member, a child claiming nothing under a vognode that is no node of the
containerward chain. The save checks the same
('restoreRel_stands_where_it_may', server/from_text/
buffer_to_viewnodes/local.rs)."
  (let ((metadata (skg--metadata-sexp-at-point-or-nil))
        (parent-sexp (save-excursion
                       (and (org-up-heading-safe)
                            (skg--metadata-sexp-at-point-or-nil)))))
    (cond ((skg--partner-folder-atom parent-sexp) t)
          ((skg--vognode-sexp-p parent-sexp)
           (and (not (skg--claims-membership-p metadata))
                (not (skg--containerward-chain-node-at-point-p))))
          (t nil))))

(defun skg--containerward-chain-node-at-point-p ()
  "Non-nil iff the headline at point is a node of the containerward
chain as the diff view, whose request restoreRel is, draws it: a
vognode claiming nothing under a member of a partner folder whose
members carry the chain there (every one but the two acquisition
folders and the illegal-edit folder),
or under another such node ('stands_in_containerward_chain',
server/types/tree/forest.rs)."
  (save-excursion
    (catch 'answer
      (while t
        (let ((metadata (skg--metadata-sexp-at-point-or-nil)))
          (unless (and (skg--vognode-sexp-p metadata)
                       (or (not (skg--occurrence-sexp-p metadata))
                           (not (skg--claims-membership-p metadata))))
            (throw 'answer nil))
          (unless (and (org-up-heading-safe)
                       (skg--vognode-sexp-p
                        (skg--metadata-sexp-at-point-or-nil)))
            (throw 'answer nil))
          (let ((grandparent-folder
                 (save-excursion
                   (and (org-up-heading-safe)
                        (skg--partner-folder-atom
                         (skg--metadata-sexp-at-point-or-nil))))))
            (when (and grandparent-folder
                       (not (memq grandparent-folder
                                  '(acquiredByFolder acquiredFolder
                                    illegalEditFolder))))
              (throw 'answer t))))))))

(provide 'skg-restore-requests)
