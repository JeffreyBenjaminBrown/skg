;;; -*- lexical-binding: t; -*-
;;;
;;; Commands that request viewbranches by adding a (viewRequests ...)
;;; atom to the headline at point and saving, letting Rust fulfill the
;;; request during completion. The folder commands,
;;; `skg-show-folderOf-*', auto-save: (folder RELNAME) builds both
;;; folders of the relation (one for containers and mentioners). A
;;; containers folder carries, under each member, that member's
;;; containerward chain: its containers, theirs under them, up to the
;;; roots.
;;; (`skg-request-editable-view' and the write-protect requests, a
;;; different concept, are also here.)

(require 'skg-metadata)
(require 'skg-request-save)
(require 'org)
(require 'org-element)

(defun skg--request-view-and-save (view-request)
  "Request VIEW-REQUEST for the headline at point, then save.
VIEW-REQUEST is a request form -- (folder RELNAME), or a bare symbol
such as editableView -- spliced into a (viewRequests ...)
atom via `skg-edit-metadata-at-point'."
  (save-excursion
    (org-back-to-heading t)
    (skg-edit-metadata-at-point
     `(skg (node (viewRequests ,view-request)))))
  (skg-request-save-buffer))

(defmacro skg--define-view-request-commands (&rest rows)
  "Define a command per ROW. Each ROW is (NAME REQUEST-FORM DOCSTRING):
an interactive command NAME that requests REQUEST-FORM and auto-saves."
  `(progn
     ,@(mapcar
        (lambda (row)
          (let ((name (nth 0 row))
                (form (nth 1 row))
                (doc  (nth 2 row)))
            `(defun ,name ()
               ,doc
               (interactive)
               (skg--request-view-and-save ',form))))
        rows)))

(skg--define-view-request-commands
  ;; Folders ('C-c l'): both folders of the relation.
  (skg-show-folderOf-aliases       (folder aliases)
    "Show the aliases folder for the headline at point.")
  (skg-show-folderOf-overrides     (folder overrides)
    "Show the override folders (overriddenFolder + overriderFolder).")
  (skg-show-folderOf-hidesFromSubs (folder hidesFromSubs)
    "Show the hide folders (hiderFolder + hiddenFolder).")
  (skg-show-folderOf-subscribesTo  (folder subscribesTo)
    "Show the subscription folders (subscribeeFolder + subscriberFolder).")
  (skg-show-folderOf-flags flags
    "Show the write-protected flags folder for the node at point.")
  (skg-show-folderOf-containers    (folder containers)
    "Show the write-protected containerFolder: the nodes that contain this one.
Under each member hangs its containerward chain: its containers, theirs
under them, up to the roots.")
  (skg-show-folderOf-mentioners    (folder mentioners)
    "Show the write-protected mentionerFolder: the nodes that link to this one."))

(defun skg-request-editable-view ()
  "Edit metadata to request a editable view for the headline at point.
On a write-protected headline, the save makes it the node's editable
occurrence and write-protects the one that was, which keeps what it
shows. On an editable headline, the save extends its content editable,
up to the server's initial_node_limit extensions. A buffer may carry
one such request per node. Does not auto-save."
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (skg-edit-metadata-at-point
     `(skg (node (viewRequests editableView))))))

(defun skg-request-write-protected ()
  "Edit metadata to request write-protection for the headline at point.
At the next save, the occurrence's own edits are saved first; then the
server makes it write-protected, resyncing its title from the graph,
and makes each of its direct children not affect it, keeping their own
editability and subtrees. A no-op on a write-protected headline.
Requesting an editable view (\\[skg-request-editable-view]) of another
occurrence of the node in the same save moves editability there.
Does not auto-save."
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (skg-edit-metadata-at-point
     `(skg (node (viewRequests writeProtectedView))))))

(defun skg-request-write-protected-recursive ()
  "Edit metadata to request write-protection for the headline at point
and its whole content subtree. At the next save, after the subtree's
own edits are saved, the server makes the occurrence and every node of
its content subtree write-protected, each child under them not
affecting its parent. Does not auto-save."
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (skg-edit-metadata-at-point
     `(skg (node (viewRequests writeProtectedSubtreeView))))))

(defun skg-fork-node ()
  "Fork the (owned) node at point: create a private clone that overrides it.
This is the explicit counterpart to the implicit foreign fork: it forks a
node you already own, deepening an override chain (e.g. E overrides D
overrides C overrides N).

Refuses if the buffer has unsaved changes (\"Save the buffer before
forking.\"), so the clone's saved state matches what you see. Otherwise
stamps (viewRequests fork) into the headline's own (skg (node ...)) --
targeting the headline's own id, never an (overridesHere N) fact it may
carry -- and auto-saves (unlike `skg-request-editable-view'). The server
returns the usual fork-confirmation buffer; Emacs prompts for the clone's
repo (unless already specified), then approve with C-c C-c or decline
with C-c C-k."
  (interactive)
  (if (buffer-modified-p)
      (message "Save the buffer before forking.")
    (save-excursion
      (org-back-to-heading t)
      (skg-edit-metadata-at-point
       `(skg (node (viewRequests fork)))))
    (skg-request-save-buffer)))

;;; The diff view's ghosts ('docs/diff-view.org'). A ghost stands where
;;; the diff view drew it, for a node HEAD has and the worktree lacks,
;;; until the user entombstones it: these commands stamp the 'tombstone'
;;; view request into its ghost form and save. The save keeps what the
;;; user put under the ghost and drops what the diff view drew there; a
;;; childless tombstone is pruned. The server refuses the request
;;; anywhere but on a ghost, with the same text.

(defconst skg--tombstone-request-off-a-ghost
  "Only a ghost can be entombstoned; the tombstone view request belongs on a ghost (skg-request-ghost-tombstone)"
  "Why `skg-request-ghost-tombstone' refuses anything but a ghost.")

(defun skg-request-ghost-tombstone ()
  "Entombstone the ghost at point, then save.
Stamps '(viewRequests tombstone)' into the headline's ghost form and
saves. The headline must be a ghost: a node of the diff view deleted
since HEAD."
  (interactive)
  (save-excursion
    (unless (and (ignore-errors (org-back-to-heading t) t)
                 (skg--ghost-sexp-p (skg--metadata-sexp-at-point-or-nil)))
      (user-error "%s" skg--tombstone-request-off-a-ghost))
    (skg--stamp-ghost-tombstone-request))
  (skg-request-save-buffer))

(defun skg-request-buffer-ghosts-tombstone ()
  "Entombstone every ghost in the buffer, then save once.
Stamps '(viewRequests tombstone)' into each ghost headline's ghost
form. With no ghost in the buffer, says so and does not save."
  (interactive)
  (let (( ghost-count (save-excursion
                        (skg--stamp-every-ghost-tombstone-request)) ))
    (if (zerop ghost-count)
        (message "No ghost in this buffer.")
      (message "Entombstoning %d ghost(s)." ghost-count)
      (skg-request-save-buffer))))

(defun skg--stamp-every-ghost-tombstone-request ()
  "Stamp the tombstone request on every ghost headline in the buffer.
Returns how many it stamped."
  (let (( ghost-count 0 ))
    (goto-char (point-min))
    (unless (org-at-heading-p)
      (outline-next-heading))
    (while (and (not (eobp))
                (org-at-heading-p))
      (when (skg--ghost-sexp-p (skg--metadata-sexp-at-point-or-nil))
        (skg--stamp-ghost-tombstone-request)
        (setq ghost-count (1+ ghost-count)))
      (outline-next-heading))
    ghost-count))

(defun skg--stamp-ghost-tombstone-request ()
  "Stamp '(viewRequests tombstone)' into the ghost form of the headline
at point."
  (skg-edit-metadata-at-point
   '(skg (ghost (viewRequests tombstone)))))

(provide 'skg-request-views)
