;;; -*- lexical-binding: t; -*-
;;;
;;; Commands that request viewbranches by adding a (viewRequests ...)
;;; atom to the headline at point and saving, letting Rust fulfill the
;;; request during completion. Two families, both auto-saving (Q10):
;;;   - folders, `skg-show-folder-*' : (folder RELNAME), builds
;;;     both folders of the relation (one for containers and mentioners);
;;;   - role trees, `skg-show-*ward-tree' : (roleTree ROLENAME), the
;;;     role tree for that one partner role.
;;; (`skg-request-editable-view' and the write-protect requests, a
;;; different concept, are also here.)

(require 'skg-metadata)
(require 'skg-request-save)
(require 'org)
(require 'org-element)

(defun skg--request-view-and-save (view-request)
  "Request VIEW-REQUEST for the headline at point, then save.
VIEW-REQUEST is a request form -- (folder RELNAME), (roleTree ROLENAME),
or the bare symbol editableView -- spliced into a (viewRequests ...)
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
    "Show the write-protected containerFolder: the nodes that contain this one.")
  (skg-show-folderOf-mentioners    (folder mentioners)
    "Show the write-protected mentionerFolder: the nodes that link to this one.")
  ;; Role trees ('C-c p'): the role tree for one partner role. UPPER = the
  ;; partner's active (first) role, lower = its passive (second) role.
  (skg-show-containerward-tree   (roleTree container)
    "Show the role tree through the containers of the node (nodes that contain it).")
  (skg-show-mentionerward-tree (roleTree mentioner)
    "Show the role tree through the mentioners of the node (nodes that link to it).")
  (skg-show-mentionedward-tree   (roleTree mentioned)
    "Show the role tree through the nodes the node mentions (links to).")
  (skg-show-overriderward-tree   (roleTree overrider)
    "Show the role tree through the overriders of the node (nodes that override it).")
  (skg-show-overriddenward-tree   (roleTree overridden)
    "Show the role tree through the nodes the node overrides.")
  (skg-show-hiderward-tree       (roleTree hider)
    "Show the role tree through the hiders of the node (nodes that hide it).")
  (skg-show-hiddenward-tree       (roleTree hidden)
    "Show the role tree through the nodes the node hides.")
  (skg-show-subscriberward-tree  (roleTree subscriber)
    "Show the role tree through the subscribers of the node (nodes that subscribe to it).")
  (skg-show-subscribeeward-tree  (roleTree subscribee)
    "Show the role tree through the nodes the node subscribes to."))

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

(provide 'skg-request-views)
