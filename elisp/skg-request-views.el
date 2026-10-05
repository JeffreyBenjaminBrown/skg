;;; -*- lexical-binding: t; -*-
;;;
;;; Commands that request viewbranches by adding a (viewRequests ...)
;;; atom to the headline at point and saving, letting Rust fulfill the
;;; request during completion. Two families, both auto-saving (Q10):
;;;   - FOLDERS, `skg-show-folder-*' : (folder RELNAME), builds
;;;     BOTH folders of the relation;
;;;   - ROLE TREES, `skg-show-*ward-tree' : (roleTree ROLENAME), the
;;;     role tree for that one partner role.
;;; (`skg-request-editable-view', a different concept, is also here.)

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
The node must be write-protected and childless. Does NOT auto-save."
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (skg-edit-metadata-at-point
     `(skg (node (viewRequests editableView))))))

(defun skg-fork-node ()
  "Fork the (owned) node at point: create a private clone that overrides it.
This is the explicit counterpart to the implicit foreign fork: it forks a
node you ALREADY own, deepening an override chain (e.g. E overrides D
overrides C overrides N).

Refuses if the buffer has unsaved changes (\"Save the buffer before
forking.\"), so the clone's saved state matches what you see. Otherwise
stamps (viewRequests fork) into the headline's own (skg (node ...)) --
targeting the headline's OWN id, never an (overridesHere N) marker it may
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
