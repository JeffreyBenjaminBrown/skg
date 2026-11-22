;;; skg-request-views.el --- View requests: folders, editability, forks  -*- lexical-binding: t; -*-
;;;
;;; Commentary:
;;;
;;; Commands that ask the server to show more of the headline at
;;; point, or to show it differently, by adding a (viewRequests ...)
;;; atom to its metadata, which the next save fulfills. The folder
;;; commands auto-save ('skg--request-view-and-save'):
;;; 'skg-show-folderOf-aliases' (C-c l a), 'skg-show-folderOf-hides'
;;; (C-c l h), 'skg-show-folderOf-forks' (C-c l f),
;;; 'skg-show-folderOf-flags' (C-c l p),
;;; 'skg-show-folderOf-containers' (C-c l c) and
;;; 'skg-show-folderOf-mentioners' (C-c l m). (folder RELNAME)
;;; builds both folders of the relation (one for containers,
;;; mentioners and forks; aliases and flags are no relation and build
;;; one). A containers folder carries, under each member, that
;;; member's containerward chain: its containers, theirs under them,
;;; up to the roots. See docs/COMMANDS.org, "C-c l".
;;;
;;; The editable-view and write-protect requests do not auto-save:
;;; 'skg-request-editable-view' (C-c s e, alias 'skg-set-editable'),
;;; 'skg-request-write-protected' (C-c s w) and
;;; 'skg-request-write-protected-recursive' (C-c s W). The explicit
;;; fork, 'skg-fork-node' (C-c m f, alias 'skg-fork'), auto-saves.
;;; Writing the atom is 'skg-metadata.el's
;;; ('skg-edit-metadata-at-point'); sending the save is
;;; 'skg-request-save.el's ('skg-request-save-buffer').
;;;
;;; Code:

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
     `(skg node (viewRequests ,view-request))))
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
  ;; Folders ('C-c l'): the relation's folders.
  (skg-show-folderOf-aliases       (folder aliases)
    "Show the aliases folder for the headline at point.")
  (skg-show-folderOf-hides (folder hides)
    "Show the hide folders (hiderFolder + hiddenFolder).")
  (skg-show-folderOf-forks         (folder forks)
    "Show the forksFolder: the notes this one forks, then those that fork it.")
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
     `(skg node (viewRequests editableView)))))

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
     `(skg node (viewRequests writeProtectedView)))))

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
     `(skg node (viewRequests writeProtectedSubtreeView)))))

(defun skg-fork-node ()
  "Fork the (owned) node at point: create a private clone that forks it.
This is the explicit counterpart to the implicit foreign fork: it forks a
node you already own, deepening a chain of forks (e.g. E forks D forks C
forks N).

Refuses if the buffer has unsaved changes (\"Save the buffer before
forking.\"), so the clone's saved state matches what you see. Otherwise
stamps (viewRequests fork) into the headline's own (skg node ...) --
targeting the headline's own id, never an (inPlaceOf N) fact it may
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
       `(skg node (viewRequests fork))))
    (skg-request-save-buffer)))

(provide 'skg-request-views)
