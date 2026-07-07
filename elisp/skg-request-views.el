;;; -*- lexical-binding: t; -*-
;;;
;;; Commands that request additional views by adding a (viewRequests ...)
;;; atom to the headline at point and saving, letting Rust fulfill the
;;; request during completion. Two families, both auto-saving (Q10):
;;;   - COLLECTIONS, `skg-show-collection-*' : (col RELNAME), builds
;;;     BOTH cols of the relation;
;;;   - PATHS, `skg-show-paths-through-*' : (path ROLENAME), the
;;;     backpath for that one partner role.
;;; (`skg-request-definitive-view', a different concept, is also here.)

(require 'skg-metadata)
(require 'skg-request-save)
(require 'org)
(require 'org-element)

(defun skg--request-view-and-save (view-request)
  "Request VIEW-REQUEST for the headline at point, then save.
VIEW-REQUEST is a request form -- (col RELNAME), (path ROLENAME),
or the bare symbol definitiveView -- spliced into a (viewRequests ...)
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
  ;; Collections ('C-c c'): both cols of the relation.
  (skg-show-collection-aliases    (col aliases)
    "Show the aliases collection for the headline at point.")
  (skg-show-collection-overrides  (col overrides)
    "Show the override collections (overriddenCol + overriderCol).")
  (skg-show-collection-hides      (col hides)
    "Show the hide collections (hiderCol + hiddenCol).")
  (skg-show-collection-subscribes (col subscribes)
    "Show the subscription collections (subscribeeCol + subscriberCol).")
  ;; Paths ('C-c p'): the backpath for one partner role. UPPER = the
  ;; partner's active (first) role, lower = its passive (second) role.
  (skg-show-paths-through-containers   (path container)
    "Show paths through the containers of the node (nodes that contain it).")
  (skg-show-paths-through-link-sources (path linkSource)
    "Show paths through the link sources of the node (nodes that link to it).")
  (skg-show-paths-through-link-dests   (path linkDest)
    "Show paths through the link dests of the node (nodes it links to).")
  (skg-show-paths-through-overriders   (path overrider)
    "Show paths through the overriders of the node (nodes that override it).")
  (skg-show-paths-through-overridden   (path overridden)
    "Show paths through the nodes the node overrides.")
  (skg-show-paths-through-hiders       (path hider)
    "Show paths through the hiders of the node (nodes that hide it).")
  (skg-show-paths-through-hidden       (path hidden)
    "Show paths through the nodes the node hides.")
  (skg-show-paths-through-subscribers  (path subscriber)
    "Show paths through the subscribers of the node (nodes that subscribe to it).")
  (skg-show-paths-through-subscribees  (path subscribee)
    "Show paths through the nodes the node subscribes to."))

(defun skg-request-definitive-view ()
  "Edit metadata to request a definitive view for the headline at point.
The node must be indefinitive and childless. Does NOT auto-save."
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (skg-edit-metadata-at-point
     `(skg (node (viewRequests definitiveView))))))

(defun skg-fork-node ()
  "Fork the (owned) node at point: create a private clone that overrides it.
This is the explicit counterpart to the implicit foreign fork: it forks a
node you ALREADY own, deepening an override chain (e.g. E overrides D
overrides C overrides N).

Refuses if the buffer has unsaved changes (\"Save the buffer before
forking.\"), so the clone's saved snapshot matches what you see. Otherwise
stamps (viewRequests fork) into the headline's own (skg (node ...)) --
targeting the headline's OWN id, never an (overridesHere N) marker it may
carry -- and auto-saves (unlike `skg-request-definitive-view'). The server
returns the usual fork-confirmation buffer; Emacs prompts for the clone's
source (unless already specified), then approve with C-c C-c or decline
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
