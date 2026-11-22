;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: `skg-set-relRepo' -- request the relRepo
;;; of one relationship, informed by the server's
;;; 'relRepo info' endpoint
;;; ('docs/api-and-formats.org', "relRepo info") -- and, below it, the
;;; buffer-local helpers it drives and the recursive command
;;; `skg-set-relRepo-recursive'.

(require 'org)
(require 'seq)
(require 'skg-config)
(require 'skg-length-prefix)
(require 'skg-metadata)
(require 'skg-shared)

(defun skg-set-relRepo (&optional recursive)
  "Set the relRepo of the relationship or alias at point.

With a prefix argument RECURSIVE, instead run
`skg-set-relRepo-recursive', which prompts for a
relationship kind and a repo and applies the repo throughout the
subtree at point.

The headline at point represents one relationship: `contains' for a content
child, the folder's relation for a writable partner folder member. This
command asks the server for the relationship's default repo and its current
repo, then
prompts -- with both tab-completion and S-left/S-right cycling,
like the other repo dialogs -- over the repos at least as
private as the default (more public ones could leak an endpoint's
ID and would be rejected at save), plus a no-override choice. The
minibuffer starts pre-filled with a pending request when one is
offerable, else the current relRepo or default, so RET preserves the
most specific available choice.

Choosing a repo writes an `(editRequest (relRepo REPO))'
metadata request. The no-override choice removes that request, which on save means the relationship
keeps its saved repo (sticky), not that it resets to its default.
To lower an relationship's privacy to its default (e.g. after making the
more private endpoint's home more public), choose the default relRepo
itself; once saved at the default, the derived fact and its red ~herald
stop being rendered.

Refuses on write-protected folder members (the relationship belongs to the other
end) and on root headlines (no relationship). Like other metadata edits,
this only modifies the buffer; it does not save. Call
`skg-request-save-buffer' afterward. The server re-validates at
save time, so a stale or hand-typed repo more public than the
relationship's default is still rejected there."
  (interactive "P")
  (if recursive
      (skg-set-relRepo-recursive)
    (skg--set-relRepo-at-point)))

(defun skg--set-relRepo-at-point ()
  "The single-relationship path of `skg-set-relRepo': classify
the relationship at point, ask the server for its (default, current) repos,
and prompt from the reply."
  (let ((buffer (current-buffer))
        (marker (point-marker)))
    (if (skg--alias-headline-p)
        (let ((default
               (save-excursion
                 (unless (and (org-up-heading-safe)
                              (org-up-heading-safe))
                   (user-error "Alias has no owning node headline"))
                 (skg--current-node-repo)))
              (current (skg--relRepo-current-value)))
          (skg--set-relRepo-from-info
           buffer marker
           (format "((response-type relRepo-info) (default %S)%s)"
                   default
                   (if current
                       (format " (current %S)" current)
                     ""))))
      (let ((relationship (skg--rel-at-point)))
        (skg-register-response-handler
         'relRepo-info
         (lambda (_tcp-proc payload)
           (skg--set-relRepo-from-info buffer marker payload))
         t)
        (skg-lp-reset)
        (process-send-string
         (skg-tcp-connect-to-rust)
         (concat
          (prin1-to-string
           `((request . "relRepo info")
             (recorder . ,(plist-get relationship :recorder))
             (member . ,(plist-get relationship :member))
             (relation . ,(plist-get relationship :relation))))
          "\n"))))))

(defun skg--set-relRepo-from-info (buffer marker payload)
  "Handle the relRepo-info response for `skg-set-relRepo'.
Parses PAYLOAD, then prompts and applies the choice at MARKER in
BUFFER. The prompt runs from a zero-delay timer so the minibuffer
opens outside the network process filter."
  (let* ((response (read payload))
         (as-string (lambda (v) (and v (format "%s" v))))
         (err     (funcall as-string (cadr (assoc 'error response))))
         (default (funcall as-string (cadr (assoc 'default response))))
         (current (funcall as-string (cadr (assoc 'current response)))))
    (run-at-time
     0 nil
     (lambda ()
       (if (not (buffer-live-p buffer))
           (message "skg: buffer vanished before the relRepo prompt")
         (with-current-buffer buffer
           (save-excursion
             (goto-char marker)
             (when err
               (message "relRepo info: %s -- offering every repo; the save will validate."
                        err))
             (let* ((ladder (skg--repo-names))
                    (requested (skg--relRepo-requested-value))
                    (choices (append (skg--relRepo-choices
                                      ladder default)
                                     (list skg--relRepo-no-override)))
                    (prompt (concat "relRepo (S-left/right cycle"
                                    (when default
                                      (format "; default %s" default))
                                    (when current
                                      (format "; currently %s" current))
                                    "): "))
                    ;; Pre-fill so RET keeps the status quo and the
                    ;; cycle starts from it. A legacy current more
                    ;; public than the default is not among the choices;
                    ;; fall back to the default, then to empty.
                    (prefill (cond ((and requested (member requested choices))
                                    requested)
                                   ((and current (member current choices))
                                    current)
                                   ((and default (member default choices))
                                    default)))
                    (choice (skg--completing-read-with-cycle
                             prompt choices nil t prefill nil nil nil
                             choices)))
               (message "%s"
                        (skg--apply-relRepo-choice
                         choice))))))))))

(defconst skg--relRepo-unsupported-folder-atoms
  '(hiderFolder hiddenFolder
    containerFolder mentionerFolder acquiredByFolder acquiredFolder
    illegalEditFolder
    integratedElsewhereFolder
    hiddenInForkPartnerFolder hiddenByForkerFolder
    hiddenOutsideOfForkPartnersFolder)
  "The partner folder atoms where an explicit relRepo
request is unsupported from this side.  The membership of
hiddenOutsideOfForkPartnersFolder, a compound partner folder, is
editable, but hide repos are still derived and cannot carry this
request.
`skg-set-relRepo' refuses on a member of one of these:
the relationship belongs to the other end, so setting its repo here would
be meaningless.")

(defconst skg--writable-folder-relations
  '((forksFolder . "forks"))
  "The partner folder atoms whose members' relationships are writable
from this side, each mapped to its relation's wire name
(NodeRelation::relation_name, server/dbs/in_rust_graph/
relation_accessors.rs). The folder's viewparent owns the outbound
relationship to each member that claims; a member of the forks
folder claiming nothing either forks the viewparent, recording that
in its own file, or is a mirror of it only through others, recorded
nowhere between them, so no relRepo is set on it from here.")

(defun skg--rel-at-point ()
  "Classify the relationship the headline at point represents.
Returns a plist (:recorder RECORDER-ID :member MEMBER-ID :relation NAME):
for a content child, the viewparent contains the node at point; for
a writable-folder member, the folder's viewparent owns the folder's
relation toward the node at point. Signals `user-error'
when point represents no writable relationship: not on an occurrence,
an unknown viewnode or a restricted viewnode, on a root
headline (no viewparent, so no relationship), on a member of a write-protected folder, or with
an ID missing."
  (unless (org-at-heading-p)
    (user-error "Not on a headline"))
  (let ((member-sexp (skg--metadata-sexp-at-point-or-nil)))
    (unless (or (skg--occurrence-sexp-p member-sexp)
                (skg--gnid-only-vognode-sexp-p member-sexp))
      (user-error "Not on an occurrence, an unknown viewnode or a restricted viewnode"))
    (let ((member-gnid (skg--relationship-member-id member-sexp))
          (parent-sexp (save-excursion
                         (and (org-up-heading-safe)
                              (skg--metadata-sexp-at-point-or-nil)))))
      (unless member-gnid
        (user-error "No id in this headline's metadata"))
      (unless parent-sexp
        (user-error
         "Root headline: there is no relationship here to set"))
      (let ((write-protected-atom
             (and (consp parent-sexp)
                  (seq-find (lambda (atom)
                              (memq atom (cdr parent-sexp)))
                            skg--relRepo-unsupported-folder-atoms))))
        (when write-protected-atom
          (user-error
           "Cannot set the relationship's repo from this write-protected %s position"
           write-protected-atom)))
      (let ((writable-folder
             (and (consp parent-sexp)
                  (seq-find (lambda (entry)
                              (memq (car entry) (cdr parent-sexp)))
                            skg--writable-folder-relations))))
        (cond
         ((and writable-folder
               (not (skg--claims-membership-p member-sexp)))
          (user-error
           "This member of the %s claims nothing: either it forks the view-parent and records that in its own file, or it is a mirror of the view-parent only through others; attach it first to make the view-parent fork it"
           (car writable-folder)))
         (writable-folder
          (let* ((folder-viewparent-sexp
                  (save-excursion
                    (and (org-up-heading-safe) ;; to the folder
                         (org-up-heading-safe) ;; to its viewparent
                         (skg--metadata-sexp-at-point-or-nil))))
                 (folder-viewparent-gnid
                  (skg--node-id folder-viewparent-sexp)))
            (unless folder-viewparent-gnid
              (user-error "Could not find the folder's viewparent"))
            (when (skg--node-write-protected-p folder-viewparent-sexp)
              (user-error "The folder's viewparent is write-protected here, so the save would not write this relationship; set it from a view where the folder's viewparent is editable"))
            (list :recorder folder-viewparent-gnid
                  :member member-gnid
                  :relation (cdr writable-folder))))
         ((skg--occurrence-sexp-p parent-sexp)
          (let ((parent-gnid (skg--node-id parent-sexp)))
            (unless parent-gnid
              (user-error "No id in the parent headline's metadata"))
            (when (skg--node-write-protected-p parent-sexp)
              (user-error "The parent is write-protected here, so the save would not write this relationship; set it from a view where the parent is editable"))
            (list :recorder parent-gnid
                  :member member-gnid
                  :relation "contains")))
         (t (user-error
             "The parent headline is neither a node nor a writable folder")))))))

(defun skg--relRepo-current-value ()
  "Return the displayed `(skgRepos (relRepo NAME))' fact at point, if any."
  (let ((values (skg-sexp-cdr-at-path
                 (or (skg--metadata-sexp-at-point-or-nil) '(skg))
                 '(skg skgRepos relRepo))))
    (when values
      (format "%s" (car values)))))

(defun skg--relRepo-requested-value ()
  "Return the pending `(editRequest (relRepo NAME))' value at point."
  (let ((values (skg-sexp-cdr-at-path
                 (or (skg--metadata-sexp-at-point-or-nil) '(skg))
                 '(skg editRequest relRepo))))
    (when values
      (format "%s" (car values)))))

(defun skg--node-edit-request-at-point-p ()
  "Whether the headline at point holds an edit request
\(`skg--node-edit-request-p')."
  (skg--node-edit-request-p
   (or (skg--metadata-sexp-at-point-or-nil) '(skg))))

(defun skg--alias-headline-p ()
  "Return non-nil when point is on an alias property headline."
  (and (org-at-heading-p)
       (let ((metadata (skg--metadata-sexp-at-point-or-nil)))
         (and metadata (memq 'alias (cdr metadata))))))

(defun skg--relRepo-choices (ladder default)
  "The repo-name menu for `skg-set-relRepo': the tail
of LADDER (the configured repos, most public first) starting at
DEFAULT -- exactly the repos the save's default floor can accept.
When DEFAULT is nil or na from LADDER, the whole LADDER (the
server's save-time floor check backstops any stale offer)."
  (or (and default (member default ladder))
      ladder))

(defconst skg--relRepo-no-override
  "(no override: follow sticky-else-default)"
  "The menu entry that removes the pending `(editRequest (relRepo ...))' request instead of
setting one. For an relationship already on disk this means the saved repo
survives (sticky); it does not mean \"reset to the default\". To
lower an relationship's privacy to its default, choose the default relRepo
explicitly.")

(defun skg--apply-relRepo-choice (choice)
  "Apply CHOICE -- a repo name, or
`skg--relRepo-no-override' -- to the headline at point.
Edits only the buffer; returns a message string describing what the
next save will do with the relationship."
  (when (skg--node-edit-request-at-point-p)
    (user-error "Cannot request a relRepo where a delete, merge or flag request is pending"))
  (if (equal choice skg--relRepo-no-override)
      (if (skg--relRepo-requested-value)
          (progn
            ;; The displayed relRepo stays under skgRepos; only the request goes.
            (skg--write-edit-request nil)
            "Override removed: on save the membership keeps its saved (sticky) relRepo, or its default if new. Save to apply.")
        "No override present; nothing to remove.")
    (progn
      (cond
       ((or (skg--alias-headline-p)
            (skg--gnid-only-vognode-sexp-p
             (skg--metadata-sexp-at-point-or-nil)))
        (skg--write-edit-request `(relRepo ,(intern choice))))
       (t
        ;; The displayed relRepo remains under skgRepos; skgrepo intent is separate.
        (skg--write-edit-request `(relRepo ,(intern choice)) 'node)))
      (format "relRepo set to '%s'. Save to apply."
              choice))))

(defconst skg--relationship-role-menu-prose
  '(("container" nil
     "The node would contain its view-parent -- the shape of a node of the containerward chain. The relationship belongs to that node's own contains list, wherever that list is shown editable; it cannot be set from the chain node's position.")
    ("content" content
     "The view-parent contains the node: ordinary content. Sets the repo of each parent-contains-child relationship.")
    ("mentioner" nil
     "Links are inferred from body text; they carry no false relRepo, so there is nothing to set.")
    ("mentioned" nil
     "Links are inferred from body text; they carry no false relRepo, so there is nothing to set.")
    ("forker" nil
     "A member of the view-parent's forksFolder that claims nothing: either it forks the view-parent, and the forks relationship belongs to the member (the forker), recorded in its own file, or it is a mirror of the view-parent only through others. Refused from here.")
    ("forkee" forkee
     "A member of the view-parent's forksFolder that claims: the view-parent forks it. Sets the repo of each forks relationship from the view-parent.")
    ("hider" nil
     "Hide repos are derived at save, floored at the most public forks relationship explaining them; the hiderFolder is write-protected.")
    ("hidden" nil
     "Hide repos are derived at save, floored at the most public forks relationship explaining them; the hiddenFolder is write-protected."))
  "For each relation role in 'shared/relations.json',
(ROLE-NAME KIND-OR-NIL DESCRIPTION), as the relationship-kind menu of
`skg-set-relRepo-recursive' shows it. ROLE-NAME is the role the
VIEW-CHILD would play toward its view-parent. KIND-OR-NIL is the
symbol the walk dispatches on (`content' or `forkee') for the two
roles whose relationship is writable from the child's buffer
position, and nil for the rest; DESCRIPTION then
explains why the relationship cannot be set from that position.")

(defun skg--relationship-kind-menu-tree ()
  "The relationship-kind menu for `skg-set-relRepo-recursive': one
entry (RELATION-NAME ROLE...) per relation in 'shared/relations.json',
each ROLE an entry of `skg--relationship-role-menu-prose'."
  (mapcar
   (lambda (relation)
     (cons (alist-get 'name relation)
           (mapcar (lambda (role)
                     (or (assoc role skg--relationship-role-menu-prose)
                         (error "No menu prose for relation role %s" role)))
                   (alist-get 'roles relation))))
   skg-shared-relations))

(defvar-local skg--relationship-kind-menu-continuation nil
  "The continuation `skg--select-relationship-kind' stores in its
menu buffer, called with the chosen kind symbol.")

(defun skg-set-relRepo-recursive ()
  "Set the relRepo of every matching relationship in the
subtree at point.

First presents an org-menu of the schema's five node-node relations
(level-1 headlines) and their roles (level-2 headlines); pick, with
RET, the role the view-children should play toward their
view-parents. Only two roles are settable from the child's
position: `content' (ordinary content) and `forkee' (a
claiming forksFolder member); RET on any other role explains why it
cannot be set from there.

Then prompts for a repo over the whole ladder, plus the
no-override choice that instead removes existing `(relRepo ...)'
atoms. Unlike `skg-set-relRepo', no per-relationship default is
fetched: the subtree's relationships have different defaults, so the save's
floor check (see 'apply_sticky_relRepos_in_graph') is what validates each one.

The walk starts at the node at point (inclusive: its own relationship to
its view-parent counts when it matches) and recurses only on
viewchildren that affect their viewparents: member
occurrences and writable folders. It prunes below write-protected nodes
and members as such of a forks folder (their viewchildren's
relationships are not collected at save), and prunes write-protected folders and other non-vognodes
entirely.

Like other metadata edits, this only modifies the buffer; it does
not save. Call `skg-request-save-buffer' afterward."
  (interactive)
  (unless (org-at-heading-p)
    (user-error "Not on a headline"))
  (let ((buffer (current-buffer))
        (marker (point-marker)))
    (skg--select-relationship-kind
     (lambda (kind)
       (unless (buffer-live-p buffer)
         (user-error "skg: buffer vanished before the relRepo prompt"))
       (with-current-buffer buffer
         (save-excursion
           (goto-char marker)
           (let* ((ladder (skg--repo-names))
                  (choices (append ladder
                                   (list skg--relRepo-no-override)))
                  (choice (skg--completing-read-with-cycle
                           (format "Repo for every '%s' relationship in the subtree (S-left/right cycle; the save validates each relationship's floor): "
                                   kind)
                           choices nil t nil nil nil nil choices))
                  (count (skg--set-relRepo-recursive-walk
                          kind choice)))
             (message "%s"
                      (if (equal choice
                                 skg--relRepo-no-override)
                          (format "Override removed on %d '%s' relationship%s: on save each keeps its saved (sticky) repo, or its default if new. Save to apply."
                                  count kind (if (= count 1) "" "s"))
                        (format "relRepo set to '%s' on %d '%s' relationship%s. Save to apply."
                                choice count kind
                                (if (= count 1) "" "s")))))))))))

(defun skg--select-relationship-kind (continuation)
  "Pop up the org-menu over `skg--relationship-kind-menu-tree'.
RET on a settable role headline buries the menu and calls
CONTINUATION with the role's kind symbol; RET on a write-protected role
explains the refusal; q aborts."
  (let ((menu-buffer (get-buffer-create "*skg-relationship-kinds*")))
    (with-current-buffer menu-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "# What kind of relationships to the view-parent should qualify?\n"
                "# Each level-2 headline is a role the view-child would play toward\n"
                "# its view-parent. RET on one picks it; q aborts.\n")
        (dolist (relation (skg--relationship-kind-menu-tree))
          (insert (format "* %s\n" (car relation)))
          (dolist (role (cdr relation))
            (let ((headline-start (point)))
              (insert (format "** %s%s\n"
                              (nth 0 role)
                              (if (nth 1 role) "" " (not settable here)")))
              (add-text-properties
               headline-start (point)
               (list 'skg-relationship-role (nth 0 role)
                     'skg-relationship-kind (nth 1 role)
                     'skg-relationship-refusal (nth 2 role))))
            (insert (format "%s\n" (nth 2 role))))))
      (org-mode)
      (setq-local org-adapt-indentation nil)
      (read-only-mode 1)
      (use-local-map
       (let ((map (make-sparse-keymap)))
         (set-keymap-parent map org-mode-map)
         (define-key map (kbd "RET")
                     #'skg--relationship-kind-menu-choose)
         (define-key map (kbd "q") #'quit-window)
         map))
      (setq skg--relationship-kind-menu-continuation continuation)
      (goto-char (point-min)))
    (pop-to-buffer menu-buffer)))

(defun skg--relationship-kind-menu-choose ()
  "Choose the role headline at point in the relationship-kind menu."
  (interactive)
  (let* ((pos (line-beginning-position))
         (role (get-text-property pos 'skg-relationship-role))
         (kind (get-text-property pos 'skg-relationship-kind))
         (refusal (get-text-property pos 'skg-relationship-refusal))
         (continuation skg--relationship-kind-menu-continuation))
    (cond
     ((not role)
      (message "Pick a role: a level-2 headline"))
     ((not kind)
      (user-error "Cannot set that kind of relationship from the child's position: %s"
                  refusal))
     (t
      (if (get-buffer-window) ;; nil in batch tests, where no window shows the menu
          (quit-window t)
        (kill-buffer))
      (funcall continuation kind)))))

(defun skg--set-relRepo-recursive-walk (kind choice)
  "Apply CHOICE (a repo name, or
`skg--relRepo-no-override') to every headline in the
subtree at point, the headline at point included, whose relationship
to its view-parent is of KIND (`content' or `forkee'; see
`skg--relationship-kind-matches-p'). Recurses only
where edits still affect the graph: it prunes below write-protected
nodes and members as such of a forks folder, prunes noClaim
nodes -- except the walk's root, which the
user chose deliberately -- and prunes non-vognodes other than the
writable forksFolder. Returns how many headlines it applied CHOICE to."
  (let ((targets (skg--relRepo-recursive-targets kind)))
    ;; Do not let a late conflict leave earlier targets edited.  This
    ;; preflight is deliberately before the first metadata rewrite.
    (dolist (marker targets)
      (save-excursion
        (goto-char marker)
        (when (skg--node-edit-request-at-point-p)
          (user-error "Cannot request a relRepo where a delete, merge or flag request is pending"))))
    (dolist (marker targets)
      (save-excursion
        (goto-char marker)
        (skg--apply-relRepo-choice choice))
      (set-marker marker nil))
    (length targets)))

(defun skg--relRepo-recursive-targets (kind)
  "Return markers for the writable relationship targets in the subtree
at point, the headline at point included when its own relationship
matches KIND.
The traversal mirrors the extraction-aware walk used by the recursive
command, but does not edit anything."
  (save-excursion
    (let ((targets nil)
          (start-level (org-outline-level))
          (root-meta (skg--metadata-sexp-at-point-or-nil)))
      (when (skg--relationship-kind-matches-p kind)
        (push (copy-marker (line-beginning-position)) targets))
      (when (or (and (skg--occurrence-sexp-p root-meta)
                     (not (skg--relRepo-prune-below-p root-meta)))
                (skg--writable-folder-sexp-p root-meta))
        (outline-next-heading)
        (while (and (not (eobp))
                    (> (org-outline-level) start-level))
          (let ((meta (skg--metadata-sexp-at-point-or-nil)))
            (cond
             ((skg--occurrence-sexp-p meta)
              (if (not (skg--claims-membership-p meta))
                  (skg--goto-next-headline-after-subtree)
                (when (skg--relationship-kind-matches-p kind)
                  (push (copy-marker (line-beginning-position)) targets))
                (if (skg--relRepo-prune-below-p meta)
                    (skg--goto-next-headline-after-subtree)
                  (outline-next-heading))))
             ((skg--gnid-only-vognode-sexp-p meta)
              (when (skg--relationship-kind-matches-p kind)
                (push (copy-marker (line-beginning-position)) targets))
              (outline-next-heading))
             ((skg--writable-folder-sexp-p meta)
              (outline-next-heading))
             (t ;; write-protected folders, alias/ID folders, ghosts.
              (skg--goto-next-headline-after-subtree))))))
      (nreverse targets))))

(defun skg--relationship-kind-matches-p (kind)
  "Non-nil iff the headline at point is a true occurrence, an
unknown viewnode or a restricted viewnode whose
relationship to its view-parent is of KIND, writable-and-collected
from this position: for `content', the view-parent must be an
editable occurrence that is no member as such of a forks folder (a
write-protected parent's contains is not collected at save, nor is
a forkee or forker as such's, which shows its content in part, so a
relRepo request under one would be inert); for `forkee', the viewparent must be the writable
forksFolder with an editable viewparent, and the headline must claim
\(a member claiming nothing forks the viewparent, its record its own,
or is a mirror of it only through others)."
  (let ((meta (skg--metadata-sexp-at-point-or-nil)))
    (and (or (and (skg--occurrence-sexp-p meta)
                  (skg--claims-membership-p meta))
             (skg--gnid-only-vognode-sexp-p meta))
         (save-excursion
           (and (org-up-heading-safe)
                (let ((parent-sexp (skg--metadata-sexp-at-point-or-nil)))
                  (cond
                   ((skg--occurrence-sexp-p parent-sexp)
                    (and (eq kind 'content)
                         (not (skg--node-write-protected-p parent-sexp))
                         (not (skg--member-as-such-at-point-p))))
                   ((skg--non-vognode-atom-present-p parent-sexp
                                                  'forksFolder)
                    (and (eq kind 'forkee)
                         (skg--folder-viewparent-editable-p)))
                   (t nil))))))))

(defun skg--relRepo-prune-below-p (metadata-sexp)
  "Non-nil iff the walk should not descend below the occurrence
headline at point (with METADATA-SEXP its parsed metadata): an
write-protected node's contains is not collected at save, and a
member as such's viewchildren are hide/unhide signals,
not writable relationships."
  (or (skg--node-write-protected-p metadata-sexp)
      (skg--member-as-such-at-point-p)))

(defun skg--member-as-such-at-point-p ()
  "Non-nil iff the headline at point is a member as such of a
forksFolder: an occurrence member of one, a forkee as such if it
claims, a forker as such if it claims nothing. Either shows its
content in part, less what the folder's viewparent contains or hides
and what the user keeps in another owned node."
  (let ((meta (skg--metadata-sexp-at-point-or-nil)))
    (and (skg--occurrence-sexp-p meta)
         (save-excursion
           (and (org-up-heading-safe)
                (skg--non-vognode-atom-present-p
                 (skg--metadata-sexp-at-point-or-nil)
                 'forksFolder))))))

(defun skg--folder-viewparent-editable-p ()
  "Non-nil iff the folder headline at point has an editable occurrence
for its viewparent. A write-protected viewparent's writable folders are
not collected at save (the folder recorder is not save-eligible), so
atoms on their members have no effect."
  (save-excursion
    (and (org-up-heading-safe)
         (let ((viewparent-sexp (skg--metadata-sexp-at-point-or-nil)))
           (and (skg--occurrence-sexp-p viewparent-sexp)
                (not (skg--node-write-protected-p viewparent-sexp)))))))

(defun skg--non-vognode-atom-present-p (metadata-sexp atom)
  "Non-nil iff METADATA-SEXP is a (skg ...) sexp carrying the bare ATOM."
  (and (consp metadata-sexp)
       (memq atom (cdr metadata-sexp))))

(defun skg--writable-folder-sexp-p (metadata-sexp)
  "Non-nil iff METADATA-SEXP is a writable-folder's metadata:
it carries one of the `skg--writable-folder-relations' atoms."
  (and (consp metadata-sexp)
       (seq-find (lambda (entry)
                   (memq (car entry) (cdr metadata-sexp)))
                 skg--writable-folder-relations)
       t))

(provide 'skg-request-relRepo-info)
