;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: Utilities to parse and edit skg headline metadata.

(require 'org)
(require 'org-fold-core)
(require 'skg-config)
(require 'skg-sexpr-search)
(require 'skg-shared)

(defun skg-delete (&optional recursive)
  "Mark the headline at point for deletion.
With a prefix argument RECURSIVE, also mark every occurrence
viewdescendant (equivalent to `skg-delete-recursive').
Edits the metadata to include `delete` in the `editRequest` section.
Does not save; call `skg-request-save-buffer' afterward."
  (interactive "P")
  (if recursive
      (skg-delete-recursive)
    (skg-edit-metadata-at-point '(skg node (editRequest delete)))
    (forward-line)
    (message "This change will only be applied when you save the buffer.")))

(defun skg-delete-recursive ()
  "Mark the headline at point, and every occurrence viewdescendant of it,
for deletion. Descendent headlines that are not occurrences (ghosts,
unknowns, aliasFolder, id-folder, etc.) are skipped. Does not save;
call `skg-request-save-buffer' afterward."
  (interactive)
  (unless (org-at-heading-p)
    (user-error "Not on a headline"))
  (save-excursion
    (let ((start-level (org-outline-level)))
      (skg-edit-metadata-at-point
       '(skg node (editRequest delete))) ;; mark this one
      (outline-next-heading)
      (while (and (not (eobp))
                  (> (org-outline-level) start-level))
        (let* ((parts (skg-split-as-stars-metadata-title
                       (skg-get-current-headline-text)))
               (meta (and parts (nth 1 parts))))
          (when (and meta (not (string-empty-p meta))
                     (skg-sexp-subtree-p (read meta) '(skg node)))
            (skg-edit-metadata-at-point ;; mark a descendent
             '(skg node (editRequest delete)))))
        (outline-next-heading))))
  (message "This change will only be applied when you save the buffer."))

(defun skg-view-without-metadata ()
  "Copy the active region to a new org buffer, stripping skg metadata.
If there is no active region, do nothing."
  (interactive)
  (when (use-region-p)
    (let ((buffer (generate-new-buffer "*skg-without-metadata*"))
          (text
           (skg-strip-metadata-from-org-text
            (buffer-substring-no-properties
             (region-beginning)
             (region-end)))))
      (with-current-buffer buffer
        (insert text)
        (org-mode)
        (setq-local org-adapt-indentation nil)
        (set-buffer-modified-p nil)
        (goto-char (point-min)))
      (switch-to-buffer buffer))))

(defun skg-strip-metadata-from-org-text (org-text)
  "Return ORG-TEXT with all skg headline metadata removed."
  (with-temp-buffer
    (insert org-text)
    (goto-char (point-min))
    (while (not (eobp))
      (let* ((line-start (line-beginning-position))
             (line-end (line-end-position))
             (line-text
              (buffer-substring-no-properties line-start line-end))
             (parts (skg-split-as-stars-metadata-title line-text)))
        (when (and parts
                   (not (string-empty-p (nth 1 parts))))
          (delete-region line-start line-end)
          (insert (concat (nth 0 parts)
                          (nth 2 parts)))))
      (forward-line 1))
    (buffer-string)))

(defun skg--current-headline-metadata-sexp ()
  "Return the parsed skg metadata sexp for the headline at point."
  (unless (org-at-heading-p)
    (user-error "Not on a headline"))
  (or (skg--metadata-sexp-at-point-or-nil)
      (user-error "Headline has no skg metadata")))

(defun skg--current-node-repo ()
  "Return the repo string for the occurrence headline at point."
  (let* ((sexp (skg--current-headline-metadata-sexp))
         (repo-values (skg-sexp-cdr-at-path sexp '(skg skgRepos nodeRepo))))
    (unless repo-values
      (user-error "Node has no repo"))
    (format "%s" (car repo-values))))

(defun skg--headline-metadata-empty-p ()
  "Return non-nil if the headline at point has no skg metadata."
  (let* (( headline (skg-get-current-headline-text) )
         ( split (skg-split-as-stars-metadata-title headline) ))
    (or (null split)
        (string-empty-p (cadr split)))))

(defun skg--populate-minimal-node-metadata ()
  "Write minimal occurrence metadata onto the metadata-less headline at point.
Prompts for an owned repo (no prompt when only one repo is owned)
and inserts (skg node (skgRepos (nodeRepo REPO))).  Returns the chosen repo.
Reuses `skg-edit-metadata-at-point', which formats and spaces the sexp
correctly relative to the existing title."
  (let (( skgrepo (skg--prompt-for-owned-repo) ))
    (skg-edit-metadata-at-point
     `(skg node (skgRepos (nodeRepo ,(intern skgrepo)))))
    skgrepo))

(defun skg-set-repo (&optional recursive)
  "Prompt for and change the repo of the node at point.
Starts with the current repo as minibuffer text.  S-left/S-right cycle
through owned repos, C-? displays all configured repos and
their paths, and typed repo names are accepted directly.
With a prefix argument RECURSIVE, changes every true content
descendent whose repo matches the repo at point.
Only member descendents (no noClaim atom) are traversed.
On a headline that has no metadata yet, instead populates it minimally
via `skg--populate-minimal-node-metadata' (RECURSIVE is then moot).

What the move strands, the save decides: relationships it would leave
at their old, more private repos come back for review before anything
is written (see `skg--strandeds-confirmation-handler'), and those it would
leave more public than a node's new home are raised automatically.

Write-protected occurrences are not changed -- the save would refuse
their repo edits -- and produce a loud warning, with the
full ID list in *Messages*.

Does not save; call `skg-request-save-buffer' afterward."
  (interactive "P")
  (if (skg--headline-metadata-empty-p)
      (skg--populate-minimal-node-metadata)
    (let* ((current-skgrepo (skg--current-node-repo))
           (new-skgrepo (string-trim
                        (skg--prompt-for-repo-change current-skgrepo))))
      (unless (string-empty-p new-skgrepo)
        (skg--validate-repo-name new-skgrepo)
        (if (string= current-skgrepo new-skgrepo)
            (message "Repo unchanged: %s" current-skgrepo)
          (skg--change-repo-and-report
           current-skgrepo new-skgrepo recursive))))))

(defun skg-set-repo-recursive ()
  "Prompt for and recursively change the repo of the node at point.
This is the recursive form of `skg-set-repo': it changes every
content-descendent whose repo matches the repo at point.
Only member descendents (no noClaim atom) are traversed.
Does not save; call `skg-request-save-buffer' afterward."
  (interactive)
  (skg-set-repo t))

(defun skg--change-repo-and-report (old-skgrepo new-skgrepo recursive)
  "The body of `skg-set-repo' once a real move is requested: retarget
the repos (skipping write-protected occurrences) and report -- loudly,
when write-protected occurrences were skipped."
  (let* ((change-result (if recursive
                            (skg--change-repo-recursive old-skgrepo
                                                          new-skgrepo)
                          (skg--change-repo-at-point-unless-write-protected
                           new-skgrepo)))
         (changed-count (car change-result))
         (write-protected-gnids (cdr change-result)))
    (dolist (gnid write-protected-gnids)
      (message "skg-set-repo: write-protected occurrence not changed (the save would refuse its edit): %s"
               gnid))
    (message "%s"
             (concat
              (format "Repo changed from %s to %s on %d node%s. Save to apply."
                      old-skgrepo new-skgrepo changed-count
                      (if (= changed-count 1) "" "s"))
              (when write-protected-gnids
                (format "  WARNING: %d write-protected node%s not changed -- the save would refuse their edits. See *Messages* for the ID list."
                        (length write-protected-gnids)
                        (if (= (length write-protected-gnids) 1) "" "s")))))))

(defun skg--change-repo-at-point-unless-write-protected (new-skgrepo)
  "Set the repo at point to NEW-REPO, unless the occurrence is
write-protected -- the save would refuse that edit. Returns
(CHANGED-COUNT . WRITE-PROTECTED-IDS), matching `skg--change-repo-recursive'."
  (let ((meta (skg--metadata-sexp-at-point-or-nil)))
    (if (skg--node-write-protected-p meta)
        (cons 0 (list (or (skg--node-id meta) "(no id)")))
      (cons (skg--change-repo-at-point new-skgrepo) nil))))

(defconst skg--relRepo-unsupported-folder-atoms
  '(subscriberFolder overriderFolder hiderFolder hiddenFolder
    containerFolder mentionerFolder acquiredByFolder acquiredFolder
    hiddenInSubscribeeFolder hiddenOutsideOfSubscribeeFolder)
  "The partner folder atoms where an explicit relRepo
request is unsupported from this side.  The membership of
hiddenOutsideOfSubscribeeFolder, a compound partner folder, is
editable, but hide repos are still derived and cannot carry this
request.
`skg-set-relRepo' refuses on a member of one of these:
the relationship belongs to the other end, so setting its repo here would
be meaningless.")

(defconst skg--writable-folder-relations
  '((subscribeeFolder . "subscribesTo")
    (overriddenFolder . "overrides"))
  "The partner folder atoms whose members' relationships are writable
from this side, each mapped to its relation's wire name
(NodeRelation::relation_name, server/dbs/in_rust_graph/
relation_accessors.rs). The folder's viewparent (the anchor) owns the
outbound relationship to each member.")

(defun skg--rel-at-point ()
  "Classify the relationship the headline at point represents.
Returns a plist (:recorder RECORDER-ID :member MEMBER-ID :relation NAME):
for a content child, the viewparent contains the node at point; for
a writable-folder member, the folder's anchor (the folder's viewparent) owns
the folder's relation toward the node at point. Signals `user-error'
when point represents no writable relationship: not on an occurrence or Unknown
headline, on a root headline (no viewparent, so no relationship), on a
member of a write-protected folder, or with an ID missing."
  (unless (org-at-heading-p)
    (user-error "Not on a headline"))
  (let ((member-sexp (skg--metadata-sexp-at-point-or-nil)))
    (unless (or (skg--occurrence-sexp-p member-sexp)
                (skg--unknown-headline-p member-sexp))
      (user-error "Not on an occurrence or Unknown headline"))
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
         (writable-folder
          (let* ((anchor-sexp
                  (save-excursion
                    (and (org-up-heading-safe) ;; to the folder
                         (org-up-heading-safe) ;; to its anchor
                         (skg--metadata-sexp-at-point-or-nil))))
                 (anchor-gnid (skg--node-id anchor-sexp)))
            (unless anchor-gnid
              (user-error "Could not find the folder's anchor headline"))
            (when (skg--node-write-protected-p anchor-sexp)
              (user-error "The folder's anchor is write-protected here, so the save would not write this relationship; set it from a view where the anchor is editable"))
            (list :recorder anchor-gnid
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
  "Whether the occurrence headline at point already requests delete or
merge: its '(editRequest ...)' holds a part that is neither a relRepo
request nor a readiness request."
  (let* ((metadata (or (skg--metadata-sexp-at-point-or-nil) '(skg)))
         (values (and (skg--occurrence-sexp-p metadata)
                      (skg-sexp-cdr-at-path metadata '(skg editRequest)))))
    (seq-some (lambda (part)
                (not (or (eq (car-safe part) 'relRepo)
                         (skg--readiness-request-p part))))
              values)))

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
    (user-error "Cannot request a relRepo where delete or merge is pending"))
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
            (skg--unknown-headline-p
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
    ("subscriber" nil
     "A subscriberFolder member: the subscribesTo relationship belongs to the member (the subscriber), not to the view-parent. Write-protected from here.")
    ("subscribee" subscribee
     "A member of the view-parent's subscribeeFolder. Sets the repo of each subscribesTo relationship from the view-parent.")
    ("hider" nil
     "Hide repos are derived at save, floored at the most public explaining subscription; the hiderFolder is write-protected.")
    ("hidden" nil
     "Hide repos are derived at save, floored at the most public explaining subscription; the hiddenFolder is write-protected.")
    ("overrider" nil
     "An overriderFolder member: the overrides relationship belongs to the member (the overrider), not to the view-parent. Write-protected from here.")
    ("overridden" overridden
     "A member of the view-parent's overriddenFolder. Sets the repo of each overrides relationship from the view-parent."))
  "For each relation role in 'shared/relations.json',
(ROLE-NAME KIND-OR-NIL DESCRIPTION), as the relationship-kind menu of
`skg-set-relRepo-recursive' shows it. ROLE-NAME is the role the
VIEW-CHILD would play toward its view-parent. KIND-OR-NIL is the
symbol the walk dispatches on (`content', `subscribee' or
`overridden') for the three roles whose relationship is writable from the
child's buffer position, and nil for the rest; DESCRIPTION then
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
view-parents. Only three roles are settable from the child's
position: `content' (ordinary content), `subscribee' (a
subscribeeFolder member) and `overridden' (an overriddenFolder member);
RET on any other role explains why it cannot be set from there.

Then prompts for a repo over the whole ladder, plus the
no-override choice that instead removes existing `(relRepo ...)'
atoms. Unlike `skg-set-relRepo', no per-relationship default is
fetched: the subtree's relationships have different defaults, so the save's
floor check (see `apply_sticky_relRepos') is what validates each one.

The walk starts at the node at point (inclusive: its own relationship to
its view-parent counts when it matches) and recurses only on
viewchildren that affect their viewparents: member
occurrences and writable folders. It prunes below write-protected nodes
and subscribee-as-such members (their viewchildren's relationships are not
collected at save), and prunes write-protected folders and other non-vognodes
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
to its view-parent is of KIND (`content', `subscribee' or
`overridden'; see `skg--relationship-kind-matches-p'). Recurses only
where edits still affect the graph: it prunes below write-protected
nodes and subscribee-as-such members, prunes noClaim
nodes -- except the walk's root, which the
user chose deliberately -- and prunes non-vognodes other than the two
writable folders. Returns the number of relationships true."
  (let ((targets (skg--relRepo-recursive-targets kind)))
    ;; Do not let a late conflict leave earlier targets edited.  This
    ;; preflight is deliberately before the first metadata rewrite.
    (dolist (marker targets)
      (save-excursion
        (goto-char marker)
        (when (skg--node-edit-request-at-point-p)
          (user-error "Cannot request a relRepo where delete or merge is pending"))))
    (dolist (marker targets)
      (save-excursion
        (goto-char marker)
        (skg--apply-relRepo-choice choice))
      (set-marker marker nil))
    (length targets)))

(defun skg--relRepo-recursive-targets (kind)
  "Return markers for the writable relationship targets below point.
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
              (if (not (skg--node-member-p meta))
                  (skg--goto-next-headline-after-subtree)
                (when (skg--relationship-kind-matches-p kind)
                  (push (copy-marker (line-beginning-position)) targets))
                (if (skg--relRepo-prune-below-p meta)
                    (skg--goto-next-headline-after-subtree)
                  (outline-next-heading))))
             ((skg--unknown-headline-p meta)
              (when (skg--relationship-kind-matches-p kind)
                (push (copy-marker (line-beginning-position)) targets))
              (outline-next-heading))
             ((skg--writable-folder-sexp-p meta)
              (outline-next-heading))
             (t ;; write-protected folders, alias/ID folders, ghosts and unknowns.
              (skg--goto-next-headline-after-subtree))))))
      (nreverse targets))))

(defun skg--relationship-kind-matches-p (kind)
  "Non-nil iff the headline at point is a true occurrence or Unknown whose
relationship to its view-parent is of KIND, writable-and-collected
from this position: for `content', the view-parent must be a
editable occurrence not in subscribee-as-such position (an
write-protected or subscribee-as-such parent's contains is not
collected at save, so a relRepo request under one would be
inert); for `subscribee' and `overridden', the viewparent must be
the matching writable folder with a editable anchor."
  (let ((meta (skg--metadata-sexp-at-point-or-nil)))
    (and (or (and (skg--occurrence-sexp-p meta)
                  (skg--node-member-p meta))
             (skg--unknown-headline-p meta))
         (save-excursion
           (and (org-up-heading-safe)
                (let ((parent-sexp (skg--metadata-sexp-at-point-or-nil)))
                  (cond
                   ((skg--occurrence-sexp-p parent-sexp)
                    (and (eq kind 'content)
                         (not (skg--node-write-protected-p parent-sexp))
                         (not (skg--subscribee-as-such-at-point-p))))
                   ((skg--non-vognode-atom-present-p parent-sexp
                                                  'subscribeeFolder)
                    (and (eq kind 'subscribee)
                         (skg--folder-anchor-editable-p)))
                   ((skg--non-vognode-atom-present-p parent-sexp
                                                  'overriddenFolder)
                    (and (eq kind 'overridden)
                         (skg--folder-anchor-editable-p)))
                   (t nil))))))))

(defun skg--relRepo-prune-below-p (metadata-sexp)
  "Non-nil iff the walk should not descend below the occurrence
headline at point (with METADATA-SEXP its parsed metadata): an
write-protected node's contains is not collected at save, and a
subscribee-as-such member's viewchildren are hide/unhide signals,
not writable relationships."
  (or (skg--node-write-protected-p metadata-sexp)
      (skg--subscribee-as-such-at-point-p)))

(defun skg--subscribee-as-such-at-point-p ()
  "Non-nil iff the headline at point sits in subscribee-as-such
position: an true occurrence member of a subscribeeFolder."
  (let ((meta (skg--metadata-sexp-at-point-or-nil)))
    (and (skg--occurrence-sexp-p meta)
         (skg--node-member-p meta)
         (save-excursion
           (and (org-up-heading-safe)
                (skg--non-vognode-atom-present-p
                 (skg--metadata-sexp-at-point-or-nil)
                 'subscribeeFolder))))))

(defun skg--folder-anchor-editable-p ()
  "Non-nil iff the folder headline at point has an editable occurrence
anchor (its viewparent). A write-protected anchor's writable folders are
not collected at save (the folder recorder is not save-eligible), so
atoms on their members have no effect."
  (save-excursion
    (and (org-up-heading-safe)
         (let ((anchor-sexp (skg--metadata-sexp-at-point-or-nil)))
           (and (skg--occurrence-sexp-p anchor-sexp)
                (not (skg--node-write-protected-p anchor-sexp)))))))

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

(defun skg-set-merge-request (acquiree-id-or-link)
  "Prompt for ACQUIREE-ID-OR-LINK and mark the node at point to merge it.
The command is run from the acquirer.  The prompt accepts either a
bare ID or an org id link like [[id:ID][label]].
Does not save; call `skg-request-save-buffer' afterward."
  (interactive
   (list (skg--read-id-or-link "Acquiree ID or link: ")))
  (let ((acquiree-gnid (skg--id-from-link-or-text acquiree-id-or-link)))
    (when (string-empty-p acquiree-gnid)
      (user-error "Acquiree ID cannot be empty"))
    (skg--write-edit-request `(merge ,(intern acquiree-gnid)) 'node)
    (message "Merge request set for acquiree %s. Save to apply."
             acquiree-gnid)))

;;; The diff view's restore requests ('docs/diff-view.org',
;;; "Restoring what a change removed"). The server refuses them on the
;;; wrong kind of headline with the same texts.

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

(defun skg--relationship-removed-p (metadata-sexp)
  "Non-nil when METADATA-SEXP's '(diff ...)' says its relationship was
removed, by a '(rel removed)'."
  (equal (skg-sexp-cdr-at-path metadata-sexp '(skg diff rel))
         '(removed)))

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
           (and (not (skg--node-member-p metadata))
                (not (skg--containerward-chain-node-at-point-p))))
          (t nil))))

(defun skg--containerward-chain-node-at-point-p ()
  "Non-nil iff the headline at point is a node of the containerward
chain: a vognode claiming nothing under a member of a partner folder whose
members carry the chain (every one but the two acquisition folders),
or under another such node ('is_in_containerward_chain',
server/to_org/extend/containerward_chain.rs)."
  (save-excursion
    (catch 'answer
      (while t
        (let ((metadata (skg--metadata-sexp-at-point-or-nil)))
          (unless (and (skg--vognode-sexp-p metadata)
                       (or (not (skg--occurrence-sexp-p metadata))
                           (not (skg--node-member-p metadata))))
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
                                  '(acquiredByFolder acquiredFolder))))
              (throw 'answer t))))))))

(defconst skg--property-folder-atoms
  '(aliasFolder idFolder flagsFolder)
  "The atoms of the property folders, whose members are a node's
aliases, IDs and flags. Every other folder is a partner folder.")

(defun skg--partner-folder-atom (metadata-sexp)
  "The partner folder's atom if METADATA-SEXP is a partner folder's
\(`skg--folder-atom', but for the property folders), else nil."
  (let ((folder (skg--folder-atom metadata-sexp)))
    (and folder
         (not (memq folder skg--property-folder-atoms))
         folder)))

;;; A headline's change facts

(defun skg--readiness-diff-form (metadata-sexp)
  "The '(diff ...)' form of METADATA-SEXP, or nil. Every kind's
'(diff ...)' stands at the top level of '(skg ...)', beside its kind
atom."
  (assq 'diff (cdr metadata-sexp)))

(defun skg--readiness-form (metadata-sexp key)
  "The (KEY ...) part of METADATA-SEXP's '(diff ...)' form, or nil."
  (assq key (cdr (skg--readiness-diff-form metadata-sexp))))

(defun skg--readiness-leaves (metadata-sexp)
  "The '(leaves N OBJECT...)' fact of the headline with METADATA-SEXP,
as (N . OBJECTS), or nil when it names no leaf."
  (cdr (skg--readiness-form metadata-sexp 'leaves)))

(defun skg--readiness-objects-ready-p (metadata-sexp objects)
  "Non-nil when every one of OBJECTS is ready on METADATA-SEXP's headline:
when it carries a '(ready ...)' that names each of them. A headline
that lists no object (one naming a single leaf) is ready when it
carries any '(ready ...)'."
  (let ((ready (skg--readiness-form metadata-sexp 'ready)))
    (and ready
         (seq-every-p (lambda (object) (memq object (cdr ready)))
                      objects))))

;;; A changed folder
;;; The server writes every folder as its bare atom. Whether the diff
;;; view shows it because of a change, and whether that change is
;;; ready, follows from its members (its child headlines), as computed
;;; here ('folder_change_facts' did it on the server before).

(defun skg--folder-atom (metadata-sexp)
  "The folder's atom if METADATA-SEXP is a folder's (a kind 'folder'
in 'shared/metadata-fields.json'), else nil."
  (and (consp metadata-sexp)
       (seq-some (lambda (item)
                   (and (symbolp item)
                        (equal (alist-get 'kind (skg-shared-metadata-kind
                                                 (symbol-name item)))
                               "folder")
                        item))
                 (cdr metadata-sexp))))

(defun skg--membership-change-readiness (member-sexp child-sexps)
  "Whether the folder member with metadata MEMBER-SEXP, whose child
headlines carry CHILD-SEXPS, carries a change of its relationship to
the folder's viewparent, and whether that change is ready: nil when
it carries none, else `ready' or `unready'. It carries one when its
'(diff ...)' has a '(rel ...)', or when a child is a
'(relRepoMove ...)'. The change is ready when both hold: a '(rel ...)'
comes with a '(ready ...)' that names 'rel' if the member's
'(leaves ...)' lists 'rel'; every '(relRepoMove ...)' child carries
'(diff (ready ...))'."
  (let ((rel-change (skg--readiness-form member-sexp 'rel))
        (relRepo-moves
         (seq-filter (lambda (child-sexp)
                       (assq 'relRepoMove (cdr child-sexp)))
                     child-sexps)))
    (when (or rel-change relRepo-moves)
      (if (and (or (not rel-change)
                   (skg--readiness-objects-ready-p
                    member-sexp
                    (and (memq 'rel (cdr (skg--readiness-leaves member-sexp)))
                         '(rel))))
               (seq-every-p (lambda (relRepo-move)
                              (skg--readiness-form relRepo-move 'ready))
                            relRepo-moves))
          'ready
        'unready))))

(defun skg--folder-change-readiness (members)
  "Whether a folder whose MEMBERS are as given is changed, and whether
that change is ready: nil when no member carries a change of its
relationship to the folder's viewparent, `ready' when every such
change is ready, else `unready'. Each of MEMBERS is (MEMBER-SEXP
. CHILD-SEXPS), as `skg--membership-change-readiness' takes them."
  (let ((changes (delq nil (mapcar (lambda (member)
                                     (skg--membership-change-readiness
                                      (car member) (cdr member)))
                                   members))))
    (when changes
      (if (memq 'unready changes) 'unready 'ready))))

(defun skg--folder-change-readiness-at-point ()
  "`skg--folder-change-readiness' of the folder headline on the line
at point, or nil when that line is no folder's headline. Reads
headline levels by their stars, so it works in any buffer."
  (save-excursion
    (beginning-of-line)
    (let ((level (skg--headline-level-on-line)))
      (when (and level
                 (skg--folder-atom (skg--metadata-sexp-at-point-or-nil)))
        (skg--folder-change-readiness
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


;;; A pending detachment
;;; A child the user marked 'noClaim' under an editable parent stays
;;; in the buffer but leaves the parent's list at the next save. Until
;;; then it is warranted and claims nothing, which no part of its own
;;; metadata says, so the heralds ask here and render it as a request.

(defun skg--pending-detachment-p (metadata-sexp parent-sexp grandparent-sexp)
  "Non-nil when METADATA-SEXP, under a parent with PARENT-SEXP whose own
parent has GRANDPARENT-SEXP, is a pending detachment: it carries
'noClaim', the graph warrants it, and its parent is editable."
  (and (consp metadata-sexp)
       (memq 'noClaim (cdr metadata-sexp))
       (skg--warranted-by-the-graph-p metadata-sexp)
       (skg--editable-parent-p parent-sexp grandparent-sexp)))

(defun skg--warranted-by-the-graph-p (metadata-sexp)
  "Non-nil unless METADATA-SEXP's '(view ...)' carries 'noWarrant' or
'ghostWarrant', or its '(diff ...)' a '(rel removed)': what HEAD alone
warrants is no detachment."
  (let ((view (cdr (assq 'view (cdr metadata-sexp)))))
    (not (or (memq 'noWarrant view)
             (memq 'ghostWarrant view)
             (skg--relationship-removed-p metadata-sexp)))))

(defun skg--editable-parent-p (parent-sexp grandparent-sexp)
  "Non-nil when a headline under a parent with PARENT-SEXP, whose own
parent has GRANDPARENT-SEXP, stands under an editable parent: a node
that is not write-protected, or a folder 'shared/herald-styles.json'
does not list as write-protected whose anchor is such a node."
  (let ((editable-node-p
         (lambda (sexp)
           (and (skg--occurrence-sexp-p sexp)
                (not (skg--node-write-protected-p sexp)))))
        (folder (skg--folder-atom parent-sexp)))
    (if folder
        (and (not (member (symbol-name folder)
                          (alist-get 'write_protected
                                     (alist-get 'folder_heralds
                                                skg-shared-herald-styles))))
             (funcall editable-node-p grandparent-sexp))
      (funcall editable-node-p parent-sexp))))

(defun skg--pending-detachment-at-point-p ()
  "`skg--pending-detachment-p' of the headline on the line at point.
Reads headline levels by their stars, so it works in any buffer, and
looks for the parents only of a headline carrying 'noClaim'."
  (let ((metadata (skg--metadata-sexp-at-point-or-nil)))
    (when (memq 'noClaim (cdr-safe metadata))
      (let ((ancestors (skg--ancestor-metadata-sexps-on-line 2)))
        (skg--pending-detachment-p metadata (nth 0 ancestors)
                                   (nth 1 ancestors))))))

(defun skg--ancestor-metadata-sexps-on-line (count)
  "The metadata of the COUNT nearest ancestor headlines of the headline
on the line at point, nearest first, nil where one has none or there
is none. Reads headline levels by their stars."
  (mapcar (lambda (ancestor-start)
            (and ancestor-start
                 (save-excursion
                   (goto-char ancestor-start)
                   (skg--metadata-sexp-at-point-or-nil))))
          (skg--ancestor-headline-starts-on-line count)))

(defun skg--ancestor-headline-starts-on-line (count)
  "Where the COUNT nearest ancestor headlines of the headline on the
line at point begin, nearest first, nil where there is none. Reads
headline levels by their stars, so it works in any buffer."
  (save-excursion
    (beginning-of-line)
    (let ((level (skg--headline-level-on-line))
          (ancestor-starts '()))
      (dotimes (_ count)
        (if (and level
                 (> level 1)
                 (re-search-backward
                  (format "^\\*\\{1,%d\\}\\(?: \\|$\\)" (1- level))
                  nil t))
            (progn (setq level (skg--headline-level-on-line))
                   (push (point) ancestor-starts))
          (setq level nil)
          (push nil ancestor-starts)))
      (nreverse ancestor-starts))))

(defun skg--headline-level-on-line ()
  "The number of stars of the headline on the line at point, or nil
when the line is no headline."
  (save-excursion
    (beginning-of-line)
    (when (looking-at "\\(\\*+\\)\\(?: \\|$\\)")
      (length (match-string 1)))))

(defun skg--metadata-on-line ()
  "The '(skg ...)' metadata of the headline on the line at point, as
`skg--metadata-from' gives it, or nil when the line is no headline or
its headline carries none. Reads the headline by its stars, so it
works in any buffer."
  (save-excursion
    (beginning-of-line)
    (when (looking-at "\\*+ +(skg[ )]")
      (skg--metadata-from (- (match-end 0) 5)))))

(defun skg--metadata-from (start)
  "The '(skg ...)' that begins at START: (START END SEXP), END the
position after its closing parenthesis and SEXP what it reads as; or
nil when its parentheses do not close on that line or it does not
read. Finds END by counting parentheses (`skg-find-sexp-end'; see
'elisp/PITFALLs.org')."
  (let* ((text (buffer-substring-no-properties
                start (save-excursion (goto-char start)
                                      (line-end-position))))
         (end (skg-find-sexp-end text))
         (sexp (and end
                    (ignore-errors
                      (car (read-from-string (substring text 0 end)))))))
    (when sexp
      (list start (+ start end) sexp))))

(defun skg--vognode-sexp-p (metadata-sexp)
  "Non-nil iff METADATA-SEXP is a vognode's: an occurrence,
a ghost or an unknown."
  (and (consp metadata-sexp)
       (or (skg--occurrence-sexp-p metadata-sexp)
           (skg--ghost-sexp-p metadata-sexp)
           (skg--unknown-headline-p metadata-sexp))))

(defun skg--ghost-sexp-p (metadata-sexp)
  "Non-nil iff METADATA-SEXP is a ghost's: a node of the diff view that
HEAD has and the worktree lacks, '(skg ghost (gnid X) ...)'."
  (and (consp metadata-sexp)
       (skg-sexp-subtree-p metadata-sexp '(skg ghost))))

(defun skg--read-id-or-link (prompt)
  "Read an ID or link with linkstack paste/pop bindings in the minibuffer."
  (minibuffer-with-setup-hook #'skg--install-linkstack-minibuffer-bindings
    (read-string prompt)))

(defun skg--install-linkstack-minibuffer-bindings ()
  "Install linkstack paste/pop bindings in the active minibuffer."
  (let ((map (copy-keymap (current-local-map))))
    (define-key map (kbd "C-c o i") #'skg-paste-id)
    (define-key map (kbd "C-c o l") #'skg-paste-link)
    (define-key map (kbd "C-c O i") #'skg-pop-id)
    (define-key map (kbd "C-c O l") #'skg-pop-link)
    (use-local-map map)))

(defun skg--id-from-link-or-text (text)
  "Extract an ID from TEXT, accepting org id links or bare IDs."
  (let ((trimmed (string-trim text)))
    (if (string-match "\\[\\[id:\\([^]]+\\)\\]\\[" trimmed)
        (match-string 1 trimmed)
      trimmed)))

(defun skg--validate-repo-name (skgrepo)
  "Signal an error if REPO cannot be represented in skg metadata."
  (when (string-match-p "[[:space:]]" skgrepo)
    (user-error "Repo names cannot contain whitespace")))

(defun skg--change-repo-recursive (old-skgrepo new-skgrepo)
  "Change OLD-REPO to NEW-REPO in this content subtree.
Returns (CHANGED-COUNT . WRITE-PROTECTED-IDS).  The root node is inclusive;
only member descendents (no noClaim atom) are traversed.
A write-protected occurrence is not edited -- the save would refuse
its repo edit -- and its ID is
collected into WRITE-PROTECTED-IDS instead, for the caller to warn about,
unless the walk also changed that ID's editable occurrence: that one
carries the move, and the rerender after the save shows it everywhere.
Its viewdescendants are still traversed: they are self-writers, so
their repo edits take effect even under a write-protected parent."
  (let* ((changed-count 0)
         (changed-gnids '())
         (write-protected-gnids '()))
    (skg--do-at-recursive-repo-move-occurrences
     old-skgrepo
     (lambda ()
       (let ((meta (skg--metadata-sexp-at-point-or-nil)))
         (if (skg--node-write-protected-p meta)
             (push (or (skg--node-id meta) "(no id)")
                   write-protected-gnids)
           (push (skg--node-id meta) changed-gnids)
           (setq changed-count
                 (+ changed-count
                    (skg--change-repo-at-point new-skgrepo)))))))
    (cons changed-count
          (nreverse (seq-remove
                     (lambda (gnid) (member gnid changed-gnids))
                     write-protected-gnids)))))

(defun skg--do-at-recursive-repo-move-occurrences (old-skgrepo fn)
  "Call FN, with point on each headline a recursive repo move from
the headline at point covers: that root, and every viewdescendant
reached through member occurrences whose repo is
OLD-REPO. Restores point afterward."
  (save-excursion
    (let ((start-level (org-outline-level)))
      (funcall fn) ;; the root
      (outline-next-heading)
      (while (and (not (eobp))
                  (> (org-outline-level) start-level))
        (let ((metadata-sexp (skg--metadata-sexp-at-point-or-nil)))
          (if (not (and (skg--occurrence-sexp-p metadata-sexp)
                        (skg--node-member-p metadata-sexp)))
              (skg--goto-next-headline-after-subtree)
            (when (equal (skg--node-repo metadata-sexp) old-skgrepo)
              (save-excursion (funcall fn)))
            (outline-next-heading)))))))

(defun skg--goto-next-headline-after-subtree ()
  "Move to the next headline after the current subtree."
  (let ((prune-level (org-outline-level)))
    (outline-next-heading)
    (while (and (not (eobp))
                (> (org-outline-level) prune-level))
      (outline-next-heading))))

(defun skg--metadata-sexp-at-point-or-nil ()
  "The '(skg ...)' metadata of the headline on the line at point, read,
or nil when the line is no headline, its headline carries none, or it
does not read (`skg--metadata-on-line')."
  (nth 2 (skg--metadata-on-line)))

(defun skg--occurrence-sexp-p (metadata-sexp)
  "Return non-nil if METADATA-SEXP describes an occurrence."
  (and metadata-sexp
       (skg-sexp-subtree-p metadata-sexp '(skg node))))

(defun skg--node-member-p (metadata-sexp)
  "Return non-nil unless METADATA-SEXP carries the bare noClaim atom.
A member carries nothing."
  (not (skg-sexp-subtree-p metadata-sexp '(skg noClaim))))

(defun skg--node-repo (metadata-sexp)
  "Return METADATA-SEXP's node repo as a string, or nil."
  (let ((repo-values (skg-sexp-cdr-at-path metadata-sexp
                                             '(skg skgRepos nodeRepo))))
    (when repo-values
      (format "%s" (car repo-values)))))

(defun skg--metadata-gnid (metadata-sexp)
  "Return METADATA-SEXP's '(gnid X)' value as a string, whatever the
headline's kind, or nil."
  (let ((id-values (skg-sexp-cdr-at-path metadata-sexp '(skg gnid))))
    (when id-values
      (format "%s" (car id-values)))))

(defun skg--node-id (metadata-sexp)
  "Return METADATA-SEXP's node ID as a string, or nil (also when
METADATA-SEXP is not an occurrence's)."
  (and (skg--occurrence-sexp-p metadata-sexp)
       (skg--metadata-gnid metadata-sexp)))

(defun skg--unknown-headline-p (metadata-sexp)
  "Return non-nil when METADATA-SEXP is an unknown."
  (and metadata-sexp
       (skg-sexp-subtree-p metadata-sexp '(skg unknown))))

(defun skg--relationship-member-id (metadata-sexp)
  "Return the raw member ID for an occurrence or Unknown headline."
  (and (or (skg--occurrence-sexp-p metadata-sexp)
           (skg--unknown-headline-p metadata-sexp))
       (skg--metadata-gnid metadata-sexp)))

(defun skg--unknown-node-id-at-point ()
  "The ID of the unknown whose headline is at point, or nil."
  (let ((metadata (skg--metadata-sexp-at-point-or-nil)))
    (when (skg--unknown-headline-p metadata)
      (skg--metadata-gnid metadata))))

(defun skg--node-write-protected-p (metadata-sexp)
  "Return non-nil if METADATA-SEXP has the bare occurrence writeProtected atom."
  (skg-sexp-subtree-p metadata-sexp '(skg node writeProtected)))

(defun skg--change-repo-at-point (new-skgrepo)
  "Set the repo at point to NEW-REPO.
Returns 1 if the current line was edited. The headline's derived
'(view repoBoundary)' is left as it is: the save recomputes it."
  (skg-edit-metadata-at-point
   `(skg (skgRepos)
         (skgRepos (ENSURE (nodeRepo ,(intern new-skgrepo))))))
  1)

(defun skg--remove-fork-from-viewrequests (host-sexp)
  "Return HOST-SEXP with the `fork' symbol removed from its
\(viewRequests ...) form, dropping that form entirely if it becomes empty.
HOST-SEXP is a (skg ...) sexp. The result is `equal' to
HOST-SEXP when there was no fork request, so callers can detect a no-op."
  (cons
   'skg
   (delq nil
         (mapcar
          (lambda (elem)
            (if (and (consp elem) (eq (car elem) 'viewRequests))
                (let ((kept (delq 'fork (copy-sequence (cdr elem)))))
                  (and kept (cons 'viewRequests kept)))
              elem))
          (cdr host-sexp)))))

(defun skg-strip-fork-requests-in-buffer ()
  "Remove every (viewRequests ... fork ...) fork request from the headlines
of the current buffer. Used on fork-decline so a lingering explicit-fork
atom does not silently re-fork on the next save."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward org-heading-regexp nil t)
      (beginning-of-line)
      (let* ((headline-text (skg-get-current-headline-text))
             (match (skg-split-as-stars-metadata-title headline-text)))
        (when (and match (not (string-empty-p (nth 1 match))))
          (let* ((stars (nth 0 match))
                 (sexp (read (nth 1 match)))
                 (title (nth 2 match))
                 (stripped (skg--remove-fork-from-viewrequests sexp)))
            (unless (equal stripped sexp)
              (skg-replace-current-line
               (skg-format-headline
                stars (substring-no-properties (format "%S" stripped))
                title))))))
      (forward-line 1))))

(defun skg-edit-metadata-at-point (edits)
  "Use EDITS to edit the metadata of the headline at point.
If there is metadata, merges it with existing metadata.
If there is no metadata, creates new metadata from EDITS.
If the current line is not a headline, no effect.
Routes the rewrite through `skg-replace-current-line' so that editing
a folded headline does not fire org-fold's fragility check — see the
\"Programmatic metadata edits must be performed ignoring fragility
checks\" entry in PITFALLs.org."
  (when (org-at-heading-p) ;; otherwise this does nothing
    (let* ((headline-text (skg-get-current-headline-text))
           (match-result ;; could be nil
            (skg-split-as-stars-metadata-title
             headline-text)))
      (if (and match-result
               (not (string-empty-p (nth 1 match-result))))
          (let* ((stars (nth 0 match-result))
                 (metadata-sexp (nth 1 match-result))
                 (title (nth 2 match-result))
                 (host-sexp (read metadata-sexp))
                 (merged-sexp (skg--derived-facts-last
                               (skg-edit-nested-sexp
                                host-sexp edits))))
            (let ((new-metadata-sexp (substring-no-properties
                                      (format "%S" merged-sexp))))
              (skg-replace-current-line
               (skg-format-headline stars new-metadata-sexp title))))
        (progn ;; Headline has no metadata.
          (when (string-match "^\\(\\*+\\s-+\\)\\(.*\\)" headline-text)
          (let* ((stars (match-string 1 headline-text))
                 (title (match-string 2 headline-text))
                 (metadata-sexp (substring-no-properties
                                 (format "%S" edits))))
            (skg-replace-current-line
             (skg-format-headline stars metadata-sexp title)))))))))

(defun skg--readiness-request-p (part)
  "Non-nil when PART, an element of an '(editRequest ...)', is a
readiness request, '(ready OBJECT...)' or '(unready OBJECT...)'."
  (memq (car-safe part) '(ready unready)))

(defun skg--readiness-request-of (metadata-sexp)
  "The readiness request in METADATA-SEXP's '(editRequest ...)', or nil."
  (seq-find #'skg--readiness-request-p
            (cdr (assq 'editRequest (cdr metadata-sexp)))))

(defun skg--metadata-with-readiness-request (metadata-sexp request)
  "METADATA-SEXP with REQUEST as the readiness request of its
'(editRequest ...)': any readiness request there replaced, the other
request kept, REQUEST last. A nil REQUEST removes it, and with it an
'(editRequest ...)' left empty. A headline without '(editRequest ...)'
gets one, before its derived facts."
  (let* ((parts (cdr metadata-sexp))
         (old (assq 'editRequest parts))
         (kept (seq-remove #'skg--readiness-request-p (cdr old)))
         (new (when (or kept request)
                (append '(editRequest) kept (and request (list request))))))
    (skg--derived-facts-last
     (cons 'skg
           (cond ((and old new)
                  (mapcar (lambda (part) (if (eq part old) new part)) parts))
                 (old (remq old parts))
                 (new (append parts (list new)))
                 (t parts))))))

(defun skg--metadata-with-edit-request (metadata-sexp request)
  "METADATA-SEXP with REQUEST as the edit request of its
'(editRequest ...)': any edit request there replaced, its readiness
request kept after it. A nil REQUEST removes the edit request, and
with it an '(editRequest ...)' left empty. A headline without
'(editRequest ...)' gets one, before its derived facts. The
counterpart of `skg--metadata-with-readiness-request'."
  (let* ((parts (cdr metadata-sexp))
         (old (assq 'editRequest parts))
         (kept (seq-filter #'skg--readiness-request-p (cdr old)))
         (new (when (or request kept)
                (append '(editRequest) (and request (list request)) kept))))
    (skg--derived-facts-last
     (cons 'skg
           (cond ((and old new)
                  (mapcar (lambda (part) (if (eq part old) new part)) parts))
                 (old (remq old parts))
                 (new (append parts (list new)))
                 (t parts))))))

(defun skg--write-edit-request (request &optional kind)
  "Make REQUEST the edit request of the headline at point, keeping the
readiness request its '(editRequest ...)' holds
\(`skg--metadata-with-edit-request'). A nil REQUEST removes the edit
request. KIND, when given, is the kind atom a headline without
metadata gets."
  (let* ((metadata-sexp (skg--metadata-sexp-at-point-or-nil))
         (edit-request
          (assq 'editRequest
                (cdr (skg--metadata-with-edit-request
                      metadata-sexp request)))))
    (skg-edit-metadata-at-point
     `(skg ,@(and kind (list kind))
           ,@(and metadata-sexp '((DELETE (editRequest))))
           ,@(and edit-request (list edit-request))))))

(defun skg--derived-facts-last (metadata-sexp)
  "METADATA-SEXP with its derived facts, '(view ...)' then '(diff ...)',
after every other part, as the server writes them, so that a part an
edit appends lands before them. The other parts keep their order."
  (if (not (and (consp metadata-sexp) (eq (car metadata-sexp) 'skg)))
      metadata-sexp
    (let ((derived-fact-p (lambda (part)
                            (and (consp part)
                                 (memq (car part) '(view diff))))))
      (cons 'skg
            (append (seq-remove derived-fact-p (cdr metadata-sexp))
                    (seq-filter (lambda (part) (eq (car-safe part) 'view))
                                (cdr metadata-sexp))
                    (seq-filter (lambda (part) (eq (car-safe part) 'diff))
                                (cdr metadata-sexp)))))))

(defun skg-replace-current-line (new-content)
  "Replace the current line with NEW-CONTENT.
Moves to beginning of line, deletes the line, and inserts NEW-CONTENT.
The `delete-region' + `insert' pair below runs inside
`org-fold-core-ignore-fragility-checks' to protect against corrupting
a folded subtree beneath the edited headline line — see the
\"Programmatic metadata edits must be performed ignoring fragility
checks\" entry in PITFALLs.org for the full explanation. Do not
bypass this helper when rewriting a headline line in place; call it
instead of raw `delete-region' + `insert'."
  (beginning-of-line)
  (org-fold-core-ignore-fragility-checks
    (delete-region (line-beginning-position)
                   (line-end-position))
    (insert new-content)))

(defun skg-split-as-stars-metadata-title (headline-text)
  "Match HEADLINE-TEXT and extract stars, metadata sexp, and title.
Returns (STARS METADATA-SEXP TITLE) or nil if no match.
METADATA-SEXP is the complete (skg ...) s-expression, or empty string if no metadata.
Handles nested parentheses in metadata correctly."
  (let ((trimmed (string-trim-left headline-text)))
    (when (string-match "^\\(\\*+\\s-+\\)" trimmed)
      (let* ((stars (match-string 1 trimmed))
             (after-stars (substring trimmed (match-end 1))))
        (if (string-prefix-p "(skg" after-stars)
            ;; Has metadata - find matching close paren
            (let ((sexp-end-pos (skg-find-sexp-end after-stars)))
              (when sexp-end-pos
                (let* ((skg-sexp (substring after-stars 0 sexp-end-pos))
                       (title-start sexp-end-pos)
                       (len (length after-stars))
                       (title (string-trim (if (< title-start len)
                                               (substring after-stars title-start)
                                             ""))))
                  (list stars skg-sexp title))))
          ;; No metadata
          (list stars "" after-stars))))))

(defun skg-get-current-headline-text ()
  "ASSUMES
point is already on a headline - does not move point.
.
Returns the current headline in its entirety,
including asterisks and metadata, but not the trailing newline."
  (save-excursion
    (beginning-of-line)
    (let ((start (point)))
      (end-of-line)
      (buffer-substring-no-properties start (point)))))

(defun skg-beginning-of-line ()
  "Toggle point between the start of the line and the start of the title.
On a headline, the title is the text following the stars and skg
metadata: press once to jump there, again to return to the true
beginning of the line. On a non-headline, just move to the beginning
of the line. Reuses `skg-split-as-stars-metadata-title' to locate the
title, so it never re-implements metadata parsing."
  (interactive)
  (let* ((parts (and (org-at-heading-p)
                     (skg-split-as-stars-metadata-title
                      (skg-get-current-headline-text))))
         (title-pos
          (and parts
               (save-excursion
                 (goto-char (+ (line-beginning-position)
                               (length (nth 0 parts))
                               (length (nth 1 parts))))
                 (skip-chars-forward " \t")
                 (point)))))
    (if (and title-pos (/= (point) title-pos))
        (goto-char title-pos)
      (beginning-of-line))))

(defun skg-format-headline
    (stars metadata-sexp title)
  "Format a headline with STARS, METADATA-SEXP, and TITLE.
METADATA-SEXP should be the complete (skg ...) s-expression,
or the empty string.
Handles empty metadata correctly."
  (if (string-empty-p metadata-sexp)
      (format "%s(skg) %s" stars title)
    (format "%s%s %s" stars metadata-sexp title)))

(defun skg--around-org-todo (orig-fn &rest args)
  "Around advice for `org-todo'.
Strip (skg ...) metadata before cycling, re-insert after."
  (let* ((on-headline (org-at-heading-p))
         (parts (when on-headline
                  (save-excursion
                    (beginning-of-line)
                    (skg-split-as-stars-metadata-title
                     (buffer-substring-no-properties
                      (line-beginning-position)
                      (line-end-position))))))
         (has-metadata (and parts
                            (not (string-empty-p (nth 1 parts))))))
    (when has-metadata
      ;; Remove metadata from the line so org sees a plain headline.
      (save-excursion
        (skg-replace-current-line
         (concat (nth 0 parts) (nth 2 parts)))))
    (apply orig-fn args)
    (when has-metadata
      ;; Re-insert metadata after stars (and any new TODO keyword).
      (save-excursion
        (beginning-of-line)
        (let* ((new-line (buffer-substring-no-properties
                          (line-beginning-position)
                          (line-end-position)))
               (new-parts (when (string-match
                                 "^\\(\\*+\\s-+\\)\\(.*\\)" new-line)
                            (list (match-string 1 new-line)
                                  (match-string 2 new-line)))))
          (when new-parts
            (skg-replace-current-line
             (concat (nth 0 new-parts)
                     (nth 1 parts)
                     " "
                     (nth 1 new-parts)))))))))

(advice-add 'org-todo :around #'skg--around-org-todo)

(provide 'skg-metadata)
