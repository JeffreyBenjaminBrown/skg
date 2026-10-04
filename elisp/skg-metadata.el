;;; -*- lexical-binding: t; -*-
;;;
;;; PURPOSE: Utilities to parse and edit skg headline metadata.

(require 'org)
(require 'org-fold-core)
(require 'skg-config)
(require 'skg-sexpr-search)

(defun skg-delete (&optional recursive)
  "Mark the headline at point for deletion.
With a prefix argument RECURSIVE, also mark every activeNode
org-descendent (equivalent to `skg-delete-recursive').
Edits the metadata to include `delete` in the `editRequest` section.
Does NOT save; call `skg-request-save-buffer' afterward."
  (interactive "P")
  (if recursive
      (skg-delete-recursive)
    (skg-edit-metadata-at-point '(skg (node (editRequest delete))))
    (forward-line)
    (message "This change will only be applied when you save the buffer.")))

(defun skg-delete-recursive ()
  "Mark the headline at point, and every activeNode org-descendent of it,
for deletion. Descendent headlines that are not activeNodes (phantoms,
aliasFolder, id-folder, etc.) are skipped. Does NOT save;
call `skg-request-save-buffer' afterward."
  (interactive)
  (unless (org-at-heading-p)
    (user-error "Not on a headline"))
  (save-excursion
    (let ((start-level (org-outline-level)))
      (skg-edit-metadata-at-point
       '(skg (node (editRequest delete)))) ;; mark this one
      (outline-next-heading)
      (while (and (not (eobp))
                  (> (org-outline-level) start-level))
        (let* ((parts (skg-split-as-stars-metadata-title
                       (skg-get-current-headline-text)))
               (meta (and parts (nth 1 parts))))
          (when (and meta (not (string-empty-p meta))
                     (skg-sexp-subtree-p (read meta) '(skg (node))))
            (skg-edit-metadata-at-point ;; mark a descendent
             '(skg (node (editRequest delete))))))
        (outline-next-heading))))
  (message "This change will only be applied when you save the buffer."))

(defun skg-set-write-protected ()
  "Mark the headline at point as write-protected.
Edits the metadata to include `writeProtected` in the `node` section.
Does NOT save; call `skg-request-save-buffer' afterward."
  (interactive)
  (skg-edit-metadata-at-point '(skg (node writeProtected))))

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
  (let* ((headline (skg-get-current-headline-text))
         (split (skg-split-as-stars-metadata-title headline))
         (metadata-str (and split (cadr split))))
    (unless (and metadata-str
                 (not (string-empty-p metadata-str)))
      (user-error "Headline has no skg metadata"))
    (read metadata-str)))

(defun skg--current-node-repo ()
  "Return the repo string for the ActiveNode headline at point."
  (let* ((sexp (skg--current-headline-metadata-sexp))
         (repo-values (skg-sexp-cdr-at-path sexp '(skg node repo))))
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
  "Write minimal ActiveNode metadata onto the metadata-less headline at point.
Prompts for an owned repo (no prompt when only one repo is owned)
and inserts (skg (node (repo REPO))).  Returns the chosen repo.
Reuses `skg-edit-metadata-at-point', which formats and spaces the sexp
correctly relative to the existing title."
  (let (( repo (skg--prompt-for-owned-repo) ))
    (skg-edit-metadata-at-point
     `(skg (node (repo ,(intern repo)))))
    repo))

(defun skg-set-repo (&optional recursive)
  "Prompt for and change the repo of the node at point.
Starts with the current repo as minibuffer text.  S-left/S-right cycle
through owned repos, C-? displays all configured repos and
their paths, and typed repo names are accepted directly.
With a prefix argument RECURSIVE, changes every true content
descendent whose repo matches the repo at point.
Only descendents for which affectsParent=true are traversed.
On a headline that has no metadata yet, instead populates it minimally
via `skg--populate-minimal-node-metadata' (RECURSIVE is then moot).

When the move would leave content relationships stuck at their old,
more private repos (the sticky rule never lowers an edge's privacy
without an explicit gesture; see
TODO/MAYBE-BUG_recursive-move-to-more-public-leaves-relations-private.org),
offers to publicize them in the same go by writing
`(editRequest (relRepo ...))' requests; declining leaves them and mentions that
`skg-set-relRepo-recursive' (C-c s R) can publicize them
later.

Write-protected instances are NOT changed -- the save would silently
ignore their repo edits -- and produce a loud warning, with the
full ID list in *Messages*.

Does NOT save; call `skg-request-save-buffer' afterward."
  (interactive "P")
  (if (skg--headline-metadata-empty-p)
      (skg--populate-minimal-node-metadata)
    (let* ((current-repo (skg--current-node-repo))
           (new-repo (string-trim
                        (skg--prompt-for-repo-change current-repo))))
      (unless (string-empty-p new-repo)
        (skg--validate-repo-name new-repo)
        (if (string= current-repo new-repo)
            (message "Repo unchanged: %s" current-repo)
          (skg--set-repo-and-handle-stuck-edges
           current-repo new-repo recursive))))))

(defun skg-set-repo-recursive ()
  "Prompt for and recursively change the repo of the node at point.
This is the recursive form of `skg-set-repo': it changes every
content-descendent whose repo matches the repo at point.
Only descendents for which affectsParent=true are traversed.
Does NOT save; call `skg-request-save-buffer' afterward."
  (interactive)
  (skg-set-repo t))

(defun skg--set-repo-and-handle-stuck-edges (old-repo new-repo recursive)
  "The body of `skg-set-repo' once a real move is requested:
analyze which content edges the move would leave stuck at more
private repos, retarget the repos (skipping write-protected
instances), offer to publicize the stuck edges in the same go, and
report -- loudly, when write-protected instances were skipped."
  (let* ((stuck ;; analyzed BEFORE any rewrite: it needs the old repos
          (skg--analyze-move-stuck-edges old-repo new-repo recursive))
         (change-result (if recursive
                            (skg--change-repo-recursive old-repo
                                                          new-repo)
                          (skg--change-repo-at-point-unless-write-protected
                           new-repo)))
         (changed-count (car change-result))
         (write-protected-ids (cdr change-result))
         (fixed-count
          (when (and stuck
                     (y-or-n-p
                      (format "This move would leave %d content relationship%s in their old, more private repo%s. Publicize them too? "
                              (length stuck)
                              (if (= (length stuck) 1) "" "s")
                              (if (= (length stuck) 1) "" "s"))))
            (skg--apply-stuck-relRepos stuck))))
    (dolist (id write-protected-ids)
      (message "skg-set-repo: write-protected instance NOT changed (the save would ignore it): %s"
               id))
    (message "%s"
             (concat
              (format "Repo changed from %s to %s on %d node%s. Save to apply."
                      old-repo new-repo changed-count
                      (if (= changed-count 1) "" "s"))
              (cond
               (fixed-count
                (format " Also publicized %d relationship%s."
                        fixed-count (if (= fixed-count 1) "" "s")))
               (stuck
                " Relationships kept their old, more private repos; C-c s R can publicize them later."))
              (when write-protected-ids
                (format "  WARNING: %d write-protected node%s NOT changed -- the save would silently ignore them. See *Messages* for the ID list."
                        (length write-protected-ids)
                        (if (= (length write-protected-ids) 1) "" "s")))))))

(defun skg--analyze-move-stuck-edges (old-repo new-repo recursive)
  "With point on the node a `skg-set-repo' move starts from, and
BEFORE any repo is rewritten: return the true content edges
the move would leave stuck in a more private repo than their new
default, as a list of (MARKER . REPO) -- MARKER at the child
headline, REPO the edge's new default. Only edges without an
existing `(relRepo ...)' atom qualify: an atom-carrying edge was
already assigned a deliberate relRepo. The walk's root itself is always
retargeted (unless write-protected); its org-parent lies outside the
move, so its repo counts as unchanging. With RECURSIVE nil only
the point node moves, so only its own edge and its direct
children's edges are examined."
  (save-excursion
    (let* ((stuck '())
           (start-level (org-outline-level))
           (consider ;; point on a candidate child C, whose inbound edge is examined; the arguments say whether each endpoint's repo is about to be retargeted
            (lambda (parent-retargets-p child-retargets-p)
              (when (skg--relationship-kind-matches-p 'contained)
                (let* ((child-meta (skg--metadata-sexp-at-point-or-nil))
                       (child-repo (skg--node-repo child-meta))
                       (child-moves ;; a write-protected instance is skipped by the retargeting walk, so its repo does not actually change
                        (and child-retargets-p
                             (not (skg--node-write-protected-p child-meta))))
                       (parent-repo
                        (save-excursion
                          (org-up-heading-safe)
                          (skg--node-repo
                           (skg--metadata-sexp-at-point-or-nil))))
                       (eff (lambda (repo retargets-p)
                              (if (and retargets-p
                                       (equal repo old-repo))
                                  new-repo
                                repo)))
                       (repo (skg--content-edge-stuck-repo
                               parent-repo
                               (funcall eff parent-repo
                                        parent-retargets-p)
                               child-repo
                               (funcall eff child-repo child-moves))))
                  (when repo
                    (push (cons (copy-marker (line-beginning-position))
                                repo)
                          stuck)))))))
      (funcall consider nil t) ;; the root's own inbound edge
      (outline-next-heading)
      (while (and (not (eobp))
                  (> (org-outline-level) start-level))
        (let ((meta (skg--metadata-sexp-at-point-or-nil)))
          (if (not (and (skg--activeNode-sexp-p meta)
                        (skg--node-affectsParent-content-of-p meta)))
              (skg--goto-next-heading-after-subtree)
            (if recursive
                (funcall consider t t)
              (when (= (org-outline-level) (1+ start-level))
                ;; Single move: only the point node moves, so only
                ;; its direct children's inbound edges can change.
                (funcall consider t nil)))
            (outline-next-heading))))
      (nreverse stuck))))

(defun skg--content-edge-stuck-repo (parent-eff-old parent-eff-new
                                      child-eff-old child-eff-new)
  "The repo to which the content edge at point (from its view-parent
to the headline at point) should be publicized after a repo move,
or nil when the move does not strand it: nil when the edge carries
an explicit `(editRequest (relRepo ...))' request (deliberately
repo-specified), when a
default cannot be computed (a repo unknown to the config -- the
save validates anyway), or when the edge's default does not become
more public. The four arguments are the endpoints' repos before
and after the move."
  (unless (skg--relRepo-requested-value)
    (let ((old-default (skg--more-private-of-repos
                        parent-eff-old child-eff-old))
          (new-default (skg--more-private-of-repos
                        parent-eff-new child-eff-new)))
      (when (and old-default new-default
                 (skg--strictly-more-public-repo-p new-default
                                                     old-default))
        new-default))))

(defun skg--apply-stuck-relRepos (stuck)
  "Write a relRepo request at each (MARKER . REPO) in
STUCK, then free the markers. Returns the number of requests written."
  (save-excursion
    (dolist (entry stuck)
      (goto-char (car entry))
      (skg--apply-relRepo-choice (cdr entry))
      (set-marker (car entry) nil))
    (length stuck)))

(defun skg--change-repo-at-point-unless-write-protected (new-repo)
  "Set the repo at point to NEW-REPO, unless the instance is
write-protected -- the save would silently ignore that edit. Returns
(CHANGED-COUNT . WRITE-PROTECTED-IDS), matching `skg--change-repo-recursive'."
  (let ((meta (skg--metadata-sexp-at-point-or-nil)))
    (if (skg--node-write-protected-p meta)
        (cons 0 (list (or (skg--node-id meta) "(no id)")))
      (cons (skg--change-repo-at-point new-repo) nil))))

(defun skg--more-private-of-repos (a b)
  "The more private of repos A and B per the config's privacy
order (later in the ladder = more private), or nil when either
names no configured repo."
  (let ((pa (skg--repo-privacy-position a))
        (pb (skg--repo-privacy-position b)))
    (when (and pa pb)
      (if (> pa pb) a b))))

(defun skg--strictly-more-public-repo-p (a b)
  "Non-nil iff repo A is strictly more public than repo B per
the config's privacy order. Nil when either is unknown."
  (let ((pa (skg--repo-privacy-position a))
        (pb (skg--repo-privacy-position b)))
    (and pa pb (< pa pb))))

(defun skg--repo-privacy-position (repo)
  "REPO's index in the config's privacy order (0 = most public),
or nil when REPO is nil or names no configured repo."
  (and repo
       (seq-position (skg--repo-names) repo #'string=)))

(defconst skg--relRepo-unsupported-folder-atoms
  '(subscriberFolder overriderFolder hiderFolder hiddenFolder
    hiddenInSubscribeeFolder hiddenOutsideOfSubscribeeFolder)
  "The PartnerFolder atoms where an explicit relRepo
request is unsupported from this side.  HiddenOutside membership is
editable as a derived filter, but hide repos are still derived and
cannot carry this request.
`skg-set-relRepo' refuses on a member of one of these:
the edge belongs to the other end, so setting its repo here would
be meaningless.")

(defconst skg--writable-folder-relations
  '((subscribeeFolder . "subscribes_to")
    (overriddenFolder . "overrides_view_of"))
  "The PartnerFolder atoms whose members' edges are WRITABLE
from this side, each mapped to its relation's wire name
(NodeRelation::relation_name, server/dbs/in_rust_graph/
relation_accessors.rs). The folder's org-parent (the anchor) owns the
outbound edge to each member.")

(defun skg--rel-at-point ()
  "Classify the relationship edge the headline at point represents.
Returns a plist (:owner OWNER-ID :member MEMBER-ID :relation NAME):
for a content child, the org-parent contains the node at point; for
a writable-folder member, the folder's anchor (the folder's org-parent) owns
the folder's relation toward the node at point. Signals `user-error'
when point represents no writable edge: not on an activeNode or Unknown
headline, on a root headline (no org-parent, so no edge), on a
member of a write-protected folder, or with an ID missing."
  (unless (org-at-heading-p)
    (user-error "Not on a headline"))
  (let ((member-sexp (skg--metadata-sexp-at-point-or-nil)))
    (unless (or (skg--activeNode-sexp-p member-sexp)
                (skg--unknown-headline-p member-sexp))
      (user-error "Not on an activeNode or Unknown headline"))
    (let ((member-id (skg--relationship-member-id member-sexp))
          (parent-sexp (save-excursion
                         (and (org-up-heading-safe)
                              (skg--metadata-sexp-at-point-or-nil)))))
      (unless member-id
        (user-error "No id in this headline's metadata"))
      (unless parent-sexp
        (user-error
         "Root headline: there is no relationship edge here to set"))
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
          (let ((anchor-id
                 (save-excursion
                   (and (org-up-heading-safe) ;; to the folder
                        (org-up-heading-safe) ;; to its anchor
                        (skg--node-id
                         (skg--metadata-sexp-at-point-or-nil))))))
            (unless anchor-id
              (user-error "Could not find the folder's anchor headline"))
            (list :owner anchor-id
                  :member member-id
                  :relation (cdr writable-folder))))
         ((skg--activeNode-sexp-p parent-sexp)
          (let ((parent-id (skg--node-id parent-sexp)))
            (unless parent-id
              (user-error "No id in the parent headline's metadata"))
            (list :owner parent-id
                  :member member-id
                  :relation "contains")))
         (t (user-error
             "The parent headline is neither a node nor a writable folder")))))))

(defun skg--relRepo-current-value ()
  "Return the displayed `(relRepo NAME)' fact at point, if any."
  (let* ((metadata (or (skg--metadata-sexp-at-point-or-nil) '(skg)))
         (alias-p (memq 'alias (cdr metadata)))
         (unknown-p (skg--unknown-headline-p metadata))
         (values (skg-sexp-cdr-at-path
                  metadata
                  (cond (alias-p '(skg relRepo))
                        (unknown-p '(skg unknown viewStats relRepo))
                        (t '(skg node viewStats relRepo))))))
    (when values
      (format "%s" (car values)))))

(defun skg--relRepo-requested-value ()
  "Return the pending `(editRequest (relRepo NAME))' value at point."
  (let* ((metadata (or (skg--metadata-sexp-at-point-or-nil) '(skg)))
         (alias-p (memq 'alias (cdr metadata)))
         (unknown-p (skg--unknown-headline-p metadata))
         (values (skg-sexp-cdr-at-path
                  metadata
                  (cond (alias-p '(skg editRequest relRepo))
                        (unknown-p '(skg unknown editRequest relRepo))
                        (t '(skg node editRequest relRepo))))))
    (when values
      (format "%s" (car values)))))

(defun skg--node-edit-request-at-point-p ()
  "Whether the Active headline at point already requests delete or merge."
  (let ((values (skg-sexp-cdr-at-path
                 (or (skg--metadata-sexp-at-point-or-nil) '(skg))
                 '(skg node editRequest))))
    (and values
         (not (and (consp (car values))
                   (eq (caar values) 'relRepo))))))

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
  "The menu entry that REMOVES the pending `(editRequest (relRepo ...))' request instead of
setting one. For an edge already on disk this means the SAVED repo
survives (sticky); it does NOT mean \"reset to the default\". To
lower an edge's privacy to its default, choose the default relRepo
explicitly.")

(defun skg--apply-relRepo-choice (choice)
  "Apply CHOICE -- a repo name, or
`skg--relRepo-no-override' -- to the headline at point.
Edits only the buffer; returns a message string describing what the
next save will do with the edge."
  (when (skg--node-edit-request-at-point-p)
    (user-error "Cannot request a relRepo where delete or merge is pending"))
  (if (equal choice skg--relRepo-no-override)
      (if (skg--relRepo-requested-value)
          (progn
            (cond
             ((skg--alias-headline-p)
              (skg-edit-metadata-at-point
               '(skg (DELETE (editRequest)))))
             ((skg--unknown-headline-p
               (skg--metadata-sexp-at-point-or-nil))
              (skg-edit-metadata-at-point
               '(skg (unknown (DELETE (editRequest))))))
             (t
              ;; The display fact stays under viewStats; only the request goes.
              (skg-edit-metadata-at-point
               '(skg (node (DELETE (editRequest)))))))
            "Override removed: on save the member keeps its saved (sticky) repo, or its default if new. Save to apply.")
        "No override present; nothing to remove.")
    (progn
      (cond
       ((skg--alias-headline-p)
        (skg-edit-metadata-at-point
         `(skg (ENSURE (editRequest (relRepo ,(intern choice)))))))
        ((skg--unknown-headline-p
         (skg--metadata-sexp-at-point-or-nil))
        (skg-edit-metadata-at-point
         `(skg (unknown (ENSURE (editRequest (relRepo ,(intern choice))))))))
       (t
        ;; The display fact remains under viewStats; repo intent is separate.
        (skg-edit-metadata-at-point '(skg (node (editRequest))))
        (skg-edit-metadata-at-point
         `(skg (node (editRequest (ENSURE (relRepo ,(intern choice)))))))))
      (format "relRepo set to '%s'. Save to apply."
              choice))))

(defconst skg--relationship-kind-menu-tree
  '(("contains"
     ("container" nil
      "The node would CONTAIN its view-parent -- the shape of a containerward ancestry graft. The edge belongs to the graft's own contains list, wherever that list is drawn definitively; it cannot be set from the graft's position.")
     ("contained" contained
      "The view-parent contains the node: ordinary content. Sets the repo of each parent-contains-child edge."))
    ("links_to"
     ("mentioner" nil
      "Links are inferred from body text; they carry no false relRepo, so there is nothing to set.")
     ("mentioned" nil
      "Links are inferred from body text; they carry no false relRepo, so there is nothing to set."))
    ("subscribes_to"
     ("subscriber" nil
      "A subscriberFolder member: the subscribes edge belongs to the member (the subscriber), not to the view-parent. Write-protected from here.")
     ("subscribee" subscribee
      "A member of the view-parent's subscribeeFolder. Sets the repo of each anchor-subscribes-to-member edge."))
    ("hides_from_its_subscriptions"
     ("hider" nil
      "Hide repos are derived at save, floored at the most public explaining subscription; the hiderFolder is write-protected.")
     ("hidden" nil
      "Hide repos are derived at save, floored at the most public explaining subscription; the hiddenFolder is write-protected."))
    ("overrides_view_of"
     ("overrider" nil
      "An overriderFolder member: the overrides edge belongs to the member (the overrider), not to the view-parent. Write-protected from here.")
     ("overridden" overridden
      "A member of the view-parent's overriddenFolder. Sets the repo of each anchor-overrides-view-of-member edge.")))
  "The relationship-kind menu for
`skg-set-relRepo-recursive': one entry per node-node
relation in docs/data-model_technical.org, each listing its two roles as
(ROLE-NAME KIND-OR-NIL DESCRIPTION). ROLE-NAME is the role the
VIEW-CHILD would play toward its view-parent. KIND-OR-NIL is the
symbol the walk dispatches on (`contained', `subscribee' or
`overridden') for the three roles whose edge is writable from the
child's buffer position, and nil for the rest; DESCRIPTION then
explains why the edge cannot be set from that position.")

(defvar-local skg--relationship-kind-menu-continuation nil
  "The continuation `skg--select-relationship-kind' stores in its
menu buffer, called with the chosen kind symbol.")

(defun skg-set-relRepo-recursive ()
  "Set the relRepo of every matching relationship edge in the
subtree at point.

First presents an org-menu of the schema's five node-node relations
(level-1 headlines) and their roles (level-2 headlines); pick, with
RET, the role the view-CHILDREN should play toward their
view-parents. Only three roles are settable from the child's
position: `contained' (ordinary content), `subscribee' (a
subscribeeFolder member) and `overridden' (an overriddenFolder member);
RET on any other role explains why it cannot be set from there.

Then prompts for a repo over the whole ladder, plus the
no-override choice that instead REMOVES existing `(relRepo ...)'
atoms. Unlike `skg-set-relRepo', no per-edge default is
fetched: the subtree's edges have different defaults, so the save's
floor check (see `apply_sticky_relRepos') is what validates each one.

The walk starts at the node at point (inclusive: its own edge to
its view-parent counts when it matches) and recurses only on
viewchildren that affect their viewparents: affectsParent=true
activeNodes and writable folders. It prunes below write-protected nodes
and subscribee-as-such members (their org-children's edges are not
collected at save), and prunes write-protected folders and other non-vognodes
entirely.

Like other metadata edits, this only modifies the buffer; it does
NOT save. Call `skg-request-save-buffer' afterward."
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
                           (format "Repo for every '%s' edge in the subtree (S-left/right cycle; the save validates each edge's floor): "
                                   kind)
                           choices nil t nil nil nil nil choices))
                  (count (skg--set-relRepo-recursive-walk
                          kind choice)))
             (message "%s"
                      (if (equal choice
                                 skg--relRepo-no-override)
                          (format "Override removed on %d '%s' edge%s: on save each keeps its saved (sticky) repo, or its default if new. Save to apply."
                                  count kind (if (= count 1) "" "s"))
                        (format "relRepo set to '%s' on %d '%s' edge%s. Save to apply."
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
                "# Each level-2 headline is a role the view-CHILD would play toward\n"
                "# its view-parent. RET on one picks it; q aborts.\n")
        (dolist (relation skg--relationship-kind-menu-tree)
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
      (user-error "Cannot set that kind of edge from the child's position: %s"
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
to its view-parent is of KIND (`contained', `subscribee' or
`overridden'; see `skg--relationship-kind-matches-p'). Recurses only
where edits still affect the graph: it prunes below write-protected
nodes and subscribee-as-such members, prunes non-true
(affectsParent=false) nodes -- except the walk's root, which the
user chose deliberately -- and prunes non-vognodes other than the two
writable folders. Returns the number of edges true."
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
      (when (or (and (skg--activeNode-sexp-p root-meta)
                     (not (skg--relRepo-prune-below-p root-meta)))
                (skg--writable-folder-sexp-p root-meta))
        (outline-next-heading)
        (while (and (not (eobp))
                    (> (org-outline-level) start-level))
          (let ((meta (skg--metadata-sexp-at-point-or-nil)))
            (cond
             ((skg--activeNode-sexp-p meta)
              (if (not (skg--node-affectsParent-content-of-p meta))
                  (skg--goto-next-heading-after-subtree)
                (when (skg--relationship-kind-matches-p kind)
                  (push (copy-marker (line-beginning-position)) targets))
                (if (skg--relRepo-prune-below-p meta)
                    (skg--goto-next-heading-after-subtree)
                  (outline-next-heading))))
             ((skg--unknown-headline-p meta)
              (when (skg--relationship-kind-matches-p kind)
                (push (copy-marker (line-beginning-position)) targets))
              (outline-next-heading))
             ((skg--writable-folder-sexp-p meta)
              (outline-next-heading))
             (t ;; write-protected folders, alias/ID folders, and other phantoms.
              (skg--goto-next-heading-after-subtree))))))
      (nreverse targets))))

(defun skg--relationship-kind-matches-p (kind)
  "Non-nil iff the headline at point is a true activeNode or Unknown whose
relationship to its view-parent is of KIND, writable-and-collected
from this position: for `contained', the view-parent must be a
definitive activeNode not in subscribee-as-such position (an
write-protected or subscribee-as-such parent's contains is not
collected at save, so a relRepo request under one would be
inert); for `subscribee' and `overridden', the view-parent must be
the matching writable folder with a definitive anchor."
  (let ((meta (skg--metadata-sexp-at-point-or-nil)))
    (and (or (and (skg--activeNode-sexp-p meta)
                  (skg--node-affectsParent-content-of-p meta))
             (skg--unknown-headline-p meta))
         (save-excursion
           (and (org-up-heading-safe)
                (let ((parent-sexp (skg--metadata-sexp-at-point-or-nil)))
                  (cond
                   ((skg--activeNode-sexp-p parent-sexp)
                    (and (eq kind 'contained)
                         (not (skg--node-write-protected-p parent-sexp))
                         (not (skg--subscribee-as-such-at-point-p))))
                   ((skg--non-vognode-atom-present-p parent-sexp
                                                  'subscribeeFolder)
                    (and (eq kind 'subscribee)
                         (skg--folder-anchor-definitive-p)))
                   ((skg--non-vognode-atom-present-p parent-sexp
                                                  'overriddenFolder)
                    (and (eq kind 'overridden)
                         (skg--folder-anchor-definitive-p)))
                   (t nil))))))))

(defun skg--relRepo-prune-below-p (metadata-sexp)
  "Non-nil iff the walk should not descend below the activeNode
headline at point (with METADATA-SEXP its parsed metadata): an
write-protected node's contains is not collected at save, and a
subscribee-as-such member's org-children are hide/unhide signals,
not writable edges."
  (or (skg--node-write-protected-p metadata-sexp)
      (skg--subscribee-as-such-at-point-p)))

(defun skg--subscribee-as-such-at-point-p ()
  "Non-nil iff the headline at point sits in subscribee-as-such
position: an true activeNode member of a subscribeeFolder."
  (let ((meta (skg--metadata-sexp-at-point-or-nil)))
    (and (skg--activeNode-sexp-p meta)
         (skg--node-affectsParent-content-of-p meta)
         (save-excursion
           (and (org-up-heading-safe)
                (skg--non-vognode-atom-present-p
                 (skg--metadata-sexp-at-point-or-nil)
                 'subscribeeFolder))))))

(defun skg--folder-anchor-definitive-p ()
  "Non-nil iff the folder headline at point has a definitive activeNode
anchor (its org-parent). A write-protected anchor's writable folders are
not collected at save (the folder owner is not save-eligible), so
atoms on their members have no effect."
  (save-excursion
    (and (org-up-heading-safe)
         (let ((anchor-sexp (skg--metadata-sexp-at-point-or-nil)))
           (and (skg--activeNode-sexp-p anchor-sexp)
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
Does NOT save; call `skg-request-save-buffer' afterward."
  (interactive
   (list (skg--read-id-or-link "Acquiree ID or link: ")))
  (let ((acquiree-id (skg--id-from-link-or-text acquiree-id-or-link)))
    (when (string-empty-p acquiree-id)
      (user-error "Acquiree ID cannot be empty"))
    (skg-edit-metadata-at-point
     `(skg (node (DELETE (editRequest))
                 (editRequest (merge ,(intern acquiree-id))))))
    (message "Merge request set for acquiree %s. Save to apply."
             acquiree-id)))

(defun skg--read-id-or-link (prompt)
  "Read an ID or link with ID-stack paste/pop bindings in the minibuffer."
  (minibuffer-with-setup-hook #'skg--install-id-stack-minibuffer-bindings
    (read-string prompt)))

(defun skg--install-id-stack-minibuffer-bindings ()
  "Install ID-stack paste/pop bindings in the active minibuffer."
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

(defun skg--validate-repo-name (repo)
  "Signal an error if REPO cannot be represented in skg metadata."
  (when (string-match-p "[[:space:]]" repo)
    (user-error "Repo names cannot contain whitespace")))

(defun skg--change-repo-recursive (old-repo new-repo)
  "Change OLD-REPO to NEW-REPO in this content subtree.
Returns (CHANGED-COUNT . WRITE-PROTECTED-IDS).  The root node is inclusive;
only descendents for which affectsParent=true are traversed.
A write-protected instance is NOT edited -- the save would silently
ignore its repo edit (see TODO/problems.org, \"skg-set-repo
silently no-ops on write-protected instances\") -- and its ID is
collected into WRITE-PROTECTED-IDS instead, for the caller to warn about.
Its org-descendents are still traversed: they are self-writers, so
their repo edits take effect even under a write-protected parent."
  (save-excursion
    (let* ((changed-count 0)
           (write-protected-ids '())
           (start-level (org-outline-level))
           (change-or-collect
            (lambda ()
              (let ((meta (skg--metadata-sexp-at-point-or-nil)))
                (if (skg--node-write-protected-p meta)
                    (push (or (skg--node-id meta) "(no id)")
                          write-protected-ids)
                  (setq changed-count
                        (+ changed-count
                           (skg--change-repo-at-point new-repo))))))))
      (funcall change-or-collect) ;; the root
      (outline-next-heading)
      (while (and (not (eobp))
                  (> (org-outline-level) start-level))
        (let ((metadata-sexp (skg--metadata-sexp-at-point-or-nil)))
          (if (not (and (skg--activeNode-sexp-p metadata-sexp)
                        (skg--node-affectsParent-content-of-p metadata-sexp)))
              (skg--goto-next-heading-after-subtree)
            (when (equal (skg--node-repo metadata-sexp) old-repo)
              (funcall change-or-collect))
            (outline-next-heading))))
      (cons changed-count (nreverse write-protected-ids)))))

(defun skg--goto-next-heading-after-subtree ()
  "Move to the next heading after the current subtree."
  (let ((prune-level (org-outline-level)))
    (outline-next-heading)
    (while (and (not (eobp))
                (> (org-outline-level) prune-level))
      (outline-next-heading))))

(defun skg--metadata-sexp-at-point-or-nil ()
  "Return the parsed skg metadata sexp for the headline at point, or nil."
  (when (org-at-heading-p)
    (let* ((headline (skg-get-current-headline-text))
           (split (skg-split-as-stars-metadata-title headline))
           (metadata-str (and split (cadr split))))
      (when (and metadata-str
                 (not (string-empty-p metadata-str)))
        (read metadata-str)))))

(defun skg--activeNode-sexp-p (metadata-sexp)
  "Return non-nil if METADATA-SEXP describes an ActiveNode."
  (and metadata-sexp
       (skg-sexp-subtree-p metadata-sexp '(skg (node)))))

(defun skg--node-affectsParent-content-of-p (metadata-sexp)
  "Return non-nil if METADATA-SEXP has implicit or explicit affectsParent=true."
  (let ((affectsParent-values (skg-sexp-cdr-at-path metadata-sexp
                                            '(skg node affectsParent))))
    (or (not affectsParent-values)
        (eq (car affectsParent-values) 'true))))

(defun skg--node-repo (metadata-sexp)
  "Return METADATA-SEXP's node repo as a string, or nil."
  (let ((repo-values (skg-sexp-cdr-at-path metadata-sexp
                                             '(skg node repo))))
    (when repo-values
      (format "%s" (car repo-values)))))

(defun skg--node-id (metadata-sexp)
  "Return METADATA-SEXP's node ID as a string, or nil."
  (let ((id-values (skg-sexp-cdr-at-path metadata-sexp
                                         '(skg node id))))
    (when id-values
      (format "%s" (car id-values)))))

(defun skg--unknown-headline-p (metadata-sexp)
  "Return non-nil when METADATA-SEXP is an Unknown placeholder."
  (and metadata-sexp
       (skg-sexp-subtree-p metadata-sexp '(skg (unknown)))))

(defun skg--relationship-member-id (metadata-sexp)
  "Return the raw member ID for an ActiveNode or Unknown headline."
  (or (skg--node-id metadata-sexp)
      (let ((id-values (skg-sexp-cdr-at-path metadata-sexp
                                              '(skg unknown id))))
        (when id-values
          (format "%s" (car id-values))))))

(defun skg--node-write-protected-p (metadata-sexp)
  "Return non-nil if METADATA-SEXP has the bare ActiveNode writeProtected marker."
  (skg-sexp-subtree-p metadata-sexp '(skg (node writeProtected))))

(defun skg--change-repo-at-point (new-repo)
  "Set the repo at point to NEW-REPO.
Returns 1 if the current line was edited."
  (skg-edit-metadata-at-point
   `(skg (node (ENSURE (repo ,(intern new-repo)))
               (viewStats))))
  (skg-edit-metadata-at-point
   `(skg (node (viewStats
                (ENSURE
                 (homeRepoHerald ,(intern
                                  (format "⌂:%s" new-repo))))))))
  1)

(defun skg-parse-headline-metadata (headline-text)
  "Parse skg metadata from HEADLINE-TEXT after org bullets.
Returns (METADATA-ALIST BARE-VALUES-SET TITLE-TEXT) or nil if no metadata found.
METADATA-ALIST contains key-value pairs, BARE-VALUES-SET contains standalone values."
  (let ((trimmed (string-trim-left headline-text)))
    (when (string-prefix-p "(skg" trimmed)
      (let ((sexp-end-pos (skg-find-sexp-end trimmed)))
        (when sexp-end-pos
          (let* ((skg-sexp (substring trimmed 0 sexp-end-pos))
                 (title-start sexp-end-pos)
                 (len (length trimmed))
                 (title (string-trim (if (< title-start len)
                                         (substring trimmed title-start)
                                       "")))
                 (parsed (skg-parse-metadata-sexp skg-sexp)))
            (list (car parsed) (cadr parsed) title)))))))

(defun skg-delete-kv-pair-from-metadata-by-key
    (key)
  "Delete all kv-pairs with KEY from the metadata of the headline at point.
If the current line is not a headline, or has no metadata, no effect.
Routes the rewrite through `skg-replace-current-line' so that editing
a folded heading does not fire org-fold's fragility check — see the
\"Programmatic metadata edits must be performed ignoring fragility
checks\" entry in PITFALLs.org."
  (when (org-at-heading-p)
    (let* ((headline-text (skg-get-current-headline-text))
           (match-result (skg-split-as-stars-metadata-title
                          headline-text)))
      (when (and match-result
                 (string-match-p "(skg" headline-text))
        (let* ((stars (nth 0 match-result))
               (metadata-sexp (nth 1 match-result))
               (title (nth 2 match-result))
               (parsed (skg-parse-metadata-sexp metadata-sexp))
               (alist (car parsed))
               (bare-values (cadr parsed))
               (filtered-alist (seq-filter
                                (lambda (kv)
                                  (not (string-equal (car kv) key)))
                                alist))
               (new-metadata-sexp (skg-reconstruct-metadata-sexp
                                   filtered-alist bare-values)))
          (skg-replace-current-line
           (skg-format-headline stars new-metadata-sexp title)))))))

(defun skg-delete-value-from-metadata
    (value)
  "Delete all instances of VALUE from the metadata of the headline at point.
If the current line is not a headline, or has no metadata, no effect.
Routes the rewrite through `skg-replace-current-line' so that editing
a folded heading does not fire org-fold's fragility check — see the
\"Programmatic metadata edits must be performed ignoring fragility
checks\" entry in PITFALLs.org."
  (when (org-at-heading-p)
    (let* ((headline-text (skg-get-current-headline-text))
           (match-result (skg-split-as-stars-metadata-title
                          headline-text)))
      (when (and match-result
                 (string-match-p "(skg" headline-text))
        (let* ((stars (nth 0 match-result))
               (metadata-sexp (nth 1 match-result))
               (title (nth 2 match-result))
               (parsed (skg-parse-metadata-sexp metadata-sexp))
               (alist (car parsed))
               (bare-values (cadr parsed))
               (filtered-values
                (seq-filter (lambda (v)
                              (not (string-equal v value)))
                            bare-values))
               (new-metadata-sexp (skg-reconstruct-metadata-sexp
                                   alist filtered-values)))
          (skg-replace-current-line
           (skg-format-headline stars new-metadata-sexp title)))))))

(defun skg--remove-fork-from-viewrequests (host-sexp)
  "Return HOST-SEXP with the `fork' symbol removed from its node's
\(viewRequests ...) form, dropping that form entirely if it becomes empty.
HOST-SEXP is a (skg (node ...) ...) sexp. The result is `equal' to
HOST-SEXP when there was no fork request, so callers can detect a no-op."
  (cons
   'skg
   (mapcar
    (lambda (elem)
      (if (and (consp elem) (eq (car elem) 'node))
          (cons 'node
                (delq nil
                      (mapcar
                       (lambda (ne)
                         (if (and (consp ne) (eq (car ne) 'viewRequests))
                             (let ((kept (delq 'fork
                                               (copy-sequence (cdr ne)))))
                               (and kept (cons 'viewRequests kept)))
                           ne))
                       (cdr elem))))
        elem))
    (cdr host-sexp))))

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
a folded heading does not fire org-fold's fragility check — see the
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
                 (merged-sexp (skg-edit-nested-sexp
                               host-sexp edits)))
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

(defun skg-replace-current-line (new-content)
  "Replace the current line with NEW-CONTENT.
Moves to beginning of line, deletes the line, and inserts NEW-CONTENT.
The `delete-region' + `insert' pair below runs inside
`org-fold-core-ignore-fragility-checks' to protect against corrupting
a folded subtree beneath the edited heading line — see the
\"Programmatic metadata edits must be performed ignoring fragility
checks\" entry in PITFALLs.org for the full explanation. Do not
bypass this helper when rewriting a heading line in place; call it
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

(defun skg-parse-metadata-sexp (metadata-sexp)
  "Parse METADATA-SEXP string containing (skg ...) s-expression.
Returns (ALIST SET) where ALIST contains (key value) pairs and SET contains bare values."
  (let ((alist '())
        (set '()))
    (when (and metadata-sexp
               (not (string-empty-p metadata-sexp)))
      (with-temp-buffer
        (insert metadata-sexp)
        (goto-char (point-min))
        (condition-case nil
            (let* ((sexp (read (current-buffer)))
                   (elements (cdr sexp))) ;; Skip 'skg symbol
              (dolist (element elements)
                (cond
                 (;; (key value) pair
                  (and (listp element)
                       (= (length element) 2))
                  (let ((key (format "%s" (car element)))
                        (val (format "%s" (cadr element))))
                    (push (cons key val) alist)))
                 (;; Special case: (graphStats ...) sub-s-expr
                  (and (listp element)
                       (> (length element) 1)
                       (eq (car element) 'graphStats))
                  (let ((graphstats-sexp (format "%S" element)))
                    (push (cons "graphStats" graphstats-sexp) alist)))
                 (;; Special case: (viewStats ...) sub-s-expr
                  (and (listp element)
                       (> (length element) 1)
                       (eq (car element) 'viewStats))
                  (let ((viewstats-sexp (format "%S" element)))
                    (push (cons "viewStats" viewstats-sexp) alist)))
                 (t ;; Bare value (symbol or other atom)
                  (let ((bare-val (format "%s" element)))
                    (push bare-val set))))))
          (error nil))))
    (list (nreverse alist) (nreverse set))))

(defun skg-reconstruct-metadata-sexp
    (alist bare-values)
  "Reconstruct complete (skg ...) metadata s-expression from ALIST and BARE-VALUES.
Returns a string containing the complete s-expression.
Key-value pairs are formatted as (key value),
except 'graphStats' and 'viewStats' which are already complete s-expressions."
  (let ((parts '()))
    (dolist (kv alist)
      (if (or (string-equal (car kv) "graphStats")
              (string-equal (car kv) "viewStats"))
          ;; graphStats/viewStats value is already a complete sexp string
          (push (cdr kv) parts)
        ;; Regular key-value pair
        (push (format "(%s %s)" (car kv) (cdr kv))
              parts)))
    (dolist (val bare-values)
      (push val parts))
    (if (null parts)
        "(skg)"
      (format "(skg %s)"
              (mapconcat #'identity (nreverse parts) " ")))))

(defun skg-format-headline
    (stars metadata-sexp title)
  "Format a headline with STARS, METADATA-SEXP, and TITLE.
METADATA-SEXP should be the complete (skg ...) s-expression,
OR the empty string.
Handles empty metadata correctly."
  (if (string-empty-p metadata-sexp)
      (format "%s(skg) %s" stars title)
    (format "%s%s %s" stars metadata-sexp title)))

(defun skg--around-org-todo (orig-fn &rest args)
  "Around advice for `org-todo'.
Strip (skg ...) metadata before cycling, re-insert after."
  (let* ((on-heading (org-at-heading-p))
         (parts (when on-heading
                  (save-excursion
                    (beginning-of-line)
                    (skg-split-as-stars-metadata-title
                     (buffer-substring-no-properties
                      (line-beginning-position)
                      (line-end-position))))))
         (has-metadata (and parts
                            (not (string-empty-p (nth 1 parts))))))
    (when has-metadata
      ;; Remove metadata from the line so org sees a plain heading.
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
