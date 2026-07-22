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
aliascol, id-col, etc.) are skipped. Does NOT save;
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

(defun skg-set-indefinitive ()
  "Mark the headline at point as indefinitive.
Edits the metadata to include `indef` in the `node` section.
Does NOT save; call `skg-request-save-buffer' afterward."
  (interactive)
  (skg-edit-metadata-at-point '(skg (node indef))))

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

(defun skg--current-node-source ()
  "Return the source string for the ActiveNode headline at point."
  (let* ((sexp (skg--current-headline-metadata-sexp))
         (source-values (skg-sexp-cdr-at-path sexp '(skg node source))))
    (unless source-values
      (user-error "Node has no source"))
    (format "%s" (car source-values))))

(defun skg--headline-metadata-empty-p ()
  "Return non-nil if the headline at point has no skg metadata."
  (let* (( headline (skg-get-current-headline-text) )
         ( split (skg-split-as-stars-metadata-title headline) ))
    (or (null split)
        (string-empty-p (cadr split)))))

(defun skg--populate-minimal-node-metadata ()
  "Write minimal ActiveNode metadata onto the metadata-less headline at point.
Prompts for an owned source (no prompt when only one source is owned)
and inserts (skg (node (source SOURCE))).  Returns the chosen source.
Reuses `skg-edit-metadata-at-point', which formats and spaces the sexp
correctly relative to the existing title."
  (let (( source (skg--prompt-for-owned-source) ))
    (skg-edit-metadata-at-point
     `(skg (node (source ,(intern source)))))
    source))

(defun skg-set-source (&optional recursive)
  "Prompt for and change the source of the node at point.
Starts with the current source as minibuffer text.  S-left/S-right cycle
through owned sources, C-? displays all configured sources and
their paths, and typed source names are accepted directly.
With a prefix argument RECURSIVE, changes every affected content
descendent whose source matches the source at point.
Only descendents for which parentIs=affected are traversed.
On a headline that has no metadata yet, instead populates it minimally
via `skg--populate-minimal-node-metadata' (RECURSIVE is then moot).

When the move would leave content relationships stuck at their old,
more private levels (the sticky rule never lowers an edge's privacy
without an explicit gesture; see
TODO/MAYBE-BUG_recursive-move-to-more-public-leaves-relations-private.org),
offers to publicize them in the same go by writing `(relSource ...)'
atoms; declining leaves them and mentions that
`skg-set-relationship-source-recursive' (C-c s R) can publicize them
later.

Indefinitive instances are NOT changed -- the save would silently
ignore their source edits -- and produce a loud warning, with the
full ID list in *Messages*.

Does NOT save; call `skg-request-save-buffer' afterward."
  (interactive "P")
  (if (skg--headline-metadata-empty-p)
      (skg--populate-minimal-node-metadata)
    (let* ((current-source (skg--current-node-source))
           (new-source (string-trim
                        (skg--prompt-for-source-change current-source))))
      (unless (string-empty-p new-source)
        (skg--validate-source-name new-source)
        (if (string= current-source new-source)
            (message "Source unchanged: %s" current-source)
          (skg--set-source-and-handle-stuck-edges
           current-source new-source recursive))))))

(defun skg-set-source-recursive ()
  "Prompt for and recursively change the source of the node at point.
This is the recursive form of `skg-set-source': it changes every
content-descendent whose source matches the source at point.
Only descendents for which parentIs=affected are traversed.
Does NOT save; call `skg-request-save-buffer' afterward."
  (interactive)
  (skg-set-source t))

(defun skg--set-source-and-handle-stuck-edges (old-source new-source recursive)
  "The body of `skg-set-source' once a real move is requested:
analyze which content edges the move would leave stuck at more
private levels, retarget the sources (skipping indefinitive
instances), offer to publicize the stuck edges in the same go, and
report -- loudly, when indefinitive instances were skipped."
  (let* ((stuck ;; analyzed BEFORE any rewrite: it needs the old sources
          (skg--analyze-move-stuck-edges old-source new-source recursive))
         (change-result (if recursive
                            (skg--change-source-recursive old-source
                                                          new-source)
                          (skg--change-source-at-point-unless-indefinitive
                           new-source)))
         (changed-count (car change-result))
         (indef-ids (cdr change-result))
         (fixed-count
          (when (and stuck
                     (y-or-n-p
                      (format "This move would leave %d content relationship%s at their old, more private level%s. Publicize them too? "
                              (length stuck)
                              (if (= (length stuck) 1) "" "s")
                              (if (= (length stuck) 1) "" "s"))))
            (skg--apply-stuck-edge-levels stuck))))
    (dolist (id indef-ids)
      (message "skg-set-source: indefinitive instance NOT changed (the save would ignore it): %s"
               id))
    (message "%s"
             (concat
              (format "Source changed from %s to %s on %d node%s. Save to apply."
                      old-source new-source changed-count
                      (if (= changed-count 1) "" "s"))
              (cond
               (fixed-count
                (format " Also publicized %d relationship%s."
                        fixed-count (if (= fixed-count 1) "" "s")))
               (stuck
                " Relationships kept their old, more private levels; C-c s R can publicize them later."))
              (when indef-ids
                (format "  WARNING: %d indefinitive node%s NOT changed -- the save would silently ignore them. See *Messages* for the ID list."
                        (length indef-ids)
                        (if (= (length indef-ids) 1) "" "s")))))))

(defun skg--analyze-move-stuck-edges (old-source new-source recursive)
  "With point on the node a `skg-set-source' move starts from, and
BEFORE any source is rewritten: return the affected content edges
the move would leave stuck at a more private level than their new
default, as a list of (MARKER . LEVEL) -- MARKER at the child
headline, LEVEL the edge's new default. Only edges without an
existing `(relSource ...)' atom qualify: an atom-carrying edge was
already leveled deliberately. The walk's root itself is always
retargeted (unless indefinitive); its org-parent lies outside the
move, so its source counts as unchanging. With RECURSIVE nil only
the point node moves, so only its own edge and its direct
children's edges are examined."
  (save-excursion
    (let* ((stuck '())
           (start-level (org-outline-level))
           (consider ;; point on a candidate child C, whose inbound edge is examined; the arguments say whether each endpoint's source is about to be retargeted
            (lambda (parent-retargets-p child-retargets-p)
              (when (skg--relationship-kind-matches-p 'contained)
                (let* ((child-meta (skg--metadata-sexp-at-point-or-nil))
                       (child-source (skg--node-source child-meta))
                       (child-moves ;; an indefinitive instance is skipped by the retargeting walk, so its source does not actually change
                        (and child-retargets-p
                             (not (skg--node-indefinitive-p child-meta))))
                       (parent-source
                        (save-excursion
                          (org-up-heading-safe)
                          (skg--node-source
                           (skg--metadata-sexp-at-point-or-nil))))
                       (eff (lambda (source retargets-p)
                              (if (and retargets-p
                                       (equal source old-source))
                                  new-source
                                source)))
                       (level (skg--content-edge-stuck-level
                               parent-source
                               (funcall eff parent-source
                                        parent-retargets-p)
                               child-source
                               (funcall eff child-source child-moves))))
                  (when level
                    (push (cons (copy-marker (line-beginning-position))
                                level)
                          stuck)))))))
      (funcall consider nil t) ;; the root's own inbound edge
      (outline-next-heading)
      (while (and (not (eobp))
                  (> (org-outline-level) start-level))
        (let ((meta (skg--metadata-sexp-at-point-or-nil)))
          (if (not (and (skg--activeNode-sexp-p meta)
                        (skg--node-parentIs-content-of-p meta)))
              (skg--goto-next-heading-after-subtree)
            (if recursive
                (funcall consider t t)
              (when (= (org-outline-level) (1+ start-level))
                ;; Single move: only the point node moves, so only
                ;; its direct children's inbound edges can change.
                (funcall consider t nil)))
            (outline-next-heading))))
      (nreverse stuck))))

(defun skg--content-edge-stuck-level (parent-eff-old parent-eff-new
                                      child-eff-old child-eff-new)
  "The level to which the content edge at point (from its view-parent
to the headline at point) should be publicized after a source move,
or nil when the move does not strand it: nil when the edge carries
an explicit `(relSource ...)' atom (deliberately leveled), when a
default cannot be computed (a source unknown to the config -- the
save validates anyway), or when the edge's default does not become
more public. The four arguments are the endpoints' sources before
and after the move."
  (unless (skg--relationship-source-current-value)
    (let ((old-default (skg--more-private-of-sources
                        parent-eff-old child-eff-old))
          (new-default (skg--more-private-of-sources
                        parent-eff-new child-eff-new)))
      (when (and old-default new-default
                 (skg--strictly-more-public-source-p new-default
                                                     old-default))
        new-default))))

(defun skg--apply-stuck-edge-levels (stuck)
  "Write a `(relSource LEVEL)' atom at each (MARKER . LEVEL) in
STUCK, then free the markers. Returns the number of atoms written."
  (save-excursion
    (dolist (entry stuck)
      (goto-char (car entry))
      (skg--apply-relationship-source-choice (cdr entry))
      (set-marker (car entry) nil))
    (length stuck)))

(defun skg--change-source-at-point-unless-indefinitive (new-source)
  "Set the source at point to NEW-SOURCE, unless the instance is
indefinitive -- the save would silently ignore that edit. Returns
(CHANGED-COUNT . INDEF-IDS), matching `skg--change-source-recursive'."
  (let ((meta (skg--metadata-sexp-at-point-or-nil)))
    (if (skg--node-indefinitive-p meta)
        (cons 0 (list (or (skg--node-id meta) "(no id)")))
      (cons (skg--change-source-at-point new-source) nil))))

(defun skg--more-private-of-sources (a b)
  "The more private of sources A and B per the config's privacy
order (later in the ladder = more private), or nil when either
names no configured source."
  (let ((pa (skg--source-privacy-position a))
        (pb (skg--source-privacy-position b)))
    (when (and pa pb)
      (if (> pa pb) a b))))

(defun skg--strictly-more-public-source-p (a b)
  "Non-nil iff source A is strictly more public than source B per
the config's privacy order. Nil when either is unknown."
  (let ((pa (skg--source-privacy-position a))
        (pb (skg--source-privacy-position b)))
    (and pa pb (< pa pb))))

(defun skg--source-privacy-position (source)
  "SOURCE's index in the config's privacy order (0 = most public),
or nil when SOURCE is nil or names no configured source."
  (and source
       (seq-position (skg--source-names) source #'string=)))

(defconst skg--set-relationship-source-readonly-col-atoms
  '(subscriberCol overriderCol hiderCol hiddenCol
    hiddenInSubscribeeCol hiddenOutsideOfSubscribeeCol)
  "The PartnerCol scaffold atoms whose membership is READ-ONLY from
this side of the relationship (ColPolicy::ReadOnlySet /
ReadOnlyFilter; see server/types/viewnode.rs PartnerCol::policy and
the matching read-only detection in
server/from_text/local_instruction_collection/traverse.rs).
`skg-set-relationship-source' refuses on a member of one of these:
the edge belongs to the other end, so setting its level here would
be meaningless.")

(defconst skg--writable-col-relations
  '((subscribeeCol . "subscribes_to")
    (overriddenCol . "overrides_view_of"))
  "The PartnerCol scaffold atoms whose members' edges are WRITABLE
from this side, each mapped to its relation's wire name
(NodeRelation::typeql_name, server/dbs/in_rust_graph/
relation_accessors.rs). The col's org-parent (the anchor) owns the
outbound edge to each member.")

(defun skg--relationship-edge-at-point ()
  "Classify the relationship edge the headline at point represents.
Returns a plist (:owner OWNER-ID :member MEMBER-ID :relation NAME):
for a content child, the org-parent contains the node at point; for
a writable-col member, the col's anchor (the col's org-parent) owns
the col's relation toward the node at point. Signals `user-error'
when point represents no writable edge: not on an activeNode
headline, on a root headline (no org-parent, so no edge), on a
member of a read-only col, or with an ID missing."
  (unless (org-at-heading-p)
    (user-error "Not on a headline"))
  (let ((member-sexp (skg--metadata-sexp-at-point-or-nil)))
    (unless (skg--activeNode-sexp-p member-sexp)
      (user-error "Not on an activeNode headline"))
    (let ((member-id (skg--node-id member-sexp))
          (parent-sexp (save-excursion
                         (and (org-up-heading-safe)
                              (skg--metadata-sexp-at-point-or-nil)))))
      (unless member-id
        (user-error "No id in this headline's metadata"))
      (unless parent-sexp
        (user-error
         "Root headline: there is no relationship edge here to set"))
      (let ((readonly-atom
             (and (consp parent-sexp)
                  (seq-find (lambda (atom)
                              (memq atom (cdr parent-sexp)))
                            skg--set-relationship-source-readonly-col-atoms))))
        (when readonly-atom
          (user-error
           "Cannot set the relationship's source: this is a member of a read-only %s -- the relationship belongs to the other end"
           readonly-atom)))
      (let ((writable-col
             (and (consp parent-sexp)
                  (seq-find (lambda (entry)
                              (memq (car entry) (cdr parent-sexp)))
                            skg--writable-col-relations))))
        (cond
         (writable-col
          (let ((anchor-id
                 (save-excursion
                   (and (org-up-heading-safe) ;; to the col
                        (org-up-heading-safe) ;; to its anchor
                        (skg--node-id
                         (skg--metadata-sexp-at-point-or-nil))))))
            (unless anchor-id
              (user-error "Could not find the col's anchor headline"))
            (list :owner anchor-id
                  :member member-id
                  :relation (cdr writable-col))))
         ((skg--activeNode-sexp-p parent-sexp)
          (let ((parent-id (skg--node-id parent-sexp)))
            (unless parent-id
              (user-error "No id in the parent headline's metadata"))
            (list :owner parent-id
                  :member member-id
                  :relation "contains")))
         (t (user-error
             "The parent headline is neither a node nor a writable col")))))))

(defun skg--relationship-source-current-value ()
  "Return the current `(relSource NAME)' value (a string) for the
headline at point, or nil when no such atom is present."
  (let ((values (skg-sexp-cdr-at-path
                 (or (skg--metadata-sexp-at-point-or-nil) '(skg))
                 '(skg node viewStats relSource))))
    (when values
      (format "%s" (car values)))))

(defun skg--relationship-source-choices (ladder default)
  "The source-name menu for `skg-set-relationship-source': the tail
of LADDER (the configured sources, most public first) starting at
DEFAULT -- exactly the levels the save's default floor can accept.
When DEFAULT is nil or absent from LADDER, the whole LADDER (the
server's save-time floor check backstops any stale offer)."
  (or (and default (member default ladder))
      ladder))

(defconst skg--relationship-source-no-override
  "(no override: follow sticky-else-default)"
  "The menu entry that REMOVES the `(relSource ...)' atom instead of
setting one. For an edge already on disk this means the SAVED level
survives (sticky); it does NOT mean \"reset to the default\". To
lower an edge's privacy to its default, choose the default level
explicitly.")

(defun skg--apply-relationship-source-choice (choice)
  "Apply CHOICE -- a source name, or
`skg--relationship-source-no-override' -- to the headline at point.
Edits only the buffer; returns a message string describing what the
next save will do with the edge."
  (if (equal choice skg--relationship-source-no-override)
      (if (skg--relationship-source-current-value)
          (progn
            ;; A relSource atom exists, so viewStats is present and
            ;; the DELETE can find it.
            (skg-edit-metadata-at-point
             '(skg (node (viewStats (DELETE (relSource))))))
            "Override removed: on save the edge keeps its saved (sticky) level, or its default if new. Save to apply.")
        "No override present; nothing to remove.")
    (progn
      ;; Two-step dance (mirrors `skg--change-source-at-point'):
      ;; the DSL's merge operation appends an unprocessed literal
      ;; when no existing (viewStats ...) form is found, so an
      ;; empty (viewStats) placeholder must exist FIRST, in its
      ;; own edit, before the ENSURE can find and recurse into it.
      (skg-edit-metadata-at-point '(skg (node (viewStats))))
      (skg-edit-metadata-at-point
       `(skg (node (viewStats (ENSURE (relSource ,(intern choice)))))))
      (format "Relationship source set to '%s'. Save to apply."
              choice))))

(defconst skg--relationship-kind-menu-tree
  '(("contains"
     ("container" nil
      "The node would CONTAIN its view-parent -- the shape of a containerward ancestry graft. The edge belongs to the graft's own contains list, wherever that list is drawn definitively; it cannot be set from the graft's position.")
     ("contained" contained
      "The view-parent contains the node: ordinary content. Sets the level of each parent-contains-child edge."))
    ("textlinks_to"
     ("source" nil
      "Textlinks are inferred from body text; they carry no independent privacy level, so there is nothing to set.")
     ("dest" nil
      "Textlinks are inferred from body text; they carry no independent privacy level, so there is nothing to set."))
    ("subscribes"
     ("subscriber" nil
      "A subscriberCol member: the subscribes edge belongs to the member (the subscriber), not to the view-parent. Read-only from here.")
     ("subscribee" subscribee
      "A member of the view-parent's subscribeeCol. Sets the level of each anchor-subscribes-to-member edge."))
    ("hides_from_its_subscriptions"
     ("hider" nil
      "Hide levels are derived at save, floored at the most public explaining subscription; the hiderCol is read-only.")
     ("hidden" nil
      "Hide levels are derived at save, floored at the most public explaining subscription; the hiddenCol is read-only."))
    ("overrides_view_of"
     ("overrider" nil
      "An overriderCol member: the overrides edge belongs to the member (the overrider), not to the view-parent. Read-only from here.")
     ("overridden" overridden
      "A member of the view-parent's overriddenCol. Sets the level of each anchor-overrides-view-of-member edge.")))
  "The relationship-kind menu for
`skg-set-relationship-source-recursive': one entry per node-node
relation in schema.tql, each listing its two roles as
(ROLE-NAME KIND-OR-NIL DESCRIPTION). ROLE-NAME is the role the
VIEW-CHILD would play toward its view-parent. KIND-OR-NIL is the
symbol the walk dispatches on (`contained', `subscribee' or
`overridden') for the three roles whose edge is writable from the
child's buffer position, and nil for the rest; DESCRIPTION then
explains why the edge cannot be set from that position.")

(defvar-local skg--relationship-kind-menu-continuation nil
  "The continuation `skg--select-relationship-kind' stores in its
menu buffer, called with the chosen kind symbol.")

(defun skg-set-relationship-source-recursive ()
  "Set the privacy level of every matching relationship edge in the
subtree at point.

First presents an org-menu of the schema's five node-node relations
(level-1 headlines) and their roles (level-2 headlines); pick, with
RET, the role the view-CHILDREN should play toward their
view-parents. Only three roles are settable from the child's
position: `contained' (ordinary content), `subscribee' (a
subscribeeCol member) and `overridden' (an overriddenCol member);
RET on any other role explains why it cannot be set from there.

Then prompts for a level over the whole ladder, plus the
no-override choice that instead REMOVES existing `(relSource ...)'
atoms. Unlike `skg-set-relationship-source', no per-edge default is
fetched: the subtree's edges have different defaults, so the save's
floor check (see `apply_sticky_levels') is what validates each one.

The walk starts at the node at point (inclusive: its own edge to
its view-parent counts when it matches) and recurses only on
viewchildren that affect their viewparents: parentIs=affected
activeNodes and writable cols. It prunes below indefinitive nodes
and subscribee-as-such members (their org-children's edges are not
collected at save), and prunes read-only cols and other scaffolds
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
         (user-error "skg: buffer vanished before the relationship-level prompt"))
       (with-current-buffer buffer
         (save-excursion
           (goto-char marker)
           (let* ((ladder (skg--source-names))
                  (choices (append ladder
                                   (list skg--relationship-source-no-override)))
                  (choice (skg--completing-read-with-cycle
                           (format "Level for every '%s' edge in the subtree (S-left/right cycle; the save validates each edge's floor): "
                                   kind)
                           choices nil t nil nil nil nil choices))
                  (count (skg--set-relationship-source-recursive-walk
                          kind choice)))
             (message "%s"
                      (if (equal choice
                                 skg--relationship-source-no-override)
                          (format "Override removed on %d '%s' edge%s: on save each keeps its saved (sticky) level, or its default if new. Save to apply."
                                  count kind (if (= count 1) "" "s"))
                        (format "Relationship source set to '%s' on %d '%s' edge%s. Save to apply."
                                choice count kind
                                (if (= count 1) "" "s")))))))))))

(defun skg--select-relationship-kind (continuation)
  "Pop up the org-menu over `skg--relationship-kind-menu-tree'.
RET on a settable role headline buries the menu and calls
CONTINUATION with the role's kind symbol; RET on a read-only role
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

(defun skg--set-relationship-source-recursive-walk (kind choice)
  "Apply CHOICE (a source name, or
`skg--relationship-source-no-override') to every headline in the
subtree at point, the headline at point included, whose relationship
to its view-parent is of KIND (`contained', `subscribee' or
`overridden'; see `skg--relationship-kind-matches-p'). Recurses only
where edits still affect the graph: it prunes below indefinitive
nodes and subscribee-as-such members, prunes non-affected
(parentIs=independent) nodes -- except the walk's root, which the
user chose deliberately -- and prunes scaffolds other than the two
writable cols. Returns the number of edges affected."
  (save-excursion
    (let ((count 0)
          (start-level (org-outline-level))
          (root-meta (skg--metadata-sexp-at-point-or-nil)))
      (when (skg--relationship-kind-matches-p kind)
        (skg--apply-relationship-source-choice choice)
        (setq count (1+ count)))
      (when (or (and (skg--activeNode-sexp-p root-meta)
                     (not (skg--relSource-prune-below-p root-meta)))
                (skg--writable-col-sexp-p root-meta))
        (outline-next-heading)
        (while (and (not (eobp))
                    (> (org-outline-level) start-level))
          (let ((meta (skg--metadata-sexp-at-point-or-nil)))
            (cond
             ((skg--activeNode-sexp-p meta)
              (if (not (skg--node-parentIs-content-of-p meta))
                  (skg--goto-next-heading-after-subtree)
                (when (skg--relationship-kind-matches-p kind)
                  (skg--apply-relationship-source-choice choice)
                  (setq count (1+ count)))
                (if (skg--relSource-prune-below-p meta)
                    (skg--goto-next-heading-after-subtree)
                  (outline-next-heading))))
             ((skg--writable-col-sexp-p meta)
              (outline-next-heading))
             (t ;; read-only cols, alias/ID cols, phantoms, etc.
              (skg--goto-next-heading-after-subtree))))))
      count)))

(defun skg--relationship-kind-matches-p (kind)
  "Non-nil iff the headline at point is an affected activeNode whose
relationship to its view-parent is of KIND, writable-and-collected
from this position: for `contained', the view-parent must be a
definitive activeNode not in subscribee-as-such position (an
indefinitive or subscribee-as-such parent's contains is not
collected at save, so a `(relSource ...)' atom under one would be
inert); for `subscribee' and `overridden', the view-parent must be
the matching writable col with a definitive anchor."
  (let ((meta (skg--metadata-sexp-at-point-or-nil)))
    (and (skg--activeNode-sexp-p meta)
         (skg--node-parentIs-content-of-p meta)
         (save-excursion
           (and (org-up-heading-safe)
                (let ((parent-sexp (skg--metadata-sexp-at-point-or-nil)))
                  (cond
                   ((skg--activeNode-sexp-p parent-sexp)
                    (and (eq kind 'contained)
                         (not (skg--node-indefinitive-p parent-sexp))
                         (not (skg--subscribee-as-such-at-point-p))))
                   ((skg--scaffold-atom-present-p parent-sexp
                                                  'subscribeeCol)
                    (and (eq kind 'subscribee)
                         (skg--col-anchor-definitive-p)))
                   ((skg--scaffold-atom-present-p parent-sexp
                                                  'overriddenCol)
                    (and (eq kind 'overridden)
                         (skg--col-anchor-definitive-p)))
                   (t nil))))))))

(defun skg--relSource-prune-below-p (metadata-sexp)
  "Non-nil iff the walk should not descend below the activeNode
headline at point (with METADATA-SEXP its parsed metadata): an
indefinitive node's contains is not collected at save, and a
subscribee-as-such member's org-children are hide/unhide signals,
not writable edges."
  (or (skg--node-indefinitive-p metadata-sexp)
      (skg--subscribee-as-such-at-point-p)))

(defun skg--subscribee-as-such-at-point-p ()
  "Non-nil iff the headline at point sits in subscribee-as-such
position: an affected activeNode member of a subscribeeCol."
  (let ((meta (skg--metadata-sexp-at-point-or-nil)))
    (and (skg--activeNode-sexp-p meta)
         (skg--node-parentIs-content-of-p meta)
         (save-excursion
           (and (org-up-heading-safe)
                (skg--scaffold-atom-present-p
                 (skg--metadata-sexp-at-point-or-nil)
                 'subscribeeCol))))))

(defun skg--col-anchor-definitive-p ()
  "Non-nil iff the col headline at point has a definitive activeNode
anchor (its org-parent). An indefinitive anchor's writable cols are
not collected at save (the col owner is not save-eligible), so
atoms on their members would be inert."
  (save-excursion
    (and (org-up-heading-safe)
         (let ((anchor-sexp (skg--metadata-sexp-at-point-or-nil)))
           (and (skg--activeNode-sexp-p anchor-sexp)
                (not (skg--node-indefinitive-p anchor-sexp)))))))

(defun skg--scaffold-atom-present-p (metadata-sexp atom)
  "Non-nil iff METADATA-SEXP is a (skg ...) sexp carrying the bare ATOM."
  (and (consp metadata-sexp)
       (memq atom (cdr metadata-sexp))))

(defun skg--writable-col-sexp-p (metadata-sexp)
  "Non-nil iff METADATA-SEXP is a writable-col scaffold's metadata:
it carries one of the `skg--writable-col-relations' atoms."
  (and (consp metadata-sexp)
       (seq-find (lambda (entry)
                   (memq (car entry) (cdr metadata-sexp)))
                 skg--writable-col-relations)
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

(defun skg--validate-source-name (source)
  "Signal an error if SOURCE cannot be represented in skg metadata."
  (when (string-match-p "[[:space:]]" source)
    (user-error "Source names cannot contain whitespace")))

(defun skg--change-source-recursive (old-source new-source)
  "Change OLD-SOURCE to NEW-SOURCE in this content subtree.
Returns (CHANGED-COUNT . INDEF-IDS).  The root node is inclusive;
only descendents for which parentIs=affected are traversed.
An indefinitive instance is NOT edited -- the save would silently
ignore its source edit (see TODO/problems.org, \"skg-set-source
silently no-ops on indefinitive instances\") -- and its ID is
collected into INDEF-IDS instead, for the caller to warn about.
Its org-descendents are still traversed: they are self-writers, so
their source edits take effect even under an indefinitive parent."
  (save-excursion
    (let* ((changed-count 0)
           (indef-ids '())
           (start-level (org-outline-level))
           (change-or-collect
            (lambda ()
              (let ((meta (skg--metadata-sexp-at-point-or-nil)))
                (if (skg--node-indefinitive-p meta)
                    (push (or (skg--node-id meta) "(no id)")
                          indef-ids)
                  (setq changed-count
                        (+ changed-count
                           (skg--change-source-at-point new-source))))))))
      (funcall change-or-collect) ;; the root
      (outline-next-heading)
      (while (and (not (eobp))
                  (> (org-outline-level) start-level))
        (let ((metadata-sexp (skg--metadata-sexp-at-point-or-nil)))
          (if (not (and (skg--activeNode-sexp-p metadata-sexp)
                        (skg--node-parentIs-content-of-p metadata-sexp)))
              (skg--goto-next-heading-after-subtree)
            (when (equal (skg--node-source metadata-sexp) old-source)
              (funcall change-or-collect))
            (outline-next-heading))))
      (cons changed-count (nreverse indef-ids)))))

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

(defun skg--node-parentIs-content-of-p (metadata-sexp)
  "Return non-nil if METADATA-SEXP has implicit or explicit parentIs=affected."
  (let ((parentIs-values (skg-sexp-cdr-at-path metadata-sexp
                                            '(skg node parentIs))))
    (or (not parentIs-values)
        (eq (car parentIs-values) 'affected))))

(defun skg--node-source (metadata-sexp)
  "Return METADATA-SEXP's node source as a string, or nil."
  (let ((source-values (skg-sexp-cdr-at-path metadata-sexp
                                             '(skg node source))))
    (when source-values
      (format "%s" (car source-values)))))

(defun skg--node-id (metadata-sexp)
  "Return METADATA-SEXP's node ID as a string, or nil."
  (let ((id-values (skg-sexp-cdr-at-path metadata-sexp
                                         '(skg node id))))
    (when id-values
      (format "%s" (car id-values)))))

(defun skg--node-indefinitive-p (metadata-sexp)
  "Return non-nil if METADATA-SEXP has the bare ActiveNode indef marker."
  (skg-sexp-subtree-p metadata-sexp '(skg (node indef))))

(defun skg--change-source-at-point (new-source)
  "Set the source at point to NEW-SOURCE.
Returns 1 if the current line was edited."
  (skg-edit-metadata-at-point
   `(skg (node (ENSURE (source ,(intern new-source)))
               (viewStats))))
  (skg-edit-metadata-at-point
   `(skg (node (viewStats
                (ENSURE
                 (sourceHerald ,(intern
                                  (format "⌂:%s" new-source))))))))
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
