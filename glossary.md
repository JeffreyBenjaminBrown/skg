Some terms and abbreviations used in this code.

# Terms and abbreviations specific to Skg

## "telescope" = privacy telescope

In code and comments the short form "telescope" always means the
privacy telescope; see "privacy telescope" below.

## privacy telescope, telescope section, home

One node can be recorded at several privacy levels at once: same-ID
`.skg` files across sources, at most one per source. That family of
files is the node's **privacy telescope**; each file is one
**telescope section** ("section" for short in code), holding the
slice of the node recorded at its source's level. The **home** is
the MOST PUBLIC RETAINED section; it alone holds title, body, and extra
ids. That the home carries the title is an INVARIANT, not a
search rule: a node's text always lives in its most public
section, so a titleless section above the title is a violation
the fold reports (`FoldWarning::TitleBelowHome`), not a shape to
look past. There is no linking relationship between sections —
sharing an ID is being the same node. See `docs/telescopes.md`
and `server/types/nodes/fs.rs`.

Before folding, an owned-PID collision is normalized: if any owned
section exists for a PID, the entire owned telescope is retained and all
same-PID foreign sections are ignored (with a deterministic warning and no
filesystem edit). A telescope with no owned section remains wholly foreign.

## privacy order and recording source

The position of a source in the config's privacy order (most public
first) is its privacy level. Live code does not use "level" for the
value carried by an edge: every relationship member carries its
**recording source**, the source whose section records it. In code
that is `MemberAtSource { source, member }`; a collection of them is
a **list of members at sources**. "More private" = later in the
privacy order.

## fold (visible vs full), unfold

The **fold** of a telescope combines its sections, most public
first, into one effective node (`server/telescope/fold.rs`). The
**full fold** uses every section; a **visible fold** uses only the
sections active under the current source-set, and is what a
restricted view renders. **Unfold** is the save-side inverse:
splitting the effective node back into sections, byte-stably.

## scalar, scalar text, ugly telescope, and scalar-release boundary

A **scalar** is one value, as opposed to a collection of values. In
Skg's telescope and privacy code, the node's **scalar text** means its
`title` and `body`. They differ from aliases and relationship lists:
list members from several telescope sections can be combined by the
fold, whereas the fold selects one effective title and one effective
body. A multiline body is still one scalar value; YAML may encode it as
a block scalar.

A conforming telescope supplies its scalar text at its home. If the
fold must select a title or body from a section below the home, the
result is an **ugly telescope** (`NodeComplete::ugly_telescope`). Skg
may hold that result internally, but a restricted source-set must not
receive its title or body without explicit approval for the affected
PID. The **scalar-release boundary** is the shared policy enforcing
that distinction. `ScalarReleaseDecision` either allows release,
allows it with a warning, or returns a deliberately text-free
challenge; the decision must precede changing client-visible view
state or sending the rendered text.

Elsewhere, such as YAML parsing and serialization, **scalar** retains
its ordinary broader meaning: any single YAML value rather than a
sequence or mapping.

## manifest, path-digest manifest, and tombstone

A **manifest** is a finite inventory: a map saying which named things belong
to one snapshot and what is known about each. In partial reload, the
**path-digest manifest** maps every selected direct `.skg` path to the BLAKE3
digest of its exact bytes. Comparing manifests finds additions, deletions and
same-size rewrites without assuming anything about repository layout.

A **tombstone** is an explicit record that a named thing is absent. Recovery
manifests map a possible telescope-section path to either `Some(bytes)` or an
absent tombstone (`None`). The tombstone matters because “this file was
deleted” is incident data, whereas omitting the path could merely mean that
it was never inspected. Here a tombstone is metadata in a snapshot, not a
placeholder `.skg` file written to disk.

## placement, anchor

How a more private section orders its members into a more public
list without the public file recording anything: in the section's
stored sequence, a `- anchor: ID` entry names a more public member,
and the members after it insert immediately after that ID; members
before the first anchor prepend. Placements exist only on disk —
anchors are derived at save from the in-memory order, so they can
never go stale in memory.

## sticky rule (sticky-else-default)

Where a relationship edge's recording source comes from at save time:
an edge already on disk keeps its source (**sticky**); a new edge
between owned nodes defaults to the more-private endpoint home. A new
edge from an owned owner to a foreign member defaults to the owner's
home, deliberately making the foreign ID and relationship visible at
that owned source without proposing a foreign write. Every recording
source is clamped to be no more public than the owner's home. Hides
floor higher (at least the most public subscription explaining them). A
`(relSource NAME)` atom (the `skg-set-relationship-source` gesture)
overrides sticky with any source at least as private as the edge's
DEFAULT — raising privacy, or moving a stuck source back to the
default; a more-public choice is a save error. A legacy or
hand-authored edge whose disk source already sits more public than
its default may be held or made more private, never made still more
public. See `server/from_text/supplement_from_disk.rs`.

## "buffer", or sometimes "forest"

Buffer is a term from Emacs.
Each view onto the graph is represented via a separate buffer.
Each buffer is a forest of "viewnodes".

## "child" and "parent"

The TypeDB graph contains a number of relationships -- 'contains', 'textlinks to', etc. Where in this code the terms "child" or "parent" are used, it refers to none of those TypeDB relations, but rather the relationship between headlines in an Emacs org mode buffer. (I have tried to stick to the more precise "org-child" and "org-parent".)

The reason for this is that the map from the child-parent relationship in an Emacs buffer to the corresponding relationship, if any, in TypeDB depends on context. Usually, if an org headline P has a child headline C, they will correspond to a TypeDB node Outer that 'contains' a TypeDB node Inner, where Outer corresponds to P and Inner to C. But that does not always hold: see, for instance, the discussion of alias nodes in [the architecture documentation](coding-advice/architecture.md).

## "col" is short for "collection"

in some type definitions or enum varieties.

## PartnerCol, and the col-policy vocabulary

A **PartnerCol** is the general concept of a generated org-collection
that gathers the members of one schema relation under a node, in one
role.  It is the `ViewNodeKind::PartnerCol` variant and the payload
type of the same name.  ("role col", "relation col" and bare "sharing
col" are retired as names for the general concept; "sharing-col" may
survive only where it genuinely denotes the subscribee-related
completers.)

There are eight PartnerCols, named by their external scaffold atom:

- writable: `subscribeeCol`, `overriddenCol`;
- read-only inbound: `subscriberCol`, `overriderCol`, `hiderCol`,
  `hiddenCol`;
- read-only filter: `hiddenInSubscribeeCol`,
  `hiddenOutsideOfSubscribeeCol`.

Each kind's behavior is decided by one method, `PartnerCol::policy`
(`server/types/viewnode.rs`), returning a **ColPolicy**.  This single
arbiter is consulted by reconciliation dispatch, save extraction's
read-only treatment, and herald/warning gating, so a new relation
cannot acquire inconsistent policies in different layers:

- **WritableSet** (`subscribeeCol`, `overriddenCol`): membership edits
  are graph edits.  An absent col means "no opinion" (its field lowers
  to `MSV::Unspecified`, filled from disk); a present-but-empty col
  means an explicit empty set (`MSV::Specified(vec![])`).
- **ReadOnlySet** (`subscriberCol`, `overriderCol`, `hiderCol`,
  `hiddenCol`): membership is generated from graph facts; the user's
  order is respected view-locally; repairs warn.  See [read-only set](#read-only-set).
- **ReadOnlyFilter** (`hiddenInSubscribeeCol`,
  `hiddenOutsideOfSubscribeeCol`): membership is derived from hide
  state rather than from a single relation role.

The SIX read-only cols (the `ReadOnlySet` four plus the `ReadOnlyFilter`
two) carry the ☮ herald on their col headline, meaning "this collection
cannot be changed from here" — the same sense ☮ (`indef`) carries on a
node.  The two `WritableSet` cols and `aliasCol` do not.  This is a
herald-table fact only (`server/heralds.rs`), with no wire atom of its
own: which cols are read-only is already known from the col atom.

The stale-member rule is uniform across all eight kinds (decided
2026-06-10, demote-not-discard): a stale `parentIs=Affected` member
that is a leaf is deleted; one with children is demoted to
`parentIs=Independent` so the user keeps any subtree they built under
it; duplicates are deleted; missing graph members are restored.

**Col scaffolds read the process-global graph handle.**  De-novo
rendering of a node's cols (all the read-only cols, and the outbound
`hiddenCol`/`overriddenCol`) consults `snapshot_global`, not the
render environment's own in-Rust graph; only the `subscribeeCol` is
built from the owner's outbound edges alone.  Production always has
the handle installed, but a test harness that renders without it will
see those cols silently missing (see `tests/partner_col_matrix.rs`,
which installs the handle in its render-heavy function).

**Independent children jump above the members.**  An `Independent`
(non-member) child parked inside any col is reordered above the
generated members on save (`complete_relevant_children` moves
irrelevant children to the front).  This is deliberate, not a bug: the
membership is generated and ordered, so a note you park inside a col is
kept but visibly separated from the live membership.

## ephem = ephemeral

Some data regarding a node is ephemeral --
it lasts only as long as the view of it in some buffer.

Non-ephemeral data, by contrast,
remains on disk when the app terminates,
and can be recovered by a later session.

## fork : can be any number > 1

Some people intpret a fork as a binary choice.
In Skg a fork can split into any number.

## clone, and the fork operation

To **fork** a node N is to turn it into an editable **clone** C in a
source you own. C copies N's title, body, and child-list (the child IDs
only — shallow), `subscribes_to` N (so N's later additions surface to C
as unintegrated subscribed content) and `overrides_view_of` N (so
override substitution draws C wherever N would appear). Children of N
that the forking edit deleted become C's
`hides_from_its_subscriptions`, and thereafter any child removed from
an owned subscriber's contains that some subscribee still contains is
hidden likewise on save (re-adding it unhides). A node may have at
most one clone you own (override monogamy).

Forking spans two gestures, foreign and owned:

- *implicit* (foreign N): make N definitive (`C-c s d`), edit it, and
  save; any real change to a foreign node is read as a fork request, and
  the save asks you to confirm. N is left untouched on disk.
- *explicit* (owned N): `skg-fork-node` (`C-c m f`), for a node you
  already own — used to deepen an override chain (D overrides C overrides
  N, all owned). N keeps its own content; only the clone is added. The
  clone is built from N's saved snapshot, so the command refuses a dirty
  buffer.

See `docs/forks.md` and `docs/sharing-model.md`.

## hiderel = "hides" relationship

See schema.tql for what a "hides" relationship is.

## interp = interpretation

## kv-pair = key-value pair

## local instruction collection

The save path's instruction extraction: one pure DFS preorder over
the buffer's viewforest, in which each position reads only itself and
its direct children, plus a context that flows down from its
ancestors. Each visit emits per-field intents into a map keyed by
node ID; downstream stages (validation, lowering, visibility
resolution, disk supplementation, the noop filter) consume the map.
Lives in `server/from_text/local_instruction_collection/`, entered
via `extract_nonmergeSavePlan_locally`.

This entry is the canonical sketch of the save pipeline; the
"save-buffer pipeline" section of
`coding-advice/claude-to-claude.org`, which lists the surrounding
stages (parse, enrich, validate, place, extract here, foreign-node
validation, nodeMerge build, graph update, rerender), links back to
this definition rather than re-explaining extraction.

## lp = length-prefixed

Sometimes an API response is prefixed with a length, letting the receiver know how many characters to expect in the response.

## An 'm' prefix

In Haskell, 'm' is sometimes prefixed to a word to indicate 'maybe'.
The Rust term for Maybe is Optional, but I've used the Haskell idiom.
For any function with a name like this, if its meaning is unclear,
the type signature should clarify what's going on.

## md = Metadata

Most org nodes are prefixed with some metadata.
In Emacs, that metadata is represented as an s-expression like
(skg (key value) bare_value ...),
with any number of key-value and bare-value pairs in any order.
The metadata is used for Rust to know what to do with it,
and for Emacs to let the user know how to interpret it.

## mk = make / create

## "merge" is always qualified: nodeMerge vs instructionMerge

A *nodeMerge* is the acquirer/acquiree operation on graph nodes (the
`NodeMerge` type, requested via `(editRequest (merge ID))`). An
*instructionMerge* is the combining of emitted save intents into the
map during local instruction collection. In prose, say "merge nodes"
or "merge instructions"; bare "merge" appears only in client-facing
wire strings, where only nodes can merge.

## mp = maybe-placed

A viewnode that might have an ID and source,
but might not, is maybe-placed.

## override substitution, and the vocabulary around it

When view completion would CREATE a viewnode for node N as
recursive content, it draws the EFFECTIVE OVERRIDER of N instead:
the end of the chain of user-owned, currently-VISIBLE
'overrides_view_of' edges leading out of N. The chain is often one
edge, but may be longer (a user-owned chain D overrides C overrides
N is legal; only a cycle is forbidden), and visibility gating can
stop it at a MIDDLE node when a later link's source is inactive --
so the drawn node is the last visible link, not necessarily the
chain's end. The resolver is 'resolve_override' in
'server/dbs/in_rust_graph/override_resolution.rs'. Foreign override
edges are display-only facts and never substitute.

The drawn substitute carries the MARKER '(overridesHere N)'
(herald red "Oh"), naming the original it stands for. A viewnode's
COLLECTED ID is what it contributes to its parent's saved lists:
the marker's original if present, else its own ID -- so saving a
view that draws R in place of N keeps N in the parent's contains.

An OVERRIDDEN-AS-SUCH is an Affected child of an overriddenCol --
the position that always shows the original; its definitive
expansion applies neither the owner's hides nor substitution to
its immediate children.

The OVERRIDE-CHOICE BUFFER (the "menu") is what a new single-root
view of an overridden node returns: the node as root, each visible
overrider an Independent indefinitive child of what it overrides,
all edges shown, foreign included. BYPASS
('(override-choice . "bypass")' on the request;
'skg-goto-bypassOverride' in Emacs; automatic from magit buffers)
skips the menu and opens the requested node itself.

## "subscribee as such"

In Skg some nodes are "subscribers", which "subscribe" to "subscribees".
(See [the schema](./schema.tql).)
A node that plays the 'subscribee' role can be viewed as an ordinary node,
or *as* a subscribee. In the latter case it appears
underneath the relevant subscriber, in a 'subscribeeCol':
```
  * subscriber
  ** subscribeeCol
  *** subsribee1
  *** subsribee2
  ...
```

In the above view, subscribee1 and subscribee2 are both "subscribees as such":
They are not just subscribees, but they are being shown *as* subscribees.
This changes the way edits are interpreted:
Edits to a "subscribee as such" can only affect
what the relevant subscriber hides, nothing else.

## read-only set

A read-only set is a generated collection whose membership follows
from graph facts rather than direct edits to that collection.  A view
may preserve an order for the set locally, but adding or removing an
item in the generated collection does not add or remove the underlying
graph membership fact.  If a user labels a nonmember inside such a set,
the save treats that item as independent of the generated membership.
If completion discovers that a real member is missing from the view, it
restores that member.

For instance, if nodes R and T subscribe to nodes N, the user might land
at a view like the following:
```
  * N
  ** subscriberCol
  *** R
  *** T
```
The subscriberCol ("collection of subscribees") is a read-only set.
Thus the user could flip the order of R and T in the subscribeeCol,
and this change would be respected in the display, but have no effect
in the graph. The user could insert X under subscriberCol,
but because it is a read-only set, X will have its parentIs field
rewritten to 'Independent', and have no effect on N.
(If the user wants X to subscribe to N, they must modify X, not N.)
The user could delete R, but it will respawn as soon as they save,
because, again, the subscriberCol is a read-only set.

## scaffold

A view of a graph consists of viewnodes.
Some of these correspond to individual graphnodes.
The rest are "scaffolds".

## title != headline

Headline is a term from org-mode. It refers to a line that begins with some asterisks, followed by at least one whitespace, followed by at least one non-whitespace. The text after the first chunk of adjacent whitespace is what I'm calling the 'title'.

# Terms specific to Tantivy

## context origin and cyclic root

A **context origin** is a node whose Tantivy document receives a search-rank
multiplier: `Root`, `CyclicRoot`, `Dest`, `HadID`, or `MultiContained`.
A **cyclic root** is specifically a member of an uncovered containment cycle
which the whole-graph context algorithm must use as a root after growing from
all obvious origins. A node can participate in a containment cycle without
being a `CyclicRoot` when content growth from an outside origin already
reaches it. The older label `CycleMember` was therefore misleading.

## Document

A Tantivy association. In my case, from a title or alias to an ID.
(Each ID can have multiple such associations.)
