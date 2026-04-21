#+TITLE: Distinguish staged vs unstaged in git diff view

* Context

The git diff view currently decorates view nodes with single heralds
('diff:new', 'diff:removed', 'diff:new-here', 'diff:removed-here',
'diff:not-in-git') that reflect a worktree-vs-HEAD comparison only. They
make no distinction between changes that have been =git add='d (staged)
and changes that haven't (unstaged). Jeff wants heralds that distinguish
the two, with enough fidelity to express any combination of staged and
unstaged changes that can occur on a single node.

Achieving this requires (a) computing two parallel diffs from git
(HEAD↔index and index↔worktree), (b) decomposing the diff state per node
into four orthogonal axes, (c) emitting that state in the metadata sexp
under new keys, (d) rendering the state in the Emacs herald rules, and
(e) honest phantom insertion that surfaces all per-stage states even
when the worktree-vs-HEAD comparison would cancel them out.

* Conceptual model: four orthogonal diff axes per ViewNode

Each ViewNode (TrueNode or phantom) carries four /diff axes/. Each axis
holds one of {none, '+', '-'}.

  stagedX    change to existence (HEAD vs index)
  unstagedX  change to existence (index vs worktree)
  stagedM    change to membership at this position (HEAD vs index)
  unstagedM  change to membership at this position (index vs worktree)

Where:

- X (existence) = does the node's '.skg' file exist in the graph at this
  stage?
- M (membership) = does the parent's 'contains' list reference this node
  at this position at this stage?

The four axes are independent across X and M. Within a single X-pair
(stagedX, unstagedX) or M-pair (stagedM, unstagedM) the two stage values
cannot both have the same sign, since that would require contradictory
index states (e.g. 'stagedX = +' implies "present in index" and
'unstagedX = +' implies "absent in index"). Opposite signs within a pair
/are/ legal: they describe a "create then delete" or "delete then
re-create" sequence across stages. Otherwise any combination of axis
values is legal, including all four simultaneously non-zero.

This decomposition replaces the legacy 'NodeDiffStatus' enum
{ 'New', 'NewHere', 'Removed', 'RemovedHere', 'NotInGit' } and
'FieldDiffStatus' enum { 'New', 'Removed' }. 'New' and 'Removed' had
been a conflation of X and M (a "new file" was always also "newly
present in some parent's contains"); the axis split unwinds the
conflation.

For Scaffolds (alias / id) the X axis is meaningless -- they have no
'.skg' file -- so they carry only the M-pair.

For Scaffold::TextChanged the analogue of X-only is a pair of bools
'(staged_text_changed, unstaged_text_changed)'; a single TextChanged
scaffold appears whenever either is true. Title and body changes are
deliberately bundled into one "text changed" signal: splitting them
would be too noisy in the herald and the user has to visit the diff to
understand the actual edit anyway.

'NotInGit' is unchanged: a per-source fact carried as a separate flag,
not as an axis value.

* Phantom rule (honest)

A node renders as a phantom iff it does not exist as a real, editable
entity in the worktree. Two independent conditions can each make that
true:

- /Membership-phantom/: this (parent, position) entry is missing from
  the parent's worktree-side contains list. Triggered by:
  - 'stagedM = -' alone (membership was removed in staging).
  - 'unstagedM = -' alone (membership was removed in worktree).
  - 'stagedM = +, unstagedM = -' (intermediate "moved twice" position).
- /Existence-phantom/: the node's '.skg' file is gone in the worktree.
  Triggered by 'unstagedX = -' (regardless of whether the parent's
  contains list still references it). If the worktree's contains list
  /does/ still reference the missing node, the phantom appears at that
  worktree position; otherwise membership-phantom rules drive position.

A node can satisfy both conditions at once (e.g. 'unstagedX = -' and
'unstagedM = -' for a coordinated delete).

The honest model expands the phantom set beyond today's HEAD↔worktree
comparison. A "moved twice" case (an item added in staging at position
P, then moved to position Q in the worktree) produces two phantoms (one
at the original HEAD position, one at the intermediate staged position
P) plus a real node at Q. This visual density is intentional: it
surfaces inconsistency between staged and unstaged changes, prompting
the user to clean it up.

* Herald format (ASCII)

Per stage with any non-zero axes, emit one stage marker. Axes within
the stage concatenate; stages join with a comma.

Axis letters:

- =X=   existence created in this stage
- =-X=  existence removed in this stage
- =M=   membership added at this position in this stage
- =-M=  membership removed at this position in this stage

Stage prefix: '=staged:=' or '=unstaged:='. Coloured green if all axes
in that stage are positive, red if all negative, yellow if mixed (the
"create the node but remove its membership" / "destroy the node but add
membership" oddities).

Examples (the indented lines below are example heralds, not org-mode
content):

  unstaged:XM                 -- new node, all in worktree
  staged:XM                   -- new node, all staged
  staged:X,unstaged:M         -- created staged, attached to parent in worktree
  staged:M,unstaged:-M        -- the intermediate-position phantom in a "move it twice" case
  staged:-X                   -- file gone in staging, parent.contains untouched ⇒ phantom under that parent
  staged:-X,unstaged:X        -- deleted then re-created
  staged:-X-M,unstaged:XM     -- removed staged, restored in worktree (rare but legal)

For TextChanged: render as a small marker per stage that has it, e.g.
=T:s=, =T:u=, =T:s,u=. Exact wording is Jeff's call.

* Wire format (sexp)

Inside =(node ...)= for TrueNodes, two optional keys per stage:

  (staged   STAGED_AXES)
  (unstaged UNSTAGED_AXES)

where each '_AXES' is a possibly-empty sequence of bare atoms drawn
from { 'newX', 'removedX', 'newM', 'removedM' }. An empty axis list is
omitted entirely (the form simply does not appear).

For Scaffold::Alias and Scaffold::ID at top level inside =(skg ...)=,
the same two keys, but axis atoms drawn only from { 'newM', 'removedM' }.

For Scaffold::TextChanged: extend it to carry the bool pair. Wire form:

  (textChanged STAGE_TAGS)

where STAGE_TAGS is a subset of { 'staged', 'unstaged' }.

The legacy =(diff ...)= key disappears entirely (no back-compat) per the
project's no-legacy convention.

Example metadata sexps (two-space-indented to keep Emacs from parsing
them as headlines):

  (skg (node (id 7) (source main) (unstaged newX newM)))
  (skg (node (id 9) (source main) indefinitive (staged removedM) (unstaged newM)))
  (skg alias (staged newM))
  (skg id (unstaged removedM))
  (skg (textChanged staged unstaged))

* Data model (Rust)

Replace the old =Option<NodeDiffStatus>= and =Option<FieldDiffStatus>=
fields with explicit per-axis pair structs:

  pub enum Sign { Plus, Minus }

  pub struct ExistenceAxes {
    pub staged   : Option<Sign>,
    pub unstaged : Option<Sign>, }

  pub struct MembershipAxes {
    pub staged   : Option<Sign>,
    pub unstaged : Option<Sign>, }

On 'TrueNode' (in [[../../server/types/viewnode.rs]]):

- 'existence  : ExistenceAxes'
- 'membership : MembershipAxes'
- 'not_in_git : bool'  -- separate, source-level fact

On scaffolds:

- 'Scaffold::Alias { text, membership : MembershipAxes }'
- 'Scaffold::ID    { id,   membership : MembershipAxes }'
- 'Scaffold::TextChanged' becomes
  'TextChanged { staged : bool, unstaged : bool }'.

Delete the enums =NodeDiffStatus= and =FieldDiffStatus= entirely,
including their 'NewHere' / 'RemovedHere' variants.

'is_phantom' becomes the disjunction of the two phantom triggers above:
returns 'true' iff any M-axis is '-', /or/ a non-worktree M-axis is '+'
while the worktree-side M-axis is '-' (the "moved twice" case),
/or/ 'unstagedX = -'.

* Compute pipeline (git_ops)

Split [[../../server/git_ops/read_repo.rs]] / 'get_changed_skg_files'
into two functions:

  fn get_staged_changed_skg_files (repo: &Repository)
    -> Result<Vec<PathDiffStatus>, Error>     // diff_tree_to_index
  fn get_unstaged_changed_skg_files (repo: &Repository)
    -> Result<Vec<PathDiffStatus>, Error>     // diff_index_to_workdir

Both keep the 'pathspec("*.skg")' filter and funnel results through
'diff_delta_to_entry'. The unstaged variant keeps 'include_untracked
(true)'.

Add 'get_file_content_at_index' as the index-side analogue of
'get_file_content_at_head' ('repo.index()?', 'index.get_path(rel_path,
0)', 'repo.find_blob(entry.id)').

In [[../../server/git_ops/diff.rs]], generalise 'compute_skgnode_diff'
to take an explicit before-loader and after-loader (each
'FnOnce() -> Option<SkgNode>'), so the same comparison logic can run
against any pair of stages. Call it once per stage per changed '.skg'
file, with the appropriate (HEAD, index) or (index, worktree) pair.

Rework 'compute_diff_for_source' to produce:

  pub struct SourceDiff {
    pub is_git_repo   : bool,
    pub staged        : HashMap<PathBuf, SkgnodeDiff>,
    pub unstaged      : HashMap<PathBuf, SkgnodeDiff>,
    pub deleted_nodes : HashMap<ID, SkgNode>,  // union of HEAD-only and index-only deletions; used for phantom titles
  }

* Apply pipeline

In [[../../server/to_org/render/diff.rs]] / 'apply_diff_to_forest', for
each TrueNode in the forest:

- Populate X axes from the file's per-stage existence in HEAD / index /
  worktree.
- Populate M axes from per-stage 'contains_diff' results for the
  parent, intersected with this child's ID at this position.

For an alias scaffold under an AliasCol: populate its M axes from the
parent's per-stage 'aliases_diff'.

For an id scaffold under an IDCol: populate its M axes from the parent's
per-stage 'ids_diff'.

For TextChanged: set 'staged' / 'unstaged' bools from the per-stage
'text_changed' booleans; insert a single TextChanged scaffold whenever
either is true.

Phantom insertion: for each parent, for each (child id, position) that
appears in HEAD's contains or index's contains but not in the
worktree's contains, insert a phantom at that position with the
appropriate M axes. Separately, for each child id whose '.skg' was
deleted in the worktree, mark the existing real ViewNode (if the
worktree contains list still references it) with the corresponding X
axes; no extra phantom is needed in that case.

* Heralds (Emacs)

In [[../../elisp/heralds-minor-mode.el]] / 'heralds--transform-rules',
replace the three '(BLUE diff ...)' rule entries with per-stage rules:

  (BLUE staged
    (GREEN newX     "staged:X")
    (RED   removedX "staged:-X")
    (GREEN newM     "staged:M")
    (RED   removedM "staged:-M"))
  (BLUE unstaged
    (GREEN newX     "unstaged:X")
    (RED   removedX "unstaged:-X")
    (GREEN newM     "unstaged:M")
    (RED   removedM "unstaged:-M"))

Both blocks appear (a) at top level (for Scaffolds), (b) nested under
=(node ...)= (for TrueNodes), and (c) nested under the =id= form (for
ID scaffolds).

For the within-stage axis concatenation ('staged:XM' rather than
'staged:X staged:M'), add a small post-pass in
'heralds--tokens->text' (or a sibling helper) that groups consecutive
tokens by their =staged:= / =unstaged:= prefix and concatenates the
suffixes -- including the leading '-' on negative axes, so
'"staged:-X"' + '"staged:-M"' becomes '"staged:-X-M"'. Mixed-sign
stage groups recolor to yellow at the same step.

For TextChanged: add a transform entry that emits a marker per stage
tag.

* Files involved

** Rust types and computation
- [[../../server/types/git.rs]] -- 'NodeDiffStatus', 'FieldDiffStatus',
  'SourceDiff', 'SkgnodeDiff', 'NodeChanges', 'GitDiffStatus',
  'node_changes_for_truenode'
- [[../../server/types/viewnode.rs]] -- 'TrueNode.diff' field,
  'Scaffold::Alias.diff', 'Scaffold::ID.diff', 'Scaffold::TextChanged',
  'is_phantom', 'mk_phantom_viewnode', 'default_truenode'
- [[../../server/types/phantom.rs]] -- 'phantom_diff_status'
- [[../../server/git_ops/read_repo.rs]] -- 'get_changed_skg_files'
  (the one place that calls 'diff_tree_to_workdir_with_index')
- [[../../server/git_ops/diff.rs]] -- 'compute_diff_for_source',
  'compute_skgnode_diff', 'collect_deleted_nodes', 'compare_skgnodes'
- [[../../server/git_ops/misc.rs]] -- 'diff_delta_to_entry'

** Diff application to view
- [[../../server/to_org/render/diff.rs]] -- 'apply_diff_to_forest',
  'process_truenode_diff', 'maybe_mark_added_or_deleted',
  'prepend_idcol_with_children', 'mark_newhere_children',
  'insert_phantom_nodes_for_removed_children',
  'process_truenode_contains_diff'
- [[../../server/to_org/expand/aliases.rs]] -- 'build_and_integrate_aliases'
  (creates 'Scaffold::Alias')
- [[../../server/to_org/expand/definitive.rs]] -- the 'is_removed_node'
  branch that special-cases 'NodeDiffStatus::Removed'
- [[../../server/from_text/buffer_to_viewnodes/add_missing_info.rs]]
  -- 'make_alias_if_appropriate' (creates 'Scaffold::Alias')

** Sexp emission and parsing
- [[../../server/org_to_text.rs]] -- 'scaffold_metadata_to_string',
  'true_node_metadata_to_string' (its inner 'diff_status' helper)
- [[../../server/serve/parse_metadata_sexp.rs]] -- 'ViewnodeMetadata',
  'parse_metadata_to_viewnodemd', 'parse_node_sexp',
  'viewnode_from_metadata'

** Save / rerender pipeline
- [[../../server/serve/handlers/save_buffer.rs]]
  -- 'compute_diff_for_every_source', 'deleted_ids_to_source',
  'validate_no_merge_commits'
- [[../../server/serve/handlers/rerender_all_views.rs]]
  -- 'stream_rerender_views'
- [[../../server/update_buffer.rs]] -- consumes 'source_diffs' during
  'rerender_view'

** Emacs client
- [[../../elisp/heralds-minor-mode.el]] -- 'heralds--transform-rules'

** Tests and fixtures
- [[../../tests/elisp/test-heralds-minor-mode.el]] --
  'test-heralds-diff-display'
- [[../../tests/git_diff_view/newhere_cycle/common.rs]]
- [[../../tests/new/buffer_to_viewnodes/validate_tree.rs]]
- [[../../tests/integration/git-diff-view-updates/]] -- expected '*.log'
  fixtures

* Tests

- Update 'test-heralds-diff-display' in
  [[../../tests/elisp/test-heralds-minor-mode.el]] to use the new keys
  and expect the new herald strings.
- Update fixture sexps in
  [[../../tests/git_diff_view/newhere_cycle/common.rs]] and
  [[../../tests/new/buffer_to_viewnodes/validate_tree.rs]] -- swap
  =(diff X)= for the appropriate =(staged ...)= / =(unstaged ...)= /
  =(textChanged ...)= forms. Most existing fixtures committed nothing
  to the index, so their changes are unstaged.
- Re-baseline expected logs in
  [[../../tests/integration/git-diff-view-updates/]] after running the
  integration script.
- Add tests for the new degrees of freedom:
  - All four "X and M drift" combinations
    (stagedX without stagedM; stagedM without stagedX; etc.).
  - The "moved twice" case: intermediate phantom shows
    =staged:M,unstaged:-M= and the final position shows =unstaged:M=.
  - Existence-only deletion: a phantom appears under a parent whose
    contains list still references a node whose file was deleted
    staged.
  - Mixed-sign coloring (yellow) on the rare cases.
  - At least one end-to-end test that stages a change with =git add=
    and verifies the herald distinguishes it from an unstaged change.

* Verification

1. =cargo nextest run= (TypeDB must be running). Update string-equality
   tests in =tests/git_diff_view/= as part of the change.
2. =bash/emacs-tests.sh= for the herald rule changes.
3. =bash/integration-tests.sh= for the end-to-end git-diff-view tests;
   re-baseline =tests/integration/git-diff-view-updates/expected= once
   the new format is stable.
4. Manual: in a real skg repo, =git add= one file, modify a second
   without staging, then 'M-x skg-view-diff-mode' and confirm the two
   files surface =staged:...= and =unstaged:...= heralds respectively.
   Also exercise a "move twice" sequence and confirm the intermediate
   phantom appears.

* Problems

** Migration of consumers that hard-code 'NodeDiffStatus::Removed'

'execute_definitive_view_request' in
[[../../server/to_org/expand/definitive.rs]] has an 'is_removed_node'
branch that checks =matches!(t.diff, Some(NodeDiffStatus::Removed))=
and loads body from git HEAD. Under the new model the question is:
which stage has a usable copy? Prefer the index when 'unstagedX = -'
but 'stagedX != -'; otherwise fall back to HEAD. The branch needs
generalising, not just renaming.

** Three SkgNode parses per changed file

Parsing HEAD, index, and worktree for every changed '.skg' on every
diff-mode toggle and every save in diff mode. Probably fine at current
sizes; flag for measurement.

** Stringly-typed coupling in the herald post-pass

The chosen post-pass discovers stage-grouping by parsing token strings
('staged:X', 'staged:-M', etc.) for prefix and suffix. Anyone changing
the herald strings must update the regex too. Manageable -- one short
helper, one test -- but a new fragility introduced by this redesign.

** Phantom condition is a disjunction, easy to get wrong

The phantom rule has two independent triggers (membership-phantom and
existence-phantom) and a third sub-case for the moved-twice scenario.
The 'is_phantom' helper has to encode all three correctly, and the
phantom-insertion code in 'apply_diff_to_forest' has to walk both
"missing from worktree.contains" /and/ "file deleted in worktree" paths
to find every position that needs a phantom. Worth a focused test
matrix to make sure no case is silently dropped (especially the
existence-only deletion when the parent.contains list is unchanged).

** Cosmetic decisions to settle now

- Stage marker color when axes mix sign (proposed: yellow).
- TextChanged herald exact text.
- Whether the M-only Scaffold lines should suppress their stage prefix
  in single-stage cases (e.g. just =M= instead of =staged:M= on an
  alias).

Cheap to change later, but easier to set upfront.

* Problems, out of scope

- 'NotInGit' is a per-source fact but is currently stamped per-truenode
  in 'process_truenode_diff'. The herald is identical for every such
  node. Could collapse to a per-source banner or prefix on the source
  herald.
- 'compute_skgnode_diff' silently swallows YAML parse failures via
  '.and_then(|s| serde_yaml::from_str(&s).ok())'. A corrupt file at
  HEAD or in the worktree therefore yields the same =None= as a
  missing file. Worth surfacing parse errors at least to logs.
- The =TODO ?= comment in [[../../server/types/git.rs]] points out that
  'SkgnodeDiff.head_node' is only populated for deleted files; the
  same logic applies to a future 'index_node' field once stage-aware
  diffing lands.
- 'apply_diff_to_forest' DFS visits Scaffold and Deleted nodes only to
  no-op. Cheap but wasted work.
- 'insert_phantom_nodes_for_removed_children' allocates fresh empty
  maps on every call and threads them through 'find_source_many_ways'
  unused; the signature could be tightened.
