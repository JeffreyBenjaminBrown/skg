/// The INVERSE SCAN
/// (TODO/full-schema/12-2_diff-mode-policy_discussion.org): for an
/// inbound folder -- subscriberFolder, overriderFolder, hiderFolder -- the edges
/// live in the MEMBERS' files, so the owner's own diff says nothing
/// about them.  This scan answers: which nodes' files asserted a
/// 'relation' edge to 'owner' at HEAD or assert one now, and in
/// which stage did each edge appear or disappear?
///
/// It reads each Skg repo's staged and unstaged maps -- which list
/// only CHANGED files, so the cost is proportional to the size of
/// the change, not the graph:
/// - a Modified file's per-stage relation diff containing New(owner)
///   / Removed(owner) contributes that stage's Plus / Minus;
/// - a Deleted file whose 'before_node' relation list names the
///   owner contributes that stage's Minus;
/// - an Added file whose 'after_node' relation list names the owner
///   contributes that stage's Plus.
///
/// A CROSS-REPO MOVE (Deleted in one Skg repo, Added in another,
/// within one stage) contributes both signs for one (member, stage);
/// they CANCEL to no sign, because the edge existed before and after
/// the move -- a move must not fabricate a relationship change.  The
/// member's node axes (its file's per-repo statuses) tell the
/// move story instead.

use crate::dbs::in_rust_graph::relation_accessors::NodeRelation;
use crate::repo_sets::ActiveRepoSet;
use crate::types::git::{GitDiffStatus, RelationshipAxes, GraphnodeDiff, Sign, RepoDiff};
use crate::types::list::Diff_Item;
use crate::types::misc::{ID, RelPartner, RepoName};
use crate::types::nodes::complete::Graphnode;

use std::collections::HashMap;
use std::path::PathBuf;

/// Per-member, per-stage membership signs for the inbound folder of
/// 'owner' under 'relation'.  Members with no surviving sign in
/// either stage (e.g. a cancelled cross-repo move) are omitted.
/// A member whose axes' net result is "gone from the worktree"
/// ('RelationshipAxes::net_is_present' = false) belongs in the folder's
/// goal list as a phantom; a present member's Plus signs become its
/// 'addedR' marks.
///
/// 'active': relRepo gating (render-and-gating, 5_plan.org). A
/// Deleted/Added file's before/after Graphnode carries full
/// RelPartner values, so those two stages gate on the specific
/// edge's relRepo -- a phantom "used to link here" must not surface
/// from a membership recorded outside the active set. The Modified
/// stage cannot: 'NodeChanges' diff lists have their repos stripped
/// (the historical relation-partner work item deferred this; still true
/// here), so a Modified-file sign is emitted regardless of Skg repo. None = ungated
/// (every stage counts), matching every other gated accessor here.
pub fn inverse_scan_for_inbound_folder (
  owner        : &ID,
  relation     : NodeRelation,
  repo_diffs : &Option<HashMap<RepoName, RepoDiff>>,
  active       : Option<&ActiveRepoSet>,
) -> HashMap<ID, RelationshipAxes> {
  let Some (diffs) = repo_diffs else {
    return HashMap::new (); };
  let mut signs : HashMap<ID, [Vec<Sign>; 2]> =
    HashMap::new ();
  for sd in diffs . values () . filter ( |sd| sd . is_gitrepo ) {
    for (stage_index, stage_map) in
      [ (0, & sd . staged), (1, & sd . unstaged) ]
    { for (path, ncd) in stage_map {
        if let Some ((member, sign)) =
          member_and_sign_for_owner (owner, relation, path, ncd, active)
        { signs . entry (member)
            . or_insert_with ( || [ Vec::new (), Vec::new () ] )
            [stage_index] . push (sign); }}}}
  signs . into_iter ()
    . map ( |(member, [staged, unstaged])| (
        member,
        RelationshipAxes {
          staged   : resolve_one_stage (&staged),
          unstaged : resolve_one_stage (&unstaged) } ))
    . filter ( |(_, axes)| ! axes . is_empty () )
    . collect () }

/// Whether 'skgrepo' is visible under 'active' (None = ungated).
fn repo_is_active (
  active : Option<&ActiveRepoSet>,
  skgrepo : &RepoName,
) -> bool {
  match active {
    None     => true,
    Some (a) => a . is_all () || a . contains_repo (skgrepo) } }

/// What one changed file says about its 'relation' edge to 'owner'
/// in one stage, if anything.  The changed FILE is the member; the
/// owner appears (or not) in its outbound relation list.
fn member_and_sign_for_owner (
  owner    : &ID,
  relation : NodeRelation,
  path     : &PathBuf,
  ncd      : &GraphnodeDiff,
  active   : Option<&ActiveRepoSet>,
) -> Option<(ID, Sign)> {
  match ncd . status {
    GitDiffStatus::Modified => {
      let nc = ncd . node_changes . as_ref () ?;
      let diff_list : &[Diff_Item<ID>] =
        relation . diff_in_nodechanges (nc) ?;
      let sign : Sign =
        diff_list . iter () . find_map ( |d| match d {
          Diff_Item::New     (id) if id == owner => Some (Sign::Plus),
          Diff_Item::Removed (id) if id == owner => Some (Sign::Minus),
          _ => None } ) ?;
      let member : ID =
        ID::from ( path . file_stem () ? . to_str () ? );
      Some ((member, sign)) },
    GitDiffStatus::Deleted => {
      let before : &Graphnode = ncd . before_node . as_ref () ?;
      match outbound_member_relRepo_of_graphnode (before, relation, owner) {
        Some (relRepo) if repo_is_active (active, &relRepo) =>
          Some (( before . pid . clone (), Sign::Minus )),
        _ => None } },
    GitDiffStatus::Added => {
      let after : &Graphnode = ncd . after_node . as_ref () ?;
      match outbound_member_relRepo_of_graphnode (after, relation, owner) {
        Some (relRepo) if repo_is_active (active, &relRepo) =>
          Some (( after . pid . clone (), Sign::Plus )),
        _ => None } } } }

/// The relRepo of nc's outbound 'relation' edge to 'target', if nc's
/// list names it. Sibling of 'outbound_ids_of_graphnode' below,
/// but keeps the RelPartner's relRepo instead of dropping it, so
/// Deleted/Added-stage signs can be relRepo gated.
fn outbound_member_relRepo_of_graphnode (
  nc       : &Graphnode,
  relation : NodeRelation,
  target   : &ID,
) -> Option<RepoName> {
  let rel_partners : &[RelPartner<ID>] = match relation {
    NodeRelation::Contains =>
      & nc . contains,
    NodeRelation::SubscribesTo =>
      nc . subscribes_to . or_default (),
    NodeRelation::HidesFromItsSubscriptions =>
      nc . hides_from_its_subscriptions . or_default (),
    NodeRelation::OverridesViewOf =>
      nc . overrides_view_of . or_default (),
    NodeRelation::LinksTo =>
      // Links are inferred from body text, not stored as a list
      // (see 'outbound_ids_of_graphnode'); this scan never fires
      // for them from a Deleted/Added stage.
      return None, };
  rel_partners . iter ()
    . find ( |m| & m . member == target )
    . map ( |m| m . relRepo . clone () ) }

/// Plus-only -> Plus; Minus-only -> Minus; both -> None (the
/// cross-repo-move cancellation); no signs -> None.
fn resolve_one_stage (
  signs : &[Sign],
) -> Option<Sign> {
  let has_plus  : bool = signs . contains (&Sign::Plus);
  let has_minus : bool = signs . contains (&Sign::Minus);
  match (has_plus, has_minus) {
    (true,  false) => Some (Sign::Plus),
    (false, true)  => Some (Sign::Minus),
    _              => None } }

#[cfg(test)]
#[path = "../../../../tests/unit/inverse_scan.rs"]
mod tests;
