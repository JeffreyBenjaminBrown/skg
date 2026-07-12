/// The INVERSE SCAN
/// (TODO/full-schema/12-2_diff-mode-policy_discussion.org): for an
/// inbound col -- subscriberCol, overriderCol, hiderCol -- the edges
/// live in the MEMBERS' files, so the owner's own diff says nothing
/// about them.  This scan answers: which nodes' files asserted a
/// 'relation' edge to 'owner' at HEAD or assert one now, and in
/// which stage did each edge appear or disappear?
///
/// It reads each source's staged and unstaged maps -- which list
/// only CHANGED files, so the cost is proportional to the size of
/// the change, not the graph:
/// - a Modified file's per-stage relation diff containing New(owner)
///   / Removed(owner) contributes that stage's Plus / Minus;
/// - a Deleted file whose 'before_node' relation list names the
///   owner contributes that stage's Minus;
/// - an Added file whose 'after_node' relation list names the owner
///   contributes that stage's Plus.
///
/// A CROSS-SOURCE MOVE (Deleted in one source, Added in another,
/// within one stage) contributes both signs for one (member, stage);
/// they CANCEL to no sign, because the edge existed before and after
/// the move -- a move must not fabricate a membership change.  The
/// member's existence axes (its file's per-source statuses) tell the
/// move story instead.

use crate::dbs::in_rust_graph::relation_accessors::NodeRelation;
use crate::source_sets::ActiveSourceSet;
use crate::types::git::{GitDiffStatus, MembershipAxes, NodeCompleteDiff, Sign, SourceDiff};
use crate::types::list::Diff_Item;
use crate::types::misc::{ID, PrivaciedMember, SourceName};
use crate::types::nodes::complete::NodeComplete;

use std::collections::HashMap;
use std::path::PathBuf;

/// Per-member, per-stage membership signs for the inbound col of
/// 'owner' under 'relation'.  Members with no surviving sign in
/// either stage (e.g. a cancelled cross-source move) are omitted.
/// A member whose axes' net result is "gone from the worktree"
/// ('MembershipAxes::net_is_present' = false) belongs in the col's
/// goal list as a phantom; a present member's Plus signs become its
/// 'newM' marks.
///
/// 'active': edge-level gating (render-and-gating, 5_plan.org). A
/// Deleted/Added file's before/after NodeComplete carries full
/// PrivaciedMember levels, so those two stages gate on the specific
/// edge's level -- a phantom "used to link here" must not surface
/// from a membership recorded outside the active set. The Modified
/// stage cannot: 'NodeChanges' diff lists are level-stripped
/// (leveled-lists work item deferred this; still true here), so a
/// Modified-file sign is emitted regardless of level. None = ungated
/// (every stage counts), matching every other gated accessor here.
pub fn inverse_scan_for_inbound_col (
  owner        : &ID,
  relation     : NodeRelation,
  source_diffs : &Option<HashMap<SourceName, SourceDiff>>,
  active       : Option<&ActiveSourceSet>,
) -> HashMap<ID, MembershipAxes> {
  let Some (diffs) = source_diffs else {
    return HashMap::new (); };
  let mut signs : HashMap<ID, [Vec<Sign>; 2]> =
    HashMap::new ();
  for sd in diffs . values () . filter ( |sd| sd . is_git_repo ) {
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
        MembershipAxes {
          staged   : resolve_one_stage (&staged),
          unstaged : resolve_one_stage (&unstaged) } ))
    . filter ( |(_, axes)| ! axes . is_empty () )
    . collect () }

/// Whether 'level' is visible under 'active' (None = ungated).
fn level_is_active (
  active : Option<&ActiveSourceSet>,
  level  : &SourceName,
) -> bool {
  match active {
    None     => true,
    Some (a) => a . is_all () || a . contains_source (level) } }

/// What one changed file says about its 'relation' edge to 'owner'
/// in one stage, if anything.  The changed FILE is the member; the
/// owner appears (or not) in its outbound relation list.
fn member_and_sign_for_owner (
  owner    : &ID,
  relation : NodeRelation,
  path     : &PathBuf,
  ncd      : &NodeCompleteDiff,
  active   : Option<&ActiveSourceSet>,
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
      let before : &NodeComplete = ncd . before_node . as_ref () ?;
      match outbound_member_level_of_nodecomplete (before, relation, owner) {
        Some (level) if level_is_active (active, &level) =>
          Some (( before . pid . clone (), Sign::Minus )),
        _ => None } },
    GitDiffStatus::Added => {
      let after : &NodeComplete = ncd . after_node . as_ref () ?;
      match outbound_member_level_of_nodecomplete (after, relation, owner) {
        Some (level) if level_is_active (active, &level) =>
          Some (( after . pid . clone (), Sign::Plus )),
        _ => None } } } }

/// The LEVEL of nc's outbound 'relation' edge to 'target', if nc's
/// list names it. Sibling of 'outbound_ids_of_nodecomplete' below,
/// but keeps the PrivaciedMember's level instead of dropping it, so
/// Deleted/Added-stage signs can be edge-level gated.
fn outbound_member_level_of_nodecomplete (
  nc       : &NodeComplete,
  relation : NodeRelation,
  target   : &ID,
) -> Option<SourceName> {
  let leveled : &[PrivaciedMember<ID>] = match relation {
    NodeRelation::Contains =>
      & nc . contains,
    NodeRelation::Subscribes =>
      nc . subscribes_to . or_default (),
    NodeRelation::HidesFromItsSubscriptions =>
      nc . hides_from_its_subscriptions . or_default (),
    NodeRelation::OverridesViewOf =>
      nc . overrides_view_of . or_default (),
    NodeRelation::TextlinksTo =>
      // Textlinks are inferred from body text, not stored as a list
      // (see 'outbound_ids_of_nodecomplete'); this scan never fires
      // for them from a Deleted/Added stage.
      return None, };
  leveled . iter ()
    . find ( |m| & m . member == target )
    . map ( |m| m . level . clone () ) }

/// Plus-only -> Plus; Minus-only -> Minus; both -> None (the
/// cross-source-move cancellation); no signs -> None.
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
