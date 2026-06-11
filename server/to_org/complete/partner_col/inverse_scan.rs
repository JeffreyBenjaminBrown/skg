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
use crate::types::git::{GitDiffStatus, MembershipAxes, NodeCompleteDiff, Sign, SourceDiff};
use crate::types::list::Diff_Item;
use crate::types::misc::{ID, SourceName};
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
pub fn inverse_scan_for_inbound_col (
  owner        : &ID,
  relation     : NodeRelation,
  source_diffs : &Option<HashMap<SourceName, SourceDiff>>,
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
          member_and_sign_for_owner (owner, relation, path, ncd)
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

/// What one changed file says about its 'relation' edge to 'owner'
/// in one stage, if anything.  The changed FILE is the member; the
/// owner appears (or not) in its outbound relation list.
fn member_and_sign_for_owner (
  owner    : &ID,
  relation : NodeRelation,
  path     : &PathBuf,
  ncd      : &NodeCompleteDiff,
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
      if outbound_ids_of_nodecomplete (before, relation)
         . contains (owner)
      { Some (( before . pid . clone (), Sign::Minus )) }
      else { None } },
    GitDiffStatus::Added => {
      let after : &NodeComplete = ncd . after_node . as_ref () ?;
      if outbound_ids_of_nodecomplete (after, relation)
         . contains (owner)
      { Some (( after . pid . clone (), Sign::Plus )) }
      else { None } } } }

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

/// The outbound list a NodeComplete holds for a relation.  Sibling
/// of 'outbound_ids_from_node' (relation_accessors.rs), which reads
/// a NodeRust instead.  Textlinks are inferred from node text, not
/// stored as a list, so they contribute nothing.
fn outbound_ids_of_nodecomplete<'a> (
  nc       : &'a NodeComplete,
  relation : NodeRelation,
) -> &'a [ID] {
  match relation {
    NodeRelation::Contains =>
      & nc . contains,
    NodeRelation::Subscribes =>
      nc . subscribes_to . or_default (),
    NodeRelation::HidesFromItsSubscriptions =>
      nc . hides_from_its_subscriptions . or_default (),
    NodeRelation::OverridesViewOf =>
      nc . overrides_view_of . or_default (),
    NodeRelation::TextlinksTo =>
      &[], } }

#[cfg(test)]
#[path = "../../../../tests/unit/inverse_scan.rs"]
mod tests;
