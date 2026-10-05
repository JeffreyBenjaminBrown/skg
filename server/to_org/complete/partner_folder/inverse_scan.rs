/// The INVERSE SCAN
/// (TODO/DONE/full-schema/DONE/12-2_diff-mode-policy_discussion.org): for an
/// inbound folder -- subscriberFolder, overriderFolder, hiderFolder -- the relationships
/// live in the MEMBERS' files, so the recorder's own diff says nothing
/// about them.  This scan answers: which nodes' files asserted a
/// 'relation' relationship to 'recorder' at HEAD or assert one now, and in
/// which stage did each relationship appear or disappear?
///
/// It reads each skgrepo's staged and unstaged maps -- which list
/// only CHANGED files, so the cost is proportional to the size of
/// the change, not the graph:
/// - a Modified file's per-stage relation diff containing New(recorder)
///   / Removed(recorder) contributes that stage's Plus / Minus;
/// - a Deleted file whose 'before_node' relation list names the
///   recorder contributes that stage's Minus;
/// - an Added file whose 'after_node' relation list names the recorder
///   contributes that stage's Plus.
///
/// A CROSS-REPO MOVE (Deleted in one skgrepo, Added in another,
/// within one stage) contributes both signs for one (member, stage);
/// they CANCEL to no sign, because the relationship existed before and after
/// the move -- a move must not fabricate a relationship change.  The
/// member's node axes (its file's per-repo statuses) tell the
/// move story instead.

use crate::dbs::in_rust_graph::relation_accessors::NodeRelation;
use crate::skgrepo_sets::SkgrepoRestriction;
use crate::types::git::{GitDiffStatus, RelationshipAxes, GraphnodeDiff, Sign, SkgrepoDiff};
use crate::types::list::Diff_Item;
use crate::types::misc::{ID, RelPartner, SkgrepoName};
use crate::types::nodes::complete::Graphnode;

use std::collections::HashMap;
use std::path::PathBuf;

/// Per-member, per-stage membership signs for the inbound folder of
/// 'recorder' under 'relation'.  Members with no surviving sign in
/// either stage (e.g. a cancelled cross-repo move) are omitted.
/// A member whose axes' net result is "gone from the worktree"
/// ('RelationshipAxes::net_is_present' = false) belongs in the folder's
/// goal list as a phantom; a present member's Plus signs become its
/// 'addedR' marks.
///
/// 'unrestricted': relRepo gating (render-and-gating, 5_plan.org). A
/// Deleted/Added file's before/after Graphnode carries full
/// RelPartner values, so those two stages gate on the specific
/// relationship's relRepo -- a phantom "used to link here" must not surface
/// from a membership recorded outside the skgrepo restriction. The Modified
/// stage cannot: 'NodeChanges' diff lists have their skgrepos stripped
/// (the historical relation-partner work item deferred this; still true
/// here), so a Modified-file sign is emitted regardless of skgrepo. None = ungated
/// (every stage counts), matching every other gated accessor here.
pub fn inverse_scan_for_inbound_folder (
  recorder      : &ID,
  relation      : NodeRelation,
  skgrepo_diffs : &Option<HashMap<SkgrepoName, SkgrepoDiff>>,
  restriction   : Option<&SkgrepoRestriction>,
) -> HashMap<ID, RelationshipAxes> {
  let Some (diffs) = skgrepo_diffs else {
    return HashMap::new (); };
  let mut signs : HashMap<ID, [Vec<Sign>; 2]> =
    HashMap::new ();
  for sd in diffs . values () . filter ( |sd| sd . is_gitrepo ) {
    for (stage_index, stage_map) in
      [ (0, & sd . staged), (1, & sd . unstaged) ]
    { for (path, ncd) in stage_map {
        if let Some ((member, sign)) =
          member_and_sign_for_recorder (recorder, relation, path, ncd, restriction)
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

/// Whether 'skgrepo' is visible under 'unrestricted' (None = ungated).
fn skgrepo_is_unrestricted (
  restriction : Option<&SkgrepoRestriction>,
  skgrepo : &SkgrepoName,
) -> bool {
  match restriction {
    None     => true,
    Some (a) => a . is_all () || a . contains_skgrepo (skgrepo) } }

/// What one changed file says about its 'relation' relationship to 'recorder'
/// in one stage, if anything.  The changed FILE is the member; the
/// recorder appears (or not) in its outbound relation list.
fn member_and_sign_for_recorder (
  recorder : &ID,
  relation : NodeRelation,
  path     : &PathBuf,
  ncd      : &GraphnodeDiff,
  restriction : Option<&SkgrepoRestriction>,
) -> Option<(ID, Sign)> {
  match ncd . status {
    GitDiffStatus::Modified => {
      let nc = ncd . node_changes . as_ref () ?;
      let diff_list : &[Diff_Item<ID>] =
        relation . diff_in_nodechanges (nc) ?;
      let sign : Sign =
        diff_list . iter () . find_map ( |d| match d {
          Diff_Item::New     (skgid) if skgid == recorder => Some (Sign::Plus),
          Diff_Item::Removed (skgid) if skgid == recorder => Some (Sign::Minus),
          _ => None } ) ?;
      let member : ID =
        ID::from ( path . file_stem () ? . to_str () ? );
      Some ((member, sign)) },
    GitDiffStatus::Deleted => {
      let before : &Graphnode = ncd . before_node . as_ref () ?;
      match outbound_member_relRepo_of_graphnode (before, relation, recorder) {
        Some (relRepo) if skgrepo_is_unrestricted (restriction, &relRepo) =>
          Some (( before . pid . clone (), Sign::Minus )),
        _ => None } },
    GitDiffStatus::Added => {
      let after : &Graphnode = ncd . after_node . as_ref () ?;
      match outbound_member_relRepo_of_graphnode (after, relation, recorder) {
        Some (relRepo) if skgrepo_is_unrestricted (restriction, &relRepo) =>
          Some (( after . pid . clone (), Sign::Plus )),
        _ => None } } } }

/// The relRepo of nc's outbound 'relation' relationship to 'target', if nc's
/// list names it. Sibling of 'outbound_ids_of_graphnode' below,
/// but keeps the RelPartner's relRepo instead of dropping it, so
/// Deleted/Added-stage signs can be relRepo gated.
fn outbound_member_relRepo_of_graphnode (
  nc       : &Graphnode,
  relation : NodeRelation,
  target   : &ID,
) -> Option<SkgrepoName> {
  let rel_partners : &[RelPartner<ID>] = match relation {
    NodeRelation::Contains =>
      & nc . contains,
    NodeRelation::SubscribesTo =>
      nc . subscribesTo . or_default (),
    NodeRelation::HidesFromSubs =>
      nc . hidesFromSubs . or_default (),
    NodeRelation::Overrides =>
      nc . overrides . or_default (),
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
