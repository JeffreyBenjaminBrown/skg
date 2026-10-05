use crate::types::viewnode::NodeEditRequest;
use crate::types::maybe_placed_viewnode::{MpViewnode, MpViewnodeKind};
use crate::types::maybe_placed_viewnode::MpVognode;
use crate::types::misc::{ID, SkgRepoName};

use ego_tree::{Tree,NodeRef};
use std::collections::{HashMap, HashSet};

/// Enum to track whether a node should be deleted or not
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum WhetherToDelete {
  Delete,
  DoNotDelete,
}

/// ASSUMES:
/// everything 'find_buffer_errors_for_saving' assumes.
/// .
/// PURPOSE:
/// Find contradictory instructions in a buffer the user asked to save.
/// The specific problems it finds are:
/// - There's a node with 'toDelete' true,
///   but another node with the same ID that has 'toDelete' false.
/// - Two nodes with the same ID 'define their contents'
///   (i.e. their 'writeProtected' fields are both false).
/// - Two nodes with the same ID have different skgrepos,
///   even if some are write-protected.
/// .
/// STRATEGY:
/// Builds a map from IDs to sets of WhetherToDelete.
/// After traversing the tree, reports every key (ID)
/// for which the associated value (set) has size 2.
/// Also builds a map from IDs to count of defining containers,
/// and a map from IDs to sets of skgrepos. */
pub fn find_inconsistent_instructions(
  viewforest: &Tree<MpViewnode>
) -> (Vec<ID>, // IDs with inconsistent deletions across nodes
      Vec<ID>, // IDs with multiple defining nodes
      Vec<(ID, // IDs with inconsistent skgrepos
           HashSet<SkgRepoName>)>)
{ let (id_toDelete_instructions, id_to_definer_count, id_to_skgrepos) =
    collect_instructions (viewforest);
  let mut inconsistent_deletion_skgids: Vec<ID> = Vec::new();
  let mut problematic_defining_skgids: Vec<ID> = Vec::new();
  let mut inconsistent_skgrepo_skgids: Vec<(ID, HashSet<SkgRepoName>)> =
    Vec::new();
  { // filter to problematic instructions
    { // Collect inconsistent deletion instructions
      for (skgid, delete_set) in id_toDelete_instructions {
        if delete_set . len() == 2 {
          // Size 2 means both Delete and DoNotDelete are present.
          inconsistent_deletion_skgids . push (skgid); }} }
    { // Collect multiple defining containers
      for (skgid, count) in id_to_definer_count {
        if count > 1 {
          // Multiple defining containers for this ID
          problematic_defining_skgids . push (skgid); }} }
    { // Collect inconsistent skgrepos
      for (skgid, skgrepos) in id_to_skgrepos {
        if skgrepos . len() > 1 {
          // Multiple different skgrepos for this ID
          inconsistent_skgrepo_skgids . push((skgid, skgrepos)); }} }}
  ( inconsistent_deletion_skgids,
    problematic_defining_skgids,
    inconsistent_skgrepo_skgids ) }

/// Collect delete instructions, defining containers, and skgrepos.
fn collect_instructions(
  viewforest: &Tree<MpViewnode>
) -> (HashMap<ID, HashSet<WhetherToDelete>>, // deletes
      HashMap<ID, usize>, // defining containers
      HashMap<ID, HashSet<SkgRepoName>>) { // skgrepos

  fn collect_instructions_rec(
    node_ref: NodeRef<MpViewnode>,
    id_toDelete_instructions: &mut
      HashMap<ID, HashSet<WhetherToDelete>>,
    id_defining_count: &mut
      HashMap<ID, usize>,
    id_to_skgrepos: &mut
      HashMap<ID, HashSet<SkgRepoName>>
  ) {
    let viewnode : &MpViewnode = node_ref . value();
    if let MpViewnodeKind::Vognode (MpVognode::Active (t))
      = &viewnode . kind
    { if let Some (skgid) = &t . skgid {
        if ! t . is_writeProtected () { // write-protected nodes contribute no instructions
          let delete_instruction : WhetherToDelete =
            if matches!(t . edit_request (),
                        Some (&NodeEditRequest::Delete)) {
              WhetherToDelete::Delete
            } else { WhetherToDelete::DoNotDelete };
          id_toDelete_instructions // record delete_instruction
            . entry(skgid . clone())
            . or_insert_with (HashSet::new)
            . insert (delete_instruction);
          *id_defining_count . entry(skgid . clone())
            // increment the count for this defining container
            . or_insert (0) += 1; }
        if let Some (skgrepo_str) = &t . home_skgrepo {
          // Collect skgrepo for this ID
          let skgrepo : SkgRepoName =
            SkgRepoName::from(skgrepo_str . as_str());
          id_to_skgrepos
            . entry(skgid . clone())
            . or_insert_with (HashSet::new)
            . insert (skgrepo); }}}
    for child in node_ref . children() { // recurse
      collect_instructions_rec(
        child,
        id_toDelete_instructions,
        id_defining_count,
        id_to_skgrepos); }} // end of inner function definition

  let mut id_toDelete_instructions
    : HashMap<ID, HashSet<WhetherToDelete>>
    = HashMap::new();
  let mut id_to_definer_count
    : HashMap<ID, usize>
    = HashMap::new();
  let mut id_to_skgrepos
    : HashMap<ID, HashSet<SkgRepoName>>
    = HashMap::new();
  collect_instructions_rec(
    viewforest . root(),
    &mut id_toDelete_instructions,
    &mut id_to_definer_count,
    &mut id_to_skgrepos);
  (id_toDelete_instructions, id_to_definer_count, id_to_skgrepos) }
