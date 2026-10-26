use crate::types::viewnode::NodeEditRequest;
use crate::types::viewnode::{Viewnode, ViewnodeKind};
use crate::types::viewnode::Vognode;
use crate::types::errors::HeadlineName;
use crate::types::misc::ID;

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
/// Write-protected nodes contribute no instructions, so they cannot
/// contradict anything -- not even with a different skgrepo, which save
/// ignores. (Two editable nodes with different skgrepos are already
/// two defining nodes.)
/// .
/// STRATEGY:
/// Builds a map from IDs to sets of WhetherToDelete.
/// After traversing the tree, reports every key (ID)
/// for which the associated value (set) has size 2.
/// Also builds a map from IDs to the defining containers' names.
/// Each reported ID comes with the names of its defining
/// containers, in buffer order. */
pub fn find_inconsistent_instructions(
  viewforest: &Tree<Viewnode>
) -> (Vec<(ID, Vec<HeadlineName>)>, // IDs with inconsistent deletions across nodes
      Vec<(ID, Vec<HeadlineName>)>) // IDs with multiple defining nodes
{ let (id_toDelete_instructions, mut id_to_definers) =
    collect_instructions (viewforest);
  let mut inconsistent_deletion_skgids: Vec<(ID, Vec<HeadlineName>)> = Vec::new();
  let mut problematic_defining_skgids: Vec<(ID, Vec<HeadlineName>)> = Vec::new();
  { // filter to problematic instructions
    { // Collect inconsistent deletion instructions
      for (skgid, delete_set) in id_toDelete_instructions {
        if delete_set . len() == 2 {
          // Size 2 means both Delete and DoNotDelete are present.
          let definers : Vec<HeadlineName> =
            id_to_definers . get (&skgid) . cloned ()
            . unwrap_or_default ();
          inconsistent_deletion_skgids . push ((skgid, definers)); }} }
    { // Collect multiple defining containers
      for (skgid, definers) in id_to_definers . drain () {
        if definers . len () > 1 {
          // Multiple defining containers for this ID
          problematic_defining_skgids . push ((skgid, definers)); }} }}
  ( inconsistent_deletion_skgids,
    problematic_defining_skgids ) }

/// Collect delete instructions and defining containers.
fn collect_instructions(
  viewforest: &Tree<Viewnode>
) -> (HashMap<ID, HashSet<WhetherToDelete>>, // deletes
      HashMap<ID, Vec<HeadlineName>>) { // defining containers

  fn collect_instructions_rec(
    node_ref: NodeRef<Viewnode>,
    id_toDelete_instructions: &mut
      HashMap<ID, HashSet<WhetherToDelete>>,
    id_definers: &mut
      HashMap<ID, Vec<HeadlineName>>
  ) {
    let viewnode : &Viewnode = node_ref . value();
    if let ViewnodeKind::Vognode (Vognode::Unrestricted (t))
      = &viewnode . kind
    { { let skgid : &ID = &t . skgid;
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
          id_definers . entry(skgid . clone())
            // record this defining container
            . or_default ()
            . push (viewnode . headline_name ()); }}}
    for child in node_ref . children() { // recurse
      collect_instructions_rec(
        child,
        id_toDelete_instructions,
        id_definers); }} // end of inner function definition

  let mut id_toDelete_instructions
    : HashMap<ID, HashSet<WhetherToDelete>>
    = HashMap::new();
  let mut id_to_definers
    : HashMap<ID, Vec<HeadlineName>>
    = HashMap::new();
  collect_instructions_rec(
    viewforest . root(),
    &mut id_toDelete_instructions,
    &mut id_to_definers);
  (id_toDelete_instructions, id_to_definers) }
