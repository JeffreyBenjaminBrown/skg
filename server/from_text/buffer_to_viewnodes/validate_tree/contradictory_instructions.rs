use crate::types::viewnode::NodeEditRequest;
use crate::types::maybe_placed_viewnode::{MpViewnode, MpViewnodeKind};
use crate::types::maybe_placed_viewnode::MpVognode;
use crate::types::misc::{ID, RepoName};

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
/// - Two nodes with the same ID have different repos,
///   even if some are write-protected.
/// .
/// STRATEGY:
/// Builds a map from IDs to sets of WhetherToDelete.
/// After traversing the tree, reports every key (ID)
/// for which the associated value (set) has size 2.
/// Also builds a map from IDs to count of defining containers,
/// and a map from IDs to sets of repos. */
pub fn find_inconsistent_instructions(
  viewforest: &Tree<MpViewnode>
) -> (Vec<ID>, // IDs with inconsistent deletions across nodes
      Vec<ID>, // IDs with multiple defining nodes
      Vec<(ID, // IDs with inconsistent repos
           HashSet<RepoName>)>)
{ let (id_toDelete_instructions, id_to_definer_count, id_to_repos) =
    collect_instructions (viewforest);
  let mut inconsistent_deletion_ids: Vec<ID> = Vec::new();
  let mut problematic_defining_ids: Vec<ID> = Vec::new();
  let mut inconsistent_repo_ids: Vec<(ID, HashSet<RepoName>)> =
    Vec::new();
  { // filter to problematic instructions
    { // Collect inconsistent deletion instructions
      for (id, delete_set) in id_toDelete_instructions {
        if delete_set . len() == 2 {
          // Size 2 means both Delete and DoNotDelete are present.
          inconsistent_deletion_ids . push (id); }} }
    { // Collect multiple defining containers
      for (id, count) in id_to_definer_count {
        if count > 1 {
          // Multiple defining containers for this ID
          problematic_defining_ids . push (id); }} }
    { // Collect inconsistent repos
      for (id, repos) in id_to_repos {
        if repos . len() > 1 {
          // Multiple different repos for this ID
          inconsistent_repo_ids . push((id, repos)); }} }}
  ( inconsistent_deletion_ids,
    problematic_defining_ids,
    inconsistent_repo_ids ) }

/// Collect delete instructions, defining containers, and repos.
fn collect_instructions(
  viewforest: &Tree<MpViewnode>
) -> (HashMap<ID, HashSet<WhetherToDelete>>, // deletes
      HashMap<ID, usize>, // defining containers
      HashMap<ID, HashSet<RepoName>>) { // repos

  fn collect_instructions_rec(
    node_ref: NodeRef<MpViewnode>,
    id_toDelete_instructions: &mut
      HashMap<ID, HashSet<WhetherToDelete>>,
    id_defining_count: &mut
      HashMap<ID, usize>,
    id_to_repos: &mut
      HashMap<ID, HashSet<RepoName>>
  ) {
    let viewnode : &MpViewnode = node_ref . value();
    if let MpViewnodeKind::Vognode (MpVognode::Active (t))
      = &viewnode . kind
    { if let Some (id) = &t . id {
        if ! t . is_writeProtected () { // write-protected nodes contribute no instructions
          let delete_instruction : WhetherToDelete =
            if matches!(t . edit_request (),
                        Some (&NodeEditRequest::Delete)) {
              WhetherToDelete::Delete
            } else { WhetherToDelete::DoNotDelete };
          id_toDelete_instructions // record delete_instruction
            . entry(id . clone())
            . or_insert_with (HashSet::new)
            . insert (delete_instruction);
          *id_defining_count . entry(id . clone())
            // increment the count for this defining container
            . or_insert (0) += 1; }
        if let Some (repo_str) = &t . home_repo {
          // Collect repo for this ID
          let repo : RepoName =
            RepoName::from(repo_str . as_str());
          id_to_repos
            . entry(id . clone())
            . or_insert_with (HashSet::new)
            . insert (repo); }}}
    for child in node_ref . children() { // recurse
      collect_instructions_rec(
        child,
        id_toDelete_instructions,
        id_defining_count,
        id_to_repos); }} // end of inner function definition

  let mut id_toDelete_instructions
    : HashMap<ID, HashSet<WhetherToDelete>>
    = HashMap::new();
  let mut id_to_definer_count
    : HashMap<ID, usize>
    = HashMap::new();
  let mut id_to_repos
    : HashMap<ID, HashSet<RepoName>>
    = HashMap::new();
  collect_instructions_rec(
    viewforest . root(),
    &mut id_toDelete_instructions,
    &mut id_to_definer_count,
    &mut id_to_repos);
  (id_toDelete_instructions, id_to_definer_count, id_to_repos) }
