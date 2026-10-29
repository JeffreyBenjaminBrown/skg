use crate::dbs::in_rust_graph::InRustGraph;
use crate::diff_view::readiness_requests::{
  NamedLeafSet, RequestedReadiness, readiness_requests_in_viewforest};
use crate::types::tree::forest::ViewForest;
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
  let mut inconsistent_deletion_gnids: Vec<(ID, Vec<HeadlineName>)> = Vec::new();
  let mut problematic_defining_gnids: Vec<(ID, Vec<HeadlineName>)> = Vec::new();
  { // filter to problematic instructions
    { // Collect inconsistent deletion instructions
      for (gnid, delete_set) in id_toDelete_instructions {
        if delete_set . len() == 2 {
          // Size 2 means both Delete and DoNotDelete are present.
          let definers : Vec<HeadlineName> =
            id_to_definers . get (&gnid) . cloned ()
            . unwrap_or_default ();
          inconsistent_deletion_gnids . push ((gnid, definers)); }} }
    { // Collect multiple defining containers
      for (gnid, definers) in id_to_definers . drain () {
        if definers . len () > 1 {
          // Multiple defining containers for this ID
          problematic_defining_gnids . push ((gnid, definers)); }} }}
  ( inconsistent_deletion_gnids,
    problematic_defining_gnids ) }

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
    if let ViewnodeKind::Vognode (Vognode::Occurrence (t))
      = &viewnode . kind
    { { let gnid : &ID = &t . gnid;
        if ! t . is_writeProtected () { // write-protected nodes contribute no instructions
          let delete_instruction : WhetherToDelete =
            if matches!(t . edit_request (),
                        Some (&NodeEditRequest::Delete)) {
              WhetherToDelete::Delete
            } else { WhetherToDelete::DoNotDelete };
          id_toDelete_instructions // record delete_instruction
            . entry(gnid . clone())
            . or_insert_with (HashSet::new)
            . insert (delete_instruction);
          id_definers . entry(gnid . clone())
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

/// PURPOSE:
/// Find the change leaves that two headlines of a buffer the user
/// asked to save name with contradictory readiness requests, one
/// '(ready ...)' and one '(unready ...)'. Each is returned as the set
/// both name ('NamedLeafSet'), with the headlines that name it, in
/// buffer order. Requests that agree contradict nothing.
/// .
/// STRATEGY:
/// Which leaves a request names is 'leaves_named_at'
/// ('server/diff_view/readiness_requests.rs'), resolving IDs through
/// 'graph'. Without the change forest, an 'all' request, or a
/// changeset marker's, names a set of leaves that the forest fills
/// in; two requests contradict where their sets can share a leaf
/// ('NamedLeafSet::overlap'), and the shared part is what is named.
pub fn find_contradictory_readiness_requests (
  viewforest : &ViewForest,
  graph      : &InRustGraph,
) -> Vec<(NamedLeafSet, Vec<HeadlineName>)> {
  let requests : Vec<RequestedReadiness> =
    readiness_requests_in_viewforest (viewforest, graph);
  let mut contradictions : Vec<(NamedLeafSet, Vec<HeadlineName>)> = Vec::new ();
  for (position, ready) in requests . iter () . enumerate () {
    for unready in &requests [position + 1 ..] {
      if ready . ready == unready . ready { continue; }
      for ready_set in &ready . named {
        for unready_set in &unready . named {
          let Some (shared) = ready_set . overlap (unready_set)
            else { continue; };
          let headlines : &mut Vec<HeadlineName> =
            match contradictions . iter () . position ( |(set, _)| *set == shared ) {
              Some (index) => &mut contradictions [index] . 1,
              None => { contradictions . push (( shared, Vec::new () ));
                        &mut contradictions . last_mut () . unwrap () . 1 }, };
          for headline in [ &ready . headline, &unready . headline ] {
            if ! headlines . contains (headline) {
              headlines . push ( headline . clone () ); }} }} }}
  contradictions . into_iter ()
    . map ( |(set, mut headlines)| {
      headlines . sort_by_key ( |headline| requests . iter ()
        . position ( |request| request . headline == *headline ) );
      ( set, headlines ) } )
    . collect () }
