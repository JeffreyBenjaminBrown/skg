//! A cut from a forker as such F, in the forks folder of a note N,
//! makes N fork F ('resolve_forker_visibility' in
//! 'local_fieldintent_collection/resolve_visibility.rs'). F's
//! headline there claims nothing, as a forker-only member does. Left
//! so, the rerender would draw a member N forks that claims nothing,
//! which is a pending detachment: both clients would show it as one,
//! and the next save would read it as "stop forking F". So the save
//! gives F's headline its claim in the saved view, where it stands,
//! with what stands under it; the refresh then moves it among the
//! forkees. 'docs/forks.org', "Cutting from a forker".

use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::in_rust_graph::relation_accessors::NodeRelation;
use crate::types::misc::{ID, members_of};
use crate::types::save::{NodeInstruction, SaveNode};
use crate::types::tree::forest::ViewForest;
use crate::types::viewnode::{Folder, Occurrence, Viewnode, ViewnodeKind, Vognode};

use ego_tree::{NodeId, NodeRef};
use std::collections::{HashMap, HashSet};

/// Give its claim to each member claiming nothing of the forks folder
/// of an editable occurrence of a note N, where N's Save in
/// 'node_instructions' forks the member's note and the graph records
/// no fork of it by N: the save makes N fork it.
pub fn claim_forkers_the_save_makes_forkees (
  viewforest        : &mut ViewForest,
  graph             : &InRustGraph,
  node_instructions : &[NodeInstruction],
) {
  let forks_after_save : HashMap<ID, HashSet<ID>> =
    node_instructions . iter ()
    . filter_map ( |instruction| match instruction {
      NodeInstruction::Save (SaveNode (node)) => Some ((
        graph . pid_else_raw_id (&node . pid),
        members_of ( node . forks . or_default () ) . iter ()
          . map ( |gnid| graph . pid_else_raw_id (gnid) ) . collect () )),
      NodeInstruction::Delete (_) => None, } )
    . collect ();
  if forks_after_save . is_empty () { return; }
  let made_forkees : Vec<NodeId> =
    viewforest . nodes ()
    . filter ( |member| {
      let ViewnodeKind::Vognode (Vognode::Occurrence (forker)) = &member . value () . kind
        else { return false; };
      if forker . claimsMembership { return false; }
      let Some (folder) : Option<NodeRef<Viewnode>> = member . parent ()
        else { return false; };
      if ! matches! ( folder . value () . kind, ViewnodeKind::Folder (Folder::Forks) )
      { return false; }
      let Some (note_at) : Option<NodeRef<Viewnode>> = folder . parent ()
        else { return false; };
      let ViewnodeKind::Vognode (Vognode::Occurrence (note)) = &note_at . value () . kind
        else { return false; };
      saves_a_new_fork (graph, &forks_after_save, note, forker) } )
    . map ( |member| member . id () )
    . collect ();
  for member_at in made_forkees {
    if let Some (mut member) = viewforest . get_mut (member_at) {
      if let ViewnodeKind::Vognode (Vognode::Occurrence (forker)) =
        &mut member . value () . kind
      { forker . claimsMembership = true; }}}}

/// Whether 'note', an editable occurrence, forks 'forker' after the
/// save by its Save in 'forks_after_save', and by no record the graph
/// has.
fn saves_a_new_fork (
  graph            : &InRustGraph,
  forks_after_save : &HashMap<ID, HashSet<ID>>,
  note             : &Occurrence,
  forker           : &Occurrence,
) -> bool {
  if note . is_writeProtected () { return false; }
  let note_pid : ID = graph . pid_else_raw_id (&note . gnid);
  let forker_pid : ID = graph . pid_else_raw_id (&forker . gnid);
  forks_after_save . get (&note_pid)
    . is_some_and ( |forks| forks . contains (&forker_pid) )
  && ! graph . outbound_gnids_for_relation (&note_pid, NodeRelation::Forks)
    . iter () . any ( |gnid| graph . pid_else_raw_id (gnid) == forker_pid ) }
