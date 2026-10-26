//! The diff view's two restore requests ('docs/diff-view.org',
//! "Restoring what a change removed"). A save collects them with its
//! graph edits, as more NodeInstructions, nodes before relationships,
//! so that a relationship restored in one save may name a node restored
//! in it. Like every edit request they are consumed after the
//! skgsave-commit; a save that stops for a confirmation or fails leaves
//! them in the buffer.
//!
//! '(editRequest restoreNode)', on a historical phantom, writes the
//! node back from the before-graph of the graph straddle, out-links
//! included, and re-adds its in-links: each recorder at HEAD gains the
//! node in that list, at the index it had at HEAD (clamped to the
//! list's length), in the skgrepo that recorded it, unless the recorder
//! is itself deleted or lists it already.
//!
//! '(editRequest restoreRel)', on a removed membership, re-adds the
//! one relationship the headline stands for
//! ('relationship_to_viewparent'), as HEAD recorded it.

use crate::diff_view::readiness_requests::{
  relationship_to_viewparent, vognode_leaf_pid};
use crate::diff_view::change_leaves::{LeafKey, RelationshipAspect};
use crate::dbs::in_rust_graph::{InRustGraph, RecorderwardRelmap};
use crate::dbs::in_rust_graph::relation_accessors::{NodeRelation, RelationshipKey};
use crate::dbs::node_lookup::graphnode_from_graph;
use crate::graph_straddle::types::GraphStraddle;
use crate::types::errors::{HeadlineName, with_org_subheadlines};
use crate::types::misc::{ID, MSV, RelPartner, SkgConfig, SkgrepoName};
use crate::types::nodes::complete::Graphnode;
use crate::types::save::{NodeInstruction, SaveNode};
use crate::types::tree::forest::ViewForest;
use crate::types::viewnode::{RelationshipEditRequest, Phantom, ViewnodeKind, Vognode};

use ego_tree::NodeRef;
use std::collections::HashSet;

/// Whether the saved buffer carries a restore request, so that the save
/// needs the graph straddle.
pub fn viewforest_has_restore_requests (
  viewforest : &ViewForest,
) -> bool {
  viewforest . root () . descendants () . any (
    |node| match &node . value () . kind {
      ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Historical (h))) =>
        h . restoreNode_request,
      ViewnodeKind::Vognode (Vognode::Unrestricted (t)) =>
        t . relationshipEditRequest == Some (RelationshipEditRequest::RestoreRel),
      _ => false, } ) }

/// Add to 'instructions' (the save's disk-supplemented
/// NodeInstructions) what the restore requests in 'viewforest' ask
/// for: a Save of each restored node from 'straddle . before', and the
/// relationships restored, written into their recorders' Saves (the
/// buffer's own, when the save already writes the recorder). 'live' is
/// the graph before the save. Returns a warning for each request
/// dropped.
pub fn add_restore_instructions (
  viewforest   : &ViewForest,
  instructions : &mut Vec<NodeInstruction>,
  live         : &InRustGraph,
  straddle     : &GraphStraddle,
  config       : &SkgConfig,
) -> Vec<String> {
  let before : &InRustGraph = &straddle . before;
  let mut warnings : Vec<String> = Vec::new ();
  let restored_pids : HashSet<ID> =
    restore_nodes ( viewforest, instructions, live, before, &mut warnings );
  let exists_after_the_save = |pid : &ID| -> bool {
    restored_pids . contains (pid)
    || ( live . pid_of (pid) . is_some ()
         && ! instructions . iter () . any ( |instruction| matches! (
              instruction, NodeInstruction::Delete (delete)
              if live . pid_of (&delete . skgid) . as_ref () == Some (pid) )) ) };
  let mut relationships : Vec<RestoredRelationship> = Vec::new ();
  let mut restored_in_order : Vec<&ID> = restored_pids . iter () . collect ();
  restored_in_order . sort (); // so that the writes do not depend on hash order
  for pid in restored_in_order {
    for relationship in in_links_at_head (before, pid) {
      if exists_after_the_save (&relationship . key . recorder) {
        relationships . push (relationship); }}}
  for (key, headline) in restoreRel_requests (viewforest, live) {
    let Some (key) : Option<RelationshipKey> = key else {
      warnings . push ( with_org_subheadlines (
        "A restoreRel request was dropped: its headline stands for no relationship to its viewparent.",
        &[ headline ] ));
      continue; };
    let refusal : Option<&str> =
      if key . relation == NodeRelation::LinksTo {
        Some ("is a link, which lives in its mentioner's text") }
      else if ! exists_after_the_save (&key . recorder)
              || ! exists_after_the_save (&key . member) {
        Some ("has a deleted node at one end; restore it first") }
      else { None };
    if let Some (refusal) = refusal {
      warnings . push ( with_org_subheadlines (
        &format! ( "A restoreRel request was dropped: its relationship {}.",
                   refusal ),
        &[ headline ] ));
      continue; }
    match relationship_at_head (before, &key) {
      Some (relationship) => relationships . push (relationship),
      None => warnings . push ( with_org_subheadlines (
        "A restoreRel request names no relationship HEAD records; it was dropped.",
        &[ headline ] )), }}
  relationships . sort_by_key ( // so that each lands at its index at HEAD
    |relationship| relationship . index );
  for relationship in relationships {
    if let Err (refusal) = add_relationship (
      &relationship, instructions, live, &restored_pids, config ) {
      warnings . push (refusal); }}
  warnings }

/// One relationship to re-add as HEAD recorded it: 'index' is its
/// position in the recorder's list at HEAD, 'relRepo' the skgrepo that
/// recorded it, and 'stored_member' the ID the recorder stored.
#[derive(Clone, Debug)]
struct RestoredRelationship {
  key           : RelationshipKey,
  stored_member : ID,
  index         : usize,
  relRepo       : SkgrepoName,
}

/// Push a Save of each node a historical phantom's restoreNode names,
/// as the before-graph holds it. Returns the restored pids. A request
/// whose node the live graph has (as a node or an extra ID), or whose
/// node the save already writes, is dropped with a warning.
fn restore_nodes (
  viewforest   : &ViewForest,
  instructions : &mut Vec<NodeInstruction>,
  live         : &InRustGraph,
  before       : &InRustGraph,
  warnings     : &mut Vec<String>,
) -> HashSet<ID> {
  let mut restored : HashSet<ID> = HashSet::new ();
  for node in viewforest . root () . descendants () {
    let ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Historical (h)))
      : &ViewnodeKind = &node . value () . kind else { continue; };
    if ! h . restoreNode_request || restored . contains (&h . skgid)
    { continue; }
    let headline : HeadlineName = node . value () . headline_name ();
    let saved_already : bool = instructions . iter () . any (
      |instruction| instruction_skgid (instruction) == &h . skgid );
    let graphnode : Option<Graphnode> =
      graphnode_from_graph (before, &h . skgid);
    match graphnode {
      _ if live . pid_of (&h . skgid) . is_some () || saved_already =>
        warnings . push ( with_org_subheadlines (
          "A restoreNode request was dropped: that node exists.",
          &[ headline ] )),
      None =>
        warnings . push ( with_org_subheadlines (
          "A restoreNode request was dropped: HEAD has no such node.",
          &[ headline ] )),
      Some (graphnode) => {
        restored . insert (h . skgid . clone ());
        instructions . push (
          NodeInstruction::Save ( SaveNode (graphnode) )); }, }}
  restored }

/// The ID a NodeInstruction writes or deletes.
fn instruction_skgid (
  instruction : &NodeInstruction,
) -> &ID {
  match instruction {
    NodeInstruction::Save (SaveNode (graphnode)) => &graphnode . pid,
    NodeInstruction::Delete (delete) => &delete . skgid, } }

/// The relationships in which 'pid' is the member at HEAD, from the
/// before-graph's inbound indexes. Links live in text, so they are not
/// among them.
fn in_links_at_head (
  before : &InRustGraph,
  pid    : &ID,
) -> Vec<RestoredRelationship> {
  let inbound : [(NodeRelation, &RecorderwardRelmap); 4] = [
    (NodeRelation::Contains,      &before . contained_by),
    (NodeRelation::SubscribesTo,  &before . subscribers_of),
    (NodeRelation::Overrides,     &before . overriders_of),
    (NodeRelation::HidesFromSubs, &before . hiders_of) ];
  let mut relationships : Vec<RestoredRelationship> = Vec::new ();
  for (relation, recorders_of) in inbound {
    let mut recorders : Vec<&ID> = recorders_of . get (pid)
      . map ( |recorders| recorders . iter () . collect () )
      . unwrap_or_default ();
    recorders . sort (); // so that the writes do not depend on hash order
    for recorder in recorders {
      relationships . extend ( relationship_at_head ( before,
        &RelationshipKey { relation,
                           recorder : recorder . clone (),
                           member   : pid . clone () } )); }}
  relationships }

/// Where the before-graph's recorder lists the member under the
/// relation, and in which skgrepo; None when it does not.
fn relationship_at_head (
  before : &InRustGraph,
  key    : &RelationshipKey,
) -> Option<RestoredRelationship> {
  let recorder : Graphnode = graphnode_from_graph (before, &key . recorder) ?;
  let list : &[RelPartner<ID>] = relation_list (&recorder, key . relation) ?;
  let (index, partner) : (usize, &RelPartner<ID>) =
    list . iter () . enumerate () . find ( |(_, partner)|
      before . pid_of (&partner . member) . as_ref () . unwrap_or (&partner . member)
      == &key . member ) ?;
  Some ( RestoredRelationship {
    key           : key . clone (),
    stored_member : partner . member . clone (),
    index,
    relRepo       : partner . relRepo . clone () } ) }

/// A recorder's list of the relation, as a slice; None for links.
fn relation_list (
  node     : &Graphnode,
  relation : NodeRelation,
) -> Option<&[RelPartner<ID>]> {
  match relation {
    NodeRelation::Contains      => Some ( &node . contains ),
    NodeRelation::SubscribesTo  => Some ( node . subscribesTo . or_default () ),
    NodeRelation::HidesFromSubs => Some ( node . hidesFromSubs . or_default () ),
    NodeRelation::Overrides     => Some ( node . overrides . or_default () ),
    NodeRelation::LinksTo       => None, } }

/// The relationship each restoreRel request in 'viewforest' stands
/// for (None when its headline stands for none, as under an
/// acquisition folder), with its headline, which a warning names.
fn restoreRel_requests (
  viewforest : &ViewForest,
  live       : &InRustGraph,
) -> Vec<(Option<RelationshipKey>, HeadlineName)> {
  let pid_of = |skgid : &ID| -> ID {
    live . pid_of (skgid) . unwrap_or_else ( || skgid . clone () ) };
  viewforest . root () . descendants ()
    . filter ( |node| matches! ( &node . value () . kind,
      ViewnodeKind::Vognode (Vognode::Unrestricted (t)) if t . relationshipEditRequest == Some (RelationshipEditRequest::RestoreRel) ))
    . map ( |node : NodeRef<_>| {
      let headline : HeadlineName = node . value () . headline_name ();
      let key : Option<RelationshipKey> =
        vognode_leaf_pid (node . value (), &pid_of)
        . and_then ( |pid| relationship_to_viewparent (
          node, &pid, &pid_of, RelationshipAspect::Existence ))
        . and_then ( |leaf| match leaf {
          LeafKey::Relationship { relationship, .. } => Some (relationship),
          LeafKey::Node { .. } => None, } );
      (key, headline) } )
    . collect () }

/// Write 'relationship' into its recorder's Save: the save's own when
/// it writes the recorder, else a new one from the live graph. Nothing
/// is written when the recorder lists the member already (which a
/// restored recorder, written whole from HEAD, does). Refuses a
/// relationship recorded in a skgrepo the user does not own.
fn add_relationship (
  relationship  : &RestoredRelationship,
  instructions  : &mut Vec<NodeInstruction>,
  live          : &InRustGraph,
  restored_pids : &HashSet<ID>,
  config        : &SkgConfig,
) -> Result<(), String> {
  let key : &RelationshipKey = &relationship . key;
  if ! config . skgrepo_is_owned (&relationship . relRepo) {
    return Err ( format! (
      "The {} relationship from {} to {} was not restored: HEAD recorded it in {}, which you do not own.",
      key . relation . relation_name (), key . recorder, key . member,
      relationship . relRepo )); }
  let position : Option<usize> = instructions . iter () . position (
    |instruction| matches! ( instruction, NodeInstruction::Save (SaveNode (graphnode))
                             if graphnode . pid == key . recorder ));
  let index : usize = match position {
    Some (index) => index,
    None => {
      let recorder : Graphnode = graphnode_from_graph (live, &key . recorder)
        . ok_or_else ( || format! (
          "The {} relationship from {} to {} was not restored: its recorder is gone.",
          key . relation . relation_name (), key . recorder, key . member )) ?;
      instructions . push ( NodeInstruction::Save ( SaveNode (recorder) ));
      instructions . len () - 1 }, };
  let NodeInstruction::Save (SaveNode (recorder)) : &mut NodeInstruction =
    &mut instructions [index] else { unreachable! ("found as a Save") };
  let live_list : Vec<RelPartner<ID>> =
    graphnode_from_graph (live, &key . recorder)
    . and_then ( |node| relation_list (&node, key . relation)
                        . map ( |list| list . to_vec () ))
    . unwrap_or_default ();
  let list : &mut Vec<RelPartner<ID>> = match key . relation {
    NodeRelation::Contains      => &mut recorder . contains,
    NodeRelation::SubscribesTo  => specified (&mut recorder . subscribesTo, live_list),
    NodeRelation::HidesFromSubs => specified (&mut recorder . hidesFromSubs, live_list),
    NodeRelation::Overrides     => specified (&mut recorder . overrides, live_list),
    NodeRelation::LinksTo       => return Ok (( )), };
  let member_of = |stored : &ID| -> ID {
    if restored_pids . contains (stored) { stored . clone () }
    else { live . pid_of (stored) . unwrap_or_else ( || stored . clone () ) } };
  if list . iter () . any ( |partner| member_of (&partner . member) == key . member )
  { return Ok (( )); }
  let at : usize = relationship . index . min ( list . len () );
  list . insert ( at, RelPartner::at_relRepo (
    relationship . relRepo . clone (),
    relationship . stored_member . clone () ));
  Ok (( )) }

/// The list an MSV holds, made specified first from 'live_list' when
/// the save had no opinion about it, so that adding to it keeps the
/// rest of the list.
fn specified (
  msv       : &mut MSV<RelPartner<ID>>,
  live_list : Vec<RelPartner<ID>>,
) -> &mut Vec<RelPartner<ID>> {
  if msv . is_unspecified () { *msv = MSV::Specified (live_list); }
  msv . ensure_specified () }
