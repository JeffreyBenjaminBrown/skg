//! A reply typed in the unread list files the message it answers.
//!
//! An owned subscriber F draws, in its forks folder, each
//! subscribee S as such: S's members that F neither contains nor
//! hides ('less_what_the_subscriber_integrates' in
//! 'server/refresh/goal.rs'). Read as mail, those are the messages F's
//! owner has not touched. Writing in one of them (a reply typed under
//! it, which forks it when it is foreign, or any other edit of it or
//! of its content, at any depth) is touching it, so the save files that
//! member in F: it appends the member to F's contains, by the ID S's
//! list names (the original, as every contains list records it; a fork
//! of it is drawn in its place wherever F's owner's perspective is
//! followed). Then the member is no longer counted unintegrated. The
//! save moves its headline, with what was typed under it, to the end
//! of F's content in the saved view, as the user would have, so that
//! the redrawn view shows the reply there, editable, where it now
//! lives. (Left in place, the headline would hold its node's one
//! editability while the rerender drew the filed member, earlier in
//! level order, write-protected and empty.)
//!
//! Only an editable F the user owns, drawn where the save reads its
//! contains, files anything, and only a member S's list names. Nothing
//! is filed in an F the save would otherwise leave as the graph has it
//! because it changed since the buffer was drawn, nor what F already
//! contains or hides, or holds a mirror of. 'docs/forks.org',
//! "Writing in an unintegrated child integrates it".

use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::in_rust_graph::mirrors::{Mirrors, mirrors_of};
use crate::dbs::in_rust_graph::relation_accessors::NodeRelation;
use crate::dbs::node_lookup::graphnode_from_graph;
use crate::from_text::local_fieldintent_collection::predicates::occurrence_child_counts_as_content;
use crate::skgrepo_sets::SkgrepoRestriction;
use crate::types::misc::{ID, RelPartner, SkgConfig, SkgrepoName};
use crate::types::nodes::complete::Graphnode;
use crate::types::save::{ForkSpec, NodeInstruction, SaveNode};
use crate::types::tree::forest::ViewForest;
use crate::types::tree::moves::move_to_last_child;
use crate::types::viewnode::{
  Folder, NodeEditRequest, Occurrence, Viewnode, ViewnodeKind, Vognode};

use ego_tree::{NodeId, NodeRef};
use std::collections::HashSet;

/// One member to file: the subscriber's occurrence and ID, and the
/// member's occurrence and the ID its subscribee's list names.
struct Filing {
  subscriber_at : NodeId,
  subscriber    : ID,
  member_at     : NodeId,
  member        : ID,
}

/// File each subscribee member the save writes in: append it to the
/// contains of its subscriber's Save in 'node_instructions' (the
/// planned one if there is one, else one from the graph), and move its
/// headline to the end of the subscriber's content in 'viewforest'.
/// What the save writes: the nodes 'node_instructions' saves or
/// deletes, and the originals 'fork_specs' clone. 'left_stale' are
/// the pids the save leaves as the graph has them, because they
/// changed since the buffer was drawn and the buffer did not edit them
/// ('without_unedited_stale_saves'): writing one would refuse the save.
pub fn file_subscribee_members_written_in (
  viewforest        : &mut ViewForest,
  graph             : &InRustGraph,
  config            : &SkgConfig,
  restriction       : &SkgrepoRestriction,
  fork_specs        : &[ForkSpec],
  left_stale        : &HashSet<ID>,
  node_instructions : &mut Vec<NodeInstruction>,
) -> Result<(), String> {
  let written : HashSet<ID> =
    node_instructions . iter ()
    . map ( |instruction| match instruction {
      NodeInstruction::Save (SaveNode (node)) => node . pid . clone (),
      NodeInstruction::Delete (delete) => delete . gnid . clone (), } )
    . chain ( fork_specs . iter () . map ( |spec| spec . original_gnid . clone () ))
    . map ( |gnid| graph . pid_else_raw_id (&gnid) )
    . collect ();
  if written . is_empty () { return Ok (( )); }
  for filing in
    subscribee_members_written_in (viewforest, graph, config, restriction, &written)
  { if left_stale . contains (&graph . pid_else_raw_id (&filing . subscriber))
    { continue; }
    if file_in_subscriber (
         graph, config, restriction, node_instructions,
         &filing . subscriber, &filing . member )
    { move_to_last_child (
        viewforest, filing . subscriber_at, filing . member_at ) ?; }}
  Ok (( )) }

/// Each member A of the content of a subscribee S as such, in the
/// forks folder of an occurrence F whose contains the save writes
/// ('saves_its_contains'), such that S's contains names A and an
/// editable occurrence at A or in its content, at any depth, is of a
/// node in 'written'. A child of S that S's list does not name is no
/// member of S, only a node the user placed there ('docs/sharing-model.org').
fn subscribee_members_written_in (
  viewforest  : &ViewForest,
  graph       : &InRustGraph,
  config      : &SkgConfig,
  restriction : &SkgrepoRestriction,
  written     : &HashSet<ID>,
) -> Vec<Filing> {
  let mut filings : Vec<Filing> = Vec::new ();
  for folder in viewforest . nodes () {
    if ! matches! ( folder . value () . kind, ViewnodeKind::Folder (Folder::Forks) )
    { continue; }
    let Some (subscriber) : Option<NodeRef<Viewnode>> = folder . parent ()
      else { continue; };
    let Some (subscriber_occurrence) : Option<&Occurrence> =
      occurrence_of (subscriber) else { continue; };
    if ! saves_its_contains (subscriber, subscriber_occurrence, config) { continue; }
    for subscribee in folder . children () {
      let Some (subscribee_occurrence) : Option<&Occurrence> = occurrence_of (subscribee)
        else { continue; };
      if ! subscribee_occurrence . claimsMembership { continue; }
      let its_list : HashSet<ID> =
        graph . outbound_gnids_for_relation_gated (
          &graph . pid_else_raw_id (&subscribee_occurrence . gnid),
          NodeRelation::Contains, restriction )
        . iter () . map ( |gnid| graph . pid_else_raw_id (gnid) ) . collect ();
      for member in subscribee . children () {
        let Some (occurrence) : Option<&Occurrence> = occurrence_of (member)
          else { continue; };
        // A shown overrider stands for the original it is shown in
        // place of, which is what the subscribee's list names.
        let named : ID = occurrence . collected_gnid ();
        if occurrence_child_counts_as_content (occurrence)
           && its_list . contains (&graph . pid_else_raw_id (&named))
           && is_written_in (member, graph, written)
        { filings . push ( Filing {
            subscriber_at : subscriber . id (),
            subscriber    : subscriber_occurrence . gnid . clone (),
            member_at     : member . id (),
            member        : named, } ); }}}}
  filings }

/// Whether the occurrence at 'node', or one in its content at any
/// depth, is editable and of a node in 'written'. Folders are not
/// content: what a member's containers folder shows, say, is not
/// written in the member.
fn is_written_in (
  node    : NodeRef<Viewnode>,
  graph   : &InRustGraph,
  written : &HashSet<ID>,
) -> bool {
  let Some (occurrence) : Option<&Occurrence> = occurrence_of (node)
    else { return false; };
  ( ! occurrence . is_writeProtected ()
    && written . contains (&graph . pid_else_raw_id (&occurrence . gnid)) )
  || node . children () . any ( |child| is_written_in (child, graph, written) ) }

/// Whether the save writes the contains of the occurrence at 'node':
/// it is editable, of a node in a skgrepo the user owns, not marked
/// for deletion, and not itself a subscribee as such.
fn saves_its_contains (
  node       : NodeRef<Viewnode>,
  occurrence : &Occurrence,
  config     : &SkgConfig,
) -> bool {
  let subscribee_as_such : bool =
    occurrence . claimsMembership
    && node . parent () . is_some_and ( |parent| matches! (
      parent . value () . kind, ViewnodeKind::Folder (Folder::Forks) ));
  ! occurrence . is_writeProtected ()
  && config . skgrepo_is_owned (&occurrence . home_skgrepo)
  && ! matches! ( occurrence . edit_request (), Some (NodeEditRequest::Delete) )
  && ! subscribee_as_such }

fn occurrence_of<'a> (
  node : NodeRef<'a, Viewnode>,
) -> Option<&'a Occurrence> {
  match &node . value () . kind {
    ViewnodeKind::Vognode (Vognode::Occurrence (occurrence)) => Some (occurrence),
    _ => None, } }

/// Append 'member' to the contains of the Save of 'subscriber' in
/// 'node_instructions', adding one from the graph if there is none,
/// unless that Save deletes the subscriber or already contains or
/// hides the member, or contains a mirror of it, under the skgrepo
/// restriction (filing the member too could draw that mirror twice
/// there). The record is
/// at the relRepo a new record between the two would take
/// ('SkgConfig::default_relRepo'). Whether it appended.
fn file_in_subscriber (
  graph             : &InRustGraph,
  config            : &SkgConfig,
  restriction       : &SkgrepoRestriction,
  node_instructions : &mut Vec<NodeInstruction>,
  subscriber        : &ID,
  member            : &ID,
) -> bool {
  let subscriber : ID = graph . pid_else_raw_id (subscriber);
  let planned : Option<usize> =
    node_instructions . iter () . position ( |instruction| match instruction {
      NodeInstruction::Save (SaveNode (node)) => node . pid == subscriber,
      NodeInstruction::Delete (delete) =>
        graph . pid_else_raw_id (&delete . gnid) == subscriber, } );
  let mut node : Graphnode = match planned {
    Some (i) => match &node_instructions [i] {
      NodeInstruction::Save (SaveNode (node)) => node . clone (),
      NodeInstruction::Delete (_) => return false, },
    None => match graphnode_from_graph (graph, &subscriber) {
      Some (node) => node,
      None => return false, }, };
  let member : ID = graph . pid_else_raw_id (member);
  let member_mirrors : Mirrors = mirrors_of (graph, restriction, &member);
  let stands_for_member = |gnid : &ID| -> bool {
    let pid : ID = graph . pid_else_raw_id (gnid);
    pid == member || member_mirrors . contains (&pid) };
  if node . contains . iter () . any ( |record| stands_for_member (&record . member) )
     || node . hides . or_default () . iter () . any ( |record|
          graph . pid_else_raw_id (&record . member) == member )
  { return false; }
  let member_home : Option<SkgrepoName> =
    graph . pid_and_skgrepo (&member) . map ( |(_pid, home)| home );
  let relRepo : SkgrepoName = match member_home {
    Some (member_home) => config . default_relRepo (&node . home_skgrepo, &member_home),
    None => node . home_skgrepo . clone (), };
  node . contains . push ( RelPartner::at_relRepo ( relRepo, member ));
  let save : NodeInstruction = NodeInstruction::Save (SaveNode (node));
  match planned {
    Some (i) => node_instructions [i] = save,
    None => node_instructions . push (save), }
  true }
