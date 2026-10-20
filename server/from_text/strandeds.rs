//! Moving a node between skgrepos: the save decides what the move
//! strands ('docs/diff-view.org', "Strandeds").
//!
//! A relationship touching a moved node, in either direction and
//! whether or not any view shows it, is STRANDED when it sat at its
//! default relRepo before the save and the sticky-else-default rule
//! ('apply_sticky_relRepos_in_graph_with_prospective_homes') would
//! leave it off its new default. A stranded relationship left more
//! public than its member's new home would leak that member's ID; that
//! only happens to an inbound relationship of a privatizing move, and
//! the save raises it to the new default (the new home) automatically.
//! The others -- those of a publicizing move, left more private than
//! they need be -- are listed for the user, who chooses which to
//! reset to their new defaults.
//!
//! Mentions are not relationships with a relRepo (links live in their
//! recorder's text), so they cannot be stranded.

use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::in_rust_graph::relation_accessors::NodeRelation;
use crate::dbs::node_lookup::graphnode_from_graph;
use crate::from_text::supplement_from_disk::hide_skgrepo;
use crate::types::misc::{ID, MSV, RelPartner, SkgConfig, SkgrepoName};
use crate::types::nodes::complete::Graphnode;
use crate::types::phantom::home_from_disk;
use crate::types::save::{NodeInstruction, SaveNode, SkgrepoMove};

use std::collections::{HashMap, HashSet};

/// The relations whose relationships carry a relRepo.
const RELATIONS_WITH_RELREPOS : [NodeRelation; 4] = [
  NodeRelation::Contains,
  NodeRelation::SubscribesTo,
  NodeRelation::HidesFromSubs,
  NodeRelation::Overrides, ];

/// One relationship: 'recorder' lists 'member' (a primary ID, or the
/// stored ID when it resolves to no node) under 'relation'.
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct RelationshipKey {
  pub relation : NodeRelation,
  pub recorder : ID,
  pub member   : ID,
}

/// See the module comment.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct StrandedRelationship {
  pub relationship : RelationshipKey,
  /// Where the save would leave it.
  pub relRepo      : SkgrepoName,
  /// Where resetting it to its default puts it.
  pub new_default  : SkgrepoName,
  /// It would leak its member's ID, so the save raises it without asking.
  pub raised_automatically : bool,
}

/// Whether the user has reviewed this save's strandeds, and if so,
/// which ones they chose to reset to their defaults.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub enum StrandedsReview {
  #[default]
  NotReviewed,
  Reviewed (HashSet<RelationshipKey>),
}

/// Every relationship that touches a node 'skgrepo_moves' moves and
/// that the save would strand, each once, sorted.
/// 'instructions' is the save's supplemented nodeInstructions, whose
/// relRepos the sticky-else-default rule has already resolved;
/// 'graph' is the graph before the save.
pub fn stranded_relationships (
  skgrepo_moves : &[SkgrepoMove],
  instructions  : &[NodeInstruction],
  graph         : &InRustGraph,
  config        : &SkgConfig,
) -> Vec<StrandedRelationship> {
  if skgrepo_moves . is_empty () { return Vec::new (); }
  let saves : HashMap<ID, &Graphnode> =
    instructions . iter ()
    . filter_map ( |instruction| match instruction {
      NodeInstruction::Save (SaveNode (node)) =>
        Some (( node . pid . clone (), node )),
      NodeInstruction::Delete (_) => None, } )
    . collect ();
  let deleted : HashSet<ID> =
    instructions . iter ()
    . filter_map ( |instruction| match instruction {
      NodeInstruction::Delete (delete) => Some (delete . skgid . clone ()),
      NodeInstruction::Save (_) => None, } )
    . collect ();
  let resolve = |skgid : &ID| -> ID {
    graph . pid_of (skgid) . unwrap_or_else ( || skgid . clone () ) };
  let home_before = |skgid : &ID| -> Option<SkgrepoName> {
    graph . pid_and_skgrepo (skgid) . map ( |(_pid, home)| home )
      . or_else ( || home_from_disk (skgid, config) ) };
  let home_after = |skgid : &ID| -> Option<SkgrepoName> {
    saves . get ( &resolve (skgid) )
      . map ( |node| node . home_skgrepo . clone () )
      . or_else ( || home_before (skgid) ) };
  // The recorder as the save leaves it: its saved form, else the graph's.
  let recorder_after = |pid : &ID| -> Option<Graphnode> {
    if deleted . contains (pid) { return None; }
    saves . get (pid) . map ( |node| (*node) . clone () )
      . or_else ( || graphnode_from_graph (graph, pid) ) };
  let mut seen : HashSet<RelationshipKey> = HashSet::new ();
  let mut strandeds : Vec<StrandedRelationship> = Vec::new ();
  for skgrepo_move in skgrepo_moves {
    let moved : &ID = &skgrepo_move . pid;
    let mut recorders : Vec<(ID, NodeRelation)> =
      RELATIONS_WITH_RELREPOS . iter ()
      . map ( |relation| ( moved . clone (), *relation ))
      . collect ();
    for relation in RELATIONS_WITH_RELREPOS {
      for recorder in graph . inbound_pids_for_relation (moved, relation) {
        recorders . push (( recorder, relation )); }}
    for (recorder_pid, relation) in recorders {
      let Some (recorder) = recorder_after (&recorder_pid) else { continue; };
      let Some (recorder_home_before) = home_before (&recorder_pid)
        else { continue; };
      let recorder_home_after : SkgrepoName = recorder . home_skgrepo . clone ();
      let subscribes_before : Vec<RelPartner<ID>> =
        graph . nodes . get (&recorder_pid)
        . map ( |node| node . subscribesTo . or_default () . to_vec () )
        . unwrap_or_default ();
      let subscribes_after : Vec<RelPartner<ID>> =
        recorder . subscribesTo . or_default () . to_vec ();
      for partner in relation_list (&recorder, relation) {
        let member : ID = resolve (&partner . member);
        if recorder_pid != *moved && member != *moved { continue; }
        let relationship : RelationshipKey = RelationshipKey {
          relation, recorder : recorder_pid . clone (), member : member . clone () };
        if ! seen . insert (relationship . clone ()) { continue; }
        let default_before : SkgrepoName = default_relRepo (
          graph, config, relation, &recorder_home_before, &partner . member,
          &subscribes_before, &resolve, &home_before );
        let default_after : SkgrepoName = default_relRepo (
          graph, config, relation, &recorder_home_after, &partner . member,
          &subscribes_after, &resolve, &home_after );
        if partner . relRepo != default_before
          || partner . relRepo == default_after { continue; }
        strandeds . push ( StrandedRelationship {
          raised_automatically :
            home_after (&member) . is_some_and ( |member_home|
              config . is_strictly_more_public (&partner . relRepo, &member_home) ),
          relationship,
          relRepo     : partner . relRepo . clone (),
          new_default : default_after, } ); }} }
  strandeds . sort_by ( |a, b| key_order (&a . relationship)
                        . cmp ( &key_order (&b . relationship) ));
  strandeds }

/// Reset each of 'strandeds' to its new default, in its recorder's
/// saved form, adding a save of the recorder when the save did not
/// already write it.
pub fn reset_to_default_relationships (
  instructions : &mut Vec<NodeInstruction>,
  strandeds    : &[&StrandedRelationship],
  graph        : &InRustGraph,
) {
  let resolve = |skgid : &ID| -> ID {
    graph . pid_of (skgid) . unwrap_or_else ( || skgid . clone () ) };
  for stranded in strandeds {
    let recorder_pid : &ID = &stranded . relationship . recorder;
    let position : Option<usize> =
      instructions . iter () . position ( |instruction| matches! (
        instruction,
        NodeInstruction::Save (SaveNode (node)) if node . pid == *recorder_pid ));
    let position : usize = match position {
      Some (position) => position,
      None => {
        let Some (mut recorder) = graphnode_from_graph (graph, recorder_pid)
          else { continue; };
        // Like a buffer-authored node: the save's publication gate
        // rederives this from disk.
        recorder . overPrivateText_telescope = false;
        instructions . push ( NodeInstruction::Save (SaveNode (recorder)) );
        instructions . len () - 1 }, };
    let NodeInstruction::Save (SaveNode (recorder)) = &mut instructions [position]
      else { continue; };
    for partner in relation_list_mut (recorder, stranded . relationship . relation) {
      if resolve (&partner . member) == stranded . relationship . member {
        partner . relRepo = stranded . new_default . clone (); }} }}

/// The warning for a relationship the save raised without asking.
pub fn raised_relationship_warning (
  stranded : &StrandedRelationship,
) -> String {
  format! (
    "Raised the relationship by which {} {} {} from '{}' to '{}': at '{}' it would publish the ID of a node that moved to a more private skgrepo.",
    stranded . relationship . recorder,
    stranded . relationship . relation . relation_name (),
    stranded . relationship . member,
    stranded . relRepo, stranded . new_default, stranded . relRepo ) }

/// The relationships in a node's list for 'relation'.
fn relation_list (
  node     : &Graphnode,
  relation : NodeRelation,
) -> Vec<RelPartner<ID>> {
  match relation {
    NodeRelation::Contains      => node . contains . clone (),
    NodeRelation::SubscribesTo  => node . subscribesTo . or_default () . to_vec (),
    NodeRelation::HidesFromSubs => node . hidesFromSubs . or_default () . to_vec (),
    NodeRelation::Overrides     => node . overrides . or_default () . to_vec (),
    NodeRelation::LinksTo       => Vec::new (), } }

fn relation_list_mut (
  node     : &mut Graphnode,
  relation : NodeRelation,
) -> &mut [RelPartner<ID>] {
  fn msv_list (
    msv : &mut MSV<RelPartner<ID>>,
  ) -> &mut [RelPartner<ID>] {
    match msv {
      MSV::Specified (list) => list . as_mut_slice (),
      MSV::Unspecified => &mut [], } }
  match relation {
    NodeRelation::Contains      => node . contains . as_mut_slice (),
    NodeRelation::SubscribesTo  => msv_list (&mut node . subscribesTo),
    NodeRelation::HidesFromSubs => msv_list (&mut node . hidesFromSubs),
    NodeRelation::Overrides     => msv_list (&mut node . overrides),
    NodeRelation::LinksTo       => &mut [], } }

/// A relationship's default relRepo, given its recorder's home, its
/// member's home ('home_of'), and, for a hide, the recorder's
/// subscriptions: the rule a new relationship follows
/// ('apply_sticky_relRepos_in_graph_with_prospective_homes').
fn default_relRepo (
  graph         : &InRustGraph,
  config        : &SkgConfig,
  relation      : NodeRelation,
  recorder_home : &SkgrepoName,
  member        : &ID,
  subscribes    : &[RelPartner<ID>],
  resolve       : &dyn Fn (&ID) -> ID,
  home_of       : &dyn Fn (&ID) -> Option<SkgrepoName>,
) -> SkgrepoName {
  if relation == NodeRelation::HidesFromSubs {
    return hide_skgrepo (
      graph, config, recorder_home, member, subscribes, resolve, home_of ); }
  match home_of (member) {
    Some (member_home) => config . default_relRepo (recorder_home, &member_home),
    None => recorder_home . clone (), } }

fn key_order (
  key : &RelationshipKey,
) -> (&str, &str, &str) {
  ( key . relation . relation_name (), key . recorder . as_str (),
    key . member . as_str () ) }

#[cfg(test)]
#[allow(non_snake_case)]
#[path = "../../tests/unit/strandeds.rs"]
mod tests;
