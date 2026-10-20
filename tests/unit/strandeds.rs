// Unit tests for the analysis of strandeds, over in-memory graphs. They port
// the scenarios of the client-side analysis this replaced. The privacy
// order is public, private, trusted.

use super::*;
use crate::types::misc::Skgrepo;
use crate::types::nodes::complete::empty_graphnode;

use std::path::PathBuf;

fn config () -> SkgConfig {
  let mut config : SkgConfig = SkgConfig::dummyFromSkgrepos (
    [ "public", "private", "trusted" ] . into_iter ()
    . map ( |name| ( SkgrepoName::from (name), Skgrepo {
      name : SkgrepoName::from (name), abbreviation : None,
      path : PathBuf::from ("/nonexistent") . join (name), owned : true } ))
    . collect () );
  config . skgrepo_order = [ "public", "private", "trusted" ] . into_iter ()
    . map (SkgrepoName::from) . collect ();
  config }

/// A node at 'home' containing each (member, relRepo).
fn node (
  pid      : &str,
  home     : &str,
  contains : &[(&str, &str)],
) -> Graphnode {
  Graphnode {
    pid          : ID::from (pid),
    title        : pid . to_string (),
    home_skgrepo : SkgrepoName::from (home),
    contains     : contains . iter ()
      . map ( |(member, relRepo)| RelPartner::at_relRepo (
        SkgrepoName::from (*relRepo), ID::from (*member) ))
      . collect (),
    .. empty_graphnode () }}

fn save (
  node : Graphnode,
) -> NodeInstruction {
  NodeInstruction::Save (SaveNode (node)) }

fn moved (
  pid : &str,
  old : &str,
  new : &str,
) -> SkgrepoMove {
  SkgrepoMove { pid : ID::from (pid),
                old_skgrepo : SkgrepoName::from (old),
                new_skgrepo : SkgrepoName::from (new) } }

/// (recorder, member, raised automatically) of each stranded
/// containment.
fn strandeds_of (
  graph        : Vec<Graphnode>,
  instructions : Vec<NodeInstruction>,
  moves        : Vec<SkgrepoMove>,
) -> Vec<(String, String, bool)> {
  stranded_relationships (
    &moves, &instructions, &InRustGraph::from_graphnodes (&graph), &config () )
    . into_iter ()
    . map ( |stranded| (
      stranded . relationship . recorder . 0,
      stranded . relationship . member . 0,
      stranded . raised_automatically ))
    . collect () }

fn stranded (
  recorder : &str,
  member   : &str,
  raised   : bool,
) -> (String, String, bool) {
  ( recorder . to_string (), member . to_string (), raised ) }

#[test]
fn a_recursive_publicizing_move_strands_every_moved_containment () {
  assert_eq! (
    strandeds_of (
      vec! [ node ("r", "private", &[("a", "private")]),
             node ("a", "private", &[("b", "private")]),
             node ("b", "private", &[]) ],
      vec! [ save ( node ("r", "public", &[("a", "private")]) ),
             save ( node ("a", "public", &[("b", "private")]) ),
             save ( node ("b", "public", &[]) ) ],
      vec! [ moved ("r", "private", "public"),
             moved ("a", "private", "public"),
             moved ("b", "private", "public") ] ),
    vec! [ stranded ("a", "b", false), stranded ("r", "a", false) ] ); }

#[test]
fn a_privatizing_move_strands_no_outbound_relationship () {
  // The sticky rule already raised r's relationship to its new home.
  assert_eq! (
    strandeds_of (
      vec! [ node ("r", "public", &[("a", "public")]),
             node ("a", "public", &[]) ],
      vec! [ save ( node ("r", "trusted", &[("a", "trusted")]) ),
             save ( node ("a", "trusted", &[]) ) ],
      vec! [ moved ("r", "public", "trusted"),
             moved ("a", "public", "trusted") ] ),
    vec! [] ); }

#[test]
fn a_single_publicizing_move_strands_only_the_defaults_that_rise () {
  // r's relationship to the public a was private, and now defaults to
  // public; to the trusted c it stays trusted.
  assert_eq! (
    strandeds_of (
      vec! [ node ("r", "private", &[("a", "private"), ("c", "trusted")]),
             node ("a", "public", &[]),
             node ("c", "trusted", &[]) ],
      vec! [ save ( node ("r", "public",
                          &[("a", "private"), ("c", "trusted")]) ) ],
      vec! [ moved ("r", "private", "public") ] ),
    vec! [ stranded ("r", "a", false) ] ); }

#[test]
fn publicizing_a_child_strands_its_parents_relationship () {
  assert_eq! (
    strandeds_of (
      vec! [ node ("parent", "public", &[("child", "private")]),
             node ("child", "private", &[]) ],
      vec! [ save ( node ("child", "public", &[]) ) ],
      vec! [ moved ("child", "private", "public") ] ),
    vec! [ stranded ("parent", "child", false) ] ); }

#[test]
fn privatizing_a_child_raises_its_parents_relationship () {
  assert_eq! (
    strandeds_of (
      vec! [ node ("parent", "public", &[("child", "public")]),
             node ("child", "public", &[]) ],
      vec! [ save ( node ("child", "private", &[]) ) ],
      vec! [ moved ("child", "public", "private") ] ),
    vec! [ stranded ("parent", "child", true) ] ); }

#[test]
fn a_deliberately_private_relationship_is_not_stranded () {
  // r's relationship to a sat at trusted, more private than its
  // default: a choice the move leaves alone.
  assert_eq! (
    strandeds_of (
      vec! [ node ("r", "private", &[("a", "trusted")]),
             node ("a", "public", &[]) ],
      vec! [ save ( node ("r", "public", &[("a", "trusted")]) ) ],
      vec! [ moved ("r", "private", "public") ] ),
    vec! [] ); }

#[test]
fn resetting_to_default_rewrites_a_recorder_outside_the_save () {
  let graph : InRustGraph = InRustGraph::from_graphnodes (&[
    node ("parent", "public", &[("child", "private")]),
    node ("child", "private", &[]) ]);
  let mut instructions : Vec<NodeInstruction> =
    vec! [ save ( node ("child", "public", &[]) ) ];
  let strandeds : Vec<StrandedRelationship> = stranded_relationships (
    &[ moved ("child", "private", "public") ], &instructions, &graph, &config () );
  reset_to_default_relationships (
    &mut instructions, &strandeds . iter () . collect::<Vec<_>> (), &graph );
  let NodeInstruction::Save (SaveNode (parent)) = &instructions [1]
    else { panic! ("expected a save of parent"); };
  assert_eq! ( parent . pid, ID::from ("parent") );
  assert_eq! ( parent . contains [0] . relRepo, SkgrepoName::from ("public") ); }
