// Unit tests for the change-forest, over graph straddles built in memory.

use super::*;
use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::in_rust_graph::relation_accessors::NodeRelation;
use crate::diff_view::processed_requests::{
  LeafKinds, NamedLeafSet, RequestedProcessed, apply_processed_requests};
use crate::types::errors::HeadlineName;
use crate::types::misc::{MSV, RelPartner};
use crate::types::nodes::complete::{Graphnode, empty_graphnode};

use std::sync::Arc;

fn node (
  pid      : &str,
  title    : &str,
  contains : &[&str],
) -> Graphnode {
  Graphnode {
    pid      : ID::from (pid),
    title    : title . to_string (),
    contains : contains . iter ()
      . map ( |member| RelPartner::at_relRepo (
        SkgrepoName::from ("main"), ID::from (*member) ))
      . collect (),
    .. empty_graphnode () }}

fn straddle (
  before : Vec<Graphnode>,
  after  : Vec<Graphnode>,
) -> GraphStraddle {
  GraphStraddle::new (
    Arc::new ( InRustGraph::from_graphnodes (&before) ),
    Arc::new ( InRustGraph::from_graphnodes (&after) ),
    String::new (),
    0 ) }

fn node_key (
  pid    : &str,
  aspect : NodeAspect,
) -> LeafKey {
  LeafKey::Node { pid : ID::from (pid), aspect } }

fn contains_key (
  recorder : &str,
  member   : &str,
  aspect   : RelationshipAspect,
) -> LeafKey {
  LeafKey::Relationship {
    relationship : RelationshipKey {
      relation : NodeRelation::Contains,
      recorder : ID::from (recorder),
      member   : ID::from (member) },
    aspect } }

fn keys_under (
  forest : &ChangeLeaves,
  pid    : &str,
) -> Vec<LeafKey> {
  forest . leaves_under (&ID::from (pid)) . iter ()
    . map ( |leaf| leaf . key . clone () )
    . collect () }

#[test]
fn each_kind_of_change_is_a_leaf_under_its_recorder () {
  let mut aliased : Graphnode = node ("t", "t", &[]);
  aliased . aliases = MSV::Specified ( vec! [ RelPartner::at_relRepo (
    SkgrepoName::from ("main"), "an alias" . to_string () ) ] );
  let straddle : GraphStraddle = straddle (
    vec! [ node ("r", "r", &["a", "gone"]),
           node ("a", "a", &[]),
           node ("gone", "gone", &[]),
           node ("t", "t", &[]) ],
    vec! [ node ("r", "r", &["a", "new"]),
           node ("a", "a, retitled", &[]),
           node ("new", "new", &[]),
           aliased ] );
  let forest : &ChangeLeaves = straddle . change_leaves ();
  assert_eq! ( keys_under (forest, "r"), vec! [
    contains_key ("r", "gone", RelationshipAspect::Existence),
    contains_key ("r", "new",  RelationshipAspect::Existence) ] );
  assert_eq! ( keys_under (forest, "a"),
               vec! [ node_key ("a", NodeAspect::Text) ] );
  assert_eq! ( keys_under (forest, "new"),
               vec! [ node_key ("new", NodeAspect::Existence) ] );
  assert_eq! ( keys_under (forest, "gone"),
               vec! [ node_key ("gone", NodeAspect::Existence) ] );
  assert_eq! ( keys_under (forest, "t"), vec! [
    node_key ("t", NodeAspect::Alias ("an alias" . to_string ())) ] ); }

#[test]
fn a_relRepo_move_is_one_leaf_recorded_in_both_files () {
  let mut moved : Graphnode = node ("r", "r", &["m"]);
  moved . contains [0] . relRepo = SkgrepoName::from ("private");
  let straddle : GraphStraddle = straddle (
    vec! [ node ("r", "r", &["m"]), node ("m", "m", &[]) ],
    vec! [ moved, node ("m", "m", &[]) ] );
  let forest : &ChangeLeaves = straddle . change_leaves ();
  let key : LeafKey = contains_key ("r", "m", RelationshipAspect::RelRepo);
  assert_eq! ( keys_under (forest, "r"), vec! [ key . clone () ] );
  assert_eq! ( forest . leaf (&key) . unwrap () . files, vec! [
    ChangedFile { skgrepo : SkgrepoName::from ("main"),    pid : ID::from ("r") },
    ChangedFile { skgrepo : SkgrepoName::from ("private"), pid : ID::from ("r") } ] ); }

#[test]
fn a_reorder_has_no_leaf_but_its_file_changed () {
  let straddle : GraphStraddle = straddle (
    vec! [ node ("r", "r", &["a", "b"]),
           node ("a", "a", &[]), node ("b", "b", &[]) ],
    vec! [ node ("r", "r", &["b", "a"]),
           node ("a", "a", &[]), node ("b", "b", &[]) ] );
  let forest : &ChangeLeaves = straddle . change_leaves ();
  assert! ( forest . leaves_under (&ID::from ("r")) . is_empty () );
  assert_eq! ( straddle . order_only_pids () . iter () . collect::<Vec<_>> (),
               vec! [ &ID::from ("r") ] );
  let summary : ProcessedSummary =
    forest . processed_summary ( &ProcessedStore::default () );
  assert_eq! ( summary, ProcessedSummary {
    processed_files : 1, changed_files : 1,
    foreign_changed_files : 0, order_only_files : 1 } );
  assert! ( summary . is_complete () ); }

#[test]
fn a_mark_lapses_when_its_change_changes () {
  let before : Vec<Graphnode> = vec! [ node ("a", "old", &[]) ];
  let first : GraphStraddle =
    straddle ( before . clone (), vec! [ node ("a", "new", &[]) ] );
  let edited : GraphStraddle =
    straddle ( before, vec! [ node ("a", "newer", &[]) ] );
  let key : LeafKey = node_key ("a", NodeAspect::Text);
  let mut store : ProcessedStore = ProcessedStore::default ();
  store . mark ( key . clone (),
                 first . change_leaves () . leaf_fingerprint (&key) . unwrap () );
  assert! ( first  . change_leaves () . processed_of_leaves (&store, [&key]) );
  assert! ( ! edited . change_leaves () . processed_of_leaves (&store, [&key]) ); }

#[test]
fn processed_aggregates_over_the_leaves_that_exist () {
  let straddle : GraphStraddle = straddle (
    vec! [ node ("r", "r", &[]), node ("a", "a", &[]) ],
    vec! [ node ("r", "r, retitled", &["a"]), node ("a", "a", &[]) ] );
  let forest : &ChangeLeaves = straddle . change_leaves ();
  let text   : LeafKey = node_key ("r", NodeAspect::Text);
  let member : LeafKey = contains_key ("r", "a", RelationshipAspect::Existence);
  let unchanged : LeafKey = node_key ("a", NodeAspect::Text);
  let mut store : ProcessedStore = ProcessedStore::default ();
  store . mark ( text . clone (), forest . leaf_fingerprint (&text) . unwrap () );
  assert! ( forest . processed_of_leaves (&store, [&text, &unchanged]),
            "a key that is no leaf is never asked" );
  assert! ( ! forest . processed_of_leaves (&store, [&text, &member]) );
  let file : ChangedFile = ChangedFile { skgrepo : SkgrepoName::from ("main"),
                                         pid     : ID::from ("r") };
  assert! ( ! forest . processed_of_file (&store, &file) );
  assert_eq! ( forest . processed_summary (&store) . processed_files, 0 );
  store . mark ( member . clone (), forest . leaf_fingerprint (&member) . unwrap () );
  assert! ( forest . processed_of_file (&store, &file) );
  assert_eq! ( forest . processed_summary (&store), ProcessedSummary {
    processed_files : 1, changed_files : 1,
    foreign_changed_files : 0, order_only_files : 0 } ); }

/// Before: r contains a. After: r also contains the new node n1, which
/// contains the new n2 and the existing a, and subscribes to the new
/// n5; n2 contains n1 back; a contains the new n3; n5 contains the new
/// n6; the new n4 stands alone.
fn straddle_with_new_nodes () -> GraphStraddle {
  let mut n1 : Graphnode = node ("n1", "n1", &["n2", "a"]);
  n1 . subscribesTo = MSV::Specified ( vec! [ RelPartner::at_relRepo (
    SkgrepoName::from ("main"), ID::from ("n5") ) ] );
  straddle (
    vec! [ node ("r", "r", &["a"]), node ("a", "a", &[]) ],
    vec! [ node ("r", "r", &["a", "n1"]),
           node ("a", "a", &["n3"]),
           n1,
           node ("n2", "n2", &["n1"]),
           node ("n3", "n3", &[]),
           node ("n4", "n4", &[]),
           node ("n5", "n5", &["n6"]),
           node ("n6", "n6", &[]) ] ) }

fn n1_subscribes_to_n5 () -> LeafKey {
  LeafKey::Relationship {
    relationship : RelationshipKey {
      relation : NodeRelation::SubscribesTo,
      recorder : ID::from ("n1"),
      member   : ID::from ("n5") },
    aspect : RelationshipAspect::Existence } }

#[test]
fn a_walk_through_new_nodes_follows_its_relations_only () {
  let straddle : GraphStraddle = straddle_with_new_nodes ();
  let forest : &ChangeLeaves = straddle . change_leaves ();
  let walk = |pid : &str, walk : NewNodeWalk| -> Vec<ID> {
    forest . new_nodes_reached_from (&ID::from (pid), walk) };
  let ids = |pids : &[&str]| -> Vec<ID> {
    pids . iter () . map ( |pid| ID::from (*pid) ) . collect () };
  assert_eq! ( walk ("n1", NewNodeWalk::Contentward), ids (&["n1", "n2"]),
               "through the cycle once, not into the existing a, nor n5" );
  assert_eq! ( walk ("n1", NewNodeWalk::Outward),
               ids (&["n1", "n2", "n5", "n6"]),
               "into what n1 subscribes to, and on from there" );
  assert_eq! ( walk ("r", NewNodeWalk::Contentward), ids (&["n1", "n2"]),
               "from a node that is not new, into its new members" );
  assert_eq! ( walk ("a", NewNodeWalk::Contentward), ids (&["n3"]) );
  assert_eq! ( walk ("n4", NewNodeWalk::Contentward), ids (&["n4"]) ); }

/// The marks a processed request on 'set' leaves in an empty store.
fn marked_by (
  forest : &ChangeLeaves,
  set    : NamedLeafSet,
) -> HashSet<LeafKey> {
  let mut store : ProcessedStore = ProcessedStore::default ();
  let warnings : Vec<String> = apply_processed_requests (
    &[ RequestedProcessed {
      processed : true,
      named    : vec! [ set ],
      headline : HeadlineName { title        : String::new (),
                                graphnode_id : None,
                                viewnode_id  : None }, } ],
    forest, &mut store );
  assert! ( warnings . is_empty (), "{:?}", warnings );
  store . marks . into_keys () . collect () }

const BOTH_KINDS : LeafKinds = LeafKinds { own : true, relationships : true };

#[test]
fn a_request_on_new_nodes_marks_what_its_walk_follows () {
  let straddle : GraphStraddle = straddle_with_new_nodes ();
  let forest : &ChangeLeaves = straddle . change_leaves ();
  let contentward : HashSet<LeafKey> = [
    node_key ("n1", NodeAspect::Existence),
    contains_key ("n1", "n2", RelationshipAspect::Existence),
    contains_key ("n1", "a",  RelationshipAspect::Existence),
    node_key ("n2", NodeAspect::Existence),
    contains_key ("n2", "n1", RelationshipAspect::Existence),
  ] . into_iter () . collect ();
  assert_eq! (
    marked_by ( forest, NamedLeafSet::NewNodesFrom (
      ID::from ("n1"), NewNodeWalk::Contentward, BOTH_KINDS )),
    contentward,
    "neither r's relationship to n1, nor n1's subscription, nor a or n3" );
  assert_eq! (
    marked_by ( forest, NamedLeafSet::NewNodesFrom (
      ID::from ("n1"), NewNodeWalk::Outward, BOTH_KINDS )),
    contentward . into_iter ()
      . chain ([ n1_subscribes_to_n5 (),
                 node_key ("n5", NodeAspect::Existence),
                 contains_key ("n5", "n6", RelationshipAspect::Existence),
                 node_key ("n6", NodeAspect::Existence) ])
      . collect::<HashSet<LeafKey>> () ); }

/// The letters beside a walk filter what it marks: 'node' the
/// existence of each new node it reaches, 'rel' the relationships
/// those nodes record that it follows.
#[test]
fn the_letters_beside_a_walk_filter_what_it_marks () {
  let straddle : GraphStraddle = straddle_with_new_nodes ();
  let forest : &ChangeLeaves = straddle . change_leaves ();
  assert_eq! (
    marked_by ( forest, NamedLeafSet::NewNodesFrom (
      ID::from ("n1"), NewNodeWalk::Contentward,
      LeafKinds { own : true, relationships : false } )),
    [ node_key ("n1", NodeAspect::Existence),
      node_key ("n2", NodeAspect::Existence) ] . into_iter () . collect () );
  assert_eq! (
    marked_by ( forest, NamedLeafSet::NewNodesFrom (
      ID::from ("n1"), NewNodeWalk::Contentward,
      LeafKinds { own : false, relationships : true } )),
    [ contains_key ("n1", "n2", RelationshipAspect::Existence),
      contains_key ("n1", "a",  RelationshipAspect::Existence),
      contains_key ("n2", "n1", RelationshipAspect::Existence) ]
      . into_iter () . collect () );
  assert_eq! (
    marked_by ( forest, NamedLeafSet::NewNodesFrom (
      ID::from ("n1"), NewNodeWalk::Outward,
      LeafKinds { own : false, relationships : true } )),
    [ contains_key ("n1", "n2", RelationshipAspect::Existence),
      contains_key ("n1", "a",  RelationshipAspect::Existence),
      contains_key ("n2", "n1", RelationshipAspect::Existence),
      n1_subscribes_to_n5 (),
      contains_key ("n5", "n6", RelationshipAspect::Existence) ]
      . into_iter () . collect () ); }
