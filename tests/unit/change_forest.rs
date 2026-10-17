// Unit tests for the change-forest, over graph pairs built in memory.

use super::*;
use crate::dbs::in_rust_graph::InRustGraph;
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

fn pair (
  before : Vec<Graphnode>,
  after  : Vec<Graphnode>,
) -> GraphPair {
  GraphPair::new (
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
    relation : NodeRelation::Contains,
    recorder : ID::from (recorder),
    member   : ID::from (member),
    aspect } }

fn keys_under (
  forest : &ChangeForest,
  pid    : &str,
) -> Vec<LeafKey> {
  forest . leaves_under (&ID::from (pid)) . iter ()
    . map ( |leaf| leaf . key . clone () )
    . collect () }

#[test]
fn each_kind_of_change_is_a_leaf_under_its_owner () {
  let mut aliased : Graphnode = node ("t", "t", &[]);
  aliased . aliases = MSV::Specified ( vec! [ RelPartner::at_relRepo (
    SkgrepoName::from ("main"), "an alias" . to_string () ) ] );
  let pair : GraphPair = pair (
    vec! [ node ("r", "r", &["a", "gone"]),
           node ("a", "a", &[]),
           node ("gone", "gone", &[]),
           node ("t", "t", &[]) ],
    vec! [ node ("r", "r", &["a", "new"]),
           node ("a", "a, retitled", &[]),
           node ("new", "new", &[]),
           aliased ] );
  let forest : &ChangeForest = pair . change_forest ();
  assert_eq! ( keys_under (forest, "r"), vec! [
    contains_key ("r", "new",  RelationshipAspect::Existence),
    contains_key ("r", "gone", RelationshipAspect::Existence) ] );
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
  let pair : GraphPair = pair (
    vec! [ node ("r", "r", &["m"]), node ("m", "m", &[]) ],
    vec! [ moved, node ("m", "m", &[]) ] );
  let forest : &ChangeForest = pair . change_forest ();
  let key : LeafKey = contains_key ("r", "m", RelationshipAspect::RelRepo);
  assert_eq! ( keys_under (forest, "r"), vec! [ key . clone () ] );
  assert_eq! ( forest . leaf (&key) . unwrap () . files, vec! [
    ( SkgrepoName::from ("main"),    ID::from ("r") ),
    ( SkgrepoName::from ("private"), ID::from ("r") ) ] ); }

#[test]
fn a_reorder_has_no_leaf_but_its_file_changed () {
  let pair : GraphPair = pair (
    vec! [ node ("r", "r", &["a", "b"]),
           node ("a", "a", &[]), node ("b", "b", &[]) ],
    vec! [ node ("r", "r", &["b", "a"]),
           node ("a", "a", &[]), node ("b", "b", &[]) ] );
  let forest : &ChangeForest = pair . change_forest ();
  assert! ( forest . leaves_under (&ID::from ("r")) . is_empty () );
  assert_eq! ( pair . order_only_pids () . iter () . collect::<Vec<_>> (),
               vec! [ &ID::from ("r") ] );
  let summary : ReadinessSummary =
    forest . readiness_summary ( &ReadinessStore::default () );
  assert_eq! ( summary, ReadinessSummary {
    ready_files : 1, changed_files : 1,
    foreign_changed_files : 0, order_only_files : 1 } );
  assert! ( summary . is_complete () ); }

#[test]
fn a_mark_lapses_when_its_change_changes () {
  let before : Vec<Graphnode> = vec! [ node ("a", "old", &[]) ];
  let first : GraphPair =
    pair ( before . clone (), vec! [ node ("a", "new", &[]) ] );
  let edited : GraphPair =
    pair ( before, vec! [ node ("a", "newer", &[]) ] );
  let key : LeafKey = node_key ("a", NodeAspect::Text);
  let mut store : ReadinessStore = ReadinessStore::default ();
  store . mark ( key . clone (),
                 first . change_forest () . leaf_fingerprint (&key) . unwrap () );
  assert! ( first  . change_forest () . readiness_of_leaves (&store, [&key]) );
  assert! ( ! edited . change_forest () . readiness_of_leaves (&store, [&key]) ); }

#[test]
fn readiness_aggregates_over_the_leaves_that_exist () {
  let pair : GraphPair = pair (
    vec! [ node ("r", "r", &[]), node ("a", "a", &[]) ],
    vec! [ node ("r", "r, retitled", &["a"]), node ("a", "a", &[]) ] );
  let forest : &ChangeForest = pair . change_forest ();
  let text   : LeafKey = node_key ("r", NodeAspect::Text);
  let member : LeafKey = contains_key ("r", "a", RelationshipAspect::Existence);
  let unchanged : LeafKey = node_key ("a", NodeAspect::Text);
  let mut store : ReadinessStore = ReadinessStore::default ();
  store . mark ( text . clone (), forest . leaf_fingerprint (&text) . unwrap () );
  assert! ( forest . readiness_of_leaves (&store, [&text, &unchanged]),
            "a key that is no leaf is never asked" );
  assert! ( ! forest . readiness_of_leaves (&store, [&text, &member]) );
  let main : SkgrepoName = SkgrepoName::from ("main");
  assert! ( ! forest . readiness_of_file (&store, &main, &ID::from ("r")) );
  assert_eq! ( forest . readiness_summary (&store) . ready_files, 0 );
  store . mark ( member . clone (), forest . leaf_fingerprint (&member) . unwrap () );
  assert! ( forest . readiness_of_file (&store, &main, &ID::from ("r")) );
  assert_eq! ( forest . readiness_summary (&store), ReadinessSummary {
    ready_files : 1, changed_files : 1,
    foreign_changed_files : 0, order_only_files : 0 } ); }
