// cargo nextest run --test grouped_overrides -E 'test(override_search_ancestry::)'
//
// Override ancestry + suppression in search results
// (TODO/override-ancestry-in-search-results.org).
//
// Fixture (tests/override_search_ancestry/fixtures): owned source
// "main" holds U, which overrides foreign F, which overrides foreign
// G; foreign M1 and M2 mutually override; foreign B overrides foreign
// E. Ownership is by location -- "main" lives under owned/, so it is
// user-owned; "foreign" is not.

use std::collections::HashSet;
use std::error::Error;
use std::sync::Arc;

use ego_tree::{NodeRef, Tree};

use skg::dbs::in_rust_graph::{InRustGraph, snapshot_global};
use skg::serve::handlers::text_search::{
  MatchGroups, build_search_viewforest, suppressed_result_ids};
use skg::serve::handlers::text_search::render_enriched_search_buffer::insert_override_ancestries_into_search_view;
use skg::source_sets::{ActiveSourceSet, SourceSetName};
use skg::test_utils::run_with_shared_test_db;
use skg::types::misc::{ID, SkgConfig, SourceName};
use skg::types::viewnode::{Birth, ViewNode, ViewNodeKind, Vognode};

/// One search hit, as a MatchGroups entry.
fn hit (
  id : &str, source : &str, title : &str,
) -> (ID, (SourceName, Vec<(f32, String)>)) {
  ( ID::from (id),
    ( SourceName::from (source),
      vec![ (1.0_f32, title . to_string ()) ] ) ) }

#[test]
fn all_tests () -> Result<(), Box<dyn Error>> {
  run_with_shared_test_db (
    "skg-test-override-search-ancestry",
    |s| Box::pin ( async move {
      s . reset_from_config (
        "override_search_ancestry",
        "tests/override_search_ancestry/fixtures/skgconfig.toml"
        ) . await ?;
      s . install_graph_handle () ?;
      let active : ActiveSourceSet =
        ActiveSourceSet::named (
          &s . config, SourceSetName::from ("all") ) ?;
      suppression_anchors_at_user_owned ( &s . config, &active ) ?;
      override_relatives_graft_as_descendants ( &active ) ?;
      Ok (( )) } )) }

/// Only nodes recursively overridden by a USER-OWNED result are
/// suppressed. A foreign overrider suppresses nothing.
fn suppression_anchors_at_user_owned (
  config : &SkgConfig,
  active : &ActiveSourceSet,
) -> Result<(), Box<dyn Error>> {
  let graph : Arc<InRustGraph> =
    snapshot_global () . expect ("graph handle installed above");
  let matches : MatchGroups = [
    hit ("U",  "main",    "cooking the owned way"),
    hit ("F",  "foreign", "cooking, forked once"),
    hit ("G",  "foreign", "cooking, the original"),
    hit ("M1", "foreign", "mutual one"),
    hit ("M2", "foreign", "mutual two"),
    hit ("B",  "foreign", "boring override"),
    hit ("E",  "foreign", "exciting original"),
  ] . into_iter () . collect ();
  let suppressed : HashSet<ID> =
    suppressed_result_ids ( &matches, &graph, config, active );
  let mut got : Vec<String> =
    suppressed . iter () . map ( |id| id . 0 . clone () ) . collect ();
  got . sort ();
  assert_eq! (
    got, vec![ "F".to_string (), "G".to_string () ],
    "Only F and G (recursively overridden by owned U) should be \
     suppressed; the foreign mutual pair M1/M2 and E (overridden only \
     by the foreign B) must survive at top level." );
  Ok (( )) }

/// U's overriddenward chain grafts beneath it as read-only descendants
/// marked with the OVERRIDDEN backpath role: U -> F -> G.
fn override_relatives_graft_as_descendants (
  active : &ActiveSourceSet,
) -> Result<(), Box<dyn Error>> {
  let matches : MatchGroups =
    [ hit ("U", "main", "cooking the owned way") ]
    . into_iter () . collect ();
  let (mut viewforest, results) =
    build_search_viewforest ( "cooking", &matches, &HashSet::new () );
  insert_override_ancestries_into_search_view (
    &mut viewforest, &results, active );
  let tree : Tree<ViewNode> = viewforest . into_internal_tree ();
  let u : NodeRef<ViewNode> =
    find_result_root ( &tree, "U" ) . expect ("U is a result root");
  let f : NodeRef<ViewNode> =
    find_child ( u, "F" )
    . expect ("F (the node U overrides) should be grafted under U");
  assert! ( is_overriddenward_graft (f),
    "F should carry the OVERRIDDEN backpath role under U" );
  let g : NodeRef<ViewNode> =
    find_child ( f, "G" )
    . expect ("G (the node F overrides) should be grafted under F");
  assert! ( is_overriddenward_graft (g),
    "G should carry the OVERRIDDEN backpath role under F" );
  // Nothing overrides U, so no overriderward graft appears there.
  assert! ( find_child (u, "G") . is_none (),
    "G must hang under F, not directly under U" );
  Ok (( )) }

fn find_result_root<'a> (
  tree : &'a Tree<ViewNode>,
  id   : &str,
) -> Option<NodeRef<'a, ViewNode>> {
  tree . root () . children ()
    . find ( |c| active_id_is (*c, id) ) }

fn find_child<'a> (
  parent : NodeRef<'a, ViewNode>,
  id     : &str,
) -> Option<NodeRef<'a, ViewNode>> {
  parent . children () . find ( |c| active_id_is (*c, id) ) }

fn active_id_is (
  node : NodeRef<ViewNode>,
  id   : &str,
) -> bool {
  matches! ( &node . value () . kind,
    ViewNodeKind::Vognode (Vognode::Active (t))
      if t . id == ID::from (id) ) }

fn is_overriddenward_graft (
  node : NodeRef<ViewNode>,
) -> bool {
  matches! ( &node . value () . kind,
    ViewNodeKind::Vognode (Vognode::Active (t))
      if matches! ( &t . birth,
        Birth::Backpath (r) if r . rolename () == "overridden" ) ) }
