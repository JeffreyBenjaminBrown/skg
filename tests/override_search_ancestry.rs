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

use ego_tree::{NodeId, NodeRef, Tree};
use typedb_driver::TypeDBDriver;

use skg::dbs::in_rust_graph::{InRustGraph, snapshot_global};
use skg::dbs::typedb::search::all_graphnodestats::{
  AllGraphNodeStats, fetch_all_graphnodestats_with_source_set};
use skg::org_to_text::viewforest_to_string;
use skg::serve::handlers::text_search::{
  MatchGroups, build_search_viewforest, suppressed_result_ids};
use skg::serve::handlers::text_search::render_enriched_search_buffer::{
  collect_override_relative_ids,
  insert_override_ancestries_into_search_view};
use skg::source_sets::{
  ActiveSourceSet, SourceSetName, apply_source_set_to_viewforest};
use skg::test_utils::run_with_shared_test_db;
use skg::to_org::util::mark_view_roots_parent_absent;
use skg::types::misc::{ID, SkgConfig, SourceName};
use skg::types::tree::forest::ViewForest;
use skg::types::viewnode::{Birth, ViewNode, ViewNodeKind, Vognode};
use skg::update_buffer::graphnodestats::set_metadata_relationships_in_node_recursive;
use skg::update_buffer::set_viewnodestats_in_viewforest;

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
      end_to_end_render_shows_suppressed_grafts_with_heralds (
        &s . config, &s . driver, &active ) . await ?;
      Ok (( )) } )) }

/// End-to-end: replay production's phase-1 + phase-2 enrichment
/// (suppression -> pre-fetch graphStats for results + override
/// relatives -> graft -> graphStats -> heralds -> render) and check the
/// rendered buffer. The suppressed F and G are absent from the top
/// level and reappear nested under U (U -> F -> G), each carrying its
/// override birth herald -- overrides inbound from the gen-1 ancestor
/// (the parent overrides it), birth = overrides -- which only renders
/// because the pre-fetch now includes the override-relative ids
/// ('collect_override_relative_ids').
async fn end_to_end_render_shows_suppressed_grafts_with_heralds (
  config : &SkgConfig,
  driver : &TypeDBDriver,
  active : &ActiveSourceSet,
) -> Result<(), Box<dyn Error>> {
  let graph : Arc<InRustGraph> =
    snapshot_global () . expect ("graph handle installed above");
  let matches : MatchGroups = [
    hit ("U", "main",    "cooking the owned way"),
    hit ("F", "foreign", "cooking, forked once"),
    hit ("G", "foreign", "cooking, the original"),
  ] . into_iter () . collect ();
  // Phase 1: suppression, then build the top-level viewforest.
  let suppressed : HashSet<ID> =
    suppressed_result_ids ( &matches, &graph, config, active );
  let (mut viewforest, search_results)
    : (ViewForest, Vec<ID>) =
    build_search_viewforest ( "cooking", &matches, &suppressed );
  assert_eq! (
    search_results, vec![ ID::from ("U") ],
    "F and G are suppressed, so only U is a top-level result" );
  // Production's pre-fetch id-set: results + ancestry (none here) +
  // the override relatives the graft will add.
  let all_ids : Vec<ID> = {
    let mut ids : HashSet<ID> =
      search_results . iter () . cloned () . collect ();
    ids . extend (
      collect_override_relative_ids ( &search_results, active ) );
    ids . into_iter () . collect () };
  let stats : AllGraphNodeStats =
    fetch_all_graphnodestats_with_source_set (
      &config . db_name, driver, &all_ids, Some (active) ) . await ?;
  // Phase 2: graft, then the same stats/herald/render passes as
  // handle_snapshot_response.
  insert_override_ancestries_into_search_view (
    &mut viewforest, &search_results, active );
  let root_id : NodeId = viewforest . root () . id ();
  set_metadata_relationships_in_node_recursive (
    &mut viewforest, root_id, &stats, config );
  mark_view_roots_parent_absent ( &mut viewforest );
  set_viewnodestats_in_viewforest (
    &mut viewforest,
    & stats . container_to_contents,
    & stats . content_to_containers,
    config, Some (active) );
  apply_source_set_to_viewforest ( &mut viewforest, active );
  let buffer : String = viewforest_to_string ( &viewforest, config ) ?;

  let level_of = |needle : &str| -> usize {
    buffer . lines ()
      . find ( |l| l . contains (needle) )
      . map ( |l| l . chars () . take_while (|c| *c == '*') . count () )
      . unwrap_or (0) };
  assert_eq! ( level_of ("cooking the owned way"), 1,
    "U is the top-level result.\n{}", buffer );
  assert_eq! ( level_of ("cooking, forked once"), 2,
    "F (suppressed) reappears one level under U.\n{}", buffer );
  assert_eq! ( level_of ("cooking, the original"), 3,
    "G (suppressed) reappears one level under F.\n{}", buffer );

  let herald_line = |needle : &str| -> String {
    buffer . lines () . find ( |l| l . contains (needle) )
      . unwrap_or ("") . to_string () };
  for title in ["cooking, forked once", "cooking, the original"] {
    let line : String = herald_line (title);
    assert! ( line . contains ("(overrides (in 1 (ancestors 1))")
              && line . contains ("(birth overrides)"),
      "the override graft must render its override birth herald -- its \
       parent overrides it (overrides inbound from the gen-1 ancestor) \
       and overrides is its reason-for-being -- proving graphStats were \
       fetched for it: {}", line ); }
  Ok (( )) }

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
