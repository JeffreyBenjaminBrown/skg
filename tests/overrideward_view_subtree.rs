// cargo nextest run --test grouped_overrides -E 'test(overrideward_view_subtree::)'
//
// Overrideward view-subtrees + suppression in search results
// (TODO/DONE/override-ancestry-in-search-results.org).
//
// Fixture (tests/overrideward_view_subtree/fixtures): owned skgrepo
// "main" holds U, which overrides foreign F, which overrides foreign
// G; foreign M1 and M2 mutually override; foreign B overrides foreign
// E. Ownership is by location -- "main" lives under owned/, so it is
// owned; "foreign" is not.

use std::collections::HashSet;
use std::error::Error;

use ego_tree::{NodeId, NodeRef, Tree};

use skg::dbs::in_rust_graph::InRustGraph;
use skg::dbs::in_rust_graph::stats::{
  AllGraphnodeStats, fetch_all_graphnodestats_with_skgrepo_set};
use skg::org_to_text::viewforest_to_string;
use skg::serve::handlers::text_search::{
  MatchGroups, build_search_viewforest, suppressed_result_skgids};
use skg::serve::handlers::text_search::render_enriched_search_buffer::{
  collect_overrideward_view_subtree_skgids,
  insert_overrideward_view_subtrees};
use skg::skgrepo_sets::{
  ActiveSkgRepoSet, SkgRepoSetName, apply_skgrepo_set_to_viewforest};
use skg::test_utils::{graph_handle_from_config, run_with_shared_test_stores};
use skg::to_org::util::mark_view_roots_parent_na;
use skg::types::misc::{ID, SkgConfig, SkgRepoName};
use skg::types::tree::forest::ViewForest;
use skg::types::viewnode::{Birth, Viewnode, ViewnodeKind, Vognode};
use skg::update_buffer::graphnodestats::set_metadata_relationships_in_node_recursive;
use skg::update_buffer::set_viewnodestats_in_viewforest;

/// One search hit, as a MatchGroups entry.
fn hit (
  skgid : &str, skgrepo : &str, title : &str,
) -> (ID, (SkgRepoName, Vec<(f32, String)>)) {
  ( ID::from (skgid),
    ( SkgRepoName::from (skgrepo),
      vec![ (1.0_f32, title . to_string ()) ] ) ) }

#[test]
fn all_tests () -> Result<(), Box<dyn Error>> {
  run_with_shared_test_stores (
    "skg-test-overrideward-view-subtree",
    |s| Box::pin ( async move {
      s . reset_from_config (
        "overrideward_view_subtree",
        "tests/overrideward_view_subtree/fixtures/skgconfig.toml"
        ) ?;
      let active : ActiveSkgRepoSet =
        ActiveSkgRepoSet::named (
          &s . config, SkgRepoSetName::from ("all") ) ?;
      let graph = graph_handle_from_config (&s . config) ? . load_full ();
      suppression_anchors_at_owned (
        &graph, &s . config, &active ) ?;
      override_relatives_are_role_grafted_as_descendants ( &graph, &active ) ?;
      end_to_end_render_shows_suppressed_role_grafts_with_heralds (
        &graph, &s . config, &active ) . await ?;
      Ok (( )) } )) }

/// End-to-end: replay production's phase-1 + phase-2 enrichment
/// (suppression -> pre-fetch graphStats for results + override
/// relatives -> graft -> graphStats -> heralds -> render) and check the
/// rendered buffer. The suppressed F and G are absent from the top
/// level and reappear nested under U (U -> F -> G), each carrying its
/// override birth herald -- overrides inbound from the gen-1 ancestor
/// (the parent overrides it), birth = overrides -- which only renders
/// because the pre-fetch now includes the override-relative ids
/// ('collect_overrideward_view_subtree_ids').
async fn end_to_end_render_shows_suppressed_role_grafts_with_heralds (
  graph  : &InRustGraph,
  config : &SkgConfig,
  active : &ActiveSkgRepoSet,
) -> Result<(), Box<dyn Error>> {
  let matches : MatchGroups = [
    hit ("U", "main",    "cooking the owned way"),
    hit ("F", "foreign", "cooking, forked once"),
    hit ("G", "foreign", "cooking, the original"),
  ] . into_iter () . collect ();
  // Phase 1: suppression, then build the top-level viewforest.
  let suppressed : HashSet<ID> =
    suppressed_result_skgids ( &matches, &graph, config, active );
  let (mut viewforest, search_results)
    : (ViewForest, Vec<ID>) =
    build_search_viewforest ( "cooking", &matches, &suppressed );
  assert_eq! (
    search_results, vec![ ID::from ("U") ],
    "F and G are suppressed, so only U is a top-level result" );
  // Production's pre-fetch id-set: results + ancestry (none here) +
  // the override relatives the role graft will add.
  let all_skgids : Vec<ID> = {
    let mut skgids : HashSet<ID> =
      search_results . iter () . cloned () . collect ();
    skgids . extend (
      collect_overrideward_view_subtree_skgids (
        &graph, &search_results, active ) );
    skgids . into_iter () . collect () };
  let stats : AllGraphnodeStats =
    fetch_all_graphnodestats_with_skgrepo_set (
      &graph, &all_skgids, Some (active) ) ?;
  // Phase 2: graft, then the same stats/herald/render passes as
  // handle_snapshot_response.
  insert_overrideward_view_subtrees (
    &mut viewforest, &graph, &search_results, active );
  let root_skgid : NodeId = viewforest . root () . id ();
  set_metadata_relationships_in_node_recursive (
    &mut viewforest, root_skgid, &graph, &stats, config );
  mark_view_roots_parent_na ( &mut viewforest );
  set_viewnodestats_in_viewforest (
    &mut viewforest,
    &graph,
    & stats . container_to_contents,
    & stats . content_to_containers,
    config, Some (active) );
  apply_skgrepo_set_to_viewforest ( &mut viewforest, active );
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
    assert! ( line . contains ("(overrides_view_of (in 1 (ancestors 1))")
              && line . contains ("(birth (overrides_view_of in 1))"),
      "the override role graft must render its override birth herald -- its \
       parent overrides it (overrides inbound from the gen-1 ancestor) \
       and overrides is its birth -- proving graphStats were \
       fetched for it: {}", line ); }
  Ok (( )) }

/// Only nodes recursively overridden by an OWNED result are
/// suppressed. A foreign overrider suppresses nothing.
fn suppression_anchors_at_owned (
  graph  : &InRustGraph,
  config : &SkgConfig,
  active : &ActiveSkgRepoSet,
) -> Result<(), Box<dyn Error>> {
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
    suppressed_result_skgids ( &matches, &graph, config, active );
  let mut got : Vec<String> =
    suppressed . iter () . map ( |skgid| skgid . 0 . clone () ) . collect ();
  got . sort ();
  assert_eq! (
    got, vec![ "F".to_string (), "G".to_string () ],
    "Only F and G (recursively overridden by owned U) should be \
     suppressed; the foreign mutual pair M1/M2 and E (overridden only \
     by the foreign B) must survive at top level." );
  Ok (( )) }

/// U's overriddenward chain grafts beneath it as write-protected descendants
/// marked with the OVERRIDDEN role-graft birth: U -> F -> G.
fn override_relatives_are_role_grafted_as_descendants (
  graph : &InRustGraph,
  active : &ActiveSkgRepoSet,
) -> Result<(), Box<dyn Error>> {
  let matches : MatchGroups =
    [ hit ("U", "main", "cooking the owned way") ]
    . into_iter () . collect ();
  let (mut viewforest, results) =
    build_search_viewforest ( "cooking", &matches, &HashSet::new () );
  insert_overrideward_view_subtrees (
    &mut viewforest, graph, &results, active );
  let tree : Tree<Viewnode> = viewforest . into_internal_tree ();
  let u : NodeRef<Viewnode> =
    find_result_root ( &tree, "U" ) . expect ("U is a result root");
  let f : NodeRef<Viewnode> =
    find_child ( u, "F" )
    . expect ("F (the node U overrides) should be grafted under U");
  assert! ( is_overriddenward_role_graft (f),
    "F should carry the OVERRIDDEN role-graft birth under U" );
  let g : NodeRef<Viewnode> =
    find_child ( f, "G" )
    . expect ("G (the node F overrides) should be grafted under F");
  assert! ( is_overriddenward_role_graft (g),
    "G should carry the OVERRIDDEN role-graft birth under F" );
  // Nothing overrides U, so no overriderward role graft appears there.
  assert! ( find_child (u, "G") . is_none (),
    "G must hang under F, not directly under U" );
  Ok (( )) }

fn find_result_root<'a> (
  tree : &'a Tree<Viewnode>,
  skgid   : &str,
) -> Option<NodeRef<'a, Viewnode>> {
  tree . root () . children ()
    . find ( |c| active_skgid_is (*c, skgid) ) }

fn find_child<'a> (
  parent : NodeRef<'a, Viewnode>,
  skgid     : &str,
) -> Option<NodeRef<'a, Viewnode>> {
  parent . children () . find ( |c| active_skgid_is (*c, skgid) ) }

fn active_skgid_is (
  node : NodeRef<Viewnode>,
  skgid   : &str,
) -> bool {
  matches! ( &node . value () . kind,
    ViewnodeKind::Vognode (Vognode::Active (t))
      if t . skgid == ID::from (skgid) ) }

fn is_overriddenward_role_graft (
  node : NodeRef<Viewnode>,
) -> bool {
  matches! ( &node . value () . kind,
    ViewnodeKind::Vognode (Vognode::Active (t))
      if matches! ( &t . birth,
        Birth::RoleGraft (r) if r . rolename () == "overridden" ) ) }
