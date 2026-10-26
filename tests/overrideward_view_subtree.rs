// cargo nextest run --test grouped_overrides -E 'test(overrideward_view_subtree::)'
//
// The override folders under search hits, and suppression in search
// results ('docs/override.org', "Override folders in search results").
//
// Fixture (tests/overrideward_view_subtree/fixtures): owned skgrepo
// "main" holds U, which overrides foreign F, which overrides foreign
// G; foreign M1 and M2 mutually override; foreign B overrides foreign
// E. Ownership is by location -- "main" lives under owned/, so it is
// owned; "foreign" is not.

use std::collections::{HashMap, HashSet};
use std::error::Error;

use ego_tree::{NodeId, NodeRef, Tree};

use skg::dbs::in_rust_graph::InRustGraph;
use skg::dbs::in_rust_graph::stats::{
  AllGraphnodeStats, fetch_all_graphnodestats_with_skgrepo_set};
use skg::org_to_text::viewforest_to_string;
use skg::serve::handlers::text_search::{
  MatchGroups, build_search_viewforest, suppressed_result_skgids};
use skg::serve::handlers::text_search::render_enriched_search_buffer::{
  collect_override_folder_member_skgids,
  insert_partner_folders_into_search_view};
use skg::skgrepo_sets::{
  SkgrepoRestriction, SkgrepoSetName, apply_skgrepo_set_to_viewforest};
use skg::test_utils::{graph_handle_from_config, run_with_shared_test_stores};
use skg::to_org::util::mark_view_roots_as_members;
use skg::types::misc::{ID, SkgConfig, SkgrepoName, TantivyIndex};
use skg::types::tree::forest::ViewForest;
use skg::types::viewnode::{PartnerFolder, Viewnode, ViewnodeKind, Vognode};
use skg::update_buffer::graphnodestats::set_metadata_relationships_in_node_recursive;
use skg::update_buffer::set_viewnodestats_in_viewforest;

/// One search hit, as a MatchGroups entry.
fn hit (
  skgid : &str, skgrepo : &str, title : &str,
) -> (ID, (SkgrepoName, Vec<(f32, String)>)) {
  ( ID::from (skgid),
    ( SkgrepoName::from (skgrepo),
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
      let restriction : SkgrepoRestriction =
        SkgrepoRestriction::named (
          &s . config, SkgrepoSetName::from ("all") ) ?;
      let graph = graph_handle_from_config (&s . config) ? . load_full ();
      suppression_anchors_at_owned (
        &graph, &s . config, &restriction ) ?;
      a_hit_shows_the_nodes_it_overrides_in_a_folder (
        &graph, &s . config, &s . tantivy, &restriction ) ?;
      end_to_end_render_shows_a_suppressed_node_in_a_folder_with_heralds (
        &graph, &s . config, &s . tantivy, &restriction ) . await ?;
      Ok (( )) } )) }

/// End-to-end: replay production's phase-1 + phase-2 enrichment
/// (suppression -> pre-fetch graphStats for results + override
/// folder members -> folders -> graphStats -> heralds -> render) and
/// check the rendered buffer. The suppressed F is absent from the top
/// level and reappears in U's overridden folder, carrying its
/// override heralds -- U, two levels up, overrides it -- which only
/// render because the pre-fetch includes the folder members
/// ('collect_override_folder_member_skgids'). G, which F overrides,
/// is not shown: the search buffer's override folders hold direct
/// relatives only, and only its containers folder's members carry
/// anything.
async fn end_to_end_render_shows_a_suppressed_node_in_a_folder_with_heralds (
  graph  : &InRustGraph,
  config : &SkgConfig,
  tantivy : &TantivyIndex,
  restriction : &SkgrepoRestriction,
) -> Result<(), Box<dyn Error>> {
  let matches : MatchGroups = [
    hit ("U", "main",    "cooking the owned way"),
    hit ("F", "foreign", "cooking, forked once"),
    hit ("G", "foreign", "cooking, the original"),
  ] . into_iter () . collect ();
  // Phase 1: suppression, then build the top-level viewforest.
  let suppressed : HashSet<ID> =
    suppressed_result_skgids ( &matches, &graph, config, restriction );
  let (mut viewforest, search_results)
    : (ViewForest, Vec<ID>) =
    build_search_viewforest ( "cooking", &matches, &suppressed );
  assert_eq! (
    search_results, vec![ ID::from ("U") ],
    "F and G are suppressed, so only U is a top-level result" );
  // Production's pre-fetch id-set: results + ancestry (none here) +
  // the members of the override folders.
  let all_skgids : Vec<ID> = {
    let mut skgids : HashSet<ID> =
      search_results . iter () . cloned () . collect ();
    skgids . extend (
      collect_override_folder_member_skgids (
        &graph, &search_results, restriction ) );
    skgids . into_iter () . collect () };
  let stats : AllGraphnodeStats =
    fetch_all_graphnodestats_with_skgrepo_set (
      &graph, &all_skgids, Some (restriction) ) ?;
  // Phase 2: the folders, then the same stats/herald/render passes as
  // handle_snapshot_response.
  insert_partner_folders_into_search_view (
    &mut viewforest, &graph, &search_results, &HashMap::new (),
    tantivy, config, restriction );
  let root_skgid : NodeId = viewforest . root () . id ();
  set_metadata_relationships_in_node_recursive (
    &mut viewforest, root_skgid, &graph, &stats, config );
  mark_view_roots_as_members ( &mut viewforest );
  set_viewnodestats_in_viewforest (
    &mut viewforest,
    &graph,
    & stats . container_to_contents,
    & stats . content_to_containers,
    config, Some (restriction) );
  apply_skgrepo_set_to_viewforest ( &mut viewforest, restriction );
  let buffer : String = viewforest_to_string ( &viewforest, config ) ?;

  let level_of = |needle : &str| -> usize {
    buffer . lines ()
      . find ( |l| l . contains (needle) )
      . map ( |l| l . chars () . take_while (|c| *c == '*') . count () )
      . unwrap_or (0) };
  assert_eq! ( level_of ("cooking the owned way"), 1,
    "U is the top-level result.\n{}", buffer );
  assert_eq! ( level_of ("(skg folded overriddenFolder"), 2,
    "U's overridden folder hangs under it, folded.\n{}", buffer );
  assert_eq! ( level_of ("cooking, forked once"), 3,
    "F (suppressed) reappears in U's overridden folder.\n{}", buffer );
  assert_eq! ( level_of ("cooking, the original"), 0,
    "G, which F overrides, is not shown.\n{}", buffer );
  let line : String = buffer . lines ()
    . find ( |l| l . contains ("cooking, forked once") )
    . unwrap_or ("") . to_string ();
  assert! ( line . contains ("(overrides (in 1 (ancestors 2))")
            && line . contains ("(birth (overrides in 2))"),
    "F must render its override heralds -- U, two levels up, overrides \
     it -- proving graphStats were fetched for it: {}", line );
  Ok (( )) }

/// Only nodes recursively overridden by an owned result are
/// suppressed. A foreign overrider suppresses nothing.
fn suppression_anchors_at_owned (
  graph  : &InRustGraph,
  config : &SkgConfig,
  restriction : &SkgrepoRestriction,
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
    suppressed_result_skgids ( &matches, &graph, config, restriction );
  let mut got : Vec<String> =
    suppressed . iter () . map ( |skgid| skgid . 0 . clone () ) . collect ();
  got . sort ();
  assert_eq! (
    got, vec![ "F".to_string (), "G".to_string () ],
    "Only F and G (recursively overridden by owned U) should be \
     suppressed; the foreign mutual pair M1/M2 and E (overridden only \
     by the foreign B) must survive at top level." );
  Ok (( )) }

/// U overrides F, which overrides G, and nothing overrides U. So
/// under U hangs, folded, an overridden folder whose one
/// write-protected member is F, and no overrider folder. F holds
/// nothing: G is not drawn under it.
fn a_hit_shows_the_nodes_it_overrides_in_a_folder (
  graph       : &InRustGraph,
  config      : &SkgConfig,
  tantivy     : &TantivyIndex,
  restriction : &SkgrepoRestriction,
) -> Result<(), Box<dyn Error>> {
  let matches : MatchGroups =
    [ hit ("U", "main", "cooking the owned way") ]
    . into_iter () . collect ();
  let (mut viewforest, results) =
    build_search_viewforest ( "cooking", &matches, &HashSet::new () );
  insert_partner_folders_into_search_view (
    &mut viewforest, graph, &results, &HashMap::new (),
    tantivy, config, restriction );
  let tree : Tree<Viewnode> = viewforest . into_internal_tree ();
  let u : NodeRef<Viewnode> =
    find_result_root ( &tree, "U" ) . expect ("U is a result root");
  let folders : Vec<NodeRef<Viewnode>> = u . children ()
    . filter ( |c| matches! ( &c . value () . kind, ViewnodeKind::PartnerFolder (_) ))
    . collect ();
  assert_eq! ( folders . len (), 1, "only the overridden folder" );
  assert! ( matches! ( &folders [0] . value () . kind,
                       ViewnodeKind::PartnerFolder (PartnerFolder::Overridden) )
            && folders [0] . value () . foldAndFocus . folded,
            "U's one folder is its overridden folder, folded" );
  let f : NodeRef<Viewnode> =
    find_child ( folders [0], "F" )
    . expect ("F (the node U overrides) is in the folder");
  assert! ( matches! ( &f . value () . kind,
                       ViewnodeKind::Vognode (Vognode::Unrestricted (t))
                       if t . claimsMembership
                          && t . is_writeProtected () ),
            "F is a write-protected member" );
  assert! ( f . children () . next () . is_none (), "F holds nothing" );
  a_hit_shows_the_nodes_that_override_it_in_a_folder (
    graph, config, tantivy, restriction ) }

/// B overrides E, and E overrides nothing. So under E hangs, folded,
/// an overrider folder whose one member is B.
fn a_hit_shows_the_nodes_that_override_it_in_a_folder (
  graph       : &InRustGraph,
  config      : &SkgConfig,
  tantivy     : &TantivyIndex,
  restriction : &SkgrepoRestriction,
) -> Result<(), Box<dyn Error>> {
  let matches : MatchGroups =
    [ hit ("E", "foreign", "exciting original") ]
    . into_iter () . collect ();
  let (mut viewforest, results) =
    build_search_viewforest ( "exciting", &matches, &HashSet::new () );
  insert_partner_folders_into_search_view (
    &mut viewforest, graph, &results, &HashMap::new (),
    tantivy, config, restriction );
  let tree : Tree<Viewnode> = viewforest . into_internal_tree ();
  let e : NodeRef<Viewnode> =
    find_result_root ( &tree, "E" ) . expect ("E is a result root");
  let folders : Vec<NodeRef<Viewnode>> = e . children ()
    . filter ( |c| matches! ( &c . value () . kind, ViewnodeKind::PartnerFolder (_) ))
    . collect ();
  assert_eq! ( folders . len (), 1, "only the overrider folder" );
  assert! ( matches! ( &folders [0] . value () . kind,
                       ViewnodeKind::PartnerFolder (PartnerFolder::Overrider) )
            && folders [0] . value () . foldAndFocus . folded,
            "E's one folder is its overrider folder, folded" );
  assert! ( find_child ( folders [0], "B" ) . is_some (),
            "B (which overrides E) is in the folder" );
  Ok (( )) }

fn find_result_root<'a> (
  tree : &'a Tree<Viewnode>,
  skgid   : &str,
) -> Option<NodeRef<'a, Viewnode>> {
  tree . root () . children ()
    . find ( |c| unrestricted_skgid_is (*c, skgid) ) }

fn find_child<'a> (
  parent : NodeRef<'a, Viewnode>,
  skgid     : &str,
) -> Option<NodeRef<'a, Viewnode>> {
  parent . children () . find ( |c| unrestricted_skgid_is (*c, skgid) ) }

fn unrestricted_skgid_is (
  node : NodeRef<Viewnode>,
  skgid   : &str,
) -> bool {
  matches! ( &node . value () . kind,
    ViewnodeKind::Vognode (Vognode::Unrestricted (t))
      if t . skgid == ID::from (skgid) ) }
