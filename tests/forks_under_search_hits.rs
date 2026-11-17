// cargo nextest run --test grouped_forks -E 'test(forks_under_search_hits::)'
//
// The forks folder under search hits, and suppression in
// search results ('docs/mirrors.org', "Partner folders in search
// results").
//
// Fixture (tests/forks_under_search_hits/fixtures): owned skgrepo
// "main" holds U, which forks foreign F, which forks foreign G;
// foreign M1 and M2 mutually fork; foreign B forks foreign E; foreign
// D forks "gone", which no file defines; owned O1 and O2 mutually
// fork; Priv, in owned "private", forks Pub, in "main". The config
// marks "foreign" with 'foreign = true'; "main" and "private" are
// owned, "main" the more public, being declared first.

use std::collections::HashSet;
use std::error::Error;

use ego_tree::{NodeRef, Tree};

use skg::dbs::in_rust_graph::InRustGraph;
use skg::serve::handlers::text_search::{
  MatchGroups, TitleOrAlias, build_search_viewforest, draw_search_buffer,
  suppressed_result_gnids};
use skg::skgrepo_sets::SkgrepoRestriction;
use skg::test_utils::{graph_handle_from_config, run_with_shared_test_stores};
use skg::types::misc::{ID, SkgConfig, SkgrepoName};
use skg::types::tree::forest::ViewForest;
use skg::types::viewnode::{Folder, GnidOnlyKind, Viewnode, ViewnodeKind, Vognode};
use skg::types::views_state::ViewnodeIdRecord;

/// One search hit, as a MatchGroups entry.
fn hit (
  gnid : &str, skgrepo : &str, title : &str,
) -> (ID, (SkgrepoName, Vec<(f32, TitleOrAlias)>)) {
  ( ID::from (gnid),
    ( SkgrepoName::from (skgrepo),
      vec![ (1.0_f32, TitleOrAlias::Title (title . to_string ())) ] ) ) }

#[test]
fn all_tests () -> Result<(), Box<dyn Error>> {
  run_with_shared_test_stores (
    "skg-test-forks-under-search-hits",
    |s| Box::pin ( async move {
      s . reset_from_config (
        "forks_under_search_hits",
        "tests/forks_under_search_hits/fixtures/skgconfig.toml"
        ) ?;
      let restriction : SkgrepoRestriction =
        SkgrepoRestriction::unrestricted (&s . config);
      let graph = graph_handle_from_config (&s . config) ? . load_full ();
      suppression_anchors_at_owned (
        &graph, &s . config, &restriction ) ?;
      an_owned_mutual_pair_both_matching_shows_one_hit (
        &graph, &s . config, &restriction ) ?;
      among_owned_hits_the_more_public_stands (
        &graph, &s . config, &restriction ) ?;
      a_hit_shows_the_nodes_it_forks_in_a_folder (
        &graph, &s . config, &restriction ) ?;
      end_to_end_render_shows_a_suppressed_node_in_a_folder_with_heralds (
        &graph, &s . config, &restriction ) ?;
      a_hit_forking_an_absent_node_shows_it_in_a_folder (
        &graph, &s . config, &restriction ) ?;
      Ok (( )) } )) }

/// End-to-end: replay production's search (suppression, the hits as a
/// stub, then the refresh of the stub, which draws the enrichment) and
/// check the rendered buffer. The suppressed F is
/// absent from the top level and reappears in U's forks folder,
/// since U forks it, carrying its heralds -- U, two levels up, forks
/// it. The suppressed G, which F forks, two forks from U, stands in
/// the same flat folder, after F, an indirect member: no 'b' flag and
/// no birth fact. No forks folder is drawn under either: every
/// suppressed hit stands in the one forks folder of the hit that
/// suppressed it.
fn end_to_end_render_shows_a_suppressed_node_in_a_folder_with_heralds (
  graph  : &InRustGraph,
  config : &SkgConfig,
  restriction : &SkgrepoRestriction,
) -> Result<(), Box<dyn Error>> {
  let matches : MatchGroups = [
    hit ("U", "main",    "cooking the owned way"),
    hit ("F", "foreign", "cooking, forked once"),
    hit ("G", "foreign", "cooking, the original"),
  ] . into_iter () . collect ();
  // Suppression, then the stub of the hits.
  let suppressed : HashSet<ID> =
    suppressed_result_gnids ( &matches, &graph, config, restriction );
  let (mut viewforest, search_results)
    : (ViewForest, Vec<ID>) =
    build_search_viewforest ( "cooking", &matches, &suppressed );
  assert_eq! (
    search_results, vec![ ID::from ("U") ],
    "F and G are suppressed, so only U is a top-level result" );
  // The refresh, which draws the enrichment.
  let buffer : String = draw_search_buffer (
    &mut viewforest, &mut ViewnodeIdRecord::default (), graph, config,
    &search_results, restriction, false ) ?;

  let level_of = |needle : &str| -> usize {
    buffer . lines ()
      . find ( |l| l . contains (needle) )
      . map ( |l| l . chars () . take_while (|c| *c == '*') . count () )
      . unwrap_or (0) };
  assert_eq! ( level_of ("cooking the owned way"), 1,
    "U is the top-level result.\n{}", buffer );
  assert_eq! ( level_of ("folded forksFolder"), 2,
    "U's forks folder hangs under it, folded.\n{}", buffer );
  assert_eq! ( level_of ("cooking, forked once"), 3,
    "F (suppressed) reappears in U's forks folder.\n{}", buffer );
  assert_eq! ( level_of ("cooking, the original"), 3,
    "G (suppressed), which F forks, stands in U's forks folder too.\n{}", buffer );
  let position_of = |needle : &str| -> Option<usize> {
    buffer . lines () . position ( |l| l . contains (needle) ) };
  assert! ( position_of ("cooking, forked once") < position_of ("cooking, the original"),
    "F, which U forks, precedes G, an indirect member.\n{}", buffer );
  assert_eq! ( buffer . matches ("forksFolder") . count (), 1,
    "one forks folder, nothing nested.\n{}", buffer );
  let g_line : &str = buffer . lines ()
    . find ( |l| l . contains ("cooking, the original") ) . unwrap_or ("");
  assert! ( ! g_line . contains ("birth") && ! g_line . contains ("ancestors 2"),
    "G is born of nothing, with no fork toward U: {}", g_line );
  let line : String = buffer . lines ()
    . find ( |l| l . contains ("cooking, forked once") )
    . unwrap_or ("") . to_string ();
  assert! ( line . contains ("(forks (in 1 (ancestors 2)) (out 1))")
            && line . contains ("(birth (forks in 2))"),
    "F must render its heralds -- U, two levels up, forks it, and it \
     forks G -- proving graphStats were fetched for it: {}", line );
  Ok (( )) }

/// Among hits that are mirrors of one another, every one but the hit
/// the user's own skgrepo list (the first owned skgrepo, then
/// declaration order) ranks first is suppressed, if that one is
/// owned: owned U suppresses F and G, its mirrors. Where the hit
/// ranked first is foreign, nothing is: the foreign mutual pair M1 and
/// M2, and E with its foreign fork B, all stay.
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
    hit ("B",  "foreign", "boring fork"),
    hit ("E",  "foreign", "exciting original"),
  ] . into_iter () . collect ();
  let suppressed : HashSet<ID> =
    suppressed_result_gnids ( &matches, &graph, config, restriction );
  let mut got : Vec<String> =
    suppressed . iter () . map ( |gnid| gnid . 0 . clone () ) . collect ();
  got . sort ();
  assert_eq! (
    got, vec![ "F".to_string (), "G".to_string () ],
    "Only F and G (mirrors of owned U) should be suppressed; the \
     foreign mutual pair M1/M2 and E (forked only by the foreign B) \
     must survive at top level." );
  Ok (( )) }

/// Owned O1 and O2 fork each other, and both match: one stands,
/// the other is suppressed. They tie (one skgrepo), and a tie among
/// hits goes to the least pid, O1.
fn an_owned_mutual_pair_both_matching_shows_one_hit (
  graph  : &InRustGraph,
  config : &SkgConfig,
  restriction : &SkgrepoRestriction,
) -> Result<(), Box<dyn Error>> {
  let matches : MatchGroups = [
    hit ("O1", "main", "owned mutual one"),
    hit ("O2", "main", "owned mutual two"),
  ] . into_iter () . collect ();
  let suppressed : HashSet<ID> =
    suppressed_result_gnids ( &matches, &graph, config, restriction );
  assert_eq! ( suppressed, HashSet::from ([ ID::from ("O2") ]),
               "one of the owned mutual pair is shown, O1" );
  Ok (( )) }

/// Two owned hits, mirrors of one another: Priv, in "private", forks
/// Pub, in "main". The user's own list ranks "main", the first owned
/// skgrepo declared and so the more public, first, so Pub stands and
/// Priv is suppressed.
fn among_owned_hits_the_more_public_stands (
  graph  : &InRustGraph,
  config : &SkgConfig,
  restriction : &SkgrepoRestriction,
) -> Result<(), Box<dyn Error>> {
  let matches : MatchGroups = [
    hit ("Priv", "private", "the private one"),
    hit ("Pub",  "main",    "the public one"),
  ] . into_iter () . collect ();
  let suppressed : HashSet<ID> =
    suppressed_result_gnids ( &matches, &graph, config, restriction );
  assert_eq! ( suppressed, HashSet::from ([ ID::from ("Priv") ]),
               "the public Pub stands; the private Priv is suppressed" );
  Ok (( )) }

/// U forks F, which forks G, and nothing forks U.
/// So under U hangs, folded, a forks folder whose write-protected
/// members are F and then G, an indirect member. F holds nothing: no
/// forks folder is drawn under it.
fn a_hit_shows_the_nodes_it_forks_in_a_folder (
  graph       : &InRustGraph,
  config      : &SkgConfig,
  restriction : &SkgrepoRestriction,
) -> Result<(), Box<dyn Error>> {
  let matches : MatchGroups =
    [ hit ("U", "main", "cooking the owned way") ]
    . into_iter () . collect ();
  let (mut viewforest, results) =
    build_search_viewforest ( "cooking", &matches, &HashSet::new () );
  draw_search_buffer (
    &mut viewforest, &mut ViewnodeIdRecord::default (), graph, config,
    &results, restriction, false ) ?;
  let tree : Tree<Viewnode> = viewforest . into_internal_tree ();
  let u : NodeRef<Viewnode> =
    find_result_root ( &tree, "U" ) . expect ("U is a result root");
  let folders : Vec<NodeRef<Viewnode>> = u . children ()
    . filter ( |c| c . value () . kind . is_partner_folder () )
    . collect ();
  assert_eq! ( folders . len (), 1, "only the forks folder" );
  assert! ( matches! ( &folders [0] . value () . kind,
                       ViewnodeKind::Folder (Folder::Forks) )
            && folders [0] . value () . foldAndFocus . folded,
            "U's one folder is its forks folder, folded" );
  let f : NodeRef<Viewnode> =
    find_child ( folders [0], "F" )
    . expect ("F (the node U forks) is in the folder");
  assert! ( matches! ( &f . value () . kind,
                       ViewnodeKind::Vognode (Vognode::Occurrence (t))
                       if t . is_writeProtected () ),
            "F is a write-protected member" );
  assert! ( f . children () . next () . is_none (), "F holds nothing" );
  assert! ( find_child ( folders [0], "G" ) . is_some (),
            "G (which F forks) is in the folder too" );
  a_hit_shows_the_nodes_that_fork_it_in_a_folder (
    graph, config, restriction ) }

/// B forks E, and E forks nothing. So under E hangs,
/// folded, a forks folder whose one member is B.
fn a_hit_shows_the_nodes_that_fork_it_in_a_folder (
  graph       : &InRustGraph,
  config      : &SkgConfig,
  restriction : &SkgrepoRestriction,
) -> Result<(), Box<dyn Error>> {
  let matches : MatchGroups =
    [ hit ("E", "foreign", "exciting original") ]
    . into_iter () . collect ();
  let (mut viewforest, results) =
    build_search_viewforest ( "exciting", &matches, &HashSet::new () );
  draw_search_buffer (
    &mut viewforest, &mut ViewnodeIdRecord::default (), graph, config,
    &results, restriction, false ) ?;
  let tree : Tree<Viewnode> = viewforest . into_internal_tree ();
  let e : NodeRef<Viewnode> =
    find_result_root ( &tree, "E" ) . expect ("E is a result root");
  let folders : Vec<NodeRef<Viewnode>> = e . children ()
    . filter ( |c| c . value () . kind . is_partner_folder () )
    . collect ();
  assert_eq! ( folders . len (), 1, "only the forks folder" );
  assert! ( matches! ( &folders [0] . value () . kind,
                       ViewnodeKind::Folder (Folder::Forks) )
            && folders [0] . value () . foldAndFocus . folded,
            "E's one folder is its forks folder, folded" );
  assert! ( find_child ( folders [0], "B" ) . is_some (),
            "B (which forks E) is in the folder" );
  Ok (( )) }

/// D forks "gone", which is on disk, in D's file, but not in
/// the graph. The stub asks the forks folder's own goal, the one
/// the refresh fills it from, whether the folder has members, so D's
/// forks folder is drawn, holding the unknown that stands for
/// "gone".
fn a_hit_forking_an_absent_node_shows_it_in_a_folder (
  graph       : &InRustGraph,
  config      : &SkgConfig,
  restriction : &SkgrepoRestriction,
) -> Result<(), Box<dyn Error>> {
  let matches : MatchGroups =
    [ hit ("D", "foreign", "dangling fork") ]
    . into_iter () . collect ();
  let (mut viewforest, results) =
    build_search_viewforest ( "dangling", &matches, &HashSet::new () );
  draw_search_buffer (
    &mut viewforest, &mut ViewnodeIdRecord::default (), graph, config,
    &results, restriction, false ) ?;
  let tree : Tree<Viewnode> = viewforest . into_internal_tree ();
  let d : NodeRef<Viewnode> =
    find_result_root ( &tree, "D" ) . expect ("D is a result root");
  let folder : NodeRef<Viewnode> = d . children ()
    . find ( |c| matches! ( &c . value () . kind,
                            ViewnodeKind::Folder (Folder::Forks) ))
    . expect ("D's forks folder is drawn");
  let members : Vec<NodeRef<Viewnode>> = folder . children () . collect ();
  assert_eq! ( members . len (), 1, "one member, for \"gone\"" );
  assert! ( matches! ( &members [0] . value () . kind,
                       ViewnodeKind::Vognode (Vognode::GnidOnly (unknown))
                       if unknown . kind == GnidOnlyKind::Unknown
                          && unknown . gnid == ID::from ("gone") ),
            "the member is the unknown \"gone\": {:?}",
            members [0] . value () . kind );
  Ok (( )) }

fn find_result_root<'a> (
  tree : &'a Tree<Viewnode>,
  gnid   : &str,
) -> Option<NodeRef<'a, Viewnode>> {
  tree . root () . children ()
    . find ( |c| occurrence_gnid_is (*c, gnid) ) }

fn find_child<'a> (
  parent : NodeRef<'a, Viewnode>,
  gnid     : &str,
) -> Option<NodeRef<'a, Viewnode>> {
  parent . children () . find ( |c| occurrence_gnid_is (*c, gnid) ) }

fn occurrence_gnid_is (
  node : NodeRef<Viewnode>,
  gnid   : &str,
) -> bool {
  matches! ( &node . value () . kind,
    ViewnodeKind::Vognode (Vognode::Occurrence (t))
      if t . gnid == ID::from (gnid) ) }
