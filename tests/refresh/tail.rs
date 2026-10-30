// The tail: after the walk, every childless tombstone is pruned,
// roots included, so a buffer may end with no roots; a tombstone that
// holds something stays; every viewnode gets a vnid. Its viewnode
// stats pass and its graphnode stats pass each read a list a bounded
// number of times, however long.

use super::*;
use crate::dbs::in_rust_graph::relation_accessors::STORED_LIST_READS;
use crate::refresh::graphnodestats::set_graphnodestats_in_viewforest;
use crate::refresh::viewnodestats::set_viewnodestats_in_viewforest;
use indoc::formatdoc;

#[test]
fn a_root_whose_node_vanished_is_pruned_and_the_buffer_may_end_empty () {
  let graph : InRustGraph = graph (&[]);
  let run : Run = run_asserting_fixpoint (
    &format! ("* {} gone\n", occurrence_metadata ("gone", "")), &graph );
  assert_eq! ( run . outline (), "" );
  assert_eq! ( run . text, "" );
  assert_eq! ( run . buffer_correction_kinds (), vec! [ BufferCorrectionKind::Entombstoned ] ); }

#[test]
fn a_tombstone_root_that_holds_something_stays () {
  let graph : InRustGraph = graph (&[ node ("k") ]);
  let input : String = formatdoc! ("
    * {} gone
    ** {} k
    * (skg tombstone)
    ", occurrence_metadata ("gone", ""), occurrence_metadata ("k", "") );
  assert_eq! ( run_asserting_fixpoint (&input, &graph) . outline (), "\
tombstone
  node k noClaim" ); }

#[test]
fn every_viewnode_gets_a_vnid () {
  let graph : InRustGraph = graph (&[
    Graphnode { contains : ids (&["c"]), .. node ("p") }, node ("c") ]);
  let run : Run = run_asserting_fixpoint (
    &format! ("* {} p\n", occurrence_metadata ("p", "(viewRequests (folder aliases))")),
                        &graph );
  assert! ( run . forest . nodes () . all ( |node| node . value () . viewnode_id . is_some () ),
            "{}", run . text );
  assert! ( run . buffer_corrections . iter () . all ( |c| c . vnid . is_some () ),
            "{:?}", run . buffer_corrections ); }

#[test]
fn a_folder_the_prune_leaves_childless_goes_too () {
  // The containers folder's one member is gone: at the folder's visit
  // it is a tombstone, so the folder is not yet empty; the prune
  // removes the tombstone and then the folder.
  let graph : InRustGraph = graph (&[ node ("p") ]);
  let input : String = formatdoc! ("
    * {} p
    ** (skg containerFolder)
    *** {} gone
    ", occurrence_metadata ("p", ""),
       occurrence_metadata ("gone", "writeProtected") );
  assert_eq! ( run_asserting_fixpoint (&input, &graph) . outline (), "node p" ); }

/// A view of p, which contains 'members' nodes, drawn under the
/// skgrepo restriction to 'main', with its graph: the stats of p's
/// members ask the relRepo of each one's relationship to p.
fn a_view_of_a_list_under_a_skgrepo_restriction (
  members : usize,
) -> (InRustGraph, ViewForest) {
  let member_pids : Vec<String> =
    ( 0 .. members ) . map ( |i| format! ("c{}", i) ) . collect ();
  let mut nodes : Vec<Graphnode> = member_pids . iter ()
    . map ( |pid| node (pid) ) . collect ();
  nodes . push ( Graphnode {
    contains : ids ( &member_pids . iter () . map ( |pid| pid . as_str () )
                     . collect::<Vec<&str>> () ),
    .. node ("p") } );
  let graph : InRustGraph = graph (&nodes);
  let forest : ViewForest = run_asserting_fixpoint_in (
    &format! ("* {} p\n", occurrence_metadata ("p", "")),
    &graph, Some (&main_only ()), RefreshMode::ContentView, 1000 ) . forest;
  assert_eq! ( forest . roots () . next () . unwrap () . children () . count (), members );
  (graph, forest) }

/// How many times the graphnode stats pass reads a recorder's stored
/// list over 'a_view_of_a_list_under_a_skgrepo_restriction'.
fn stored_list_reads_of_the_graphnode_stats_pass (
  members : usize,
) -> usize {
  let (graph, mut forest) : (InRustGraph, ViewForest) =
    a_view_of_a_list_under_a_skgrepo_restriction (members);
  STORED_LIST_READS . with ( |reads| reads . set (0) );
  set_graphnodestats_in_viewforest (
    forest . as_internal_tree_mut (), &graph, &config (), Some (&main_only ()) )
    . unwrap ();
  STORED_LIST_READS . with ( |reads| reads . get () ) }

/// How many times the viewnode stats pass reads a recorder's stored
/// list over 'a_view_of_a_list_under_a_skgrepo_restriction'.
fn stored_list_reads_of_the_viewnode_stats_pass (
  members : usize,
) -> usize {
  let config : SkgConfig = config ();
  let restriction : SkgrepoRestriction = main_only ();
  let (graph, mut forest) : (InRustGraph, ViewForest) =
    a_view_of_a_list_under_a_skgrepo_restriction (members);
  let tree : &mut Tree<Viewnode> = forest . as_internal_tree_mut ();
  let (container_to_contents, content_to_containers)
    : (HashMap<ID, HashSet<ID>>, HashMap<ID, HashSet<ID>>) =
    set_graphnodestats_in_viewforest (tree, &graph, &config, Some (&restriction))
    . unwrap ();
  STORED_LIST_READS . with ( |reads| reads . set (0) );
  set_viewnodestats_in_viewforest (
    tree, &graph, &container_to_contents, &content_to_containers,
    &config, Some (&restriction) );
  STORED_LIST_READS . with ( |reads| reads . get () ) }

#[test]
fn the_graphnode_stats_pass_reads_a_list_a_bounded_number_of_times () {
  let (few, many) : (usize, usize) =
    ( stored_list_reads_of_the_graphnode_stats_pass (10),
      stored_list_reads_of_the_graphnode_stats_pass (200) );
  assert! ( few > 0, "the pass asks of the relationships" );
  assert_eq! ( few, many,
               "a list of 200 members is read as often as one of 10, not \
                once per member ('RelRepoLookup')" ); }

#[test]
fn the_viewnode_stats_pass_reads_a_list_a_bounded_number_of_times () {
  let (few, many) : (usize, usize) =
    ( stored_list_reads_of_the_viewnode_stats_pass (10),
      stored_list_reads_of_the_viewnode_stats_pass (200) );
  assert! ( few > 0, "the pass asks of the relationships" );
  assert_eq! ( few, many,
               "a list of 200 members is read as often as one of 10, not \
                once per member ('RelRepoLookup')" ); }
