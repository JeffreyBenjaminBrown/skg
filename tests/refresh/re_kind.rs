// Rung 1: re-kind. Each child that stands for a gnid becomes what the
// ID rule says, where it stands, keeping its vnid, fold state and
// children; each descriptor whose position the ancestry table forbids,
// or whose change is gone, is entombstoned.

use super::*;
use indoc::formatdoc;

fn p_lists (
  contents : &[&str],
) -> Graphnode {
  Graphnode { contains : ids (contents), .. node ("p") } }

#[test]
fn an_occurrence_its_parent_lists_and_no_node_answers_to_becomes_an_unknown () {
  let graph : InRustGraph = graph (&[ p_lists (&["c", "x"]), node ("c"), node ("k") ]);
  let input : String = formatdoc! ("
    * {} p
    ** {} c
    ** (skg (vnid zz) folded node (gnid x) (skgRepos (nodeRepo main))) x
    *** {} k
    ", occurrence_metadata ("p", ""), occurrence_metadata ("c", ""),
       occurrence_metadata ("k", "") );
  let run : Run = run_asserting_fixpoint (&input, &graph);
  assert_eq! ( run . outline (), "\
node p
  node c
  unknown x folded
    node k noClaim" );
  let x : NodeRef<Viewnode> = run . forest . roots () . next () . unwrap ()
    . children () . nth (1) . unwrap ();
  assert_eq! ( x . value () . viewnode_id, Some ( ViewnodeId ("zz" . into ()) ),
               "the unknown keeps the occurrence's vnid" ); }

#[test]
fn an_occurrence_no_node_answers_to_and_no_list_names_vanishes_with_its_subtree () {
  let graph : InRustGraph = graph (&[ p_lists (&["c"]), node ("c"), node ("k") ]);
  let input : String = formatdoc! ("
    * {} p
    ** {} c
    ** {} x
    *** {} k
    ** {} y
    ", occurrence_metadata ("p", ""), occurrence_metadata ("c", ""),
       occurrence_metadata ("x", ""), occurrence_metadata ("k", ""),
       occurrence_metadata ("y", "") );
  let run : Run = run_asserting_fixpoint (&input, &graph);
  assert_eq! ( run . outline (), "\
node p
  node c",
    "x vanishes, and k with it; so does y" ); }

#[test]
fn a_root_no_node_answers_to_vanishes_with_its_subtree () {
  let graph : InRustGraph = graph (&[ node ("c"), node ("k") ]);
  let input : String = formatdoc! ("
    * {} x
    ** {} k
    * {} c
    ", occurrence_metadata ("x", ""), occurrence_metadata ("k", ""),
       occurrence_metadata ("c", "") );
  let run : Run = run_asserting_fixpoint (&input, &graph);
  assert_eq! ( run . outline (), "node c" ); }

#[test]
fn a_restricted_node_its_parent_lists_in_a_restricted_record_vanishes () {
  // p lists h in its 'private' section, which the skgrepo restriction
  // keeps out: no record the skgrepo restriction shows names h. (Where
  // one does, h becomes a Restricted vognode: 'restricted.rs'.)
  let graph : InRustGraph = graph (&[
    Graphnode { contains : [ rel_partners_at_relRepo (
                               &SkgrepoName::from ("private"), vec! [ ID::from ("h") ] ),
                             ids (&["c"]) ] . concat (),
                .. node ("p") },
    node ("c"), node ("k"),
    Graphnode { home_skgrepo : SkgrepoName::from ("private"), .. node ("h") } ]);
  let input : String = formatdoc! ("
    * {} p
    ** (skg node (gnid h) (skgRepos (nodeRepo private))) h
    *** {} k
    ** {} c
    ", occurrence_metadata ("p", ""), occurrence_metadata ("k", ""),
       occurrence_metadata ("c", "") );
  let restriction : SkgrepoRestriction = main_only ();
  let run : Run = run_asserting_fixpoint_in ( &input, &graph, &restriction,
                           RefreshMode::ContentView, 100 );
  assert_eq! ( run . outline (), "\
node p
  node c" );
  assert! ( ! run . text . contains ("(gnid h)"),
            "nothing of the restricted node shows: {}", run . text ); }

#[test]
fn an_unknown_whose_node_appeared_becomes_an_occurrence () {
  let graph : InRustGraph = graph (&[ p_lists (&["u"]), node ("u") ]);
  let input : String = formatdoc! ("
    * {} p
    ** (skg unknown (gnid u))
    ", occurrence_metadata ("p", "") );
  let run : Run = run_asserting_fixpoint (&input, &graph);
  assert_eq! ( run . outline (), "\
node p
  node u" ); }

#[test]
fn an_occurrence_whose_gnid_became_an_extra_id_stands_for_its_node () {
  // a acquired b by a nodeMerge: b is now an extra ID of a.
  let graph : InRustGraph = graph (&[
    p_lists (&["b"]),
    Graphnode { extra_ids : vec! [ ID::from ("b") ], .. node ("a") } ]);
  let input : String = formatdoc! ("
    * {} p
    ** {} b
    ", occurrence_metadata ("p", ""), occurrence_metadata ("b", "writeProtected") );
  let mut forest : ViewForest = parse (&input, IdResolution::AsWritten);
  refresh_once (&mut forest, &graph);
  assert_eq! ( outline (&forest), "\
node p
  node a wp" ); }

/// a acquired b by a nodeMerge, and the buffer held an editable
/// occurrence of each: b's, the root, comes first in level order, so
/// it stays editable as a's, and a's below it is write-protected.
#[test]
fn of_two_editable_occurrences_a_nodeMerge_made_one_nodes_the_first_stays_editable () {
  let graph : InRustGraph = graph (&[
    Graphnode { extra_ids : vec! [ ID::from ("b") ],
                contains  : ids (&["a", "c"]),
                .. node ("a") },
    node ("c") ]);
  let input : String = formatdoc! ("
    * {} b
    ** {} a
    ", occurrence_metadata ("b", ""), occurrence_metadata ("a", "") );
  let mut forest : ViewForest = parse (&input, IdResolution::AsWritten);
  refresh_once (&mut forest, &graph);
  assert_eq! ( outline (&forest), "\
node a
  node a wp
  node c" ); }

/// a acquired b by a nodeMerge. r1 lists a, and the buffer shows none
/// of r1's content, so the refresh draws a there; the user's one
/// editable occurrence of a stands under q, written under the extra
/// ID b, and its parent q is visited after the a drawn under r1 (both
/// stand at depth 2, r1 first). The drawn a hears that the node's
/// editability is held, whichever of its IDs holds it, and is drawn
/// write-protected; the user's occurrence stays editable as a's.
const AN_OCCURRENCE_UNDER_Q_WRITTEN_UNDER_AN_EXTRA_ID_OR_THE_PID : &str = "\
node r1
  node a wp
node r2
  node q
    node a
      node c";

fn r1_and_q_list_a_which_acquired_b () -> InRustGraph {
  graph (&[
    Graphnode { extra_ids : vec! [ ID::from ("b") ],
                contains  : ids (&["c"]), .. node ("a") },
    node ("c"),
    Graphnode { contains : ids (&["a"]), .. node ("r1") },
    Graphnode { contains : ids (&["q"]), .. node ("r2") },
    Graphnode { contains : ids (&["a"]), .. node ("q") } ]) }

fn r1_and_r2_over_q_over (
  gnid : &str,
) -> String {
  formatdoc! ("
    * {} r1
    * {} r2
    ** {} q
    *** {} {}
    ", occurrence_metadata ("r1", ""), occurrence_metadata ("r2", ""),
       occurrence_metadata ("q", ""), occurrence_metadata (gnid, ""), gnid ) }

#[test]
fn an_editable_occurrence_written_under_an_extra_id_keeps_its_editability () {
  let run : Run = run_as_written_asserting_fixpoint_in (
    &r1_and_r2_over_q_over ("b"), &r1_and_q_list_a_which_acquired_b (),
    RefreshMode::ContentView );
  assert_eq! ( run . outline (),
               AN_OCCURRENCE_UNDER_Q_WRITTEN_UNDER_AN_EXTRA_ID_OR_THE_PID ); }

/// The control: the same buffer with the user's occurrence written
/// under the pid.
#[test]
fn an_editable_occurrence_written_under_the_pid_keeps_its_editability () {
  let run : Run = run_as_written_asserting_fixpoint_in (
    &r1_and_r2_over_q_over ("a"), &r1_and_q_list_a_which_acquired_b (),
    RefreshMode::ContentView );
  assert_eq! ( run . outline (),
               AN_OCCURRENCE_UNDER_Q_WRITTEN_UNDER_AN_EXTRA_ID_OR_THE_PID ); }

/// a acquired b by a nodeMerge, and p1 and p2 both contain it. The
/// buffer holds an editable occurrence under p1 written under b, and
/// under p2 a write-protected occurrence of a carrying an
/// editable-view request: the request makes its occurrence a's
/// editable one, and the occurrence written under b, though first in
/// level order, is write-protected.
#[test]
fn an_editable_view_request_takes_the_editability_from_an_occurrence_under_an_extra_id () {
  let graph : InRustGraph = graph (&[
    Graphnode { extra_ids : vec! [ ID::from ("b") ],
                contains  : ids (&["c"]), .. node ("a") },
    node ("c"),
    Graphnode { contains : ids (&["a"]), .. node ("p1") },
    Graphnode { contains : ids (&["a"]), .. node ("p2") } ]);
  let input : String = formatdoc! ("
    * {} p1
    ** {} b
    * {} p2
    ** {} a
    ", occurrence_metadata ("p1", ""), occurrence_metadata ("b", ""),
       occurrence_metadata ("p2", ""),
       occurrence_metadata ("a", "writeProtected (viewRequests editableView)") );
  let run : Run = run_as_written_asserting_fixpoint_in (
    &input, &graph, RefreshMode::ContentView );
  assert_eq! ( run . outline (), "\
node p1
  node a wp
node p2
  node a
    node c" ); }

#[test]
fn a_deleted_node_that_its_parent_listed_by_an_extra_id_leaves_a_tombstone_and_an_unknown () {
  // n had the extra ID x, by which p lists it; a save deleted n. Its
  // occurrence stands for n, which nothing names any more, and no
  // input of the refresh says that x was n's: so n's occurrence is a
  // tombstone (pruned, being childless), and x is reinstated as an
  // unknown, which keeps p's list intact.
  let graph : InRustGraph = graph (&[ p_lists (&["c", "x"]), node ("c") ]);
  let input : String = formatdoc! ("
    * {} p
    ** {} c
    ** {} n
    ", occurrence_metadata ("p", ""), occurrence_metadata ("c", ""),
       occurrence_metadata ("n", "") );
  let run : Run = run_asserting_fixpoint (&input, &graph);
  assert_eq! ( run . outline (), "\
node p
  node c
  unknown x" ); }

#[test]
fn a_folder_its_position_forbids_is_entombstoned_keeping_its_children () {
  let graph : InRustGraph = graph (&[ p_lists (&["c"]), node ("c"), node ("k") ]);
  let input : String = formatdoc! ("
    * {} p
    ** (skg tombstone)
    *** (skg containerFolder)
    **** {} k
    ", occurrence_metadata ("p", ""), occurrence_metadata ("k", "writeProtected") );
  let run : Run = run_asserting_fixpoint (&input, &graph);
  assert_eq! ( run . outline (), "\
node p
  tombstone
    tombstone
      node k wp noClaim
  node c" ); }

#[test]
fn a_change_viewnode_in_a_content_view_is_entombstoned () {
  let graph : InRustGraph = graph (&[ p_lists (&[]) ]);
  let input : String = formatdoc! ("
    * {} p
    ** (skg (changesetMarker content))
    ", occurrence_metadata ("p", "") );
  let run : Run = run_asserting_fixpoint (&input, &graph);
  assert_eq! ( run . outline (), "node p" ); }
