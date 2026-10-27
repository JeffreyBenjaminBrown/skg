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
               "the unknown keeps the occurrence's vnid" );
  assert_eq! ( run . buffer_correction_kinds (),
               vec! [ BufferCorrectionKind::Rekinded (VognodeKind::Unknown) ] ); }

#[test]
fn an_occurrence_no_node_answers_to_and_no_list_names_becomes_a_tombstone () {
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
  node c
  tombstone
    node k noClaim",
    "the tombstone keeps x's child; y, childless, is pruned" );
  assert_eq! ( run . count (BufferCorrectionKind::Entombstoned), 2 ); }

#[test]
fn a_restricted_node_its_parent_lists_in_a_restricted_record_becomes_a_tombstone () {
  // p lists h in its 'private' section, which the restriction keeps
  // out: no record the restriction shows names h. (Where one does, h
  // becomes a Restricted vognode: 'restricted.rs'.)
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
  let run : Run = run_asserting_fixpoint_in ( &input, &graph, Some (&restriction),
                           RefreshMode::ContentView, 100 );
  assert_eq! ( run . outline (), "\
node p
  tombstone
    node k noClaim
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
  node u" );
  assert_eq! ( run . buffer_correction_kinds (), vec! [ BufferCorrectionKind::Rekinded (VognodeKind::Occurrence) ] ); }

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
  refresh_once (&mut forest, &graph, &BufferCorrectionPolicy::default ());
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
  refresh_once (&mut forest, &graph, &BufferCorrectionPolicy::default ());
  assert_eq! ( outline (&forest), "\
node a
  node a wp
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
  assert_eq! ( run . outline (), "node p" );
  assert_eq! ( run . buffer_correction_kinds (), vec! [ BufferCorrectionKind::Entombstoned ] ); }
