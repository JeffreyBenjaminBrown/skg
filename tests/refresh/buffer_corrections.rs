// What the refresh reports. A buffer correction is a headline the refresh
// changed because the buffer contradicted the truth. Drawing the
// content of an occurrence first extended, the members of a folder
// first requested, or what stands under the roots of a stub is
// extent, which it does not report; a reinstatement is reported only
// where a list already drawn was missing a member. Nor does it report
// what its caller's policy says the buffer was not wrong about: a
// tombstone the saving user's own save left, or a member drawn after
// a change of the skgrepo restriction.

use super::*;
use crate::update_buffer::stub_of_fresh_content_view;
use indoc::formatdoc;

/// p contains a and b; a contains a1; q contains p; s subscribes to
/// a.
fn family () -> InRustGraph {
  graph (&[
    Graphnode { contains : ids (&["a", "b"]), .. node ("p") },
    Graphnode { contains : ids (&["a1"]), .. node ("a") },
    Graphnode { contains : ids (&["p"]), .. node ("q") },
    Graphnode { contains : ids (&["s"]), subscribesTo : msv_ids (&["a"]),
                .. node ("b") },
    node ("a1"), node ("s"), ]) }

#[test]
fn a_fresh_view_reports_nothing () {
  let graph : InRustGraph = family ();
  let mut forest : ViewForest = stub_of_fresh_content_view (
    &[ ID::from ("p"), ID::from ("b") ], &graph );
  let refreshed : Refreshed =
    refresh_once (&mut forest, &graph, &BufferCorrectionPolicy::default ());
  assert_eq! ( outline (&forest), "\
node p
  containerFolder
    node q wp noClaim
  node a
    node a1
  node b wp
node b
  containerFolder
    node p wp noClaim
      node q wp noClaim
  subscribeeFolder
    node a wp
      node p wp noClaim
        node q wp noClaim
  node s",
    "the stub drew its roots' content and containers" );
  assert! ( refreshed . buffer_corrections . is_empty (),
            "{:?}", refreshed . buffer_corrections ); }

#[test]
fn the_members_of_a_folder_first_requested_are_not_reported () {
  let input : String = formatdoc! ("
    * {} p
    ", occurrence_metadata ("p", "writeProtected (viewRequests (folder containers))") );
  let run : Run = run_asserting_fixpoint (&input, &family ());
  assert_eq! ( run . outline (), "\
node p wp
  containerFolder
    node q wp noClaim" );
  assert! ( run . buffer_corrections . is_empty (), "{:?}", run . buffer_corrections ); }

#[test]
fn the_content_an_editable_view_request_extends_is_not_reported () {
  let input : String = formatdoc! ("
    * {} q
    ** {} p
    ", occurrence_metadata ("q", "writeProtected"),
       occurrence_metadata ("p", "writeProtected (viewRequests editableView)") );
  let run : Run = run_asserting_fixpoint (&input, &family ());
  assert_eq! ( run . outline (), "\
node q wp
  node p noClaim
    node a
      node a1
    node b
      subscribeeFolder
        node a wp
          node p wp noClaim
            node q wp noClaim
      node s" );
  assert! ( run . buffer_corrections . is_empty (), "{:?}", run . buffer_corrections ); }

#[test]
fn a_member_missing_from_a_list_already_drawn_is_reported () {
  let input : String = formatdoc! ("
    * {} p
    ** {} a
    *** {} a1
    ** (skg containerFolder)
    ", occurrence_metadata ("p", ""), occurrence_metadata ("a", ""),
       occurrence_metadata ("a1", "") );
  let run : Run = run_asserting_fixpoint (&input, &family ());
  assert_eq! ( run . outline (), "\
node p
  node a
    node a1
  containerFolder
    node q wp noClaim
  node b
    node s" );
  let reinstated : Vec<&str> =
    run . buffer_corrections . iter ()
    . filter ( |c| c . kind == BufferCorrectionKind::Reinstated )
    . map ( |c| c . title . as_str () )
    . collect ();
  assert_eq! ( reinstated, vec! [ "b", "q" ],
               "b was missing from p's content, q from its containers \
                folder; b's own content is drawn as b is first \
                extended" ); }

/// Under a policy naming x as deleted by the save, x's tombstone is no
/// buffer correction, nor is its folder, which the tombstone makes
/// unsayable; y, gone too but not by the save, is reported.
#[test]
fn a_tombstone_the_save_left_is_not_reported () {
  let graph : InRustGraph = graph (&[
    Graphnode { contains : ids (&["c"]), .. node ("p") },
    node ("c"), node ("k") ]);
  let input : String = formatdoc! ("
    * {} p
    ** {} c
    ** {} x
    *** (skg containerFolder)
    ** {} y
    *** {} k
    ", occurrence_metadata ("p", ""), occurrence_metadata ("c", ""),
       occurrence_metadata ("x", ""), occurrence_metadata ("y", ""),
       occurrence_metadata ("k", "") );
  let mut forest : ViewForest = parse (&input, IdResolution::ThroughGraph (&graph));
  let policy : BufferCorrectionPolicy =
    BufferCorrectionPolicy::after_a_save_deleting (
      HashSet::from ([ ID::from ("x") ]) );
  let refreshed : Refreshed = refresh_once (&mut forest, &graph, &policy);
  assert_eq! ( outline (&forest), "\
node p
  node c
  tombstone
    node k noClaim",
    "x's tombstone and its folder are pruned; y's keeps k" );
  let reported : Vec<(BufferCorrectionKind, &str)> =
    refreshed . buffer_corrections . iter ()
    . map ( |c| ( c . kind, c . title . as_str () ))
    . collect ();
  assert_eq! ( reported, vec! [ ( BufferCorrectionKind::Entombstoned, "y" ) ] ); }

/// After a change of the skgrepo restriction, what a list already
/// drawn lacked and the refresh draws is the restriction's doing, not
/// a buffer correction.
#[test]
fn a_member_drawn_after_a_restriction_change_is_not_reported () {
  let graph : InRustGraph = family ();
  let input : String = formatdoc! ("
    * {} p
    ** {} a
    *** {} a1
    ", occurrence_metadata ("p", ""), occurrence_metadata ("a", ""),
       occurrence_metadata ("a1", "") );
  let mut forest : ViewForest = parse (&input, IdResolution::ThroughGraph (&graph));
  let refreshed : Refreshed = refresh_once (
    &mut forest, &graph, &BufferCorrectionPolicy::after_a_switch (true) );
  assert! ( refreshed . text . contains ("(gnid b)"), "b is drawn:\n{}", refreshed . text );
  assert! ( refreshed . buffer_corrections . is_empty (), "{:?}", refreshed . buffer_corrections ); }

/// n, which o overrides, is shown as o where it stood: the truth
/// asks for the substitution, so the buffer was not wrong.
#[test]
fn a_substitution_is_not_reported () {
  let graph : InRustGraph = graph (&[
    Graphnode { contains : ids (&["n"]), .. node ("p") },
    node ("n"),
    Graphnode { overrides : msv_ids (&["n"]), .. node ("o") }, ]);
  let input : String = formatdoc! ("
    * {} p
    ** {} n
    ", occurrence_metadata ("p", ""), occurrence_metadata ("n", "writeProtected") );
  let run : Run = run_asserting_fixpoint (&input, &graph);
  assert_eq! ( run . outline (), "\
node p
  node o wp (overridesHere n)" );
  assert! ( run . buffer_corrections . is_empty (), "{:?}", run . buffer_corrections ); }

/// q, which contained p, was deleted. p's containers folder loses its
/// one member, whose entombstoning is reported; the folder, left
/// empty and going with it, is not.
#[test]
fn an_emptied_folder_is_not_reported () {
  let graph : InRustGraph = graph (&[ node ("p") ]);
  let input : String = formatdoc! ("
    * {} p
    ** (skg containerFolder)
    *** {} q
    ", occurrence_metadata ("p", ""), occurrence_metadata ("q", "writeProtected") );
  let run : Run = run_asserting_fixpoint (&input, &graph);
  assert_eq! ( run . outline (), "node p" );
  let reported : Vec<(BufferCorrectionKind, &str)> =
    run . buffer_corrections . iter ()
    . map ( |c| ( c . kind, c . title . as_str () ))
    . collect ();
  assert_eq! ( reported, vec! [ ( BufferCorrectionKind::Entombstoned, "q" ) ] ); }
