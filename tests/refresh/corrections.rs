// What the refresh reports. A correction is a headline the refresh
// changed because the buffer contradicted the truth. Drawing the
// content of an occurrence first extended, the members of a folder
// first requested, or what stands under the roots of a stub is
// extent, which it does not report; a reinstatement is reported only
// where a list already drawn was missing a member.

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
  let config : SkgConfig = config ();
  let truth : Truth = Truth { graph : &graph, config : &config, restriction : None };
  let mut forest : ViewForest = stub_of_fresh_content_view (
    &[ ID::from ("p"), ID::from ("b") ], &graph );
  let refreshed : Refreshed = refresh_viewforest (
    &mut forest, &mut ViewnodeIdRecord::default (), &truth,
    CompletionMode::ContentView, 100, None ) . unwrap ();
  assert! ( refreshed . text . contains ("(gnid a1)")
            && refreshed . text . contains ("containerFolder"),
            "the stub drew its roots' content and containers:\n{}",
            refreshed . text );
  assert! ( refreshed . corrections . is_empty (),
            "{:?}", refreshed . corrections ); }

#[test]
fn the_members_of_a_folder_first_requested_are_not_reported () {
  let input : String = formatdoc! ("
    * {} p
    ", occ ("p", "writeProtected (viewRequests (folder containers))") );
  let run : Run = run (&input, &family ());
  assert_eq! ( run . outline (), "\
node p wp
  containerFolder
    node q wp noClaim" );
  assert! ( run . corrections . is_empty (), "{:?}", run . corrections ); }

#[test]
fn the_content_an_editable_view_request_extends_is_not_reported () {
  let input : String = formatdoc! ("
    * {} q
    ** {} p
    ", occ ("q", "writeProtected"),
       occ ("p", "writeProtected (viewRequests editableView)") );
  let run : Run = run (&input, &family ());
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
  assert! ( run . corrections . is_empty (), "{:?}", run . corrections ); }

#[test]
fn a_member_missing_from_a_list_already_drawn_is_reported () {
  let input : String = formatdoc! ("
    * {} p
    ** {} a
    *** {} a1
    ** (skg containerFolder)
    ", occ ("p", ""), occ ("a", ""), occ ("a1", "") );
  let run : Run = run (&input, &family ());
  assert_eq! ( run . outline (), "\
node p
  node a
    node a1
  containerFolder
    node q wp noClaim
  node b
    node s" );
  let reinstated : Vec<&str> =
    run . corrections . iter ()
    . filter ( |c| c . kind == CorrectionKind::Reinstated )
    . map ( |c| c . title . as_str () )
    . collect ();
  assert_eq! ( reinstated, vec! [ "b", "q" ],
               "b was missing from p's content, q from its containers \
                folder; b's own content is drawn as b is first \
                extended" ); }
