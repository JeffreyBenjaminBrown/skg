// Rung 7: substitution. In a content view an overridden node is shown
// as its overrider, with '(overridesHere ID)', except at a root, in a
// folder, and as the content of an overridden node shown raw. When
// the override is gone, the shown headline loses 'overridesHere',
// becomes unwarranted (so, under an editable parent, 'noClaim'), and
// the overridden node is reinstated beside it.

use super::*;
use indoc::formatdoc;

/// p contains n, which o overrides.
fn overridden () -> InRustGraph {
  graph (&[
    Graphnode { contains : ids (&["n"]), .. node ("p") },
    node ("n"),
    Graphnode { overrides : msv_ids (&["n"]), .. node ("o") }, ]) }

#[test]
fn a_drawn_member_is_shown_as_its_overrider () {
  assert_eq! ( run ( &format! ("* {} p\n", occ ("p", "")), &overridden () ) . outline (), "\
node p
  node o (overridesHere n)" ); }

#[test]
fn a_member_shown_raw_is_shown_as_its_overrider_in_place () {
  let input : String = formatdoc! ("
    * {} p
    ** (skg (vnid n1) folded node (gnid n) (skgRepos (nodeRepo main))) n
    ", occ ("p", "") );
  let run : Run = run (&input, &overridden ());
  assert_eq! ( run . outline (), "\
node p
  node o (overridesHere n) folded" );
  let shown : NodeRef<Viewnode> =
    run . forest . roots () . next () . unwrap () . first_child () . unwrap ();
  assert_eq! ( shown . value () . viewnode_id, Some ( ViewnodeId ("n1" . into ()) )); }

#[test]
fn when_the_override_is_gone_the_overrider_is_unwarranted_and_the_original_returns () {
  let graph : InRustGraph = graph (&[
    Graphnode { contains : ids (&["n"]), .. node ("p") },
    node ("n"), node ("o"), ]);
  let input : String = formatdoc! ("
    * {} p
    ** {} o
    ", occ ("p", ""), occ ("o", "(overridesHere n)") );
  let run : Run = run (&input, &graph);
  assert_eq! ( run . outline (), "\
node p
  node o noClaim ⊥
  node n" );
  assert_eq! ( run . correction_kinds (),
               vec! [ CorrectionKind::MarkedNoClaim, CorrectionKind::Reinstated ] ); }

#[test]
fn a_root_and_a_folder_member_are_shown_raw () {
  let graph : InRustGraph = graph (&[
    Graphnode { contains : ids (&["n"]), .. node ("q") },
    node ("n"),
    Graphnode { overrides : msv_ids (&["n"]), .. node ("o") }, ]);
  let input : String = formatdoc! ("
    * {} n
    ** (skg containerFolder)
    *** {} q
    ", occ ("n", ""), occ ("q", "writeProtected") );
  assert_eq! ( run (&input, &graph) . outline (), "\
node n
  containerFolder
    node q wp noClaim" ); }

#[test]
fn the_content_of_an_overridden_node_shown_raw_is_shown_raw () {
  let graph : InRustGraph = graph (&[
    Graphnode { contains : ids (&["n"]), .. node ("r") },
    Graphnode { overrides : msv_ids (&["r"]), .. node ("s") },
    node ("n"),
    Graphnode { overrides : msv_ids (&["n"]), .. node ("o") }, ]);
  assert_eq! ( run ( &format! ("* {} r\n", occ ("r", "")), &graph ) . outline (), "\
node r
  node n" ); }
