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
  assert_eq! ( run_asserting_fixpoint (
    &format! ("* {} p\n", occurrence_metadata ("p", "")), &overridden () ) . outline (), "\
node p
  node o (overridesHere n)" ); }

#[test]
fn a_member_shown_raw_is_shown_as_its_overrider_in_place () {
  let input : String = formatdoc! ("
    * {} p
    ** (skg (vnid n1) folded node (gnid n) (skgRepos (nodeRepo main))) n
    ", occurrence_metadata ("p", "") );
  let run : Run = run_asserting_fixpoint (&input, &overridden ());
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
    ", occurrence_metadata ("p", ""),
       occurrence_metadata ("o", "(overridesHere n)") );
  let run : Run = run_asserting_fixpoint (&input, &graph);
  assert_eq! ( run . outline (), "\
node p
  node o noClaim ⊥
  node n" );
  assert_eq! ( run . buffer_correction_kinds (),
               vec! [ BufferCorrectionKind::MarkedNoClaim, BufferCorrectionKind::Reinstated ] ); }

/// n, a root, is overridden by o; q, a member of n's containers
/// folder, is overridden by v. Each is shown as itself.
#[test]
fn a_root_and_a_folder_member_are_shown_raw () {
  let graph : InRustGraph = graph (&[
    Graphnode { contains : ids (&["n"]), .. node ("q") },
    node ("n"),
    Graphnode { overrides : msv_ids (&["n"]), .. node ("o") },
    Graphnode { overrides : msv_ids (&["q"]), .. node ("v") }, ]);
  let input : String = formatdoc! ("
    * {} n
    ** (skg containerFolder)
    *** {} q
    ", occurrence_metadata ("n", ""), occurrence_metadata ("q", "writeProtected") );
  assert_eq! ( run_asserting_fixpoint (&input, &graph) . outline (), "\
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
  assert_eq! ( run_asserting_fixpoint (
    &format! ("* {} r\n", occurrence_metadata ("r", "")), &graph ) . outline (), "\
node r
  node n" ); }

// An overrider shown in place of an editable occurrence of the node
// it overrides, while another occurrence holds the overrider's one
// editability, is write-protected, and the other keeps it: an
// occurrence of the overrider drawn later is write-protected too.
#[test]
fn an_overrider_editable_elsewhere_is_shown_write_protected () {
  let graph : InRustGraph = graph (&[
    Graphnode { contains : ids (&["n", "q"]), .. node ("p") },
    node ("n"),
    Graphnode { contains : ids (&["o"]), .. node ("q") },
    Graphnode { overrides : msv_ids (&["n"]), .. node ("o") }, ]);
  let input : String = formatdoc! ("
    * {} p
    ** {} n
    ** {} q
    * {} o
    ", occurrence_metadata ("p", ""), occurrence_metadata ("n", ""),
       occurrence_metadata ("q", ""), occurrence_metadata ("o", "") );
  assert_eq! ( run_asserting_fixpoint (&input, &graph) . outline (), "\
node p
  node o wp (overridesHere n)
  node q
    node o wp
node o" ); }
