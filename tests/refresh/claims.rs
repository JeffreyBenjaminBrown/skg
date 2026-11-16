// Rung 4: claims. Under an editable occurrence or an editable folder,
// every child that claims and that the truth does not warrant
// vanishes, with its subtree; a warranted child the user marked
// 'noClaim' (to detach it) stays so. Under a write-protected parent no child claims,
// whatever it carried. A root claims.

use super::*;
use indoc::formatdoc;

#[test]
fn under_an_editable_occurrence_an_unwarranted_claiming_child_vanishes () {
  let graph : InRustGraph = graph (&[
    Graphnode { contains : ids (&["c", "d"]), .. node ("p") },
    node ("c"), node ("d"), node ("z"), ]);
  let input : String = formatdoc! ("
    * {} p
    ** {} c
    ** {} d
    ** {} z
    *** {} c
    ", occurrence_metadata ("p", ""), occurrence_metadata ("c", ""),
       occurrence_metadata ("d", "noClaim"), occurrence_metadata ("z", ""),
       occurrence_metadata ("c", "writeProtected") );
  let run : Run = run_asserting_fixpoint (&input, &graph);
  assert_eq! ( run . outline (), "\
node p
  node c
  node d noClaim",
    "z vanishes with what it held; d, detached by the user, stays \
     detached, and is not drawn again" ); }

#[test]
fn under_a_write_protected_occurrence_no_child_claims () {
  let graph : InRustGraph = graph (&[
    Graphnode { contains : ids (&["c"]), .. node ("p") },
    node ("c"), node ("z"), ]);
  let input : String = formatdoc! ("
    * {} p
    ** {} c
    ** {} z
    ", occurrence_metadata ("p", "writeProtected"),
       occurrence_metadata ("c", "writeProtected"),
       occurrence_metadata ("z", "writeProtected") );
  let run : Run = run_asserting_fixpoint (&input, &graph);
  assert_eq! ( run . outline (), "\
node p wp
  node c wp noClaim
  node z wp noClaim ⊥" );
  assert! ( ! run . text . contains ("noClaim"),
            "the outline shows the field, but no atom is written there:\n{}",
            run . text ); }

#[test]
fn a_root_claims () {
  let graph : InRustGraph = graph (&[ node ("p") ]);
  let input : String = format! ("* {} p\n", occurrence_metadata ("p", "noClaim"));
  assert_eq! ( run_asserting_fixpoint (&input, &graph) . outline (), "node p" ); }

#[test]
fn in_an_editable_folder_an_unwarranted_member_vanishes () {
  let graph : InRustGraph = graph (&[
    Graphnode { forks : msv_ids (&["e"]), .. node ("s") },
    node ("e"), node ("z"), ]);
  let input : String = formatdoc! ("
    * {} s
    ** (skg forksFolder)
    *** {} e
    *** {} z
    ", occurrence_metadata ("s", ""), occurrence_metadata ("e", "writeProtected"),
       occurrence_metadata ("z", "writeProtected") );
  let run : Run = run_asserting_fixpoint (&input, &graph);
  assert_eq! ( run . outline (), "\
node s
  forksFolder
    node e wp" ); }

#[test]
fn in_an_editable_folder_of_a_write_protected_node_no_member_claims () {
  let graph : InRustGraph = graph (&[
    Graphnode { forks : msv_ids (&["e"]), .. node ("s") },
    node ("e"), ]);
  let input : String = formatdoc! ("
    * {} s
    ** (skg forksFolder)
    *** {} e
    ", occurrence_metadata ("s", "writeProtected"),
       occurrence_metadata ("e", "writeProtected") );
  assert_eq! ( run_asserting_fixpoint (&input, &graph) . outline (), "\
node s wp
  forksFolder
    node e wp noClaim" ); }

/// In a forks folder a member claims exactly when the folder's
/// viewparent forks it. s forks neither e nor f, and f forks s: e,
/// claiming, as in a collateral view drawn before s stopped forking
/// it, vanishes; f, claiming, vanishes and is drawn again claiming
/// nothing, since s does not fork it, and the next save must not make
/// s fork it. g, a forker drawn claiming nothing, stays so.
#[test]
fn in_a_forks_folder_a_member_claims_only_what_its_viewparent_forks () {
  let graph : InRustGraph = graph (&[
    node ("s"), node ("e"),
    Graphnode { forks : msv_ids (&["s"]), .. node ("f") },
    Graphnode { forks : msv_ids (&["s"]), .. node ("g") }, ]);
  let input : String = formatdoc! ("
    * {} s
    ** (skg forksFolder)
    *** {} e
    *** {} f
    *** {} g
    ", occurrence_metadata ("s", ""), occurrence_metadata ("e", "writeProtected"),
       occurrence_metadata ("f", "writeProtected"),
       occurrence_metadata ("g", "writeProtected noClaim") );
  assert_eq! ( run_asserting_fixpoint (&input, &graph) . outline (), "\
node s
  forksFolder
    node g wp noClaim
    node f wp noClaim" ); }

/// A forker its viewparent hides is no member of the forks folder: f,
/// drawn there claiming nothing before s hid it (in another view, say),
/// vanishes, and stands in the folder of what s hides outside its fork
/// partners, whence removing it unhides it.
#[test]
fn in_a_forks_folder_a_forker_its_viewparent_hides_vanishes () {
  let graph : InRustGraph = graph (&[
    Graphnode { hides : msv_ids (&["f"]), .. node ("s") },
    Graphnode { forks : msv_ids (&["s"]), .. node ("f") }, ]);
  let input : String = formatdoc! ("
    * {} s
    ** (skg forksFolder)
    *** {} f
    ", occurrence_metadata ("s", ""),
       occurrence_metadata ("f", "writeProtected noClaim") );
  assert_eq! ( run_asserting_fixpoint (&input, &graph) . outline (), "\
node s
  forksFolder
    hiddenOutsideOfForkPartnersFolder
      node f wp folded" ); }
