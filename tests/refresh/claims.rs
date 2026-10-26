// Rung 4: claims. Under an editable occurrence or an editable folder,
// every child that claims and that the truth does not warrant is
// marked 'noClaim'; a warranted child the user marked 'noClaim' (to
// detach it) stays so. Under a write-protected parent no child claims,
// whatever it carried. A root claims.

use super::*;
use indoc::formatdoc;

#[test]
fn under_an_editable_occurrence_an_unwarranted_claimant_is_marked_noClaim () {
  let graph : InRustGraph = graph (&[
    Graphnode { contains : ids (&["c", "d"]), .. node ("p") },
    node ("c"), node ("d"), node ("z"), ]);
  let input : String = formatdoc! ("
    * {} p
    ** {} c
    ** {} d
    ** {} z
    ", occ ("p", ""), occ ("c", ""), occ ("d", "noClaim"), occ ("z", "") );
  let run : Run = run (&input, &graph);
  assert_eq! ( run . outline (), "\
node p
  node c
  node d noClaim
  node z noClaim ⊥",
    "z is marked; d, detached by the user, stays detached, and is not \
     drawn again" );
  assert_eq! ( run . buffer_correction_kinds (), vec! [ BufferCorrectionKind::MarkedNoClaim ] ); }

#[test]
fn under_a_write_protected_occurrence_no_child_claims () {
  let graph : InRustGraph = graph (&[
    Graphnode { contains : ids (&["c"]), .. node ("p") },
    node ("c"), node ("z"), ]);
  let input : String = formatdoc! ("
    * {} p
    ** {} c
    ** {} z
    ", occ ("p", "writeProtected"), occ ("c", "writeProtected"),
       occ ("z", "writeProtected") );
  let run : Run = run (&input, &graph);
  assert_eq! ( run . outline (), "\
node p wp
  node c wp noClaim
  node z wp noClaim ⊥" );
  assert! ( run . buffer_corrections . is_empty (),
            "no claim is read there, so none is corrected: {:?}", run . buffer_corrections ); }

#[test]
fn a_root_claims () {
  let graph : InRustGraph = graph (&[ node ("p") ]);
  let input : String = format! ("* {} p\n", occ ("p", "noClaim"));
  assert_eq! ( run (&input, &graph) . outline (), "node p" ); }

#[test]
fn in_an_editable_folder_an_unwarranted_member_is_marked_noClaim () {
  let graph : InRustGraph = graph (&[
    Graphnode { subscribesTo : msv_ids (&["e"]), .. node ("s") },
    node ("e"), node ("z"), ]);
  let input : String = formatdoc! ("
    * {} s
    ** (skg subscribeeFolder)
    *** {} e
    *** {} z
    ", occ ("s", ""), occ ("e", "writeProtected"), occ ("z", "writeProtected") );
  let run : Run = run (&input, &graph);
  assert_eq! ( run . outline (), "\
node s
  subscribeeFolder
    node e wp
    node z wp noClaim ⊥" ); }

#[test]
fn in_an_editable_folder_of_a_write_protected_node_no_member_claims () {
  let graph : InRustGraph = graph (&[
    Graphnode { subscribesTo : msv_ids (&["e"]), .. node ("s") },
    node ("e"), ]);
  let input : String = formatdoc! ("
    * {} s
    ** (skg subscribeeFolder)
    *** {} e
    ", occ ("s", "writeProtected"), occ ("e", "writeProtected") );
  assert_eq! ( run (&input, &graph) . outline (), "\
node s wp
  subscribeeFolder
    node e wp noClaim" ); }
