// Rung 2: merge. Siblings standing for one gnid are unified: the first
// in position survives, editable if any was, keeping its vnid and fold
// state; the others' children are appended to it, each marked 'noClaim'
// unless the truth warrants it under the survivor.

use super::*;
use indoc::formatdoc;

#[test]
fn siblings_standing_for_one_gnid_are_unified_under_the_first () {
  let graph : InRustGraph = graph (&[
    Graphnode { contains : ids (&["c"]), .. node ("p") },
    Graphnode { contains : ids (&["d"]), .. node ("c") },
    node ("d"), node ("e"), ]);
  // Unwarranted under p, the second copy of c is not claimed.
  let input : String = formatdoc! ("
    * {} p
    ** (skg (vnid c1) folded node (gnid c) (skgRepos (nodeRepo main)) writeProtected) c
    *** {} e
    ** (skg (vnid c2) node (gnid c) (skgRepos (nodeRepo main)) noClaim) c
    *** {} d
    *** {} e
    ", occ ("p", ""), occ ("e", "writeProtected"), occ ("d", ""),
       occ ("e", "writeProtected") );
  let run : Run = run (&input, &graph);
  assert_eq! ( run . outline (), "\
node p
  node c folded
    node e wp noClaim ⊥
    node d",
    "the survivor is the first, made editable by the second; at its \
     own visit the two copies of e merge in turn, and e, unwarranted, \
     stays first, ahead of the content" );
  let survivor : NodeRef<Viewnode> =
    run . forest . roots () . next () . unwrap () . first_child () . unwrap ();
  assert_eq! ( survivor . value () . viewnode_id, Some ( ViewnodeId ("c1" . into ()) ));
  assert_eq! ( run . count (CorrectionKind::Merged), 2 ); }

#[test]
fn an_appended_child_the_truth_does_not_warrant_under_the_survivor_is_marked_noClaim () {
  let graph : InRustGraph = graph (&[
    Graphnode { contains : ids (&["c"]), .. node ("p") },
    Graphnode { contains : ids (&["d"]), .. node ("c") },
    node ("d"), node ("e"), ]);
  let input : String = formatdoc! ("
    * {} p
    ** {} c
    *** {} d
    ** {} c
    *** {} e
    ", occ ("p", ""), occ ("c", ""), occ ("d", ""),
       occ ("c", "writeProtected noClaim"), occ ("e", "") );
  let run : Run = run (&input, &graph);
  assert_eq! ( run . outline (), "\
node p
  node c
    node d
    node e noClaim ⊥" );
  assert_eq! ( run . correction_kinds (),
               vec! [ CorrectionKind::MarkedNoClaim, CorrectionKind::Merged ] ); }

#[test]
fn copies_of_one_folder_member_are_unified () {
  let graph : InRustGraph = graph (&[
    Graphnode { contains : ids (&["p"]), .. node ("q") },
    node ("p"), ]);
  let input : String = formatdoc! ("
    * {} p
    ** (skg containerFolder)
    *** {} q
    *** {} q
    ", occ ("p", ""), occ ("q", "writeProtected"), occ ("q", "writeProtected") );
  let run : Run = run (&input, &graph);
  assert_eq! ( run . outline (), "\
node p
  containerFolder
    node q wp noClaim" ); }

#[test]
fn roots_standing_for_one_gnid_are_unified () {
  let graph : InRustGraph = graph (&[ node ("p") ]);
  let input : String = formatdoc! ("
    * {} p
    * {} p
    ", occ ("p", ""), occ ("p", "writeProtected") );
  assert_eq! ( run (&input, &graph) . outline (), "node p" ); }
