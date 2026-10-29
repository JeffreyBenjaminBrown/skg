// Rung 2: the viewnode merge. Duplicate viewnode siblings (siblings
// standing for one gnid) are merged into one viewnode, the result,
// which stands where the first stood and has a fresh vnid. It is
// folded only if every sibling was, and so is its body; it is focused,
// editable and claiming if any sibling was, but not editable when it
// shows an overrider whose editable occurrence is elsewhere; it carries
// the union of their view requests, and the edit request and the
// relationship request of whichever carried one; it holds every
// sibling's children in sibling order, each marked 'noClaim' unless the
// truth warrants it under the result.

use super::*;
use crate::types::viewnode::{NodeEditRequest, RelationshipEditRequest};
use indoc::formatdoc;

/// p contains c; c contains d; e is a node no list names.
fn family () -> InRustGraph {
  graph (&[
    Graphnode { contains : ids (&["c"]), .. node ("p") },
    Graphnode { contains : ids (&["d"]), .. node ("c") },
    node ("d"), node ("e"), ]) }

/// The children of the first root.
fn children_of_first_root (
  run : &Run,
) -> Vec<NodeRef<'_, Viewnode>> {
  run . forest . roots () . next () . unwrap () . children () . collect () }

#[test]
fn duplicate_siblings_are_merged_into_one_where_the_first_stood () {
  // c2 is a pending detachment: p contains c, and c2 claims nothing.
  // c1 claims, so the result claims.
  let input : String = formatdoc! ("
    * {} p
    ** (skg (vnid c1) folded node (gnid c) (skgRepos (nodeRepo main)) writeProtected) c
    *** {} e
    ** (skg (vnid c2) node (gnid c) (skgRepos (nodeRepo main)) noClaim) c
    *** {} d
    *** {} e
    ", occurrence_metadata ("p", ""), occurrence_metadata ("e", "writeProtected"),
       occurrence_metadata ("d", ""),
       occurrence_metadata ("e", "writeProtected") );
  let run : Run = run_asserting_fixpoint (&input, &family ());
  assert_eq! ( run . outline (), "\
node p
  node c
    node e wp noClaim ⊥
    node d",
    "the result is editable, by the second, and unfolded, since the \
     second was; at its own visit the two copies of e merge in turn, \
     and e, unwarranted, stays first, ahead of the content" );
  assert_eq! ( run . count (BufferCorrectionKind::ViewnodeMerged), 2 ); }

#[test]
fn the_result_has_a_fresh_vnid_which_the_buffer_correction_names () {
  let input : String = formatdoc! ("
    * (skg (vnid p1) node (gnid p) (skgRepos (nodeRepo main))) p
    ** (skg (vnid c1) node (gnid c) (skgRepos (nodeRepo main)) writeProtected) c
    ** (skg (vnid c2) node (gnid c) (skgRepos (nodeRepo main)) writeProtected noClaim) c
    " );
  let run : Run = run_asserting_fixpoint (&input, &family ());
  let result : NodeRef<Viewnode> = children_of_first_root (&run) [0];
  let vnid : ViewnodeId = result . value () . viewnode_id . clone ()
    . expect ("the tail gives the result a vnid");
  assert! ( vnid != ViewnodeId ("c1" . into ()) && vnid != ViewnodeId ("c2" . into ()),
            "the result's vnid is fresh, not {}", vnid );
  let merged : Vec<&BufferCorrection> = run . buffer_corrections . iter ()
    . filter ( |c| c . kind == BufferCorrectionKind::ViewnodeMerged ) . collect ();
  assert_eq! ( merged . len (), 1 );
  assert_eq! ( merged [0] . vnid, Some (vnid) ); }

#[test]
fn the_result_claims_when_the_claiming_sibling_is_second () {
  // The first is a pending detachment, the second, editable, claims:
  // the result claims, so that saving the buffer back unchanged keeps
  // c in p.
  let input : String = formatdoc! ("
    * {} p
    ** {} c
    ** {} c
    ", occurrence_metadata ("p", ""),
       occurrence_metadata ("c", "writeProtected noClaim"),
       occurrence_metadata ("c", "") );
  let run : Run = run_asserting_fixpoint (&input, &family ());
  assert_eq! ( run . outline (), "\
node p
  node c
    node d" ); }

#[test]
fn the_result_is_unfolded_when_any_sibling_was () {
  let input : String = formatdoc! ("
    * {} p
    ** (skg folded node (gnid c) (skgRepos (nodeRepo main)) writeProtected) c
    ** (skg node (gnid c) (skgRepos (nodeRepo main)) writeProtected noClaim) c
    ** (skg folded node (gnid c) (skgRepos (nodeRepo main)) writeProtected noClaim) c
    ", occurrence_metadata ("p", "") );
  let run : Run = run_asserting_fixpoint (&input, &family ());
  assert_eq! ( run . outline (), "\
node p
  node c wp" ); }

#[test]
fn the_result_is_folded_when_every_sibling_was () {
  let input : String = formatdoc! ("
    * {} p
    ** (skg folded bodyFolded node (gnid c) (skgRepos (nodeRepo main)) writeProtected) c
    ** (skg folded bodyFolded focused node (gnid c) (skgRepos (nodeRepo main)) writeProtected noClaim) c
    ** (skg folded node (gnid c) (skgRepos (nodeRepo main)) writeProtected noClaim) c
    ", occurrence_metadata ("p", "") );
  let run : Run = run_asserting_fixpoint (&input, &family ());
  assert_eq! ( run . outline (), "\
node p
  node c wp folded" );
  let result : NodeRef<Viewnode> = children_of_first_root (&run) [0];
  assert! ( ! result . value () . foldAndFocus . bodyFolded,
            "the third's body was not folded" );
  assert! ( result . value () . foldAndFocus . focused,
            "the second was focused" ); }

#[test]
fn a_view_request_on_the_second_sibling_is_carried () {
  let graph : InRustGraph = graph (&[
    Graphnode { contains : ids (&["c"]), .. node ("p") },
    Graphnode { contains : ids (&["c"]), .. node ("q") },
    node ("c"), ]);
  let input : String = formatdoc! ("
    * {} p
    ** {} c
    ** {} c
    ", occurrence_metadata ("p", ""), occurrence_metadata ("c", "writeProtected"),
       occurrence_metadata ("c", "writeProtected noClaim (viewRequests (folder containers))") );
  let run : Run = run_asserting_fixpoint (&input, &graph);
  assert_eq! ( run . outline (), "\
node p
  node c wp
    containerFolder
      node p wp noClaim
      node q wp noClaim" ); }

#[test]
fn an_appended_child_the_truth_does_not_warrant_under_the_result_is_marked_noClaim () {
  let input : String = formatdoc! ("
    * {} p
    ** {} c
    *** {} d
    ** {} c
    *** {} e
    ", occurrence_metadata ("p", ""), occurrence_metadata ("c", ""),
       occurrence_metadata ("d", ""),
       occurrence_metadata ("c", "writeProtected noClaim"),
       occurrence_metadata ("e", "") );
  let run : Run = run_asserting_fixpoint (&input, &family ());
  assert_eq! ( run . outline (), "\
node p
  node c
    node d
    node e noClaim ⊥" );
  assert_eq! ( run . buffer_correction_kinds (),
               vec! [ BufferCorrectionKind::MarkedNoClaim, BufferCorrectionKind::ViewnodeMerged ] ); }

#[test]
fn copies_of_one_folder_member_are_merged () {
  let graph : InRustGraph = graph (&[
    Graphnode { contains : ids (&["p"]), .. node ("q") },
    node ("p"), ]);
  let input : String = formatdoc! ("
    * {} p
    ** (skg containerFolder)
    *** {} q
    *** {} q
    ", occurrence_metadata ("p", ""), occurrence_metadata ("q", "writeProtected"),
       occurrence_metadata ("q", "writeProtected") );
  let run : Run = run_asserting_fixpoint (&input, &graph);
  assert_eq! ( run . outline (), "\
node p
  containerFolder
    node q wp noClaim" ); }

#[test]
fn roots_standing_for_one_gnid_are_merged () {
  let graph : InRustGraph = graph (&[ node ("p") ]);
  let input : String = formatdoc! ("
    * {} p
    * {} p
    ", occurrence_metadata ("p", ""), occurrence_metadata ("p", "writeProtected") );
  assert_eq! ( run_asserting_fixpoint (&input, &graph) . outline (), "node p" ); }

#[test]
fn the_merged_siblings_node_edit_request_rides_with_its_editability () {
  // The first c is write-protected; the second, editable, carries a
  // delete request. The result is editable, and so carries the delete.
  let input : String = formatdoc! ("
    * {} p
    ** {} c
    ** {} c
    ", occurrence_metadata ("p", ""), occurrence_metadata ("c", "writeProtected"),
       occurrence_metadata ("c", "noClaim (editRequest delete)") );
  let run : Run = run_asserting_fixpoint (&input, &family ());
  let result : NodeRef<Viewnode> = children_of_first_root (&run) [0];
  let ViewnodeKind::Vognode (Vognode::Occurrence (occurrence)) = &result . value () . kind
    else { panic! ("the result is an occurrence") };
  assert_eq! ( occurrence . editability,
               Editability::Editable { body         : None,
                                       edit_request : Some (NodeEditRequest::Delete) } ); }

#[test]
fn the_merged_siblings_relationship_request_is_carried () {
  let input : String = formatdoc! ("
    * {} p
    ** {} c
    ** {} c
    ", occurrence_metadata ("p", ""), occurrence_metadata ("c", "writeProtected"),
       occurrence_metadata ("c", "writeProtected noClaim (editRequest (relRepo private))") );
  let run : Run = run_asserting_fixpoint (&input, &family ());
  let result : NodeRef<Viewnode> = children_of_first_root (&run) [0];
  let ViewnodeKind::Vognode (Vognode::Occurrence (occurrence)) = &result . value () . kind
    else { panic! ("the result is an occurrence") };
  assert_eq! ( occurrence . relationshipEditRequest,
               Some ( RelationshipEditRequest::RelRepo ( SkgrepoName::from ("private") )) ); }

/// p contains n, which o overrides. Under p, o stands for n
/// ('overridesHere'), write-protected, and beside it n itself is
/// editable: the two stand for one gnid, so they merge into o's place.
fn an_overrider_beside_its_editable_original (
  first_roots : &str,
) -> String {
  formatdoc! ("
    {}* {} p
    ** {} o
    ** {} n
    ", first_roots, occurrence_metadata ("p", ""),
       occurrence_metadata ("o", "writeProtected (overridesHere n)"),
       occurrence_metadata ("n", "noClaim") ) }

fn overridden () -> InRustGraph {
  graph (&[
    Graphnode { contains : ids (&["n"]), .. node ("p") },
    node ("n"),
    Graphnode { overrides : msv_ids (&["n"]), .. node ("o") }, ]) }

#[test]
fn the_result_takes_the_merged_siblings_editability () {
  let run : Run = run_asserting_fixpoint (
    &an_overrider_beside_its_editable_original (""), &overridden ());
  assert_eq! ( run . outline (), "\
node p
  node o (overridesHere n)" ); }

#[test]
fn the_result_stays_write_protected_when_its_node_is_editable_elsewhere () {
  // The overrider exception: the result shows o, and o's editable
  // occurrence is the first root, so the result does not take n's
  // editability.
  let run : Run = run_asserting_fixpoint (
    &an_overrider_beside_its_editable_original (
      &format! ("* {} o\n", occurrence_metadata ("o", ""))),
    &overridden ());
  assert_eq! ( run . outline (), "\
node o
node p
  node o wp (overridesHere n)" ); }
