// The tail: after the walk, every childless tombstone is pruned,
// roots included, so a buffer may end with no roots; a tombstone that
// holds something stays; every viewnode gets a vnid.

use super::*;
use indoc::formatdoc;

#[test]
fn a_root_whose_node_vanished_is_pruned_and_the_buffer_may_end_empty () {
  let graph : InRustGraph = graph (&[]);
  let run : Run = run ( &format! ("* {} gone\n", occ ("gone", "")), &graph );
  assert_eq! ( run . outline (), "" );
  assert_eq! ( run . text, "" );
  assert_eq! ( run . correction_kinds (), vec! [ CorrectionKind::Entombstoned ] ); }

#[test]
fn a_tombstone_root_that_holds_something_stays () {
  let graph : InRustGraph = graph (&[ node ("k") ]);
  let input : String = formatdoc! ("
    * {} gone
    ** {} k
    * (skg tombstone)
    ", occ ("gone", ""), occ ("k", "") );
  assert_eq! ( run (&input, &graph) . outline (), "\
tombstone
  node k noClaim" ); }

#[test]
fn every_viewnode_gets_a_vnid () {
  let graph : InRustGraph = graph (&[
    Graphnode { contains : ids (&["c"]), .. node ("p") }, node ("c") ]);
  let run : Run = run ( &format! ("* {} p\n", occ ("p", "(viewRequests (folder aliases))")),
                        &graph );
  assert! ( run . forest . nodes () . all ( |node| node . value () . viewnode_id . is_some () ),
            "{}", run . text );
  assert! ( run . corrections . iter () . all ( |c| c . vnid . is_some () ),
            "{:?}", run . corrections ); }

#[test]
fn a_folder_the_prune_leaves_childless_goes_too () {
  // The containers folder's one member is gone: at the folder's visit
  // it is a tombstone, so the folder is not yet empty; the prune
  // removes the tombstone and then the folder.
  let graph : InRustGraph = graph (&[ node ("p") ]);
  let input : String = formatdoc! ("
    * {} p
    ** (skg containerFolder)
    *** {} gone
    ", occ ("p", ""), occ ("gone", "writeProtected") );
  assert_eq! ( run (&input, &graph) . outline (), "node p" ); }
