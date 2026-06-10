use super::*;

// Conventions: lowercase = visible, uppercase = invisible.
fn vis (s : &&str) -> bool {
  s . chars () . all ( |c| c . is_lowercase () ) }

fn w (disk : &[&str], buffer : &[&str]) -> Vec<&'static str> {
  // Leak is fine in tests; keeps the assertions readable.
  let disk : Vec<&'static str> =
    disk . iter () . map ( |s| -> &'static str {
      Box::leak ( s . to_string () . into_boxed_str () ) } ) . collect ();
  let buffer : Vec<&'static str> =
    buffer . iter () . map ( |s| -> &'static str {
      Box::leak ( s . to_string () . into_boxed_str () ) } ) . collect ();
  weave ( &disk, vis, &buffer ) }

fn sdm (disk : &[&str], buffer : &[&str]) -> Vec<&'static str> {
  let disk : Vec<&'static str> =
    disk . iter () . map ( |s| -> &'static str {
      Box::leak ( s . to_string () . into_boxed_str () ) } ) . collect ();
  let buffer : Vec<&'static str> =
    buffer . iter () . map ( |s| -> &'static str {
      Box::leak ( s . to_string () . into_boxed_str () ) } ) . collect ();
  set_difference_merge ( &disk, vis, &buffer ) }

#[test]
fn weave_identity_when_all_visible () {
  assert_eq! ( w (&["a","b","c"], &["a","b","c"]),
               vec! ["a","b","c"] );
  assert_eq! ( w (&["a","b","c"], &["c","a","b"]),
               vec! ["c","a","b"] ); // pure reorder
  assert_eq! ( w (&["a","b"], &["a","b","new"]),
               vec! ["a","b","new"] ); // insert
  assert_eq! ( w (&["a","b"], &["b"]),
               vec! ["b"] ); } // visible delete lands

#[test]
fn weave_invisible_members_cling_to_their_anchor () {
  // disk: a X b Y c ; buffer reorders visible to c a b.
  assert_eq! ( w (&["a","X","b","Y","c"], &["c","a","b"]),
               vec! ["c","a","X","b","Y"] ); }

#[test]
fn weave_leading_run_stays_leading () {
  assert_eq! ( w (&["X","Y","a","b"], &["b","a"]),
               vec! ["X","Y","b","a"] ); }

#[test]
fn weave_orphaned_run_reattaches_leftward () {
  // b is deleted; its follower Y reattaches to a (b's nearest
  // preceding surviving positioned sibling).
  assert_eq! ( w (&["a","X","b","Y","c"], &["a","c"]),
               vec! ["a","X","Y","c"] );
  // Everything before the run is deleted: reattach to START.
  assert_eq! ( w (&["a","X","b"], &["b"]),
               vec! ["X","b"] ); }

#[test]
fn weave_all_visible_deleted () {
  assert_eq! ( w (&["a","X","b","Y"], &[]),
               vec! ["X","Y"] ); }

#[test]
fn weave_positioned_invisible_member_moves_with_buffer () {
  // R is inactive but PRESENT in the buffer (a retained
  // InactiveNode): the buffer's order wins for it, and it serves as
  // an anchor for the run that follows it on disk.
  assert_eq! ( w (&["a","R","X","b"], &["b","R","a"]),
               vec! ["b","R","X","a"] ); }

#[test]
fn weave_new_members_carry_no_runs () {
  assert_eq! ( w (&["a","X"], &["new","a"]),
               vec! ["new","a","X"] ); }

#[test]
fn weave_dedups_buffer_duplicates () {
  assert_eq! ( w (&["a","X"], &["a","a"]),
               vec! ["a","X"] ); }

#[test]
fn sdm_disk_order_kept_and_reorder_is_noop () {
  assert_eq! ( sdm (&["a","X","b"], &["b","a"]),
               vec! ["a","X","b"] ); }

#[test]
fn sdm_visible_delete_lands_invisible_survives () {
  assert_eq! ( sdm (&["a","X","b"], &["b"]),
               vec! ["X","b"] ); }

#[test]
fn sdm_appends_new_members_deduped () {
  assert_eq! ( sdm (&["a"], &["a","n","n"]),
               vec! ["a","n"] ); }

#[test]
fn sdm_identity_when_all_visible_and_unchanged () {
  assert_eq! ( sdm (&["a","b"], &["a","b"]),
               vec! ["a","b"] ); }
