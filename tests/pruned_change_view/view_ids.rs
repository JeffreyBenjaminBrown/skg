// Who made the diff view's own viewnodes, as the view's record of view
// ids says ('ViewIdRecord' in 'server/types/views_state.rs'). Fixture
// 'removed_partners' ('buffer.rs').

use crate::pruned_change_view::common::{
  all_changes_view, open_all_changes_buffer, save, with_fixture};

use skg::metadata_canon::{vid_atom_of, without_vids};
use skg::serve::handlers::save_buffer::SaveResponse;
use skg::types::viewnode::{Owner, Vid};

use std::error::Error;

/// The owner the view's record gives the first headline of 'buffer'
/// that, view ids aside, starts with 'prefix'.
fn owner_of_first_line_starting (
  f      : &crate::git_fixture::Fixture,
  buffer : &str,
  prefix : &str,
) -> Owner {
  let line : &str = buffer . lines ()
    . find ( |line| without_vids (line) . starts_with (prefix) )
    . unwrap_or_else ( || panic! ( "no headline starts {:?}:\n{}", prefix, buffer ));
  let vid : Vid = Vid ( vid_atom_of (line) . expect ("a vid")
                        . trim_start_matches ("(vid ") . trim_end_matches (')')
                        . to_string () );
  f . views_state . open_views . view_id_record_of (&all_changes_view ())
    . owner_by_view_id . get (&vid) . copied () . unwrap_or (Owner::User) }

/// b's removed member x and the content marker under b, b's first
/// two children, are the diff view's reconciler's, when the buffer
/// opens and after a save.
#[test]
fn a_removed_member_and_a_marker_are_the_diff_views_reconcilers (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("removed_partners", &["main"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    let removed_member : &str = "** (skg (node (id x) (repo main) nonMember";
    let marker : &str = "** (skg (changesetMarker content)";
    for needle in [ removed_member, marker ] {
      assert_eq! ( owner_of_first_line_starting (f, &buffer, needle),
                   Owner::DiffViewReconciler, "{:?} when opened:\n{}", needle, buffer ); }
    let response : SaveResponse = save (f, &buffer) ?;
    assert! ( response . errors . is_empty (), "{:?}", response . errors );
    for needle in [ removed_member, marker ] {
      assert_eq! ( owner_of_first_line_starting (f, &response . saved_view, needle),
                   Owner::DiffViewReconciler, "{:?} after a save:\n{}",
                   needle, response . saved_view ); }
    Ok (( )) }) }
