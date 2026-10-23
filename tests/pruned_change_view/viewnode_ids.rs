// Who made the diff view's own viewnodes, as the view's record of
// viewnode IDs says ('ViewnodeIdRecord' in 'server/types/views_state.rs'). Fixture
// 'removed_partners' ('buffer.rs').

use crate::pruned_change_view::common::{
  all_changes_view, open_all_changes_buffer, save, with_fixture};

use skg::metadata_canon::{viewnode_id_atom_of, without_viewnode_ids};
use skg::serve::handlers::save_buffer::SaveResponse;
use skg::types::viewnode::{Owner, ViewnodeId};

use std::error::Error;

/// The owner the view's record gives the first headline of 'buffer'
/// that, viewnode IDs aside, starts with 'prefix'.
fn owner_of_first_line_starting (
  f      : &crate::git_fixture::Fixture,
  buffer : &str,
  prefix : &str,
) -> Owner {
  let line : &str = buffer . lines ()
    . find ( |line| without_viewnode_ids (line) . starts_with (prefix) )
    . unwrap_or_else ( || panic! ( "no headline starts {:?}:\n{}", prefix, buffer ));
  let viewnode_id : ViewnodeId = ViewnodeId ( viewnode_id_atom_of (line) . expect ("a viewnode ID")
                        . trim_start_matches ("(vnid ") . trim_end_matches (')')
                        . to_string () );
  f . views_state . open_views . viewnode_id_record_of (&all_changes_view ())
    . owner_by_viewnode_id . get (&viewnode_id) . copied () . unwrap_or (Owner::User) }

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
