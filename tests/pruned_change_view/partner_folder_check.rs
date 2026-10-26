// The save's check of a write-protected occurrence's partner folders
// reads each member as the folder reconciler matches it
// ('skgid_a_folder_child_stands_for'). The 'partner_ids' fixture (one
// skgrepo, main): c1 and c2, whose bodies changed, each contain
// recorder, unchanged, which subscribes to x by its extra ID x2 and
// to ghost, which no node has. So recorder is shown write-protected
// in the content of each.

use crate::pruned_change_view::common::{open_all_changes_buffer, save, with_fixture};

use skg::serve::handlers::save_buffer::SaveResponse;
use skg::types::errors::{BufferValidationError, SaveError};
use skg::types::misc::ID;

use std::error::Error;
use skg::metadata_canon::without_viewnode_ids;

/// The buffer with a subscribee folder holding 'members' under the
/// write-protected recorder in c2's content.
fn with_subscribee_folder_under_recorder (
  buffer  : &str,
  members : &[&str],
) -> String {
  let recorder_under_c2 : &str = buffer . lines ()
    . skip_while ( |line| ! without_viewnode_ids (line) . starts_with ("* (skg node (gnid c2) ") )
    . find ( |line| without_viewnode_ids (line) . starts_with (
      "** (skg node (gnid recorder) (skgRepos (nodeRepo main)) writeProtected ") )
    . unwrap_or_else ( || panic! (
      "recorder is shown write-protected under c2:\n{}", buffer ));
  buffer . replacen ( recorder_under_c2, &format! (
    "{}\n*** (skg subscribeeFolder)\n{}", recorder_under_c2,
    members . iter () . map ( |member| format! ("**** {}", member) )
    . collect::<Vec<String>> () . join ("\n") ), 1 ) }

/// x shown by its extra ID x2, as recorder lists it, and ghost shown
/// as an unknown phantom with its raw ID: the folder says what the
/// graph says, and the buffer saves.
#[test]
fn a_member_shown_by_an_extra_id_and_an_unknown_member_pass (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("partner_ids", &["main"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    let response : SaveResponse = save ( f, &with_subscribee_folder_under_recorder (
      &buffer, &[ "(skg node (gnid x2) (skgRepos (nodeRepo main)) writeProtected) x",
                  "(skg unknown (gnid ghost))" ] )) ?;
    assert! ( response . errors . is_empty (), "{:?}", response . errors );
    Ok (( )) }) }

/// A dangling removed member, as the diff view shows one in a
/// write-protected folder (an unknown phantom with '(rel removed)'),
/// is not counted: beside the members recorder lists, the folder
/// saves unchanged.
#[test]
fn a_dangling_removed_member_is_not_counted (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("partner_ids", &["main"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    let response : SaveResponse = save ( f, &with_subscribee_folder_under_recorder (
      &buffer, &[ "(skg node (gnid x2) (skgRepos (nodeRepo main)) writeProtected) x",
                  "(skg unknown (gnid ghost))",
                  "(skg unknown (gnid gone) (diff (rel removed)))" ] )) ?;
    assert! ( response . errors . is_empty (), "{:?}", response . errors );
    Ok (( )) }) }

/// An unknown phantom is compared by its raw ID: shown with an ID
/// recorder does not list, the folder is refused.
#[test]
fn an_unknown_member_with_another_raw_id_is_refused (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("partner_ids", &["main"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    let refused = save ( f, &with_subscribee_folder_under_recorder (
      &buffer, &[ "(skg node (gnid x2) (skgRepos (nodeRepo main)) writeProtected) x",
                  "(skg unknown (gnid other_ghost))" ] ));
    let errors : Vec<BufferValidationError> = match refused {
      Err (error) => match error . downcast::<SaveError> () {
        Ok (save_error) => match *save_error {
          SaveError::BufferValidationErrors { errors, .. } => errors,
          other => panic! ( "expected validation errors, got {:?}", other ), },
        Err (other) => panic! ( "expected a SaveError, got {:?}", other ), },
      Ok (response) => panic! (
        "the save must be refused:\n{}", response . saved_view ), };
    assert! ( matches! (
      &errors [..],
      [ BufferValidationError::EditedWriteProtectedOccurrence {
          skgid, changes, .. } ]
      if *skgid == ID::from ("recorder")
         && *changes == vec! [ "changed subscriptions" . to_string () ] ),
      "{:?}", errors );
    Ok (( )) }) }
