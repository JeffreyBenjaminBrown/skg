// Editing a buffer of the diff view like any view: what the
// user may change, what the save refuses, and what is rerendered. The
// 'kinds' fixture is described in 'buffer.rs'. The 'overrides'
// fixture: r overrides n, and p, which contains n, is retitled.

use skg::metadata_canon::without_viewnode_ids;
use crate::git_fixture::{Fixture, edit_line, skg_files};
use crate::pruned_change_view::common::{
  insert_after_line_starting, open_all_changes_buffer, save, viewforest_id_of,
  with_fixture, without_subtree_starting};
use crate::git_fixture_helpers::{
  mk_test_tcp_stream_pair, read_graphnode, without_lines_containing};

use skg::refresh::buffer_corrections::{BufferCorrectionKind, ViewBufferCorrections};
use skg::serve::handlers::save_buffer::SaveResponse;
use skg::test_utils::{
  extract_string_field_from_sexp, read_all_lp_messages,
  update_from_and_rerender_buffer_test};
use skg::test_utils::multi_root_view;
use skg::types::misc::ID;
use skg::types::nodes::complete::Graphnode;
use skg::types::views_state::{DiffViewBufferKind, ViewforestId};
use skg::types::viewnode::Viewnode;

use ego_tree::Tree;
use futures::executor::block_on;
use std::collections::BTreeMap;
use std::error::Error;
use std::io::BufReader;
use std::net::TcpStream;

/// Deleting the changed ID folders changes nothing: the save writes
/// no file, and the refreshed buffer shows the folders again.
#[test]
fn a_deleted_id_folder_respawns (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("kinds", &["public", "private"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    let edited : String = [ "(skg id", "(skg (idFolder" ]
      . into_iter () . fold ( buffer . clone (), |edited, atom|
        without_lines_containing (&edited, atom) );
    let files_before : BTreeMap<String, String> = skg_files (f) ?;
    let response : SaveResponse = save (f, &edited) ?;
    assert! ( response . errors . is_empty (),
              "the save must succeed, got {:?}", response . errors );
    assert_eq! ( skg_files (f) ?, files_before,
                 "deleting the ID folder must change no file" );
    assert_eq! ( without_viewnode_ids (&response . saved_view), without_viewnode_ids (&buffer),
                 "the ID folder must be shown again" );
    Ok (( )) }) }

/// Deleting a member of the changed ID folder, while keeping the
/// folder, aborts the save: the folder no longer matches the node's
/// IDs. No file changes.
#[test]
fn editing_the_id_folder_aborts_the_save (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("kinds", &["public", "private"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    let edited : String =
      without_lines_containing (&buffer, "(skg id (diff (rel added) (leaves 1))) acqd");
    let files_before : BTreeMap<String, String> = skg_files (f) ?;
    let error : String = format! ( "{:?}",
      save (f, &edited) . err ()
      . expect ("editing the ID folder must abort the save") );
    assert! ( error . contains ("IDFolder_Edited"),
              "the error should be IDFolder_Edited: {}", error );
    assert_eq! ( skg_files (f) ?, files_before,
                 "an aborted save must change no file" );
    Ok (( )) }) }

/// A save does not restore a removed alias: ali's removed alias a1
/// is not an alias, so ali keeps a2 and a3, and the refreshed buffer
/// shows a1 as removed again.
#[test]
fn a_removed_alias_is_not_restored_by_a_save (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("kinds", &["public", "private"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    assert! ( without_viewnode_ids (&buffer) . contains ("*** (skg alias noClaim (view ghostWarrant) (diff (rel removed) (leaves 1))) a1"),
              "{}", buffer );
    let response : SaveResponse = save (f, &buffer) ?;
    assert! ( response . errors . is_empty (),
              "the save must succeed, got {:?}", response . errors );
    let ali : Graphnode =
      read_graphnode ( &f . root . path () . join ("public"), "ali" ) ?;
    assert_eq! ( ali . aliases . or_default () . iter ()
                 . map ( |alias| alias . member . as_str () ) . collect::<Vec<&str>> (),
                 vec! ["a2", "a3"] );
    assert_eq! ( response . saved_view, buffer );
    Ok (( )) }) }

/// A removed ID does not abort the save: ali's lost extra ID ali-old
/// is not one of its IDs, so its ID folder matches the node's.
#[test]
fn a_removed_id_does_not_abort_the_save (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("kinds", &["public", "private"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    assert! ( without_viewnode_ids (&buffer) . contains ("*** (skg id (view ghostWarrant) (diff (rel removed) (leaves 1))) ali-old"),
              "{}", buffer );
    let files_before : BTreeMap<String, String> = skg_files (f) ?;
    let response : SaveResponse = save (f, &buffer) ?;
    assert! ( response . errors . is_empty (),
              "the save must succeed, got {:?}", response . errors );
    assert_eq! ( skg_files (f) ?, files_before );
    assert_eq! ( response . saved_view, buffer );
    Ok (( )) }) }

/// Deleting the save-inert images of removed things changes nothing:
/// k1, removed from ct's content (editable and claiming nothing, deleted
/// with what it holds), and gone, a deleted subscriber of orph. Both
/// are shown again where they stood.
#[test]
fn deleted_images_of_removed_members_respawn (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("kinds", &["public", "private"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    let edited : String = without_lines_containing (
      &without_subtree_starting (&buffer, "** (skg node (gnid k1)"),
      "*** (skg ghost (gnid gone)" );
    let files_before : BTreeMap<String, String> = skg_files (f) ?;
    let response : SaveResponse = save (f, &edited) ?;
    assert! ( response . errors . is_empty (),
              "the save must succeed, got {:?}", response . errors );
    assert_eq! ( skg_files (f) ?, files_before,
                 "deleting a save-inert image must change no file" );
    assert_eq! ( without_viewnode_ids (&response . saved_view), without_viewnode_ids (&buffer),
                 "both removed members must be shown again" );
    Ok (( )) }) }

/// A child added under an editable node of the buffer is created on
/// disk, and the refreshed buffer shows it as new, in a new
/// relationship.
#[test]
fn a_new_child_is_created_and_shown_as_new (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("kinds", &["public", "private"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    let edited : String = insert_after_line_starting (
      &buffer, "* (skg node (gnid ct)", "** a new child" );
    let response : SaveResponse = save (f, &edited) ?;
    assert! ( response . errors . is_empty (),
              "the save must succeed, got {:?}", response . errors );
    let ct : Graphnode =
      read_graphnode ( &f . root . path () . join ("public"), "ct" ) ?;
    let new_child : ID = ct . contains . first ()
      . expect ("ct contains the new child first") . member . clone ();
    assert! ( f . root . path () . join ("public")
              . join ( format! ("{}.skg", new_child . 0) ) . exists (),
              "the new child's file must exist" );
    assert! ( response . saved_view . lines () . any ( |line|
      without_viewnode_ids (line) . starts_with ( &format! ("** (skg node (gnid {})", new_child . 0) )
      && line . contains ("(diff (node added) (rel added)")
      && line . ends_with ("a new child") ),
      "the new child must be shown as new:\n{}", response . saved_view );
    Ok (( )) }) }

/// When another view's save changes what a buffer of the diff
/// view shows, the buffer is rerendered as a collateral view over the
/// fresh graph straddle: it shows the new change and keeps the old ones.
#[test]
fn a_collateral_buffer_shows_the_new_change_and_keeps_the_old (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("kinds", &["public", "private"], |f| {
    open_all_changes_buffer (f) ?;
    let content_viewforest_id : ViewforestId =
      ViewforestId::ContentView ( "content-view-of-ct" . to_string () );
    let content_view : String = register_content_view_of (
      f, "ct", &content_viewforest_id ) ?;
    let edited : String = insert_after_line_starting (
      &content_view, "* (skg node (gnid ct)", "** a new child" );
    let (mut stream, read_end) : (TcpStream, TcpStream) =
      mk_test_tcp_stream_pair ();
    let response : SaveResponse = block_on (
      update_from_and_rerender_buffer_test (
        &mut stream, &edited, &f . config, &f . tantivy, &f . graph,
        &Ok (content_viewforest_id), &mut f . views_state )) ?;
    assert! ( response . errors . is_empty (),
              "the save must succeed, got {:?}", response . errors );
    drop (stream); // so that reading the stream ends
    let collateral : String = read_all_lp_messages (
      &mut BufReader::new (read_end) ) . into_iter ()
      . find ( |message| message . contains ("collateral-view")
               && message . contains (
                 &viewforest_id_of (DiffViewBufferKind::AllChanges) . repr_in_client () ))
      . and_then ( |message|
        extract_string_field_from_sexp (&message, "content") )
      . expect ("the all-changes buffer is rerendered as a collateral view");
    assert! ( without_viewnode_ids (&collateral) . lines () . any ( |line|
      line . contains ("(diff (node added) (rel added)")
      && line . ends_with ("a new child") ),
      "the new child must be shown as new:\n{}", collateral );
    assert! ( without_viewnode_ids (&collateral) . contains ("** (skg (oldText titleAndBody) (diff (leaves 1))) txt"),
              "txt's old text must stay:\n{}", collateral );
    assert! ( without_viewnode_ids (&collateral) . contains ("(diff (rel removed) (leaves 1 rel))) k1"),
              "the removed member k1 must stay:\n{}", collateral );
    Ok (( )) }) }

/// Register a content view of 'root' under 'viewforest_id'; return its text.
fn register_content_view_of (
  f       : &mut Fixture,
  root    : &str,
  viewforest_id : &ViewforestId,
) -> Result<String, Box<dyn Error>> {
  let (text, pids, tree) : (String, Vec<ID>, Tree<Viewnode>) =
    multi_root_view ( &f . config, Some (&f . tantivy), &[ ID::from (root) ] ) ?;
  f . views_state . open_views . register_view (
    &f . graph . load_full (), viewforest_id . clone (), tree, &pids );
  Ok (text) }

/// The diff view shows the graph as stored: a member that an
/// overrider would replace in an ordinary view is shown as itself.
#[test]
fn override_substitution_is_off (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("overrides", &["main"], |f| {
    let (ordinary_view, _pids, _tree) : (String, Vec<ID>, Tree<Viewnode>) =
      multi_root_view ( &f . config, Some (&f . tantivy), &[ ID::from ("p") ] ) ?;
    assert! ( ordinary_view . contains ("(overridesHere n)"),
              "precondition: an ordinary view of p shows r for n:\n{}",
              ordinary_view );
    let buffer : String = open_all_changes_buffer (f) ?;
    assert! ( buffer . lines () . any ( |line|
                without_viewnode_ids (line) . starts_with ("** (skg node (gnid n)") ),
              "n must be shown as itself under p:\n{}", buffer );
    assert! ( ! buffer . contains ("overridesHere"),
              "no member is shown in place of another:\n{}", buffer );
    Ok (( )) }) }

/// The diff view's buffer corrections ride the save response like a content
/// view's ('SaveResponse.buffer_corrections'). A plain save of the "all
/// changes" buffer reports nothing, and nor does a save that retitles
/// a node: drawing the old text a change calls for is no buffer correction.
/// In the 'shared' fixture n, editable under x in the diff view, gains
/// a child in a content view of n; the diff view, rerendered as
/// collateral, draws it where n's list already drawn lacked it, and
/// reports that, tagged with the diff view.
#[test]
fn the_diff_views_buffer_corrections_ride_the_save_response (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("shared", &["main"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    let plain : SaveResponse = save (f, &buffer) ?;
    assert! ( plain . errors . is_empty (), "{:?}", plain . errors );
    assert! ( plain . buffer_corrections . is_empty (),
              "a plain save corrects nothing: {:?}", plain . buffer_corrections );
    let retitled : SaveResponse = save ( f, &edit_line (
      &plain . saved_view, "* (skg node (gnid x) (skgRepos (nodeRepo main))",
      |line| format! ("{}, retitled", line) )) ?;
    assert! ( retitled . errors . is_empty (), "{:?}", retitled . errors );
    assert! ( without_viewnode_ids (&retitled . saved_view) . contains ("x, retitled\n"),
              "{}", retitled . saved_view );
    assert! ( retitled . buffer_corrections . is_empty (),
              "a retitle corrects nothing: {:?}", retitled . buffer_corrections );
    let view_of_n : ViewforestId = ViewforestId::ContentView ("content view of n" . into ());
    let (text, pids, tree) : (String, Vec<ID>, Tree<Viewnode>) =
      multi_root_view ( &f . config, Some (&f . tantivy), &[ ID::from ("n") ] ) ?;
    f . views_state . open_views . register_view (
      &f . graph . load_full (), view_of_n . clone (), tree, &pids );
    let (mut stream, _read_end) : (TcpStream, TcpStream) = mk_test_tcp_stream_pair ();
    let response : SaveResponse = block_on (
      update_from_and_rerender_buffer_test (
        &mut stream, &format! ("{}** new child\n", text),
        &f . config, &f . tantivy, &f . graph,
        &Ok (view_of_n), &mut f . views_state )) ?;
    assert! ( response . errors . is_empty (), "{:?}", response . errors );
    assert_eq! ( response . buffer_corrections . len (), 1,
                 "only the diff view is corrected: {:?}", response . buffer_corrections );
    let diff_view : &ViewBufferCorrections = &response . buffer_corrections [0];
    assert_eq! ( diff_view . viewforest_id, viewforest_id_of (DiffViewBufferKind::AllChanges) );
    assert_eq! ( diff_view . buffer_corrections . iter () . map ( |c| ( c . kind, c . title . as_str () ))
                 . collect::<Vec<(BufferCorrectionKind, &str)>> (),
                 vec! [ (BufferCorrectionKind::Reinstated, "new child") ],
                 "{:?}", diff_view . buffer_corrections );
    Ok (( )) }) }
