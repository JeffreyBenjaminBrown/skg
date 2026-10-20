// Editing a buffer of the diff view like any view: what the
// user may change, what the save refuses, and what is redrawn. The
// 'kinds' fixture is described in 'buffer.rs'. The 'overrides'
// fixture: r overrides n, and p, which contains n, is retitled.

use crate::pcsvf::common::{
  Fixture, open_all_changes_buffer, save, skg_files, view_id_of,
  with_fixture};
use crate::pcsvf::common::git_fixture_helpers::{
  mk_test_tcp_stream_pair, read_graphnode, without_lines_containing};

use skg::serve::handlers::save_buffer::SaveResponse;
use skg::test_utils::{
  extract_string_field_from_sexp, read_all_lp_messages,
  update_from_and_rerender_buffer_test};
use skg::to_org::render::content_view::multi_root_view;
use skg::types::misc::ID;
use skg::types::nodes::complete::Graphnode;
use skg::types::views_state::{DiffViewBufferKind, ViewId};
use skg::types::viewnode::Viewnode;

use ego_tree::Tree;
use futures::executor::block_on;
use std::collections::BTreeMap;
use std::error::Error;
use std::io::BufReader;
use std::net::TcpStream;

/// 'buffer' with 'new_line' inserted after its first line starting
/// 'prefix'.
fn insert_after_line_starting (
  buffer   : &str,
  prefix   : &str,
  new_line : &str,
) -> String {
  let mut inserted : bool = false;
  let mut lines : Vec<&str> = Vec::new ();
  for line in buffer . lines () {
    lines . push (line);
    if ! inserted && line . starts_with (prefix) {
      lines . push (new_line);
      inserted = true; }}
  assert! ( inserted, "no line starts {:?}:\n{}", prefix, buffer );
  lines . join ("\n") + "\n" }

/// Deleting the changed ID folders changes nothing: the save writes
/// no file, and the reconciled buffer draws the folders again.
#[test]
fn a_deleted_id_folder_respawns (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("kinds", &["public", "private"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    let edited : String = [ "(skg id", "(skg removedID", "(skg (idFolder" ]
      . into_iter () . fold ( buffer . clone (), |edited, atom|
        without_lines_containing (&edited, atom) );
    let files_before : BTreeMap<String, String> = skg_files (f) ?;
    let response : SaveResponse = save (f, &edited) ?;
    assert! ( response . errors . is_empty (),
              "the save must succeed, got {:?}", response . errors );
    assert_eq! ( skg_files (f) ?, files_before,
                 "deleting the ID folder must change no file" );
    assert_eq! ( response . saved_view, buffer,
                 "the ID folder must be drawn again" );
    Ok (( )) }) }

/// Deleting an entry of the changed ID folder, while keeping the
/// folder, aborts the save: the folder no longer matches the node's
/// IDs. No file changes.
#[test]
fn editing_the_id_folder_aborts_the_save (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("kinds", &["public", "private"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    let edited : String =
      without_lines_containing (&buffer, "(skg id (diff rel+)) acqd");
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
/// is not an alias, so ali keeps a2 and a3, and the reconciled buffer
/// draws a1 as removed again.
#[test]
fn a_removed_alias_is_not_restored_by_a_save (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("kinds", &["public", "private"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    assert! ( buffer . contains ("*** (skg removedAlias (diff rel-)) a1"),
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
    assert! ( buffer . contains ("*** (skg removedID (diff rel-)) ali-old"),
              "{}", buffer );
    let files_before : BTreeMap<String, String> = skg_files (f) ?;
    let response : SaveResponse = save (f, &buffer) ?;
    assert! ( response . errors . is_empty (),
              "the save must succeed, got {:?}", response . errors );
    assert_eq! ( skg_files (f) ?, files_before );
    assert_eq! ( response . saved_view, buffer );
    Ok (( )) }) }

/// Deleting the save-inert images of removed things changes nothing:
/// k1, removed from ct's content, and gone, a deleted subscriber of
/// orph. Both are drawn again where they stood.
#[test]
fn deleted_images_of_removed_members_respawn (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("kinds", &["public", "private"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    let edited : String = without_lines_containing (
      &without_lines_containing (&buffer, "(id k1)"),
      "*** (skg (historical (id gone)" );
    let files_before : BTreeMap<String, String> = skg_files (f) ?;
    let response : SaveResponse = save (f, &edited) ?;
    assert! ( response . errors . is_empty (),
              "the save must succeed, got {:?}", response . errors );
    assert_eq! ( skg_files (f) ?, files_before,
                 "deleting a save-inert image must change no file" );
    assert_eq! ( response . saved_view, buffer,
                 "both removed members must be drawn again" );
    Ok (( )) }) }

/// A child added under an editable node of the buffer is created on
/// disk, and the reconciled buffer draws it as new, in a new
/// relationship.
#[test]
fn a_new_child_is_created_and_drawn_as_new (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("kinds", &["public", "private"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    let edited : String = insert_after_line_starting (
      &buffer, "* (skg (node (id ct)", "** a new child" );
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
      line . starts_with ( &format! ("** (skg (node (id {})", new_child . 0) )
      && line . contains ("(diff node+ rel+)")
      && line . ends_with ("a new child") ),
      "the new child must be drawn as new:\n{}", response . saved_view );
    Ok (( )) }) }

/// When another view's save changes what a buffer of the total diff
/// view shows, the buffer is rerendered as a collateral view over the
/// fresh graph straddle: it shows the new change and keeps the old ones.
#[test]
fn a_collateral_buffer_shows_the_new_change_and_keeps_the_old (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("kinds", &["public", "private"], |f| {
    open_all_changes_buffer (f) ?;
    let content_view_id : ViewId =
      ViewId::ContentView ( "content-view-of-ct" . to_string () );
    let content_view : String = register_content_view_of (
      f, "ct", &content_view_id ) ?;
    let edited : String = insert_after_line_starting (
      &content_view, "* (skg (node (id ct)", "** a new child" );
    let (mut stream, read_end) : (TcpStream, TcpStream) =
      mk_test_tcp_stream_pair ();
    let response : SaveResponse = block_on (
      update_from_and_rerender_buffer_test (
        &mut stream, &edited, &f . config, &f . tantivy, &f . graph,
        &Ok (content_view_id), &mut f . views_state )) ?;
    assert! ( response . errors . is_empty (),
              "the save must succeed, got {:?}", response . errors );
    drop (stream); // so that reading the stream ends
    let collateral : String = read_all_lp_messages (
      &mut BufReader::new (read_end) ) . into_iter ()
      . find ( |message| message . contains ("collateral-view")
               && message . contains (
                 &view_id_of (DiffViewBufferKind::AllChanges) . repr_in_client () ))
      . and_then ( |message|
        extract_string_field_from_sexp (&message, "content") )
      . expect ("the all-changes buffer is rerendered as a collateral view");
    assert! ( collateral . lines () . any ( |line|
      line . contains ("(diff node+ rel+)")
      && line . ends_with ("a new child") ),
      "the new child must be drawn as new:\n{}", collateral );
    assert! ( collateral . contains ("** (skg (oldText titleAndBody)) txt"),
              "txt's old text must stay:\n{}", collateral );
    assert! ( collateral . contains ("(diff rel-))) k1"),
              "the removed member k1 must stay:\n{}", collateral );
    Ok (( )) }) }

/// Register a content view of 'root' under 'view_id'; return its text.
fn register_content_view_of (
  f       : &mut Fixture,
  root    : &str,
  view_id : &ViewId,
) -> Result<String, Box<dyn Error>> {
  let (text, pids, tree) : (String, Vec<ID>, Tree<Viewnode>) =
    multi_root_view ( &f . config, Some (&f . tantivy), &[ ID::from (root) ] ) ?;
  f . views_state . open_views . register_view (
    &f . graph . load_full (), view_id . clone (), tree, &pids );
  Ok (text) }

/// The diff view shows the graph as stored: a member that an
/// overrider would replace in an ordinary view is drawn as itself.
#[test]
fn override_substitution_is_off (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("overrides", &["main"], |f| {
    let (ordinary_view, _pids, _tree) : (String, Vec<ID>, Tree<Viewnode>) =
      multi_root_view ( &f . config, Some (&f . tantivy), &[ ID::from ("p") ] ) ?;
    assert! ( ordinary_view . contains ("(overridesHere n)"),
              "precondition: an ordinary view of p draws r for n:\n{}",
              ordinary_view );
    let buffer : String = open_all_changes_buffer (f) ?;
    assert! ( buffer . lines () . any ( |line|
                line . starts_with ("** (skg (node (id n)") ),
              "n must be drawn as itself under p:\n{}", buffer );
    assert! ( ! buffer . contains ("overridesHere"),
              "no member is drawn in place of another:\n{}", buffer );
    Ok (( )) }) }
