// The "all changes" buffer, drawn from the graph pair. The 'sets'
// fixture is described in 'sets.rs'. In the 'kinds' fixture (owned
// skgrepos public and private) each root shows one kind of change:
// txt is retitled and its body edited; ali loses alias a1 and gains
// a3; flg gains a flag; ct drops k1 and gains k2 (ctx, with its
// content, is unchanged); sub subscribes to s2 instead of s1; mv
// moves from public to private; rr's containment of rm moves to
// private; acq acquired acqd by a nodeMerge; gone, which subscribed
// to orph, is deleted.

use crate::pcsvf::common::{
  Fixture, all_changes_buffer, all_changes_buffer_with_marks,
  open_all_changes_buffer, save, skg_files, with_fixture};

use skg::change_forest::{LeafKey, NodeAspect};
use skg::serve::handlers::save_buffer::SaveResponse;
use skg::types::misc::ID;

use std::collections::BTreeMap;
use std::error::Error;

fn buffer_of (
  f : &Fixture,
) -> Result<String, Box<dyn Error>> {
  Ok ( all_changes_buffer (f) ? . expect ("the fixture has changes") ) }

/// The kinds fixture's buffer.
fn kinds_buffer (
) -> Result<String, Box<dyn Error>> {
  let mut buffer : String = String::new ();
  with_fixture ("kinds", &["public", "private"], |f| {
    buffer = buffer_of (f) ?;
    Ok (( )) }) ?;
  Ok (buffer) }

fn sets_buffer (
) -> Result<String, Box<dyn Error>> {
  let mut buffer : String = String::new ();
  with_fixture ("sets", &["main"], |f| {
    buffer = buffer_of (f) ?;
    Ok (( )) }) ?;
  Ok (buffer) }

fn assert_lines_in_order (
  buffer : &str,
  lines  : &[&str],
) {
  let mut rest : &str = buffer;
  for line in lines {
    match rest . find (line) {
      Some (at) => rest = &rest [ at + line . len () .. ],
      None => panic! ( "expected, in order, {:?}; missing {:?} in\n{}",
                       lines, line, buffer ), }}}

#[test]
fn the_roots_are_d_in_id_order (
) -> Result<(), Box<dyn Error>> {
  let roots : Vec<String> =
    sets_buffer () ? . lines ()
    . filter ( |line| line . starts_with ("* ") )
    . map ( |line| line . to_string () )
    . collect ();
  assert_eq! ( roots . len (), 4, "{:?}", roots );
  for (root, id) in roots . iter () . zip ([ "cy2", "ln", "mc", "r" ]) {
    assert! ( root . contains (&format! ("(id {})", id)), "{} in {}", id, root ); }
  Ok (( )) }

#[test]
fn context_bodies_are_folded_and_members_outside_u_are_leaves (
) -> Result<(), Box<dyn Error>> {
  let buffer : String = sets_buffer () ?;
  assert! ( buffer . contains (
    "** (skg bodyFolded (node (id cy1) (repo main)" ),
    "cy1, in U but not C, is context:\n{}", buffer );
  assert! ( buffer . contains (
    "** (skg (node (id keep) (repo main) writeProtected" ),
    "keep, outside U, is a write-protected leaf:\n{}", buffer );
  Ok (( )) }

#[test]
fn a_multiply_contained_root_gets_a_folded_container_folder (
) -> Result<(), Box<dyn Error>> {
  assert_lines_in_order ( &sets_buffer () ?, &[
    "* (skg (node (id mc)",
    "** (skg folded containerFolder)",
    "*** (skg (node (id ma) (repo main) writeProtected",
    "*** (skg (node (id mb) (repo main) writeProtected" ] );
  Ok (( )) }

#[test]
fn a_deleted_subtree_is_drawn_where_it_stood (
) -> Result<(), Box<dyn Error>> {
  assert_lines_in_order ( &sets_buffer () ?, &[
    "* (skg (node (id r)",
    "** (skg (changesetMarker content))",
    "** (skg (historical (id del) (repo main) (diff node- rel-))) del",
    "the body del had",
    "*** (skg (historical (id delc) (repo main) (diff node- rel-))) delc",
    "** (skg (node (id keep)" ] );
  Ok (( )) }

#[test]
fn a_leftward_only_change_shows_its_changed_containers (
) -> Result<(), Box<dyn Error>> {
  assert_lines_in_order ( &sets_buffer () ?, &[
    "* (skg (node (id ln)",
    "** (skg (changesetMarker content))",
    "** (skg (node (id lo)", "(diff rel+))) lo",
    "*** (skg (containerFolder changed))",
    "**** (skg (node (id ln) (repo main) writeProtected", "(diff rel+))) ln",
    "**** (skg (node (id lp) (repo main) writeProtected" ] );
  Ok (( )) }

#[test]
fn a_text_change_shows_the_old_text_and_its_body_diff (
) -> Result<(), Box<dyn Error>> {
  assert_lines_in_order ( &kinds_buffer () ?, &[
    "* (skg (node (id txt)", "txt, retitled\nline one\nline 2\n",
    "** (skg (oldText titleAndBody)) txt\nline one\nline two\n",
    "*** (skg bodyDiff)\n line one\n-line two\n+line 2\n" ] );
  Ok (( )) }

#[test]
fn a_content_change_tags_the_members_and_draws_the_removed_one (
) -> Result<(), Box<dyn Error>> {
  assert_lines_in_order ( &kinds_buffer () ?, &[
    "* (skg (node (id ct)",
    "** (skg (changesetMarker content))",
    "** (skg (node (id k1) (repo public) (affectsParent false) writeProtected (diff rel-))) k1",
    "** (skg (node (id ctx) (repo public) writeProtected",
    "** (skg (node (id k2) (repo public)", "(diff rel+))) k2" ] );
  Ok (( )) }

#[test]
fn alias_id_and_flag_changes_fill_their_folders (
) -> Result<(), Box<dyn Error>> {
  let buffer : String = kinds_buffer () ?;
  assert_lines_in_order ( &buffer, &[
    "** (skg (aliasFolder changed))",
    "*** (skg alias (diff rel-)) a1",
    "*** (skg alias) a2",
    "*** (skg alias (diff rel+)) a3" ] );
  assert_lines_in_order ( &buffer, &[
    "** (skg (idFolder changed))",
    "*** (skg id) acq",
    "*** (skg id (diff rel+)) acqd" ] );
  assert_lines_in_order ( &buffer, &[
    "** (skg (flagsFolder changed))",
    "*** (skg (flag hadId) (diff rel+))" ] );
  Ok (( )) }

#[test]
fn a_subscription_change_shows_on_both_sides (
) -> Result<(), Box<dyn Error>> {
  let buffer : String = kinds_buffer () ?;
  assert_lines_in_order ( &buffer, &[
    "* (skg (node (id sub)",
    "** (skg (subscribeeFolder changed))",
    "*** (skg (node (id s2)", "(diff rel+))) s2",
    "*** (skg (node (id s1) (repo public) (affectsParent false) writeProtected (diff rel-))) s1" ] );
  assert_lines_in_order ( &buffer, &[
    "* (skg (node (id s1)",
    "** (skg (subscriberFolder changed))",
    "*** (skg (node (id sub)", "(diff rel-))) sub" ] );
  Ok (( )) }

#[test]
fn home_and_relRepo_moves_are_drawn (
) -> Result<(), Box<dyn Error>> {
  let buffer : String = kinds_buffer () ?;
  assert_lines_in_order ( &buffer, &[
    "* (skg (node (id mv) (repo private)",
    "** (skg (homeMove (old public) (new private)))" ] );
  assert_lines_in_order ( &buffer, &[
    "* (skg (node (id rr)",
    "** (skg (changesetMarker relRepo))",
    "** (skg (node (id rm)",
    "*** (skg (relRepoMove (old public) (new private)))" ] );
  Ok (( )) }

#[test]
fn a_node_merge_draws_both_acquisition_folders (
) -> Result<(), Box<dyn Error>> {
  let buffer : String = kinds_buffer () ?;
  assert_lines_in_order ( &buffer, &[
    "* (skg (node (id acq)",
    "** (skg acquiredFolder)",
    "*** (skg (historical (id acqd) (repo public) (diff node-))) acqd" ] );
  assert_lines_in_order ( &buffer, &[
    "* (skg (historical (id acqd) (repo public) (diff node-))) acqd",
    "** (skg acquiredByFolder)",
    "*** (skg (node (id acq) (repo public) writeProtected" ] );
  Ok (( )) }

#[test]
fn marks_in_the_store_show_as_ready (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("kinds", &["public", "private"], |f| {
    let alias = |text : &str| LeafKey::Node {
      pid : ID::from ("ali"), aspect : NodeAspect::Alias (text . to_string ()) };
    let buffer : String = all_changes_buffer_with_marks ( f, &[
      LeafKey::Node { pid : ID::from ("txt"), aspect : NodeAspect::Text },
      alias ("a1") ] ) ? . unwrap ();
    assert! ( buffer . contains ("** (skg (oldText titleAndBody) (ready)) txt"),
              "{}", buffer );
    assert_lines_in_order ( &buffer, &[
      "** (skg (aliasFolder changed))", // a3 is not ready, so neither is the folder
      "*** (skg alias (diff rel-) (ready)) a1",
      "*** (skg alias (diff rel+)) a3" ] );
    let buffer : String = all_changes_buffer_with_marks (
      f, &[ alias ("a1"), alias ("a3") ] ) ? . unwrap ();
    assert! ( buffer . contains ("** (skg (aliasFolder changed) (ready))"),
              "{}", buffer );
    Ok (( )) }) }

#[test]
fn a_deleted_subscriber_is_drawn_in_the_changed_subscriber_folder (
) -> Result<(), Box<dyn Error>> {
  assert_lines_in_order ( &kinds_buffer () ?, &[
    "* (skg (node (id orph)",
    "** (skg (subscriberFolder changed))",
    "*** (skg (historical (id gone) (repo public) (diff node- rel-))) gone" ] );
  Ok (( )) }

/// The round-trip invariant for a full buffer: saved unchanged, it
/// changes no file, reports no error, and is reconciled to itself.
fn saves_back_unchanged (
  case     : &str,
  skgrepos : &[&str],
) -> Result<(), Box<dyn Error>> {
  with_fixture (case, skgrepos, |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    let files_before : BTreeMap<String, String> = skg_files (f) ?;
    let response : SaveResponse = save (f, &buffer) ?;
    assert! ( response . errors . is_empty (),
              "the buffer must save without errors, got {:?}\n\nbuffer:\n{}",
              response . errors, buffer );
    assert_eq! ( skg_files (f) ?, files_before,
                 "saving the buffer unchanged must not change the graph:\n{}",
                 buffer );
    assert_eq! ( response . saved_view, buffer,
                 "the reconciled buffer must be the buffer saved" );
    Ok (( )) }) }

#[test]
fn the_sets_buffer_saves_back_unchanged (
) -> Result<(), Box<dyn Error>> {
  saves_back_unchanged ("sets", &["main"]) }

#[test]
fn the_kinds_buffer_saves_back_unchanged (
) -> Result<(), Box<dyn Error>> {
  saves_back_unchanged ("kinds", &["public", "private"]) }
