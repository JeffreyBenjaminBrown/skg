// The "all changes" buffer, rendered from the graph straddle. The 'sets'
// fixture is described in 'sets.rs'. In the 'kinds' fixture (owned
// skgrepos public and private) each root shows one kind of change:
// txt is retitled and its body edited; ali loses alias a1 and gains
// a3, and loses its extra ID ali-old; flg gains a flag; ct drops k1
// (which contains k1c) and gains k2 (ctx, with its content, is unchanged); sub subscribes to s2 instead of s1; mv
// moves from public to private; rr's containment of rm moves to
// private; acq acquired acqd by a nodeMerge; gone, which subscribed
// to orph, is deleted.

use skg::metadata_canon::without_viewnode_ids;
use crate::git_fixture::{Fixture, edit_line, save_cleanly, skg_files};
use crate::git_fixture_helpers::read_graphnode;
use crate::pruned_change_view::common::{
  all_changes_buffer, all_changes_buffer_with_marks, all_changes_view,
  assert_lines_in_order, node_lines, open_all_changes_buffer,
  saves_back_unchanged, with_fixture};

use skg::diff_view::change_leaves::{LeafKey, NodeAspect};
use skg::types::misc::ID;
use skg::types::views_state::DiffViewBufferKind;

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
    "keep, outside U, is a write-protected member without its content:\n{}", buffer );
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
fn a_deleted_subtree_is_shown_where_it_stood (
) -> Result<(), Box<dyn Error>> {
  assert_lines_in_order ( &sets_buffer () ?, &[
    "* (skg (node (id r)",
    "** (skg (changesetMarker content) (diff (leaves 1 all)))",
    "** (skg (historical (id del) (repo main) (diff (node removed) (rel removed) (leaves 2 node rel)))) del",
    "the body del had",
    "*** (skg (historical (id delc) (repo main) (diff (node removed) (rel removed) (leaves 2 node rel)))) delc",
    "** (skg (node (id keep)" ] );
  Ok (( )) }

#[test]
fn an_in_changed_only_change_shows_its_changed_containers (
) -> Result<(), Box<dyn Error>> {
  assert_lines_in_order ( &sets_buffer () ?, &[
    "* (skg (node (id ln)",
    "** (skg (changesetMarker content) (diff (leaves 1 all)))",
    "** (skg (node (id lo)", "(diff (rel added) (leaves 1 rel)))) lo",
    "*** (skg (containerFolder changed))",
    "**** (skg (node (id ln) (repo main) writeProtected",
    "(diff (rel added) (properties changed) (leaves 1 rel all)))) ln",
    "**** (skg (node (id lp) (repo main) writeProtected" ] );
  Ok (( )) }

#[test]
fn a_text_change_shows_the_old_text_and_its_body_diff (
) -> Result<(), Box<dyn Error>> {
  assert_lines_in_order ( &kinds_buffer () ?, &[
    "* (skg (node (id txt)", "txt, retitled\nline one\nline 2\n",
    "** (skg (oldText titleAndBody) (diff (leaves 1))) txt\nline one\nline two\n",
    "*** (skg bodyDiff)\n line one\n-line two\n+line 2\n" ] );
  Ok (( )) }

#[test]
fn a_content_change_tags_the_members_and_shows_the_removed_one (
) -> Result<(), Box<dyn Error>> {
  assert_lines_in_order ( &kinds_buffer () ?, &[
    "* (skg (node (id ct)",
    "** (skg (changesetMarker content) (diff (leaves 2 all)))",
    "** (skg (node (id k1) (repo public) noClaim (rels (contains (out 1))) (diff (rel removed) (leaves 1 rel)))) k1",
    "** (skg (node (id ctx) (repo public) writeProtected",
    "** (skg (node (id k2) (repo public)", "(diff (rel added) (leaves 1 rel)))) k2" ] );
  Ok (( )) }

/// k1, whose only container ct dropped it, stands only as ct's
/// removed member. That is its first occurrence, so it is editable
/// there, with its content list (k1c, outside U, a write-protected
/// member without its content) and its changed container folder.
#[test]
fn a_node_its_only_container_dropped_is_editable_as_the_removed_member (
) -> Result<(), Box<dyn Error>> {
  let buffer : String = kinds_buffer () ?;
  assert_lines_in_order ( &buffer, &[
    "* (skg (node (id ct)",
    "** (skg (node (id k1) (repo public) noClaim (rels",
    "*** (skg (containerFolder changed))",
    "**** (skg (node (id ct) (repo public) noClaim writeProtected",
    "*** (skg (node (id k1c) (repo public) writeProtected" ] );
  assert_eq! ( node_lines (&buffer, "k1") . len (), 1, "{}", buffer );
  Ok (( )) }

#[test]
fn alias_id_and_flag_changes_fill_their_folders (
) -> Result<(), Box<dyn Error>> {
  let buffer : String = kinds_buffer () ?;
  assert_lines_in_order ( &buffer, &[
    "** (skg (aliasFolder changed))",
    "*** (skg removedAlias (diff (rel removed) (leaves 1))) a1",
    "*** (skg alias) a2",
    "*** (skg alias (diff (rel added) (leaves 1))) a3",
    "** (skg (idFolder changed))",
    "*** (skg id) ali",
    "*** (skg removedID (diff (rel removed) (leaves 1))) ali-old" ] );
  assert_lines_in_order ( &buffer, &[
    "** (skg (idFolder changed))",
    "*** (skg id) acq",
    "*** (skg id (diff (rel added) (leaves 1))) acqd" ] );
  assert_lines_in_order ( &buffer, &[
    "** (skg (flagsFolder changed))",
    "*** (skg (flag hadId) (diff (rel added) (leaves 1)))" ] );
  Ok (( )) }

#[test]
fn a_subscription_change_shows_on_both_sides (
) -> Result<(), Box<dyn Error>> {
  let buffer : String = kinds_buffer () ?;
  assert_lines_in_order ( &buffer, &[
    "* (skg (node (id sub)",
    "** (skg (subscribeeFolder changed))",
    "*** (skg (node (id s1) (repo public) noClaim writeProtected (diff (rel removed) (leaves 1 rel)))) s1",
    "*** (skg (node (id s2)", "(diff (rel added) (leaves 1 rel)))) s2" ] );
  assert_lines_in_order ( &buffer, &[
    "* (skg (node (id s1)",
    "** (skg (subscriberFolder changed))",
    "*** (skg (node (id sub) (repo public) noClaim writeProtected",
    "(diff (rel removed) (properties changed) (leaves 2 rel all)))) sub" ] );
  Ok (( )) }

#[test]
fn home_and_relRepo_moves_are_shown (
) -> Result<(), Box<dyn Error>> {
  let buffer : String = kinds_buffer () ?;
  assert_lines_in_order ( &buffer, &[
    "* (skg (node (id mv) (repo private)",
    "** (skg (homeSkgrepoMove (old public) (new private)) (diff (leaves 1)))" ] );
  assert_lines_in_order ( &buffer, &[
    "* (skg (node (id rr)",
    "** (skg (changesetMarker relRepo) (diff (leaves 1 all)))",
    "** (skg (node (id rm)",
    "*** (skg (relRepoMove (old public) (new private)) (diff (leaves 1)))" ] );
  Ok (( )) }

#[test]
fn a_node_merge_shows_both_acquisition_folders (
) -> Result<(), Box<dyn Error>> {
  let buffer : String = kinds_buffer () ?;
  assert_lines_in_order ( &buffer, &[
    "* (skg (node (id acq)",
    "** (skg acquiredFolder)",
    "*** (skg (historical (id acqd) (repo public) (diff (node removed) (leaves 1 node)))) acqd" ] );
  assert_lines_in_order ( &buffer, &[
    "* (skg (historical (id acqd) (repo public) (diff (node removed) (leaves 1 node)))) acqd",
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
    assert! ( without_viewnode_ids (&buffer) . contains ("** (skg (oldText titleAndBody) (diff (leaves 1) (ready))) txt"),
              "{}", buffer );
    assert_lines_in_order ( &buffer, &[
      "** (skg (aliasFolder changed))", // a3 is not ready, so neither is the folder
      "*** (skg removedAlias (diff (rel removed) (leaves 1) (ready))) a1",
      "*** (skg alias (diff (rel added) (leaves 1))) a3" ] );
    let buffer : String = all_changes_buffer_with_marks (
      f, &[ alias ("a1"), alias ("a3") ] ) ? . unwrap ();
    assert! ( without_viewnode_ids (&buffer) . contains ("** (skg (aliasFolder changed) (diff (ready)))"),
              "{}", buffer );
    Ok (( )) }) }

#[test]
fn a_deleted_subscriber_is_shown_in_the_changed_subscriber_folder (
) -> Result<(), Box<dyn Error>> {
  assert_lines_in_order ( &kinds_buffer () ?, &[
    "* (skg (node (id orph)",
    "** (skg (subscriberFolder changed))",
    "*** (skg (historical (id gone) (repo public) (diff (node removed) (rel removed) (leaves 2 node rel)))) gone" ] );
  Ok (( )) }

#[test]
fn the_sets_buffer_saves_back_unchanged (
) -> Result<(), Box<dyn Error>> {
  saves_back_unchanged ("sets", &["main"], DiffViewBufferKind::AllChanges) }

#[test]
fn the_kinds_buffer_saves_back_unchanged (
) -> Result<(), Box<dyn Error>> {
  saves_back_unchanged ("kinds", &["public", "private"], DiffViewBufferKind::AllChanges) }

/// The 'removed_partners' fixture (one skgrepo, main): at HEAD a, b,
/// c and e contain x, which contains xc; d contains c; s subscribes
/// to s1, s2 and s3. Since then b and e stopped containing x, d
/// stopped containing c, and s stopped subscribing to s2. So b, d and
/// e are roots, and b's removed member x is x's first occurrence.
fn removed_partners_buffer (
) -> Result<String, Box<dyn Error>> {
  let mut buffer : String = String::new ();
  with_fixture ("removed_partners", &["main"], |f| {
    buffer = buffer_of (f) ?;
    Ok (( )) }) ?;
  Ok (buffer) }

/// x's container folder shows b, the container it lost, between the
/// two it kept, as a removed member: a non-member with '(diff (rel
/// removed))', and, as a content member would, '(properties changed)',
/// since b's file records the change.
#[test]
fn a_removed_container_stands_between_the_kept_ones (
) -> Result<(), Box<dyn Error>> {
  assert_lines_in_order ( &removed_partners_buffer () ?, &[
    "** (skg (node (id x) (repo main) noClaim (rels",
    "*** (skg (containerFolder changed))",
    "**** (skg (node (id a) (repo main) writeProtected",
    "**** (skg (node (id b) (repo main) noClaim writeProtected",
    "(diff (rel removed) (properties changed) (leaves 1 rel all)))) b",
    "**** (skg (node (id c) (repo main) writeProtected" ] );
  Ok (( )) }

/// x stands first as b's removed member, so it is editable there,
/// with its content list (xc) and its container folder: b and e
/// marked '~-' beside its kept containers a and c, each with its
/// containerward chain, under c the chain node d, which contained c
/// at HEAD only. Its member occurrence under c (d's removed member,
/// editable too) is write-protected.
#[test]
fn a_removed_member_first_in_level_order_is_editable_with_its_changes (
) -> Result<(), Box<dyn Error>> {
  let buffer : String = removed_partners_buffer () ?;
  assert_lines_in_order ( &buffer, &[
    "* (skg (node (id b)",
    "** (skg (node (id x) (repo main) noClaim (rels (contains (in 2) (out 1))) (diff (rel removed) (leaves 1 rel)))) x",
    "*** (skg (containerFolder changed))",
    "**** (skg (node (id a) (repo main) writeProtected",
    "**** (skg (node (id b) (repo main) noClaim writeProtected",
    "**** (skg (node (id c) (repo main) writeProtected",
    "***** (skg (node (id d) (repo main) noClaim writeProtected",
    "**** (skg (node (id e) (repo main) noClaim writeProtected",
    "*** (skg (node (id xc) (repo main) writeProtected",
    "* (skg (node (id d)",
    "** (skg (node (id c) (repo main) noClaim (rels",
    "*** (skg (node (id x) (repo main) writeProtected" ] );
  let editable : Vec<String> = node_lines (&buffer, "x") . into_iter ()
    . filter ( |line| ! line . contains ("writeProtected") ) . collect ();
  assert_eq! ( editable . len (), 1, "{}", buffer );
  Ok (( )) }

/// The 'subscribee_member' fixture (one skgrepo, main): s1
/// subscribes to s0; since HEAD, sub subscribes to s1; c contains s1
/// and p contains sub. So the stub roots c and p show s1 and sub, and
/// s1's changed subscriber folder holds sub, its relationship added,
/// with sub's containerward chain under it: p, a write-protected
/// non-member carrying no change facts. Likewise sub's changed
/// subscribee folder holds s1, with c under it. The buffer saves back
/// unchanged.
#[test]
fn a_member_of_a_changed_subscriber_folder_holds_its_chain (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("subscribee_member", &["main"], |f| {
    let buffer : String = without_viewnode_ids (&buffer_of (f) ?);
    let lines : Vec<&str> = buffer . lines () . collect ();
    for (folder, member, chain_node) in [ ("subscriberFolder", "sub", "p"),
                                          ("subscribeeFolder", "s1", "c") ] {
      let folder_index : usize = lines . iter ()
        . position ( |line| line . starts_with (
          &format! ( "*** (skg ({} changed)", folder )) )
        . unwrap_or_else ( || panic! ( "a changed {}:\n{}", folder, buffer ));
      let member_line : &str = lines [folder_index + 1];
      assert! ( member_line . starts_with (
                  &format! ( "**** (skg (node (id {}) ", member ))
                && member_line . contains ("(diff (rel added)"),
                "{} is the folder's added member:\n{}", member, buffer );
      let chain_line : &str = lines . get (folder_index + 2) . copied () . unwrap_or ("");
      assert! ( chain_line . starts_with (
                  &format! ( "***** (skg (node (id {}) ", chain_node ))
                && chain_line . contains ("noClaim")
                && chain_line . contains ("writeProtected")
                && ! chain_line . contains ("(diff"),
                "{}'s chain, {}, hangs under it:\n{}", member, chain_node, buffer ); }
    Ok (( )) }) ?;
  saves_back_unchanged (
    "subscribee_member", &["main"], DiffViewBufferKind::AllChanges ) }

/// x is editable as b's removed member, which stands before e's, so
/// e's removed member x is write-protected, and holds nothing.
#[test]
fn a_removed_member_whose_node_has_an_earlier_occurrence_is_write_protected (
) -> Result<(), Box<dyn Error>> {
  assert_lines_in_order ( &removed_partners_buffer () ?, &[
    "* (skg (node (id b)",
    "** (skg (node (id x) (repo main) noClaim (rels",
    "* (skg (node (id e)",
    "** (skg (node (id x) (repo main) noClaim writeProtected (rels (contains (in 2) (out 1))) (diff (rel removed) (leaves 1 rel)))) x\n* " ] );
  Ok (( )) }

/// The chain node d under c in x's container folder, from a
/// relationship only the before-graph has, keeps its place through
/// a save, and is not shown a second time.
#[test]
fn a_chain_node_under_an_editable_removed_member_keeps_its_place_through_a_save (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("removed_partners", &["main"], |f| {
    let chain_node : &str =
      "***** (skg (node (id d) (repo main) noClaim writeProtected";
    let buffer : String = open_all_changes_buffer (f) ?;
    assert_eq! ( buffer . matches (chain_node) . count (), 1, "{}", buffer );
    let saved : String = save_cleanly (f, &all_changes_view (), &buffer) ? . saved_view;
    let again : String = save_cleanly (f, &all_changes_view (), &saved) ? . saved_view;
    for view in [ &saved, &again ] {
      assert_eq! ( view . matches (chain_node) . count (), 1, "{}", view );
      assert_lines_in_order ( view, &[
        "** (skg (node (id x) (repo main) noClaim",
        "**** (skg (node (id c) (repo main) writeProtected",
        chain_node ] ); }
    Ok (( )) }) }

/// x, editable as b's removed member, is retitled there. The save
/// writes the title, and x stays editable where it was, now marked
/// modified, its old title beneath it.
#[test]
fn a_removed_member_editable_before_a_save_stays_editable_after_it (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("removed_partners", &["main"], |f| {
    let x_line : &str =
      "** (skg (node (id x) (repo main) noClaim (rels (contains (in 2) (out 1))) (diff (rel removed) (leaves 1 rel)))) x\n";
    let buffer : String = open_all_changes_buffer (f) ?;
    assert! ( without_viewnode_ids (&buffer) . contains (x_line), "{}", buffer );
    let retitled : String = edit_line (
      &buffer, x_line . trim_end (),
      |line| line . replacen ( ")))) x", ")))) x, retitled", 1 ));
    let files_before : BTreeMap<String, String> = skg_files (f) ?;
    let saved : String = save_cleanly (f, &all_changes_view (), &retitled) ? . saved_view;
    assert_ne! ( skg_files (f) ?, files_before );
    assert_eq! ( read_graphnode ( &f . root . path () . join ("main"), "x" ) ? . title,
                 "x, retitled" );
    assert_lines_in_order ( &saved, &[
      "* (skg (node (id b)",
      "** (skg (node (id x) (repo main) noClaim (rels (contains (in 2) (out 1))) (diff (rel removed) (properties changed)",
      "x, retitled\n",
      "*** (skg (oldText title)" ] );
    let editable : Vec<String> = node_lines (&saved, "x") . into_iter ()
      . filter ( |line| ! line . contains ("writeProtected") ) . collect ();
    assert_eq! ( editable . len (), 1, "{}", saved );
    Ok (( )) }) }

/// s's subscribee folder shows s2, the subscribee it lost, where it
/// stood in s's list, not after the subscribees it kept.
#[test]
fn a_removed_subscribee_is_shown_where_it_stood (
) -> Result<(), Box<dyn Error>> {
  assert_lines_in_order ( &removed_partners_buffer () ?, &[
    "* (skg (node (id s)",
    "** (skg (subscribeeFolder changed))",
    "*** (skg (node (id s1) (repo main) writeProtected",
    "*** (skg (node (id s2) (repo main) noClaim writeProtected (diff (rel removed) (leaves 1 rel)))) s2",
    "*** (skg (node (id s3) (repo main) writeProtected" ] );
  Ok (( )) }

#[test]
fn the_removed_partners_buffer_saves_back_unchanged (
) -> Result<(), Box<dyn Error>> {
  saves_back_unchanged ("removed_partners", &["main"], DiffViewBufferKind::AllChanges) }

/// In the 'historical_content' fixture h, which contained m, was
/// deleted: h's historical phantom holds its old content m as a
/// removed member, a non-member with '(diff (rel removed))', beside
/// '(properties changed)' since m was retitled. It is m's first occurrence, so it is
/// editable, its old title beneath it. A buffer showing it saves back
/// unchanged.
#[test]
fn a_historical_phantoms_old_content_is_shown_as_removed_members (
) -> Result<(), Box<dyn Error>> {
  let mut buffer : String = String::new ();
  with_fixture ("historical_content", &["main"], |f| {
    buffer = buffer_of (f) ?;
    Ok (( )) }) ?;
  assert_lines_in_order ( &buffer, &[
    "** (skg (historical (id h) (repo main) (diff (node removed) (rel removed) (leaves 2 node rel)))) h",
    "*** (skg (node (id m) (repo main) noClaim (diff (rel removed) (properties changed) (leaves 2 rel all)))) m, retitled",
    "**** (skg (oldText title) (diff (leaves 1))) m" ] );
  saves_back_unchanged ("historical_content", &["main"], DiffViewBufferKind::AllChanges) }
