// The "all changes" buffer, drawn from the graph straddle. The 'sets'
// fixture is described in 'sets.rs'. In the 'kinds' fixture (owned
// skgrepos public and private) each root shows one kind of change:
// txt is retitled and its body edited; ali loses alias a1 and gains
// a3, and loses its extra ID ali-old; flg gains a flag; ct drops k1 and gains k2 (ctx, with its
// content, is unchanged); sub subscribes to s2 instead of s1; mv
// moves from public to private; rr's containment of rm moves to
// private; acq acquired acqd by a nodeMerge; gone, which subscribed
// to orph, is deleted.

use crate::git_fixture::Fixture;
use crate::pcsvf::common::{
  all_changes_buffer, all_changes_buffer_with_marks, assert_lines_in_order,
  saves_back_unchanged, with_fixture};

use skg::diff_view::change_leaves::{LeafKey, NodeAspect};
use skg::types::misc::ID;
use skg::types::views_state::DiffViewBufferKind;

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
    "keep, outside U, is an unexpanded write-protected member:\n{}", buffer );
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
    "** (skg (changesetMarker content) (leaves 1 all))",
    "** (skg (historical (id del) (repo main) (diff node- rel-) (leaves 2 node rel))) del",
    "the body del had",
    "*** (skg (historical (id delc) (repo main) (diff node- rel-) (leaves 2 node rel))) delc",
    "** (skg (node (id keep)" ] );
  Ok (( )) }

#[test]
fn an_in_changed_only_change_shows_its_changed_containers (
) -> Result<(), Box<dyn Error>> {
  assert_lines_in_order ( &sets_buffer () ?, &[
    "* (skg (node (id ln)",
    "** (skg (changesetMarker content) (leaves 1 all))",
    "** (skg (node (id lo)", "(diff rel+) (leaves 1 rel))) lo",
    "*** (skg (containerFolder changed))",
    "**** (skg (node (id ln) (repo main) writeProtected", "(diff rel+) (leaves 1 rel))) ln",
    "**** (skg (node (id lp) (repo main) writeProtected" ] );
  Ok (( )) }

#[test]
fn a_text_change_shows_the_old_text_and_its_body_diff (
) -> Result<(), Box<dyn Error>> {
  assert_lines_in_order ( &kinds_buffer () ?, &[
    "* (skg (node (id txt)", "txt, retitled\nline one\nline 2\n",
    "** (skg (oldText titleAndBody) (leaves 1)) txt\nline one\nline two\n",
    "*** (skg bodyDiff)\n line one\n-line two\n+line 2\n" ] );
  Ok (( )) }

#[test]
fn a_content_change_tags_the_members_and_draws_the_removed_one (
) -> Result<(), Box<dyn Error>> {
  assert_lines_in_order ( &kinds_buffer () ?, &[
    "* (skg (node (id ct)",
    "** (skg (changesetMarker content) (leaves 2 all))",
    "** (skg (node (id k1) (repo public) (affectsParent false) writeProtected (diff rel-) (leaves 1 rel))) k1",
    "** (skg (node (id ctx) (repo public) writeProtected",
    "** (skg (node (id k2) (repo public)", "(diff rel+) (leaves 1 rel))) k2" ] );
  Ok (( )) }

#[test]
fn alias_id_and_flag_changes_fill_their_folders (
) -> Result<(), Box<dyn Error>> {
  let buffer : String = kinds_buffer () ?;
  assert_lines_in_order ( &buffer, &[
    "** (skg (aliasFolder changed))",
    "*** (skg removedAlias (diff rel-) (leaves 1)) a1",
    "*** (skg alias) a2",
    "*** (skg alias (diff rel+) (leaves 1)) a3",
    "** (skg (idFolder changed))",
    "*** (skg id) ali",
    "*** (skg removedID (diff rel-) (leaves 1)) ali-old" ] );
  assert_lines_in_order ( &buffer, &[
    "** (skg (idFolder changed))",
    "*** (skg id) acq",
    "*** (skg id (diff rel+) (leaves 1)) acqd" ] );
  assert_lines_in_order ( &buffer, &[
    "** (skg (flagsFolder changed))",
    "*** (skg (flag hadId) (diff rel+) (leaves 1))" ] );
  Ok (( )) }

#[test]
fn a_subscription_change_shows_on_both_sides (
) -> Result<(), Box<dyn Error>> {
  let buffer : String = kinds_buffer () ?;
  assert_lines_in_order ( &buffer, &[
    "* (skg (node (id sub)",
    "** (skg (subscribeeFolder changed))",
    "*** (skg (node (id s1) (repo public) (affectsParent false) writeProtected (diff rel-) (leaves 1 rel))) s1",
    "*** (skg (node (id s2)", "(diff rel+) (leaves 1 rel))) s2" ] );
  assert_lines_in_order ( &buffer, &[
    "* (skg (node (id s1)",
    "** (skg (subscriberFolder changed))",
    "*** (skg (node (id sub) (repo public) (affectsParent false) writeProtected",
    "(diff rel-) (leaves 1 rel))) sub" ] );
  Ok (( )) }

#[test]
fn home_and_relRepo_moves_are_drawn (
) -> Result<(), Box<dyn Error>> {
  let buffer : String = kinds_buffer () ?;
  assert_lines_in_order ( &buffer, &[
    "* (skg (node (id mv) (repo private)",
    "** (skg (homeSkgrepoMove (old public) (new private)) (leaves 1))" ] );
  assert_lines_in_order ( &buffer, &[
    "* (skg (node (id rr)",
    "** (skg (changesetMarker relRepo) (leaves 1 all))",
    "** (skg (node (id rm)",
    "*** (skg (relRepoMove (old public) (new private)) (leaves 1))" ] );
  Ok (( )) }

#[test]
fn a_node_merge_draws_both_acquisition_folders (
) -> Result<(), Box<dyn Error>> {
  let buffer : String = kinds_buffer () ?;
  assert_lines_in_order ( &buffer, &[
    "* (skg (node (id acq)",
    "** (skg acquiredFolder)",
    "*** (skg (historical (id acqd) (repo public) (diff node-) (leaves 1 node))) acqd" ] );
  assert_lines_in_order ( &buffer, &[
    "* (skg (historical (id acqd) (repo public) (diff node-) (leaves 1 node))) acqd",
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
    assert! ( buffer . contains ("** (skg (oldText titleAndBody) (ready) (leaves 1)) txt"),
              "{}", buffer );
    assert_lines_in_order ( &buffer, &[
      "** (skg (aliasFolder changed))", // a3 is not ready, so neither is the folder
      "*** (skg removedAlias (diff rel-) (ready) (leaves 1)) a1",
      "*** (skg alias (diff rel+) (leaves 1)) a3" ] );
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
    "*** (skg (historical (id gone) (repo public) (diff node- rel-) (leaves 2 node rel))) gone" ] );
  Ok (( )) }

#[test]
fn the_sets_buffer_saves_back_unchanged (
) -> Result<(), Box<dyn Error>> {
  saves_back_unchanged ("sets", &["main"], DiffViewBufferKind::AllChanges) }

#[test]
fn the_kinds_buffer_saves_back_unchanged (
) -> Result<(), Box<dyn Error>> {
  saves_back_unchanged ("kinds", &["public", "private"], DiffViewBufferKind::AllChanges) }

/// The 'removed_partners' fixture (one skgrepo, main): at HEAD a, b
/// and c contain x, and s subscribes to s1, s2 and s3. Since then b
/// stopped containing x, s stopped subscribing to s2, and a was
/// retitled, so that x is drawn editable in a's content.
fn removed_partners_buffer (
) -> Result<String, Box<dyn Error>> {
  let mut buffer : String = String::new ();
  with_fixture ("removed_partners", &["main"], |f| {
    buffer = buffer_of (f) ?;
    Ok (( )) }) ?;
  Ok (buffer) }

/// x's container folder draws b, the container it lost, between the
/// two it kept, as a removed member: a non-member with '(diff rel-)'.
#[test]
fn a_removed_container_is_drawn_between_the_kept_ones (
) -> Result<(), Box<dyn Error>> {
  assert_lines_in_order ( &removed_partners_buffer () ?, &[
    "** (skg (node (id x) (repo main) (rels",
    "*** (skg (containerFolder changed))",
    "**** (skg (node (id a) (repo main) writeProtected",
    "**** (skg (node (id b) (repo main) (affectsParent false) writeProtected (diff rel-) (leaves 1 rel))) b",
    "**** (skg (node (id c) (repo main) writeProtected" ] );
  Ok (( )) }

/// s's subscribee folder draws s2, the subscribee it lost, where it
/// stood in s's list, not after the subscribees it kept.
#[test]
fn a_removed_subscribee_is_drawn_where_it_stood (
) -> Result<(), Box<dyn Error>> {
  assert_lines_in_order ( &removed_partners_buffer () ?, &[
    "* (skg (node (id s)",
    "** (skg (subscribeeFolder changed))",
    "*** (skg (node (id s1) (repo main) writeProtected",
    "*** (skg (node (id s2) (repo main) (affectsParent false) writeProtected (diff rel-) (leaves 1 rel))) s2",
    "*** (skg (node (id s3) (repo main) writeProtected" ] );
  Ok (( )) }

#[test]
fn the_removed_partners_buffer_saves_back_unchanged (
) -> Result<(), Box<dyn Error>> {
  saves_back_unchanged ("removed_partners", &["main"], DiffViewBufferKind::AllChanges) }

/// In the 'historical_content' fixture h, which contained m, was
/// deleted: h's historical phantom draws its old content m as a
/// removed member, a non-member with '(diff rel-)'. A buffer showing
/// it saves back unchanged.
#[test]
fn a_historical_phantoms_old_content_is_drawn_as_removed_members (
) -> Result<(), Box<dyn Error>> {
  let mut buffer : String = String::new ();
  with_fixture ("historical_content", &["main"], |f| {
    buffer = buffer_of (f) ?;
    Ok (( )) }) ?;
  assert_lines_in_order ( &buffer, &[
    "** (skg (historical (id h) (repo main) (diff node- rel-) (leaves 2 node rel))) h",
    "*** (skg (node (id m) (repo main) (affectsParent false) writeProtected (diff rel-) (leaves 1 rel))) m, retitled" ] );
  saves_back_unchanged ("historical_content", &["main"], DiffViewBufferKind::AllChanges) }
