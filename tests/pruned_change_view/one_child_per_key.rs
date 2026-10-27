// The refresh of the diff view matches by key among all the
// children of a node, whoever drew them: a child of a key its goal
// has stands for that item, and siblings of one key are joined by a
// viewnode merge ('docs/diff-view.org', "What the refresh keeps"). Each case below once drew a second child of one key.
//
// Fixtures: 'requested_alias_folder' (x, with the alias xa1, its body
// changed); 'graft_beside_removed_member' (x contains r, r's body
// changed); 'copy_beside_removed_member' (p contains y, p's body
// changed); 'users_notes' (see 'users_notes.rs').

use crate::pruned_change_view::common::{
  edit_in_a_content_view, insert_after_line_starting, open_all_changes_buffer,
  rerendered_view_content, save, send_stage_and_commit_request, with_fixture};
use crate::git_fixture::{Fixture, edit_line, skg_files};
use crate::git_fixture_helpers::commit_all;

use skg::metadata_canon::without_viewnode_ids;
use skg::serve::handlers::save_buffer::SaveResponse;
use skg::test_utils::skg_env_from_parts;
use skg::types::env::SkgEnv;

use git2::Repository;
use std::collections::BTreeMap;
use std::error::Error;
use std::fs;
use std::path::PathBuf;

const ROOT_X : &str = "* (skg node (gnid x)";
const REQUESTED_ALIASES : &str = "(skgRepos (nodeRepo main)) (viewRequests (folder aliases))";

/// The child headlines, without their viewnode IDs, of the one line
/// of 'buffer' that starts, viewnode ID aside, with 'parent'.
fn children_of (
  buffer : &str,
  parent : &str,
) -> Vec<String> {
  let depth = |line : &str| line . chars () . take_while ( |c| *c == '*' ) . count ();
  let headlines : Vec<String> = buffer . lines ()
    . filter ( |line| line . starts_with ('*') )
    . map (without_viewnode_ids) . collect ();
  let parents : Vec<usize> = (0 .. headlines . len ())
    . filter ( |&i| headlines [i] . starts_with (parent) ) . collect ();
  assert_eq! ( parents . len (), 1, "one line starts {:?} in\n{}", parent, buffer );
  let parent_depth : usize = depth (&headlines [ parents [0] ]);
  headlines [ parents [0] + 1 .. ] . iter ()
    . take_while ( |line| depth (line) > parent_depth )
    . filter ( |line| depth (line) == parent_depth + 1 )
    . cloned () . collect () }

/// Panic unless exactly one child of 'parent' in 'buffer' contains
/// 'key'.
fn assert_one_child_with (
  buffer : &str,
  parent : &str,
  key    : &str,
) {
  let matching : usize = children_of (buffer, parent) . iter ()
    . filter ( |child| without_viewnode_ids (child) . contains (key) ) . count ();
  assert_eq! ( matching, 1,
               "one child of {:?} should contain {:?}, in\n{}", parent, key, buffer ); }

/// Saved unchanged, 'buffer' reports no error and no warning, changes
/// no file, and is refreshed to itself.
fn assert_saves_back_cleanly (
  f      : &mut Fixture,
  buffer : &str,
) -> Result<(), Box<dyn Error>> {
  let files_before : BTreeMap<String, String> = skg_files (f) ?;
  let response : SaveResponse = save (f, buffer) ?;
  assert! ( response . errors . is_empty () && response . warnings . is_empty (),
            "an unchanged save must be clean, got {:?} and {:?}\n{}",
            response . errors, response . warnings, buffer );
  assert_eq! ( skg_files (f) ?, files_before, "{}", buffer );
  assert_eq! ( response . saved_view, buffer, "the save must give the buffer back" );
  Ok (( )) }

/// Save 'buffer'; it must save without errors. Returns what it rendered.
fn saved (
  f      : &mut Fixture,
  buffer : &str,
) -> Result<String, Box<dyn Error>> {
  let response : SaveResponse = save (f, buffer) ?;
  assert! ( response . errors . is_empty (),
            "the save must succeed, got {:?}\n{}", response . errors, buffer );
  Ok (response . saved_view) }

/// Make HEAD of the gitrepo 'main' hold 'head_text' as the node file
/// 'node', leaving that file in the worktree as it was.
fn rewrite_head (
  f         : &Fixture,
  node      : &str,
  head_text : &str,
) -> Result<(), Box<dyn Error>> {
  let gitrepo_path : PathBuf = f . root . path () . join ("main");
  let path : PathBuf = gitrepo_path . join ( format! ("{}.skg", node) );
  let worktree_text : String = fs::read_to_string (&path) ?;
  fs::write (&path, head_text) ?;
  commit_all ( &Repository::open (&gitrepo_path) ?, "rewrite HEAD" );
  fs::write (&path, &worktree_text) ?;
  Ok (( )) }

/// An alias folder the user requested, then changed: it becomes the
/// changed folder, and no second folder is drawn beside it.
#[test]
fn a_requested_alias_folder_becomes_the_changed_one (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("requested_alias_folder", &["main"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    let requested : String = saved ( f, &edit_line ( &buffer, ROOT_X, |line|
      line . replacen ("(skgRepos (nodeRepo main))", REQUESTED_ALIASES, 1) )) ?;
    assert_one_child_with (&requested, ROOT_X, "aliasFolder");
    let changed : String = saved ( f, &insert_after_line_starting (
      &requested, "** (skg aliasFolder", "*** (skg alias) xa2" )) ?;
    assert_one_child_with (&changed, ROOT_X, "aliasFolder");
    assert_eq! ( children_of (&changed, "** (skg aliasFolder"), vec! [
      "*** (skg alias (diff (rel added) (leaves 1))) xa2" . to_string (),
      "*** (skg alias) xa1" . to_string () ],
      "{}", changed );
    assert_saves_back_cleanly (f, &changed) }) }

/// An alias folder the user requested is refreshed when its aliases
/// change elsewhere, as the changed folder, a lost alias marked so;
/// once a commit ends the change it stays, unmarked, holding the
/// aliases as they are and, unwarranted and claiming nothing, the
/// alias the commit made lost for good.
#[test]
fn a_requested_alias_folder_is_refreshed_and_kept (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("requested_alias_folder", &["main"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    let requested : String = saved ( f, &edit_line ( &buffer, ROOT_X, |line|
      line . replacen ("(skgRepos (nodeRepo main))", REQUESTED_ALIASES, 1) )) ?;
    let collateral : String = edit_in_a_content_view ( f, "x", &requested,
      |text : &str| format! ( "{}** (skg aliasFolder)\n*** (skg alias) xa2\n", text )) ?;
    assert_eq! ( children_of (&collateral, "** (skg aliasFolder"), vec! [
      "*** (skg alias noClaim (view ghostWarrant) (diff (rel removed) (leaves 1))) xa1" . to_string (),
      "*** (skg alias (diff (rel added) (leaves 1))) xa2" . to_string () ],
      "{}", collateral );
    assert_one_child_with (&collateral, ROOT_X, "aliasFolder");
    assert_saves_back_cleanly (f, &collateral) ?;
    let env : SkgEnv = skg_env_from_parts (&f . config, &f . tantivy, &f . graph);
    let response : String = send_stage_and_commit_request ( f, &env,
      "((request . \"stage and commit\") (message . \"commit\") (confirmed . t))\n" ) ?;
    let committed : String = rerendered_view_content (&response);
    assert_eq! ( children_of (&committed, ROOT_X),
                 vec! [ "** (skg aliasFolder)" . to_string () ], "{}", committed );
    assert_eq! ( children_of (&committed, "** (skg aliasFolder)"),
                 vec! [ "*** (skg alias noClaim (view noWarrant)) xa1" . to_string (),
                        "*** (skg alias) xa2" . to_string () ], "{}", committed );
    assert_saves_back_cleanly (f, &committed) }) }

/// A write-protected x claiming nothing, which the user put under r, when
/// HEAD comes to say that r contained x, is adopted as the removed
/// member: x stands under r once.
#[test]
fn a_note_of_a_node_becomes_its_removed_member (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("graft_beside_removed_member", &["main"], |f| {
    let member_r : &str = "** (skg node (gnid r)";
    let buffer : String = open_all_changes_buffer (f) ?;
    let noted : String = { // x typed as r's first child, after r's body
      let mut lines : Vec<&str> = buffer . lines () . collect ();
      let r_index : usize = lines . iter ()
        . position ( |line| without_viewnode_ids (line) . starts_with (member_r) )
        . expect ("r is shown");
      let after_r_body : usize = r_index + 1 + lines [ r_index + 1 .. ] . iter ()
        . take_while ( |line| ! line . starts_with ('*') ) . count ();
      lines . insert ( after_r_body, "*** (skg node (gnid x) noClaim writeProtected) x" );
      saved ( f, &( lines . join ("\n") + "\n" )) ? };
    assert_one_child_with (&noted, member_r, "node (gnid x) (skgRepos (nodeRepo main)) noClaim writeProtected");
    rewrite_head (f, "r", "pid: r\ntitle: r\nbody: old\ncontains:\n- x\n") ?;
    let after_head : String = saved (f, &noted) ?;
    assert_one_child_with (&after_head, member_r, "node (gnid x)");
    assert_one_child_with (&after_head, member_r, "(diff (rel removed) (leaves 1 rel))");
    assert_saves_back_cleanly (f, &after_head) }) }

/// A write-protected copy of y that the user pasted beside the member
/// y merges into it (siblings standing for one node are unified), so
/// when p then drops y elsewhere, y stands under p once, as the
/// removed member.
#[test]
fn a_pasted_copy_beside_a_removed_member_is_not_doubled (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("copy_beside_removed_member", &["main"], |f| {
    let root_p : &str = "* (skg node (gnid p)";
    let buffer : String = open_all_changes_buffer (f) ?;
    let pasted : String = saved ( f, &format! (
      "{}** (skg node (gnid y) noClaim writeProtected) y\n", buffer )) ?;
    assert_eq! ( children_of (&pasted, root_p) . iter ()
                 . filter ( |child| child . contains ("node (gnid y)") ) . count (),
                 1, "the copy merges into the member: {}", pasted );
    let collateral : String = edit_in_a_content_view ( f, "p", &pasted,
      |text : &str| text . lines ()
        . filter ( |line| ! line . contains ("node (gnid y)") )
        . collect::<Vec<&str>> () . join ("\n") + "\n" ) ?;
    assert_one_child_with (&collateral, root_p, "node (gnid y)");
    assert_one_child_with (&collateral, root_p, "(diff (rel removed) (leaves 1 rel))");
    assert_saves_back_cleanly (f, &collateral) }) }

/// A copy the user pasted of a write-protected graft in a member role
/// tree is a duplicate of it, and goes.
#[test]
fn copies_of_a_write_protected_graft_merge (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("users_notes", &["main"], |f| {
    let member_a : &str = "**** (skg node (gnid a) (skgRepos (nodeRepo main)) writeProtected";
    let graft_p  : &str =
      "***** (skg node (gnid p) (skgRepos (nodeRepo main)) writeProtected";
    let buffer : String = open_all_changes_buffer (f) ?;
    let graft_line : String = buffer . lines ()
      . find ( |line| without_viewnode_ids (line) . starts_with (graft_p) )
      . unwrap_or_else ( || panic! ( "no graft of p in\n{}", buffer )) . to_string ();
    let copied : String = insert_after_line_starting ( &buffer, graft_p,
      &without_viewnode_ids (&graft_line) );
    assert_eq! ( children_of (&copied, member_a) . iter ()
                 . filter ( |child| without_viewnode_ids (child) . starts_with (graft_p) ) . count (),
                 2, "{}", copied );
    let merged : String = saved (f, &copied) ?;
    assert_one_child_with (&merged, member_a, "node (gnid p)");
    assert_saves_back_cleanly (f, &merged) }) }
