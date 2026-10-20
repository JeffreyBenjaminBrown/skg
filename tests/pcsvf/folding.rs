// Folding survives saves ('TODO/total-diff-view/04_plan-fixes.org',
// "Folding survives, because nothing that exists is recreated"): the
// server never changes the fold state of a viewnode that exists, and
// a viewnode whose kind changes keeps its fold state. The fixtures
// are described in 'buffer.rs' ('sets', 'kinds') and in
// 'other_buffers.rs' ('buffers').

use crate::git_fixture::{edit_line, save_cleanly};
use crate::pcsvf::common::{
  all_changes_view, open_all_changes_buffer, rerendered_view_content,
  retitle_in_a_content_view, send_stage_and_commit_request, with_fixture};

use skg::test_utils::skg_env_from_parts;
use skg::types::env::SkgEnv;

use std::error::Error;

/// 'buffer' with 'atom' put first in the metadata of its one line
/// starting 'prefix'.
fn with_atom (
  buffer : &str,
  prefix : &str,
  atom   : &str,
) -> String {
  edit_line ( buffer, prefix,
    |line| line . replacen ("(skg ", &format! ("(skg {} ", atom), 1 )) }

/// 'buffer' with 'lines' inserted after its one line starting
/// 'prefix'.
fn insert_after (
  buffer : &str,
  prefix : &str,
  lines  : &[&str],
) -> String {
  edit_line ( buffer, prefix,
    |line| std::iter::once (line) . chain ( lines . iter () . copied () )
           . collect::<Vec<&str>> () . join ("\n") ) }

/// The one line of 'buffer' starting 'prefix'.
fn line_starting<'a> (
  buffer : &'a str,
  prefix : &str,
) -> &'a str {
  let matching : Vec<&str> = buffer . lines ()
    . filter ( |line| line . starts_with (prefix) ) . collect ();
  assert_eq! ( matching . len (), 1,
               "exactly one line should start {:?}:\n{}", prefix, buffer );
  matching [0] }

/// A context node's body starts folded; once the client unfolds it
/// (the save carries no 'bodyFolded'), it stays open.
#[test]
fn a_context_body_unfolded_by_the_client_stays_open (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("sets", &["main"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    let unfolded : String = edit_line (
      &buffer, "** (skg bodyFolded (node (id cy1)",
      |line| line . replacen ("bodyFolded ", "", 1) );
    let saved : String = save_cleanly (f, &all_changes_view (), &unfolded) ? . saved_view;
    assert_eq! ( saved, unfolded, "cy1's body stays open" );
    assert_eq! ( save_cleanly (f, &all_changes_view (), &saved) ? . saved_view, saved, "and again" );
    Ok (( )) }) }

/// The folds of the 'kinds' buffer the tests put there: the alias
/// folder folded (its entries hidden), and txt's old text with its
/// body folded and its body diff hidden.
fn fold_kinds_buffer (
  buffer : &str,
) -> String {
  let mut folded : String = buffer . to_string ();
  for entry in [ "*** (skg removedAlias (diff rel-)) a1",
                 "*** (skg alias) a2",
                 "*** (skg alias (diff rel+)) a3",
                 "*** (skg bodyDiff)" ] {
    folded = with_atom (&folded, entry, "folded"); }
  with_atom (&folded, "** (skg (oldText titleAndBody)) txt", "bodyFolded") }

fn assert_kinds_folds_stay (
  saved : &str,
) {
  for line in [ "*** (skg folded removedAlias (diff rel-)) a1",
                "*** (skg folded alias) a2",
                "*** (skg folded alias (diff rel+)) a3",
                "** (skg bodyFolded (oldText titleAndBody)) txt",
                "*** (skg folded bodyDiff)" ] {
    assert! ( saved . lines () . any ( |l| l == line ),
              "{:?} must stay:\n{}", line, saved ); }}

/// A folded change folder and a folded old-text line stay folded
/// after a save that changes nothing (the buffer comes back
/// byte-identical, twice), and after one that adds a container above
/// them.
#[test]
fn folded_change_folders_and_old_text_stay_folded (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("kinds", &["public", "private"], |f| {
    let folded : String = fold_kinds_buffer ( &open_all_changes_buffer (f) ? );
    let saved : String = save_cleanly (f, &all_changes_view (), &folded) ? . saved_view;
    assert_eq! ( saved, folded, "saved unchanged, the buffer comes back as it was" );
    assert_eq! ( save_cleanly (f, &all_changes_view (), &saved) ? . saved_view, saved, "and again" );
    let contained : String = insert_after (
      &saved, "* (skg (node (id ct)", &[
        "** a new container",
        "*** (skg (node (id txt) (repo public) writeProtected)) txt, retitled",
        "*** (skg (node (id ali) (repo public) writeProtected)) ali" ] );
    let saved : String = save_cleanly (f, &all_changes_view (), &contained) ? . saved_view;
    assert! ( saved . contains ("a new container"), "{}", saved );
    assert_kinds_folds_stay (&saved);
    Ok (( )) }) }

/// A folded historical phantom (its old content hidden, its body
/// folded) stays folded after a save that changes nothing and after
/// one that adds a container above it.
#[test]
fn a_folded_historical_phantom_stays_folded (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("sets", &["main"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    let folded : String = with_atom (
      &with_atom ( &buffer, "*** (skg (historical (id delc)", "folded" ),
      "** (skg (historical (id del)", "bodyFolded" );
    let saved : String = save_cleanly (f, &all_changes_view (), &folded) ? . saved_view;
    assert_eq! ( saved, folded, "saved unchanged, the buffer comes back as it was" );
    let contained : String = insert_after (
      &saved, "* (skg (node (id ln)", &[
        "** a new container",
        "*** (skg (node (id r) (repo main) writeProtected)) r" ] );
    let saved : String = save_cleanly (f, &all_changes_view (), &contained) ? . saved_view;
    assert! ( saved . contains ("a new container"), "{}", saved );
    assert! ( line_starting (&saved, "** (skg bodyFolded (historical (id del)")
              . ends_with (" del"), "{}", saved );
    assert! ( line_starting (&saved, "*** (skg folded (historical (id delc)")
              . ends_with (" delc"), "{}", saved );
    Ok (( )) }) }

/// A folded headline inside a member role tree (top, under acq in pres's
/// changed container folder) stays folded after a save that changes
/// nothing and after one that adds a container above it.
#[test]
fn a_folded_headline_in_a_member_role_tree_stays_folded (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("buffers", &["public", "private"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    let memberRoleTree_headline : &str = "****** (skg (node (id top)";
    let folded : String = with_atom (&buffer, memberRoleTree_headline, "folded");
    let saved : String = save_cleanly (f, &all_changes_view (), &folded) ? . saved_view;
    assert_eq! ( saved, folded, "saved unchanged, the buffer comes back as it was" );
    let contained : String = insert_after (
      &saved, "* (skg (node (id txt)", &[
        "** a new container",
        "*** (skg (node (id top) (repo public) writeProtected)) top" ] );
    let saved : String = save_cleanly (f, &all_changes_view (), &contained) ? . saved_view;
    assert! ( saved . contains ("a new container"), "{}", saved );
    line_starting (&saved, "****** (skg folded (node (id top)");
    Ok (( )) }) }

/// A removed member keeps its fold state, and its place where the
/// LCS puts it (before ctx), across two saves.
#[test]
fn a_removed_member_keeps_its_fold_state_and_its_place (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("kinds", &["public", "private"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    let folded : String = with_atom (&buffer, "** (skg (node (id k1)", "folded");
    let mut saved : String = folded . clone ();
    for _ in 0 .. 2 {
      saved = save_cleanly (f, &all_changes_view (), &saved) ? . saved_view;
      let lines : Vec<&str> = saved . lines () . collect ();
      let k1 : usize = lines . iter ()
        . position ( |line| line . starts_with ("** (skg folded (node (id k1)") )
        . unwrap_or_else ( || panic! ("k1 stays org_folded:\n{}", saved) );
      assert! ( lines [k1 + 1] . starts_with ("** (skg (node (id ctx)"),
                "k1 stays before ctx:\n{}", saved ); }
    assert_eq! ( saved, folded );
    Ok (( )) }) }

/// A node a save deletes becomes a historical phantom, in place, whose
/// subtree keeps its folding; restored, it becomes a live member, in
/// place, whose subtree keeps its folding too.
#[test]
fn a_deleted_then_restored_node_keeps_the_folding_of_its_subtree (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("sets", &["main"], |f| {
    let buffer : String = open_all_changes_buffer (f) ?;
    let folded : String = with_atom (
      &edit_line ( &buffer, "** (skg bodyFolded (node (id cy1)",
        |line| line . replacen ("(node (id cy1)", "(node (id cy1) (editRequest delete)", 1) ),
      "*** (skg (node (id cy2)", "folded" );
    let deleted : String = save_cleanly (f, &all_changes_view (), &folded) ? . saved_view;
    assert! ( deleted . contains (
      "** (skg bodyFolded (historical (id cy1) (repo main) (diff node- rel-))) cy1\n\
       *** (skg folded (node (id cy2)" ),
      "cy1 is historical where it stood, its subtree folded as it was:\n{}", deleted );
    let restoring : String = edit_line (
      &deleted, "** (skg bodyFolded (historical (id cy1)",
      |_| "** (skg bodyFolded (node (id cy1) (repo main))) cy1" . to_string () );
    let restoring : String = edit_line (
      &restoring, "*** (skg folded (node (id cy2)",
      |_| "*** (skg folded (node (id cy2) (repo main) writeProtected)) cy2, retitled"
          . to_string () );
    let restored : String = save_cleanly (f, &all_changes_view (), &restoring) ? . saved_view;
    assert! ( restored . contains ("** (skg bodyFolded (node (id cy1)")
              && restored . contains ("*** (skg folded (node (id cy2)")
              && ! restored . contains ("historical (id cy1)"),
              "cy1 is a live member again, its subtree folded as it was:\n{}",
              restored );
    Ok (( )) }) }

/// The folds the 'buffers' buffer gets: mvp (a root), mvc (its
/// member) and acq (top's member) folded, and mvs's body, which the
/// server draws folded, unfolded.
fn fold_buffers_buffer (
  buffer : &str,
) -> String {
  let mut folded : String = buffer . to_string ();
  for headline in [ "* (skg (node (id mvp)",
                    "** (skg (node (id mvc)",
                    "** (skg (node (id acq)" ] {
    folded = with_atom (&folded, headline, "folded"); }
  edit_line ( &folded, "** (skg bodyFolded (node (id mvs)",
              |line| line . replacen ("bodyFolded ", "", 1) ) }

fn assert_buffers_folds_stay (
  buffer : &str,
) {
  for (folded, unfolded) in [
    ( "* (skg folded (node (id mvp)",   "* (skg (node (id mvp)" ),
    ( "** (skg folded (node (id mvc)",  "** (skg (node (id mvc)" ),
    ( "** (skg folded (node (id acq)",  "** (skg (node (id acq)" ),
    ( "** (skg (node (id mvs)",         "** (skg bodyFolded (node (id mvs)" ) ] {
    assert! ( buffer . lines () . any ( |line| line . starts_with (folded) )
              && ! buffer . lines () . any ( |line| line . starts_with (unfolded) ),
              "{:?} must stay:\n{}", folded, buffer ); }}

/// Folds survive a git-commit: the 'buffers' buffer, saved with its
/// folds ('fold_buffers_buffer'), is rerendered by a confirmed
/// stage-and-commit, and the headlines folded before are folded after.
#[test]
fn folds_survive_a_commit (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("buffers", &["public", "private"], |f| {
    let folded : String = fold_buffers_buffer ( &open_all_changes_buffer (f) ? );
    assert_buffers_folds_stay ( &save_cleanly (f, &all_changes_view (), &folded) ? . saved_view );
    let env : SkgEnv = skg_env_from_parts (&f . config, &f . tantivy, &f . graph);
    let response : String = send_stage_and_commit_request ( f, &env,
      "((request . \"stage and commit\") (message . \"commit\") (confirmed . t))\n" ) ?;
    assert! ( response . contains ("(commits ((public "), "{}", response );
    let committed : String = rerendered_view_content (&response);
    assert! ( ! committed . contains ("homeSkgrepoMove"),
              "the buffer shows no change after the commit:\n{}", committed );
    assert_buffers_folds_stay (&committed);
    Ok (( )) }) }

/// Folds survive a collateral rerender: the 'buffers' buffer, saved
/// with its folds, is rerendered when a content view retitles txt,
/// and shows the new title with the same folds.
#[test]
fn folds_survive_a_collateral_rerender (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("buffers", &["public", "private"], |f| {
    let folded : String = fold_buffers_buffer ( &open_all_changes_buffer (f) ? );
    let saved : String = save_cleanly (f, &all_changes_view (), &folded) ? . saved_view;
    let rerendered : String =
      retitle_in_a_content_view (f, "txt", "txt, retitled again", &saved) ?;
    assert! ( line_starting (&rerendered, "* (skg (node (id txt)")
                . ends_with (" txt, retitled again"),
              "the buffer was rerendered collaterally:\n{}", rerendered );
    assert_buffers_folds_stay (&rerendered);
    Ok (( )) }) }
