// cargo nextest run --test grouped_repos -E 'test(users_graph_heralds::)'
//
// The in-sides of contains and hides say when a note of the user's
// graph is among their members ('usersGraph' in
// 'server/herald_tokens.rs'; 'docs/heralds.org', "Your notes"): on
// contains, one but the nearest node occurrence above; on hides, any.
//
// The fixture has the owned skgrepo "mine" and the foreign one
// "theirs". theirs's mailbox contains m1 to m5. mine's fork of the
// mailbox hides m1 and contains m5; mine's filing contains m2. theirs's
// other contains m3 and hides m4.

use crate::git_fixture::{
  Fixture, OwnedSkgrepos, open_content_view, with_git_fixture,
  write_unchanged_skgrepos};

use skg::skgrepo_sets::SkgrepoRestriction;
use skg::metadata_canon::without_viewnode_ids;

use std::error::Error;
use tempfile::TempDir;

fn with_mine_and_theirs (
  body : impl FnOnce (&mut Fixture) -> Result<(), Box<dyn Error>>,
) -> Result<(), Box<dyn Error>> {
  let case : TempDir = TempDir::new () ?;
  write_unchanged_skgrepos ( case . path (), &[
    ("mine", &[
      ("fork", "pid: fork\ntitle: my fork of the mailbox\ncontains:\n- m5\nhides:\n- m1\n"),
      ("filing", "pid: filing\ntitle: filing\ncontains:\n- m2\n") ]),
    ("theirs", &[
      ("mailbox", "pid: mailbox\ntitle: mailbox\ncontains:\n- m1\n- m2\n- m3\n- m4\n- m5\n"),
      ("other", "pid: other\ntitle: other\ncontains:\n- m3\nhides:\n- m4\n"),
      ("m1", "pid: m1\ntitle: m1\n"),
      ("m2", "pid: m2\ntitle: m2\n"),
      ("m3", "pid: m3\ntitle: m3\n"),
      ("m4", "pid: m4\ntitle: m4\n"),
      ("m5", "pid: m5\ntitle: m5\n") ]) ] ) ?;
  with_git_fixture (
    case . path (), OwnedSkgrepos::Named (&["mine"]), &["theirs"], body ) }

/// The line of the view that draws 'gnid', viewnode IDs aside.
fn line_of (
  view : &str,
  gnid : &str,
) -> String {
  view . lines ()
    . find ( |line| line . contains (&format! ("(gnid {})", gnid)) )
    . unwrap_or_else ( || panic! ("no line draws {}:\n{}", gnid, view) )
    . to_string () }

/// Under a foreign mailbox, a message an owned note hides, or one an
/// owned note also contains, says so; one a foreign note hides or
/// also contains does not.
#[test]
fn under_a_foreign_root_the_users_hides_and_filings_say_so (
) -> Result<(), Box<dyn Error>> {
  with_mine_and_theirs ( |f| {
    let restriction : SkgrepoRestriction =
      SkgrepoRestriction::unrestricted (&f . config);
    let view : String = without_viewnode_ids (
      &open_content_view (f, "mailbox", "mailbox", &restriction) ?);
    for (gnid, rels) in [
      ("m1", "(rels (contains (in 1 (ancestors 1))) (hides (in 1 usersGraph)))"),
      ("m2", "(rels (contains (in 2 (ancestors 1) usersGraph)))"),
      ("m3", "(rels (contains (in 2 (ancestors 1))))"),
      ("m4", "(rels (contains (in 1 (ancestors 1))) (hides (in 1)))"),
      ("m5", "(rels (contains (in 2 (ancestors 1) usersGraph)))") ] {
      assert! ( line_of (&view, gnid) . contains (rels),
                "{} reads {}:\n{}", gnid, rels, view ); }
    Ok (( )) } ) }

/// The note a message is drawn under does not count, though the user
/// owns it: under the fork, m5 is contained only by the fork among the
/// user's notes, and says nothing of them; m2, under the filing, the
/// same.
#[test]
fn the_note_it_is_drawn_under_is_no_other_filing (
) -> Result<(), Box<dyn Error>> {
  with_mine_and_theirs ( |f| {
    let restriction : SkgrepoRestriction =
      SkgrepoRestriction::unrestricted (&f . config);
    let fork_view : String = without_viewnode_ids (
      &open_content_view (f, "fork", "fork", &restriction) ?);
    assert! ( line_of (&fork_view, "m5")
              . contains ("(rels (contains (in 2 (ancestors 1))))"),
              "{}", fork_view );
    let filing_view : String = without_viewnode_ids (
      &open_content_view (f, "filing", "filing", &restriction) ?);
    assert! ( line_of (&filing_view, "m2")
              . contains ("(rels (contains (in 2 (ancestors 1))))"),
              "{}", filing_view );
    Ok (( )) } ) }
