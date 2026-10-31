// cargo nextest run --test grouped_repos -E 'test(editability_carrier::)'
//
// The editable-view request as the carrier of editability
// ('server/refresh/editability.rs'): a member the refresh draws under
// an editable occurrence carries the request, so it is drawn
// editable, with the default folders an editable occurrence shows. A
// save's collateral rerender draws such a member.
//
// The fixture, written by the test, one skgrepo "main": p contains a;
// s subscribes to e.

use crate::git_fixture::{
  OwnedSkgrepos, open_content_view, save_cleanly, with_git_fixture,
  write_unchanged_skgrepos};

use skg::org_to_text::viewforest_to_string;
use skg::skgrepo_sets::SkgrepoRestriction;
use skg::types::tree::forest::ViewForest;
use skg::types::views_state::ViewforestId;

use std::error::Error;

/// p contains a; s subscribes to e.
fn write_fixture (
  dir : &std::path::Path,
) -> Result<(), Box<dyn Error>> {
  write_unchanged_skgrepos (dir, &[ ( "main", &[
    ("p", "pid: p\ntitle: p\ncontains:\n- a\n"),
    ("a", "pid: a\ntitle: a\n"),
    ("s", "pid: s\ntitle: s\nsubscribesTo:\n- e\n"),
    ("e", "pid: e\ntitle: e\n"), ] ) ]) }

/// Two views of p. A save of the first adds s to p's contents; the
/// collateral rerender of the second draws s under its editable p,
/// carrying the editable-view request, so s is editable and shows its
/// subscribee folder, as a fresh view's members do.
#[test]
fn a_member_a_save_draws_shows_its_default_subscribee_folder (
) -> Result<(), Box<dyn Error>> {
  let case : tempfile::TempDir = tempfile::TempDir::new () ?;
  write_fixture (case . path ()) ?;
  with_git_fixture (
    case . path (), OwnedSkgrepos::Named (&["main"]), &[],
    |f| {
      let unrestricted : SkgrepoRestriction =
        SkgrepoRestriction::unrestricted (&f . config);
      let first : String = open_content_view (f, "p", "first", &unrestricted) ?;
      open_content_view (f, "p", "second", &unrestricted) ?;
      let edited : String = first . replacen (
        " a\n", " a\n** (skg node (gnid s) (skgRepos (nodeRepo main))) s\n", 1 );
      assert_ne! ( edited, first, "s typed under p" );
      save_cleanly (f, &ViewforestId::ContentView ("first" . into ()), &edited) ?;
      let second : &ViewForest = f . views_state . open_views
        . viewforest_id_to_view ( &ViewforestId::ContentView ("second" . into ()) )
        . expect ("the second view stays registered");
      let text : String = viewforest_to_string (second) ?;
      let s_line : &str = text . lines ()
        . find ( |line| line . contains ("(gnid s)") )
        . unwrap_or_else ( || panic! ("the rerender draws s: {}", text) );
      assert! ( ! s_line . contains ("writeProtected"),
                "s is drawn editable: {}", text );
      let after_s : Vec<&str> = text . lines ()
        . skip_while ( |line| ! line . contains ("(gnid s)") )
        . collect ();
      assert! ( after_s . get (1) . is_some_and ( |line|
                  line . starts_with ("*** ") && line . contains ("subscribeeFolder") )
                && after_s . get (2) . is_some_and ( |line|
                  line . starts_with ("**** ") && line . contains ("(gnid e)") ),
                "s shows its subscribee folder, holding e: {}", text );
      Ok (( )) } ) }
