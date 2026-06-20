/// The round-trip invariant: any buffer the server renders in diff mode must
/// pass save-validation. Each test renders a diff view, checks the move/removal
/// is shown, then saves the rendered buffer and asserts no validation errors.

use super::common::*;
use std::sync::Arc;
use skg::test_utils::{run_with_shared_test_db, SharedDbSession};

#[test]
fn all_tests
  () -> Result<(), Box<dyn Error>> {
  run_with_shared_test_db (
    "skg-test-git-diff-roundtrip",
    |s| Box::pin ( async move {
      reorder_within_parent_shows_move_and_roundtrips (s) . await ?;
      dangling_at_head_member_roundtrips (s) . await ?;
      Ok (( )) } )) }

/// A within-parent reorder must render the moved member at BOTH slots -- a
/// 'removedM' phantom (old slot) and a 'newM' live child (new slot), git-style
/// -- and the rendered buffer must save with no validation errors. Regression:
/// the old slot used to come out 'newM', re-parse as a second live vognode, and
/// trip the content-child uniqueness check.
async fn reorder_within_parent_shows_move_and_roundtrips (
  s : &mut SharedDbSession,
) -> Result<(), Box<dyn Error>>
{
  let temp_dir = TempDir::new()?;
  let repo_path = temp_dir . path();
  setup_git_repo_with_reorder_fixtures (repo_path)?;
  s . reset_with_source_path ( "roundtrip_reorder", repo_path ) . await ?;
  let (config, driver, tantivy)
    : (&SkgConfig, &Arc<TypeDBDriver>, &TantivyIndex)
    = (&s . config, &s . driver, &s . tantivy);

  let root_ids = vec![ ID ("parent" . to_string ()) ];
  let (actual, _pids, _) : (String, Vec<ID>, _) =
    multi_root_view (&driver, &config, None, &root_ids, true) . await ?;

  // The moved member is drawn at BOTH slots: 'removedM' at its old slot and
  // 'newM' at its new slot (a git-style move). Which member git's LCS treats as
  // "moved" is not contractual, so identify it by the axes, not by name.
  let removedm_line : &str = actual . lines ()
    . find ( |l| l . contains ("removedM") )
    . unwrap_or_else ( || panic! ("expected a removedM old-slot phantom:\n{actual}") );
  let newm_line : &str = actual . lines ()
    . find ( |l| l . contains ("newM") )
    . unwrap_or_else ( || panic! ("expected a newM new-slot child:\n{actual}") );
  let id_of = | line : &str | -> String {
    line . split ("(id ") . nth (1) . unwrap ()
      . split ( |c| c == ')' || c == ' ' ) . next () . unwrap () . to_string () };
  assert_eq! ( id_of (removedm_line), id_of (newm_line),
    "the removedM (old slot) and newM (new slot) must be the same moved member:\n{actual}" );

  assert_diff_buffer_roundtrips ( &actual, driver, config, tantivy ) . await ?;
  Ok (( )) }

/// A contains member referenced at HEAD whose .skg file exists in no source
/// renders as a removedM phantom carrying the NOT_FOUND source sentinel; that
/// buffer must save with no validation errors. Regression: validate_phantom
/// used to reject a phantom whose source is not in the config.
async fn dangling_at_head_member_roundtrips (
  s : &mut SharedDbSession,
) -> Result<(), Box<dyn Error>>
{
  let temp_dir = TempDir::new()?;
  let repo_path = temp_dir . path();
  setup_git_repo_with_dangling_fixtures (repo_path)?;
  s . reset_with_source_path ( "roundtrip_dangling", repo_path ) . await ?;
  let (config, driver, tantivy)
    : (&SkgConfig, &Arc<TypeDBDriver>, &TantivyIndex)
    = (&s . config, &s . driver, &s . tantivy);

  let root_ids = vec![ ID ("parent" . to_string ()) ];
  let (actual, _pids, _) : (String, Vec<ID>, _) =
    multi_root_view (&driver, &config, None, &root_ids, true) . await ?;

  assert! (
    actual . contains ("(id ghost)") && actual . contains ("NOT_FOUND"),
    "the dangling member should render as a NOT_FOUND phantom:\n{actual}" );

  assert_diff_buffer_roundtrips ( &actual, driver, config, tantivy ) . await ?;
  Ok (( )) }

/// Save the rendered diff buffer (no user edit) and assert it produced no
/// validation errors -- the round-trip invariant.
async fn assert_diff_buffer_roundtrips (
  buffer  : &str,
  driver  : &Arc<TypeDBDriver>,
  config  : &SkgConfig,
  tantivy : &TantivyIndex,
) -> Result<(), Box<dyn Error>>
{
  let graph : InRustGraphHandle = new_handle (InRustGraph::new ());
  let mut views_state : ViewsState = ViewsState {
    diff_mode_enabled : true,
    open_views        : OpenViews::new (), };
  let (mut stream, _) = mk_test_tcp_stream_pair ();
  let response = update_from_and_rerender_buffer (
    &mut stream,
    buffer, driver, config, tantivy, &graph, true,
    &Err ( String::new () ), &mut views_state ) . await ?;
  assert! ( response . errors . is_empty (),
    "a rendered diff buffer must save without validation errors, got {:?}\n\nbuffer:\n{}",
    response . errors, buffer );
  Ok (( )) }
