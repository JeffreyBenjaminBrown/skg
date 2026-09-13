// cargo nextest run --test grouped_saves -E 'test(save::indefinitive_edits::)'

use indoc::indoc;
use skg::dbs::filesystem::one_node::nodecomplete_from_id;
use skg::dbs::in_rust_graph::InRustGraphHandle;
use skg::from_text::buffer_to_viewnodes::uninterpreted
  ::org_to_uninterpreted_viewforest;
use skg::test_utils::{graph_handle_from_config, run_with_test_stores};
use skg::test_utils::update_from_and_rerender_buffer_test
  as update_from_and_rerender_buffer;
use skg::serve::ViewsState;
use skg::types::errors::{BufferValidationError, SaveError};
use skg::types::misc::{members_of, ID, SkgConfig, TantivyIndex};
use skg::types::nodes::complete::NodeComplete;
use skg::types::views_state::{OpenViews, ViewUri};

use std::error::Error;
use std::net::TcpStream;

#[test]
fn whitespace_only_body_under_indefinitive_is_not_an_edit () {
  let input : &str =
    "* (skg (node (id shown) (source main) indef)) shown\n\n";
  let (_viewforest, parsing_errors, _warnings) =
    org_to_uninterpreted_viewforest (input) . unwrap ();
  assert! ( ! parsing_errors . iter () . any ( |error| matches! (
    error, BufferValidationError::EditedIndefinitive (_)) ),
    "Blank separator lines do not edit an indefinitive occurrence: {:?}",
    parsing_errors );
}

#[test]
fn saving_an_edited_indefinitive_occurrence_is_rejected ()
  -> Result<(), Box<dyn Error>> {
  run_with_test_stores (
    "skg-test-indefinitive-edits",
    "tests/save/birth_and_indefinitive/fixtures",
    "/tmp/tantivy-test-indefinitive-edits",
    |config, tantivy| Box::pin (async move {
      saving_an_edited_indefinitive_occurrence_impl (config, tantivy) . await
    })) }

async fn saving_an_edited_indefinitive_occurrence_impl (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let graph : InRustGraphHandle = graph_handle_from_config (config)?;
  let mut views_state : ViewsState = ViewsState {
    diff_mode_enabled : false,
    open_views        : OpenViews::new (), };
  let listener : std::net::TcpListener =
    std::net::TcpListener::bind ("127.0.0.1:0")?;
  let mut stream : TcpStream =
    TcpStream::connect (listener . local_addr ()?)?;
  let uri : Result<ViewUri, String> = Ok (
    ViewUri::ContentView ("indefinitive-edits-test" . to_string ()));
  let rendered = indoc! {"
    * (skg (node (id 1) (source main))) 1
    ** (skg (node (id 2) (source main) indef)) 2
  "};
  let first = update_from_and_rerender_buffer (
    &mut stream, rendered, config, tantivy, &graph, false,
    &uri, &mut views_state ) . await ?;

  // A second, newly inserted occurrence of node 1 is not an edit to a
  // previously rendered indefinitive occurrence. It may therefore express
  // the root's new self-content relationship.
  let with_new_self_occurrence : String = first . saved_view . replacen (
    '\n',
    "\n** (skg (node (id 1) (source main) indef)) new self occurrence\n",
    1);
  let second = update_from_and_rerender_buffer (
    &mut stream, &with_new_self_occurrence, config, tantivy, &graph, false,
    &uri, &mut views_state ) . await ?;
  assert! (second . errors . is_empty (),
    "the new self occurrence should be accepted: {:?}", second . errors);
  let saved_root : NodeComplete =
    nodecomplete_from_id (config, &ID::from ("1"))?;
  assert! (members_of (&saved_root . contains) . contains (&ID::from ("1")),
    "the accepted new occurrence should make node 1 content of itself");

  // The old occurrence of node 2 is still protected at its rendered location.
  let old_line : &str = second . saved_view . lines ()
    . find ( |line| line . contains ("(id 2)") )
    . expect ("the rendered view should still contain node 2");
  let (metadata, _title) = old_line . rsplit_once (") ")
    . expect ("the rendered node should have a title");
  let new_line : String = format! ("{}) silently changed", metadata);
  let edited : String = second . saved_view . replace (old_line, &new_line);
  let result = update_from_and_rerender_buffer (
    &mut stream, &edited, config, tantivy, &graph, false,
    &uri, &mut views_state ) . await;
  let error = match result {
    Err (error) => error,
    Ok (_) => panic! (
      "editing an open view's indefinitive occurrence must fail"), };
  let save_error = error . downcast_ref::<SaveError> ()
    . expect ("the rejection should be a SaveError");
  let SaveError::BufferValidationErrors { errors, .. } = save_error else {
    panic! ("expected BufferValidationErrors, got {:?}", save_error); };
  assert_eq! (errors,
    &vec! [BufferValidationError::EditedIndefinitive ("2" . into ())]);
  Ok (( ))
}
