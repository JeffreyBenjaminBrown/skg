// cargo nextest run --test grouped_overrides -E 'test(partner_folder_warnings::)'
//
// When the completion pass repairs a write-protected PartnerFolder in the
// view the user just saved, the save succeeds and
// SaveResponse.warnings says what was repaired
// ('docs/sharing-model.org', "The write-protected folders: who
// subscribes to, overrides, or hides this node"):
// - a deleted generated member is reinstated, with a warning that
//   explains membership is edited from the other side;
// - a child placed as a member with a subtree, though not
//   a member, is retained as non-member, with a warning.
//
// Fixture: r and t subscribe to n; the test explicitly asks for n's
// subscription folders. x is an unrelated node the test places inside
// that folder.

use std::error::Error;
use std::net::TcpStream;

use skg::test_utils::{run_with_shared_test_stores, graph_handle_from_config};
use skg::test_utils::update_from_and_rerender_buffer_test as update_from_and_rerender_buffer;
use skg::to_org::render::content_view::multi_root_view;
use skg::serve::ViewsState;
use skg::types::views_state::{OpenViews, ViewforestId};
use skg::types::misc::{ID, SkgConfig, TantivyIndex};
use skg::serve::handlers::save_buffer::SaveResponse;
use skg::types::errors::{SaveError, BufferValidationError};
use skg::metadata_canon::viewnode_id_atom_of;

use skg::dbs::in_rust_graph::InRustGraphHandle;

#[test]
fn all_tests
  () -> Result<(), Box<dyn Error>> {
  let fixtures : &str = "tests/partner_folder_warnings/fixtures";
  run_with_shared_test_stores (
    "skg-test-partner-folder-warnings",
    |s| Box::pin ( async move {
      s . reset ("write_protected_folder_repairs_warn", fixtures) ?;
      write_protected_folder_repairs_warn_impl (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("failed_save_carries_warnings_with_errors", fixtures) ?;
      failed_save_carries_warnings_with_errors (
        &s . config, &mut s . tantivy ) . await ?;
      Ok (( )) } )) }

/// Save 'buf' as the view of n. The save registers the view in
/// 'views_state', so a later save there knows the viewnode IDs this
/// one rendered.
async fn save_buffer (
  buf         : &str,
  config      : &SkgConfig,
  tantivy     : &mut TantivyIndex,
  graph       : &InRustGraphHandle, // must be the explicit handle, or the save's coherence debug-assert reads a stale graph
  views_state : &mut ViewsState,
) -> Result<SaveResponse, Box<dyn Error>> {
  let listener : std::net::TcpListener =
    std::net::TcpListener::bind ("127.0.0.1:0") . unwrap ();
  let mut stream : TcpStream =
    TcpStream::connect (listener . local_addr () . unwrap ()) . unwrap ();
  update_from_and_rerender_buffer (
    &mut stream,
    buf, config, tantivy, graph,
    &Ok ( ViewforestId::ContentView ("view of n" . to_string ()) ),
    views_state ) . await }

fn fresh_views_state () -> ViewsState {
  ViewsState { open_views     : OpenViews::new (),
               graph_straddle : None } }

fn line_containing<'a> (
  buf      : &'a str,
  fragment : &str,
) -> &'a str {
  buf . lines ()
    . find ( |l| l . contains (fragment) )
    . unwrap_or_else (
      || panic! ( "no line contains {:?} in:\n{}", fragment, buf )) }

async fn write_protected_folder_repairs_warn_impl (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let graph : InRustGraphHandle =
    (
      graph_handle_from_config (config) ? );
  let (initial_buffer, _pids, _tree)
    : (String, Vec<ID>, _) =
    multi_root_view (
      config, Some (tantivy),
      &[ ID ("n" . to_string ()) ] ) ?;
  let folder_request : String = initial_buffer . replacen (
    "node ",
    "node (viewRequests (folder subscribesTo))", 1 );
  let mut views_state : ViewsState = fresh_views_state ();
  let complete_buffer : String =
    save_buffer (&folder_request, config, tantivy, &graph, &mut views_state)
    . await ? . saved_view;
  let edited : String = {
    let r_line : String =
      line_containing (&complete_buffer, "(gnid r)") . to_string ();
    let t_line : String =
      line_containing (&complete_buffer, "(gnid t)") . to_string ();
    let x_line : String = {
      // x: an editable member leaf placed inside the folder, with a
      // (new) child so the repair makes it a non-member rather than removing it.
      let mut l : String =
        r_line
        . replace ("(gnid r)", "(gnid x)")
        . replace (" writeProtected", "");
      if l . ends_with (" r") {
        l . truncate (l . len () - 2);
        l . push_str (" x"); }
      l };
    let x_child : String = {
      let stars : usize =
        x_line . chars () . take_while ( |c| *c == '*' ) . count ();
      format! ( "{} xx", "*" . repeat (stars + 1) ) };
    let folder_line : String =
      line_containing (&complete_buffer, "subscriberFolder") . to_string ();
    complete_buffer
      . replace ( &format! ("{}\n", r_line), "" ) // delete member r
      . replace ( &t_line,                        // place x (with child) after t
                  &format! ("{}\n{}\n{}", t_line, x_line, x_child) )
      . replace ( &folder_line,                      // edit the folder headline text
                  &format! ("{} HELLO", folder_line) ) };
  let response : SaveResponse =
    save_buffer (&edited, config, tantivy, &graph, &mut views_state) . await ?;
  assert! ( response . errors . is_empty (),
    "save must succeed; got errors: {:?}", response . errors );
  let warning : &String =
    response . warnings . iter ()
    . find ( |w| w . contains ("Repaired subscriberFolder") )
    . unwrap_or_else (
      || panic! ( "no subscriberFolder repair warning in {:?}",
                  response . warnings ));
  { // The warning names the folder's viewnode ID.
    let folder_viewnode_id : &str =
      viewnode_id_atom_of ( line_containing (&complete_buffer, "subscriberFolder") )
      . expect ("the rendered folder has a viewnode ID")
      . trim_start_matches ("(vnid ") . trim_end_matches (')');
    assert! ( warning . starts_with ("Repaired subscriberFolder under node n")
              && warning . ends_with ( &format! (
                "\n*** (untitled)\n- viewnode ID: {}. To find it: C-c g v {}\n",
                folder_viewnode_id, folder_viewnode_id )),
              "{}", warning ); }
  assert! ( warning . contains ("reinstated 1 member(s): r"),
            "{}", warning );
  assert! ( warning . contains ("retained 1 child(ren) as non-members, since they are not members: x"),
            "{}", warning );
  assert! ( warning . contains ("edited from the other side"),
            "{}", warning );
  assert! ( response . warnings . iter ()
            . any ( |w| w . contains ("Headline text on a subscriberFolder") ),
    "discarded folder headline text must warn: {:?}",
    response . warnings );
  let saved : String = response . saved_view;
  assert! ( saved . contains ("(gnid r)"),
    "deleted member r must respawn:\n{}", saved );
  { let x_line_after : &str = line_containing (&saved, "(gnid x)");
    assert! ( x_line_after . contains ("noClaim"),
      "x must be rerendered as a non-member: {}", x_line_after ); }
  assert! ( ! line_containing (&saved, "subscriberFolder")
              . contains ("HELLO"),
    "the folder headline edit must not survive the rerender:\n{}", saved );
  { // A body on a non-vognode still aborts the save (Body_of_NonVognode).
    let folder_line : String =
      line_containing (&saved, "subscriberFolder") . to_string ();
    let with_body : String =
      saved . replace ( &folder_line,
                        &format! ("{}\nan illegal body", folder_line) );
    let result =
      save_buffer (&with_body, config, tantivy, &graph, &mut views_state) . await;
    assert! ( result . is_err (),
      "a body on a folder must still abort the save" ); }
  Ok (( )) }

async fn failed_save_carries_warnings_with_errors (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  // A failed save's response carries
  // its warnings alongside its errors. The buffer below produces both
  // a parse-time warning (leftover text on a subscriberFolder headline)
  // and a validation error (an idFolder claiming an id node n lacks).
      let graph : InRustGraphHandle =
        (
          graph_handle_from_config (config) ? );
      let buffer : &str = "\
* (skg node (gnid n) (skgRepos (nodeRepo main))) n
** (skg subscriberFolder) leftover headline text
*** (skg node (gnid r) (skgRepos (nodeRepo main)) writeProtected) r
*** (skg node (gnid t) (skgRepos (nodeRepo main)) writeProtected) t
** (skg idFolder)
*** (skg id) bogus-id
";
      let result : Result<SaveResponse, Box<dyn Error>> =
        save_buffer (buffer, config, tantivy, &graph,
                     &mut fresh_views_state ()) . await;
      let err : Box<dyn Error> = match result {
        Ok (_)  => panic! ("save should fail (idFolder edited)"),
        Err (e) => e, };
      let save_error : &SaveError =
        err . downcast_ref::<SaveError> ()
        . unwrap_or_else ( || panic! ("expected a SaveError: {}", err) );
      match save_error {
        SaveError::BufferValidationErrors { errors, warnings } => {
          assert! (
            errors . iter () . any (
              |e| matches! (e, BufferValidationError::IDFolder_Edited (..)) ),
            "expected an IDFolder_Edited error: {:?}", errors );
          assert! (
            warnings . iter () . any (
              |w| w . contains ("Headline text on a subscriberFolder") ),
            "a failed save must carry its parse warning: {:?}", warnings ); }
        other => panic! (
          "expected BufferValidationErrors, got {:?}", other ), }
      Ok (( )) }
