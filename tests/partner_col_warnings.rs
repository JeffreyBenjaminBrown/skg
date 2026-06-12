// cargo nextest run --test grouped_overrides -E 'test(partner_col_warnings::)'
//
// When the completion pass repairs a read-only PartnerCol in the
// view the user just saved, the save succeeds and
// SaveResponse.warnings says what was repaired
// (TODO/full-schema/8_readonly-set-ergonomics.org):
// - a deleted generated member is restored, with a warning that
//   explains membership is edited from the other side;
// - a non-member parked as Affected with a subtree is demoted to
//   independent, with a warning.
//
// Fixture: r and t subscribe to n (so n's view has a SubscriberCol);
// x is an unrelated node the test parks inside that col.

use std::error::Error;
use std::net::TcpStream;
use std::sync::Arc;

use skg::dbs::in_rust_graph::install_or_swap_global_handle;
use skg::test_utils::{run_with_shared_test_db, graph_handle_from_config};
use skg::test_utils::update_from_and_rerender_buffer_test as update_from_and_rerender_buffer;
use skg::to_org::render::content_view::multi_root_view;
use skg::serve::ViewsState;
use skg::types::views_state::OpenViews;
use skg::types::misc::{ID, SkgConfig, TantivyIndex};
use skg::serve::handlers::save_buffer::SaveResponse;
use skg::types::errors::{SaveError, BufferValidationError};

use skg::dbs::in_rust_graph::InRustGraphHandle;
use typedb_driver::TypeDBDriver;

#[test]
fn all_tests
  () -> Result<(), Box<dyn Error>> {
  let fixtures : &str = "tests/partner_col_warnings/fixtures";
  run_with_shared_test_db (
    "skg-test-partner-col-warnings",
    |s| Box::pin ( async move {
      s . reset ("readonly_col_repairs_warn", fixtures) . await ?;
      readonly_col_repairs_warn_impl (
        &s . config, &s . driver, &mut s . tantivy ) . await ?;
      s . reset ("failed_save_carries_warnings_with_errors", fixtures) . await ?;
      failed_save_carries_warnings_with_errors (
        &s . config, &s . driver, &mut s . tantivy ) . await ?;
      Ok (( )) } )) }

async fn save_buffer (
  buf     : &str,
  config  : &SkgConfig,
  driver  : &Arc<TypeDBDriver>,
  tantivy : &mut TantivyIndex,
  graph   : &InRustGraphHandle, // must be the process-global handle, or the save's coherence debug-assert reads a stale graph
) -> Result<SaveResponse, Box<dyn Error>> {
  let mut views_state : ViewsState = ViewsState {
    diff_mode_enabled : false,
    open_views        : OpenViews::new (),
  };
  let listener : std::net::TcpListener =
    std::net::TcpListener::bind ("127.0.0.1:0") . unwrap ();
  let mut stream : TcpStream =
    TcpStream::connect (listener . local_addr () . unwrap ()) . unwrap ();
  update_from_and_rerender_buffer (
    &mut stream,
    buf, driver, config, tantivy, graph, false,
    &Err ( String::new () ), &mut views_state ) . await }

fn line_containing<'a> (
  buf      : &'a str,
  fragment : &str,
) -> &'a str {
  buf . lines ()
    . find ( |l| l . contains (fragment) )
    . unwrap_or_else (
      || panic! ( "no line contains {:?} in:\n{}", fragment, buf )) }

async fn readonly_col_repairs_warn_impl (
  config  : &SkgConfig,
  driver  : &Arc<TypeDBDriver>,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let graph : InRustGraphHandle =
    install_or_swap_global_handle (
      graph_handle_from_config (config) ? );
  let (complete_buffer, _pids, _tree)
    : (String, Vec<ID>, _) =
    multi_root_view (
      driver, config, Some (tantivy),
      &[ ID ("n" . to_string ()) ], false ) . await ?;
  let edited : String = {
    let r_line : String =
      line_containing (&complete_buffer, "(id r)") . to_string ();
    let t_line : String =
      line_containing (&complete_buffer, "(id t)") . to_string ();
    let x_line : String = {
      // x: a definitive Affected leaf parked inside the col, with a
      // (new) child so the repair is a demotion, not a removal.
      let mut l : String =
        r_line
        . replace ("(id r)", "(id x)")
        . replace (" indef", "");
      if l . ends_with (" r") {
        l . truncate (l . len () - 2);
        l . push_str (" x"); }
      l };
    let x_child : String = {
      let stars : usize =
        x_line . chars () . take_while ( |c| *c == '*' ) . count ();
      format! ( "{} xx", "*" . repeat (stars + 1) ) };
    let col_line : String =
      line_containing (&complete_buffer, "subscriberCol") . to_string ();
    complete_buffer
      . replace ( &format! ("{}\n", r_line), "" ) // delete member r
      . replace ( &t_line,                        // park x (with child) after t
                  &format! ("{}\n{}\n{}", t_line, x_line, x_child) )
      . replace ( &col_line,                      // edit the col headline text
                  &format! ("{} HELLO", col_line) ) };
  let response : SaveResponse =
    save_buffer (&edited, config, driver, tantivy, &graph) . await ?;
  assert! ( response . errors . is_empty (),
    "save must succeed; got errors: {:?}", response . errors );
  let warning : &String =
    response . warnings . iter ()
    . find ( |w| w . contains ("Repaired subscriberCol") )
    . unwrap_or_else (
      || panic! ( "no subscriberCol repair warning in {:?}",
                  response . warnings ));
  assert! ( warning . contains ("under node n"), "{}", warning );
  assert! ( warning . contains ("restored 1 member(s): r"),
            "{}", warning );
  assert! ( warning . contains ("demoted 1 non-member(s) to independent: x"),
            "{}", warning );
  assert! ( warning . contains ("edited from the other side"),
            "{}", warning );
  assert! ( response . warnings . iter ()
            . any ( |w| w . contains ("Headline text on a subscriberCol") ),
    "discarded col headline text must warn: {:?}",
    response . warnings );
  let saved : String = response . saved_view;
  assert! ( saved . contains ("(id r)"),
    "deleted member r must respawn:\n{}", saved );
  { let x_line_after : &str = line_containing (&saved, "(id x)");
    assert! ( x_line_after . contains ("independent"),
      "x must be rerendered as independent: {}", x_line_after ); }
  assert! ( ! line_containing (&saved, "subscriberCol")
              . contains ("HELLO"),
    "the col headline edit must not survive the rerender:\n{}", saved );
  { // A BODY on a scaffold still aborts the save (Body_of_Scaffold).
    let col_line : String =
      line_containing (&saved, "subscriberCol") . to_string ();
    let with_body : String =
      saved . replace ( &col_line,
                        &format! ("{}\nan illegal body", col_line) );
    let result =
      save_buffer (&with_body, config, driver, tantivy, &graph) . await;
    assert! ( result . is_err (),
      "a body on a col scaffold must still abort the save" ); }
  Ok (( )) }

async fn failed_save_carries_warnings_with_errors (
  config  : &SkgConfig,
  driver  : &Arc<TypeDBDriver>,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  // Decided 2026-06-12 (option b): a failed save's response carries
  // its warnings alongside its errors. The buffer below produces BOTH
  // a parse-time warning (leftover text on a subscriberCol headline)
  // and a validation error (an idCol claiming an id node n lacks).
      let graph : InRustGraphHandle =
        install_or_swap_global_handle (
          graph_handle_from_config (config) ? );
      let buffer : &str = "\
* (skg (node (id n) (source main))) n
** (skg subscriberCol) leftover headline text
*** (skg (node (id r) (source main) indef)) r
*** (skg (node (id t) (source main) indef)) t
** (skg idCol)
*** (skg id) bogus-id
";
      let result : Result<SaveResponse, Box<dyn Error>> =
        save_buffer (buffer, config, driver, tantivy, &graph) . await;
      let err : Box<dyn Error> = match result {
        Ok (_)  => panic! ("save should fail (idCol edited)"),
        Err (e) => e, };
      let save_error : &SaveError =
        err . downcast_ref::<SaveError> ()
        . unwrap_or_else ( || panic! ("expected a SaveError: {}", err) );
      match save_error {
        SaveError::BufferValidationErrors { errors, warnings } => {
          assert! (
            errors . iter () . any (
              |e| matches! (e, BufferValidationError::IDCol_Edited (..)) ),
            "expected an IDCol_Edited error: {:?}", errors );
          assert! (
            warnings . iter () . any (
              |w| w . contains ("Headline text on a subscriberCol") ),
            "a failed save must carry its parse warning: {:?}", warnings ); }
        other => panic! (
          "expected BufferValidationErrors, got {:?}", other ), }
      Ok (( )) }
