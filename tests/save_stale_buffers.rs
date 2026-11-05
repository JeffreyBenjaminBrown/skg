// cargo nextest run --test grouped_saves -E 'test(save_stale_buffers::)'
//
// A save refuses a buffer rendered before a node it would write last
// changed (server/serve/handlers/save_staleness.rs). Two connections
// share one runtime, as Emacs and skg-for-ai do: each holds its own
// views, so neither's save rerenders the other's buffers.

use skg::serve::ViewsState;
use skg::serve::handlers::rebuild_session_lifetime_stores::rebuild_session_lifetime_stores_in_place;
use skg::serve::handlers::runtime_generation::handle_runtime_generation_request;
use skg::serve::handlers::save_buffer::handle_save_buffer_request;
use skg::serve::handlers::single_root_view::handle_single_root_view_request;
use skg::skgrepo_sets::SkgrepoRestriction;
use skg::test_utils::{
  copy_dir_all, extract_string_field_from_sexp, graph_handle_from_config,
  load_config_with_overrides, read_all_lp_messages, skg_env_from_parts};
use skg::dbs::init::create_empty_tantivy_index;
use skg::types::env::SkgEnv;
use skg::types::misc::{SkgConfig, TantivyIndex};
use skg::types::views_state::OpenViews;

use crate::tcp_stream_pair::mk_test_tcp_stream_pair;
use std::error::Error;
use std::fs;
use std::io::{BufReader, Write};
use std::net::TcpStream;
use std::path::Path;
use tempfile::TempDir;

struct Fixture {
  dir    : TempDir,
  config : SkgConfig,
  env    : SkgEnv,
}

fn fixture (
) -> Result<Fixture, Box<dyn Error>> {
  let dir : TempDir = TempDir::new () ?;
  copy_dir_all ( Path::new ("tests/save_stale_buffers/fixtures"), dir . path () ) ?;
  let root : &Path = dir . path ();
  let config : SkgConfig = load_config_with_overrides (
    root . join ("skgconfig.toml") . to_str () . unwrap (),
    Some ("save-stale-buffers"),
    &[ ("main", root . join ("main")) ] ) ?;
  let config : SkgConfig = SkgConfig {
    tantivy_folder : root . join ("tantivy"),
    data_root      : root . to_path_buf (),
    .. config };
  let tantivy : TantivyIndex =
    create_empty_tantivy_index (&config . tantivy_folder) ?;
  let env : SkgEnv = skg_env_from_parts (
    &config, &tantivy, &graph_handle_from_config (&config) ?);
  Ok ( Fixture { dir, config, env } ) }

fn new_connection (
) -> ViewsState {
  ViewsState { open_views     : OpenViews::new (),
               graph_straddle : None, } }

/// Open a content view of 'gnid' as 'view_id': its text, and the
/// generation it was rendered at.
fn open_view (
  f           : &mut Fixture,
  views_state : &mut ViewsState,
  gnid        : &str,
  view_id     : &str,
) -> (String, u64) {
  let (mut stream, read_end) : (TcpStream, TcpStream) = mk_test_tcp_stream_pair ();
  handle_single_root_view_request (
    &mut stream,
    &format! ( "((request . \"single root content view\") (id . \"{}\") (view-id . \"{}\"))",
               gnid, view_id ),
    &f . env, views_state,
    &SkgrepoRestriction::unrestricted (&f . config) );
  drop (stream);
  let messages : Vec<String> = read_all_lp_messages (&mut BufReader::new (read_end));
  let response : &String = messages . last () . expect ("a content-view response");
  ( extract_string_field_from_sexp (response, "content") . expect ("content"),
    extract_string_field_from_sexp (response, "rendered-at") . expect ("rendered-at")
      . parse () . unwrap () ) }

/// Save 'buffer' as 'view_id' through the save handler, with
/// '(rendered-at . RENDERED_AT)' when given, and with the envelope's
/// '(baseline BASELINE)' when given: the terminal save-result message.
fn save (
  f           : &mut Fixture,
  views_state : &mut ViewsState,
  view_id     : &str,
  buffer      : &str,
  rendered_at : Option<String>,
  baseline    : Option<&str>,
) -> String {
  let request : String = format! (
    "((request . \"save buffer\") (view-id . \"{}\"){})",
    view_id,
    rendered_at . map ( |n| format! (" (rendered-at . \"{}\")", n) )
      . unwrap_or_default () );
  let envelope : String = format! (
    "((saved-buffer \"{}\"){} (other-views ()))",
    sexp_string_body (buffer),
    baseline . map ( |text| format! (" (baseline \"{}\")", sexp_string_body (text)) )
      . unwrap_or_default () );
  let (mut client_writes, server_reads) : (TcpStream, TcpStream) =
    mk_test_tcp_stream_pair ();
  write! ( client_writes, "Content-Length: {}\r\n\r\n{}",
           envelope . len (), envelope ) . unwrap ();
  let (mut server_writes, client_reads) : (TcpStream, TcpStream) =
    mk_test_tcp_stream_pair ();
  handle_save_buffer_request (
    &mut BufReader::new (server_reads), &mut server_writes, &request,
    &mut f . env, views_state,
    &SkgrepoRestriction::unrestricted (&f . config) );
  drop (server_writes);
  read_all_lp_messages (&mut BufReader::new (client_reads))
    . into_iter ()
    . find ( |message| message . contains ("save-result") )
    . expect ("a save-result") }

/// 'text' escaped to sit between the quotes of a sexp string.
fn sexp_string_body (
  text : &str,
) -> String {
  text . replace ('\\', "\\\\") . replace ('"', "\\\"") }

fn file_text (
  f    : &Fixture,
  gnid : &str,
) -> String {
  fs::read_to_string (
    f . dir . path () . join ("main") . join (format! ("{}.skg", gnid)) )
    . unwrap () }

fn errors_of (
  save_result : &str,
) -> String {
  save_result . split ("(errors") . nth (1) . unwrap_or ("") . to_string () }

#[test]
fn a_buffer_rendered_before_a_node_changed_cannot_write_it
  () -> Result<(), Box<dyn Error>> {
  let mut f : Fixture = fixture () ?;
  let mut first  : ViewsState = new_connection ();
  let mut second : ViewsState = new_connection ();
  let (a_in_first, rendered_at) : (String, u64) =
    open_view (&mut f, &mut first, "a", "a-first");
  let (a_in_second, second_rendered_at) : (String, u64) =
    open_view (&mut f, &mut second, "a", "a-second");
  let (c_in_second, _) : (String, u64) =
    open_view (&mut f, &mut second, "c", "c-second");
  assert_eq! (second_rendered_at, rendered_at);
  { // The first connection's save of a succeeds, and says when its rerender was drawn.
    let result : String = save (
      &mut f, &mut first, "a-first",
      &a_in_first . replace ("a as first written", "a by first"), Some (rendered_at . to_string ()),
      Some (&a_in_first) );
    assert! ( errors_of (&result) . starts_with (" ())"), "{}", result );
    assert_eq! (
      extract_string_field_from_sexp (&result, "rendered-at"),
      Some ( (rendered_at + 1) . to_string () )); }
  { // The second connection's older view of a would undo that: refused, nothing written.
    let result : String = save (
      &mut f, &mut second, "a-second",
      &a_in_second . replace ("a as first written", "a by second"), Some (rendered_at . to_string ()),
      Some (&a_in_second) );
    let errors : String = errors_of (&result);
    assert! ( errors . contains ("NOTHING WAS SAVED")
              && errors . contains ("changed after the buffer was rendered")
              && errors . contains ("- ID: a")
              && errors . contains ( &format! (
                "rendered at runtime generation {}", rendered_at ))
              && errors . contains ( &format! (
                "last changed at runtime generation {}", rendered_at + 1 ))
              && errors . contains ("- graphnode ID: a"),
              "{}", result );
    assert! ( extract_string_field_from_sexp (&result, "rendered-at") . is_none () );
    assert! ( file_text (&f, "a") . contains ("a by first") ); }
  { // The same old buffer, a's text untouched, still differs from the graph,
    // so saving it would also undo the change: refused.
    let result : String = save (
      &mut f, &mut second, "a-second", &a_in_second, Some (rendered_at . to_string ()),
      Some (&a_in_second) );
    assert! ( errors_of (&result) . contains ("NOTHING WAS SAVED"), "{}", result );
    assert! ( file_text (&f, "a") . contains ("a by first") ); }
  { // An equally old buffer that does not write a saves.
    let result : String = save (
      &mut f, &mut second, "c-second",
      &c_in_second . replace ("c as first written", "c by second"), Some (rendered_at . to_string ()),
      Some (&c_in_second) );
    assert! ( errors_of (&result) . starts_with (" ())"), "{}", result );
    assert! ( file_text (&f, "c") . contains ("c by second") ); }
  Ok (()) }

/// The 'runtime generation' request's answer, as a client dating a
/// new empty view receives it.
fn runtime_generation_answer (
  f : &Fixture,
) -> String {
  let (mut stream, read_end) : (TcpStream, TcpStream) = mk_test_tcp_stream_pair ();
  handle_runtime_generation_request (&mut stream, &f . env);
  drop (stream);
  let messages : Vec<String> = read_all_lp_messages (&mut BufReader::new (read_end));
  assert! ( messages [0] . starts_with ("((response-type runtime-generation)"),
            "{}", messages [0] );
  extract_string_field_from_sexp (&messages [0], "content") . expect ("content") }

#[test]
fn a_new_empty_view_is_dated_by_the_runtime_generation_request
  () -> Result<(), Box<dyn Error>> {
  // A buffer the client built itself, like a new empty view the user
  // typed an existing node's headline into, saves as rendered at the
  // generation the client asked for when it opened the view.
  let mut f : Fixture = fixture () ?;
  let mut connection : ViewsState = new_connection ();
  let opened_at : String = runtime_generation_answer (&f);
  let (c_view, rendered_at) : (String, u64) =
    open_view (&mut f, &mut connection, "c", "c-view");
  assert_eq! (opened_at, rendered_at . to_string ());
  let first : String = save (
    &mut f, &mut connection, "c-view",
    &c_view . replace ("c as first written", "c once"), Some (rendered_at . to_string ()),
    Some (&c_view) );
  assert! ( errors_of (&first) . starts_with (" ())"), "{}", first );
  { // Typed into a view opened before c changed: saving it would undo that change.
    let typed : String = save (
      &mut f, &mut connection, "typed-view",
      "* (skg node (gnid c) (skgRepos (nodeRepo main))) c typed\n",
      Some (opened_at), Some ("") );
    assert! ( errors_of (&typed) . contains ("changed after the buffer was rendered"),
              "{}", typed );
    assert! ( file_text (&f, "c") . contains ("c once") ); }
  { // An empty view opened after the change saves.
    let opened_later_at : String = runtime_generation_answer (&f);
    let typed : String = save (
      &mut f, &mut connection, "typed-later-view",
      "* (skg node (gnid c) (skgRepos (nodeRepo main))) c typed\n",
      Some (opened_later_at), Some ("") );
    assert! ( errors_of (&typed) . starts_with (" ())"), "{}", typed );
    assert! ( file_text (&f, "c") . contains ("c typed") ); }
  Ok (()) }

#[test]
fn a_save_whose_rendered_at_is_not_a_number_is_refused
  () -> Result<(), Box<dyn Error>> {
  let mut f : Fixture = fixture () ?;
  let mut connection : ViewsState = new_connection ();
  let result : String = save (
    &mut f, &mut connection, "typed-view",
    "* (skg node (gnid c) (skgRepos (nodeRepo main))) c typed\n",
    Some ("never" . to_string ()), Some ("") );
  assert! ( errors_of (&result) . contains ("not a runtime generation number"),
            "{}", result );
  assert! ( file_text (&f, "c") . contains ("c as first written") );
  Ok (()) }

#[test]
fn a_save_without_rendered_at_is_refused
  () -> Result<(), Box<dyn Error>> {
  let mut f : Fixture = fixture () ?;
  let mut connection : ViewsState = new_connection ();
  let (c_view, _) : (String, u64) =
    open_view (&mut f, &mut connection, "c", "c-view");
  let result : String = save (
    &mut f, &mut connection, "c-view",
    &c_view . replace ("c as first written", "c edited"), None, Some (&c_view) );
  assert! ( errors_of (&result) . contains ("rendered-at"), "{}", result );
  assert! ( file_text (&f, "c") . contains ("c as first written") );
  Ok (()) }

#[test]
fn a_save_without_a_baseline_is_refused
  () -> Result<(), Box<dyn Error>> {
  // The envelope must carry the saved buffer's baseline: without it the
  // save is refused, naming the field, and nothing is written.
  let mut f : Fixture = fixture () ?;
  let mut connection : ViewsState = new_connection ();
  let (c_view, rendered_at) : (String, u64) =
    open_view (&mut f, &mut connection, "c", "c-view");
  let result : String = save (
    &mut f, &mut connection, "c-view",
    &c_view . replace ("c as first written", "c edited"),
    Some (rendered_at . to_string ()), None );
  assert! ( errors_of (&result) . contains ("missing baseline"), "{}", result );
  assert! ( file_text (&f, "c") . contains ("c as first written") );
  Ok (()) }

#[test]
fn a_rebuild_makes_every_earlier_buffer_stale
  () -> Result<(), Box<dyn Error>> {
  let mut f : Fixture = fixture () ?;
  let mut connection : ViewsState = new_connection ();
  let (c_view, rendered_at) : (String, u64) =
    open_view (&mut f, &mut connection, "c", "c-view");
  rebuild_session_lifetime_stores_in_place (&mut f . env, &mut connection) ?;
  { let result : String = save (
      &mut f, &mut connection, "c-view",
      &c_view . replace ("c as first written", "c edited"), Some (rendered_at . to_string ()),
      Some (&c_view) );
    assert! ( errors_of (&result) . contains ( &format! (
                "last changed at runtime generation {}", rendered_at + 1 )),
              "{}", result );
    assert! ( file_text (&f, "c") . contains ("c as first written") ); }
  { // A view opened after the rebuild saves.
    let (fresh, fresh_rendered_at) : (String, u64) =
      open_view (&mut f, &mut connection, "c", "c-fresh");
    assert_eq! (fresh_rendered_at, rendered_at + 1);
    let result : String = save (
      &mut f, &mut connection, "c-fresh",
      &fresh . replace ("c as first written", "c edited"), Some (fresh_rendered_at . to_string ()),
      Some (&fresh) );
    assert! ( errors_of (&result) . starts_with (" ())"), "{}", result );
    assert! ( file_text (&f, "c") . contains ("c edited") ); }
  Ok (()) }
