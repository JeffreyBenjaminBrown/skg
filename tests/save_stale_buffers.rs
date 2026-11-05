// cargo nextest run --test grouped_saves -E 'test(save_stale_buffers::)'
//
// A save refuses a buffer whose copy of a graphnode it would write no
// longer matches the graph (server/serve/handlers/save_staleness.rs).
// Each save sends the text the view was opened with as its baseline.
// Two connections share one runtime, as Emacs and skg-for-ai do: each
// holds its own views, so neither's save rerenders the other's buffers.

use skg::repo_watch::reload::reload_changed_skg_files;
use skg::serve::ViewsState;
use skg::serve::handlers::rebuild_session_lifetime_stores::rebuild_session_lifetime_stores_in_place;
use skg::serve::handlers::save_buffer::handle_save_buffer_request;
use skg::serve::handlers::single_root_view::handle_single_root_view_request;
use skg::serve::handlers::strip_body_whitespace::handle_strip_body_whitespace_request;
use skg::skgrepo_sets::SkgrepoRestriction;
use skg::test_utils::{
  copy_dir_all, extract_string_field_from_sexp, graph_handle_from_config,
  load_config_with_overrides, read_all_lp_messages, skg_env_from_parts};
use skg::dbs::init::create_empty_tantivy_index;
use skg::types::env::SkgEnv;
use skg::types::misc::{SkgConfig, TantivyIndex};
use skg::types::views_state::OpenViews;

use crate::tcp_stream_pair::mk_test_tcp_stream_pair;
use std::collections::BTreeSet;
use std::error::Error;
use std::fs;
use std::io::{BufReader, Write};
use std::net::TcpStream;
use std::path::{Path, PathBuf};
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
    &[ ("main",   root . join ("main")),
       ("theirs", root . join ("theirs")) ] ) ?;
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

/// Open a content view of 'gnid' as 'view_id': its text, which the
/// client keeps as the view's baseline.
fn open_view (
  f           : &mut Fixture,
  views_state : &mut ViewsState,
  gnid        : &str,
  view_id     : &str,
) -> String {
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
  extract_string_field_from_sexp (response, "content") . expect ("content") }

/// Save 'buffer' as 'view_id' through the save handler, with the
/// envelope's '(baseline BASELINE)' when given: the terminal message.
fn save (
  f           : &mut Fixture,
  views_state : &mut ViewsState,
  view_id     : &str,
  buffer      : &str,
  baseline    : Option<&str>,
) -> String {
  let request : String = format! (
    "((request . \"save buffer\") (view-id . \"{}\") (rendered-at . \"0\"))",
    view_id );
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
    . pop ()
    . expect ("a terminal message") }

/// Open 'gnid' on a connection of its own, apply 'edit' to the view,
/// and save it: a change made elsewhere. It must succeed.
fn change_elsewhere (
  f     : &mut Fixture,
  gnid  : &str,
  edit  : &dyn Fn (&str) -> String,
) {
  let mut elsewhere : ViewsState = new_connection ();
  let view_id : String = format! ("{}-elsewhere", gnid);
  let view : String = open_view (f, &mut elsewhere, gnid, &view_id);
  let result : String = save (
    f, &mut elsewhere, &view_id, &edit (&view), Some (&view) );
  assert! ( saved (&result), "{}", result ); }

/// 'text' escaped to sit between the quotes of a sexp string.
fn sexp_string_body (
  text : &str,
) -> String {
  text . replace ('\\', "\\\\") . replace ('"', "\\\"") }

fn file_path (
  f       : &Fixture,
  skgrepo : &str,
  gnid    : &str,
) -> PathBuf {
  f . dir . path () . join (skgrepo) . join (format! ("{}.skg", gnid)) }

fn file_text (
  f    : &Fixture,
  gnid : &str,
) -> String {
  fs::read_to_string ( file_path (f, "main", gnid) ) . unwrap () }

fn errors_of (
  save_result : &str,
) -> String {
  save_result . split ("(errors") . nth (1) . unwrap_or ("") . to_string () }

/// Whether a terminal save message is a save-result without errors.
fn saved (
  save_result : &str,
) -> bool {
  save_result . starts_with ("((response-type save-result)")
  && errors_of (save_result) . starts_with (" ())") }

/// Replace b's body in a view of b.
fn with_b_body_edited (
  view : &str,
) -> String {
  view . replace ("b's second line", "b's second line, edited") }

#[test]
fn a_buffer_whose_copy_of_a_node_changed_cannot_write_it
  () -> Result<(), Box<dyn Error>> {
  // Connection two retitles b after connection one opened it; one's
  // edit of b's body would undo the new title: refused, saying what
  // changed, and nothing written.
  let mut f : Fixture = fixture () ?;
  let mut first : ViewsState = new_connection ();
  let b_in_first : String = open_view (&mut f, &mut first, "b", "b-first");
  change_elsewhere (&mut f, "b", &|view : &str|
    view . replace ("b as first written", "b retitled"));
  let result : String = save (
    &mut f, &mut first, "b-first",
    &with_b_body_edited (&b_in_first), Some (&b_in_first) );
  let errors : String = errors_of (&result);
  assert! ( errors . contains ("NOTHING WAS SAVED")
            && errors . contains ("changed after the buffer was rendered")
            && errors . contains ("- Title: b retitled")
            && errors . contains ("- ID: b")
            && errors . contains (
              "- Changed since this buffer was rendered: the title (then \\\"b as first written\\\", now \\\"b retitled\\\").")
            && ! errors . contains ("the body")
            && errors . contains ("- graphnode ID: b"),
            "{}", result );
  assert! ( file_text (&f, "b") . contains ("b retitled")
            && ! file_text (&f, "b") . contains ("edited") );
  Ok (()) }

#[test]
fn the_stale_error_shows_the_bodys_changed_lines
  () -> Result<(), Box<dyn Error>> {
  let mut f : Fixture = fixture () ?;
  let mut first : ViewsState = new_connection ();
  let b_in_first : String = open_view (&mut f, &mut first, "b", "b-first");
  change_elsewhere (&mut f, "b", &|view : &str|
    view . replace ("b's first line", "b's new first line"));
  let result : String = save (
    &mut f, &mut first, "b-first",
    &b_in_first . replace ("b as first written", "b retitled"),
    Some (&b_in_first) );
  let errors : String = errors_of (&result);
  assert! ( errors . contains (
              "- Changed since this buffer was rendered: the body.\n\
               - The body's changed lines, then against now:\n  \
               - b's first line\n  \
               + b's new first line\n" ),
            "{}", result );
  assert! ( file_text (&f, "b") . contains ("b as first written") );
  Ok (()) }

#[test]
fn an_old_buffer_that_does_not_write_the_changed_node_saves
  () -> Result<(), Box<dyn Error>> {
  let mut f : Fixture = fixture () ?;
  let mut first : ViewsState = new_connection ();
  let c_in_first : String = open_view (&mut f, &mut first, "c", "c-first");
  change_elsewhere (&mut f, "b", &|view : &str|
    view . replace ("b as first written", "b retitled"));
  let result : String = save (
    &mut f, &mut first, "c-first",
    &c_in_first . replace ("c as first written", "c edited"), Some (&c_in_first) );
  assert! ( saved (&result), "{}", result );
  assert! ( file_text (&f, "c") . contains ("c edited") );
  Ok (()) }

#[test]
fn a_buffer_cannot_write_a_node_deleted_after_it_was_rendered
  () -> Result<(), Box<dyn Error>> {
  let mut f : Fixture = fixture () ?;
  let mut first : ViewsState = new_connection ();
  let b_in_first : String = open_view (&mut f, &mut first, "b", "b-first");
  change_elsewhere (&mut f, "b", &|view : &str|
    view . replacen ("(gnid b)", "(gnid b) (editRequest delete)", 1));
  assert! ( ! file_path (&f, "main", "b") . exists () );
  let result : String = save (
    &mut f, &mut first, "b-first",
    &with_b_body_edited (&b_in_first), Some (&b_in_first) );
  let errors : String = errors_of (&result);
  assert! ( errors . contains ("changed after the buffer was rendered")
            && errors . contains ("- ID: b\n- It was deleted after this buffer was rendered.")
            && ! errors . contains ("- Title:"),
            "{}", result );
  assert! ( ! file_path (&f, "main", "b") . exists () );
  Ok (()) }

#[test]
fn a_change_undone_before_the_save_leaves_the_buffer_current
  () -> Result<(), Box<dyn Error>> {
  let mut f : Fixture = fixture () ?;
  let mut first : ViewsState = new_connection ();
  let b_in_first : String = open_view (&mut f, &mut first, "b", "b-first");
  change_elsewhere (&mut f, "b", &|view : &str|
    view . replace ("b as first written", "b retitled"));
  change_elsewhere (&mut f, "b", &|view : &str|
    view . replace ("b retitled", "b as first written"));
  let result : String = save (
    &mut f, &mut first, "b-first",
    &with_b_body_edited (&b_in_first), Some (&b_in_first) );
  assert! ( saved (&result), "{}", result );
  assert! ( file_text (&f, "b") . contains ("edited") );
  Ok (()) }

#[test]
fn a_merge_request_is_never_stale
  () -> Result<(), Box<dyn Error>> {
  // The merge is built from the graph at save time, so the text
  // preserver carries b's new title.
  let mut f : Fixture = fixture () ?;
  let mut first : ViewsState = new_connection ();
  let a_in_first : String = open_view (&mut f, &mut first, "a", "a-first");
  change_elsewhere (&mut f, "b", &|view : &str|
    view . replace ("b as first written", "b retitled"));
  let result : String = save (
    &mut f, &mut first, "a-first",
    &a_in_first . replacen ("(gnid a)", "(gnid a) (editRequest (merge b))", 1),
    Some (&a_in_first) );
  assert! ( saved (&result), "{}", result );
  assert! ( ! file_path (&f, "main", "b") . exists () );
  assert! ( fs::read_dir ( f . dir . path () . join ("main") ) ?
            . any ( |entry| fs::read_to_string ( entry . unwrap () . path () )
                    . unwrap () . contains ("MERGED: b retitled") ),
            "a text preserver titled after b's new title" );
  Ok (()) }

#[test]
fn a_whitespace_strip_does_not_make_a_buffer_stale
  () -> Result<(), Box<dyn Error>> {
  let mut f : Fixture = fixture () ?;
  let mut connection : ViewsState = new_connection ();
  let w_view : String = open_view (&mut f, &mut connection, "w", "w-view");
  { let (mut stream, read_end) : (TcpStream, TcpStream) = mk_test_tcp_stream_pair ();
    handle_strip_body_whitespace_request (&mut stream, &mut f . env);
    drop (stream);
    let messages : Vec<String> = read_all_lp_messages (&mut BufReader::new (read_end));
    assert! ( messages [0] . contains ("1"), "{:?}", messages ); }
  assert! ( ! file_text (&f, "w") . contains ("w's body "), "{}", file_text (&f, "w") );
  let result : String = save (
    &mut f, &mut connection, "w-view",
    &w_view . replace ("its second line", "its edited second line"), Some (&w_view) );
  assert! ( saved (&result), "{}", result );
  assert! ( file_text (&f, "w") . contains ("its edited second line") );
  Ok (()) }

#[test]
fn trailing_whitespace_on_disk_does_not_make_a_buffer_stale
  () -> Result<(), Box<dyn Error>> {
  // The view draws w's body with its trailing whitespace, and the
  // parser trims it, so the baseline's copy of w differs from the
  // file; it still matches the graph as drawn.
  let mut f : Fixture = fixture () ?;
  let mut connection : ViewsState = new_connection ();
  let w_view : String = open_view (&mut f, &mut connection, "w", "w-view");
  assert! ( w_view . contains ("w's body   \n"), "{}", w_view );
  let result : String = save (
    &mut f, &mut connection, "w-view",
    &w_view . replace ("its second line", "its edited second line"), Some (&w_view) );
  assert! ( saved (&result), "{}", result );
  assert! ( file_text (&f, "w") . contains ("its edited second line") );
  Ok (()) }

#[test]
fn a_rebuild_does_not_make_a_buffer_stale
  () -> Result<(), Box<dyn Error>> {
  let mut f : Fixture = fixture () ?;
  let mut connection : ViewsState = new_connection ();
  let c_view : String = open_view (&mut f, &mut connection, "c", "c-view");
  rebuild_session_lifetime_stores_in_place (&mut f . env, &mut connection) ?;
  let result : String = save (
    &mut f, &mut connection, "c-view",
    &c_view . replace ("c as first written", "c edited"), Some (&c_view) );
  assert! ( saved (&result), "{}", result );
  assert! ( file_text (&f, "c") . contains ("c edited") );
  Ok (()) }

#[test]
fn a_buffer_cannot_fork_a_foreign_node_reloaded_after_it_was_rendered
  () -> Result<(), Box<dyn Error>> {
  // Editing a foreign node asks to fork it; the clone would copy the
  // buffer's stale text of it, so the save is refused, naming it.
  let mut f : Fixture = fixture () ?;
  let mut connection : ViewsState = new_connection ();
  let n_view : String = open_view (&mut f, &mut connection, "n", "n-view");
  { let n_path : PathBuf = file_path (&f, "theirs", "n");
    fs::write ( &n_path, "pid: n\ntitle: n as they rewrote it\n" ) ?;
    let (notice, _unread) = reload_changed_skg_files (
      &f . env, & BTreeSet::from ([ n_path ]) );
    assert! ( ! notice . files_reloaded . is_empty (), "{:?}", notice . files_reloaded ); }
  let result : String = save (
    &mut f, &mut connection, "n-view",
    &n_view . replace ("n as they wrote it", "n as I would have it"), Some (&n_view) );
  let errors : String = errors_of (&result);
  assert! ( errors . contains ("changed after the buffer was rendered")
            && errors . contains ("- Title: n as they rewrote it")
            && errors . contains ("- ID: n"),
            "{}", result );
  Ok (()) }

#[test]
fn an_unreadable_baseline_refuses_the_save
  () -> Result<(), Box<dyn Error>> {
  let mut f : Fixture = fixture () ?;
  let mut connection : ViewsState = new_connection ();
  let c_view : String = open_view (&mut f, &mut connection, "c", "c-view");
  let result : String = save (
    &mut f, &mut connection, "c-view",
    &c_view . replace ("c as first written", "c edited"),
    Some ("* (skg (node (gnid c) c\n") );
  assert! ( errors_of (&result) . contains ("baseline")
            && errors_of (&result) . contains ("could not be read"),
            "{}", result );
  assert! ( file_text (&f, "c") . contains ("c as first written") );
  Ok (()) }

#[test]
fn a_save_without_a_baseline_is_refused
  () -> Result<(), Box<dyn Error>> {
  // The envelope must carry the saved buffer's baseline: without it the
  // save is refused, naming the field, and nothing is written.
  let mut f : Fixture = fixture () ?;
  let mut connection : ViewsState = new_connection ();
  let c_view : String = open_view (&mut f, &mut connection, "c", "c-view");
  let result : String = save (
    &mut f, &mut connection, "c-view",
    &c_view . replace ("c as first written", "c edited"), None );
  assert! ( errors_of (&result) . contains ("missing baseline"), "{}", result );
  assert! ( file_text (&f, "c") . contains ("c as first written") );
  Ok (()) }
