// cargo nextest run --test grouped_saves -E 'test(save_stale_buffers::)'
//
// A save is refused when the buffer edited a graphnode whose copy in
// its baseline no longer matches the graph, and leaves out one the
// buffer did not edit (server/serve/handlers/save_staleness.rs).
// Each save sends the text the view was opened with as its baseline.
// Two connections share one runtime, as Emacs and skg-for-ai do: each
// holds its own views, so neither's save rerenders the other's buffers.

use skg::repo_watch::reload::reload_changed_skg_files;
use skg::serve::ViewsState;
use skg::serve::handlers::rebuild_session_lifetime_stores::rebuild_session_lifetime_stores_in_place;
use skg::serve::handlers::rerender_all_views::handle_rerender_all_views_request;
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
use skg::types::views_state::{OpenViews, ViewforestId};

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
       ("theirs", root . join ("theirs")),
       ("other",  root . join ("other")) ] ) ?;
  fs::create_dir_all ( root . join ("other") ) ?;
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
    "((request . \"save buffer\") (view-id . \"{}\"))",
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

/// Rerender every view 'views_state' holds: the "rerender-view"
/// messages of the stream.
fn rerender_all_views (
  f           : &Fixture,
  views_state : &mut ViewsState,
) -> Vec<String> {
  let (mut stream, read_end) : (TcpStream, TcpStream) = mk_test_tcp_stream_pair ();
  handle_rerender_all_views_request (
    &mut stream, "((request . \"rerender all views\"))",
    &f . env, views_state,
    &SkgrepoRestriction::unrestricted (&f . config) );
  drop (stream);
  read_all_lp_messages (&mut BufReader::new (read_end))
    . into_iter ()
    . filter ( |message| message . starts_with ("((response-type rerender-view)") )
    . collect () }

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

/// The text of every file in the skgrepo folder 'skgrepo'.
fn texts_of_files_in (
  f       : &Fixture,
  skgrepo : &str,
) -> Vec<String> {
  fs::read_dir ( f . dir . path () . join (skgrepo) ) . unwrap ()
    . map ( |entry| fs::read_to_string ( entry . unwrap () . path () ) . unwrap () )
    . collect () }

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

/// Through a view of s, merge k2 into its sibling k1, and return the
/// view's text as the save redrew it. s's file still lists both IDs,
/// which now name one graphnode.
fn merge_the_siblings_of_s (
  f           : &mut Fixture,
  views_state : &mut ViewsState,
) -> String {
  let s_view : String = open_view (f, views_state, "s", "s-view");
  let result : String = save (
    f, views_state, "s-view",
    &s_view . replacen ("(gnid k1)", "(gnid k1) (editRequest (merge k2))", 1),
    Some (&s_view) );
  assert! ( saved (&result), "{}", result );
  assert! ( ! file_path (f, "main", "k2") . exists () );
  assert! ( file_text (f, "s") . contains ("- k2"), "{}", file_text (f, "s") );
  extract_string_field_from_sexp (&result, "content") . expect ("content") }

#[test]
fn a_fresh_view_of_a_container_of_merged_siblings_saves
  () -> Result<(), Box<dyn Error>> {
  // The baseline draws s's members once per pid; the graph's s lists
  // k1 and k2, now one graphnode. That is no change since the render.
  let mut f : Fixture = fixture () ?;
  let mut connection : ViewsState = new_connection ();
  merge_the_siblings_of_s (&mut f, &mut connection);
  let mut fresh : ViewsState = new_connection ();
  let s_fresh : String = open_view (&mut f, &mut fresh, "s", "s-fresh");
  let result : String = save (
    &mut f, &mut fresh, "s-fresh", &s_fresh, Some (&s_fresh) );
  assert! ( saved (&result), "{}", result );
  Ok (()) }

#[test]
fn the_view_that_merged_two_siblings_can_edit_their_container
  () -> Result<(), Box<dyn Error>> {
  let mut f : Fixture = fixture () ?;
  let mut connection : ViewsState = new_connection ();
  let s_redrawn : String = merge_the_siblings_of_s (&mut f, &mut connection);
  let result : String = save (
    &mut f, &mut connection, "s-view",
    &s_redrawn . replace ("s, a container of two siblings", "s retitled"),
    Some (&s_redrawn) );
  assert! ( saved (&result), "{}", result );
  assert! ( file_text (&f, "s") . contains ("s retitled")
            && ! file_text (&f, "s") . contains ("- k2"),
            "{}", file_text (&f, "s") );
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
  // file; it still matches the graph as a view reads it back.
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
fn views_survive_a_rebuild_and_a_rerender_redraws_them
  () -> Result<(), Box<dyn Error>> {
  // c changes on disk and a rebuild reads it. The connection still
  // holds its view of c; rerendering every view redraws it from the
  // rebuilt graph, and a save of the redrawn view succeeds.
  let mut f : Fixture = fixture () ?;
  let mut connection : ViewsState = new_connection ();
  open_view (&mut f, &mut connection, "c", "c-view");
  fs::write ( file_path (&f, "main", "c"),
              "pid: c\ntitle: c as rewritten on disk\n" ) ?;
  rebuild_session_lifetime_stores_in_place (&mut f . env, &mut connection) ?;
  assert! ( connection . open_views
              . viewforest_id_to_view ( &ViewforestId::from_client_string (
                "c-view" . to_string () ))
              . is_some () );
  let rerendered : Vec<String> = rerender_all_views (&f, &mut connection);
  let c_rerendered : String = rerendered . iter ()
    . find ( |message| message . contains ("c-view") )
    . and_then ( |message| extract_string_field_from_sexp (message, "content") )
    . expect ("a rerender of c-view");
  assert! ( c_rerendered . contains ("c as rewritten on disk"), "{}", c_rerendered );
  let result : String = save (
    &mut f, &mut connection, "c-view",
    &c_rerendered . replace ("c as rewritten on disk", "c edited after the rebuild"),
    Some (&c_rerendered) );
  assert! ( saved (&result), "{}", result );
  assert! ( file_text (&f, "c") . contains ("c edited after the rebuild") );
  Ok (()) }

#[test]
fn a_view_left_unrerendered_across_a_rebuild_is_compared_at_its_save
  () -> Result<(), Box<dyn Error>> {
  // A view with unsaved edits is not rerendered after a rebuild. It
  // keeps its baseline; c changed on disk before the rebuild, so a
  // save writing c from that baseline is refused.
  let mut f : Fixture = fixture () ?;
  let mut connection : ViewsState = new_connection ();
  let c_view : String = open_view (&mut f, &mut connection, "c", "c-view");
  fs::write ( file_path (&f, "main", "c"),
              "pid: c\ntitle: c as rewritten on disk\n" ) ?;
  rebuild_session_lifetime_stores_in_place (&mut f . env, &mut connection) ?;
  let result : String = save (
    &mut f, &mut connection, "c-view",
    &c_view . replace ("c as first written", "c edited"), Some (&c_view) );
  assert! ( errors_of (&result) . contains ("changed after the buffer was rendered")
            && errors_of (&result) . contains ("- Title: c as rewritten on disk"),
            "{}", result );
  assert! ( file_text (&f, "c") . contains ("c as rewritten on disk") );
  Ok (()) }

#[test]
fn a_buffer_cannot_fork_a_foreign_node_reloaded_after_it_was_rendered
  () -> Result<(), Box<dyn Error>> {
  // Editing a foreign node asks to fork it; the clone would copy the
  // buffer's stale text of it, so the save is refused, naming it.
  let mut f : Fixture = fixture () ?;
  let mut connection : ViewsState = new_connection ();
  let n_view : String = open_view (&mut f, &mut connection, "n", "n-view");
  rewrite_and_reload_n (&mut f) ?;
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
  assert! ( errors_of (&result) . contains ("NOTHING WAS SAVED")
            && errors_of (&result) . contains ("missing baseline"),
            "{}", result );
  assert! ( file_text (&f, "c") . contains ("c as first written") );
  Ok (()) }

/// Replace rb's body in a view showing rb.
fn with_rb_body_edited (
  view : &str,
) -> String {
  view . replace ("rb's second line", "rb's second line, edited") }

/// The warning a save gives for leaving 'gnid', titled 'title' in the
/// buffer, as the graph has it, after the graph changed it.
fn left_as_the_graph_has_it (
  title : &str,
  gnid  : &str,
) -> String {
  format! ( "Left as the graph has it: {} ({}), which changed after this buffer was rendered and which this buffer did not edit.",
            title, gnid ) }

/// The same warning, for a 'gnid' the graph deleted.
fn left_deleted_as_the_graph_has_it (
  title : &str,
  gnid  : &str,
) -> String {
  format! ( "Left as the graph has it: {} ({}), which was deleted after this buffer was rendered and which this buffer did not edit.",
            title, gnid ) }

/// Retitle ra on a connection of its own.
fn retitle_ra_elsewhere (
  f : &mut Fixture,
) {
  change_elsewhere (f, "ra", &|view : &str|
    view . replace ("ra as first written", "ra retitled")); }

#[test]
fn an_unedited_stale_node_is_left_out_of_the_save
  () -> Result<(), Box<dyn Error>> {
  // Connection one's view of r still shows ra as first written; it
  // edits only rb. ra is left as the graph has it, with a warning, and
  // the redraw shows ra's new title.
  let mut f : Fixture = fixture () ?;
  let mut first : ViewsState = new_connection ();
  let r_in_first : String = open_view (&mut f, &mut first, "r", "r-first");
  retitle_ra_elsewhere (&mut f);
  let result : String = save (
    &mut f, &mut first, "r-first",
    &with_rb_body_edited (&r_in_first), Some (&r_in_first) );
  assert! ( saved (&result), "{}", result );
  assert! ( file_text (&f, "rb") . contains ("rb's second line, edited") );
  assert! ( file_text (&f, "ra") . contains ("ra retitled") );
  assert! ( result . contains (
              &left_as_the_graph_has_it ("ra as first written", "ra") )
            && ! result . contains ("Left as the graph has it: r,")
            && ! result . contains ("Left as the graph has it: rb"),
            "{}", result );
  let redrawn : String = extract_string_field_from_sexp (&result, "content")
    . expect ("content");
  assert! ( redrawn . contains ("ra retitled")
            && ! redrawn . contains ("ra as first written"),
            "{}", redrawn );
  Ok (()) }

#[test]
fn an_edited_stale_node_still_refuses_the_save
  () -> Result<(), Box<dyn Error>> {
  let mut f : Fixture = fixture () ?;
  let mut first : ViewsState = new_connection ();
  let r_in_first : String = open_view (&mut f, &mut first, "r", "r-first");
  retitle_ra_elsewhere (&mut f);
  let result : String = save (
    &mut f, &mut first, "r-first",
    &with_rb_body_edited (&r_in_first)
      . replace ("ra as first written", "ra as I would have it"),
    Some (&r_in_first) );
  assert! ( errors_of (&result) . contains ("changed after the buffer was rendered")
            && errors_of (&result) . contains ("- ID: ra")
            && ! result . contains ("Left as the graph has it"),
            "{}", result );
  assert! ( ! file_text (&f, "rb") . contains ("edited")
            && file_text (&f, "ra") . contains ("ra retitled") );
  Ok (()) }

#[test]
fn deleting_a_stale_node_still_refuses_the_save
  () -> Result<(), Box<dyn Error>> {
  // A delete is an edit, never unedited.
  let mut f : Fixture = fixture () ?;
  let mut first : ViewsState = new_connection ();
  let r_in_first : String = open_view (&mut f, &mut first, "r", "r-first");
  retitle_ra_elsewhere (&mut f);
  let result : String = save (
    &mut f, &mut first, "r-first",
    &with_rb_body_edited (&r_in_first)
      . replacen ("(gnid ra)", "(gnid ra) (editRequest delete)", 1),
    Some (&r_in_first) );
  assert! ( errors_of (&result) . contains ("changed after the buffer was rendered")
            && errors_of (&result) . contains ("- ID: ra"),
            "{}", result );
  assert! ( file_path (&f, "main", "ra") . exists ()
            && ! file_text (&f, "rb") . contains ("edited") );
  Ok (()) }

#[test]
fn an_unedited_node_deleted_elsewhere_stays_deleted
  () -> Result<(), Box<dyn Error>> {
  // Connection two deletes ra, which also drops it from r's content.
  // Connection one's view still shows both, unedited: each is left as
  // the graph has it, so ra is not recreated.
  let mut f : Fixture = fixture () ?;
  let mut first : ViewsState = new_connection ();
  let r_in_first : String = open_view (&mut f, &mut first, "r", "r-first");
  change_elsewhere (&mut f, "ra", &|view : &str|
    view . replacen ("(gnid ra)", "(gnid ra) (editRequest delete)", 1));
  assert! ( ! file_path (&f, "main", "ra") . exists () );
  let result : String = save (
    &mut f, &mut first, "r-first",
    &with_rb_body_edited (&r_in_first), Some (&r_in_first) );
  assert! ( saved (&result), "{}", result );
  assert! ( ! file_path (&f, "main", "ra") . exists ()
            && ! file_text (&f, "r") . contains ("- ra")
            && file_text (&f, "rb") . contains ("rb's second line, edited") );
  assert! ( result . contains (
              &left_deleted_as_the_graph_has_it ("ra as first written", "ra") )
            && result . contains (
              &left_as_the_graph_has_it ("r, a root", "r") ),
            "{}", result );
  Ok (()) }

#[test]
fn an_unedited_node_moved_elsewhere_is_not_moved_back
  () -> Result<(), Box<dyn Error>> {
  // Connection two moves ra from main to other, both owned. Connection
  // one's view still shows ra's home as main, unedited: no move back.
  let mut f : Fixture = fixture () ?;
  let mut first : ViewsState = new_connection ();
  let r_in_first : String = open_view (&mut f, &mut first, "r", "r-first");
  change_elsewhere (&mut f, "ra", &|view : &str| {
    assert! ( view . contains ("(gnid ra) (skgRepos (nodeRepo main))"), "{}", view );
    view . replacen ( "(gnid ra) (skgRepos (nodeRepo main))",
                      "(gnid ra) (skgRepos (nodeRepo other))", 1 ) });
  assert! ( file_path (&f, "other", "ra") . exists ()
            && ! file_path (&f, "main", "ra") . exists () );
  let result : String = save (
    &mut f, &mut first, "r-first",
    &with_rb_body_edited (&r_in_first), Some (&r_in_first) );
  assert! ( saved (&result), "{}", result );
  assert! ( file_path (&f, "other", "ra") . exists ()
            && ! file_path (&f, "main", "ra") . exists ()
            && file_text (&f, "rb") . contains ("rb's second line, edited") );
  assert! ( result . contains (
              &left_as_the_graph_has_it ("ra as first written", "ra") ),
            "{}", result );
  Ok (()) }

/// Rewrite foreign n on disk and reload it, as the repo watcher would.
fn rewrite_and_reload_n (
  f : &mut Fixture,
) -> Result<(), Box<dyn Error>> {
  let n_path : PathBuf = file_path (f, "theirs", "n");
  fs::write ( &n_path, "pid: n\ntitle: n as they rewrote it\n" ) ?;
  let (notice, _unread) = reload_changed_skg_files (
    &f . env, & BTreeSet::from ([ n_path ]) );
  assert! ( ! notice . files_reloaded . is_empty (), "{:?}", notice . files_reloaded );
  Ok (()) }

#[test]
fn an_unedited_stale_foreign_node_is_not_forked
  () -> Result<(), Box<dyn Error>> {
  let mut f : Fixture = fixture () ?;
  let mut first : ViewsState = new_connection ();
  let r_in_first : String = open_view (&mut f, &mut first, "r", "r-first");
  rewrite_and_reload_n (&mut f) ?;
  let result : String = save (
    &mut f, &mut first, "r-first",
    &with_rb_body_edited (&r_in_first), Some (&r_in_first) );
  assert! ( saved (&result), "{}", result );
  assert! ( result . contains (
              &left_as_the_graph_has_it ("n as they wrote it", "n") ),
            "{}", result );
  assert! ( file_text (&f, "rb") . contains ("rb's second line, edited")
            && fs::read_to_string ( file_path (&f, "theirs", "n") ) ?
               . contains ("n as they rewrote it") );
  assert! ( ! texts_of_files_in (&f, "main") . iter ()
              . chain ( texts_of_files_in (&f, "other") . iter () )
              . any ( |text| text . contains ("overrides") ),
            "no clone of n" );
  Ok (()) }

#[test]
fn an_edited_stale_foreign_node_still_refuses_the_save
  () -> Result<(), Box<dyn Error>> {
  let mut f : Fixture = fixture () ?;
  let mut first : ViewsState = new_connection ();
  let r_in_first : String = open_view (&mut f, &mut first, "r", "r-first");
  rewrite_and_reload_n (&mut f) ?;
  let result : String = save (
    &mut f, &mut first, "r-first",
    &with_rb_body_edited (&r_in_first)
      . replace ("n as they wrote it", "n as I would have it"),
    Some (&r_in_first) );
  assert! ( errors_of (&result) . contains ("changed after the buffer was rendered")
            && errors_of (&result) . contains ("- ID: n"),
            "{}", result );
  assert! ( ! file_text (&f, "rb") . contains ("edited") );
  Ok (()) }

#[test]
fn a_write_protected_occurrence_as_the_baseline_drew_it_saves
  () -> Result<(), Box<dyn Error>> {
  // q's view shows ra twice, the repeat write-protected. After ra is
  // retitled elsewhere, both still show its first title.
  let mut f : Fixture = fixture () ?;
  let mut first : ViewsState = new_connection ();
  let q_in_first : String = open_view (&mut f, &mut first, "q", "q-first");
  assert! ( q_in_first . matches ("ra as first written") . count () == 2
            && q_in_first . contains ("writeProtected"),
            "{}", q_in_first );
  retitle_ra_elsewhere (&mut f);
  let result : String = save (
    &mut f, &mut first, "q-first",
    &q_in_first . replace ("qb as first written", "qb edited"),
    Some (&q_in_first) );
  assert! ( saved (&result), "{}", result );
  assert! ( file_text (&f, "qb") . contains ("qb edited")
            && file_text (&f, "ra") . contains ("ra retitled") );
  assert! ( result . contains (
              &left_as_the_graph_has_it ("ra as first written", "ra") ),
            "{}", result );
  Ok (()) }

#[test]
fn a_node_shown_only_write_protected_as_the_baseline_drew_it_saves
  () -> Result<(), Box<dyn Error>> {
  // A write-protect request makes r's view show ra only
  // write-protected, so the baseline writes nothing of ra. After ra
  // is retitled elsewhere, the view still shows its first title.
  let mut f : Fixture = fixture () ?;
  let mut first : ViewsState = new_connection ();
  let r_in_first : String = open_view (&mut f, &mut first, "r", "r-first");
  let ra_metadata : &str = "(gnid ra) (skgRepos (nodeRepo main))";
  assert! ( r_in_first . matches (ra_metadata) . count () == 1, "{}", r_in_first );
  let write_protecting : String = save (
    &mut f, &mut first, "r-first",
    &r_in_first . replacen (
      ra_metadata,
      &format! ("{} (viewRequests writeProtectedView)", ra_metadata), 1 ),
    Some (&r_in_first) );
  assert! ( saved (&write_protecting), "{}", write_protecting );
  let r_redrawn : String = extract_string_field_from_sexp (&write_protecting, "content")
    . expect ("content");
  assert! ( r_redrawn . matches ("(gnid ra)") . count () == 1
            && r_redrawn . lines () . any ( |line|
                 line . contains ("(gnid ra)") && line . contains ("writeProtected") ),
            "{}", r_redrawn );
  retitle_ra_elsewhere (&mut f);
  let result : String = save (
    &mut f, &mut first, "r-first",
    &with_rb_body_edited (&r_redrawn), Some (&r_redrawn) );
  assert! ( saved (&result), "{}", result );
  assert! ( file_text (&f, "rb") . contains ("rb's second line, edited")
            && file_text (&f, "ra") . contains ("ra retitled") );
  Ok (()) }
