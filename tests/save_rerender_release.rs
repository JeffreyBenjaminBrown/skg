// cargo nextest run --test grouped_saves -E 'test(save_rerender_release::)'
//
// Under a skgrepo restriction, a save whose rerender would show text
// from an overPrivateText telescope is written, and its rerender
// withheld: the challenge names the views withheld, and the "rerender
// views" request releases them ('server/serve/handlers/rerender_views.rs').
// The save itself is never sent again, since a save that made a node
// is stale once written ('server/serve/handlers/save_staleness.rs').
//
// Fixture: 'tele' is homed in 'main', which records its content; its
// title sits only in 'private', more private than 'main', so it is an
// overPrivateText telescope under the restriction "main".

use skg::serve::ViewsState;
use skg::serve::handlers::rerender_views::handle_rerender_views_request;
use skg::serve::handlers::save_buffer::handle_save_buffer_request;
use skg::serve::handlers::single_root_view::handle_single_root_view_request;
use skg::skgrepo_sets::SkgrepoRestriction;
use skg::test_utils::{
  copy_dir_all, extract_string_field_from_sexp, graph_handle_from_config,
  load_config_with_overrides, read_all_lp_messages, skg_env_from_parts};
use skg::dbs::init::create_empty_tantivy_index;
use skg::types::env::SkgEnv;
use skg::types::misc::{SkgConfig, SkgrepoName, TantivyIndex};
use skg::types::views_state::OpenViews;

use crate::tcp_stream_pair::mk_test_tcp_stream_pair;
use std::error::Error;
use std::fs;
use std::io::{BufReader, Write};
use std::net::TcpStream;
use std::path::Path;
use tempfile::TempDir;

struct Fixture {
  dir         : TempDir,
  env         : SkgEnv,
  restriction : SkgrepoRestriction,
}

fn fixture (
) -> Result<Fixture, Box<dyn Error>> {
  let dir : TempDir = TempDir::new () ?;
  copy_dir_all ( Path::new ("tests/save_rerender_release/fixtures"), dir . path () ) ?;
  let root : &Path = dir . path ();
  let config : SkgConfig = load_config_with_overrides (
    root . join ("skgconfig.toml") . to_str () . unwrap (),
    Some ("save-rerender-release"),
    &[ ("main", root . join ("main")),
       ("private", root . join ("private")) ] ) ?;
  let config : SkgConfig = SkgConfig {
    tantivy_folder : root . join ("tantivy"),
    data_root      : root . to_path_buf (),
    .. config };
  let tantivy : TantivyIndex =
    create_empty_tantivy_index (&config . tantivy_folder) ?;
  let env : SkgEnv = skg_env_from_parts (
    &config, &tantivy, &graph_handle_from_config (&config) ?);
  let restriction : SkgrepoRestriction =
    SkgrepoRestriction::named (&config, SkgrepoName::from ("main")) ?;
  Ok ( Fixture { dir, env, restriction } ) }

/// Open a content view of 'tele' as "tele-view" under the restriction,
/// approving its title: its text, and the generation it was rendered at.
fn open_tele_view (
  f           : &Fixture,
  views_state : &mut ViewsState,
) -> (String, String) {
  let (mut stream, read_end) : (TcpStream, TcpStream) = mk_test_tcp_stream_pair ();
  handle_single_root_view_request (
    &mut stream,
    "((request . \"single root content view\") (id . \"tele\") \
      (view-id . \"tele-view\") (approved-overPrivateText-pids \"tele\"))",
    &f . env, views_state, &f . restriction );
  drop (stream);
  let messages : Vec<String> = read_all_lp_messages (&mut BufReader::new (read_end));
  let response : &String = messages . last () . expect ("a content-view response");
  ( extract_string_field_from_sexp (response, "content") . expect ("content"),
    extract_string_field_from_sexp (response, "rendered-at") . expect ("rendered-at") ) }

/// Save 'buffer' as "tele-view", rendered at 'rendered_at', under the
/// restriction, with 'extra_fields' in the request: every message the
/// save sent.
fn save_tele_view (
  f            : &mut Fixture,
  views_state  : &mut ViewsState,
  buffer       : &str,
  rendered_at  : &str,
  extra_fields : &str,
) -> Vec<String> {
  let request : String = format! (
    "((request . \"save buffer\") (view-id . \"tele-view\") (rendered-at . \"{}\"){})",
    rendered_at, extra_fields );
  let envelope : String = format! (
    "((saved-buffer \"{}\") (baseline \"\") (other-views ()))", // the empty baseline, which writes nothing
    buffer . replace ('\\', "\\\\") . replace ('"', "\\\"") );
  let (mut client_writes, server_reads) : (TcpStream, TcpStream) =
    mk_test_tcp_stream_pair ();
  write! ( client_writes, "Content-Length: {}\r\n\r\n{}",
           envelope . len (), envelope ) . unwrap ();
  let (mut server_writes, client_reads) : (TcpStream, TcpStream) =
    mk_test_tcp_stream_pair ();
  let restriction : SkgrepoRestriction = f . restriction . clone ();
  handle_save_buffer_request (
    &mut BufReader::new (server_reads), &mut server_writes, &request,
    &mut f . env, views_state, &restriction );
  drop (server_writes);
  read_all_lp_messages (&mut BufReader::new (client_reads)) }

/// Send "rerender views" naming "tele-view", with 'extra_fields':
/// every message of the answer.
fn rerender_tele_view (
  f            : &Fixture,
  views_state  : &mut ViewsState,
  extra_fields : &str,
) -> Vec<String> {
  let (mut stream, read_end) : (TcpStream, TcpStream) = mk_test_tcp_stream_pair ();
  handle_rerender_views_request (
    &mut stream,
    &format! ( "((request . \"rerender views\") (view-ids \"tele-view\"){})",
               extra_fields ),
    &f . env, views_state, &f . restriction );
  drop (stream);
  read_all_lp_messages (&mut BufReader::new (read_end)) }

fn main_files_titled (
  f     : &Fixture,
  title : &str,
) -> Vec<String> {
  fs::read_dir ( f . dir . path () . join ("main") ) . unwrap ()
    . map ( |entry| fs::read_to_string ( entry . unwrap () . path () ) . unwrap () )
    . filter ( |text| text . contains ( &format! ("title: {}\n", title) ))
    . collect () }

#[test]
fn a_withheld_save_rerender_is_released_by_rerendering_its_views
  () -> Result<(), Box<dyn Error>> {
  let mut f : Fixture = fixture () ?;
  let mut connection : ViewsState =
    ViewsState { open_views : OpenViews::new (), graph_straddle : None };
  let (view, rendered_at) : (String, String) =
    open_tele_view (&f, &mut connection);
  assert! ( view . contains ("A title kept private"), "{}", view );
  let edited : String = format! ("{}*** a new node\n", view);
  { // The save writes the new node, and withholds its rerender.
    let messages : Vec<String> =
      save_tele_view (&mut f, &mut connection, &edited, &rendered_at, "");
    let terminal : &String = messages . last () . unwrap ();
    assert! ( terminal . starts_with (
                "((response-type overPrivateText-telescope-confirmation) \
                  (operation save-rerender) (pids (tele))" )
              && terminal . ends_with ("(view-ids (tele-view)))"),
              "{}", terminal );
    assert! ( ! messages . iter () . any ( |message|
                message . contains ("A title kept private") ),
              "{:?}", messages );
    assert_eq! ( main_files_titled (&f, "a new node") . len (), 1 ); }
  { // Sent again, the same save is stale: it wrote the child.
    let messages : Vec<String> = save_tele_view (
      &mut f, &mut connection, &edited, &rendered_at,
      " (approved-overPrivateText-pids \"tele\")" );
    let terminal : &String = messages . last () . unwrap ();
    assert! ( terminal . contains ("NOTHING WAS SAVED")
              && terminal . contains ("changed after the buffer was rendered"),
              "{}", terminal );
    assert_eq! ( main_files_titled (&f, "a new node") . len (), 1 ); }
  { // Unapproved, the rerender asks again, and sends no text.
    let messages : Vec<String> =
      rerender_tele_view (&f, &mut connection, "");
    assert_eq! ( messages . len (), 3, "{:?}", messages );
    assert! ( messages [0] . contains ("(operation rerender-views) (pids (tele))"),
              "{}", messages [0] );
    assert! ( messages [1] . contains ("(lock-views ())"), "{}", messages [1] );
    assert! ( ! messages . iter () . any ( |message|
                message . contains ("A title kept private") )); }
  let (fresh, fresh_rendered_at) : (String, String) =
  { // Approved, it sends the view as the save left it, the new node with its ID.
    let messages : Vec<String> = rerender_tele_view (
      &f, &mut connection, " (approved-overPrivateText-pids \"tele\")" );
    assert_eq! ( messages . len (), 3, "{:?}", messages );
    assert! ( messages [0] . contains ("(lock-views (tele-view))"), "{}", messages [0] );
    assert! ( messages [2] . contains (
                "rerender views includes title or body text selected below \
                 its node's home repo for PID tele." ),
              "{}", messages [2] );
    let fresh : String = extract_string_field_from_sexp (&messages [1], "content")
      . expect ("content");
    let new_pid : String = main_files_titled (&f, "a new node") [0]
      . lines () . find_map ( |line| line . strip_prefix ("pid: ") ) . unwrap () . to_string ();
    assert! ( fresh . contains ("A title kept private")
              && fresh . contains ( &format! ("(gnid {})", new_pid) )
              && fresh . contains ("a new node"),
              "{}", fresh );
    ( fresh,
      extract_string_field_from_sexp (&messages [1], "rendered-at")
        . expect ("rendered-at") ) };
  { // The released view saves: it is not stale.
    let messages : Vec<String> = save_tele_view (
      &mut f, &mut connection,
      &fresh . replace ("a new node", "a renamed node"), &fresh_rendered_at, "" );
    let terminal : &String = messages . last () . unwrap ();
    assert! ( terminal . contains ("(operation save-rerender)"), "{}", terminal );
    assert_eq! ( main_files_titled (&f, "a renamed node") . len (), 1 );
    assert_eq! ( main_files_titled (&f, "a new node") . len (), 0 ); }
  Ok (()) }
