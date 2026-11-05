// cargo nextest run --test grouped_saves -E 'test(repo_watch::)'
//
// The repo watcher (server/repo_watch/) reloads '.skg' files that
// change on disk while the server runs: into the graph as a new
// runtime generation, to each connection as one "repo-files-reloaded"
// message naming the views to rerender. The server's own writes are
// not reloaded again. Each test runs its own watcher over its own
// copy of the fixture, with a short quiet interval.

use skg::repo_watch::reload::RepoChangeNotice;
use skg::repo_watch::own_writes::own_write_awaits_its_event;
use skg::repo_watch::watcher::{RepoWatcherHandle, start_repo_watcher};
use skg::serve::ViewsState;
use skg::serve::handlers::repo_files_reloaded::{
  deliver_repo_change_notices, files_reloaded_per_skgrepo, warnings_of_notice};
use skg::serve::handlers::rerender_all_views::handle_rerender_all_views_request;
use skg::serve::handlers::save_buffer::handle_save_buffer_request;
use skg::serve::handlers::single_root_view::handle_single_root_view_request;
use skg::skgrepo_sets::SkgrepoRestriction;
use skg::test_utils::{
  copy_dir_all, extract_string_field_from_sexp, graph_handle_from_config,
  load_config_with_overrides, read_all_lp_messages, skg_env_from_parts};
use skg::dbs::init::create_empty_tantivy_index;
use skg::types::env::SkgEnv;
use skg::types::misc::{ID, SkgConfig, SkgrepoName, TantivyIndex};
use skg::types::views_state::OpenViews;

use crate::tcp_stream_pair::mk_test_tcp_stream_pair;
use std::error::Error;
use std::fs;
use std::io::{BufReader, Write};
use std::net::TcpStream;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::thread::sleep;
use std::time::{Duration, Instant};
use tempfile::TempDir;
use tokio::sync::broadcast::Receiver;
use tokio::sync::broadcast::error::TryRecvError;

/// Long enough for any batch to arrive on a loaded test machine.
const NOTICE_TIMEOUT : Duration = Duration::from_secs (15);

/// How long a test waits to be sure no further batch comes: many
/// times the fixture's quiet interval (150 ms).
const NO_NOTICE_WAIT : Duration = Duration::from_millis (1500);

struct Fixture {
  dir      : TempDir,
  config   : SkgConfig,
  env      : SkgEnv,
  _watcher : RepoWatcherHandle,
}

fn fixture (
) -> Result<Fixture, Box<dyn Error>> {
  let dir : TempDir = TempDir::new () ?;
  copy_dir_all ( Path::new ("tests/repo_watch/fixtures"), dir . path () ) ?;
  let root : &Path = dir . path ();
  let config : SkgConfig = load_config_with_overrides (
    root . join ("skgconfig.toml") . to_str () . unwrap (),
    Some ("repo-watch"),
    &[ ("theirs", root . join ("theirs")),
       ("mine",   root . join ("mine")) ] ) ?;
  let config : SkgConfig = SkgConfig {
    tantivy_folder : root . join ("tantivy"),
    data_root      : root . to_path_buf (),
    .. config };
  let tantivy : TantivyIndex =
    create_empty_tantivy_index (&config . tantivy_folder) ?;
  let env : SkgEnv = skg_env_from_parts (
    &config, &tantivy, &graph_handle_from_config (&config) ?);
  let watcher : RepoWatcherHandle = start_repo_watcher (env . clone ()) ?;
  sleep ( Duration::from_millis (100) ); // the watcher thread starts watching
  Ok ( Fixture { dir, config, env, _watcher : watcher } ) }

fn new_connection (
) -> ViewsState {
  ViewsState { open_views     : OpenViews::new (),
               graph_straddle : None, } }

fn skg_path (
  f       : &Fixture,
  skgrepo : &str,
  pid     : &str,
) -> PathBuf {
  f . dir . path () . join (skgrepo) . join ( format! ("{}.skg", pid) ) }

fn write_skg_file_as_another_writer (
  f       : &Fixture,
  skgrepo : &str,
  pid     : &str,
  text    : &str,
) {
  fs::write ( skg_path (f, skgrepo, pid), text ) . unwrap (); }

/// The next notice, waiting for it.
fn next_notice (
  receiver : &mut Receiver<Arc<RepoChangeNotice>>,
) -> Arc<RepoChangeNotice> {
  let started : Instant = Instant::now ();
  loop {
    match receiver . try_recv () {
      Ok (notice) => return notice,
      Err (TryRecvError::Empty) if started . elapsed () < NOTICE_TIMEOUT =>
        sleep ( Duration::from_millis (20) ),
      Err (e) => panic! ("no repo change notice arrived: {:?}", e), }}}

fn assert_no_notice_comes (
  receiver : &mut Receiver<Arc<RepoChangeNotice>>,
) {
  sleep (NO_NOTICE_WAIT);
  match receiver . try_recv () {
    Err (TryRecvError::Empty) => {},
    other => panic! ("an unexpected repo change notice: {:?}", other), }}

/// Open a content view of 'gnid' as 'view_id': its text, and the
/// generation it was rendered at.
fn open_view (
  f           : &Fixture,
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

/// Save 'buffer' as 'view_id', rendered at 'rendered_at' as
/// 'baseline': the terminal save-result message.
fn save (
  f           : &mut Fixture,
  views_state : &mut ViewsState,
  view_id     : &str,
  buffer      : &str,
  baseline    : &str,
  rendered_at : u64,
) -> String {
  let request : String = format! (
    "((request . \"save buffer\") (view-id . \"{}\") (rendered-at . \"{}\"))",
    view_id, rendered_at );
  let sexp_string_body = |text : &str| -> String {
    text . replace ('\\', "\\\\") . replace ('"', "\\\"") };
  let envelope : String = format! (
    "((saved-buffer \"{}\") (baseline \"{}\") (other-views ()))",
    sexp_string_body (buffer), sexp_string_body (baseline) );
  let (mut client_writes, server_reads) : (TcpStream, TcpStream) =
    mk_test_tcp_stream_pair ();
  write! ( client_writes, "Content-Length: {}\r\n\r\n{}",
           envelope . len (), envelope ) . unwrap ();
  let (mut server_writes, client_reads) : (TcpStream, TcpStream) =
    mk_test_tcp_stream_pair ();
  let restriction : SkgrepoRestriction = SkgrepoRestriction::unrestricted (&f . config);
  handle_save_buffer_request (
    &mut BufReader::new (server_reads), &mut server_writes, &request,
    &mut f . env, views_state, &restriction );
  drop (server_writes);
  read_all_lp_messages (&mut BufReader::new (client_reads))
    . into_iter ()
    . find ( |message| message . contains ("save-result") )
    . expect ("a save-result") }

/// What the connection's loop sends its client, between requests, of
/// the notices 'receiver' holds.
fn delivered_messages (
  f           : &Fixture,
  receiver    : &mut Receiver<Arc<RepoChangeNotice>>,
  views_state : &mut ViewsState,
) -> Vec<String> {
  let (mut stream, read_end) : (TcpStream, TcpStream) = mk_test_tcp_stream_pair ();
  deliver_repo_change_notices (
    &mut stream, receiver, views_state,
    &SkgrepoRestriction::unrestricted (&f . config) );
  drop (stream);
  read_all_lp_messages (&mut BufReader::new (read_end)) }

/// The client's answer to a "repo-files-reloaded" message: a
/// rerender of every open view it does not exclude. The rerendered
/// views' texts.
fn rerender_views (
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

fn title_in_graph (
  f   : &Fixture,
  pid : &str,
) -> Option<String> {
  f . env . runtime_snapshot () . graph . nodes
    . get ( &ID::from (pid) )
    . map ( |node| node . title . clone () ) }

#[test]
fn files_written_into_a_foreign_and_an_owned_repo_are_reloaded
  () -> Result<(), Box<dyn Error>> {
  let f : Fixture = fixture () ?;
  let mut connection : ViewsState = new_connection ();
  let mut receiver : Receiver<Arc<RepoChangeNotice>> = f . env . runtime . subscribe_to_repo_change_notices ();
  let (a_view, _) : (String, u64) = open_view (&f, &mut connection, "a", "a-view");
  assert! ( a_view . contains ("b as first written"), "{}", a_view );
  let generation_before : u64 = f . env . runtime_snapshot () . generation;
  write_skg_file_as_another_writer (&f, "theirs", "u", "pid: u\ntitle: u as they wrote it\n");
  write_skg_file_as_another_writer (&f, "mine", "n", "pid: n\ntitle: n from another writer\n");
  write_skg_file_as_another_writer (&f, "mine", "b", "pid: b\ntitle: b rewritten on disk\n");
  let notice : Arc<RepoChangeNotice> = next_notice (&mut receiver);
  assert_eq! ( notice . files_reloaded,
               vec! [ (SkgrepoName::from ("theirs"), 1),
                      (SkgrepoName::from ("mine"), 2) ] );
  assert! ( notice . problems . is_empty (), "{:?}", notice . problems );
  assert! ( notice . foreign_files_reloaded );
  assert_eq! ( notice . generation, Some (generation_before + 1) );
  assert_eq! ( f . env . runtime_snapshot () . generation, generation_before + 1 );
  assert_eq! ( title_in_graph (&f, "b") . as_deref (), Some ("b rewritten on disk") );
  { // A fresh view shows each new node.
    let mut other : ViewsState = new_connection ();
    let (u_view, _) : (String, u64) = open_view (&f, &mut other, "u", "u-view");
    assert! ( u_view . contains ("u as they wrote it"), "{}", u_view );
    let (n_view, _) : (String, u64) = open_view (&f, &mut other, "n", "n-view");
    assert! ( n_view . contains ("n from another writer"), "{}", n_view ); }
  Ok (()) }

#[test]
fn a_connection_is_told_and_its_open_view_rerenders
  () -> Result<(), Box<dyn Error>> {
  let f : Fixture = fixture () ?;
  let mut connection : ViewsState = new_connection ();
  let mut receiver : Receiver<Arc<RepoChangeNotice>> = f . env . runtime . subscribe_to_repo_change_notices ();
  let (_a_view, _) : (String, u64) = open_view (&f, &mut connection, "a", "a-view");
  let (_t_view, _) : (String, u64) = open_view (&f, &mut connection, "t", "t-view");
  let mut receiver_of_connection : Receiver<Arc<RepoChangeNotice>> =
    f . env . runtime . subscribe_to_repo_change_notices ();
  write_skg_file_as_another_writer (&f, "mine", "b", "pid: b\ntitle: b rewritten again\n");
  let _notice : Arc<RepoChangeNotice> = next_notice (&mut receiver);
  let messages : Vec<String> =
    delivered_messages (&f, &mut receiver_of_connection, &mut connection);
  assert_eq! ( messages . len (), 1, "{:?}", messages );
  assert! ( messages [0] . starts_with ("((response-type repo-files-reloaded)"),
            "{}", messages [0] );
  assert! ( messages [0] . contains ("(reloaded ((mine 1)))"), "{}", messages [0] );
  assert! ( messages [0] . contains ("(warnings ())"), "{}", messages [0] );
  assert! ( messages [0] . contains ("(view-ids (a-view))"), "{}", messages [0] );
  { // Nothing is told twice.
    assert! ( delivered_messages (&f, &mut receiver_of_connection, &mut connection)
              . is_empty () ); }
  { // The client asks for the rerender; the open view shows the new title.
    let rerendered : Vec<String> = rerender_views (&f, &mut connection);
    let a_rerendered : &String = rerendered . iter ()
      . find ( |message| message . contains ("a-view") )
      . expect ("a rerender of a-view");
    assert! ( a_rerendered . contains ("b rewritten again"), "{}", a_rerendered ); }
  Ok (()) }

#[test]
fn a_burst_of_files_is_reloaded_once
  () -> Result<(), Box<dyn Error>> {
  let f : Fixture = fixture () ?;
  let mut receiver : Receiver<Arc<RepoChangeNotice>> = f . env . runtime . subscribe_to_repo_change_notices ();
  for index in 0 .. 25 {
    write_skg_file_as_another_writer (
      &f, "mine", &format! ("burst{}", index),
      &format! ("pid: burst{}\ntitle: burst {}\n", index, index) ); }
  let notice : Arc<RepoChangeNotice> = next_notice (&mut receiver);
  assert_eq! ( notice . files_reloaded,
               vec! [ (SkgrepoName::from ("mine"), 25) ] );
  assert_no_notice_comes (&mut receiver);
  assert_eq! ( title_in_graph (&f, "burst24") . as_deref (), Some ("burst 24") );
  Ok (()) }

#[test]
fn a_bad_file_warns_and_the_old_generation_serves
  () -> Result<(), Box<dyn Error>> {
  let f : Fixture = fixture () ?;
  let mut receiver : Receiver<Arc<RepoChangeNotice>> = f . env . runtime . subscribe_to_repo_change_notices ();
  let generation_before : u64 = f . env . runtime_snapshot () . generation;
  write_skg_file_as_another_writer (&f, "mine", "b", "pid: b\ntitle: [never closed\n");
  let notice : Arc<RepoChangeNotice> = next_notice (&mut receiver);
  assert_eq! ( notice . generation, None );
  assert! ( notice . files_reloaded . is_empty () );
  assert_eq! ( notice . problems . len (), 1, "{:?}", notice . problems );
  assert! ( notice . problems [0] . message . contains ("Node b changed on disk but could not be read"),
            "{}", notice . problems [0] . message );
  assert_eq! ( f . env . runtime_snapshot () . generation, generation_before );
  assert_eq! ( title_in_graph (&f, "b") . as_deref (), Some ("b as first written") );
  { // Each connection is told, as a warning, and of no file reloaded.
    let restriction : SkgrepoRestriction = SkgrepoRestriction::unrestricted (&f . config);
    let warnings : Vec<String> = warnings_of_notice (&notice, &restriction);
    assert_eq! ( warnings . len (), 1, "{:?}", warnings );
    assert! ( files_reloaded_per_skgrepo (
                & [ notice . clone () ], &restriction ) . is_empty () ); }
  { // Fixed, it is reloaded.
    write_skg_file_as_another_writer (&f, "mine", "b", "pid: b\ntitle: b fixed\n");
    let fixed : Arc<RepoChangeNotice> = next_notice (&mut receiver);
    assert_eq! ( fixed . files_reloaded, vec! [ (SkgrepoName::from ("mine"), 1) ] );
    { // The count is no warning; two notices' counts add up per skgrepo.
      let restriction : SkgrepoRestriction = SkgrepoRestriction::unrestricted (&f . config);
      assert! ( warnings_of_notice (&fixed, &restriction) . is_empty () );
      assert_eq! ( files_reloaded_per_skgrepo (
                     & [ fixed . clone (), fixed . clone () ], &restriction ),
                   vec! [ (SkgrepoName::from ("mine"), 2) ] ); }
    assert_eq! ( title_in_graph (&f, "b") . as_deref (), Some ("b fixed") ); }
  Ok (()) }

#[test]
fn the_servers_own_save_is_not_reloaded_again
  () -> Result<(), Box<dyn Error>> {
  let mut f : Fixture = fixture () ?;
  let mut connection : ViewsState = new_connection ();
  let mut receiver : Receiver<Arc<RepoChangeNotice>> = f . env . runtime . subscribe_to_repo_change_notices ();
  let (a_view, rendered_at) : (String, u64) = open_view (&f, &mut connection, "a", "a-view");
  let result : String = save (
    &mut f, &mut connection, "a-view",
    &a_view . replace ("a as first written", "a saved by the server")
            . replace ("b as first written", "b saved by the server"),
    &a_view, rendered_at );
  assert! ( result . contains ("a saved by the server"), "{}", result );
  assert! ( fs::read_to_string ( skg_path (&f, "mine", "b") ) ?
            . contains ("b saved by the server") );
  let generation_after_save : u64 = f . env . runtime_snapshot () . generation;
  for pid in ["a", "b"] { // recorded as the server's own
    assert! ( own_write_awaits_its_event ( &skg_path (&f, "mine", pid) ), "{}", pid ); }
  assert_no_notice_comes (&mut receiver);
  assert_eq! ( f . env . runtime_snapshot () . generation, generation_after_save );
  for pid in ["a", "b"] { // the watcher saw each write, recognized it, and dropped it
    assert! ( ! own_write_awaits_its_event ( &skg_path (&f, "mine", pid) ), "{}", pid ); }
  { // Another writer's change to the same file afterwards is reloaded.
    write_skg_file_as_another_writer (&f, "mine", "a", "pid: a\ntitle: a by another writer\ncontains:\n- b\n");
    let notice : Arc<RepoChangeNotice> = next_notice (&mut receiver);
    assert_eq! ( notice . files_reloaded, vec! [ (SkgrepoName::from ("mine"), 1) ] ); }
  Ok (()) }

/// A buffer with unsaved edits, rendered before a reload changed b,
/// is saved with an edit of b and an edit of a. The save is refused
/// whole: it names b, and writes neither b nor a.
#[test]
fn a_buffer_rendered_before_a_reload_cannot_write_a_reloaded_node
  () -> Result<(), Box<dyn Error>> {
  let mut f : Fixture = fixture () ?;
  let mut connection : ViewsState = new_connection ();
  let mut receiver : Receiver<Arc<RepoChangeNotice>> = f . env . runtime . subscribe_to_repo_change_notices ();
  let (a_view, rendered_at) : (String, u64) = open_view (&f, &mut connection, "a", "a-view");
  write_skg_file_as_another_writer (&f, "mine", "b", "pid: b\ntitle: b from another writer\n");
  let _notice : Arc<RepoChangeNotice> = next_notice (&mut receiver);
  let result : String = save (
    &mut f, &mut connection, "a-view",
    &a_view . replace ("b as first written", "b from the old buffer")
            . replace ("a as first written", "a from the old buffer"),
    &a_view, rendered_at );
  assert! ( result . contains ("changed after the buffer was rendered")
            && result . contains ("- ID: b")
            && ! result . contains ("- ID: a"),
            "{}", result );
  assert! ( fs::read_to_string ( skg_path (&f, "mine", "b") ) ?
            . contains ("b from another writer") );
  assert! ( fs::read_to_string ( skg_path (&f, "mine", "a") ) ?
            . contains ("a as first written"),
            "the refused save writes no node, not even a fresh one" );
  assert_eq! ( title_in_graph (&f, "a") . as_deref (), Some ("a as first written") );
  Ok (()) }
