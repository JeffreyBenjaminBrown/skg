// Helpers for tests that drive the server's handlers as a client
// does: open a content view, keep its text as the view's baseline,
// and save a buffer with that baseline, on one or more connections
// ('ViewsState's) sharing one runtime. Declared in
// 'tests/grouped_saves.rs'.

use skg::dbs::init::create_empty_tantivy_index;
use skg::serve::ViewsState;
use skg::serve::handlers::save_buffer::handle_save_buffer_request;
use skg::serve::handlers::single_root_view::handle_single_root_view_request;
use skg::skgrepo_sets::SkgrepoRestriction;
use skg::test_utils::{
  empty_views_state,
  copy_dir_all, extract_string_field_from_sexp, graph_handle_from_config,
  load_config_with_overrides, read_all_lp_messages, skg_env_from_parts};
use skg::types::env::SkgEnv;
use skg::types::config::SkgConfig;
use skg::types::misc::TantivyIndex;

use crate::tcp_stream_pair::mk_test_tcp_stream_pair;
use std::error::Error;
use std::fs;
use std::io::{BufReader, Write};
use std::net::TcpStream;
use std::path::{Path, PathBuf};
use tempfile::TempDir;

pub struct Fixture {
  pub dir    : TempDir,
  pub config : SkgConfig,
  pub env    : SkgEnv,
}

/// A copy of the fixture folder 'fixtures' in a temporary folder,
/// with a runtime over it. 'skgrepos' names each skgrepo of its
/// config, whose folder is the one of that name in the copy (made if
/// absent); 'name' names the test's stores.
pub fn fixture_from (
  fixtures : &str,
  name     : &str,
  skgrepos : &[&str],
) -> Result<Fixture, Box<dyn Error>> {
  let dir : TempDir = TempDir::new () ?;
  copy_dir_all ( Path::new (fixtures), dir . path () ) ?;
  let root : &Path = dir . path ();
  for skgrepo in skgrepos {
    fs::create_dir_all ( root . join (skgrepo) ) ?; }
  let skgrepo_paths : Vec<(&str, PathBuf)> =
    skgrepos . iter () . map ( |skgrepo| (*skgrepo, root . join (skgrepo)) ) . collect ();
  let config : SkgConfig = load_config_with_overrides (
    root . join ("skgconfig.toml") . to_str () . unwrap (),
    Some (name), &skgrepo_paths ) ?;
  let config : SkgConfig = SkgConfig {
    tantivy_folder : root . join ("tantivy"),
    data_root      : root . to_path_buf (),
    .. config };
  let tantivy : TantivyIndex =
    create_empty_tantivy_index (&config . tantivy_folder) ?;
  let env : SkgEnv = skg_env_from_parts (
    &config, &tantivy, &graph_handle_from_config (&config) ?);
  Ok ( Fixture { dir, config, env } ) }

/// A client connection: the views it holds.
pub fn new_connection (
) -> ViewsState {
  empty_views_state () }

/// Open a content view of 'gnid' as 'view_id': its text, which the
/// client keeps as the view's baseline.
pub fn open_view (
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
pub fn save (
  f           : &mut Fixture,
  views_state : &mut ViewsState,
  view_id     : &str,
  buffer      : &str,
  baseline    : Option<&str>,
) -> String {
  save_with_other_views (f, views_state, view_id, buffer, baseline, &[])
    . pop ()
    . expect ("a terminal message") }

/// As 'save', where the client also holds 'clean_other_views', each
/// unedited since the server drew it: every message the save sends,
/// the collateral views among them, the terminal one last.
pub fn save_with_other_views (
  f                 : &mut Fixture,
  views_state       : &mut ViewsState,
  view_id           : &str,
  buffer            : &str,
  baseline          : Option<&str>,
  clean_other_views : &[&str],
) -> Vec<String> {
  let request : String = format! (
    "((request . \"save buffer\") (view-id . \"{}\"))",
    view_id );
  let other_views : String = clean_other_views . iter ()
    . map ( |other| format! ( "((view-id \"{}\") (dirty false))", other ))
    . collect::<Vec<String>> () . join (" ");
  let envelope : String = format! (
    "((saved-buffer \"{}\"){} (other-views ({})))",
    sexp_string_body (buffer),
    baseline . map ( |text| format! (" (baseline \"{}\")", sexp_string_body (text)) )
      . unwrap_or_default (),
    other_views );
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
  read_all_lp_messages (&mut BufReader::new (client_reads)) }

/// The text of the collateral view 'view_id' among a save's
/// 'messages', if the save redrew it.
pub fn collateral_view (
  messages : &[String],
  view_id  : &str,
) -> Option<String> {
  messages . iter ()
    . filter ( |message| message . starts_with ("((response-type collateral-view)") )
    . find ( |message| message . contains (&format! ("(view-id {})", view_id))
                       || message . contains (&format! ("(view-id \"{}\")", view_id)) )
    . and_then ( |message| extract_string_field_from_sexp (message, "content") ) }

/// The view a successful save drew, from its terminal message.
pub fn saved_view (
  save_result : &str,
) -> String {
  extract_string_field_from_sexp (save_result, "content")
    . unwrap_or_else ( || panic! ( "no saved view in:\n{}", save_result )) }

/// 'text' escaped to sit between the quotes of a sexp string.
pub fn sexp_string_body (
  text : &str,
) -> String {
  text . replace ('\\', "\\\\") . replace ('"', "\\\"") }

pub fn errors_of (
  save_result : &str,
) -> String {
  save_result . split ("(errors") . nth (1) . unwrap_or ("") . to_string () }

/// What follows the warnings' key in a terminal save message: its
/// warnings, then whatever entries come after them.
pub fn warnings_of (
  save_result : &str,
) -> String {
  save_result . split ("(warnings") . nth (1) . unwrap_or ("") . to_string () }

/// Whether a terminal save message is a save-result without errors.
pub fn saved (
  save_result : &str,
) -> bool {
  save_result . starts_with ("((response-type save-result)")
  && errors_of (save_result) . starts_with (" ())") }

/// The warning a save gives for leaving 'gnid', titled 'title' in the
/// buffer, as the graph has it, after the graph changed it.
pub fn left_as_the_graph_has_it (
  title : &str,
  gnid  : &str,
) -> String {
  format! ( "Left as the graph has it: {} ({}), which changed after this buffer was rendered and which this buffer did not edit.",
            title, gnid ) }
