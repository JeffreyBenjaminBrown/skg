// cargo nextest run --test grouped_saves -E 'test(reload_config::)'
//
// A rebuild of the session-lifetime stores re-reads the config file
// (server/serve/handlers/rebuild_session_lifetime_stores.rs), so an
// edited config takes effect without a restart, and an invalid one is
// refused with the old generation still serving.

use skg::serve::ViewsState;
use skg::serve::handlers::rebuild_session_lifetime_stores::{
  handle_rebuild_session_lifetime_stores_request,
  rebuild_session_lifetime_stores_in_place};
use skg::skgrepo_sets::SkgrepoRestriction;
use skg::test_utils::{
  copy_dir_all, extract_string_field_from_sexp, graph_handle_from_config,
  read_all_lp_messages, skg_env_from_parts};
use skg::dbs::filesystem::not_nodes::load_config;
use skg::dbs::init::create_empty_tantivy_index;
use skg::types::env::{RuntimeGeneration, SkgEnv};
use skg::types::misc::{ID, SkgConfig, SkgrepoName, TantivyIndex};
use skg::types::views_state::OpenViews;

use crate::tcp_stream_pair::mk_test_tcp_stream_pair;
use std::error::Error;
use std::fs;
use std::io::BufReader;
use std::net::TcpStream;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use tempfile::TempDir;

struct Fixture {
  _dir        : TempDir,
  config_path : PathBuf,
  env         : SkgEnv,
}

/// A runtime over 'main' and the more private 'second', its config at
/// a temporary path.
fn fixture (
) -> Result<Fixture, Box<dyn Error>> {
  let dir : TempDir = TempDir::new () ?;
  copy_dir_all ( Path::new ("tests/reload_config/fixtures"), dir . path () ) ?;
  let config_path : PathBuf = dir . path () . join ("skgconfig.toml");
  let config : SkgConfig = load_config ( config_path . to_str () . unwrap () ) ? . 0;
  let tantivy : TantivyIndex =
    create_empty_tantivy_index (&config . tantivy_folder) ?;
  let env : SkgEnv = skg_env_from_parts (
    &config, &tantivy, &graph_handle_from_config (&config) ?);
  Ok ( Fixture { _dir : dir, config_path, env } ) }

fn new_connection (
) -> ViewsState {
  ViewsState { open_views     : OpenViews::new (),
               graph_straddle : None, } }

/// Append to the config file the skgrepo 'third', more private than
/// 'second', and change its port to 1731.
fn add_third_skgrepo_and_change_port (
  f : &Fixture,
) -> Result<(), Box<dyn Error>> {
  let old : String = fs::read_to_string (&f . config_path) ?;
  fs::write ( &f . config_path,
              format! ( "{}\n[[repos]]\nname = \"third\"\npath = \"third\"\n",
                        old . replace ("port = 1730", "port = 1731") )) ?;
  Ok (()) }

fn skgrepo_names (
  runtime : &RuntimeGeneration,
) -> Vec<SkgrepoName> {
  runtime . config . ordered_skgrepos () }

fn graph_has (
  runtime : &RuntimeGeneration,
  pid     : &str,
) -> bool {
  runtime . graph . nodes . contains_key ( &ID::from (pid) ) }

#[test]
fn a_rebuild_reloads_an_edited_config
  () -> Result<(), Box<dyn Error>> {
  let mut f : Fixture = fixture () ?;
  let mut connection : ViewsState = new_connection ();
  let before : Arc<RuntimeGeneration> = f . env . runtime_snapshot ();
  assert_eq! ( skgrepo_names (&before), vec! [ SkgrepoName::from ("main"),
                                               SkgrepoName::from ("second") ] );
  assert! ( graph_has (&before, "b") && ! graph_has (&before, "c") );
  let restriction_before : SkgrepoRestriction =
    SkgrepoRestriction::unrestricted (&before . config);
  add_third_skgrepo_and_change_port (&f) ?;
  let warnings : Vec<String> =
    rebuild_session_lifetime_stores_in_place (&mut f . env, &mut connection) ?;
  let after : Arc<RuntimeGeneration> = f . env . runtime_snapshot ();
  assert_eq! ( skgrepo_names (&after),
               vec! [ SkgrepoName::from ("main"), SkgrepoName::from ("second"),
                      SkgrepoName::from ("third") ] );
  assert! ( graph_has (&after, "a") && graph_has (&after, "c") );
  assert_eq! (after . generation, before . generation + 1);
  assert! ( warnings . iter () . any ( |warning|
              warning . contains ("keeps listening on 1730") ),
            "{:?}", warnings );
  { // An unrestricted connection stays unrestricted, so it shows the new skgrepo.
    let restriction_after : SkgrepoRestriction =
      restriction_before . under_config (&after . config);
    assert! ( restriction_after . is_unrestricted () );
    assert! ( restriction_after . contains_skgrepo ( &SkgrepoName::from ("third") )); }
  { // A connection restricted to 'main' keeps the new skgrepo out too.
    let restricted_after : SkgrepoRestriction =
      SkgrepoRestriction::named ( &before . config, SkgrepoName::from ("main") ) ?
      . under_config (&after . config);
    assert! ( ! restricted_after . is_unrestricted () );
    assert! ( ! restricted_after . contains_skgrepo ( &SkgrepoName::from ("second") ));
    assert! ( SkgrepoRestriction::node_is_restricted (
      &restricted_after, &SkgrepoName::from ("third") )); }
  Ok (()) }

#[test]
fn an_invalid_config_is_refused_and_the_old_one_keeps_serving
  () -> Result<(), Box<dyn Error>> {
  let mut f : Fixture = fixture () ?;
  let mut connection : ViewsState = new_connection ();
  let before : Arc<RuntimeGeneration> = f . env . runtime_snapshot ();
  fs::write ( &f . config_path, "tantivy_folder = \"tantivy\"\n[[repos]]\nname = \"main\"\n" ) ?;
  let (mut stream, read_end) : (TcpStream, TcpStream) = mk_test_tcp_stream_pair ();
  handle_rebuild_session_lifetime_stores_request (
    &mut stream, &mut f . env, &mut connection );
  drop (stream);
  let messages : Vec<String> = read_all_lp_messages (&mut BufReader::new (read_end));
  let content : String =
    extract_string_field_from_sexp (&messages [0], "content") . expect ("content");
  assert! ( content . starts_with ("Rebuild failed: Reloading config from"), "{}", content );
  let after : Arc<RuntimeGeneration> = f . env . runtime_snapshot ();
  assert_eq! (after . generation, before . generation);
  assert! ( Arc::ptr_eq (&after . config, &before . config) );
  assert! ( graph_has (&after, "a") );
  assert! ( rebuild_session_lifetime_stores_in_place (&mut f . env, &mut connection)
            . is_err () );
  Ok (()) }
