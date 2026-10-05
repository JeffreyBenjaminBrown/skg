// cargo nextest run --test grouped_repos -E 'test(search_enrichment_terminal::)'
//
// Every 'search-results' is followed by exactly one 'search-enrichment',
// because that is what releases the client's stream guard. When there
// is nothing to enrich, or the client closes the search buffer before
// enrichment arrives, that message has no content.

use std::error::Error;
use std::io::BufReader;
use std::net::{TcpListener, TcpStream};
use std::sync::atomic::AtomicBool;
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};

use skg::dbs::filesystem::multiple_nodes::read_all_skg_files_from_skgrepos;
use skg::dbs::tantivy::write::update_index_with_nodes;
use skg::serve::ViewsState;
use skg::serve::handlers::text_search::{
  SearchEnrichmentPayload, abandon_search_enrichment,
  handle_text_search_request};
use skg::skgrepo_sets::{SkgrepoRestriction, SkgrepoSetName};
use skg::test_utils::{graph_handle_from_config, read_lp_message,
                      skg_env_from_parts};
use skg::test_utils::run_with_shared_test_stores;
use skg::types::env::SkgEnv;
use skg::types::misc::{SkgConfig, TantivyIndex};
use skg::types::nodes::tantivy::GraphnodeInTantivy;
use skg::types::views_state::OpenViews;

#[test]
fn all_tests
  () -> Result<(), Box<dyn Error>> {
  let fixtures : &str = "tests/repo_sets/fixtures";
  run_with_shared_test_stores (
    "skg-test-search-enrichment-terminal",
    |s| Box::pin ( async move {
      s . reset ("no_match_search_ends_with_contentless_enrichment", fixtures) ?;
      no_match_search_ends_with_contentless_enrichment (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("abandoned_enrichment_ends_with_contentless_enrichment", fixtures) ?;
      abandoned_enrichment_ends_with_contentless_enrichment (
        &s . config, &mut s . tantivy ) . await ?;
      Ok (( )) } )) }

fn connected_tcp_stream_pair (
) -> Result<(TcpStream, TcpStream), Box<dyn Error>> {
  let listener : TcpListener =
    TcpListener::bind ("127.0.0.1:0")?;
  let addr = listener . local_addr ()?;
  let client : TcpStream =
    TcpStream::connect (addr)?;
  let (server, _addr) =
    listener . accept ()?;
  Ok ((server, client)) }

fn read_all_lp_messages (
  client : TcpStream,
) -> Vec<String> {
  let mut reader : BufReader<TcpStream> =
    BufReader::new (client);
  let mut messages : Vec<String> = Vec::new ();
  while let Ok (m) = read_lp_message (&mut reader) {
    messages . push (m); }
  messages }

fn assert_contentless_enrichment (
  message : &str,
  terms   : &str,
) {
  assert! ( message . contains ("search-enrichment"),
            "expected a search-enrichment: {}", message );
  assert! ( message . contains (&format! ("(terms {})", terms))
            || message . contains (&format! ("(terms \"{}\")", terms)),
            "the enrichment names the search terms: {}", message );
  assert! ( ! message . contains ("content"),
            "a contentless enrichment leaves the buffer alone: {}",
            message ); }

async fn no_match_search_ends_with_contentless_enrichment (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let graph = graph_handle_from_config (config)?;
  let env : SkgEnv =
    skg_env_from_parts (config, tantivy, &graph);
  let mut views_state : ViewsState =
    ViewsState {
      diff_mode_enabled : false,
      open_views        : OpenViews::new (), };
  let all : SkgrepoRestriction =
    SkgrepoRestriction::named (config, SkgrepoSetName::from ("all"))?;
  let enrichment_slot : Arc<Mutex<Option<SearchEnrichmentPayload>>> =
    Arc::new (Mutex::new (None));
  let search_cancelled : Arc<AtomicBool> =
    Arc::new (AtomicBool::new (false));
  let (mut server, client) = connected_tcp_stream_pair ()?;
  let owed : Option<String> =
    handle_text_search_request (
      &mut server,
      "((request . \"text search\") (terms . \"zzqxv\") (regex . \"false\") (body . \"false\") (operators . \"false\"))",
      &env, &enrichment_slot, &search_cancelled,
      &mut views_state, &all );
  drop (server);
  assert_eq! ( owed, None,
               "nothing to enrich, so no enrichment is owed" );
  let messages : Vec<String> = read_all_lp_messages (client);
  assert_eq! ( messages . len (), 2, "{:?}", messages );
  assert! ( messages [0] . contains ("search-results")
            && messages [0] . contains ("No matches found."),
            "{}", messages [0] );
  assert_contentless_enrichment (&messages [1], "zzqxv");
  Ok (( )) }

async fn abandoned_enrichment_ends_with_contentless_enrichment (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  { // The session's search index starts empty.
    let tantivy_nodes : Vec<GraphnodeInTantivy> =
      read_all_skg_files_from_skgrepos (config)?
      . iter () . map (GraphnodeInTantivy::from) . collect ();
    update_index_with_nodes (&tantivy_nodes, tantivy)?; }
  let graph = graph_handle_from_config (config)?;
  let env : SkgEnv =
    skg_env_from_parts (config, tantivy, &graph);
  let mut views_state : ViewsState =
    ViewsState {
      diff_mode_enabled : false,
      open_views        : OpenViews::new (), };
  let all : SkgrepoRestriction =
    SkgrepoRestriction::named (config, SkgrepoSetName::from ("all"))?;
  let enrichment_slot : Arc<Mutex<Option<SearchEnrichmentPayload>>> =
    Arc::new (Mutex::new (None));
  let search_cancelled : Arc<AtomicBool> =
    Arc::new (AtomicBool::new (false));
  let (mut server, client) = connected_tcp_stream_pair ()?;
  let owed : Option<String> =
    handle_text_search_request (
      &mut server,
      "((request . \"text search\") (terms . \"shared ranking term\") (regex . \"false\") (body . \"false\") (operators . \"false\"))",
      &env, &enrichment_slot, &search_cancelled,
      &mut views_state, &all );
  assert_eq! ( owed . as_deref (), Some ("shared ranking term"),
               "a search with hits owes its enrichment" );
  { // Let the background thread finish, so that abandoning must
    // discard a payload already written, not merely stop the thread.
    let deadline : Instant = Instant::now () + Duration::from_secs (10);
    while enrichment_slot . lock () . unwrap () . is_none () {
      assert! ( Instant::now () < deadline,
                "enrichment never reached the slot" );
      std::thread::sleep (Duration::from_millis (10)); }}
  abandon_search_enrichment (
    &mut server, "shared ranking term",
    &enrichment_slot, &search_cancelled );
  drop (server);
  assert! ( enrichment_slot . lock () . unwrap () . is_none (),
            "abandoning discards the payload, so the server will not \
             request a snapshot for it" );
  let messages : Vec<String> = read_all_lp_messages (client);
  assert_eq! ( messages . len (), 2, "{:?}", messages );
  assert! ( messages [0] . contains ("search-results")
            && messages [0] . contains ("active-search-hit"),
            "{}", messages [0] );
  assert_contentless_enrichment (&messages [1], "shared ranking term");
  Ok (( )) }
