// cargo nextest run --test diff_mode_refusals
//
// The two refusals of
// TODO/full-schema/12-2_diff-mode-policy_discussion.org: enabling
// diff mode under a restricted source-set, and switching to a
// restricted source-set while diff mode is on.  Each refusal takes
// the quiet shape: the endpoint's normal first message carries the
// refusal text, then an EMPTY rerender stream (a "rerender-lock"
// naming no views, then "rerender-done") unwinds Emacs's preemptive
// stream guard and buffer locks.  A refused request changes nothing
// observable but the response.

use std::collections::HashMap;
use std::error::Error;
use std::io::BufReader;
use std::net::{TcpListener, TcpStream};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};

use skg::dbs::typedb::search::all_graphnodestats::AllGraphNodeStats;
use skg::serve::ViewsState;
use skg::serve::handlers::rerender_all_views::handle_git_diff_toggle_and_rerender;
use skg::serve::handlers::source_sets::handle_source_set_request;
use skg::serve::handlers::text_search::SearchEnrichmentPayload;
use skg::source_sets::{
  ActiveSourceSet, SourceSetName, run_with_source_set_test_db};
use skg::test_utils::{graph_handle_from_config, read_lp_message,
                      skg_env_from_parts};
use skg::types::env::SkgEnv;
use skg::types::views_state::OpenViews;

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

/// Read three LP messages and assert the last two are the empty
/// rerender stream.  Returns the first message.
fn read_first_then_assert_empty_stream (
  reader : &mut BufReader<TcpStream>,
) -> Result<String, Box<dyn Error>> {
  let first : String = read_lp_message (reader)?;
  let lock : String = read_lp_message (reader)?;
  assert! ( lock . contains ("rerender-lock"),
            "second message is the lock: {}", lock );
  assert! ( lock . contains ("(lock-views ())"),
            "the lock names no views, so Emacs unlocks every \
             buffer: {}", lock );
  let done : String = read_lp_message (reader)?;
  assert! ( done . contains ("rerender-done"),
            "third message is the done: {}", done );
  assert! ( done . contains ("(errors ())")
            && done . contains ("(warnings ())"),
            "the done carries no errors or warnings: {}", done );
  Ok (first) }

#[test]
fn toggle_refused_under_restricted_set_and_allowed_at_all (
) -> Result<(), Box<dyn Error>> {
  run_with_source_set_test_db (
    "skg-test-diff-refusal-toggle",
    "tests/source_sets/fixtures/skgconfig.toml",
    "/tmp/tantivy-test-diff-refusal-toggle",
    |config, driver, tantivy| Box::pin ( async move {
      let graph = graph_handle_from_config (config)?;
      let env : SkgEnv =
        skg_env_from_parts (
          config, Arc::clone (driver), tantivy, &graph );
      let mut views_state : ViewsState =
        ViewsState {
          diff_mode_enabled : false,
          open_views        : OpenViews::new (), };
      let toggle = |views_state : &mut ViewsState,
                    active : &ActiveSourceSet| -> Vec<String> {
        let (mut server, client) =
          connected_tcp_stream_pair () . unwrap ();
        std::thread::scope ( |scope| {
          scope . spawn ( || {
            handle_git_diff_toggle_and_rerender (
              &mut server, &env, views_state, active ); } ); } );
        drop (server);
        let mut reader : BufReader<TcpStream> =
          BufReader::new (client);
        let mut messages : Vec<String> = Vec::new ();
        while let Ok (m) = read_lp_message (&mut reader) {
          messages . push (m); }
        messages };
      let restricted : ActiveSourceSet =
        ActiveSourceSet::named (
          config, SourceSetName::from ("public"))?;
      let all : ActiveSourceSet =
        ActiveSourceSet::named (
          config, SourceSetName::from ("all"))?;
      { // Enabling under a restricted set is refused: refusal text
        // in the normal first message, the empty stream after, and
        // the flag unchanged.
        let messages : Vec<String> =
          toggle (&mut views_state, &restricted);
        assert_eq! ( messages . len (), 3, "{:?}", messages );
        assert! ( messages [0] . contains ("git-diff-mode"),
                  "{}", messages [0] );
        assert! ( messages [0] . contains (
            "Git diff mode requires active source-set all; \
             current active source-set is public" ),
          "{}", messages [0] );
        assert! ( ! messages [0] . contains ("\\nWarning:")
                  && ! messages [0] . contains ("\nWarning:"),
          "refusal text must not trip the Emacs warning-window \
           substring dispatch: {}", messages [0] );
        assert! ( messages [1] . contains ("(lock-views ())"),
                  "{}", messages [1] );
        assert! ( messages [2] . contains ("rerender-done"),
                  "{}", messages [2] );
        assert! ( ! views_state . diff_mode_enabled,
          "a refused toggle changes nothing" ); }
      { // Enabling at 'all' works.
        let messages : Vec<String> =
          toggle (&mut views_state, &all);
        assert! ( messages [0] . contains ("Git diff mode enabled"),
                  "{}", messages [0] );
        assert! ( views_state . diff_mode_enabled ); }
      { // DISABLING is allowed under any set (it only ever makes
        // the state legal), even a restricted one.
        let messages : Vec<String> =
          toggle (&mut views_state, &restricted);
        assert! ( messages [0] . contains ("Git diff mode disabled"),
                  "{}", messages [0] );
        assert! ( ! views_state . diff_mode_enabled ); }
      Ok (( )) } )) }

#[test]
fn switch_refusals_take_the_unwinding_shape (
) -> Result<(), Box<dyn Error>> {
  run_with_source_set_test_db (
    "skg-test-diff-refusal-switch",
    "tests/source_sets/fixtures/skgconfig.toml",
    "/tmp/tantivy-test-diff-refusal-switch",
    |config, driver, tantivy| Box::pin ( async move {
      let graph = graph_handle_from_config (config)?;
      let env : SkgEnv =
        skg_env_from_parts (
          config, Arc::clone (driver), tantivy, &graph );
      let request_to = |name : &str| -> String {
        format! ( "((request . \"set active source set\") \
                    (name . \"{}\"))", name ) };
      let switch = |views_state : &mut ViewsState,
                    active : &mut ActiveSourceSet,
                    enrichment_slot : &Arc<Mutex<Option<SearchEnrichmentPayload>>>,
                    search_cancelled : &Arc<AtomicBool>,
                    request : &str| -> Vec<String> {
        let (mut server, client) =
          connected_tcp_stream_pair () . unwrap ();
        std::thread::scope ( |scope| {
          scope . spawn ( || {
            handle_source_set_request (
              &mut server, request, &env, views_state,
              active, enrichment_slot, search_cancelled ); } ); } );
        drop (server);
        let mut reader : BufReader<TcpStream> =
          BufReader::new (client);
        let mut messages : Vec<String> = Vec::new ();
        while let Ok (m) = read_lp_message (&mut reader) {
          messages . push (m); }
        messages };
      let mut views_state : ViewsState =
        ViewsState {
          diff_mode_enabled : true,
          open_views        : OpenViews::new (), };
      let mut active : ActiveSourceSet =
        ActiveSourceSet::named (
          config, SourceSetName::from ("all"))?;
      let enrichment_slot : Arc<Mutex<Option<SearchEnrichmentPayload>>> =
        Arc::new (Mutex::new (Some (SearchEnrichmentPayload {
          terms          : "untouched by a refusal" . to_string (),
          search_results : vec![],
          ancestry_by_id : HashMap::new (),
          graphnodestats : AllGraphNodeStats::empty (), })));
      let search_cancelled : Arc<AtomicBool> =
        Arc::new (AtomicBool::new (false));
      { // Switching to a restricted set while diff mode is on is
        // refused, before every side effect.
        let messages : Vec<String> =
          switch (&mut views_state, &mut active,
                  &enrichment_slot, &search_cancelled,
                  &request_to ("public"));
        assert_eq! ( messages . len (), 3, "{:?}", messages );
        assert! ( messages [0] . contains ("active-source-set"),
                  "{}", messages [0] );
        assert! ( messages [0] . contains (
            "Cannot switch to source-set public: git diff mode is \
             on, and it requires active source-set all. Disable \
             diff mode first." ),
          "{}", messages [0] );
        assert! ( messages [0] . contains ("(active \"all\")"),
          "the response names the UNCHANGED set: {}", messages [0] );
        assert! ( messages [1] . contains ("(lock-views ())"),
                  "{}", messages [1] );
        assert! ( messages [2] . contains ("rerender-done"),
                  "{}", messages [2] );
        assert_eq! ( active . name, SourceSetName::from ("all"),
          "a refused switch changes nothing" );
        assert! ( enrichment_slot . lock () . unwrap () . is_some (),
          "a refused switch does not cancel search enrichment" );
        assert! ( ! search_cancelled . load (Ordering::SeqCst),
          "a refused switch does not cancel in-flight search" ); }
      { // Switching TO 'all' while diff mode is on works.
        let messages : Vec<String> =
          switch (&mut views_state, &mut active,
                  &enrichment_slot, &search_cancelled,
                  &request_to ("all"));
        assert! ( messages [0] . contains ("Active source-set: all"),
                  "{}", messages [0] );
        assert_eq! ( active . name, SourceSetName::from ("all") ); }
      { // The ride-along: an unknown set name answers in the same
        // unwinding shape (the old response-type "error" reply left
        // Emacs wedged: guard set, all buffers locked, no handler).
        views_state . diff_mode_enabled = false;
        let messages : Vec<String> =
          switch (&mut views_state, &mut active,
                  &enrichment_slot, &search_cancelled,
                  &request_to ("no-such-set"));
        assert_eq! ( messages . len (), 3, "{:?}", messages );
        assert! ( messages [0] . contains ("active-source-set"),
          "the error rides the normal response-type: {}",
          messages [0] );
        assert! ( messages [0] . contains ("no-such-set"),
                  "{}", messages [0] );
        assert! ( messages [1] . contains ("(lock-views ())"),
                  "{}", messages [1] );
        assert! ( messages [2] . contains ("rerender-done"),
                  "{}", messages [2] );
        assert_eq! ( active . name, SourceSetName::from ("all") ); }
      { // Restricted-to-restricted switching with diff mode off is
        // unaffected by the refusals.
        let _ : Vec<String> =
          switch (&mut views_state, &mut active,
                  &enrichment_slot, &search_cancelled,
                  &request_to ("public"));
        assert_eq! ( active . name, SourceSetName::from ("public") );
        let messages : Vec<String> =
          switch (&mut views_state, &mut active,
                  &enrichment_slot, &search_cancelled,
                  &request_to ("private"));
        assert! ( messages [0] . contains (
                    "Active source-set: private"),
                  "{}", messages [0] );
        assert_eq! ( active . name,
                     SourceSetName::from ("private") ); }
      Ok (( )) } )) }

#[test]
fn refusal_first_messages_parse_and_read_as_documented (
) -> Result<(), Box<dyn Error>> {
  // The quiet shape end to end, on a live reader rather than a
  // drained message list: first message, then exactly the empty
  // stream trio, then EOF.
  run_with_source_set_test_db (
    "skg-test-diff-refusal-shape",
    "tests/source_sets/fixtures/skgconfig.toml",
    "/tmp/tantivy-test-diff-refusal-shape",
    |config, driver, tantivy| Box::pin ( async move {
      let graph = graph_handle_from_config (config)?;
      let env : SkgEnv =
        skg_env_from_parts (
          config, Arc::clone (driver), tantivy, &graph );
      let mut views_state : ViewsState =
        ViewsState {
          diff_mode_enabled : false,
          open_views        : OpenViews::new (), };
      let restricted : ActiveSourceSet =
        ActiveSourceSet::named (
          config, SourceSetName::from ("public"))?;
      let (mut server, client) =
        connected_tcp_stream_pair ()?;
      std::thread::scope ( |scope| {
        scope . spawn ( || {
          handle_git_diff_toggle_and_rerender (
            &mut server, &env, &mut views_state, &restricted ); } ); } );
      drop (server);
      let mut reader : BufReader<TcpStream> =
        BufReader::new (client);
      let first : String =
        read_first_then_assert_empty_stream (&mut reader)?;
      assert! ( first . contains ("Switch the source-set to all \
                                   first") , "{}", first );
      assert! ( read_lp_message (&mut reader) . is_err (),
                "nothing follows the empty stream" );
      Ok (( )) } )) }
