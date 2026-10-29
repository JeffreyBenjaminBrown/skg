// cargo nextest run --test grouped_repos -E 'test(search_buffer::)'
//
// A text search is answered by one 'search-results' message, whose
// content is the search buffer: each hit with what the enrichment
// draws under it ('docs/api-and-formats.org', "Text search").
//
// Fixture (tests/search_buffer/fixtures, one skgrepo): "nest
// of birds" contains "kestrel feather" and "osprey", whose aliases are
// "kestrel cousin" and "fish hawk". A search for "kestrel" finds the
// first by its title and the second through an alias.

use indoc::indoc;
use std::collections::HashSet;
use std::error::Error;
use std::io::BufReader;
use std::net::{TcpListener, TcpStream};

use skg::dbs::filesystem::multiple_nodes::read_all_skg_files_from_skgrepos;
use skg::dbs::filesystem::not_nodes::load_config;
use skg::dbs::in_rust_graph::InRustGraph;
use skg::dbs::tantivy::write::update_index_with_nodes;
use skg::metadata_canon::without_viewnode_ids;
use skg::serve::ViewsState;
use skg::serve::handlers::text_search::{
  MatchGroups, TitleOrAlias, build_search_viewforest, draw_search_buffer,
  handle_text_search_request};
use skg::skgrepo_sets::{SkgrepoRestriction, SkgrepoSetName};
use skg::test_utils::{extract_string_field_from_sexp, graph_handle_from_config,
                      read_lp_message, set_skgrepo_retagging_relRepos,
                      skg_env_from_parts};
use skg::test_utils::run_with_shared_test_stores;
use skg::types::env::SkgEnv;
use skg::types::misc::{ID, SkgConfig, SkgrepoName, TantivyIndex, rel_partners_at_relRepo};
use skg::types::nodes::complete::{Graphnode, empty_graphnode};
use skg::types::nodes::tantivy::GraphnodeInTantivy;
use skg::types::tree::forest::ViewForest;
use skg::types::views_state::{OpenViews, ViewforestId, ViewnodeIdRecord};

#[test]
fn all_tests
  () -> Result<(), Box<dyn Error>> {
  let fixtures : &str = "tests/search_buffer/fixtures";
  run_with_shared_test_stores (
    "skg-test-search-buffer",
    |s| Box::pin ( async move {
      s . reset ("a_search_without_hits_answers_in_one_message", fixtures) ?;
      a_search_without_hits_answers_in_one_message (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("a_search_answers_with_the_search_buffer_in_one_message", fixtures) ?;
      a_search_answers_with_the_search_buffer_in_one_message (
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

/// Search for 'terms' under the restriction "all", and return every
/// message the server sent, with the views state the search left.
fn search (
  config  : &SkgConfig,
  tantivy : &TantivyIndex,
  terms   : &str,
) -> Result<(Vec<String>, ViewsState), Box<dyn Error>> {
  let graph = graph_handle_from_config (config)?;
  let env : SkgEnv =
    skg_env_from_parts (config, tantivy, &graph);
  let mut views_state : ViewsState =
    ViewsState {
      open_views        : OpenViews::new (),
      graph_straddle    : None, };
  let all : SkgrepoRestriction =
    SkgrepoRestriction::named (config, SkgrepoSetName::from ("all"))?;
  let (mut server, client) = connected_tcp_stream_pair ()?;
  handle_text_search_request (
    &mut server,
    &format! ( "((request . \"text search\") (terms . \"{}\") (regex . \"false\") (body . \"false\") (operators . \"false\"))",
               terms ),
    &env, &mut views_state, &all );
  drop (server);
  Ok (( read_all_lp_messages (client), views_state )) }

async fn a_search_without_hits_answers_in_one_message (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let (messages, views_state) : (Vec<String>, ViewsState) =
    search (config, tantivy, "zzqxv") ?;
  assert_eq! ( messages . len (), 1, "{:?}", messages );
  assert! ( messages [0] . contains ("search-results")
            && messages [0] . contains ("No matches found."),
            "{}", messages [0] );
  assert! ( views_state . open_views . views . is_empty (),
            "no view is registered for a search without hits" );
  Ok (( )) }

/// The one message holds the search buffer, with what the enrichment
/// draws under each hit. The hit found by its
/// title shows collapsed: its containers folder is folded. The hit
/// found through an alias shows its node's title, not the alias, and
/// its folders: the containers folder collapsed (its member folded),
/// and the alias folder open, holding all of the node's aliases. The
/// search view is registered with every node the buffer shows.
async fn a_search_answers_with_the_search_buffer_in_one_message (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  { // The session's search index starts empty.
    let tantivy_nodes : Vec<GraphnodeInTantivy> =
      read_all_skg_files_from_skgrepos (config)?
      . iter () . map (GraphnodeInTantivy::from) . collect ();
    update_index_with_nodes (&tantivy_nodes, tantivy)?; }
  let (messages, views_state) : (Vec<String>, ViewsState) =
    search (config, tantivy, "kestrel") ?;
  assert_eq! ( messages . len (), 1, "{:?}", messages );
  assert! ( messages [0] . contains ("search-results"), "{}", messages [0] );
  let content : String =
    extract_string_field_from_sexp (&messages [0], "content")
    . expect ("the search results carry content");
  assert_eq! (
    hit_subtrees (&without_viewnode_ids (&content)),
    hit_subtrees ( indoc! {"
      * (skg node (gnid kestrel) (skgRepos (nodeRepo main)) writeProtected (view (rels (contains (in 1))))) kestrel feather
      ** (skg folded containerFolder)
      *** (skg node (gnid nest) (skgRepos (nodeRepo main)) writeProtected (view (rels (contains (out 2 (ancestors 2)))) (birth (contains out 2)))) nest of birds
      * (skg node (gnid osprey) (skgRepos (nodeRepo main)) writeProtected (view (rels (contains (in 1))) (props (aliases 2)))) osprey
      ** (skg containerFolder)
      *** (skg folded node (gnid nest) (skgRepos (nodeRepo main)) writeProtected (view (rels (contains (out 2 (ancestors 2)))) (birth (contains out 2)))) nest of birds
      ** (skg aliasFolder)
      *** (skg alias) kestrel cousin
      *** (skg alias) fish hawk
    "} ),
    "the hits, with what the enrichment draws, in one message:\n{}", content );
  let view_pids : &HashSet<ID> =
    & views_state . open_views . views
      . get (&ViewforestId::SearchView ("kestrel" . to_string ()))
      . expect ("the search view is registered") . pids;
  assert_eq! ( *view_pids,
               [ "kestrel", "osprey", "nest" ] . into_iter ()
               . map (ID::from) . collect::<HashSet<ID>> (),
               "the view knows every node it shows" );
  Ok (( )) }

/// The subtree of each hit, as text. (The two hits tie in score, so
/// their order is not fixed.)
fn hit_subtrees (
  buffer : &str,
) -> HashSet<String> {
  let mut subtrees : Vec<String> = Vec::new ();
  for line in buffer . lines () {
    if line . starts_with ("* ") || subtrees . is_empty () {
      subtrees . push (String::new ()); }
    let subtree : &mut String = subtrees . last_mut () . unwrap ();
    subtree . push_str (line);
    subtree . push ('\n'); }
  subtrees . into_iter () . collect () }

/// A search that excludes overPrivateText telescopes excludes them
/// from what the enrichment draws too: the hit's container, an
/// overPrivateText telescope, is entombstoned after the refresh, and
/// the tombstone, childless, is pruned with the containers folder it
/// emptied. A search that includes them shows the container.
#[test]
fn an_excluded_overPrivateText_container_leaves_no_trace (
) -> Result<(), Box<dyn Error>> {
  let config : SkgConfig =
    load_config ("tests/repo_sets/fixtures/skgconfig.toml") ? . 0;
  let restriction : SkgrepoRestriction =
    SkgrepoRestriction::named (&config, SkgrepoSetName::from ("public")) ?;
  let mut hit : Graphnode = empty_graphnode ();
  hit . pid = ID::from ("hit");
  hit . title = "the hit" . to_string ();
  set_skgrepo_retagging_relRepos (&mut hit, &SkgrepoName::from ("public"));
  let mut container : Graphnode = empty_graphnode ();
  container . pid = ID::from ("container");
  container . title = "container text from below home" . to_string ();
  container . contains = rel_partners_at_relRepo (
    &SkgrepoName::from ("public"), vec! [ ID::from ("hit") ] );
  container . overPrivateText_telescope = true;
  set_skgrepo_retagging_relRepos (&mut container, &SkgrepoName::from ("public"));
  let graph : InRustGraph = InRustGraph::from_graphnodes (&[hit, container]);
  let matches : MatchGroups =
    [ ( ID::from ("hit"),
        ( SkgrepoName::from ("public"),
          vec! [ (1.0, TitleOrAlias::Title ("the hit" . to_string ())) ] )) ]
    . into_iter () . collect ();
  let drawn = |include_overPrivateText_telescopes : bool|
    -> Result<String, Box<dyn Error>> {
      let (mut viewforest, search_results) : (ViewForest, Vec<ID>) =
        build_search_viewforest ("hit", &matches, &HashSet::new ());
      draw_search_buffer (
        &mut viewforest, &mut ViewnodeIdRecord::default (), &graph, &config,
        &search_results, &restriction, include_overPrivateText_telescopes ) };
  let excluded : String = drawn (false) ?;
  assert_eq! ( without_viewnode_ids (&excluded),
               "* (skg node (gnid hit) (skgRepos (nodeRepo public)) writeProtected (view (rels (contains (in 1))) repoBoundary)) the hit\n",
               "nothing of the container shows, not even a tombstone" );
  let included : String = drawn (true) ?;
  assert! ( included . contains ("container text from below home"),
            "included, the container shows: {}", included );
  Ok (( )) }
