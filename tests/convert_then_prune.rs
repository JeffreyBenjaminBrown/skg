// cargo nextest run --test grouped_overrides -E 'test(convert_then_prune::)'
//
// The one rule ('docs/glossary.org', "the one rule: convert, then
// prune"): a reconciler never detaches. A child the graph no longer
// warrants is converted -- a leaf, and every non-vognode, to a dead
// viewnode that the prune ending the completion removes; an unrestricted
// vognode with a subtree to a write-protected non-member -- and a folder
// adopts such a non-member back when its membership returns. Each test
// rerenders a view over a fixture and saves the result.
//
// Fixture 'main': k contains l and m, m contains mc; a, b and c contain
// x, x contains xc, p contains b; al has the aliases one and two; d
// contains e and f, e contains ec. Fixture 'restricted': skgrepos public
// (u, qc) and private.

use ego_tree::Tree;
use std::error::Error;
use std::io::BufReader;
use std::net::TcpStream;
use std::sync::atomic::AtomicBool;
use std::sync::{Arc, Mutex};

use crate::git_fixture_helpers::{mk_test_tcp_stream_pair, without_lines_containing};
use skg::dbs::in_rust_graph::InRustGraphHandle;
use skg::from_text::buffer_to_viewnodes::uninterpreted::org_to_uninterpreted_nodes;
use skg::metadata_canon::without_vids;
use skg::serve::ViewsState;
use skg::serve::handlers::rerender_all_views::handle_rerender_all_views_request;
use skg::serve::handlers::save_buffer::SaveResponse;
use skg::serve::handlers::skgrepo_sets::handle_skgrepo_set_request;
use skg::serve::handlers::text_search::SearchEnrichmentPayload;
use skg::skgrepo_sets::{SkgrepoRestriction, SkgrepoSetName};
use skg::test_utils::{
  extract_string_field_from_sexp, graph_handle_from_config,
  read_all_lp_messages, run_with_shared_test_stores, skg_env_from_parts};
use skg::test_utils::update_from_and_rerender_buffer_test as update_from_and_rerender_buffer;
use skg::to_org::render::content_view::single_root_view;
use skg::types::env::SkgEnv;
use skg::types::maybe_placed_viewnode::maybePlaced_to_placed_tree;
use skg::types::misc::{ID, SkgConfig, TantivyIndex};
use skg::types::viewnode::Viewnode;
use skg::types::views_state::{OpenViews, ViewId};

#[test]
fn all_tests
  () -> Result<(), Box<dyn Error>> {
  let main : &str = "tests/convert_then_prune/fixtures/main";
  let restricted : &str = "tests/convert_then_prune/fixtures/restricted";
  run_with_shared_test_stores (
    "skg-test-convert-then-prune",
    |s| Box::pin ( async move {
      s . reset ("a_stale_content_leaf_goes_and_a_stale_branch_is_parked", main) ?;
      a_stale_content_leaf_goes_and_a_stale_branch_is_parked (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("a_parked_folder_member_is_adopted_when_its_membership_returns", main) ?;
      a_parked_folder_member_is_adopted_when_its_membership_returns (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("a_duplicate_member_leaves_one_member_and_one_parked_copy", main) ?;
      a_duplicate_member_leaves_one_member_and_one_parked_copy (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("a_stale_alias_goes", main) ?;
      a_stale_alias_goes (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("a_deleted_phantom_of_a_restricted_node_loses_its_text", restricted) ?;
      a_deleted_phantom_of_a_restricted_node_loses_its_text (
        &s . config, &mut s . tantivy ) . await ?;
      Ok (( )) } )) }

fn empty_views_state () -> ViewsState {
  ViewsState {
    open_views     : OpenViews::new (),
    graph_straddle : None, }}

/// Save 'buffer' as the view 'view_id'; return the saved view and the
/// contents of the collateral views the save sent. Panics on a refusal.
async fn save_view (
  buffer      : &str,
  view_id     : &ViewId,
  config      : &SkgConfig,
  tantivy     : &mut TantivyIndex,
  graph       : &InRustGraphHandle,
  views_state : &mut ViewsState,
) -> Result<(String, Vec<String>), Box<dyn Error>> {
  let (mut stream, read_end) : (TcpStream, TcpStream) = mk_test_tcp_stream_pair ();
  let response : SaveResponse = update_from_and_rerender_buffer (
    &mut stream, buffer, config, tantivy, graph,
    &Ok (view_id . clone ()), views_state ) . await ?;
  assert! ( response . errors . is_empty (),
    "save must not error; got: {:?}\nbuffer:\n{}", response . errors, buffer );
  drop (stream);
  let collateral_views : Vec<String> =
    contents_of_messages (read_end, "collateral-view");
  Ok (( response . saved_view, collateral_views )) }

/// The 'content' of each message of this kind written to 'read_end'.
fn contents_of_messages (
  read_end : TcpStream,
  kind     : &str,
) -> Vec<String> {
  read_all_lp_messages ( &mut BufReader::new (read_end) )
    . into_iter ()
    . filter ( |message| message . contains (kind) )
    . filter_map ( |message| extract_string_field_from_sexp (&message, "content") )
    . collect () }

/// The lines of 'buffer' containing 'fragment', view ids aside.
fn lines_containing (
  buffer   : &str,
  fragment : &str,
) -> Vec<String> {
  without_vids (buffer) . lines ()
    . filter ( |line| line . contains (fragment) )
    . map ( |line| line . to_string () )
    . collect () }

/// The one line of 'buffer' that starts with 'prefix' and contains
/// 'fragment', view ids aside.
fn the_line (
  buffer   : &str,
  prefix   : &str,
  fragment : &str,
) -> String {
  let lines : Vec<String> = lines_containing (buffer, fragment) . into_iter ()
    . filter ( |line| line . starts_with (prefix) )
    . collect ();
  assert_eq! ( lines . len (), 1,
    "expected one line starting {:?} with {:?} in:\n{}", prefix, fragment, buffer );
  lines [0] . clone () }

/// The line after the one line starting with 'prefix' and containing
/// 'fragment', view ids aside.
fn the_line_after (
  buffer   : &str,
  prefix   : &str,
  fragment : &str,
) -> String {
  let unvidded : String = without_vids (buffer);
  let lines : Vec<&str> = unvidded . lines () . collect ();
  let index : usize = lines . iter ()
    . position ( |line| line . starts_with (prefix) && line . contains (fragment) )
    . unwrap_or_else ( || panic! (
      "no line starts {:?} with {:?} in:\n{}", prefix, fragment, buffer ));
  lines . get (index + 1) . map ( |line| line . to_string () )
    . unwrap_or_default () }

fn is_parked (line : &str) -> bool {
  line . contains ("(affectsParent false)") && line . contains ("writeProtected") }

/// k loses l (a leaf) and m (which holds mc), saved from a second view
/// of k. The first view's rerender drops l and keeps m as a
/// write-protected non-member holding mc, now a non-member too; that
/// buffer saves.
async fn a_stale_content_leaf_goes_and_a_stale_branch_is_parked (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let graph : InRustGraphHandle = graph_handle_from_config (config) ?;
  let mut views_state : ViewsState = empty_views_state ();
  let first_view_id : ViewId = ViewId::ContentView ("convert-k-1" . into ());
  let second_view_id : ViewId = ViewId::ContentView ("convert-k-2" . into ());
  let (k_view, _, _) = single_root_view (config, Some (tantivy), &ID::from ("k")) ?;
  save_view ( &k_view, &first_view_id, config, tantivy, &graph,
              &mut views_state ) . await ?;
  let without_content : String =
    without_lines_containing (
      &without_lines_containing (
        &without_lines_containing (&k_view, "(id l)"),
        "(id m)" ),
      "(id mc)" );
  let (_, collateral_views) : (String, Vec<String>) = save_view (
    &without_content, &second_view_id, config, tantivy, &graph,
    &mut views_state ) . await ?;
  assert_eq! ( collateral_views . len (), 1, "{:?}", collateral_views );
  let first_view : &str = &collateral_views [0];
  assert! ( lines_containing (first_view, "(id l)") . is_empty (),
    "the stale leaf is gone:\n{}", first_view );
  assert! ( is_parked ( &the_line (first_view, "** ", "(id m)") ),
    "the stale branch is a write-protected non-member:\n{}", first_view );
  assert! ( the_line (first_view, "*** ", "(id mc)") . contains ("(affectsParent false)"),
    "a write-protected occurrence holds no member:\n{}", first_view );
  save_view ( first_view, &first_view_id, config, tantivy, &graph,
              &mut views_state ) . await ?;
  Ok (( )) }

/// A note typed under b in x's containerFolder. When b stops
/// containing x, x's view keeps b there as a write-protected non-member
/// holding the note. When
/// b contains x again, the folder adopts the parked b as the member and
/// creates no second one, and x's view saves.
async fn a_parked_folder_member_is_adopted_when_its_membership_returns (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let graph : InRustGraphHandle = graph_handle_from_config (config) ?;
  let mut views_state : ViewsState = empty_views_state ();
  let x_view_id : ViewId = ViewId::ContentView ("convert-x" . into ());
  let b_view_id : ViewId = ViewId::ContentView ("convert-b" . into ());
  let (x_view, _, _) = single_root_view (config, Some (tantivy), &ID::from ("x")) ?;
  let (x_view, _) : (String, Vec<String>) = save_view (
    &x_view . replacen (
      "(affectsParent na)",
      "(affectsParent na) (viewRequests (folder containers))", 1 ),
    &x_view_id, config, tantivy, &graph, &mut views_state ) . await ?;
  let with_note : String = {
    let member_b : String = x_view . lines ()
      . find ( |line| line . starts_with ("*** ") && line . contains ("(id b)") )
      . unwrap_or_else ( || panic! ( "b is in the folder:\n{}", x_view ))
      . to_string ();
    x_view . replacen (
      &member_b,
      &format! ( "{}\n**** (skg (node (affectsParent false))) a note under b", member_b ),
      1 ) };
  save_view ( &with_note, &x_view_id, config, tantivy, &graph,
              &mut views_state ) . await ?;
  let (b_view, _, _) = single_root_view (config, Some (tantivy), &ID::from ("b")) ?;
  let (b_view, collateral_views) : (String, Vec<String>) = save_view (
    &without_lines_containing (
      &without_lines_containing (&b_view, "(id x)"), "(id xc)" ),
    &b_view_id, config, tantivy, &graph, &mut views_state ) . await ?;
  let x_after_drop : &String = collateral_views . iter ()
    . find ( |view| view . contains ("containerFolder") )
    . unwrap_or_else ( || panic! ( "x's view is rerendered: {:?}", collateral_views ));
  assert! ( is_parked ( &the_line (x_after_drop, "*** ", "(id b)") ),
    "the stale member is a write-protected non-member:\n{}", x_after_drop );
  assert! ( the_line_after (x_after_drop, "*** ", "(id b)") . contains ("a note under b"),
    "the note stands under it:\n{}", x_after_drop );
  let (_, collateral_views) : (String, Vec<String>) = save_view (
    &format! ( "{}** (skg (node (id x) (repo main))) x\n\
                *** (skg (node (id xc) (repo main))) xc\n", b_view ),
    &b_view_id, config, tantivy, &graph, &mut views_state ) . await ?;
  let x_after_return : &String = collateral_views . iter ()
    . find ( |view| view . contains ("containerFolder") )
    . unwrap_or_else ( || panic! ( "x's view is rerendered: {:?}", collateral_views ));
  let member_b : String = the_line (x_after_return, "*** ", "(id b)");
  assert! ( ! member_b . contains ("(affectsParent false)"),
    "the parked b is the member again, and the only b in the folder:\n{}",
    x_after_return );
  assert! ( the_line_after (x_after_return, "*** ", "(id b)") . contains ("a note under b"),
    "the note still stands under it:\n{}", x_after_return );
  save_view ( x_after_return, &x_view_id, config, tantivy, &graph,
              &mut views_state ) . await ?;
  Ok (( )) }

/// The rerender of a stored view whose d holds two copies of e (a
/// write-protected one holding ec, then an editable leaf) and two of f
/// keeps the editable e as the member, parks the other, drops the
/// second f, and gives a buffer the save accepts.
async fn a_duplicate_member_leaves_one_member_and_one_parked_copy (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let graph : InRustGraphHandle = graph_handle_from_config (config) ?;
  let mut views_state : ViewsState = empty_views_state ();
  let view_id : ViewId = ViewId::ContentView ("convert-d" . into ());
  let stored : Tree<Viewnode> = maybePlaced_to_placed_tree (
    org_to_uninterpreted_nodes (
      "* (skg (node (id d) (repo main))) d\n\
       ** (skg (node (id e) (repo main) writeProtected)) e\n\
       *** (skg (node (id ec) (repo main))) ec\n\
       ** (skg (node (id f) (repo main))) f\n\
       ** (skg (node (id e) (repo main))) e\n\
       ** (skg (node (id f) (repo main))) f\n" ) ? . 0 ) ?;
  views_state . open_views . register_view (
    &graph . load_full (), view_id . clone (), stored, &[ID::from ("d")] );
  let env : SkgEnv = skg_env_from_parts (config, tantivy, &graph);
  let all : SkgrepoRestriction =
    SkgrepoRestriction::named (config, SkgrepoSetName::from ("all")) ?;
  let (mut stream, read_end) : (TcpStream, TcpStream) = mk_test_tcp_stream_pair ();
  std::thread::scope ( |scope| {
    // The handler blocks on futures itself, so it cannot run inside
    // this test's executor.
    scope . spawn ( || handle_rerender_all_views_request (
      &mut stream, "((request . \"rerender all views\"))\n", &env,
      &mut views_state, &all )); } );
  drop (stream);
  let rerendered : Vec<String> = contents_of_messages (read_end, "rerender-view");
  assert_eq! ( rerendered . len (), 1, "{:?}", rerendered );
  let view : &str = &rerendered [0];
  let e_lines : Vec<String> = lines_containing (view, "(id e)");
  assert_eq! ( e_lines . len (), 2, "two copies of e remain:\n{}", view );
  assert! ( is_parked (&e_lines [0]),
    "the write-protected copy is parked:\n{}", view );
  assert! ( ! e_lines [1] . contains ("(affectsParent false)")
            && ! e_lines [1] . contains ("writeProtected"),
    "the editable copy is the member:\n{}", view );
  assert_eq! ( lines_containing (view, "(id f)") . len (), 1,
    "the extra leaf copy of f is gone:\n{}", view );
  drop (env);
  save_view ( view, &view_id, config, tantivy, &graph,
              &mut views_state ) . await ?;
  Ok (( )) }

/// al loses its alias one, saved from a second view of al; the first
/// view's alias folder no longer shows it.
async fn a_stale_alias_goes (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let graph : InRustGraphHandle = graph_handle_from_config (config) ?;
  let mut views_state : ViewsState = empty_views_state ();
  let first_view_id : ViewId = ViewId::ContentView ("convert-al-1" . into ());
  let second_view_id : ViewId = ViewId::ContentView ("convert-al-2" . into ());
  let (al_view, _, _) = single_root_view (config, Some (tantivy), &ID::from ("al")) ?;
  let requesting : String = al_view . replacen (
    "(affectsParent na)",
    "(affectsParent na) (viewRequests (folder aliases))", 1 );
  save_view ( &requesting, &first_view_id, config, tantivy, &graph,
              &mut views_state ) . await ?;
  let (second_view, _) : (String, Vec<String>) = save_view (
    &requesting, &second_view_id, config, tantivy, &graph,
    &mut views_state ) . await ?;
  assert_eq! ( lines_containing (&second_view, "alias) one") . len (), 1,
    "the alias folder shows one:\n{}", second_view );
  let (_, collateral_views) : (String, Vec<String>) = save_view (
    &without_lines_containing (&second_view, "alias) one"),
    &second_view_id, config, tantivy, &graph, &mut views_state ) . await ?;
  assert_eq! ( collateral_views . len (), 1, "{:?}", collateral_views );
  let first_view : &str = &collateral_views [0];
  assert! ( lines_containing (first_view, "alias) one") . is_empty (),
    "the stale alias is gone:\n{}", first_view );
  assert_eq! ( lines_containing (first_view, "alias) two") . len (), 1,
    "the other alias stays:\n{}", first_view );
  Ok (( )) }

/// A view of u shows two deleted phantoms of private nodes, q as a
/// leaf and q2 holding qc. After the switch to the public skgrepo-set,
/// q is gone and q2 is a dead viewnode without its title, still
/// holding qc; nothing of either's text remains.
async fn a_deleted_phantom_of_a_restricted_node_loses_its_text (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let graph : InRustGraphHandle = graph_handle_from_config (config) ?;
  let mut views_state : ViewsState = empty_views_state ();
  let view_id : ViewId = ViewId::ContentView ("convert-u" . into ());
  let stored : Tree<Viewnode> = maybePlaced_to_placed_tree (
    org_to_uninterpreted_nodes (
      "* (skg (node (id u) (repo public))) u\n\
       ** (skg (deleted (id q) (repo private))) q secret title\n\
       q secret body\n\
       ** (skg (deleted (id q2) (repo private))) q2 secret title\n\
       *** (skg (node (id qc) (repo public) (affectsParent false))) qc\n" ) ? . 0 ) ?;
  views_state . open_views . register_view (
    &graph . load_full (), view_id . clone (), stored, &[ID::from ("u")] );
  let env : SkgEnv = skg_env_from_parts (config, tantivy, &graph);
  let mut restriction : SkgrepoRestriction =
    SkgrepoRestriction::named (config, SkgrepoSetName::from ("all")) ?;
  let enrichment_slot : Arc<Mutex<Option<SearchEnrichmentPayload>>> =
    Arc::new (Mutex::new (None));
  let search_cancelled : Arc<AtomicBool> = Arc::new (AtomicBool::new (false));
  let (mut stream, read_end) : (TcpStream, TcpStream) = mk_test_tcp_stream_pair ();
  std::thread::scope ( |scope| {
    // The handler blocks on futures itself, so it cannot run inside
    // this test's executor.
    scope . spawn ( || handle_skgrepo_set_request (
      &mut stream,
      "((request . \"set skgrepo restriction\") (name . \"public\"))",
      &env, &mut views_state, &mut restriction,
      &enrichment_slot, &search_cancelled )); } );
  drop (stream);
  let rerendered : Vec<String> = contents_of_messages (read_end, "rerender-view");
  assert_eq! ( rerendered . len (), 1, "{:?}", rerendered );
  let view : &str = &rerendered [0];
  assert! ( ! view . contains ("secret"),
    "no private text remains:\n{}", view );
  assert! ( lines_containing (view, "(id q)") . is_empty ()
            && lines_containing (view, "(id q2)") . is_empty (),
    "no private ID remains:\n{}", view );
  assert_eq! ( the_line (view, "** ", "deadViewnode"), "** (skg deadViewnode)",
    "the phantom holding qc is a dead viewnode, untitled:\n{}", view );
  assert! ( the_line_after (view, "** ", "deadViewnode") . contains ("(id qc)"),
    "it still holds qc:\n{}", view );
  Ok (( )) }
