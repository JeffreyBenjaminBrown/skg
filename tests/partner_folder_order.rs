// cargo nextest run --test grouped_forks -E 'test(partner_folder_order::)'
//
// The order of a forks folder's members that its note does not fork
// is the graph's and the config's, not the user's ('docs/forks.org',
// "Every mirror, flat"): the refresh puts them back in it after a save
// that moved them, and the order survives a further unchanged save.
// A child the folder's goal does not name rides after the sibling it
// followed ('docs/diff-view.org', "What the refresh keeps").
//
// Fixture: r and t both fork n, in one skgrepo; u forks nothing. The
// first test explicitly requests n's forks folder, whose member order
// is by pid (r, t), then swaps them in the buffer and saves. The
// second also places a note in the folder, after t, then makes u fork
// n from a view of u: n's view, rerendered collaterally, shows u in
// its place by pid, after t and the note riding after t.

use skg::skgrepo_sets::SkgrepoRestriction;
use skg::test_utils::save_and_rerender;
use std::error::Error;
use std::io::BufReader;
use std::net::TcpStream;

use skg::test_utils::{
  mk_test_tcp_stream_pair,
  empty_views_state,
  extract_string_field_from_sexp, graph_handle_from_config,
  read_all_lp_messages, run_with_test_stores};
use skg::test_utils::update_from_and_rerender_buffer_test as update_from_and_rerender_buffer;
use skg::test_utils::{multi_root_view, single_root_view};
use skg::serve::ViewsState;
use skg::serve::handlers::save_buffer::SaveResponse;
use skg::types::views_state::ViewforestId;
use skg::types::config::SkgConfig;
use skg::types::misc::{ID, TantivyIndex};

use skg::dbs::in_rust_graph::InRustGraphHandle;

#[test]
fn a_forks_folders_forkers_keep_the_graphs_order
  () -> Result<(), Box<dyn Error>> {
  run_with_test_stores (
    "skg-test-partner-folder-order",
    "tests/partner_folder_order/fixtures",
    "/tmp/tantivy-test-partner-folder-order",
    |config, tantivy| Box::pin ( async move {
      a_forks_folders_forkers_keep_the_graphs_order_impl (
        config, tantivy ) . await
    } )) }

/// The whole line containing the given metadata fragment.
fn line_containing<'a> (
  buf      : &'a str,
  fragment : &str,
) -> &'a str {
  buf . lines ()
    . find ( |l| l . contains (fragment) )
    . unwrap_or_else (
      || panic! ( "no line contains {:?} in:\n{}", fragment, buf )) }

fn assert_member_order (
  buf    : &str,
  first  : &str, // a metadata fragment, e.g. "(gnid t)"
  second : &str,
  label  : &str,
) {
  let i : usize = buf . find (first) . unwrap_or_else (
    || panic! ( "{}: {:?} not found in:\n{}", label, first, buf ));
  let j : usize = buf . find (second) . unwrap_or_else (
    || panic! ( "{}: {:?} not found in:\n{}", label, second, buf ));
  assert! ( i < j,
    "{}: expected {:?} before {:?} in:\n{}", label, first, second, buf ); }

async fn a_forks_folders_forkers_keep_the_graphs_order_impl (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let (initial_buffer, _pids, _tree)
    : (String, Vec<ID>, _) =
    multi_root_view (
      config, Some (tantivy),
      &[ ID ("n" . to_string ()) ] ) ?;
  let folder_request : String = initial_buffer . replacen (
    "node ",
    "node (viewRequests (folder forks))", 1 );
  let complete_buffer : String =
    save_and_rerender (&folder_request, config, tantivy) . await ?;
  assert_member_order (
    &complete_buffer, "(gnid r)", "(gnid t)", "de novo (sorted)" );
  let swapped : String = {
    // Swap the two member lines, putting t before r.
    let r_line : &str = line_containing (&complete_buffer, "(gnid r)");
    let t_line : &str = line_containing (&complete_buffer, "(gnid t)");
    complete_buffer
      . replace ( r_line, "SWAP_PLACEHOLDER" )
      . replace ( t_line, r_line )
      . replace ( "SWAP_PLACEHOLDER", t_line ) };
  let first_save : String =
    save_and_rerender (&swapped, config, tantivy) . await ?;
  assert_member_order (
    &first_save, "(gnid r)", "(gnid t)", "after reorder + save, the graph's order again" );
  let second_save : String =
    save_and_rerender (&first_save, config, tantivy) . await ?;
  assert_eq! ( first_save, second_save,
    "the redrawn view must be a fixpoint of the refresh" );
  Ok (( )) }

#[test]
fn a_member_added_elsewhere_follows_the_kept_ones
  () -> Result<(), Box<dyn Error>> {
  run_with_test_stores (
    "skg-test-partner-folder-order-added",
    "tests/partner_folder_order/fixtures",
    "/tmp/tantivy-test-partner-folder-order-added",
    |config, tantivy| Box::pin ( async move {
      a_member_added_elsewhere_follows_the_kept_ones_impl (
        config, tantivy ) . await
    } )) }

/// Save 'buffer' as the view 'viewforest_id'; return the saved view and the
/// contents of the collateral views the save sent.
async fn save_view_and_read_collateral (
  buffer      : &str,
  viewforest_id : &ViewforestId,
  config      : &SkgConfig,
  tantivy     : &mut TantivyIndex,
  graph       : &InRustGraphHandle,
  views_state : &mut ViewsState,
) -> Result<(String, Vec<String>), Box<dyn Error>> {
  let (mut stream, read_end) : (TcpStream, TcpStream) =
    mk_test_tcp_stream_pair ();
  let response : SaveResponse = update_from_and_rerender_buffer (
    &mut stream, buffer, config, tantivy, graph,
    &Ok (viewforest_id . clone ()), views_state ) . await ?;
  assert! ( response . errors . is_empty (),
    "save must not error; got: {:?}", response . errors );
  drop (stream);
  let collateral_views : Vec<String> =
    read_all_lp_messages ( &mut BufReader::new (read_end) )
    . into_iter ()
    . filter ( |message| message . contains ("collateral-view") )
    . map ( |message| extract_string_field_from_sexp (&message, "content")
            . expect ("a collateral view has content") )
    . collect ();
  Ok (( response . saved_view, collateral_views )) }

/// Panic unless the lines containing 'fragments' come in this order.
fn assert_lines_in_order (
  buf       : &str,
  fragments : &[&str],
  label     : &str,
) {
  let positions : Vec<usize> = fragments . iter ()
    . map ( |fragment| buf . lines () . position ( |line| line . contains (fragment) )
            . unwrap_or_else ( || panic! (
              "{}: no line contains {:?} in:\n{}", label, fragment, buf )) )
    . collect ();
  assert! ( positions . windows (2) . all ( |pair| pair [0] < pair [1] ),
    "{}: expected lines with {:?} in that order in:\n{}", label, fragments, buf ); }

async fn a_member_added_elsewhere_follows_the_kept_ones_impl (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let graph : InRustGraphHandle = graph_handle_from_config (config) ?;
  let mut views_state : ViewsState = empty_views_state ();
  let n_viewforest_id : ViewforestId = ViewforestId::ContentView ("order-n" . into ());
  let u_viewforest_id : ViewforestId = ViewforestId::ContentView ("order-u" . into ());
  let (n_view, _, _) = single_root_view (config, Some (tantivy), &ID::from ("n")) ?;
  let (n_view, _) : (String, Vec<String>) = save_view_and_read_collateral (
    &n_view . replacen (
      "node ",
      "node (viewRequests (folder forks))", 1 ),
    &n_viewforest_id, config, tantivy, &graph, &mut views_state ) . await ?;
  let placed : String = { // a note placed after t
    let t_line : &str = line_containing (&n_view, "(gnid t)");
    n_view . replace ( t_line, &format! (
      "{}\n*** (skg node noClaim) a placed note", t_line )) };
  let (n_view, _) : (String, Vec<String>) = save_view_and_read_collateral (
    &placed, &n_viewforest_id, config, tantivy, &graph, &mut views_state ) . await ?;
  assert_lines_in_order (
    &n_view, &[ "(gnid r)", "(gnid t)", "a placed note" ],
    "the note stays below the members" );
  let (u_view, u_pids, u_viewforest) =
    single_root_view (config, Some (tantivy), &ID::from ("u")) ?;
  views_state . open_views . register_view (
    &graph . load_full (), u_viewforest_id . clone (), u_viewforest, &u_pids, &SkgrepoRestriction::unrestricted (config) );
  let repo : &str = { // u's skgrepo, which n shares
    let start : usize = u_view . find ("(nodeRepo ") . expect ("u's line names its repo") + 10;
    &u_view [ start .. start + u_view [start ..] . find (')') . unwrap () ] };
  let forking : String = format! (
    "{}** (skg forksFolder)\n*** (skg node (gnid n) (skgRepos (nodeRepo {}))) n\n",
    u_view, repo );
  let (_, collateral_views) : (String, Vec<String>) = save_view_and_read_collateral (
    &forking, &u_viewforest_id, config, tantivy, &graph, &mut views_state ) . await ?;
  let collateral_n : &String = collateral_views . iter ()
    . find ( |view| view . contains ("forksFolder") )
    . unwrap_or_else ( || panic! ( "n's view is rerendered: {:?}", collateral_views ));
  assert_lines_in_order (
    collateral_n, &[ "(gnid r)", "(gnid t)", "a placed note", "(gnid u)" ],
    "u takes its place by pid, after t and the note riding after t" );
  Ok (( )) }
