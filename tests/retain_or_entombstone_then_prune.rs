// cargo nextest run --test grouped_overrides -E 'test(retain_or_entombstone_then_prune::)'
//
// What the refresh keeps of what the graph no longer warrants
// ('docs/data-model_technical.org', "The refresh: how a view follows
// the graph"): it never removes a child. A child the graph no longer
// warrants stays where it stands, unwarranted ('noWarrant'), and,
// where its parent reads claims, claiming nothing ('noClaim'); it
// keeps its editability and its subtree. Siblings standing for one
// node are joined by a viewnode merge. The first editable occurrence of a node keeps its
// editability; a later one the refresh draws is write-protected. Each
// test rerenders a view over a fixture and saves the result.
//
// Fixture 'main': k contains l and m, m contains mc; a, b and c contain
// x, x contains xc, p contains b; al has the aliases one and two; d
// contains e and f, e contains ec.

use ego_tree::Tree;
use std::error::Error;
use std::io::BufReader;
use std::net::TcpStream;

use crate::git_fixture_helpers::{mk_test_tcp_stream_pair, without_lines_containing};
use skg::dbs::in_rust_graph::InRustGraphHandle;
use skg::from_text::buffer_to_viewnodes::add_missing_info::IdResolution;
use skg::from_text::buffer_to_viewnodes::uninterpreted::org_to_viewnode_tree;
use skg::metadata_canon::without_viewnode_ids;
use skg::serve::ViewsState;
use skg::serve::handlers::rerender_all_views::handle_rerender_all_views_request;
use skg::serve::handlers::save_buffer::SaveResponse;
use skg::skgrepo_sets::{SkgrepoRestriction, SkgrepoSetName};
use skg::test_utils::{
  extract_string_field_from_sexp, graph_handle_from_config,
  read_all_lp_messages, run_with_shared_test_stores, skg_env_from_parts};
use skg::test_utils::update_from_and_rerender_buffer_test as update_from_and_rerender_buffer;
use skg::to_org::render::content_view::single_root_view;
use skg::types::env::SkgEnv;
use skg::types::misc::{ID, SkgConfig, TantivyIndex};
use skg::types::viewnode::Viewnode;
use skg::types::views_state::{OpenViews, ViewforestId};

#[test]
fn all_tests
  () -> Result<(), Box<dyn Error>> {
  let main : &str = "tests/retain_or_entombstone_then_prune/fixtures/main";
  run_with_shared_test_stores (
    "skg-test-retain-or-entombstone-then-prune",
    |s| Box::pin ( async move {
      s . reset ("stale_content_is_kept_unwarranted", main) ?;
      stale_content_is_kept_unwarranted (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("a_pasted_copy_beside_a_member_merges_into_it", main) ?;
      a_pasted_copy_beside_a_member_merges_into_it (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("a_kept_editable_occurrence_keeps_its_editability", main) ?;
      a_kept_editable_occurrence_keeps_its_editability (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("a_folder_member_kept_unwarranted_is_warranted_when_its_membership_returns", main) ?;
      a_folder_member_kept_unwarranted_is_warranted_when_its_membership_returns (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("duplicate_members_merge", main) ?;
      duplicate_members_merge (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("editable_duplicates_merge_with_their_content", main) ?;
      editable_duplicates_merge_with_their_content (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("a_stale_alias_is_kept_unwarranted", main) ?;
      a_stale_alias_is_kept_unwarranted (
        &s . config, &mut s . tantivy ) . await ?;
      Ok (( )) } )) }

fn empty_views_state () -> ViewsState {
  ViewsState {
    open_views     : OpenViews::new (),
    graph_straddle : None, }}

/// Save 'buffer' as the view 'viewforest_id'; return the saved view and the
/// contents of the collateral views the save sent. Panics on a refusal.
async fn save_view (
  buffer      : &str,
  viewforest_id : &ViewforestId,
  config      : &SkgConfig,
  tantivy     : &mut TantivyIndex,
  graph       : &InRustGraphHandle,
  views_state : &mut ViewsState,
) -> Result<(String, Vec<String>), Box<dyn Error>> {
  let (mut stream, read_end) : (TcpStream, TcpStream) = mk_test_tcp_stream_pair ();
  let response : SaveResponse = update_from_and_rerender_buffer (
    &mut stream, buffer, config, tantivy, graph,
    &Ok (viewforest_id . clone ()), views_state ) . await ?;
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

/// The lines of 'buffer' containing 'fragment', viewnode IDs aside.
fn lines_containing (
  buffer   : &str,
  fragment : &str,
) -> Vec<String> {
  without_viewnode_ids (buffer) . lines ()
    . filter ( |line| line . contains (fragment) )
    . map ( |line| line . to_string () )
    . collect () }

/// The one line of 'buffer' that starts with 'prefix' and contains
/// 'fragment', viewnode IDs aside.
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
/// 'fragment', viewnode IDs aside.
fn the_line_after (
  buffer   : &str,
  prefix   : &str,
  fragment : &str,
) -> String {
  let buffer_without_viewnode_ids : String = without_viewnode_ids (buffer);
  let lines : Vec<&str> = buffer_without_viewnode_ids . lines () . collect ();
  let index : usize = lines . iter ()
    . position ( |line| line . starts_with (prefix) && line . contains (fragment) )
    . unwrap_or_else ( || panic! (
      "no line starts {:?} with {:?} in:\n{}", prefix, fragment, buffer ));
  lines . get (index + 1) . map ( |line| line . to_string () )
    . unwrap_or_default () }

/// The lines of the subtree under the first line that starts with
/// 'prefix' and contains 'fragment': those after it that start with
/// one more asterisk, until one that does not.
fn the_lines_under (
  buffer   : &str,
  prefix   : &str,
  fragment : &str,
) -> Vec<String> {
  let buffer_without_viewnode_ids : String = without_viewnode_ids (buffer);
  let lines : Vec<&str> = buffer_without_viewnode_ids . lines () . collect ();
  let index : usize = lines . iter ()
    . position ( |line| line . starts_with (prefix) && line . contains (fragment) )
    . unwrap_or_else ( || panic! (
      "no line starts {:?} with {:?} in:\n{}", prefix, fragment, buffer ));
  let deeper : String = format! ( "*{}", prefix . trim_end () );
  lines [ index + 1 .. ] . iter ()
    . take_while ( |line| line . starts_with (&deeper) )
    . map ( |line| line . to_string () )
    . collect () }

/// An unwarranted child of a parent that reads claims: it claims
/// nothing, and the truth does not relate it to its parent.
fn is_unwarranted_and_claims_nothing (line : &str) -> bool {
  line . contains ("noClaim") && line . contains ("noWarrant") }

/// k loses l (a leaf) and m (which holds mc), saved from a second view
/// of k. The first view's rerender keeps both, unwarranted and claiming
/// nothing, m still editable, with mc still its content; that buffer
/// saves.
async fn stale_content_is_kept_unwarranted (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let graph : InRustGraphHandle = graph_handle_from_config (config) ?;
  let mut views_state : ViewsState = empty_views_state ();
  let first_viewforest_id : ViewforestId = ViewforestId::ContentView ("convert-k-1" . into ());
  let second_viewforest_id : ViewforestId = ViewforestId::ContentView ("convert-k-2" . into ());
  let (k_view, _, _) = single_root_view (config, Some (tantivy), &ID::from ("k")) ?;
  save_view ( &k_view, &first_viewforest_id, config, tantivy, &graph,
              &mut views_state ) . await ?;
  let without_content : String =
    without_lines_containing (
      &without_lines_containing (
        &without_lines_containing (&k_view, "(gnid l)"),
        "(gnid m)" ),
      "(gnid mc)" );
  let (_, collateral_views) : (String, Vec<String>) = save_view (
    &without_content, &second_viewforest_id, config, tantivy, &graph,
    &mut views_state ) . await ?;
  assert_eq! ( collateral_views . len (), 1, "{:?}", collateral_views );
  let first_view : &str = &collateral_views [0];
  assert! ( is_unwarranted_and_claims_nothing ( &the_line (first_view, "** ", "(gnid l)") ),
    "the stale leaf is kept, unwarranted:\n{}", first_view );
  let m : String = the_line (first_view, "** ", "(gnid m)");
  assert! ( is_unwarranted_and_claims_nothing (&m) && ! m . contains ("writeProtected"),
    "the stale branch is kept, unwarranted, still editable:\n{}", first_view );
  let mc : String = the_line (first_view, "*** ", "(gnid mc)");
  assert! ( ! mc . contains ("noClaim") && ! mc . contains ("writeProtected"),
    "mc stays m's content:\n{}", first_view );
  save_view ( first_view, &first_viewforest_id, config, tantivy, &graph,
              &mut views_state ) . await ?;
  Ok (( )) }

/// In a view of k, a write-protected copy of m holding a note is
/// pasted, claiming nothing, beside the member m. The two siblings stand
/// for one node, so the refresh merges them: the first, the member m,
/// survives, still editable, and the note joins its content. When k
/// loses l and m, saved from a second view of k, the first view's
/// rerender keeps that m, unwarranted, with mc and the note; that
/// buffer saves.
async fn a_pasted_copy_beside_a_member_merges_into_it (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let graph : InRustGraphHandle = graph_handle_from_config (config) ?;
  let mut views_state : ViewsState = empty_views_state ();
  let first_viewforest_id : ViewforestId = ViewforestId::ContentView ("convert-k-1" . into ());
  let second_viewforest_id : ViewforestId = ViewforestId::ContentView ("convert-k-2" . into ());
  let (k_view, _, _) = single_root_view (config, Some (tantivy), &ID::from ("k")) ?;
  let with_copy : String = {
    let mc_line : String = k_view . lines ()
      . find ( |line| line . contains ("(gnid mc)") )
      . unwrap_or_else ( || panic! ( "mc is in the view:\n{}", k_view ))
      . to_string ();
    k_view . replacen (
      &mc_line,
      &format! ( "{}\n** (skg node (gnid m) (skgRepos (nodeRepo main)) noClaim writeProtected) m\n\
                  *** (skg node noClaim) a note under the copy", mc_line ),
      1 ) };
  save_view ( &with_copy, &first_viewforest_id, config, tantivy, &graph,
              &mut views_state ) . await ?;
  let without_content : String =
    without_lines_containing (
      &without_lines_containing (
        &without_lines_containing (&k_view, "(gnid l)"),
        "(gnid m)" ),
      "(gnid mc)" );
  let (_, collateral_views) : (String, Vec<String>) = save_view (
    &without_content, &second_viewforest_id, config, tantivy, &graph,
    &mut views_state ) . await ?;
  assert_eq! ( collateral_views . len (), 1, "{:?}", collateral_views );
  let first_view : &str = &collateral_views [0];
  let m : String = the_line (first_view, "** ", "(gnid m)");
  assert! ( is_unwarranted_and_claims_nothing (&m) && ! m . contains ("writeProtected"),
    "one m remains, unwarranted, still editable:\n{}", first_view );
  let under_m : Vec<String> = the_lines_under (first_view, "** ", "(gnid m)");
  assert! ( under_m . iter () . any ( |line| line . contains ("(gnid mc)") )
            && under_m . iter () . any ( |line| line . contains ("a note under the copy") ),
    "mc and the note stand under it:\n{}", first_view );
  save_view ( first_view, &first_viewforest_id, config, tantivy, &graph,
              &mut views_state ) . await ?;
  Ok (( )) }

/// A second view of k moves m, with mc, from k into l. The first
/// view's rerender keeps its m under k, unwarranted, editable, with mc,
/// and draws l's new member m, a repeat of a node whose editable
/// occurrence stands elsewhere, so write-protected: nothing yields its
/// editability. That buffer saves.
async fn a_kept_editable_occurrence_keeps_its_editability (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let graph : InRustGraphHandle = graph_handle_from_config (config) ?;
  let mut views_state : ViewsState = empty_views_state ();
  let first_viewforest_id : ViewforestId = ViewforestId::ContentView ("convert-k-1" . into ());
  let second_viewforest_id : ViewforestId = ViewforestId::ContentView ("convert-k-2" . into ());
  let (k_view, _, _) = single_root_view (config, Some (tantivy), &ID::from ("k")) ?;
  save_view ( &k_view, &first_viewforest_id, config, tantivy, &graph,
              &mut views_state ) . await ?;
  let m_under_l : String = // m directly follows l, so one level deeper it is l's
    k_view . lines ()
    . map ( |line| if line . contains ("(gnid m)") || line . contains ("(gnid mc)")
                   { format! ("*{}", line) } else { line . to_string () } )
    . collect::<Vec<String>> () . join ("\n") + "\n";
  let (second_view, collateral_views) : (String, Vec<String>) = save_view (
    &m_under_l, &second_viewforest_id, config, tantivy, &graph,
    &mut views_state ) . await ?;
  the_line (&second_view, "*** ", "(gnid m)"); // the second view moved m under l
  assert_eq! ( collateral_views . len (), 1, "{:?}", collateral_views );
  let first_view : &str = &collateral_views [0];
  let kept_m : String = the_line (first_view, "** ", "(gnid m)");
  assert! ( is_unwarranted_and_claims_nothing (&kept_m)
            && ! kept_m . contains ("writeProtected"),
    "the m under k is kept, unwarranted, editable:\n{}", first_view );
  let kept_mc : String = the_line (first_view, "*** ", "(gnid mc)");
  assert! ( ! kept_mc . contains ("noClaim") && ! kept_mc . contains ("writeProtected"),
    "with mc its content:\n{}", first_view );
  let member_m : String = the_line (first_view, "*** ", "(gnid m)");
  assert! ( ! member_m . contains ("noClaim") && member_m . contains ("writeProtected"),
    "l's member m is drawn write-protected:\n{}", first_view );
  save_view ( first_view, &first_viewforest_id, config, tantivy, &graph,
              &mut views_state ) . await ?;
  Ok (( )) }

/// A note typed under b in x's containerFolder. When b stops
/// containing x, x's view keeps b there, unwarranted, holding the note
/// and its chain (p). When b contains x again, b is warranted again,
/// the folder holds no second b, and x's view saves.
async fn a_folder_member_kept_unwarranted_is_warranted_when_its_membership_returns (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let graph : InRustGraphHandle = graph_handle_from_config (config) ?;
  let mut views_state : ViewsState = empty_views_state ();
  let x_viewforest_id : ViewforestId = ViewforestId::ContentView ("convert-x" . into ());
  let b_viewforest_id : ViewforestId = ViewforestId::ContentView ("convert-b" . into ());
  let (x_view, _, _) = single_root_view (config, Some (tantivy), &ID::from ("x")) ?;
  let (x_view, _) : (String, Vec<String>) = save_view (
    &x_view . replacen (
      "node ",
      "node (viewRequests (folder containers))", 1 ),
    &x_viewforest_id, config, tantivy, &graph, &mut views_state ) . await ?;
  let with_note : String = {
    let member_b : String = x_view . lines ()
      . find ( |line| line . starts_with ("*** ") && line . contains ("(gnid b)") )
      . unwrap_or_else ( || panic! ( "b is in the folder:\n{}", x_view ))
      . to_string ();
    x_view . replacen (
      &member_b,
      &format! ( "{}\n**** (skg node noClaim) a note under b", member_b ),
      1 ) };
  save_view ( &with_note, &x_viewforest_id, config, tantivy, &graph,
              &mut views_state ) . await ?;
  let (b_view, _, _) = single_root_view (config, Some (tantivy), &ID::from ("b")) ?;
  let (b_view, collateral_views) : (String, Vec<String>) = save_view (
    &without_lines_containing (
      &without_lines_containing (&b_view, "(gnid x)"), "(gnid xc)" ),
    &b_viewforest_id, config, tantivy, &graph, &mut views_state ) . await ?;
  let x_after_drop : &String = collateral_views . iter ()
    . find ( |view| view . contains ("containerFolder") )
    . unwrap_or_else ( || panic! ( "x's view is rerendered: {:?}", collateral_views ));
  let stale_b : String = the_line (x_after_drop, "*** ", "(gnid b)");
  assert! ( stale_b . contains ("noWarrant") && stale_b . contains ("writeProtected"),
    "the stale member is kept, unwarranted:\n{}", x_after_drop );
  assert! ( the_lines_under (x_after_drop, "*** ", "(gnid b)") . iter ()
              . any ( |line| line . contains ("a note under b") ),
    "the note stands under it:\n{}", x_after_drop );
  let (_, collateral_views) : (String, Vec<String>) = save_view (
    &format! ( "{}** (skg node (gnid x) (skgRepos (nodeRepo main))) x\n\
                *** (skg node (gnid xc) (skgRepos (nodeRepo main))) xc\n", b_view ),
    &b_viewforest_id, config, tantivy, &graph, &mut views_state ) . await ?;
  let x_after_return : &String = collateral_views . iter ()
    . find ( |view| view . contains ("containerFolder") )
    . unwrap_or_else ( || panic! ( "x's view is rerendered: {:?}", collateral_views ));
  let member_b : String = the_line (x_after_return, "*** ", "(gnid b)");
  assert! ( ! member_b . contains ("noWarrant"),
    "the kept b is warranted again, and the only b in the folder:\n{}",
    x_after_return );
  let under_b : Vec<String> = the_lines_under (x_after_return, "*** ", "(gnid b)");
  assert! ( under_b . len () == 2
            && under_b . iter () . any ( |line| line . contains ("(gnid p)")
                                                && line . contains ("writeProtected") )
            && under_b . iter () . any ( |line| line . contains ("a note under b") ),
    "b holds its chain and the note:\n{}", x_after_return );
  save_view ( x_after_return, &x_viewforest_id, config, tantivy, &graph,
              &mut views_state ) . await ?;
  Ok (( )) }

/// The rerender of a stored view whose d holds two copies of e (a
/// write-protected one holding ec, then an editable leaf) and two of f
/// merges each pair: the first e survives, editable since the other
/// was, holding ec; one f remains. That buffer saves.
async fn duplicate_members_merge (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let graph : InRustGraphHandle = graph_handle_from_config (config) ?;
  let mut views_state : ViewsState = empty_views_state ();
  let viewforest_id : ViewforestId = ViewforestId::ContentView ("convert-d" . into ());
  let view : String = rerendered_stored_view (
    "* (skg node (gnid d) (skgRepos (nodeRepo main))) d\n\
     ** (skg node (gnid e) (skgRepos (nodeRepo main)) writeProtected) e\n\
     *** (skg node (gnid ec) (skgRepos (nodeRepo main))) ec\n\
     ** (skg node (gnid f) (skgRepos (nodeRepo main))) f\n\
     ** (skg node (gnid e) (skgRepos (nodeRepo main))) e\n\
     ** (skg node (gnid f) (skgRepos (nodeRepo main)) writeProtected) f\n",
    &viewforest_id, config, tantivy, &graph, &mut views_state ) ?;
  let e : String = the_line (&view, "** ", "(gnid e)");
  assert! ( ! e . contains ("noClaim") && ! e . contains ("writeProtected"),
    "one e remains, the editable member:\n{}", view );
  assert! ( the_line_after (&view, "** ", "(gnid e)") . contains ("(gnid ec)"),
    "holding ec:\n{}", view );
  assert_eq! ( lines_containing (&view, "(gnid f)") . len (), 1,
    "one f remains:\n{}", view );
  save_view ( &view, &viewforest_id, config, tantivy, &graph,
              &mut views_state ) . await ?;
  Ok (( )) }

/// The rerender of a stored view whose d holds two copies of e, the
/// first editable, the second write-protected, each holding ec, merges
/// them, and then the two ec under the result: one e remains,
/// editable, holding one ec; that buffer saves. (A buffer holds at
/// most one editable occurrence of a node: validation refuses a saved
/// one with two, and every other is the server's own output.)
async fn editable_duplicates_merge_with_their_content (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let graph : InRustGraphHandle = graph_handle_from_config (config) ?;
  let mut views_state : ViewsState = empty_views_state ();
  let viewforest_id : ViewforestId = ViewforestId::ContentView ("convert-d" . into ());
  let view : String = rerendered_stored_view (
    "* (skg node (gnid d) (skgRepos (nodeRepo main))) d\n\
     ** (skg node (gnid e) (skgRepos (nodeRepo main))) e\n\
     *** (skg node (gnid ec) (skgRepos (nodeRepo main))) ec\n\
     ** (skg node (gnid f) (skgRepos (nodeRepo main))) f\n\
     ** (skg node (gnid e) (skgRepos (nodeRepo main)) writeProtected) e\n\
     *** (skg node (gnid ec) (skgRepos (nodeRepo main)) writeProtected) ec\n",
    &viewforest_id, config, tantivy, &graph, &mut views_state ) ?;
  let e : String = the_line (&view, "** ", "(gnid e)");
  assert! ( ! e . contains ("noClaim") && ! e . contains ("writeProtected"),
    "one e remains, the editable member:\n{}", view );
  let ec : String = the_line (&view, "*** ", "(gnid ec)");
  assert! ( ! ec . contains ("noClaim"), "holding one ec:\n{}", view );
  save_view ( &view, &viewforest_id, config, tantivy, &graph,
              &mut views_state ) . await ?;
  Ok (( )) }

/// Register 'stored', a buffer's text rooted at d, as the view
/// 'viewforest_id', rerender every view, and return its text.
fn rerendered_stored_view (
  stored        : &str,
  viewforest_id : &ViewforestId,
  config        : &SkgConfig,
  tantivy       : &mut TantivyIndex,
  graph         : &InRustGraphHandle,
  views_state   : &mut ViewsState,
) -> Result<String, Box<dyn Error>> {
  let stored_tree : Tree<Viewnode> = org_to_viewnode_tree (stored, IdResolution::AsWritten) ? . 0;
  views_state . open_views . register_view (
    &graph . load_full (), viewforest_id . clone (), stored_tree, &[ID::from ("d")] );
  let env : SkgEnv = skg_env_from_parts (config, tantivy, graph);
  let all : SkgrepoRestriction =
    SkgrepoRestriction::named (config, SkgrepoSetName::from ("all")) ?;
  let (mut stream, read_end) : (TcpStream, TcpStream) = mk_test_tcp_stream_pair ();
  std::thread::scope ( |scope| {
    // The handler blocks on futures itself, so it cannot run inside
    // this test's executor.
    scope . spawn ( || handle_rerender_all_views_request (
      &mut stream, "((request . \"rerender all views\"))\n", &env,
      views_state, &all )); } );
  drop (stream);
  let rerendered : Vec<String> = contents_of_messages (read_end, "rerender-view");
  assert_eq! ( rerendered . len (), 1, "{:?}", rerendered );
  Ok ( rerendered [0] . clone () ) }

/// al loses its alias one, saved from a second view of al; the first
/// view's alias folder keeps it, unwarranted and claiming nothing.
async fn a_stale_alias_is_kept_unwarranted (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let graph : InRustGraphHandle = graph_handle_from_config (config) ?;
  let mut views_state : ViewsState = empty_views_state ();
  let first_viewforest_id : ViewforestId = ViewforestId::ContentView ("convert-al-1" . into ());
  let second_viewforest_id : ViewforestId = ViewforestId::ContentView ("convert-al-2" . into ());
  let (al_view, _, _) = single_root_view (config, Some (tantivy), &ID::from ("al")) ?;
  let requesting : String = al_view . replacen (
    "node ",
    "node (viewRequests (folder aliases))", 1 );
  save_view ( &requesting, &first_viewforest_id, config, tantivy, &graph,
              &mut views_state ) . await ?;
  let (second_view, _) : (String, Vec<String>) = save_view (
    &requesting, &second_viewforest_id, config, tantivy, &graph,
    &mut views_state ) . await ?;
  assert_eq! ( lines_containing (&second_view, "alias) one") . len (), 1,
    "the alias folder shows one:\n{}", second_view );
  let (_, collateral_views) : (String, Vec<String>) = save_view (
    &without_lines_containing (&second_view, "alias) one"),
    &second_viewforest_id, config, tantivy, &graph, &mut views_state ) . await ?;
  assert_eq! ( collateral_views . len (), 1, "{:?}", collateral_views );
  let first_view : &str = &collateral_views [0];
  assert! ( lines_containing (first_view, ") one") . iter ()
              . any ( |line| is_unwarranted_and_claims_nothing (line) ),
    "the stale alias is kept, unwarranted:\n{}", first_view );
  assert_eq! ( lines_containing (first_view, "alias) two") . len (), 1,
    "the other alias stays:\n{}", first_view );
  Ok (( )) }
