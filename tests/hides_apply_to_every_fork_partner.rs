// cargo nextest run --test grouped_forks -E 'test(hides_apply_to_every_fork_partner::)'
//
// A note's hides apply to every member its forks folder shows,
// forkees, forkers and indirect members alike ('docs/glossary.org',
// "hides"); a forker the note hides is none of them. This file tests
// the fork partners, forkees and forkers; 'hides_by_integration.rs'
// and 'flat_forks_folders.rs' take in indirect members. The fixture: N,
// owned, contains X1 and X2, forks E and hides W and H2. E, its
// forkee, contains e1; F, its forker, contains X1, W and f1. G stands
// alone. So W, which only F contains, has a reason: it stands in F's
// folder of what N hides in it, and not in the folder of what N
// hides outside its fork partners, which holds H2 alone. Each
// sub-test edits a view of N and saves it:
// - a fresh view draws W under F and H2 outside;
// - removing H2 from the hidden-outside folder unhides it, and adding
//   G there hides G, as before;
// - W moved out of F's folder to stand under F is unhidden, though the
//   buffer keeps the hidden-outside folder (which does not list W);
// - X1, removed from N's content, is hidden in N, since F still
//   contains it: otherwise it would come back under F; it is drawn
//   folded, as W beside it is;
// - H2 removed from the hidden-outside folder, its only member, is
//   unhidden, and the emptied folder goes, as in a fresh view.
// The floors and the skgrepo narrowings of the same inference are in
// 'tests/hide_inference_under_a_narrowing.rs'.

use std::error::Error;
use crate::tcp_stream_pair::mk_test_tcp_stream_pair;
use std::net::TcpStream;

use skg::dbs::in_rust_graph::InRustGraphHandle;
use skg::dbs::node_lookup::graphnode_by_gnid;
use skg::metadata_canon::without_viewnode_ids;
use skg::serve::ViewsState;
use skg::serve::handlers::save_buffer::SaveResponse;
use skg::test_utils::{
  graph_handle_from_config, run_with_shared_test_stores, single_root_view};
use skg::test_utils::update_from_and_rerender_buffer_test as update_from_and_rerender_buffer;
use skg::types::misc::{ID, SkgConfig, TantivyIndex, members_of};
use skg::types::nodes::complete::Graphnode;
use skg::types::views_state::{OpenViews, ViewforestId};

const FIXTURES : &str = "tests/hides_apply_to_every_fork_partner/fixtures";

#[test]
fn all_tests
  () -> Result<(), Box<dyn Error>> {
  run_with_shared_test_stores (
    "skg-test-hides-apply-to-every-fork-partner",
    |s| Box::pin ( async move {
      s . reset ("a_hide_only_a_forker_explains_stands_under_that_forker", FIXTURES) ?;
      a_hide_only_a_forker_explains_stands_under_that_forker (&s . config, &mut s . tantivy) . await ?;
      s . reset ("editing_the_hidden_outside_folder_unhides_and_hides", FIXTURES) ?;
      editing_the_hidden_outside_folder_unhides_and_hides (&s . config, &mut s . tantivy) . await ?;
      s . reset ("an_unhide_under_a_forker_survives_the_hidden_outside_folder", FIXTURES) ?;
      an_unhide_under_a_forker_survives_the_hidden_outside_folder (&s . config, &mut s . tantivy) . await ?;
      s . reset ("removing_a_child_a_forker_contains_hides_it", FIXTURES) ?;
      removing_a_child_a_forker_contains_hides_it (&s . config, &mut s . tantivy) . await ?;
      s . reset ("emptying_the_hidden_outside_folder_lets_it_go", FIXTURES) ?;
      emptying_the_hidden_outside_folder_lets_it_go (&s . config, &mut s . tantivy) . await ?;
      Ok (( )) } )) }

//
// Helpers
//

/// Save 'buf' as a view of N; it must save without errors.
async fn save_cleanly (
  buf     : &str,
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<SaveResponse, Box<dyn Error>> {
  let graph : InRustGraphHandle = graph_handle_from_config (config) ?;
  let mut views_state : ViewsState = ViewsState {
    open_views     : OpenViews::new (),
    graph_straddle : None, };
  let (mut stream, _read_end) : (TcpStream, TcpStream) =
    mk_test_tcp_stream_pair ();
  let response : SaveResponse = update_from_and_rerender_buffer (
    &mut stream, buf, config, tantivy, &graph,
    &Ok ( ViewforestId::ContentView ( "view of N" . to_string () )),
    &mut views_state ) . await ?;
  assert! ( response . errors . is_empty (),
            "save errors: {:?}\n\nbuffer:\n{}", response . errors, buf );
  Ok (response) }

/// A fresh view of N, without viewnode IDs.
fn view_of_n (
  config  : &SkgConfig,
  tantivy : &TantivyIndex,
) -> Result<String, Box<dyn Error>> {
  let (view, _, _) = single_root_view (config, Some (tantivy), &ID::from ("N")) ?;
  Ok ( without_viewnode_ids (&view) ) }

fn depth (
  line : &str,
) -> usize {
  line . chars () . take_while ( |c| *c == '*' ) . count () }

/// The first line showing 'gnid'.
fn line_of<'a> (
  buf  : &'a str,
  gnid : &str,
) -> &'a str {
  buf . lines ()
    . find ( |line| line . contains (&format! ("(gnid {})", gnid)) )
    . unwrap_or_else ( || panic! ( "no line shows {} in:\n{}", gnid, buf )) }

/// The first line containing 'text', and the lines under it.
fn subtree_at<'a> (
  buf  : &'a str,
  text : &str,
) -> Vec<&'a str> {
  let lines : Vec<&str> = buf . lines () . collect ();
  let at : usize = lines . iter ()
    . position ( |line| line . contains (text) )
    . unwrap_or_else ( || panic! ( "no line holds {} in:\n{}", text, buf ));
  let below : usize = lines [at + 1 ..] . iter ()
    . take_while ( |line| depth (line) > depth (lines [at]) ) . count ();
  lines [at ..= at + below] . to_vec () }

/// The gnids of the child headlines (one level below) of the first
/// line containing 'text'.
fn gnids_under (
  buf  : &str,
  text : &str,
) -> Vec<String> {
  let subtree : Vec<&str> = subtree_at (buf, text);
  let child_depth : usize = depth (subtree [0]) + 1;
  subtree [1 ..] . iter ()
    . filter ( |line| depth (line) == child_depth )
    . filter_map ( |line| line . split ("(gnid ") . nth (1) )
    . map ( |rest| rest . split (')') . next () . unwrap () . to_string () )
    . collect () }

/// 'buf' with 'lines' (consecutive, in order) removed.
fn without_lines (
  buf   : &str,
  lines : &[&str],
) -> String {
  buf . replacen ( &(lines . join ("\n") + "\n"), "", 1 ) }

/// 'buf' with 'new' right after the first line showing 'gnid'.
fn with_line_after (
  buf  : &str,
  gnid : &str,
  new  : &str,
) -> String {
  let line : &str = line_of (buf, gnid);
  buf . replacen ( &format! ("{}\n", line), &format! ("{}\n{}\n", line, new), 1 ) }

fn hides_of (
  config : &SkgConfig,
  pid    : &str,
) -> Result<Vec<ID>, Box<dyn Error>> {
  let node : Graphnode = graphnode_by_gnid (
    &graph_handle_from_config (config) ? . load_full (), config, &ID::from (pid) ) ?;
  Ok ( members_of ( node . hides . or_default () ) ) }

fn ids (
  pids : &[&str],
) -> Vec<ID> {
  pids . iter () . map ( |pid| ID::from (*pid) ) . collect () }

//
// The sub-tests
//

/// W, which N hides and only F (a forker) contains, stands in F's
/// folder of what N hides in it, and not in the folder of what N
/// hides outside its fork partners; H2, which no partner contains,
/// stands there.
async fn a_hide_only_a_forker_explains_stands_under_that_forker (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let view : String = view_of_n (config, tantivy) ?;
  let under_f : Vec<&str> = subtree_at (&view, "(gnid F)");
  let under_f : String = under_f . join ("\n") + "\n";
  assert_eq! ( gnids_under (&under_f, "hiddenInForkPartnerFolder"), vec! ["W"],
               "W stands in F's folder of what N hides in it:\n{}", view );
  assert_eq! ( gnids_under (&view, "hiddenOutsideOfForkPartnersFolder"), vec! ["H2"],
               "the hidden-outside folder holds H2 alone:\n{}", view );
  Ok (( )) }

/// Removing H2 from the hidden-outside folder unhides it, and adding
/// G there hides G. W, which the folder does not list, stays hidden.
async fn editing_the_hidden_outside_folder_unhides_and_hides (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let view : String = view_of_n (config, tantivy) ?;
  let h2 : &str = line_of (&view, "H2");
  let g : String = format! ( "{} (skg node (gnid G) (skgRepos (nodeRepo main))) G",
                             "*" . repeat (depth (h2)) );
  let edited : String = view . replacen ( &format! ("{}\n", h2), &format! ("{}\n", g), 1 );
  let response : SaveResponse = save_cleanly (&edited, config, tantivy) . await ?;
  assert_eq! ( hides_of (config, "N") ?, ids (&["W", "G"]),
               "H2 is unhidden, G hidden, W kept" );
  assert_eq! ( gnids_under ( &without_viewnode_ids (&response . saved_view),
                             "hiddenOutsideOfForkPartnersFolder" ),
               vec! ["G"], "{}", response . saved_view );
  Ok (( )) }

/// W, moved out of F's folder of what N hides in it to stand under F,
/// claiming, is unhidden in N, though the buffer keeps the folder of
/// what N hides outside its fork partners (holding H2): that folder
/// does not list W, so its edit does not hide W again.
async fn an_unhide_under_a_forker_survives_the_hidden_outside_folder (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let view : String = view_of_n (config, tantivy) ?;
  let hidden_in : Vec<&str> = subtree_at (&view, "hiddenInForkPartnerFolder");
  assert! ( hidden_in [1] . contains ("(gnid W)")
            && hidden_in [2 ..] . iter () . all ( |line| depth (line) > depth (hidden_in [1]) ),
            "the folder holds W alone (with its chain): {:?}", hidden_in );
  let f1 : &str = line_of (&view, "f1");
  let w : String = format! ( "{} (skg node (gnid W) (skgRepos (nodeRepo main))) W",
                             "*" . repeat (depth (f1)) );
  let moved : String = with_line_after (
    &without_lines (&view, &hidden_in), "f1", &w );
  assert! ( moved . contains ("hiddenOutsideOfForkPartnersFolder"),
            "the buffer keeps the hidden-outside folder:\n{}", moved );
  let response : SaveResponse = save_cleanly (&moved, config, tantivy) . await ?;
  assert_eq! ( hides_of (config, "N") ?, ids (&["H2"]), "W is unhidden, H2 kept" );
  let saved : String = without_viewnode_ids (&response . saved_view);
  assert! ( gnids_under (&saved, "(gnid F)") . contains (&"W" . to_string ()),
            "W stands under F:\n{}", saved );
  Ok (( )) }

/// X1, which F contains, removed from N's content, is hidden in N:
/// otherwise it would come back as content F adds to N. The redrawn
/// view draws it in F's folder of what N hides in it, folded, as W
/// beside it is: the folder stays collapsed.
async fn removing_a_child_a_forker_contains_hides_it (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let view : String = view_of_n (config, tantivy) ?;
  let x1 : &str = view . lines ()
    . find ( |line| depth (line) == 2 && line . contains ("(gnid X1)") )
    . unwrap_or_else ( || panic! ( "no X1 in N's content:\n{}", view ));
  let response : SaveResponse =
    save_cleanly (&without_lines (&view, &[x1]), config, tantivy) . await ?;
  assert_eq! ( hides_of (config, "N") ?, ids (&["W", "H2", "X1"]), "X1 is hidden" );
  assert! ( response . hides . iter ()
              . any ( |line| line == "Hides 'X1' from 'N', as content of 'F'." ),
            "{:?}", response . hides );
  let saved : String = without_viewnode_ids (&response . saved_view);
  let under_f : String = subtree_at (&saved, "(gnid F)") . join ("\n") + "\n";
  assert_eq! ( gnids_under (&under_f, "hiddenInForkPartnerFolder"), vec! ["W", "X1"],
               "X1 joins W in F's folder of what N hides in it:\n{}", saved );
  assert! ( line_of (&under_f, "W") . contains ("(skg folded node")
            && line_of (&under_f, "X1") . contains ("(skg folded node"),
            "X1 is drawn folded, as W is:\n{}", saved );
  Ok (( )) }

/// H2, the one member of the folder of what N hides outside its fork
/// partners, removed there, is unhidden, and the emptied folder goes
/// from the redrawn view, as a fresh view draws none.
async fn emptying_the_hidden_outside_folder_lets_it_go (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let view : String = view_of_n (config, tantivy) ?;
  let response : SaveResponse =
    save_cleanly (&without_lines (&view, &[line_of (&view, "H2")]), config, tantivy) . await ?;
  assert_eq! ( hides_of (config, "N") ?, ids (&["W"]), "H2 is unhidden" );
  let saved : String = without_viewnode_ids (&response . saved_view);
  assert! ( ! saved . contains ("hiddenOutsideOfForkPartnersFolder"),
            "the emptied folder goes:\n{}", saved );
  assert_eq! ( saved, view_of_n (config, tantivy) ?, "the redrawn view is the fresh view" );
  Ok (( )) }
