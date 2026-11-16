// cargo nextest run --test grouped_forks -E 'test(forks_folder::)'
//
// The one forks folder ('docs/glossary.org', "partner folder"): N's
// fork partners, each once. N forks E and M; M and F fork N; N hides
// H, which no partner contains; T relates to nothing. So N's forks
// folder holds E (N forks it: =bK=, claiming), M (a mutual: =bKb=,
// claiming), then F (it forks N: =Kb=, claiming nothing), and last the
// folder of what N hides outside its fork partners, holding H. Each
// sub-test edits that fresh view and saves it:
// - unchanged, it writes nothing;
// - a member typed there, claiming, makes N fork it;
// - deleting E ends N's fork of it, and E is gone;
// - deleting M ends N's fork of it and hides it, so it stands in the
//   hidden-outside folder;
// - detaching M ends the fork only, and M stands as a forker,
//   claiming nothing;
// - deleting F hides it, likewise; removing it from the hidden-outside
//   folder then unhides it, and it is drawn in the forks folder again;
// - attaching F makes N fork it, a mutual;
// - a relRepo request on F, which claims nothing, is refused.

use std::error::Error;
use crate::tcp_stream_pair::mk_test_tcp_stream_pair;
use std::net::TcpStream;
use std::collections::BTreeMap;

use skg::dbs::in_rust_graph::InRustGraphHandle;
use skg::dbs::node_lookup::graphnode_by_gnid;
use skg::metadata_canon::without_viewnode_ids;
use skg::serve::ViewsState;
use skg::serve::handlers::save_buffer::SaveResponse;
use skg::test_utils::{
  graph_handle_from_config, run_with_shared_test_stores, single_root_view};
use skg::test_utils::update_from_and_rerender_buffer_test as update_from_and_rerender_buffer;
use skg::types::errors::SaveError;
use skg::types::misc::{ID, SkgConfig, TantivyIndex, members_of};
use skg::types::nodes::complete::Graphnode;
use skg::types::views_state::{OpenViews, ViewforestId};

const FIXTURES : &str = "tests/forks_folder/fixtures";

#[test]
fn all_tests
  () -> Result<(), Box<dyn Error>> {
  run_with_shared_test_stores (
    "skg-test-forks-folder",
    |s| Box::pin ( async move {
      s . reset ("the_folder_holds_forkees_mutuals_then_forkers", FIXTURES) ?;
      the_folder_holds_forkees_mutuals_then_forkers (&s . config, &mut s . tantivy) . await ?;
      s . reset ("an_unchanged_save_writes_nothing", FIXTURES) ?;
      an_unchanged_save_writes_nothing (&s . config, &mut s . tantivy) . await ?;
      s . reset ("a_typed_member_is_forked", FIXTURES) ?;
      a_typed_member_is_forked (&s . config, &mut s . tantivy) . await ?;
      s . reset ("deleting_a_forkee_ends_the_fork", FIXTURES) ?;
      deleting_a_forkee_ends_the_fork (&s . config, &mut s . tantivy) . await ?;
      s . reset ("deleting_a_mutual_ends_the_fork_and_hides_it", FIXTURES) ?;
      deleting_a_mutual_ends_the_fork_and_hides_it (&s . config, &mut s . tantivy) . await ?;
      s . reset ("detaching_a_mutual_ends_the_fork_only", FIXTURES) ?;
      detaching_a_mutual_ends_the_fork_only (&s . config, &mut s . tantivy) . await ?;
      s . reset ("deleting_a_forker_hides_it_until_the_hide_is_removed", FIXTURES) ?;
      deleting_a_forker_hides_it_until_the_hide_is_removed (&s . config, &mut s . tantivy) . await ?;
      s . reset ("attaching_a_forker_makes_it_a_mutual", FIXTURES) ?;
      attaching_a_forker_makes_it_a_mutual (&s . config, &mut s . tantivy) . await ?;
      s . reset ("a_relRepo_request_on_a_forker_is_refused", FIXTURES) ?;
      a_relRepo_request_on_a_forker_is_refused (&s . config, &mut s . tantivy) . await ?;
      Ok (( )) } )) }

//
// Helpers
//

/// A fresh view of N, without viewnode IDs.
fn view_of_n (
  config  : &SkgConfig,
  tantivy : &TantivyIndex,
) -> Result<String, Box<dyn Error>> {
  let (view, _, _) = single_root_view (config, Some (tantivy), &ID::from ("N")) ?;
  Ok ( without_viewnode_ids (&view) ) }

async fn save (
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
    &Ok ( ViewforestId::ContentView ("view of N" . to_string ()) ),
    &mut views_state ) . await ?;
  assert! ( response . errors . is_empty (), "save errors: {:?}", response . errors );
  Ok (response) }

fn line_of<'a> (
  buf  : &'a str,
  gnid : &str,
) -> &'a str {
  buf . lines ()
    . find ( |line| line . contains (&format! ("(gnid {})", gnid)) )
    . unwrap_or_else ( || panic! ( "no line shows {} in:\n{}", gnid, buf )) }

/// 'buf' without the first line showing 'gnid', and the lines under
/// it: a forkee as such, drawn editable, shows its own folders.
fn without_line_of (
  buf  : &str,
  gnid : &str,
) -> String {
  let depth : &dyn Fn (&str) -> usize = &|line : &str| -> usize {
    line . chars () . take_while ( |c| *c == '*' ) . count () };
  let lines : Vec<&str> = buf . lines () . collect ();
  let at : usize = lines . iter ()
    . position ( |line| line . contains (&format! ("(gnid {})", gnid)) )
    . unwrap_or_else ( || panic! ( "no line shows {} in:\n{}", gnid, buf ));
  let below : usize = lines [at + 1 ..] . iter ()
    . take_while ( |line| depth (line) == 0 || depth (line) > depth (lines [at]) )
    . count ();
  lines [.. at] . iter () . chain ( lines [at + 1 + below ..] . iter () )
    . map ( |line| format! ("{}\n", line) ) . collect () }

/// The headline atoms (stars and the first words of the metadata) of
/// each line of 'buf', to read the outline of N's forks folder: down
/// to its members and the members of its hidden-outside folder, not
/// what a member as such shows under it.
fn outline (
  buf : &str,
) -> Vec<String> {
  let depth : &dyn Fn (&str) -> usize = &|line : &str| -> usize {
    line . chars () . take_while ( |c| *c == '*' ) . count () };
  let mut under_hidden_outside : bool = false;
  buf . lines ()
    . filter ( |line| line . starts_with ('*') )
    . filter ( |line| {
      if depth (line) == 3 {
        under_hidden_outside = line . contains ("hiddenOutsideOfForkPartnersFolder"); }
      depth (line) <= 3 || ( depth (line) == 4 && under_hidden_outside ) } )
    . map ( |line| line . split (" (skgRepos") . next () . unwrap () . to_string () )
    . collect () }

fn node (
  config : &SkgConfig,
  pid    : &str,
) -> Result<Graphnode, Box<dyn Error>> {
  graphnode_by_gnid (
    &graph_handle_from_config (config) ? . load_full (), config, &ID::from (pid) ) }

fn forks_of (
  config : &SkgConfig,
  pid    : &str,
) -> Result<Vec<ID>, Box<dyn Error>> {
  Ok ( members_of ( node (config, pid) ? . forks . or_default () )) }

fn hides_of (
  config : &SkgConfig,
  pid    : &str,
) -> Result<Vec<ID>, Box<dyn Error>> {
  Ok ( members_of ( node (config, pid) ? . hides . or_default () )) }

fn ids (
  pids : &[&str],
) -> Vec<ID> {
  pids . iter () . map ( |pid| ID::from (*pid) ) . collect () }

/// Every .skg file of the fixture's one skgrepo, by name.
fn skg_files (
  config : &SkgConfig,
) -> Result<BTreeMap<String, String>, Box<dyn Error>> {
  let folder : &std::path::Path = &config . skgrepos . values () . next () . unwrap () . path;
  let mut files : BTreeMap<String, String> = BTreeMap::new ();
  for entry in std::fs::read_dir (folder) ? {
    let path : std::path::PathBuf = entry ? . path ();
    if path . extension () . is_some_and ( |extension| extension == "skg" ) {
      files . insert ( path . file_name () . unwrap () . to_string_lossy () . to_string (),
                       std::fs::read_to_string (&path) ? ); }}
  Ok (files) }

//
// The sub-tests
//

async fn the_folder_holds_forkees_mutuals_then_forkers (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let view : String = view_of_n (config, tantivy) ?;
  assert_eq! ( outline (&view), [
    "* (skg node (gnid N)",
    "** (skg forksFolder)",
    "*** (skg node (gnid E)",
    "*** (skg node (gnid M)",
    "*** (skg node (gnid F)",
    "*** (skg hiddenOutsideOfForkPartnersFolder)",
    "**** (skg folded node (gnid H)" ], "{}", view );
  let e : &str = line_of (&view, "E");
  assert! ( ! e . contains ("noClaim")
            && e . contains ("(forks (in 1 (ancestors 2)))")
            && e . contains ("(birth (forks in 2))"),
            "E, which N forks, claims (bK):\n{}", view );
  let m : &str = line_of (&view, "M");
  assert! ( ! m . contains ("noClaim")
            && m . contains ("(forks (in 1 (ancestors 2)) (out 1 (ancestors 2)))")
            && m . contains ("(birth (forks in 2) (forks out 2))"),
            "M, a mutual, claims (bKb):\n{}", view );
  let f : &str = line_of (&view, "F");
  assert! ( f . contains ("noClaim")
            && f . contains ("(forks (out 1 (ancestors 2)))")
            && f . contains ("(birth (forks out 2))"),
            "F, which forks N, claims nothing (Kb):\n{}", view );
  Ok (( )) }

async fn an_unchanged_save_writes_nothing (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let view : String = view_of_n (config, tantivy) ?;
  let before : BTreeMap<String, String> = skg_files (config) ?;
  let response : SaveResponse = save (&view, config, tantivy) . await ?;
  assert_eq! ( skg_files (config) ?, before );
  assert_eq! ( without_viewnode_ids (&response . saved_view), view );
  Ok (( )) }

async fn a_typed_member_is_forked (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let view : String = view_of_n (config, tantivy) ?;
  let m : &str = line_of (&view, "M");
  let typed : String = view . replace (
    m, &format! ( "{}\n*** (skg node (gnid T) (skgRepos (nodeRepo main))) T", m ));
  let response : SaveResponse = save (&typed, config, tantivy) . await ?;
  assert_eq! ( forks_of (config, "N") ?, ids (&["E", "M", "T"]) );
  assert! ( ! line_of (&response . saved_view, "T") . contains ("noClaim"),
            "{}", response . saved_view );
  Ok (( )) }

async fn deleting_a_forkee_ends_the_fork (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let view : String = view_of_n (config, tantivy) ?;
  let response : SaveResponse =
    save (&without_line_of (&view, "E"), config, tantivy) . await ?;
  assert_eq! ( forks_of (config, "N") ?, ids (&["M"]) );
  assert_eq! ( hides_of (config, "N") ?, ids (&["H"]) );
  assert! ( ! response . saved_view . contains ("(gnid E)"),
            "{}", response . saved_view );
  Ok (( )) }

async fn deleting_a_mutual_ends_the_fork_and_hides_it (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let view : String = view_of_n (config, tantivy) ?;
  let response : SaveResponse =
    save (&without_line_of (&view, "M"), config, tantivy) . await ?;
  assert_eq! ( forks_of (config, "N") ?, ids (&["E"]) );
  assert_eq! ( hides_of (config, "N") ?, ids (&["H", "M"]) );
  assert_eq! ( outline (&without_viewnode_ids (&response . saved_view)), [
    "* (skg node (gnid N)",
    "** (skg forksFolder)",
    "*** (skg node (gnid E)",
    "*** (skg node (gnid F)",
    "*** (skg hiddenOutsideOfForkPartnersFolder)",
    "**** (skg folded node (gnid H)",
    "**** (skg folded node (gnid M)" ], "{}", response . saved_view ); // a folder that stood folded draws a new member folded
  Ok (( )) }

async fn detaching_a_mutual_ends_the_fork_only (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let view : String = view_of_n (config, tantivy) ?;
  let m : &str = line_of (&view, "M");
  let detached : String = view . replace (
    m, &m . replacen ("(skgRepos (nodeRepo main))", "(skgRepos (nodeRepo main)) noClaim", 1) );
  let response : SaveResponse = save (&detached, config, tantivy) . await ?;
  assert_eq! ( forks_of (config, "N") ?, ids (&["E"]) );
  assert_eq! ( hides_of (config, "N") ?, ids (&["H"]) );
  let m_after : &str = line_of (&response . saved_view, "M");
  assert! ( m_after . contains ("noClaim") && m_after . contains ("(birth (forks out 2))"),
            "M stands as a forker, claiming nothing:\n{}", response . saved_view );
  Ok (( )) }

async fn deleting_a_forker_hides_it_until_the_hide_is_removed (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let view : String = view_of_n (config, tantivy) ?;
  let hidden : SaveResponse =
    save (&without_line_of (&view, "F"), config, tantivy) . await ?;
  assert_eq! ( forks_of (config, "N") ?, ids (&["E", "M"]) );
  assert_eq! ( hides_of (config, "N") ?, ids (&["H", "F"]) );
  let hidden_view : String = without_viewnode_ids (&hidden . saved_view);
  assert_eq! ( outline (&hidden_view), [
    "* (skg node (gnid N)",
    "** (skg forksFolder)",
    "*** (skg node (gnid E)",
    "*** (skg node (gnid M)",
    "*** (skg hiddenOutsideOfForkPartnersFolder)",
    "**** (skg folded node (gnid H)",
    "**** (skg folded node (gnid F)" ], "{}", hidden_view );
  let unhidden : SaveResponse =
    save (&without_line_of (&hidden_view, "F"), config, tantivy) . await ?;
  assert_eq! ( hides_of (config, "N") ?, ids (&["H"]) );
  assert_eq! ( outline (&without_viewnode_ids (&unhidden . saved_view)), outline (&view),
               "{}", unhidden . saved_view );
  Ok (( )) }

async fn attaching_a_forker_makes_it_a_mutual (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let view : String = view_of_n (config, tantivy) ?;
  let f : &str = line_of (&view, "F");
  let attached : String = view . replace ( f, &f . replacen (" noClaim", "", 1) );
  let response : SaveResponse = save (&attached, config, tantivy) . await ?;
  assert_eq! ( forks_of (config, "N") ?, ids (&["E", "M", "F"]) );
  assert! ( line_of (&response . saved_view, "F")
            . contains ("(birth (forks in 2) (forks out 2))"),
            "F is a mutual:\n{}", response . saved_view );
  Ok (( )) }

async fn a_relRepo_request_on_a_forker_is_refused (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let view : String = view_of_n (config, tantivy) ?;
  let f : &str = line_of (&view, "F");
  let requested : String = view . replace (
    f, &f . replacen (" noClaim", " noClaim (editRequest (relRepo main))", 1) );
  let graph : InRustGraphHandle = graph_handle_from_config (config) ?;
  let mut views_state : ViewsState = ViewsState {
    open_views     : OpenViews::new (),
    graph_straddle : None, };
  let (mut stream, _read_end) : (TcpStream, TcpStream) =
    mk_test_tcp_stream_pair ();
  let refusal : String = match update_from_and_rerender_buffer (
    &mut stream, &requested, config, tantivy, &graph,
    &Ok ( ViewforestId::ContentView ("view of N" . to_string ()) ),
    &mut views_state ) . await {
    Ok (response) => response . errors . join ("\n"),
    Err (error) => match error . downcast_ref::<SaveError> () {
      Some (SaveError::BufferValidationErrors { errors, .. }) =>
        format! ("{:?}", errors),
      _ => error . to_string (), }, };
  assert! ( refusal . contains ("a member of a forks folder that claims nothing"),
            "{}", refusal );
  assert_eq! ( forks_of (config, "N") ?, ids (&["E", "M"]) );
  Ok (( )) }
