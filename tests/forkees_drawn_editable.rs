// cargo nextest run --test grouped_forks -E 'test(forkees_drawn_editable::)'
//
// Forkees as such drawn editable ('TODO/every-mirror/2_plan.org', slice
// 2; 'docs/glossary.org', "forkee as such"). N, owned, contains n1,
// forks E, owned, and F, foreign, and hides H, which E contains, and
// G, which F contains. E contains e1, H, e2; F contains f1, G. FN,
// foreign, forks E. Each sub-test edits a view and saves it:
// - a fresh view of N draws E and F editable, each showing its
//   content less what N hides, and its folder of what N hides there;
// - saved unchanged, it writes nothing;
// - cutting e2 from E makes N hide e2;
// - E's title, edited there, is saved; F's is refused;
// - a headline typed under E joins E's content, after the sibling it
//   follows; one typed under F is refused;
// - with a node budget of 1, E and F are drawn write-protected, bare;
// - a buffer drawing E write-protected (as an older server did)
//   rerenders as it is, and an editable-view request on N extends E;
// - a cut from E in a view of FN, foreign, is refused.

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
use skg::types::save::format_save_error_as_org;
use skg::types::views_state::{OpenViews, ViewforestId};

const FIXTURES : &str = "tests/forkees_drawn_editable/fixtures";

#[test]
fn all_tests
  () -> Result<(), Box<dyn Error>> {
  run_with_shared_test_stores (
    "skg-test-forkees-drawn-editable",
    |s| Box::pin ( async move {
      s . reset ("a_fresh_view_draws_each_forkee_editable", FIXTURES) ?;
      a_fresh_view_draws_each_forkee_editable (&s . config, &mut s . tantivy) . await ?;
      s . reset ("an_unchanged_save_writes_nothing", FIXTURES) ?;
      an_unchanged_save_writes_nothing (&s . config, &mut s . tantivy) . await ?;
      s . reset ("cutting_from_an_owned_forkee_hides", FIXTURES) ?;
      cutting_from_an_owned_forkee_hides (&s . config, &mut s . tantivy) . await ?;
      s . reset ("an_owned_forkees_title_is_saved", FIXTURES) ?;
      an_owned_forkees_title_is_saved (&s . config, &mut s . tantivy) . await ?;
      s . reset ("a_foreign_forkees_title_is_refused", FIXTURES) ?;
      a_foreign_forkees_title_is_refused (&s . config, &mut s . tantivy) . await ?;
      s . reset ("a_child_typed_under_an_owned_forkee_joins_its_content", FIXTURES) ?;
      a_child_typed_under_an_owned_forkee_joins_its_content (&s . config, &mut s . tantivy) . await ?;
      s . reset ("a_child_typed_under_a_foreign_forkee_is_refused", FIXTURES) ?;
      a_child_typed_under_a_foreign_forkee_is_refused (&s . config, &mut s . tantivy) . await ?;
      s . reset ("a_spent_budget_leaves_forkees_write_protected", FIXTURES) ?;
      a_spent_budget_leaves_forkees_write_protected (&s . config, &mut s . tantivy) . await ?;
      s . reset ("a_write_protected_forkee_stays_until_its_forker_is_extended", FIXTURES) ?;
      a_write_protected_forkee_stays_until_its_forker_is_extended (&s . config, &mut s . tantivy) . await ?;
      s . reset ("a_cut_under_a_foreign_forker_is_refused", FIXTURES) ?;
      a_cut_under_a_foreign_forker_is_refused (&s . config, &mut s . tantivy) . await ?;
      Ok (( )) } )) }

//
// Helpers
//

/// A fresh view of 'root', without viewnode IDs.
fn view_of (
  config  : &SkgConfig,
  tantivy : &TantivyIndex,
  root    : &str,
) -> Result<String, Box<dyn Error>> {
  let (view, _, _) = single_root_view (config, Some (tantivy), &ID::from (root)) ?;
  Ok ( without_viewnode_ids (&view) ) }

/// Save 'buf' as a view of 'root'.
async fn save (
  buf     : &str,
  root    : &str,
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<SaveResponse, Box<dyn Error>> {
  let graph : InRustGraphHandle = graph_handle_from_config (config) ?;
  let mut views_state : ViewsState = ViewsState {
    open_views     : OpenViews::new (),
    graph_straddle : None, };
  let (mut stream, _read_end) : (TcpStream, TcpStream) =
    mk_test_tcp_stream_pair ();
  update_from_and_rerender_buffer (
    &mut stream, buf, config, tantivy, &graph,
    &Ok ( ViewforestId::ContentView ( format! ("view of {}", root) )),
    &mut views_state ) . await }

/// Save 'buf', which must save cleanly; the saved view, without
/// viewnode IDs.
async fn save_cleanly (
  buf     : &str,
  root    : &str,
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<String, Box<dyn Error>> {
  let response : SaveResponse = save (buf, root, config, tantivy) . await ?;
  assert! ( response . errors . is_empty (), "save errors: {:?}", response . errors );
  Ok ( without_viewnode_ids (&response . saved_view) ) }

/// What the save of 'buf' says, which must refuse it.
async fn refusal_of (
  buf     : &str,
  root    : &str,
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<String, Box<dyn Error>> {
  Ok ( match save (buf, root, config, tantivy) . await {
    Ok (response) => {
      assert! ( ! response . errors . is_empty (),
                "the save must be refused; it saved:\n{}", response . saved_view );
      format! ("{:?}", response . errors) },
    Err (error) => match error . downcast_ref::<SaveError> () {
      Some (save_error) => format_save_error_as_org (save_error),
      None => error . to_string (), }, } ) }

/// The first line showing 'gnid'.
fn line_of<'a> (
  buf  : &'a str,
  gnid : &str,
) -> &'a str {
  buf . lines ()
    . find ( |line| line . contains (&format! ("(gnid {})", gnid)) )
    . unwrap_or_else ( || panic! ( "no line shows {} in:\n{}", gnid, buf )) }

fn depth (
  line : &str,
) -> usize {
  line . chars () . take_while ( |c| *c == '*' ) . count () }

/// The lines under the first line showing 'gnid'.
fn under<'a> (
  buf  : &'a str,
  gnid : &str,
) -> Vec<&'a str> {
  let lines : Vec<&str> = buf . lines () . collect ();
  let at : usize = lines . iter ()
    . position ( |line| line . contains (&format! ("(gnid {})", gnid)) )
    . unwrap_or_else ( || panic! ( "no line shows {} in:\n{}", gnid, buf ));
  lines [at + 1 ..] . iter () . copied ()
    . take_while ( |line| depth (line) == 0 || depth (line) > depth (lines [at]) )
    . collect () }

/// 'buf' without the first line showing 'gnid' and the lines under it.
fn without_subtree_of (
  buf  : &str,
  gnid : &str,
) -> String {
  let head : &str = line_of (buf, gnid);
  let below : Vec<&str> = under (buf, gnid);
  let mut removed : String = format! ("{}\n", head);
  for line in below { removed . push_str (&format! ("{}\n", line)); }
  buf . replacen (&removed, "", 1) }

/// 'buf' with the line 'typed' right after the first line showing
/// 'gnid' and what stands under it.
fn with_line_after_subtree_of (
  buf   : &str,
  gnid  : &str,
  typed : &str,
) -> String {
  let head : &str = line_of (buf, gnid);
  let below : Vec<&str> = under (buf, gnid);
  let mut subtree : String = format! ("{}\n", head);
  for line in below { subtree . push_str (&format! ("{}\n", line)); }
  buf . replacen (&subtree, &format! ("{}{}\n", subtree, typed), 1) }

/// The child headlines (one level below) of the first line showing
/// 'gnid'.
fn children_of<'a> (
  buf  : &'a str,
  gnid : &str,
) -> Vec<&'a str> {
  let parent_depth : usize = depth (line_of (buf, gnid));
  under (buf, gnid) . into_iter ()
    . filter ( |line| depth (line) == parent_depth + 1 )
    . collect () }

fn node (
  config : &SkgConfig,
  pid    : &str,
) -> Result<Graphnode, Box<dyn Error>> {
  graphnode_by_gnid (
    &graph_handle_from_config (config) ? . load_full (), config, &ID::from (pid) ) }

fn ids (
  pids : &[&str],
) -> Vec<ID> {
  pids . iter () . map ( |pid| ID::from (*pid) ) . collect () }

/// Every .skg file of the fixture, by skgrepo and name.
fn skg_files (
  config : &SkgConfig,
) -> Result<BTreeMap<String, String>, Box<dyn Error>> {
  let mut files : BTreeMap<String, String> = BTreeMap::new ();
  for (name, skgrepo) in &config . skgrepos {
    for entry in std::fs::read_dir (&skgrepo . path) ? {
      let path : std::path::PathBuf = entry ? . path ();
      if path . extension () . is_some_and ( |extension| extension == "skg" ) {
        files . insert (
          format! ( "{}/{}", name, path . file_name () . unwrap () . to_string_lossy () ),
          std::fs::read_to_string (&path) ? ); }}}
  Ok (files) }

//
// The sub-tests
//

/// E, owned, and F, foreign, each stand in N's forks folder editable,
/// claiming, showing their content less what N hides (e1 and e2; f1),
/// with the folder of what N hides in it first (H; G), folded.
async fn a_fresh_view_draws_each_forkee_editable (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let view : String = view_of (config, tantivy, "N") ?;
  for (forkee, shown, hidden) in [ ("E", vec! ["e1", "e2"], "H"),
                                   ("F", vec! ["f1"], "G") ] {
    let line : &str = line_of (&view, forkee);
    assert! ( line . starts_with ("*** ")
              && ! line . contains ("writeProtected")
              && ! line . contains ("noClaim"),
              "{} is drawn editable, claiming, in N's forks folder:\n{}", forkee, view );
    let children : Vec<&str> = children_of (&view, forkee);
    assert! ( children . first () . is_some_and (
                |line| line . contains ("hiddenInForkPartnerFolder") ),
              "{}'s folder of what N hides in it comes first:\n{}", forkee, view );
    let content : Vec<&str> = children . iter () . copied ()
      . filter ( |line| line . contains ("(skg node ") ) . collect ();
    assert_eq! ( content . len (), shown . len (), "{}:\n{}", forkee, view );
    for (line, gnid) in content . iter () . zip (shown) {
      assert! ( line . contains (&format! ("(gnid {})", gnid))
                && ! line . contains ("writeProtected"),
                "{} shows {}, editable:\n{}", forkee, gnid, view ); }
    assert! ( under (&view, forkee) . iter () . any ( |line|
                line . contains (&format! ("(gnid {})", hidden))
                && line . contains ("folded") ),
              "{} holds {} in its folder of what N hides:\n{}", forkee, hidden, view ); }
  Ok (( )) }

/// The fresh view, saved unchanged, writes no file.
async fn an_unchanged_save_writes_nothing (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let view : String = view_of (config, tantivy, "N") ?;
  let before : BTreeMap<String, String> = skg_files (config) ?;
  let saved : String = save_cleanly (&view, "N", config, tantivy) . await ?;
  assert_eq! ( skg_files (config) ?, before, "an unchanged save writes nothing" );
  assert_eq! ( saved, view, "and draws the view again as it was" );
  Ok (( )) }

/// Cutting e2 from E, N's forkee as such, hides e2 in N.
async fn cutting_from_an_owned_forkee_hides (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let view : String = view_of (config, tantivy, "N") ?;
  save_cleanly (&without_subtree_of (&view, "e2"), "N", config, tantivy) . await ?;
  assert_eq! ( members_of ( node (config, "N") ? . hides . or_default () ),
               ids (&["H", "G", "e2"]) );
  assert_eq! ( members_of (& node (config, "E") ? . contains),
               ids (&["e1", "H", "e2"]), "E's own list is as it was" );
  Ok (( )) }

/// E's title, edited in N's forks folder, is saved, and nothing else
/// of E changes.
async fn an_owned_forkees_title_is_saved (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let view : String = view_of (config, tantivy, "N") ?;
  let e_line : &str = line_of (&view, "E");
  let edited : String = view . replacen (
    e_line, &e_line . replace (")) E", ")) E, retitled"), 1 );
  assert_ne! ( edited, view );
  let saved : String = save_cleanly (&edited, "N", config, tantivy) . await ?;
  let e : Graphnode = node (config, "E") ?;
  assert_eq! ( e . title, "E, retitled" );
  assert_eq! ( members_of (&e . contains), ids (&["e1", "H", "e2"]) );
  assert! ( line_of (&saved, "E") . ends_with (" E, retitled"), "{}", saved );
  Ok (( )) }

/// F's title, edited in N's forks folder, is refused: F is foreign.
async fn a_foreign_forkees_title_is_refused (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let view : String = view_of (config, tantivy, "N") ?;
  let f_line : &str = line_of (&view, "F");
  let edited : String = view . replacen (
    f_line, &f_line . replace (")) F", ")) F, retitled"), 1 );
  let before : BTreeMap<String, String> = skg_files (config) ?;
  let refusal : String = refusal_of (&edited, "N", config, tantivy) . await ?;
  assert! ( refusal . contains ("Cannot edit title/body")
            && refusal . contains ("foreign"),
            "{}", refusal );
  assert_eq! ( skg_files (config) ?, before, "nothing is written" );
  Ok (( )) }

/// A headline typed under E, after e1, joins E's content right after
/// e1 (before H, which N hides), and the saved view shows it there.
async fn a_child_typed_under_an_owned_forkee_joins_its_content (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let view : String = view_of (config, tantivy, "N") ?;
  let stars : String = "*" . repeat ( depth (line_of (&view, "e1")) );
  let typed : String = with_line_after_subtree_of (
    &view, "e1", &format! ("{} typed under E", stars) );
  let saved : String = save_cleanly (&typed, "N", config, tantivy) . await ?;
  let e : Graphnode = node (config, "E") ?;
  let contains : Vec<ID> = members_of (&e . contains);
  assert_eq! ( contains . len (), 4, "{:?}", contains );
  assert_eq! ( ( &contains [0], &contains [2], &contains [3] ),
               ( &ID::from ("e1"), &ID::from ("H"), &ID::from ("e2") ),
               "the typed child follows e1: {:?}", contains );
  let typed_pid : &ID = &contains [1];
  assert_eq! ( node (config, &typed_pid . 0) ? . title, "typed under E" );
  let shown : Vec<&str> = children_of (&saved, "E") . into_iter ()
    . filter ( |line| line . contains ("(skg node ") ) . collect ();
  assert! ( shown . len () == 3
            && shown [1] . contains (&format! ("(gnid {})", typed_pid . 0)),
            "the saved view shows it under E, after e1:\n{}", saved );
  assert_eq! ( members_of (& node (config, "N") ? . contains), ids (&["n1"]),
               "N's own content is as it was" );
  Ok (( )) }

/// A headline typed under F is refused, saying to make F one's own
/// first; nothing is written.
async fn a_child_typed_under_a_foreign_forkee_is_refused (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let view : String = view_of (config, tantivy, "N") ?;
  let stars : String = "*" . repeat ( depth (line_of (&view, "f1")) );
  let typed : String = with_line_after_subtree_of (
    &view, "f1", &format! ("{} typed under F", stars) );
  let before : BTreeMap<String, String> = skg_files (config) ?;
  let refusal : String = refusal_of (&typed, "N", config, tantivy) . await ?;
  assert! ( refusal . contains ("Cannot add a child under F")
            && refusal . contains ("make the node your own first"),
            "{}", refusal );
  assert_eq! ( skg_files (config) ?, before, "nothing is written" );
  Ok (( )) }

/// With a node budget of 1, N spends it: E and F are drawn
/// write-protected, showing nothing.
async fn a_spent_budget_leaves_forkees_write_protected (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let mut config : SkgConfig = config . clone ();
  config . initial_node_limit = 1;
  let view : String = view_of (&config, tantivy, "N") ?;
  for forkee in ["E", "F"] {
    assert! ( line_of (&view, forkee) . contains ("writeProtected")
              && under (&view, forkee) . is_empty (),
              "{} is write-protected and bare:\n{}", forkee, view ); }
  Ok (( )) }

/// A buffer that draws E write-protected and bare, as a server drew it
/// before forkees as such were drawn editable, rerenders as it is: a
/// plain rerender extends nothing standing. An editable-view request
/// on N (C-c s e) extends E, through N's forks folder.
async fn a_write_protected_forkee_stays_until_its_forker_is_extended (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let view : String = view_of (config, tantivy, "N") ?;
  let e_line : &str = line_of (&view, "E");
  let old : String = without_subtree_of (&view, "E")
    . replacen (
      &format! ( "{}\n", line_of (&view, "F") ),
      &format! ( "{}\n{}\n",
                 e_line . replace ( "(gnid E) (skgRepos (nodeRepo main))",
                                    "(gnid E) (skgRepos (nodeRepo main)) writeProtected" ),
                 line_of (&view, "F") ),
      1 );
  let before : BTreeMap<String, String> = skg_files (config) ?;
  let rerendered : String = save_cleanly (&old, "N", config, tantivy) . await ?;
  assert_eq! ( skg_files (config) ?, before, "the old buffer writes nothing" );
  assert! ( line_of (&rerendered, "E") . contains ("writeProtected")
            && under (&rerendered, "E") . is_empty (),
            "E stays write-protected and bare:\n{}", rerendered );
  let requested : String = rerendered . replacen (
    "(skg node (gnid N)", "(skg node (viewRequests editableView) (gnid N)", 1 );
  assert_ne! ( requested, rerendered );
  let extended : String = save_cleanly (&requested, "N", config, tantivy) . await ?;
  assert! ( ! line_of (&extended, "E") . contains ("writeProtected")
            && under (&extended, "E") . iter () . any ( |line| line . contains ("(gnid e1)") ),
            "the request on N extends E:\n{}", extended );
  Ok (( )) }

/// FN, foreign, forks E. In a view of FN, cutting e1 from E would make
/// FN hide e1, which only FN's owner can record: the save is refused,
/// naming FN.
async fn a_cut_under_a_foreign_forker_is_refused (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let view : String = view_of (config, tantivy, "FN") ?;
  assert! ( ! line_of (&view, "E") . contains ("writeProtected"),
            "E is drawn editable under FN, foreign:\n{}", view );
  let before : BTreeMap<String, String> = skg_files (config) ?;
  let refusal : String =
    refusal_of (&without_subtree_of (&view, "e1"), "FN", config, tantivy) . await ?;
  assert! ( refusal . contains ("FN") && refusal . contains ("is foreign"),
            "{}", refusal );
  assert_eq! ( skg_files (config) ?, before, "nothing is written" );
  Ok (( )) }
