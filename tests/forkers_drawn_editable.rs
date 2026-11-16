// cargo nextest run --test grouped_forks -E 'test(forkers_drawn_editable::)'
//
// Forkers as such ('TODO/every-mirror/2_plan.org', slice 3;
// 'docs/glossary.org', "forker as such"). The fixture is the plan's
// "forks of one note meet", with a forkee and a foreign forker added.
// N, owned, contains X1 to X4, forks K and hides Z. R and T, owned,
// fork N: R contains X1, X2, X3; T contains X1, X3, Y, Z and hides X2.
// K contains k1. FF, foreign, forks N and contains X1 and f1. So in
// N's forks folder T shows Y (what it adds), with Z in its folder of
// what N hides in it, and X2 in its folder of what it hides of N's
// content; R shows nothing; FF shows f1. Each sub-test edits a view
// and saves it:
// - a buffer showing T editable, saved unchanged, writes nothing: T's
//   partial list is never read as its whole contains;
// - cutting Y from T there is refused (the cut is not read yet), and
//   nothing is written.

use std::error::Error;
use crate::tcp_stream_pair::mk_test_tcp_stream_pair;
use std::net::TcpStream;
use std::collections::BTreeMap;
use indoc::indoc;

use skg::dbs::in_rust_graph::InRustGraphHandle;
use skg::serve::ViewsState;
use skg::serve::handlers::save_buffer::SaveResponse;
use skg::test_utils::{graph_handle_from_config, run_with_shared_test_stores};
use skg::test_utils::update_from_and_rerender_buffer_test as update_from_and_rerender_buffer;
use skg::types::errors::SaveError;
use skg::types::misc::{SkgConfig, TantivyIndex};
use skg::types::save::format_save_error_as_org;
use skg::types::views_state::{OpenViews, ViewforestId};

const FIXTURES : &str = "tests/forkers_drawn_editable/fixtures";

/// N's view as a buffer might hold it with T, a forker as such,
/// editable, showing Y, what it adds to N's content.
const N_WITH_T_EDITABLE : &str = indoc! {"
  * (skg node (gnid N) (skgRepos (nodeRepo main))) N
  ** (skg forksFolder)
  *** (skg node (gnid K) (skgRepos (nodeRepo main)) writeProtected) K
  *** (skg node (gnid FF) (skgRepos (nodeRepo theirs)) noClaim writeProtected) FF
  *** (skg node (gnid R) (skgRepos (nodeRepo main)) noClaim writeProtected) R
  *** (skg node (gnid T) (skgRepos (nodeRepo main)) noClaim) T
  **** (skg node (gnid Y) (skgRepos (nodeRepo main))) Y
  ** (skg node (gnid X1) (skgRepos (nodeRepo main))) X1
  ** (skg node (gnid X2) (skgRepos (nodeRepo main))) X2
  ** (skg node (gnid X3) (skgRepos (nodeRepo main))) X3
  ** (skg node (gnid X4) (skgRepos (nodeRepo main))) X4
  "};

#[test]
fn all_tests
  () -> Result<(), Box<dyn Error>> {
  run_with_shared_test_stores (
    "skg-test-forkers-drawn-editable",
    |s| Box::pin ( async move {
      s . reset ("an_unchanged_save_of_an_editable_forker_writes_nothing", FIXTURES) ?;
      an_unchanged_save_of_an_editable_forker_writes_nothing (&s . config, &mut s . tantivy) . await ?;
      s . reset ("a_cut_from_an_editable_forker_is_refused", FIXTURES) ?;
      a_cut_from_an_editable_forker_is_refused (&s . config, &mut s . tantivy) . await ?;
      Ok (( )) } )) }

//
// Helpers
//

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

/// Save 'buf', which must save cleanly.
async fn save_cleanly (
  buf     : &str,
  root    : &str,
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<SaveResponse, Box<dyn Error>> {
  let response : SaveResponse = save (buf, root, config, tantivy) . await ?;
  assert! ( response . errors . is_empty (), "save errors: {:?}", response . errors );
  Ok (response) }

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

/// A buffer showing T, a forker as such, editable, with only Y (what
/// T adds to N's content), saved unchanged, writes no file: T's list
/// keeps X1, X3 and Z, which N contains or hides.
async fn an_unchanged_save_of_an_editable_forker_writes_nothing (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let before : BTreeMap<String, String> = skg_files (config) ?;
  save_cleanly (N_WITH_T_EDITABLE, "N", config, tantivy) . await ?;
  assert_eq! ( skg_files (config) ?, before, "an unchanged save writes nothing" );
  Ok (( )) }

/// Cutting Y from T there would make N fork T, which the save does
/// not read yet: it is refused, and nothing is written.
async fn a_cut_from_an_editable_forker_is_refused (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let cut : String = N_WITH_T_EDITABLE . replacen (
    "**** (skg node (gnid Y) (skgRepos (nodeRepo main))) Y\n", "", 1 );
  assert_ne! ( cut, N_WITH_T_EDITABLE );
  let before : BTreeMap<String, String> = skg_files (config) ?;
  let refusal : String = refusal_of (&cut, "N", config, tantivy) . await ?;
  assert! ( refusal . contains ("Cannot cut Y from T")
            && refusal . contains ("not yet supported"),
            "{}", refusal );
  assert_eq! ( skg_files (config) ?, before, "nothing is written" );
  Ok (( )) }
