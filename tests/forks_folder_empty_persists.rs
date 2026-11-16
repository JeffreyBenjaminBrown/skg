// cargo nextest run --test grouped_views -E 'test(forks_folder_empty_persists::)'
//
// An exception to the prune: an *empty* forks folder is preserved, not
// self-deleted. It is the editable interface onto the origin's forks; if
// it vanished when emptied, the user would lose the place to add one
// back.
//
// Fixture: node s forks nothing, and nothing forks it. The input buffer
// nonetheless carries an (skg forksFolder) under s (as it would right
// after the user deleted s's last fork). On rerender the forks folder's
// goal list is empty; the folder must survive as an empty `forksFolder`
// headline rather than being detached (the pre-fix behavior).

use indoc::indoc;
use std::error::Error;
use crate::tcp_stream_pair::mk_test_tcp_stream_pair;
use std::net::TcpStream;

use skg::test_utils::{run_with_shared_test_stores, graph_handle_from_config};
use skg::test_utils::update_from_and_rerender_buffer_test as update_from_and_rerender_buffer;
use skg::serve::ViewsState;
use skg::types::views_state::OpenViews;

use skg::dbs::in_rust_graph::InRustGraphHandle;
use skg::types::misc::{SkgConfig, TantivyIndex};


#[test]
fn all_tests
  () -> Result<(), Box<dyn Error>> {
  let fixtures : &str = "tests/forks_folder_empty_persists/fixtures";
  run_with_shared_test_stores (
    "skg-test-forks-folder-empty-persists",
    |s| Box::pin ( async move {
      s . reset ("test_empty_forks_folder_persists", fixtures) ?;
      test_empty_forks_folder_persists (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("test_empty_hider_folder_is_removed", fixtures) ?;
      test_empty_hider_folder_is_removed (
        &s . config, &mut s . tantivy ) . await ?;
      Ok (( )) } )) }

async fn test_empty_forks_folder_persists (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      empty_forks_folder_persists_impl (
        config, tantivy ) . await }

async fn empty_forks_folder_persists_impl (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  // s forks nothing (fixture has no forks), but the buffer
  // still shows a forksFolder -- the editable interface the user just emptied.
  let input_org_text : &str = indoc! {"
    * (skg node (gnid s) (skgRepos (nodeRepo main))) s
    ** (skg forksFolder)
  "};

  let graph : InRustGraphHandle =
    graph_handle_from_config (config) ?;
  let mut views_state : ViewsState = ViewsState {
    open_views        : OpenViews::new (),
    graph_straddle    : None,
  };
  let (mut stream, _read_end) : (TcpStream, TcpStream) =
    mk_test_tcp_stream_pair ();
  let response = update_from_and_rerender_buffer (
    &mut stream,
    input_org_text, config, tantivy, &graph,
    &Err ( String::new () ), &mut views_state ) . await ?;

  println!("Rendered buffer:\n{}", response . saved_view);
  if ! response . errors . is_empty () {
    println!("Errors: {:?}", response . errors); }

  assert! ( response . saved_view . contains ("forksFolder"),
    "an empty forks folder must be PRESERVED on rerender (the editable \
     interface onto the origin's forks), not self-deleted; got:\n{}",
    response . saved_view );
  assert! ( response . errors . is_empty (),
    "rerender of a node with an empty forks folder must not error; got: {:?}",
    response . errors );
  Ok (( )) }

// Contrast: an empty *write-protected* partner folder -- here a
// hiderFolder -- is removed by the postorder prune sweep, because (unlike the
// forks folder) it is not an editable interface; an emptied one is just noise.
async fn test_empty_hider_folder_is_removed (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      empty_hider_folder_removed_impl (
        config, tantivy ) . await }

async fn empty_hider_folder_removed_impl (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  // Nobody hides s, but the buffer carries a hiderFolder under it.
  let input_org_text : &str = indoc! {"
    * (skg node (gnid s) (skgRepos (nodeRepo main))) s
    ** (skg hiderFolder)
  "};

  let graph : InRustGraphHandle =
    graph_handle_from_config (config) ?;
  let mut views_state : ViewsState = ViewsState {
    open_views        : OpenViews::new (),
    graph_straddle    : None,
  };
  let (mut stream, _read_end) : (TcpStream, TcpStream) =
    mk_test_tcp_stream_pair ();
  let response = update_from_and_rerender_buffer (
    &mut stream,
    input_org_text, config, tantivy, &graph,
    &Err ( String::new () ), &mut views_state ) . await ?;

  println!("Rendered buffer:\n{}", response . saved_view);
  assert! ( ! response . saved_view . contains ("hiderFolder"),
    "an empty write-protected hiderFolder must be REMOVED by the §3.4 prune sweep; \
     got:\n{}", response . saved_view );
  assert! ( response . errors . is_empty (),
    "rerender must not error; got: {:?}", response . errors );
  Ok (( )) }
