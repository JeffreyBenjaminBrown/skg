// cargo nextest run --test grouped_views -E 'test(subscribee_folder_empty_persists::)'
//
// plan_v2 §3.4/§6.7 exception: an *empty* SubscribeeFolder is PRESERVED, not
// self-deleted. It is the editable interface onto the origin's outgoing
// subscriptions; if it vanished when emptied, the user would lose the place to
// add one back.
//
// Fixture: node s subscribes to nothing. The input buffer nonetheless carries
// an (skg subscribeeFolder) under s (as it would right after the user deleted s's
// last subscription). On rerender the SubscribeeFolder's goal list is empty; the
// folder must survive as an empty `subscribeeFolder` headline rather than being
// detached (the pre-fix behavior).

use indoc::indoc;
use std::error::Error;
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
  let fixtures : &str = "tests/subscribee_folder_empty_persists/fixtures";
  run_with_shared_test_stores (
    "skg-test-subscribee-folder-empty-persists",
    |s| Box::pin ( async move {
      s . reset ("test_empty_subscribee_folder_persists", fixtures) ?;
      test_empty_subscribee_folder_persists (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("test_empty_subscriber_folder_is_removed", fixtures) ?;
      test_empty_subscriber_folder_is_removed (
        &s . config, &mut s . tantivy ) . await ?;
      Ok (( )) } )) }

async fn test_empty_subscribee_folder_persists (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      empty_subscribee_folder_persists_impl (
        config, tantivy ) . await }

async fn empty_subscribee_folder_persists_impl (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  // s subscribes to nothing (fixture has no subscribes_to), but the buffer
  // still shows a subscribeeFolder -- the editable interface the user just emptied.
  let input_org_text : &str = indoc! {"
    * (skg (node (id s) (source main))) s
    ** (skg subscribeeFolder)
  "};

  let graph : InRustGraphHandle =
    graph_handle_from_config (config) ?;
  let mut views_state : ViewsState = ViewsState {
    diff_mode_enabled : false,
    open_views        : OpenViews::new (),
  };
  let listener : std::net::TcpListener =
    std::net::TcpListener::bind ("127.0.0.1:0") . unwrap ();
  let mut stream : TcpStream =
    TcpStream::connect (listener . local_addr () . unwrap ()) . unwrap ();
  let response = update_from_and_rerender_buffer (
    &mut stream,
    input_org_text, config, tantivy, &graph, false,
    &Err ( String::new () ), &mut views_state ) . await ?;

  println!("Rendered buffer:\n{}", response . saved_view);
  if ! response . errors . is_empty () {
    println!("Errors: {:?}", response . errors); }

  assert! ( response . saved_view . contains ("subscribeeFolder"),
    "an empty SubscribeeFolder must be PRESERVED on rerender (the editable \
     interface onto the origin's subscriptions), not self-deleted; got:\n{}",
    response . saved_view );
  assert! ( response . errors . is_empty (),
    "rerender of a node with an empty SubscribeeFolder must not error; got: {:?}",
    response . errors );
  Ok (( )) }

// Contrast (plan_v2 §3.4/§6.8): an empty *read-only* PartnerFolder -- here a
// subscriberFolder -- IS removed by the postorder prune sweep, because (unlike the
// SubscribeeFolder) it is not an editable interface; an emptied one is just noise.
async fn test_empty_subscriber_folder_is_removed (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      empty_subscriber_folder_removed_impl (
        config, tantivy ) . await }

async fn empty_subscriber_folder_removed_impl (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  // Nobody subscribes to s, but the buffer carries a subscriberFolder under it.
  let input_org_text : &str = indoc! {"
    * (skg (node (id s) (source main))) s
    ** (skg subscriberFolder)
  "};

  let graph : InRustGraphHandle =
    graph_handle_from_config (config) ?;
  let mut views_state : ViewsState = ViewsState {
    diff_mode_enabled : false,
    open_views        : OpenViews::new (),
  };
  let listener : std::net::TcpListener =
    std::net::TcpListener::bind ("127.0.0.1:0") . unwrap ();
  let mut stream : TcpStream =
    TcpStream::connect (listener . local_addr () . unwrap ()) . unwrap ();
  let response = update_from_and_rerender_buffer (
    &mut stream,
    input_org_text, config, tantivy, &graph, false,
    &Err ( String::new () ), &mut views_state ) . await ?;

  println!("Rendered buffer:\n{}", response . saved_view);
  assert! ( ! response . saved_view . contains ("subscriberFolder"),
    "an empty read-only subscriberFolder must be REMOVED by the §3.4 prune sweep; \
     got:\n{}", response . saved_view );
  assert! ( response . errors . is_empty (),
    "rerender must not error; got: {:?}", response . errors );
  Ok (( )) }
