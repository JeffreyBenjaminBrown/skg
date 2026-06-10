// cargo test --test subscribee_col_empty_persists -- --nocapture
//
// plan_v2 §3.4/§6.7 exception: an *empty* SubscribeeCol is PRESERVED, not
// self-deleted. It is the editable interface onto the origin's outgoing
// subscriptions; if it vanished when emptied, the user would lose the place to
// add one back.
//
// Fixture: node s subscribes to nothing. The input buffer nonetheless carries
// an (skg subscribeeCol) under s (as it would right after the user deleted s's
// last subscription). On rerender the SubscribeeCol's goal list is empty; the
// col must survive as an empty `subscribeeCol` headline rather than being
// detached (the pre-fix behavior).

use indoc::indoc;
use std::error::Error;
use std::net::TcpStream;
use std::sync::Arc;

use skg::test_utils::{run_with_test_db, graph_handle_from_config};
use skg::test_utils::update_from_and_rerender_buffer_test as update_from_and_rerender_buffer;
use skg::serve::ViewsState;
use skg::types::views_state::OpenViews;

use skg::dbs::in_rust_graph::InRustGraphHandle;
use skg::types::misc::{SkgConfig, TantivyIndex};

use typedb_driver::TypeDBDriver;

#[test]
fn test_empty_subscribee_col_persists
  () -> Result<(), Box<dyn Error>> {
  run_with_test_db (
    "skg-test-subscribee-col-empty-persists",
    "tests/subscribee_col_empty_persists/fixtures",
    "/tmp/tantivy-test-subscribee-col-empty-persists",
    |config, driver, tantivy| Box::pin ( async move {
      empty_subscribee_col_persists_impl (
        config, driver, tantivy ) . await
    } )) }

async fn empty_subscribee_col_persists_impl (
  config  : &SkgConfig,
  driver  : &Arc<TypeDBDriver>,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  // s subscribes to nothing (fixture has no subscribes_to), but the buffer
  // still shows a subscribeeCol -- the editable interface the user just emptied.
  let input_org_text : &str = indoc! {"
    * (skg (node (id s) (source main))) s
    ** (skg subscribeeCol)
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
    input_org_text, driver, config, tantivy, &graph, false,
    &Err ( String::new () ), &mut views_state ) . await ?;

  println!("Rendered buffer:\n{}", response . saved_view);
  if ! response . errors . is_empty () {
    println!("Errors: {:?}", response . errors); }

  assert! ( response . saved_view . contains ("subscribeeCol"),
    "an empty SubscribeeCol must be PRESERVED on rerender (the editable \
     interface onto the origin's subscriptions), not self-deleted; got:\n{}",
    response . saved_view );
  assert! ( response . errors . is_empty (),
    "rerender of a node with an empty SubscribeeCol must not error; got: {:?}",
    response . errors );
  Ok (( )) }

// Contrast (plan_v2 §3.4/§6.8): an empty *read-only* PartnerCol -- here a
// subscriberCol -- IS removed by the postorder prune sweep, because (unlike the
// SubscribeeCol) it is not an editable interface; an emptied one is just noise.
#[test]
fn test_empty_subscriber_col_is_removed
  () -> Result<(), Box<dyn Error>> {
  run_with_test_db (
    "skg-test-subscriber-col-empty-removed",
    "tests/subscribee_col_empty_persists/fixtures",
    "/tmp/tantivy-test-subscriber-col-empty-removed",
    |config, driver, tantivy| Box::pin ( async move {
      empty_subscriber_col_removed_impl (
        config, driver, tantivy ) . await
    } )) }

async fn empty_subscriber_col_removed_impl (
  config  : &SkgConfig,
  driver  : &Arc<TypeDBDriver>,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  // Nobody subscribes to s, but the buffer carries a subscriberCol under it.
  let input_org_text : &str = indoc! {"
    * (skg (node (id s) (source main))) s
    ** (skg subscriberCol)
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
    input_org_text, driver, config, tantivy, &graph, false,
    &Err ( String::new () ), &mut views_state ) . await ?;

  println!("Rendered buffer:\n{}", response . saved_view);
  assert! ( ! response . saved_view . contains ("subscriberCol"),
    "an empty read-only subscriberCol must be REMOVED by the §3.4 prune sweep; \
     got:\n{}", response . saved_view );
  assert! ( response . errors . is_empty (),
    "rerender must not error; got: {:?}", response . errors );
  Ok (( )) }
