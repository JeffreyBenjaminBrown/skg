// cargo test --test indef_should_not_count_as_donotdelete -- --nocapture
//
// Reproduces the bug Jeff hit: a buffer that has the same node
// shown definitive-with-editRequest-delete in one place AND
// indefinitive elsewhere triggers AmbiguousDeletion validation,
// even though the indef view is read-only and shouldn't count as
// a "do not delete" stance.
//
// Fixture:
//   parent.skg contains [victim, via]
//   via.skg    contains [victim]
//   victim.skg
//
// Buffer (modeled on Jeff's actual scenario):
//   * parent
//   ** victim (editRequest delete)   <-- definitive, asks for delete
//   ** via
//   *** victim indef                 <-- indef view of the same node
//
// Expected: save succeeds and victim is deleted.
// Currently: AmbiguousDeletion error -- the indef occurrence is
// being treated as a "do not delete" stance, contradicting the
// definitive editRequest-delete elsewhere.

use indoc::indoc;
use std::error::Error;
use std::net::TcpStream;
use std::sync::Arc;

use skg::test_utils::run_with_test_db;
use skg::test_utils::update_from_and_rerender_buffer_test as update_from_and_rerender_buffer;
use skg::serve::ViewsState;
use skg::types::memory::OpenViews;
use skg::types::misc::{SkgConfig, TantivyIndex};
use typedb_driver::TypeDBDriver;

#[test]
fn test_indef_should_not_count_as_donotdelete
  () -> Result<(), Box<dyn Error>> {
  run_with_test_db (
    "skg-test-indef-not-donotdelete",
    "tests/indef_should_not_count_as_donotdelete/fixtures",
    "/tmp/tantivy-test-indef-not-donotdelete",
    |config, driver, tantivy| Box::pin ( async move {
      indef_should_not_count_as_donotdelete_impl (
        config, driver, tantivy ) . await
    } )) }

async fn indef_should_not_count_as_donotdelete_impl (
  config  : &SkgConfig,
  driver: &Arc<TypeDBDriver>,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let input_org_text : &str = indoc! {"
    * (skg (node (id parent) (source main))) parent
    ** (skg (node (id victim) (source main) (editRequest delete))) victim
    ** (skg (node (id via) (source main))) via
    *** (skg (node (id victim) (source main) indef)) victim
  "};
  let graph : skg::dbs::memory::InRustGraphHandle =
    skg::dbs::memory::new_handle (skg::dbs::memory::InRustGraph::new ());
  let mut views_state : ViewsState = ViewsState {
        diff_mode_enabled : false,
        open_views            : OpenViews::new (),
        };
  let listener : std::net::TcpListener =
    std::net::TcpListener::bind ("127.0.0.1:0") . unwrap ();
  let mut stream : TcpStream =
    TcpStream::connect (listener . local_addr () . unwrap ()) . unwrap ();
  let response = update_from_and_rerender_buffer (
    &mut stream,
    input_org_text, driver, config, tantivy, &graph, false,
    &Err ( String::new () ), &mut views_state ) . await ?;
  if ! response . errors . is_empty () {
    panic! ("save returned errors: {:?}", response . errors); }
  Ok (( )) }
