// cargo test --test delete_strips_references_from_neighbors -- --nocapture
//
// When a node is deleted via a save buffer, every other on-disk
// node that referenced it in an outbound list field should have
// that reference stripped. Without this, deletes leave dangling
// references which surface as PhantomUnknown placeholders -- correct
// rendering, but the inconsistency shouldn't have been left on
// disk in the first place.
//
// Fixture:
//   container.skg     contains: [victim, sibling]
//   subscriber.skg    subscribes_to: [victim, sibling]
//   victim.skg        (the one being deleted)
//   sibling.skg       (left alone)
//
// Save: a buffer that places (editRequest delete) on victim. The
// pipeline should:
//   - delete victim.skg
//   - rewrite container.skg to contains: [sibling]
//   - rewrite subscriber.skg to subscribes_to: [sibling]
//   - leave sibling.skg unchanged.

use indoc::indoc;
use std::error::Error;
use std::net::TcpStream;
use std::path::Path;
use std::sync::Arc;

use skg::dbs::filesystem::one_node::{
  nodecomplete_from_pid_and_source,
  nodecomplete_from_pid_and_source as load_nc};
use skg::dbs::in_rust_graph::InRustGraphHandle;
use skg::save::update_graph_minus_nodeMerges;
use skg::test_utils::{run_with_shared_test_db, graph_handle_from_config};
use skg::test_utils::update_from_and_rerender_buffer_test as update_from_and_rerender_buffer;
use skg::serve::ViewsState;
use skg::types::views_state::OpenViews;
use skg::types::misc::{ID, SkgConfig, TantivyIndex, SourceName};
use skg::types::nodes::complete::NodeComplete;
use skg::types::save::{DefineNode, SaveNode, DeleteNode};
use skg::util::path_from_pid_and_source;

use typedb_driver::TypeDBDriver;

#[test]
fn all_tests
  () -> Result<(), Box<dyn Error>> {
  run_with_shared_test_db (
    "skg-test-delete-strips-references-from-neighbors",
    |s| Box::pin ( async move {
      s . reset ("test_delete_strips_references_from_neighbors",
                 "tests/delete_strips_references_from_neighbors/fixtures") . await ?;
      test_delete_strips_references_from_neighbors (
        &s . config, &s . driver, &mut s . tantivy ) . await ?;
      s . reset ("test_strip_pass_amends_user_supplied_savenode",
                 "tests/delete_strips_references_from_neighbors/fixtures-with-existing-save") . await ?;
      test_strip_pass_amends_user_supplied_savenode (
        &s . config, &s . driver, &mut s . tantivy ) . await ?;
      s . reset ("test_strip_pass_handles_extra_ids",
                 "tests/delete_strips_references_from_neighbors/fixtures-extra-ids") . await ?;
      test_strip_pass_handles_extra_ids (
        &s . config, &s . driver, &mut s . tantivy ) . await ?;
      Ok (( )) } )) }

async fn test_delete_strips_references_from_neighbors (
  config  : &SkgConfig,
  driver  : &Arc<TypeDBDriver>,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      delete_strips_references_impl (
        config, driver, tantivy ) . await }

async fn delete_strips_references_impl (
  config  : &SkgConfig,
  driver: &Arc<TypeDBDriver>,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  // Single-root content view of victim with editRequest delete.
  let input_org_text : &str = indoc! {"
    * (skg (node (id victim) (source main) (editRequest delete))) victim
  "};

  let graph : InRustGraphHandle =
    graph_handle_from_config (config) ?;
  let mut views_state : ViewsState = ViewsState {
        diff_mode_enabled : false,
        open_views            : OpenViews::new (),
        };
  let listener : std::net::TcpListener =
    std::net::TcpListener::bind ("127.0.0.1:0") . unwrap ();
  let mut stream : TcpStream =
    TcpStream::connect (listener . local_addr () . unwrap ()) . unwrap ();
  let _response = update_from_and_rerender_buffer (
    &mut stream,
    input_org_text, driver, config, tantivy, &graph, false,
    &Err ( String::new () ), &mut views_state ) . await ?;

  let mut failures : Vec<String> = Vec::new ();
  let main : SourceName = SourceName::from ("main");

  // 1. victim.skg deleted.
  let victim_path : String =
    path_from_pid_and_source (
      config, &main, ID::from ("victim") ) ?;
  if Path::new (&victim_path) . exists () {
    failures . push (
      "victim.skg should have been deleted" . to_string ()); }

  // 2. container.skg's contains has only sibling now.
  let container : NodeComplete =
    nodecomplete_from_pid_and_source (
      config, ID::from ("container"), &main ) ?;
  if container . contains . contains (&ID::from ("victim")) {
    failures . push ( format! (
      "container.contains still has victim: {:?}",
      container . contains )); }
  if ! container . contains . contains (&ID::from ("sibling")) {
    failures . push ( format! (
      "container.contains lost sibling: {:?}",
      container . contains )); }

  // 3. subscriber.skg's subscribes_to has only sibling.
  let subscriber : NodeComplete =
    nodecomplete_from_pid_and_source (
      config, ID::from ("subscriber"), &main ) ?;
  let sub_vec : Vec<ID> =
    subscriber . subscribes_to . or_default () . to_vec ();
  if sub_vec . contains (&ID::from ("victim")) {
    failures . push ( format! (
      "subscriber.subscribes_to still has victim: {:?}",
      sub_vec )); }
  if ! sub_vec . contains (&ID::from ("sibling")) {
    failures . push ( format! (
      "subscriber.subscribes_to lost sibling: {:?}",
      sub_vec )); }

  // 4. sibling.skg unchanged.
  let sibling : NodeComplete =
    nodecomplete_from_pid_and_source (
      config, ID::from ("sibling"), &main ) ?;
  if sibling . title != "sibling" {
    failures . push ( format! (
      "sibling.skg title changed: {}", sibling . title )); }

  if !failures . is_empty () {
    panic! ("\n{} assertion(s) failed:\n  - {}",
            failures . len (),
            failures . join ("\n  - ") ); }

  Ok (( )) }

// ----------------------------------------------------------------
// Test 2: a hand-constructed batch where a SaveNode for the
// container is present alongside a DeleteNode for victim, and the
// SaveNode still mentions victim in contains. (Buffer-derived
// instructions can't easily produce this configuration -- buffer
// validation rejects it as AmbiguousDeletion -- but the strip pass
// shouldn't depend on buffer-level invariants. This test calls
// update_graph_minus_nodeMerges directly to exercise that.)
//
// With the strip pass, container.contains ends up empty after the
// save even though the user-supplied SaveNode said otherwise.
// ----------------------------------------------------------------

async fn test_strip_pass_amends_user_supplied_savenode (
  config  : &SkgConfig,
  driver  : &Arc<TypeDBDriver>,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      strip_pass_amends_user_supplied_savenode_impl (
        config, driver, tantivy ) . await }

async fn strip_pass_amends_user_supplied_savenode_impl (
  config  : &SkgConfig,
  driver: &Arc<TypeDBDriver>,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let main : SourceName = SourceName::from ("main");
  // Build the SaveNode for container by reading the on-disk node
  // verbatim -- contents are still [victim] -- and pair with a
  // DeleteNode for victim.
  let container_nc : NodeComplete =
    load_nc ( config, ID::from ("container"), &main ) ?;
  assert! ( container_nc . contains . contains (&ID::from ("victim")),
            "fixture precondition: container should reference victim" );
  let node_defs : Vec<DefineNode> = vec! [
    DefineNode::Save ( SaveNode (container_nc) ),
    DefineNode::Delete ( DeleteNode {
      id: ID::from ("victim"),
      source: main . clone (), } ), ];
  let graph : InRustGraphHandle =
    graph_handle_from_config (config) ?;
  update_graph_minus_nodeMerges (
    node_defs, &[], config . clone (), tantivy, driver, &graph
  ) . await ?;
  let container : NodeComplete =
    load_nc ( config, ID::from ("container"), &main ) ?;
  if container . contains . contains (&ID::from ("victim")) {
    panic! ("container.contains still has victim after strip pass: {:?}",
            container . contains ); }
  let victim_path : String =
    path_from_pid_and_source (
      config, &main, ID::from ("victim") ) ?;
  if Path::new (&victim_path) . exists () {
    panic! ("victim.skg should have been deleted"); }
  Ok (( )) }

// ----------------------------------------------------------------
// Test 3: extra_ids of a deleted node also get stripped from
// referencers. Other nodes' on-disk lists may store any of a
// node's ids (primary or extra), so the strip-set must include
// every id of every deleted node.
//
// Fixture:
//   aliased.skg     pid: aliased,    extra_ids: [aliased_alt]
//   referencer.skg  contains: [aliased_alt]   <-- stored under the alias
//
// Buffer puts editRequest delete on `aliased` (by its primary pid).
// Expected: referencer.contains becomes [].
// ----------------------------------------------------------------

async fn test_strip_pass_handles_extra_ids (
  config  : &SkgConfig,
  driver  : &Arc<TypeDBDriver>,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      strip_pass_handles_extra_ids_impl (
        config, driver, tantivy ) . await }

async fn strip_pass_handles_extra_ids_impl (
  config  : &SkgConfig,
  driver: &Arc<TypeDBDriver>,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let input_org_text : &str = indoc! {"
    * (skg (node (id aliased) (source main) (editRequest delete))) aliased
  "};
  let graph : InRustGraphHandle =
    graph_handle_from_config (config) ?;
  let mut views_state : ViewsState = ViewsState {
        diff_mode_enabled : false,
        open_views            : OpenViews::new (),
        };
  let listener : std::net::TcpListener =
    std::net::TcpListener::bind ("127.0.0.1:0") . unwrap ();
  let mut stream : TcpStream =
    TcpStream::connect (listener . local_addr () . unwrap ()) . unwrap ();
  let _response = update_from_and_rerender_buffer (
    &mut stream,
    input_org_text, driver, config, tantivy, &graph, false,
    &Err ( String::new () ), &mut views_state ) . await ?;
  let main : SourceName = SourceName::from ("main");
  let referencer : NodeComplete =
    nodecomplete_from_pid_and_source (
      config, ID::from ("referencer"), &main ) ?;
  if referencer . contains . contains (&ID::from ("aliased_alt")) {
    panic! ("referencer.contains still has aliased_alt (an extra_id of \
             the deleted node) after strip pass: {:?}",
            referencer . contains ); }
  if referencer . contains . contains (&ID::from ("aliased")) {
    panic! ("referencer.contains has aliased (primary pid of deleted node)"); }
  let aliased_path : String =
    path_from_pid_and_source (
      config, &main, ID::from ("aliased") ) ?;
  if Path::new (&aliased_path) . exists () {
    panic! ("aliased.skg should have been deleted"); }
  Ok (( )) }
