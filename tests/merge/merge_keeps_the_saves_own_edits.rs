// cargo nextest run --test grouped_saves -E 'test(merge::merge_keeps_the_saves_own_edits)'
//
// A save's NodeMerges are built from the graph as the save's ordinary
// nodeInstructions leave it, so a buffer that merges and also edits
// keeps both. A buffer that deletes a member of its own merge pair is
// refused.
//
// Fixture graph (skgrepo main):
//   a contains [c, d]
//   b (body "b body") contains [e]
//   c, d, e, u: leaves
//
// Every buffer below makes a the acquirer and b the acquiree.

use indoc::indoc;
use std::collections::BTreeMap;
use std::error::Error;
use std::fs;
use std::net::TcpStream;
use std::path::{Path, PathBuf};
use crate::tcp_stream_pair::mk_test_tcp_stream_pair;

use skg::dbs::filesystem::one_node::graphnode_from_pid_and_skgrepo;
use skg::dbs::in_rust_graph::InRustGraphHandle;
use skg::serve::ViewsState;
use skg::test_utils::{run_with_test_stores, graph_handle_from_config, audit_inrustgraph_or_panic};
use skg::test_utils::update_from_and_rerender_buffer_test as update_from_and_rerender_buffer;
use skg::types::errors::{BufferValidationError, NodeMergeRole, SaveError};
use skg::types::misc::{ID, SkgConfig, SkgrepoName, TantivyIndex, members_of};
use skg::types::nodes::complete::Graphnode;
use skg::serve::handlers::save_buffer::SaveResponse;
use skg::types::views_state::OpenViews;

const FIXTURES : &str = "tests/merge/merge_keeps_the_saves_own_edits/fixtures";

/// The acquirer's file carries the buffer's new title, the acquiree's
/// ID among its extra IDs, and the text preserver as its first child.
#[test]
fn a_retitled_acquirer_keeps_its_new_title
  () -> Result<(), Box<dyn Error>> {
  run_with_test_stores (
    "skg-test-merge-keeps-retitled-acquirer",
    FIXTURES,
    "/tmp/tantivy-test-merge-keeps-retitled-acquirer",
    |config, tantivy| Box::pin ( async move {
      let buffer : &str = indoc! {"
        * (skg node (gnid a) (skgRepos (nodeRepo main)) (editRequest (merge b))) a retitled
        ** (skg node (gnid c) (skgRepos (nodeRepo main))) c
        ** (skg node (gnid d) (skgRepos (nodeRepo main))) d
      "};
      save (buffer, config, tantivy) . await ?;
      let acquirer : Graphnode = node_on_disk (config, "a") ?;
      assert_eq! (acquirer . title, "a retitled");
      assert! (acquirer . extra_ids . contains (&ID::from ("b")),
               "a should carry b as an extra ID: {:?}", acquirer . extra_ids);
      let preserver : Graphnode = first_child_on_disk (config, &acquirer) ?;
      assert_eq! (preserver . title, "MERGED: b");
      assert_eq! (members_of (&acquirer . contains),
                  vec! [ preserver . pid . clone (),
                         ID::from ("c"), ID::from ("d"), ID::from ("e") ]);
      assert! (! file_exists (config, "b") ?, "b.skg should be gone");
      Ok (( )) } )) }

/// The text preserver carries the body the buffer gave the acquiree.
#[test]
fn an_edited_acquiree_body_reaches_the_text_preserver
  () -> Result<(), Box<dyn Error>> {
  run_with_test_stores (
    "skg-test-merge-keeps-edited-acquiree-body",
    FIXTURES,
    "/tmp/tantivy-test-merge-keeps-edited-acquiree-body",
    |config, tantivy| Box::pin ( async move {
      let buffer : &str = indoc! {"
        * (skg node (gnid a) (skgRepos (nodeRepo main)) (editRequest (merge b))) a
        ** (skg node (gnid c) (skgRepos (nodeRepo main))) c
        ** (skg node (gnid d) (skgRepos (nodeRepo main))) d
        * (skg node (gnid b) (skgRepos (nodeRepo main))) b
        edited b body
        ** (skg node (gnid e) (skgRepos (nodeRepo main))) e
      "};
      save (buffer, config, tantivy) . await ?;
      let acquirer : Graphnode = node_on_disk (config, "a") ?;
      let preserver : Graphnode = first_child_on_disk (config, &acquirer) ?;
      assert_eq! (preserver . title, "MERGED: b");
      assert_eq! (preserver . body . as_deref (), Some ("edited b body"));
      assert! (! file_exists (config, "b") ?, "b.skg should be gone");
      Ok (( )) } )) }

/// A graphnode the buffer deletes from the acquirer's contents is
/// absent from the merged contents, and its file is gone.
#[test]
fn a_deleted_content_of_the_acquirer_is_not_merged
  () -> Result<(), Box<dyn Error>> {
  run_with_test_stores (
    "skg-test-merge-drops-deleted-content",
    FIXTURES,
    "/tmp/tantivy-test-merge-drops-deleted-content",
    |config, tantivy| Box::pin ( async move {
      let buffer : &str = indoc! {"
        * (skg node (gnid a) (skgRepos (nodeRepo main)) (editRequest (merge b))) a
        ** (skg node (gnid c) (skgRepos (nodeRepo main)) (editRequest delete)) c
        ** (skg node (gnid d) (skgRepos (nodeRepo main))) d
      "};
      let graph : InRustGraphHandle = save (buffer, config, tantivy) . await ?;
      let acquirer : Graphnode = node_on_disk (config, "a") ?;
      let preserver : Graphnode = first_child_on_disk (config, &acquirer) ?;
      assert_eq! (members_of (&acquirer . contains),
                  vec! [ preserver . pid . clone (),
                         ID::from ("d"), ID::from ("e") ]);
      assert! (! file_exists (config, "c") ?, "c.skg should be gone");
      assert! (graph . load_full () . get (&ID::from ("c")) . is_none (),
               "c should be gone from the graph");
      Ok (( )) } )) }

/// An edit to a graphnode unrelated to the merge lands beside it.
#[test]
fn an_unrelated_edit_lands_beside_the_merge
  () -> Result<(), Box<dyn Error>> {
  run_with_test_stores (
    "skg-test-merge-keeps-unrelated-edit",
    FIXTURES,
    "/tmp/tantivy-test-merge-keeps-unrelated-edit",
    |config, tantivy| Box::pin ( async move {
      let buffer : &str = indoc! {"
        * (skg node (gnid a) (skgRepos (nodeRepo main)) (editRequest (merge b))) a
        ** (skg node (gnid c) (skgRepos (nodeRepo main))) c
        ** (skg node (gnid d) (skgRepos (nodeRepo main))) d
        * (skg node (gnid u) (skgRepos (nodeRepo main))) u retitled
      "};
      save (buffer, config, tantivy) . await ?;
      assert_eq! (node_on_disk (config, "u") ? . title, "u retitled");
      let acquirer : Graphnode = node_on_disk (config, "a") ?;
      assert! (acquirer . extra_ids . contains (&ID::from ("b")),
               "a should carry b as an extra ID: {:?}", acquirer . extra_ids);
      assert! (! file_exists (config, "b") ?, "b.skg should be gone");
      Ok (( )) } )) }

/// A buffer that deletes the acquiree of its own merge request is
/// refused, naming the acquiree, and no file changes.
#[test]
fn deleting_the_acquiree_while_merging_it_is_refused
  () -> Result<(), Box<dyn Error>> {
  run_with_test_stores (
    "skg-test-merge-refuses-deleted-acquiree",
    FIXTURES,
    "/tmp/tantivy-test-merge-refuses-deleted-acquiree",
    |config, tantivy| Box::pin ( async move {
      let buffer : &str = indoc! {"
        * (skg node (gnid a) (skgRepos (nodeRepo main)) (editRequest (merge b))) a
        ** (skg node (gnid c) (skgRepos (nodeRepo main))) c
        ** (skg node (gnid d) (skgRepos (nodeRepo main))) d
        * (skg node (gnid b) (skgRepos (nodeRepo main)) (editRequest delete)) b
      "};
      let files_before : BTreeMap<PathBuf, Vec<u8>> = skgrepo_files (config) ?;
      let graph : InRustGraphHandle = graph_handle_from_config (config) ?;
      let result : Result<SaveResponse, Box<dyn Error>> =
        save_on_graph (buffer, config, tantivy, &graph) . await;
      let error : Box<dyn Error> = match result {
        Err (error) => error,
        Ok (_) => panic! ("a save deleting its own acquiree must be refused"), };
      let errors : &[BufferValidationError] =
        match error . downcast_ref::<SaveError> () {
          Some (SaveError::BufferValidationErrors { errors, .. }) => errors,
          _ => panic! ("expected buffer validation errors, got: {}", error), };
      assert_eq! (errors, & [ BufferValidationError::CannotDeleteAndMergeSimultaneously {
        pid   : ID::from ("b"),
        title : "b" . to_string (),
        role  : NodeMergeRole::Acquiree, } ]);
      assert_eq! (skgrepo_files (config) ?, files_before,
                  "a refused save must change no file");
      Ok (( )) } )) }

/// Save 'buffer' through the full pipeline; the save must succeed.
/// Returns the graph handle the save updated, after auditing it.
async fn save (
  buffer  : &str,
  config  : &SkgConfig,
  tantivy : &TantivyIndex,
) -> Result<InRustGraphHandle, Box<dyn Error>> {
  let graph : InRustGraphHandle = graph_handle_from_config (config) ?;
  let response : SaveResponse =
    save_on_graph (buffer, config, tantivy, &graph) . await ?;
  assert! (response . errors . is_empty (),
           "save errors: {:?}", response . errors);
  audit_inrustgraph_or_panic (&graph) ?;
  Ok (graph) }

async fn save_on_graph (
  buffer  : &str,
  config  : &SkgConfig,
  tantivy : &TantivyIndex,
  graph   : &InRustGraphHandle,
) -> Result<SaveResponse, Box<dyn Error>> {
  let mut views_state : ViewsState = ViewsState {
    open_views     : OpenViews::new (),
    graph_straddle : None, };
  let (mut stream, _read_end) : (TcpStream, TcpStream) =
    mk_test_tcp_stream_pair ();
  update_from_and_rerender_buffer (
    &mut stream, buffer, config, tantivy, graph,
    &Err ( String::new () ), &mut views_state ) . await }

fn node_on_disk (
  config : &SkgConfig,
  pid    : &str,
) -> Result<Graphnode, Box<dyn Error>> {
  Ok ( graphnode_from_pid_and_skgrepo (
    config, ID::from (pid), &SkgrepoName::from ("main") ) ? ) }

/// The node's first content, read from disk.
fn first_child_on_disk (
  config : &SkgConfig,
  node   : &Graphnode,
) -> Result<Graphnode, Box<dyn Error>> {
  let first : &ID = &node . contains . first ()
    . ok_or ("the node has no contents") ? . member;
  node_on_disk (config, first . as_str ()) }

fn file_exists (
  config : &SkgConfig,
  pid    : &str,
) -> Result<bool, Box<dyn Error>> {
  let path : String = skg::util::path_from_pid_and_skgrepo (
    config, &SkgrepoName::from ("main"), ID::from (pid) ) ?;
  Ok (Path::new (&path) . exists ()) }

/// Every file of the 'main' skgrepo, by path, with its bytes.
fn skgrepo_files (
  config : &SkgConfig,
) -> Result<BTreeMap<PathBuf, Vec<u8>>, Box<dyn Error>> {
  let folder : &Path = &config . skgrepos [&SkgrepoName::from ("main")] . path;
  let mut files : BTreeMap<PathBuf, Vec<u8>> = BTreeMap::new ();
  for entry in fs::read_dir (folder) ? {
    let path : PathBuf = entry ? . path ();
    if path . is_file () {
      files . insert (path . clone (), fs::read (&path) ?); }}
  Ok (files) }
