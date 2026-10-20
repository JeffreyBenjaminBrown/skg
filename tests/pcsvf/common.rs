// Fixtures for the PCSVF tests: one gitrepo per owned skgrepo of a
// case, at HEAD '<case>/<skgrepo>/head' with
// '<case>/<skgrepo>/worktree' in the worktree.

#[path = "../shared/git_fixture_helpers.rs"]
#[allow(unused_imports, dead_code)] // shared by several test targets, each using part of it
pub mod git_fixture_helpers;

use git_fixture_helpers::{mk_test_tcp_stream_pair, setup_gitrepo_with_fixtures};

use skg::change_leaves::LeafKey;
use skg::dbs::in_rust_graph::InRustGraphHandle;
use skg::dbs::init::create_empty_tantivy_index;
use skg::graph_straddle::build::current_graph_straddle_in_slot;
use skg::graph_straddle::types::GraphStraddle;
use skg::readiness_store::ReadinessStore;
use skg::serve::ViewsState;
use skg::serve::handlers::save_buffer::SaveResponse;
use skg::serve::handlers::show_unshown_changes::handle_show_unshown_changes_request;
use skg::serve::handlers::stage_and_commit::handle_stage_and_commit_request;
use skg::skgrepo_sets::SkgrepoRestriction;
use skg::serve::handlers::diff_view::{
  DiffViewBuffer, diffView_buffer};
use skg::test_utils::{
  cleanup_test_tantivy, extract_string_field_from_sexp, graph_handle_from_config,
  skg_env_from_parts, update_from_and_rerender_buffer_test};
use skg::types::env::{RuntimeGeneration, SkgEnv};
use skg::types::misc::{ID, SkgConfig, Skgrepo, SkgrepoName, TantivyIndex};
use skg::types::views_state::{OpenViews, DiffViewBufferKind, ViewId};

use futures::executor::block_on;
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::error::Error;
use std::fs;
use std::io::Read;
use std::net::TcpStream;
use std::path::{Path, PathBuf};
use std::sync::{Arc, MutexGuard};
use tempfile::TempDir;

const FIXTURES : &str = "tests/pcsvf/fixtures";

pub struct Fixture {
  pub root        : TempDir,
  pub skgrepos    : Vec<String>,
  pub config      : SkgConfig,
  pub tantivy     : TantivyIndex,
  pub graph       : InRustGraphHandle,
  pub views_state : ViewsState,
}

/// The case's gitrepos, one owned skgrepo each, in privacy order.
pub fn fixture (
  case     : &str,
  skgrepos : &[&str],
) -> Result<Fixture, Box<dyn Error>> {
  let root : TempDir = TempDir::new () ?;
  let mut skgrepo_map : HashMap<SkgrepoName, Skgrepo> = HashMap::new ();
  for name in skgrepos {
    let fixtures : PathBuf = Path::new (FIXTURES) . join (case) . join (name);
    setup_gitrepo_with_fixtures (
      &root . path () . join (name),
      fixtures . join ("head") . to_str () . unwrap (),
      fixtures . join ("worktree") . to_str () . unwrap () ) ?;
    skgrepo_map . insert ( SkgrepoName::from (*name), Skgrepo {
      name : SkgrepoName::from (*name), abbreviation : None,
      path : root . path () . join (name), owned : true } ); }
  let mut config : SkgConfig = SkgConfig::fromSkgreposAndTantivyFolder (
    skgrepo_map, root . path () . join ("tantivy") . to_str () . unwrap () );
  config . skgrepo_order =
    skgrepos . iter () . map ( |name| SkgrepoName::from (*name) ) . collect ();
  config . data_root = root . path () . to_path_buf ();
  let tantivy : TantivyIndex = create_empty_tantivy_index (&config . tantivy_folder) ?;
  let graph : InRustGraphHandle = graph_handle_from_config (&config) ?;
  Ok ( Fixture {
    root,
    skgrepos : skgrepos . iter () . map ( |name| name . to_string () ) . collect (),
    config, tantivy, graph,
    views_state : ViewsState {
      open_views        : OpenViews::new (),
      graph_straddle    : None, } } ) }

/// Run 'body' on the fixture, then remove its search index.
pub fn with_fixture (
  case     : &str,
  skgrepos : &[&str],
  body     : impl FnOnce (&mut Fixture) -> Result<(), Box<dyn Error>>,
) -> Result<(), Box<dyn Error>> {
  let mut f : Fixture = fixture (case, skgrepos) ?;
  let result : Result<(), Box<dyn Error>> = body (&mut f);
  cleanup_test_tantivy ( Some (&f . config . tantivy_folder) ) ?;
  result }

pub fn graph_straddle (
  f : &Fixture,
) -> Result<GraphStraddle, Box<dyn Error>> {
  Ok ( GraphStraddle::from_head_and_live (&f . config, f . graph . load_full ()) ? ) }

/// The text of the "all changes" buffer, with no marks; None when it
/// would be empty.
pub fn all_changes_buffer (
  f : &Fixture,
) -> Result<Option<String>, Box<dyn Error>> {
  all_changes_buffer_with_marks (f, &[]) }

/// The same, with the leaves 'marked' marked ready at their current
/// fingerprints.
pub fn all_changes_buffer_with_marks (
  f      : &Fixture,
  marked : &[LeafKey],
) -> Result<Option<String>, Box<dyn Error>> {
  let env : SkgEnv = skg_env_from_parts (&f . config, &f . tantivy, &f . graph);
  let runtime : Arc<RuntimeGeneration> = env . runtime_snapshot ();
  let straddle : GraphStraddle = graph_straddle (f) ?;
  let mut store : ReadinessStore = ReadinessStore::default ();
  for key in marked {
    store . mark ( key . clone (),
                   straddle . change_leaves () . leaf_fingerprint (key)
                   . expect ("a marked key is a leaf") ); }
  Ok ( match diffView_buffer (
    &runtime, &straddle, &store, DiffViewBufferKind::AllChanges ) ? {
    DiffViewBuffer::Empty => None,
    DiffViewBuffer::Content { text, .. } => Some (text), } ) }

/// The view id under which 'open_buffer' registers a buffer: its
/// client's name for it.
pub fn view_id_of (
  diffView_buffer_kind : DiffViewBufferKind,
) -> ViewId {
  ViewId::ContentView ( format! ( "diff-view: {}", diffView_buffer_kind . wire_name () )) }

/// Open the "all changes" buffer; see 'open_buffer'.
pub fn open_all_changes_buffer (
  f : &mut Fixture,
) -> Result<String, Box<dyn Error>> {
  open_buffer (f, DiffViewBufferKind::AllChanges) ?
    . ok_or_else ( || "the all-changes buffer is empty" . into () ) }

/// Open a buffer of the diff view as the "diff view"
/// request does: drawn from the connection's graph straddle with the
/// marks on disk, and registered as a view of the diff view.
/// None when it would be empty.
pub fn open_buffer (
  f                    : &mut Fixture,
  diffView_buffer_kind : DiffViewBufferKind,
) -> Result<Option<String>, Box<dyn Error>> {
  let env : SkgEnv = skg_env_from_parts (&f . config, &f . tantivy, &f . graph);
  let runtime : Arc<RuntimeGeneration> = env . runtime_snapshot ();
  let straddle : Arc<GraphStraddle> = current_graph_straddle_in_slot (
    &mut f . views_state . graph_straddle, &f . config, runtime . graph . clone () ) ?;
  let buffer : DiffViewBuffer = {
    let store : MutexGuard<ReadinessStore> =
      env . runtime . readiness_store . lock () . unwrap ();
    diffView_buffer (&runtime, &straddle, &store, diffView_buffer_kind) ? };
  Ok ( match buffer {
    DiffViewBuffer::Empty => None,
    DiffViewBuffer::Content { text, pids, viewforest } => {
      f . views_state . open_views . register_diffView_view (
        &runtime . graph, view_id_of (diffView_buffer_kind),
        viewforest, &pids, diffView_buffer_kind );
      Some (text) }, } ) }

/// Save the "all changes" buffer; see 'save_buffer'.
pub fn save (
  f      : &mut Fixture,
  buffer : &str,
) -> Result<SaveResponse, Box<dyn Error>> {
  save_buffer (f, DiffViewBufferKind::AllChanges, buffer) }

/// Save 'text' as the buffer 'open_buffer' registered.
pub fn save_buffer (
  f                    : &mut Fixture,
  diffView_buffer_kind : DiffViewBufferKind,
  text                 : &str,
) -> Result<SaveResponse, Box<dyn Error>> {
  let (mut stream, _read_end) : (TcpStream, TcpStream) =
    mk_test_tcp_stream_pair ();
  Ok ( block_on ( update_from_and_rerender_buffer_test (
    &mut stream, text, &f . config, &f . tantivy, &f . graph,
    &Ok ( view_id_of (diffView_buffer_kind) ), &mut f . views_state )) ? ) }

/// The round-trip invariant for a buffer of the diff view:
/// saved unchanged, it changes no file, reports no error, and is
/// reconciled to itself.
pub fn saves_back_unchanged (
  case                 : &str,
  skgrepos             : &[&str],
  diffView_buffer_kind : DiffViewBufferKind,
) -> Result<(), Box<dyn Error>> {
  with_fixture (case, skgrepos, |f| {
    let text : String = open_buffer (f, diffView_buffer_kind) ?
      . ok_or ("the fixture's buffer is empty") ?;
    let files_before : BTreeMap<String, String> = skg_files (f) ?;
    let response : SaveResponse = save_buffer (f, diffView_buffer_kind, &text) ?;
    assert! ( response . errors . is_empty (),
              "the buffer must save without errors, got {:?}\n\nbuffer:\n{}",
              response . errors, text );
    assert_eq! ( skg_files (f) ?, files_before,
                 "saving the buffer unchanged must not change the graph:\n{}",
                 text );
    assert_eq! ( response . saved_view, text,
                 "the reconciled buffer must be the buffer saved" );
    Ok (( )) }) }

/// Every '.skg' file of the fixture's skgrepos, by path, with its
/// contents.
pub fn skg_files (
  f : &Fixture,
) -> Result<BTreeMap<String, String>, Box<dyn Error>> {
  let mut files : BTreeMap<String, String> = BTreeMap::new ();
  for skgrepo in &f . skgrepos {
    for entry in fs::read_dir ( f . root . path () . join (skgrepo) ) ? {
      let path : PathBuf = entry ? . path ();
      if path . extension () . is_some_and ( |ext| ext == "skg" ) {
        files . insert (
          format! ( "{}/{}", skgrepo,
                    path . file_name () . unwrap () . to_string_lossy () ),
          fs::read_to_string (&path) ? ); }}}
  Ok (files) }

pub fn ids (
  list : &[&str],
) -> BTreeSet<ID> {
  list . iter () . map ( |s| ID::from (*s) ) . collect () }

/// Send 'request' to the "stage and commit" handler, under the
/// skgrepo set 'all'; return everything it wrote.
pub fn send_stage_and_commit_request (
  f       : &mut Fixture,
  env     : &SkgEnv,
  request : &str,
) -> Result<String, Box<dyn Error>> {
  let all_skgrepos : SkgrepoRestriction =
    SkgrepoRestriction::named (&f . config, "all" . into ()) ?;
  let (mut stream, mut read_end) : (TcpStream, TcpStream) =
    mk_test_tcp_stream_pair ();
  handle_stage_and_commit_request (
    &mut stream, request, env, &mut f . views_state, &all_skgrepos );
  drop (stream);
  let mut response : String = String::new ();
  read_end . read_to_string (&mut response) ?;
  Ok (response) }

/// Send a "show unshown changes" request for the buffer 'diffView_buffer_kind'
/// to its handler, under the skgrepo set 'all'; return everything it
/// wrote.
pub fn send_show_unshown_changes_request (
  f                    : &mut Fixture,
  diffView_buffer_kind : DiffViewBufferKind,
) -> Result<String, Box<dyn Error>> {
  let env : SkgEnv = skg_env_from_parts (&f . config, &f . tantivy, &f . graph);
  let all_skgrepos : SkgrepoRestriction =
    SkgrepoRestriction::named (&f . config, "all" . into ()) ?;
  let (mut stream, mut read_end) : (TcpStream, TcpStream) =
    mk_test_tcp_stream_pair ();
  handle_show_unshown_changes_request (
    &mut stream,
    &format! ( "((request . \"show unshown changes\") (view-id . \"{}\"))\n",
               view_id_of (diffView_buffer_kind) . repr_in_client () ),
    &env, &mut f . views_state, &all_skgrepos );
  drop (stream);
  let mut response : String = String::new ();
  read_end . read_to_string (&mut response) ?;
  Ok (response) }

/// The "all changes" buffer as the "show unshown changes" request
/// redraws it, and the unshown count of its readiness summary.
pub fn show_unshown_changes (
  f : &mut Fixture,
) -> Result<(String, usize), Box<dyn Error>> {
  let response : String =
    send_show_unshown_changes_request (f, DiffViewBufferKind::AllChanges) ?;
  let unshown : usize = response . split ("(unshown ") . nth (1)
    . and_then ( |rest| rest . split (')') . next () )
    . and_then ( |count| count . parse () . ok () )
    . unwrap_or_else ( || panic! ( "no unshown count in:\n{}", response ));
  Ok (( rerendered_view_content (&response), unshown )) }

/// The content of the first rerendered view in 'response'.
pub fn rerendered_view_content (
  response : &str,
) -> String {
  first_streamed_view_content (response, "rerender-view")
    . expect ("a view is rerendered") }

/// The content of the first collateral view in 'response', if any.
pub fn collateral_view_content (
  response : &str,
) -> Option<String> {
  first_streamed_view_content (response, "collateral-view") }

fn first_streamed_view_content (
  response      : &str,
  response_type : &str,
) -> Option<String> {
  response . split ("Content-Length")
    . find ( |message| message . contains (response_type) )
    . map ( |message|
      extract_string_field_from_sexp (
        &message [ message . find ('(') . unwrap () .. ], "content" )
      . expect ("the streamed view has content") ) }

/// The lines of 'buffer' that draw an unrestricted vognode of 'node'.
pub fn node_lines<'a> (
  buffer : &'a str,
  node   : &str,
) -> Vec<&'a str> {
  let atom : String = format! ("(node (id {}) ", node);
  buffer . lines () . filter ( |line| line . contains (&atom) ) . collect () }

/// 'buffer' with an editable-view request at the first occurrence of
/// 'metadata', which must draw a write-protected node.
pub fn requesting_editable_at (
  buffer   : &str,
  metadata : &str,
) -> String {
  let at : usize = buffer . find (metadata)
    . unwrap_or_else ( || panic! ( "no {:?} in:\n{}", metadata, buffer ));
  format! ( "{}{}", &buffer [.. at],
            buffer [at ..] . replacen (
              "writeProtected", "writeProtected (viewRequests editableView)", 1 )) }
