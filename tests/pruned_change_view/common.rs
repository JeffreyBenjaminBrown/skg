// Helpers for the pruned change view tests, over the git fixtures
// ('tests/shared/git_fixture.rs') of the cases in 'tests/pruned_change_view/fixtures/',
// each of whose owned skgrepos is a folder of the case.

use crate::git_fixture::{
  Fixture, OwnedSkgrepos, graph_straddle, save_view, skg_files,
  with_git_fixture};
use crate::git_fixture_helpers::mk_test_tcp_stream_pair;

use skg::diff_view::change_leaves::LeafKey;
use skg::metadata_canon::without_viewnode_ids;
use skg::graph_straddle::build::current_graph_straddle_in_slot;
use skg::graph_straddle::types::GraphStraddle;
use skg::diff_view::readiness_store::ReadinessStore;
use skg::serve::handlers::save_buffer::SaveResponse;
use skg::serve::handlers::integrate_unshown_changes::handle_integrate_unshown_changes_request;
use skg::serve::handlers::stage_and_commit::handle_stage_and_commit_request;
use skg::diff_view::pruned_change_view::RenderedDiffViewBuffer;
use skg::skgrepo_sets::SkgrepoRestriction;
use skg::serve::handlers::diff_view::diffView_buffer;
use skg::test_utils::{
  extract_string_field_from_sexp, skg_env_from_parts,
  update_from_and_rerender_buffer_test};
use skg::to_org::render::content_view::multi_root_view;
use skg::types::env::{RuntimeGeneration, SkgEnv};
use skg::types::misc::ID;
use skg::types::views_state::{DiffViewBufferKind, ViewId};
use skg::types::viewnode::Viewnode;

use ego_tree::Tree;

use futures::executor::block_on;
use std::collections::{BTreeMap, BTreeSet};
use std::error::Error;
use std::io::Read;
use std::net::TcpStream;
use std::path::Path;
use std::sync::{Arc, MutexGuard};

const FIXTURES : &str = "tests/pruned_change_view/fixtures";

/// Run 'body' on the case's fixture, its owned skgrepos 'skgrepos' in
/// privacy order; see 'with_git_fixture'.
pub fn with_fixture (
  case     : &str,
  skgrepos : &[&str],
  body     : impl FnOnce (&mut Fixture) -> Result<(), Box<dyn Error>>,
) -> Result<(), Box<dyn Error>> {
  with_fixture_and_foreign (case, skgrepos, &[], body) }

/// The same, with the foreign skgrepos 'foreign' after them.
pub fn with_fixture_and_foreign (
  case     : &str,
  skgrepos : &[&str],
  foreign  : &[&str],
  body     : impl FnOnce (&mut Fixture) -> Result<(), Box<dyn Error>>,
) -> Result<(), Box<dyn Error>> {
  with_git_fixture ( &Path::new (FIXTURES) . join (case),
                     OwnedSkgrepos::Named (skgrepos), foreign, body ) }

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
  Ok ( diffView_buffer (
    &runtime, &straddle, &store, DiffViewBufferKind::AllChanges ) ?
    . map ( |rendered| rendered . text ) ) }

/// The viewforest id under which 'open_buffer' registers a buffer: its
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
/// request does: rendered from the connection's graph straddle with the
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
  let buffer : Option<RenderedDiffViewBuffer> = {
    let store : MutexGuard<ReadinessStore> =
      env . runtime . readiness_store . lock () . unwrap ();
    diffView_buffer (&runtime, &straddle, &store, diffView_buffer_kind) ? };
  Ok ( match buffer {
    None => None,
    Some (RenderedDiffViewBuffer { text, pids, viewforest }) => {
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
  save_view (f, &view_id_of (diffView_buffer_kind), text) }

/// The viewforest id of the "all changes" buffer.
pub fn all_changes_view (
) -> ViewId {
  view_id_of (DiffViewBufferKind::AllChanges) }

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

/// Send a "integrate unshown changes" request for the buffer 'diffView_buffer_kind'
/// to its handler, under the skgrepo set 'all'; return everything it
/// wrote.
pub fn send_integrate_unshown_changes_request (
  f                    : &mut Fixture,
  diffView_buffer_kind : DiffViewBufferKind,
) -> Result<String, Box<dyn Error>> {
  let env : SkgEnv = skg_env_from_parts (&f . config, &f . tantivy, &f . graph);
  let all_skgrepos : SkgrepoRestriction =
    SkgrepoRestriction::named (&f . config, "all" . into ()) ?;
  let (mut stream, mut read_end) : (TcpStream, TcpStream) =
    mk_test_tcp_stream_pair ();
  handle_integrate_unshown_changes_request (
    &mut stream,
    &format! ( "((request . \"integrate unshown changes\") (view-id . \"{}\"))\n",
               view_id_of (diffView_buffer_kind) . repr_in_client () ),
    &env, &mut f . views_state, &all_skgrepos );
  drop (stream);
  let mut response : String = String::new ();
  read_end . read_to_string (&mut response) ?;
  Ok (response) }

/// The "all changes" buffer as the "integrate unshown changes" request
/// rerenders it, and the not-editable count of its readiness summary.
pub fn integrate_unshown_changes (
  f : &mut Fixture,
) -> Result<(String, usize), Box<dyn Error>> {
  let response : String =
    send_integrate_unshown_changes_request (f, DiffViewBufferKind::AllChanges) ?;
  let not_editable : usize = response . split ("(notEditable ") . nth (1)
    . and_then ( |rest| rest . split (')') . next () )
    . and_then ( |count| count . parse () . ok () )
    . unwrap_or_else ( || panic! ( "no notEditable count in:\n{}", response ));
  Ok (( rerendered_view_content (&response), not_editable )) }

/// Retitle 'node' to 'title' by saving a content view of it, as
/// another buffer would. Returns the "all changes" buffer as its
/// client then holds it: 'buffer', or its collateral rerendering if
/// the save sent one.
pub fn retitle_in_a_content_view (
  f      : &mut Fixture,
  node   : &str,
  title  : &str,
  buffer : &str,
) -> Result<String, Box<dyn Error>> {
  let view_id : ViewId =
    ViewId::ContentView ( format! ("content-view-of-{}", node) );
  let (text, pids, tree) : (String, Vec<ID>, Tree<Viewnode>) =
    multi_root_view ( &f . config, Some (&f . tantivy), &[ ID::from (node) ] ) ?;
  f . views_state . open_views . register_view (
    &f . graph . load_full (), view_id . clone (), tree, &pids );
  let root_line : &str = text . lines () . next () . unwrap ();
  let edited : String = text . replacen (
    root_line,
    &format! ( "{} {}", &root_line [ .. root_line . rfind (' ') . unwrap () ],
               title ),
    1 );
  let (mut stream, mut read_end) : (TcpStream, TcpStream) =
    mk_test_tcp_stream_pair ();
  let response : SaveResponse = block_on (
    update_from_and_rerender_buffer_test (
      &mut stream, &edited, &f . config, &f . tantivy, &f . graph,
      &Ok (view_id), &mut f . views_state )) ?;
  assert! ( response . errors . is_empty (),
            "the content view must save, got {:?}", response . errors );
  drop (stream);
  let mut streamed : String = String::new ();
  read_end . read_to_string (&mut streamed) ?;
  Ok ( collateral_view_content (&streamed)
       . unwrap_or_else ( || buffer . to_string () )) }
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

/// The lines of 'buffer' that show an unrestricted vognode of 'node',
/// without their viewnode IDs.
pub fn node_lines (
  buffer : &str,
  node   : &str,
) -> Vec<String> {
  let atom : String = format! ("(node (id {}) ", node);
  buffer . lines () . filter ( |line| line . contains (&atom) )
    . map (without_viewnode_ids) . collect () }

/// 'buffer' with an editable-view request at the first occurrence of
/// 'metadata', which must show a write-protected node.
pub fn requesting_editable_at (
  buffer   : &str,
  metadata : &str,
) -> String {
  let at : usize = buffer . find (metadata)
    . unwrap_or_else ( || panic! ( "no {:?} in:\n{}", metadata, buffer ));
  format! ( "{}{}", &buffer [.. at],
            buffer [at ..] . replacen (
              "writeProtected", "writeProtected (viewRequests editableView)", 1 )) }

/// 'buffer' with 'new_line' inserted after its first line starting,
/// viewnode ID aside, 'prefix'.
pub fn insert_after_line_starting (
  buffer   : &str,
  prefix   : &str,
  new_line : &str,
) -> String {
  let mut inserted : bool = false;
  let mut lines : Vec<&str> = Vec::new ();
  for line in buffer . lines () {
    lines . push (line);
    if ! inserted && without_viewnode_ids (line) . starts_with (prefix) {
      lines . push (new_line);
      inserted = true; }}
  assert! ( inserted, "no line starts {:?}:\n{}", prefix, buffer );
  lines . join ("\n") + "\n" }

/// 'buffer' without its first line starting, viewnode ID aside, 'prefix' and the
/// headlines below it, with their bodies.
pub fn without_subtree_starting (
  buffer : &str,
  prefix : &str,
) -> String {
  let depth = |line : &str| line . chars () . take_while ( |c| *c == '*' ) . count ();
  let mut subtree_depth : Option<usize> = None;
  let mut found : bool = false;
  let mut kept : Vec<&str> = Vec::new ();
  for line in buffer . lines () {
    if let Some (own) = subtree_depth {
      if line . starts_with ('*') && depth (line) <= own {
        subtree_depth = None; }
      else { continue; }}
    if ! found && without_viewnode_ids (line) . starts_with (prefix) {
      found = true;
      subtree_depth = Some ( depth (line) );
      continue; }
    kept . push (line); }
  assert! ( found, "no line starts {:?}:\n{}", prefix, buffer );
  kept . join ("\n") + "\n" }

/// Panic unless 'lines' occur in 'buffer', its viewnode IDs ignored, in
/// this order.
pub fn assert_lines_in_order (
  buffer : &str,
  lines  : &[&str],
) {
  let buffer : &str = &without_viewnode_ids (buffer);
  let mut rest : &str = buffer;
  for line in lines {
    match rest . find (line) {
      Some (at) => rest = &rest [ at + line . len () .. ],
      None => panic! ( "expected, in order, {:?}; missing {:?} in\n{}",
                       lines, line, buffer ), }}}

/// A new headline of the user's that affects no parent, 'stars' deep.
/// It names no skgrepo: it inherits that of its nearest unrestricted
/// ancestor, through a phantom or a folder.
pub fn note (
  stars : usize,
  title : &str,
) -> String {
  format! ( "{} (skg (node nonMember)) {}",
            "*" . repeat (stars), title ) }

/// Save 'buffer', then save what the save rendered: both without errors,
/// and the second rendering the same as the first. Returns it.
pub fn saved_and_rerendered (
  f      : &mut Fixture,
  buffer : &str,
) -> Result<String, Box<dyn Error>> {
  let first : SaveResponse = save (f, buffer) ?;
  assert! ( first . errors . is_empty (),
            "the save must succeed, got {:?}\n{}", first . errors, buffer );
  let second : SaveResponse = save (f, &first . saved_view) ?;
  assert! ( second . errors . is_empty (),
            "the second save must succeed, got {:?}\n{}",
            second . errors, first . saved_view );
  assert_eq! ( second . saved_view, first . saved_view,
               "a rerender keeps what the first rendered" );
  Ok (second . saved_view) }

/// The headline of the parent of the one line whose title is 'title'.
pub fn parent_line_of (
  buffer : &str,
  title  : &str,
) -> String {
  let headlines : Vec<&str> =
    buffer . lines () . filter ( |line| line . starts_with ('*') ) . collect ();
  let suffix : String = format! (" {}", title);
  let matching : Vec<usize> = (0 .. headlines . len ())
    . filter ( |&i| headlines [i] . ends_with (&suffix) ) . collect ();
  assert_eq! ( matching . len (), 1, "one headline titled {:?} in\n{}", title, buffer );
  let depth = |line : &str| line . chars () . take_while ( |c| *c == '*' ) . count ();
  let at : usize = matching [0];
  without_viewnode_ids ( headlines [.. at] . iter () . rev ()
    . find ( |line| depth (line) + 1 == depth (headlines [at]) )
    . unwrap_or_else ( || panic! ( "{:?} has no parent in\n{}", title, buffer )) ) }
