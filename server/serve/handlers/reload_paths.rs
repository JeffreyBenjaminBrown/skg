//! Partial reload of specific telescopes from disk.
//!
//! When a `.skg` worktree file changes out of band (a magit discard is
//! the motivating case), the client sends the affected paths and the
//! server re-reads those telescopes from disk and updates the three
//! derived stores to match -- a *partial* reload, not a full rebuild.
//!
//! Reload is READ-ONLY with respect to the filesystem: it never writes
//! `.skg` files, so it must not go through `update_graph_minus_nodeMerges`
//! (whose delete-propagation cleanup rewrites other nodes' files). See
//! TODO/partial-reload-and-magit/.

use crate::dbs::filesystem::multiple_nodes::nodecomplete_from_telescope_on_disk;
use crate::dbs::in_rust_graph::{
  InRustGraph,
  apply_definenodes_to_inRustGraph,
  override_invariants::error_unless_override_invariants_hold,
};
use crate::save::apply_define_nodes_to_stores;
use crate::serve::ViewsState;
use crate::serve::protocol::TcpToClient;
use crate::serve::util::{send_response_with_length_prefix, tag_text_response};
use crate::source_sets::ActiveSourceSet;
use crate::types::env::SkgEnv;
use crate::types::misc::{ID, SkgConfig, SourceName};
use crate::types::nodes::complete::NodeComplete;
use crate::types::save::{DefineNode, DeleteNode, SaveNode};
use crate::types::sexp::extract_string_list_from_sexp;

use futures::executor::block_on;
use std::collections::HashSet;
use std::io::ErrorKind;
use std::net::TcpStream;
use std::path::{Path, PathBuf};

/// What a single touched telescope resolves to when re-read from disk.
pub enum TelescopeReloadOutcome {
  /// The telescope still folds; upsert it into the stores.
  Save (NodeComplete),
  /// No section of this telescope remains on disk; delete it from the
  /// stores (its inbound references become legal danglers).
  Delete,
  /// The telescope's on-disk state is unloadable (unparseable YAML,
  /// empty/missing pid, pid != filename, or sections-but-no-home). Keep
  /// last-good in the graph; the String is the reason, for the warning.
  Fatal (String),
}

/// One telescope touched by a reload request, with its classification.
pub struct TouchedTelescope {
  pub pid     : ID,
  /// A source under which a touched section of this telescope lived.
  pub source  : SourceName,
  /// One representative touched path (for messages / repo grouping).
  pub path    : PathBuf,
  pub outcome : TelescopeReloadOutcome,
}

pub fn handle_reload_paths_request (
  stream            : &mut TcpStream,
  request           : &str,
  env               : &mut SkgEnv,
  views_state       : &mut ViewsState,
  active_source_set : &ActiveSourceSet,
) {
  let _ = ( views_state, active_source_set );
  let parsed = match sexp::parse (request) {
    Ok (s) => s,
    Err (e) => {
      send_reload_error (stream, &format! (
        "reload paths: failed to parse request: {}", e ));
      return; } };
  let path_strings : Vec<String> =
    match extract_string_list_from_sexp (&parsed, "paths") {
      Ok (v) => v,
      Err (e) => {
        send_reload_error (stream, &format! (
          "reload paths: failed to extract paths: {}", e ));
        return; } };
  let paths : Vec<PathBuf> =
    path_strings . into_iter () . map (PathBuf::from) . collect ();
  let touched : Vec<TouchedTelescope> =
    classify_touched_telescopes (&env . config, &paths);
  let msg : String =
    match block_on ( reload_touched_telescopes (env, touched) ) {
      Ok (m)  => m,
      Err (e) => {
        send_reload_error (stream, &format! ("Reload failed: {}", e));
        return; } };
  send_response_with_length_prefix (
    stream,
    & tag_text_response ( TcpToClient::ReloadPaths, &msg )); }

/// Apply the survivors of a classification to the three derived stores
/// WITHOUT writing the filesystem. Fatal telescopes keep their last-good
/// graph state. Returns a human-readable summary, or `Err` if the store
/// update itself failed.
pub async fn reload_touched_telescopes (
  env     : &mut SkgEnv,
  touched : Vec<TouchedTelescope>,
) -> Result<String, String> {
  let mut defs : Vec<DefineNode> = Vec::new ();
  let mut fatals : Vec<(ID, String)> = Vec::new ();
  let (mut saves, mut deletes) : (usize, usize) = (0, 0);
  for t in touched {
    match t . outcome {
      TelescopeReloadOutcome::Save (nc) => {
        saves += 1;
        defs . push ( DefineNode::Save ( SaveNode (nc) )); }
      TelescopeReloadOutcome::Delete => {
        deletes += 1;
        defs . push ( DefineNode::Delete ( DeleteNode {
          id     : t . pid,
          source : t . source, } )); }
      TelescopeReloadOutcome::Fatal (reason) =>
        fatals . push (( t . pid, reason )), } }

  if defs . is_empty () {
    return Ok ( summarize_reload (0, 0, &fatals) ); }

  // Serialize this store mutation against any concurrent save / reload /
  // rebuild so no RCU update is lost (last-store-wins on the ArcSwap).
  let _write_guard =
    crate::write_lock::acquire_graph_write_lock () . await;

  // Batch guard: applying these to a clone of the live graph must not
  // break override invariants. If it would, reject the whole batch and
  // keep last-good (coarse attribution; see progress.org).
  { let mut candidate : InRustGraph =
      (* env . in_rust_graph . load_full () ) . clone ();
    apply_definenodes_to_inRustGraph (&mut candidate, &defs);
    if let Err (e) =
      error_unless_override_invariants_hold (&env . config, &candidate) {
        return Err ( format! (
          "reloading would violate override invariants ({}); \
           kept last-good state, stores unchanged", e )); } }

  // Commit to the three stores, filesystem untouched.
  let config = env . config . clone ();
  match apply_define_nodes_to_stores (
    defs, &[], config,
    &env . tantivy_index, &env . driver, &env . in_rust_graph,
    false /* write_fs */ ) . await {
    Ok (Some (new_index)) => { env . tantivy_index = new_index; }
    Ok (None) => {}
    Err (e) => return Err ( format! (
      "store update failed: {}", e )), }
  Ok ( summarize_reload (saves, deletes, &fatals) ) }

fn summarize_reload (
  saves   : usize,
  deletes : usize,
  fatals  : &[(ID, String)],
) -> String {
  let mut msg : String = format! (
    "Reloaded {} telescope(s), removed {}.", saves, deletes );
  if ! fatals . is_empty () {
    msg . push_str ( &format! (
      " {} telescope(s) could not be reloaded and kept their last-good \
       state:", fatals . len () ));
    for (pid, reason) in fatals {
      msg . push_str ( &format! (
        "\n  {}: {}", pid . as_str (), reason )); } }
  msg }

/// Dedup the request's paths to telescope pids (first-seen order) and
/// classify each by attempting a fresh fold from disk. Pure w.r.t. the
/// filesystem: reads only, writes nothing.
pub fn classify_touched_telescopes (
  config : &SkgConfig,
  paths  : &[PathBuf],
) -> Vec<TouchedTelescope> {
  let sources : Vec<(SourceName, PathBuf)> =
    config . sources . iter ()
    . map ( |(name, src)| ( name . clone (), src . path . clone () ) )
    . collect ();
  let mut seen : HashSet<ID> = HashSet::new ();
  let mut unique : Vec<(ID, SourceName, PathBuf)> = Vec::new ();
  for path in paths {
    match resolve_path_to_source_pid (&sources, path) {
      Some ((source, pid)) => {
        if seen . insert ( pid . clone () ) {
          unique . push (( pid, source, path . clone () )); } }
      None =>
        tracing::warn! (
          path = %path . display (),
          "reload: path is not a .skg file under any source; skipping" ), } }
  unique . into_iter () . map ( |(pid, source, path)| {
    let outcome : TelescopeReloadOutcome =
      match nodecomplete_from_telescope_on_disk (config, &pid) {
        Ok (nc) =>
          TelescopeReloadOutcome::Save (nc),
        Err (e) if e . kind () == ErrorKind::NotFound =>
          TelescopeReloadOutcome::Delete,
        Err (e) =>
          TelescopeReloadOutcome::Fatal ( e . to_string () ), };
    TouchedTelescope { pid, source, path, outcome } } )
    . collect () }

/// Which configured source contains `path`, and the telescope pid (=
/// file stem) of the `.skg` file there. `None` if `path` is not a
/// `.skg` file inside any source. Split from the config so it can be
/// unit-tested without building an `SkgConfig`.
fn resolve_path_to_source_pid (
  sources : &[(SourceName, PathBuf)],
  path    : &Path,
) -> Option<(SourceName, ID)> {
  if path . extension () . and_then ( |e| e . to_str () )
     != Some ("skg") {
    return None; }
  let stem : &str =
    path . file_stem () . and_then ( |s| s . to_str () ) ?;
  for (name, dir) in sources {
    if path . starts_with (dir) {
      return Some (( name . clone (),
                     ID ( stem . to_string () ) )); } }
  None }

fn send_reload_error (
  stream : &mut TcpStream,
  msg    : &str,
) {
  tracing::error! ("{}", msg);
  send_response_with_length_prefix (
    stream,
    & tag_text_response ( TcpToClient::Error, msg )); }

#[cfg(test)]
mod tests {
  use super::*;

  fn src (name : &str, dir : &str) -> (SourceName, PathBuf) {
    ( SourceName::from (name), PathBuf::from (dir) ) }

  #[test]
  fn resolves_path_in_a_source_to_its_stem_and_source () {
    let sources = vec! [
      src ("public", "/data/public"),
      src ("private", "/data/private"), ];
    assert_eq! (
      resolve_path_to_source_pid (
        &sources, Path::new ("/data/private/abc123.skg") ),
      Some (( SourceName::from ("private"),
              ID ("abc123" . to_string ()) )) ); }

  #[test]
  fn non_skg_files_do_not_resolve () {
    let sources = vec! [ src ("public", "/data/public") ];
    assert_eq! (
      resolve_path_to_source_pid (
        &sources, Path::new ("/data/public/notes.org") ),
      None ); }

  #[test]
  fn paths_outside_every_source_do_not_resolve () {
    let sources = vec! [ src ("public", "/data/public") ];
    assert_eq! (
      resolve_path_to_source_pid (
        &sources, Path::new ("/elsewhere/abc.skg") ),
      None ); }
}
