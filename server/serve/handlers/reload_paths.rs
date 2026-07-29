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
use crate::serve::ViewsState;
use crate::serve::protocol::TcpToClient;
use crate::serve::util::{send_response_with_length_prefix, tag_text_response};
use crate::source_sets::ActiveSourceSet;
use crate::types::env::SkgEnv;
use crate::types::misc::{ID, SkgConfig, SourceName};
use crate::types::nodes::complete::NodeComplete;
use crate::types::sexp::extract_string_list_from_sexp;

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
  let (mut saves, mut deletes, mut fatals)
    : (usize, usize, usize) = (0, 0, 0);
  for t in & touched {
    match t . outcome {
      TelescopeReloadOutcome::Save (_)   => saves   += 1,
      TelescopeReloadOutcome::Delete     => deletes += 1,
      TelescopeReloadOutcome::Fatal (_)  => fatals  += 1, } }
  let msg : String = format! (
    "Reload: {} to save, {} to delete, {} fatal \
     (store writes not yet implemented).",
    saves, deletes, fatals );
  send_response_with_length_prefix (
    stream,
    & tag_text_response ( TcpToClient::ReloadPaths, &msg )); }

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
