//! Partial reload of specific telescopes from disk.
//!
//! When a `.skg` worktree file changes out of band (a magit discard is
//! the motivating case), the client sends the affected paths and the
//! server re-reads those telescopes from disk and updates the three
//! derived stores to match. Reload is READ-ONLY with respect to the
//! filesystem: it never writes `.skg` files, so it must not go through
//! `update_graph_minus_nodeMerges` (whose delete-propagation cleanup
//! rewrites other nodes' files). See TODO/partial-reload-and-magit/.

use crate::serve::ViewsState;
use crate::serve::protocol::TcpToClient;
use crate::serve::util::{send_response_with_length_prefix, tag_text_response};
use crate::source_sets::ActiveSourceSet;
use crate::types::env::SkgEnv;

use std::net::TcpStream;

pub fn handle_reload_paths_request (
  stream            : &mut TcpStream,
  request           : &str,
  env               : &mut SkgEnv,
  views_state       : &mut ViewsState,
  active_source_set : &ActiveSourceSet,
) {
  let _ = ( request, env, views_state, active_source_set );
  send_response_with_length_prefix (
    stream,
    & tag_text_response (
      TcpToClient::ReloadPaths,
      "Reload not yet implemented." )); }
