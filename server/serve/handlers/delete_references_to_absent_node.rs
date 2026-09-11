use crate::delete_references_to_absent_node::{
  preview, preview_warning_org, result_org, rewrite };
use crate::save::{
  preflight_fs_from_saveinstructions_with_hoist_approval,
  update_graph_minus_nodeMerges };
use crate::serve::handlers::rerender_all_views::{
  stream_empty_rerender, stream_rerender_views_after_absent_reference_cleanup };
use crate::serve::protocol::TcpToClient;
use crate::serve::util::{
  send_response_with_length_prefix, tag_sexp_response, tag_text_response,
  value_from_request_sexp };
use crate::source_sets::ActiveSourceSet;
use crate::types::env::SkgEnv;
use crate::types::misc::ID;
use crate::serve::ViewsState;

use futures::executor::block_on;
use std::collections::HashSet;
use std::net::TcpStream;

pub fn handle_delete_references_to_absent_node_request (
  stream : &mut TcpStream,
  request : &str,
  env : &mut SkgEnv,
  views_state : &mut ViewsState,
  active_source_set : &ActiveSourceSet,
) {
  let raw_id = match value_from_request_sexp ("id", request) {
    Ok (id) => ID::from (id),
    Err (e) => return refuse (stream, &e), };
  let graph = env . in_rust_graph_snapshot ();
  let current = match preview (&graph, &env . config, &raw_id) {
    Ok (preview) => preview,
    Err (e) => return refuse (stream, &e), };
  let approved = value_from_request_sexp ("approved-preview", request) . ok ();
  if let Some (approved) = approved . as_deref () {
    if approved != current . opaque_approval () {
      return refuse (stream, "Cleanup preview is stale; rescan before rewriting."); }}
  if ! current . text_links . is_empty () && approved . is_none () {
    let content = preview_warning_org (&current);
    let response = format! (
      "((id \"{}\") (approved-preview \"{}\") (content \"{}\") (prompt \"Remove the structured references?\"))",
      escape (&raw_id . 0), escape (&current . opaque_approval ()), escape (&content));
    send_response_with_length_prefix (
      stream, &tag_sexp_response (
        TcpToClient::DeleteReferencesConfirmation, &response));
    stream_empty_rerender (stream);
    return; }
  let writes = match rewrite (&graph, &env . config, &current) {
    Ok (writes) => writes,
    Err (e) => return refuse (stream, &e), };
  if ! writes . is_empty () {
    if let Err (e) = preflight_fs_from_saveinstructions_with_hoist_approval (
      &writes, &[], &env . config, &HashSet::new ())
    { return refuse (stream, &e . to_string ()); }
    if let Err (e) = block_on (update_graph_minus_nodeMerges (
      writes, &[], env . config . clone (), &env . tantivy_index,
      &env . driver, &env . in_rust_graph ))
    { return refuse (stream, &e . to_string ()); }}
  let result = result_org (&current);
  let response = format! (
    "((content \"{}\") (changed-nodes {}) (changed-memberships {}))",
    escape (&result), current . changed_nodes (), current . structural . len ());
  send_response_with_length_prefix (
    stream, &tag_sexp_response (TcpToClient::DeleteReferencesResult, &response));
  let affected_owner_pids : HashSet<ID> = current . structural . iter ()
    . map (|occurrence| occurrence . owner_pid . clone ()) . collect ();
  stream_rerender_views_after_absent_reference_cleanup (
    stream, env, views_state, active_source_set, &raw_id, &affected_owner_pids );
}

fn refuse (stream : &mut TcpStream, error : &str) {
  send_response_with_length_prefix (
    stream, &tag_text_response (TcpToClient::Error, error));
  stream_empty_rerender (stream);
}

fn escape (text : &str) -> String {
  text . replace ('\\', "\\\\") . replace ('\"', "\\\"")
    . replace ('\n', "\\n")
}
