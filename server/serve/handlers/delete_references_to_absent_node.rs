use crate::delete_references_to_absent_node::{
  preview, preview_warning_org, result_org, rewrite };
use crate::dbs::in_rust_graph::new_handle;
use crate::save::update_graph_minus_nodeMerges_with_hoist_approval;
use crate::serve::rerender_stream::{
  PreparedRerenders, prepare_rerender_views_where,
  stream_empty_rerender, stream_prepared_rerenders };
use crate::serve::protocol::TcpToClient;
use crate::serve::util::{
  send_response_with_length_prefix, tag_sexp_response, tag_text_response,
  value_from_request_sexp };
use crate::sexp_string::quote_sexp_string;
use crate::skgrepo_sets::SkgrepoRestriction;
use crate::types::env::SkgEnv;
use crate::types::misc::ID;
use crate::types::tree::forest::ViewForest;
use crate::types::views_state::pids_from_viewforest;
use crate::update_buffer::RerenderOptions;
use crate::serve::ViewsState;

use futures::executor::block_on;
use std::collections::HashSet;
use std::net::TcpStream;

pub fn handle_delete_references_to_absent_node_request (
  stream : &mut TcpStream,
  request : &str,
  env : &mut SkgEnv,
  views_state : &mut ViewsState,
  skgrepo_restriction : &SkgrepoRestriction,
) {
  let raw_skgid = match value_from_request_sexp ("id", request) {
    Ok (skgid) => ID::from (skgid),
    Err (e) => return refuse (stream, &e), };
  // Preview validation and the resulting rewrite are one mutation.  Taking
  // the gate before the preview prevents an approved token from becoming
  // stale again between its check and the filesystem write and graph swap-in.
  let mutation_gate = env . mutation_gate ();
  let _mutation_guard = block_on (mutation_gate . lock ());
  let runtime = env . runtime_snapshot ();
  let working_graph = new_handle ((*runtime . graph) . clone ());
  let graph = working_graph . load_full ();
  let current = match preview (&graph, &runtime . config, &raw_skgid) {
    Ok (preview) => preview,
    Err (e) => return refuse (stream, &e), };
  let approved = value_from_request_sexp ("approved-preview", request) . ok ();
  if let Some (approved) = approved . as_deref () {
    if approved != current . opaque_approval () {
      return refuse (stream, "Cleanup preview is stale; rescan before rewriting."); }}
  if ! current . links . is_empty () && approved . is_none () {
    let content = preview_warning_org (&current);
    let response = format! (
      "((id {}) (approved-preview {}) (content {}) (prompt \"Remove the structured references?\"))",
      quote_sexp_string (&raw_skgid . 0),
      quote_sexp_string (&current . opaque_approval ()),
      quote_sexp_string (&content));
    send_response_with_length_prefix (
      stream, &tag_sexp_response (
        TcpToClient::DeleteReferencesConfirmation, &response));
    stream_empty_rerender (stream);
    return; }
  let writes = match rewrite (&graph, &runtime . config, &current) {
    Ok (writes) => writes,
    Err (e) => return refuse (stream, &e), };
  if ! writes . is_empty () {
    if let Err (e) = update_graph_minus_nodeMerges_with_hoist_approval (
      writes, &[], (*runtime . config) . clone (), &runtime . tantivy_index,
      &working_graph, &HashSet::new () )
    { return refuse (stream, &e . to_string ()); }
    env . runtime . swap_in (
      runtime . config . clone (), working_graph . load_full (),
      runtime . tantivy_index . clone ()); }
  drop (_mutation_guard);
  let result = result_org (&current);
  let response = format! (
    "((content {}) (changed-nodes {}) (changed-memberships {}))",
    quote_sexp_string (&result), current . changed_nodes (), current . structural . len ());
  send_response_with_length_prefix (
    stream, &tag_sexp_response (TcpToClient::DeleteReferencesResult, &response));
  let affected_recorder_pids : HashSet<ID> = current . structural . iter ()
    . map (|occurrence| occurrence . recorder_pid . clone ()) . collect ();
  stream_rerender_views_after_absent_reference_cleanup (
    stream, env, views_state, skgrepo_restriction, &raw_skgid, &affected_recorder_pids );
}

fn refuse (stream : &mut TcpStream, error : &str) {
  send_response_with_length_prefix (
    stream, &tag_text_response (TcpToClient::Error, error));
  stream_empty_rerender (stream);
}

/// The absent-reference command only removes relationships to an ID freshly proven to
/// have no node.  It cannot introduce title/body text into any view, so this
/// narrowly scoped post-skgsave-commit rerender bypasses the text-release challenge.
fn stream_rerender_views_after_absent_reference_cleanup (
  stream     : &mut TcpStream,
  env        : &SkgEnv,
  views_state : &mut ViewsState,
  skgrepo_restriction : &SkgrepoRestriction,
  raw_skgid     : &crate::types::misc::ID,
  affected_recorder_pids : &HashSet<crate::types::misc::ID>,
) {
  let prepared : PreparedRerenders = prepare_rerender_views_where (
    env, env . runtime_snapshot (), views_state,
    Some (skgrepo_restriction), None, RerenderOptions::ordinary,
    |_, state| view_can_display_absent_reference_change (
      &state . viewforest, raw_skgid, affected_recorder_pids ));
  stream_prepared_rerenders (stream, views_state, prepared);
}

/// A cleanup changes an open view only when it currently displays the exact
/// dangling member, or when it displays a graph-member recorder whose relationship
/// list was rewritten.  `pids_from_viewforest` intentionally excludes Unknown,
/// so inspect both representations rather than introducing a new view index.
fn view_can_display_absent_reference_change (
  viewforest             : &ViewForest,
  raw_skgid              : &crate::types::misc::ID,
  affected_recorder_pids : &HashSet<crate::types::misc::ID>,
) -> bool {
  pids_from_viewforest (viewforest) . iter ()
    .any (|pid| affected_recorder_pids . contains (pid))
  || viewforest . nodes () . any (|node| matches! (
       &node . value () . kind,
       crate::types::viewnode::ViewnodeKind::Vognode (
         crate::types::viewnode::Vognode::Unknown (unknown))
       if unknown . skgid == *raw_skgid ))
}

#[cfg(test)]
mod tests {
  use super::view_can_display_absent_reference_change;
  use crate::types::misc::{ID, SkgrepoName};
  use crate::types::tree::forest::ViewForest;
  use crate::types::viewnode::{mk_editable_viewnode, mk_unknown_viewnode};
  use std::collections::HashSet;

  fn skgid (text : &str) -> ID { ID::from (text) }

  fn unrestricted_view (pid : &str) -> ViewForest {
    let mut view : ViewForest = ViewForest::new ();
    view . append_root (mk_editable_viewnode (
      skgid (pid), SkgrepoName::from ("main"), pid . to_string (), None ));
    view
  }

  fn view_with_unknown (raw_skgid : &str) -> ViewForest {
    let mut view : ViewForest = unrestricted_view ("unrelated-owner");
    let root = view . first_root () . unwrap () . id ();
    view . get_mut (root) . unwrap () . append (mk_unknown_viewnode (skgid (raw_skgid)));
    view
  }

  #[test]
  fn absent_reference_cleanup_selects_only_affected_recorder_or_raw_unknown () {
    let recorders : HashSet<ID> = HashSet::from ([skgid ("changed-owner")]);
    assert! (view_can_display_absent_reference_change (
      &unrestricted_view ("changed-owner"), &skgid ("gone"), &recorders),
      "an open recorder can display its rewritten relationship" );
    assert! (view_can_display_absent_reference_change (
      &view_with_unknown ("gone"), &skgid ("gone"), &recorders),
      "Unknowns are absent from the PID index but still need removal" );
    assert! (! view_can_display_absent_reference_change (
      &unrestricted_view ("unrelated-owner"), &skgid ("gone"), &recorders),
      "an unrelated view must receive neither a lock nor a replacement" );
  }
}
