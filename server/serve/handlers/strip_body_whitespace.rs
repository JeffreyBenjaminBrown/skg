//! The "strip body whitespace" request (TODO/fork-fixes.org): strips
//! trailing whitespace from every line of every body (and trailing
//! blank lines from the body's tail), in every OWNED source in the
//! config -- foreign sources are read-only, and stripping them would
//! make them diverge from their upstreams (Jeff settled on owned
//! only) -- rewriting only the .skg files whose bodies changed. The
//! selected graph and Tantivy index are refreshed through the ordinary
//! save batch.

#[cfg(test)]
use crate::dbs::filesystem::multiple_nodes::{
  LoadedCorpus,
  read_all_skg_files_with_manifest,
};
use crate::save::{nodecompletes_from_graph,
                  update_graph_including_nodeMerges_with_operation};
use crate::maintenance::save_journal::SaveOperationStatus;
use crate::runtime::save_operations::SaveOperation;
use crate::runtime::{SelectedRuntimeSnapshot, ServerRuntime};
use crate::serve::protocol::TcpToClient;
use crate::serve::util::{send_response_with_length_prefix,
                         tag_terminal_text_response,
                         tag_text_response};
use crate::types::env::SkgEnv;
use crate::types::misc::SourceName;
use crate::types::nodes::complete::NodeComplete;
use crate::types::save::{DefineNode, SaveNode};
use crate::types::store_state::SelectedStoreState;
use crate::source_sets::ActiveSourceSet;

#[cfg(test)]
use crate::dbs::filesystem::one_node::prepare_nodecomplete_telescope;
#[cfg(test)]
use crate::types::misc::SkgConfig;
#[cfg(test)]
use crate::types::store_state::SelectedPathManifest;
use std::collections::{BTreeMap, HashSet};
use std::sync::Arc;
use std::net::TcpStream;
use futures::executor::block_on;

pub(crate) fn strip_body_whitespace_with_operation (
  env       : &mut SkgEnv,
  operation : &SaveOperation,
) -> Result<String, String> {
  tracing::info!("Stripping trailing whitespace from bodies...");
  let selected : Arc<SelectedStoreState> = env . in_rust_graph . load_full ();
  let all_nodes : Vec<NodeComplete> = nodecompletes_from_graph (
    &selected . graph);
  let owned_checked : usize = all_nodes . iter ()
    . filter (|node| env . config . user_owns_source (&node . source))
    . count ();
  let mut changed : Vec<NodeComplete> = Vec::new ();
  let mut definitions : Vec<DefineNode> = Vec::new ();
  for node in all_nodes . iter () {
    if ! env . config . user_owns_source (&node . source) { continue; }
    let Some (body) = &node . body else { continue; };
    let stripped : String = strip_trailing_whitespace_from_body (body);
    if stripped == *body { continue; }
    let mut updated : NodeComplete = node . clone ();
    updated . body = if stripped . is_empty () {
      None
    } else { Some (stripped) };
    changed . push (updated . clone ());
    definitions . push (DefineNode::Save (SaveNode (updated)));
  }
  if definitions . is_empty () {
    operation . prepare (Vec::new ())?;
    operation . apply_authorized ()?;
  } else {
    block_on (update_graph_including_nodeMerges_with_operation (
      definitions,
      &[],
      &[],
      env . config . clone (),
      &mut env . tantivy_index,
      &env . in_rust_graph,
      &HashSet::new (),
      selected . graph_generation . get (),
      Some (operation))) . map_err (|error| error . to_string ())?;
    env . searcher = env . in_rust_graph . load_full () . searcher . clone ()
      . expect ("completed strip has a matching Searcher");
  }
  let breakdown : String = {
    let mut counts : BTreeMap<SourceName, usize> = BTreeMap::new ();
    for node in &changed {
      * counts . entry (node . source . clone ()) . or_insert (0) += 1;
    }
    counts . iter () . map (|(source, count)|
      format! ("{}: {}", source, count)) . collect::<Vec<String>> () . join (", ")
  };
  if changed . is_empty () {
    Ok (format! (
      "No body has trailing whitespace ({} files checked, in owned sources).",
      owned_checked))
  } else {
    Ok (format! (
      "Stripped trailing whitespace from {} of {} files in owned sources ({}).",
      changed . len (), owned_checked, breakdown))
  }
}

pub(crate) fn handle_strip_body_whitespace_request (
  stream  : &mut TcpStream,
  request : &str,
  runtime : &ServerRuntime,
) {
  let snapshot : Arc<SelectedRuntimeSnapshot> = runtime . selected_snapshot ();
  let active : ActiveSourceSet = runtime . interactive
    . lock () . unwrap () . active_source_set . clone ();
  let operation : SaveOperation = match SaveOperation::from_command (
    request, &snapshot . env . config, &active) {
    Ok (operation) => operation,
    Err (reason) => { send_strip_runtime_error (stream, &reason); return; }, };
  match operation . recorded_response () {
    Ok (Some (response)) => {
      let _ = send_response_with_length_prefix (stream, &response);
      return; }
    Err (reason) => {
      let response : String = operation . tag_response (
        &tag_text_response (TcpToClient::StripBodyWhitespace, &reason),
        "blocked");
      let _ = send_response_with_length_prefix (stream, &response);
      return; }
    Ok (None) => {}
  }
  if let Err (reason) = runtime . validate_session_authority (request) {
    let response : String = operation . tag_response (
      &tag_text_response (TcpToClient::StripBodyWhitespace, &reason),
      "refused");
    match operation . refuse (&response) {
      Ok (()) => { let _ = send_response_with_length_prefix (stream, &response); }
      Err (error) => send_strip_runtime_error (stream, &error), }
    return;
  }
  let result : Result<Result<String, String>, String> = runtime . with_store_transition (
    operation . operation_id . clone (), |env, interactive, control| {
      let locked_active : ActiveSourceSet =
        interactive . active_source_set . clone ();
      let locked_operation : SaveOperation = SaveOperation::from_command (
        request, &env . config, &locked_active)?;
      if ! operation . matches_interpretation (&locked_operation) {
        return Err ("selected save interpretation changed before execution" . into ()); }
      let operation : SaveOperation = operation . clone ()
        . with_control (control . clone ());
      match strip_body_whitespace_with_operation (env, &operation) {
        Ok (report) => {
          let response : String = operation . tag_response (
            &tag_text_response (TcpToClient::StripBodyWhitespace, &report),
            "committed");
          let recorded : Result<(), String> = runtime
            . publish_selected_from_env (control, env) . and_then (|_|
              operation . commit (
                &response,
                &format! ("graph-{}-manifest-{}",
                  env . in_rust_graph . load_full () . graph_generation . get (),
                  env . in_rust_graph . load_full () . manifest_revision . get ())));
          match recorded {
            Ok (()) => Ok (response),
            Err (reason) => {
              let _ = control . block (reason . clone ());
              Err (reason)
            }
          }
        }
        Err (reason) => {
          let dispatched : bool = match operation . status () {
            Ok (Some (snapshot)) => matches! (
              snapshot . status,
              SaveOperationStatus::Authorized { .. }
              | SaveOperationStatus::AppliedAwaitingCommit),
            Ok (None) => false,
            Err (_) => true,
          };
          if dispatched { let _ = control . block (reason . clone ()); }
          Err (reason)
        }
      }
    });
  match result {
    Ok (Ok (response)) => {
      let _ = send_response_with_length_prefix (stream, &response); }
    Ok (Err (reason)) | Err (reason) => {
      let can_refuse : bool = match operation . status () {
        Ok (None) => true,
        Ok (Some (snapshot)) => matches! (
          snapshot . status,
          SaveOperationStatus::PreparedUnAuthorized
          | SaveOperationStatus::StagingUnAuthorized),
        Err (_) => false,
      };
      if can_refuse {
        let response : String = operation . tag_response (
          &tag_text_response (TcpToClient::StripBodyWhitespace, &reason),
          "refused");
        match operation . refuse (&response) {
          Ok (()) => { let _ = send_response_with_length_prefix (stream, &response); }
          Err (error) => send_strip_runtime_error (stream, &error), }
      } else { send_strip_runtime_error (stream, &reason); }
    }
  }
}

fn send_strip_runtime_error (
  stream : &mut TcpStream,
  error  : &str,
) {
  let _ = send_response_with_length_prefix (
    stream, &tag_terminal_text_response (TcpToClient::Error, "failed", error));
}

/// Reads every node from every source in the config, then strips
/// trailing whitespace from each line of each OWNED node's body,
/// rewriting exactly the files whose bodies changed (a file with a
/// clean body is left byte-identical). Foreign sources are read (the
/// caller rebuilds whole-graph caches from the returned nodes) but
/// never written: they are read-only, and local edits would make
/// them diverge from their upstreams. A body that strips to the
/// empty string is dropped entirely, so the written file omits the
/// field rather than carrying 'body: ""'. Returns every node read
/// (post-strip) and separately the changed nodes (for per-node cache
/// updates and the report).
#[cfg(test)]
pub fn strip_body_whitespace_on_disk (
  config : &SkgConfig,
) -> Result<(Vec<NodeComplete>, Vec<NodeComplete>), String> {
  let (all, changed, _) = strip_body_whitespace_on_disk_with_manifest (config) ?;
  Ok ((all, changed))
}

#[cfg(test)]
fn strip_body_whitespace_on_disk_with_manifest (
  config : &SkgConfig,
) -> Result<(Vec<NodeComplete>, Vec<NodeComplete>, SelectedPathManifest), String> {
  let loaded : LoadedCorpus = read_all_skg_files_with_manifest (config)
    . map_err ( |e| format! ("Reading .skg files: {}", e) ) ?;
  let mut all_nodes : Vec<NodeComplete> = loaded . nodes;
  let mut manifest : SelectedPathManifest = loaded . manifest;
  let mut changed : Vec<NodeComplete> = Vec::new ();
  for node in all_nodes . iter_mut () {
    if ! config . user_owns_source (& node . source) { continue; }
    let Some (body) = & node . body else { continue; };
    let stripped : String =
      strip_trailing_whitespace_from_body (body);
    if stripped == * body { continue; }
    node . body =
      if stripped . is_empty () { None }
      else { Some (stripped) };
    let prepared = prepare_nodecomplete_telescope (node, config, false)
      . map_err ( |e| format! (
        "Preparing node {} in source {}: {}",
        node . pid . as_str (), node . source, e) ) ?;
    prepared . apply (config)
      . map_err ( |e| format! (
        "Writing node {} to source {}: {}",
        node . pid . as_str (), node . source, e) ) ?;
    prepared . apply_to_manifest (&mut manifest);
    changed . push ( node . clone () ); }
  Ok (( all_nodes, changed, manifest )) }

/// Strips trailing whitespace (spaces, tabs, '\r') from each line,
/// then any trailing newlines from the whole body. Interior empty
/// lines survive; only the body's tail is trimmed. The tail trim
/// matches the canonical on-disk form: the YAML writer emits bodies
/// as strip-chomped block scalars, which cannot carry trailing
/// newlines, so keeping one here would make the returned node (and
/// hence the refreshed caches) disagree with the file just written.
pub fn strip_trailing_whitespace_from_body (
  text : &str,
) -> String {
  text . split ('\n')
    . map (str::trim_end)
    . collect::<Vec<&str>> ()
    . join ("\n")
    . trim_end ()
    . to_string () }

#[cfg(test)]
#[allow(non_snake_case)]
#[path = "../../../tests/unit/strip_body_whitespace.rs"]
mod tests;
