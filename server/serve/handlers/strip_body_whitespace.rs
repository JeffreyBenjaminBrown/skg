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
use crate::runtime::save_operations::SaveOperation;
use crate::runtime::ServerRuntime;
use crate::serve::protocol::TcpToClient;
use crate::types::env::SkgEnv;
use crate::types::misc::SourceName;
use crate::types::nodes::complete::NodeComplete;
use crate::types::save::{DefineNode, SaveNode};
use crate::types::store_state::SelectedStoreState;

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
  stream : &mut TcpStream,
  request : &str,
  runtime : &ServerRuntime,
) {
  super::durable_command::handle_durable_command_request (
    stream, request, runtime, TcpToClient::StripBodyWhitespace,
    strip_body_whitespace_with_operation);
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
