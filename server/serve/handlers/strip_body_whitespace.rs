//! The "strip body whitespace" request (TODO/fork-fixes.org): strips
//! trailing whitespace from every line of every body (and trailing
//! blank lines from the body's tail), in every source named in the
//! config -- foreign sources included, since the request asks for
//! every repo -- rewriting only the .skg files whose bodies changed. Bodies also live in two derived stores, the in-Rust graph
//! and the Tantivy index; both are refreshed here. TypeDB is
//! untouched: it stores no body text, and the textlinks it derives
//! from bodies cannot be changed by stripping trailing whitespace.

use crate::dbs::filesystem::multiple_nodes::read_all_skg_files_from_sources;
use crate::dbs::filesystem::one_node::write_nodecomplete_to_source;
use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::tantivy::write::update_index_with_nodes;
use crate::serve::protocol::TcpToClient;
use crate::serve::util::{send_response_with_length_prefix, tag_text_response};
use crate::types::env::SkgEnv;
use crate::types::misc::{SkgConfig, SourceName};
use crate::types::nodes::complete::NodeComplete;
use crate::types::nodes::tantivy::NodeTantivy;

use std::collections::BTreeMap;
use std::net::TcpStream;
use std::sync::Arc;

pub fn handle_strip_body_whitespace_request (
  stream : &mut TcpStream,
  env    : &mut SkgEnv,
) {
  tracing::info!("Stripping trailing whitespace from bodies...");
  let msg : String =
    match strip_body_whitespace_and_refresh_caches (env) {
      Ok (report) => report,
      Err (e) => {
        tracing::error!("Body whitespace strip failed: {}", e);
        format! ("Body whitespace strip failed: {}", e) }};
  send_response_with_length_prefix (
    stream,
    & tag_text_response (
      TcpToClient::StripBodyWhitespace, &msg )); }

/// Strips on disk, then refreshes the two body-holding caches: the
/// in-Rust graph (rebuilt whole, from the nodes already in hand) and
/// the Tantivy index (per changed node, body search would otherwise
/// match stale text).
fn strip_body_whitespace_and_refresh_caches (
  env : &mut SkgEnv,
) -> Result<String, String> {
  let (all_nodes, changed) : (Vec<NodeComplete>, Vec<NodeComplete>) =
    strip_body_whitespace_on_disk (& env . config) ?;
  if changed . is_empty () {
    return Ok ( format! (
      "No body has trailing whitespace ({} files checked).",
      all_nodes . len () )); }
  env . in_rust_graph . store (
    Arc::new ( InRustGraph::from_nodecompletes (&all_nodes) ));
  { let tantivy_nodes : Vec<NodeTantivy> =
      changed . iter () . map (NodeTantivy::from) . collect ();
    update_index_with_nodes (&tantivy_nodes, & env . tantivy_index)
      . map_err ( |e| format! ("Tantivy update failed: {}", e) ) ?; }
  let breakdown : String = {
    // BTreeMap so the report lists sources in a stable order.
    let mut counts : BTreeMap<SourceName, usize> = BTreeMap::new ();
    for node in &changed {
      * counts . entry ( node . source . clone () ) . or_insert (0)
        += 1; }
    counts . iter ()
      . map ( |(source, n)| format! ("{}: {}", source, n) )
      . collect::<Vec<String>> ()
      . join (", ") };
  Ok ( format! (
    "Stripped trailing whitespace from {} of {} files ({}).",
    changed . len (), all_nodes . len (), breakdown )) }

/// Reads every node from every source in the config -- owned and
/// foreign alike -- strips trailing whitespace from each line of each
/// body, and rewrites exactly the files whose bodies changed (a file
/// with a clean body is left byte-identical). A body that strips to
/// the empty string is dropped entirely, so the written file omits
/// the field rather than carrying 'body: ""'. Returns every node
/// read (post-strip, so a whole-cache rebuild sees the new bodies)
/// and separately the changed nodes (for per-node cache updates and
/// the report).
pub fn strip_body_whitespace_on_disk (
  config : &SkgConfig,
) -> Result<(Vec<NodeComplete>, Vec<NodeComplete>), String> {
  let mut all_nodes : Vec<NodeComplete> =
    read_all_skg_files_from_sources (config)
    . map_err ( |e| format! ("Reading .skg files: {}", e) ) ?;
  let mut changed : Vec<NodeComplete> = Vec::new ();
  for node in all_nodes . iter_mut () {
    let Some (body) = & node . body else { continue; };
    let stripped : String =
      strip_trailing_whitespace_from_body (body);
    if stripped == * body { continue; }
    node . body =
      if stripped . is_empty () { None }
      else { Some (stripped) };
    write_nodecomplete_to_source (node, config)
      . map_err ( |e| format! (
        "Writing node {} to source {}: {}",
        node . pid . as_str (), node . source, e) ) ?;
    changed . push ( node . clone () ); }
  Ok (( all_nodes, changed )) }

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
