use crate::dbs::tantivy::titles_by_ids;
use crate::dbs::in_rust_graph::{InRustGraph, snapshot_global};
use crate::serve::handlers::scalar_release::{
  ScalarReleaseDecision,
  approved_pids_from_request,
  challenge_response,
  decide_for_ugly_pids};
use crate::serve::handlers::save_buffer::compute_diff_for_every_source;
use crate::serve::protocol::TcpToClient;
use crate::serve::util::send_response_with_length_prefix;
use crate::source_sets::{
  ActiveSourceSet,
  SourceSetName,
  titles_for_source_set_for_test,
};
use crate::types::env::find_source_with_optional_tantivy;
use crate::types::git::SourceDiff;
use crate::types::misc::{ID, SourceName, SkgConfig, TantivyIndex};
use crate::types::sexp::extract_string_list_from_sexp;

use sexp::{Sexp, Atom};
use std::collections::{HashMap, HashSet};
use std::net::TcpStream;
use std::sync::Arc;

pub fn titles_by_ids_for_source_set_for_test (
  config : &SkgConfig,
  active : &ActiveSourceSet,
  ids    : &[ID],
) -> Result<HashMap<ID, String>, Box<dyn std::error::Error>> {
  titles_for_source_set_for_test (config, active, ids) }

/// Handle a "titles by ids" request from Emacs.
/// Parses the ID list, performs a bulk Tantivy lookup, supplements
/// deleted-node titles from git diff data when needed,
/// and returns an alist of (id . title) pairs.
pub fn handle_titles_by_ids_request (
  stream            : &mut TcpStream,
  request           : &str,
  tantivy_index     : &TantivyIndex,
  config            : &SkgConfig,
  diff_mode_enabled : bool,
) {
  let active : ActiveSourceSet =
    ActiveSourceSet::named (
      config,
      SourceSetName::from ("all"))
    . expect ("reserved source-set all should always resolve");
  let graph : Arc<InRustGraph> =
    snapshot_global () . unwrap_or_else (
      || Arc::new (InRustGraph::new ()) );
  handle_titles_by_ids_request_with_source_set (
    stream, request, tantivy_index, config,
    diff_mode_enabled, &active, &graph ) }

pub fn handle_titles_by_ids_request_with_source_set (
  stream            : &mut TcpStream,
  request           : &str,
  tantivy_index     : &TantivyIndex,
  config            : &SkgConfig,
  diff_mode_enabled : bool,
  active            : &ActiveSourceSet,
  graph             : &InRustGraph,
) {
  let parsed : Sexp =
    match sexp::parse (request) {
      Ok (s) => s,
      Err (e) => {
        tracing::error! (
          "titles_by_ids: failed to parse request: {}", e );
        send_error_response (stream, &format! (
          "Failed to parse request: {}", e ));
        return; } };
  let id_strings : Vec<String> =
    match extract_string_list_from_sexp (&parsed, "ids") {
      Ok (ids) => ids,
      Err (e) => {
        tracing::error! (
          "titles_by_ids: failed to extract ids: {}", e );
        send_error_response (stream, &format! (
          "Failed to extract ids: {}", e ));
        return; } };
  let ids : Vec<ID> =
    id_strings . into_iter ()
    . map (ID)
    . collect ();
  let mut title_map : HashMap<ID, String> =
    titles_by_ids (tantivy_index, &ids);
  let source_diffs : Option<HashMap<SourceName, SourceDiff>> =
    if diff_mode_enabled || title_map . len () < ids . len () {
      Some (compute_diff_for_every_source (config))
    } else { None };
  if let Some (source_diffs) = &source_diffs {
    add_addedNode_titles_by_ids (
      &mut title_map, &ids, source_diffs );
    add_deleted_node_titles_by_ids (
      &mut title_map, &ids, source_diffs ); }
  title_map . retain ( |id, _| {
    if active . is_all () {
      true
    } else {
      let deleted_since_head_pid_src_map : HashMap<ID, SourceName> =
        HashMap::new ();
      find_source_with_optional_tantivy (
        id, &deleted_since_head_pid_src_map,
        Some (tantivy_index), config )
      . map ( |source| active . contains_source (&source) )
      . unwrap_or (false) } } );
  let requested : HashSet<ID> = ids . iter () . cloned () . collect ();
  let mut ugly_pids : Vec<ID> = ids . iter ()
    . filter_map ( |id| graph . pid_of (id) )
    . filter ( |pid| graph . get (pid)
      . map ( |node| node . ugly_telescope )
      . unwrap_or (false) )
    . collect ();
  if let Some (source_diffs) = &source_diffs {
    for source_diff in source_diffs . values () {
      for node in source_diff . added_nodes . values ()
        . chain (source_diff . deleted_nodes . values ()) {
        if node . ugly_telescope
           && node . all_ids () . any ( |id| requested . contains (id) ) {
          ugly_pids . push (node . pid . clone ()); }}}}
  let release = decide_for_ugly_pids (
    "titles-by-ids", active, ugly_pids,
    &approved_pids_from_request (request) );
  if matches! (release, ScalarReleaseDecision::Challenge { .. }) {
    send_response_with_length_prefix (
      stream, &challenge_response (&release) . unwrap () );
    return; }
  let warnings : Vec<String> = match release {
    ScalarReleaseDecision::AllowWithWarning { warning } => vec! [warning],
    _ => Vec::new (), };
  let content_pairs : Vec<String> =
    title_map . iter ()
    . map ( |(id, title)|
      format! (
        "({} . {})",
        elisp_string_literal (id . as_str ()),
        elisp_string_literal (title)) )
    . collect ();
  let response : String =
    format! (
      "((response-type {}) (content ({})) (warnings ({})))",
      TcpToClient::TitlesByIds . repr_in_client (),
      content_pairs . join (" "),
      warnings . iter ()
      . map ( |warning| elisp_string_literal (warning) )
      . collect::<Vec<String>> () . join (" ") );
  send_response_with_length_prefix (stream, &response); }

fn elisp_string_literal (
  s : &str,
) -> String {
  let mut result : String =
    String::from ("\"");
  for ch in s . chars () {
    match ch {
      '\\' => result . push_str ("\\\\"),
      '"'  => result . push_str ("\\\""),
      '\n' => result . push_str ("\\n"),
      '\r' => result . push_str ("\\r"),
      '\t' => result . push_str ("\\t"),
      _    => result . push (ch), }}
  result . push ('"');
  result }

pub fn add_deleted_node_titles_by_ids (
  title_map    : &mut HashMap<ID, String>,
  ids          : &[ID],
  source_diffs : &HashMap<SourceName, SourceDiff>,
) {
  let requested_ids : HashSet<ID> =
    ids . iter () . cloned () . collect ();
  for source_diff in source_diffs . values () {
    for node in source_diff . deleted_nodes . values () {
      for id in node . all_ids () {
        if requested_ids . contains (id) {
          title_map
            . entry (id . clone ())
            . or_insert_with (|| node . title . clone ()); }}} }}

pub fn add_addedNode_titles_by_ids (
  title_map    : &mut HashMap<ID, String>,
  ids          : &[ID],
  source_diffs : &HashMap<SourceName, SourceDiff>,
) {
  let requested_ids : HashSet<ID> =
    ids . iter () . cloned () . collect ();
  for source_diff in source_diffs . values () {
    for node in source_diff . added_nodes . values () {
      for id in node . all_ids () {
        if requested_ids . contains (id) {
          title_map
            . entry (id . clone ())
            . or_insert_with (|| node . title . clone ()); }}} }}

fn send_error_response (
  stream : &mut TcpStream,
  msg    : &str,
) {
  let response : String =
    Sexp::List ( vec! [
      Sexp::List ( vec! [
        Sexp::Atom ( Atom::S (
          "response-type" . to_string () )),
        Sexp::Atom ( Atom::S (
          TcpToClient::Error
          . repr_in_client () . to_string () )), ] ),
      Sexp::List ( vec! [
        Sexp::Atom ( Atom::S (
          "content" . to_string () )),
        Sexp::Atom ( Atom::S (
          msg . to_string () )), ] ),
    ] ) . to_string ();
  send_response_with_length_prefix (stream, &response); }
