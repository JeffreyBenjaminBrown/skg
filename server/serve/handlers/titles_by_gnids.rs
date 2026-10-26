use crate::dbs::tantivy::titles_by_gnids;
use crate::dbs::in_rust_graph::InRustGraph;
use crate::serve::handlers::text_release::{
  TextReleaseDecision,
  approved_pids_from_request,
  challenge_response,
  decide_for_overPrivateText_pids};
use crate::serve::handlers::save_buffer::compute_diff_for_every_skgrepo;
use crate::serve::protocol::TcpToClient;
use crate::serve::util::{send_response_with_length_prefix, sexp_to_client_string};
use crate::sexp_string::quote_sexp_string;
use crate::skgrepo_sets::{
  SkgrepoRestriction,
  SkgrepoSetName,
  titles_for_skgrepo_set_for_test,
};
use crate::types::disk_home::home_from_disk;
use crate::types::git::SkgrepoDiff;
use crate::types::misc::{ID, SkgrepoName, SkgConfig, TantivyIndex};
use crate::types::sexp::extract_string_list_from_sexp;

use sexp::{Sexp, Atom};
use std::collections::{HashMap, HashSet};
use std::net::TcpStream;

pub fn titles_by_gnids_for_skgrepo_set_for_test (
  config : &SkgConfig,
  restriction : &SkgrepoRestriction,
  gnids    : &[ID],
) -> Result<HashMap<ID, String>, Box<dyn std::error::Error>> {
  titles_for_skgrepo_set_for_test (config, restriction, gnids) }

/// Handle a "titles by ids" request from Emacs.
/// Parses the ID list, performs a bulk Tantivy lookup, supplements
/// deleted-node titles from git diff data when needed,
/// and returns an alist of (id . title) pairs.
pub fn handle_titles_by_gnids_request (
  stream            : &mut TcpStream,
  request           : &str,
  graph             : &InRustGraph,
  tantivy_index     : &TantivyIndex,
  config            : &SkgConfig,
) {
  let restriction : SkgrepoRestriction =
    SkgrepoRestriction::named (
      config,
      SkgrepoSetName::from ("all"))
    . expect ("reserved repo-set all should always resolve");
  handle_titles_by_gnids_request_with_skgrepo_set (
    stream, request, tantivy_index, config,
    &restriction, graph ) }

pub fn handle_titles_by_gnids_request_with_skgrepo_set (
  stream            : &mut TcpStream,
  request           : &str,
  tantivy_index     : &TantivyIndex,
  config            : &SkgConfig,
  restriction       : &SkgrepoRestriction,
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
      Ok (gnids) => gnids,
      Err (e) => {
        tracing::error! (
          "titles_by_ids: failed to extract ids: {}", e );
        send_error_response (stream, &format! (
          "Failed to extract ids: {}", e ));
        return; } };
  let gnids : Vec<ID> =
    id_strings . into_iter ()
    . map (ID)
    . collect ();
  let mut title_map : HashMap<ID, String> =
    titles_by_gnids (tantivy_index, &gnids);
  let skgrepo_diffs : Option<HashMap<SkgrepoName, SkgrepoDiff>> =
    if title_map . len () < gnids . len () {
      Some (compute_diff_for_every_skgrepo (config))
    } else { None };
  if let Some (skgrepo_diffs) = &skgrepo_diffs {
    add_addedNode_titles_by_gnids (
      &mut title_map, &gnids, skgrepo_diffs );
    add_deleted_node_titles_by_gnids (
      &mut title_map, &gnids, skgrepo_diffs ); }
  title_map . retain ( |gnid, _| {
    if restriction . is_all () {
      true
    } else {
      graph . pid_and_skgrepo (gnid) . map (|(_, skgrepo)| skgrepo)
      . or_else (|| crate::dbs::tantivy::title_and_skgrepo_by_gnid (
        tantivy_index, gnid ) . map (|(_, skgrepo)| skgrepo))
      . or_else (|| home_from_disk (gnid, config))
      . map ( |skgrepo| restriction . contains_skgrepo (&skgrepo) )
      . unwrap_or (false) } } );
  let requested : HashSet<ID> = gnids . iter () . cloned () . collect ();
  let mut overPrivateText_pids : Vec<ID> = gnids . iter ()
    . filter_map ( |gnid| graph . pid_of (gnid) )
    . filter ( |pid| graph . get (pid)
      . map ( |node| node . overPrivateText_telescope )
      . unwrap_or (false) )
    . collect ();
  if let Some (skgrepo_diffs) = &skgrepo_diffs {
    for skgrepo_diff in skgrepo_diffs . values () {
      for node in skgrepo_diff . added_nodes . values ()
        . chain (skgrepo_diff . deleted_nodes . values ()) {
        if node . overPrivateText_telescope
           && node . all_gnids () . any ( |gnid| requested . contains (gnid) ) {
          overPrivateText_pids . push (node . pid . clone ()); }}}}
  let release = decide_for_overPrivateText_pids (
    "titles-by-ids", restriction, overPrivateText_pids,
    &approved_pids_from_request (request) );
  if matches! (release, TextReleaseDecision::Challenge { .. }) {
    send_response_with_length_prefix (
      stream, &challenge_response (&release) . unwrap () );
    return; }
  let warnings : Vec<String> = match release {
    TextReleaseDecision::AllowWithWarning { warning } => vec! [warning],
    _ => Vec::new (), };
  let content_pairs : Vec<String> =
    title_map . iter ()
    . map ( |(gnid, title)|
      format! (
        "({} . {})",
        quote_sexp_string (gnid . as_str ()),
        quote_sexp_string (title)) )
    . collect ();
  let response : String =
    format! (
      "((response-type {}) (content ({})) (warnings ({})))",
      TcpToClient::TitlesByIds . repr_in_client (),
      content_pairs . join (" "),
      warnings . iter ()
      . map ( |warning| quote_sexp_string (warning) )
      . collect::<Vec<String>> () . join (" ") );
  send_response_with_length_prefix (stream, &response); }

pub fn add_deleted_node_titles_by_gnids (
  title_map     : &mut HashMap<ID, String>,
  gnids           : &[ID],
  skgrepo_diffs : &HashMap<SkgrepoName, SkgrepoDiff>,
) {
  let requested_gnids : HashSet<ID> =
    gnids . iter () . cloned () . collect ();
  for skgrepo_diff in skgrepo_diffs . values () {
    for node in skgrepo_diff . deleted_nodes . values () {
      for gnid in node . all_gnids () {
        if requested_gnids . contains (gnid) {
          title_map
            . entry (gnid . clone ())
            . or_insert_with (|| node . title . clone ()); }}} }}

pub fn add_addedNode_titles_by_gnids (
  title_map     : &mut HashMap<ID, String>,
  gnids           : &[ID],
  skgrepo_diffs : &HashMap<SkgrepoName, SkgrepoDiff>,
) {
  let requested_gnids : HashSet<ID> =
    gnids . iter () . cloned () . collect ();
  for skgrepo_diff in skgrepo_diffs . values () {
    for node in skgrepo_diff . added_nodes . values () {
      for gnid in node . all_gnids () {
        if requested_gnids . contains (gnid) {
          title_map
            . entry (gnid . clone ())
            . or_insert_with (|| node . title . clone ()); }}} }}

fn send_error_response (
  stream : &mut TcpStream,
  msg    : &str,
) {
  let response : String =
    sexp_to_client_string ( &Sexp::List ( vec! [
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
    ] ));
  send_response_with_length_prefix (stream, &response); }
