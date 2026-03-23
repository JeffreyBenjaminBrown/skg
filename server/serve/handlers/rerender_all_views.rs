use crate::serve::ConnectionState;
use crate::serve::handlers::save_buffer::{
  compute_diff_for_every_source, deleted_ids_to_source};
use crate::serve::protocol::TcpToClient;
use crate::serve::util::send_response_with_length_prefix;
use crate::types::git::SourceDiff;
use crate::types::misc::{ID, SourceName, SkgConfig};
use crate::types::memory::{SkgNodeMap, ViewUri};
use crate::types::viewnode::ViewNode;
use crate::update_buffer::rerender_view;

use ego_tree::Tree;
use futures::executor::block_on;
use sexp::{Sexp, Atom};
use std::collections::{HashMap, HashSet};
use std::net::TcpStream;
use typedb_driver::TypeDBDriver;

/// Handle a "rerender all views" request.
/// Recomputes every open view (with or without diff annotations,
/// depending on diff_mode_enabled) and sends the results to Emacs.
pub fn handle_rerender_all_views_request (
  stream        : &mut TcpStream,
  typedb_driver : &TypeDBDriver,
  config        : &SkgConfig,
  conn_state    : &mut ConnectionState,
) {
  match block_on (
    rerender_all_views (
      typedb_driver, config, conn_state ))
  { Ok (( views, errors )) => {
      let sexp : String =
        format_rerender_all_views_sexp ( &views, &errors );
      send_response_with_length_prefix (
        stream,
        & tag_sexp_rerender ( &sexp )); }
    Err (e) => {
      let sexp : String =
        format_rerender_all_views_sexp (
          &Vec::new (),
          &vec! [ format! ("rerender_all_views: {}", e) ] );
      send_response_with_length_prefix (
        stream,
        & tag_sexp_rerender ( &sexp )); } } }

async fn rerender_all_views (
  typedb_driver : &TypeDBDriver,
  config        : &SkgConfig,
  conn_state    : &mut ConnectionState,
) -> Result<( Vec<(ViewUri, String)>, Vec<String> ),
            Box<dyn std::error::Error>> {
  let source_diffs
    : Option<HashMap<SourceName, SourceDiff>>
    = if conn_state . diff_mode_enabled
      { Some ( compute_diff_for_every_source (config)) }
      else { None };
  let deleted_since_head_pid_src_map : HashMap<ID, SourceName> =
    source_diffs . as_ref ()
    . map ( |d| deleted_ids_to_source (d))
    . unwrap_or_default ();
  let deleted_by_this_save_pids : HashSet<ID> =
    HashSet::new ();
  let mut views_out : Vec<(ViewUri, String)> = Vec::new ();
  let mut errors    : Vec<String> = Vec::new ();
  let uris : Vec<ViewUri> =
    conn_state . memory . views . keys () . cloned () . collect ();
  for uri in uris {
    let mut forest : Tree<ViewNode> = match
      conn_state . memory . viewuri_to_view (&uri) {
        Some (f) => f . clone (),
        None => {
          errors . push ( format! (
            "View {}: no viewforest found",
            uri . repr_in_client () ));
          continue; } };
    let mut map : SkgNodeMap =
      seed_skgnodemap_from_pool (&uri, conn_state);
    match { let _span : tracing::span::EnteredSpan =
              tracing::info_span! (
                "rerender_view (rerender-all)"
              ) . entered ();
      rerender_view (
        &mut forest, &mut map,
        &source_diffs, config, typedb_driver,
        &mut errors, &deleted_since_head_pid_src_map,
        &deleted_by_this_save_pids,
        false ) . await }
    { Ok (text) => {
        merge_skgnodemap_into_pool (&map, conn_state);
        conn_state . memory . update_view (&uri, forest);
        views_out . push (( uri, text )); },
      Err (e) => {
        errors . push ( format! (
          "View {}: {}",
          uri . repr_in_client (), e )); } } }
  Ok (( views_out, errors )) }

/// Build a SkgNodeMap for a view by pulling every PID
/// in that view's forest from the pool.
fn seed_skgnodemap_from_pool (
  uri        : &ViewUri,
  conn_state : &ConnectionState,
) -> SkgNodeMap {
  let mut it : SkgNodeMap = SkgNodeMap::new ();
  for pid in conn_state . memory . viewuri_to_pids (uri) {
    if let Some (skgnode)
      = conn_state . memory . pool . get (&pid)
      { it . insert ( pid, skgnode . clone () ); } }
  it }

/// Merge a completed SkgNodeMap into the pool.
fn merge_skgnodemap_into_pool (
  map        : &SkgNodeMap,
  conn_state : &mut ConnectionState,
) {
  for (pid, skgnode) in map {
    conn_state . memory . pool . insert (
      pid . clone (), skgnode . clone () ); } }

/// Format: ((views (("URI1" "CONTENT1") ...)) (errors ("e1" ...)))
fn format_rerender_all_views_sexp (
  views  : &[(ViewUri, String)],
  errors : &[String],
) -> String {
  Sexp::List ( vec! [
    Sexp::List ( vec! [
      Sexp::Atom ( Atom::S ( "views" . to_string () )),
      Sexp::List (
        views . iter ()
          . map ( |(uri, content)| Sexp::List ( vec! [
            Sexp::Atom ( Atom::S ( uri . repr_in_client () )),
            Sexp::Atom ( Atom::S ( content . clone () )) ] ) )
          . collect () ) ] ),
    Sexp::List ( vec! [
      Sexp::Atom ( Atom::S ( "errors" . to_string () )),
      Sexp::List (
        errors . iter ()
          . map ( |e| Sexp::Atom ( Atom::S ( e . clone () )) )
          . collect () ) ] ) ] )
    . to_string () }

/// Prepend response-type tag for rerender-all-views.
fn tag_sexp_rerender (
  sexp_payload : &str,
) -> String {
  let tag : String =
    Sexp::List ( vec! [
      Sexp::Atom ( Atom::S ( "response-type" . to_string () )),
      Sexp::Atom ( Atom::S (
        TcpToClient::RerenderAllViews . repr_in_client ()
          . to_string () )),
    ] ) . to_string ();
  format! ( "({} {}", tag, &sexp_payload [ 1.. ] ) }
