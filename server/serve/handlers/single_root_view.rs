use crate::serve::ConnectionState;
use crate::to_org::render::content_view::single_root_view;
use crate::serve::timing_log::timed;
use crate::serve::util::{
  view_uri_from_request,
  send_response_with_length_prefix,
  format_buffer_response_sexp,
  tag_sexp_response,
  tag_text_response};
use crate::types::sexp::extract_v_from_kv_pair_in_sexp;
use crate::types::misc::{SkgConfig, ID};
use crate::types::viewnode::ViewUri;

use futures::executor::block_on;
use sexp::{Sexp, Atom};
use std::net::TcpStream; // handles two-way communication
use typedb_driver::TypeDBDriver;

/// Gets a node id from the request,
/// generates an org view of that id's content (recursively),
/// and sends the response to Emacs (length-prefixed).
/// Response format: ((content "...") (errors ("error1" "error2" ...)))
/// If the requested ID is already a root of an open view,
/// returns ((switch-to-view "VIEW_URI")) instead of rendering.
pub fn handle_single_root_view_request (
  stream        : &mut TcpStream,
  request       : &str,
  typedb_driver : &TypeDBDriver,
  config        : &SkgConfig,
  conn_state    : &mut ConnectionState,
) {
  let view_uri_result : Result<ViewUri, String> =
    view_uri_from_request (request);
  match node_id_from_single_root_view_request (request) {
    Ok (node_id) => {
      if let Some (existing_uri)
        = conn_state . memory . view_uri_for_root_id ( &node_id )
      { let switch_sexp : String =
          Sexp::List ( vec! [
            Sexp::List ( vec! [
              Sexp::Atom ( Atom::S (
                "switch-to-view" . to_string () )),
              Sexp::Atom ( Atom::S (
                existing_uri . 0 . clone () )) ] ) ] )
          . to_string ();
        send_response_with_length_prefix (
          stream,
          & tag_sexp_response (
            "content-view", &switch_sexp ));
        return; }
      let response_sexp : String =
        timed ( config, "single_root_view", || {
          block_on ( async {
            match single_root_view (
              typedb_driver,
              config,
              &node_id,
              conn_state . diff_mode_enabled ) . await
            { Ok ( (buffer_content, map, pids, forest) ) => {
                if let Ok (view_uri) = &view_uri_result {
                  for (pid, skgnode) in map {
                    conn_state . memory . pool . insert (
                      pid, skgnode ); }
                  conn_state . memory . register_view (
                    view_uri . clone (),
                    forest,
                    &pids ); }
                format_buffer_response_sexp (
                  & buffer_content,
                  & vec![] ) },
              Err (e) => { // If we fail to generate the view, return error in content
                let error_content : String = format!(
                  "Error generating document: {}", e);
                format_buffer_response_sexp (
                  & error_content,
                  & vec![] ) }} } ) } );
      send_response_with_length_prefix (
        stream,
        & tag_sexp_response (
          "content-view", &response_sexp )); },
    Err (err) => {
      let error_msg : String = format!(
        "Error extracting node ID: {}", err);
      println! ( "{}", error_msg ) ;
      send_response_with_length_prefix (
        stream,
        & tag_text_response (
          "content-view", &error_msg )); } } }

pub fn node_id_from_single_root_view_request (
  request : &str
) -> Result<ID, String> {
  extract_v_from_kv_pair_in_sexp (
    & { let sexp : Sexp =
          sexp::parse (request)
          . map_err ( |e| format! (
            "Failed to parse S-expression: {}", e ) ) ?;
        sexp },
    "id"
  ) . map (ID) }
