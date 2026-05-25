use crate::dbs::in_rust_graph::scheduled_audit::take_pending_audit_warning;
use crate::types::env::SkgEnv;
use crate::serve::ViewsState;
use crate::to_org::render::content_view::multi_root_view_with_source_set;
use crate::serve::protocol::TcpToClient;
use crate::serve::util::{
  view_uri_from_request,
  send_response_with_length_prefix,
  format_buffer_response_sexp,
  tag_sexp_response,
  tag_text_response};
use crate::types::sexp::extract_v_from_kv_pair_in_sexp;
use crate::types::misc::ID;
use crate::source_sets::ActiveSourceSet;
use crate::types::views_state::ViewUri;

use futures::executor::block_on;
use sexp::{Sexp, Atom};
use std::net::TcpStream; // handles two-way communication

/// Gets a node id from the request,
/// generates an org view of that id's content (recursively),
/// and sends the response to Emacs (length-prefixed).
/// Response format: ((content "...") (errors ("error1" "error2" ...)))
/// If the requested ID is already a root of an open view,
/// returns ((switch-to-view "VIEW_URI")) instead of rendering.
pub fn handle_single_root_view_request (
  stream     : &mut TcpStream,
  request    : &str,
  env        : &SkgEnv,
  views_state : &mut ViewsState,
  active_source_set : &ActiveSourceSet,
) {
  let view_uri_result : Result<ViewUri, String> =
    view_uri_from_request (request);
  match node_id_from_single_root_view_request (request) {
    Ok (node_id) => {
      match active_source_set . id_source_is_active (
        &env . config, &node_id ) {
        Ok (true) => {},
        Ok (false) => {
          let response_sexp : String =
            format_buffer_response_sexp (
              &String::new (),
              &vec! [format! (
                "Node {} is not in active source-set {}",
                node_id,
                active_source_set . name )] );
          send_response_with_length_prefix (
            stream,
            & tag_sexp_response (
              TcpToClient::ContentView, &response_sexp ));
          return; },
        Err (e) => {
          let response_sexp : String =
            format_buffer_response_sexp (
              &String::new (),
              &vec! [format! (
                "Error checking source-set visibility: {}", e )] );
          send_response_with_length_prefix (
            stream,
            & tag_sexp_response (
              TcpToClient::ContentView, &response_sexp ));
          return; }}
      if let Some (existing_uri)
        = views_state . open_views
          . content_view_uri_for_root_id ( &node_id )
        { let switch_sexp : String =
            Sexp::List ( vec! [
              Sexp::List ( vec! [
                Sexp::Atom ( Atom::S (
                  "switch-to-view" . to_string () )),
                Sexp::Atom ( Atom::S (
                  existing_uri . repr_in_client () )) ] ) ] )
            . to_string ();
          send_response_with_length_prefix (
            stream,
            & tag_sexp_response (
              TcpToClient::ContentView, &switch_sexp ));
          return; }
      let response_sexp : String =
      { let _span : tracing::span::EnteredSpan =
          tracing::info_span!( "single_root_view" ). entered();
        block_on ( async {
            match multi_root_view_with_source_set (
              &env . driver,
              &env . config,
              Some (&env . tantivy_index),
              &[node_id . clone ()],
              views_state . diff_mode_enabled,
              active_source_set ) . await
            { Ok ( (buffer_content, pids, viewforest) ) => {
                if let Ok (view_uri) = &view_uri_result {
                  views_state . open_views . register_view (
                    view_uri . clone (),
                    viewforest,
                    &pids ); }
                let errors : Vec<String> =
                  take_pending_audit_warning ()
                    . map ( |w| vec! [w] )
                    . unwrap_or_default ();
                format_buffer_response_sexp (
                  & buffer_content,
                  & errors ) },
              Err (e) => { // If we fail to generate the view, ship the generation error (and any pending audit warning) in the errors vec, with empty content so the client skips opening a main buffer.
                let mut errors : Vec<String> = Vec::new ();
                if let Some (w) = take_pending_audit_warning ()
                { errors . push (w); }
                errors . push ( format! (
                  "Error generating document: {}", e ));
                format_buffer_response_sexp (
                  & String::new (),
                  & errors ) }} } ) };
      send_response_with_length_prefix (
        stream,
        & tag_sexp_response (
          TcpToClient::ContentView, &response_sexp )); },
    Err (err) => {
      let error_msg : String = format!(
        "Error extracting node ID: {}", err);
      tracing::error! ( "{}", error_msg ) ;
      send_response_with_length_prefix (
        stream,
        & tag_text_response (
          TcpToClient::ContentView, &error_msg )); } } }

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
