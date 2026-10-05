use crate::types::env::SkgEnv;
use crate::serve::ViewsState;
use crate::to_org::render::content_view::multi_root_view_via_runtime;
use crate::serve::protocol::TcpToClient;
use crate::serve::handlers::text_release::{
  TextReleaseDecision,
  approved_pids_from_request,
  challenge_response,
  decide as decide_text_release};
use crate::serve::util::{
  view_id_from_request,
  send_response_with_length_prefix,
  format_buffer_response_sexp,
  tag_sexp_response,
  tag_text_response};
use crate::types::sexp::extract_v_from_kv_pair_in_sexp;
use crate::types::misc::ID;
use crate::skgrepo_sets::SkgrepoRestriction;
use crate::types::views_state::ViewId;

use futures::executor::block_on;
use sexp::{Sexp, Atom};
use std::net::TcpStream; // handles two-way communication

/// Gets a node id from the request,
/// generates an org view of that id's content (recursively),
/// and sends the response to Emacs (length-prefixed).
/// Response format:
/// ((content "...") (errors ("error1" ...)) (warnings ("warning1" ...)))
/// If the requested ID is already a root of an open view,
/// returns ((switch-to-view "VIEW_ID")) instead of rendering.
pub fn handle_single_root_view_request (
  stream     : &mut TcpStream,
  request    : &str,
  env        : &SkgEnv,
  views_state : &mut ViewsState,
  skgrepo_restriction : &SkgrepoRestriction,
) {
  let runtime = env . runtime_snapshot ();
  let view_id_result : Result<ViewId, String> =
    view_id_from_request (request);
  match node_skgid_from_single_root_view_request (request) {
    Ok (skgid) => {
      match skgrepo_restriction . skgid_skgrepo_is_unrestricted (
        &runtime . graph, &runtime . config, &skgid ) {
        Ok (true) => {},
        Ok (false) => {
          let response_sexp : String =
            format_buffer_response_sexp (
              &String::new (),
              &vec! [format! (
                "Node {} is not in skgrepo restriction {}",
                skgid,
                skgrepo_restriction . name )],
              &[] );
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
                "Error checking repo-set visibility: {}", e )],
              &[] );
          send_response_with_length_prefix (
            stream,
            & tag_sexp_response (
              TcpToClient::ContentView, &response_sexp ));
          return; }}
      if let Some (existing_view_id)
        = views_state . open_views
          . content_view_id_for_root_skgid ( &skgid )
        { // Following a link to a root that is already open lands in
          // that ordinary content buffer.
          let switch_sexp : String =
            Sexp::List ( vec! [
              Sexp::List ( vec! [
                Sexp::Atom ( Atom::S (
                  "switch-to-view" . to_string () )),
                Sexp::Atom ( Atom::S (
                  existing_view_id . repr_in_client () )) ] ) ] )
            . to_string ();
          send_response_with_length_prefix (
            stream,
            & tag_sexp_response (
              TcpToClient::ContentView, &switch_sexp ));
          return; }
      let approved_overPrivateText_pids =
        approved_pids_from_request (request);
      let response : String =
      { let _span : tracing::span::EnteredSpan =
          tracing::info_span!( "single_root_view" ). entered();
        block_on ( async {
            let mut render_warnings : Vec<String> = Vec::new ();
            match multi_root_view_via_runtime (
              &runtime,
              &[skgid . clone ()],
              views_state . diff_mode_enabled,
              Some (skgrepo_restriction),
              &mut render_warnings )
            { Ok ( (buffer_content, pids, viewforest) ) => {
                let release = decide_text_release (
                  "single-root-view",
                  skgrepo_restriction,
                  &pids,
                  &runtime . graph,
                  &approved_overPrivateText_pids );
                if matches! (
                  release, TextReleaseDecision::Challenge { .. } ) {
                  return challenge_response (&release) . unwrap (); }
                if let Ok (view_id) = &view_id_result {
                  views_state . open_views . register_view (
                    &runtime . graph,
                    view_id . clone (),
                    viewforest,
                    &pids ); }
                let warnings : Vec<String> =
                  { let mut warnings : Vec<String> =
                      render_warnings;
                    if let TextReleaseDecision::AllowWithWarning {
                      warning,
                    } = release {
                      warnings . push (warning); }
                    warnings };
                tag_sexp_response (
                  TcpToClient::ContentView,
                  & format_buffer_response_sexp (
                    & buffer_content,
                    &[],
                    & warnings ) ) },
              Err (e) => {
                let mut errors : Vec<String> = Vec::new ();
                errors . push ( format! (
                  "Error generating document: {}", e ));
                tag_sexp_response (
                  TcpToClient::ContentView,
                  & format_buffer_response_sexp (
                    & String::new (),
                    & errors,
                    &[] ) ) }} } ) };
      send_response_with_length_prefix (
        stream, &response ); },
    Err (err) => {
      let error_msg : String = format!(
        "Error extracting node ID: {}", err);
      tracing::error! ( "{}", error_msg ) ;
      send_response_with_length_prefix (
        stream,
        & tag_text_response (
          TcpToClient::ContentView, &error_msg )); } } }

pub fn node_skgid_from_single_root_view_request (
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
