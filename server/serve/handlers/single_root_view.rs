//! The "single root content view" request: draw a new view rooted at
//! one node ('render_initial_view' in 'server/update_buffer.rs'), or
//! name the open view already rooted there.

use crate::types::env::{RuntimeGeneration, SkgEnv};
use crate::serve::ViewsState;
use crate::update_buffer::multi_root_view_via_runtime;
use crate::serve::protocol::TcpToClient;
use crate::serve::handlers::text_release::{
  TextReleaseDecision,
  approved_pids_from_request,
  challenge_response,
  decide as decide_text_release};
use crate::serve::util::{
  viewforest_id_from_request,
  send_response_with_length_prefix,
  format_buffer_response_sexp,
  sexp_to_client_string,
  tag_sexp_response,
  tag_text_response};
use crate::types::sexp::extract_v_from_kv_pair_in_sexp;
use crate::types::misc::ID;
use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::in_rust_graph::relation_accessors::GnidPresence;
use crate::skgrepo_sets::SkgrepoRestriction;
use crate::types::views_state::ViewforestId;

use futures::executor::block_on;
use sexp::{Sexp, Atom};
use std::collections::HashSet;
use std::net::TcpStream; // handles two-way communication

/// Gets a node id from the request,
/// generates an org view of that id's content (recursively),
/// and sends the response to Emacs (length-prefixed).
/// Response format:
/// ((content "...") (errors ("error1" ...)) (warnings ("warning1" ...)))
/// If the requested ID is already a root of an open content view
/// proper ('OpenViews::is_content_view_proper'),
/// returns ((switch-to-view "VIEW_ID")) instead of rendering.
pub fn handle_single_root_view_request (
  stream     : &mut TcpStream,
  request    : &str,
  env        : &SkgEnv,
  views_state : &mut ViewsState,
  skgrepo_restriction : &SkgrepoRestriction,
) {
  let runtime = env . runtime_snapshot ();
  let viewforest_id_result : Result<ViewforestId, String> =
    viewforest_id_from_request (request);
  match node_gnid_from_single_root_view_request (request) {
    Ok (gnid) => {
      if gnid_is_kept_out (&runtime . graph, &gnid, skgrepo_restriction) {
        let response_sexp : String =
          format_buffer_response_sexp (
            &String::new (),
            &vec! [format! (
              "Node {} is not in skgrepo restriction {}",
              gnid,
              skgrepo_restriction . most_private_shown )],
            &[] );
        send_response_with_length_prefix (
          stream,
          & tag_sexp_response (
            TcpToClient::ContentView, &response_sexp ));
        return; }
      if let Some (existing_viewforest_id)
        = views_state . open_views
          . content_viewforest_id_for_root_gnid ( &gnid )
        { // Following a link to a root that is already open lands in
          // that content view proper's buffer.
          send_response_with_length_prefix (
            stream,
            & tag_sexp_response (
              TcpToClient::ContentView,
              & switch_to_view_sexp (&existing_viewforest_id) ));
          return; }
      let approved_overPrivateText_pids =
        approved_pids_from_request (request);
      let response : String =
      { let _span : tracing::span::EnteredSpan =
          tracing::info_span!( "single_root_view" ). entered();
        block_on ( async {
          draw_and_register_single_root_view (
            &runtime, &gnid, &viewforest_id_result, views_state,
            skgrepo_restriction, &approved_overPrivateText_pids ) } ) };
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

/// Whether the skgrepo restriction keeps 'gnid' out of a view: it
/// keeps out a restricted node, and, unless the connection is
/// unrestricted, an absent one.
fn gnid_is_kept_out (
  graph               : &InRustGraph,
  gnid                : &ID,
  skgrepo_restriction : &SkgrepoRestriction,
) -> bool {
  match graph . gnid_presence_gated (gnid, skgrepo_restriction) {
    GnidPresence::Present    => false,
    GnidPresence::Restricted => true,
    GnidPresence::Absent     => // refused under any skgrepo restriction
      ! skgrepo_restriction . is_unrestricted (), }}

/// '((switch-to-view "VIEW_ID"))': the client should show the open
/// view 'viewforest_id' instead of a new one.
fn switch_to_view_sexp (
  viewforest_id : &ViewforestId,
) -> String {
  sexp_to_client_string ( &Sexp::List ( vec! [
    Sexp::List ( vec! [
      Sexp::Atom ( Atom::S (
        "switch-to-view" . to_string () )),
      Sexp::Atom ( Atom::S (
        viewforest_id . repr_in_client () )) ] ) ] )) }

/// Draw the view rooted at 'gnid' and answer it as a content view,
/// registering it under 'viewforest_id_result' if the request named
/// one. Text the release check would challenge is answered with the
/// challenge instead, and nothing is registered. A drawing error is
/// answered as the view's one error.
fn draw_and_register_single_root_view (
  runtime                       : &RuntimeGeneration,
  gnid                          : &ID,
  viewforest_id_result          : &Result<ViewforestId, String>,
  views_state                   : &mut ViewsState,
  skgrepo_restriction           : &SkgrepoRestriction,
  approved_overPrivateText_pids : &HashSet<ID>,
) -> String {
  match multi_root_view_via_runtime (
    runtime,
    &[gnid . clone ()],
    skgrepo_restriction )
  { Ok ( (buffer_content, pids, viewforest) ) => {
      let release = decide_text_release (
        "single-root-view",
        skgrepo_restriction,
        &pids,
        &runtime . graph,
        approved_overPrivateText_pids );
      if matches! (
        release, TextReleaseDecision::Challenge { .. } ) {
        return challenge_response (&release) . unwrap (); }
      if let Ok (viewforest_id) = viewforest_id_result {
        views_state . open_views . register_view (
          &runtime . graph,
          viewforest_id . clone (),
          viewforest,
          &pids,
          skgrepo_restriction ); }
      let warnings : Vec<String> =
        { let mut warnings : Vec<String> = Vec::new ();
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
          &[] ) ) }} }

pub fn node_gnid_from_single_root_view_request (
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
