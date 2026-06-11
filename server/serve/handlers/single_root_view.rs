use crate::dbs::in_rust_graph::scheduled_audit::take_pending_audit_warning;
use crate::types::env::SkgEnv;
use crate::serve::ViewsState;
use crate::to_org::render::content_view::multi_root_view_via_env;
use crate::to_org::render::override_menu::override_menu_view;
use crate::serve::protocol::TcpToClient;
use crate::serve::util::{
  view_uri_from_request,
  value_from_request_sexp,
  send_response_with_length_prefix,
  format_buffer_response_sexp,
  format_override_menu_response_sexp,
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
/// Response format:
/// ((content "...") (errors ("error1" ...)) (warnings ("warning1" ...)))
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
                active_source_set . name )],
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
                "Error checking source-set visibility: {}", e )],
              &[] );
          send_response_with_length_prefix (
            stream,
            & tag_sexp_response (
              TcpToClient::ContentView, &response_sexp ));
          return; }}
      if let Some (existing_uri)
        = views_state . open_views
          . content_view_uri_for_root_id ( &node_id )
        { // An open raw view of the node beats the menu: this check
          // runs first, so following a link lands in the open buffer.
          let switch_sexp : String =
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
      let bypass_menu : bool =
        // The optional (override-choice . "menu" | "bypass") field,
        // defaulting to menu. Bypass surfaces (magit jumps,
        // skg-goto-bypassOverride) skip the menu and open the
        // requested root raw; recursive content beneath it still
        // follows the substitution rules.
        match value_from_request_sexp ("override-choice", request) {
          Err (_)  => false, // absent: the default, menu
          Ok (v) => match v . as_str () {
            "menu"   => false,
            "bypass" => true,
            other => {
              let response_sexp : String =
                format_buffer_response_sexp (
                  &String::new (),
                  &vec! [format! (
                    "Unknown override-choice value: {} (expected \"menu\" or \"bypass\")",
                    other )],
                  &[] );
              send_response_with_length_prefix (
                stream,
                & tag_sexp_response (
                  TcpToClient::ContentView, &response_sexp ));
              return; }}};
      let pid : ID = // the menu is per resolved node, extra-IDs included
        env . in_rust_graph . load_full ()
        . pid_of (&node_id)
        . unwrap_or_else ( || node_id . clone () );
      let menu_uri : ViewUri =
        ViewUri::OverrideMenu ( pid . 0 . clone () );
      if ! bypass_menu
        && views_state . open_views . views
        . contains_key (&menu_uri)
        { // One menu per node: a second request switches to it.
          let switch_sexp : String =
            Sexp::List ( vec! [
              Sexp::List ( vec! [
                Sexp::Atom ( Atom::S (
                  "switch-to-view" . to_string () )),
                Sexp::Atom ( Atom::S (
                  menu_uri . repr_in_client () )) ] ) ] )
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
            // The override-choice buffer: a NEW single-root view of
            // an overridden node offers the chain of overriders
            // instead of silently choosing. Offered in diff mode
            // too (decided 2026-06-11): the menu is navigation, not
            // decoration, and it presents raw graph facts.
            match if bypass_menu { Ok (None) }
                  else { override_menu_view (
                           env, &pid,
                           Some (active_source_set) ) . await }
            { Ok ( Some ((menu_content, menu_pids, menu_forest)) ) => {
                views_state . open_views . register_view (
                  menu_uri . clone (),
                  menu_forest,
                  &menu_pids );
                let warnings : Vec<String> =
                  take_pending_audit_warning ()
                    . map ( |w| vec! [w] )
                    . unwrap_or_default ();
                return format_override_menu_response_sexp (
                  &menu_content,
                  &menu_uri,
                  "The requested node is overridden. Choose a destination.",
                  &warnings ); },
              Ok (None) => {}, // not overridden (visibly): render normally
              Err (e) => {
                return format_buffer_response_sexp (
                  &String::new (),
                  &[ format! (
                      "Error generating override menu: {}", e ) ],
                  &[] ); }}
            match multi_root_view_via_env (
              env,
              &[node_id . clone ()],
              views_state . diff_mode_enabled,
              Some (active_source_set) ) . await
            { Ok ( (buffer_content, pids, viewforest) ) => {
                if let Ok (view_uri) = &view_uri_result {
                  views_state . open_views . register_view (
                    view_uri . clone (),
                    viewforest,
                    &pids ); }
                let warnings : Vec<String> =
                  take_pending_audit_warning ()
                    . map ( |w| vec! [w] )
                    . unwrap_or_default ();
                format_buffer_response_sexp (
                  & buffer_content,
                  &[],
                  & warnings ) },
              Err (e) => { // If we fail to generate the view, ship the generation error (and any pending audit warning) in the errors vec, with empty content so the client skips opening a main buffer.
                let mut errors : Vec<String> = Vec::new ();
                let mut warnings : Vec<String> = Vec::new ();
                if let Some (w) = take_pending_audit_warning ()
                { warnings . push (w); }
                errors . push ( format! (
                  "Error generating document: {}", e ));
                format_buffer_response_sexp (
                  & String::new (),
                  & errors,
                  & warnings ) }} } ) };
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
