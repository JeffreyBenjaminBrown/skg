use crate::types::env::SkgEnv;
use crate::serve::ViewsState;
use crate::to_org::render::content_view::multi_root_view_via_env;
use crate::to_org::render::override_menu::override_menu_view;
use crate::serve::protocol::TcpToClient;
use crate::serve::handlers::scalar_release::{
  ScalarReleaseDecision,
  approved_pids_from_request,
  challenge_response,
  decide as decide_scalar_release};
use crate::serve::util::{
  view_uri_from_request,
  value_from_request_sexp,
  send_response_with_length_prefix,
  format_buffer_response_sexp,
  format_override_menu_response_sexp,
  add_view_authority_to_response,
  tag_sexp_response,
  tag_text_response};
use crate::types::sexp::extract_v_from_kv_pair_in_sexp;
use crate::types::misc::ID;
use crate::source_sets::ActiveSourceSet;
use crate::types::views_state::{ViewUri, single_root_recipe};
use crate::maintenance::BufferKind;
use crate::runtime::ServerRuntime;

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
  runtime     : &ServerRuntime,
) {
  let view_uri_result : Result<ViewUri, String> =
    view_uri_from_request (request);
  let fresh_view : bool = sexp::parse (request) . ok ()
    . and_then (|request| extract_v_from_kv_pair_in_sexp (
      &request, "fresh-view") . ok ())
    . map (|value| value == "true")
    . unwrap_or (false);
  match node_id_from_single_root_view_request (request) {
    Ok (node_id) => {
      match active_source_set . id_source_is_active (
        &env . in_rust_graph_snapshot (), &node_id ) {
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
          let _ = send_response_with_length_prefix (
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
          let _ = send_response_with_length_prefix (
            stream,
            & tag_sexp_response (
              TcpToClient::ContentView, &response_sexp ));
          return; }}
      if !fresh_view && runtime . maintenance_snapshot () . state . policy () . skg_saves_allowed
        && value_from_request_sexp ("requested-view-write-authority", request)
          . as_deref () != Ok ("read-only") {
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
          let _ = send_response_with_length_prefix (
            stream,
            & tag_sexp_response (
              TcpToClient::ContentView, &switch_sexp ));
          return; }}
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
              let _ = send_response_with_length_prefix (
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
          let _ = send_response_with_length_prefix (
            stream,
            & tag_sexp_response (
              TcpToClient::ContentView, &switch_sexp ));
          return; }
      let approved_ugly_pids =
        approved_pids_from_request (request);
      let response : String =
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
                let release = decide_scalar_release (
                  "override-menu",
                  active_source_set,
                  &menu_pids,
                  &env . in_rust_graph_snapshot (),
                  &approved_ugly_pids );
                if matches! (
                  release, ScalarReleaseDecision::Challenge { .. } ) {
                  return challenge_response (&release) . unwrap (); }
                views_state . open_views . register_view_with_authority (
                    &env . in_rust_graph_snapshot (),                   menu_uri . clone (),
                  menu_forest,
                  &menu_pids,
                  env . in_rust_graph . load_full ()
                    . graph_generation . get (),
                  0,
                  1,
                  BufferKind::OverrideChoiceMenu,
                  active_source_set . name . 0 . clone (),
                  Some (single_root_recipe (&pid)) );
                let mut warnings : Vec<String> = Vec::new ();
                if let ScalarReleaseDecision::AllowWithWarning {
                  warning,
                } = release {
                  warnings . push (warning); }
                let formatted = format_override_menu_response_sexp (
                  &menu_content,
                  &menu_uri,
                  "The requested node is overridden. Choose a destination.",
                  &warnings );
                let state = views_state . open_views . views . get_mut (&menu_uri)
                  . expect ("registered override menu exists");
                if let Err (error) = runtime . admit_view_response (request, state)
                {
                  views_state . open_views . unregister_view (&menu_uri);
                  return tag_text_response (TcpToClient::Error, &error); }
                let state = views_state . open_views . views . get (&menu_uri)
                  . expect ("enrolled override menu exists");
                return tag_sexp_response (
                  TcpToClient::ContentView,
                  &add_view_authority_to_response (&formatted, state)); },
              Ok (None) => {}, // not overridden (visibly): render normally
              Err (e) => {
                return tag_sexp_response (
                  TcpToClient::ContentView,
                  & format_buffer_response_sexp (
                    &String::new (),
                    &[ format! (
                        "Error generating override menu: {}", e ) ],
                    &[] ) ); }}
            let mut render_warnings : Vec<String> = Vec::new ();
            match multi_root_view_via_env (
              env,
              &[node_id . clone ()],
              views_state . diff_mode_enabled,
              Some (active_source_set),
              &mut render_warnings ) . await
            { Ok ( (buffer_content, pids, viewforest) ) => {
                let release = decide_scalar_release (
                  "single-root-view",
                  active_source_set,
                  &pids,
                  &env . in_rust_graph_snapshot (),
                  &approved_ugly_pids );
                if matches! (
                  release, ScalarReleaseDecision::Challenge { .. } ) {
                  return challenge_response (&release) . unwrap (); }
                if let Ok (view_uri) = &view_uri_result {
                  views_state . open_views . register_view_with_authority (
                    &env . in_rust_graph_snapshot (),                     view_uri . clone (),
                    viewforest,
                    &pids,
                    env . in_rust_graph . load_full ()
                      . graph_generation . get (),
                    0,
                    1,
                    BufferKind::ContentView,
                    active_source_set . name . 0 . clone (),
                    Some (single_root_recipe (&node_id)) ); }
                let warnings : Vec<String> =
                  { let mut warnings : Vec<String> =
                      render_warnings;
                    if let ScalarReleaseDecision::AllowWithWarning {
                      warning,
                    } = release {
                      warnings . push (warning); }
                    warnings };
                let formatted = format_buffer_response_sexp (
                  &buffer_content, &[], &warnings);
                let formatted = if let Ok (view_uri) = &view_uri_result {
                  let state = views_state . open_views . views . get_mut (view_uri)
                    . expect ("registered content view exists");
                  if let Err (error) = runtime . admit_view_response (request, state)
                  {
                    views_state . open_views . unregister_view (view_uri);
                    return tag_text_response (TcpToClient::Error, &error); }
                  let state = views_state . open_views . views . get (view_uri)
                    . expect ("enrolled content view exists");
                  add_view_authority_to_response (&formatted, state)
                } else { formatted };
                tag_sexp_response (TcpToClient::ContentView, &formatted) },
              Err (e) => { // If we fail to generate the view, ship the generation error in the errors vec, with empty content so the client skips opening a main buffer.
                let mut errors : Vec<String> = Vec::new ();
                let warnings : Vec<String> = Vec::new ();
                errors . push ( format! (
                  "Error generating document: {}", e ));
                tag_sexp_response (
                  TcpToClient::ContentView,
                  & format_buffer_response_sexp (
                    & String::new (),
                    & errors,
                    & warnings ) ) }} } ) };
      let response : String = super::maintenance_protocol::with_current_state_fields (
        runtime, &response) . expect ("content response is a formatted list");
      let _ = send_response_with_length_prefix (
        stream, &response ); },
    Err (err) => {
      let error_msg : String = format!(
        "Error extracting node ID: {}", err);
      tracing::error! ( "{}", error_msg ) ;
      let _ = send_response_with_length_prefix (
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
