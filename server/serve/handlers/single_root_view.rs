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
};
use crate::types::sexp::extract_v_from_kv_pair_in_sexp;
use crate::types::misc::ID;
use crate::source_sets::ActiveSourceSet;
use crate::types::views_state::{ViewState, ViewUri, single_root_recipe};
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
          send_content_view_response (
            stream, runtime, &tag_sexp_response (
              TcpToClient::ContentView, &response_sexp), None);
          return; },
        Err (e) => {
          let response_sexp : String =
            format_buffer_response_sexp (
              &String::new (),
              &vec! [format! (
                "Error checking source-set visibility: {}", e )],
              &[] );
          send_content_view_response (
            stream, runtime, &tag_sexp_response (
              TcpToClient::ContentView, &response_sexp), None);
          return; }}
      let saves_allowed : bool = runtime . maintenance_snapshot () . state
        . policy () . skg_saves_allowed
        && runtime . authority_failure () . is_none ();
      let requested_read_only : bool =
        value_from_request_sexp ("requested-view-write-authority", request)
          . as_deref () == Ok ("read-only");
      if !fresh_view && saves_allowed
        && ! requested_read_only {
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
          let state : Option<&ViewState> = views_state . open_views . views
            . get (&existing_uri);
          send_content_view_response (
            stream, runtime, &tag_sexp_response (
              TcpToClient::ContentView, &switch_sexp), state);
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
              send_content_view_response (
                stream, runtime, &tag_sexp_response (
                  TcpToClient::ContentView, &response_sexp), None);
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
          let state : Option<&ViewState> = if saves_allowed
            && ! requested_read_only {
            views_state . open_views . views . get (&menu_uri)
          } else { None };
          send_content_view_response (
            stream, runtime, &tag_sexp_response (
              TcpToClient::ContentView, &switch_sexp), state);
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
                if let Err (error) = runtime . admit_view_response (request, env, state)
                {
                  views_state . open_views . unregister_view (&menu_uri);
                  return content_view_error_response (runtime, &error); }
                let state = views_state . open_views . views . get (&menu_uri)
                  . expect ("enrolled override menu exists");
                return tag_sexp_response (
                  TcpToClient::ContentView,
                  &add_view_authority_to_response (&formatted, state)); },
              Ok (None) => {}, // not overridden (visibly): render normally
              Err (e) => {
                let response_sexp : String = format_buffer_response_sexp (
                  &String::new (),
                  &[format! ("Error generating override menu: {}", e)],
                  &[] );
                return content_view_response_with_authority (
                  runtime,
                  &tag_sexp_response (TcpToClient::ContentView, &response_sexp),
                  None); }}
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
                  if let Err (error) = runtime . admit_view_response (request, env, state)
                  {
                    views_state . open_views . unregister_view (view_uri);
                    return content_view_error_response (runtime, &error); }
                  let state = views_state . open_views . views . get (view_uri)
                    . expect ("enrolled content view exists");
                  add_view_authority_to_response (&formatted, state)
                } else { add_read_only_view_authority (&formatted) };
                tag_sexp_response (TcpToClient::ContentView, &formatted) },
              Err (e) => { // If we fail to generate the view, ship the generation error in the errors vec, with empty content so the client skips opening a main buffer.
                let mut errors : Vec<String> = Vec::new ();
                let warnings : Vec<String> = Vec::new ();
                errors . push ( format! (
                  "Error generating document: {}", e ));
                let response_sexp : String = format_buffer_response_sexp (
                  &String::new (), &errors, &warnings);
                content_view_response_with_authority (
                  runtime,
                  &tag_sexp_response (TcpToClient::ContentView, &response_sexp),
                  None) }} } ) };
      let response : String = super::maintenance_protocol::with_current_state_fields (
        runtime, &response) . expect ("content response is a formatted list");
      let _ = send_response_with_length_prefix (
        stream, &response ); },
    Err (err) => {
      let error_msg : String = format!(
        "Error extracting node ID: {}", err);
      tracing::error! ( "{}", error_msg ) ;
      let response : String = content_view_error_response (runtime, &error_msg);
      let _ = send_response_with_length_prefix (stream, &response); } } }

fn send_content_view_response (
  stream : &mut TcpStream,
  runtime : &ServerRuntime,
  response : &str,
  state : Option<&ViewState>,
) {
  let response : String = content_view_response_with_authority (
    runtime, response, state);
  let _ = send_response_with_length_prefix (stream, &response);
}

fn content_view_error_response (
  runtime : &ServerRuntime,
  error : &str,
) -> String {
  let response_sexp : String = format_buffer_response_sexp (
    &String::new (), &[error . to_string ()], &[]);
  content_view_response_with_authority (
    runtime,
    &tag_sexp_response (TcpToClient::ContentView, &response_sexp),
    None)
}

fn content_view_response_with_authority (
  runtime : &ServerRuntime,
  response : &str,
  state : Option<&ViewState>,
) -> String {
  let response : String = match state {
    Some (state) => add_view_authority_to_response (response, state),
    None => add_read_only_view_authority (response),
  };
  super::maintenance_protocol::with_current_state_fields (runtime, &response)
    . expect ("content response is a formatted list")
}

fn add_read_only_view_authority (
  response : &str,
) -> String {
  let Ok (Sexp::List (mut fields)) = sexp::parse (response) else {
    unreachable! ("content response formatter produced invalid sexp"); };
  fields . push (Sexp::List (vec![
    Sexp::Atom (Atom::S ("view-write-authority" . into ())),
    Sexp::Atom (Atom::S ("read-only" . into ())),
  ]));
  Sexp::List (fields) . to_string ()
}

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

#[cfg(test)]
mod tests {
  use super::*;
  use crate::dbs::in_rust_graph::{InRustGraph, InRustGraphHandle};
  use crate::dbs::init::empty_in_ram_tantivy_index;
  use crate::maintenance::MaintenanceOrigin;
  use crate::source_sets::SourceSetName;
  use crate::test_utils::read_lp_message;
  use crate::types::misc::{SkgConfig, SkgfileSource, TantivyIndex};
  use crate::types::store_state::{SelectedPathManifest, SelectedStoreState};
  use crate::types::tree::forest::ViewForest;
  use crate::types::views_state::OpenViews;
  use std::collections::HashMap;
  use std::io::BufReader;
  use std::net::{SocketAddr, TcpListener, TcpStream};
  use std::sync::Arc;
  use arc_swap::ArcSwap;
  use tempfile::{TempDir, tempdir};

  fn runtime_fixture (
  ) -> (TempDir, SkgEnv, ServerRuntime) {
    let directory : TempDir = tempdir () . unwrap ();
    let source : SkgfileSource = SkgfileSource {
      name: "public" . into (),
      abbreviation: None,
      path: directory . path () . join ("public"),
      user_owns_it: false, };
    let mut config : SkgConfig = SkgConfig::dummyFromSources (
      HashMap::from ([("public" . into (), source)]));
    config . config_path = directory . path () . join ("config.toml");
    config . data_root = directory . path () . to_path_buf ();
    config . maintenance_archive_identity =
      directory . path () . join ("archive");
    let index : TantivyIndex = empty_in_ram_tantivy_index () . unwrap ();
    let graph : InRustGraphHandle = Arc::new (ArcSwap::from (Arc::new (
      SelectedStoreState::initial (
        InRustGraph::new (), SelectedPathManifest::new ())
        . with_searcher (index . reader . searcher ()) )));
    let env : SkgEnv = SkgEnv {
      config: config . clone (),
      in_rust_graph: graph,
      searcher: index . reader . searcher (),
      tantivy_index: index,
      startup_warnings: Arc::new (Vec::new ()), };
    let runtime : ServerRuntime = ServerRuntime::new (env . clone ())
      . unwrap ();
    (directory, env, runtime) }

  fn connected_tcp_stream_pair (
  ) -> (TcpStream, TcpStream) {
    let listener : TcpListener = TcpListener::bind ("127.0.0.1:0")
      . unwrap ();
    let address : SocketAddr = listener . local_addr () . unwrap ();
    let client : TcpStream = TcpStream::connect (address) . unwrap ();
    let accepted : (TcpStream, SocketAddr) =
      listener . accept () . unwrap ();
    let server : TcpStream = accepted . 0;
    (server, client) }

  fn handler_response (
    request : &str,
    env : &SkgEnv,
    views_state : &mut ViewsState,
    active_source_set : &ActiveSourceSet,
    runtime : &ServerRuntime,
  ) -> String {
    let (mut server, client) : (TcpStream, TcpStream) =
      connected_tcp_stream_pair ();
    std::thread::scope (|scope| {
      scope . spawn (|| {
        handle_single_root_view_request (
          &mut server, request, env, views_state,
          active_source_set, runtime); }); });
    let mut reader : BufReader<TcpStream> = BufReader::new (client);
    read_lp_message (&mut reader) . unwrap () }

  fn registered_menu (
    env : &SkgEnv,
    views_state : &mut ViewsState,
  ) -> ViewUri {
    let menu_uri : ViewUri = ViewUri::OverrideMenu ("node" . into ());
    views_state . open_views . register_view_with_authority (
      &env . in_rust_graph_snapshot (), menu_uri . clone (),
      ViewForest::new (), &[], 1, 0, 1,
      BufferKind::OverrideChoiceMenu, "all" . into (), None);
    menu_uri }

  #[test]
  fn reused_menu_handler_is_read_only_during_closed_admission () {
    let (_directory, env, runtime) : (TempDir, SkgEnv, ServerRuntime) =
      runtime_fixture ();
    let active : ActiveSourceSet = ActiveSourceSet::named (
      &env . config, SourceSetName::from ("all")) . unwrap ();
    let mut views_state : ViewsState = ViewsState {
      diff_mode_enabled: false,
      open_views: OpenViews::new (), };
    let menu_uri : ViewUri = registered_menu (&env, &mut views_state);
    runtime . transition_maintenance (|coordinator| {
      coordinator . begin (MaintenanceOrigin::ExplicitPartialReload, None)
        . map (|_| ()) }) . unwrap ();
    let request : String = "((request . \"single root content view\") \
      (id . \"node\") (view-uri . \"new-view\") \
      (requested-view-write-authority . \"editable\"))" . into ();
    let response : String = handler_response (
      &request, &env, &mut views_state, &active, &runtime);
    assert! (response . contains ("switch-to-view"), "{}", response);
    assert! (response . contains ("override-menu:node"), "{}", response);
    assert! (response . contains ("(view-write-authority read-only)"),
      "{}", response);
    assert! (! response . contains ("(view-write-authority editable)"),
      "{}", response);
    assert! (response . contains ("(current-graph-generation 1)"),
      "{}", response);
    assert! (views_state . open_views . views
      . get (&menu_uri) . map (|state| state . writes_admitted) == Some (true));
  }

  #[test]
  fn reused_menu_handler_stays_editable_when_admission_is_open () {
    let (_directory, env, runtime) : (TempDir, SkgEnv, ServerRuntime) =
      runtime_fixture ();
    let active : ActiveSourceSet = ActiveSourceSet::named (
      &env . config, SourceSetName::from ("all")) . unwrap ();
    let mut views_state : ViewsState = ViewsState {
      diff_mode_enabled: false,
      open_views: OpenViews::new (), };
    let menu_uri : ViewUri = registered_menu (&env, &mut views_state);
    let request : String = "((request . \"single root content view\") \
      (id . \"node\") (view-uri . \"new-view\") \
      (requested-view-write-authority . \"editable\"))" . into ();
    let response : String = handler_response (
      &request, &env, &mut views_state, &active, &runtime);
    assert! (response . contains ("switch-to-view"), "{}", response);
    assert! (response . contains ("override-menu:node"), "{}", response);
    assert! (response . contains ("(view-write-authority editable)"),
      "{}", response);
    assert! (response . contains ("(current-graph-generation 1)"),
      "{}", response);
    assert! (views_state . open_views . views
      . get (&menu_uri) . map (|state| state . writes_admitted) == Some (true));
  }

  #[test]
  fn reused_menu_handler_honors_explicit_read_only_request () {
    let (_directory, env, runtime) : (TempDir, SkgEnv, ServerRuntime) =
      runtime_fixture ();
    let active : ActiveSourceSet = ActiveSourceSet::named (
      &env . config, SourceSetName::from ("all")) . unwrap ();
    let mut views_state : ViewsState = ViewsState {
      diff_mode_enabled: false,
      open_views: OpenViews::new (), };
    let menu_uri : ViewUri = registered_menu (&env, &mut views_state);
    let request : String = "((request . \"single root content view\") \
      (id . \"node\") (view-uri . \"new-view\") \
      (requested-view-write-authority . \"read-only\"))" . into ();
    let response : String = handler_response (
      &request, &env, &mut views_state, &active, &runtime);
    assert! (response . contains ("switch-to-view"), "{}", response);
    assert! (response . contains ("override-menu:node"), "{}", response);
    assert! (response . contains ("(view-write-authority read-only)"),
      "{}", response);
    assert! (! response . contains ("(view-write-authority editable)"),
      "{}", response);
    assert! (views_state . open_views . views
      . get (&menu_uri) . map (|state| state . writes_admitted) == Some (true));
  }

  #[test]
  fn unknown_id_handler_has_current_fields_and_no_editable_uri () {
    let (_directory, env, runtime) : (TempDir, SkgEnv, ServerRuntime) =
      runtime_fixture ();
    let active : ActiveSourceSet = ActiveSourceSet::named (
      &env . config, SourceSetName::from ("public")) . unwrap ();
    let mut views_state : ViewsState = ViewsState {
      diff_mode_enabled: false,
      open_views: OpenViews::new (), };
    let request : String = "((request . \"single root content view\") \
      (id . \"unknown\") (view-uri . \"new-view\"))" . into ();
    let response : String = handler_response (
      &request, &env, &mut views_state, &active, &runtime);
    assert! (response . contains ("(errors"), "{}", response);
    assert! (response . contains ("not in active source-set"), "{}", response);
    assert! (response . contains ("(view-write-authority read-only)"),
      "{}", response);
    assert! (! response . contains ("(view-write-authority editable)"),
      "{}", response);
    assert! (response . contains ("(current-graph-generation 1)"),
      "{}", response);
    assert! (response . contains ("owner-publication-revision"),
      "{}", response);
    assert! (response . contains ("current-manifest-revision"),
      "{}", response);
    assert! (! response . contains ("new-view"), "{}", response);
  }
}
