use crate::dbs::in_rust_graph::in_rust_graph_coherent_with_save_instructions;
use crate::from_text::buffer_to_validated_saveplan_with_fork_sources;
use crate::serve::handlers::save_dependencies::validate_save_dependencies;
use crate::git_ops::diff::compute_diff_for_source;
use crate::git_ops::read_repo::{open_repo, head_is_merge_commit};
use crate::save::{
  apply_delete_propagation_cleanup,
  preflight_fs_from_saveinstructions_with_hoist_approval,
  update_graph_including_nodeMerges_with_operation,
};
use crate::serve::ViewsState;
use crate::runtime::ServerRuntime;
use crate::maintenance::QueuedObservationReason;
use crate::source_sets::ActiveSourceSet;
use crate::serve::protocol::TcpToClient;
use crate::serve::handlers::telescope_hoist::{
  HoistCandidate,
  approved_pids_from_request as hoist_approved_pids_from_request,
  candidates_from_selected as hoist_candidates_from_selected,
  confirmation_response as hoist_confirmation_response,
  needs_confirmation as hoist_needs_confirmation,
  repair_saves_for_unwritten_candidates,
};
use crate::serve::handlers::scalar_release::{
  approved_pids_from_request as scalar_approved_pids_from_request,
};
use crate::serve::handlers::collateral_scheduler::CollateralScheduler;
use crate::serve::util::{
  view_uri_from_request,
  format_buffer_response_sexp,
  format_fork_confirmation_response_sexp,
  format_lock_views_sexp,
  send_response_with_length_prefix,
  tag_sexp_response,
  value_from_request_sexp };
use crate::from_text::fork::build_fork_confirmation_buffer;
use crate::types::env::SkgEnv;
use crate::types::errors::SaveError;
use crate::types::git::{SourceDiff, GitDiffStatus};
use crate::types::misc::{ID, SourceName, SkgConfig};
use crate::types::save::{DefineNode, SavePlan, format_save_error_as_org};
use crate::types::tree::forest::ViewForest;
use crate::types::views_state::{ViewUri, ViewSaveBase};
use crate::types::store_state::SelectedStoreState;
use crate::update_buffer::update_views_after_save;

use futures::executor::block_on;
use sexp::{Sexp, Atom};
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::net::TcpStream;
use std::path::Path;
use std::sync::Arc;

/// The ordinary terminal message in the save protocol. Hoist, fork, and
/// scalar-release confirmations are alternative terminal messages.
/// Contains the re-rendered saved buffer and any warnings/errors when sent.
/// See <api-and-formats.md § Save buffer> for the full sequence:
///   save-lock → save-relax-lock → collateral-view* → save-result.
pub struct SaveResponse {
  pub saved_view          : String,
  pub errors              : Vec<String>,
  pub warnings            : Vec<String>,
  pub save_point_position : Option<SavePointPosition>,
  /// Some when this save found fork candidates and was NOT pre-approved:
  /// nothing was committed, 'saved_view' instead holds the read-only
  /// fork-confirmation buffer, and this holds the one-line minibuffer
  /// prompt. The handler then sends a 'fork-confirmation' message rather
  /// than 'save-result'. None for an ordinary save.
  pub fork_confirmation   : Option<String>,
  /// Some when this save found an ugly current disk telescope without an
  /// exact publication approval. This is checked before fork confirmation;
  /// nothing was committed and the response contains no scalar text.
  pub hoist_confirmation  : Option<Vec<HoistCandidate>>,
  /// Fully tagged ugly-telescope-confirmation response produced after the
  /// save's rerenders have been staged but before text or view-state release.
  pub scalar_release_confirmation : Option<String>,
}

#[derive(Clone, Debug)]
pub struct RequestedSaveAuthority {
  buffer_id         : String,
  kind              : String,
  graph_generation  : u64,
  server_revision   : u64,
  application_token : u64,
}

#[derive(Clone)]
pub struct SavePointPosition {
  pub point_lines_below_focused_headline    : usize,
  pub point_column                          : usize,
  pub point_screen_lines_below_window_start : usize,
}

impl SaveResponse {
  /// Format: ((content "...") (errors (...)) (warnings (...)))
  fn to_sexp_string (&self) -> String {
    let mut response : Sexp =
      sexp::parse (
        &format_buffer_response_sexp (
          & self . saved_view,
          & self . errors,
          & self . warnings ))
      . expect (
        "format_buffer_response_sexp should produce valid sexp" );
    if let ( Sexp::List (items),
             Some (point_position) ) =
      ( &mut response, &self . save_point_position )
    { push_save_point_position_to_sexp_items (
        items, point_position ); }
    response . to_string () }}

/// Execute one admitted save with its durable identity and owner reservation.
pub(crate) fn handle_save_buffer_request (
  stream : &mut TcpStream,
  request : &str,
  content : &str,
  env : &mut SkgEnv,
  views_state : &mut ViewsState,
  active_source_set : &ActiveSourceSet,
  collateral_scheduler : &mut CollateralScheduler,
  runtime : &ServerRuntime,
  operation : &crate::runtime::save_operations::SaveOperation,
  control : &crate::runtime::MutationControl,
  pre_parse_refusal : Option<&str>,
) {
  let view_uri : Result<ViewUri, String> = view_uri_from_request (request);
  let point : Option<SavePointPosition> = save_point_position_from_request (request);
  let authority : Result<RequestedSaveAuthority, String> = requested_save_authority (request);
  if let Some (reason) = pre_parse_refusal {
    let response = save_refusal_response (reason, request);
    finish_save_response (stream, runtime, env, operation, control, &response, "refused");
    return;
  }
  let authority = match authority {
    Ok (authority) => authority,
    Err (reason) => {
      let response = save_refusal_response (&reason, request);
      finish_save_response (stream, runtime, env, operation, control, &response, "refused");
      return;
    }
  };
  let uris : Vec<ViewUri> = uris_of_views_to_lock (&view_uri, views_state);
  let _ = send_response_with_length_prefix (stream,
    &tag_sexp_response (TcpToClient::SaveLock, &format_lock_views_sexp (&uris)));
  let result = block_on (update_from_and_rerender_buffer_with_approvals_with_operation (
    stream, content, env, views_state . diff_mode_enabled, &view_uri,
    views_state, Some (active_source_set), fork_approved_from_request (request),
    &fork_sources_from_request (request), &hoist_approved_pids_from_request (request),
    &scalar_approved_pids_from_request (request), Some (&authority),
    Some (collateral_scheduler), Some (operation)));
  let (response, state) : (String, &str) = match result {
    Ok (mut saved) => {
      saved . save_point_position = point . clone ();
      match (&saved . hoist_confirmation, &saved . fork_confirmation,
             &saved . scalar_release_confirmation) {
        (Some (candidates), _, _) => (
          tag_sexp_response (TcpToClient::TelescopeHoistConfirmation,
            &hoist_confirmation_response (candidates)), "refused"),
        (None, Some (prompt), _) => (
          tag_sexp_response (TcpToClient::ForkConfirmation,
            &format_fork_confirmation_response_sexp (&saved . saved_view, prompt)), "refused"),
        (None, None, Some (confirmation)) => (confirmation . clone (), "committed"),
        (None, None, None) => {
          if let Ok (uri) = &view_uri {
            let _ = views_state . open_views . set_client_application_authority (
              uri, env . in_rust_graph . load_full () . graph_generation . get (),
              collateral_scheduler . presentation_generation (),
              authority . application_token . saturating_add (1));
            if let Some (state) = views_state . open_views . views . get_mut (uri) {
              state . retain_save_base (ViewSaveBase::from_env (env, &state . source_set))
                . expect ("successful save and its returned view share one base"); }
          }
          let mut payload : String = saved . to_sexp_string ();
          if let Ok (uri) = &view_uri {
            if let Some (view) = views_state . open_views . views . get (uri) {
              payload = crate::serve::util::add_view_authority_to_response (&payload, view); }
          }
          (tag_sexp_response (TcpToClient::SaveResult, &payload), "committed")
        }
      }
    }
    Err (error) => {
      if let Some (SaveError::DiskSelectionChanged { paths, .. }) = error . downcast_ref::<SaveError> () {
        if let Err (reason) = runtime . schedule_path_observation (
            paths . clone (), QueuedObservationReason::SaveFenceMismatch) {
          tracing::warn! (%reason, "could not schedule save-fence observation"); }
      }
      let details : String = error . downcast_ref::<SaveError> ()
        . map (format_save_error_as_org) . unwrap_or_else (|| error . to_string ());
      let state : &str = match operation . status () {
        Ok (None) => "refused",
        Ok (Some (snapshot)) if matches! (snapshot . status,
          crate::maintenance::save_journal::SaveOperationStatus::PreparedUnAuthorized
          | crate::maintenance::save_journal::SaveOperationStatus::StagingUnAuthorized) => "refused",
        _ => "blocked",
      };
      (tag_sexp_response (TcpToClient::SaveResult,
        &empty_response_sexp (&details, &[], &point) . to_string ()), state)
    }
  };
  finish_save_response (stream, runtime, env, operation, control, &response, state);
}

pub(crate) fn save_refusal_response (reason : &str, request : &str) -> String {
  tag_sexp_response (TcpToClient::SaveResult,
    &empty_response_sexp (reason, &[], &save_point_position_from_request (request)) . to_string ())
}

fn finish_save_response (
  stream : &mut TcpStream,
  runtime : &ServerRuntime,
  env : &SkgEnv,
  operation : &crate::runtime::save_operations::SaveOperation,
  control : &crate::runtime::MutationControl,
  response : &str,
  state : &str,
) {
  let response : String = operation . tag_response (response, state);
  let recorded : Result<(), String> = match state {
    "committed" => runtime . publish_selected_from_env (control, env)
      . and_then (|_| operation . commit (&response, &format! ("graph-{}-manifest-{}",
        env . in_rust_graph . load_full () . graph_generation . get (),
        env . in_rust_graph . load_full () . manifest_revision . get ()))),
    "refused" => operation . refuse (&response),
    _ => Err ("save has unresolved authorized effects; use save operation status to recover" . into ()),
  };
  match recorded {
    Ok (( )) => { let _ = send_response_with_length_prefix (stream, &response); }
    Err (reason) => {
      let _ = control . block (reason . clone ());
      let blocked = operation . tag_response (
        &tag_sexp_response (TcpToClient::SaveResult,
          &empty_response_sexp (&reason, &[], &None) . to_string ()), "blocked");
      let _ = send_response_with_length_prefix (stream, &blocked);
    }
  }
}

/// Create an s-expression with nil content and an error message.
fn empty_response_sexp (
  error_buffer_content : &str,
  warnings             : &[String],
  save_point_position   : &Option<SavePointPosition>,
) -> Sexp {
  let mut items : Vec<Sexp> = vec! [
    Sexp::List ( vec! [
      Sexp::Atom ( Atom::S ( "content" . to_string () )),
      Sexp::Atom ( Atom::S ( "nil" . to_string () )) ] ),
    Sexp::List ( vec! [
      Sexp::Atom ( Atom::S ( "errors" . to_string () )),
      Sexp::List ( vec! [
        Sexp::Atom ( Atom::S (
          error_buffer_content . to_string () )) ] ) ] ),
    Sexp::List ( vec! [
      Sexp::Atom ( Atom::S ( "warnings" . to_string () )),
      Sexp::List (
        warnings . iter ()
          . map ( |warning| Sexp::Atom (
            Atom::S ( warning . clone () )) )
          . collect () ) ] ) ];
  if let Some (point_position) = save_point_position {
    push_save_point_position_to_sexp_items (
      &mut items, point_position ); }
  Sexp::List (items) }

fn push_save_point_position_to_sexp_items (
  items          : &mut Vec<Sexp>,
  point_position : &SavePointPosition,
) {
  items . push (
    Sexp::List ( vec! [
      Sexp::Atom ( Atom::S (
        "point-lines-below-focused-headline" . to_string () )),
      Sexp::Atom ( Atom::I (
        point_position . point_lines_below_focused_headline
        as i64 )), ] ));
  items . push (
    Sexp::List ( vec! [
      Sexp::Atom ( Atom::S (
        "point-column" . to_string () )),
      Sexp::Atom ( Atom::I (
        point_position . point_column
        as i64 )), ] ));
  items . push (
    Sexp::List ( vec! [
      Sexp::Atom ( Atom::S (
        "point-screen-lines-below-window-start" . to_string () )),
      Sexp::Atom ( Atom::I (
        point_position . point_screen_lines_below_window_start
        as i64 )), ] )); }

fn save_point_position_from_request (
  request : &str,
) -> Option<SavePointPosition> {
  let point_lines_below_focused_headline : usize =
    nat_from_request (
      request,
      "point-lines-below-focused-headline" ) ?;
  let point_column : usize =
    nat_from_request (
      request,
      "point-column" ) ?;
  let point_screen_lines_below_window_start : usize =
    nat_from_request (
      request,
      "point-screen-lines-below-window-start" ) ?;
  Some ( SavePointPosition {
    point_lines_below_focused_headline,
    point_column,
    point_screen_lines_below_window_start, } ) }

fn nat_from_request (
  request : &str,
  key     : &str,
) -> Option<usize> {
  value_from_request_sexp (key, request) . ok () ? . parse () . ok () }

fn requested_save_authority (
  request : &str,
) -> Result<RequestedSaveAuthority, String> {
  let integer = |key : &str| -> Result<u64, String> {
    value_from_request_sexp (key, request)? . parse::<u64> ()
      . map_err (|_| format! ("{} must be an unsigned integer", key)) };
  Ok (RequestedSaveAuthority {
    buffer_id: value_from_request_sexp ("client-buffer-id", request)?,
    kind: value_from_request_sexp ("view-kind", request)?,
    graph_generation: integer ("graph-generation")?,
    server_revision: integer ("server-revision")?,
    application_token: integer ("client-application-token")?,
  })
}

fn validate_save_authority (
  requested  : &RequestedSaveAuthority,
  view_uri   : &Result<ViewUri, String>,
  views_state : &ViewsState,
  current_graph_generation : u64,
) -> Result<(), SaveError> {
  let uri = view_uri . as_ref () . map_err (|reason|
    SaveError::StaleViewAuthority (reason . clone ()))?;
  let Some (state) = views_state . open_views . views . get (uri) else {
    if requested . kind == "new-empty-content-view"
       && matches! (uri, ViewUri::ContentView (_))
       && requested . server_revision == 0
       && requested . application_token == 1
       && requested . graph_generation == current_graph_generation
    { return Ok (( )); }
    return Err (SaveError::StaleViewAuthority (format! (
      "view '{}' is not registered by the server",
      uri . repr_in_client ()))); };
  if !state . writes_admitted {
    return Err (SaveError::StaleViewAuthority (
      "this query result has no write authority; explicitly reopen a fresh view" . into ())); }
  if state . save_base . is_none () {
    return Err (SaveError::StaleViewAuthority (
      "this view has no retained semantic save base; preserve its text and open a fresh view" . into ())); }
  if state . graph_generation != requested . graph_generation
     || state . revision != requested . server_revision
     || state . client_application_token != requested . application_token
  {
    return Err (SaveError::StaleViewAuthority (format! (
      "view '{}' expected graph/revision/token {}/{}/{}, client supplied {}/{}/{}",
      uri . repr_in_client (), state . graph_generation, state . revision,
      state . client_application_token, requested . graph_generation,
      requested . server_revision, requested . application_token)));
  }
  if let Some (buffer_id) = &state . client_buffer_id {
    if buffer_id != &requested . buffer_id {
      return Err (SaveError::StaleViewAuthority (format! (
        "view '{}' belongs to client buffer {}, not {}",
        uri . repr_in_client (), buffer_id, requested . buffer_id))); }}
  Ok (( ))
}

/// Whether this save request carries '(fork-approved . "true")', set by
/// the client when the user approves a fork-confirmation. Absent or any
/// other value means not approved.
fn fork_approved_from_request (
  request : &str,
) -> bool {
  value_from_request_sexp ("fork-approved", request)
    . map ( |v| v == "true" )
    . unwrap_or (false) }

/// Parse the optional '(fork-sources ((N . SOURCE) ...))' field of a
/// save request into a map from each forked node's id N to the OWNED
/// source the user chose for its clone (rotated, or left at the default,
/// in the fork-confirmation buffer). Absent on an ordinary save and on
/// the first (unapproved) save -- then every clone source resolves by
/// inference-else-default. The chosen source is validated owned + active
/// downstream in 'validate_fork_specs'.
fn fork_sources_from_request (
  request : &str,
) -> HashMap<ID, SourceName> {
  let mut map : HashMap<ID, SourceName> = HashMap::new ();
  let Ok (sexp) = sexp::parse (request) else { return map; };
  let Sexp::List (items) = sexp else { return map; };
  for item in & items {
    let Sexp::List (pair) = item else { continue; };
    let Some ( Sexp::Atom ( Atom::S (k) )) = pair . first ()
      else { continue; };
    if k != "fork-sources" { continue; }
    let Some ( Sexp::List (entries) ) = pair . get (1)
      else { continue; };
    for entry in entries {
      // Each entry is a dotted pair (N . SOURCE), which the sexp crate
      // parses as the 3-element list [N, ".", SOURCE].
      if let Sexp::List (kv) = entry {
        if let [ Sexp::Atom ( Atom::S (id) ),
                 Sexp::Atom ( Atom::S (dot) ),
                 Sexp::Atom ( Atom::S (source) ) ] = & kv [..]
        { if dot == "." {
            map . insert ( ID ( id . clone () ),
                           SourceName::from ( source . as_str () )); }} }} }
  map }

/// If 'err' is a buffer-validation SaveError that carries no warnings
/// of its own, attach the save's parse-time warnings so a failed save
/// still reports them. A no-op for other errors or when there are no
/// parse warnings.
fn backfill_parse_warnings (
  err            : Box<dyn Error>,
  parse_warnings : &[String],
) -> Box<dyn Error> {
  if parse_warnings . is_empty () { return err; }
  match err . downcast::<SaveError> () {
    Ok (boxed) => match *boxed {
      SaveError::BufferValidationErrors { errors, warnings } => {
        let warnings : Vec<String> =
          if warnings . is_empty () { parse_warnings . to_vec () }
          else { warnings };
        Box::new (SaveError::BufferValidationErrors { errors, warnings }) }
      other => Box::new (other), },
    Err (err) => err, } }

/// PURPOSE: Process the buffer that a user wants to save.
/// - "save": Update dbs and filesystem.
/// - "rerender": Create a new buffer for the user.
/// ERRORS: If the buffer is invalid.
/// COMPLEX:
/// - Validation must happen at many stages.
/// - NodeMerges must follow the execution of other save instructions, because the user may have updated one of the nodes to be merged.
/// - complete_viewforest is complex: it is one level-order BFS in which each node is completed at its own visit (content, cols, view requests, inline diff), then a postorder prune sweep removes the empty self-deletable nodes.
pub async fn update_from_and_rerender_buffer (
  stream                      : &mut TcpStream,
  org_buffer_text             : &str,
  env                         : &mut SkgEnv,
  diff_mode_enabled           : bool,
  viewuri_from_request_result : &Result<ViewUri, String>,
  views_state                  : &mut ViewsState,
  active_source_set            : Option<&ActiveSourceSet>,
  fork_approved                : bool, // true once the user has approved the forks (a re-issued save); false on the first save, which returns a fork-confirmation instead of committing.
  fork_sources                 : &HashMap<ID, SourceName>, // per-fork clone sources the user chose in the confirmation buffer (keyed by N's pid); empty otherwise.
) -> Result<SaveResponse, Box<dyn Error>> {
  let no_hoist_approvals : HashSet<ID> = HashSet::new ();
  update_from_and_rerender_buffer_with_hoist_approval (
    stream, org_buffer_text, env, diff_mode_enabled,
    viewuri_from_request_result, views_state, active_source_set,
    fork_approved, fork_sources, &no_hoist_approvals ) . await
}

pub async fn update_from_and_rerender_buffer_with_hoist_approval (
  stream                      : &mut TcpStream,
  org_buffer_text             : &str,
  env                         : &mut SkgEnv,
  diff_mode_enabled           : bool,
  viewuri_from_request_result : &Result<ViewUri, String>,
  views_state                  : &mut ViewsState,
  active_source_set            : Option<&ActiveSourceSet>,
  fork_approved                : bool,
  fork_sources                 : &HashMap<ID, SourceName>,
  hoist_approved_pids         : &HashSet<ID>,
) -> Result<SaveResponse, Box<dyn Error>> {
  update_from_and_rerender_buffer_with_approvals (
    stream, org_buffer_text, env, diff_mode_enabled,
    viewuri_from_request_result, views_state, active_source_set,
    fork_approved, fork_sources, hoist_approved_pids,
    &HashSet::new (), None, None ) . await
}

pub async fn update_from_and_rerender_buffer_with_approvals (
  stream                      : &mut TcpStream,
  org_buffer_text             : &str,
  env                         : &mut SkgEnv,
  diff_mode_enabled           : bool,
  viewuri_from_request_result : &Result<ViewUri, String>,
  views_state                  : &mut ViewsState,
  active_source_set            : Option<&ActiveSourceSet>,
  fork_approved                : bool,
  fork_sources                 : &HashMap<ID, SourceName>,
  hoist_approved_pids         : &HashSet<ID>,
  scalar_approved_pids        : &HashSet<ID>,
  requested_authority         : Option<&RequestedSaveAuthority>,
  collateral_scheduler    : Option<&mut CollateralScheduler>,
) -> Result<SaveResponse, Box<dyn Error>> {
  update_from_and_rerender_buffer_with_approvals_with_operation (
    stream, org_buffer_text, env, diff_mode_enabled, viewuri_from_request_result, views_state, active_source_set, fork_approved, fork_sources, hoist_approved_pids, scalar_approved_pids, requested_authority, collateral_scheduler, None) . await
}

pub(crate) async fn update_from_and_rerender_buffer_with_approvals_with_operation (
  stream                      : &mut TcpStream,
  org_buffer_text             : &str,
  env                         : &mut SkgEnv,
  diff_mode_enabled           : bool,
  viewuri_from_request_result : &Result<ViewUri, String>,
  views_state                  : &mut ViewsState,
  active_source_set            : Option<&ActiveSourceSet>,
  fork_approved                : bool,
  fork_sources                 : &HashMap<ID, SourceName>,
  hoist_approved_pids         : &HashSet<ID>,
  scalar_approved_pids        : &HashSet<ID>,
  requested_authority         : Option<&RequestedSaveAuthority>,
  mut collateral_scheduler    : Option<&mut CollateralScheduler>,
  operation : Option<&crate::runtime::save_operations::SaveOperation>,
) -> Result<SaveResponse, Box<dyn Error>> {
  let planning_selected : Arc<SelectedStoreState> = env . in_rust_graph . load_full ();
  if let Some (authority) = requested_authority {
    validate_save_authority (
      authority, viewuri_from_request_result, views_state,
      planning_selected . graph_generation . get ())?; }
  if diff_mode_enabled { // diff mode is undefined for merge commits
    let sources : Vec<SourceName> =
      env . config . sources . keys() . cloned() . collect();
    validate_no_merge_commits ( &sources, &env . config )
      . map_err ( |e| -> Box<dyn Error> { e . into() } ) ?; }

  let ( viewforest, save_plan, parse_warnings )
    : ( ViewForest, SavePlan, Vec<String> ) =
    { let _span : tracing::span::EnteredSpan = tracing::info_span!(
            "buffer_to_validated_saveplan"
          ) . entered();
        buffer_to_validated_saveplan_with_fork_sources (
          &planning_selected . graph, org_buffer_text, &env . config,
          active_source_set, fork_sources, &planning_selected . manifest ) . await
      } . map_err (
        |e| Box::new (e) as Box<dyn Error> ) ?;
  if viewforest . is_empty ()
    { return Err ( "Nothing to save found in org_buffer_text"
                   . into() ); }
  let SavePlan {
    define_nodes       : mut nonmerge_defineNodes,
    nodeMerge_instructions : nodeMerges,
    source_moves,
    fork_specs }
    = save_plan;
  { // Delete propagation adds collateral writes. Derive them before the
    // disk Hoist classification so "touched pids" means every telescope
    // this save will actually rewrite, not only what appeared in the buffer.
    let graph_snap = env . in_rust_graph . load_full () . graph . clone ();
    apply_delete_propagation_cleanup (
      &mut nonmerge_defineNodes, &graph_snap ); }
  let hoist_candidates : Vec<HoistCandidate> =
    hoist_candidates_from_selected (
      &env . in_rust_graph_snapshot (), &nonmerge_defineNodes, &nodeMerges, &env . config ) ?;
  // In particular, make a dirty nodeMerge acquiree clean before the merge
  // copies its text into a fresh preservation node and deletes it.
  nonmerge_defineNodes . extend (
    repair_saves_for_unwritten_candidates (
      &hoist_candidates, &nonmerge_defineNodes, &env . in_rust_graph_snapshot ())? );
  // Proposed forks: editing a foreign node
  // N is a request to clone it. The clone C commits with the rest of the
  // save -- its 'overrides_view_of = [N]' edge rides in the same
  // DefineNodes, so the touched-override-invariant check (which reads the
  // simulated post-save graph) sees C before validating.
  let nonmerge_defineNodes : Vec<DefineNode> = {
    let mut nodes : Vec<DefineNode> = nonmerge_defineNodes;
    for spec in &fork_specs {
      nodes . push ( DefineNode::Save ( spec . clone . clone () )); }
    nodes };

  let define_nodes : Vec<DefineNode> = crate::save::combined_save_definitions (
    &env . in_rust_graph_snapshot (), nonmerge_defineNodes . clone (), &nodeMerges);

  let all_filesystem_outputs : Vec<DefineNode> =
    nonmerge_defineNodes . iter () . cloned ()
    . chain (nodeMerges . iter () . flat_map (|node_merge| node_merge . to_vec ()))
    . collect ();
  if requested_authority . is_some () {
    if let Ok (uri) = viewuri_from_request_result {
      if let Some (state) = views_state . open_views . views . get (uri) {
        let base : &ViewSaveBase = state . save_base . as_ref ()
          . expect ("save admission verified the retained semantic base");
        let source_set : &ActiveSourceSet = active_source_set . ok_or_else (||
          SaveError::StaleViewAuthority ("save requires its active source interpretation" . into ()))?;
        validate_save_dependencies (
          base, &planning_selected, &env . config, source_set,
          &state . viewforest, &viewforest, &all_filesystem_outputs,
          &source_moves, &fork_specs)
          . map_err (|reason| {
            tracing::debug! (%reason, "retained save dependency check refused");
            SaveError::StaleViewAuthority (
              "a save dependency changed since this view was accepted; preserve your edits and reopen a current view before saving" . into ()) })?;
      }
    }
  }
  if hoist_needs_confirmation (
      &hoist_candidates, hoist_approved_pids ) {
    return Ok ( SaveResponse {
      saved_view          : String::new (),
      errors              : Vec::new (),
      warnings            : parse_warnings,
      save_point_position : None,
      fork_confirmation   : None,
      hoist_confirmation  : Some (hoist_candidates),
      scalar_release_confirmation : None,
    } ); }
  if ! fork_specs . is_empty () && ! fork_approved {
    // A save that found forks but was not pre-approved commits NOTHING.
    // Return a read-only fork-confirmation buffer; the client shows it,
    // and on approval re-issues the save with (fork-approved . "true").
    // (Monogamy and source validation already ran in
    // buffer_to_validated_saveplan, so every fork here is admissible.)
    return Ok ( SaveResponse {
      saved_view          : build_fork_confirmation_buffer (&fork_specs),
      errors              : Vec::new (),
      warnings            : parse_warnings,
      save_point_position : None,
      fork_confirmation   : Some (
        format! ( "{} node(s) will be forked. Save again to approve, \
                   or kill this buffer to decline.",
                  fork_specs . len () )),
      hoist_confirmation  : None,
      scalar_release_confirmation : None, } ); }
  preflight_fs_from_saveinstructions_with_hoist_approval (
    &all_filesystem_outputs, &source_moves, &env . config,
    hoist_approved_pids, &planning_selected)?;

  { // update the graph. Context origin types (for search ranking) are
    // computed from the post-save in-Rust graph and written inside the
    // single Tantivy index pass, so there is no separate context pass.
    let _span : tracing::span::EnteredSpan = tracing::info_span!(
      "update_graph_including_nodeMerges" ). entered();
    update_graph_including_nodeMerges_with_operation (
      nonmerge_defineNodes . clone(),
      &nodeMerges,
      &source_moves,
      env . config . clone(),
      &mut env . tantivy_index,
      &env . in_rust_graph,
      hoist_approved_pids,
      planning_selected . graph_generation . get (), operation ) . await
    // Warnings always accompany errors: a post-parse validation
    // failure (e.g. the override-invariant check) back-fills the
    // save's parse-time warnings, which the error itself did not see.
    . map_err ( |e| backfill_parse_warnings (e, &parse_warnings) ) ?; }
  env . searcher = env . in_rust_graph . load_full () . searcher . clone ()
    . expect ("completed save has a matching Searcher");


  { let _span : tracing::span::EnteredSpan = tracing::info_span!(
      "coherence_debug_assert" ). entered();
    debug_assert! (
      // TODO | PITFALL: This is quite a weak assertion.
      // PURPOSE: The in-Rust graph must already reflect every Save and Delete in 'define_nodes' by the time this function runs. Violating this invariant (e.g. by reordering the save pipeline so that 'update_views_after_save' runs before 'apply_definenodes') would let the rerender read stale NodeCompletes from the in-Rust graph.
      in_rust_graph_coherent_with_save_instructions (
        &env . in_rust_graph_snapshot (),           &define_nodes
        ) . is_ok (),
      "update_views_after_save: in-Rust graph not coherent with define_nodes" ); }

  { let _span : tracing::span::EnteredSpan = tracing::info_span!(
      "update_views_after_save" ). entered();
    let mut response : SaveResponse =
      update_views_after_save (
        stream,
        viewforest,
        define_nodes,
        diff_mode_enabled,
        env,
        viewuri_from_request_result,
        views_state,
        active_source_set,
        scalar_approved_pids,
        collateral_scheduler . as_deref_mut () ) . await ?;
    { // Nonfatal parse warnings (e.g. discarded col headline text)
      // precede the completion-repair warnings.
      let mut warnings : Vec<String> = parse_warnings;
      warnings . extend ( response . warnings );
      response . warnings = warnings; }
    Ok (response) } }

/// Check if any source's HEAD is a merge commit.
/// Returns an error message if so,
/// as diff computation is ambiguous for merge commits.
pub fn validate_no_merge_commits (
  sources : &[SourceName],
  config  : &SkgConfig,
) -> Result<(), String> {
  for source in sources { // Get the source path from config
    if let Some (source_config) = config . sources . get (source) {
      let source_path : &Path =
        Path::new ( &source_config . path );
      if let Some (repo) = open_repo (source_path) {
        match head_is_merge_commit (&repo) {
          Ok (true) => {
            return Err ( format! (
              "Cannot compute diff: HEAD is a merge commit in source '{}'.",
              source )); },
          Ok (false) => {},
          Err (e) => { // Git error - log but continue
            tracing::warn! ( "Could not check merge commit status for '{}': {}",
                        source, e ); }} }} }
  Ok (( )) }

pub fn compute_diff_for_every_source (
  config : &SkgConfig
) -> HashMap<SourceName, SourceDiff> {
  let mut source_diffs : HashMap<SourceName, SourceDiff> =
    HashMap::new();
  for (source_name, source_config) in config . sources . iter () {
    let source_path : &Path =
      Path::new ( &source_config . path );
    match compute_diff_for_source (source_path, source_name) {
      Ok (diff) => {
        source_diffs . insert ( source_name . clone(), diff ); },
      Err (e) => { // Log error but continue with other sources
        tracing::warn! (
          "Failed to compute diff for source '{}': {}",
          source_name, e ); }} }
  source_diffs }

/// Build a map from ID to HOME for all deleted files: the source of
/// nodes that exist in git HEAD but not in the worktree.
///
/// Walks the sources in privacy order and keeps the FIRST (most
/// public) hit, because deleting a node deletes its whole
/// TELESCOPE -- every owned section, in as many sources as hold one
/// ('delete_all_nodes_from_fs') -- so one id routinely appears as
/// deleted in several sources at once. Iterating 'source_diffs'
/// (a HashMap) and letting the last writer win answered that
/// arbitrarily, per process.
pub fn deleted_ids_to_source (
  source_diffs : &HashMap<SourceName, SourceDiff>,
  config       : &SkgConfig,
) -> HashMap<ID, SourceName> {
  let mut result : HashMap<ID, SourceName> =
    HashMap::new();
  for source_name in config . ordered_sources () {
    let Some (source_diff) : Option<&SourceDiff> =
      source_diffs . get (&source_name) else { continue; };
    for diffs in [ &source_diff . staged,
                   &source_diff . unstaged ] {
      for (path, nodecomplete_diff) in diffs {
        if nodecomplete_diff . status == GitDiffStatus::Deleted {
          if let Some (stem) = path . file_stem() {
            let id : ID = ID ( stem . to_string_lossy()
                               . into_owned() );
            result . entry (id) // most public wins
              . or_insert_with ( || source_name . clone() ); }} }} }
  result }

/// Every other open view. A save can affect inverse/generated context in a
/// view which shares no currently rendered PID with the saved forest, so PID
/// overlap is not a safe pre-transition lock boundary.
fn uris_of_views_to_lock (
  viewuri_from_request_result : &Result<ViewUri, String>,
  views_state                  : &ViewsState,
) -> Vec<ViewUri> {
  let saved_uri : &ViewUri = match viewuri_from_request_result {
    Ok (uri) => uri,
    Err (_)  => return Vec::new () };
  views_state . open_views . views . keys ()
    . filter (|uri| *uri != saved_uri)
    . cloned ()
    . collect () }

#[cfg(test)]
mod query_authority_tests {
  use super::*;
  use crate::types::views_state::OpenViews;

  #[test]
  fn unregistered_query_uri_cannot_claim_new_content_save_authority () {
    let views : ViewsState = ViewsState { diff_mode_enabled: false, open_views: OpenViews::new (), };
    let requested : RequestedSaveAuthority = RequestedSaveAuthority {
      buffer_id: uuid::Uuid::new_v4 () . to_string (), kind: "new-empty-content-view" . into (),
      graph_generation: 2, server_revision: 0, application_token: 1, };
    let content : Result<ViewUri, String> = Ok (ViewUri::ContentView (uuid::Uuid::new_v4 () . to_string ()));
    assert! (validate_save_authority (&requested, &content, &views, 2) . is_ok ());
    let query : Result<ViewUri, String> = Ok (ViewUri::from_client_string (
      format! ("search:wait:{}", uuid::Uuid::new_v4 ())));
    assert! (validate_save_authority (&requested, &query, &views, 2) . is_err ());
  }
}
