//! Ordinary save computation uses retained inputs; settlement checks live views.

use super::{RequestedSaveAuthority, SaveExecution, SaveResponse,
  apply_save_from_buffer_with_approvals, empty_response_sexp,
  fork_approved_from_request, fork_sources_from_request,
  hoist_approved_pids_from_request, requested_save_authority,
  response_for_save_result, save_refusal_response, scalar_approved_pids_from_request,
  uris_of_views_to_lock, validate_save_authority};
use crate::maintenance::BufferKind;
use crate::runtime::{MutationControl, SelectedRuntimeSnapshot, ServerRuntime};
use crate::runtime::save_operations::SaveOperation;
use crate::serve::ViewsState;
use crate::serve::handlers::maintenance_protocol::skg_save_policy_refusal;
use crate::serve::protocol::TcpToClient;
use crate::serve::util::{DetachedRequestContext, format_lock_views_sexp,
  send_response_with_length_prefix, tag_sexp_response, take_request_context,
  view_uri_from_request};
use crate::source_sets::ActiveSourceSet;
use crate::types::views_state::{ViewSaveBase, ViewState, ViewUri};
use crate::types::tree::forest::ViewForest;
use crate::types::save::DefineNode;
use crate::update_buffer::{PreparedSaveViewUpdate, apply_save_view_update,
  prepare_save_view_update};
use futures::executor::block_on;
use std::error::Error;
use std::net::TcpStream;
use std::sync::Arc;
use std::sync::mpsc::Sender;
use std::thread;

struct PreparedSave {
  request : String,
  content : String,
  operation : SaveOperation,
  control : MutationControl,
  before : Arc<SelectedRuntimeSnapshot>,
  views : ViewsState,
  active : ActiveSourceSet,
  authority : RequestedSaveAuthority,
  uri : Result<ViewUri, String>,
  client_session : Option<String>,
  locked_uris : Vec<ViewUri>,
}

pub(crate) fn dispatch_save_request (
  stream : &mut TcpStream,
  request : &str,
  content : &str,
  runtime : &Arc<ServerRuntime>,
  completions : &Sender<String>,
) {
  let prepared : PreparedSave = match prepare_save (request, content, runtime) {
    Ok (prepared) => prepared,
    Err (response) => {
      let _ = send_response_with_length_prefix (stream, &response);
      return;
    }
  };
  let _ = send_response_with_length_prefix (stream, &tag_sexp_response (
    TcpToClient::SaveLock, &format_lock_views_sexp (&prepared . locked_uris)));
  let _ = send_response_with_length_prefix (stream,
    "((response-type request-yield) (frame-kind request-yield))");
  let Some (context) : Option<DetachedRequestContext> = take_request_context () else {
    let response : String = refuse_reserved_save (&prepared,
      "save dispatch lost its request context");
    let _ = send_response_with_length_prefix (stream, &response);
    return;
  };
  let worker_runtime : Arc<ServerRuntime> = Arc::clone (runtime);
  let worker_context : DetachedRequestContext = context . clone ();
  let completions : Sender<String> = completions . clone ();
  let failed_control : MutationControl = prepared . control . clone ();
  let failed_operation : SaveOperation = prepared . operation . clone ();
  let failed_request : String = prepared . request . clone ();
  let dispatched : std::io::Result<thread::JoinHandle<()>> = thread::Builder::new ()
    . name ("skg-save" . into ()) . spawn (move || {
      for response in run_save (&worker_runtime, prepared) {
        let _ = completions . send (worker_context . decorate_response (&response));
      }
    });
  if let Err (error) = dispatched {
    let response : String = refuse_operation (&failed_operation, &failed_request,
      &format! ("could not start save worker: {}", error));
    let _ = failed_control . finish ();
    let _ = send_response_with_length_prefix (stream, &context . decorate_response (&response));
  }
}

fn prepare_save (
  request : &str,
  content : &str,
  runtime : &ServerRuntime,
) -> Result<PreparedSave, String> {
  let before : Arc<SelectedRuntimeSnapshot> = runtime . selected_snapshot ();
  let active : ActiveSourceSet = runtime . interactive . lock () . unwrap ()
    . active_source_set . clone ();
  let operation : SaveOperation = SaveOperation::from_request (
    request, content, &before . env . config, &active)
    . map_err (|reason| save_refusal_response (&reason, request))?;
  match operation . recorded_response () {
    Ok (Some (response)) => return Err (response),
    Err (reason) => return Err (blocked_response (&operation, request, &reason)),
    Ok (None) => {}
  }
  let control : MutationControl = match runtime . reserve_mutation (
    operation . operation_id . clone (), before . selected . graph_generation,
    before . selected . manifest_revision) {
    Ok (control) => control,
    Err (reason) => {
      let same_operation_reserved : bool = runtime . publication_with_mutation () . 4
        . is_some_and (|mutation| mutation . operation_id == operation . operation_id);
      return Err (if same_operation_reserved {
        blocked_response (&operation, request, &reason)
      } else {
        match operation . recorded_response () {
          Ok (Some (response)) => response,
          Err (error) => blocked_response (&operation, request, &error),
          Ok (None) => refuse_operation (&operation, request, &reason),
        }
      });
    }
  };
  let captured : Result<PreparedSave, String> = (|| {
    runtime . validate_session_authority (request)?;
    if let Some (reason) = skg_save_policy_refusal (&runtime . maintenance_snapshot () . state) {
      return Err (reason);
    }
    let authority : RequestedSaveAuthority = requested_save_authority (request)?;
    let uri : Result<ViewUri, String> = view_uri_from_request (request);
    let mut interactive = runtime . interactive . lock ()
      . map_err (|_| "interactive session poisoned" . to_string ())?;
    let current_operation : SaveOperation = SaveOperation::from_request (
      request, content, &before . env . config, &interactive . active_source_set)?;
    if !operation . matches_interpretation (&current_operation) {
      return Err ("source interpretation changed before save admission" . into ());
    }
    validate_save_authority (&authority, &uri, &interactive . views,
      before . selected . graph_generation . get ()) . map_err (|error| error . to_string ())?;
    let view_uri : &ViewUri = uri . as_ref () . map_err (Clone::clone)?;
    if !interactive . views . open_views . views . contains_key (view_uri) {
      // A validated new-empty target gets an identity before dispatch. Closing
      // it while the worker runs then removes a real entry rather than being
      // indistinguishable from its original absence.
      let presentation : u64 = interactive . collateral_scheduler . presentation_generation ();
      interactive . views . open_views . register_view_with_authority (
        &before . selected . graph, view_uri . clone (), ViewForest::new (), &[],
        authority . graph_generation, presentation, authority . application_token,
        BufferKind::ContentView, active . name . 0 . clone (), None);
      let state : &mut ViewState = interactive . views . open_views . views . get_mut (view_uri)
        . expect ("save admission registered its new view identity");
      state . retain_save_base (ViewSaveBase {
        selected: before . selected . graph_base (), config: before . env . config . clone (),
        source_set: active . name . 0 . clone (),
      })?;
    }
    if let Some (state) = interactive . views . open_views . views . get_mut (view_uri) {
      state . client_buffer_id = Some (authority . buffer_id . clone ());
    }
    Ok (PreparedSave {
      request: request . into (), content: content . into (),
      operation: operation . clone (), control: control . clone (), before: before . clone (),
      views: ViewsState { diff_mode_enabled: interactive . views . diff_mode_enabled,
        open_views: interactive . views . open_views . snapshot_view (view_uri) },
      active, authority, locked_uris: uris_of_views_to_lock (&uri, &interactive . views),
      uri, client_session: interactive . attached_client . as_ref ()
        . map (|client| client . session_id . clone ()),
    })
  }) ();
  captured . map_err (|reason| {
    let response : String = refuse_operation (&operation, request, &reason);
    let _ = control . finish ();
    response
  })
}

fn run_save (
  runtime : &ServerRuntime,
  prepared : PreparedSave,
) -> Vec<String> {
  let mut frames : Vec<String> = Vec::new ();
  let result : Result<(), String> = runtime . with_reserved_writer_transition (
    prepared . before . clone (), prepared . control . clone (), |env, control| {
      let operation : SaveOperation = prepared . operation . clone () . with_control (control . clone ());
      let mut effects_applied : bool = false;
      let mut affected_nodes : Vec<DefineNode> = Vec::new ();
      let result : Result<PreparedSaveViewUpdate, Box<dyn Error>> = (|| {
        let current_active : ActiveSourceSet = runtime . interactive . lock () . unwrap ()
          . active_source_set . clone ();
        let current_operation : SaveOperation = SaveOperation::from_request (
          &prepared . request, &prepared . content, &env . config, &current_active)?;
        if !operation . matches_interpretation (&current_operation) {
          return Err ("source interpretation changed before save execution" . into ());
        }
        let execution : SaveExecution = block_on (apply_save_from_buffer_with_approvals (
          &prepared . content, env, prepared . views . diff_mode_enabled, &prepared . uri,
          &prepared . views, Some (&prepared . active), fork_approved_from_request (&prepared . request),
          &fork_sources_from_request (&prepared . request),
          &hoist_approved_pids_from_request (&prepared . request), Some (&prepared . authority), Some (&operation)))?;
        match execution {
          SaveExecution::Confirmation (response) => Ok (PreparedSaveViewUpdate::Confirmation (response)),
          SaveExecution::Applied { viewforest, define_nodes, parse_warnings } => {
            effects_applied = true;
            affected_nodes = define_nodes . clone ();
            let mut rendered : PreparedSaveViewUpdate = block_on (prepare_save_view_update (
              viewforest, define_nodes, prepared . views . diff_mode_enabled, env,
              &prepared . uri, &prepared . views, Some (&prepared . active),
              &scalar_approved_pids_from_request (&prepared . request), true))?;
            rendered . prepend_warnings (parse_warnings);
            runtime . publish_selected_from_env (control, env)?;
            Ok (rendered)
          }
        }
      }) ();
      let (response, state) : (String, &str) = {
        let mut interactive = runtime . interactive . lock () . unwrap ();
        let current_session : Option<&str> = interactive . attached_client . as_ref ()
          . map (|client| client . session_id . as_str ());
        let same_view : bool = current_session == prepared . client_session . as_deref ()
          && same_view_base (&prepared . views, &interactive . views, &prepared . uri);
        if result . is_ok () && !same_view {
          if effects_applied {
            let crate::runtime::interactive_session::InteractiveSession {
              views, collateral_scheduler, .. } = &mut *interactive;
            let refresh = collateral_scheduler . replace_after_transition (
              None, views, env, &affected_nodes, &prepared . active,
              &scalar_approved_pids_from_request (&prepared . request));
            let _ = refresh . send_to (&mut frames);
          }
          (fresh_view_response (if effects_applied {
            "The save committed after this view changed or closed. Preserve its text and open a fresh view."
          } else {
            "This view changed or closed before the save could finish. Nothing was saved; open a fresh view."
          }), if effects_applied { "committed" } else { "refused" })
        } else {
          let result : Result<SaveResponse, Box<dyn Error>> = result . map (|rendered| {
            let crate::runtime::interactive_session::InteractiveSession {
              views, collateral_scheduler, .. } = &mut *interactive;
            apply_save_view_update (&mut frames, rendered, env, views,
              Some (&prepared . active), &scalar_approved_pids_from_request (&prepared . request),
              Some (collateral_scheduler))
          });
          let crate::runtime::interactive_session::InteractiveSession {
            views, collateral_scheduler, .. } = &mut *interactive;
          response_for_save_result (result, &prepared . request, &prepared . uri,
            &prepared . authority, env, views, collateral_scheduler, runtime, &operation)
        }
      };
      let response : String = operation . tag_response (&response, state);
      let recorded : Result<(), String> = match state {
        "committed" => operation . commit (&response, &format! ("graph-{}-manifest-{}",
          env . in_rust_graph . load_full () . graph_generation . get (),
          env . in_rust_graph . load_full () . manifest_revision . get ())),
        "refused" => operation . refuse (&response),
        _ => Err ("save has unresolved authorized effects; use save operation status to recover" . into ()),
      };
      match recorded {
        Ok (()) => frames . push (response),
        Err (reason) => {
          let _ = control . block (reason . clone ());
          frames . clear ();
          frames . push (blocked_response (&operation, &prepared . request, &reason));
        }
      }
    });
  if let Err (reason) = result {
    if frames . is_empty () {
      frames . push (blocked_response (&prepared . operation, &prepared . request, &reason));
    }
  }
  frames
}

fn same_view_base (
  before : &ViewsState,
  current : &ViewsState,
  uri : &Result<ViewUri, String>,
) -> bool {
  let Ok (uri) : &Result<ViewUri, String> = uri else { return false; };
  match (before . open_views . views . get (uri), current . open_views . views . get (uri)) {
    (None, None) => true,
    (Some (before), Some (current)) => same_authority (before, current),
    _ => false,
  }
}

fn same_authority (
  before : &ViewState,
  current : &ViewState,
) -> bool {
  before . incarnation == current . incarnation
    && before . revision == current . revision
    && before . graph_generation == current . graph_generation
    && before . client_application_token == current . client_application_token
    && before . client_buffer_id == current . client_buffer_id
    && before . writes_admitted == current . writes_admitted
    && before . kind == current . kind && before . source_set == current . source_set
    && match (&before . save_base, &current . save_base) {
      (Some (before), Some (current)) => Arc::ptr_eq (&before . selected . graph, &current . selected . graph)
        && before . selected . manifest_revision == current . selected . manifest_revision,
      (None, None) => true,
      _ => false,
    }
}

fn fresh_view_response (
  reason : &str,
) -> String {
  let response : String = tag_sexp_response (TcpToClient::SaveResult,
    &empty_response_sexp (reason, &[], &None) . to_string ());
  format! ("{} (requires-fresh-view true))", &response [..response . len () - 1])
}

fn blocked_response (
  operation : &SaveOperation,
  request : &str,
  reason : &str,
) -> String {
  operation . tag_response (&save_refusal_response (reason, request), "blocked")
}

fn refuse_operation (
  operation : &SaveOperation,
  request : &str,
  reason : &str,
) -> String {
  let response : String = operation . tag_response (&save_refusal_response (reason, request), "refused");
  match operation . refuse (&response) {
    Ok (()) => response,
    Err (error) => blocked_response (operation, request, &error),
  }
}

fn refuse_reserved_save (
  prepared : &PreparedSave,
  reason : &str,
) -> String {
  let response : String = refuse_operation (&prepared . operation, &prepared . request, reason);
  let _ = prepared . control . finish ();
  response
}
