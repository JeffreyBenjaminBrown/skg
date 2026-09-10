//! Shared durable command admission, publication and terminal outcome ordering.

use crate::maintenance::save_journal::SaveOperationStatus;
use crate::runtime::save_operations::SaveOperation;
use crate::runtime::{MutationControl, SelectedRuntimeSnapshot, ServerRuntime};
use crate::serve::protocol::TcpToClient;
use crate::serve::util::{send_response_with_length_prefix, tag_terminal_text_response, tag_text_response, take_request_context, DetachedRequestContext};
use crate::source_sets::ActiveSourceSet;
use crate::types::env::SkgEnv;
use std::net::TcpStream;
use std::sync::Arc;
use std::sync::mpsc::Sender;
use std::thread;

pub(crate) type CommandAction =
  fn (&mut SkgEnv, &SaveOperation) -> Result<String, String>;

pub(crate) enum CommandAdmission {
  Response (String),
  Start (PreparedCommand),
}

pub(crate) struct PreparedCommand {
  operation : SaveOperation,
  before : Arc<SelectedRuntimeSnapshot>,
  control : MutationControl,
  response_type : TcpToClient,
}

/// Reserve on the connection thread, then leave it free to service reads.
/// Worker completions return here through the connection's sole socket writer.
pub(crate) fn dispatch_command_request (
  stream : &mut TcpStream,
  request : &str,
  runtime : &Arc<ServerRuntime>,
  response_type : TcpToClient,
  action : CommandAction,
  completions : &Sender<String>,
) {
  let prepared : PreparedCommand = match prepare_command (request, runtime, response_type) {
    CommandAdmission::Response (response) => {
      let _ = send_response_with_length_prefix (stream, &response);
      return;
    }
    CommandAdmission::Start (prepared) => prepared,
  };
  // Even a failed yield delivery must not abandon the accepted command.
  let _ = send_response_with_length_prefix (stream,
    "((response-type request-yield) (frame-kind request-yield))");
  let Some (context) : Option<DetachedRequestContext> = take_request_context () else {
    let _ = prepared . control . finish ();
    let _ = send_response_with_length_prefix (stream,
      &command_runtime_error ("command dispatch lost its request context"));
    return;
  };
  let runtime : Arc<ServerRuntime> = Arc::clone (runtime);
  let request : String = request . to_string ();
  let completions : Sender<String> = completions . clone ();
  let failed_dispatch_control : MutationControl = prepared . control . clone ();
  let worker_context : DetachedRequestContext = context . clone ();
  let dispatched : std::io::Result<thread::JoinHandle<()>> = thread::Builder::new ()
    . name ("skg-durable-command" . into ()) . spawn (move || {
      let response : String = run_prepared_command (&request, &runtime, prepared, action);
      // Disconnect only loses delivery; the operation has already settled.
      let _ = completions . send (worker_context . decorate_response (&response));
    });
  if let Err (error) = dispatched {
    let _ = failed_dispatch_control . finish ();
    let _ = send_response_with_length_prefix (stream,
      &context . decorate_response (&command_runtime_error (&format! (
        "could not start command worker: {}", error))));
  }
}

// Compatibility entry point for direct handler callers.
pub(crate) fn handle_durable_command_request (
  stream : &mut TcpStream,
  request : &str,
  runtime : &ServerRuntime,
  response_type : TcpToClient,
  action : CommandAction,
) {
  let response : String = match prepare_command (request, runtime, response_type) {
    CommandAdmission::Response (response) => response,
    CommandAdmission::Start (prepared) =>
      run_prepared_command (request, runtime, prepared, action),
  };
  let _ = send_response_with_length_prefix (stream, &response);
}

pub(crate) fn prepare_command (
  request : &str,
  runtime : &ServerRuntime,
  response_type : TcpToClient,
) -> CommandAdmission {
  let before : Arc<SelectedRuntimeSnapshot> = runtime . selected_snapshot ();
  let active : ActiveSourceSet = runtime . interactive
    . lock () . unwrap () . active_source_set . clone ();
  let operation : SaveOperation = match SaveOperation::from_command (
    request, &before . env . config, &active) {
    Ok (operation) => operation,
    Err (reason) => return CommandAdmission::Response (command_runtime_error (&reason)),
  };
  match operation . recorded_response () {
    Ok (Some (response)) => return CommandAdmission::Response (response),
    Err (reason) => return CommandAdmission::Response (operation . tag_response (
      &tag_text_response (response_type, &reason), "blocked")),
    Ok (None) => {}
  }
  if let Err (reason) = runtime . validate_session_authority (request) {
    return CommandAdmission::Response (refuse_command (&operation, response_type, &reason));
  }
  let control : MutationControl = match runtime . reserve_mutation (
    operation . operation_id . clone (), before . selected . graph_generation,
    before . selected . manifest_revision) {
    Ok (control) => control,
    // Reservation failure creates no journal record: the same operation can
    // already be computing before it has staged its durable preparation.
    Err (reason) => return CommandAdmission::Response (operation . tag_response (
      &tag_text_response (response_type, &reason), "blocked")),
  };
  CommandAdmission::Start (PreparedCommand { operation, before, control, response_type })
}

pub(crate) fn run_prepared_command (
  request : &str,
  runtime : &ServerRuntime,
  prepared : PreparedCommand,
  action : CommandAction,
) -> String {
  let PreparedCommand { operation, before, control, response_type } = prepared;
  let result : Result<Result<String, String>, String> = runtime . with_reserved_writer_transition (
    before, control, |env, control| {
      let outcome : Result<String, String> = (|| {
        let locked_active : ActiveSourceSet = runtime . interactive . lock ()
          . map_err (|_| "interactive session poisoned" . to_string ())?
          . active_source_set . clone ();
        let locked_operation : SaveOperation = SaveOperation::from_command (
          request, &env . config, &locked_active)?;
        if ! operation . matches_interpretation (&locked_operation) {
          return Err ("source interpretation changed before command execution" . into ()); }
        let operation : SaveOperation = operation . clone ()
          . with_control (control . clone ());
        match action (env, &operation) {
          Ok (report) => {
            let response : String = operation . tag_response (
              &tag_text_response (response_type, &report),
              "committed");
            let recorded : Result<(), String> = runtime
              . publish_selected_from_env (control, env) . and_then (|_|
                operation . commit (
                  &response,
                  &format! ("graph-{}-manifest-{}",
                    env . in_rust_graph . load_full () . graph_generation . get (),
                    env . in_rust_graph . load_full () . manifest_revision . get ())));
            match recorded {
              Ok (()) => Ok (response),
              Err (reason) => {
                let _ = control . block (reason . clone ());
                Err (reason)
              }
            }
          }
          Err (reason) => Err (reason),
        }
      }) ();
      match outcome {
        Ok (response) => Ok (response),
        Err (reason) => settle_command_error (&operation, response_type, control, &reason),
      }
    });
  match result {
    Ok (Ok (response)) => response,
    Ok (Err (reason)) | Err (reason) => command_runtime_error (&reason),
  }
}

fn settle_command_error (
  operation : &SaveOperation,
  response_type : TcpToClient,
  control : &MutationControl,
  reason : &str,
) -> Result<String, String> {
  let can_refuse : bool = match operation . status () {
    Ok (None) => true,
    Ok (Some (snapshot)) => matches! (snapshot . status,
      SaveOperationStatus::PreparedUnAuthorized | SaveOperationStatus::StagingUnAuthorized),
    Err (_) => false,
  };
  if can_refuse {
    let response : String = operation . tag_response (
      &tag_text_response (response_type, reason), "refused");
    // Settle before releasing the reservation: a duplicate must never race
    // this durable refusal against a newly admitted worker's preparation.
    match operation . refuse (&response) {
      Ok (()) => return Ok (response),
      Err (error) => {
        let _ = control . block (error . clone ());
        return Err (error);
      }
    }
  }
  let _ = control . block (reason . to_string ());
  Err (reason . to_string ())
}

fn refuse_command (
  operation : &SaveOperation,
  response_type : TcpToClient,
  reason : &str,
) -> String {
  let response : String = operation . tag_response (
    &tag_text_response (response_type, reason), "refused");
  match operation . refuse (&response) {
    Ok (()) => response,
    Err (error) => command_runtime_error (&error),
  }
}

fn command_runtime_error (
  error : &str,
) -> String {
  tag_terminal_text_response (TcpToClient::Error, "failed", error)
}
