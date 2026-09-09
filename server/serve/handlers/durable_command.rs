//! Shared durable command admission, publication and terminal outcome ordering.

use crate::maintenance::save_journal::SaveOperationStatus;
use crate::runtime::save_operations::SaveOperation;
use crate::runtime::{SelectedRuntimeSnapshot, ServerRuntime};
use crate::serve::protocol::TcpToClient;
use crate::serve::util::{send_response_with_length_prefix, tag_terminal_text_response, tag_text_response};
use crate::source_sets::ActiveSourceSet;
use crate::types::env::SkgEnv;
use std::net::TcpStream;
use std::sync::Arc;

pub(crate) fn handle_durable_command_request (
  stream  : &mut TcpStream,
  request : &str,
  runtime : &ServerRuntime,
  response_type : TcpToClient,
  action : impl FnOnce (&mut SkgEnv, &SaveOperation) -> Result<String, String>,
) {
  let snapshot : Arc<SelectedRuntimeSnapshot> = runtime . selected_snapshot ();
  let active : ActiveSourceSet = runtime . interactive
    . lock () . unwrap () . active_source_set . clone ();
  let operation : SaveOperation = match SaveOperation::from_command (
    request, &snapshot . env . config, &active) {
    Ok (operation) => operation,
    Err (reason) => { send_command_runtime_error (stream, &reason); return; }, };
  match operation . recorded_response () {
    Ok (Some (response)) => {
      let _ = send_response_with_length_prefix (stream, &response);
      return; }
    Err (reason) => {
      let response : String = operation . tag_response (
        &tag_text_response (response_type, &reason),
        "blocked");
      let _ = send_response_with_length_prefix (stream, &response);
      return; }
    Ok (None) => {}
  }
  if let Err (reason) = runtime . validate_session_authority (request) {
    let response : String = operation . tag_response (
      &tag_text_response (response_type, &reason),
      "refused");
    match operation . refuse (&response) {
      Ok (()) => { let _ = send_response_with_length_prefix (stream, &response); }
      Err (error) => send_command_runtime_error (stream, &error), }
    return;
  }
  let result : Result<Result<String, String>, String> = runtime . with_writer_transition (
    operation . operation_id . clone (), |env, control| {
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
        Err (reason) => {
          let dispatched : bool = match operation . status () {
            Ok (Some (snapshot)) => matches! (
              snapshot . status,
              SaveOperationStatus::Authorized { .. }
              | SaveOperationStatus::AppliedAwaitingCommit),
            Ok (None) => false,
            Err (_) => true,
          };
          if dispatched { let _ = control . block (reason . clone ()); }
          Err (reason)
        }
      }
    });
  match result {
    Ok (Ok (response)) => {
      let _ = send_response_with_length_prefix (stream, &response); }
    Ok (Err (reason)) | Err (reason) => {
      let can_refuse : bool = match operation . status () {
        Ok (None) => true,
        Ok (Some (snapshot)) => matches! (
          snapshot . status,
          SaveOperationStatus::PreparedUnAuthorized
          | SaveOperationStatus::StagingUnAuthorized),
        Err (_) => false,
      };
      if can_refuse {
        let response : String = operation . tag_response (
          &tag_text_response (response_type, &reason),
          "refused");
        match operation . refuse (&response) {
          Ok (()) => { let _ = send_response_with_length_prefix (stream, &response); }
          Err (error) => send_command_runtime_error (stream, &error), }
      } else { send_command_runtime_error (stream, &reason); }
    }
  }
}

fn send_command_runtime_error (
  stream : &mut TcpStream,
  error  : &str,
) {
  let _ = send_response_with_length_prefix (
    stream, &tag_terminal_text_response (TcpToClient::Error, "failed", error));
}

