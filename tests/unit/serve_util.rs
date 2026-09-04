use super::{
  begin_request_context,
  clear_request_context,
  envelope_response,
  format_buffer_response_sexp,
  format_errors_warnings_sexp,
  tag_server_push_sexp_response,
  tag_sexp_response,
  value_from_request_sexp,
  TcpToClient,
};

#[test]
fn request_value_accepts_numeric_atoms () {
  let request = "((request . \"view visited\") (visit-sequence . 2))";
  assert_eq! (
    value_from_request_sexp ("visit-sequence", request),
    Ok ("2" . to_string ()));
}

#[test]
fn response_envelope_carries_request_frame_and_terminal_status () {
  clear_request_context ();
  begin_request_context (
    "((request . \"verify connection\") (request-id . \"req-7\"))")
    . unwrap ();
  let response = envelope_response (
    "((response-type verify-connection) (content \"ok\"))");
  assert! (response . contains ("(request-id req-7)"));
  assert! (response . contains ("(frame-kind verify-connection)"));
  assert! (response . contains ("(terminal-status complete)"));
  clear_request_context ();
}

#[test]
fn response_envelope_carries_incident_identity_across_a_continuation () {
  clear_request_context ();
  let request = "((request . \"text search\") (request-id . \"req-8\") \
                 (incident-id . \"incident-a\"))";
  begin_request_context (request) . unwrap ();
  begin_request_context (request) . unwrap ();
  let response = envelope_response (
    "((response-type search-enrichment) (content \"ok\"))");
  assert! (response . contains ("(request-id req-8)"));
  assert! (response . contains ("(incident-id incident-a)"));
  clear_request_context ();
}

#[test]
fn response_envelope_leaves_stream_frames_nonterminal () {
  clear_request_context ();
  begin_request_context (
    "((request . \"save buffer\") (request-id . \"req-save\"))")
    . unwrap ();
  let response = envelope_response (
    "((response-type collateral-view) (content \"view\"))");
  assert! (response . contains ("(request-id req-save)"));
  assert! (! response . contains ("terminal-status"));
  clear_request_context ();
}

#[test]
fn format_buffer_response_includes_empty_errors_and_warnings () {
  let response : String =
    format_buffer_response_sexp ("* root\n", &[], &[]);

  assert_eq!(
    response,
    "((content \"* root\n\") (errors ()) (warnings ()))" );
}

#[test]
fn format_buffer_response_separates_errors_and_warnings () {
  let errors : Vec<String> =
    vec! [ "fatal problem" . to_string () ];
  let warnings : Vec<String> =
    vec! [ "nonfatal problem" . to_string () ];
  let response : String =
    format_buffer_response_sexp ("* root\n", &errors, &warnings);

  assert_eq!(
    response,
    "((content \"* root\n\") (errors (\"fatal problem\")) (warnings (\"nonfatal problem\")))" );
}

#[test]
fn format_errors_warnings_response_supports_warnings_only () {
  let warnings : Vec<String> =
    vec! [ "rerender note" . to_string () ];
  let response : String =
    format_errors_warnings_sexp (&[], &warnings);

  assert_eq!(
    response,
    "((errors ()) (warnings (\"rerender note\")))" );
}

#[test]
fn tagged_structured_response_keeps_warning_channel () {
  let warnings : Vec<String> =
    vec! [ "non fatal" . to_string () ];
  let payload : String =
    format_buffer_response_sexp ("", &[], &warnings);
  let response : String =
    tag_sexp_response (TcpToClient::SaveResult, &payload);

  assert_eq!(
    response,
    "((response-type save-result) (content ) (errors ()) (warnings (\"non fatal\")))" );
}

#[test]
fn server_push_has_operation_identity_but_no_request_identity () {
  clear_request_context ();
  let response = tag_server_push_sexp_response (
    TcpToClient::CollateralView,
    "background-19",
    "((view-uri view-a) (content fresh))");
  assert! (response . contains ("(server-push true)"));
  assert! (response . contains ("(operation-id background-19)"));
  assert! (response . contains ("(frame-kind collateral-view)"));
  assert! (! response . contains ("request-id"));
}

#[test]
fn active_request_context_does_not_capture_a_server_push () {
  clear_request_context ();
  begin_request_context (
    "((request . \"text search\") (request-id . \"search-1\"))")
    . unwrap ();
  let pushed = tag_server_push_sexp_response (
    TcpToClient::CollateralView, "background-20",
    "((view-uri view-a) (content fresh))");
  let enveloped = envelope_response (&pushed);
  assert! (! enveloped . contains ("request-id"));
  clear_request_context ();
}
