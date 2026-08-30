use super::{
  begin_request_context,
  clear_request_context,
  envelope_response,
  format_buffer_response_sexp,
  format_errors_warnings_sexp,
  tag_sexp_response,
  TcpToClient,
};

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
