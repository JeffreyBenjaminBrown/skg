use super::{
  format_buffer_response_sexp,
  format_rerender_done_sexp,
  format_single_view_sexp,
  sexp_to_client_string,
  tag_sexp_response,
  TcpToClient,
  ViewforestId,
};


use sexp::{Atom, Sexp};

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
fn rerender_done_carries_the_errors_and_the_warnings () {
  let warnings : Vec<String> =
    vec! [ "rerender note" . to_string () ];
  assert_eq!(
    format_rerender_done_sexp (&[], &warnings),
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
    "((response-type save-result) (content \"\") (errors ()) (warnings (\"non fatal\")))" );
}

#[test]
fn an_empty_title_serializes_quoted () {
  let response : Sexp =
    Sexp::List ( vec! [
      Sexp::List ( vec! [
        Sexp::Atom ( Atom::S ( "title" . to_string () )),
        Sexp::Atom ( Atom::S ( String::new () )) ] ) ] );
  assert_eq!(
    sexp_to_client_string (&response),
    "((title \"\"))" );
}

#[test]
fn an_empty_view_content_serializes_quoted () {
  let response : String =
    format_single_view_sexp (
      &ViewforestId::from_client_string ( "v1" . to_string () ), "", 7 );
  assert_eq!(
    response,
    "((view-id v1) (content \"\") (rendered-at \"7\"))" );
}

#[test]
fn non_empty_atoms_serialize_as_the_sexp_crate_prints_them () {
  let response : Sexp =
    Sexp::List ( vec! [
      Sexp::Atom ( Atom::S ( "bare-symbol" . to_string () )),
      Sexp::Atom ( Atom::S ( "two words" . to_string () )),
      Sexp::Atom ( Atom::S ( "say \"hi\"" . to_string () )),
      Sexp::Atom ( Atom::S ( "12" . to_string () )),
      Sexp::Atom ( Atom::I (7) ),
      Sexp::List ( Vec::new () ) ] );
  assert_eq!(
    sexp_to_client_string (&response),
    response . to_string () );
}
