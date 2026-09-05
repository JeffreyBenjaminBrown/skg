//! Compatibility endpoint for client hints to the process-owned observer.
//!
//! The historical `reload paths` endpoint selected disk directly.  Its wire
//! spelling remains useful to older clients as an observation hint, but it
//! must never cross the maintenance archive boundary or mutate a store.

use crate::maintenance::QueuedObservationReason;
use crate::runtime::ServerRuntime;
use crate::serve::protocol::TcpToClient;
use crate::serve::util::{
  send_response_with_length_prefix,
  tag_terminal_sexp_response,
  tag_terminal_text_response,
  value_from_request_sexp,
};
use crate::types::sexp::extract_string_list_from_sexp;

use sexp::{Atom, Sexp};
use std::net::TcpStream;
use std::path::PathBuf;

pub fn handle_observation_hint_request (
  stream  : &mut TcpStream,
  request : &str,
  runtime : &ServerRuntime,
) {
  let result = queue_observation_hint (request, runtime);
  let response = match result {
    Ok (payload) => tag_terminal_sexp_response (
      TcpToClient::ReloadPaths, "complete", &payload),
    Err (error) => tag_terminal_text_response (
      TcpToClient::ReloadPaths, "failed", &error),
  };
  let _ = send_response_with_length_prefix (stream, &response);
}

fn queue_observation_hint (
  request : &str,
  runtime : &ServerRuntime,
) -> Result<String, String> {
  let parsed = sexp::parse (request)
    . map_err (|error| format! ("invalid observation hint: {}", error))?;
  let paths = optional_string_list (&parsed, "paths")?;
  let ids = optional_string_list (&parsed, "ids")?;
  if !ids . is_empty () {
    return Err (
      "direct ID reload is retired; use skg-reload-from-id-stack / :SkgReloadIds so recovery maintenance can archive editor state"
        . into ()); }
  let full_sweep = value_from_request_sexp ("full-sweep", request)
    . map (|value| value == "true") . unwrap_or (false);
  if paths . is_empty () && !full_sweep {
    return Err ("observation hint names no paths and requests no full sweep"
      . into ()); }

  if full_sweep {
    runtime . schedule_full_observation (QueuedObservationReason::ClientHint)?;
  } else {
    runtime . schedule_path_observation (
      paths . iter () . map (PathBuf::from) . collect (),
      QueuedObservationReason::ClientHint)?;
  }
  Ok (Sexp::List (vec![
    atom_field ("content",
      "Disk observation was queued; any semantic difference will be offered as durable maintenance."),
    atom_field ("observation-queued", "true"),
    atom_field ("full-sweep", if full_sweep { "true" } else { "nil" }),
  ]) . to_string ())
}

fn optional_string_list (sexp : &Sexp, key : &str)
  -> Result<Vec<String>, String>
{
  let present = match sexp {
    Sexp::List (items) => items . iter () . any (|item| match item {
      Sexp::List (parts) => matches! (parts . first (),
        Some (Sexp::Atom (Atom::S (candidate))) if candidate == key),
      _ => false,
    }),
    _ => false,
  };
  if present { extract_string_list_from_sexp (sexp, key) }
  else { Ok (Vec::new ()) }
}

fn atom_field (key : &str, value : &str) -> Sexp {
  Sexp::List (vec![
    Sexp::Atom (Atom::S (key . into ())),
    Sexp::Atom (Atom::S (value . into ())),
  ])
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn optional_lists_distinguish_absent_and_present () {
    let request = sexp::parse (
      "((request . \"reload paths\") (paths a b) (full-sweep . false))")
      . unwrap ();
    assert_eq! (optional_string_list (&request, "paths") . unwrap (),
                vec!["a" . to_string (), "b" . to_string ()]);
    assert! (optional_string_list (&request, "ids") . unwrap () . is_empty ());
  }
}
