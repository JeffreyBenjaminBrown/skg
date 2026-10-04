use crate::import_md_and_org::batch::{
  ImportPreparation, PreparedImportBatch, prepare_import_batch};
use crate::serve::protocol::TcpToClient;
use crate::serve::util::{
  send_response_with_length_prefix, tag_sexp_response,
  value_from_request_sexp};
use crate::types::env::SkgEnv;
use crate::types::misc::SourceName;
use futures::executor::block_on;
use sexp::{Atom, Sexp};
use std::net::TcpStream;
use std::path::{Path, PathBuf};
use uuid::Uuid;

/// Owned by one TCP connection, so an approval cannot cross sessions.
pub struct PendingImport {
  token : String,
  batch : PreparedImportBatch,
}

pub fn handle_import_md_and_org_request (
  stream : &mut TcpStream,
  request : &str,
  env : &SkgEnv,
  pending : &mut Option<PendingImport>,
) {
  let action : String = value_from_request_sexp ("action", request)
    .unwrap_or_else (|_| "preview" . to_string ());
  match action . as_str () {
    "cancel" => {
      *pending = None;
      send_import_response (stream, TcpToClient::ImportMdAndOrgResult,
        "Import cancelled; nothing was written.", None, None, &[], &[]); }
    "apply" => {
      let token : String = match value_from_request_sexp ("approval-token", request) {
        Ok (token) => token,
        Err (error) => return refuse (stream, &error), };
      if pending . as_ref () . map (|approval| approval . token . as_str ())
        != Some (token . as_str ()) {
        return refuse (stream, "Unknown or stale import approval; preview again."); }
      let approval : PendingImport = pending . take () . unwrap ();
      let warnings : Vec<String> = diagnostic_warnings (&approval . batch);
      let gate = env . mutation_gate ();
      let _guard = block_on (gate . lock ());
      let result = approval . batch . apply_under_mutation_gate (env);
      drop (_guard);
      match result {
        Ok ((count, record_id)) => send_import_response (
          stream, TcpToClient::ImportMdAndOrgResult,
          &format! ("Imported {} nodes. Record: [[id:{}][Import record]]",
            count, record_id), None, Some (&record_id . 0), &[], &warnings),
        Err (error) => refuse (stream, &format! (
          "Import was not published: {} Preview again.", error)), } }
    "preview" => {
      *pending = None;
      let input : String = match value_from_request_sexp ("input-directory", request) {
        Ok (input) => input,
        Err (error) => return refuse (stream, &error), };
      let source : String = match value_from_request_sexp ("destination-source", request) {
        Ok (source) => source,
        Err (error) => return refuse (stream, &error), };
      let host_answer : Option<String> =
        value_from_request_sexp ("host-root", request) . ok ();
      let host_root : Option<PathBuf> = host_answer . as_deref ()
        .filter (|value| ! value . is_empty ()) . map (PathBuf::from);
      match prepare_import_batch (
        Path::new (&input), &SourceName::from (source),
        host_root . as_deref (), host_answer . is_some (), env) {
        Ok (ImportPreparation::HostMappingNeeded) => send_import_response (
          stream, TcpToClient::ImportMdAndOrgHostMappingNeeded,
          "Absolute filesystem links were found. Supply the absolute host path corresponding to the input directory, or leave it blank to preserve unresolved links.",
          None, None, &[], &[]),
        Ok (ImportPreparation::Prepared (batch)) => {
          let report : String = batch . preview_report ();
          let warnings : Vec<String> = diagnostic_warnings (&batch);
          let token : Option<String> = if batch . record_id . is_some () {
            Some (Uuid::new_v4 () . to_string ()) } else { None };
          if let Some (token) = &token {
            *pending = Some (PendingImport {
              token : token . clone (), batch }); }
          send_import_response (stream, TcpToClient::ImportMdAndOrgPreview,
            &report, token . as_deref (), None, &[], &warnings); }
        Err (error) => send_import_response (
          stream, TcpToClient::ImportMdAndOrgPreview,
          &format! ("Import preview cannot be applied:\n{}", error),
          None, None, &[error], &[]), } }
    _ => refuse (stream, "Unknown import action"),
  }
}

fn diagnostic_warnings (
  batch : &PreparedImportBatch,
) -> Vec<String> {
  batch . documents . iter () . flat_map (|document|
    document . diagnostics . iter () . map (|diagnostic|
      format! ("{}: {}", document . path . display (), diagnostic . message)))
    . collect ()
}

fn refuse (
  stream : &mut TcpStream,
  error : &str,
) {
  send_import_response (stream, TcpToClient::ImportMdAndOrgResult,
    error, None, None, &[error . to_string ()], &[]);
}

fn string_entry (
  name : &str,
  value : &str,
) -> Sexp {
  Sexp::List (vec! [
    Sexp::Atom (Atom::S (name . to_string ())),
    Sexp::Atom (Atom::S (value . to_string ())),
  ])
}

fn send_import_response (
  stream : &mut TcpStream,
  kind : TcpToClient,
  content : &str,
  token : Option<&str>,
  record_id : Option<&str>,
  errors : &[String],
  warnings : &[String],
) {
  let mut fields : Vec<Sexp> = vec! [string_entry ("content", content)];
  if let Some (token) = token { fields . push (string_entry ("approval-token", token)); }
  if let Some (record_id) = record_id {
    fields . push (string_entry ("record-id", record_id)); }
  fields . push (Sexp::List (vec! [
    Sexp::Atom (Atom::S ("errors" . to_string ())),
    Sexp::List (errors . iter () . map (|error|
      Sexp::Atom (Atom::S (error . clone ()))) . collect ()) ]));
  fields . push (Sexp::List (vec! [
    Sexp::Atom (Atom::S ("warnings" . to_string ())),
    Sexp::List (warnings . iter () . map (|warning|
      Sexp::Atom (Atom::S (warning . clone ()))) . collect ()) ]));
  let response : String = tag_sexp_response (kind,
    &Sexp::List (fields) . to_string ());
  send_response_with_length_prefix (stream, &response);
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::dbs::init::empty_in_ram_tantivy_index;
  use crate::dbs::in_rust_graph::InRustGraph;
  use crate::dbs::tantivy::background_writer::wait_for_tantivy_writes_idle;
  use crate::types::misc::{SkgConfig, SkgfileSource};
  use std::collections::HashMap;
  use std::fs;
  use std::io::{BufRead, BufReader, Read};
  use std::net::TcpListener;
  use std::sync::Arc;

  fn response (
    client : &mut BufReader<TcpStream>,
  ) -> String {
    let mut header : String = String::new ();
    client . read_line (&mut header) . unwrap ();
    let length : usize = header . trim () . strip_prefix ("Content-Length: ")
      .unwrap () . parse () . unwrap ();
    let mut blank : String = String::new ();
    client . read_line (&mut blank) . unwrap ();
    let mut bytes : Vec<u8> = vec![0; length];
    client . read_exact (&mut bytes) . unwrap ();
    String::from_utf8 (bytes) . unwrap ()
  }

  fn field (
    response : &str,
    name : &str,
  ) -> String {
    let parsed = sexp::parse (response) . unwrap ();
    crate::types::sexp::extract_string_list_from_sexp (&parsed, name)
      .unwrap () [0] . clone ()
  }

  #[test]
  fn approval_is_session_bound_single_use_and_replaced_by_new_preview () {
    let temp : tempfile::TempDir = tempfile::tempdir () . unwrap ();
    let input : PathBuf = temp . path () . join ("input");
    let source : PathBuf = temp . path () . join ("source");
    fs::create_dir (&input) . unwrap ();
    fs::create_dir (&source) . unwrap ();
    fs::write (input . join ("note.md"), "# Note\nBody\n") . unwrap ();
    let source_name : SourceName = SourceName::from ("notes");
    let config : SkgConfig = SkgConfig::dummyFromSources (HashMap::from ([
      (source_name . clone (), SkgfileSource {
        name : source_name . clone (), abbreviation : None,
        path : source . clone (), user_owns_it : true, }),
    ]));
    let env : SkgEnv = SkgEnv::new (config, Arc::new (InRustGraph::new ()),
      empty_in_ram_tantivy_index () . unwrap ());
    let listener : TcpListener = TcpListener::bind ("127.0.0.1:0") . unwrap ();
    let client_stream : TcpStream =
      TcpStream::connect (listener . local_addr () . unwrap ()) . unwrap ();
    let (mut server_stream, _) = listener . accept () . unwrap ();
    let mut client : BufReader<TcpStream> = BufReader::new (client_stream);
    let preview_request : String = format! (
      "((request . \"import md and org\") (action . \"preview\") (input-directory . \"{}\") (destination-source . \"notes\"))",
      input . display ());
    let mut pending : Option<PendingImport> = None;
    handle_import_md_and_org_request (
      &mut server_stream, &preview_request, &env, &mut pending);
    let first : String = response (&mut client);
    let first_token : String = field (&first, "approval-token");
    handle_import_md_and_org_request (
      &mut server_stream, &preview_request, &env, &mut pending);
    let second : String = response (&mut client);
    let second_token : String = field (&second, "approval-token");
    assert_ne! (first_token, second_token);
    let stale : String = format! (
      "((request . \"import md and org\") (action . \"apply\") (approval-token . \"{}\"))",
      first_token);
    handle_import_md_and_org_request (
      &mut server_stream, &stale, &env, &mut pending);
    assert! (response (&mut client) . contains ("stale"));
    let valid : String = format! (
      "((request . \"import md and org\") (action . \"apply\") (approval-token . \"{}\"))",
      second_token);
    let mut other_session : Option<PendingImport> = None;
    handle_import_md_and_org_request (
      &mut server_stream, &valid, &env, &mut other_session);
    assert! (response (&mut client) . contains ("stale"));
    assert! (pending . is_some ());
    handle_import_md_and_org_request (
      &mut server_stream, &valid, &env, &mut pending);
    let success : String = response (&mut client);
    assert! (success . contains ("record-id"));
    assert! (pending . is_none ());
    handle_import_md_and_org_request (
      &mut server_stream, &valid, &env, &mut pending);
    assert! (response (&mut client) . contains ("stale"));
    wait_for_tantivy_writes_idle ();
  }
}
