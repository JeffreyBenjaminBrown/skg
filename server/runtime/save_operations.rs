//! Exact ordinary-save identities and their recoverable source effects.

use crate::maintenance::candidate::{config_identity, source_catalog_blake3};
use crate::maintenance::journal::MaintenanceJournalStore;
use crate::maintenance::save_journal::{
  DurablePathMutation, DurableSaveOutcome, DurableSaveRequest,
  SaveInterpretationEvidence, SaveJournalError, SaveJournalStore,
  SaveOperationSnapshot,
};
use crate::serve::util::value_from_request_sexp;
use crate::source_sets::ActiveSourceSet;
use crate::types::misc::SkgConfig;

use serde::Serialize;
use sha2::{Digest, Sha256};
use std::path::PathBuf;

#[derive(Clone)]
pub(crate) struct SaveOperation {
  pub operation_id : String,
  pub fingerprint  : String,
  server_session_id : String,
  store            : SaveJournalStore,
  control          : Option<super::MutationControl>,
  interpretation   : SaveInterpretationEvidence,
}

#[derive(Serialize)]
struct Interpretation<'a> {
  format_version       : u32,
  config_identity      : PathBuf,
  data_root            : &'a std::path::Path,
  archive_identity     : &'a std::path::Path,
  config               : &'a SkgConfig,
  source_catalog_blake3 : String,
  source_set           : &'a str,
  request              : &'a str,
  content              : &'a str,
}

impl SaveOperation {
  pub(crate) fn from_request (
    request : &str,
    content : &str,
    config  : &SkgConfig,
    active  : &ActiveSourceSet,
  ) -> Result<Self, String> {
    let operation_id : String = value_from_request_sexp ("operation-id", request)?;
    uuid::Uuid::parse_str (&operation_id)
      . map_err (|_| "save operation-id must be a UUID" . to_string ())?;
    let fingerprint : String = validate_request_fingerprint (request, content)?;
    let server_session_id : String = value_from_request_sexp ("server-session-id", request)?;
    uuid::Uuid::parse_str (&server_session_id)
      . map_err (|_| "save server-session-id must be a UUID" . to_string ())?;
    let evidence : Interpretation<'_> = Interpretation {
      format_version: 1,
      config_identity: config_identity (config),
      data_root: &config . data_root,
      archive_identity: &config . maintenance_archive_identity,
      config,
      source_catalog_blake3: source_catalog_blake3 (config),
      source_set: &active . name . 0,
      request,
      content,
    };
    let interpretation : SaveInterpretationEvidence = SaveInterpretationEvidence {
      identity: interpretation_identity (config, active),
      bytes: serde_yaml::to_string (&evidence)
        . map_err (|error| error . to_string ())? . into_bytes (),
    };
    Ok (Self {
      operation_id, fingerprint, server_session_id, control: None,
      store: save_journal_for_config (config), interpretation,
    })
  }

  pub(crate) fn with_control (mut self, control : super::MutationControl) -> Self {
    self . control = Some (control);
    self
  }

  pub(crate) fn status (&self) -> Result<Option<SaveOperationSnapshot>, String> {
    match self . store . status (&self . operation_id, &self . fingerprint) {
      Ok (snapshot) => Ok (Some (snapshot)),
      Err (SaveJournalError::OperationNotFound { .. }) => Ok (None),
      Err (error) => Err (error . to_string ()),
    }
  }

  pub(crate) fn recorded_response (&self) -> Result<Option<String>, String> {
    use crate::maintenance::save_journal::SaveOperationStatus;
    let Some (snapshot) = self . status ()? else { return Ok (None); };
    if snapshot . interpretation_identity != self . interpretation . identity {
      return Err ("the saved outcome belongs to a different source authorization context; reopen a fresh view" . into ()); }
    match snapshot . status {
      SaveOperationStatus::Committed { outcome, .. }
      | SaveOperationStatus::Refused { outcome, .. } =>
        String::from_utf8 (outcome . client_result) . map (Some)
          . map_err (|error| error . to_string ()),
      _ => Err ("this save operation is unresolved; inspect its status before submitting new work" . into ()),
    }
  }

  pub(crate) fn prepare (
    &self,
    mutations : Vec<DurablePathMutation>,
  ) -> Result<(), String> {
    self . store . prepare (&DurableSaveRequest {
      operation_id: self . operation_id . clone (),
      request_base_fingerprint: self . fingerprint . clone (),
      interpretation_evidence: self . interpretation . clone (),
      mutations,
    }) . map_err (|error| error . to_string ())?;
    #[cfg(test)]
    socket_tests::crash_point ("after-staging");
    Ok (( ))
  }

  /// The owner grants this operation effect authority before this call.
  pub(crate) fn apply_authorized (&self) -> Result<(), String> {
    self . control . as_ref ()
      . ok_or ("save effects require an owner reservation")? . authorize ()?;
    self . store . authorize (&self . operation_id, &self . fingerprint)
      . map_err (|error| error . to_string ())?;
    #[cfg(test)]
    socket_tests::crash_point ("after-authorization");
    self . store . apply_authorized (&self . operation_id, &self . fingerprint)
      . map_err (|error| error . to_string ())?;
    #[cfg(test)]
    socket_tests::crash_point ("after-application");
    Ok (( ))
  }

  /// Called after the matching selected pair has been published. No terminal
  /// response may be transmitted until this durable result succeeds.
  pub(crate) fn commit (
    &self,
    response : &str,
    resulting_base : &str,
  ) -> Result<(), String> {
    #[cfg(test)]
    socket_tests::crash_point ("after-publication");
    self . store . record_committed_outcome (
      &self . operation_id, &self . fingerprint, &DurableSaveOutcome {
        resulting_base_fingerprint: resulting_base . into (),
        client_result: response . as_bytes () . to_vec (),
      }) . map_err (|error| error . to_string ())?;
    #[cfg(test)]
    socket_tests::crash_point ("after-commit");
    Ok (( ))
  }

  pub(crate) fn refuse (
    &self,
    response : &str,
  ) -> Result<(), String> {
    if self . status ()? . is_none () { self . prepare (Vec::new ())?; }
    self . store . record_refused_outcome (
      &self . operation_id, &self . fingerprint, &DurableSaveOutcome {
        resulting_base_fingerprint: self . fingerprint . clone (),
        client_result: response . as_bytes () . to_vec (),
      }) . map (|_| ()) . map_err (|error| error . to_string ())
  }

  pub(crate) fn tag_response (&self, response : &str, state : &str) -> String {
    crate::serve::util::add_server_session_to_response (
      &tag_operation_response (response, &self . operation_id, &self . fingerprint, state),
      &self . server_session_id)
  }
}

#[cfg(test)]
pub(crate) mod socket_tests;

pub(crate) fn save_journal_for_config (config : &SkgConfig) -> SaveJournalStore {
  let maintenance : MaintenanceJournalStore =
    MaintenanceJournalStore::for_config (&config . config_path);
  SaveJournalStore::at_root (maintenance . directory () . join ("ordinary-saves"))
}

/// Runtime construction is also used outside main. It may not bypass the
/// recovery-before-authority ordering merely because it received a valid graph.
pub(crate) fn require_resolved_startup_saves (config : &SkgConfig) -> Result<(), String> {
  use crate::maintenance::save_journal::SaveOperationStatus;
  for operation in save_journal_for_config (config) . load_all () . require_clean ()
    . map_err (|error| error . to_string ())?
  {
    if !matches! (operation . status, SaveOperationStatus::Committed { .. }
      | SaveOperationStatus::Refused { .. })
    { return Err (format! (
        "save {} requires startup recovery before new authority can be issued",
        operation . operation_id)); }
  }
  Ok (( ))
}

pub(crate) fn interpretation_identity (
  config : &SkgConfig,
  active : &ActiveSourceSet,
) -> String {
  let mut hasher : blake3::Hasher = blake3::Hasher::new ();
  hasher . update (config_identity (config) . to_string_lossy () . as_bytes ());
  hasher . update (&[0]);
  hasher . update (source_catalog_blake3 (config) . as_bytes ());
  hasher . update (&[0]);
  hasher . update (active . name . 0 . as_bytes ());
  hasher . finalize () . to_hex () . to_string ()
}

pub(crate) fn tag_operation_response (
  response : &str,
  operation_id : &str,
  fingerprint : &str,
  state : &str,
) -> String {
  let mut parsed : sexp::Sexp = sexp::parse (response)
    . expect ("server save response is a valid s-expression");
  let sexp::Sexp::List (fields) = &mut parsed else {
    panic! ("server save response must be a field list"); };
  for (key, value) in [
    ("operation-id", operation_id),
    ("request-base-fingerprint", fingerprint),
    ("save-operation-state", state),
  ] {
    fields . push (sexp::Sexp::List (vec![
      sexp::Sexp::Atom (sexp::Atom::S (key . into ())),
      sexp::Sexp::Atom (sexp::Atom::S (value . into ())),
    ])); }
  parsed . to_string ()
}

fn validate_request_fingerprint (request : &str, content : &str) -> Result<String, String> {
  let request : &str = request . trim_end_matches (['\r', '\n']);
  let fingerprint : String = value_from_request_sexp ("request-base-fingerprint", request)?;
  if fingerprint . len () != 64 || !fingerprint . bytes () . all (|byte|
      byte . is_ascii_digit () || (b'a' ..= b'f') . contains (&byte))
  { return Err ("save request-base-fingerprint must be lowercase SHA-256" . into ()); }
  // This is the final logical intent field. The transport subsequently adds
  // request/incident routing identities; those may change on redelivery.
  // Preserve the exact logical bytes, rather than reserializing parsed data.
  let marker : String = format! (" (request-base-fingerprint . \"{}\")", fingerprint);
  let (prefix, trailer) : (&str, &str) = request . rsplit_once (&marker)
    . ok_or_else (|| "save fingerprint must be the final canonical intent field" . to_string ())?;
  let transport : &str = trailer . strip_suffix (')')
    . ok_or ("save request has no closing delimiter")?;
  let parsed_transport : sexp::Sexp = sexp::parse (&format! ("({})", transport))
    . map_err (|_| "malformed save transport envelope" . to_string ())?;
  let sexp::Sexp::List (fields) = parsed_transport else { unreachable! (); };
  let mut seen : std::collections::HashSet<String> = std::collections::HashSet::new ();
  for field in fields {
    let sexp::Sexp::List (parts) = field else {
      return Err ("save intent follows its fingerprint" . into ()); };
    let Some (sexp::Sexp::Atom (sexp::Atom::S (key))) = parts . first () else {
      return Err ("save transport field has no key" . into ()); };
    if !matches! (key . as_str (), "request-id" | "incident-id")
      || !seen . insert (key . clone ())
    { return Err ("only distinct routing fields may follow a save fingerprint" . into ()); }
  }
  let intent : String = format! ("{})", prefix);
  let mut digest : Sha256 = Sha256::new ();
  digest . update (intent . as_bytes ());
  digest . update ([0]);
  digest . update (content . as_bytes ());
  let computed : String = format! ("{:x}", digest . finalize ());
  if computed != fingerprint {
    return Err ("save request/body differs from its durable fingerprint" . into ()); }
  Ok (fingerprint)
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn exact_utf8_intent_and_body_are_bound_before_duplicate_lookup () {
    let intent : &str = "((request . \"save buffer\") (operation-id . \"id\") (base . \"first\"))";
    let body : &str = "* café\nλ\n";
    let mut hash : Sha256 = Sha256::new ();
    hash . update (intent . as_bytes ());
    hash . update ([0]);
    hash . update (body . as_bytes ());
    let fingerprint : String = format! ("{:x}", hash . finalize ());
    let request : String = format! ("{} (request-base-fingerprint . \"{}\"))",
      &intent [..intent . len () - 1], fingerprint);
    assert_eq! (validate_request_fingerprint (&request, body) . unwrap (), fingerprint);
    assert_eq! (validate_request_fingerprint (&format! ("{}\n", request), body)
      . unwrap (), fingerprint);
    let framed : String = format! ("{} (request-id . \"delivery-2\"))\n",
      &request [..request . len () - 1]);
    assert_eq! (validate_request_fingerprint (&framed, body) . unwrap (), fingerprint);
    assert! (validate_request_fingerprint (&framed . replace ("request-id", "base"), body)
      . is_err ());
    assert! (validate_request_fingerprint (&request, "* cafe\nλ\n") . is_err ());
    assert! (validate_request_fingerprint (&request . replace ("first", "second"), body) . is_err ());
  }

  #[test]
  fn a_replayed_terminal_keeps_its_original_server_session () {
    let original : String = crate::serve::util::add_server_session_to_response (
      "((response-type save-result) (content preserved))", "old-server");
    let replayed : String = crate::serve::util::add_server_session_to_response (
      &original, "new-server");
    assert_eq! (replayed, original);
    assert! (!replayed . contains ("new-server"));
  }
}

/// Recovery obligations loaded before startup reads any source semantics.
pub struct StartupSaveRecovery {
  store      : SaveJournalStore,
  operations : Vec<SaveOperationSnapshot>,
}

pub fn recover_source_effects_before_startup (
  config : &SkgConfig,
) -> Result<StartupSaveRecovery, String> {
  // A malformed active incident can name unresolved external effects. It must
  // not silently become an idle startup simply because source files parse.
  MaintenanceJournalStore::for_config (&config . config_path)
    . load () . require_authority ()?;
  let store : SaveJournalStore = save_journal_for_config (config);
  let recovered = store . recover_all_unfinished ()
    . map_err (|error| error . to_string ())?;
  for path in &recovered . incomplete_publications {
    tracing::warn! (path = %path . display (),
      "preserving unused initial save-journal publication"); }
  Ok (StartupSaveRecovery { store, operations: recovered . operations })
}

/// Run after startup validates current disk and prepares its graph/search pair,
/// before admitting an interactive client. A recovered result never grants an
/// old editor buffer authority in the fresh server session.
pub fn commit_recovered_source_effects (
  recovery : StartupSaveRecovery,
  graph_generation : u64,
) -> Result<(), String> {
  use crate::maintenance::save_journal::SaveOperationStatus;
  use crate::serve::protocol::TcpToClient;
  use crate::serve::util::{format_buffer_response_sexp, tag_sexp_response};
  for operation in recovery . operations {
    let committed : bool = match operation . status {
      SaveOperationStatus::Committed { .. } | SaveOperationStatus::Refused { .. } => continue,
      SaveOperationStatus::AppliedAwaitingCommit => true,
      SaveOperationStatus::PreparedUnAuthorized
      | SaveOperationStatus::StagingUnAuthorized => false,
      SaveOperationStatus::Authorized { .. } => return Err (format! (
        "save {} still has unresolved authorized effects after startup recovery",
        operation . operation_id)),
    };
    let warning : String = if committed {
      "The interrupted save was recovered. Open a fresh live view before saving this preserved buffer." . into ()
    } else {
      "The interrupted save was never authorized; nothing was saved. Open a fresh live view before submitting new work." . into ()
    };
    let payload : String = format_buffer_response_sexp ("", &[], &[warning]);
    let response : String = tag_operation_response (
      &tag_sexp_response (TcpToClient::SaveResult, &payload),
      &operation . operation_id, &operation . request_base_fingerprint,
      if committed { "committed" } else { "refused" });
    let response : String = format! (
      "{} (recovered-after-restart true) (requires-fresh-view true))",
      &response [..response . len () - 1]);
    let outcome : DurableSaveOutcome = DurableSaveOutcome {
      resulting_base_fingerprint: format! ("fresh-startup-graph-{}", graph_generation),
      client_result: response . into_bytes (),
    };
    let result = if committed {
      recovery . store . record_committed_outcome (
        &operation . operation_id, &operation . request_base_fingerprint, &outcome)
    } else {
      recovery . store . record_refused_outcome (
        &operation . operation_id, &operation . request_base_fingerprint, &outcome)
    };
    result . map_err (|error| error . to_string ())?;
  }
  Ok (( ))
}

pub(crate) fn handle_save_operation_request (
  stream : &mut std::net::TcpStream,
  request : &str,
  runtime : &super::ServerRuntime,
  acknowledge : bool,
) {
  use crate::maintenance::save_journal::SaveOperationStatus;
  use crate::serve::protocol::TcpToClient;
  use crate::serve::util::{send_response_with_length_prefix, tag_text_response};
  let result = (|| -> Result<String, String> {
    let operation_id = value_from_request_sexp ("operation-id", request)?;
    let fingerprint = value_from_request_sexp ("request-base-fingerprint", request)?;
    let snapshot = runtime . selected_snapshot ();
    let store = save_journal_for_config (&snapshot . env . config);
    let loaded = if acknowledge {
      store . acknowledge_delivery (&operation_id, &fingerprint)
    } else { store . status (&operation_id, &fingerprint) };
    let mut terminal : Option<String> = None;
    let mut reason : Option<String> = None;
    let state : &str = match loaded {
      Err (SaveJournalError::OperationNotFound { .. }) => "unknown",
      Err (error) => { reason = Some (error . to_string ()); "blocked" }
      Ok (operation) => {
        let state : &str = match &operation . status {
          SaveOperationStatus::StagingUnAuthorized | SaveOperationStatus::PreparedUnAuthorized => "prepared",
          SaveOperationStatus::Authorized { .. } => "authorized",
          SaveOperationStatus::AppliedAwaitingCommit => "applied",
          SaveOperationStatus::Committed { .. } => "committed",
          SaveOperationStatus::Refused { .. } => "refused",
        };
        if !acknowledge {
          match operation . status {
            SaveOperationStatus::Committed { outcome, .. }
            | SaveOperationStatus::Refused { outcome, .. } => {
              let active = runtime . interactive . lock () . unwrap () . active_source_set . clone ();
              if operation . interpretation_identity == interpretation_identity (&snapshot . env . config, &active) {
                terminal = Some (String::from_utf8 (outcome . client_result)
                  . map_err (|error| error . to_string ())?);
              } else {
                reason = Some ("the outcome is retained under a different source authorization context; open a fresh live view" . into ());
              }
            }
            _ => {}
          }
        }
        state
      }
    };
    let field = |key : &str, value : &str| sexp::Sexp::List (vec![
      sexp::Sexp::Atom (sexp::Atom::S (key . into ())),
      sexp::Sexp::Atom (sexp::Atom::S (value . into ())),
    ]);
    let mut fields = vec![
      field ("response-type", if acknowledge { "save-operation-ack" } else { "save-operation-status" }),
      field ("operation-id", &operation_id),
      field ("request-base-fingerprint", &fingerprint),
      field ("state", state),
    ];
    if let Some (response) = terminal { fields . push (field ("terminal-response", &response)); }
    if let Some (reason) = reason { fields . push (field ("reason", &reason)); }
    Ok (sexp::Sexp::List (fields) . to_string ())
  }) ();
  let response = result . unwrap_or_else (|reason| tag_text_response (TcpToClient::Error, &reason));
  let _ = send_response_with_length_prefix (stream, &response);
}
