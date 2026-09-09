//! TCP control and conditional delivery of durable explicit queries.

use super::maintenance_protocol::with_current_state_fields;
use super::query_wait_protocol::{parse_registration, status_fields};
use crate::from_text::buffer_to_viewnodes::uninterpreted::org_to_uninterpreted_viewforest;
use crate::maintenance::candidate::source_catalog_blake3;
use crate::maintenance::query_waits::{QueryWaitDestination, QueryWaitRecord, QueryWaitResult, QueryWaitState, QueryWaitTarget, QueryWaitTargetOutcome};
use crate::maintenance::types::{BufferKind, CoordinatorState};
use crate::runtime::query_waits::decode_config;
use crate::runtime::{SelectedRuntimeSnapshot, ServerRuntime};
use crate::runtime::interactive_session::InteractiveSession;
use crate::serve::protocol::RequestType;
use crate::serve::util::{request_context_active, send_response_with_length_prefix, value_from_request_sexp};
use crate::source_sets::{ActiveSourceSet, SourceSetName};
use crate::types::maybe_placed_viewnode::maybePlaced_to_placed_viewforest;
use crate::types::misc::{ID, SkgConfig};
use crate::types::tree::forest::ViewForest;
use crate::types::views_state::{pids_from_viewforest, ViewState, ViewUri};

use sexp::{Atom, Sexp};
use sha2::{Digest, Sha256};
use std::collections::{BTreeMap, BTreeSet};
use std::net::TcpStream;
use std::sync::{Arc, MutexGuard};

#[derive(Default)]
pub(crate) struct QueryDeliveries {
  requested : BTreeSet<String>,
  sent : BTreeMap<String, String>,
}

pub(crate) fn handle_request (
  runtime : &ServerRuntime,
  stream : &mut TcpStream,
  request : &str,
  kind : RequestType,
  deliveries : &mut QueryDeliveries,
) {
  let operation_id : String = value_from_request_sexp ("query-operation-id", request) . unwrap_or_default ();
  let result : Result<Vec<Sexp>, String> = control_request (runtime, request, kind, deliveries);
  let mut fields : Vec<Sexp> = match result {
    Ok (fields) => fields,
    Err (reason) => vec![text_field ("response-type", "query-wait-status"),
      text_field ("query-operation-id", &operation_id), text_field ("status", "refused"),
      text_field ("reason", &reason)],
  };
  fields . push (text_field ("terminal-status", "complete"));
  send_fields (runtime, stream, fields);
}

fn control_request (
  runtime : &ServerRuntime,
  request : &str,
  kind : RequestType,
  deliveries : &mut QueryDeliveries,
) -> Result<Vec<Sexp>, String> {
  runtime . validate_session_authority (request)?;
  let operation_id : String = value_from_request_sexp ("query-operation-id", request)?;
  uuid::Uuid::parse_str (&operation_id) . map_err (|_| "invalid query operation UUID")?;
  match kind {
    RequestType::QueryWait => {
      let existing : Option<QueryWaitRecord> = runtime . maintenance_snapshot ()
        . query_waits . get (&operation_id) . cloned ();
      let config : SkgConfig = match &existing {
        Some (record) => decode_config (&record . recipe . config_snapshot)?,
        None => runtime . selected_snapshot () . env . config . clone (), };
      let active : ActiveSourceSet = match &existing {
        Some (record) => ActiveSourceSet::named (&config, SourceSetName::from (record . recipe . source_set . as_str ()))
          . map_err (|error| error . to_string ())?,
        None => runtime . interactive . lock () . map_err (|_| "interactive query state is unavailable")?
          . active_source_set . clone (), };
      let record : QueryWaitRecord = parse_registration (request, &config, &active)?;
      let retained : Option<QueryWaitTargetOutcome> = crate::runtime::query_waits::retained_outcome (runtime, &record)?;
      runtime . transition_maintenance (|coordinator| {
        if coordinator . query_waits . get (&operation_id) . is_none () {
          let known : bool = match &record . target {
            QueryWaitTarget::Incident { incident_id, epoch } => coordinator . incidents () . iter ()
              . any (|incident| &incident . incident_id == incident_id && &incident . epoch == epoch),
            QueryWaitTarget::Candidate { candidate_id } => match &coordinator . state {
              CoordinatorState::Pending (pending) => pending . candidate . as_ref ()
                . is_some_and (|candidate| &candidate . id == candidate_id),
              CoordinatorState::Active (active) => active . candidate . as_ref ()
                . is_some_and (|candidate| &candidate . id == candidate_id), _ => false, }, };
          if !known { return Err ("query target is not a retained maintenance outcome" . into ()); }
        }
        let registered : bool = coordinator . register_query_wait (record . clone ())?;
        if let Some (outcome) = &retained {
          coordinator . resolve_query_wait_target (&operation_id, outcome . clone ())?;
        }
        Ok (registered)
      })?;
      deliveries . requested . insert (operation_id . clone ());
      deliveries . sent . remove (&operation_id);
    }
    RequestType::QueryWaitCancel => {
      runtime . transition_maintenance (|coordinator| coordinator
        . cancel_query_wait (&operation_id, "cancelled by the requesting editor" . into ()))?;
      deliveries . requested . remove (&operation_id);
    }
    RequestType::QueryWaitApplied => {
      let record : QueryWaitRecord = lookup (runtime, &operation_id)?;
      require_interpretation (runtime, &record)?;
      if !matches! (record . state, QueryWaitState::Ready | QueryWaitState::Delivered) {
        return Err ("query result is not awaiting or repeating delivery" . into ()); }
      if value_from_request_sexp ("applied", request)? != "true" || !destination_matches (&record, request, true)? {
        return Err ("query result ACK does not name its applied destination" . into ()); }
      let digest : String = value_from_request_sexp ("result-digest", request)?;
      if record . result . as_ref () . is_none_or (|result| result . content_sha256 != digest) {
        return Err ("query result ACK checksum differs from the staged result" . into ()); }
      // Verify and register read-only text before durable delivery ACK. Replay
      // can repeat this after a crash; it cannot issue save authority.
      register_applied_view (runtime, &record)?;
      runtime . transition_maintenance (|coordinator| coordinator . acknowledge_query_wait (&operation_id, &digest))?;
      let mut fields : Vec<Sexp> = status_fields (&lookup (runtime, &operation_id)?, false);
      set_text_field (&mut fields, "response-type", "query-wait-applied");
      return Ok (fields);
    }
    RequestType::QueryWaitStatus => {
      let record : QueryWaitRecord = lookup (runtime, &operation_id)?;
      if value_from_request_sexp ("retry", request) . ok () . as_deref () == Some ("true") {
        runtime . transition_maintenance (|coordinator| coordinator . retry_query_wait (&operation_id))?;
      }
      if value_from_request_sexp ("client-buffer-id", request) . is_ok () {
        if destination_matches (&record, request, false)? || destination_matches (&record, request, true)? {
          deliveries . requested . insert (operation_id . clone ());
          deliveries . sent . remove (&operation_id);
        } else { deliveries . requested . remove (&operation_id); }
      }
    }
    _ => return Err ("unsupported query wait control request" . into ()),
  }
  let record : QueryWaitRecord = lookup (runtime, &operation_id)?;
  let authorized : bool = require_interpretation (runtime, &record) . is_ok ();
  let mut fields : Vec<Sexp> = status_fields (&record, authorized && kind == RequestType::QueryWaitStatus);
  if !authorized {
    set_text_field (&mut fields, "status", "blocked");
    set_text_field (&mut fields, "reason", "query belongs to another source interpretation");
  }
  Ok (fields)
}

pub(crate) fn drain (
  runtime : &ServerRuntime,
  stream : &mut TcpStream,
  deliveries : &mut QueryDeliveries,
) {
  if request_context_active () { return; }
  for operation_id in deliveries . requested . clone () {
    let Ok (record) = lookup (runtime, &operation_id) else { continue; };
    let ready_signature : Option<String> = record . result . as_ref ()
      . filter (|_| record . state == QueryWaitState::Ready)
      . map (|result| format! ("result:{}", result . artifact_sha256));
    if ready_signature . as_ref () . is_some_and (|signature|
      deliveries . sent . get (&operation_id) == Some (signature))
    && require_interpretation (runtime, &record) . is_ok () { continue; }
    let mut fields : Vec<Sexp> = if record . state == QueryWaitState::Ready {
      match result_fields (runtime, &record) {
        Ok (fields) => fields,
        Err (reason) => {
          let mut fields : Vec<Sexp> = status_fields (&record, false);
          set_text_field (&mut fields, "status", "blocked");
          set_text_field (&mut fields, "reason", &reason);
          fields
        }
      }
    } else { status_fields (&record, false) };
    let is_result : bool = fields . contains (&text_field ("response-type", "query-wait-result"));
    // Read and hash staged content once per requested delivery. Freshness or
    // unrelated owner changes cannot cause automatic content retransmission.
    let signature : String = if is_result { ready_signature . expect ("ready result") }
      else { format! ("{:x}", Sha256::digest (Sexp::List (fields . clone ()) . to_string () . as_bytes ())) };
    if deliveries . sent . get (&operation_id) == Some (&signature) { continue; }
    let frame : &str = if is_result { "query-wait-result" } else { "query-wait-status" };
    fields . extend ([text_field ("server-push", "true"), text_field ("frame-kind", frame),
      text_field ("operation-id", &operation_id)]);
    send_fields (runtime, stream, fields);
    deliveries . sent . insert (operation_id, signature);
  }
}

fn result_fields (
  runtime : &ServerRuntime,
  record : &QueryWaitRecord,
) -> Result<Vec<Sexp>, String> {
  require_interpretation (runtime, record)?;
  let result : &QueryWaitResult = record . result . as_ref () . ok_or ("query result is not staged")?;
  let content : String = runtime . read_query_wait_result (record)?;
  let raw_recipe : &str = record . client_recipe . as_deref () . ok_or ("query result lacks its client recipe identity")?;
  let base : &QueryWaitDestination = &record . destination;
  let target_token : u64 = base . base_application_token . checked_add (1) . ok_or ("query application token exhausted")?;
  let target_revision : u64 = base . base_server_revision . checked_add (1) . ok_or ("query view revision exhausted")?;
  let (_, current, coordinator, _) = runtime . publication ();
  let freshness : &str = if current . selected . graph_generation . get () != result . graph_generation {
    "stale-search-membership"
  } else if !matches! (coordinator . state, CoordinatorState::Idle) { "pending-reconciliation" }
  else { "current" };
  let mut fields : Vec<Sexp> = vec![
    text_field ("response-type", "query-wait-result"), text_field ("query-operation-id", &record . operation_id),
    text_field ("view-write-authority", "read-only"), text_field ("view-uri", &base . view_uri),
    text_field ("client-buffer-id", base . client_buffer_id . as_deref () . ok_or ("query destination buffer is absent")?),
    text_field ("query-recipe-digest", &format! ("{:x}", Sha256::digest (raw_recipe . as_bytes ()))),
    text_field ("source-set", &result . source_set), text_field ("base-content-sha256", &base . base_content_sha256),
    text_field ("result-digest", &result . content_sha256), text_field ("freshness", freshness),
    number_field ("expected-client-application-token", base . base_application_token),
    number_field ("expected-graph-generation", base . base_graph_generation),
    number_field ("expected-presentation-generation", base . base_presentation_generation),
    number_field ("expected-server-revision", base . base_server_revision),
    number_field ("resulting-client-application-token", target_token),
    number_field ("graph-generation", result . graph_generation),
    number_field ("manifest-revision", result . manifest_revision),
    number_field ("presentation-generation", result . presentation_generation),
    number_field ("server-revision", target_revision), text_field ("content", &content),
  ];
  fields . push (Sexp::List (vec![atom ("warnings"), Sexp::List (result . warnings . iter () . map (|warning| atom (warning)) . collect ())]));
  require_interpretation (runtime, record)?;
  Ok (fields)
}

fn destination_matches (
  record : &QueryWaitRecord,
  request : &str,
  applied : bool,
) -> Result<bool, String> {
  let base : &QueryWaitDestination = &record . destination;
  let (token, graph, presentation, revision, digest) = if applied {
    let Some (result) = &record . result else { return Ok (false); };
    (base . base_application_token . checked_add (1) . ok_or ("query token exhausted")?, result . graph_generation,
      result . presentation_generation, base . base_server_revision . checked_add (1) . ok_or ("query revision exhausted")?,
      result . content_sha256 . as_str ())
  } else { (base . base_application_token, base . base_graph_generation, base . base_presentation_generation,
    base . base_server_revision, base . base_content_sha256 . as_str ()) };
  for (key, expected) in [
    ("view-uri", base . view_uri . as_str ()), ("client-buffer-id", base . client_buffer_id . as_deref () . unwrap_or ("")),
    ("source-set", record . recipe . source_set . as_str ()), ("base-content-sha256", digest),
  ] {
    if value_from_request_sexp (key, request)? != expected { return Ok (false); }
  }
  for (key, expected) in [("client-application-token", token), ("graph-generation", graph),
    ("presentation-generation", presentation), ("server-revision", revision)] {
    if value_from_request_sexp (key, request)? . parse::<u64> () . ok () != Some (expected) { return Ok (false); }
  }
  if let Some (raw) = &record . client_recipe {
    if let Ok (digest) = value_from_request_sexp ("query-recipe-digest", request) {
      if digest != format! ("{:x}", Sha256::digest (raw . as_bytes ())) { return Ok (false); }
    }
  }
  Ok (value_from_request_sexp ("destination-state", request) . ok () . as_deref () == Some ("clean"))
}

fn register_applied_view (
  runtime : &ServerRuntime,
  record : &QueryWaitRecord,
) -> Result<(), String> {
  let result : &QueryWaitResult = record . result . as_ref () . ok_or ("query result is not staged")?;
  let content : String = runtime . read_query_wait_result (record)?;
  let forest : ViewForest = if content == "No matches found.\n" { ViewForest::new () } else {
    let (parsed, errors, _) = org_to_uninterpreted_viewforest (&content)?;
    if !errors . is_empty () { return Err ("staged query result cannot reconstruct its read-only view" . into ()); }
    maybePlaced_to_placed_viewforest (parsed)?
  };
  let uri : ViewUri = ViewUri::from_client_string (record . destination . view_uri . clone ());
  let selected : Arc<SelectedRuntimeSnapshot> = runtime . selected_snapshot ();
  let mut interactive : MutexGuard<'_, InteractiveSession> = runtime . interactive . lock () . map_err (|_| "interactive query state is unavailable")?;
  if let Some (existing) = interactive . views . open_views . views . get (&uri) {
    if existing . client_buffer_id != record . destination . client_buffer_id || existing . writes_admitted {
      return Err ("query destination already names another live view" . into ()); }
    // Replayed delivery ACK must not roll back a view updated since delivery.
    return Ok (( ));
  }
  let pids : Vec<ID> = pids_from_viewforest (&forest) . into_iter () . collect ();
  interactive . views . open_views . register_view_with_authority (
    &selected . selected . graph, uri . clone (), forest, &pids,
    result . graph_generation, result . presentation_generation, record . destination . base_application_token + 1,
    BufferKind::SearchView, result . source_set . clone (), record . client_recipe . clone ());
  let state : &mut ViewState = interactive . views . open_views . views . get_mut (&uri) . expect ("registered query view");
  state . writes_admitted = false;
  state . client_buffer_id = record . destination . client_buffer_id . clone ();
  state . revision = record . destination . base_server_revision + 1;
  state . search_stale = result . graph_generation != selected . selected . graph_generation . get ();
  state . presentation_stale = false;
  Ok (( ))
}

fn require_interpretation (
  runtime : &ServerRuntime,
  record : &QueryWaitRecord,
) -> Result<(), String> {
  let selected : Arc<SelectedRuntimeSnapshot> = runtime . selected_snapshot ();
  let interactive : MutexGuard<'_, InteractiveSession> = runtime . interactive . lock () . map_err (|_| "interactive query state is unavailable")?;
  if interactive . active_source_set . name . 0 != record . recipe . source_set
  || source_catalog_blake3 (&selected . env . config) != record . recipe . source_catalog_blake3 {
    return Err ("query belongs to another source interpretation" . into ()); }
  Ok (( ))
}

fn lookup (
  runtime : &ServerRuntime,
  operation_id : &str,
) -> Result<QueryWaitRecord, String> {
  runtime . maintenance_snapshot () . query_waits . get (operation_id) . cloned ()
    . ok_or_else (|| "unknown query operation ID" . into ())
}

fn send_fields (runtime : &ServerRuntime, stream : &mut TcpStream, fields : Vec<Sexp>) {
  if let Ok (response) = with_current_state_fields (runtime, &Sexp::List (fields) . to_string ()) {
    let _ = send_response_with_length_prefix (stream, &response);
  }
}

fn atom (
  value : &str,
) -> Sexp { Sexp::Atom (Atom::S (value . into ())) }
fn text_field (
  key : &str,
  value : &str,
) -> Sexp { Sexp::List (vec![atom (key), atom (value)]) }
fn number_field (
  key : &str,
  value : u64,
) -> Sexp { Sexp::List (vec![atom (key), Sexp::Atom (Atom::I (value as i64))]) }
fn set_text_field (fields : &mut Vec<Sexp>, key : &str, value : &str) {
  fields . retain (|field| !matches! (field, Sexp::List (parts) if parts . first () == Some (&atom (key))));
  fields . push (text_field (key, value));
}
