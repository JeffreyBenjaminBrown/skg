//! Two-step client census and retained-view reattachment.

use crate::from_text::buffer_to_viewnodes::uninterpreted::org_to_uninterpreted_viewforest;
use crate::dbs::in_rust_graph::InRustGraph;
use crate::maintenance::archive::{
  InitialArchiveExpectation,
  verify_initial_archive,
};
use crate::maintenance::{
  ActiveMaintenance,
  ArchiveStatus,
  BufferKind,
  CoordinatorState,
  MaintenanceCoordinator,
  IncidentId,
  MaintenanceEpoch,
  ViewApplicationAcknowledgement,
  ViewSettlementRequirement,
};
use crate::runtime::ServerRuntime;
use crate::runtime::interactive_session::{
  AttachedClient,
  CensusDescriptor,
  ClientCapabilities,
  ClientKind,
  InteractiveSession,
};
use crate::serve::handlers::reload_recovery::pending_incidents_for_config;
use crate::serve::protocol::TcpToClient;
use crate::serve::util::{
  read_length_prefixed_content,
  send_response_with_length_prefix,
  tag_sexp_response,
  tag_terminal_text_response,
  value_from_request_sexp,
};
use crate::types::env::{GraphReadSnapshot, SkgEnv};
use crate::types::maybe_placed_viewnode::maybePlaced_to_placed_viewforest;
use crate::types::misc::SkgConfig;
use crate::types::sexp::{atom_to_string, extract_v_from_kv_pair_in_sexp};
use crate::types::store_state::{SelectedStoreState, StoreHealth};
use crate::types::views_state::{
  ViewSaveBase,
  ViewState,
  ViewUri,
  pids_from_viewforest,
  root_ids_from_viewforest,
};
use crate::telescope::invariants::TelescopeViolation;

use super::maintenance_protocol::{
  ServerSettlementEffect,
  prepare_server_settlement_effect,
};

use sexp::{Atom, Sexp};
use sha2::{Digest, Sha256};
use std::collections::{BTreeSet, HashSet};
use std::io::BufReader;
use std::net::TcpStream;
use std::sync::Arc;

pub const PROTOCOL_VERSION : u32 = 2;

pub fn handle_verify_connection_request (
  stream  : &mut TcpStream,
  request : &str,
  runtime : &ServerRuntime,
) {
  let result = (|| -> Result<String, String> {
    let lease = runtime . query_lease ()?;
    let (census_required, client_session_id) = {
      let mut interactive = runtime . interactive . lock () . unwrap ();
      let census_required = install_client_handshake (
        request, &lease . snapshot . env, &mut interactive)?;
      let session_id = interactive . attached_client . as_ref ()
        . expect ("successful handshake installed its client")
        . session_id . clone ();
      (census_required, session_id)
    };
    let active_source_set_name = runtime . interactive . lock () . unwrap ()
      . active_source_set . name . 0 . clone ();
    let abandoned = runtime . transition_maintenance (|coordinator|
      Ok (coordinator . reconnected_for_session (&client_session_id)))?;
    let maintenance = runtime . maintenance_snapshot ();
    Ok (super::maintenance_protocol::with_current_state_fields (runtime, &verify_connection_response (
      &lease . snapshot . env . config,
      &lease . snapshot . env . startup_warnings,
      &lease . snapshot . env . in_rust_graph . load_full (),
      &active_source_set_name,
      census_required,
      &maintenance,
      abandoned . as_ref (), runtime . server_session_id ()) )?)
  }) ();
  let response = match result {
    Ok (response) => response,
    Err (error) => tag_terminal_text_response (
      TcpToClient::Error, "failed", &error),
  };
  let _ = send_response_with_length_prefix (stream, &response);
}

fn install_client_handshake (
  request     : &str,
  env         : &SkgEnv,
  interactive : &mut InteractiveSession,
) -> Result<bool, String> {
  // A failed replacement handshake cannot inherit its predecessor's census.
  interactive . attached_client = None;
  validate_protocol_version (request)?;
  let kind = match value_from_request_sexp ("client-kind", request)? . as_str () {
    "emacs" => ClientKind::Emacs,
    "neovim" => ClientKind::Neovim,
    other => return Err (format! ("unsupported interactive client '{}'", other)),
  };
  let version = value_from_request_sexp ("client-version", request)?;
  let session_id = value_from_request_sexp ("client-session-id", request)?;
  if session_id . is_empty () || session_id . len () > 256 {
    return Err ("client-session-id must contain 1 through 256 bytes" . into ()); }
  let archive_format_version = value_from_request_sexp (
    "archive-format-version", request)? . parse::<u32> ()
    . map_err (|_| "archive-format-version must be an integer" . to_string ())?;
  let native_undo_kind = value_from_request_sexp ("native-undo-kind", request)?;
  let native_undo_version = value_from_request_sexp (
    "native-undo-version", request)?;
  let claimed_source_set = value_from_request_sexp ("source-set", request)?;
  if claimed_source_set != "server-default"
  && claimed_source_set != interactive . active_source_set . name . 0
  {
    return Err (format! (
      "client source-set '{}' does not match retained session source-set '{}'",
      claimed_source_set, interactive . active_source_set . name)); }
  // Verification admits the socket but never grants ordinary request/write
  // authority by itself.  Even an empty editor must explicitly close the
  // census phase so reconnect and restart have the same protocol.
  let census_required = true;
  interactive . attached_client = Some (AttachedClient {
    kind,
    version,
    session_id,
    capabilities: ClientCapabilities {
      archive_format_version,
      native_undo_kind,
      native_undo_version,
    },
    census_complete: false,
  });
  if env . config . maintenance_archive_identity . as_os_str () . is_empty () {
    return Err ("server has no validated maintenance archive root" . into ()); }
  Ok (census_required)
}

pub(crate) fn validate_protocol_version (request : &str) -> Result<(), String> {
  let version : Option<u32> = value_from_request_sexp ("protocol-version", request)
    . ok () . and_then (|version| version . parse () . ok ());
  if version != Some (PROTOCOL_VERSION) {
    return Err (format! (
      "incompatible Skg protocol: server requires version {}; preserve editor text and reconnect with a compatible client",
      PROTOCOL_VERSION)); }
  Ok (( ))
}

fn verify_connection_response (
  config   : &SkgConfig,
  warnings : &[(crate::types::misc::ID, TelescopeViolation)],
  selected : &SelectedStoreState,
  active_source_set_name : &str,
  census_required : bool,
  maintenance : &MaintenanceCoordinator,
  abandoned_prearchive : Option<&(crate::maintenance::IncidentId, String)>,
  server_session_id : &str,
) -> String {
  let atom = |value : &str| -> Sexp {
    Sexp::Atom (Atom::S (value . to_string ())) };
  let field = |key : &str, value : Sexp| -> Sexp {
    Sexp::List (vec! [atom (key), value]) };
  let warning_entries : Vec<Sexp> = warnings . iter ()
    . map ( |(pid, warning)| {
      let (kind, winning_paths, ignored_paths) = match warning {
        TelescopeViolation::IgnoredForeignPidCollision {
          winning_paths, ignored_paths, .. } =>
          ("ignored-foreign-pid-collision",
           winning_paths . as_slice (), ignored_paths . as_slice ()),
        _ => ("telescope-warning", &[][..], &[][..]), };
      let path_list = |paths : &[std::path::PathBuf]| -> Sexp {
        Sexp::List (paths . iter () . map ( |path|
          atom (&path . to_string_lossy ()) ) . collect ()) };
      Sexp::List (vec! [
        field ("pid", atom (pid)),
        field ("kind", atom (kind)),
        field ("message", atom (&warning . to_string ())),
        field ("winning-paths", path_list (winning_paths)),
        field ("ignored-paths", path_list (ignored_paths)),
      ]) })
    . collect ();
  let recovery_entries : Vec<Sexp> = pending_incidents_for_config (config)
    . into_iter () . map (|incident| {
      let fatal = incident . draft . fatal . iter () . map (|(pid, reason)|
        Sexp::List (vec![
          field ("pid", atom (pid)),
          field ("reason", atom (reason)),
        ])) . collect ();
      Sexp::List (vec![
        field ("incident-id", atom (&incident . incident_id)),
        field ("fatal", Sexp::List (fatal)),
      ])
    }) . collect ();
  let health = |health : &StoreHealth| -> Sexp { match health {
    StoreHealth::Healthy => atom ("healthy"),
    StoreHealth::Poisoned (reason) => Sexp::List (vec! [
      atom ("poisoned"), atom (reason)]), }};
  let mut fields = vec! [
    field ("protocol-version", Sexp::Atom (Atom::I (PROTOCOL_VERSION as i64))),
    field ("server-session-id", atom (server_session_id)),
    field ("response-type", atom (
      TcpToClient::VerifyConnection . repr_in_client ())),
    field ("content", atom (
      "This is the skg server verifying the connection.")),
    source_inventory_field (config),
    field ("telescope-warnings", Sexp::List (warning_entries)),
    field ("pending-recovery-incidents", Sexp::List (recovery_entries)),
    field ("active-source-set", atom (active_source_set_name)),
    field ("graph-generation", Sexp::Atom (Atom::I (
      selected . graph_generation . get () as i64))),
    field ("manifest-revision", Sexp::Atom (Atom::I (
      selected . manifest_revision . get () as i64))),
    field ("maintenance-epoch", Sexp::Atom (Atom::I (
      maintenance . epoch . get () as i64))),
    field ("maintenance-state", atom (maintenance . state . label ())),
    field ("census-required", atom (
      if census_required { "true" } else { "nil" })),
    field ("maintenance-archive-folder", atom (
      &config . maintenance_archive_folder . to_string_lossy ())),
    field ("maintenance-archive-identity", atom (
      &config . maintenance_archive_identity . to_string_lossy ())),
    field ("tantivy-health", health (&selected . tantivy_health)),
  ];
  if let Some ((incident, origin)) = abandoned_prearchive {
    fields . push (field (
      "abandoned-prearchive-incident", atom (incident . as_str ())));
    fields . push (field ("abandoned-prearchive-origin", atom (origin)));
  }
  Sexp::List (fields) . to_string ()
}

pub(crate) fn source_inventory_field (config : &SkgConfig) -> Sexp {
  let atom = |value : &str| -> Sexp {
    Sexp::Atom (Atom::S (value . to_string ())) };
  let field = |key : &str, value : Sexp| -> Sexp {
    Sexp::List (vec![atom (key), value]) };
  let entries = config . ordered_sources () . into_iter () . enumerate ()
    . map (|(position, name)| {
      let source = config . sources . get (&name)
        . expect ("ordered source exists");
      Sexp::List (vec![
        field ("name", atom (&name)),
        field ("abbreviation", source . abbreviation . as_deref ()
          . map (&atom) . unwrap_or_else (|| atom ("nil"))),
        field ("owned", atom (
          if source . user_owns_it { "true" } else { "nil" })),
        field ("position", Sexp::Atom (Atom::I (position as i64))),
        field ("configured-path", atom (
          &config . sources . configured_path (&name)
            . unwrap_or (&source . path) . to_string_lossy ())),
        field ("directory", atom (&source . path . to_string_lossy ())),
        field ("directory-identity", atom (
          &config . sources . directory_identity (&name)
            . unwrap_or (&source . path) . to_string_lossy ())),
      ])
    }) . collect ();
  field ("source-inventory", Sexp::List (entries))
}

pub fn handle_client_census_request (
  reader       : &mut BufReader<TcpStream>,
  stream       : &mut TcpStream,
  request      : &str,
  env          : &SkgEnv,
  interactive  : &mut InteractiveSession,
  writes_allowed : bool,
  runtime      : &ServerRuntime,
) {
  let result = (|| -> Result<String, String> {
    let selected_env : SkgEnv = env . pinned ();
    let payload = read_length_prefixed_content (reader)
      . map_err (|error| format! ("could not read client census: {}", error))?;
    let descriptors = parse_descriptors (&payload)?;
    let requested_epoch = requested_maintenance_epoch (request)?;
    let live_buffer_ids : BTreeSet<String> = descriptors . iter ()
      . map (|descriptor| descriptor . buffer_id . clone ()) . collect ();
    let current_generation = selected_env . in_rust_graph . load_full ()
      . graph_generation . get ();
    let maintenance = runtime . maintenance_snapshot ();
    let mut live_uris : HashSet<ViewUri> = HashSet::new ();
    let text_required : Vec<String> = Vec::new ();
    let mut stale : Vec<String> = Vec::new ();
    let mut presentation_stale : Vec<String> = Vec::new ();
    let mut census_applications = Vec::new ();
    let mut enrollment_records : Vec<_> = descriptors . iter ()
      . filter (|descriptor| descriptor . view_uri . is_none ())
      . map (CensusDescriptor::frozen_record)
      . collect::<Result<_, _>> ()?;
    interactive . pending_census_texts . clear ();
    interactive . live_census = descriptors . iter () . map (|descriptor|
      (descriptor . buffer_id . clone (), descriptor . clone ())) . collect ();

    for descriptor in descriptors {
      let Some (uri) = descriptor . view_uri . clone () else { continue; };
      if !descriptor_has_session_authority (&descriptor, runtime . server_session_id ()) {
        stale . push (descriptor . buffer_id . clone ());
        continue;
      }
      let descriptor_kind = validate_live_descriptor (&descriptor)?;
      let census_graph : Arc<InRustGraph> = census_application_graph (
        runtime, &maintenance, &descriptor)?;
      let census_application = census_application_ack (
        &census_graph, &maintenance, &descriptor)?;
      if !live_uris . insert (uri . clone ()) {
        return Err (format! (
          "client census names view '{}' more than once",
          uri . repr_in_client ())); }
      match interactive . views . open_views . views . get_mut (&uri) {
        Some (state) if state_matches_descriptor (
          state, &descriptor, &descriptor_kind) =>
        {
          state . client_buffer_id = Some (descriptor . buffer_id . clone ());
          enrollment_records . push (descriptor . frozen_record ()?);
        }
        Some (_) if census_application . is_some () => {
          census_applications . push ((
            descriptor . clone (), census_application . unwrap ()));
        }
        Some (state) if server_requires_presentation_stale (
            state, &descriptor, &descriptor_kind) =>
        {
          state . client_buffer_id = Some (descriptor . buffer_id . clone ());
          enrollment_records . push (descriptor . frozen_record ()?);
          presentation_stale . push (uri . repr_in_client ());
        }
        Some (_) => stale . push (descriptor . buffer_id . clone ()),
        None if unmaterialized_new_empty_authority (
            &descriptor, &descriptor_kind, current_generation) =>
        {
          // A new-empty view is client-created authority until its first save.
          // There is deliberately no server forest to reconstruct from its
          // initial (possibly id-less) text.  Preserve the descriptor for
          // maintenance enrollment; the save endpoint remains the only place
          // which may materialize that text as a server view.
          enrollment_records . push (descriptor . frozen_record ()?);
        }
        // A missing regular view has no proven server base. Client-provided
        // text and equal generation counters cannot manufacture that proof.
        None => stale . push (descriptor . buffer_id . clone ()),
      }
    }

    let absent_server_views : Vec<ViewUri> = interactive . views . open_views
      . views . keys () . filter (|uri| !live_uris . contains (*uri))
      . cloned () . collect ();
    for uri in absent_server_views {
      interactive . views . open_views . unregister_view (&uri); }

    runtime . transition_maintenance (|coordinator|
      coordinator . enroll_presentation_census (
        enrollment_records . clone (), requested_epoch)
        . map (|_| ( )))?;
    reconcile_maintenance_census (runtime, interactive, &live_buffer_ids)?;
    reconcile_census_applications (
      runtime, interactive, &census_applications)?;

    let complete = text_required . is_empty ();
    if let Some (client) = &mut interactive . attached_client {
      client . census_complete = complete; }
    Ok (census_response (
      complete, writes_allowed && complete, &text_required, &stale,
      &presentation_stale))
  })();
  send_result (stream, result);
}

/// Bind the complete census to durable maintenance before granting the
/// replacement editor normal protocol authority.  Before store selection, a
/// restarted runtime also reconstructs its verified-archive cache from the
/// immutable initial checksum; after selection, the journaled selected store
/// and evidence records are the point-of-no-return authority.
fn reconcile_maintenance_census (
  runtime         : &ServerRuntime,
  interactive     : &InteractiveSession,
  live_buffer_ids : &BTreeSet<String>,
) -> Result<(), String> {
  let attached_session_id = interactive . attached_client . as_ref ()
    . ok_or_else (|| "client census has no attached session" . to_string ())?
    . session_id . clone ();
  let coordinator = runtime . maintenance_snapshot ();
  let verified = match &coordinator . state {
    CoordinatorState::Active (active)
      if active . selected_store . is_none ()
      && matches! (active . archive_status, ArchiveStatus::Ready { .. })
      && runtime . verified_archive (&active . incident_id) . is_none () =>
    {
      let manifest_sha256 = active . initial_archive_manifest_sha256
        . as_deref () . ok_or_else (||
          "active archive-ready incident has no initial checksum" . to_string ())?;
      let selected = runtime . selected_snapshot ();
      Some ((active . incident_id . clone (), verify_initial_archive (
        InitialArchiveExpectation {
          archive_root: &selected . env . config . maintenance_archive_identity,
          active,
          manifest_sha256,
        })?))
    }
    _ => None,
  };
  runtime . transition_maintenance (|coordinator| {
    coordinator . adopt_attached_session (&attached_session_id)?;
    coordinator . reconcile_absent_preselection_retirements (live_buffer_ids);
    for incident in coordinator . incidents () {
      if incident . terminal_acknowledged { continue; }
      if matches! (&coordinator . state, CoordinatorState::Terminal (terminal)
        if terminal . incident_id == incident . incident_id) { continue; }
      coordinator . adopt_incident_session (
        &incident . incident_id, incident . epoch, &attached_session_id)?;
      if incident . disposition . is_none () {
        coordinator . reconcile_incident_view_settlements (
          &incident . incident_id, incident . epoch, live_buffer_ids)?; }
    }
    Ok (( ))
  })?;
  if let Some ((incident, verified)) = verified {
    runtime . retain_verified_archive (incident, verified); }
  Ok (( ))
}

fn census_application_ack (
  graph : &crate::dbs::in_rust_graph::InRustGraph,
  coordinator : &MaintenanceCoordinator,
  descriptor : &CensusDescriptor,
) -> Result<Option<(IncidentId, MaintenanceEpoch, ViewApplicationAcknowledgement)>, String> {
  let Some (summary) = coordinator . incidents () . into_iter () . find (|incident|
    Some (incident . epoch . get ()) == descriptor . maintenance_epoch
    && incident . disposition . is_none ()) else { return Ok (None); };
  let active = coordinator . incident (&summary . incident_id, summary . epoch)?;
  let Some (record) = active . view_settlements . get (&descriptor . buffer_id)
    else { return Ok (None); };
  if record . acknowledged
  || record . requirement != ViewSettlementRequirement::ApplicationAck
  {
    return Ok (None); }
  let Some (application) = &record . application else {
    return Err (format! (
      "buffer '{}' has application debt without a staged offer",
      descriptor . buffer_id)); };
  let Some (frozen) = active . presentation_buffer (&descriptor . buffer_id)
    else {
      return Err (format! (
        "buffer '{}' application debt is absent from the frozen census",
        descriptor . buffer_id)); };
  let offered_sha = sha256 (&application . content);
  if offered_sha != application . content_sha256 {
    return Err (format! (
      "buffer '{}' staged application checksum is inconsistent",
      descriptor . buffer_id)); }
  let (maybe_placed, parse_errors, _) = org_to_uninterpreted_viewforest (
    &application . content) . map_err (|error| format! (
      "could not reconstruct staged census application '{}': {}",
      descriptor . buffer_id, error))?;
  if !parse_errors . is_empty () {
    return Err (format! (
      "staged census application '{}' reparsed with errors",
      descriptor . buffer_id)); }
  let offered_forest = maybePlaced_to_placed_viewforest (maybe_placed)
    . map_err (|error| format! (
      "could not place staged census application '{}': {}",
      descriptor . buffer_id, error))?;
  let offered_roots : HashSet<String> = root_ids_from_viewforest (
    graph, &offered_forest) . into_iter () . map (|id| id . 0) . collect ();
  let described_roots : HashSet<String> = descriptor . root_ids . iter ()
    . cloned () . collect ();
  let described_uri = descriptor . view_uri . as_ref ()
    . map (ViewUri::repr_in_client);
  let expected_search_stale = frozen . search_stale
    || record . kind == BufferKind::SearchView;
  if descriptor . dirty
  || descriptor . logical_dirty
  || descriptor . lifecycle != "live-view"
  || descriptor . kind != record . kind . label ()
  || described_uri . as_deref () != record . view_uri . as_deref ()
  || descriptor . recipe != frozen . recipe
  || descriptor . source_set != frozen . source_set
  || descriptor . graph_generation != application . resulting_graph_generation
  || descriptor . presentation_generation
       != application . resulting_presentation_generation
  || descriptor . server_revision != application . resulting_server_revision
  || descriptor . application_token
       != application . resulting_application_token
  || descriptor . maintenance_epoch != Some (active . epoch . get ())
  || descriptor . presentation_stale
  || descriptor . search_stale != expected_search_stale
  || descriptor . last_fetched_sha256 != offered_sha
  || descriptor . current_sha256 != offered_sha
  || described_roots != offered_roots
  {
    return Ok (None); }
  Ok (Some ((active . incident_id . clone (), active . epoch, ViewApplicationAcknowledgement {
    content_sha256: offered_sha,
    resulting_graph_generation: descriptor . graph_generation,
    resulting_presentation_generation: descriptor . presentation_generation,
    resulting_server_revision: descriptor . server_revision,
    resulting_application_token: descriptor . application_token,
  })))
}

fn census_application_graph (
  runtime     : &ServerRuntime,
  coordinator : &MaintenanceCoordinator,
  descriptor  : &CensusDescriptor,
) -> Result<Arc<InRustGraph>, String> {
  let Some (summary) = coordinator . incidents () . into_iter () . find (|incident|
    Some (incident . epoch . get ()) == descriptor . maintenance_epoch
    && incident . disposition . is_none ()) else {
      return Ok (runtime . selected_snapshot () . selected . graph . clone ()); };
  let active : &ActiveMaintenance = coordinator . incident (
    &summary . incident_id, summary . epoch)?;
  if active . authority_retired_by_session . is_some () {
    return Ok (runtime . selected_snapshot () . selected . graph . clone ()); }
  let has_live_application : bool = active . view_settlements
    . get (&descriptor . buffer_id)
    . map (|record| record . requirement == ViewSettlementRequirement::ApplicationAck
      && !record . acknowledged && record . application . is_some ())
    . unwrap_or (false);
  if !has_live_application {
    return Ok (runtime . selected_snapshot () . selected . graph . clone ()); }
  Ok (runtime . incident_snapshot (&summary . incident_id)? . selected
    . graph . clone ())
}

fn reconcile_census_applications (
  runtime      : &ServerRuntime,
  interactive  : &mut InteractiveSession,
  applications : &[(CensusDescriptor, (IncidentId, MaintenanceEpoch, ViewApplicationAcknowledgement))],
) -> Result<(), String> {
  if applications . is_empty () { return Ok (( )); }
  let coordinator = runtime . maintenance_snapshot ();
  let mut effects = Vec::new ();
  for (descriptor, (incident, epoch, acknowledgement)) in applications {
    let active = coordinator . incident (incident, *epoch)?;
    let record = active . view_settlements . get (&descriptor . buffer_id)
      . ok_or_else (|| format! (
        "buffer '{}' lost its application settlement",
        descriptor . buffer_id))?;
    let uri = descriptor . view_uri . as_ref ()
      . ok_or_else (|| "census application has no view URI" . to_string ())?;
    let state = interactive . views . open_views . views . get (uri)
      . ok_or_else (|| format! (
        "buffer '{}' census application has no retained forest",
        descriptor . buffer_id))?;
    if state_matches_descriptor (state, descriptor, &record . kind) {
      effects . push ((incident . clone (), ServerSettlementEffect::None));
    } else {
      effects . push ((incident . clone (), prepare_server_settlement_effect (
        active, record, Some (state), Some (acknowledgement))?)); }
  }
  runtime . transition_maintenance (|coordinator| {
    for (descriptor, (incident, epoch, acknowledgement)) in applications {
      coordinator . acknowledge_incident_application_from_census (
        incident, *epoch, &descriptor . buffer_id, acknowledgement)?; }
    Ok (( ))
  })?;
  for (incident, effect) in effects {
    match effect {
      ServerSettlementEffect::None => {}
      ServerSettlementEffect::Apply {
        uri, base_revision, viewforest, graph_generation,
        presentation_generation, application_token, search_stale,
      } => {
        let selected_snapshot : Arc<GraphReadSnapshot> =
          runtime . incident_snapshot (&incident)?;
        let source_set : String = interactive . views . open_views . views
          . get (&uri) . expect ("census-applied view remains registered")
          . source_set . clone ();
        if !interactive . views . open_views . update_view_if_revision (
            &selected_snapshot . selected . graph, &uri, base_revision, viewforest)
        {
          return Err ("census application base advanced after validation"
            . into ()); }
        interactive . views . open_views . set_client_application_authority (
          &uri, graph_generation, presentation_generation, application_token)?;
        let state : &mut ViewState = interactive . views . open_views . views . get_mut (&uri)
          . expect ("census-applied view remains registered");
        state . retain_save_base (ViewSaveBase::from_snapshot (
          &selected_snapshot, &source_set))?;
        state . search_stale |= search_stale;
      }
      _ => return Err (
        "application census prepared a non-application server effect" . into ()),
    }
  }
  Ok (( ))
}

pub fn handle_client_census_texts_request (
  reader       : &mut BufReader<TcpStream>,
  stream       : &mut TcpStream,
  request      : &str,
  env          : &SkgEnv,
  interactive  : &mut InteractiveSession,
  writes_allowed : bool,
  runtime      : &ServerRuntime,
) {
  let result = (|| -> Result<String, String> {
    let selected_env : SkgEnv = env . pinned ();
    let payload = read_length_prefixed_content (reader)
      . map_err (|error| format! ("could not read census texts: {}", error))?;
    let records = parse_text_records (&payload)?;
    let requested_epoch = requested_maintenance_epoch (request)?;
    let mut restored : Vec<String> = Vec::new ();
    let mut stale : Vec<String> = Vec::new ();
    let mut restored_descriptors = Vec::new ();
    for (buffer_id, last_fetched, current) in records {
      let Some (descriptor) = interactive . pending_census_texts
        . remove (&buffer_id)
      else {
        return Err (format! (
          "census texts include unrequested buffer '{}'", buffer_id)); };
      if !descriptor_has_session_authority (&descriptor, runtime . server_session_id ())
      || descriptor . graph_generation != selected_env . in_rust_graph . load_full ()
        . graph_generation . get ()
      {
        stale . push (buffer_id);
        continue; }
      if sha256 (&last_fetched) != descriptor . last_fetched_sha256
         || sha256 (&current) != descriptor . current_sha256
      {
        stale . push (buffer_id);
        continue; }
      let Some (uri) = descriptor . view_uri . clone () else {
        continue; };
      let descriptor_kind = validate_live_descriptor (&descriptor)?;
      let (maybe_placed, parse_errors, _) = org_to_uninterpreted_viewforest (
        &last_fetched).map_err (|error| format! (
          "could not reconstruct '{}': {}", buffer_id, error))?;
      if !parse_errors . is_empty () {
        stale . push (buffer_id);
        continue; }
      let viewforest = maybePlaced_to_placed_viewforest (maybe_placed)
        . map_err (|error| format! (
          "could not place reconstructed '{}': {}", buffer_id, error))?;
      let pids : Vec<_> = pids_from_viewforest (&viewforest)
        . into_iter () . collect ();
      interactive . views . open_views . register_view_with_authority (
        &selected_env . in_rust_graph_snapshot (), uri . clone (), viewforest, &pids,
        descriptor . graph_generation,
        descriptor . presentation_generation,
        descriptor . application_token,
        descriptor_kind,
        descriptor . source_set . clone (),
        Some (descriptor . recipe . clone ()));
      interactive . views . open_views . views . get_mut (&uri)
        . expect ("reconstructed census view exists")
        . revision = descriptor . server_revision;
      let state = interactive . views . open_views . views . get_mut (&uri)
        . expect ("reconstructed census view exists");
      state . retain_save_base (ViewSaveBase::from_env (
        &selected_env, &descriptor . source_set))?;
      state . client_buffer_id = Some (descriptor . buffer_id . clone ());
      state . presentation_stale = descriptor . presentation_stale;
      state . search_stale = descriptor . search_stale;
      let restored_roots : HashSet<String> = state . root_ids . iter ()
        . map (|id| id . 0 . clone ()) . collect ();
      let described_roots : HashSet<String> = descriptor . root_ids . iter ()
        . cloned () . collect ();
      if restored_roots != described_roots {
        interactive . views . open_views . unregister_view (&uri);
        stale . push (descriptor . buffer_id);
        continue; }
      restored . push (descriptor . buffer_id . clone ());
      restored_descriptors . push (descriptor);
    }
    stale . extend (
      interactive . pending_census_texts . keys () . cloned ());
    interactive . pending_census_texts . clear ();
    let enrollment_records : Vec<_> = restored_descriptors . iter ()
      . map (CensusDescriptor::frozen_record)
      . collect::<Result<_, _>> ()?;
    runtime . transition_maintenance (|coordinator|
      coordinator . enroll_presentation_census (
        enrollment_records . clone (), requested_epoch)
        . map (|_| ( )))?;
    let maintenance = runtime . maintenance_snapshot ();
    let mut census_applications = Vec::new ();
    for descriptor in &restored_descriptors {
      let census_graph : Arc<InRustGraph> = census_application_graph (
        runtime, &maintenance, descriptor)?;
      if let Some (ack) = census_application_ack (
          &census_graph, &maintenance, descriptor)?
      {
        census_applications . push ((descriptor . clone (), ack)); }
    }
    reconcile_census_applications (
      runtime, interactive, &census_applications)?;
    if let Some (client) = &mut interactive . attached_client {
      client . census_complete = true; }
    let mut response = census_response (
      true, writes_allowed, &[], &stale, &[]);
    let Ok (Sexp::List (mut fields)) = sexp::parse (&response) else {
      unreachable! (); };
    fields . push (list_field ("restored-buffer-ids", &restored));
    response = Sexp::List (fields) . to_string ();
    Ok (response)
  })();
  send_result (stream, result);
}

fn parse_descriptors (payload : &str) -> Result<Vec<CensusDescriptor>, String> {
  let parsed = sexp::parse (payload)
    . map_err (|error| format! ("invalid census S-expression: {}", error))?;
  let records = match parsed {
    Sexp::List (records) => records,
    Sexp::Atom (Atom::S (nil)) if nil == "nil" => Vec::new (),
    _ => return Err ("client census must be a list" . into ()),
  };
  let mut result = Vec::new ();
  let mut ids = HashSet::new ();
  for record in records {
    if !matches! (record, Sexp::List (_)) {
      return Err ("each census descriptor must be a list" . into ()); }
    let buffer_id = field (&record, "buffer-id")?;
    if !ids . insert (buffer_id . clone ()) {
      return Err (format! ("duplicate census buffer-id '{}'", buffer_id)); }
    let uri = field (&record, "view-uri")?;
    let mut root_ids = list_field_values (&record, "root-ids")?;
    root_ids . sort ();
    root_ids . dedup ();
    let dirty = bool_field (&record, "dirty")?;
    let last_fetched_sha256 = sha256_field (&record, "last-fetched-sha256")?;
    let current_sha256 = sha256_field (&record, "current-sha256")?;
    result . push (CensusDescriptor {
      buffer_id,
      server_session_id: field (&record, "server-session-id")?,
      writes_admitted: match field (&record, "view-write-authority")? . as_str () {
        "editable" => true,
        "read-only" => false,
        _ => return Err ("invalid census view-write-authority" . into ()),
      },
      kind: field (&record, "kind")?,
      lifecycle: field (&record, "lifecycle")?,
      disposable: bool_field (&record, "disposable")?,
      continuation_id: optional_text_field (&record, "continuation-id")?,
      origin_buffer_id: optional_text_field (&record, "origin-buffer-id")?,
      origin_view_uri: optional_text_field (&record, "origin-view-uri")?,
      origin_application_token: optional_unsigned_field (
        &record, "origin-application-token")?,
      origin_location: optional_text_field (&record, "origin-location")?,
      view_uri: if uri == "nil" { None }
                else { Some (ViewUri::from_client_string (uri)) },
      recipe: normalized_recipe_field (&record)?,
      root_ids,
      source_set: field (&record, "source-set")?,
      graph_generation: unsigned_field (&record, "graph-generation")?,
      presentation_generation: unsigned_field (
        &record, "presentation-generation") . unwrap_or (0),
      server_revision: unsigned_field (&record, "server-revision")?,
      application_token: unsigned_field (&record, "application-token")?,
      dirty,
      logical_dirty: bool_field (&record, "logical-dirty")?,
      // A pre-extension peer which omits this field is conservative: every
      // dirty record might carry native undo which must not be discarded.
      undo_required: field (&record, "undo-required")
        . map (|value| value == "true") . unwrap_or (dirty),
      maintenance_epoch: optional_unsigned_field (&record, "maintenance-epoch")?,
      modification_tick: unsigned_field (&record, "modification-tick")?,
      presentation_stale: bool_field (&record, "presentation-stale")?,
      search_stale: bool_field (&record, "search-stale")?,
      herald_bearing: bool_field (&record, "herald-bearing")?,
      last_fetched_sha256,
      current_sha256,
    });
  }
  Ok (result)
}

fn descriptor_has_session_authority (
  descriptor : &CensusDescriptor,
  server_session_id : &str,
) -> bool {
  !server_session_id . is_empty ()
    && descriptor . server_session_id == server_session_id
}

fn requested_maintenance_epoch (request : &str) -> Result<Option<u64>, String> {
  let parsed = sexp::parse (request)
    . map_err (|error| format! ("invalid census request: {}", error))?;
  let Ok (value) = extract_v_from_kv_pair_in_sexp (
    &parsed, "maintenance-epoch") else { return Ok (None); };
  value . parse::<u64> () . map (Some)
    . map_err (|_| "client census maintenance epoch must be unsigned" . into ())
}

fn parse_text_records (
  payload : &str,
) -> Result<Vec<(String, String, String)>, String> {
  let parsed = sexp::parse (payload)
    . map_err (|error| format! ("invalid census text S-expression: {}", error))?;
  let Sexp::List (records) = parsed else {
    return Err ("census texts must be a list" . into ()); };
  records . into_iter () . map (|record| Ok ((
    field (&record, "buffer-id")?,
    field (&record, "last-fetched")?,
    field (&record, "current")?,
  ))) . collect ()
}

fn field (record : &Sexp, key : &str) -> Result<String, String> {
  extract_v_from_kv_pair_in_sexp (record, key)
}

fn unsigned_field (record : &Sexp, key : &str) -> Result<u64, String> {
  field (record, key)? . parse::<u64> ()
    . map_err (|_| format! ("census field '{}' must be unsigned", key))
}

fn optional_unsigned_field (
  record : &Sexp,
  key    : &str,
) -> Result<Option<u64>, String> {
  let value = field (record, key)?;
  if value == "nil" { return Ok (None); }
  value . parse::<u64> () . map (Some)
    . map_err (|_| format! ("census field '{}' must be unsigned or nil", key))
}

fn optional_text_field (
  record : &Sexp,
  key    : &str,
) -> Result<Option<String>, String> {
  let value = field (record, key)?;
  Ok (if value == "nil" { None } else { Some (value) })
}

fn bool_field (record : &Sexp, key : &str) -> Result<bool, String> {
  match field (record, key)? . as_str () {
    "true" => Ok (true),
    "nil" => Ok (false),
    _ => Err (format! ("census field '{}' must be true or nil", key)),
  }
}

fn list_field_values (record : &Sexp, key : &str) -> Result<Vec<String>, String> {
  let Sexp::List (fields) = record else {
    return Err ("census descriptor must be a list" . into ()); };
  for candidate in fields {
    let Sexp::List (parts) = candidate else { continue; };
    let Some (Sexp::Atom (Atom::S (name))) = parts . first () else {
      continue; };
    if name != key { continue; }
    if parts . len () == 2 {
      return match &parts [1] {
        Sexp::List (values) => values . iter () . map (atom_to_string) . collect (),
        Sexp::Atom (Atom::S (nil)) if nil == "nil" => Ok (Vec::new ()),
        _ => Err (format! ("census field '{}' must be a list", key)),
      }; }
    return parts [1..] . iter () . map (atom_to_string) . collect ();
  }
  Err (format! ("No {} list found in S-expression", key))
}

fn normalized_recipe_field (record : &Sexp) -> Result<String, String> {
  let recipe = field (record, "recipe")?;
  match sexp::parse (&recipe) {
    Ok (Sexp::List (items)) => Ok (Sexp::List (items) . to_string ()),
    Ok (_) => Err ("census recipe must encode a list" . into ()),
    Err (error) => Err (format! ("census recipe is invalid: {}", error)),
  }
}

fn sha256_field (record : &Sexp, key : &str) -> Result<String, String> {
  let value = field (record, key)?;
  if value . len () == 64
     && value . bytes () . all (|byte| byte . is_ascii_hexdigit ())
  { Ok (value . to_ascii_lowercase ()) }
  else { Err (format! ("census field '{}' must be a SHA-256 digest", key)) }
}

fn parse_kind (kind : &str) -> Result<BufferKind, String> {
  let parsed = BufferKind::parse (kind)?;
  if matches! (parsed,
    BufferKind::ContentView
    | BufferKind::NewEmptyContentView
    | BufferKind::SearchView
    | BufferKind::OverrideChoiceMenu)
  { Ok (parsed) }
  else { Err (format! (
    "buffer kind '{}' cannot be reconstructed as a live view", kind)) }
}

fn validate_live_descriptor (
  descriptor : &CensusDescriptor,
) -> Result<BufferKind, String> {
  if descriptor . lifecycle != "live-view" {
    return Err (format! (
      "buffer '{}' has a view URI but lifecycle '{}'",
      descriptor . buffer_id, descriptor . lifecycle)); }
  let kind = parse_kind (&descriptor . kind)?;
  let recipe_kind = recipe_atom (&descriptor . recipe, "kind")
    . ok_or_else (|| format! (
      "live buffer '{}' recipe has no kind", descriptor . buffer_id))?;
  match kind {
    BufferKind::ContentView | BufferKind::OverrideChoiceMenu => {
      if recipe_kind != "single-root" {
        return Err (format! (
          "buffer '{}' has a non-content recipe", descriptor . buffer_id)); }
      let root = recipe_atom (&descriptor . recipe, "root-id")
        . ok_or_else (|| format! (
          "buffer '{}' recipe has no root-id", descriptor . buffer_id))?;
      if ! descriptor . root_ids . contains (&root) {
        return Err (format! (
          "buffer '{}' recipe root is absent from its roots",
          descriptor . buffer_id)); }
      if matches! (kind, BufferKind::OverrideChoiceMenu)
         && !descriptor . disposable
      {
        return Err (format! (
          "override menu '{}' is not disposable", descriptor . buffer_id)); }
    }
    BufferKind::NewEmptyContentView if recipe_kind != "new-empty" => {
      return Err (format! (
        "buffer '{}' has a non-empty-view recipe", descriptor . buffer_id)); }
    BufferKind::SearchView => {
      if recipe_kind != "search"
         || recipe_atom (&descriptor . recipe, "terms") . is_none ()
      {
        return Err (format! (
          "buffer '{}' has an incomplete search recipe",
          descriptor . buffer_id)); }
      for axis in ["body", "operators", "regex"] {
        if !matches! (recipe_atom (&descriptor . recipe, axis) . as_deref (),
          Some ("true" | "nil"))
        {
          return Err (format! (
            "buffer '{}' search recipe has invalid {}",
            descriptor . buffer_id, axis)); }
      }
    }
    _ => {}
  }
  Ok (kind)
}

fn recipe_atom (recipe : &str, key : &str) -> Option<String> {
  let Sexp::List (entries) = sexp::parse (recipe) . ok ()? else {
    return None; };
  entries . iter () . find_map (|entry| {
    let Sexp::List (parts) = entry else { return None; };
    if parts . len () != 2 { return None; }
    if atom_to_string (&parts [0]) . ok ()? != key { return None; }
    atom_to_string (&parts [1]) . ok ()
  })
}

fn state_matches_descriptor (
  state      : &ViewState,
  descriptor : &CensusDescriptor,
  kind       : &BufferKind,
) -> bool {
  let roots : HashSet<String> = state . root_ids . iter ()
    . map (|id| id . 0 . clone ()) . collect ();
  state . writes_admitted == descriptor . writes_admitted
  && state . graph_generation == descriptor . graph_generation
  && state . presentation_generation == descriptor . presentation_generation
  && state . revision == descriptor . server_revision
  && state . client_application_token == descriptor . application_token
  && &state . kind == kind
  && state . recipe . as_deref () == Some (&descriptor . recipe)
  && state . source_set == descriptor . source_set
  && state . presentation_stale == descriptor . presentation_stale
  && state . search_stale == descriptor . search_stale
  && roots == descriptor . root_ids . iter () . cloned () . collect ()
}

/// A queued server refresh is conservative state, not loss of view authority.
/// Reconnect propagates that one-way stale bit instead of retiring an
/// otherwise exact client buffer merely because its status frame was missed.
fn server_requires_presentation_stale (
  state      : &ViewState,
  descriptor : &CensusDescriptor,
  kind       : &BufferKind,
) -> bool {
  if !state . presentation_stale || descriptor . presentation_stale {
    return false; }
  let mut stale_descriptor = descriptor . clone ();
  stale_descriptor . presentation_stale = true;
  state_matches_descriptor (state, &stale_descriptor, kind)
}

/// The one live-view authority which legitimately precedes a server forest.
/// Its current text may differ from its initial text: that is the unsaved node
/// the user is constructing, not text from which census may invent a forest.
fn unmaterialized_new_empty_authority (
  descriptor        : &CensusDescriptor,
  kind              : &BufferKind,
  current_generation : u64,
) -> bool {
  descriptor . writes_admitted
  && kind == &BufferKind::NewEmptyContentView
  && descriptor . graph_generation == current_generation
  && descriptor . server_revision == 0
  && descriptor . application_token == 1
}

fn sha256 (text : &str) -> String {
  format! ("{:x}", Sha256::digest (text . as_bytes ()))
}

fn census_response (
  complete      : bool,
  write_enabled : bool,
  text_required : &[String],
  stale         : &[String],
  presentation_stale : &[String],
) -> String {
  Sexp::List (vec![
    atom_field ("census-complete", if complete { "true" } else { "nil" }),
    atom_field ("write-enabled", if write_enabled { "true" } else { "nil" }),
    list_field ("text-required-buffer-ids", text_required),
    list_field ("stale-buffer-ids", stale),
    list_field ("presentation-stale-view-uris", presentation_stale),
  ]) . to_string ()
}

fn atom_field (key : &str, value : &str) -> Sexp {
  Sexp::List (vec![
    Sexp::Atom (Atom::S (key . into ())),
    Sexp::Atom (Atom::S (value . into ())),
  ])
}

fn list_field (key : &str, values : &[String]) -> Sexp {
  Sexp::List (vec![
    Sexp::Atom (Atom::S (key . into ())),
    Sexp::List (values . iter () . map (|value|
      Sexp::Atom (Atom::S (value . clone ()))) . collect ()),
  ])
}

fn send_result (stream : &mut TcpStream, result : Result<String, String>) {
  let response = match result {
    Ok (payload) => tag_sexp_response (TcpToClient::ClientCensus, &payload),
    Err (error) => crate::serve::util::tag_terminal_text_response (
      TcpToClient::Error, "failed", &error),
  };
  let _ = send_response_with_length_prefix (stream, &response);
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::types::misc::{SkgfileSource, SourceName};
  use std::collections::HashMap;
  use std::path::PathBuf;

  #[test]
  fn incompatible_protocol_has_no_legacy_authority_path () {
    for request in ["()", "((protocol-version . 1))",
                    "((protocol-version . 3))", "((protocol-version . nope))"] {
      assert! (validate_protocol_version (request) . unwrap_err ()
        . contains ("incompatible Skg protocol")); }
    assert! (validate_protocol_version ("((protocol-version . 2))") . is_ok ());
  }

  #[test]
  fn equal_counters_do_not_restore_an_old_server_session () {
    let descriptor : CensusDescriptor = parse_descriptors (
      &complete_descriptor ("")) . unwrap () . remove (0);
    assert! (descriptor_has_session_authority (&descriptor, "server-test-session"));
    assert! (!descriptor_has_session_authority (&descriptor, "restarted-server"));
    assert_eq! (descriptor . graph_generation, 7);
    assert_eq! (descriptor . application_token, 5);
  }

  #[test]
  fn verification_carries_the_ordered_normalized_source_inventory () {
    let mut sources = HashMap::new ();
    for (name, path, owned) in [
      ("second", "/tmp/second", false),
      ("first", "/tmp/first", true),
    ] {
      sources . insert (SourceName::from (name), SkgfileSource {
        name: SourceName::from (name), abbreviation: None,
        path: PathBuf::from (path), user_owns_it: owned,
      }); }
    let mut config : SkgConfig = SkgConfig::dummyFromSources (sources);
    config . sources . set_order (vec! [
      SourceName::from ("first"), SourceName::from ("second") ]);
    let warnings = vec! [(crate::types::misc::ID::from ("X"),
      TelescopeViolation::IgnoredForeignPidCollision {
        winning_sources: vec![SourceName::from ("first")],
        winning_paths: vec![PathBuf::from ("/tmp/first/X.skg")],
        ignored_sources: vec![SourceName::from ("second")],
        ignored_paths: vec![PathBuf::from ("/tmp/second/X.skg")],
      })];
    let selected = SelectedStoreState::initial (
      crate::dbs::in_rust_graph::InRustGraph::new (),
      crate::types::store_state::SelectedPathManifest::from ([
        (PathBuf::from ("/tmp/first/X.skg"),
         crate::types::store_state::PathDigest::of_bytes (b"pid: X\n")),
      ]));
    let response : String = verify_connection_response (
      &config, &warnings, &selected, "all", false,
      &MaintenanceCoordinator::new (), None, "server-test-session");
    let first : usize = response . find ("(name first)") . unwrap ();
    let second : usize = response . find ("(name second)") . unwrap ();
    assert! (first < second, "{}", response);
    assert! (response . contains ("(position 0)"), "{}", response);
    assert! (response . contains ("(owned true)"), "{}", response);
    assert! (response . contains ("(directory /tmp/first)"), "{}", response);
    assert! (response . contains (
      "(kind ignored-foreign-pid-collision)"), "{}", response);
    assert! (response . contains ("/tmp/second/X.skg"), "{}", response);
    assert! (response . contains ("(graph-generation 1)"), "{}", response);
    assert! (response . contains ("(manifest-revision 1)"), "{}", response);
    assert! (! response . contains ("path-outcomes"), "{}", response);
    assert! (response . contains ("(tantivy-health healthy)"), "{}", response);
  }

  #[test]
  fn verification_exposes_only_a_maintenance_state_label () {
    let config = SkgConfig::dummyFromSources (HashMap::new ());
    let selected = SelectedStoreState::initial (
      crate::dbs::in_rust_graph::InRustGraph::new (),
      crate::types::store_state::SelectedPathManifest::default ());
    let mut maintenance = MaintenanceCoordinator::new ();
    maintenance . state = CoordinatorState::BlockedStoreHealth {
      reason: "SECRET-MAINTENANCE-PAYLOAD" . into (),
    };
    let response = verify_connection_response (
      &config, &[], &selected, "all", true, &maintenance, None, "server-test-session");
    assert! (response . contains (
      "(maintenance-state blocked-store-health)"), "{}", response);
    assert! (!response . contains ("SECRET-MAINTENANCE-PAYLOAD"),
      "{}", response);
  }

  #[test]
  fn verification_names_safely_abandoned_prearchive_work () {
    let config = SkgConfig::dummyFromSources (HashMap::new ());
    let selected = SelectedStoreState::initial (
      crate::dbs::in_rust_graph::InRustGraph::new (),
      crate::types::store_state::SelectedPathManifest::default ());
    let incident = crate::maintenance::IncidentId::new ();
    let abandoned = (incident . clone (), "explicit-partial-reload" . into ());
    let response = verify_connection_response (
      &config, &[], &selected, "all", true,
      &MaintenanceCoordinator::new (), Some (&abandoned), "server-test-session");
    assert! (response . contains (&format! (
      "(abandoned-prearchive-incident {})", incident)), "{}", response);
    assert! (response . contains (
      "(abandoned-prearchive-origin explicit-partial-reload)"), "{}", response);
  }

  fn complete_descriptor (overrides : &str) -> String {
    format! (concat! (
      "(((buffer-id . \"buffer-1\") (kind . \"search-view\") ",
      "(lifecycle . \"live-view\") (disposable . \"nil\") ",
      "(continuation-id . \"continuation-1\") ",
      "(origin-buffer-id . \"nil\") (origin-view-uri . \"nil\") ",
      "(origin-application-token . \"nil\") (origin-location . \"nil\") ",
      "(view-uri . \"search:dog\") ",
      "(recipe . \"((body \\\"nil\\\") (kind \\\"search\\\") ",
      "(operators \\\"true\\\") (regex \\\"true\\\") ",
      "(terms \\\"dog\\\"))\") ",
      "(root-ids (\"z\" \"a\" \"z\")) (source-set . \"private\") ",
      "(server-session-id . \"server-test-session\") ",
      "(view-write-authority . \"editable\") ",
      "(graph-generation . 7) (presentation-generation . 3) ",
      "(server-revision . 11) (application-token . 5) ",
      "(dirty . \"true\") (logical-dirty . \"true\") ",
      "(undo-required . \"true\") (maintenance-epoch . 9) ",
      "(modification-tick . 17) (presentation-stale . \"true\") ",
      "(search-stale . \"true\") (herald-bearing . \"true\") ",
      "(last-fetched-sha256 . \"{}\") (current-sha256 . \"{}\") {}))"),
      "a" . repeat (64), "B" . repeat (64), overrides)
  }

  #[test]
  fn parses_the_complete_normalized_descriptor () {
    let parsed = parse_descriptors (&complete_descriptor ("")).unwrap ();
    let descriptor = &parsed [0];
    assert_eq! (descriptor . lifecycle, "live-view");
    assert_eq! (descriptor . continuation_id . as_deref (), Some ("continuation-1"));
    assert_eq! (descriptor . recipe,
      "((body nil) (kind search) (operators true) (regex true) (terms dog))");
    assert_eq! (descriptor . root_ids, ["a", "z"]);
    assert_eq! (descriptor . source_set, "private");
    assert_eq! (descriptor . maintenance_epoch, Some (9));
    assert! (descriptor . dirty && descriptor . logical_dirty);
    assert! (descriptor . presentation_stale && descriptor . search_stale);
    assert! (descriptor . herald_bearing);
    assert_eq! (descriptor . current_sha256, "b" . repeat (64));
  }

  #[test]
  fn rejects_non_list_recipes_and_non_boolean_flags () {
    let malformed_recipe = complete_descriptor ("")
      . replace (
        "((body \\\"nil\\\") (kind \\\"search\\\") (operators \\\"true\\\") (regex \\\"true\\\") (terms \\\"dog\\\"))",
        "not-a-list");
    assert! (parse_descriptors (&malformed_recipe) . is_err ());
    let malformed_flag = complete_descriptor ("")
      . replace ("(logical-dirty . \"true\")", "(logical-dirty . \"maybe\")");
    assert! (parse_descriptors (&malformed_flag) . is_err ());
  }

  #[test]
  fn retained_authority_includes_recipe_roots_source_set_and_staleness () {
    let descriptor = parse_descriptors (&complete_descriptor (""))
      . unwrap () . remove (0);
    let kind = validate_live_descriptor (&descriptor) . unwrap ();
    let state = ViewState {
      incarnation: uuid::Uuid::new_v4 (),
      save_base: None,
      viewforest: crate::types::tree::forest::ViewForest::new (),
      pids: Default::default (),
      root_ids: ["a", "z"] . into_iter ()
        . map (crate::types::misc::ID::from) . collect (),
      revision: 11,
      graph_generation: 7,
      presentation_generation: 3,
      client_application_token: 5,
      client_buffer_id: None,
      writes_admitted: true,
      kind: BufferKind::SearchView,
      recipe: Some (descriptor . recipe . clone ()),
      source_set: "private" . into (),
      presentation_stale: true,
      search_stale: true,
    };
    assert! (state_matches_descriptor (&state, &descriptor, &kind));
    let mut missed_status = descriptor . clone ();
    missed_status . presentation_stale = false;
    assert! (server_requires_presentation_stale (
      &state, &missed_status, &kind));
    let mut changed = state;
    changed . source_set = "all" . into ();
    assert! (!state_matches_descriptor (&changed, &descriptor, &kind));
    assert! (!server_requires_presentation_stale (
      &changed, &missed_status, &kind));
  }

  #[test]
  fn content_recipe_root_must_appear_in_the_descriptor_roots () {
    let mut descriptor = parse_descriptors (&complete_descriptor (""))
      . unwrap () . remove (0);
    descriptor . kind = "content-view" . into ();
    descriptor . view_uri = Some (ViewUri::ContentView ("view" . into ()));
    descriptor . recipe = crate::types::views_state::single_root_recipe (
      &crate::types::misc::ID::from ("requested"));
    assert! (validate_live_descriptor (&descriptor) . is_err ());
    descriptor . root_ids . push ("requested" . into ());
    assert_eq! (
      validate_live_descriptor (&descriptor) . unwrap (),
      BufferKind::ContentView);
  }

  #[test]
  fn never_saved_new_empty_authority_does_not_require_a_server_forest () {
    let mut descriptor = parse_descriptors (&complete_descriptor (""))
      . unwrap () . remove (0);
    descriptor . kind = "new-empty-content-view" . into ();
    descriptor . view_uri = Some (ViewUri::ContentView ("new-view" . into ()));
    descriptor . recipe = "((kind new-empty))" . into ();
    descriptor . graph_generation = 7;
    descriptor . server_revision = 0;
    descriptor . application_token = 1;
    // Unsaved edits are expected and remain solely client-side until save.
    descriptor . dirty = true;
    descriptor . current_sha256 = "c" . repeat (64);
    let kind = validate_live_descriptor (&descriptor) . unwrap ();
    assert! (unmaterialized_new_empty_authority (
      &descriptor, &kind, 7));

    let mut advanced = descriptor . clone ();
    advanced . application_token = 2;
    assert! (!unmaterialized_new_empty_authority (&advanced, &kind, 7));
    let mut materialized = descriptor . clone ();
    materialized . server_revision = 1;
    assert! (!unmaterialized_new_empty_authority (&materialized, &kind, 7));
    assert! (!unmaterialized_new_empty_authority (&descriptor, &kind, 8));
  }

  #[test]
  fn exact_staged_application_is_a_census_ack_but_changed_text_is_not () {
    use crate::maintenance::{
      FrozenBufferRecord,
      MaintenanceCoordinator,
      MaintenanceOrigin,
      MaintenancePhase,
      MaintenanceTargets,
      ViewApplicationRecord,
      ViewDisposition,
      ViewSettlementRecord,
      ViewSettlementResolution,
    };
    use crate::types::store_state::{GraphGeneration, ManifestRevision};

    let content = String::new ();
    let content_sha = sha256 (&content);
    let frozen = FrozenBufferRecord {
      buffer_id: "buffer" . into (), kind: BufferKind::ContentView,
      lifecycle: "live-view" . into (), disposable: false,
      continuation_id: None, origin_buffer_id: None, origin_view_uri: None,
      origin_application_token: None, origin_location: None,
      view_uri: Some ("view" . into ()), recipe: "()" . into (),
      root_ids: Vec::new (), source_set: "all" . into (),
      graph_generation: 1, presentation_generation: 3, server_revision: 4,
      application_token: 7, dirty: false, logical_dirty: false,
      undo_required: false, maintenance_epoch: Some (9),
      presentation_stale: false, search_stale: false, herald_bearing: false,
      last_fetched_sha256: "a" . repeat (64),
      current_sha256: "a" . repeat (64),
    };
    let mut coordinator = MaintenanceCoordinator::new ();
    let active = coordinator . begin_with_archive_contract_and_targets (
      MaintenanceOrigin::ExplicitPartialReload, None, "session" . into (),
      "emacs" . into (), "all" . into (), GraphGeneration::INITIAL,
      ManifestRevision::INITIAL, vec![frozen], MaintenanceTargets {
        ids: vec!["node" . into ()], ..MaintenanceTargets::default ()
      }) . unwrap ();
    let CoordinatorState::Active (state) = &mut coordinator . state else {
      unreachable! () };
    state . phase = MaintenancePhase::Presenting;
    state . view_settlements . insert ("buffer" . into (),
      ViewSettlementRecord {
        buffer_id: "buffer" . into (), buffer_key: None,
        kind: BufferKind::ContentView, view_uri: Some ("view" . into ()),
        origin_buffer_id: None, origin_view_uri: None,
        origin_application_token: None, origin_location: None,
        dirty: false, impacted: true, parse_uncertain: false,
        uncertainty_reason: None, observed_ids: Vec::new (),
        resolved_primary_ids: Vec::new (), base_graph_generation: 1,
        base_presentation_generation: 3, base_server_revision: 4,
        base_application_token: 7,
        planned_disposition: ViewDisposition::Refreshed,
        requirement: ViewSettlementRequirement::ApplicationAck,
        application: Some (ViewApplicationRecord {
          content, content_sha256: content_sha . clone (),
          resulting_graph_generation: 2,
          resulting_presentation_generation: 8,
          resulting_server_revision: 5,
          resulting_application_token: 8,
          warnings: Vec::new (),
        }),
        resolution: ViewSettlementResolution::Pending,
        acknowledged: false,
      });
    let descriptor = CensusDescriptor {
      server_session_id: "server-test-session" . into (),
      writes_admitted: true,
      buffer_id: "buffer" . into (), kind: "content-view" . into (),
      lifecycle: "live-view" . into (), disposable: false,
      continuation_id: None, origin_buffer_id: None, origin_view_uri: None,
      origin_application_token: None, origin_location: None,
      view_uri: Some (ViewUri::ContentView ("view" . into ())),
      recipe: "()" . into (), root_ids: Vec::new (), source_set: "all" . into (),
      graph_generation: 2, presentation_generation: 8, server_revision: 5,
      application_token: 8, dirty: false, logical_dirty: false,
      undo_required: false, maintenance_epoch: Some (active . epoch . get ()),
      modification_tick: 1, presentation_stale: false, search_stale: false,
      herald_bearing: false, last_fetched_sha256: content_sha . clone (),
      current_sha256: content_sha,
    };
    assert! (census_application_ack (
      &crate::dbs::in_rust_graph::InRustGraph::new (), &coordinator, &descriptor) . unwrap () . is_some ());
    let CoordinatorState::Active (retained) = &coordinator . state else { unreachable! (); };
    let retained = retained . clone ();
    coordinator . state = CoordinatorState::Idle;
    coordinator . committed_incidents . insert (retained . incident_id . clone (),
      crate::maintenance::CommittedIncident::Settling (retained));
    assert! (census_application_ack (
      &crate::dbs::in_rust_graph::InRustGraph::new (), &coordinator, &descriptor)
      . unwrap () . is_some ());
    let mut wrong_epoch = descriptor . clone ();
    wrong_epoch . maintenance_epoch = Some (active . epoch . successor () . get ());
    assert! (census_application_ack (
      &crate::dbs::in_rust_graph::InRustGraph::new (), &coordinator, &wrong_epoch)
      . unwrap () . is_none ());
    let mut changed = descriptor;
    changed . current_sha256 = "f" . repeat (64);
    assert! (census_application_ack (
      &crate::dbs::in_rust_graph::InRustGraph::new (), &coordinator, &changed) . unwrap () . is_none ());
  }
}
