//! Two-step client census and retained-view reattachment.

use crate::from_text::buffer_to_viewnodes::uninterpreted::org_to_uninterpreted_viewforest;
use crate::maintenance::archive::{
  InitialArchiveExpectation,
  verify_initial_archive,
};
use crate::maintenance::{
  ArchiveStatus,
  BufferKind,
  CoordinatorState,
  ViewApplicationAcknowledgement,
  ViewSettlementRequirement,
};
use crate::runtime::ServerRuntime;
use crate::runtime::interactive_session::{CensusDescriptor, InteractiveSession};
use crate::serve::protocol::TcpToClient;
use crate::serve::util::{
  read_length_prefixed_content,
  send_response_with_length_prefix,
  tag_sexp_response,
};
use crate::types::env::SkgEnv;
use crate::types::maybe_placed_viewnode::maybePlaced_to_placed_viewforest;
use crate::types::sexp::{atom_to_string, extract_v_from_kv_pair_in_sexp};
use crate::types::views_state::{
  ViewState,
  ViewUri,
  pids_from_viewforest,
  root_ids_from_viewforest,
};

use super::maintenance_protocol::{
  ServerSettlementEffect,
  prepare_server_settlement_effect,
};

use sexp::{Atom, Sexp};
use sha2::{Digest, Sha256};
use std::collections::{BTreeSet, HashSet};
use std::io::BufReader;
use std::net::TcpStream;

pub fn handle_client_census_request (
  reader       : &mut BufReader<TcpStream>,
  stream       : &mut TcpStream,
  env          : &SkgEnv,
  interactive  : &mut InteractiveSession,
  writes_allowed : bool,
  runtime      : &ServerRuntime,
) {
  let result = (|| -> Result<String, String> {
    let payload = read_length_prefixed_content (reader)
      . map_err (|error| format! ("could not read client census: {}", error))?;
    let descriptors = parse_descriptors (&payload)?;
    let live_buffer_ids : BTreeSet<String> = descriptors . iter ()
      . map (|descriptor| descriptor . buffer_id . clone ()) . collect ();
    let current_generation = env . in_rust_graph . load_full ()
      . graph_generation . get ();
    let maintenance = runtime . maintenance . lock ()
      . map_err (|_| "maintenance coordinator poisoned" . to_string ())?
      . clone ();
    let mut live_uris : HashSet<ViewUri> = HashSet::new ();
    let mut text_required : Vec<String> = Vec::new ();
    let mut stale : Vec<String> = Vec::new ();
    let mut presentation_stale : Vec<String> = Vec::new ();
    let mut census_applications = Vec::new ();
    interactive . pending_census_texts . clear ();
    interactive . live_census = descriptors . iter () . map (|descriptor|
      (descriptor . buffer_id . clone (), descriptor . clone ())) . collect ();

    for descriptor in descriptors {
      let Some (uri) = descriptor . view_uri . clone () else { continue; };
      let descriptor_kind = validate_live_descriptor (&descriptor)?;
      let census_application = census_application_ack (
        &maintenance . state, &descriptor)?;
      if !live_uris . insert (uri . clone ()) {
        return Err (format! (
          "client census names view '{}' more than once",
          uri . repr_in_client ())); }
      match interactive . views . open_views . views . get_mut (&uri) {
        Some (state) if state_matches_descriptor (
          state, &descriptor, &descriptor_kind) =>
        {
          state . client_buffer_id = Some (descriptor . buffer_id . clone ());
        }
        Some (_) if census_application . is_some () => {
          census_applications . push ((
            descriptor . clone (), census_application . unwrap ()));
        }
        Some (state) if server_requires_presentation_stale (
            state, &descriptor, &descriptor_kind) =>
        {
          state . client_buffer_id = Some (descriptor . buffer_id . clone ());
          presentation_stale . push (uri . repr_in_client ());
        }
        Some (_) => stale . push (descriptor . buffer_id . clone ()),
        None if descriptor . graph_generation == current_generation => {
          text_required . push (descriptor . buffer_id . clone ());
          interactive . pending_census_texts . insert (
            descriptor . buffer_id . clone (), descriptor);
        }
        None => stale . push (descriptor . buffer_id . clone ()),
      }
    }

    let absent_server_views : Vec<ViewUri> = interactive . views . open_views
      . views . keys () . filter (|uri| !live_uris . contains (*uri))
      . cloned () . collect ();
    for uri in absent_server_views {
      interactive . views . open_views . unregister_view (&uri); }

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
  let coordinator = runtime . maintenance . lock ()
    . map_err (|_| "maintenance coordinator poisoned" . to_string ())?
    . clone ();
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
    coordinator . reconcile_absent_view_settlements (live_buffer_ids)?;
    Ok (( ))
  })?;
  if let Some ((incident, verified)) = verified {
    runtime . retain_verified_archive (incident, verified); }
  Ok (( ))
}

fn census_application_ack (
  state      : &CoordinatorState,
  descriptor : &CensusDescriptor,
) -> Result<Option<ViewApplicationAcknowledgement>, String> {
  let CoordinatorState::Active (active) = state else { return Ok (None); };
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
  let Some (frozen) = active . buffer_census . get (&descriptor . buffer_id)
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
    &offered_forest) . into_iter () . map (|id| id . 0) . collect ();
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
  Ok (Some (ViewApplicationAcknowledgement {
    content_sha256: offered_sha,
    resulting_graph_generation: descriptor . graph_generation,
    resulting_presentation_generation: descriptor . presentation_generation,
    resulting_server_revision: descriptor . server_revision,
    resulting_application_token: descriptor . application_token,
  }))
}

fn reconcile_census_applications (
  runtime      : &ServerRuntime,
  interactive  : &mut InteractiveSession,
  applications : &[(CensusDescriptor, ViewApplicationAcknowledgement)],
) -> Result<(), String> {
  if applications . is_empty () { return Ok (( )); }
  let coordinator = runtime . maintenance . lock ()
    . map_err (|_| "maintenance coordinator poisoned" . to_string ())?
    . clone ();
  let CoordinatorState::Active (active) = &coordinator . state else {
    return Err ("census application lost its active incident" . into ()); };
  let mut effects = Vec::new ();
  for (descriptor, acknowledgement) in applications {
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
      effects . push (ServerSettlementEffect::None);
    } else {
      effects . push (prepare_server_settlement_effect (
        active, record, Some (state), Some (acknowledgement))?); }
  }
  runtime . transition_maintenance (|coordinator| {
    for (descriptor, acknowledgement) in applications {
      coordinator . acknowledge_view_application_from_census (
        &descriptor . buffer_id, acknowledgement)?; }
    Ok (( ))
  })?;
  for effect in effects {
    match effect {
      ServerSettlementEffect::None => {}
      ServerSettlementEffect::Apply {
        uri, base_revision, viewforest, graph_generation,
        presentation_generation, application_token, search_stale,
      } => {
        if !interactive . views . open_views . update_view_if_revision (
            &uri, base_revision, viewforest)
        {
          return Err ("census application base advanced after validation"
            . into ()); }
        interactive . views . open_views . set_client_application_authority (
          &uri, graph_generation, presentation_generation, application_token)?;
        interactive . views . open_views . views . get_mut (&uri)
          . expect ("census-applied view remains registered")
          . search_stale |= search_stale;
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
  env          : &SkgEnv,
  interactive  : &mut InteractiveSession,
  writes_allowed : bool,
  runtime      : &ServerRuntime,
) {
  let result = (|| -> Result<String, String> {
    let payload = read_length_prefixed_content (reader)
      . map_err (|error| format! ("could not read census texts: {}", error))?;
    let records = parse_text_records (&payload)?;
    let mut restored : Vec<String> = Vec::new ();
    let mut stale : Vec<String> = Vec::new ();
    let mut restored_descriptors = Vec::new ();
    for (buffer_id, last_fetched, current) in records {
      let Some (descriptor) = interactive . pending_census_texts
        . remove (&buffer_id)
      else {
        return Err (format! (
          "census texts include unrequested buffer '{}'", buffer_id)); };
      if descriptor . graph_generation != env . in_rust_graph . load_full ()
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
        uri . clone (), viewforest, &pids,
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
    let maintenance = runtime . maintenance . lock ()
      . map_err (|_| "maintenance coordinator poisoned" . to_string ())?
      . clone ();
    let mut census_applications = Vec::new ();
    for descriptor in &restored_descriptors {
      if let Some (ack) = census_application_ack (
          &maintenance . state, descriptor)?
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
  state . graph_generation == descriptor . graph_generation
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
      viewforest: crate::types::tree::forest::ViewForest::new (),
      pids: Default::default (),
      root_ids: ["a", "z"] . into_iter ()
        . map (crate::types::misc::ID::from) . collect (),
      revision: 11,
      graph_generation: 7,
      presentation_generation: 3,
      client_application_token: 5,
      client_buffer_id: None,
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
      &coordinator . state, &descriptor) . unwrap () . is_some ());
    let mut changed = descriptor;
    changed . current_sha256 = "f" . repeat (64);
    assert! (census_application_ack (
      &coordinator . state, &changed) . unwrap () . is_none ());
  }
}
