//! Pure wire parsing and status formatting for durable query waits.
//!
//! This adapter validates the complete request before the owner is asked to
//! journal it. It does not select a graph, execute a search, or write a view.

use crate::maintenance::candidate::source_catalog_blake3;
use crate::maintenance::query_waits::{QueryWaitRecord, QueryWaitRecipe,
  QueryWaitState, QueryWaitTarget};
use crate::runtime::query_waits::encode_config;
use crate::source_sets::ActiveSourceSet;
use crate::types::misc::SkgConfig;
use crate::maintenance::types::{CandidateId, IncidentId, MaintenanceEpoch};

use serde_yaml;
use sha2::{Digest, Sha256};
use sexp::{Atom, Sexp};
use std::collections::BTreeMap;
use uuid::Uuid;

pub fn parse_registration (
  request : &str,
  config  : &SkgConfig,
  active  : &ActiveSourceSet,
) -> Result<QueryWaitRecord, String> {
  let fields : BTreeMap<String, Sexp> = parse_fields (request)?;
  let operation_id : String = string_field (&fields, "query-operation-id")?;
  Uuid::parse_str (&operation_id)
    . map_err (|_| "query-operation-id is not a UUID" . to_string ())?;
  if string_field (&fields, "outcome-kind")? != "graph-publication" {
    return Err ("query wait requires graph-publication outcome-kind" . into ()); }

  let incident : Option<IncidentId> = optional_string_field (
    &fields, "incident-id")? . map (|value| IncidentId::parse (&value))
    . transpose ()?;
  let candidate : Option<CandidateId> = optional_string_field (
    &fields, "candidate-id")? . map (|value| CandidateId::parse (&value))
    . transpose ()?;
  let target : QueryWaitTarget = match (incident, candidate) {
    (Some (incident_id), None) => QueryWaitTarget::Incident {
      incident_id, epoch: MaintenanceEpoch::parse (
        &string_field (&fields, "maintenance-epoch")?)?, },
    (None, Some (candidate_id)) => QueryWaitTarget::Candidate { candidate_id },
    (Some (_), Some (_)) =>
      return Err ("query wait target must be incident+epoch or candidate-id" . into ()),
    (None, None) =>
      return Err ("query wait target has no incident+epoch or candidate-id" . into ()),
  };
  if matches! (&target, QueryWaitTarget::Candidate { .. })
  && fields . contains_key ("maintenance-epoch") {
    return Err ("candidate query wait must not include maintenance-epoch" . into ()); }

  let raw_recipe : String = string_field (&fields, "query-recipe")?;
  let digest : String = string_field (&fields, "query-recipe-digest")?;
  validate_digest (&digest, "query-recipe-digest")?;
  if sha256_hex (raw_recipe . as_bytes ()) != digest {
    return Err ("query-recipe-digest does not match query-recipe" . into ()); }
  let recipe : QueryWaitRecipe = parse_recipe (
    &raw_recipe, config, active)?;
  if string_field (&fields, "source-set")? != active . name . 0 {
    return Err ("query destination source-set differs from the active source-set" . into ()); }

  let view_uri : String = string_field (&fields, "view-uri")?;
  if view_uri != format! ("search:wait:{}", operation_id) {
    return Err ("query wait view-uri does not match query-operation-id" . into ()); }
  let client_buffer_id : String = string_field (&fields, "client-buffer-id")?;
  Uuid::parse_str (&client_buffer_id)
    . map_err (|_| "client-buffer-id is not a UUID" . to_string ())?;
  let base_application_token : u64 = unsigned_field (
    &fields, "client-application-token")?;
  let base_graph_generation : u64 = unsigned_field (
    &fields, "graph-generation")?;
  let base_presentation_generation : u64 = unsigned_field (
    &fields, "presentation-generation")?;
  let base_server_revision : u64 = unsigned_field (
    &fields, "server-revision")?;
  let base_content_sha256 : String = string_field (
    &fields, "base-content-sha256")?;
  validate_digest (&base_content_sha256, "base-content-sha256")?;
  let destination_state : String = string_field (&fields, "destination-state")?;
  if !matches! (destination_state . as_str (), "clean" | "placeholder") {
    return Err ("destination-state must be clean or placeholder" . into ()); }

  let mut record : QueryWaitRecord = QueryWaitRecord::new (
    operation_id, recipe,
    crate::maintenance::query_waits::QueryWaitDestination {
      view_uri, client_buffer_id: Some (client_buffer_id),
      base_graph_generation, base_presentation_generation,
      base_server_revision, base_application_token, base_content_sha256,
    }, target)?;
  record . client_recipe = Some (raw_recipe);
  record . validate ()?;
  Ok (record)
}

fn parse_recipe (
  raw    : &str,
  config : &SkgConfig,
  active : &ActiveSourceSet,
) -> Result<QueryWaitRecipe, String> {
  let fields : BTreeMap<String, Sexp> = parse_fields (raw)?;
  if fields . keys () . any (|key| !matches! (key . as_str (),
    "kind" | "terms" | "regex" | "body" | "operators" | "ugly-telescopes" | "source-set")) {
    return Err ("query recipe contains an unsupported field" . into ()); }
  let kind : String = string_field (&fields, "kind")?;
  if kind != "text-search" {
    return Err ("query wait recipe kind must be text-search" . into ()); }
  let terms : String = string_field (&fields, "terms")?;
  let regex : bool = bool_field (&fields, "regex")?;
  let body : bool = bool_field (&fields, "body")?;
  let operators : bool = bool_field (&fields, "operators")?;
  let ugly_choice : Option<String> = match string_field (
    &fields, "ugly-telescopes")? . as_str () {
    "default" => None,
    "include" | "exclude" => Some (string_field (&fields, "ugly-telescopes")?),
    _ => return Err ("ugly-telescopes must be default, include, or exclude" . into ()),
  };
  let source_set : String = string_field (&fields, "source-set")?;
  if source_set != active . name . 0 {
    return Err ("query wait recipe source-set differs from the active source-set" . into ()); }
  let config_snapshot : String = encode_config (config)?;
  let source_catalog_snapshot : String = serde_yaml::to_string (&config . sources)
    . map_err (|error| error . to_string ())?;
  // This is the selected snapshot identity; registration never reads a live
  // config file to manufacture a historical wait interpretation.
  let config_file_blake3 : String = blake3::hash (
    config_snapshot . as_bytes ()) . to_hex () . to_string ();
  let source_catalog_blake3 : String = source_catalog_blake3 (config);
  Ok (QueryWaitRecipe { terms, regex, body, operators, ugly_choice,
    source_set, config_snapshot, source_catalog_snapshot,
    config_file_blake3, source_catalog_blake3 })
}

pub fn status_fields (
  record         : &QueryWaitRecord,
  include_recipe : bool,
) -> Vec<Sexp> {
  let mut fields : Vec<Sexp> = vec![
    atom_field ("response-type", "query-wait-status"),
    atom_field ("query-operation-id", &record . operation_id),
    atom_field ("status", record . state . label()),
    atom_field ("outcome-kind", "graph-publication"),
  ];
  match &record . target {
    QueryWaitTarget::Incident { incident_id, epoch } => {
      fields . push (atom_field ("incident-id", incident_id . as_str ()));
      fields . push (integer_field ("maintenance-epoch", epoch . get ())); }
    QueryWaitTarget::Candidate { candidate_id } =>
      fields . push (atom_field ("candidate-id", candidate_id . as_str ())),
  }
  if let Some (publication) = &record . resolved_target {
    fields . push (atom_field ("outcome-operation-id", &publication . operation_id)); }
  if let Some (result) = &record . result {
    fields . push (atom_field ("result-digest", &result . content_sha256)); }
  match &record . state {
    QueryWaitState::Blocked { reason }
    | QueryWaitState::Cancelled { reason }
    | QueryWaitState::TargetSuperseded { reason }
    | QueryWaitState::Failed { reason } =>
      fields . push (atom_field ("reason", reason)),
    QueryWaitState::Superseded { successor_operation_id } => {
      fields . push (atom_field ("successor-operation-id", successor_operation_id)); }
    _ => {}
  }
  if include_recipe {
    if let Some (raw_recipe) = &record . client_recipe {
      fields . push (atom_field ("query-recipe", raw_recipe));
      fields . push (atom_field ("query-recipe-digest", &sha256_hex (
        raw_recipe . as_bytes ()))); }}
  fields
}

fn parse_fields (
  text : &str,
) -> Result<BTreeMap<String, Sexp>, String> {
  let parsed : Sexp = sexp::parse (text)
    . map_err (|error| format! ("malformed query wait S-expression: {}", error))?;
  let Sexp::List (items) : Sexp = parsed else {
    return Err ("query wait request must be a list" . into ()); };
  let mut fields : BTreeMap<String, Sexp> = BTreeMap::new ();
  for item in items {
    let Sexp::List (pair) : Sexp = item else {
      return Err ("query wait fields must be dotted pairs" . into ()); };
    if pair . len () != 3 || pair[1] != atom (".") {
      return Err ("query wait fields must be dotted pairs" . into ()); }
    let Sexp::Atom (Atom::S (key)) : &Sexp = &pair[0] else {
      return Err ("query wait field key is not an atom" . into ()); };
    if fields . insert (key . clone (), pair[2] . clone ()) . is_some () {
      return Err (format! ("duplicate query wait field '{}'", key)); }
  }
  Ok (fields)
}

fn string_field (
  fields : &BTreeMap<String, Sexp>,
  key : &str,
) -> Result<String, String> {
  let value : &Sexp = fields . get (key)
    . ok_or_else (|| format! ("query wait request lacks '{}'", key))?;
  atom_text (value) . ok_or_else (|| format! ("query wait field '{}' is not a string", key))
}

fn optional_string_field (
  fields : &BTreeMap<String, Sexp>,
  key    : &str,
) -> Result<Option<String>, String> {
  fields . get (key) . map (|value| atom_text (value)
    . ok_or_else (|| format! ("query wait field '{}' is not a string", key)))
    . transpose ()
}

fn bool_field (
  fields : &BTreeMap<String, Sexp>,
  key : &str,
) -> Result<bool, String> {
  match string_field (fields, key)? . as_str () {
    "true" => Ok (true), "false" => Ok (false),
    _ => Err (format! ("query wait field '{}' must be true or false", key)),
  }
}

fn unsigned_field (
  fields : &BTreeMap<String, Sexp>,
  key : &str,
) -> Result<u64, String> {
  let value : u64 = string_field (fields, key)? . parse::<u64> ()
    . map_err (|_| format! ("query wait field '{}' is not unsigned", key))
    ?;
  if value >= i64::MAX as u64 {
    return Err (format! ("query wait field '{}' exceeds the supported wire range", key)); }
  Ok (value)
}

fn atom_text (
  value : &Sexp,
) -> Option<String> {
  match value {
    Sexp::Atom (Atom::S (value)) => Some (value . clone ()),
    Sexp::Atom (Atom::I (value)) => Some (value . to_string ()),
    _ => None,
  }
}

fn validate_digest (
  digest : &str,
  key : &str,
) -> Result<(), String> {
  if digest . len () != 64 || !digest . bytes () . all (|byte| byte . is_ascii_hexdigit ()) {
    return Err (format! ("{} is not a SHA-256 digest", key)); }
  Ok (( ))
}

fn sha256_hex (
  bytes : &[u8],
) -> String {
  format! ("{:x}", Sha256::digest (bytes))
}

fn atom (
  value : &str,
) -> Sexp {
  Sexp::Atom (Atom::S (value . into ()))
}

fn atom_field (
  key : &str,
  value : &str,
) -> Sexp {
  Sexp::List (vec![atom (key), atom (value)])
}

fn integer_field (
  key : &str,
  value : u64,
) -> Sexp {
  Sexp::List (vec![atom (key), Sexp::Atom (Atom::I (value as i64))])
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::types::misc::{SkgConfig, SourceSetName};

  fn context () -> (SkgConfig, ActiveSourceSet) {
    let config : SkgConfig = SkgConfig::dummyFromSources (Default::default ());
    let active : ActiveSourceSet = ActiveSourceSet::named (
      &config, SourceSetName::from ("all")) . unwrap ();
    (config, active)
  }

  fn recipe () -> String {
    "((kind . \"text-search\") (terms . \"needle\") (regex . \"false\") (body . \"true\") (operators . \"false\") (ugly-telescopes . \"default\") (source-set . \"all\"))" . into ()
  }

  fn request (recipe_text : &str) -> String {
    let operation_id : String = "11111111-1111-4111-8111-111111111111" . into ();
    format! ("((query-operation-id . \"{}\") (incident-id . \"22222222-2222-4222-8222-222222222222\") (maintenance-epoch . 4) (outcome-kind . \"graph-publication\") (query-recipe . \"{}\") (query-recipe-digest . \"{}\") (view-uri . \"search:wait:{}\") (client-buffer-id . \"33333333-3333-4333-8333-333333333333\") (client-application-token . 7) (graph-generation . 8) (presentation-generation . 9) (server-revision . 10) (base-content-sha256 . \"{}\") (destination-state . \"clean\") (source-set . \"all\"))",
      operation_id, recipe_text . replace ('\\', "\\\\") . replace ('"', "\\\""),
      sha256_hex (recipe_text . as_bytes ()), operation_id, "a" . repeat (64))
  }

  #[test]
  fn valid_registration_retains_raw_recipe_and_snapshot () {
    let (config, active) : (SkgConfig, ActiveSourceSet) = context ();
    let record : QueryWaitRecord = parse_registration (&request (&recipe ()), &config, &active) . unwrap ();
    assert_eq! (record . client_recipe . as_deref (), Some (recipe () . as_str ()));
    assert_eq! (record . destination . base_application_token, 7);
    assert! (record . recipe . config_snapshot . contains ("format_version"));
  }

  #[test]
  fn altered_digest_wrong_source_and_missing_base_are_refused () {
    let (config, active) : (SkgConfig, ActiveSourceSet) = context ();
    let mut bad_digest : String = request (&recipe ());
    bad_digest = bad_digest . replace (&sha256_hex (recipe () . as_bytes ()), &"b" . repeat (64));
    assert! (parse_registration (&bad_digest, &config, &active) . is_err ());
    let wrong_source : String = request (&recipe () . replace (
      "source-set . \"all\"", "source-set . \"wrong\""));
    assert! (parse_registration (&wrong_source, &config, &active) . is_err ());
    let missing_base : String = request (&recipe ()) . replace (
      " (base-content-sha256 . \"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\")", "");
    assert! (parse_registration (&missing_base, &config, &active) . is_err ());
  }

  #[test]
  fn duplicate_fields_are_rejected_and_status_hides_recipe_by_default () {
    let (config, active) : (SkgConfig, ActiveSourceSet) = context ();
    let record : QueryWaitRecord = parse_registration (&request (&recipe ()), &config, &active) . unwrap ();
    let mut duplicate : String = request (&recipe ());
    duplicate . push_str (" (outcome-kind . \"graph-publication\")");
    assert! (parse_registration (&duplicate, &config, &active) . is_err ());
    let hidden : Vec<Sexp> = status_fields (&record, false);
    assert! (!hidden . iter () . any (|field| field . to_string () . contains ("query-recipe")));
    let shown : Vec<Sexp> = status_fields (&record, true);
    assert! (shown . iter () . any (|field| field . to_string () . contains ("query-recipe")));
  }
}
