//! Durable recipes and outcomes for searches waiting on a maintenance result.
//!
//! This module contains no graph, index, socket, or callback state.  The
//! coordinator owns the ledger and journals it with the other maintenance
//! obligations; query execution is an owner-controlled later step.

use super::types::{CandidateId, IncidentId, MaintenanceEpoch};
use crate::types::misc::ID;

use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet};
use std::path::PathBuf;
use uuid::Uuid;

pub const QUERY_WAIT_FORMAT_VERSION : u32 = 1;

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct QueryWaitRecipe {
  pub terms       : String,
  pub regex       : bool,
  pub body        : bool,
  pub operators   : bool,
  pub ugly_choice : Option<String>,
  pub source_set  : String,
  /// The accepted interpretation travels with the wait. These serialized
  /// values prevent a restart from silently adopting current configuration.
  pub config_snapshot         : String,
  pub source_catalog_snapshot : String,
  pub config_file_blake3      : String,
  pub source_catalog_blake3   : String,
}

impl QueryWaitRecipe {
  pub fn validate (&self) -> Result<(), String> {
    if self . source_set . is_empty () {
      return Err ("query wait recipe has no source-set" . into ()); }
    if self . config_snapshot . is_empty ()
    || self . source_catalog_snapshot . is_empty () {
      return Err ("query wait recipe has no config/source interpretation" . into ()); }
    for (label, digest) in [
      ("config", &self . config_file_blake3),
      ("source catalog", &self . source_catalog_blake3),
    ] {
      if digest . len () != 64
      || !digest . bytes () . all (|byte| byte . is_ascii_hexdigit ()) {
        return Err (format! ("query wait recipe has an invalid {} checksum", label)); }}
    if let Some (choice) = &self . ugly_choice {
      if !matches! (choice . as_str (), "include" | "exclude") {
        return Err (format! (
          "query wait recipe has unsupported ugly telescope choice '{}'",
          choice)); }}
    Ok (( )) }
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct QueryWaitDestination {
  pub view_uri                   : String,
  pub client_buffer_id           : Option<String>,
  pub base_graph_generation     : u64,
  pub base_presentation_generation : u64,
  pub base_server_revision      : u64,
  pub base_application_token    : u64,
  pub base_content_sha256       : String,
}

impl QueryWaitDestination {
  pub fn validate (&self) -> Result<(), String> {
    if self . view_uri . is_empty () {
      return Err ("query wait destination has no view URI" . into ()); }
    if self . client_buffer_id . as_deref () == Some ("") {
      return Err ("query wait destination has an empty client buffer ID" . into ()); }
    if self . base_content_sha256 . len () != 64
    || !self . base_content_sha256 . bytes () . all (|byte| byte . is_ascii_hexdigit ()) {
      return Err ("query wait destination has an invalid base content checksum" . into ()); }
    Ok (( ))
  }
}

/// One exact maintenance outcome is the wait's target.  The candidate is
/// optional because no-change and pre-candidate outcomes are identified by
/// incident and epoch alone.  `outcome_operation_id` is the durable identity
/// of the owner operation which must complete; graph growth is never used as
/// the completion test.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "kebab-case", tag = "kind", content = "identity")]
pub enum QueryWaitTarget {
  Incident {
    incident_id : IncidentId,
    epoch       : MaintenanceEpoch,
  },
  Candidate {
    candidate_id : CandidateId,
  },
}

impl QueryWaitTarget {
  pub fn validate (&self) -> Result<(), String> {
    match self {
      Self::Incident { incident_id, .. } => {
        IncidentId::parse (incident_id . as_str ())?; }
      Self::Candidate { candidate_id } => {
        CandidateId::parse (candidate_id . as_str ())?; }
    }
    Ok (( ))
  }

  fn matches (&self, outcome : &QueryWaitTargetOutcome) -> bool {
    match self {
      Self::Incident { incident_id, epoch } =>
        incident_id == &outcome . incident_id && *epoch == outcome . epoch,
      Self::Candidate { candidate_id } =>
        outcome . candidate_id . as_ref () == Some (candidate_id),
    }
  }
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct QueryWaitTargetOutcome {
  pub incident_id  : IncidentId,
  pub epoch        : MaintenanceEpoch,
  pub candidate_id : Option<CandidateId>,
  pub operation_id : String,
  pub outcome      : QueryWaitOutcomeKind,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "kebab-case")]
pub enum QueryWaitOutcomeKind {
  Published {
    graph_generation  : u64,
    manifest_revision : u64,
    source_set        : String,
    config_snapshot   : String,
    source_catalog_snapshot : String,
    cyclic_root_ids   : BTreeSet<ID>,
  },
  NoChange {
    graph_generation  : u64,
    manifest_revision : u64,
    source_set        : String,
    config_snapshot   : String,
    source_catalog_snapshot : String,
    cyclic_root_ids   : BTreeSet<ID>,
  },
  Cancelled {
    reason       : String,
  },
  Failed {
    reason       : String,
  },
  Blocked {
    reason       : String,
  },
}

impl QueryWaitTargetOutcome {
  pub fn validate (&self) -> Result<(), String> {
    if self . operation_id . is_empty () {
      return Err ("query wait outcome has no operation ID" . into ()); }
    let canonical : String = canonical_incident_operation_id (
      &self . incident_id, self . epoch);
    IncidentId::parse (self . incident_id . as_str ())?;
    if let Some (candidate_id) = &self . candidate_id {
      CandidateId::parse (candidate_id . as_str ())?; }
    if self . operation_id != canonical {
      return Err ("query wait outcome operation ID is not its incident identity" . into ()); }
    if matches! (&self . outcome,
      QueryWaitOutcomeKind::Published { source_set, .. }
      | QueryWaitOutcomeKind::NoChange { source_set, .. }
        if source_set . is_empty ()) {
      return Err ("query wait publication outcome has no source-set" . into ()); }
    if matches! (&self . outcome,
      QueryWaitOutcomeKind::Cancelled { reason }
      | QueryWaitOutcomeKind::Failed { reason }
      | QueryWaitOutcomeKind::Blocked { reason }
        if reason . is_empty ()) {
      return Err ("query wait non-publication outcome has no reason" . into ()); }
    if let Some (publication) = self . publication () {
      publication . validate ()?; }
    Ok (( ))
  }

  pub fn operation_id (&self) -> &str { &self . operation_id }

  fn publication (&self) -> Option<QueryWaitPublication> {
    match &self . outcome {
      QueryWaitOutcomeKind::Published { graph_generation,
        manifest_revision, source_set, config_snapshot,
        source_catalog_snapshot, cyclic_root_ids }
      | QueryWaitOutcomeKind::NoChange { graph_generation,
        manifest_revision, source_set, config_snapshot,
        source_catalog_snapshot, cyclic_root_ids } => Some (QueryWaitPublication {
          operation_id: self . operation_id . clone (),
          graph_generation: *graph_generation,
          manifest_revision: *manifest_revision,
          source_set: source_set . clone (),
          config_snapshot: config_snapshot . clone (),
          source_catalog_snapshot: source_catalog_snapshot . clone (),
          cyclic_root_ids: cyclic_root_ids . clone (),
        }),
      _ => None,
    }
  }
}

pub fn canonical_incident_operation_id (
  incident_id : &IncidentId,
  epoch       : MaintenanceEpoch,
) -> String {
  format! ("incident:{}:{}", incident_id . as_str (), epoch . get ())
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct QueryWaitPublication {
  pub operation_id      : String,
  pub graph_generation  : u64,
  pub manifest_revision : u64,
  pub source_set        : String,
  pub config_snapshot   : String,
  pub source_catalog_snapshot : String,
  pub cyclic_root_ids   : BTreeSet<ID>,
}

impl QueryWaitPublication {
  fn validate (&self) -> Result<(), String> {
    if self . operation_id . is_empty () || self . source_set . is_empty ()
    || self . config_snapshot . is_empty ()
    || self . source_catalog_snapshot . is_empty () {
      return Err ("query wait publication is missing identity or source-set" . into ()); }
    Ok (( ))
  }
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "kebab-case")]
pub enum QueryWaitFreshness {
  Current,
  Stale,
  PendingReconciliation,
  StaleSearchMembership,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "kebab-case")]
pub struct QueryWaitResult {
  pub artifact_path     : PathBuf,
  pub artifact_bytes    : u64,
  pub content_sha256    : String,
  pub warnings          : Vec<String>,
  pub graph_generation  : u64,
  pub manifest_revision : u64,
  pub source_set        : String,
  pub freshness         : QueryWaitFreshness,
}

impl QueryWaitResult {
  pub fn new (
    artifact_path     : PathBuf,
    artifact_bytes    : u64,
    content_sha256    : String,
    warnings          : Vec<String>,
    graph_generation  : u64,
    manifest_revision : u64,
    source_set        : String,
    freshness         : QueryWaitFreshness,
  ) -> Self {
    Self { artifact_path, artifact_bytes, content_sha256, warnings, graph_generation,
      manifest_revision, source_set, freshness }
  }

  fn validate (&self) -> Result<(), String> {
    if self . source_set . is_empty () {
      return Err ("query wait result has no source-set" . into ()); }
    if self . artifact_path . as_os_str () . is_empty () {
      return Err ("query wait result has no private artifact path" . into ()); }
    if self . content_sha256 . len () != 64
    || !self . content_sha256 . bytes () . all (|byte| byte . is_ascii_hexdigit ()) {
      return Err ("query wait result has an invalid content checksum" . into ()); }
    Ok (( ))
  }
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "kebab-case", tag = "status", content = "details")]
pub enum QueryWaitState {
  Pending,
  Blocked { reason : String },
  Executing,
  Ready,
  Delivered,
  Cancelled { reason : String },
  Superseded { successor_operation_id : String },
  TargetSuperseded { reason : String },
  Failed { reason : String },
}

impl QueryWaitState {
  pub fn label (&self) -> &'static str {
    match self {
      Self::Pending => "pending",
      Self::Blocked { .. } => "blocked",
      Self::Executing => "executing",
      Self::Ready => "ready",
      Self::Delivered => "delivered",
      Self::Cancelled { .. } => "cancelled",
      Self::Superseded { .. } => "superseded",
      Self::TargetSuperseded { .. } => "superseded",
      Self::Failed { .. } => "failed",
    }
  }

  fn terminal (&self) -> bool {
    matches! (self, Self::Delivered | Self::Cancelled { .. }
      | Self::Superseded { .. } | Self::TargetSuperseded { .. }
      | Self::Failed { .. })
  }
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub struct QueryWaitRecord {
  pub format_version : u32,
  pub operation_id   : String,
  pub recipe         : QueryWaitRecipe,
  pub destination    : QueryWaitDestination,
  pub target         : QueryWaitTarget,
  pub state          : QueryWaitState,
  #[serde(default)]
  pub resolved_target : Option<QueryWaitPublication>,
  #[serde(default)]
  pub result         : Option<QueryWaitResult>,
}

impl QueryWaitRecord {
  pub fn new (
    operation_id : String,
    recipe       : QueryWaitRecipe,
    destination  : QueryWaitDestination,
    target       : QueryWaitTarget,
  ) -> Result<Self, String> {
    let record = Self { format_version: QUERY_WAIT_FORMAT_VERSION,
      operation_id, recipe, destination, target, state: QueryWaitState::Pending,
      resolved_target: None, result: None };
    record . validate ()?;
    Ok (record)
  }

  pub fn validate (&self) -> Result<(), String> {
    if self . format_version != QUERY_WAIT_FORMAT_VERSION {
      return Err (format! ("unsupported query wait format version {}", self . format_version)); }
    Uuid::parse_str (&self . operation_id)
      . map_err (|_| "query wait operation ID is not a UUID" . to_string ())?;
    self . recipe . validate ()?;
    self . destination . validate ()?;
    self . target . validate ()?;
    if let Some (publication) = &self . resolved_target {
      publication . validate ()?; }
    if let Some (result) = &self . result {
      result . validate ()?;
      if self . resolved_target . is_none () {
        return Err ("query wait result has no resolved target" . into ()); }
      if let Some (publication) = &self . resolved_target {
        if result . graph_generation != publication . graph_generation
        || result . manifest_revision != publication . manifest_revision
        || result . source_set != publication . source_set {
          return Err ("query wait result does not match its target publication" . into ()); }} }
    match &self . state {
      QueryWaitState::Blocked { reason }
      | QueryWaitState::Cancelled { reason }
      | QueryWaitState::TargetSuperseded { reason }
      | QueryWaitState::Failed { reason } if reason . is_empty () =>
        return Err ("query wait state has no reason" . into ()),
      QueryWaitState::Superseded { successor_operation_id } => {
        Uuid::parse_str (successor_operation_id)
          . map_err (|_| "query wait successor operation ID is not a UUID" . to_string ())?;
        if successor_operation_id == &self . operation_id {
          return Err ("query wait cannot supersede itself" . into ()); }}
      QueryWaitState::Ready | QueryWaitState::Delivered => {
        if self . result . is_none () {
          return Err ("query wait ready state has no staged result" . into ()); }}
      _ => {}
    }
    if matches! (self . state, QueryWaitState::Executing)
    && self . resolved_target . is_none () {
      return Err ("query wait executing state has no resolved target" . into ()); }
    Ok (( ))
  }

  pub fn resolve_target (
    &mut self,
    outcome : QueryWaitTargetOutcome,
  ) -> Result<bool, String> {
    outcome . validate ()?;
    if !self . target . matches (&outcome) {
      return Err ("query wait outcome does not match its exact target" . into ()); }
    if self . state . terminal () || matches! (self . state, QueryWaitState::Ready) {
      return Ok (false); }
    let publication : Option<QueryWaitPublication> = outcome . publication ();
    match outcome . outcome {
      QueryWaitOutcomeKind::Published { .. }
      | QueryWaitOutcomeKind::NoChange { .. } => {
        let publication = publication . expect ("publication outcome");
        if let Some (existing) = &self . resolved_target {
          if existing != &publication {
            return Err ("query wait target publication changed" . into ()); }
        } else { self . resolved_target = Some (publication); }
        self . state = QueryWaitState::Executing;
      }
      QueryWaitOutcomeKind::Cancelled { reason } => {
        self . state = QueryWaitState::Cancelled { reason }; }
      QueryWaitOutcomeKind::Failed { reason } => {
        self . state = QueryWaitState::Failed { reason }; }
      QueryWaitOutcomeKind::Blocked { reason } => {
        self . state = QueryWaitState::Blocked { reason }; }
    }
    Ok (true)
  }

  pub fn retry (&mut self) -> Result<bool, String> {
    if !matches! (self . state, QueryWaitState::Blocked { .. }) {
      return Ok (false); }
    self . state = if self . resolved_target . is_some () {
      QueryWaitState::Executing
    } else { QueryWaitState::Pending };
    Ok (true)
  }

  pub fn recover_executing_after_restart (&mut self) -> Result<bool, String> {
    if !matches! (self . state, QueryWaitState::Executing) { return Ok (false); }
    self . state = QueryWaitState::Blocked {
      reason: "query execution was interrupted; target publication must be revalidated" . into (),
    };
    Ok (true)
  }

  pub fn mark_result_ready (&mut self, result : QueryWaitResult) -> Result<bool, String> {
    if matches! (self . state, QueryWaitState::Ready | QueryWaitState::Delivered) {
      if self . result . as_ref () == Some (&result) { return Ok (false); }
      return Err ("query wait result changed after staging" . into ()); }
    if !matches! (self . state, QueryWaitState::Executing) {
      return Err (format! (
        "query wait result is invalid during {}", self . state . label())); }
    result . validate ()?;
    let publication = self . resolved_target . as_ref ()
      . ok_or_else (|| "query wait result has no resolved target" . to_string ())?;
    if result . graph_generation != publication . graph_generation
    || result . manifest_revision != publication . manifest_revision
    || result . source_set != publication . source_set {
      return Err ("query wait result does not match its resolved target" . into ()); }
    self . result = Some (result);
    self . state = QueryWaitState::Ready;
    Ok (true)
  }

  pub fn acknowledge (&mut self, content_sha256 : &str) -> Result<bool, String> {
    let Some (result) = &self . result else {
      return Err ("query wait ACK has no staged result" . into ()); };
    if result . content_sha256 != content_sha256 {
      return Err ("query wait ACK checksum does not match staged result" . into ()); }
    if self . state == QueryWaitState::Delivered { return Ok (false); }
    if self . state != QueryWaitState::Ready {
      return Err (format! (
        "query wait ACK is invalid during {}", self . state . label())); }
    self . state = QueryWaitState::Delivered;
    Ok (true)
  }

  pub fn cancel (&mut self, reason : String) -> Result<bool, String> {
    if reason . is_empty () { return Err ("query wait cancellation has no reason" . into ()); }
    if self . state . terminal () { return Ok (false); }
    self . state = QueryWaitState::Cancelled { reason };
    Ok (true)
  }

  pub fn supersede (&mut self, successor_operation_id : String) -> Result<bool, String> {
    Uuid::parse_str (&successor_operation_id)
      . map_err (|_| "query wait successor operation ID is not a UUID" . to_string ())?;
    if successor_operation_id == self . operation_id {
      return Err ("query wait cannot supersede itself" . into ()); }
    if self . state . terminal () { return Ok (false); }
    self . state = QueryWaitState::Superseded { successor_operation_id };
    Ok (true)
  }

  pub fn supersede_target (&mut self, reason : String) -> Result<bool, String> {
    if reason . is_empty () {
      return Err ("query wait target supersession has no reason" . into ()); }
    if self . state . terminal () { return Ok (false); }
    self . state = QueryWaitState::TargetSuperseded { reason };
    Ok (true)
  }

  pub fn fail (&mut self, reason : String) -> Result<bool, String> {
    if reason . is_empty () { return Err ("query wait failure has no reason" . into ()); }
    if self . state . terminal () || self . state == QueryWaitState::Ready {
      return Ok (false); }
    self . state = QueryWaitState::Failed { reason };
    Ok (true)
  }
}

#[derive(Clone, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub struct QueryWaitLedger {
  #[serde(default)]
  pub waits : BTreeMap<String, QueryWaitRecord>,
}

impl QueryWaitLedger {
  pub fn validate (&self) -> Result<(), String> {
    for (operation_id, record) in &self . waits {
      if operation_id != &record . operation_id {
        return Err ("query wait ledger key does not match operation ID" . into ()); }
      record . validate ()?; }
    Ok (( ))
  }

  pub fn register (&mut self, record : QueryWaitRecord) -> Result<bool, String> {
    record . validate ()?;
    let operation_id : String = record . operation_id . clone ();
    if let Some (existing) = self . waits . get (&operation_id) {
      if existing . recipe == record . recipe
      && existing . destination == record . destination
      && existing . target == record . target { return Ok (false); }
      return Err ("query wait operation ID was reused with different contents" . into ()); }
    if record . state != QueryWaitState::Pending
    || record . resolved_target . is_some () || record . result . is_some () {
      return Err ("new query wait must start pending without an outcome" . into ()); }
    self . waits . insert (operation_id, record);
    Ok (true)
  }

  pub fn get (&self, operation_id : &str) -> Option<&QueryWaitRecord> {
    self . waits . get (operation_id)
  }

  pub fn get_mut (&mut self, operation_id : &str) -> Result<&mut QueryWaitRecord, String> {
    self . waits . get_mut (operation_id)
      . ok_or_else (|| "unknown query wait operation ID" . into ())
  }

  pub fn resolve_target (
    &mut self,
    operation_id : &str,
    outcome      : QueryWaitTargetOutcome,
  ) -> Result<bool, String> {
    self . get_mut (operation_id)? . resolve_target (outcome)
  }

  pub fn acknowledge (
    &mut self,
    operation_id : &str,
    content_sha256 : &str,
  ) -> Result<bool, String> {
    self . get_mut (operation_id)? . acknowledge (content_sha256)
  }

  pub fn retry (&mut self, operation_id : &str) -> Result<bool, String> {
    self . get_mut (operation_id)? . retry ()
  }

  pub fn mark_result_ready (
    &mut self,
    operation_id : &str,
    result : QueryWaitResult,
  ) -> Result<bool, String> {
    self . get_mut (operation_id)? . mark_result_ready (result)
  }

  pub fn cancel (&mut self, operation_id : &str, reason : String) -> Result<bool, String> {
    self . get_mut (operation_id)? . cancel (reason)
  }

  pub fn supersede (
    &mut self,
    operation_id : &str,
    successor_operation_id : String,
  ) -> Result<bool, String> {
    self . get_mut (operation_id)? . supersede (successor_operation_id)
  }

  pub fn supersede_target (
    &mut self,
    operation_id : &str,
    reason : String,
  ) -> Result<bool, String> {
    self . get_mut (operation_id)? . supersede_target (reason)
  }

  pub fn fail (&mut self, operation_id : &str, reason : String) -> Result<bool, String> {
    self . get_mut (operation_id)? . fail (reason)
  }

  pub fn recover_executing_after_restart (&mut self) -> Result<Vec<String>, String> {
    let mut recovered : Vec<String> = Vec::new ();
    for (operation_id, wait) in &mut self . waits {
      if wait . recover_executing_after_restart ()? {
        recovered . push (operation_id . clone ()); }}
    Ok (recovered)
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::maintenance::coordinator::MaintenanceCoordinator;
  use crate::maintenance::journal::MaintenanceJournalStore;

  #[test]
  fn idle_journal_retains_waits_across_restart_and_refuses_compaction () {
    let temp : tempfile::TempDir = tempfile::tempdir () . unwrap ();
    let store : MaintenanceJournalStore = MaintenanceJournalStore::at_root (
      temp . path () . join ("state"), PathBuf::from ("/config"));
    let mut coordinator : MaintenanceCoordinator = MaintenanceCoordinator::new ();
    coordinator . register_query_wait (record ()) . unwrap ();
    store . persist (&coordinator) . unwrap ();
    assert! (store . remove_completed (&coordinator) . is_err ());
    assert_eq! (store . load () . require_authority () . unwrap () . unwrap ()
      . coordinator, coordinator);
  }

  fn record () -> QueryWaitRecord {
    let config_hash : String = "a" . repeat (64);
    let source_hash : String = "b" . repeat (64);
    QueryWaitRecord::new (
      Uuid::new_v4 () . to_string (),
      QueryWaitRecipe { terms: "needle" . into (), regex: false,
        body: true, operators: false, ugly_choice: None,
        source_set: "all" . into (), config_snapshot: "config" . into (),
        source_catalog_snapshot: "sources" . into (),
        config_file_blake3: config_hash, source_catalog_blake3: source_hash },
      QueryWaitDestination { view_uri: "search:wait" . into (),
        client_buffer_id: Some ("buffer-1" . into ()),
        base_graph_generation: 1, base_presentation_generation: 2,
        base_server_revision: 3, base_application_token: 4,
        base_content_sha256: "c" . repeat (64) },
      QueryWaitTarget::Incident { incident_id: IncidentId::new (),
        epoch: MaintenanceEpoch::INITIAL },
    ) . unwrap ()
  }

  fn published (wait : &QueryWaitRecord) -> QueryWaitTargetOutcome {
    let QueryWaitTarget::Incident { incident_id, epoch } = &wait . target
      else { panic! ("test record target") };
    QueryWaitTargetOutcome {
      incident_id: incident_id . clone (), epoch: *epoch,
      candidate_id: None,
      operation_id: canonical_incident_operation_id (incident_id, *epoch),
      outcome: QueryWaitOutcomeKind::Published {
        graph_generation: 4, manifest_revision: 9, source_set: "all" . into (),
        config_snapshot: "target-config" . into (),
        source_catalog_snapshot: "target-sources" . into (),
        cyclic_root_ids: BTreeSet::new (), }, }
  }

  fn no_change (wait : &QueryWaitRecord) -> QueryWaitTargetOutcome {
    let QueryWaitTarget::Incident { incident_id, epoch } = &wait . target
      else { panic! ("test record target") };
    QueryWaitTargetOutcome {
      incident_id: incident_id . clone (), epoch: *epoch,
      candidate_id: None,
      operation_id: canonical_incident_operation_id (incident_id, *epoch),
      outcome: QueryWaitOutcomeKind::NoChange {
        graph_generation: 4, manifest_revision: 10, source_set: "all" . into (),
        config_snapshot: "target-config" . into (),
        source_catalog_snapshot: "target-sources" . into (),
        cyclic_root_ids: BTreeSet::new (), }, }
  }

  #[test]
  fn operation_reuse_requires_identical_record () {
    let first : QueryWaitRecord = record ();
    let mut ledger : QueryWaitLedger = QueryWaitLedger::default ();
    assert! (ledger . register (first . clone ()) . unwrap ());
    assert! (!ledger . register (first . clone ()) . unwrap ());
    ledger . resolve_target (&first . operation_id, no_change (&first)) . unwrap ();
    assert! (!ledger . register (first . clone ()) . unwrap ());
    assert_eq! (ledger . get (&first . operation_id) . unwrap () . state,
                QueryWaitState::Executing);
    let mut changed : QueryWaitRecord = first;
    changed . recipe . terms = "other" . into ();
    assert! (ledger . register (changed) . is_err ());
  }

  #[test]
  fn no_change_resolves_and_exact_result_ack_is_idempotent () {
    let mut wait : QueryWaitRecord = record ();
    let no_change : QueryWaitTargetOutcome = no_change (&wait);
    assert! (wait . resolve_target (no_change) . is_ok ());
    assert_eq! (wait . state, QueryWaitState::Executing);
    let result : QueryWaitResult = QueryWaitResult::new (
      PathBuf::from ("/private/query-result"), 9, "d" . repeat (64),
      vec![], 4, 10, "all" . into (), QueryWaitFreshness::Current);
    let digest : String = result . content_sha256 . clone ();
    assert! (wait . mark_result_ready (result) . unwrap ());
    assert! (wait . acknowledge (&digest) . unwrap ());
    assert! (!wait . acknowledge (&digest) . unwrap ());
    wait . resolved_target = None;
    assert! (wait . validate () . is_err ());
  }

  #[test]
  fn cancelled_superseded_and_failed_are_terminal_without_expiry () {
    let mut cancelled : QueryWaitRecord = record ();
    assert! (cancelled . cancel ("user cancelled" . into ()) . unwrap ());
    assert! (!cancelled . cancel ("again" . into ()) . unwrap ());
    let mut superseded : QueryWaitRecord = record ();
    assert! (superseded . supersede (Uuid::new_v4 () . to_string ()) . unwrap ());
    let mut failed : QueryWaitRecord = record ();
    assert! (failed . fail ("index unavailable" . into ()) . unwrap ());
    assert_eq! (failed . state . label (), "failed");
  }

  #[test]
  fn interrupted_execution_is_recoverable_and_retryable () {
    let mut wait : QueryWaitRecord = record ();
    let outcome : QueryWaitTargetOutcome = published (&wait);
    wait . resolve_target (outcome) . unwrap ();
    assert! (wait . recover_executing_after_restart () . unwrap ());
    assert_eq! (wait . state . label (), "blocked");
    assert! (wait . retry () . unwrap ());
    assert_eq! (wait . state . label (), "executing");
  }

  #[test]
  fn serde_roundtrip_preserves_recipe_target_and_result () {
    let mut wait : QueryWaitRecord = record ();
    let outcome : QueryWaitTargetOutcome = published (&wait);
    wait . resolve_target (outcome) . unwrap ();
    wait . mark_result_ready (QueryWaitResult::new (
      PathBuf::from ("/private/query-result"), 9, "e" . repeat (64),
      vec!["warning" . into ()], 4, 9, "all" . into (),
      QueryWaitFreshness::Stale)) . unwrap ();
    let encoded : String = serde_yaml::to_string (&wait) . unwrap ();
    let decoded : QueryWaitRecord = serde_yaml::from_str (&encoded) . unwrap ();
    assert_eq! (decoded, wait);
    decoded . validate () . unwrap ();
  }

  #[test]
  fn candidate_target_requires_exact_incident_mapping () {
    let candidate_id : CandidateId = CandidateId::new ();
    let mut wait : QueryWaitRecord = record ();
    wait . target = QueryWaitTarget::Candidate { candidate_id: candidate_id . clone () };
    let incident_id : IncidentId = IncidentId::new ();
    let wrong : QueryWaitTargetOutcome = QueryWaitTargetOutcome {
      incident_id: incident_id . clone (), epoch: MaintenanceEpoch::INITIAL,
      candidate_id: Some (CandidateId::new ()),
      operation_id: canonical_incident_operation_id (&incident_id, MaintenanceEpoch::INITIAL),
      outcome: QueryWaitOutcomeKind::Published {
        graph_generation: 4, manifest_revision: 9, source_set: "all" . into (),
        config_snapshot: "target-config" . into (),
        source_catalog_snapshot: "target-sources" . into (),
        cyclic_root_ids: BTreeSet::new (), }, };
    assert! (wait . resolve_target (wrong) . is_err ());
    let matching : QueryWaitTargetOutcome = QueryWaitTargetOutcome {
      incident_id: incident_id . clone (), epoch: MaintenanceEpoch::INITIAL,
      candidate_id: Some (candidate_id),
      operation_id: canonical_incident_operation_id (&incident_id, MaintenanceEpoch::INITIAL),
      outcome: QueryWaitOutcomeKind::NoChange {
        graph_generation: 4, manifest_revision: 9, source_set: "all" . into (),
        config_snapshot: "target-config" . into (),
        source_catalog_snapshot: "target-sources" . into (),
        cyclic_root_ids: BTreeSet::new (), }, };
    assert! (wait . resolve_target (matching) . unwrap ());
  }
}
