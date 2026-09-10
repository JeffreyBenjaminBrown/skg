//! The process owner orders durable coordinator publication and selected-store
//! mutations. Workers prepare immutable results; only this loop can accept
//! their named reservation and publish the resulting graph/Searcher snapshot.

use crate::maintenance::coordinator::MaintenanceCoordinator;
use crate::maintenance::journal::MaintenanceJournalStore;
use crate::maintenance::types::{ActiveMaintenance, CandidateSummary, CommittedIncident,
  CoordinatorState, IncidentId, MaintenanceEpoch, MaintenancePhase};
use crate::runtime::SelectedRuntimeSnapshot;
use crate::types::env::SkgEnv;
use crate::types::store_state::{GraphGeneration, ManifestRevision, SelectedStoreState};

use arc_swap::ArcSwap;
use std::sync::{Arc, Mutex, MutexGuard};
use std::sync::mpsc::{Receiver, Sender, SyncSender, channel, sync_channel};
use std::thread;
use uuid::Uuid;

const OBSOLETE_COORDINATOR_PUBLICATION : &str =
  "coordinator proposal has an obsolete publication base";

/// Opaque authority for one reservation, including when an operation ID is
/// reused. Dropping a token never releases the owner's reservation.
#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct ReservationToken (Uuid);

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum MutationStage {
  Prepared,
  Authorized,
  Published, }

/// Readable progress without the token's authority to complete the operation.
/// The base names the selection a publication must still observe; after a
/// successful publication it advances to that selection for possible recovery.
#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct MutationStatus {
  // Diagnostic observations also fence supervisor claims. A reused operation
  // name or a second recovery supervisor cannot claim an earlier incarnation.
  incarnation : Uuid,
  pub operation_id : String,
  pub graph_generation : GraphGeneration,
  pub manifest_revision : ManifestRevision,
  pub stage : MutationStage,
  pub blocked_reason : Option<String>, }

#[derive(Clone)]
struct Reservation {
  token : ReservationToken,
  kind : MutationKind,
  status : MutationStatus, }

#[derive(Clone)]
enum MutationKind {
  Ordinary,
  Manifest,
  Maintenance {
    incident_id : IncidentId,
    epoch : MaintenanceEpoch,
    phase : MaintenancePhase,
    candidate : Option<CandidateSummary>, }, }

#[derive(Clone)]
struct PublishedCoordinator {
  revision : u64,
  publication_revision : u64,
  coordinator : MaintenanceCoordinator,
  failure : Option<String>,
  selected : Option<Arc<SelectedRuntimeSnapshot>>,
  reservation : Option<Reservation>, }

struct Proposal {
  base_revision : u64,
  coordinator : MaintenanceCoordinator,
  reply : SyncSender<Result<(), String>>, }

struct Reserve {
  operation_id : String,
  graph_generation : GraphGeneration,
  manifest_revision : ManifestRevision,
  reply : SyncSender<Result<ReservationToken, String>>, }

enum MutationAction {
  Authorize,
  Publish (Arc<SelectedRuntimeSnapshot>),
  Finish,
  Block (String),
  Recover (Arc<SelectedRuntimeSnapshot>), }

enum Message {
  SubscribeQueryWaits (SyncSender<()>),
  Propose (Proposal),
  Published (Result<(), String>),
  AwaitPublication (SyncSender<Result<(), String>>),
  Reserve (Reserve),
  ClaimBlocked {
    expected : MutationStatus,
    reply : SyncSender<Result<ReservationToken, String>>, },
  Mutate {
    token : ReservationToken,
    action : MutationAction,
    reply : SyncSender<Result<(), String>>, },
  Stop, }

pub(crate) struct CoordinatorOwner {
  published : Arc<ArcSwap<PublishedCoordinator>>,
  sender : Sender<Message>,
  preparation : Mutex<()>, }

/// An operation may carry this across asynchronous preparation to authorize
/// precisely at its durable effects boundary. It does not own the owner
/// thread's lifetime and has no automatic release on drop.
#[derive(Clone)]
pub(crate) struct MutationControl {
  sender : Sender<Message>,
  token : ReservationToken, }

impl MutationControl {
  pub(crate) fn authorize (&self) -> Result<(), String> {
    request_mutation (&self . sender, &self . token, MutationAction::Authorize) }

  pub(crate) fn finish (&self) -> Result<(), String> {
    request_mutation (&self . sender, &self . token, MutationAction::Finish) }

  pub(crate) fn block (
    &self,
    reason : impl Into<String>,
  ) -> Result<(), String> {
    request_mutation (&self . sender, &self . token,
      MutationAction::Block (reason . into ())) }

  pub(crate) fn publish (
    &self,
    snapshot : Arc<SelectedRuntimeSnapshot>,
  ) -> Result<(), String> {
    request_mutation (&self . sender, &self . token, MutationAction::Publish (snapshot)) }

  pub(crate) fn recover (
    &self,
    snapshot : Arc<SelectedRuntimeSnapshot>,
  ) -> Result<(), String> {
    request_mutation (&self . sender, &self . token, MutationAction::Recover (snapshot)) }
}

impl CoordinatorOwner {
  /// Legacy coordinator-only construction, retained for focused fixtures.
  pub(crate) fn start (
    coordinator : MaintenanceCoordinator,
    journal : MaintenanceJournalStore,
  ) -> Self {
    Self::with_publisher (coordinator, move |next|
      journal . persist (next) . map (|_| ( ))) }

  pub(crate) fn start_with_snapshot (
    coordinator : MaintenanceCoordinator,
    journal : MaintenanceJournalStore,
    snapshot : Arc<SelectedRuntimeSnapshot>,
  ) -> Self {
    Self::with_snapshot_publisher (coordinator, Some (snapshot), move |next|
      journal . persist (next) . map (|_| ( ))) }

  fn with_publisher (
    coordinator : MaintenanceCoordinator,
    persist : impl FnMut (&MaintenanceCoordinator) -> Result<(), String>
      + Send + 'static,
  ) -> Self {
    Self::with_snapshot_publisher (coordinator, None, persist) }

  fn with_snapshot_publisher (
    coordinator : MaintenanceCoordinator,
    snapshot : Option<Arc<SelectedRuntimeSnapshot>>,
    mut persist : impl FnMut (&MaintenanceCoordinator) -> Result<(), String>
      + Send + 'static,
  ) -> Self {
    let selected : Option<Arc<SelectedRuntimeSnapshot>> = snapshot
      . map (|snapshot| pin_snapshot (&snapshot)
        . expect ("the owner's initial selected snapshot requires a Searcher"));
    let published : Arc<ArcSwap<PublishedCoordinator>> =
      Arc::new (ArcSwap::from_pointee (PublishedCoordinator {
        revision: 0, publication_revision: 0,
        coordinator, failure: None, selected, reservation: None }));
    let (sender, receiver) : (Sender<Message>, Receiver<Message>) = channel ();
    let (publisher, writes) :
      (Sender<MaintenanceCoordinator>, Receiver<MaintenanceCoordinator>) =
      channel ();
    let completions : Sender<Message> = sender . clone ();
    thread::spawn (move || {
      while let Ok (next) = writes . recv () {
        let result : Result<(), String> = persist (&next);
        if completions . send (Message::Published (result)) . is_err () {
          break; } } });
    let owner_publication : Arc<ArcSwap<PublishedCoordinator>> =
      published . clone ();
    thread::spawn (move || run_owner (owner_publication, receiver, publisher));
    Self { published, sender, preparation: Mutex::new (( )) } }

  pub(crate) fn snapshot (&self) -> MaintenanceCoordinator {
    self . published . load () . coordinator . clone () }

  pub(crate) fn subscribe_query_waits (
    &self,
    wake : SyncSender<()>,
  ) -> Result<(), String> {
    self . sender . send (Message::SubscribeQueryWaits (wake))
      . map_err (|_| "state owner stopped before query worker registration" . into ())
  }

  pub(crate) fn selected_snapshot (
    &self,
  ) -> Result<Arc<SelectedRuntimeSnapshot>, String> {
    self . published . load () . selected . clone ()
      . ok_or_else (|| "state owner has no selected snapshot" . into ()) }

  pub(crate) fn publication (
    &self,
  ) -> (u64, Arc<SelectedRuntimeSnapshot>, MaintenanceCoordinator, Option<String>) {
    let (revision, selected, coordinator, failure, _mutation) :
      (u64, Arc<SelectedRuntimeSnapshot>, MaintenanceCoordinator,
       Option<String>, Option<MutationStatus>) =
      self . publication_with_mutation ();
    (revision, selected, coordinator, failure)
  }

  pub(crate) fn publication_with_mutation (
    &self,
  ) -> (u64, Arc<SelectedRuntimeSnapshot>, MaintenanceCoordinator,
        Option<String>, Option<MutationStatus>) {
    let published : Arc<PublishedCoordinator> = self . published . load_full ();
    let failure : Option<String> = published . failure . clone () . or_else (||
      published . reservation . as_ref ()
        . and_then (|reservation| reservation . status . blocked_reason . clone ()));
    let mutation : Option<MutationStatus> = published . reservation . as_ref ()
      . map (|reservation| reservation . status . clone ());
    (published . publication_revision,
      published . selected . clone () . expect ("live owner has a selected pair"),
      published . coordinator . clone (), failure, mutation)
  }

  pub(crate) fn failure (&self) -> Option<String> {
    self . published . load () . failure . clone () }

  pub(crate) fn mutation_status (&self) -> Option<MutationStatus> {
    self . published . load () . reservation . as_ref ()
      . map (|reservation| reservation . status . clone ()) }

  pub(crate) fn mutation_control (
    &self,
    token : &ReservationToken,
  ) -> MutationControl {
    MutationControl { sender: self . sender . clone (), token: token . clone () } }

  /// The supervisor first observes a blocked operation, then claims exactly
  /// that incarnation after taking responsibility for its recovery evidence.
  /// Claiming rotates the token; the abandoned worker can no longer publish,
  /// recover, block or finish even if it subsequently resumes. The reservation
  /// remains blocked until explicit recovery publication succeeds. The caller
  /// must establish worker quiescence before applying recovery effects: token
  /// rotation fences owner completions, not external I/O already in flight.
  pub(crate) fn claim_blocked_mutation (
    &self,
    expected : &MutationStatus,
  ) -> Result<MutationControl, String> {
    let (reply, completed) :
      (SyncSender<Result<ReservationToken, String>>,
       Receiver<Result<ReservationToken, String>>) = sync_channel (1);
    self . sender . send (Message::ClaimBlocked {
      expected: expected . clone (), reply })
      . map_err (|_| "state owner stopped before recovery claim" . to_string ())?;
    let token : ReservationToken = completed . recv ()
      . map_err (|_| "state owner stopped before recovery claim acknowledgement" . to_string ())??;
    Ok (self . mutation_control (&token)) }

  pub(crate) fn reserve_mutation (
    &self,
    operation_id : impl Into<String>,
    graph_generation : GraphGeneration,
    manifest_revision : ManifestRevision,
  ) -> Result<ReservationToken, String> {
    let (reply, completed) :
      (SyncSender<Result<ReservationToken, String>>,
       Receiver<Result<ReservationToken, String>>) = sync_channel (1);
    self . sender . send (Message::Reserve (Reserve {
      operation_id: operation_id . into (), graph_generation,
      manifest_revision, reply }))
      . map_err (|_| "state owner stopped before mutation reservation" . to_string ())?;
    completed . recv ()
      . map_err (|_| "state owner stopped before reservation acknowledgement" . to_string ())? }

  /// Parent calls this only after any required durable effect authorization.
  /// Losing the acknowledgement leaves the owner Authorized, never idle.
  pub(crate) fn authorize_mutation (
    &self,
    token : &ReservationToken,
  ) -> Result<(), String> {
    self . mutate (token, MutationAction::Authorize) }

  pub(crate) fn publish_selected (
    &self,
    token : &ReservationToken,
    snapshot : Arc<SelectedRuntimeSnapshot>,
  ) -> Result<(), String> {
    self . mutate (token, MutationAction::Publish (snapshot)) }

  /// Before authorization this cancels preparation. After authorization only
  /// a completed selected publication (ordinary or recovery) permits release.
  pub(crate) fn finish_mutation (
    &self,
    token : &ReservationToken,
  ) -> Result<(), String> {
    self . mutate (token, MutationAction::Finish) }

  pub(crate) fn block_mutation (
    &self,
    token : &ReservationToken,
    reason : impl Into<String>,
  ) -> Result<(), String> {
    self . mutate (token, MutationAction::Block (reason . into ())) }

  /// Explicit recovery after the parent reconciles durable journal evidence.
  /// An ordinary worker cannot clear a block by publishing its late result.
  pub(crate) fn recover_mutation (
    &self,
    token : &ReservationToken,
    snapshot : Arc<SelectedRuntimeSnapshot>,
  ) -> Result<(), String> {
    self . mutate (token, MutationAction::Recover (snapshot)) }

  fn mutate (
    &self,
    token : &ReservationToken,
    action : MutationAction,
  ) -> Result<(), String> {
    request_mutation (&self . sender, token, action) }

  /// The adapter serializes preparation by callers while they are migrated
  /// to typed operation messages. Its closure can propose state, never publish
  /// it or perform an external effect, and may be replayed before acceptance.
  /// Journal I/O runs on the ordered worker.
  pub(crate) fn transition<T> (
    &self,
    mut transition : impl FnMut (&mut MaintenanceCoordinator) -> Result<T, String>,
  ) -> Result<T, String> {
    loop {
      let preparation : MutexGuard<'_, ()> = self . admission_guard ()?;
      let base : Arc<PublishedCoordinator> = self . published . load_full ();
      if let Some (reason) = &base . failure { return Err (reason . clone ()); }
      let mut coordinator : MaintenanceCoordinator = base . coordinator . clone ();
      let result : T = transition (&mut coordinator)?;
      if coordinator == base . coordinator { return Ok (result); }
      // Release preparation before waiting on durability so status and mutation
      // admission remain available. Obsolete precomputed states still refuse;
      // only this pure operation adapter can prepare another proposal.
      drop (preparation);
      match self . propose (base . revision, coordinator) {
        Ok (( )) => return Ok (result),
        Err (reason) if reason == OBSOLETE_COORDINATOR_PUBLICATION => {
          // The closure is pure and its result has not escaped. Recompute from
          // the next durable coordinator after a concurrent observer/report
          // proposal, never after a selected-base or persistence failure.
          self . await_publication ()?; }
        Err (reason) => return Err (reason), }
    }
  }

  fn await_publication (
    &self,
  ) -> Result<(), String> {
    let (reply, completed) :
      (SyncSender<Result<(), String>>, Receiver<Result<(), String>>) = sync_channel (1);
    self . sender . send (Message::AwaitPublication (reply))
      . map_err (|_| "state owner stopped before publication wait" . to_string ())?;
    completed . recv ()
      . map_err (|_| "state owner stopped during publication wait" . to_string ())? }

  pub(crate) fn admission_guard (&self) -> Result<MutexGuard<'_, ()>, String> {
    self . preparation . lock ()
      . map_err (|_| "coordinator proposal preparation failed" . to_string ()) }

  fn propose (
    &self,
    base_revision : u64,
    coordinator : MaintenanceCoordinator,
  ) -> Result<(), String> {
    let (reply, completed) :
      (SyncSender<Result<(), String>>, Receiver<Result<(), String>>) =
      sync_channel (1);
    self . sender . send (Message::Propose (Proposal {
      base_revision, coordinator, reply }))
      . map_err (|_| "state owner stopped before accepting proposal" . to_string ())?;
    completed . recv ()
      . map_err (|_| "state owner stopped before publication completed" . to_string ())? }
}

impl Drop for CoordinatorOwner {
  fn drop (&mut self) {
    let _ : Result<(), _> = self . sender . send (Message::Stop); }
}

fn request_mutation (
  sender : &Sender<Message>,
  token : &ReservationToken,
  action : MutationAction,
) -> Result<(), String> {
  let (reply, completed) :
    (SyncSender<Result<(), String>>, Receiver<Result<(), String>>) =
    sync_channel (1);
  sender . send (Message::Mutate {
    token: token . clone (), action, reply })
    . map_err (|_| "state owner stopped before mutation completion" . to_string ())?;
  completed . recv ()
    . map_err (|_| "state owner stopped before mutation acknowledgement" . to_string ())? }

fn run_owner (
  published : Arc<ArcSwap<PublishedCoordinator>>,
  receiver : Receiver<Message>,
  publisher : Sender<MaintenanceCoordinator>,
) {
  let mut state : PublishedCoordinator = (**published . load ()) . clone ();
  let mut pending : Option<Proposal> = None;
  let mut publication_waiters : Vec<SyncSender<Result<(), String>>> = Vec::new ();
  let mut query_wait_worker : Option<SyncSender<()>> = None;
  while let Ok (message) = receiver . recv () {
    match message {
      Message::SubscribeQueryWaits (wake) => {
        let _ = wake . try_send (());
        query_wait_worker = Some (wake);
      }
      Message::Propose (mut proposal) => {
        match admit_proposal (&state, pending . is_some (), &publisher, &mut proposal) {
          Ok (( )) => { pending = Some (proposal); },
          Err (reason) => {
            let _ : Result<(), _> = proposal . reply . send (Err (reason)); } } }
      Message::Published (result) => {
        let Some (proposal) : Option<Proposal> = pending . take () else {
          continue; };
        if result . is_ok () {
          state . revision = proposal . base_revision + 1;
          state . coordinator = proposal . coordinator;
        } else if let Err (reason) = &result {
          // An I/O error may follow rename. Keep the old readable state, but
          // require recovery before any subsequent authority-changing action.
          state . failure = Some (format! (
            "journal publication failed; recovery required: {}", reason)); }
        publish_state (&published, &mut state);
        if result . is_ok () {
          if let Some (wake) = &query_wait_worker { let _ = wake . try_send (()); }
        }
        let _ : Result<(), _> = proposal . reply . send (result . clone ());
        for waiter in publication_waiters . drain (..) {
          let _ : Result<(), _> = waiter . send (result . clone ()); } }
      Message::AwaitPublication (reply) => {
        if pending . is_some () { publication_waiters . push (reply); }
        else {
          let result : Result<(), String> = state . failure . clone ()
            . map_or (Ok (( )), Err);
          let _ : Result<(), _> = reply . send (result); } }
      Message::Reserve (request) => {
        let result : Result<ReservationToken, String> =
          reserve (&mut state,
            pending . as_ref () . map (|proposal| &proposal . coordinator . state),
            &request);
        if result . is_ok () { publish_state (&published, &mut state); }
        // A disconnected caller does not cancel an accepted reservation.
        let _ : Result<(), _> = request . reply . send (result); }
      Message::ClaimBlocked { expected, reply } => {
        let result : Result<ReservationToken, String> =
          claim_blocked (&mut state, pending . is_some (), &expected);
        if result . is_ok () { publish_state (&published, &mut state); }
        let _ : Result<(), _> = reply . send (result); }
      Message::Mutate { token, action, reply } => {
        let result : Result<(), String> =
          apply_mutation (&mut state,
            pending . as_ref () . map (|proposal| &proposal . coordinator . state),
            &token, action);
        if result . is_ok () { publish_state (&published, &mut state); }
        let _ : Result<(), _> = reply . send (result); }
      Message::Stop => break, } } }

/// Client metadata must fence graph and admission changes, including failures
/// and reservations which do not advance the durable coordinator revision.
fn publish_state (
  published : &ArcSwap<PublishedCoordinator>,
  state : &mut PublishedCoordinator,
) {
  state . publication_revision = state . publication_revision . checked_add (1)
    . expect ("owner publication revision exhausted");
  published . store (Arc::new (state . clone ())); }

fn admit_proposal (
  state : &PublishedCoordinator,
  journal_pending : bool,
  publisher : &Sender<MaintenanceCoordinator>,
  proposal : &mut Proposal,
) -> Result<(), String> {
  if let Some (reason) = &state . failure { return Err (reason . clone ()); }
  if journal_pending || proposal . base_revision != state . revision {
    return Err (OBSOLETE_COORDINATOR_PUBLICATION . into ()); }
  let committing_selected : bool = admit_selected_incident (state, &proposal . coordinator)?;
  if let Some (reservation) = &state . reservation {
    let completed_selection : bool = committing_selected
      && matches! (reservation . kind, MutationKind::Maintenance { .. })
      && reservation . status . stage == MutationStage::Published
      && reservation . status . blocked_reason . is_none ()
      && proposal . coordinator . state == CoordinatorState::Idle;
    if !completed_selection {
      admit_reserved_proposal (reservation, &proposal . coordinator . state)?; } }
  admit_proposal_base (state, &proposal . coordinator . state)?;
  super::query_waits::resolve_wait_targets (
    &mut proposal . coordinator, state . selected . as_deref ())?;
  publisher . send (proposal . coordinator . clone ())
    . map_err (|_| "journal publisher stopped; no durable success" . into ()) }

/// Moving an incident out of graph authority may reopen saves. Bind that
/// durable claim to the pair the owner has actually published, while allowing
/// subsequent report-only updates to describe their older historical pair.
fn admit_selected_incident (
  state : &PublishedCoordinator,
  next : &MaintenanceCoordinator,
) -> Result<bool, String> {
  let CoordinatorState::Active (previous) : &CoordinatorState = &state . coordinator . state else {
    return Ok (false); };
  let Some (CommittedIncident::Settling (incident)) =
    next . committed_incidents . get (&previous . incident_id) else { return Ok (false); };
  crate::maintenance::coordinator::validate_committed_incident (incident)?;
  let phase : &MaintenancePhase = previous . suspended_phase . as_ref ()
    . filter (|_| previous . phase == MaintenancePhase::AwaitingClient)
    . unwrap_or (&previous . phase);
  if !is_selection_phase (phase) || incident . epoch != previous . epoch
  || incident . candidate != previous . candidate
  || incident . buffer_census != previous . buffer_census
  || incident . initial_archive_manifest_sha256 != previous . initial_archive_manifest_sha256
  || incident . server_evidence != previous . server_evidence {
    return Err ("committed incident changed its graph-transition authority" . into ()); }
  let selected : &Arc<SelectedRuntimeSnapshot> = state . selected . as_ref ()
    . ok_or ("committed incident has no owner-published selected pair")?;
  let record : &crate::maintenance::types::SelectedStoreRecord = incident . selected_store . as_ref ()
    . expect ("validated committed selected-store record");
  if record . graph_generation != selected . selected . graph_generation
  || record . manifest_revision != selected . selected . manifest_revision {
    return Err ("committed incident does not name the owner-published selected pair" . into ()); }
  Ok (true) }

/// Coordinator revision alone does not fence a proposal prepared before a
/// selected-store mutation: graph publication deliberately leaves that revision
/// unchanged. New incidents and pending candidates must still name the selected
/// pair they observed. Existing incidents keep their historical G0 after G1.
fn admit_proposal_base (
  state : &PublishedCoordinator,
  next : &CoordinatorState,
) -> Result<(), String> {
  let Some (selected) : Option<&Arc<SelectedRuntimeSnapshot>> =
    state . selected . as_ref () else { return Ok (( )); };
  let base : Option<(GraphGeneration, ManifestRevision)> = match next {
    CoordinatorState::Active (active) => {
      let same_incident : bool = matches! (&state . coordinator . state,
        CoordinatorState::Active (current)
        if current . incident_id == active . incident_id
          && current . epoch == active . epoch);
      if same_incident { None }
      else { Some ((active . g0_graph_generation, active . g0_manifest_revision)) } }
    CoordinatorState::Pending (pending) => pending . candidate . as_ref ()
      . map (|candidate| (candidate . base_graph_generation,
        candidate . base_manifest_revision)),
    _ => None, };
  if let Some ((graph, manifest)) = base {
    if graph != selected . selected . graph_generation
    || manifest != selected . selected . manifest_revision {
      return Err ("coordinator proposal has an obsolete selected base" . into ()); } }
  Ok (( )) }

fn reserve (
  state : &mut PublishedCoordinator,
  pending : Option<&CoordinatorState>,
  request : &Reserve,
) -> Result<ReservationToken, String> {
  if let Some (reason) = &state . failure { return Err (reason . clone ()); }
  if let Some (reservation) = &state . reservation {
    return Err (format! ("mutation '{}' already reserved",
      reservation . status . operation_id)); }
  if request . operation_id . trim () . is_empty () {
    return Err ("mutation operation ID must not be empty" . into ()); }
  let kind : MutationKind = mutation_kind (
    &request . operation_id, &state . coordinator . state)?;
  admit_mutation (&kind, &state . coordinator . state, false)?;
  if let Some (next) = pending { admit_mutation (&kind, next, false)?; }
  let selected : &Arc<SelectedRuntimeSnapshot> = state . selected . as_ref ()
    . ok_or ("state owner has no selected snapshot")?;
  if selected . selected . graph_generation != request . graph_generation
  || selected . selected . manifest_revision != request . manifest_revision {
    return Err ("mutation reservation has an obsolete selected base" . into ()); }
  let token : ReservationToken = ReservationToken (Uuid::new_v4 ());
  state . reservation = Some (Reservation {
    token: token . clone (), kind,
    status: MutationStatus {
      incarnation: token . 0,
      operation_id: request . operation_id . clone (),
      graph_generation: request . graph_generation,
      manifest_revision: request . manifest_revision,
      stage: MutationStage::Prepared, blocked_reason: None }, });
  Ok (token) }

/// The runtime adapter currently names maintenance reservations in its
/// operation ID. Parse that name once into typed authority, rejecting malformed
/// maintenance/manifest names; every other name has only ordinary-save authority.
fn mutation_kind (
  operation_id : &str,
  state : &CoordinatorState,
) -> Result<MutationKind, String> {
  if operation_id == "maintenance" {
    return Err ("maintenance reservation requires an incident and epoch" . into ()); }
  if operation_id == "manifest" {
    return Err ("manifest reservation requires an operation UUID" . into ()); }
  if let Some (name) = operation_id . strip_prefix ("manifest/") {
    Uuid::parse_str (name)
      . map_err (|_| "manifest reservation requires an operation UUID" . to_string ())?;
    return Ok (MutationKind::Manifest); }
  let Some (name) : Option<&str> = operation_id . strip_prefix ("maintenance/") else {
    return Ok (MutationKind::Ordinary); };
  let (incident, epoch) : (&str, &str) = name . split_once ('/')
    . ok_or ("maintenance reservation requires an incident and epoch")?;
  let incident_id : IncidentId = IncidentId::parse (incident)?;
  let epoch : MaintenanceEpoch = MaintenanceEpoch::parse (epoch)?;
  let active : &ActiveMaintenance = matching_incident (state, &incident_id, epoch)?;
  Ok (MutationKind::Maintenance {
    incident_id, epoch, phase: active . phase . clone (),
    candidate: active . candidate . clone (), }) }

fn matching_incident<'a> (
  state : &'a CoordinatorState,
  incident_id : &IncidentId,
  epoch : MaintenanceEpoch,
) -> Result<&'a ActiveMaintenance, String> {
  match state {
    CoordinatorState::Active (active)
    if &active . incident_id == incident_id && active . epoch == epoch => Ok (active),
    _ => Err ("maintenance reservation has no matching active incident and epoch" . into ()), } }

fn is_selection_phase (phase : &MaintenancePhase) -> bool {
  matches! (phase,
    MaintenancePhase::SelectingPartial | MaintenancePhase::FullRebuildExclusive) }

fn admit_mutation (
  kind : &MutationKind,
  state : &CoordinatorState,
  publication : bool,
) -> Result<(), String> {
  match kind {
    MutationKind::Ordinary => {
      if !state . policy () . skg_saves_allowed {
        return Err (format! (
          "selected-store mutation refused during {} maintenance state", state . label ())); } }
    MutationKind::Manifest => {
      // A no-op disk observation can reconcile an earlier Pending candidate.
      // This updates only selected metadata; it cannot cross an active incident.
      if state . policy () . maintenance_locked {
        return Err ("manifest publication refused during active maintenance" . into ()); } }
    MutationKind::Maintenance { incident_id, epoch, phase, candidate } => {
      let active : &ActiveMaintenance = matching_incident (state, incident_id, *epoch)?;
      // A disconnected client cannot grant new effects. Already-authorized
      // selection can still complete under the same suspended incident phase.
      let effective_phase : &MaintenancePhase = if publication
          && active . phase == MaintenancePhase::AwaitingClient {
        active . suspended_phase . as_ref () . unwrap_or (&active . phase)
      } else { &active . phase };
      if !is_selection_phase (phase) || effective_phase != phase
      || &active . candidate != candidate {
        return Err ("maintenance mutation no longer has its reserved selection phase and candidate" . into ()); } } }
  Ok (( )) }

/// A proposal may update status while a worker is preparing or publishing.
/// It cannot establish a competing external-write barrier or another incident
/// over an ordinary reservation. Selection's own journal updates retain its
/// named incident, while authorization/publication recheck any new phase.
fn admit_reserved_proposal (
  reservation : &Reservation,
  next : &CoordinatorState,
) -> Result<(), String> {
  match &reservation . kind {
    MutationKind::Ordinary | MutationKind::Manifest =>
      admit_mutation (&reservation . kind, next, false),
    MutationKind::Maintenance { incident_id, epoch, .. } => {
      let active : &ActiveMaintenance = matching_incident (next, incident_id, *epoch)?;
      if active . phase == MaintenancePhase::RunningExternalMutation
      || active . suspended_phase == Some (MaintenancePhase::RunningExternalMutation) {
        return Err ("external mutation cannot start while a store selection is reserved" . into ()); }
      Ok (( )) } } }

fn claim_blocked (
  state : &mut PublishedCoordinator,
  journal_pending : bool,
  expected : &MutationStatus,
) -> Result<ReservationToken, String> {
  if let Some (reason) = &state . failure { return Err (reason . clone ()); }
  if journal_pending {
    return Err ("mutation recovery claim refused during journal publication" . into ()); }
  let reservation : &mut Reservation = state . reservation . as_mut ()
    . filter (|reservation| &reservation . status == expected)
    . ok_or ("mutation recovery claim has an obsolete reservation observation")?;
  if reservation . status . blocked_reason . is_none () {
    return Err ("mutation recovery claim requires a blocked reservation" . into ()); }
  let token : ReservationToken = ReservationToken (Uuid::new_v4 ());
  reservation . token = token . clone ();
  reservation . status . incarnation = token . 0;
  Ok (token) }

fn apply_mutation (
  state : &mut PublishedCoordinator,
  pending : Option<&CoordinatorState>,
  token : &ReservationToken,
  action : MutationAction,
) -> Result<(), String> {
  let reservation : &Reservation = state . reservation . as_ref ()
    . filter (|reservation| &reservation . token == token)
    . ok_or ("mutation completion has an obsolete or foreign reservation token")?;
  if let MutationAction::Block (reason) = action {
    state . reservation . as_mut () . unwrap () . status . blocked_reason =
      Some (reason);
    return Ok (( )); }
  let recovery : bool = matches! (&action, MutationAction::Recover (_));
  if recovery != reservation . status . blocked_reason . is_some () {
    return Err (if recovery {
      "mutation recovery requires a blocked reservation"
    } else { "mutation is blocked; explicit recovery is required" } . into ()); }
  match action {
    MutationAction::Authorize => {
      if let Some (reason) = &state . failure { return Err (reason . clone ()); }
      admit_mutation (&reservation . kind, &state . coordinator . state, false)?;
      if let Some (next) = pending { admit_mutation (&reservation . kind, next, false)?; }
      if reservation . status . stage != MutationStage::Prepared {
        return Err ("mutation authorization requires the prepared stage" . into ()); }
      state . reservation . as_mut () . unwrap () . status . stage =
        MutationStage::Authorized; }
    MutationAction::Publish (snapshot) | MutationAction::Recover (snapshot) => {
      if let Some (reason) = &state . failure { return Err (reason . clone ()); }
      if !recovery {
        admit_mutation (&reservation . kind, &state . coordinator . state, true)?;
        if let Some (next) = pending { admit_mutation (&reservation . kind, next, true)?; } }
      if !recovery && reservation . status . stage != MutationStage::Authorized {
        return Err ("selected publication requires the authorized stage" . into ()); }
      let selected : Arc<SelectedRuntimeSnapshot> =
        validate_publication (state, reservation, &snapshot)?;
      let status : &mut MutationStatus =
        &mut state . reservation . as_mut () . unwrap () . status;
      status . stage = MutationStage::Published;
      status . blocked_reason = None;
      status . graph_generation = selected . selected . graph_generation;
      status . manifest_revision = selected . selected . manifest_revision;
      state . selected = Some (selected); }
    MutationAction::Finish => {
      if reservation . status . stage == MutationStage::Authorized {
        return Err ("authorized mutation requires selected publication or recovery before finish" . into ()); }
      state . reservation = None; }
    MutationAction::Block (_) => unreachable! (), }
  Ok (( )) }

fn validate_publication (
  state : &PublishedCoordinator,
  reservation : &Reservation,
  snapshot : &Arc<SelectedRuntimeSnapshot>,
) -> Result<Arc<SelectedRuntimeSnapshot>, String> {
  let current : &Arc<SelectedRuntimeSnapshot> = state . selected . as_ref ()
    . ok_or ("state owner has no selected snapshot")?;
  let old : &Arc<SelectedStoreState> = &current . selected;
  let new : &Arc<SelectedStoreState> = &snapshot . selected;
  if old . graph_generation != reservation . status . graph_generation
  || old . manifest_revision != reservation . status . manifest_revision {
    return Err ("mutation publication has an obsolete selected base" . into ()); }
  if new . graph_generation < old . graph_generation
  || new . manifest_revision < old . manifest_revision {
    return Err ("mutation publication would regress the selected version" . into ()); }
  if new . graph_generation == old . graph_generation
  && !Arc::ptr_eq (&new . graph, &old . graph) {
    return Err ("a changed graph requires a new graph generation" . into ()); }
  if new . manifest_revision == old . manifest_revision
  && new . manifest != old . manifest {
    return Err ("a changed manifest requires a new manifest revision" . into ()); }
  if matches! (reservation . kind, MutationKind::Manifest) {
    // A generation counter alone does not identify the same Searcher across
    // distinct indexes. Clones retain this exact generation object's address.
    let same_searcher : bool = matches! ((&old . searcher, &new . searcher),
      (Some (old), Some (new))
        if std::ptr::eq (old . generation (), new . generation ()));
    if new . graph_generation != old . graph_generation
    || !Arc::ptr_eq (&new . graph, &old . graph)
    || !same_searcher
    || new . path_outcomes != old . path_outcomes
    || new . cyclic_roots != old . cyclic_roots
    || new . tantivy_health != old . tantivy_health
    || snapshot . env . config != current . env . config {
      return Err ("manifest reservation may publish only manifest metadata" . into ()); }
    // Index handles, warnings and all other environment inputs stay pinned to
    // the same publication; a manifest-only worker cannot replace them.
    return pin_snapshot (&SelectedRuntimeSnapshot {
      env: current . env . clone (), selected: new . clone (), }); }
  pin_snapshot (snapshot) }

/// Do not retain a worker's mutable graph handle. The selected store's actual
/// Searcher and graph are repinned together in the environment we publish.
fn pin_snapshot (
  snapshot : &SelectedRuntimeSnapshot,
) -> Result<Arc<SelectedRuntimeSnapshot>, String> {
  let mut env : SkgEnv = snapshot . env . clone ();
  env . searcher = snapshot . selected . searcher . clone ()
    . ok_or ("selected snapshot has no pinned Searcher")?;
  env . in_rust_graph = Arc::new (ArcSwap::from (snapshot . selected . clone ()));
  Ok (Arc::new (SelectedRuntimeSnapshot {
    env, selected: snapshot . selected . clone () })) }
#[cfg(test)]
mod tests {
  use super::*;
  use crate::dbs::in_rust_graph::InRustGraph;
  use crate::dbs::init::empty_in_ram_tantivy_index;
  use crate::maintenance::types::{MaintenanceOrigin, PendingReason};
  use crate::types::misc::{SkgConfig, TantivyIndex};
  use std::collections::HashMap;
  use std::path::PathBuf;
  use std::time::Duration;
  use tempfile::tempdir;

  fn fixture_snapshot () -> Arc<SelectedRuntimeSnapshot> {
    let index : TantivyIndex = empty_in_ram_tantivy_index () . unwrap ();
    let selected : Arc<SelectedStoreState> = Arc::new (
      SelectedStoreState::initial (InRustGraph::new (), Default::default ())
        . with_searcher (index . reader . searcher ()));
    Arc::new (SelectedRuntimeSnapshot {
      env: SkgEnv {
        config: SkgConfig::dummyFromSources (HashMap::new ()),
        in_rust_graph: Arc::new (ArcSwap::from (selected . clone ())),
        searcher: index . reader . searcher (),
        tantivy_index: index,
        startup_warnings: Arc::new (Vec::new ()), },
      selected, }) }

  fn next_snapshot (
    old : &SelectedRuntimeSnapshot,
  ) -> Arc<SelectedRuntimeSnapshot> {
    let selected : SelectedStoreState = old . selected
      . with_acknowledged_rebuild (InRustGraph::new (), Default::default ())
      . with_searcher (old . env . searcher . clone ());
    Arc::new (SelectedRuntimeSnapshot {
      env: old . env . clone (), selected: Arc::new (selected) }) }

  fn fixture_owner (
    snapshot : Arc<SelectedRuntimeSnapshot>,
  ) -> CoordinatorOwner {
    CoordinatorOwner::with_snapshot_publisher (
      MaintenanceCoordinator::new (), Some (snapshot), |_| Ok (( ))) }

  fn fixture_maintenance (
    phase : MaintenancePhase,
  ) -> (MaintenanceCoordinator, String) {
    let mut coordinator : MaintenanceCoordinator = MaintenanceCoordinator::new ();
    let active : ActiveMaintenance = coordinator . begin (
      MaintenanceOrigin::FullRebuild, None) . unwrap ();
    let operation : String = format! (
      "maintenance/{}/{}", active . incident_id, active . epoch . get ());
    let CoordinatorState::Active (active) : &mut CoordinatorState = &mut coordinator . state else {
      panic! ("fixture begin must be active"); };
    active . phase = phase;
    (coordinator, operation) }

  fn reserve_current (
    owner : &CoordinatorOwner,
    operation_id : &str,
  ) -> Result<ReservationToken, String> {
    let snapshot : Arc<SelectedRuntimeSnapshot> =
      owner . selected_snapshot ()?;
    owner . reserve_mutation (operation_id,
      snapshot . selected . graph_generation,
      snapshot . selected . manifest_revision) }

  #[test]
  fn publication_revision_orders_admission_graph_and_failure_without_conflating_them () {
    let before : Arc<SelectedRuntimeSnapshot> = fixture_snapshot ();
    let owner : CoordinatorOwner = fixture_owner (before . clone ());
    assert_eq! (owner . publication () . 0, 0);
    owner . transition (|coordinator| coordinator . set_pending_invalid (
      PendingReason::InvalidDisk, vec!["outside change" . into ()])) . unwrap ();
    let closed : (u64, Arc<SelectedRuntimeSnapshot>, MaintenanceCoordinator, Option<String>) =
      owner . publication ();
    owner . transition (|coordinator| coordinator . observation_equal ()) . unwrap ();
    let open : (u64, Arc<SelectedRuntimeSnapshot>, MaintenanceCoordinator, Option<String>) =
      owner . publication ();
    assert! (open . 0 > closed . 0);
    assert! (!closed . 2 . state . policy () . skg_saves_allowed);
    assert! (open . 2 . state . policy () . skg_saves_allowed);
    assert! (Arc::ptr_eq (&closed . 1, &open . 1));
    let token : ReservationToken = reserve_current (&owner, "save") . unwrap ();
    owner . authorize_mutation (&token) . unwrap ();
    owner . publish_selected (&token, next_snapshot (&before)) . unwrap ();
    let selected : (u64, Arc<SelectedRuntimeSnapshot>, MaintenanceCoordinator, Option<String>) =
      owner . publication ();
    assert! (selected . 0 > open . 0);
    assert! (selected . 1 . selected . graph_generation > open . 1 . selected . graph_generation);
    assert_eq! (selected . 2, open . 2);
    owner . mutation_control (&token) . block ("unresolved delivery") . unwrap ();
    let blocked : (u64, Arc<SelectedRuntimeSnapshot>, MaintenanceCoordinator, Option<String>) =
      owner . publication ();
    assert! (blocked . 0 > selected . 0);
    assert_eq! (blocked . 3 . as_deref (), Some ("unresolved delivery"));
    assert! (Arc::ptr_eq (&blocked . 1, &selected . 1));
  }

  #[test]
  fn publication_with_mutation_closes_admission_until_finish () {
    let before : Arc<SelectedRuntimeSnapshot> = fixture_snapshot ();
    let owner : CoordinatorOwner = fixture_owner (before . clone ());
    let initial :
      (u64, Arc<SelectedRuntimeSnapshot>, MaintenanceCoordinator,
       Option<String>, Option<MutationStatus>) =
      owner . publication_with_mutation ();
    assert! (initial . 4 . is_none ());
    let token : ReservationToken = reserve_current (&owner, "save") . unwrap ();
    let reserved :
      (u64, Arc<SelectedRuntimeSnapshot>, MaintenanceCoordinator,
       Option<String>, Option<MutationStatus>) =
      owner . publication_with_mutation ();
    assert! (reserved . 0 > initial . 0);
    assert! (reserved . 4 . is_some ());
    assert! (Arc::ptr_eq (&reserved . 1 . selected, &initial . 1 . selected));
    owner . finish_mutation (&token) . unwrap ();
    let finished :
      (u64, Arc<SelectedRuntimeSnapshot>, MaintenanceCoordinator,
       Option<String>, Option<MutationStatus>) =
      owner . publication_with_mutation ();
    assert! (finished . 0 > reserved . 0);
    assert! (finished . 4 . is_none ());
    assert! (Arc::ptr_eq (&finished . 1 . selected, &reserved . 1 . selected));
  }

  #[test]
  fn ordinary_mutations_obey_owner_policy_for_every_dispatch_name () {
    let names : [&str; 3] = ["save-uuid", "strip-whitespace/uuid", "recompute/uuid"];
    let owner : CoordinatorOwner = fixture_owner (fixture_snapshot ());
    for name in names {
      let token : ReservationToken = reserve_current (&owner, name) . unwrap ();
      owner . finish_mutation (&token) . unwrap (); }
    owner . transition (|coordinator| coordinator . set_pending_invalid (
      PendingReason::InvalidDisk, vec!["source changed" . into ()])) . unwrap ();
    for name in names { assert! (reserve_current (&owner, name) . is_err ()); }
    owner . transition (|coordinator| coordinator . observation_equal ()) . unwrap ();
    owner . transition (|coordinator| coordinator . begin (
      MaintenanceOrigin::FullRebuild, None) . map (|_| ( ))) . unwrap ();
    for name in names { assert! (reserve_current (&owner, name) . is_err ()); } }

  #[test]
  fn manifest_reconciliation_can_resolve_pending_but_cannot_change_graph_or_searcher () {
    let before : Arc<SelectedRuntimeSnapshot> = fixture_snapshot ();
    let owner : CoordinatorOwner = fixture_owner (before . clone ());
    owner . transition (|coordinator| coordinator . set_pending_invalid (
      PendingReason::InvalidDisk, Vec::new ())) . unwrap ();
    assert! (reserve_current (&owner, "manifest/not-a-uuid") . is_err ());
    let name : String = format! ("manifest/{}", Uuid::new_v4 ());
    let token : ReservationToken = reserve_current (&owner, &name) . unwrap ();
    assert! (owner . transition (|coordinator| coordinator . begin (
      MaintenanceOrigin::FullRebuild, None) . map (|_| ( ))) . is_err ());
    owner . authorize_mutation (&token) . unwrap ();
    assert! (owner . publish_selected (&token, next_snapshot (&before)) . is_err ());
    let different_searcher : Arc<SelectedRuntimeSnapshot> = fixture_snapshot ();
    let replacement : SelectedStoreState = before . selected . as_ref () . clone ()
      . with_searcher (different_searcher . env . searcher . clone ());
    assert! (owner . publish_selected (&token, Arc::new (SelectedRuntimeSnapshot {
      env: before . env . clone (), selected: Arc::new (replacement), })) . is_err ());
    let replacement : SelectedStoreState = before . selected . with_semantically_equal_manifest (
      std::collections::BTreeMap::from ([(PathBuf::from ("/source/n.skg"),
        crate::types::store_state::PathDigest::of_bytes (b"reformatted"))]));
    owner . publish_selected (&token, Arc::new (SelectedRuntimeSnapshot {
      env: before . env . clone (), selected: Arc::new (replacement), })) . unwrap ();
    owner . finish_mutation (&token) . unwrap ();
    let actual : Arc<SelectedRuntimeSnapshot> = owner . selected_snapshot () . unwrap ();
    assert_eq! (actual . selected . graph_generation, before . selected . graph_generation);
    assert_eq! (actual . selected . manifest_revision,
      before . selected . manifest_revision . successor ());
    assert! (Arc::ptr_eq (&actual . selected . graph, &before . selected . graph));
    owner . transition (|coordinator| coordinator . observation_equal ()) . unwrap ();
    let ordinary : ReservationToken = reserve_current (&owner, "save") . unwrap ();
    owner . finish_mutation (&ordinary) . unwrap (); }

  #[test]
  fn maintenance_names_require_matching_incident_epoch_and_selection_phase () {
    let (coordinator, name) : (MaintenanceCoordinator, String) =
      fixture_maintenance (MaintenancePhase::SelectingPartial);
    let owner : CoordinatorOwner = CoordinatorOwner::with_snapshot_publisher (
      coordinator, Some (fixture_snapshot ()), |_| Ok (( )));
    for malformed in ["maintenance", "maintenance/", "maintenance/id/1",
      "maintenance/id/1/extra"] {
      assert! (reserve_current (&owner, malformed) . is_err ()); }
    assert! (reserve_current (&owner,
      &format! ("maintenance/{}/1", IncidentId::new ())) . is_err ());
    assert! (reserve_current (&owner, &format! ("{}0", name)) . is_err ());
    let token : ReservationToken = reserve_current (&owner, &name) . unwrap ();
    owner . finish_mutation (&token) . unwrap ();
    owner . transition (|coordinator| {
      let CoordinatorState::Active (active) : &mut CoordinatorState = &mut coordinator . state else {
        unreachable! (); };
      active . phase = MaintenancePhase::RunningExternalMutation;
      Ok (( ))
    }) . unwrap ();
    assert! (reserve_current (&owner, &name) . is_err ()); }

  #[test]
  fn prepared_and_authorized_saves_refuse_maintenance_barriers_before_journaling () {
    let owner : CoordinatorOwner = fixture_owner (fixture_snapshot ());
    let token : ReservationToken = reserve_current (&owner, "A") . unwrap ();
    for authorized in [false, true] {
      if authorized { owner . authorize_mutation (&token) . unwrap (); }
      assert! (owner . transition (|coordinator| coordinator . begin (
        MaintenanceOrigin::Pull, None) . map (|_| ( ))) . is_err ());
      assert! (owner . transition (|coordinator| coordinator . set_pending_invalid (
        PendingReason::InvalidDisk, Vec::new ())) . is_err ());
      assert_eq! (owner . snapshot () . state, CoordinatorState::Idle); }
    owner . publish_selected (&token, owner . selected_snapshot () . unwrap ()) . unwrap ();
    // Published remains reserved until the worker's explicit finish.
    assert! (owner . transition (|coordinator| coordinator . begin (
      MaintenanceOrigin::Pull, None) . map (|_| ( ))) . is_err ());
    owner . finish_mutation (&token) . unwrap ();
    owner . transition (|coordinator| coordinator . begin (
      MaintenanceOrigin::Pull, None) . map (|_| ( ))) . unwrap ();
    assert! (reserve_current (&owner, "B") . is_err ()); }

  #[test]
  fn pending_maintenance_begin_refuses_save_against_still_idle_snapshot () {
    let (entered, started) : (SyncSender<()>, Receiver<()>) = sync_channel (1);
    let (release, resume) : (SyncSender<()>, Receiver<()>) = sync_channel (1);
    let owner : Arc<CoordinatorOwner> = Arc::new (
      CoordinatorOwner::with_snapshot_publisher (
        MaintenanceCoordinator::new (), Some (fixture_snapshot ()), move |_| {
          entered . send (( )) . unwrap ();
          resume . recv () . unwrap ();
          Ok (( )) }));
    let worker_owner : Arc<CoordinatorOwner> = owner . clone ();
    let worker : thread::JoinHandle<Result<(), String>> = thread::spawn (move ||
      worker_owner . transition (|coordinator| coordinator . begin (
        MaintenanceOrigin::Pull, None) . map (|_| ( ))));
    started . recv_timeout (Duration::from_secs (2)) . unwrap ();
    assert_eq! (owner . snapshot () . state, CoordinatorState::Idle);
    let refused : bool = reserve_current (&owner, "A") . is_err ();
    release . send (( )) . unwrap ();
    worker . join () . unwrap () . unwrap ();
    assert! (refused);
    assert! (reserve_current (&owner, "A") . is_err ()); }

  #[test]
  fn maintenance_proposal_prepared_before_save_cannot_reuse_obsolete_g0 () {
    let before : Arc<SelectedRuntimeSnapshot> = fixture_snapshot ();
    let owner : CoordinatorOwner = fixture_owner (before . clone ());
    let mut proposal : MaintenanceCoordinator = owner . snapshot ();
    proposal . begin (MaintenanceOrigin::Pull, None) . unwrap ();
    let token : ReservationToken = reserve_current (&owner, "save") . unwrap ();
    owner . authorize_mutation (&token) . unwrap ();
    owner . publish_selected (&token, next_snapshot (&before)) . unwrap ();
    owner . finish_mutation (&token) . unwrap ();
    assert_eq! (owner . published . load () . revision, 0);
    assert! (owner . propose (0, proposal) . unwrap_err () . contains ("selected base"));
    assert_eq! (owner . snapshot () . state, CoordinatorState::Idle); }

  #[test]
  fn query_wait_no_change_uses_the_named_outcome_without_a_generation_bump () {
    let selected : Arc<SelectedRuntimeSnapshot> = fixture_snapshot ();
    let mut coordinator : MaintenanceCoordinator = MaintenanceCoordinator::new ();
    let incident : ActiveMaintenance = coordinator . begin (MaintenanceOrigin::FullRebuild, None) . unwrap ();
    let wait : crate::maintenance::query_waits::QueryWaitRecord =
      super::super::query_waits::tests::wait_for (&incident);
    coordinator . register_query_wait (wait . clone ()) . unwrap ();
    let CoordinatorState::Active (active) : &mut CoordinatorState = &mut coordinator . state else { unreachable! (); };
    active . selected_store = Some (crate::maintenance::types::SelectedStoreRecord {
      graph_generation: selected . selected . graph_generation,
      manifest_revision: selected . selected . manifest_revision,
      tantivy_generation: 1, tantivy_outcome: "unchanged" . into (), });
    super::super::query_waits::resolve_wait_targets (&mut coordinator, Some (&selected)) . unwrap ();
    let resolved : &crate::maintenance::query_waits::QueryWaitRecord = coordinator
      . query_waits . get (&wait . operation_id) . unwrap ();
    assert_eq! (resolved . state, crate::maintenance::query_waits::QueryWaitState::Executing);
    assert_eq! (resolved . resolved_target . as_ref () . unwrap () . graph_generation,
                selected . selected . graph_generation . get ());
  }

  #[test]
  fn incident_readiness_requires_the_published_pair_and_old_reports_cannot_reselect_it () {
    let before : Arc<SelectedRuntimeSnapshot> = fixture_snapshot ();
    let after : Arc<SelectedRuntimeSnapshot> = next_snapshot (&before);
    let mut coordinator : MaintenanceCoordinator = MaintenanceCoordinator::new ();
    let active : ActiveMaintenance = coordinator . begin (MaintenanceOrigin::FullRebuild, None) . unwrap ();
    coordinator . archive_ready (&active . incident_id, active . epoch, "initial" . into ()) . unwrap ();
    coordinator . record_server_evidence (&active . incident_id, active . epoch,
      crate::maintenance::types::ServerEvidenceRecord {
        path: "evidence" . into (), bundle_sha256: "bundle" . into (),
        artifact_count: 1, total_file_bytes: 2, }) . unwrap ();
    coordinator . transition (&active . incident_id, active . epoch,
      MaintenancePhase::FullRebuildExclusive) . unwrap ();
    let wait : crate::maintenance::query_waits::QueryWaitRecord =
      super::super::query_waits::tests::wait_for (&active);
    coordinator . register_query_wait (wait . clone ()) . unwrap ();
    let journal : Arc<Mutex<Vec<MaintenanceCoordinator>>> = Arc::new (Mutex::new (Vec::new ()));
    let journal_writer : Arc<Mutex<Vec<MaintenanceCoordinator>>> = journal . clone ();
    let owner : CoordinatorOwner = CoordinatorOwner::with_snapshot_publisher (
      coordinator, Some (before), move |next| {
        journal_writer . lock () . unwrap () . push (next . clone ()); Ok (( )) });
    let mut proposal : MaintenanceCoordinator = owner . snapshot ();
    proposal . store_rebuilt (&active . incident_id, active . epoch,
      crate::maintenance::types::SelectedStoreRecord {
        graph_generation: after . selected . graph_generation,
        manifest_revision: after . selected . manifest_revision,
        tantivy_generation: 1, tantivy_outcome: "committed" . into (), }) . unwrap ();
    assert! (owner . propose (0, proposal . clone ()) . unwrap_err () . contains ("published selected pair"));
    let token : ReservationToken = reserve_current (&owner, &format! (
      "maintenance/{}/{}", active . incident_id, active . epoch . get ())) . unwrap ();
    owner . authorize_mutation (&token) . unwrap ();
    owner . publish_selected (&token, after . clone ()) . unwrap ();
    let mut wrong_manifest : MaintenanceCoordinator = proposal . clone ();
    let Some (CommittedIncident::Settling (incident)) =
      wrong_manifest . committed_incidents . get_mut (&active . incident_id) else { unreachable! (); };
    incident . selected_store . as_mut () . unwrap () . manifest_revision = ManifestRevision::INITIAL;
    assert! (owner . propose (0, wrong_manifest) . is_err ());
    owner . propose (0, proposal) . unwrap ();
    assert_eq! (owner . snapshot () . state, CoordinatorState::Idle);
    let resolved : crate::maintenance::query_waits::QueryWaitRecord = owner . snapshot ()
      . query_waits . get (&wait . operation_id) . unwrap () . clone ();
    assert_eq! (resolved . state, crate::maintenance::query_waits::QueryWaitState::Executing);
    assert_eq! (resolved . resolved_target . as_ref () . unwrap () . graph_generation,
                after . selected . graph_generation . get ());
    assert_eq! (journal . lock () . unwrap () [0] . query_waits . get (&wait . operation_id),
                Some (&resolved));
    assert! (super::super::query_waits::decode_config (
      &resolved . resolved_target . as_ref () . unwrap () . config_snapshot) . unwrap () == after . env . config);
    assert! (reserve_current (&owner, "save") . is_err ());
    owner . finish_mutation (&token) . unwrap ();
    let save : ReservationToken = reserve_current (&owner, "save") . unwrap ();
    owner . authorize_mutation (&save) . unwrap ();
    let newer : Arc<SelectedRuntimeSnapshot> = next_snapshot (&after);
    owner . publish_selected (&save, newer . clone ()) . unwrap ();
    owner . finish_mutation (&save) . unwrap ();
    owner . transition (|coordinator| coordinator . record_view_settlements (
      &active . incident_id, active . epoch, Vec::new ())) . unwrap ();
    assert! (Arc::ptr_eq (&owner . selected_snapshot () . unwrap () . selected, &newer . selected));
    assert_eq! (owner . snapshot () . query_waits . get (&wait . operation_id), Some (&resolved));
    assert_eq! (owner . snapshot () . incident (&active . incident_id, active . epoch)
      . unwrap () . selected_store . as_ref () . unwrap () . graph_generation,
      after . selected . graph_generation); }

  #[test]
  fn selection_cannot_grant_external_mutation_and_publishes_after_disconnect () {
    let before : Arc<SelectedRuntimeSnapshot> = fixture_snapshot ();
    let (coordinator, name) : (MaintenanceCoordinator, String) =
      fixture_maintenance (MaintenancePhase::FullRebuildExclusive);
    let owner : CoordinatorOwner = CoordinatorOwner::with_snapshot_publisher (
      coordinator, Some (before . clone ()), |_| Ok (( )));
    let token : ReservationToken = reserve_current (&owner, &name) . unwrap ();
    assert! (owner . transition (|coordinator| {
      let CoordinatorState::Active (active) : &mut CoordinatorState = &mut coordinator . state else {
        unreachable! (); };
      active . phase = MaintenancePhase::RunningExternalMutation;
      Ok (( ))
    }) . is_err ());
    owner . transition (|coordinator| { coordinator . disconnected (); Ok (( )) }) . unwrap ();
    assert! (owner . authorize_mutation (&token) . is_err ());
    owner . transition (|coordinator| { coordinator . reconnected (); Ok (( )) }) . unwrap ();
    owner . authorize_mutation (&token) . unwrap ();
    owner . transition (|coordinator| { coordinator . disconnected (); Ok (( )) }) . unwrap ();
    owner . publish_selected (&token, next_snapshot (&before)) . unwrap ();
    owner . finish_mutation (&token) . unwrap ();
    assert! (reserve_current (&owner, "ordinary") . is_err ()); }

  #[test]
  fn pending_selection_revocation_refuses_authorization_and_publication () {
    for authorized in [false, true] {
      let (entered, started) : (SyncSender<()>, Receiver<()>) = sync_channel (1);
      let (release, resume) : (SyncSender<()>, Receiver<()>) = sync_channel (1);
      let (coordinator, name) : (MaintenanceCoordinator, String) =
        fixture_maintenance (MaintenancePhase::SelectingPartial);
      let before : Arc<SelectedRuntimeSnapshot> = fixture_snapshot ();
      let owner : Arc<CoordinatorOwner> = Arc::new (
        CoordinatorOwner::with_snapshot_publisher (
          coordinator, Some (before . clone ()), move |_| {
            entered . send (( )) . unwrap ();
            resume . recv () . unwrap ();
            Ok (( )) }));
      let token : ReservationToken = reserve_current (&owner, &name) . unwrap ();
      if authorized { owner . authorize_mutation (&token) . unwrap (); }
      let worker_owner : Arc<CoordinatorOwner> = owner . clone ();
      let worker : thread::JoinHandle<Result<(), String>> = thread::spawn (move ||
        worker_owner . transition (|coordinator| {
          let CoordinatorState::Active (active) : &mut CoordinatorState = &mut coordinator . state else {
            unreachable! (); };
          active . phase = MaintenancePhase::BlockedStoreHealth;
          Ok (( )) }));
      started . recv_timeout (Duration::from_secs (2)) . unwrap ();
      let refused : bool = if authorized {
        owner . publish_selected (&token, next_snapshot (&before)) . is_err ()
      } else { owner . authorize_mutation (&token) . is_err () };
      release . send (( )) . unwrap ();
      worker . join () . unwrap () . unwrap ();
      assert! (refused);
      assert! (Arc::ptr_eq (&before . selected,
        &owner . selected_snapshot () . unwrap () . selected));
      assert! (owner . publish_selected (&token, next_snapshot (&before)) . is_err ());
      if authorized {
        owner . block_mutation (&token, "effect reconciliation needed") . unwrap ();
        owner . recover_mutation (&token, before) . unwrap (); }
      owner . finish_mutation (&token) . unwrap (); } }

  #[test]
  fn supervisor_claim_survives_worker_drop_and_fences_every_abandoned_control () {
    let before : Arc<SelectedRuntimeSnapshot> = fixture_snapshot ();
    let owner : CoordinatorOwner = fixture_owner (before . clone ());
    let token : ReservationToken = reserve_current (&owner, "A") . unwrap ();
    let worker : MutationControl = owner . mutation_control (&token);
    let original : MutationStatus = owner . mutation_status () . unwrap ();
    assert! (owner . claim_blocked_mutation (&original) . is_err ());
    worker . authorize () . unwrap ();
    worker . block ("worker exited with unresolved effects") . unwrap ();
    let abandoned : MutationControl = worker . clone ();
    drop (worker);
    drop (token);
    let observed : MutationStatus = owner . mutation_status () . unwrap ();
    assert! (owner . claim_blocked_mutation (&original) . is_err ());
    let supervisor : MutationControl = owner . claim_blocked_mutation (&observed) . unwrap ();
    assert! (owner . claim_blocked_mutation (&observed) . is_err ());
    assert! (abandoned . authorize () . is_err ());
    assert! (abandoned . publish (before . clone ()) . is_err ());
    assert! (abandoned . recover (before . clone ()) . is_err ());
    assert! (abandoned . block ("obsolete worker") . is_err ());
    assert! (abandoned . finish () . is_err ());
    assert! (supervisor . finish () . is_err ());
    assert! (reserve_current (&owner, "B") . is_err ());
    supervisor . recover (before) . unwrap ();
    supervisor . finish () . unwrap ();
    let next : ReservationToken = reserve_current (&owner, "A") . unwrap ();
    owner . authorize_mutation (&next) . unwrap ();
    owner . block_mutation (&next, "worker exited with unresolved effects") . unwrap ();
    assert! (owner . claim_blocked_mutation (&observed) . is_err ());
    let latest : MutationStatus = owner . mutation_status () . unwrap ();
    let recovery : MutationControl = owner . claim_blocked_mutation (&latest) . unwrap ();
    recovery . recover (owner . selected_snapshot () . unwrap ()) . unwrap ();
    recovery . finish () . unwrap (); }

  #[test]
  fn journal_failure_blocks_selected_publication_and_retains_authorized_operation () {
    let (entered, started) : (SyncSender<()>, Receiver<()>) = sync_channel (1);
    let (release, resume) : (SyncSender<()>, Receiver<()>) = sync_channel (1);
    let before : Arc<SelectedRuntimeSnapshot> = fixture_snapshot ();
    let owner : Arc<CoordinatorOwner> = Arc::new (
      CoordinatorOwner::with_snapshot_publisher (
        MaintenanceCoordinator::new (), Some (before . clone ()), move |_| {
          entered . send (( )) . unwrap ();
          resume . recv () . unwrap ();
          Err ("journal acknowledgement failed" . into ()) }));
    let token : ReservationToken = reserve_current (&owner, "A") . unwrap ();
    let worker_owner : Arc<CoordinatorOwner> = owner . clone ();
    let worker : thread::JoinHandle<Result<(), String>> = thread::spawn (move ||
      worker_owner . transition (|coordinator| coordinator . observation_started ()));
    started . recv_timeout (Duration::from_secs (2)) . unwrap ();
    // The proposed observation still permits this exact operation. Once its
    // journal failure is known, neither ordinary nor recovery publication can
    // acknowledge a new selected pair under uncertain durable authority.
    let authorization : Result<(), String> = owner . authorize_mutation (&token);
    release . send (( )) . unwrap ();
    assert! (worker . join () . unwrap () . is_err ());
    authorization . unwrap ();
    assert! (owner . publish_selected (&token, next_snapshot (&before)) . is_err ());
    owner . block_mutation (&token, "journal reconciliation needed") . unwrap ();
    assert! (owner . recover_mutation (&token, before . clone ()) . is_err ());
    let observed : MutationStatus = owner . mutation_status () . unwrap ();
    assert! (owner . claim_blocked_mutation (&observed) . is_err ());
    assert! (owner . finish_mutation (&token) . is_err ());
    assert! (reserve_current (&owner, "B") . is_err ());
    assert! (Arc::ptr_eq (&owner . selected_snapshot () . unwrap () . selected,
      &before . selected)); }

  #[test]
  fn reservation_refuses_competitors_and_stale_bases_until_explicit_finish () {
    let snapshot : Arc<SelectedRuntimeSnapshot> = fixture_snapshot ();
    let owner : CoordinatorOwner = fixture_owner (snapshot . clone ());
    assert! (owner . reserve_mutation ("obsolete graph",
      GraphGeneration::INITIAL . successor (), ManifestRevision::INITIAL) . is_err ());
    assert! (owner . reserve_mutation ("obsolete manifest",
      GraphGeneration::INITIAL, ManifestRevision::INITIAL . successor ()) . is_err ());
    let first : ReservationToken = reserve_current (&owner, "A") . unwrap ();
    assert! (reserve_current (&owner, "B") . is_err ());
    let wrong : ReservationToken = ReservationToken (Uuid::new_v4 ());
    assert! (owner . authorize_mutation (&wrong) . is_err ());
    assert! (owner . publish_selected (&wrong, snapshot . clone ()) . is_err ());
    assert! (owner . finish_mutation (&wrong) . is_err ());
    assert! (owner . publish_selected (&first, snapshot) . is_err ());
    owner . finish_mutation (&first) . unwrap ();
    let second : ReservationToken = reserve_current (&owner, "A") . unwrap ();
    assert_ne! (first, second);
    assert! (owner . finish_mutation (&first) . is_err ());
    assert! (owner . authorize_mutation (&first) . is_err ());
    owner . finish_mutation (&second) . unwrap (); }

  #[test]
  fn authorized_reservation_survives_lost_reply_and_token_drop () {
    let owner : CoordinatorOwner = fixture_owner (fixture_snapshot ());
    let token : ReservationToken = reserve_current (&owner, "A") . unwrap ();
    let (reply, abandoned) :
      (SyncSender<Result<(), String>>, Receiver<Result<(), String>>) =
      sync_channel (1);
    drop (abandoned);
    owner . sender . send (Message::Mutate {
      token: token . clone (), action: MutationAction::Authorize, reply })
      . unwrap ();
    // FIFO processing means this acknowledgement observes the abandoned
    // authorization above; no timing or sleeping is needed.
    assert! (owner . finish_mutation (&token) . is_err ());
    assert_eq! (owner . mutation_status () . unwrap () . stage,
      MutationStage::Authorized);
    drop (token);
    assert! (reserve_current (&owner, "B") . is_err ());
    assert_eq! (owner . mutation_status () . unwrap () . operation_id, "A"); }

  #[test]
  fn mutation_control_clones_neither_stop_owner_nor_release_authority_on_drop () {
    let before : Arc<SelectedRuntimeSnapshot> = fixture_snapshot ();
    let owner : CoordinatorOwner = fixture_owner (before . clone ());
    let token : ReservationToken = reserve_current (&owner, "A") . unwrap ();
    let control : MutationControl = owner . mutation_control (&token);
    let worker : MutationControl = control . clone ();
    drop (token);
    control . authorize () . unwrap ();
    drop (control);
    assert! (reserve_current (&owner, "B") . is_err ());
    worker . block ("worker needs journal reconciliation") . unwrap ();
    assert! (worker . publish (before . clone ()) . is_err ());
    worker . recover (next_snapshot (&before)) . unwrap ();
    worker . finish () . unwrap ();
    drop (worker);
    let next : ReservationToken = reserve_current (&owner, "B") . unwrap ();
    owner . finish_mutation (&next) . unwrap (); }

  #[test]
  fn publish_and_finish_release_only_the_matching_operation () {
    let before : Arc<SelectedRuntimeSnapshot> = fixture_snapshot ();
    let after : Arc<SelectedRuntimeSnapshot> = next_snapshot (&before);
    let owner : CoordinatorOwner = fixture_owner (before . clone ());
    let first : ReservationToken = reserve_current (&owner, "A") . unwrap ();
    owner . authorize_mutation (&first) . unwrap ();
    assert! (owner . finish_mutation (&first) . is_err ());
    owner . publish_selected (&first, after . clone ()) . unwrap ();
    assert_eq! (owner . selected_snapshot () . unwrap () . selected . graph_generation,
      after . selected . graph_generation);
    assert! (reserve_current (&owner, "B") . is_err ());
    assert! (owner . publish_selected (&first, after . clone ()) . is_err ());
    owner . finish_mutation (&first) . unwrap ();
    let second : ReservationToken = reserve_current (&owner, "B") . unwrap ();
    assert! (owner . publish_selected (&first, before . clone ()) . is_err ());
    owner . authorize_mutation (&second) . unwrap ();
    assert! (owner . publish_selected (&second, before) . is_err ());
    owner . publish_selected (&second, after) . unwrap ();
    owner . finish_mutation (&second) . unwrap (); }

  #[test]
  fn blocked_mutation_requires_recovery_and_cannot_regress_published_selection () {
    let before : Arc<SelectedRuntimeSnapshot> = fixture_snapshot ();
    let after : Arc<SelectedRuntimeSnapshot> = next_snapshot (&before);
    let owner : CoordinatorOwner = fixture_owner (before . clone ());
    let token : ReservationToken = reserve_current (&owner, "A") . unwrap ();
    owner . authorize_mutation (&token) . unwrap ();
    owner . block_mutation (&token, "effect outcome unknown") . unwrap ();
    assert! (owner . finish_mutation (&token) . is_err ());
    assert! (owner . publish_selected (&token, after . clone ()) . is_err ());
    assert! (reserve_current (&owner, "B") . is_err ());
    owner . recover_mutation (&token, after . clone ()) . unwrap ();
    assert_eq! (owner . mutation_status () . unwrap () . stage, MutationStage::Published);
    owner . block_mutation (&token, "final acknowledgement unknown") . unwrap ();
    assert! (owner . recover_mutation (&token, before) . is_err ());
    owner . recover_mutation (&token, after) . unwrap ();
    owner . finish_mutation (&token) . unwrap ();
    assert! (reserve_current (&owner, "B") . is_ok ()); }

  #[test]
  fn publication_pins_graph_and_searcher_and_refuses_unversioned_changes () {
    let before : Arc<SelectedRuntimeSnapshot> = fixture_snapshot ();
    let owner : CoordinatorOwner = fixture_owner (before . clone ());
    let token : ReservationToken = reserve_current (&owner, "A") . unwrap ();
    owner . authorize_mutation (&token) . unwrap ();
    let mut changed : SelectedStoreState = (*before . selected) . clone ();
    changed . graph = Arc::new (InRustGraph::new ());
    assert! (owner . publish_selected (&token, Arc::new (SelectedRuntimeSnapshot {
      env: before . env . clone (), selected: Arc::new (changed) })) . is_err ());
    let mut missing_searcher : SelectedStoreState = (*before . selected) . clone ();
    missing_searcher . searcher = None;
    assert! (owner . publish_selected (&token, Arc::new (SelectedRuntimeSnapshot {
      env: before . env . clone (), selected: Arc::new (missing_searcher) })) . is_err ());
    let after : Arc<SelectedRuntimeSnapshot> = next_snapshot (&before);
    owner . publish_selected (&token, after . clone ()) . unwrap ();
    // The worker's mutable environment handle still points at old data.
    // Its later stores cannot change the pinned environment the owner emits.
    after . env . in_rust_graph . store (before . selected . clone ());
    let actual : Arc<SelectedRuntimeSnapshot> = owner . selected_snapshot () . unwrap ();
    assert! (Arc::ptr_eq (&actual . selected,
      &actual . env . in_rust_graph . load_full ()));
    assert_eq! (actual . env . searcher . generation (),
      actual . selected . searcher . as_ref () . unwrap () . generation ());
    assert_eq! (actual . selected . graph_generation,
      GraphGeneration::INITIAL . successor ());
    owner . finish_mutation (&token) . unwrap (); }

  #[test]
  fn reservation_survives_coordinator_publication_without_stalling_status () {
    let (entered, started) : (SyncSender<()>, Receiver<()>) = sync_channel (1);
    let (release, resume) : (SyncSender<()>, Receiver<()>) = sync_channel (1);
    let snapshot : Arc<SelectedRuntimeSnapshot> = fixture_snapshot ();
    let owner : Arc<CoordinatorOwner> = Arc::new (
      CoordinatorOwner::with_snapshot_publisher (
        MaintenanceCoordinator::new (), Some (snapshot . clone ()), move |_| {
          entered . send (( )) . unwrap ();
          resume . recv () . unwrap ();
          Ok (( )) }));
    let token : ReservationToken = reserve_current (&owner, "A") . unwrap ();
    owner . authorize_mutation (&token) . unwrap ();
    let worker_owner : Arc<CoordinatorOwner> = owner . clone ();
    let worker : thread::JoinHandle<Result<(), String>> = thread::spawn (move ||
      worker_owner . transition (|coordinator| coordinator . observation_started ()));
    started . recv_timeout (Duration::from_secs (2)) . unwrap ();
    assert_eq! (owner . snapshot () . state, CoordinatorState::Idle);
    assert_eq! (owner . mutation_status () . unwrap () . stage, MutationStage::Authorized);
    assert! (Arc::ptr_eq (&owner . selected_snapshot () . unwrap () . selected,
      &snapshot . selected));
    let after : Arc<SelectedRuntimeSnapshot> = next_snapshot (&snapshot);
    owner . publish_selected (&token, after . clone ()) . unwrap ();
    let (answered, response) :
      (SyncSender<Result<(), String>>, Receiver<Result<(), String>>) = sync_channel (1);
    let competing_owner : Arc<CoordinatorOwner> = owner . clone ();
    let competing : thread::JoinHandle<()> = thread::spawn (move || {
      // A precomputed proposal cannot be replayed and still refuses promptly.
      // Pure operation retries are covered by the archive/observer schedule.
      let mut proposal : MaintenanceCoordinator = competing_owner . snapshot ();
      proposal . observation_started () . unwrap ();
      let result : Result<(), String> = competing_owner . propose (0, proposal);
      answered . send (result) . unwrap (); });
    let timely : Result<Result<(), String>, _> =
      response . recv_timeout (Duration::from_secs (2));
    // Release before asserting so a regression cannot strand either worker.
    release . send (( )) . unwrap ();
    worker . join () . unwrap () . unwrap ();
    competing . join () . unwrap ();
    assert! (timely . unwrap () . is_err ());
    assert_eq! (owner . mutation_status () . unwrap () . operation_id, "A");
    assert_eq! (owner . mutation_status () . unwrap () . stage, MutationStage::Published);
    assert! (Arc::ptr_eq (&owner . selected_snapshot () . unwrap () . selected,
      &after . selected));
    assert_eq! (owner . snapshot () . state, CoordinatorState::Observing);
    owner . finish_mutation (&token) . unwrap (); }

  #[test]
  fn compatible_pending_status_allows_preparation_but_failure_refuses_effects () {
    let (entered, started) : (SyncSender<()>, Receiver<()>) = sync_channel (1);
    let (release, resume) : (SyncSender<()>, Receiver<()>) = sync_channel (1);
    let owner : Arc<CoordinatorOwner> = Arc::new (
      CoordinatorOwner::with_snapshot_publisher (
        MaintenanceCoordinator::new (), Some (fixture_snapshot ()), move |_| {
          entered . send (( )) . unwrap ();
          resume . recv () . unwrap ();
          Err ("disk full" . into ()) }));
    let worker_owner : Arc<CoordinatorOwner> = owner . clone ();
    let worker : thread::JoinHandle<Result<(), String>> = thread::spawn (move ||
      worker_owner . transition (|coordinator| coordinator . observation_started ()));
    started . recv_timeout (Duration::from_secs (2)) . unwrap ();
    let prepared : ReservationToken = reserve_current (&owner, "during journal") . unwrap ();
    release . send (( )) . unwrap ();
    assert! (worker . join () . unwrap () . is_err ());
    assert! (owner . failure () . unwrap () . contains ("recovery required"));
    assert! (owner . authorize_mutation (&prepared) . is_err ());
    owner . finish_mutation (&prepared) . unwrap ();
    assert! (reserve_current (&owner, "after failure") . is_err ());
    assert_eq! (owner . snapshot () . state, CoordinatorState::Idle); }

  #[test]
  fn archive_transition_rebases_after_observer_publication_but_never_after_io_failure () {
    for fail_publication in [false, true] {
      let (entered, started) : (SyncSender<()>, Receiver<()>) = sync_channel (1);
      let (release, resume) : (SyncSender<()>, Receiver<()>) = sync_channel (1);
      let (prepared, first_attempt) : (Sender<()>, Receiver<()>) = channel ();
      let (finished, completion) :
        (Sender<(Result<(), String>, usize)>, Receiver<(Result<(), String>, usize)>) = channel ();
      let (coordinator, _) : (MaintenanceCoordinator, String) =
        fixture_maintenance (MaintenancePhase::PreparingArchive);
      let active : ActiveMaintenance = match &coordinator . state {
        CoordinatorState::Active (active) => active . clone (),
        _ => unreachable! (), };
      let mut first : bool = true;
      let owner : Arc<CoordinatorOwner> = Arc::new (
        CoordinatorOwner::with_snapshot_publisher (
          coordinator, Some (fixture_snapshot ()), move |_| {
            if first {
              first = false;
              entered . send (( )) . unwrap ();
              resume . recv () . unwrap ();
              if fail_publication { return Err ("disk full" . into ()); } }
            Ok (( )) }));
      let observing : Arc<CoordinatorOwner> = owner . clone ();
      let observer : thread::JoinHandle<Result<(), String>> = thread::spawn (move ||
        observing . transition (|coordinator| {
          coordinator . defer_ordinary_observation () . expect ("active incident defers");
          Ok (( )) }));
      started . recv_timeout (Duration::from_secs (2)) . unwrap ();
      let archiving : Arc<CoordinatorOwner> = owner . clone ();
      let archive : thread::JoinHandle<()> = thread::spawn (move || {
        let mut attempts : usize = 0;
        let result : Result<(), String> = archiving . transition (|coordinator| {
          attempts += 1;
          prepared . send (( )) . unwrap ();
          coordinator . archive_ready (&active . incident_id, active . epoch, "a" . repeat (64)) });
        finished . send ((result, attempts)) . unwrap (); });
      first_attempt . recv_timeout (Duration::from_secs (2)) . unwrap ();
      assert! (completion . recv_timeout (Duration::from_millis (50)) . is_err (),
        "archive transition must wait for the pending observer publication");
      assert! (reserve_current (&owner, "save") . is_err ());
      assert_eq! (owner . snapshot () . observation_sequence . get (), 0);
      release . send (( )) . unwrap ();
      let observation_result : Result<(), String> = observer . join () . unwrap ();
      let (archive_result, attempts) : (Result<(), String>, usize) =
        completion . recv_timeout (Duration::from_secs (2)) . unwrap ();
      archive . join () . unwrap ();
      if fail_publication {
        assert_eq! (observation_result, Err ("disk full" . into ()));
        assert_eq! (archive_result, Err ("disk full" . into ()));
        assert_eq! (attempts, 1);
        assert! (owner . failure () . is_some ());
      } else {
        observation_result . unwrap ();
        archive_result . unwrap ();
        assert_eq! (attempts, 2);
        assert_eq! (owner . snapshot () . observation_sequence . get (), 1);
        assert! (matches! (owner . snapshot () . state,
          CoordinatorState::Active (active) if active . phase == MaintenancePhase::ArchiveReady)); }
    }
  }

  #[test]
  fn obsolete_proposal_never_reaches_durable_publisher () {
    let directory : tempfile::TempDir = tempdir () . unwrap ();
    let journal : MaintenanceJournalStore = MaintenanceJournalStore::at_root (
      directory . path () . join ("journal"), PathBuf::from ("/config"));
    let owner : CoordinatorOwner = CoordinatorOwner::start (
      MaintenanceCoordinator::new (), journal . clone ());
    let obsolete : MaintenanceCoordinator = owner . snapshot ();
    owner . transition (|coordinator| {
      coordinator . begin (MaintenanceOrigin::Pull, None) . map (|_| ( ))
    }) . unwrap ();
    assert! (owner . propose (0, obsolete) . is_err ());
    assert! (matches! (journal . load () . active . unwrap ()
      . coordinator . state, CoordinatorState::Active (_))); }

  #[test]
  fn journal_failure_cannot_publish_or_acknowledge_proposed_authority () {
    let owner : CoordinatorOwner = CoordinatorOwner::with_publisher (
      MaintenanceCoordinator::new (), |_| Err ("disk full" . into ()));
    assert_eq! (owner . transition (|coordinator| {
      coordinator . observation_started ()
    }), Err ("disk full" . into ()));
    assert_eq! (owner . snapshot () . state, CoordinatorState::Idle); }

  #[test]
  fn slow_publication_keeps_old_authority_readable_and_rejects_late_state () {
    let (entered, started) : (SyncSender<()>, Receiver<()>) = sync_channel (1);
    let (release, resume) : (SyncSender<()>, Receiver<()>) = sync_channel (1);
    let owner : Arc<CoordinatorOwner> = Arc::new (CoordinatorOwner::with_publisher (
      MaintenanceCoordinator::new (), move |_| {
        entered . send (( )) . unwrap ();
        resume . recv () . unwrap ();
        Ok (( )) }));
    let worker_owner : Arc<CoordinatorOwner> = owner . clone ();
    let worker : thread::JoinHandle<Result<(), String>> = thread::spawn (move ||
      worker_owner . transition (|coordinator| coordinator . observation_started ()));
    started . recv () . unwrap ();
    assert_eq! (owner . snapshot () . state, CoordinatorState::Idle);
    assert! (owner . propose (0, MaintenanceCoordinator::new ()) . is_err ());
    release . send (( )) . unwrap ();
    worker . join () . unwrap () . unwrap ();
    assert_eq! (owner . snapshot () . state, CoordinatorState::Observing); }
}
