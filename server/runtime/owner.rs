//! The process owner orders durable state publication. Preparation operates on
//! copies; only the event loop accepts a proposal against its named base.

use crate::maintenance::coordinator::MaintenanceCoordinator;
use crate::maintenance::journal::MaintenanceJournalStore;

use arc_swap::ArcSwap;
use std::sync::{Arc, Mutex, MutexGuard};
use std::sync::mpsc::{Receiver, Sender, SyncSender, channel, sync_channel};
use std::thread;

#[derive(Clone)]
struct PublishedCoordinator {
  revision : u64,
  coordinator : MaintenanceCoordinator,
  failure : Option<String>, }

struct Proposal {
  base_revision : u64,
  coordinator : MaintenanceCoordinator,
  reply : SyncSender<Result<(), String>>, }

enum Message {
  Propose (Proposal),
  Published (Result<(), String>),
  Stop, }

pub(crate) struct CoordinatorOwner {
  published : Arc<ArcSwap<PublishedCoordinator>>,
  sender : Sender<Message>,
  preparation : Mutex<()>, }

impl CoordinatorOwner {
  pub(crate) fn start (
    coordinator : MaintenanceCoordinator,
    journal : MaintenanceJournalStore,
  ) -> Self {
    Self::with_publisher (coordinator, move |next|
      journal . persist (next) . map (|_| ( ))) }

  fn with_publisher (
    coordinator : MaintenanceCoordinator,
    mut persist : impl FnMut (&MaintenanceCoordinator) -> Result<(), String>
      + Send + 'static,
  ) -> Self {
    let published : Arc<ArcSwap<PublishedCoordinator>> =
      Arc::new (ArcSwap::from_pointee (PublishedCoordinator {
        revision: 0, coordinator, failure: None }));
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

  pub(crate) fn failure (&self) -> Option<String> {
    self . published . load () . failure . clone () }

  /// The adapter serializes preparation by callers while they are migrated
  /// to typed operation messages. Its closure can propose state, never publish
  /// it or perform an external effect. Journal I/O runs on the ordered worker.
  pub(crate) fn transition<T> (
    &self,
    transition : impl FnOnce (&mut MaintenanceCoordinator) -> Result<T, String>,
  ) -> Result<T, String> {
    let _preparation : MutexGuard<'_, ()> = self . admission_guard ()?;
    let base : Arc<PublishedCoordinator> = self . published . load_full ();
    if let Some (reason) = &base . failure { return Err (reason . clone ()); }
    let mut coordinator : MaintenanceCoordinator = base . coordinator . clone ();
    let result : T = transition (&mut coordinator)?;
    if coordinator == base . coordinator { return Ok (result); }
    self . propose (base . revision, coordinator)?;
    Ok (result) }

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

fn run_owner (
  published : Arc<ArcSwap<PublishedCoordinator>>,
  receiver : Receiver<Message>,
  publisher : Sender<MaintenanceCoordinator>,
) {
  let mut pending : Option<Proposal> = None;
  while let Ok (message) = receiver . recv () {
    match message {
      Message::Propose (proposal) => {
        if let Some (reason) = &published . load () . failure {
          let _ : Result<(), _> = proposal . reply . send (Err (reason . clone ()));
          continue; }
        if pending . is_some ()
        || proposal . base_revision != published . load () . revision
        {
          let _ : Result<(), _> = proposal . reply . send (Err (
            "coordinator proposal has an obsolete publication base" . into ()));
          continue; }
        if publisher . send (proposal . coordinator . clone ()) . is_err () {
          let _ : Result<(), _> = proposal . reply . send (Err (
            "journal publisher stopped; no durable success" . into ()));
          continue; }
        pending = Some (proposal); }
      Message::Published (result) => {
        let Some (proposal) : Option<Proposal> = pending . take () else {
          continue; };
        if result . is_ok () {
          published . store (Arc::new (PublishedCoordinator {
            revision: proposal . base_revision + 1,
            coordinator: proposal . coordinator, failure: None }));
        } else if let Err (reason) = &result {
          // An I/O error may follow rename. Keep the old readable state, but
          // require recovery before any subsequent authority-changing action.
          let mut blocked : PublishedCoordinator = (**published . load ()) . clone ();
          blocked . failure = Some (format! (
            "journal publication failed; recovery required: {}", reason));
          published . store (Arc::new (blocked)); }
        let _ : Result<(), _> = proposal . reply . send (result); }
      Message::Stop => break, } } }

#[cfg(test)]
mod tests {
  use super::*;
  use crate::maintenance::types::{CoordinatorState, MaintenanceOrigin};
  use std::path::PathBuf;
  use tempfile::tempdir;

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
