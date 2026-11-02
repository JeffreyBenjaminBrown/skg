//! Watch every configured skgrepo directory, owned and foreign, with
//! the platform's file-change notification ('notify': inotify on
//! Linux, which costs nothing while nothing changes). A skgrepo's
//! '.skg' files sit directly in its directory, so each is watched
//! without recursion.
//!
//! Events are debounced: once one arrives, the watcher gathers more
//! until the directories have been quiet for the config's
//! 'repo_watch_quiet_ms', then reloads the batch once
//! ('reload_changed_skg_files') and publishes its notice to every
//! connection. A 'git pull' of a hundred files, or a file written in
//! several steps, is then one batch. While nothing happens, the
//! watcher wakes once a second to watch the directories the current
//! config names, which a rebuild may have changed.

use crate::repo_watch::reload::reload_changed_skg_files;
use crate::types::env::SkgEnv;

use notify::{Event, EventKind, RecommendedWatcher, RecursiveMode, Watcher};
use std::collections::{BTreeSet, HashSet};
use std::panic::{AssertUnwindSafe, catch_unwind};
use std::path::PathBuf;
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::mpsc::{Receiver, RecvTimeoutError, Sender, channel};
use std::thread;
use std::time::Duration;

/// How often an idle watcher checks the config for skgrepo
/// directories to start or stop watching.
const CONFIG_CHECK_INTERVAL : Duration = Duration::from_secs (1);

/// The running watcher. Dropping it stops the watcher within a
/// config check interval.
pub struct RepoWatcherHandle {
  stop : Arc<AtomicBool>,
}

impl Drop for RepoWatcherHandle {
  fn drop (&mut self) {
    self . stop . store (true, Ordering::Release); }}

/// Start watching 'env''s skgrepos on a thread of its own, until the
/// handle returned is dropped. Errs if the platform's watcher cannot
/// be created.
pub fn start_repo_watcher (
  env : SkgEnv,
) -> notify::Result<RepoWatcherHandle> {
  let (sender, receiver) : (Sender<notify::Result<Event>>, Receiver<notify::Result<Event>>) =
    channel ();
  let watcher : RecommendedWatcher = notify::recommended_watcher (
    move |event : notify::Result<Event>| {
      // A stopped watcher's receiver is gone; its events matter to no one.
      let _sent : Result<(), _> = sender . send (event); } ) ?;
  let stop : Arc<AtomicBool> = Arc::new (AtomicBool::new (false));
  let stop_seen_by_thread : Arc<AtomicBool> = stop . clone ();
  thread::Builder::new ()
    . name ( "repo watcher" . to_string () )
    . spawn ( move || watch_and_reload (
      env, watcher, receiver, stop_seen_by_thread ))
    . map_err ( |e| notify::Error::io (e) ) ?;
  Ok ( RepoWatcherHandle { stop } ) }

/// The watcher's loop: wait for an event, gather the batch, reload
/// it, publish its notice.
fn watch_and_reload (
  env      : SkgEnv,
  mut watcher : RecommendedWatcher,
  receiver : Receiver<notify::Result<Event>>,
  stop     : Arc<AtomicBool>,
) {
  let mut watched : WatchedDirectories = WatchedDirectories::default ();
  watch_the_configured_skgrepos (&env, &mut watcher, &mut watched);
  let mut pending : BTreeSet<PathBuf> = BTreeSet::new ();
  loop {
    if stop . load (Ordering::Acquire) { return; }
    match receiver . recv_timeout (CONFIG_CHECK_INTERVAL) {
      Ok (event) => add_changed_paths (&mut pending, event),
      Err (RecvTimeoutError::Timeout) => {
        watch_the_configured_skgrepos (&env, &mut watcher, &mut watched);
        continue; },
      Err (RecvTimeoutError::Disconnected) => return, }
    let quiet : Duration = Duration::from_millis (
      env . runtime_snapshot () . config . repo_watch_quiet_ms );
    loop { // until the directories are quiet
      match receiver . recv_timeout (quiet) {
        Ok (event) => add_changed_paths (&mut pending, event),
        Err (RecvTimeoutError::Timeout) => break,
        Err (RecvTimeoutError::Disconnected) => return, }}
    if stop . load (Ordering::Acquire) { return; }
    match catch_unwind ( AssertUnwindSafe (
      || reload_changed_skg_files (&env, &pending) ))
    { Ok ((notice, retry)) => {
        pending = retry;
        env . runtime . publish_repo_change_notice (notice); },
      Err (_panic) => {
        // Logged by the panic hook; the watcher keeps watching.
        tracing::error! ( "reloading files changed on disk panicked" );
        pending . clear (); }, }
    watch_the_configured_skgrepos (&env, &mut watcher, &mut watched); }}

/// Add the '.skg' paths an event names to 'pending'. Reads (access
/// events) change nothing.
fn add_changed_paths (
  pending : &mut BTreeSet<PathBuf>,
  event   : notify::Result<Event>,
) {
  match event {
    Ok (event) => {
      if matches! (event . kind, EventKind::Access (_)) { return; }
      pending . extend ( event . paths . into_iter ()
                         . filter ( |path| path . extension ()
                                    . is_some_and ( |ext| ext == "skg" )) ); },
    Err (e) => tracing::warn! ( error = %e, "repo watcher error" ), }}

/// Watch each skgrepo directory the current config names that is not
/// yet watched, and stop watching those it no longer names.
fn watch_the_configured_skgrepos (
  env     : &SkgEnv,
  watcher : &mut RecommendedWatcher,
  watched : &mut WatchedDirectories,
) {
  let configured : HashSet<PathBuf> =
    env . runtime_snapshot () . config . skgrepos . values ()
    . map ( |skgrepo| skgrepo . path . clone () )
    . collect ();
  for directory in watched . watched . difference (&configured) . cloned () . collect::<Vec<PathBuf>> () {
    if let Err (e) = watcher . unwatch (&directory) {
      tracing::warn! ( directory = %directory . display (), error = %e,
                       "could not stop watching a skgrepo directory" ); }
    watched . watched . remove (&directory); }
  watched . unwatchable . retain ( |directory| configured . contains (directory) );
  for directory in configured . difference (&watched . watched) . cloned () . collect::<Vec<PathBuf>> () {
    match watcher . watch (&directory, RecursiveMode::NonRecursive) {
      Ok (( )) => {
        tracing::info! ( directory = %directory . display (),
                         "watching a skgrepo directory" );
        watched . unwatchable . remove (&directory);
        watched . watched . insert (directory); },
      Err (e) => {
        // Tried again at each config check; reported once.
        if watched . unwatchable . insert ( directory . clone () ) {
          tracing::warn! ( directory = %directory . display (), error = %e,
                           "could not watch a skgrepo directory" ); }}, }}}

/// The skgrepo directories being watched, and those the watcher could
/// not watch (reported once, tried again at each config check).
#[derive(Default)]
struct WatchedDirectories {
  watched     : HashSet<PathBuf>,
  unwatchable : HashSet<PathBuf>,
}
