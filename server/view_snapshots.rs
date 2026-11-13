//! What each open view held at its last exchange with its client, kept
//! beside the logs so that a session can be read back afterwards: the
//! log records each request's header line only, never a buffer.
//!
//! In 'LOGS/views/' ('SkgConfig::logs_dir'), for each view, two files,
//! each overwritten at every exchange, so they hold the last one only:
//! - 'VIEW.sent.org': the view as the server holds it after its last
//!   render (first render, save, or rerender of any cause), with every
//!   metadata atom. That is the text the client was sent, but for a
//!   save refused for releasing hidden text, whose saved buffer is
//!   registered though the client is sent only the refusal.
//! - 'VIEW.received.org': the buffer of the view's last save, as the
//!   client sent it. Its 'folded' and 'bodyFolded' atoms say what the
//!   user had folded, which the client otherwise keeps to itself.
//! VIEW is the view ID as the client names it, made safe for a file
//! name ('file_stem'). A closed view's files stay, for reading after.
//!
//! Off until 'enable' is called; 'main.rs' calls it, tests do not, so
//! a test writes nothing. The cost is one file write per render or
//! save (and for 'sent', one more rendering of the forest to text):
//! measured 2026-11-13 at 1.6 ms, in the unoptimized build, for an
//! 80 KB diff-view buffer whose save took 2.9 s. The folder holds two
//! files per view a run opened; a busy run of Jeff's opened 73.

use crate::org_to_text::viewforest_to_string;
use crate::types::tree::forest::ViewForest;
use crate::types::views_state::ViewforestId;

use std::path::{Path, PathBuf};
use std::sync::OnceLock;

static SNAPSHOT_FOLDER : OnceLock<PathBuf> = OnceLock::new ();

/// Start keeping snapshots in 'logs_dir/views/'. A second call does
/// nothing.
pub fn enable (
  logs_dir : &Path,
) {
  let folder : PathBuf = logs_dir . join ("views");
  match std::fs::create_dir_all (&folder) {
    Ok (()) => { let _ = SNAPSHOT_FOLDER . set (folder); },
    Err (e) => tracing::warn! (
      folder = %folder . display (), error = %e,
      "view snapshots disabled: cannot create their folder"), }}

/// Record 'viewforest' as what the view 'viewforest_id' was last sent.
pub fn record_sent (
  viewforest_id : &ViewforestId,
  viewforest    : &ViewForest,
) {
  let Some (folder) = SNAPSHOT_FOLDER . get () else { return; };
  match viewforest_to_string (viewforest) {
    Ok (text) => write (folder, viewforest_id, "sent", &text),
    Err (e) => tracing::warn! (
      view = %viewforest_id . repr_in_client (), error = %e,
      "view snapshot not written: the view would not render"), }}

/// Record 'buffer' as the view's last saved buffer.
pub fn record_received (
  viewforest_id : &ViewforestId,
  buffer        : &str,
) {
  let Some (folder) = SNAPSHOT_FOLDER . get () else { return; };
  write (folder, viewforest_id, "received", buffer); }

fn write (
  folder        : &Path,
  viewforest_id : &ViewforestId,
  which         : &str,
  text          : &str,
) {
  let name : String = format! (
    "{}.{}.org", file_stem (viewforest_id), which );
  let path : PathBuf = folder . join (&name);
  // Written beside it and renamed over it, so that two connections
  // holding one search view never leave a file half of each.
  let temporary : PathBuf = folder . join ( format! (
    ".{}.{:?}.tmp", name, std::thread::current () . id () ));
  if let Err (e) = std::fs::write (&temporary, text)
    . and_then ( |()| std::fs::rename (&temporary, &path) ) {
    tracing::warn! ( file = %path . display (), error = %e,
                     "view snapshot not written" ); }}

/// The view ID as the client names it, made safe for a file name. A
/// content view's ID (a UUID) is kept as it is. A search view's ID
/// holds its query, which may hold '/', other characters and any case:
/// each character but an ASCII letter, digit, '-' or '_' becomes '_',
/// the result is cut to 100 characters, and '_' and the FNV-1a hash of
/// the whole ID in hex follow, so that queries that read alike, or
/// differ only in case, keep apart.
pub fn file_stem (
  viewforest_id : &ViewforestId,
) -> String {
  let id : String = viewforest_id . repr_in_client ();
  let safe : String = id . chars ()
    . map ( |c| if c . is_ascii_alphanumeric () || c == '-' || c == '_'
                { c } else { '_' } )
    . take (100)
    . collect ();
  match viewforest_id {
    ViewforestId::ContentView (_) if safe == id => safe,
    _ => format! ( "{}_{:016x}", safe, fnv1a (id . as_bytes ()) ), }}

/// The 64-bit FNV-1a hash: stable across runs and Rust versions, which
/// the standard library's hashers do not promise.
fn fnv1a (
  bytes : &[u8],
) -> u64 {
  bytes . iter () . fold ( 0xcbf29ce484222325_u64, |hash, byte|
    ( hash ^ u64::from (*byte) ) . wrapping_mul (0x100000001b3) ) }

#[cfg(test)]
#[path = "../tests/unit/view_snapshots.rs"]
mod tests;
