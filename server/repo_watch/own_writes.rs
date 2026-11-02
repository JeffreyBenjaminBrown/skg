//! What the server itself last wrote to each '.skg' path: the content
//! hash of a write, or absence for a removal. A save writes files into
//! owned skgrepos, and the repo watcher sees those writes as it sees
//! anyone's; without this record every save would be reloaded a
//! second time. The watcher drops an event whose file now matches the
//! record ('is_own_write_then_forget'), which also forgets the entry.
//!
//! The record is process-wide, keyed by path: every writer of '.skg'
//! files records through 'write_skg_file' and 'remove_skg_file' (or
//! 'record_own_write' when it writes through a handle of its own), so
//! no caller has to carry the runtime to its file writes.

use std::collections::HashMap;
use std::collections::hash_map::DefaultHasher;
use std::fs;
use std::hash::{Hash, Hasher};
use std::io;
use std::path::{Path, PathBuf};
use std::sync::{Mutex, MutexGuard, OnceLock};

/// What the server last left at a path: content with this hash, or
/// no file.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum OwnWrite {
  Content (u64),
  Removed,
}

/// Write 'content' to 'path', recording it as the server's own.
pub fn write_skg_file (
  path    : &Path,
  content : &str,
) -> io::Result<()> {
  record_own_write (path, content . as_bytes ());
  fs::write (path, content) }

/// Remove the file at 'path', recording its absence as the server's
/// own. A file already absent is no error, and leaves no record.
pub fn remove_skg_file (
  path : &Path,
) -> io::Result<()> {
  if ! path . exists () { return Ok (( )); }
  record_own_removal (path);
  match fs::remove_file (path) {
    Ok (( )) => Ok (( )),
    Err (e) if e . kind () == io::ErrorKind::NotFound => Ok (( )),
    Err (e) => Err (e), }}

/// Record that the server is about to write 'content' to 'path'.
pub fn record_own_write (
  path    : &Path,
  content : &[u8],
) {
  record_own_entry (path, OwnWrite::Content (content_hash (content))); }

/// Record that the server is about to remove the file at 'path'.
pub fn record_own_removal (
  path : &Path,
) {
  record_own_entry (path, OwnWrite::Removed); }

/// Whether the file at 'path' is now as the server last left it. If
/// so, the event that reported it is the server's own, and the record
/// forgets the path. A file that differs also clears the entry, since
/// another writer has since changed it.
pub fn is_own_write_then_forget (
  path : &Path,
) -> bool {
  let key : PathBuf = own_write_key (path);
  let mut record : MutexGuard<HashMap<PathBuf, OwnWrite>> = locked_record ();
  let Some (own) : Option<OwnWrite> = record . remove (&key)
    else { return false; };
  let now : Option<OwnWrite> = match fs::read (path) {
    Ok (bytes) => Some ( OwnWrite::Content (content_hash (&bytes)) ),
    Err (e) if e . kind () == io::ErrorKind::NotFound => Some (OwnWrite::Removed),
    Err (_) => None, };
  now == Some (own) }

/// Whether the record holds 'path': a write of the server's own that
/// the watcher has not yet seen.
pub fn own_write_awaits_its_event (
  path : &Path,
) -> bool {
  locked_record () . contains_key ( &own_write_key (path) ) }

fn record_own_entry (
  path : &Path,
  own  : OwnWrite,
) {
  locked_record () . insert ( own_write_key (path), own ); }

/// The path with its directory canonical, so that a write and the
/// watcher's event name one file the same way whatever spelling of
/// the skgrepo path each was given.
fn own_write_key (
  path : &Path,
) -> PathBuf {
  match ( path . parent (), path . file_name () ) {
    (Some (parent), Some (name)) =>
      canonical_directory (parent) . join (name),
    _ => path . to_path_buf (), }}

/// 'directory' canonical, or as given if it cannot be.
pub(crate) fn canonical_directory (
  directory : &Path,
) -> PathBuf {
  fs::canonicalize (directory)
    . unwrap_or_else ( |_| directory . to_path_buf () ) }

fn content_hash (
  content : &[u8],
) -> u64 {
  let mut hasher : DefaultHasher = DefaultHasher::new ();
  content . hash (&mut hasher);
  hasher . finish () }

fn locked_record (
) -> MutexGuard<'static, HashMap<PathBuf, OwnWrite>> {
  static RECORD : OnceLock<Mutex<HashMap<PathBuf, OwnWrite>>> = OnceLock::new ();
  RECORD . get_or_init ( || Mutex::new (HashMap::new ()) )
    . lock ()
    . unwrap_or_else ( |poisoned| poisoned . into_inner () ) }
