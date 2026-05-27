//! The on-disk audit store: '.audit.csv' in the data root.
//!
//! Tracks, per pid, the last date the audit visited it and what (if
//! anything) the audit found. Purpose:
//!
//!   - /Resumability within a day/: if the server crashes mid-audit,
//!     the next launch sees which pids were already audited today
//!     and only re-audits the others.
//!   - /Flagged-node list/: pids that had a mismatch last time are
//!     visible in the store, so a future UI can surface them.
//!
//! # Schema
//!
//! CSV, no header. One line per (pid, mismatch) — so a clean audit
//! produces a single row with empty mismatch fields, and a
//! mismatched audit produces one row per mismatch:
//!
//!   pid,audited_on,relation,role,in_rust_graph,typedb
//!
//! Fields:
//!   - 'pid': the audited node's primary ID.
//!   - 'audited_on': YYYY-MM-DD (local).
//!   - 'relation': empty for a clean row; one of
//!     'contains', 'subscribes', 'hides_from_its_subscriptions',
//!     'overrides_view_of', 'textlinks_to' for a relationship
//!     mismatch row, or 'source' for a source mismatch row.
//!   - 'role': empty for a clean row or source mismatch row;
//!     otherwise the role 'pid' plays in the
//!     relation (e.g. "container", "subscribee"). See
//!     [[../../typedb/relationships.rs]] 'OUTBOUND_RELATIONSHIP_TYPES'
//!     for the (relation, role_a, role_b) triples.
//!   - 'in_rust_graph': pipe-separated list of IDs for a relationship
//!     mismatch, or a source name for a source mismatch.
//!   - 'typedb': pipe-separated list of IDs for a relationship
//!     mismatch, or a source-audit result for a source mismatch.
//!
//! IDs and source-audit payloads must not contain commas. Relationship
//! ID sets also must not contain pipes. If that ever stops being true,
//! this format needs revisiting.
//!
//! # Atomicity
//!
//! Writes are via temp-file + rename: the store is rewritten in full
//! per batch commit. At 29k nodes × one row each ≈ 1 MB, trivial.

use std::collections::HashSet;
use std::error::Error;
use std::fs;
use std::io::Write;
use std::path::{Path, PathBuf};

use crate::dbs::in_rust_graph::audit::Mismatch;
use crate::types::misc::ID;

/// A single mismatch as serialized to/from the store. Differs from
/// 'Mismatch' only in owning its strings instead of borrowing
/// 'static ones (so it survives a read round-trip).
#[derive (Debug, Clone)]
pub struct RecordedMismatch {
  pub relation   : String,
  pub role       : String,
  pub in_rust_graph : String,
  pub typedb     : String,
}

/// One pid's in-Rust-graph record: when it was last audited, and any
/// mismatches from that audit. A clean audit has an empty 'mismatches'
/// vec.
#[derive (Debug, Clone)]
pub struct AuditRecord {
  pub pid           : ID,
  pub audited_on    : String, // YYYY-MM-DD
  pub mismatches    : Vec<RecordedMismatch>,
}

impl AuditRecord {
  /// True iff the last audit found no mismatches.
  pub fn is_clean (&self) -> bool {
    self . mismatches . is_empty () }
}

/// Build a RecordedMismatch from the in-Rust-graph audit output.
pub fn record_from_mismatch (m : &Mismatch) -> RecordedMismatch {
  match m {
    Mismatch::Relationship (m) =>
      RecordedMismatch {
        relation      : m . relation . to_string (),
        role          : m . role . to_string (),
        in_rust_graph : pipe_join_hashset (&m . in_rust_graph),
        typedb        : pipe_join_hashset (&m . typedb), },
    Mismatch::Source (m) =>
      RecordedMismatch {
        relation      : "source" . to_string (),
        role          : String::new (),
        in_rust_graph : m . in_rust_graph . to_string (),
        typedb        : m . typedb . to_string (), },
    Mismatch::MissingTypedbNode { .. } =>
      RecordedMismatch {
        relation      : "missing_typedb_node" . to_string (),
        role          : String::new (),
        in_rust_graph : String::new (),
        typedb        : String::new (), },
  } }

/// The path '<data_root>/.audit.csv'.
pub fn audit_store_path (data_root : &Path) -> PathBuf {
  data_root . join (".audit.csv") }

/// Load the store. Missing file → empty result. Malformed lines are
/// logged and skipped rather than aborting startup. Multiple rows
/// for the same pid are grouped into one AuditRecord.
pub fn load (data_root : &Path) -> Vec<AuditRecord> {
  let path : PathBuf = audit_store_path (data_root);
  let text : String = match fs::read_to_string (&path) {
    Ok (t)  => t,
    Err (_) => return Vec::new (), };
  let mut by_pid : std::collections::BTreeMap
    <ID, AuditRecord> = std::collections::BTreeMap::new ();
  for (line_no, line) in text . lines () . enumerate () {
    if line . is_empty () { continue; }
    match parse_row (line) {
      Ok ((pid, date, opt_mismatch)) => {
        let rec : &mut AuditRecord =
          by_pid . entry (pid . clone ()) . or_insert_with (
            || AuditRecord { pid: pid . clone (),
                             audited_on: date . clone (),
                             mismatches: Vec::new () } );
        if rec . audited_on != date {
          // Two rows for the same pid with different dates. Keep
          // the more recent date; it's the authoritative last-audit.
          if date > rec . audited_on {
            rec . audited_on = date; } }
        if let Some (rm) = opt_mismatch {
          rec . mismatches . push (rm); } }
      Err (e) => {
        tracing::warn! (
          path = %path . display (), line = line_no + 1,
          error = %e, content = %line,
          "audit_store: skipping malformed row"); } } }
  by_pid . into_values () . collect () }

/// Atomically rewrite the store from the given records. Uses
/// temp-file + rename so a crash mid-write can't truncate the file.
pub fn save (
  data_root : &Path,
  records   : &[AuditRecord],
) -> Result<(), Box<dyn Error>> {
  let path     : PathBuf = audit_store_path (data_root);
  let tmp_path : PathBuf = path . with_extension ("csv.tmp");
  { let mut f : fs::File = fs::File::create (&tmp_path) ?;
    for rec in records {
      if rec . mismatches . is_empty () {
        // Clean audit: one row with empty mismatch fields.
        writeln! (f, "{},{},,,,",
                  rec . pid, rec . audited_on) ?;
      } else {
        for m in &rec . mismatches {
          writeln! (
            f, "{},{},{},{},{},{}",
            rec . pid, rec . audited_on,
            m . relation, m . role,
            m . in_rust_graph,
            m . typedb) ?; }}}
    f . sync_all () ?; }
  fs::rename (&tmp_path, &path) ?;
  Ok (( )) }

/// Parse one CSV row. Returns (pid, date, optional mismatch).
/// Invariant: pid and date are always present; the last four fields
/// are either all empty (clean) or all present (one mismatch).
fn parse_row (
  line : &str,
) -> Result<(ID, String, Option<RecordedMismatch>), String> {
  let parts : Vec<&str> = line . split (',') . collect ();
  if parts . len () != 6 {
    return Err ( format! (
      "expected 6 comma-separated fields, got {}",
      parts . len () )); }
  let pid : ID = ID::new (parts[0]);
  let date : String = parts[1] . to_string ();
  if date . is_empty () {
    return Err ("audited_on field is empty" . to_string ()); }
  let relation : &str = parts[2];
  let role     : &str = parts[3];
  let in_rust_graph : &str = parts[4];
  let typedb        : &str = parts[5];
  if relation . is_empty ()
     && role . is_empty ()
     && in_rust_graph . is_empty ()
     && typedb . is_empty ()
  { // Clean row.
    return Ok ((pid, date, None)); }
  if relation . is_empty () {
    return Err (
      "relation must be present for mismatch rows"
      . to_string ()); }
  Ok ((
    pid, date,
    Some ( RecordedMismatch {
      relation   : relation . to_string (),
      role       : role . to_string (),
      in_rust_graph : in_rust_graph . to_string (),
      typedb     : typedb . to_string (), } ))) }

fn pipe_join (ids : &[ID]) -> String {
  ids . iter () . map ( |id| id . as_str () )
    . collect::<Vec<&str>> () . join ("|") }

fn pipe_join_hashset (ids : &HashSet<ID>) -> String {
  let mut ids : Vec<ID> =
    ids . iter () . cloned () . collect ();
  ids . sort ();
  pipe_join (&ids) }

/// Given an up-to-date corpus of pids and a store, return the pids
/// that have NOT been audited today — i.e. pids that need auditing.
/// This is the set the scheduled audit loop walks.
///
/// Any pid in the store whose pid is NOT in the corpus is dropped
/// (the node was deleted from disk since its last audit; it can't
/// be audited again).
pub fn pids_needing_audit_today (
  store   : &[AuditRecord],
  corpus  : &HashSet<ID>,
  today   : &str,
) -> Vec<ID> {
  let audited_today : HashSet<&ID> =
    store . iter ()
    . filter ( |rec| rec . audited_on == today
                     && corpus . contains (&rec . pid) )
    . map ( |rec| &rec . pid )
    . collect ();
  corpus . iter ()
    . filter ( |pid| ! audited_today . contains (pid) )
    . cloned () . collect () }

/// Prune records for pids no longer in the corpus. Returns a filtered
/// store (new allocation — cheap at 29k records).
pub fn prune_to_corpus (
  store  : Vec<AuditRecord>,
  corpus : &HashSet<ID>,
) -> Vec<AuditRecord> {
  store . into_iter ()
    . filter ( |rec| corpus . contains (&rec . pid) )
    . collect () }
