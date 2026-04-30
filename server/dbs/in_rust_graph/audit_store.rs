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
//!   pid,audited_on,relation,role,in_rust_graph_ids,typedb_ids
//!
//! Fields:
//!   - 'pid': the audited node's primary ID.
//!   - 'audited_on': YYYY-MM-DD (local).
//!   - 'relation': empty for a clean row; one of
//!     'contains', 'subscribes', 'hides_from_its_subscriptions',
//!     'overrides_view_of', 'textlinks_to' for a mismatch row.
//!   - 'role': empty for a clean row; the role 'pid' plays in the
//!     relation (e.g. "container", "subscribee"). See
//!     [[../../typedb/relationships.rs]] 'OUTBOUND_RELATIONSHIP_TYPES'
//!     for the (relation, role_a, role_b) triples.
//!   - 'in_rust_graph_ids': pipe-separated list of IDs (possibly empty) —
//!     what in-Rust graph said the peer set was at audit time.
//!   - 'typedb_ids': same, but what TypeDB said.
//!
//! IDs are UUIDs or simple tokens (no commas, no pipes). If that
//! ever stops being true, this format needs revisiting.
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

/// One pid's in-Rust-graph record: when it was last audited, and any
/// mismatches from that audit. A clean audit has an empty 'mismatches'
/// vec.
#[derive (Debug, Clone)]
pub struct AuditRecord {
  pub pid           : ID,
  pub audited_on    : String, // YYYY-MM-DD
  pub mismatches    : Vec<RecordedMismatch>,
}

/// A single mismatch as serialized to/from the store. Differs from
/// 'Mismatch' only in owning its strings instead of borrowing
/// 'static ones (so it survives a read round-trip).
#[derive (Debug, Clone)]
pub struct RecordedMismatch {
  pub relation   : String,
  pub role       : String,
  pub in_rust_graph_ids : Vec<ID>,
  pub typedb_ids : Vec<ID>,
}

impl AuditRecord {
  /// True iff the last audit found no mismatches.
  pub fn is_clean (&self) -> bool {
    self . mismatches . is_empty () }
}

/// Build a RecordedMismatch from the in-Rust-graph audit output.
pub fn record_from_mismatch (m : &Mismatch) -> RecordedMismatch {
  RecordedMismatch {
    relation   : m . relation . to_string (),
    role       : m . role . to_string (),
    in_rust_graph_ids : m . in_rust_graph . iter () . cloned () . collect (),
    typedb_ids : m . typedb . iter () . cloned () . collect (), } }

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
            pipe_join (&m . in_rust_graph_ids),
            pipe_join (&m . typedb_ids)) ?; } } }
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
  if relation . is_empty () && role . is_empty () {
    // Clean row.
    return Ok ((pid, date, None)); }
  if relation . is_empty () || role . is_empty () {
    return Err (
      "relation and role must either both be empty or both present"
      . to_string ()); }
  Ok ((
    pid, date,
    Some ( RecordedMismatch {
      relation   : relation . to_string (),
      role       : role . to_string (),
      in_rust_graph_ids : pipe_split (parts[4]),
      typedb_ids : pipe_split (parts[5]), } ))) }

fn pipe_join (ids : &[ID]) -> String {
  ids . iter () . map ( |id| id . as_str () )
    . collect::<Vec<&str>> () . join ("|") }

fn pipe_split (s : &str) -> Vec<ID> {
  if s . is_empty () { return Vec::new (); }
  s . split ('|') . map ( |t| ID::new (t) ) . collect () }

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

#[cfg(test)]
mod tests {
  use super::*;
  use tempfile::TempDir;

  fn mk_record (pid : &str, date : &str) -> AuditRecord {
    AuditRecord {
      pid        : ID::new (pid),
      audited_on : date . to_string (),
      mismatches : Vec::new (), } }

  fn mk_mismatched_record (
    pid : &str, date : &str, relation : &str, role : &str,
    mem : &[&str], db : &[&str],
  ) -> AuditRecord {
    AuditRecord {
      pid        : ID::new (pid),
      audited_on : date . to_string (),
      mismatches : vec! [ RecordedMismatch {
        relation   : relation . to_string (),
        role       : role . to_string (),
        in_rust_graph_ids : mem . iter () . map ( |s| ID::new (*s) ) . collect (),
        typedb_ids : db  . iter () . map ( |s| ID::new (*s) ) . collect (), } ], } }

  #[test]
  fn roundtrip_clean_record () {
    let tmp : TempDir = TempDir::new () . unwrap ();
    let records : Vec<AuditRecord> = vec! [
      mk_record ("pid1", "2026-04-21"),
      mk_record ("pid2", "2026-04-21"),
    ];
    save (tmp . path (), &records) . unwrap ();
    let loaded : Vec<AuditRecord> = load (tmp . path ());
    assert_eq! (loaded . len (), 2);
    assert! (loaded . iter () . all ( |r| r . is_clean ()));
    let pids : HashSet<&str> = loaded . iter ()
      . map ( |r| r . pid . as_str () ) . collect ();
    assert! (pids . contains ("pid1"));
    assert! (pids . contains ("pid2")); }

  #[test]
  fn roundtrip_with_mismatches () {
    let tmp : TempDir = TempDir::new () . unwrap ();
    let records : Vec<AuditRecord> = vec! [
      mk_mismatched_record ("p1", "2026-04-21",
        "contains", "container",
        &["x", "y"], &["y"]),
    ];
    save (tmp . path (), &records) . unwrap ();
    let loaded : Vec<AuditRecord> = load (tmp . path ());
    assert_eq! (loaded . len (), 1);
    let rec : &AuditRecord = &loaded[0];
    assert_eq! (rec . pid . as_str (), "p1");
    assert_eq! (rec . mismatches . len (), 1);
    let m : &RecordedMismatch = &rec . mismatches[0];
    assert_eq! (m . relation, "contains");
    assert_eq! (m . role, "container");
    assert_eq! (m . in_rust_graph_ids . len (), 2);
    assert_eq! (m . typedb_ids . len (), 1); }

  #[test]
  fn malformed_rows_are_skipped () {
    let tmp : TempDir = TempDir::new () . unwrap ();
    let path : PathBuf = audit_store_path (tmp . path ());
    fs::write (
      &path,
      "pid1,2026-04-21,,,,\n\
       BAD_ROW_NO_COMMAS\n\
       pid2,2026-04-21,contains,container,x|y,y\n\
      " ) . unwrap ();
    let loaded : Vec<AuditRecord> = load (tmp . path ());
    assert_eq! (loaded . len (), 2); }

  #[test]
  fn pids_needing_audit_filters_correctly () {
    let corpus : HashSet<ID> = ["a", "b", "c"] . iter ()
      . map ( |s| ID::new (*s) ) . collect ();
    let store : Vec<AuditRecord> = vec! [
      mk_record ("a", "2026-04-21"), // audited today
      mk_record ("b", "2026-04-20"), // yesterday
      mk_record ("d", "2026-04-21"), // no longer in corpus
    ];
    let need : Vec<ID> = pids_needing_audit_today (
      &store, &corpus, "2026-04-21");
    let need_set : HashSet<&str> = need . iter ()
      . map ( |id| id . as_str () ) . collect ();
    assert_eq! (need_set . len (), 2);
    assert! (need_set . contains ("b")); // yesterday → needs today
    assert! (need_set . contains ("c")); // never audited → needs today
    assert! (! need_set . contains ("a")); // today → skip
    assert! (! need_set . contains ("d")); // out of corpus → skip
  }

  #[test]
  fn prune_to_corpus_drops_missing () {
    let corpus : HashSet<ID> = ["a", "c"] . iter ()
      . map ( |s| ID::new (*s) ) . collect ();
    let store : Vec<AuditRecord> = vec! [
      mk_record ("a", "2026-04-21"),
      mk_record ("b", "2026-04-21"), // gone from corpus
      mk_record ("c", "2026-04-21"),
    ];
    let pruned : Vec<AuditRecord> = prune_to_corpus (store, &corpus);
    assert_eq! (pruned . len (), 2);
    let pids : HashSet<&str> = pruned . iter ()
      . map ( |r| r . pid . as_str () ) . collect ();
    assert! (pids . contains ("a"));
    assert! (pids . contains ("c"));
    assert! (! pids . contains ("b")); } }
