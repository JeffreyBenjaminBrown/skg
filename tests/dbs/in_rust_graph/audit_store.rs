use skg::dbs::in_rust_graph::audit_store::{
  AuditRecord,
  RecordedMismatch,
  audit_store_path,
  load,
  pids_needing_audit_today,
  prune_to_corpus,
  save,
};
use skg::types::misc::ID;

use std::collections::HashSet;
use std::fs;
use std::path::PathBuf;
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
      in_rust_graph : mem . join ("|"),
      typedb     : db . join ("|"), } ], }}

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
  assert_eq! (m . in_rust_graph, "x|y");
  assert_eq! (m . typedb, "y"); }

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
  assert! (need_set . contains ("b")); // yesterday -> needs today
  assert! (need_set . contains ("c")); // never audited -> needs today
  assert! (! need_set . contains ("a")); // today -> skip
  assert! (! need_set . contains ("d")); // out of corpus -> skip
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
  assert! (! pids . contains ("b")); }
