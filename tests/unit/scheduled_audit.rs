use super::*;
use tempfile::TempDir;

#[test]
fn date_conversion_known_values () {
  assert_eq! (days_since_epoch_to_date_string (0), "1970-01-01");
  assert_eq! (days_since_epoch_to_date_string (10_957), "2000-01-01");
  assert_eq! (days_since_epoch_to_date_string (11_016), "2000-02-29");
  assert_eq! (days_since_epoch_to_date_string (11_017), "2000-03-01");
  assert_eq! (days_since_epoch_to_date_string (20_453), "2025-12-31");
  assert_eq! (days_since_epoch_to_date_string (20_454), "2026-01-01"); }

#[test]
fn today_heading_detection () {
  let tmp : TempDir = TempDir::new () . unwrap ();
  let path : PathBuf = tmp . path () . join ("audits.org");
  // Missing file: no heading.
  assert! (! today_heading_present (&path, "2026-04-21"));
  fs::write (
    &path,
    "\n* 2026-04-20\n** prior stuff\n\n* 2026-04-21\n** today\n"
  ) . unwrap ();
  assert! (today_heading_present (&path, "2026-04-21"));
  assert! (today_heading_present (&path, "2026-04-20"));
  assert! (! today_heading_present (&path, "2026-04-22")); }

#[test]
fn upsert_replaces_existing () {
  let mut store : Vec<AuditRecord> = vec! [
    AuditRecord { pid : ID::new ("a"),
                  audited_on : "2026-04-20" . to_string (),
                  mismatches : Vec::new () },
  ];
  upsert_record (&mut store, AuditRecord {
    pid : ID::new ("a"),
    audited_on : "2026-04-21" . to_string (),
    mismatches : Vec::new () });
  assert_eq! (store . len (), 1);
  assert_eq! (store[0] . audited_on, "2026-04-21"); }

#[test]
fn upsert_inserts_new () {
  let mut store : Vec<AuditRecord> = Vec::new ();
  upsert_record (&mut store, AuditRecord {
    pid : ID::new ("a"),
    audited_on : "2026-04-21" . to_string (),
    mismatches : Vec::new () });
  assert_eq! (store . len (), 1);
  assert_eq! (store[0] . pid . as_str (), "a"); }

