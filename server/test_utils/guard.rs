use crate::test_utils::cleanup_test_tantivy_and_typedb_dbs;

use std::path::PathBuf;
use typedb_driver::{TypeDBDriver, Credentials, DriverOptions};

/// Safety net for test database cleanup.
///
/// If the test panics past the normal cleanup call, the Drop impl
/// spawns a new thread (to avoid nested `block_on`) and deletes the
/// TypeDB database and Tantivy folder. Call `disarm` after successful
/// cleanup to suppress the Drop behaviour.
pub struct TestDbGuard {
  db_name: String,
  tantivy_folder: Option<PathBuf>,
  disarmed: bool,
}

impl TestDbGuard {
  pub fn new(
    db_name: &str,
    tantivy_folder: Option<PathBuf>,
  ) -> TestDbGuard {
    TestDbGuard {
      db_name: db_name . to_string(),
      tantivy_folder,
      disarmed: false, }}
  pub fn disarm(
    &mut self
  ) {
    self . disarmed = true; }
}

impl Drop for TestDbGuard {
  fn drop(
    &mut self
  ) {
    if self . disarmed { return; }
    let db_name: String = self . db_name . clone();
    let tantivy_folder: Option<PathBuf> =
      self . tantivy_folder . clone();
    // Spawn a new thread so we can call block_on without nesting.
    let handle: std::thread::JoinHandle<()> =
      std::thread::spawn(move || {
        let rt: tokio::runtime::Runtime =
          match tokio::runtime::Runtime::new() {
            Ok (rt) => rt,
            Err (e) => {
              eprintln!(
                "TestDbGuard: failed to create runtime \
                 for cleanup of '{}': {}", db_name, e);
              return; }, };
        rt . block_on(async {
          let driver_result: Result<TypeDBDriver, _> =
            TypeDBDriver::new(
              crate::consts::TYPEDB_ADDRESS,
              Credentials::new("admin", "password"),
              match DriverOptions::new(false, None) {
                Ok (opts) => opts,
                Err (e) => {
                  eprintln!(
                    "TestDbGuard: failed to create driver options \
                     for cleanup of '{}': {}", db_name, e);
                  return; }, },
            ) . await;
          match driver_result {
            Ok (driver) => {
              if let Err (e) =
                cleanup_test_tantivy_and_typedb_dbs(
                  &db_name,
                  &driver,
                  tantivy_folder . as_deref(),
                ) . await
              { eprintln!(
                  "TestDbGuard: cleanup failed for '{}': {}",
                  db_name, e); }
              else
              { eprintln!(
                  "TestDbGuard: emergency cleanup succeeded \
                   for '{}'", db_name); }},
            Err (e) => {
              eprintln!(
                "TestDbGuard: failed to connect to TypeDB \
                 for cleanup of '{}': {}", db_name, e); }, }; }); });
    // Best-effort: wait for the cleanup thread.
    let _: Result<(), _> = handle . join(); }}
