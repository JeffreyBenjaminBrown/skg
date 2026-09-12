use crate::test_utils::cleanup_test_tantivy;

use std::path::PathBuf;

/// Safety net for test Tantivy cleanup.
///
/// If the test panics past normal cleanup, Drop removes the Tantivy
/// folder. Call `disarm` after successful cleanup.
pub struct TestStoreGuard {
  tantivy_folder: Option<PathBuf>,
  disarmed: bool,
}

impl TestStoreGuard {
  pub fn new(
    _test_name: &str,
    tantivy_folder: Option<PathBuf>,
  ) -> TestStoreGuard {
    TestStoreGuard {
      tantivy_folder,
      disarmed: false, }}

  pub fn disarm(&mut self) {
    self . disarmed = true; }
}

impl Drop for TestStoreGuard {
  fn drop(&mut self) {
    if self . disarmed { return; }
    let tantivy_folder : Option<PathBuf> =
      self . tantivy_folder . clone();
    let handle : std::thread::JoinHandle<()> =
      std::thread::spawn (move || {
        if let Err (e) = cleanup_test_tantivy (tantivy_folder . as_deref ()) {
          tracing::error! (
            "TestStoreGuard: emergency Tantivy cleanup failed: {}", e); } });
    let _ = handle . join (); }
}
