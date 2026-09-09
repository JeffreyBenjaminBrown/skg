use crate::test_utils::cleanup_test_tantivy;

use std::path::PathBuf;

/// Panic safety net for a disposable test search index.
pub struct TestFixtureGuard {
  tantivy_folder : Option<PathBuf>,
  disarmed       : bool,
}

impl TestFixtureGuard {
  pub fn new (
    tantivy_folder : Option<PathBuf>,
  ) -> TestFixtureGuard {
    TestFixtureGuard { tantivy_folder, disarmed: false } }

  pub fn disarm (&mut self) {
    self . disarmed = true; }
}

impl Drop for TestFixtureGuard {
  fn drop (&mut self) {
    if self . disarmed { return; }
    if let Err (error) = cleanup_test_tantivy (
      self . tantivy_folder . as_deref ())
    { tracing::error! ("test fixture cleanup failed: {}", error); }
  }
}
