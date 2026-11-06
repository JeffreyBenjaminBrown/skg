// Unit tests for where the server's run folder sits.

use super::*;

use std::fs;
use std::os::unix::fs::symlink;
use tempfile::TempDir;

/// The run folder sits beside the config path as given: a symlinked
/// config's folder is beside the symlink, and its target's beside the
/// target, so a server started through the symlink shares its folder
/// with the start script, which names the symlink.
#[test]
fn run_folder_sits_beside_the_path_as_given () {
  let dir : TempDir = TempDir::new () . unwrap ();
  let target : PathBuf = dir . path () . join ("real.toml");
  fs::write ( &target, "" ) . unwrap ();
  let link : PathBuf = dir . path () . join ("skgconfig.toml");
  symlink ( "real.toml", &link ) . unwrap ();
  assert_eq! ( run_folder_beside (&link),
               dir . path () . join ("skgconfig.toml.running") );
  assert_eq! ( run_folder_beside (&target),
               dir . path () . join ("real.toml.running") );
  assert_eq! ( run_folder_beside ( Path::new ("data/skgconfig.toml") ),
               PathBuf::from ("data/skgconfig.toml.running") ); }
