// cargo nextest run --test grouped_unit -E 'test(subcommand_usage::)'
//
// The binary answers a subcommand's '--help', or arguments that do not
// fit the subcommand, with its usage ('server/subcommands.rs'), naming
// the binary by a path that runs it as pasted.

use std::fs;
use std::path::PathBuf;
use std::process::{Command, Output};

fn skg_binary_absolutely (
) -> String {
  let binary : PathBuf = fs::canonicalize ( env! ("CARGO_BIN_EXE_skg") ) . unwrap ();
  binary . to_string_lossy () . to_string () }

fn run_skg (
  arguments : &[&str],
) -> Output {
  Command::new ( env! ("CARGO_BIN_EXE_skg") ) . args (arguments) . output () . unwrap () }

#[test]
fn fix_attributions_answers_help_with_its_usage () {
  let output : Output = run_skg (&[ "fix-attributions", "--help" ]);
  assert! ( output . status . success (), "{:?}", output );
  let stdout : String = String::from_utf8 (output . stdout) . unwrap ();
  assert! ( stdout . starts_with ( &format! (
              "Usage: {} fix-attributions CONFIG OLD SKGREPO\n\n",
              skg_binary_absolutely () )),
            "{}", stdout ); }

#[test]
fn arguments_that_do_not_fit_get_the_usage_and_exit_2 () {
  let output : Output = run_skg (&[ "show-forked-original", "CONFIG" ]);
  assert_eq! ( output . status . code (), Some (2), "{:?}", output );
  let stderr : String = String::from_utf8 (output . stderr) . unwrap ();
  assert! ( stderr . starts_with ( &format! (
              "Usage: {} show-forked-original CONFIG FORK_ID\n",
              skg_binary_absolutely () )),
            "{}", stderr ); }

#[test]
fn help_alone_lists_every_subcommand () {
  let output : Output = run_skg (&[ "--help" ]);
  assert! ( output . status . success (), "{:?}", output );
  let stdout : String = String::from_utf8 (output . stdout) . unwrap ();
  for name in [ "check-attributions", "fix-attributions", "show-forked-original",
                "check-config", "export-org", "export-obsidian" ] {
    assert! ( stdout . contains ( &format! ( "{} {} ", skg_binary_absolutely (), name )),
              "{} missing from\n{}", name, stdout ); }}
