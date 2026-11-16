// cargo nextest run --test grouped_unit -E 'test(init_refusal::)'
//
// The init/rebuild refusal wiring ('docs/data-model_technical.org', "Hard
// validation constraints"): the rules of forks, contents and hides are
// validated not only at their own seam (tests/dbs/in_rust_graph/
// override_invariants.rs) but actually block initialization, unless a
// foreign note's record takes part in the violation, which only warns.
// This drives the graph validator shared by server start and rebuild.

use std::error::Error;

use futures::executor::block_on;
use skg::test_utils::load_config_with_overrides;
use skg::dbs::filesystem::multiple_nodes::read_all_skg_files_from_skgrepos;
use skg::dbs::in_rust_graph::complete_validation::validated_graph;
use skg::dbs::filesystem::not_nodes::load_config;
use skg::types::misc::SkgConfig;
use skg::types::nodes::complete::Graphnode;
use std::fs;
use tempfile::TempDir;

#[test]
fn init_refuses_a_self_fork
  () -> Result<(), Box<dyn Error>> {
  block_on ( async {
    let config : SkgConfig =
      load_config_with_overrides (
        "tests/init_refusal/fixtures/skgconfig.toml",
        Some ("skg-test-init-refusal"),
        &[] ) ?;
    let nodes : Vec<Graphnode> =
      read_all_skg_files_from_skgrepos (&config) ?;
    let result = validated_graph (&config, &nodes);
    let msg : String = match result {
      Ok (_)  => panic! (
        "init must refuse a note that forks itself"),
      Err (e) => e, };
    assert! (
      msg . contains ("node R forks itself"),
      "the refusal must name the self-fork of R: {}",
      msg );
    Ok (( )) } ) }

/// Owned E contains foreign R, which forks E. Only a
/// hand edit of E could mend what R's author did, so the server starts,
/// with a warning naming both notes.
#[test]
fn init_warns_and_starts_when_a_foreign_note_takes_part
  () -> Result<(), Box<dyn Error>> {
  block_on ( async {
    let config : SkgConfig =
      load_config_with_overrides (
        "tests/init_refusal/fixtures-foreign-forker/skgconfig.toml",
        Some ("skg-test-init-refusal-foreign-forker"),
        &[] ) ?;
    let nodes : Vec<Graphnode> =
      read_all_skg_files_from_skgrepos (&config) ?;
    let fork_rule_warnings : Vec<String> =
      match validated_graph (&config, &nodes) {
        Ok ((_graph, _telescope_warnings, fork_rule_warnings)) =>
          fork_rule_warnings,
        Err (e) => panic! (
          "init must not refuse a violation a foreign note takes part in: {}",
          e ), };
    assert_eq! ( fork_rule_warnings . len (), 1,
      "one warning: {:?}", fork_rule_warnings );
    assert! (
      fork_rule_warnings [0] . contains ("node E contains node R, which forks it")
      && fork_rule_warnings [0] . contains ("Node R is foreign"),
      "the warning must name both notes, and R as foreign: {}",
      fork_rule_warnings [0] );
    Ok (( )) } ) }

/// A file holding a key retired in November 2026 is unreadable, in an
/// owned skgrepo or a foreign one ('server/dbs/filesystem/retired_keys.rs'):
/// init refuses, and its report names every such file of each folder,
/// the keys each holds, and what to do, by the skgrepo's owner. A file
/// in today's format beside them is not named.
#[test]
fn init_refuses_retired_keys_naming_every_such_file
  () -> Result<(), Box<dyn Error>> {
  let dir : TempDir = TempDir::new () ?;
  fs::write ( dir . path () . join ("skgconfig.toml"), "\
tantivy_folder = \".index.tantivy\"
port = 1
timing_log = false

[[repos]]
name = \"mine\"
path = \"me/mine\"

[[repos]]
name = \"theirs\"
path = \"them/theirs\"
foreign = true
" ) ?;
  let mine : std::path::PathBuf = dir . path () . join ("me/mine");
  let theirs : std::path::PathBuf = dir . path () . join ("them/theirs");
  fs::create_dir_all (&mine) ?;
  fs::create_dir_all (&theirs) ?;
  fs::write ( mine . join ("a.skg"),
              "pid: a\ntitle: a\nsubscribesTo:\n- c\noverrides:\n- c\n" ) ?;
  fs::write ( mine . join ("b.skg"), "pid: b\ntitle: b\nhidesFromSubs:\n- c\n" ) ?;
  fs::write ( mine . join ("c.skg"), "pid: c\ntitle: c\nforks:\n- t\nhides:\n- a\n" ) ?;
  fs::write ( theirs . join ("t.skg"), "pid: t\ntitle: t\noverrides:\n- a\n" ) ?;
  let (config, _warnings) : (SkgConfig, Vec<String>) =
    load_config ( &dir . path () . join ("skgconfig.toml") . to_string_lossy () ) ?;
  let error : String = match read_all_skg_files_from_skgrepos (&config) {
    Ok (_) => panic! ("init must refuse files holding retired keys"),
    Err (e) => e . to_string (), };
  assert_eq! ( error, "3 unreadable file(s)" );
  let report : String = fs::read_to_string (
    dir . path () . join ("initialization-error_unreadable-skg-files.org") ) ?;
  for expected in [
    &format! ("* {}", mine . join ("a.skg") . display ()),
    "holds 'subscribesTo', 'overrides'; these keys were retired in November 2026: run tools/migrate-forks-2026-11/migrate.py over your skgrepos",
    &format! ("* {}", mine . join ("b.skg") . display ()),
    "holds 'hidesFromSubs';",
    &format! ("* {}", theirs . join ("t.skg") . display ()),
    "holds 'overrides'; these keys were retired in November 2026: this skgrepo's owner should run tools/migrate-forks-2026-11/migrate.py over their skgrepos; until they have, you may remove it from skgconfig.toml",
  ] {
    assert! ( report . contains (expected), "missing {:?}:\n{}", expected, report ); }
  assert! ( ! report . contains ("c.skg"), "{}", report );
  Ok (( )) }
