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
use skg::types::misc::SkgConfig;
use skg::types::nodes::complete::Graphnode;

#[test]
fn init_refuses_a_self_override
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
        "init must refuse a note that overrides itself"),
      Err (e) => e, };
    assert! (
      msg . contains ("node R forks itself"),
      "the refusal must name the self-fork of R: {}",
      msg );
    Ok (( )) } ) }

/// Owned E contains foreign R, which subscribes to (forks) E. Only a
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
