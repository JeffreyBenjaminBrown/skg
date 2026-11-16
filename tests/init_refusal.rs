// cargo nextest run --test grouped_unit -E 'test(init_refusal::)'
//
// The init/rebuild refusal wiring ('docs/data-model_technical.org', "Hard
// validation constraints"): the rules of forks, contents and hides are
// validated not only at their own seam (tests/dbs/in_rust_graph/
// override_invariants.rs) but actually block initialization. This
// drives the graph validator shared by server start and rebuild.

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
