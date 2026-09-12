// cargo nextest run --test grouped_unit -E 'test(init_refusal::)'
//
// The init/rebuild refusal wiring
// (TODO/full-schema/13_test-rel-matrix.org): override invariants are
// validated not only at their own seam (tests/dbs/in_rust_graph/
// override_invariants.rs) but actually BLOCK initialization. This
// drives the graph validator shared by server start and rebuild.

use std::error::Error;

use futures::executor::block_on;
use skg::dbs::filesystem::not_nodes::load_config_with_overrides;
use skg::dbs::filesystem::multiple_nodes::read_all_skg_files_from_sources;
use skg::dbs::in_rust_graph::complete_validation::validated_graph;
use skg::types::misc::SkgConfig;
use skg::types::nodes::complete::NodeComplete;

#[test]
fn init_refuses_two_user_owned_overriders
  () -> Result<(), Box<dyn Error>> {
  block_on ( async {
    let config : SkgConfig =
      load_config_with_overrides (
        "tests/init_refusal/fixtures/skgconfig.toml",
        Some ("skg-test-init-refusal"),
        &[] ) ?;
    let nodes : Vec<NodeComplete> =
      read_all_skg_files_from_sources (&config) ?;
    let result = validated_graph (&config, &nodes);
    let msg : String = match result {
      Ok (_)  => panic! (
        "init must refuse a node with two user-owned overriders"),
      Err (e) => e, };
    assert! (
      msg . contains ("overridden by user-owned")
        && msg . contains ("N")
        && msg . contains ("R1")
        && msg . contains ("R2"),
      "the refusal must name the collision (node N, overriders R1/R2): {}",
      msg );
    Ok (( )) } ) }
