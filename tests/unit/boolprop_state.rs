use super::*;
use crate::types::misc::SkgfileSource;
use crate::types::nodes::complete::{NodeComplete, empty_node_complete};

use std::collections::HashMap;
use std::path::PathBuf;

fn config () -> SkgConfig {
  let sources = HashMap::from ([
    (SourceName::from ("owned"), SkgfileSource {
      name: SourceName::from ("owned"), abbreviation: None,
      path: PathBuf::from ("owned"), user_owns_it: true }),
    (SourceName::from ("foreign"), SkgfileSource {
      name: SourceName::from ("foreign"), abbreviation: None,
      path: PathBuf::from ("foreign"), user_owns_it: false }),
  ]);
  SkgConfig::fromSourcesAndTantivyFolder (sources, "/tmp/none")
}

#[test]
fn state_resolves_extra_ids_and_reports_every_property_and_ownership () {
  let owned : NodeComplete = NodeComplete {
    pid       : ID::from ("canonical"),
    extra_ids : vec![ID::from ("alias-id")],
    source    : SourceName::from ("owned"),
    misc      : vec![FileProperty::NoSearchMatching,
                     FileProperty::Had_ID_Before_Import],
    .. empty_node_complete () };
  let foreign : NodeComplete = NodeComplete {
    pid    : ID::from ("foreign-node"),
    source : SourceName::from ("foreign"),
    misc   : vec![FileProperty::Was_Overloaded],
    .. empty_node_complete () };
  let graph : InRustGraph = InRustGraph::from_nodecompletes (&[owned, foreign]);
  let cfg : SkgConfig = config ();
  assert_eq! (boolprop_state (
    &graph, &cfg, &ID::from ("alias-id"),
    FileProperty::NoSearchMatching) . unwrap (),
    (ID::from ("canonical"), SourceName::from ("owned"), true, true));
  assert_eq! (boolprop_state (
    &graph, &cfg, &ID::from ("canonical"),
    FileProperty::Was_Overloaded) . unwrap () . 2, false);
  assert_eq! (boolprop_state (
    &graph, &cfg, &ID::from ("foreign-node"),
    FileProperty::Was_Overloaded) . unwrap (),
    (ID::from ("foreign-node"), SourceName::from ("foreign"), true, false));
  assert! (boolprop_state (
    &graph, &cfg, &ID::from ("missing"),
    FileProperty::Had_ID_Before_Import) . is_err ());
}
