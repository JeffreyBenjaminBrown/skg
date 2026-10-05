use super::*;
use crate::types::misc::SkgRepo;
use crate::types::nodes::complete::{Graphnode, empty_graphnode};

use std::collections::HashMap;
use std::path::PathBuf;

fn config () -> SkgConfig {
  let skgrepos = HashMap::from ([
    (SkgRepoName::from ("owned"), SkgRepo {
      name: SkgRepoName::from ("owned"), abbreviation: None,
      path: PathBuf::from ("owned"), owned: true }),
    (SkgRepoName::from ("foreign"), SkgRepo {
      name: SkgRepoName::from ("foreign"), abbreviation: None,
      path: PathBuf::from ("foreign"), owned: false }),
  ]);
  SkgConfig::fromSkgReposAndTantivyFolder (skgrepos, "/tmp/none")
}

#[test]
fn state_resolves_extra_ids_and_reports_every_flag_and_ownership () {
  let owned : Graphnode = Graphnode {
    pid          : ID::from ("canonical"),
    extra_ids    : vec![ID::from ("alias-id")],
    home_skgrepo : SkgRepoName::from ("owned"),
    flags        : vec![Flag::NoSearchMatching,
                     Flag::Had_ID_Before_Import],
    .. empty_graphnode () };
  let foreign : Graphnode = Graphnode {
    pid          : ID::from ("foreign-node"),
    home_skgrepo : SkgRepoName::from ("foreign"),
    flags        : vec![Flag::Was_Overloaded],
    .. empty_graphnode () };
  let graph : InRustGraph = InRustGraph::from_graphnodes (&[owned, foreign]);
  let cfg : SkgConfig = config ();
  assert_eq! (flag_state (
    &graph, &cfg, &ID::from ("alias-id"),
    Flag::NoSearchMatching) . unwrap (),
    (ID::from ("canonical"), SkgRepoName::from ("owned"), true, true));
  assert_eq! (flag_state (
    &graph, &cfg, &ID::from ("canonical"),
    Flag::Was_Overloaded) . unwrap () . 2, false);
  assert_eq! (flag_state (
    &graph, &cfg, &ID::from ("foreign-node"),
    Flag::Was_Overloaded) . unwrap (),
    (ID::from ("foreign-node"), SkgRepoName::from ("foreign"), true, false));
  assert! (flag_state (
    &graph, &cfg, &ID::from ("missing"),
    Flag::Had_ID_Before_Import) . is_err ());
}
