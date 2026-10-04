use super::*;
use crate::types::misc::SkgfileRepo;
use crate::types::nodes::complete::{Graphnode, empty_node_complete};

use std::collections::HashMap;
use std::path::PathBuf;

fn config () -> SkgConfig {
  let repos = HashMap::from ([
    (RepoName::from ("owned"), SkgfileRepo {
      name: RepoName::from ("owned"), abbreviation: None,
      path: PathBuf::from ("owned"), user_owns_it: true }),
    (RepoName::from ("foreign"), SkgfileRepo {
      name: RepoName::from ("foreign"), abbreviation: None,
      path: PathBuf::from ("foreign"), user_owns_it: false }),
  ]);
  SkgConfig::fromReposAndTantivyFolder (repos, "/tmp/none")
}

#[test]
fn state_resolves_extra_ids_and_reports_every_flag_and_ownership () {
  let owned : Graphnode = Graphnode {
    pid       : ID::from ("canonical"),
    extra_ids : vec![ID::from ("alias-id")],
    home_repo    : RepoName::from ("owned"),
    misc      : vec![Flag::NoSearchMatching,
                     Flag::Had_ID_Before_Import],
    .. empty_node_complete () };
  let foreign : Graphnode = Graphnode {
    pid    : ID::from ("foreign-node"),
    home_repo : RepoName::from ("foreign"),
    misc   : vec![Flag::Was_Overloaded],
    .. empty_node_complete () };
  let graph : InRustGraph = InRustGraph::from_graphnodes (&[owned, foreign]);
  let cfg : SkgConfig = config ();
  assert_eq! (flag_state (
    &graph, &cfg, &ID::from ("alias-id"),
    Flag::NoSearchMatching) . unwrap (),
    (ID::from ("canonical"), RepoName::from ("owned"), true, true));
  assert_eq! (flag_state (
    &graph, &cfg, &ID::from ("canonical"),
    Flag::Was_Overloaded) . unwrap () . 2, false);
  assert_eq! (flag_state (
    &graph, &cfg, &ID::from ("foreign-node"),
    Flag::Was_Overloaded) . unwrap (),
    (ID::from ("foreign-node"), RepoName::from ("foreign"), true, false));
  assert! (flag_state (
    &graph, &cfg, &ID::from ("missing"),
    Flag::Had_ID_Before_Import) . is_err ());
}
