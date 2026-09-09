use super::*;
use crate::dbs::in_rust_graph::relation_accessors::RelationRole;
use crate::source_sets::ActiveSourceSet;
use crate::to_org::complete::contents::clobberIndefinitiveViewnode;
use crate::to_org::complete::partner_col::goal_list::goal_list_for_hiddenoutsideof_subscribeecol;
use crate::to_org::expand::backpath_graph::{
  PathToFirstNonlinearity, paths_to_first_nonlinearities};
use crate::types::git::SourceDiff;
use crate::types::misc::{MemberAtSource, SkgfileSource, SourceSetName};
use crate::types::nodes::complete::empty_node_complete;
use crate::types::phantom::title_for_phantom;
use crate::types::viewnode::{
  ParentIs, ViewNode, ViewNodeKind, Vognode,
  mk_indefinitive_viewnode, viewforest_root_viewnode};
use crate::update_buffer::viewnodestats::set_viewnodestats_in_viewforest;

use ego_tree::{NodeId, Tree};
use std::collections::{BTreeSet, HashMap, HashSet};
use std::fs;
use std::path::Path;

#[test]
fn absence_in_selected_graph_stays_absent_when_new_files_appear () {
  let dir : tempfile::TempDir = tempfile::tempdir () . unwrap ();
  let config : SkgConfig = config_at (dir . path ());
  let graph : InRustGraph = InRustGraph::from_nodecompletes (&[]);
  fs::write (dir . path () . join ("new.skg"),
             "pid: new\ntitle: newer disk title\n") . unwrap ();
  assert! (nodecomplete_from_in_rust_graph (&graph, &ID::from ("new"))
    . is_none ());
  assert! (nodecomplete_rustFirst_by_pid_and_source (
    &graph, &config, &ID::from ("new"), &SourceName::from ("main"))
    . is_err ()); }

#[test]
fn retained_snapshot_resolves_extra_ids_titles_and_body_from_its_own_nodes () {
  let dir : tempfile::TempDir = tempfile::tempdir () . unwrap ();
  let config : SkgConfig = config_at (dir . path ());
  let mut selected : NodeComplete = node ("n", "selected title");
  selected . extra_ids = vec![ID::from ("old-id")];
  selected . body = Some ("selected body" . to_string ());
  let graph : InRustGraph = InRustGraph::from_nodecompletes (&[selected]);
  fs::write (dir . path () . join ("n.skg"),
             "pid: n\ntitle: newer disk title\n") . unwrap ();
  let mut view : Tree<ViewNode> = Tree::new (viewforest_root_viewnode ());
  let treeid : NodeId = view . root_mut () . append (
    mk_indefinitive_viewnode (
      ID::from ("old-id"), SourceName::from ("old-home"),
      "stale title" . to_string (), ParentIs::Absent)) . id ();
  clobberIndefinitiveViewnode (&graph, &mut view, treeid, &config) . unwrap ();
  set_viewnodestats_in_viewforest (
    &graph, &mut view, &HashMap::new (), &HashMap::new (), &config, None);
  let ViewNodeKind::Vognode (Vognode::Active (rendered)) =
    &view . get (treeid) . unwrap () . value () . kind
    else { panic! ("expected active node"); };
  assert_eq! (rendered . title, "selected title");
  assert_eq! (rendered . source, SourceName::from ("main"));
  assert! (rendered . viewStats . hidden_body);
  fs::remove_file (dir . path () . join ("n.skg")) . unwrap ();
  assert_eq! (nodecomplete_from_in_rust_graph (&graph, &ID::from ("old-id"))
    . unwrap () . body . as_deref (), Some ("selected body")); }

#[test]
fn phantom_title_uses_named_deleted_evidence_then_selected_graph () {
  let config : SkgConfig = config_at (Path::new ("/unused-selected-graph-test"));
  let selected : NodeComplete = node ("n", "selected title");
  let deleted : NodeComplete = node ("n", "recorded deleted title");
  let graph : InRustGraph = InRustGraph::from_nodecompletes (&[selected]);
  let source : SourceName = SourceName::from ("main");
  let id : ID = ID::from ("n");
  let diffs : HashMap<SourceName, SourceDiff> = HashMap::from ([
    (source . clone (), SourceDiff {
      is_git_repo : true,
      staged : HashMap::new (), unstaged : HashMap::new (),
      added_nodes : HashMap::new (),
      deleted_nodes : HashMap::from ([(id . clone (), deleted)]), })]);
  assert_eq! (title_for_phantom (&graph, &id, &source, Some (&diffs), &config),
              "recorded deleted title");
  assert_eq! (title_for_phantom (&graph, &id, &source, None, &config),
              "selected title"); }

#[test]
fn backpaths_use_selected_edges_and_keep_fork_cycle_semantics () {
  let mut a : NodeComplete = node ("a", "a title");
  let mut b : NodeComplete = node ("b", "b title");
  let mut c : NodeComplete = node ("c", "c title");
  a . contains = vec![MemberAtSource::at_source (
    SourceName::from ("main"), ID::from ("c"))];
  b . contains = vec![MemberAtSource::at_source (
    SourceName::from ("main"), ID::from ("a"))];
  c . contains = vec![MemberAtSource::at_source (
    SourceName::from ("main"), ID::from ("b"))];
  let graph : InRustGraph = InRustGraph::from_nodecompletes (&[a, b, c]);
  let paths : Vec<PathToFirstNonlinearity> = paths_to_first_nonlinearities (
    &graph, &ID::from ("a"), RelationRole::CONTAINER, None);
  assert_eq! (paths . len (), 1);
  assert_eq! (paths [0] . path, vec![ID::from ("b"), ID::from ("c")]);
  assert_eq! (paths [0] . cycle_nodes, HashSet::from ([ID::from ("a")]));
  assert! (paths [0] . branches . is_empty ()); }

#[test]
fn hidden_edges_do_not_create_forks_in_a_public_backpath () {
  let mut public : NodeComplete = node ("public", "public title");
  let mut private : NodeComplete = node ("private", "private title");
  public . contains = vec![MemberAtSource::at_source (
    SourceName::from ("main"), ID::from ("target"))];
  private . contains = vec![MemberAtSource::at_source (
    SourceName::from ("private"), ID::from ("target"))];
  let graph : InRustGraph = InRustGraph::from_nodecompletes (
    &[public, private, node ("target", "target title")]);
  let active : ActiveSourceSet = ActiveSourceSet {
    name : SourceSetName::from ("public"),
    sources : BTreeSet::from ([SourceName::from ("main")]), };
  let paths : Vec<PathToFirstNonlinearity> = paths_to_first_nonlinearities (
    &graph, &ID::from ("target"), RelationRole::CONTAINER, Some (&active));
  assert_eq! (paths . len (), 1);
  assert_eq! (paths [0] . path, vec![ID::from ("public")]);
  assert! (paths [0] . branches . is_empty ()); }

#[test]
fn private_contains_does_not_suppress_a_public_hiddenoutside_member () {
  let mut subscribee : NodeComplete = node ("subscribee", "subscribee title");
  subscribee . contains = vec![MemberAtSource::at_source (
    SourceName::from ("private"), ID::from ("hidden"))];
  let graph : InRustGraph = InRustGraph::from_nodecompletes (
    &[subscribee, node ("hidden", "hidden title")]);
  let active : ActiveSourceSet = ActiveSourceSet {
    name : SourceSetName::from ("public"),
    sources : BTreeSet::from ([SourceName::from ("main")]), };
  let goal_at = |active : Option<&ActiveSourceSet>| -> Vec<ID> {
    goal_list_for_hiddenoutsideof_subscribeecol (
      &graph, &ID::from ("subscriber"), &SourceName::from ("main"),
      &[ID::from ("hidden")], &[ID::from ("subscribee")], &None, active) . 0 };
  assert_eq! (goal_at (Some (&active)), vec![ID::from ("hidden")]);
  assert! (goal_at (None) . is_empty ()); }

fn node (
  id    : &str,
  title : &str,
) -> NodeComplete {
  let mut result : NodeComplete = empty_node_complete ();
  result . pid = ID::from (id);
  result . source = SourceName::from ("main");
  result . title = title . to_string ();
  result }

fn config_at (
  path : &Path,
) -> SkgConfig {
  let source : SourceName = SourceName::from ("main");
  SkgConfig::dummyFromSources (HashMap::from ([(source . clone (), SkgfileSource {
    name : source, abbreviation : None, path : path . to_path_buf (),
    user_owns_it : true, })])) }
