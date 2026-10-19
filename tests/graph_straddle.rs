// The graph straddle and its union walk, over git fixtures in
// 'tests/graph_straddle/fixtures/'. Each case is a HEAD commit and a
// worktree state per skgrepo.

#[path = "shared/git_fixture_helpers.rs"]
#[allow(unused_imports, dead_code)] // shared by several test targets, each using part of it
mod git_fixture_helpers;

use git_fixture_helpers::{commit_all, setup_gitrepo_with_fixtures};

use skg::dbs::in_rust_graph::InRustGraph;
use skg::dbs::in_rust_graph::relation_accessors::{
  BinaryRolePosition, NodeRelation, RelationRole};
use skg::graph_straddle::build::current_graph_straddle_in_slot;
use skg::graph_straddle::types::{GraphStraddle, Partner, Presence};
use skg::test_utils::graph_handle_from_config;
use skg::types::misc::{ID, SkgConfig, Skgrepo, SkgrepoName};

use git2::Repository;
use std::collections::{BTreeSet, HashMap};
use std::path::{Path, PathBuf};
use std::sync::Arc;
use tempfile::TempDir;

const FIXTURES : &str = "tests/graph_straddle/fixtures";

const CONTENT : RelationRole =
  RelationRole { relation : NodeRelation::Contains,
                 position : BinaryRolePosition::Second };

/// A config over owned skgrepos under 'root', in the given privacy
/// order, plus foreign ones.
fn config_for (
  root          : &Path,
  owned_names   : &[&str],
  foreign_names : &[&str],
) -> SkgConfig {
  let mut skgrepos : HashMap<SkgrepoName, Skgrepo> =
    HashMap::new ();
  for (name, owned) in owned_names . iter () . map ( |n| (n, true) )
    . chain ( foreign_names . iter () . map ( |n| (n, false) )) {
    skgrepos . insert ( SkgrepoName::from (*name), Skgrepo {
      name : SkgrepoName::from (*name),
      abbreviation : None,
      path : root . join (name),
      owned, } ); }
  let mut config : SkgConfig =
    SkgConfig::dummyFromSkgrepos (skgrepos);
  config . skgrepo_order =
    owned_names . iter () . chain (foreign_names)
    . map ( |n| SkgrepoName::from (*n) ) . collect ();
  config }

/// Make one gitrepo per owned skgrepo of the case, at HEAD
/// '<case>/<skgrepo>/head' with '<case>/<skgrepo>/worktree' in the
/// worktree. With 'skgrepo' None the case has one skgrepo, "main",
/// whose fixtures are '<case>/head' and '<case>/worktree'.
fn gitrepos_for_case (
  root     : &Path,
  case     : &str,
  skgrepos : &[Option<&str>],
) -> Vec<Repository> {
  skgrepos . iter () . map ( |skgrepo| {
    let fixtures : PathBuf = match skgrepo {
      Some (name) => Path::new (FIXTURES) . join (case) . join (name),
      None        => Path::new (FIXTURES) . join (case), };
    setup_gitrepo_with_fixtures (
      &root . join ( skgrepo . unwrap_or ("main") ),
      fixtures . join ("head") . to_str () . unwrap (),
      fixtures . join ("worktree") . to_str () . unwrap () )
      . unwrap () } )
    . collect () }

fn live_graph (
  config : &SkgConfig,
) -> Arc<InRustGraph> {
  graph_handle_from_config (config) . unwrap () . load_full () }

fn one_skgrepo_straddle (
  case : &str,
) -> (TempDir, GraphStraddle) {
  let root : TempDir = TempDir::new () . unwrap ();
  gitrepos_for_case (root . path (), case, &[None]);
  let config : SkgConfig = config_for (root . path (), &["main"], &[]);
  let straddle : GraphStraddle =
    GraphStraddle::from_head_and_live (&config, live_graph (&config))
    . unwrap ();
  (root, straddle) }

fn id (s : &str) -> ID { ID::from (s) }

fn ids (
  list : &[&str],
) -> BTreeSet<ID> {
  list . iter () . map ( |s| id (s) ) . collect () }

fn partner (
  partner_id     : &str,
  presence       : Presence,
  relRepo_before : Option<&str>,
  relRepo_after  : Option<&str>,
) -> Partner {
  Partner {
    id             : id (partner_id),
    presence,
    relRepo_before : relRepo_before . map (SkgrepoName::from),
    relRepo_after  : relRepo_after  . map (SkgrepoName::from), } }

#[test]
fn removed_containers_are_reachable_from_the_deleted_node () {
  // D is deleted. K survives without it; K2 is deleted too.
  let (_root, straddle) : (TempDir, GraphStraddle) =
    one_skgrepo_straddle ("deleted_node");
  assert_eq! ( straddle . node_presence (&id ("D")),
               Some (Presence::OnlyBefore) );
  assert_eq! ( straddle . node_presence (&id ("K2")),
               Some (Presence::OnlyBefore) );
  assert_eq! ( straddle . node_presence (&id ("nonexistent")), None );
  assert_eq! (
    straddle . partners (&id ("D"), RelationRole::CONTAINER),
    vec! [ partner ("K",  Presence::OnlyBefore, Some ("main"), None),
           partner ("K2", Presence::OnlyBefore, Some ("main"), None) ] );
  assert_eq! ( // surviving content first, then the removed member
    straddle . partners (&id ("K"), CONTENT),
    vec! [ partner ("E", Presence::Both, Some ("main"), Some ("main")),
           partner ("D", Presence::OnlyBefore, Some ("main"), None) ] );
  assert_eq! ( straddle . changed_pids () . rightward, ids (&["D", "K", "K2"]) );
  assert_eq! ( straddle . changed_pids () . leftward,  ids (&["D"]) ); }

#[test]
fn a_relRepo_only_change_is_reported_from_both_ends () {
  // N's containment of C moves from N's public section to its
  // private one.
  let root : TempDir = TempDir::new () . unwrap ();
  gitrepos_for_case (
    root . path (), "relRepo_move", &[Some ("public"), Some ("private")] );
  let config : SkgConfig =
    config_for (root . path (), &["public", "private"], &[]);
  let straddle : GraphStraddle =
    GraphStraddle::from_head_and_live (&config, live_graph (&config))
    . unwrap ();
  let moved : Partner =
    partner ("C", Presence::Both, Some ("public"), Some ("private"));
  assert_eq! ( straddle . partners (&id ("N"), CONTENT), vec! [moved] );
  assert! ( straddle . partners (&id ("N"), CONTENT) [0] . relRepo_moved () );
  assert_eq! (
    straddle . partners (&id ("C"), RelationRole::CONTAINER),
    vec! [ partner ("N", Presence::Both, Some ("public"), Some ("private")) ] );
  assert! ( straddle . node_changes (&id ("N")) . is_empty () );
  // N's file records the move; C's set of containers is unchanged.
  assert_eq! ( straddle . changed_pids () . rightward, ids (&["N"]) );
  assert_eq! ( straddle . changed_pids () . leftward,  ids (&[]) ); }

#[test]
fn changed_pids_splits_rightward_and_leftward () {
  // A gains content Y; X's title changes; New is created; O only
  // reorders its content.
  let (_root, straddle) : (TempDir, GraphStraddle) =
    one_skgrepo_straddle ("rightward_and_leftward");
  assert_eq! ( straddle . changed_pids () . rightward,
               ids (&["A", "New", "X"]) );
  assert_eq! ( straddle . changed_pids () . leftward, ids (&["Y"]) );
  assert_eq! ( straddle . changed_pids () . all (),
               ids (&["A", "New", "X", "Y"]) );
  assert! ( straddle . node_changes (&id ("X")) . title_changed );
  assert! ( ! straddle . node_changes (&id ("X")) . body_changed );
  assert_eq! ( straddle . node_presence (&id ("New")),
               Some (Presence::OnlyAfter) ); }

#[test]
fn a_duplicate_id_at_head_refuses () {
  let root : TempDir = TempDir::new () . unwrap ();
  gitrepos_for_case (root . path (), "duplicate_id_at_head", &[None]);
  let config : SkgConfig = config_for (root . path (), &["main"], &[]);
  let refusal : String =
    GraphStraddle::from_head_and_live (&config, live_graph (&config))
    . err () . expect ("a duplicate ID at HEAD should refuse");
  assert! ( refusal . contains ("duplicate extra id 'dup'"),
            "unexpected refusal: {}", refusal ); }

#[test]
fn a_foreign_skgrepo_needs_no_gitrepo_and_contributes_no_change () {
  // Owned M contains foreign F; the foreign skgrepo is a plain folder.
  let root : TempDir = TempDir::new () . unwrap ();
  gitrepos_for_case (root . path (), "foreign_skgrepo", &[Some ("main")]);
  git_fixture_helpers::copy_dir_all (
    &Path::new (FIXTURES) . join ("foreign_skgrepo/foreign"),
    &root . path () . join ("foreign") ) . unwrap ();
  let config : SkgConfig =
    config_for (root . path (), &["main"], &["foreign"]);
  let straddle : GraphStraddle =
    GraphStraddle::from_head_and_live (&config, live_graph (&config))
    . unwrap ();
  assert_eq! ( straddle . node_presence (&id ("F")), Some (Presence::Both) );
  assert_eq! ( straddle . changed_pids () . all (), ids (&[]) ); }

#[test]
fn the_slot_reuses_repairs_or_rebuilds_the_straddle () {
  let root : TempDir = TempDir::new () . unwrap ();
  let gitrepos : Vec<Repository> =
    gitrepos_for_case (root . path (), "rightward_and_leftward", &[None]);
  let config : SkgConfig = config_for (root . path (), &["main"], &[]);
  let mut slot : Option<Arc<GraphStraddle>> = None;
  let live : Arc<InRustGraph> = live_graph (&config);
  let built : Arc<GraphStraddle> =
    current_graph_straddle_in_slot (&mut slot, &config, live . clone ())
    . unwrap ();
  let reused : Arc<GraphStraddle> =
    current_graph_straddle_in_slot (&mut slot, &config, live . clone ())
    . unwrap ();
  assert! ( Arc::ptr_eq (&built, &reused) );
  let repaired : Arc<GraphStraddle> = // a save swaps in a new live graph
    current_graph_straddle_in_slot (&mut slot, &config, live_graph (&config))
    . unwrap ();
  assert! ( ! Arc::ptr_eq (&built, &repaired) );
  assert! ( Arc::ptr_eq (&built . before, &repaired . before) );
  commit_all (&gitrepos [0], "commit the worktree");
  let rebuilt : Arc<GraphStraddle> =
    current_graph_straddle_in_slot (&mut slot, &config, live . clone ())
    . unwrap ();
  assert! ( ! Arc::ptr_eq (&built . before, &rebuilt . before) );
  assert_eq! ( rebuilt . changed_pids () . all (), ids (&[]) ); }
