// The graph straddle and its union walk, over git fixtures in
// 'tests/graph_straddle/fixtures/'. Each case is a HEAD commit and a
// worktree state per skgrepo.

use crate::git_fixture::{Fixture, OwnedSkgrepos, git_fixture, graph_straddle};
use crate::git_fixture_helpers::{commit_all, setup_gitrepo_with_fixtures};

use skg::dbs::in_rust_graph::InRustGraph;
use skg::dbs::in_rust_graph::relation_accessors::{
  BinaryRolePosition, NodeRelation, RelationRole};
use skg::dbs::filesystem::not_nodes::load_config;
use skg::dbs::init::create_empty_tantivy_index;
use skg::graph_straddle::build::current_graph_straddle_in_slot;
use skg::graph_straddle::types::{GraphStraddle, Partner, Presence};
use skg::serve::ViewsState;
use skg::serve::handlers::rebuild_session_lifetime_stores::rebuild_session_lifetime_stores_in_place;
use skg::test_utils::{copy_dir_all, graph_handle_from_config, skg_env_from_parts};
use skg::types::env::{RuntimeGeneration, SkgEnv};
use skg::types::misc::{ID, SkgConfig, SkgrepoName, TantivyIndex};
use skg::types::views_state::OpenViews;

use git2::Repository;
use std::collections::BTreeSet;
use std::fs;
use std::path::Path;
use std::sync::Arc;
use tempfile::TempDir;

const FIXTURES : &str = "tests/graph_straddle/fixtures";

const CONTENT : RelationRole =
  RelationRole { relation : NodeRelation::Contains,
                 position : BinaryRolePosition::Second };

/// The fixture of the case whose folder is 'FIXTURES/<case>'.
fn case_fixture (
  case    : &str,
  owned   : OwnedSkgrepos,
  foreign : &[&str],
) -> Fixture {
  git_fixture ( &Path::new (FIXTURES) . join (case), owned, foreign )
    . unwrap () }

/// The fixture of a case with one skgrepo, main, and its straddle.
fn one_skgrepo_straddle (
  case : &str,
) -> (Fixture, GraphStraddle) {
  let f : Fixture = case_fixture (case, OwnedSkgrepos::OnlyMain, &[]);
  let straddle : GraphStraddle = graph_straddle (&f) . unwrap ();
  (f, straddle) }

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
  let (_f, straddle) : (Fixture, GraphStraddle) =
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
  assert_eq! ( straddle . changed_pids () . out_changed, ids (&["D", "K", "K2"]) );
  assert_eq! ( straddle . changed_pids () . in_changed,   ids (&["D"]) ); }

#[test]
fn a_relRepo_only_change_is_reported_from_both_ends () {
  // N's containment of C moves from N's public section to its
  // private one.
  let f : Fixture = case_fixture (
    "relRepo_move", OwnedSkgrepos::Named (&["public", "private"]), &[] );
  let straddle : GraphStraddle = graph_straddle (&f) . unwrap ();
  let moved : Partner =
    partner ("C", Presence::Both, Some ("public"), Some ("private"));
  assert_eq! ( straddle . partners (&id ("N"), CONTENT), vec! [moved] );
  assert! ( straddle . partners (&id ("N"), CONTENT) [0] . relRepo_moved () );
  assert_eq! (
    straddle . partners (&id ("C"), RelationRole::CONTAINER),
    vec! [ partner ("N", Presence::Both, Some ("public"), Some ("private")) ] );
  assert! ( straddle . node_changes (&id ("N")) . is_empty () );
  // N's file records the move; C's set of containers is unchanged.
  assert_eq! ( straddle . changed_pids () . out_changed, ids (&["N"]) );
  assert_eq! ( straddle . changed_pids () . in_changed,   ids (&[]) ); }

#[test]
fn changed_pids_splits_out_changed_and_in_changed () {
  // A gains content Y; X's title changes; New is created; O only
  // reorders its content.
  let (_f, straddle) : (Fixture, GraphStraddle) =
    one_skgrepo_straddle ("out_changed_and_in_changed");
  assert_eq! ( straddle . changed_pids () . out_changed,
               ids (&["A", "New", "X"]) );
  assert_eq! ( straddle . changed_pids () . in_changed, ids (&["Y"]) );
  assert_eq! ( straddle . changed_pids () . all (),
               ids (&["A", "New", "X", "Y"]) );
  assert! ( straddle . node_changes (&id ("X")) . title_changed );
  assert! ( ! straddle . node_changes (&id ("X")) . body_changed );
  assert_eq! ( straddle . node_presence (&id ("New")),
               Some (Presence::OnlyAfter) ); }

#[test]
fn a_duplicate_id_at_head_refuses () {
  let f : Fixture =
    case_fixture ("duplicate_id_at_head", OwnedSkgrepos::OnlyMain, &[]);
  let refusal : String = graph_straddle (&f) . err ()
    . expect ("a duplicate ID at HEAD should refuse") . to_string ();
  assert! ( refusal . contains ("duplicate extra id 'dup'"),
            "unexpected refusal: {}", refusal ); }

#[test]
fn a_foreign_skgrepo_needs_no_gitrepo_and_contributes_no_change () {
  // Owned M contains foreign F; the foreign skgrepo is a plain folder.
  let f : Fixture = case_fixture (
    "foreign_skgrepo", OwnedSkgrepos::Named (&["main"]), &["foreign"] );
  let straddle : GraphStraddle = graph_straddle (&f) . unwrap ();
  assert_eq! ( straddle . node_presence (&id ("F")), Some (Presence::Both) );
  assert_eq! ( straddle . changed_pids () . all (), ids (&[]) ); }

#[test]
fn the_slot_reuses_repairs_or_rebuilds_the_straddle () {
  let f : Fixture =
    case_fixture ("out_changed_and_in_changed", OwnedSkgrepos::OnlyMain, &[]);
  let config : &SkgConfig = &f . config;
  let mut slot : Option<Arc<GraphStraddle>> = None;
  let live : Arc<InRustGraph> = f . graph . load_full ();
  let built : Arc<GraphStraddle> =
    current_graph_straddle_in_slot (&mut slot, config, live . clone ())
    . unwrap ();
  let reused : Arc<GraphStraddle> =
    current_graph_straddle_in_slot (&mut slot, config, live . clone ())
    . unwrap ();
  assert! ( Arc::ptr_eq (&built, &reused) );
  let repaired : Arc<GraphStraddle> = // a save swaps in a new live graph
    current_graph_straddle_in_slot (
      &mut slot, config, graph_handle_from_config (config) . unwrap () . load_full () )
    . unwrap ();
  assert! ( ! Arc::ptr_eq (&built, &repaired) );
  assert! ( Arc::ptr_eq (&built . before, &repaired . before) );
  commit_all ( &Repository::open ( f . root . path () . join ("main") ) . unwrap (),
               "commit the worktree" );
  let rebuilt : Arc<GraphStraddle> =
    current_graph_straddle_in_slot (&mut slot, config, live . clone ())
    . unwrap ();
  assert! ( ! Arc::ptr_eq (&built . before, &rebuilt . before) );
  assert_eq! ( rebuilt . changed_pids () . all (), ids (&[]) ); }

/// A rebuild of the session-lifetime stores drops the connection's
/// straddle, and the next one reads both halves from the disk the
/// rebuilt live graph was read from: a foreign file edited since the
/// straddle was built changes neither, so no foreign node is in C.
#[test]
fn a_rebuild_drops_the_straddle_and_the_next_reads_foreign_files_afresh (
) -> Result<(), Box<dyn std::error::Error>> {
  let root : TempDir = TempDir::new () ?;
  setup_gitrepo_with_fixtures (
    &root . path () . join ("owned/main"),
    &format! ("{}/foreign_skgrepo/main/head", FIXTURES),
    &format! ("{}/foreign_skgrepo/main/worktree", FIXTURES) ) ?;
  copy_dir_all (
    &Path::new (FIXTURES) . join ("foreign_skgrepo/foreign"),
    &root . path () . join ("foreign") ) ?;
  fs::write ( root . path () . join ("skgconfig.toml"), concat! (
    "tantivy_folder = \"tantivy\"\n",
    "port = 1730\n",
    "beep_when_server_becomes_available = false\n",
    "[[repos]]\nname = \"main\"\npath = \"owned/main\"\n",
    "[[repos]]\nname = \"foreign\"\npath = \"foreign\"\n" )) ?;
  let config : SkgConfig = load_config (
    root . path () . join ("skgconfig.toml") . to_str () . unwrap () ) ? . 0;
  let tantivy : TantivyIndex =
    create_empty_tantivy_index (&config . tantivy_folder) ?;
  let mut env : SkgEnv = skg_env_from_parts (
    &config, &tantivy, &graph_handle_from_config (&config) ? );
  let mut views_state : ViewsState = ViewsState {
    open_views     : OpenViews::new (),
    graph_straddle : None, };
  let built : Arc<GraphStraddle> = current_graph_straddle_in_slot (
    &mut views_state . graph_straddle, &config,
    env . runtime_snapshot () . graph . clone () ) ?;
  assert_eq! ( built . changed_pids () . all (), ids (&[]) );
  fs::write ( root . path () . join ("foreign/F.skg"),
              "pid: F\ntitle: F, edited\n" ) ?;
  rebuild_session_lifetime_stores_in_place (&mut env, &mut views_state) ?;
  assert! ( views_state . graph_straddle . is_none () );
  let runtime : Arc<RuntimeGeneration> = env . runtime_snapshot ();
  let rebuilt : Arc<GraphStraddle> = current_graph_straddle_in_slot (
    &mut views_state . graph_straddle, &runtime . config,
    runtime . graph . clone () ) ?;
  assert_eq! ( rebuilt . changed_pids () . all (), ids (&[]),
               "a foreign file edited before the rebuild is in neither half" );
  Ok (( )) }

#[test]
fn a_nodeMerge_since_head_puts_the_acquirers_container_in_c () {
  // a acquired b since HEAD. P, untouched on disk, still lists b,
  // which the after-graph resolves to a: P is equal in both graphs
  // and still records a changed relationship.
  let (_f, straddle) : (Fixture, GraphStraddle) =
    one_skgrepo_straddle ("nodeMerge_container");
  assert_eq! ( straddle . before . get (&id ("P")),
               straddle . after  . get (&id ("P")) );
  assert_eq! (
    straddle . partners (&id ("P"), CONTENT),
    vec! [ partner ("a", Presence::OnlyAfter,  None, Some ("main")),
           partner ("b", Presence::OnlyBefore, Some ("main"), None) ] );
  assert_eq! ( straddle . change_leaves () . leaves_under (&id ("P")) . len (), 2 );
  assert_eq! ( straddle . changed_pids () . out_changed, ids (&["P", "a", "b"]) );
  assert_eq! ( straddle . changed_pids () . in_changed,  ids (&["a", "b"]) ); }

#[test]
fn a_newly_mentioned_node_is_in_changed () {
  // M's title gains a link to T. A mention is no leaf, but T's set of
  // mentioners changed.
  let (_f, straddle) : (Fixture, GraphStraddle) =
    one_skgrepo_straddle ("mention");
  assert_eq! ( straddle . changed_pids () . out_changed, ids (&["M"]) );
  assert_eq! ( straddle . changed_pids () . in_changed,  ids (&["T"]) ); }
