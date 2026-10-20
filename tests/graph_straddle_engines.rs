// cargo nextest run --test grouped_repos -E 'test(graph_straddle_engines::)'
//
// The partner-folder reconciler and the role-tree engine, run over the
// union of a graph straddle instead of the live graph. The straddle is built
// in memory: before, c1 and c2 contain r; after, c1 and c3 do, and c2
// is gone.

use skg::dbs::in_rust_graph::InRustGraph;
use skg::dbs::in_rust_graph::relation_accessors::RelationRole;
use skg::dbs::init::create_empty_tantivy_index;
use skg::graph_straddle::types::GraphStraddle;
use skg::to_org::expand::role_tree::{
  RoleTreePartners, build_and_integrate_role_tree_with_skgrepo_set};
use skg::types::env::{RuntimeGeneration, SkgEnv};
use skg::types::git::Sign;
use skg::types::misc::{ID, RelPartner, SkgConfig, Skgrepo, SkgrepoName};
use skg::types::nodes::complete::{Graphnode, empty_graphnode};
use skg::types::viewnode::{
  ChangeFacts, DiffPresence, PartnerFolder, Phantom, Viewnode, ViewnodeKind,
  Vognode, mk_editable_viewnode, viewforest_root_viewnode};
use skg::update_buffer::reconcile::partner_folder::{
  PartnerFolderGoalSource, reconcile_partnerFolder_children};

use ego_tree::{NodeId, NodeRef, Tree};
use std::collections::HashMap;
use std::error::Error;
use std::sync::Arc;
use tempfile::TempDir;

fn node (
  pid      : &str,
  contains : &[&str],
) -> Graphnode {
  Graphnode {
    pid      : ID::from (pid),
    title    : pid . to_string (),
    contains : contains . iter ()
      . map ( |member| RelPartner::at_relRepo (
        SkgrepoName::from ("main"), ID::from (*member) ))
      . collect (),
    .. empty_graphnode () }}

struct Fixture {
  _dir     : TempDir,
  straddle : GraphStraddle,
  runtime  : Arc<RuntimeGeneration>,
}

fn fixture () -> Result<Fixture, Box<dyn Error>> {
  let dir : TempDir = TempDir::new () ?;
  let mut config : SkgConfig = SkgConfig::dummyFromSkgrepos (
    HashMap::from ([ ( SkgrepoName::from ("main"), Skgrepo {
      name : SkgrepoName::from ("main"), abbreviation : None,
      path : dir . path () . join ("main"), owned : true } ) ]));
  config . data_root = dir . path () . to_path_buf ();
  let before : Arc<InRustGraph> = Arc::new ( InRustGraph::from_graphnodes (&[
    node ("r", &[]), node ("c1", &["r"]), node ("c2", &["r"]) ]));
  let after : Arc<InRustGraph> = Arc::new ( InRustGraph::from_graphnodes (&[
    node ("r", &[]), node ("c1", &["r"]), node ("c3", &["r"]) ]));
  let env : SkgEnv = SkgEnv::new (
    config, after . clone (),
    create_empty_tantivy_index (&dir . path () . join ("tantivy")) ? );
  Ok ( Fixture {
    _dir     : dir,
    straddle : GraphStraddle::new (before, after, String::new (), 0),
    runtime  : env . runtime_snapshot (), } ) }

/// A tree holding the editable root r, and its treeid.
fn tree_with_r () -> (Tree<Viewnode>, NodeId) {
  let mut tree : Tree<Viewnode> = Tree::new (viewforest_root_viewnode ());
  let r : NodeId = tree . root_mut () . append ( mk_editable_viewnode (
    ID::from ("r"), SkgrepoName::from ("main"), "r" . into (), None )) . id ();
  (tree, r) }

/// Each child's ID, and its '(diff ...)' presence.
fn children_with_presence (
  node : NodeRef<Viewnode>,
) -> Vec<(String, DiffPresence)> {
  node . children ()
    . filter_map ( |child| {
      let (skgid, facts) : (&ID, &ChangeFacts) = match &child . value () . kind {
        ViewnodeKind::Vognode (Vognode::Unrestricted (t)) =>
          (&t . skgid, &t . change_facts),
        ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Unknown (u))) =>
          (&u . skgid, &u . change_facts),
        ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Historical (h))) =>
          (&h . skgid, &h . change_facts),
        _ => return None, };
      Some (( skgid . 0 . clone (), facts . presence )) } )
    . collect () }

#[test]
fn a_container_folder_from_a_graph_straddle_holds_both_graphs_containers (
) -> Result<(), Box<dyn Error>> {
  let f : Fixture = fixture () ?;
  let (mut tree, r) : (Tree<Viewnode>, NodeId) = tree_with_r ();
  let folder : NodeId = tree . get_mut (r) . unwrap () . append ( Viewnode {
    focused : false, org_folded : false, org_body_folded : false,
    kind : ViewnodeKind::PartnerFolder (PartnerFolder::Container) } ) . id ();
  reconcile_partnerFolder_children (
    folder, &mut tree, PartnerFolder::Container,
    PartnerFolderGoalSource::GraphStraddle (&f . straddle),
    &f . runtime, &f . runtime . graph,
    &HashMap::new (), None, None ) ?;
  let both : DiffPresence = DiffPresence::default ();
  assert_eq! ( children_with_presence (tree . get (folder) . unwrap ()), vec! [
    ( "c1" . to_string (), both ),
    ( "c2" . to_string (), DiffPresence { node : Some (Sign::Minus),
                                          rel  : Some (Sign::Minus) } ),
    ( "c3" . to_string (), DiffPresence { node : Some (Sign::Plus),
                                          rel  : Some (Sign::Plus) } ) ] );
  Ok (( )) }

#[test]
fn the_role_tree_engine_walks_a_graph_straddles_union (
) -> Result<(), Box<dyn Error>> {
  let f : Fixture = fixture () ?;
  let child_ids = |tree : &Tree<Viewnode>, node : NodeId| -> Vec<String> {
    let mut ids : Vec<String> =
      children_with_presence (tree . get (node) . unwrap ()) . into_iter ()
      . map ( |(id, _)| id ) . collect ();
    ids . sort ();
    ids };
  { // Over the live graph: the containers now.
    let (mut tree, r) : (Tree<Viewnode>, NodeId) = tree_with_r ();
    build_and_integrate_role_tree_with_skgrepo_set (
      &mut tree, r, &f . runtime . graph, RelationRole::CONTAINER,
      &f . runtime . config, None, RoleTreePartners::LiveGraph ) ?;
    assert_eq! ( child_ids (&tree, r), vec! [ "c1", "c3" ] ); }
  { // Over the straddle: the containers in either graph.
    let (mut tree, r) : (Tree<Viewnode>, NodeId) = tree_with_r ();
    build_and_integrate_role_tree_with_skgrepo_set (
      &mut tree, r, &f . runtime . graph, RelationRole::CONTAINER,
      &f . runtime . config, None, RoleTreePartners::GraphStraddle (&f . straddle) ) ?;
    assert_eq! ( child_ids (&tree, r), vec! [ "c1", "c2", "c3" ] ); }
  Ok (( )) }
