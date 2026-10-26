// cargo nextest run --test grouped_repos -E 'test(graph_straddle_engines::)'
//
// A partner folder refreshed in diff-view mode, and the containerward
// chain, over the union of a graph straddle instead of the live graph. The straddle is built
// in memory: before, c1 and c2 contain r; after, c1 and c3 do, and c2
// is gone.

use skg::dbs::in_rust_graph::InRustGraph;
use skg::dbs::init::create_empty_tantivy_index;
use skg::graph_straddle::types::GraphStraddle;
use skg::to_org::extend::containerward_chain::{
  ContainerwardChainPartners, attach_containerward_chains_under_members};
use skg::types::env::{RuntimeGeneration, SkgEnv};
use skg::types::misc::{ID, RelPartner, SkgConfig, Skgrepo, SkgrepoName};
use skg::types::nodes::complete::{Graphnode, empty_graphnode};
use skg::types::viewnode::{
  DiffChange, Existence, Folder, Viewnode, ViewnodeKind,
  Vognode, mk_editable_viewnode, viewforest_root_viewnode, viewnode_of};
use skg::types::views_state::DiffViewBufferKind;
use skg::diff_view::readiness_store::ReadinessStore;
use skg::refresh::{Truth, refresh_viewforest};
use skg::refresh::corrections::BufferCorrectionPolicy;
use skg::types::tree::forest::ViewForest;
use skg::types::views_state::ViewnodeIdRecord;
use skg::update_buffer::completion_mode::CompletionMode;

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

/// Each child's ID, and the change its '(diff ...)' shows.
fn children_with_change (
  node : NodeRef<Viewnode>,
) -> Vec<(String, DiffChange)> {
  node . children ()
    . filter_map ( |child| {
      let gnid : &ID = match &child . value () . kind {
        ViewnodeKind::Vognode (Vognode::Occurrence (t)) => &t . gnid,
        ViewnodeKind::Vognode (Vognode::Unknown (u)) => &u . gnid,
        ViewnodeKind::Vognode (Vognode::Ghost (h)) => &h . gnid,
        _ => return None, };
      Some (( gnid . 0 . clone (), child . value () . change_facts . change )) } )
    . collect () }

#[test]
fn a_container_folder_from_a_graph_straddle_holds_both_graphs_containers (
) -> Result<(), Box<dyn Error>> {
  let f : Fixture = fixture () ?;
  let mut forest : ViewForest = ViewForest::new ();
  let r : NodeId = forest . append_root ( mk_editable_viewnode (
    ID::from ("r"), SkgrepoName::from ("main"), "r" . into (), None ));
  let folder : NodeId =
    forest . as_internal_tree_mut () . get_mut (r) . unwrap ()
    . append ( viewnode_of (
      ViewnodeKind::Folder (Folder::Container) ))
    . id ();
  refresh_viewforest (
    &mut forest, &mut ViewnodeIdRecord::default (),
    &Truth { graph       : &f . runtime . graph,
             config      : &f . runtime . config,
             restriction : None },
    CompletionMode::DiffView { straddle    : &f . straddle,
                               buffer_kind : DiffViewBufferKind::AllChanges },
    100, Some (&ReadinessStore::default ()),
    &BufferCorrectionPolicy::default () ) ?;
  let both : DiffChange = DiffChange::default ();
  assert_eq! ( children_with_change (forest . get (folder) . unwrap ()), vec! [
    ( "c1" . to_string (), both ),
    ( "c2" . to_string (), DiffChange { node : Some (Existence::Removed),
                                        rel  : Some (Existence::Removed),
                                        properties : false } ),
    ( "c3" . to_string (), DiffChange { node : Some (Existence::Added),
                                        rel  : Some (Existence::Added),
                                        properties : false } ) ] );
  Ok (( )) }

#[test]
fn the_containerward_chain_walks_a_graph_straddles_union (
) -> Result<(), Box<dyn Error>> {
  let f : Fixture = fixture () ?;
  let child_ids = |tree : &Tree<Viewnode>, node : NodeId| -> Vec<String> {
    let mut ids : Vec<String> =
      children_with_change (tree . get (node) . unwrap ()) . into_iter ()
      . map ( |(id, _)| id ) . collect ();
    ids . sort ();
    ids };
  { // Over the live graph: the containers now.
    let (mut tree, r) : (Tree<Viewnode>, NodeId) = tree_with_r ();
    attach_containerward_chains_under_members (
      &mut tree, &[r], &f . runtime . graph,
      &f . runtime . config, None, ContainerwardChainPartners::LiveGraph ) ?;
    assert_eq! ( child_ids (&tree, r), vec! [ "c1", "c3" ] ); }
  { // Over the straddle: the containers in either graph.
    let (mut tree, r) : (Tree<Viewnode>, NodeId) = tree_with_r ();
    attach_containerward_chains_under_members (
      &mut tree, &[r], &f . runtime . graph,
      &f . runtime . config, None, ContainerwardChainPartners::GraphStraddle (&f . straddle) ) ?;
    assert_eq! ( child_ids (&tree, r), vec! [ "c1", "c2", "c3" ] ); }
  Ok (( )) }
