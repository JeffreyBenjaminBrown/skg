// cargo nextest run --test grouped_unit -E 'test(repo_inheritance_for_non_content::)'
//
// Verifies that if a node's parent has the same repo,
// then the node's repo is not heralded,
// even if the parent ignores it.

use skg::types::misc::{ ID, RepoName, SkgConfig, SkgfileRepo };
use skg::types::viewnode::{ AffectsParent, ViewNode, ViewNodeKind, viewforest_root_viewnode, mk_definitive_viewnode, mk_writeProtected_viewnode };
use skg::types::viewnode::Vognode;
use skg::update_buffer::viewnodestats::set_viewnodestats_in_viewforest;
use skg::dbs::in_rust_graph::InRustGraph;

use ego_tree::Tree;
use std::collections::HashMap;
use std::path::PathBuf;

fn two_repo_config () -> SkgConfig {
  let mut repos : HashMap<RepoName, SkgfileRepo> =
    HashMap::new ();
  repos . insert (
    RepoName::from ("pub"),
    SkgfileRepo {
      name         : RepoName::from ("pub"),
      abbreviation : None,
      path         : PathBuf::from ("/tmp/pub"),
      user_owns_it : true } );
  repos . insert (
    RepoName::from ("priv"),
    SkgfileRepo {
      name         : RepoName::from ("priv"),
      abbreviation : None,
      path         : PathBuf::from ("/tmp/priv"),
      user_owns_it : true } );
  SkgConfig::dummyFromRepos (repos) }

/// When a node N has the same repo as its nearest activeNode ancestor,
/// even if N is marked affectsParent=false,
/// homeRepoAtBoundary should be false.
#[test]
fn repo_inheritance_across_non_content_same_repo () {
  let config : SkgConfig = two_repo_config ();
  let container_to_contents : HashMap<ID, _> = HashMap::new ();
  let content_to_containers : HashMap<ID, _> = HashMap::new ();
  let mut viewforest : Tree<ViewNode> =
    Tree::new ( viewforest_root_viewnode () );
  let a_id = {
    let vn : ViewNode = mk_definitive_viewnode (
      ID::from ("a"),
      RepoName::from ("pub"),
      "node A" . to_string (),
      None );
    viewforest . root_mut () . append (vn) . id () };
  { let vn : ViewNode = mk_writeProtected_viewnode (
      ID::from ("b"),
      RepoName::from ("pub"),
      "node B" . to_string (),
      AffectsParent::False );
    viewforest . get_mut (a_id) . unwrap () . append (vn); }
  set_viewnodestats_in_viewforest (
    &mut viewforest,
    &InRustGraph::new (),
    &container_to_contents,
    &content_to_containers,
    &config,
    None );
  // B has same repo as A, so homeRepoAtBoundary should be false,
  // even though B has affectsParent != Affected.
  let b_ref =
    viewforest . get (a_id) . unwrap ()
    . first_child () . unwrap ();
  let ViewNodeKind::Vognode ( Vognode::Active (t) )
    = & b_ref . value () . kind
    else { panic! ("expected ActiveNode") };
  assert! ( ! t . viewStats . homeRepoAtBoundary,
            "Same repo across non-content boundary \
             should NOT be at boundary" ); }

/// When a non-content child (affectsParent != Affected) has a different repo
/// from its nearest activeNode ancestor,
/// homeRepoAtBoundary should be true.
#[test]
fn repo_inheritance_across_non_content_different_repo () {
  let config : SkgConfig = two_repo_config ();
  let container_to_contents : HashMap<ID, _> = HashMap::new ();
  let content_to_containers : HashMap<ID, _> = HashMap::new ();
  let mut viewforest : Tree<ViewNode> =
    Tree::new ( viewforest_root_viewnode () );
  let a_id = {
    let vn : ViewNode = mk_definitive_viewnode (
      ID::from ("a"),
      RepoName::from ("pub"),
      "node A" . to_string (),
      None );
    viewforest . root_mut () . append (vn) . id () };
  { let vn : ViewNode = mk_writeProtected_viewnode (
      ID::from ("b"),
      RepoName::from ("priv"),
      "node B" . to_string (),
      AffectsParent::False );
    viewforest . get_mut (a_id) . unwrap () . append (vn); }
  set_viewnodestats_in_viewforest (
    &mut viewforest,
    &InRustGraph::new (),
    &container_to_contents,
    &content_to_containers,
    &config,
    None );
  let b_ref =
    viewforest . get (a_id) . unwrap ()
    . first_child () . unwrap ();
  let ViewNodeKind::Vognode ( Vognode::Active (t) )
    = & b_ref . value () . kind
    else { panic! ("expected ActiveNode") };
  assert! ( t . viewStats . homeRepoAtBoundary,
            "Different repo across non-content boundary \
             should be at boundary" ); }
