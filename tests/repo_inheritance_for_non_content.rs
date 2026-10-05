// cargo nextest run --test grouped_unit -E 'test(repo_inheritance_for_non_content::)'
//
// Verifies that if a node's parent has the same skgrepo,
// then the node's skgrepo is not heralded,
// even if the parent ignores it.

use skg::types::misc::{ ID, SkgRepoName, SkgConfig, SkgRepo };
use skg::types::viewnode::{ AffectsParent, Viewnode, ViewnodeKind, viewforest_root_viewnode, mk_editable_viewnode, mk_writeProtected_viewnode };
use skg::types::viewnode::Vognode;
use skg::update_buffer::viewnodestats::set_viewnodestats_in_viewforest;
use skg::dbs::in_rust_graph::InRustGraph;

use ego_tree::Tree;
use std::collections::HashMap;
use std::path::PathBuf;

fn two_skgrepo_config () -> SkgConfig {
  let mut skgrepos : HashMap<SkgRepoName, SkgRepo> =
    HashMap::new ();
  skgrepos . insert (
    SkgRepoName::from ("pub"),
    SkgRepo {
      name         : SkgRepoName::from ("pub"),
      abbreviation : None,
      path         : PathBuf::from ("/tmp/pub"),
      owned        : true } );
  skgrepos . insert (
    SkgRepoName::from ("priv"),
    SkgRepo {
      name         : SkgRepoName::from ("priv"),
      abbreviation : None,
      path         : PathBuf::from ("/tmp/priv"),
      owned        : true } );
  SkgConfig::dummyFromSkgRepos (skgrepos) }

/// When a node N has the same skgrepo as its nearest unrestrictedVognode ancestor,
/// even if N is marked affectsParent=false,
/// homeRepoAtBoundary should be false.
#[test]
fn skgrepo_inheritance_across_non_content_same_skgrepo () {
  let config : SkgConfig = two_skgrepo_config ();
  let container_to_contents : HashMap<ID, _> = HashMap::new ();
  let content_to_containers : HashMap<ID, _> = HashMap::new ();
  let mut viewforest : Tree<Viewnode> =
    Tree::new ( viewforest_root_viewnode () );
  let a_skgid = {
    let vn : Viewnode = mk_editable_viewnode (
      ID::from ("a"),
      SkgRepoName::from ("pub"),
      "node A" . to_string (),
      None );
    viewforest . root_mut () . append (vn) . id () };
  { let vn : Viewnode = mk_writeProtected_viewnode (
      ID::from ("b"),
      SkgRepoName::from ("pub"),
      "node B" . to_string (),
      AffectsParent::False );
    viewforest . get_mut (a_skgid) . unwrap () . append (vn); }
  set_viewnodestats_in_viewforest (
    &mut viewforest,
    &InRustGraph::new (),
    &container_to_contents,
    &content_to_containers,
    &config,
    None );
  // B has same skgrepo as A, so homeRepoAtBoundary should be false,
  // even though B has affectsParent != True.
  let b_ref =
    viewforest . get (a_skgid) . unwrap ()
    . first_child () . unwrap ();
  let ViewnodeKind::Vognode ( Vognode::Unrestricted (t) )
    = & b_ref . value () . kind
    else { panic! ("expected UnrestrictedVognode") };
  assert! ( ! t . viewStats . homeSkgRepoAtBoundary,
            "Same repo across non-content boundary \
             should NOT be at boundary" ); }

/// When a non-content child (affectsParent != True) has a different skgrepo
/// from its nearest unrestrictedVognode ancestor,
/// homeRepoAtBoundary should be true.
#[test]
fn skgrepo_inheritance_across_non_content_different_skgrepo () {
  let config : SkgConfig = two_skgrepo_config ();
  let container_to_contents : HashMap<ID, _> = HashMap::new ();
  let content_to_containers : HashMap<ID, _> = HashMap::new ();
  let mut viewforest : Tree<Viewnode> =
    Tree::new ( viewforest_root_viewnode () );
  let a_skgid = {
    let vn : Viewnode = mk_editable_viewnode (
      ID::from ("a"),
      SkgRepoName::from ("pub"),
      "node A" . to_string (),
      None );
    viewforest . root_mut () . append (vn) . id () };
  { let vn : Viewnode = mk_writeProtected_viewnode (
      ID::from ("b"),
      SkgRepoName::from ("priv"),
      "node B" . to_string (),
      AffectsParent::False );
    viewforest . get_mut (a_skgid) . unwrap () . append (vn); }
  set_viewnodestats_in_viewforest (
    &mut viewforest,
    &InRustGraph::new (),
    &container_to_contents,
    &content_to_containers,
    &config,
    None );
  let b_ref =
    viewforest . get (a_skgid) . unwrap ()
    . first_child () . unwrap ();
  let ViewnodeKind::Vognode ( Vognode::Unrestricted (t) )
    = & b_ref . value () . kind
    else { panic! ("expected UnrestrictedVognode") };
  assert! ( t . viewStats . homeSkgRepoAtBoundary,
            "Different repo across non-content boundary \
             should be at boundary" ); }
