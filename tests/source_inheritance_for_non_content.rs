// cargo test --test source_inheritance_for_non_content -- --nocapture
//
// Verifies that if a node's parent has the same source,
// then the node's source is not heralded,
// even if the parent ignores it.

use skg::types::misc::{ ID, SourceName, SkgConfig, SkgfileSource };
use skg::types::viewnode::{ Birth, ViewNode, ViewNodeKind, forest_root_viewnode, mk_definitive_viewnode, mk_indefinitive_viewnode };
use skg::update_buffer::viewnodestats::set_viewnodestats_in_forest;

use ego_tree::Tree;
use std::collections::HashMap;
use std::path::PathBuf;

fn two_source_config () -> SkgConfig {
  let mut sources : HashMap<SourceName, SkgfileSource> =
    HashMap::new ();
  sources . insert (
    SourceName::from ("pub"),
    SkgfileSource {
      name         : SourceName::from ("pub"),
      abbreviation : None,
      path         : PathBuf::from ("/tmp/pub"),
      user_owns_it : true } );
  sources . insert (
    SourceName::from ("priv"),
    SkgfileSource {
      name         : SourceName::from ("priv"),
      abbreviation : None,
      path         : PathBuf::from ("/tmp/priv"),
      user_owns_it : true } );
  SkgConfig::dummyFromSources (sources) }

/// When a node N has the same source as its nearest truenode ancestor,
/// even if N is marked birth=Independent,
/// sourceAtBoundary should be false.
#[test]
fn source_inheritance_across_non_content_same_source () {
  let config : SkgConfig = two_source_config ();
  let container_to_contents : HashMap<ID, _> = HashMap::new ();
  let content_to_containers : HashMap<ID, _> = HashMap::new ();
  let mut forest : Tree<ViewNode> =
    Tree::new ( forest_root_viewnode () );
  let a_id = {
    let vn : ViewNode = mk_definitive_viewnode (
      ID::from ("a"),
      SourceName::from ("pub"),
      "node A" . to_string (),
      None );
    forest . root_mut () . append (vn) . id () };
  { let vn : ViewNode = mk_indefinitive_viewnode (
      ID::from ("b"),
      SourceName::from ("pub"),
      "node B" . to_string (),
      Birth::Independent );
    forest . get_mut (a_id) . unwrap () . append (vn); }
  set_viewnodestats_in_forest (
    &mut forest,
    &container_to_contents,
    &content_to_containers,
    &config );
  // B has same source as A, so sourceAtBoundary should be false,
  // even though B has birth != ContentOf.
  let b_ref =
    forest . get (a_id) . unwrap ()
    . first_child () . unwrap ();
  let ViewNodeKind::True (t) = & b_ref . value () . kind
    else { panic! ("expected TrueNode") };
  assert! ( ! t . viewStats . sourceAtBoundary,
            "Same source across non-content boundary \
             should NOT be at boundary" ); }

/// When a non-content child (birth != ContentOf) has a different source
/// from its nearest truenode ancestor,
/// sourceAtBoundary should be true.
#[test]
fn source_inheritance_across_non_content_different_source () {
  let config : SkgConfig = two_source_config ();
  let container_to_contents : HashMap<ID, _> = HashMap::new ();
  let content_to_containers : HashMap<ID, _> = HashMap::new ();
  let mut forest : Tree<ViewNode> =
    Tree::new ( forest_root_viewnode () );
  let a_id = {
    let vn : ViewNode = mk_definitive_viewnode (
      ID::from ("a"),
      SourceName::from ("pub"),
      "node A" . to_string (),
      None );
    forest . root_mut () . append (vn) . id () };
  { let vn : ViewNode = mk_indefinitive_viewnode (
      ID::from ("b"),
      SourceName::from ("priv"),
      "node B" . to_string (),
      Birth::Independent );
    forest . get_mut (a_id) . unwrap () . append (vn); }
  set_viewnodestats_in_forest (
    &mut forest,
    &container_to_contents,
    &content_to_containers,
    &config );
  let b_ref =
    forest . get (a_id) . unwrap ()
    . first_child () . unwrap ();
  let ViewNodeKind::True (t) = & b_ref . value () . kind
    else { panic! ("expected TrueNode") };
  assert! ( t . viewStats . sourceAtBoundary,
            "Different source across non-content boundary \
             should be at boundary" ); }
