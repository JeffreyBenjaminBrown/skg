//! Unit tests for the sticky-else-default leveling rule
//! ('apply_sticky_levels'), its home clamp, the hide floor, and the
//! restricted-set deletion refusal. Installs the process-global
//! graph handle, so it assumes per-test process isolation (nextest),
//! like tests/override_substitution.rs.

use super::{apply_sticky_levels, refuse_delete_with_inactive_sections};
use crate::dbs::in_rust_graph::{InRustGraph, install_or_swap_global_handle, new_handle};
use crate::source_sets::ActiveSourceSet;
use crate::types::misc::{
  ID, MSV, PrivaciedMember, SkgConfig, SkgfileSource, SourceName,
  SourceSetName};
use crate::types::nodes::complete::{NodeComplete, empty_node_complete};

use std::collections::HashMap;
use std::path::PathBuf;

fn config_with_order (
  names : &[&str],
) -> SkgConfig {
  let mut sources : HashMap<SourceName, SkgfileSource> =
    HashMap::new ();
  for name in names {
    sources . insert (
      SourceName::from (*name),
      SkgfileSource {
        name         : SourceName::from (*name),
        abbreviation : None,
        path         : PathBuf::from ( format! ("owned/{}", name) ),
        user_owns_it : true, } ); }
  let mut config : SkgConfig =
    SkgConfig::dummyFromSources (sources);
  config . source_order =
    names . iter () . map ( |n| SourceName::from (*n) ) . collect ();
  config }

fn node_at (
  pid    : &str,
  source : &str,
) -> NodeComplete {
  let mut n : NodeComplete = empty_node_complete ();
  n . pid = ID::new (pid);
  n . title = pid . to_string ();
  n . source = SourceName::from (source);
  n }

fn install_graph (
  nodes : &[NodeComplete],
) {
  install_or_swap_global_handle ( new_handle (
    InRustGraph::from_nodecompletes (nodes) )); }

fn pm (
  level  : &str,
  member : &str,
) -> PrivaciedMember<ID> {
  PrivaciedMember::at (
    SourceName::from (level), ID::new (member) ) }

#[test]
fn sticky_preserves_disk_levels_and_default_takes_more_private_home (
) {
  let config : SkgConfig =
    config_with_order ( & ["public", "private"] );
  let old_target : NodeComplete = node_at ("old", "public");
  let new_target : NodeComplete = node_at ("fresh", "private");
  let owner      : NodeComplete = node_at ("owner", "public");
  install_graph ( & [ owner . clone (), old_target, new_target ] );
  let mut disk : NodeComplete = owner . clone ();
  disk . contains = vec! [
    pm ("private", "old") ]; // privatized on disk
  let mut buffer : NodeComplete = owner;
  buffer . contains = vec! [
    pm ("public", "old"),    // degenerate intent tag
    pm ("public", "fresh") ]; // new edge to a private-homed target
  let leveled : NodeComplete =
    apply_sticky_levels (buffer, &disk, &config);
  assert_eq! ( leveled . contains, vec! [
    pm ("private", "old"),    // STICKY: the disk's privatization survives
    pm ("private", "fresh") ] ); // DEFAULT: more private of the homes
}

#[test]
fn home_move_to_more_private_clamps_levels_up (
) {
  let config : SkgConfig =
    config_with_order ( & ["public", "private"] );
  let child : NodeComplete = node_at ("child", "public");
  let owner_before : NodeComplete = node_at ("owner", "public");
  install_graph ( & [ owner_before . clone (), child ] );
  let mut disk : NodeComplete = owner_before;
  disk . contains = vec! [ pm ("public", "child") ];
  let mut buffer : NodeComplete = node_at ("owner", "private");
  // the buffer moved the node's home to private
  buffer . contains = vec! [ pm ("private", "child") ];
  let leveled : NodeComplete =
    apply_sticky_levels (buffer, &disk, &config);
  assert_eq! ( leveled . contains, vec! [
    pm ("private", "child") ],
    "the sticky public level rises to the new, more private home" );
}

#[test]
fn hide_floor_is_the_most_public_explaining_subscription (
) {
  let config : SkgConfig =
    config_with_order ( & ["public", "trusted", "private"] );
  let hidden : NodeComplete = node_at ("victim", "public");
  let mut container_a : NodeComplete = node_at ("expl-a", "public");
  container_a . contains = vec! [ pm ("public", "victim") ];
  let mut container_b : NodeComplete = node_at ("expl-b", "public");
  container_b . contains = vec! [ pm ("public", "victim") ];
  let owner : NodeComplete = node_at ("owner", "public");
  install_graph ( & [
    owner . clone (), hidden, container_a, container_b ] );
  { // Only a PRIVATE subscription explains the hide: the hide must
    // be private, else it leaks the inference that the private
    // subscription exists. The privatized subscription is a DISK
    // fact (sticky preserves it); a buffer-raised level would come
    // through the (relSource ...) atom, which lands with
    // render-and-gating.
    let mut disk : NodeComplete = owner . clone ();
    disk . subscribes_to = MSV::Specified ( vec! [
      pm ("private", "expl-a") ] );
    let mut buffer : NodeComplete = owner . clone ();
    buffer . subscribes_to = MSV::Specified ( vec! [
      pm ("public", "expl-a") ] ); // degenerate tag; sticky restores
    buffer . hides_from_its_subscriptions = MSV::Specified ( vec! [
      pm ("public", "victim") ] ); // degenerate tag
    let leveled : NodeComplete =
      apply_sticky_levels (buffer, &disk, &config);
    assert_eq! (
      leveled . subscribes_to . or_default (),
      & [ pm ("private", "expl-a") ] );
    assert_eq! (
      leveled . hides_from_its_subscriptions . or_default (),
      & [ pm ("private", "victim") ] ); }
  { // A PUBLIC explanation exists too: the inference is innocent,
    // so the hide may stay public.
    let mut disk : NodeComplete = owner . clone ();
    disk . subscribes_to = MSV::Specified ( vec! [
      pm ("private", "expl-a"),
      pm ("public",  "expl-b") ] );
    let mut buffer : NodeComplete = owner;
    buffer . subscribes_to = MSV::Specified ( vec! [
      pm ("public", "expl-a"),
      pm ("public", "expl-b") ] );
    buffer . hides_from_its_subscriptions = MSV::Specified ( vec! [
      pm ("public", "victim") ] );
    let leveled : NodeComplete =
      apply_sticky_levels (buffer, &disk, &config);
    assert_eq! (
      leveled . hides_from_its_subscriptions . or_default (),
      & [ pm ("public", "victim") ] ); }
}

#[test]
fn restricted_delete_refusal_sees_inactive_sections (
) {
  let tmp : tempfile::TempDir = tempfile::tempdir () . unwrap ();
  let mut config : SkgConfig =
    config_with_order ( & ["public", "private"] );
  for (name, dir) in [("public", "public"), ("private", "private")] {
    let path : PathBuf = tmp . path () . join (dir);
    std::fs::create_dir_all (&path) . unwrap ();
    config . sources . get_mut ( &SourceName::from (name) )
      . unwrap () . path = path; }
  std::fs::write (
    tmp . path () . join ("private/n.skg"),
    "pid: n\n" ) . unwrap ();
  let active : ActiveSourceSet = ActiveSourceSet {
    name    : SourceSetName::from ("public"),
    sources : [ SourceName::from ("public") ]
      . into_iter () . collect (), };
  let refusal : Result<(), String> =
    refuse_delete_with_inactive_sections (
      &config, &active, &ID::new ("n") );
  assert! ( refusal . is_err (), "private section must refuse" );
  assert! ( refusal . unwrap_err ()
            . contains ("inactive sources") );
  assert! ( refuse_delete_with_inactive_sections (
    &config, &active, &ID::new ("only-public") ) . is_ok (),
    "a node with no inactive sections deletes fine" );
}
