// Unit tests for the readiness store's marks and its persistence.

use super::*;
use crate::change_leaves::{NodeAspect, RelationshipAspect};
use crate::dbs::in_rust_graph::relation_accessors::{NodeRelation, RelationshipKey};
use crate::types::misc::ID;
use crate::types::nodes::complete::Flag;

use std::fs;
use tempfile::TempDir;

fn some_keys () -> Vec<LeafKey> {
  vec! [
    LeafKey::Node { pid : ID::from ("a"), aspect : NodeAspect::Text },
    LeafKey::Node { pid : ID::from ("a"),
                    aspect : NodeAspect::Alias ("x" . to_string ()) },
    LeafKey::Node { pid : ID::from ("b"),
                    aspect : NodeAspect::Flag (Flag::NoSearchMatching) },
    LeafKey::Relationship {
      relationship : RelationshipKey {
        relation : NodeRelation::Contains,
        recorder : ID::from ("a"),
        member   : ID::from ("b") },
      aspect       : RelationshipAspect::RelRepo }, ] }

#[test]
fn a_mark_is_ready_only_with_its_fingerprint () {
  let key : LeafKey = some_keys () . remove (0);
  let mut store : ReadinessStore = ReadinessStore::default ();
  assert! ( ! store . is_ready (&key, 7) );
  store . mark (key . clone (), 7);
  assert! ( store . is_ready (&key, 7) );
  assert! ( ! store . is_ready (&key, 8) );
  store . unmark (&key);
  assert! ( ! store . is_ready (&key, 7) ); }

#[test]
fn the_store_round_trips_through_its_file () {
  let dir : TempDir = TempDir::new () . unwrap ();
  let mut store : ReadinessStore = ReadinessStore::default ();
  for (fingerprint, key) in some_keys () . into_iter () . enumerate () {
    store . mark (key, fingerprint as u64 + 1); }
  store . save (dir . path ()) . unwrap ();
  assert! ( readiness_marks_path (dir . path ()) . is_file () );
  assert_eq! ( ReadinessStore::load (dir . path ()), store ); }

#[test]
fn a_missing_or_unreadable_file_is_an_empty_store () {
  let dir : TempDir = TempDir::new () . unwrap ();
  assert_eq! ( ReadinessStore::load (dir . path ()),
               ReadinessStore::default () );
  fs::write ( readiness_marks_path (dir . path ()), "not json" )
    . unwrap ();
  assert_eq! ( ReadinessStore::load (dir . path ()),
               ReadinessStore::default () ); }

/// A marks file written while 'NodeAspect::HomeSkgrepo' was spelled
/// 'Home' still loads, and saving writes that spelling back.
#[test]
fn a_marks_file_spelling_the_aspect_Home_still_loads () {
  let dir : TempDir = TempDir::new () . unwrap ();
  let fixture : &str =
    include_str! ("readiness_store/marks_with_the_aspect_Home.json");
  fs::write ( readiness_marks_path (dir . path ()), fixture ) . unwrap ();
  let store : ReadinessStore = ReadinessStore::load (dir . path ());
  let key : LeafKey = LeafKey::Node { pid    : ID::from ("moved"),
                                      aspect : NodeAspect::HomeSkgrepo };
  assert! ( store . is_ready (&key, 42) );
  store . save (dir . path ()) . unwrap ();
  let saved : String =
    fs::read_to_string ( readiness_marks_path (dir . path ()) ) . unwrap ();
  assert! ( saved . contains ("\"aspect\": \"Home\""), "{}", saved ); }

/// A relationship leaf is one flat object of 'relation', 'recorder',
/// 'member' and 'aspect' in a marks file ('LeafKey' flattens its
/// 'RelationshipKey'); it loads, and saving writes that form back.
#[test]
fn a_relationship_mark_is_one_flat_object_in_the_file () {
  let dir : TempDir = TempDir::new () . unwrap ();
  let fixture : &str =
    include_str! ("readiness_store/marks_with_a_relationship.json");
  fs::write ( readiness_marks_path (dir . path ()), fixture ) . unwrap ();
  let store : ReadinessStore = ReadinessStore::load (dir . path ());
  let key : LeafKey = some_keys () . remove (3);
  assert! ( store . is_ready (&key, 42) );
  store . save (dir . path ()) . unwrap ();
  let saved : String =
    fs::read_to_string ( readiness_marks_path (dir . path ()) ) . unwrap ();
  assert_eq! ( saved . trim_end (), fixture . trim_end () ); }
