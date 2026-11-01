// Unit tests for the readiness store's marks and its persistence.

use super::*;
use crate::diff_view::change_leaves::{NodeAspect, RelationshipAspect};
use crate::dbs::in_rust_graph::relation_accessors::{NodeRelation, RelationshipKey};
use crate::types::misc::ID;
use crate::types::nodes::complete::Flag;

use crate::dbs::filesystem::not_nodes::load_config;
use crate::types::misc::SkgConfig;

use std::fs;
use std::os::unix::fs::symlink;
use std::path::PathBuf;
use tempfile::TempDir;

fn marks_file (
  dir : &TempDir,
) -> PathBuf {
  dir . path () . join ("marks.json") }

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
  store . save ( &marks_file (&dir) ) . unwrap ();
  assert! ( marks_file (&dir) . is_file () );
  assert_eq! ( ReadinessStore::load ( &marks_file (&dir) ), store ); }

#[test]
fn a_missing_or_unreadable_file_is_an_empty_store () {
  let dir : TempDir = TempDir::new () . unwrap ();
  assert_eq! ( ReadinessStore::load ( &marks_file (&dir) ),
               ReadinessStore::default () );
  fs::write ( marks_file (&dir), "not json" )
    . unwrap ();
  assert_eq! ( ReadinessStore::load ( &marks_file (&dir) ),
               ReadinessStore::default () ); }

/// A marks file written while 'NodeAspect::HomeSkgrepo' was spelled
/// 'Home' still loads, and saving writes that spelling back.
#[test]
fn a_marks_file_spelling_the_aspect_Home_still_loads () {
  let dir : TempDir = TempDir::new () . unwrap ();
  let fixture : &str =
    include_str! ("readiness_store/marks_with_the_aspect_Home.json");
  fs::write ( marks_file (&dir), fixture ) . unwrap ();
  let store : ReadinessStore = ReadinessStore::load ( &marks_file (&dir) );
  let key : LeafKey = LeafKey::Node { pid    : ID::from ("moved"),
                                      aspect : NodeAspect::HomeSkgrepo };
  assert! ( store . is_ready (&key, 42) );
  store . save ( &marks_file (&dir) ) . unwrap ();
  let saved : String =
    fs::read_to_string ( marks_file (&dir) ) . unwrap ();
  assert! ( saved . contains ("\"aspect\": \"Home\""), "{}", saved ); }

/// A relationship leaf is one flat object of 'relation', 'recorder',
/// 'member' and 'aspect' in a marks file ('LeafKey' flattens its
/// 'RelationshipKey'); it loads, and saving writes that form back.
#[test]
fn a_relationship_mark_is_one_flat_object_in_the_file () {
  let dir : TempDir = TempDir::new () . unwrap ();
  let fixture : &str =
    include_str! ("readiness_store/marks_with_a_relationship.json");
  fs::write ( marks_file (&dir), fixture ) . unwrap ();
  let store : ReadinessStore = ReadinessStore::load ( &marks_file (&dir) );
  let key : LeafKey = some_keys () . remove (3);
  assert! ( store . is_ready (&key, 42) );
  store . save ( &marks_file (&dir) ) . unwrap ();
  let saved : String =
    fs::read_to_string ( marks_file (&dir) ) . unwrap ();
  assert_eq! ( saved . trim_end (), fixture . trim_end () ); }

/// Two configs in one data root keep their marks apart, each in a file
/// named after its config; a symlink to a config shares its file; and
/// 'readiness_marks_file' states another, relative to the data root.
#[test]
fn each_config_keeps_its_own_marks_file () {
  let dir : TempDir = TempDir::new () . unwrap ();
  let write_config = |file_name : &str, extra_line : &str| -> PathBuf {
    let path : PathBuf = dir . path () . join (file_name);
    fs::write ( &path, format! (
      "tantivy_folder = \".index-{}\"\n{}\n[[repos]]\nname = \"main\"\npath = \"main\"\n",
      file_name, extra_line )) . unwrap ();
    path };
  let config_of = |path : &PathBuf| -> SkgConfig {
    load_config ( path . to_str () . unwrap () ) . unwrap () . 0 };
  let jeff : SkgConfig =
    config_of ( &write_config ("skgconfig-jeff.toml", "") );
  let user : SkgConfig =
    config_of ( &write_config ("skgconfig-user.toml", "") );
  let stated : SkgConfig = config_of ( &write_config (
    "skgconfig-stated.toml", "readiness_marks_file = \"marks/stated.json\"" ));
  let symlinked : SkgConfig = {
    let link : PathBuf = dir . path () . join ("skgconfig.toml");
    symlink ( "skgconfig-jeff.toml", &link ) . unwrap ();
    config_of (&link) };
  let root : PathBuf = fs::canonicalize ( dir . path () ) . unwrap ();
  assert_eq! ( jeff . readiness_marks_path (),
               root . join ("readiness-marks-skgconfig-jeff.json") );
  assert_eq! ( user . readiness_marks_path (),
               root . join ("readiness-marks-skgconfig-user.json") );
  assert_eq! ( stated . readiness_marks_path (),
               root . join ("marks/stated.json") );
  assert_eq! ( symlinked . readiness_marks_path (),
               jeff . readiness_marks_path () );
  let keys : Vec<LeafKey> = some_keys ();
  let mut jeff_store : ReadinessStore = ReadinessStore::default ();
  jeff_store . mark (keys [0] . clone (), 1);
  jeff_store . save ( &jeff . readiness_marks_path () ) . unwrap ();
  let mut user_store : ReadinessStore = ReadinessStore::default ();
  user_store . mark (keys [1] . clone (), 2);
  user_store . save ( &user . readiness_marks_path () ) . unwrap ();
  assert_eq! ( ReadinessStore::load ( &jeff . readiness_marks_path () ),
               jeff_store );
  assert_eq! ( ReadinessStore::load ( &user . readiness_marks_path () ),
               user_store );
  assert_eq! ( ReadinessStore::load ( &symlinked . readiness_marks_path () ),
               jeff_store ); }
