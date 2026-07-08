//! Unit tests for the "strip body whitespace" request's core:
//! the line transform, and the on-disk pass over every source.

use super::{strip_body_whitespace_on_disk,
            strip_trailing_whitespace_from_body};
use crate::dbs::filesystem::one_node::nodecomplete_from_pid_and_source;
use crate::types::misc::{ID, SkgConfig, SkgfileSource, SourceName};
use crate::types::nodes::complete::NodeComplete;

use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::PathBuf;

#[test]
fn strip_preserves_interior_structure_and_trims_the_tail () {
  assert_eq! ( strip_trailing_whitespace_from_body ("a  \nb\t\r\n\n c"),
               "a\nb\n\n c" );
  assert_eq! ( // a final newline is trimmed (the canonical on-disk
               // block scalar cannot carry one)
    strip_trailing_whitespace_from_body ("x \n"), "x" );
  assert_eq! ( // trailing blank lines are trimmed, interior ones kept
    strip_trailing_whitespace_from_body ("x\n\ny\n\n \n"), "x\n\ny" );
  assert_eq! ( // already-clean text is a fixed point
    strip_trailing_whitespace_from_body ("x\ny"), "x\ny" );
  assert_eq! ( // a whitespace-only body strips to nothing
    strip_trailing_whitespace_from_body ("   "), "" ); }

#[test]
fn strips_on_disk_only_where_needed () {
  let tmp : tempfile::TempDir =
    tempfile::tempdir () . unwrap ();
  let owned_dir : PathBuf = tmp . path () . join ("owned");
  let foreign_dir : PathBuf = tmp . path () . join ("foreign");
  fs::create_dir_all (&owned_dir) . unwrap ();
  fs::create_dir_all (&foreign_dir) . unwrap ();
  fs::write ( owned_dir . join ("dirty.skg"),
              "title: dirty\npid: dirty\nbody: \"one  \\ntwo\\t\\n\"\n"
            ) . unwrap ();
  fs::write ( owned_dir . join ("clean.skg"),
              "title: clean\npid: clean\nbody: \"ti dy\"\n"
            ) . unwrap ();
  fs::write ( owned_dir . join ("blank.skg"),
              "title: blank\npid: blank\nbody: \"   \"\n"
            ) . unwrap ();
  fs::write ( owned_dir . join ("bodyless.skg"),
              "title: bodyless\npid: bodyless\n"
            ) . unwrap ();
  fs::write ( foreign_dir . join ("theirs.skg"),
              // Foreign sources are processed too: the request asks
              // for every repo in the config.
              "title: theirs\npid: theirs\nbody: \"alpha \\nbeta\"\n"
            ) . unwrap ();
  let clean_bytes_before : Vec<u8> =
    fs::read ( owned_dir . join ("clean.skg") ) . unwrap ();
  let config : SkgConfig = {
    let mut sources : HashMap<SourceName, SkgfileSource> =
      HashMap::new ();
    for (name, dir, owns) in [ ("owned",   &owned_dir,   true),
                               ("foreign", &foreign_dir, false) ] {
      sources . insert (
        SourceName::from (name),
        SkgfileSource {
          name         : SourceName::from (name),
          abbreviation : None,
          path         : dir . clone (),
          user_owns_it : owns, } ); }
    SkgConfig::fromSourcesAndDbName (
      sources, "db",
      & tmp . path () . join ("tantivy") . to_string_lossy () ) };
  let (all_nodes, changed) : (Vec<NodeComplete>, Vec<NodeComplete>) =
    strip_body_whitespace_on_disk (&config) . unwrap ();
  assert_eq! ( all_nodes . len (), 5 );
  { let changed_pids : HashSet<&str> =
      changed . iter ()
      . map ( |n| n . pid . as_str () ) . collect ();
    assert_eq! ( changed_pids,
                 HashSet::from ([ "dirty", "blank", "theirs" ]) ); }
  let from_disk = |pid : &str, source : &str| -> NodeComplete {
    nodecomplete_from_pid_and_source (
      &config, ID::from (pid), & SourceName::from (source)
    ) . unwrap () };
  assert_eq! ( from_disk ("dirty", "owned") . body,
               Some ( "one\ntwo" . to_string () ));
  assert_eq! ( // a body that strips to nothing is dropped
    from_disk ("blank", "owned") . body, None );
  assert_eq! ( from_disk ("theirs", "foreign") . body,
               Some ( "alpha\nbeta" . to_string () ));
  for node in &changed {
    assert_eq! ( // the returned nodes match the canonical disk form,
                 // so caches refreshed from them agree with the files
      node . body,
      from_disk ( node . pid . as_str (),
                  node . source . 0 . as_str () ) . body ); }
  assert_eq! ( // an already-clean file is not rewritten
    fs::read ( owned_dir . join ("clean.skg") ) . unwrap (),
    clean_bytes_before );
  { let (_, changed_again) : (Vec<NodeComplete>, Vec<NodeComplete>) =
      // the pass is idempotent
      strip_body_whitespace_on_disk (&config) . unwrap ();
    assert! ( changed_again . is_empty () ); }}
