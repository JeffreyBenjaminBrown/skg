//! Unit tests for the "strip body whitespace" request's core:
//! the line transform, and the on-disk pass over every source.

use super::{strip_body_whitespace_on_disk,
            strip_trailing_whitespace_from_body};
use crate::dbs::filesystem::multiple_nodes::{
  LoadedCorpus,
  read_all_skg_files_with_manifest};
use crate::dbs::filesystem::one_node::write_nodecomplete_telescope;
use crate::dbs::init::empty_in_ram_tantivy_index;
use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::tantivy::search::{search_index, SearchOptions};
use crate::dbs::tantivy::write::reconstruct_index_from_nodes;
use crate::runtime::{SelectedRuntimeSnapshot, ServerRuntime};
use crate::types::env::SkgEnv;
use crate::dbs::filesystem::one_node::nodecomplete_from_pid_and_source;
use crate::types::misc::{ID, SkgConfig, SkgfileSource, SourceName, TantivyIndex};
use crate::types::nodes::complete::NodeComplete;
use crate::types::store_state::{PathDigest, SelectedStoreState};

use std::collections::{HashMap, HashSet};
use std::fs;
use std::io::{BufRead, BufReader, Read};
use std::net::{TcpListener, TcpStream};
use std::path::PathBuf;
use std::sync::Arc;
use arc_swap::ArcSwap;
use tempfile::TempDir;

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
              // A foreign source is read-only: its trailing
              // whitespace survives the strip.
              "title: theirs\npid: theirs\nbody: \"alpha \\nbeta\"\n"
            ) . unwrap ();
  let clean_bytes_before : Vec<u8> =
    fs::read ( owned_dir . join ("clean.skg") ) . unwrap ();
  let foreign_bytes_before : Vec<u8> =
    fs::read ( foreign_dir . join ("theirs.skg") ) . unwrap ();
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
    SkgConfig::from_sources (
      sources,
      & tmp . path () . join ("tantivy") . to_string_lossy () ) };
  let (all_nodes, changed) : (Vec<NodeComplete>, Vec<NodeComplete>) =
    strip_body_whitespace_on_disk (&config) . unwrap ();
  assert_eq! ( all_nodes . len (), 5 );
  { let changed_pids : HashSet<&str> =
      changed . iter ()
      . map ( |n| n . pid . as_str () ) . collect ();
    assert_eq! ( changed_pids,
                 HashSet::from ([ "dirty", "blank" ]) ); }
  let from_disk = |pid : &str, source : &str| -> NodeComplete {
    nodecomplete_from_pid_and_source (
      &config, ID::from (pid), & SourceName::from (source)
    ) . unwrap () };
  assert_eq! ( from_disk ("dirty", "owned") . body,
               Some ( "one\ntwo" . to_string () ));
  assert_eq! ( // a body that strips to nothing is dropped
    from_disk ("blank", "owned") . body, None );
  assert_eq! ( // a foreign body keeps its whitespace, untouched
    from_disk ("theirs", "foreign") . body,
    Some ( "alpha \nbeta" . to_string () ));
  assert_eq! ( // ... indeed the whole foreign file is not rewritten
    fs::read ( foreign_dir . join ("theirs.skg") ) . unwrap (),
    foreign_bytes_before );
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

fn operation_fixture () -> (TempDir, ServerRuntime, PathBuf, PathBuf, PathBuf) {
  let tmp : TempDir = tempfile::tempdir () . unwrap ();
  let owned_dir : PathBuf = tmp . path () . join ("owned");
  let foreign_dir : PathBuf = tmp . path () . join ("foreign");
  fs::create_dir_all (&owned_dir) . unwrap ();
  fs::create_dir_all (&foreign_dir) . unwrap ();
  let mut sources : HashMap<SourceName, SkgfileSource> = HashMap::new ();
  sources . insert (SourceName::from ("owned"), SkgfileSource {
    name: SourceName::from ("owned"), abbreviation: None,
    path: owned_dir . clone (), user_owns_it: true, });
  sources . insert (SourceName::from ("foreign"), SkgfileSource {
    name: SourceName::from ("foreign"), abbreviation: None,
    path: foreign_dir . clone (), user_owns_it: false, });
  let mut config : SkgConfig = SkgConfig::from_sources (
    sources, &tmp . path () . join ("tantivy") . to_string_lossy ());
  config . config_path = tmp . path () . join ("skgconfig.toml");
  config . data_root = tmp . path () . to_path_buf ();
  config . maintenance_archive_identity = tmp . path () . join ("archive");

  let dirty_one : NodeComplete = empty_node ("dirty-one", "one  \ntwo\t");
  let dirty_one_path : PathBuf = owned_dir . join ("dirty-one.skg");
  write_nodecomplete_telescope (&dirty_one, &config) . unwrap ();
  let dirty_two : NodeComplete = empty_node ("dirty-two", "three  \nfour\t");
  let dirty_two_path : PathBuf = owned_dir . join ("dirty-two.skg");
  write_nodecomplete_telescope (&dirty_two, &config) . unwrap ();
  let foreign_path : PathBuf = foreign_dir . join ("foreign.skg");
  fs::write (&foreign_path,
    "title: foreign\npid: foreign\nbody: \"foreign  \"\n") . unwrap ();

  let loaded : LoadedCorpus =
    read_all_skg_files_with_manifest (&config) . unwrap ();
  let index : TantivyIndex = empty_in_ram_tantivy_index () . unwrap ();
  reconstruct_index_from_nodes (&loaded . nodes, &index, &HashMap::new ()) . unwrap ();
  let selected : Arc<SelectedStoreState> = Arc::new (
    SelectedStoreState::initial (InRustGraph::from_nodecompletes (&loaded . nodes),
                                 loaded . manifest)
      . with_searcher (index . reader . searcher ()));
  let env : SkgEnv = SkgEnv {
    config,
    in_rust_graph: Arc::new (ArcSwap::from (selected)),
    searcher: index . reader . searcher (),
    tantivy_index: index,
    startup_warnings: Arc::new (Vec::new ()), };
  let runtime : ServerRuntime = ServerRuntime::new (env) . unwrap ();
  (tmp, runtime, dirty_one_path, dirty_two_path, foreign_path)
}

fn empty_node (pid : &str, body : &str) -> NodeComplete {
  let mut node : NodeComplete =
    crate::types::nodes::complete::empty_node_complete ();
  node . pid = ID::from (pid);
  node . source = SourceName::from ("owned");
  node . title = pid . into ();
  node . body = Some (body . into ());
  node
}

fn strip_request (runtime : &ServerRuntime, operation_id : &str) -> String {
  format! ("((request . \"strip body whitespace\") (request-id . \"request-{}\") (server-session-id . \"{}\") (operation-id . \"{}\"))",
    operation_id, runtime . server_session_id (), operation_id)
}

fn execute_strip (runtime : &ServerRuntime, request : &str) -> Result<String, String> {
  let listener : TcpListener = TcpListener::bind ("127.0.0.1:0")
    . map_err (|error| error . to_string ())?;
  let address = listener . local_addr () . map_err (|error| error . to_string ())?;
  let client : TcpStream = TcpStream::connect (address)
    . map_err (|error| error . to_string ())?;
  let (mut server, _) : (TcpStream, std::net::SocketAddr) = listener . accept ()
    . map_err (|error| error . to_string ())?;
  super::handle_strip_body_whitespace_request (&mut server, request, runtime);
  let mut reader : BufReader<TcpStream> = BufReader::new (client . try_clone ()
    . map_err (|error| error . to_string ())?);
  let mut header : String = String::new ();
  let mut content_length : Option<usize> = None;
  loop {
    header . clear ();
    reader . read_line (&mut header) . map_err (|error| error . to_string ())?;
    if header == "\r\n" || header == "\n" { break; }
    if let Some (value) = header . strip_prefix ("Content-Length: ") {
      content_length = Some (value . trim () . parse ()
        . map_err (|error : std::num::ParseIntError| error . to_string ())?); }}
  let length : usize = content_length . ok_or ("response omitted Content-Length")?;
  let mut payload : Vec<u8> = vec![0; length];
  reader . read_exact (&mut payload) . map_err (|error| error . to_string ())?;
  let response : String = String::from_utf8 (payload)
    . map_err (|error| error . to_string ())?;
  if response . contains ("refused") || response . contains ("(response-type \"error\")") {
    Err (response)
  } else { Ok (response) }
}

#[test]
fn ordinary_strip_uses_selected_graph_and_publishes_matching_search () {
  let (_tmp, runtime, dirty_one_path, dirty_two_path, foreign_path) =
    operation_fixture ();
  let selected_foreign_body : Option<String> = runtime . selected_snapshot ()
    . selected . graph . get (&ID::from ("foreign"))
    . and_then (|node| node . body . clone ());
  fs::write (&foreign_path, "newer foreign bytes\n") . unwrap ();
  let request : String = strip_request (
    &runtime, "550e8400-e29b-41d4-a716-446655440001");
  let response : String = execute_strip (&runtime, &request) . unwrap ();
  assert! (response . contains ("Stripped trailing whitespace from 2 of 2"));
  assert_eq! (fs::read (&foreign_path) . unwrap (), b"newer foreign bytes\n");
  let selected : Arc<SelectedRuntimeSnapshot> = runtime . selected_snapshot ();
  assert_eq! (selected . selected . graph . get (&ID::from ("foreign"))
                . and_then (|node| node . body . clone ()), selected_foreign_body);
  assert_eq! (selected . selected . graph . get (&ID::from ("dirty-one"))
                . and_then (|node| node . body . clone ()),
              Some ("one\ntwo" . to_string ()));
  let dirty_one_digest : PathDigest = PathDigest::of_bytes (
    &fs::read (&dirty_one_path) . unwrap ());
  assert_eq! (selected . selected . manifest . get (&dirty_one_path),
              Some (&dirty_one_digest));
  assert_eq! (selected . selected . graph_generation,
              selected . env . in_rust_graph . load_full () . graph_generation);
  assert! (search_index (&selected . env . tantivy_index, &selected . env . searcher,
                         "one", &SearchOptions { body: true, ..Default::default () })
             . unwrap () . 0 . len () > 0);
  assert_eq! (fs::read_to_string (&dirty_one_path) . unwrap (),
              "title: dirty-one\npid: dirty-one\nbody: |2-\n  one\n  two\n");
  assert_eq! (fs::read_to_string (&dirty_two_path) . unwrap (),
              "title: dirty-two\npid: dirty-two\nbody: |2-\n  three\n  four\n");
}

#[test]
fn ordinary_strip_conflict_refuses_the_whole_batch () {
  let (_tmp, runtime, dirty_one_path, dirty_two_path, _foreign_path) =
    operation_fixture ();
  fs::write (&dirty_one_path, "external changed bytes\n") . unwrap ();
  let before_two : Vec<u8> = fs::read (&dirty_two_path) . unwrap ();
  let request : String = strip_request (
    &runtime, "550e8400-e29b-41d4-a716-446655440002");
  assert! (execute_strip (&runtime, &request) . is_err ());
  assert_eq! (fs::read (&dirty_one_path) . unwrap (), b"external changed bytes\n");
  assert_eq! (fs::read (&dirty_two_path) . unwrap (), before_two);
}

#[test]
fn ordinary_strip_duplicate_replay_does_not_overwrite_newer_bytes () {
  let (_tmp, runtime, dirty_one_path, _dirty_two_path, _foreign_path) =
    operation_fixture ();
  let request : String = strip_request (
    &runtime, "550e8400-e29b-41d4-a716-446655440003");
  let first : String = execute_strip (&runtime, &request) . unwrap ();
  fs::write (&dirty_one_path, "newer external bytes\n") . unwrap ();
  let replay : String = execute_strip (&runtime, &request) . unwrap ();
  assert_eq! (replay, first);
  assert_eq! (fs::read (&dirty_one_path) . unwrap (), b"newer external bytes\n");
}
