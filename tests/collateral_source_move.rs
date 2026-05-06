// cargo nextest run --test collateral_source_move

use futures::executor::block_on;
use skg::dbs::filesystem::multiple_nodes::read_all_skg_files_from_sources;
use skg::dbs::filesystem::not_nodes::load_config_with_overrides;
use skg::dbs::init::{
  create_empty_tantivy_index,
  overwrite_new_empty_typedb_db,
  read_and_use_schema};
use skg::dbs::in_rust_graph::{
  InRustGraph,
  InRustGraphHandle,
  init_global_handle_for_first_time_or_panic,
  new_handle};
use skg::dbs::typedb::nodes::create_all_nodes;
use skg::dbs::typedb::relationships::create_all_relationships;
use skg::serve::ViewsState;
use skg::serve::handlers::save_buffer::SaveResponse;
use skg::test_utils::{
  cleanup_test_tantivy_and_typedb_dbs,
  extract_string_field_from_sexp,
  read_all_lp_messages,
  update_from_and_rerender_buffer_test as update_from_and_rerender_buffer};
use skg::to_org::render::content_view::multi_root_view;
use skg::types::misc::{ID, SkgConfig, TantivyIndex};
use skg::types::nodes::complete::NodeComplete;
use skg::types::nodes::typedb::NodeTypedb;
use skg::types::views_state::{OpenViews, ViewUri};
use skg::types::viewnode::ViewNode;

use ego_tree::Tree;
use std::error::Error;
use std::fs;
use std::io::BufReader;
use std::net::{TcpListener, TcpStream};
use std::path::{Path, PathBuf};
use std::sync::Arc;
use tempfile::TempDir;
use typedb_driver::{Credentials, DriverOptions, TypeDBDriver};

#[test]
fn test_source_move_updates_collateral_view_metadata (
) -> Result<(), Box<dyn Error>> {
  let db_name : &str =
    "skg-test-collateral-source-move";
  let temp_dir : TempDir =
    TempDir::new()?;
  copy_dir_all (
    Path::new ("tests/move_source/fixtures"),
    temp_dir . path() )?;
  let tantivy_folder : PathBuf =
    temp_dir . path() . join ("tantivy");
  let (config, driver, tantivy, initial_nodes)
    : (SkgConfig, Arc<TypeDBDriver>, TantivyIndex, Vec<NodeComplete>) =
    block_on ( setup_test_dbs (
      db_name,
      temp_dir . path(),
      &tantivy_folder ) ) ?;
  let graph : InRustGraphHandle =
    new_handle ( InRustGraph::from_nodecompletes (&initial_nodes) );
  init_global_handle_for_first_time_or_panic (graph . clone ());

  let (_save_response, collateral_buffer)
    : (SaveResponse, String) =
    block_on ( async {
      let root_ids : Vec<ID> =
        vec![ ID::new ("a") ];
      let (initial_buffer, pids, viewforest)
        : (String, Vec<ID>, Tree<ViewNode>) =
        multi_root_view (
          &driver, &config, None, &root_ids, false ) . await?;
      assert! (
        buffer_has_source_for_title (
          &initial_buffer, "b", "public" ),
        "initial view should show b in public:\n{}",
        initial_buffer );

      let mut views_state : ViewsState = ViewsState {
        diff_mode_enabled : false,
        open_views        : OpenViews::new (), };
      let uri_1 : ViewUri =
        ViewUri::ContentView ( "source-move-buffer-1" . to_string() );
      let uri_2 : ViewUri =
        ViewUri::ContentView ( "source-move-buffer-2" . to_string() );
      views_state . open_views . register_view (
        uri_1 . clone(), viewforest . clone(), &pids );
      views_state . open_views . register_view (
        uri_2 . clone(), viewforest, &pids );

      let save_input : String =
        initial_buffer . replace (
          "(id b) (source public)",
          "(id b) (source private)" );
      let (mut stream, read_end) : (TcpStream, TcpStream) =
        mk_test_tcp_stream_pair ();
      let save_response : SaveResponse =
        update_from_and_rerender_buffer (
          &mut stream,
          &save_input,
          &driver,
          &config,
          &tantivy,
          &graph,
          false,
          &Ok ( uri_1 . clone() ),
          &mut views_state ) . await?;
      drop (stream);

      let mut reader : BufReader<TcpStream> =
        BufReader::new (read_end);
      let collateral_msgs : Vec<String> =
        read_all_lp_messages (&mut reader);
      assert_eq! (
        collateral_msgs . len(), 1,
        "expected one collateral message, got {:?}",
        collateral_msgs );
      assert! (
        collateral_msgs[0] . contains (&uri_2 . repr_in_client()),
        "collateral message should target buffer 2:\n{}",
        collateral_msgs[0] );
      let collateral_buffer : String =
        extract_string_field_from_sexp (
          &collateral_msgs[0], "content" )
        . expect ("content field not found in collateral-view sexp");
      Result::<_, Box<dyn Error>>::Ok ((
        save_response, collateral_buffer )) } ) ?;

  assert! (
    buffer_has_source_for_title (
      &collateral_buffer, "b", "private" ),
    "collateral view should show b's new source:\n{}",
    collateral_buffer );

  block_on ( cleanup_test_tantivy_and_typedb_dbs (
    db_name, &driver, Some (config . tantivy_folder . as_path()) ))?;
  Ok (( )) }

async fn setup_test_dbs (
  db_name        : &str,
  fixtures_root  : &Path,
  tantivy_folder : &Path,
) -> Result<(SkgConfig, Arc<TypeDBDriver>, TantivyIndex, Vec<NodeComplete>),
            Box<dyn Error>> {
  let config : SkgConfig =
    load_config_with_overrides (
      fixtures_root . join ("skgconfig.toml")
        . to_str() . unwrap(),
      Some (db_name),
      &[ ("public",  fixtures_root . join ("public")),
         ("private", fixtures_root . join ("private")),
         ("foreign", fixtures_root . join ("foreign")) ] )?;
  let config : SkgConfig =
    SkgConfig {
      tantivy_folder : tantivy_folder . to_path_buf(),
      .. config };
  let driver : TypeDBDriver =
    TypeDBDriver::new (
      "127.0.0.1:1729",
      Credentials::new ("admin", "password"),
      DriverOptions::new (false, None)? ) . await?;
  let nodes : Vec<NodeComplete> =
    read_all_skg_files_from_sources (&config)?;
  let typedb_nodes : Vec<NodeTypedb> =
    nodes . iter ()
    . map (NodeTypedb::from_complete_parsing_textlinks)
    . collect ();
  overwrite_new_empty_typedb_db (db_name, &driver) . await?;
  read_and_use_schema (db_name, &driver) . await?;
  create_all_nodes (db_name, &driver, &typedb_nodes) . await?;
  create_all_relationships (db_name, &driver, &typedb_nodes) . await?;
  let tantivy_index : TantivyIndex =
    create_empty_tantivy_index (&config . tantivy_folder)?;
  Ok ((config, Arc::new (driver), tantivy_index, nodes)) }

fn copy_dir_all (
  src : &Path,
  dst : &Path,
) -> Result<(), Box<dyn Error>> {
  fs::create_dir_all (dst)?;
  for entry in fs::read_dir (src)? {
    let entry : fs::DirEntry = entry?;
    let src_path : PathBuf =
      entry . path();
    let dst_path : PathBuf =
      dst . join (entry . file_name());
    if entry . file_type()? . is_dir() {
      copy_dir_all (&src_path, &dst_path)?;
    } else {
      fs::copy (&src_path, &dst_path)?;
    }}
  Ok (( )) }

fn mk_test_tcp_stream_pair (
) -> (TcpStream, TcpStream) {
  let listener : TcpListener =
    TcpListener::bind ("127.0.0.1:0") . unwrap();
  let addr =
    listener . local_addr() . unwrap();
  let write_end : TcpStream =
    TcpStream::connect (addr) . unwrap();
  let (read_end, _) =
    listener . accept() . unwrap();
  (write_end, read_end) }

fn buffer_has_source_for_title (
  buffer : &str,
  title  : &str,
  source : &str,
) -> bool {
  let id_fragment : String =
    format! ("(id {})", title);
  let source_fragment : String =
    format! ("(source {})", source);
  let title_suffix : String =
    format! (") {}", title);
  buffer . lines() . any ( |line|
    line . contains (&id_fragment)
    && line . contains (&source_fragment)
    && line . ends_with (&title_suffix) ) }
