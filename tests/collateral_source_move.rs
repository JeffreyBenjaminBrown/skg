// cargo nextest run --test grouped_overrides -E 'test(collateral_source_move::)'

use futures::executor::block_on;
use skg::dbs::filesystem::not_nodes::load_config_with_overrides;
use skg::dbs::init::create_empty_tantivy_index;
use skg::dbs::in_rust_graph::InRustGraphHandle;
use skg::serve::ViewsState;
use skg::serve::handlers::save_buffer::SaveResponse;
use skg::test_utils::{
  cleanup_test_tantivy,
  extract_string_field_from_sexp,
  graph_handle_from_config,
  read_all_lp_messages,
  update_from_and_rerender_buffer_test as update_from_and_rerender_buffer};
use skg::to_org::render::content_view::multi_root_view;
use skg::types::misc::{ID, SkgConfig, TantivyIndex};
use skg::types::views_state::{OpenViews, ViewUri};
use skg::types::viewnode::ViewNode;

use ego_tree::Tree;
use std::error::Error;
use std::fs;
use std::io::BufReader;
use std::net::{TcpListener, TcpStream};
use std::path::{Path, PathBuf};
use tempfile::TempDir;

#[test]
fn test_source_move_updates_collateral_view_metadata (
) -> Result<(), Box<dyn Error>> {
  let temp_dir : TempDir =
    TempDir::new()?;
  copy_dir_all (
    Path::new ("tests/move_source/fixtures"),
    temp_dir . path() )?;
  let tantivy_folder : PathBuf =
    temp_dir . path() . join ("tantivy");
  let (config, graph, tantivy)
    : (SkgConfig, InRustGraphHandle, TantivyIndex) =
    setup_test_graph (
      temp_dir . path(),
      &tantivy_folder ) ?;

  let (_save_response, collateral_buffer)
    : (SaveResponse, String) =
    block_on ( async {
      let root_ids : Vec<ID> =
        vec![ ID::new ("a") ];
      let (initial_buffer, pids, viewforest)
        : (String, Vec<ID>, Tree<ViewNode>) =
        multi_root_view (
          &config, None, &root_ids, false ) . await?;
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
        &graph . load_full () . graph,
        uri_1 . clone(), viewforest . clone(), &pids );
      views_state . open_views . register_view (
        &graph . load_full () . graph,
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
          &config,
          &tantivy,
          &graph,
          false,
          &Ok ( uri_1 . clone() ),
          &mut views_state ) . await?;
      drop (stream);

      let mut reader : BufReader<TcpStream> =
        BufReader::new (read_end);
      // The stream now also carries a save-relax-lock message (plan_v2 §8.1)
      // before the collateral-view(s); select the collateral-view by its
      // response-type.
      let collateral_msgs : Vec<String> =
        read_all_lp_messages (&mut reader)
        . into_iter ()
        . filter ( |m| m . contains ("collateral-view") )
        . collect ();
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

  cleanup_test_tantivy (Some (config . tantivy_folder . as_path()))?;
  Ok (( )) }

fn setup_test_graph (
  fixtures_root  : &Path,
  tantivy_folder : &Path,
) -> Result<(SkgConfig, InRustGraphHandle, TantivyIndex),
            Box<dyn Error>> {
  let config : SkgConfig =
    load_config_with_overrides (
      fixtures_root . join ("skgconfig.toml")
        . to_str() . unwrap(),
      &[ ("public",  fixtures_root . join ("owned/public")),
         ("private", fixtures_root . join ("owned/private")),
         ("foreign", fixtures_root . join ("foreign")) ] )?;
  let config : SkgConfig =
    SkgConfig {
      tantivy_folder : tantivy_folder . to_path_buf(),
      .. config };
  let graph : InRustGraphHandle = graph_handle_from_config (&config)?;
  let tantivy_index : TantivyIndex =
    create_empty_tantivy_index (&config . tantivy_folder)?;
  Ok ((config, graph, tantivy_index)) }

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
