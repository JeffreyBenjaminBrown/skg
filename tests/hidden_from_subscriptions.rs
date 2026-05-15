// cargo test --test hidden_from_subscriptions -- --nocapture
//
// Tests for HiddenOutsideOfSubscribeeCol, HiddenInSubscribeeCol, and HiddenFromSubscribees.
// These test that:
// 1. Initial view shows subscribees as indef with HiddenOutsideOfSubscribeeCol
// 2. After saving with definitive view requests, HiddenInSubscribeeCol is shown

use indoc::indoc;
use skg::dbs::init::{overwrite_new_empty_typedb_db, read_and_use_schema, create_empty_tantivy_index};
use skg::dbs::filesystem::not_nodes::load_config_with_overrides;
use skg::dbs::filesystem::multiple_nodes::read_all_skg_files_from_sources;
use skg::dbs::typedb::nodes::create_all_nodes;
use skg::dbs::typedb::relationships::create_all_relationships;
use skg::test_utils::{
  TestDbGuard,
  cleanup_test_tantivy_and_typedb_dbs,
  extract_string_field_from_sexp,
  read_all_lp_messages,
  update_from_and_rerender_buffer_test as update_from_and_rerender_buffer };
use skg::to_org::render::content_view::single_root_view;
use skg::types::misc::{SkgConfig, ID, TantivyIndex};
use skg::types::nodes::typedb::NodeTypedb;
use skg::types::nodes::complete::NodeComplete;
use std::sync::Arc;
use std::fs;
use std::path::{Path, PathBuf};

use skg::serve::ViewsState;
use skg::types::views_state::{OpenViews, ViewUri};
use skg::dbs::in_rust_graph::{InRustGraph, InRustGraphHandle, new_handle};

use futures::executor::block_on;
use std::error::Error;
use std::io::BufReader;
use std::net::TcpStream;
use typedb_driver::{TypeDBDriver, Credentials, DriverOptions};

fn mk_test_tcp_stream ()
  -> TcpStream
{ let listener : std::net::TcpListener =
    std::net::TcpListener::bind ("127.0.0.1:0") . unwrap ();
  let addr : std::net::SocketAddr =
    listener . local_addr () . unwrap ();
  let write_end : TcpStream =
    TcpStream::connect (addr) . unwrap ();
  write_end }

fn mk_test_tcp_stream_pair ()
  -> (TcpStream, TcpStream)
{ let listener : std::net::TcpListener =
    std::net::TcpListener::bind ("127.0.0.1:0") . unwrap ();
  let addr : std::net::SocketAddr =
    listener . local_addr () . unwrap ();
  let write_end : TcpStream =
    TcpStream::connect (addr) . unwrap ();
  let (read_end, _) =
    listener . accept () . unwrap ();
  (write_end, read_end) }

async fn setup_test(
  db_name: &str,
  config_path: &str,
) -> Result<(SkgConfig,
             Arc<TypeDBDriver>,
             TantivyIndex),
            Box<dyn Error>> {
  let config: SkgConfig =
    load_config_with_overrides(config_path, Some (db_name), &[])?;
  let driver: TypeDBDriver = TypeDBDriver::new(
    "127.0.0.1:1729",
    Credentials::new("admin", "password"),
    DriverOptions::new(false, None)?,
  ) . await?;
  let nodes: Vec<NodeComplete> =
    read_all_skg_files_from_sources (&config)?;
  let typedb_nodes : Vec<NodeTypedb> =
    nodes . iter ()
    . map (NodeTypedb::from_complete_parsing_textlinks)
    . collect ();
  overwrite_new_empty_typedb_db(db_name, &driver) . await?;
  read_and_use_schema(db_name, &driver) . await?;
  create_all_nodes(db_name, &driver, &typedb_nodes) . await?;
  create_all_relationships(db_name, &driver, &typedb_nodes) . await?;
  let tantivy_index: TantivyIndex =
    create_empty_tantivy_index(&config . tantivy_folder)?;
  Ok((config, Arc::new (driver), tantivy_index)) }

async fn cleanup_test(
  db_name: &str,
  driver: &Arc<TypeDBDriver>,
  tantivy_folder: &std::path::Path,
) -> Result<(), Box<dyn Error>> {
  cleanup_test_tantivy_and_typedb_dbs (
    db_name, driver, Some (tantivy_folder) ) . await }

/// Add (viewRequests definitiveView) to all subscribee nodes in org text.
/// Modifies the node section of each subscribee to request a definitive view.
/// Subscribees are TrueNodes under SubscribeeCol scaffolds.
///
/// KLUDGE: We identify subscribees by matching on "subscribee-" in the title.
/// That's easier than navigating the org-tree's topoogy.
fn add_definitive_view_request_to_subscribees (
  org_text : &str,
) -> String {
  // Process line-by-line, inserting viewRequests after indefinitive
  // for lines containing "subscribee-" in the title
  org_text
    . lines()
    . map(|line| {
      if line . contains ("subscribee-") && line . contains ("indef") {
        // Insert viewRequests after the `indef' marker, before ) or (graphStats.
        line . replace("indef)", "indef (viewRequests definitiveView))")
            . replace("indef (graphStats", "indef (viewRequests definitiveView) (graphStats")
      } else {
        line . to_string()
      }
    })
    . collect::<Vec<_>>()
    . join ("\n") + "\n" }

fn copy_dir_all (
  src : &Path,
  dst : &Path,
) -> Result<(), Box<dyn Error>> {
  fs::create_dir_all (dst)?;
  for entry in fs::read_dir (src)? {
    let entry : fs::DirEntry = entry?;
    let file_type : fs::FileType = entry . file_type()?;
    let target : PathBuf = dst . join (entry . file_name());
    if file_type . is_dir() {
      copy_dir_all (&entry . path(), &target)?;
    } else {
      fs::copy (entry . path(), target)?; }}
  Ok (( )) }

async fn setup_temp_test(
  db_name: &str,
  fixture_dir: &str,
) -> Result<(SkgConfig,
             Arc<TypeDBDriver>,
             TantivyIndex,
             PathBuf),
            Box<dyn Error>> {
  let temp_fixture_dir : PathBuf =
    PathBuf::from (format! ("/tmp/{}-fixtures", db_name));
  if temp_fixture_dir . exists() {
    fs::remove_dir_all (&temp_fixture_dir)?; }
  copy_dir_all (Path::new (fixture_dir), &temp_fixture_dir)?;
  let config_path : PathBuf =
    temp_fixture_dir . join ("skgconfig.toml");
  let (config, driver, tantivy) =
    setup_test (db_name, config_path . to_str() . unwrap()) . await?;
  Ok ((config, driver, tantivy, temp_fixture_dir)) }

async fn save_buffer_for_hidden_subscriptions_test (
  buffer      : &str,
  driver      : &Arc<TypeDBDriver>,
  config      : &SkgConfig,
  tantivy     : &mut TantivyIndex,
  graph       : &InRustGraphHandle,
  views_state : &mut ViewsState,
) -> Result<String, Box<dyn Error>> {
  let mut stream : TcpStream = mk_test_tcp_stream ();
  let response = update_from_and_rerender_buffer (
    &mut stream,
    buffer, driver, config, tantivy, graph, false,

    &Err ( String::new () ), views_state
  ) . await ?;
  Ok (response . saved_view) }

async fn save_buffer_and_read_collateral_views (
  buffer      : &str,
  uri         : &ViewUri,
  driver      : &Arc<TypeDBDriver>,
  config      : &SkgConfig,
  tantivy     : &mut TantivyIndex,
  graph       : &InRustGraphHandle,
  views_state : &mut ViewsState,
) -> Result<(String, Vec<String>), Box<dyn Error>> {
  let (mut stream, read_end) : (TcpStream, TcpStream) =
    mk_test_tcp_stream_pair ();
  let response =
    update_from_and_rerender_buffer (
      &mut stream,
      buffer, driver, config, tantivy, graph, false,
      &Ok (uri . clone ()), views_state
    ) . await ?;
  drop (stream);
  let mut reader : BufReader<TcpStream> =
    BufReader::new (read_end);
  let collateral_views : Vec<String> =
    read_all_lp_messages (&mut reader)
    . into_iter()
    . map ( |msg|
      extract_string_field_from_sexp (&msg, "content")
      . unwrap_or_else (||
        panic! ("content field not found in collateral-view sexp: {}",
                msg)) )
    . collect();
  Ok ((response . saved_view, collateral_views)) }

fn remove_e1_subtree (
  buffer : &str,
) -> String {
  buffer
    . lines()
    . filter ( |line| {
      ! line . contains ("(id e1)")
      && ! line . contains ("(id e11)") })
    . collect::<Vec<_>>()
    . join ("\n") + "\n" }

fn insert_before_r1 (
  buffer : &str,
  inserted : &str,
) -> String {
  buffer . replace (
    "** (skg (node (id r1)",
    &format! ("{}** (skg (node (id r1)", inserted)) }

fn expanded_subscribee_edit_view (
  initial_view : &str,
  edit_kind    : &str,
) -> String {
  let without_e1 : String = remove_e1_subtree (initial_view);
  match edit_kind {
    "delete" => without_e1,
    "move_to_r" => insert_before_r1 (
      &without_e1,
      "** (skg (node (id e1) (source foreign) indef)) e1\n" ),
    "move_to_a" => format! (
      "{}* (skg (node (id a) (source owned))) a\n** (skg (node (id e1) (source foreign) indef)) e1\n",
      without_e1 ),
    _ => panic! ("unknown edit kind: {}", edit_kind), }}

fn assert_hides_e1_in_subscribee_col (
  buffer : &str,
) {
  assert! (
    buffer . contains ("**** (skg hiddenInSubscribeeCol) hidden from this subscription\n***** (skg (node (id e1) (source foreign) indef"),
    "Expected e1 to be rendered under HiddenInSubscribeeCol:\n{}",
    buffer );
  assert! (
    ! buffer . lines() . any ( |line|
      line . starts_with ("**** (skg (node (id e1)") ),
    "Expected e1 not to remain visible as direct subscribee content:\n{}",
    buffer ); }

fn assert_does_not_hide_e1 (
  buffer : &str,
) {
  assert! (
    ! buffer . contains ("hiddenInSubscribeeCol"),
    "Expected no HiddenInSubscribeeCol for e1:\n{}",
    buffer ); }

fn move_h_from_hiddenin_col_to_visible_subscribee_content (
  buffer : &str,
) -> String {
  buffer
    . lines()
    . filter_map ( |line| {
      if line . contains ("(skg hiddenInSubscribeeCol)") {
        None
      } else if line . contains ("(id H)") {
        Some (line . replacen ("***** ", "**** ", 1))
      } else {
        Some (line . to_string()) }})
    . collect::<Vec<_>>()
    . join ("\n") + "\n" }

fn remove_hiddenin_branch (
  buffer : &str,
) -> String {
  buffer
    . lines()
    . filter ( |line|
      ! line . contains ("(skg hiddenInSubscribeeCol)")
      && ! line . contains ("(id H)") )
    . collect::<Vec<_>>()
    . join ("\n") + "\n" }

fn add_e11_to_hiddenoutside_col (
  buffer : &str,
) -> String {
  insert_after_line_containing (
    buffer,
    "(skg hiddenOutsideOfSubscribeeCol)",
    "**** (skg (node (id E11) (source main) indef)) E11" ) }

fn assert_e1_removed_from_visible_subscribee_branch (
  buffer : &str,
) {
  assert! (
    ! buffer . lines() . any ( |line|
      line . starts_with ("**** (skg (node (id e1)") ),
    "Expected e1 not to be regenerated as direct subscribee content:\n{}",
    buffer );
  assert! (
    buffer . contains ("**** (skg (node (id e2)"),
    "Expected unrelated direct subscribee content e2 to remain visible:\n{}",
    buffer ); }

fn remove_e1_and_e2_subtrees (
  buffer : &str,
) -> String {
  buffer
    . lines()
    . filter ( |line| {
      ! line . contains ("(id e1)")
      && ! line . contains ("(id e11)")
      && ! line . contains ("(id e2)")
      && ! line . contains ("(id e12)") })
    . collect::<Vec<_>>()
    . join ("\n") + "\n" }

fn insert_after_line_containing (
  buffer     : &str,
  needle     : &str,
  insertion  : &str,
) -> String {
  let mut result : Vec<String> = Vec::new();
  for line in buffer . lines() {
    result . push (line . to_string());
    if line . contains (needle) {
      result . extend (
        insertion . lines() . map ( |l| l . to_string() )); }}
  result . join ("\n") + "\n" }

fn assert_line_contains_all (
  buffer : &str,
  parts  : &[&str],
) {
  assert!(
    buffer . lines() . any ( |line|
      parts . iter() . all ( |part| line . contains (part) )),
    "Expected a line containing {:?}:\n{}",
    parts, buffer ); }

fn assert_line_order (
  buffer : &str,
  earlier : &str,
  later   : &str,
) {
  let earlier_pos : usize =
    buffer . find (earlier) . unwrap_or_else (||
      panic! ("Expected {:?} in:\n{}", earlier, buffer));
  let later_pos : usize =
    buffer . find (later) . unwrap_or_else (||
      panic! ("Expected {:?} in:\n{}", later, buffer));
  assert!(
    earlier_pos < later_pos,
    "Expected {:?} before {:?} in:\n{}",
    earlier, later, buffer ); }

fn node_from_disk (
  config : &SkgConfig,
  pid    : &str,
) -> Result<NodeComplete, Box<dyn Error>> {
  let id : ID = ID::from (pid);
  read_all_skg_files_from_sources (config)?
    . into_iter()
    . find ( |node| node . pid == id )
    . ok_or_else ( || format! ("node not found on disk: {}", pid) . into() ) }

#[test]
fn test_deleting_foreign_subscribee_content_preserves_branch_edit(
) -> Result<(), Box<dyn Error>> {
  block_on(async {
    let db_name = "skg-test-delete-subscribee-content-preserves-branch";
    let mut guard : TestDbGuard =
      TestDbGuard::new (db_name, None);
    let (config, driver, mut tantivy, temp_fixture_dir) =
      setup_temp_test (
        db_name,
        "tests/hidden_from_subscriptions/fixtures-subscribee-edit"
      ) . await?;
    let graph : InRustGraphHandle =
      new_handle (InRustGraph::new ());
    let mut views_state : ViewsState = ViewsState {
      diff_mode_enabled : false,
      open_views        : OpenViews::new (),};

    let (initial_view, _pids, _)
      : (String, Vec<ID>, _)
      = single_root_view(
          &driver, &config, None,
          &ID ("r" . to_string()),
          false ) . await?;
    let expanded : String =
      save_buffer_for_hidden_subscriptions_test (
        &add_definitive_view_request_to_subscribees (&initial_view),
        &driver, &config, &mut tantivy, &graph, &mut views_state
      ) . await?;
    let edited : String =
      expanded_subscribee_edit_view (&expanded, "delete");
    let rerendered : String =
      save_buffer_for_hidden_subscriptions_test (
        &edited, &driver, &config, &mut tantivy, &graph, &mut views_state
      ) . await?;

    assert_e1_removed_from_visible_subscribee_branch (&rerendered);
    cleanup_test (
      db_name, &driver, &config . tantivy_folder ) . await?;
    guard . disarm ();
    if temp_fixture_dir . exists() {
      fs::remove_dir_all (temp_fixture_dir)?; }
    Ok (( )) }) }

#[test]
fn test_deleting_foreign_subscribee_content_infers_hide(
) -> Result<(), Box<dyn Error>> {
  block_on(async {
    let db_name = "skg-test-delete-subscribee-content-infers-hide";
    let mut guard : TestDbGuard =
      TestDbGuard::new (db_name, None);
    let (config, driver, mut tantivy, temp_fixture_dir) =
      setup_temp_test (
        db_name,
        "tests/hidden_from_subscriptions/fixtures-subscribee-edit"
      ) . await?;
    let graph : InRustGraphHandle =
      new_handle (InRustGraph::new ());
    let mut views_state : ViewsState = ViewsState {
      diff_mode_enabled : false,
      open_views        : OpenViews::new (),};

    let (initial_view, _pids, _)
      : (String, Vec<ID>, _)
      = single_root_view(
          &driver, &config, None,
          &ID ("r" . to_string()),
          false ) . await?;
    let expanded : String =
      save_buffer_for_hidden_subscriptions_test (
        &add_definitive_view_request_to_subscribees (&initial_view),
        &driver, &config, &mut tantivy, &graph, &mut views_state
      ) . await?;
    let edited : String =
      expanded_subscribee_edit_view (&expanded, "delete");
    let rerendered : String =
      save_buffer_for_hidden_subscriptions_test (
        &edited, &driver, &config, &mut tantivy, &graph, &mut views_state
      ) . await?;

    assert_hides_e1_in_subscribee_col (&rerendered);
    let r_skg : NodeComplete =
      node_from_disk (&config, "r")?;
    assert_eq!(
      r_skg . hides_from_its_subscriptions . or_default(),
      &[ID::from ("e1")],
      "Expected inferred hide to be persisted in r.skg: {:?}",
      r_skg . hides_from_its_subscriptions );
    cleanup_test (
      db_name, &driver, &config . tantivy_folder ) . await?;
    guard . disarm ();
    if temp_fixture_dir . exists() {
      fs::remove_dir_all (temp_fixture_dir)?; }
    Ok (( )) }) }

#[test]
fn test_collateral_view_reflects_newly_hidden_subscribee_content(
) -> Result<(), Box<dyn Error>> {
  block_on(async {
    let db_name = "skg-test-collateral-subscribee-hide";
    let mut guard : TestDbGuard =
      TestDbGuard::new (db_name, None);
    let (config, driver, mut tantivy, temp_fixture_dir) =
      setup_temp_test (
        db_name,
        "tests/hidden_from_subscriptions/fixtures-subscribee-edit"
      ) . await?;
    let graph : InRustGraphHandle =
      new_handle (InRustGraph::new ());
    let mut views_state : ViewsState = ViewsState {
      diff_mode_enabled : false,
      open_views        : OpenViews::new (),};
    let saved_uri : ViewUri =
      ViewUri::ContentView (
        "collateral-hide-saved" . to_string());
    let collateral_uri : ViewUri =
      ViewUri::ContentView (
        "collateral-hide-other" . to_string());

    let (initial_view, pids, viewforest) =
      single_root_view(
        &driver, &config, None,
        &ID ("r" . to_string()),
        false ) . await?;
    views_state . open_views . register_view (
      saved_uri . clone(), viewforest, &pids);

    let (expanded, collateral_views) =
      save_buffer_and_read_collateral_views (
        &add_definitive_view_request_to_subscribees (&initial_view),
        &saved_uri, &driver, &config, &mut tantivy, &graph,
        &mut views_state ) . await?;
    assert!(
      collateral_views . is_empty(),
      "No collateral view should be registered yet: {:?}",
      collateral_views);

    let expanded_viewforest =
      views_state . open_views . viewuri_to_view (&saved_uri)
      . expect ("saved view should be registered")
      . clone();
    let expanded_pids : Vec<ID> =
      views_state . open_views . viewuri_to_pids (&saved_uri);
    views_state . open_views . register_view (
      collateral_uri, expanded_viewforest, &expanded_pids);

    let edited : String =
      expanded_subscribee_edit_view (&expanded, "delete");
    let (_saved_view, collateral_views) =
      save_buffer_and_read_collateral_views (
        &edited, &saved_uri, &driver, &config, &mut tantivy, &graph,
        &mut views_state ) . await?;

    assert_eq!(
      collateral_views . len(), 1,
      "Expected one collateral view:\n{:?}",
      collateral_views);
    assert_hides_e1_in_subscribee_col (&collateral_views[0]);

    cleanup_test (
      db_name, &driver, &config . tantivy_folder ) . await?;
    guard . disarm ();
    if temp_fixture_dir . exists() {
      fs::remove_dir_all (temp_fixture_dir)?; }
    Ok (( )) }) }

#[test]
fn test_moving_foreign_subscribee_content_to_subscriber_does_not_hide(
) -> Result<(), Box<dyn Error>> {
  block_on(async {
    let db_name = "skg-test-move-subscribee-content-to-subscriber";
    let mut guard : TestDbGuard =
      TestDbGuard::new (db_name, None);
    let (config, driver, mut tantivy, temp_fixture_dir) =
      setup_temp_test (
        db_name,
        "tests/hidden_from_subscriptions/fixtures-subscribee-edit"
      ) . await?;
    let graph : InRustGraphHandle =
      new_handle (InRustGraph::new ());
    let mut views_state : ViewsState = ViewsState {
      diff_mode_enabled : false,
      open_views        : OpenViews::new (),};

    let (initial_view, _pids, _)
      : (String, Vec<ID>, _)
      = single_root_view(
          &driver, &config, None,
          &ID ("r" . to_string()),
          false ) . await?;
    let expanded : String =
      save_buffer_for_hidden_subscriptions_test (
        &add_definitive_view_request_to_subscribees (&initial_view),
        &driver, &config, &mut tantivy, &graph, &mut views_state
      ) . await?;
    let edited : String =
      expanded_subscribee_edit_view (&expanded, "move_to_r");
    let rerendered : String =
      save_buffer_for_hidden_subscriptions_test (
        &edited, &driver, &config, &mut tantivy, &graph, &mut views_state
      ) . await?;

    assert_does_not_hide_e1 (&rerendered);
    assert! (
      rerendered . contains (
        "** (skg (node (id e1) (source foreign) indef" ),
      "Expected e1 to remain ordinary content of r:\n{}",
      rerendered );
    cleanup_test (
      db_name, &driver, &config . tantivy_folder ) . await?;
    guard . disarm ();
    if temp_fixture_dir . exists() {
      fs::remove_dir_all (temp_fixture_dir)?; }
    Ok (( )) }) }

#[test]
fn test_moving_foreign_subscribee_content_elsewhere_still_hides(
) -> Result<(), Box<dyn Error>> {
  block_on(async {
    let db_name = "skg-test-move-subscribee-content-elsewhere";
    let mut guard : TestDbGuard =
      TestDbGuard::new (db_name, None);
    let (config, driver, mut tantivy, temp_fixture_dir) =
      setup_temp_test (
        db_name,
        "tests/hidden_from_subscriptions/fixtures-subscribee-edit"
      ) . await?;
    let graph : InRustGraphHandle =
      new_handle (InRustGraph::new ());
    let mut views_state : ViewsState = ViewsState {
      diff_mode_enabled : false,
      open_views        : OpenViews::new (),};

    let (initial_view, _pids, _)
      : (String, Vec<ID>, _)
      = single_root_view(
          &driver, &config, None,
          &ID ("r" . to_string()),
          false ) . await?;
    let expanded : String =
      save_buffer_for_hidden_subscriptions_test (
        &add_definitive_view_request_to_subscribees (&initial_view),
        &driver, &config, &mut tantivy, &graph, &mut views_state
      ) . await?;
    let edited : String =
      expanded_subscribee_edit_view (&expanded, "move_to_a");
    let rerendered : String =
      save_buffer_for_hidden_subscriptions_test (
        &edited, &driver, &config, &mut tantivy, &graph, &mut views_state
      ) . await?;

    assert_hides_e1_in_subscribee_col (&rerendered);
    assert! (
      rerendered . contains (
        "** (skg (node (id e1) (source foreign) indef" ),
      "Expected e1 to remain content of a:\n{}",
      rerendered );
    cleanup_test (
      db_name, &driver, &config . tantivy_folder ) . await?;
    guard . disarm ();
    if temp_fixture_dir . exists() {
      fs::remove_dir_all (temp_fixture_dir)?; }
    Ok (( )) }) }

#[test]
fn test_extra_view_child_under_foreign_subscribee_is_independent(
) -> Result<(), Box<dyn Error>> {
  block_on(async {
    let db_name = "skg-test-extra-foreign-subscribee-child-independent";
    let mut guard : TestDbGuard =
      TestDbGuard::new (db_name, None);
    let (config, driver, mut tantivy, temp_fixture_dir) =
      setup_temp_test (
        db_name,
        "tests/hidden_from_subscriptions/fixtures-subscribee-edit"
      ) . await?;
    let graph : InRustGraphHandle =
      new_handle (InRustGraph::new ());
    let mut views_state : ViewsState = ViewsState {
      diff_mode_enabled : false,
      open_views        : OpenViews::new (),};

    let (initial_view, _pids, _)
      : (String, Vec<ID>, _)
      = single_root_view(
          &driver, &config, None,
          &ID ("r" . to_string()),
          false ) . await?;
    let expanded : String =
      save_buffer_for_hidden_subscriptions_test (
        &add_definitive_view_request_to_subscribees (&initial_view),
        &driver, &config, &mut tantivy, &graph, &mut views_state
      ) . await?;
    let edited : String =
      insert_after_line_containing (
        &remove_e1_and_e2_subtrees (&expanded),
        "(id e)",
        "**** (skg (node (id e2) (source foreign) indef)) e2\n\
         **** (skg (node (id a) (source owned) indef)) a\n\
         **** (skg (node (id e1) (source foreign) indef)) e1" );
    let rerendered : String =
      save_buffer_for_hidden_subscriptions_test (
        &edited, &driver, &config, &mut tantivy, &graph, &mut views_state
      ) . await?;

    assert_line_contains_all (
      &rerendered,
      &["(id a)", "(birth independent)"]);
    assert_line_order (&rerendered, "(id e1)", "(id e2)");
    assert_does_not_hide_e1 (&rerendered);
    let e_skg : NodeComplete =
      node_from_disk (&config, "e")?;
    assert!(
      ! e_skg . contains . contains (&ID::from ("a")),
      "Extra view-child should not become graph-content of e: {:?}",
      e_skg . contains );

    cleanup_test (
      db_name, &driver, &config . tantivy_folder ) . await?;
    guard . disarm ();
    if temp_fixture_dir . exists() {
      fs::remove_dir_all (temp_fixture_dir)?; }
    Ok (( )) }) }

#[test]
fn test_extra_view_child_under_owned_subscribee_is_independent(
) -> Result<(), Box<dyn Error>> {
  block_on(async {
    let db_name = "skg-test-extra-owned-subscribee-child-independent";
    let mut guard : TestDbGuard =
      TestDbGuard::new (db_name, None);
    let (config, driver, mut tantivy, temp_fixture_dir) =
      setup_temp_test (
        db_name,
        "tests/hidden_from_subscriptions/fixtures-subscribee-edit"
      ) . await?;
    let graph : InRustGraphHandle =
      new_handle (InRustGraph::new ());
    let mut views_state : ViewsState = ViewsState {
      diff_mode_enabled : false,
      open_views        : OpenViews::new (),};
    let edited : String =
      indoc! {"
              * (skg (node (id a) (source owned))) a
              ** (skg subscribeeCol) subscribees
              *** (skg (node (id r) (source owned))) r
              **** (skg (node (id r2) (source owned) indef)) r2
              **** (skg (node (id e) (source foreign) indef)) subscribee-e
              **** (skg (node (id r1) (source owned) indef)) r1
              "} . to_string();
    let rerendered : String =
      save_buffer_for_hidden_subscriptions_test (
        &edited, &driver, &config, &mut tantivy, &graph, &mut views_state
      ) . await?;

    assert_line_contains_all (
      &rerendered,
      &["(id e)", "(birth independent)"]);
    assert_line_order (&rerendered, "(id r1)", "(id r2)");
    assert!(
      ! rerendered . contains ("hiddenInSubscribeeCol"),
      "Extra view-child should not infer a hide:\n{}",
      rerendered );
    let r_skg : NodeComplete =
      node_from_disk (&config, "r")?;
    assert!(
      ! r_skg . contains . contains (&ID::from ("e")),
      "Extra view-child should not become graph-content of r: {:?}",
      r_skg . contains );

    cleanup_test (
      db_name, &driver, &config . tantivy_folder ) . await?;
    guard . disarm ();
    if temp_fixture_dir . exists() {
      fs::remove_dir_all (temp_fixture_dir)?; }
    Ok (( )) }) }

/// Every kind of Col:
/// - R subscribes to E1, E2
/// - R hides hidden-in-E1, hidden-in-E2, hidden-for-no-reason
/// - E1 contains hidden-in-E1 (hidden) and E11 (not hidden)
/// - E2 contains E21 (not hidden) and hidden-in-E2 (hidden)
/// - hidden-for-no-reason is not in any subscribee's content
///
/// Tests two views:
/// - Initial view from R: subscribees are indef (bare leaves)
/// - View from R with definitive views expanded at each subscribee
///
/// Also tests ordering rule: HiddenInSubscribeeCol precedes content regardless of .skg order.
/// E2's .skg has [E21, hidden-in-E2] but view shows HiddenInSubscribeeCol before E21.
#[test]
fn test_every_kind_of_col(
) -> Result<(), Box<dyn Error>> {
  block_on(async {
    let db_name = "skg-test-hidden-every-kind-of-col";
    let mut guard : TestDbGuard =
      TestDbGuard::new (db_name, None);
    let (config, driver, mut tantivy) = setup_test(
      db_name,
      "tests/hidden_from_subscriptions/fixtures-every-kind-of-col/skgconfig.toml"
    ) . await?;

    let (initial_view, _pids, _)
      : (String, Vec<ID>, _)
      = single_root_view(
          &driver, &config, None,
          &ID("R" . to_string()), // Initial view from R ("subscribeR")
          false ) . await?;
    println!("Initial view from R:\n{}", initial_view);

    let expected_initial = indoc! {
      "* (skg (node (id R) (source main) (birth independent) (graphStats (contents 1) subscribing))) R
       ** (skg subscribeeCol) it subscribes to these
       *** (skg (node (id E1) (source main) indef (graphStats (containers 0) (contents 2) subscribing))) subscribee-1
       *** (skg (node (id E2) (source main) indef (graphStats (containers 0) (contents 2) subscribing))) subscribee-2
       *** (skg hiddenOutsideOfSubscribeeCol) hidden from all subscriptions
       **** (skg (node (id hidden-for-no-reason) (source main) indef (graphStats (containers 0)))) hidden-for-no-reason
       ** (skg (node (id R1) (source main))) R1
       "};
    assert_eq!(initial_view, expected_initial,
      "Initial view from R: indef subscribees are bare leaves; only HiddenOutsideOfSubscribeeCol shown");

    let expanded = { // Request definitive views, then save
      let modified_view : String =
        add_definitive_view_request_to_subscribees (&initial_view);
      println!("Modified view (with definitive requests):\n{}", modified_view);
      let graph : InRustGraphHandle =
        new_handle (InRustGraph::new ());
      let mut views_state : ViewsState = ViewsState {
        diff_mode_enabled : false,
        open_views            : OpenViews::new (),};
      let mut stream : TcpStream = mk_test_tcp_stream ();
      let response = update_from_and_rerender_buffer (
          &mut stream,
          &modified_view, &driver, &config, &mut tantivy, &graph, false,

          &Err ( String::new () ), &mut views_state
        ) . await ?;
      response . saved_view };
    println!("View from R after save with definitive view requests:\n{}", expanded);

    let expected_expanded = indoc! {
      "* (skg (node (id R) (source main) (birth independent) (graphStats (contents 1) subscribing))) R
       ** (skg subscribeeCol) it subscribes to these
       *** (skg (node (id E1) (source main) (graphStats (containers 0) (contents 2) subscribing))) subscribee-1
       **** (skg hiddenInSubscribeeCol) hidden from this subscription
       ***** (skg (node (id hidden-in-E1) (source main) indef)) hidden-in-E1
       **** (skg (node (id E11) (source main))) E11
       *** (skg (node (id E2) (source main) (graphStats (containers 0) (contents 2) subscribing))) subscribee-2
       **** (skg hiddenInSubscribeeCol) hidden from this subscription
       ***** (skg (node (id hidden-in-E2) (source main) indef)) hidden-in-E2
       **** (skg (node (id E21) (source main))) E21
       *** (skg hiddenOutsideOfSubscribeeCol) hidden from all subscriptions
       **** (skg (node (id hidden-for-no-reason) (source main) indef (graphStats (containers 0)))) hidden-for-no-reason
       ** (skg (node (id R1) (source main))) R1
       "};
    assert_eq!(expanded, expected_expanded,
      "View with expanded subscribees: HiddenInSubscribeeCol shown before content; HiddenOutsideOfSubscribeeCol at end");

    cleanup_test(
      db_name,
      &driver,
      &config . tantivy_folder,
    ) . await?;
    guard . disarm ();
    Ok (( )) } ) }

/// Hidden within but none hidden without:
/// - R subscribes to E1
/// - R hides H
/// - E1 contains [E11, H, E12] where H is hidden and E11, E12 are not
///
/// Tests two views:
/// - Initial view from R: E1 is indef (bare leaf), H doesn't appear
/// - View from R with definitive views expanded at each subscribee: H appears in HiddenInSubscribeeCol before E11 and E12
///
/// No HiddenOutsideOfSubscribeeCol in either state (H is in E1's content).
#[test]
fn test_hidden_within_but_none_without(
) -> Result<(), Box<dyn Error>> {
  block_on(async {
    let db_name = "skg-test-hidden-within-but-none-without";
    let mut guard : TestDbGuard =
      TestDbGuard::new (db_name, None);
    let (config, driver, mut tantivy) = setup_test(
      db_name, "tests/hidden_from_subscriptions/fixtures-hidden-within-but-none-without/skgconfig.toml" ) . await?;
    let (initial_view, _pids, _)
      : (String, Vec<ID>, _)
      = single_root_view( &driver, &config, None,
                          &ID("R" . to_string()), // the root
                          false ) . await?;
    println!("Initial view from R:\n{}", initial_view);

    let expected_initial = indoc! {
      "* (skg (node (id R) (source main) (birth independent) (graphStats (contents 1) subscribing))) R
       ** (skg subscribeeCol) it subscribes to these
       *** (skg (node (id E1) (source main) indef (graphStats (containers 0) (contents 3) subscribing))) subscribee-1
       ** (skg (node (id R1) (source main))) R1
       "};
    assert_eq!(initial_view, expected_initial,
      "Initial view from R: indef subscribee is bare leaf; H doesn't appear");

    let expanded = { // request definitive views, then save
      let modified_view : String =
        add_definitive_view_request_to_subscribees (&initial_view);
      println!("Modified view (with definitive requests):\n{}", modified_view);
      let graph : InRustGraphHandle =
        new_handle (InRustGraph::new ());
      let mut views_state : ViewsState = ViewsState {
        diff_mode_enabled : false,
        open_views            : OpenViews::new (),};
      let mut stream : TcpStream = mk_test_tcp_stream ();
      let response = update_from_and_rerender_buffer (
          &mut stream,
          &modified_view, &driver, &config, &mut tantivy, &graph, false,

          &Err ( String::new () ), &mut views_state
        ) . await ?;
      response . saved_view };
    println!("View from R after save with definitive view requests:\n{}", expanded);

    // HiddenInSubscribeeCol precedes content regardless of .skg order.
    // E1.skg has [E11, H, E12] but view shows HiddenInSubscribeeCol (with H) before E11 and E12.
    let expected_expanded = indoc! {
      "* (skg (node (id R) (source main) (birth independent) (graphStats (contents 1) subscribing))) R
       ** (skg subscribeeCol) it subscribes to these
       *** (skg (node (id E1) (source main) (graphStats (containers 0) (contents 3) subscribing))) subscribee-1
       **** (skg hiddenInSubscribeeCol) hidden from this subscription
       ***** (skg (node (id H) (source main) indef)) H
       **** (skg (node (id E11) (source main))) E11
       **** (skg (node (id E12) (source main))) E12
       ** (skg (node (id R1) (source main))) R1
       "};
    assert_eq!(expanded, expected_expanded,
      "View with expanded subscribees: HiddenInSubscribeeCol with H before E11 and E12");

    cleanup_test(
      db_name,
      &driver,
      &config . tantivy_folder,
    ) . await?;
    guard . disarm ();
    Ok (( )) } ) }

#[test]
fn test_moving_hidden_subscribee_content_to_visible_branch_infers_unhide(
) -> Result<(), Box<dyn Error>> {
  block_on(async {
    let db_name =
      "skg-test-visible-subscribee-content-infers-unhide";
    let mut guard : TestDbGuard =
      TestDbGuard::new (db_name, None);
    let (config, driver, mut tantivy, temp_fixture_dir) =
      setup_temp_test (
        db_name,
        "tests/hidden_from_subscriptions/fixtures-hidden-within-but-none-without"
      ) . await?;
    let graph : InRustGraphHandle =
      new_handle (InRustGraph::new ());
    let mut views_state : ViewsState = ViewsState {
      diff_mode_enabled : false,
      open_views        : OpenViews::new (),};

    let (initial_view, _pids, _)
      : (String, Vec<ID>, _)
      = single_root_view(
          &driver, &config, None,
          &ID ("R" . to_string()),
          false ) . await?;
    let expanded : String =
      save_buffer_for_hidden_subscriptions_test (
        &add_definitive_view_request_to_subscribees (&initial_view),
        &driver, &config, &mut tantivy, &graph, &mut views_state
      ) . await?;
    let edited : String =
      move_h_from_hiddenin_col_to_visible_subscribee_content (
        &expanded );
    let rerendered : String =
      save_buffer_for_hidden_subscriptions_test (
        &edited, &driver, &config, &mut tantivy, &graph, &mut views_state
      ) . await?;

    assert! (
      ! rerendered . contains ("hiddenInSubscribeeCol"),
      "Expected no HiddenInSubscribeeCol after unhiding H:\n{}",
      rerendered );
    assert! (
      rerendered . lines() . any ( |line|
        line . starts_with ("**** (skg (node (id H)") ),
      "Expected H to be visible direct subscribee content:\n{}",
      rerendered );
    let r_skg : NodeComplete =
      node_from_disk (&config, "R")?;
    assert!(
      ! r_skg . hides_from_its_subscriptions
        . or_default()
        . contains (&ID::from ("H")),
      "Expected inferred unhide to be persisted in R.skg: {:?}",
      r_skg . hides_from_its_subscriptions );

    cleanup_test (
      db_name, &driver, &config . tantivy_folder ) . await?;
    guard . disarm ();
    if temp_fixture_dir . exists() {
      fs::remove_dir_all (temp_fixture_dir)?; }
    Ok (( )) }) }

#[test]
fn test_collateral_view_reflects_newly_unhidden_subscribee_content(
) -> Result<(), Box<dyn Error>> {
  block_on(async {
    let db_name =
      "skg-test-collateral-subscribee-unhide";
    let mut guard : TestDbGuard =
      TestDbGuard::new (db_name, None);
    let (config, driver, mut tantivy, temp_fixture_dir) =
      setup_temp_test (
        db_name,
        "tests/hidden_from_subscriptions/fixtures-hidden-within-but-none-without"
      ) . await?;
    let graph : InRustGraphHandle =
      new_handle (InRustGraph::new ());
    let mut views_state : ViewsState = ViewsState {
      diff_mode_enabled : false,
      open_views        : OpenViews::new (),};
    let saved_uri : ViewUri =
      ViewUri::ContentView (
        "collateral-unhide-saved" . to_string());
    let collateral_uri : ViewUri =
      ViewUri::ContentView (
        "collateral-unhide-other" . to_string());

    let (initial_view, pids, viewforest) =
      single_root_view(
        &driver, &config, None,
        &ID ("R" . to_string()),
        false ) . await?;
    views_state . open_views . register_view (
      saved_uri . clone(), viewforest, &pids);

    let (expanded, collateral_views) =
      save_buffer_and_read_collateral_views (
        &add_definitive_view_request_to_subscribees (&initial_view),
        &saved_uri, &driver, &config, &mut tantivy, &graph,
        &mut views_state ) . await?;
    assert!(
      collateral_views . is_empty(),
      "No collateral view should be registered yet: {:?}",
      collateral_views);

    let expanded_viewforest =
      views_state . open_views . viewuri_to_view (&saved_uri)
      . expect ("saved view should be registered")
      . clone();
    let expanded_pids : Vec<ID> =
      views_state . open_views . viewuri_to_pids (&saved_uri);
    views_state . open_views . register_view (
      collateral_uri, expanded_viewforest, &expanded_pids);

    let edited : String =
      move_h_from_hiddenin_col_to_visible_subscribee_content (
        &expanded );
    let (_saved_view, collateral_views) =
      save_buffer_and_read_collateral_views (
        &edited, &saved_uri, &driver, &config, &mut tantivy, &graph,
        &mut views_state ) . await?;

    assert_eq!(
      collateral_views . len(), 1,
      "Expected one collateral view:\n{:?}",
      collateral_views);
    assert!(
      ! collateral_views[0] . contains ("hiddenInSubscribeeCol"),
      "Expected collateral view to remove HiddenInSubscribeeCol:\n{}",
      collateral_views[0]);
    assert! (
      collateral_views[0] . lines() . any ( |line|
        line . starts_with ("**** (skg (node (id H)") ),
      "Expected H to be visible direct subscribee content in collateral view:\n{}",
      collateral_views[0] );

    cleanup_test (
      db_name, &driver, &config . tantivy_folder ) . await?;
    guard . disarm ();
    if temp_fixture_dir . exists() {
      fs::remove_dir_all (temp_fixture_dir)?; }
    Ok (( )) }) }

#[test]
fn test_deleting_from_hiddenin_col_does_not_unhide(
) -> Result<(), Box<dyn Error>> {
  block_on(async {
    let db_name =
      "skg-test-hiddenin-col-delete-is-read-only";
    let mut guard : TestDbGuard =
      TestDbGuard::new (db_name, None);
    let (config, driver, mut tantivy, temp_fixture_dir) =
      setup_temp_test (
        db_name,
        "tests/hidden_from_subscriptions/fixtures-hidden-within-but-none-without"
      ) . await?;
    let graph : InRustGraphHandle =
      new_handle (InRustGraph::new ());
    let mut views_state : ViewsState = ViewsState {
      diff_mode_enabled : false,
      open_views        : OpenViews::new (),};

    let (initial_view, _pids, _)
      : (String, Vec<ID>, _)
      = single_root_view(
          &driver, &config, None,
          &ID ("R" . to_string()),
          false ) . await?;
    let expanded : String =
      save_buffer_for_hidden_subscriptions_test (
        &add_definitive_view_request_to_subscribees (&initial_view),
        &driver, &config, &mut tantivy, &graph, &mut views_state
      ) . await?;
    let edited : String =
      remove_hiddenin_branch (&expanded);
    let rerendered : String =
      save_buffer_for_hidden_subscriptions_test (
        &edited, &driver, &config, &mut tantivy, &graph, &mut views_state
      ) . await?;

    assert! (
      rerendered . contains (
        "**** (skg hiddenInSubscribeeCol) hidden from this subscription\n***** (skg (node (id H)"),
      "Expected H to be regenerated under HiddenInSubscribeeCol:\n{}",
      rerendered );
    assert! (
      ! rerendered . lines() . any ( |line|
        line . starts_with ("**** (skg (node (id H)") ),
      "Expected deleting from HiddenInSubscribeeCol not to unhide H:\n{}",
      rerendered );

    cleanup_test (
      db_name, &driver, &config . tantivy_folder ) . await?;
    guard . disarm ();
    if temp_fixture_dir . exists() {
      fs::remove_dir_all (temp_fixture_dir)?; }
    Ok (( )) }) }

/// Hidden without but none hidden within:
/// - R subscribes to E1, E2
/// - R hides H
/// - H is NOT in any subscribee's content
/// - E1 contains [E11, E12], E12 contains [E121]
/// - E2 has no content
///
/// Tests two views:
/// - Initial view from R: E1, E2 are indef (bare leaves), H in HiddenOutsideOfSubscribeeCol
/// - View from R with definitive views expanded at each subscribee: E1 shows E11, E12 (E12 indef); E2 expanded but empty; H still in HiddenOutsideOfSubscribeeCol
///
/// No HiddenInSubscribeeCol in either state (H is not in any subscribee's content).
#[test]
fn test_hidden_without_but_none_within(
) -> Result<(), Box<dyn Error>> {
  block_on(async {
    let db_name = "skg-test-hidden-without-but-none-within";
    let mut guard : TestDbGuard =
      TestDbGuard::new (db_name, None);
    let (config, driver, mut tantivy) =
      setup_test(db_name,
                 "tests/hidden_from_subscriptions/fixtures-hidden-without-but-none-within/skgconfig.toml") . await?;
    let (initial_view, _pids, _)
      : (String, Vec<ID>, _)
      = single_root_view( &driver, &config, None,
                          &ID("R" . to_string()), // origin of the view
                          false ) . await?;
    println!("Initial view from R:\n{}", initial_view);
    let expected_initial = indoc! {
      "* (skg (node (id R) (source main) (birth independent) (graphStats (contents 1) subscribing))) R
       ** (skg subscribeeCol) it subscribes to these
       *** (skg (node (id E1) (source main) indef (graphStats (containers 0) (contents 2) subscribing))) subscribee-1
       *** (skg (node (id E2) (source main) indef (graphStats (containers 0) subscribing))) subscribee-2
       *** (skg hiddenOutsideOfSubscribeeCol) hidden from all subscriptions
       **** (skg (node (id H) (source main) indef (graphStats (containers 0)))) H
       ** (skg (node (id R1) (source main))) R1
       "};
    assert_eq!(initial_view, expected_initial,
      "Initial view from R: H in HiddenOutsideOfSubscribeeCol; E1 and E2 are indef bare leaves");
    let with_subscribees_expanded = {
      let modified_view : String =
        add_definitive_view_request_to_subscribees (&initial_view);
      println!("Modified view (with definitive requests):\n{}", modified_view);
      let graph : InRustGraphHandle =
        new_handle (InRustGraph::new ());
      let mut views_state : ViewsState = ViewsState {
        diff_mode_enabled : false,
        open_views            : OpenViews::new (),};
      let mut stream : TcpStream = mk_test_tcp_stream ();
      let response = update_from_and_rerender_buffer (
        &mut stream,
        &modified_view, &driver, &config, &mut tantivy, &graph, false,

        &Err ( String::new () ), &mut views_state ) . await ?;
      response . saved_view };
    println!("View from R after save with definitive view requests:\n{}",
             with_subscribees_expanded);
    let expected_expanded = indoc! {
      "* (skg (node (id R) (source main) (birth independent) (graphStats (contents 1) subscribing))) R
       ** (skg subscribeeCol) it subscribes to these
       *** (skg (node (id E1) (source main) (graphStats (containers 0) (contents 2) subscribing))) subscribee-1
       **** (skg (node (id E11) (source main))) E11
       **** (skg (node (id E12) (source main) (graphStats (contents 1)))) E12
       ***** (skg (node (id E121) (source main))) E121
       *** (skg (node (id E2) (source main) (graphStats (containers 0) subscribing))) subscribee-2
       *** (skg hiddenOutsideOfSubscribeeCol) hidden from all subscriptions
       **** (skg (node (id H) (source main) indef (graphStats (containers 0)))) H
       ** (skg (node (id R1) (source main))) R1
       "};
    assert_eq!(with_subscribees_expanded, expected_expanded,
      "View with expanded subscribees: H still in HiddenOutsideOfSubscribeeCol (at end); E1 expanded with E11, E12; E2 expanded but empty");

    cleanup_test(
      db_name,
      &driver,
      &config . tantivy_folder,
    ) . await?;
    guard . disarm ();
    Ok (( )) } ) }

#[test]
fn test_adding_to_hiddenoutside_col_does_not_hide(
) -> Result<(), Box<dyn Error>> {
  block_on(async {
    let db_name =
      "skg-test-hiddenoutside-col-add-is-read-only";
    let mut guard : TestDbGuard =
      TestDbGuard::new (db_name, None);
    let (config, driver, mut tantivy, temp_fixture_dir) =
      setup_temp_test (
        db_name,
        "tests/hidden_from_subscriptions/fixtures-hidden-without-but-none-within"
      ) . await?;
    let graph : InRustGraphHandle =
      new_handle (InRustGraph::new ());
    let mut views_state : ViewsState = ViewsState {
      diff_mode_enabled : false,
      open_views        : OpenViews::new (),};

    let (initial_view, _pids, _)
      : (String, Vec<ID>, _)
      = single_root_view(
          &driver, &config, None,
          &ID ("R" . to_string()),
          false ) . await?;
    let edited : String =
      add_e11_to_hiddenoutside_col (&initial_view);
    let rerendered : String =
      save_buffer_for_hidden_subscriptions_test (
        &edited, &driver, &config, &mut tantivy, &graph, &mut views_state
      ) . await?;

    assert! (
      rerendered . contains (
        "*** (skg hiddenOutsideOfSubscribeeCol) hidden from all subscriptions\n**** (skg (node (id H)"),
      "Expected original hidden-outside row H to remain:\n{}",
      rerendered );
    assert! (
      ! rerendered . contains ("**** (skg (node (id E11)"),
      "Expected adding to HiddenOutsideOfSubscribeeCol not to hide E11:\n{}",
      rerendered );

    cleanup_test (
      db_name, &driver, &config . tantivy_folder ) . await?;
    guard . disarm ();
    if temp_fixture_dir . exists() {
      fs::remove_dir_all (temp_fixture_dir)?; }
    Ok (( )) }) }

/// Overlapping hidden within:
/// - R subscribes to E1, E2
/// - R hides H
/// - Both E1 and E2 contain H (and nothing else)
///
/// Tests two views:
/// - Initial view from R: E1, E2 are indef (bare leaves), H doesn't appear
/// - View from R with definitive views expanded at each subscribee: H appears in HiddenInSubscribeeCol under BOTH E1 and E2
///
/// No HiddenOutsideOfSubscribeeCol in either state (H is in subscribees' content).
#[test]
fn test_overlapping_hidden_within(
) -> Result<(), Box<dyn Error>> {
  block_on(async {
    let db_name = "skg-test-overlapping-hidden-within";
    let mut guard : TestDbGuard =
      TestDbGuard::new (db_name, None);
    let (config, driver, mut tantivy) =
      setup_test(db_name,
                 "tests/hidden_from_subscriptions/fixtures-overlapping-hidden-within/skgconfig.toml") . await?;
    let (initial_view, _pids, _)
      : (String, Vec<ID>, _)
      = single_root_view( &driver, &config, None,
                          &ID("R" . to_string()), // root of the view
                          false ) . await?;
    println!("Initial view from R:\n{}", initial_view);
    let expected_initial = indoc! {
      "* (skg (node (id R) (source main) (birth independent) (graphStats (contents 1) subscribing))) R
       ** (skg subscribeeCol) it subscribes to these
       *** (skg (node (id E1) (source main) indef (graphStats (containers 0) (contents 1) subscribing))) subscribee-1
       *** (skg (node (id E2) (source main) indef (graphStats (containers 0) (contents 1) subscribing))) subscribee-2
       ** (skg (node (id R1) (source main))) R1
       "};
    assert_eq!(initial_view, expected_initial,
      "Initial view from R: indef subscribees are bare leaves; H doesn't appear");
    let expanded = {
      let modified_view : String =
        add_definitive_view_request_to_subscribees (&initial_view);
      println!("Modified view (with definitive requests):\n{}",
               modified_view);
      let graph : InRustGraphHandle =
        new_handle (InRustGraph::new ());
      let mut views_state : ViewsState = ViewsState {
        diff_mode_enabled : false,
        open_views            : OpenViews::new (),};
      let mut stream : TcpStream = mk_test_tcp_stream ();
      let response = update_from_and_rerender_buffer (
        &mut stream,
        &modified_view, &driver, &config, &mut tantivy, &graph, false,

        &Err ( String::new () ), &mut views_state ) . await ?;
      response . saved_view };
    println!("View from R after save with definitive view requests:\n{}", expanded);
    let expected_expanded = indoc! {
      "* (skg (node (id R) (source main) (birth independent) (graphStats (contents 1) subscribing))) R
       ** (skg subscribeeCol) it subscribes to these
       *** (skg (node (id E1) (source main) (graphStats (containers 0) (contents 1) subscribing))) subscribee-1
       **** (skg hiddenInSubscribeeCol) hidden from this subscription
       ***** (skg (node (id H) (source main) indef (graphStats (containers 2)))) H
       *** (skg (node (id E2) (source main) (graphStats (containers 0) (contents 1) subscribing))) subscribee-2
       **** (skg hiddenInSubscribeeCol) hidden from this subscription
       ***** (skg (node (id H) (source main) indef (graphStats (containers 2)))) H
       ** (skg (node (id R1) (source main))) R1
       "};
    assert_eq!(expanded, expected_expanded,
      "View with expanded subscribees: H appears in HiddenInSubscribeeCol under both E1 and E2");
    cleanup_test(
      db_name,
      &driver,
      &config . tantivy_folder,
    ) . await?;
    guard . disarm ();
    Ok (( )) } ) }
