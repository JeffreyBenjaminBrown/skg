// cargo test --test source_sets
//
// These are feature-first tests for TODO/source-sets/plan.org. They
// intentionally name the source-set API before the implementation
// exists, and should fail until that feature is wired in.

use indoc::indoc;
use ego_tree::{NodeId, Tree};

use skg::dbs::init::wipe_then_init_tantivy_db;
use skg::dbs::filesystem::not_nodes::load_config;
use skg::dbs::typedb::ancestry::AncestryTree;
use skg::dbs::typedb::search::all_graphnodestats::AllGraphNodeStats;
use skg::serve::ViewsState;
use skg::serve::handlers::source_sets::handle_source_set_request;
use skg::serve::handlers::text_search::SearchEnrichmentPayload;
use skg::source_sets::{
  ActiveSourceSet,
  SourceSetName,
  filter_path_to_active_sources_for_test,
  filter_branches_to_active_sources_for_test,
  run_with_source_set_test_db};
use skg::from_text::buffer_to_validated_saveplan;
use skg::from_text::buffer_to_viewnodes::uninterpreted::org_to_uninterpreted_nodes;
use skg::org_to_text::viewforest_to_string;
use skg::to_org::expand::backpath::{
  build_and_integrate_containerward_path_with_source_set,
  integrate_path_that_might_fork_or_cycle_with_source_set};
use skg::to_org::render::content_view::multi_root_view_with_source_set;
use skg::types::maybe_placed_viewnode::maybePlaced_to_placed_tree;
use skg::types::errors::SaveError;
use skg::types::misc::{ID, MSV, SourceName};
use skg::types::nodes::complete::NodeComplete;
use skg::types::save::{DefineNode, SaveNode};
use skg::types::viewnode::{
  ParentIs,
  ViewNode,
  ViewNodeKind,
  viewforest_root_viewnode};
use skg::types::views_state::{OpenViews, ViewState, ViewUri};

use std::collections::{BTreeSet, HashMap, HashSet};
use std::error::Error;
use std::fs;
use std::net::{TcpListener, TcpStream};
use std::path::Path;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};

fn save_ids (
  instructions : &[DefineNode],
) -> Vec<ID> {
  instructions . iter() . filter_map (|instruction| match instruction {
    DefineNode::Save (SaveNode (node)) => Some (node . pid . clone()),
    DefineNode::Delete (_) => None,
  }) . collect() }

fn saved_node_by_id<'a> (
  instructions : &'a [DefineNode],
  id           : &str,
) -> &'a NodeComplete {
  for instruction in instructions {
    if let DefineNode::Save (SaveNode (node)) = instruction {
      if node . pid == ID::from (id) {
        return node; }}}
  panic! ("SaveNode not found: {}", id) }

fn viewforest_from_org (
  input : &str,
) -> Result<Tree<ViewNode>, Box<dyn Error>> {
  let unchecked_viewforest =
    org_to_uninterpreted_nodes (input)? . 0;
  Ok ( maybePlaced_to_placed_tree (unchecked_viewforest)? ) }

fn first_child_id (
  tree : &Tree<ViewNode>,
) -> NodeId {
  tree . root () . first_child () . unwrap () . id () }

fn true_child_ids (
  tree      : &Tree<ViewNode>,
  parent_id : NodeId,
) -> BTreeSet<ID> {
  tree . get (parent_id) . unwrap () . children ()
    . filter_map ( |child| match &child . value () . kind {
      ViewNodeKind::True (node) => Some (node . id . clone ()),
      _ => None, })
    . collect () }

fn connected_tcp_stream_pair (
) -> Result<(TcpStream, TcpStream), Box<dyn Error>> {
  let listener : TcpListener =
    TcpListener::bind ("127.0.0.1:0")?;
  let addr = listener . local_addr ()?;
  let client : TcpStream =
    TcpStream::connect (addr)?;
  let (server, _addr) =
    listener . accept ()?;
  Ok ((server, client)) }

#[test]
fn config_loads_default_source_set_and_named_source_sets (
) -> Result<(), Box<dyn Error>> {
  let config =
    load_config ("tests/source_sets/fixtures/skgconfig.toml")?;
  assert_eq! (
    config . default_source_set_name (),
    &SourceSetName::from ("public"));
  assert_eq! (
    config . source_set_sources (&SourceSetName::from ("public"))?,
    BTreeSet::from ([SourceName::from ("public")]));
  assert_eq! (
    config . source_set_sources (&SourceSetName::from ("all"))?,
    BTreeSet::from ([
      SourceName::from ("private"),
      SourceName::from ("public")]));
  Ok (( )) }

#[test]
fn config_rejects_reserved_all_source_and_source_set_names (
) {
  let source_all =
    load_config ("tests/source_sets/fixtures-invalid/source-all/skgconfig.toml");
  assert! (
    source_all . is_err (),
    "configured source named all must be rejected" );
  let source_set_all =
    load_config ("tests/source_sets/fixtures-invalid/source-set-all/skgconfig.toml");
  assert! (
    source_set_all . is_err (),
	    "user-defined source-set named all must be rejected" );
}

#[test]
fn source_set_switch_closes_views_and_cancels_stale_search_enrichment (
) -> Result<(), Box<dyn Error>> {
  let config =
    load_config ("tests/source_sets/fixtures/skgconfig.toml")?;
  let mut active : ActiveSourceSet =
    ActiveSourceSet::named (
      &config,
      SourceSetName::from ("public"))?;
  let mut views_state : ViewsState =
    ViewsState {
      diff_mode_enabled : false,
      open_views        : OpenViews::new (), };
  views_state . open_views . views . insert (
    ViewUri::SearchView ("shared ranking term" . to_string ()),
    ViewState {
      viewforest : Tree::new (viewforest_root_viewnode ()),
      pids       : HashSet::from ([ID::from ("active-search-hit")]), });
  let enrichment_slot : Arc<Mutex<Option<SearchEnrichmentPayload>>> =
    Arc::new (Mutex::new (Some (SearchEnrichmentPayload {
      terms          : "shared ranking term" . to_string (),
      search_results : vec![ID::from ("active-search-hit")],
      ancestry_by_id : HashMap::new (),
      graphnodestats : AllGraphNodeStats::empty (), })));
  let search_cancelled : Arc<AtomicBool> =
    Arc::new (AtomicBool::new (false));
  let (mut server_stream, _client_stream) =
    connected_tcp_stream_pair ()?;
  handle_source_set_request (
    &mut server_stream,
    "((request . \"set active source set\") (name . \"all\"))",
    &config,
    &mut views_state,
    &mut active,
    &enrichment_slot,
    &search_cancelled);
  assert_eq! (
    active . name,
    SourceSetName::from ("all"),
    "source-set switch should update the active set" );
  assert! (
    views_state . open_views . views . is_empty (),
    "source-set switch should close registered views" );
  assert! (
    enrichment_slot . lock () . unwrap () . is_none (),
    "source-set switch should drop stale search enrichment payloads" );
  assert! (
    search_cancelled . load (Ordering::SeqCst),
    "source-set switch should cancel in-flight search enrichment" );
  Ok (( )) }

#[test]
fn content_view_renders_inactive_contained_nodes_as_placeholders (
) -> Result<(), Box<dyn Error>> {
  run_with_source_set_test_db (
    "skg-test-source-sets-content-placeholder",
    "tests/source_sets/fixtures/skgconfig.toml",
    "/tmp/tantivy-test-source-sets-content-placeholder",
    |config, driver, _tantivy| Box::pin ( async move {
      let active : ActiveSourceSet =
        ActiveSourceSet::named (
          &config,
          SourceSetName::from ("public"))?;
      let (actual, pids, _viewforest) : (String, Vec<ID>, Tree<ViewNode>) =
        multi_root_view_with_source_set (
          driver, config, None,
          &[ID::from ("root")],
          false,
          &active ) . await ?;
      assert! (actual . contains (
        "(skg (inactiveNode (id private-a) (source private))) node from inactive source"),
        "inactive contained node should render as a placeholder: {}",
        actual );
      assert! (actual . contains ("active-a"));
      assert! (actual . contains ("active-b"));
      assert! (
        ! actual . contains ("private title must not leak"),
        "inactive placeholder must not reveal title: {}",
        actual );
      assert! (
        ! pids . contains (&ID::from ("private-a")),
        "initial content traversal should not render inactive-source \
         children as TrueNodes: {:?}",
        pids );
      Ok (( )) } )) }

#[test]
fn diff_view_marks_inactive_placeholders_newhere_and_removedhere_without_content_leak (
) -> Result<(), Box<dyn Error>> {
  run_with_source_set_test_db (
    "skg-test-source-sets-diff-placeholder",
    "tests/source_sets/fixtures/skgconfig.toml",
    "/tmp/tantivy-test-source-sets-diff-placeholder",
    |config, driver, _tantivy| Box::pin ( async move {
      let active : ActiveSourceSet =
        ActiveSourceSet::named (
          &config,
          SourceSetName::from ("public"))?;
      let (actual, _pids, _viewforest) : (String, Vec<ID>, Tree<ViewNode>) =
        multi_root_view_with_source_set (
          driver, config, None,
          &[ID::from ("diff-root")],
          true,
          &active ) . await ?;
      assert! (
        actual . contains (
          "(skg (inactiveNode (id private-new) (source private) (unstaged newM))) node from inactive source"),
        "inactive new-here placeholder should carry only relationship-position diff metadata: {}",
        actual );
      assert! (
        actual . contains (
          "(skg (inactiveNode (id private-removed) (source private) (unstaged removedM))) node from inactive source"),
        "inactive removed-here placeholder should carry only relationship-position diff metadata: {}",
        actual );
      for forbidden in [
        "private new title must not leak",
        "private removed title must not leak",
        "private body must not leak",
        "source private) private",
      ] {
        assert! (
          ! actual . contains (forbidden),
          "inactive diff placeholder leaked '{}': {}",
          forbidden,
          actual ); }
      Ok (( )) } )) }

#[test]
fn search_filters_inactive_sources_before_ranking_and_truncation (
) -> Result<(), Box<dyn Error>> {
  run_with_source_set_test_db (
    "skg-test-source-sets-search-filtering",
    "tests/source_sets/fixtures/skgconfig.toml",
    "/tmp/tantivy-test-source-sets-search-filtering",
    |config, _driver, tantivy| Box::pin ( async move {
      let active : ActiveSourceSet =
        ActiveSourceSet::named (
          &config,
          SourceSetName::from ("public"))?;
      let ids : Vec<ID> =
        skg::serve::handlers::text_search::search_ids_for_source_set_for_test (
          &tantivy,
          &config,
          &active,
          "shared ranking term",
          2 )?;
      assert_eq! (
        ids,
        vec![ID::from ("active-search-hit")],
        "inactive high-scoring hits must be filtered before ranking \
         and display truncation" );
      Ok (( )) } )) }

#[test]
fn saving_inactive_placeholder_moves_and_deletes_update_contains (
) -> Result<(), Box<dyn Error>> {
  run_with_source_set_test_db (
    "skg-test-source-sets-save-placeholders",
    "tests/source_sets/fixtures/skgconfig.toml",
    "/tmp/tantivy-test-source-sets-save-placeholders",
    |config, driver, _tantivy| Box::pin ( async move {
      let moved_buffer = indoc! {"
        * (skg (node (id root) (source public))) root
        ** (skg (node (id active-b) (source public))) active-b
        ** (skg (inactiveNode (id private-a) (source private))) node from inactive source
      "};
      let moved_instructions : Vec<DefineNode> =
        buffer_to_validated_saveplan (
          moved_buffer, config, driver) . await?
        . define_nodes;
      assert_eq! (
        saved_node_by_id (&moved_instructions, "root") . contains,
        vec![ID::from ("active-b"), ID::from ("private-a")],
        "moving an inactive placeholder should reorder the active \
         container's contains list" );
      assert! (
        ! save_ids (&moved_instructions) . contains (&ID::from ("private-a")),
        "inactive placeholder should not produce a SaveNode" );

      let deleted_buffer = indoc! {"
        * (skg (node (id root) (source public))) root
        ** (skg (node (id active-b) (source public))) active-b
      "};
      let deleted_instructions : Vec<DefineNode> =
        buffer_to_validated_saveplan (
          deleted_buffer, config, driver) . await?
        . define_nodes;
      assert_eq! (
        saved_node_by_id (&deleted_instructions, "root") . contains,
        vec![ID::from ("active-b")],
        "deleting an inactive placeholder should remove that inactive \
         ID from contains" );
      Ok (( )) } )) }

#[test]
fn saving_edits_to_inactive_placeholder_content_are_rejected (
) -> Result<(), Box<dyn Error>> {
  run_with_source_set_test_db (
    "skg-test-source-sets-save-placeholder-edit",
    "tests/source_sets/fixtures/skgconfig.toml",
    "/tmp/tantivy-test-source-sets-save-placeholder-edit",
    |config, driver, _tantivy| Box::pin ( async move {
      let buffer = indoc! {"
        * (skg (node (id root) (source public))) root
        ** (skg (inactiveNode (id private-a) (source private))) edited title
        This body edit should be rejected.
      "};
      let result =
        buffer_to_validated_saveplan (
          buffer, config, driver) . await;
      assert! (
        matches! ( result, Err (SaveError::BufferValidationErrors (_)) ),
        "editing inactive placeholder title/body should be rejected: {:?}",
        result );
	      Ok (( )) } )) }

#[test]
fn restricted_source_search_and_save_work_together_end_to_end (
) -> Result<(), Box<dyn Error>> {
  run_with_source_set_test_db (
    "skg-test-source-sets-search-save-e2e",
    "tests/source_sets/fixtures/skgconfig.toml",
    "/tmp/tantivy-test-source-sets-search-save-e2e",
    |config, driver, tantivy| Box::pin ( async move {
      let active : ActiveSourceSet =
        ActiveSourceSet::named (
          &config,
          SourceSetName::from ("public"))?;
      let ids : Vec<ID> =
        skg::serve::handlers::text_search::search_ids_for_source_set_for_test (
          tantivy,
          &config,
          &active,
          "shared ranking term",
          10 )?;
      assert_eq! (
        ids,
        vec![ID::from ("active-search-hit")],
        "restricted search should only return active-source hits" );
      let (rendered, _pids, _viewforest) : (String, Vec<ID>, Tree<ViewNode>) =
        multi_root_view_with_source_set (
          driver, config, None,
          &[ID::from ("root")],
          false,
          &active ) . await ?;
      assert! (
        rendered . contains (
          "(skg (inactiveNode (id private-a) (source private))) node from inactive source"),
        "restricted content view should include an inactive placeholder: {}",
        rendered );
      let edited_buffer = indoc! {"
        * (skg (node (id root) (source public))) root
        ** (skg (node (id active-b) (source public))) active-b edited through restricted view
        ** (skg (inactiveNode (id private-a) (source private))) node from inactive source
      "};
      let instructions : Vec<DefineNode> =
        buffer_to_validated_saveplan (
          edited_buffer, config, driver) . await?
        . define_nodes;
      assert_eq! (
        saved_node_by_id (&instructions, "root") . contains,
        vec![ID::from ("active-b"), ID::from ("private-a")],
        "restricted save should preserve inactive placeholders as \
         content positions" );
      assert! (
        ! save_ids (&instructions) . contains (&ID::from ("private-a")),
        "restricted save should not write inactive-source nodes" );
      assert_eq! (
        saved_node_by_id (&instructions, "active-b") . title,
        "active-b edited through restricted view",
        "restricted save should still write active-source edits" );
      Ok (( )) } )) }

#[test]
fn backward_path_truncates_before_first_inactive_node (
) -> Result<(), Box<dyn Error>> {
  let config =
    load_config ("tests/source_sets/fixtures/skgconfig.toml")?;
  let active : ActiveSourceSet =
    ActiveSourceSet::named (
      &config,
      SourceSetName::from ("public"))?;
  let path : Vec<ID> =
    vec![
      ID::from ("active-container"),
      ID::from ("private-container"),
      ID::from ("active-root-after-private") ];
  assert_eq! (
    filter_path_to_active_sources_for_test (&config, &active, path)?,
    vec![ID::from ("active-container")],
    "mid-path filtering should keep exactly the active prefix \
     and stop before the first inactive node" );
  Ok (( )) }

#[test]
fn backward_path_filters_forks_per_branch_and_omits_empty_forks (
) -> Result<(), Box<dyn Error>> {
  let config =
    load_config ("tests/source_sets/fixtures/skgconfig.toml")?;
  let active : ActiveSourceSet =
    ActiveSourceSet::named (
      &config,
      SourceSetName::from ("public"))?;
  let mixed_branches : BTreeSet<ID> =
    BTreeSet::from ([
      ID::from ("active-fork-branch"),
      ID::from ("private-fork-branch")]);
  assert_eq! (
    filter_branches_to_active_sources_for_test (
      &config, &active, mixed_branches)?,
    BTreeSet::from ([ID::from ("active-fork-branch")]),
    "partially inactive forks should render only active branches" );
  let inactive_branches : BTreeSet<ID> =
    BTreeSet::from ([
      ID::from ("private-fork-branch"),
      ID::from ("private-other-branch")]);
  assert! (
    filter_branches_to_active_sources_for_test (
      &config, &active, inactive_branches)?
    . is_empty (),
    "fully inactive forks should not render an empty fork scaffold" );
  Ok (( )) }

#[test]
fn containerward_expansion_truncates_before_inactive_container (
) -> Result<(), Box<dyn Error>> {
  run_with_source_set_test_db (
    "skg-test-source-sets-containerward-truncation",
    "tests/source_sets/fixtures/skgconfig.toml",
    "/tmp/tantivy-test-source-sets-containerward-truncation",
    |config, driver, _tantivy| Box::pin ( async move {
      let active : ActiveSourceSet =
        ActiveSourceSet::named (
          &config,
          SourceSetName::from ("public"))?;
      let mut viewforest : Tree<ViewNode> =
        viewforest_from_org (indoc! {"
          * (skg (node (id child-for-backpath) (source public))) child-for-backpath
        "})?;
      let child_id : NodeId = first_child_id (&viewforest);
      build_and_integrate_containerward_path_with_source_set (
        &mut viewforest,
        child_id,
        config,
        driver,
        Some (&active)) . await ?;
      let child_children : BTreeSet<ID> =
        true_child_ids (&viewforest, child_id);
      assert_eq! (
        child_children,
        BTreeSet::from ([ID::from ("active-container")]),
        "containerward expansion should keep the active prefix and \
         truncate before the inactive container" );
      let rendered : String =
        viewforest_to_string (&viewforest, config)?;
      assert! (
        ! rendered . contains ("private-container"),
        "inactive container should not render as a placeholder or \
         TrueNode: {}",
        rendered );
      assert! (
        ! rendered . contains ("active-root-after-private"),
        "nodes beyond the first inactive container should be unreachable: {}",
        rendered );
      Ok (( )) } )) }

#[test]
fn sourceward_expansion_filters_forks_per_branch_and_omits_empty_forks (
) -> Result<(), Box<dyn Error>> {
  run_with_source_set_test_db (
    "skg-test-source-sets-sourceward-fork-truncation",
    "tests/source_sets/fixtures/skgconfig.toml",
    "/tmp/tantivy-test-source-sets-sourceward-fork-truncation",
    |config, driver, _tantivy| Box::pin ( async move {
      let active : ActiveSourceSet =
        ActiveSourceSet::named (
          &config,
          SourceSetName::from ("public"))?;
      let mut viewforest : Tree<ViewNode> =
        viewforest_from_org (indoc! {"
          * (skg (node (id child-with-fork) (source public))) child-with-fork
        "})?;
      let child_id : NodeId = first_child_id (&viewforest);
      integrate_path_that_might_fork_or_cycle_with_source_set (
        &mut viewforest,
        child_id,
        Vec::new (),
        HashSet::from ([
          ID::from ("active-fork-branch"),
          ID::from ("private-fork-branch"),
          ID::from ("private-other-branch")]),
        HashSet::new (),
        config,
        driver,
        ParentIs::LinkTarget,
        Some (&active)) . await ?;
      assert_eq! (
        true_child_ids (&viewforest, child_id),
        BTreeSet::from ([ID::from ("active-fork-branch")]),
        "sourceward fork expansion should retain active branches \
         independently and omit inactive branches" );

      let mut empty_fork_viewforest : Tree<ViewNode> =
        viewforest_from_org (indoc! {"
          * (skg (node (id child-with-fork) (source public))) child-with-fork
        "})?;
      let empty_fork_child_id : NodeId =
        first_child_id (&empty_fork_viewforest);
      integrate_path_that_might_fork_or_cycle_with_source_set (
        &mut empty_fork_viewforest,
        empty_fork_child_id,
        Vec::new (),
        HashSet::from ([
          ID::from ("private-fork-branch"),
          ID::from ("private-other-branch")]),
        HashSet::new (),
        config,
        driver,
        ParentIs::LinkTarget,
        Some (&active)) . await ?;
      assert! (
        true_child_ids (&empty_fork_viewforest, empty_fork_child_id)
        . is_empty (),
        "all-inactive sourceward forks should not leave children or \
         empty fork scaffolding" );
      Ok (( )) } )) }

#[test]
fn search_enrichment_truncates_ancestry_before_inactive_container (
) -> Result<(), Box<dyn Error>> {
  let config =
    load_config ("tests/source_sets/fixtures/skgconfig.toml")?;
  let active : ActiveSourceSet =
    ActiveSourceSet::named (
      &config,
      SourceSetName::from ("public"))?;
  let mut result_node : NodeComplete =
    skg::types::nodes::complete::empty_node_complete ();
  result_node . pid = ID::from ("active-search-hit");
  result_node . title = "active search hit" . to_string ();
  result_node . source = SourceName::from ("public");
  result_node . aliases =
    MSV::Specified (vec!["search term" . to_string ()]);
  let mut active_container : NodeComplete =
    skg::types::nodes::complete::empty_node_complete ();
  active_container . pid = ID::from ("active-container");
  active_container . title = "active-container" . to_string ();
  active_container . source = SourceName::from ("public");
  let mut private_container : NodeComplete =
    skg::types::nodes::complete::empty_node_complete ();
  private_container . pid = ID::from ("private-container");
  private_container . title =
    "private container title must not leak" . to_string ();
  private_container . source = SourceName::from ("private");
  let index_dir : &str =
    "/tmp/tantivy-test-source-sets-search-enrichment-truncation";
  let (tantivy, _count) =
    wipe_then_init_tantivy_db (
      &[ result_node, active_container, private_container ],
      Path::new (index_dir))?;
  let mut matches_by_id =
    skg::serve::handlers::text_search::MatchGroups::new ();
  matches_by_id . insert (
    ID::from ("active-search-hit"),
    ( SourceName::from ("public"),
      vec![(1.0, "active search hit" . to_string ())] ));
  let ancestry_by_id : HashMap<ID, AncestryTree> =
    HashMap::from ([(
      ID::from ("active-search-hit"),
      AncestryTree::Inner (
        ID::from ("active-search-hit"),
        vec![AncestryTree::Inner (
          ID::from ("active-container"),
          vec![AncestryTree::Root (
            ID::from ("private-container"))])]))]);
  let rendered : String =
    skg::serve::handlers::text_search
      ::enriched_search_buffer_for_source_set_for_test (
        "search term",
        &matches_by_id,
        &[ID::from ("active-search-hit")],
        &ancestry_by_id,
        &tantivy,
        &config,
        &active)?;
  assert! (
    rendered . contains ("active-container"),
    "active ancestry should render: {}",
    rendered );
  assert! (
    ! rendered . contains ("private-container"),
    "inactive enrichment ancestry should be truncated before the \
     inactive container: {}",
    rendered );
  assert! (
    ! rendered . contains ("private container title must not leak"),
    "inactive enrichment ancestry must not reveal title text: {}",
    rendered );
  if Path::new (index_dir) . exists () {
    fs::remove_dir_all (index_dir)?; }
  Ok (( )) }

#[test]
fn titles_by_ids_omits_inactive_source_titles (
) -> Result<(), Box<dyn Error>> {
  let config =
    load_config ("tests/source_sets/fixtures/skgconfig.toml")?;
  let active : ActiveSourceSet =
    ActiveSourceSet::named (
      &config,
      SourceSetName::from ("public"))?;
  let titles =
    skg::serve::handlers::titles_by_ids::titles_by_ids_for_source_set_for_test (
      &config,
      &active,
      &[ ID::from ("active-a"),
         ID::from ("private-a") ])?;
  assert_eq! (
    titles . get (&ID::from ("active-a")),
    Some (&"active-a" . to_string ()));
  assert! (
    ! titles . contains_key (&ID::from ("private-a")),
    "inactive-source title lookup must omit private-a" );
  Ok (( )) }
