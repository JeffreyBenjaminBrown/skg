// cargo nextest run --test grouped_sources -E 'test(source_sets::)'
//
// These are feature-first tests for TODO/source-sets/plan.org. They
// intentionally name the source-set API before the implementation
// exists, and should fail until that feature is wired in.

use indoc::indoc;
use ego_tree::{NodeId, Tree};

use skg::dbs::init::wipe_then_init_tantivy_db;
use skg::dbs::in_rust_graph::relation_accessors::RelationRole;
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
  prepare_git_diff_fixture,
  run_with_source_set_test_db};
use skg::dbs::in_rust_graph::install_or_swap_global_handle;
use skg::to_org::render::content_view::multi_root_view;
use skg::test_utils::run_with_shared_test_db;
use skg::from_text::buffer_to_validated_saveplan;
use skg::from_text::buffer_to_viewnodes::uninterpreted::org_to_uninterpreted_nodes;
use skg::org_to_text::viewforest_to_string;
use skg::to_org::expand::backpath::{
  build_and_integrate_containerward_path_with_source_set,
  integrate_path_that_might_fork_or_cycle_with_source_set};
use skg::to_org::render::content_view::multi_root_view_with_source_set;
use skg::types::maybe_placed_viewnode::maybePlaced_to_placed_tree;
use skg::types::errors::SaveError;
use skg::types::misc::{ID, MSV, SkgConfig, SourceName, TantivyIndex};
use skg::types::nodes::complete::NodeComplete;
use skg::types::save::{DefineNode, SaveNode};
use skg::types::viewnode::{
  Birth,
  ViewNode,
  ViewNodeKind,
  viewforest_root_viewnode};
use skg::types::viewnode::{Vognode, Phantom};
use skg::types::views_state::{OpenViews, ViewState, ViewUri};

use std::collections::{BTreeSet, HashMap, HashSet};
use std::error::Error;
use std::fs;
use std::net::{TcpListener, TcpStream};
use std::path::Path;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};
use typedb_driver::TypeDBDriver;

#[test]
fn all_tests
  () -> Result<(), Box<dyn Error>> {
  let fixtures : &str = "tests/source_sets/fixtures";
  run_with_shared_test_db (
    "skg-test-source-sets",
    |s| Box::pin ( async move {
      s . reset ("source_set_switch_rerenders_views_and_cancels_stale_search_enrichment", fixtures) . await ?;
      source_set_switch_rerenders_views_and_cancels_stale_search_enrichment (
        &s . config, &s . driver, &mut s . tantivy ) . await ?;
      s . reset ("content_view_omits_inactive_contained_nodes", fixtures) . await ?;
      content_view_omits_inactive_contained_nodes (
        &s . config, &s . driver, &mut s . tantivy ) . await ?;
      s . reset_with_fixture_prep (
        // prepare_git_diff_fixture leaves the public source with a
        // real worktree-vs-HEAD diff, which this sub-test renders.
        "diff_view_omits_inactive_members_without_content_leak", fixtures,
        |root| prepare_git_diff_fixture (root) ) . await ?;
      diff_view_omits_inactive_members_without_content_leak (
        &s . config, &s . driver, &mut s . tantivy ) . await ?;
      s . reset ("search_filters_inactive_sources_before_ranking_and_truncation", fixtures) . await ?;
      search_filters_inactive_sources_before_ranking_and_truncation (
        &s . config, &s . driver, &mut s . tantivy ) . await ?;
      s . reset ("inactive_placeholder_in_buffer_does_not_drive_contains", fixtures) . await ?;
      inactive_placeholder_in_buffer_does_not_drive_contains (
        &s . config, &s . driver, &mut s . tantivy ) . await ?;
      s . reset ("saving_edits_to_inactive_placeholder_content_are_rejected", fixtures) . await ?;
      saving_edits_to_inactive_placeholder_content_are_rejected (
        &s . config, &s . driver, &mut s . tantivy ) . await ?;
      s . reset ("restricted_source_search_and_save_work_together_end_to_end", fixtures) . await ?;
      restricted_source_search_and_save_work_together_end_to_end (
        &s . config, &s . driver, &mut s . tantivy ) . await ?;
      s . reset ("containerward_expansion_truncates_before_inactive_container", fixtures) . await ?;
      containerward_expansion_truncates_before_inactive_container (
        &s . config, &s . driver, &mut s . tantivy ) . await ?;
      s . reset ("sourceward_expansion_filters_forks_per_branch_and_omits_empty_forks", fixtures) . await ?;
      sourceward_expansion_filters_forks_per_branch_and_omits_empty_forks (
        &s . config, &s . driver, &mut s . tantivy ) . await ?;
      s . reset ("stale_inactive_placeholders_under_cols_save_without_error", fixtures) . await ?;
      stale_inactive_placeholders_under_cols_save_without_error (
        &s . config, &s . driver, &mut s . tantivy ) . await ?;
      s . reset ("inactive_subscribee_placeholder_does_not_contribute_to_subscribes_to", fixtures) . await ?;
      inactive_subscribee_placeholder_does_not_contribute_to_subscribes_to (
        &s . config, &s . driver, &mut s . tantivy ) . await ?;
      s . reset ("weave_preserves_omitted_inactive_content_members", fixtures) . await ?;
      weave_preserves_omitted_inactive_content_members (
        &s . config, &s . driver, &mut s . tantivy ) . await ?;
      s . reset ("restricted_save_preserves_invisible_override_targets", fixtures) . await ?;
      restricted_save_preserves_invisible_override_targets (
        &s . config, &s . driver, &mut s . tantivy ) . await ?;
      Ok (( )) } )) }

/// PIN (the override-substitution-across-switch case discussed in
/// TODO/strip-inactive-node-fields-progress.org): a drawn override
/// substitute whose source goes inactive on a source-set switch
/// becomes an anonymous bare-atom 'inactiveNode'; the rerender draws the
/// original directly (an inactive overrider does not substitute) with
/// NO leak of the overrider's title or id, retaining the overrider's
/// active descendant; and the container's contains still saves to the
/// original, never to the overrider.
///
/// Fixtures: public 'ovr-sub-container' -> 'ovr-sub-original' (N);
/// private 'ovr-sub-overrider' (R) overrides ovr-sub-original and
/// contains the public 'ovr-sub-child' (D). Under "all" R substitutes
/// for N; switching to "public" makes R inactive.
///
/// Installs the process-global graph handle (override resolution reads
/// it), so it assumes per-test process isolation (nextest), like
/// tests/override_substitution.rs.
#[test]
fn override_substitute_across_source_switch_anonymizes_and_keeps_original (
) -> Result<(), Box<dyn Error>> {
  run_with_source_set_test_db (
    "skg-test-ovr-sub-switch",
    "tests/source_sets/fixtures/skgconfig.toml",
    "/tmp/tantivy-test-ovr-sub-switch",
    |config, driver, tantivy| Box::pin ( async move {
      install_or_swap_global_handle (
        skg::test_utils::graph_handle_from_config (config) ? );

      // 1. Under "all", R is drawn in place of N (substitution).
      let (view_all, _pids, tree_all)
        : (String, Vec<ID>, Tree<ViewNode>) =
        multi_root_view (
          driver, config, Some (tantivy),
          &[ ID::from ("ovr-sub-container") ], false ) . await ?;
      assert! (
        view_all . contains ("(overridesHere ovr-sub-original)"),
        "under 'all' the overrider should substitute for N:\n{}",
        view_all );
      assert! (
        view_all . contains ("private overrider title must not leak"),
        "under 'all' the active overrider is drawn:\n{}", view_all );

      // 2. Switch to "public": R goes inactive; the view re-renders.
      let graph : skg::dbs::in_rust_graph::InRustGraphHandle =
        skg::test_utils::graph_handle_from_config (config) ?;
      let env : skg::types::env::SkgEnv =
        skg::test_utils::skg_env_from_parts (
          config, Arc::clone (driver), tantivy, &graph );
      let mut active : ActiveSourceSet =
        ActiveSourceSet::named (config, SourceSetName::from ("all")) ?;
      let mut views_state : ViewsState =
        ViewsState { diff_mode_enabled : false,
                     open_views        : OpenViews::new () };
      let uri : ViewUri =
        ViewUri::SearchView ("ovr-sub" . to_string ());
      views_state . open_views . views . insert (
        uri . clone (),
        ViewState { viewforest : tree_all . into (),
                    pids       : HashSet::new () });
      let enrichment_slot
        : Arc<Mutex<Option<SearchEnrichmentPayload>>> =
        Arc::new (Mutex::new (None));
      let search_cancelled : Arc<AtomicBool> =
        Arc::new (AtomicBool::new (false));
      let (mut server_stream, _client_stream) =
        connected_tcp_stream_pair ()?;
      std::thread::scope ( |scope| {
        scope . spawn ( || {
          handle_source_set_request (
            &mut server_stream,
            "((request . \"set active source set\") (name . \"public\"))",
            &env, &mut views_state, &mut active,
            &enrichment_slot, &search_cancelled); } ); } );

      // 3. The re-rendered view: N drawn directly, R anonymized.
      let view_public : String = {
        let forest = views_state . open_views
          . viewuri_to_view (&uri)
          . expect ("the switched view should still be registered");
        viewforest_to_string (forest, config) ? };
      assert! ( view_public . contains ("inactiveNode"),
        "the overrider should become an anonymous placeholder:\n{}",
        view_public );
      assert! ( view_public . contains ("ovr-sub-original"),
        "the original N should be drawn directly:\n{}", view_public );
      assert! (
        ! view_public . contains ("private overrider title must not leak"),
        "the inactive overrider's title must not leak:\n{}", view_public );
      assert! ( ! view_public . contains ("ovr-sub-overrider"),
        "the inactive overrider's id must not leak:\n{}", view_public );
      assert! ( ! view_public . contains ("overridesHere"),
        "an anonymous placeholder carries no override marker:\n{}",
        view_public );
      assert! ( view_public . contains ("ovr-sub-child"),
        "the overrider's active descendant is retained:\n{}",
        view_public );

      // 4. Saving the switched view keeps N in the container's
      //    contains, and the inactive overrider writes nothing.
      let public : ActiveSourceSet =
        ActiveSourceSet::named (config, SourceSetName::from ("public")) ?;
      let plan = buffer_to_validated_saveplan (
        &view_public, config, driver, Some (&public) ) . await ? . 1;
      if let Some (c) = plan . define_nodes . iter () . find_map (
        |i| match i {
          DefineNode::Save (SaveNode (n))
            if n . pid == ID::from ("ovr-sub-container") => Some (n),
          _ => None } ) {
        assert_eq! ( c . contains,
          vec![ ID::from ("ovr-sub-original") ],
          "the container keeps the original in contains, not R" ); }
      assert! (
        ! save_ids (&plan . define_nodes)
          . contains (&ID::from ("ovr-sub-overrider")),
        "the inactive overrider produces no SaveNode" );
      Ok (( )) } )) }

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
      ViewNodeKind::Vognode ( Vognode::Active (node) )
        => Some (node . id . clone ()),
      ViewNodeKind::Phantom ( Phantom::Diff (p) )
        => Some (p . id . clone ()),
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

async fn source_set_switch_rerenders_views_and_cancels_stale_search_enrichment (
  config  : &SkgConfig,
  driver  : &Arc<TypeDBDriver>,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  // TODO/full-schema/9-2_source-set-safety.org: a switch RE-RENDERS
  // open views in place instead of closing them.
      let graph : skg::dbs::in_rust_graph::InRustGraphHandle =
        skg::test_utils::graph_handle_from_config (config) ?;
      let env : skg::types::env::SkgEnv =
        skg::test_utils::skg_env_from_parts (
          config, std::sync::Arc::clone (driver), tantivy, &graph );
      let mut active : ActiveSourceSet =
        ActiveSourceSet::named (
          config,
          SourceSetName::from ("public"))?;
      let mut views_state : ViewsState =
        ViewsState {
          diff_mode_enabled : false,
          open_views        : OpenViews::new (), };
      let uri : ViewUri =
        ViewUri::SearchView ("shared ranking term" . to_string ());
      views_state . open_views . views . insert (
        uri . clone (),
        ViewState {
          viewforest : Tree::new (viewforest_root_viewnode ()) . into (),
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
      std::thread::scope ( |scope| {
        // The handler is sync and calls block_on internally (as the
        // real connection thread does); it cannot run inside this
        // test's executor, so give it its own thread.
        scope . spawn ( || {
          handle_source_set_request (
            &mut server_stream,
            "((request . \"set active source set\") (name . \"all\"))",
            &env,
            &mut views_state,
            &mut active,
            &enrichment_slot,
            &search_cancelled); } ); } );
      assert_eq! (
        active . name,
        SourceSetName::from ("all"),
        "source-set switch should update the active set" );
      assert! (
        views_state . open_views . views . contains_key (&uri),
        "source-set switch should KEEP registered views (re-rendered \
         in place), not close them" );
      assert! (
        enrichment_slot . lock () . unwrap () . is_none (),
        "source-set switch should drop stale search enrichment payloads" );
      assert! (
        search_cancelled . load (Ordering::SeqCst),
        "source-set switch should cancel in-flight search enrichment" );
      Ok (( )) }

async fn content_view_omits_inactive_contained_nodes (
  config   : &SkgConfig,
  driver   : &Arc<TypeDBDriver>,
  _tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  // TODO/full-schema/9-2_source-set-safety.org: rendering OMITS
  // inactive children (no placeholders); the weave preserves their
  // memberships at save.
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
      assert! (
        ! actual . contains ("private-a"),
        "an inactive contained node must be omitted entirely: {}",
        actual );
      assert! (actual . contains ("active-a"));
      assert! (actual . contains ("active-b"));
      assert! (
        ! actual . contains ("private title must not leak"),
        "inactive content must not reveal its title: {}",
        actual );
      assert! (
        ! pids . contains (&ID::from ("private-a")),
        "an omitted inactive node is not in the view, so not in its pid set: {:?}",
        pids );
      Ok (( )) }

async fn diff_view_omits_inactive_members_without_content_leak (
  config   : &SkgConfig,
  driver   : &Arc<TypeDBDriver>,
  _tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
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
      // Defense in depth: the connection-level refusals
      // (TODO/full-schema/12-2_diff-mode-policy_discussion.org) keep
      // diff mode and restricted source-sets from combining through
      // the two state doors, but this render seam remains directly
      // constructible (as this test does), so when the modes mix,
      // inactive members are omitted from restricted diff views
      // entirely -- current members and removed-member phantoms
      // alike -- and nothing private leaks.
      for forbidden in [
        "private-new",
        "private-removed",
        "private new title must not leak",
        "private removed title must not leak",
        "private body must not leak",
        "source private) private",
      ] {
        assert! (
          ! actual . contains (forbidden),
          "restricted diff view leaked '{}': {}",
          forbidden,
          actual ); }
      assert! ( actual . contains ("active-a"),
        "active content still renders: {}", actual );
      Ok (( )) }

async fn search_filters_inactive_sources_before_ranking_and_truncation (
  config  : &SkgConfig,
  _driver : &Arc<TypeDBDriver>,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
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
      Ok (( )) }

async fn inactive_placeholder_in_buffer_does_not_drive_contains (
  config   : &SkgConfig,
  driver   : &Arc<TypeDBDriver>,
  _tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  // An inactive placeholder is read-only: it emits no save intention
  // for its container. Its presence and position in the container's
  // contains are owned by the disk merge (weave), not the buffer. So
  // reordering the placeholder cannot move its disk member, and a
  // stale placeholder for a node absent from disk is not resurrected.
  // (Disk root.contains = [active-a, private-a, active-b]; private-a's
  // source is inactive under the "public" set.)
      let active : ActiveSourceSet =
        ActiveSourceSet::named (
          config, SourceSetName ("public" . to_string ())) ?;
      { // The user drags the placeholder to the end. The save keeps
        // private-a at its DISK position (after active-a), not the
        // buffer position, and writes no SaveNode for it.
        let reordered = indoc! {"
          * (skg (node (id root) (source public))) root
          ** (skg (node (id active-a) (source public) indef)) active-a
          ** (skg (node (id active-b) (source public) indef)) active-b
          ** (skg (inactiveNode (id private-a) (source private)))
        "};
        let instructions : Vec<DefineNode> =
          buffer_to_validated_saveplan (
            reordered, config, driver, Some (&active) ) . await?
          . 1 . define_nodes;
        assert_eq! (
          saved_node_by_id (&instructions, "root") . contains,
          vec![ ID::from ("active-a"), ID::from ("private-a"),
                ID::from ("active-b") ],
          "reordering a read-only placeholder must not move its disk \
           member" );
        assert! (
          ! save_ids (&instructions) . contains (&ID::from ("private-a")),
          "an inactive placeholder must not produce a SaveNode" ); }
      { // A stale placeholder for a node NOT in root's disk contains
        // (private-removed) must not be resurrected into contains; the
        // real invisible member (private-a) is still preserved.
        let stale = indoc! {"
          * (skg (node (id root) (source public))) root
          ** (skg (node (id active-a) (source public) indef)) active-a
          ** (skg (node (id active-b) (source public) indef)) active-b
          ** (skg (inactiveNode (id private-removed) (source private)))
        "};
        let instructions : Vec<DefineNode> =
          buffer_to_validated_saveplan (
            stale, config, driver, Some (&active) ) . await?
          . 1 . define_nodes;
        let contains : Vec<ID> =
          saved_node_by_id (&instructions, "root") . contains . clone ();
        assert_eq! (
          contains,
          vec![ ID::from ("active-a"), ID::from ("private-a"),
                ID::from ("active-b") ],
          "a stale placeholder absent from disk must not be resurrected" );
        assert! (
          ! contains . contains (&ID::from ("private-removed")),
          "private-removed must not appear in contains" ); }
      Ok (( )) }

async fn saving_edits_to_inactive_placeholder_content_are_rejected (
  config   : &SkgConfig,
  driver   : &Arc<TypeDBDriver>,
  _tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      let buffer = indoc! {"
        * (skg (node (id root) (source public))) root
        ** (skg (inactiveNode (id private-a) (source private))) edited title
        This body edit should be rejected.
      "};
      let result =
        buffer_to_validated_saveplan (
          buffer, config, driver, None ) . await;
      assert! (
        matches! ( result, Err (SaveError::BufferValidationErrors { .. }) ),
        "editing inactive placeholder title/body should be rejected: {:?}",
        result );
	      Ok (( )) }

async fn restricted_source_search_and_save_work_together_end_to_end (
  config  : &SkgConfig,
  driver  : &Arc<TypeDBDriver>,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
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
        ! rendered . contains ("private-a"),
        "restricted content view must omit inactive members: {}",
        rendered );
      let edited_buffer = indoc! {"
        * (skg (node (id root) (source public))) root
        ** (skg (node (id active-a) (source public) indef)) active-a
        ** (skg (node (id active-b) (source public))) active-b edited through restricted view
      "};
      let instructions : Vec<DefineNode> =
        buffer_to_validated_saveplan (
          edited_buffer, config, driver, Some (&active) ) . await?
        . 1 . define_nodes;
      assert_eq! (
        saved_node_by_id (&instructions, "root") . contains,
        vec![ ID::from ("active-a"), ID::from ("private-a"),
              ID::from ("active-b") ],
        "restricted save should preserve the omitted inactive member \
         via the weave" );
      assert! (
        ! save_ids (&instructions) . contains (&ID::from ("private-a")),
        "restricted save should not write inactive-source nodes" );
      assert_eq! (
        saved_node_by_id (&instructions, "active-b") . title,
        "active-b edited through restricted view",
        "restricted save should still write active-source edits" );
      Ok (( )) }

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

async fn containerward_expansion_truncates_before_inactive_container (
  config   : &SkgConfig,
  driver   : &Arc<TypeDBDriver>,
  _tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
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
         ActiveNode: {}",
        rendered );
      assert! (
        ! rendered . contains ("active-root-after-private"),
        "nodes beyond the first inactive container should be unreachable: {}",
        rendered );
      Ok (( )) }

async fn sourceward_expansion_filters_forks_per_branch_and_omits_empty_forks (
  config   : &SkgConfig,
  driver   : &Arc<TypeDBDriver>,
  _tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
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
        Birth::Backpath (RelationRole::LINK_SOURCE),
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
        Birth::Backpath (RelationRole::LINK_SOURCE),
        Some (&active)) . await ?;
      assert! (
        true_child_ids (&empty_fork_viewforest, empty_fork_child_id)
        . is_empty (),
        "all-inactive sourceward forks should not leave children or \
         empty fork scaffolding" );
      Ok (( )) }

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
    rendered . contains ("(sourceHerald ⌂:public)"),
    "enriched search results should show the source herald at the \
     source boundary (the active-source root): {}",
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

async fn stale_inactive_placeholders_under_cols_save_without_error (
  config   : &SkgConfig,
  driver   : &Arc<TypeDBDriver>,
  _tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  // TODO/full-schema/9-2_source-set-safety.org: the formerly-unsavable
  // buffer. A buffer rendered before a source-set switch can hold
  // InactiveNodes under cols; saving it must not error.
      let buffer = indoc! {"
        * (skg (node (id root) (source public))) root
        ** (skg subscriberCol)
        *** (skg (inactiveNode (id private-a) (source private)))
        ** (skg (node (id active-b) (source public) indef)) active-b
      "};
      let active : ActiveSourceSet =
        ActiveSourceSet::named (
          config, SourceSetName ("public" . to_string ())) ?;
      let result =
        buffer_to_validated_saveplan (
          buffer, config, driver, Some (&active) ) . await;
      assert! ( result . is_ok (),
        "an InactiveNode under a col must not block saving: {:?}",
        result . err () . map ( |e| format! ("{:?}", e)) );
      Ok (( )) }

async fn inactive_subscribee_placeholder_does_not_contribute_to_subscribes_to (
  config   : &SkgConfig,
  driver   : &Arc<TypeDBDriver>,
  _tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  // An inactive placeholder emits no subscribes_to membership, just as
  // it emits no contains membership: 'subscribes_to' is
  // order-meaningful, but the disk merge (weave) owns invisible
  // subscribees, so a buffer-present placeholder must not feed the
  // owner's subscribeeCol. (root has no subscribes_to on disk, so the
  // active member is the only one written.)
      let buffer = indoc! {"
        * (skg (node (id root) (source public))) root
        ** (skg subscribeeCol)
        *** (skg (inactiveNode (id private-a) (source private)))
        *** (skg (node (id active-b) (source public) indef)) active-b
      "};
      let active : ActiveSourceSet =
        ActiveSourceSet::named (
          config, SourceSetName ("public" . to_string ())) ?;
      let instructions : Vec<DefineNode> =
        buffer_to_validated_saveplan (
          buffer, config, driver, Some (&active) ) . await ?
        . 1 . define_nodes;
      assert_eq! (
        saved_node_by_id (&instructions, "root")
          . subscribes_to . or_default (),
        &[ ID::from ("active-b") ],
        "the inactive placeholder must not be a subscribee member" );
      Ok (( )) }

async fn weave_preserves_omitted_inactive_content_members (
  config   : &SkgConfig,
  driver   : &Arc<TypeDBDriver>,
  _tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  // TODO/full-schema/9-2_source-set-safety.org: under a restricted
  // set, a buffer that omits inactive members must not delete them;
  // visible edits (reorder, delete) still land.
      let active : ActiveSourceSet =
        ActiveSourceSet::named (
          config, SourceSetName ("public" . to_string ())) ?;
      { // Disk: root contains [active-a, private-a, active-b].
        // The restricted buffer omits private-a; saving must keep it,
        // anchored after active-a.
        let buffer = indoc! {"
          * (skg (node (id root) (source public))) root
          ** (skg (node (id active-a) (source public) indef)) active-a
          ** (skg (node (id active-b) (source public) indef)) active-b
        "};
        let instructions : Vec<DefineNode> =
          buffer_to_validated_saveplan (
            buffer, config, driver, Some (&active) ) . await ?
          . 1 . define_nodes;
        assert_eq! (
          saved_node_by_id (&instructions, "root") . contains,
          vec![ ID::from ("active-a"), ID::from ("private-a"),
                ID::from ("active-b") ],
          "omitted inactive member must survive, anchored" ); }
      { // Reordering the visible members carries the anchored
        // invisible member with its anchor.
        let buffer = indoc! {"
          * (skg (node (id root) (source public))) root
          ** (skg (node (id active-b) (source public) indef)) active-b
          ** (skg (node (id active-a) (source public) indef)) active-a
        "};
        let instructions : Vec<DefineNode> =
          buffer_to_validated_saveplan (
            buffer, config, driver, Some (&active) ) . await ?
          . 1 . define_nodes;
        assert_eq! (
          saved_node_by_id (&instructions, "root") . contains,
          vec![ ID::from ("active-b"), ID::from ("active-a"),
                ID::from ("private-a") ],
          "the invisible member follows its anchor" ); }
      { // Deleting a visible member lands; the invisible member
        // reattaches leftward (here, to START's successor region).
        let buffer = indoc! {"
          * (skg (node (id root) (source public))) root
          ** (skg (node (id active-b) (source public) indef)) active-b
        "};
        let instructions : Vec<DefineNode> =
          buffer_to_validated_saveplan (
            buffer, config, driver, Some (&active) ) . await ?
          . 1 . define_nodes;
        assert_eq! (
          saved_node_by_id (&instructions, "root") . contains,
          vec![ ID::from ("private-a"), ID::from ("active-b") ],
          "visible deletion lands; invisible member survives" ); }
      Ok (( )) }


async fn restricted_save_preserves_invisible_override_targets (
  config   : &SkgConfig,
  driver   : &Arc<TypeDBDriver>,
  _tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  // The pipeline-level half of the second named regression
  // (TODO/full-schema/13_test-rel-matrix.org): a node overrides
  // [ovr-visible, ovr-inactive] where ovr-inactive's source is
  // inactive. Rendering restricted shows only ovr-visible; the
  // set-difference merge must keep ovr-inactive across a restricted
  // save, even when the visible member is deleted.
      let active : ActiveSourceSet =
        ActiveSourceSet::named (
          &config, SourceSetName::from ("public") )?;
      let override_set = |node : &NodeComplete| -> Vec<ID> {
        match &node . overrides_view_of {
          MSV::Specified (ids) => {
            let mut v : Vec<ID> = ids . clone (); v . sort (); v }
          MSV::Unspecified => Vec::new (), } };
      { // Unmodified restricted save: the invisible target is preserved.
        // (If the merge reproduces disk exactly the owner is a no-op and
        // emits no SaveNode -- which is itself preservation.)
        let unmodified = indoc! {"
          * (skg (node (id ovr-owner) (source public))) ovr-owner
          ** (skg overriddenCol)
          *** (skg (node (id ovr-visible) (source public) indef)) ovr-visible
        "};
        let instructions : Vec<DefineNode> =
          buffer_to_validated_saveplan (
            unmodified, &config, driver, Some (&active) ) . await?
          . 1 . define_nodes;
        if let Some (DefineNode::Save (SaveNode (owner))) =
          instructions . iter () . find ( |i| matches! (
            i, DefineNode::Save (SaveNode (n))
              if n . pid == ID::from ("ovr-owner") )) {
          assert! (
            override_set (owner) . contains (&ID::from ("ovr-inactive")),
            "unmodified restricted save dropped the invisible override \
             target: {:?}", owner . overrides_view_of ); } }
      { // Delete the visible member: disk holds exactly [ovr-inactive].
        let deleted = indoc! {"
          * (skg (node (id ovr-owner) (source public))) ovr-owner
          ** (skg overriddenCol)
        "};
        let instructions : Vec<DefineNode> =
          buffer_to_validated_saveplan (
            deleted, &config, driver, Some (&active) ) . await?
          . 1 . define_nodes;
        assert_eq! (
          override_set ( saved_node_by_id (&instructions, "ovr-owner") ),
          vec![ID::from ("ovr-inactive")],
          "deleting the visible override member must leave exactly the \
           invisible one" ); }
      Ok (( )) }
