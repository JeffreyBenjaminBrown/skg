// cargo nextest run --test grouped_repos -E 'test(repo_sets::)'
//
// These are feature-first tests for TODO/DONE/source-sets/plan.org. They
// intentionally name the skgrepo-set API before the implementation
// exists, and should fail until that feature is wired in.

use indoc::indoc;
use ego_tree::{NodeId, Tree};

use skg::dbs::init::wipe_then_init_tantivy_db;
use skg::dbs::in_rust_graph::relation_accessors::RelationRole;
use skg::dbs::filesystem::not_nodes::load_config;
use skg::dbs::in_rust_graph::containerward_role_tree::ContainerwardRoleTree;
use skg::dbs::in_rust_graph::stats::AllGraphnodeStats;
use skg::serve::ViewsState;
use skg::serve::handlers::skgrepo_sets::handle_skgrepo_set_request;
use skg::serve::handlers::text_search::SearchEnrichmentPayload;
use skg::skgrepo_sets::{
  SkgrepoRestriction,
  SkgrepoSetName,
  filter_path_to_unrestricted_skgrepos_for_test,
  filter_branches_to_unrestricted_skgrepos_for_test,
  prepare_git_diff_fixture,
  run_with_skgrepo_set_test_db};
use skg::dbs::node_lookup::graphnode_from_graph;
use skg::to_org::render::content_view::multi_root_view;
use skg::test_utils::{set_skgrepo_retagging_relRepos, graph_handle_from_config};
use skg::test_utils::run_with_shared_test_stores;
use skg::from_text::buffer_to_validated_saveplan;
use skg::from_text::buffer_to_viewnodes::uninterpreted::org_to_uninterpreted_nodes;
use skg::org_to_text::viewforest_to_string;
use skg::to_org::expand::role_tree::{
  build_and_integrate_containerward_role_tree_with_skgrepo_set,
  integrate_path_that_might_branch_or_cycle_with_skgrepo_set};
use skg::to_org::render::content_view::multi_root_view_with_skgrepo_set;
use skg::types::maybe_placed_viewnode::maybePlaced_to_placed_tree;
use skg::types::errors::SaveError;
use skg::types::misc::{ID, MSV, SkgConfig, SkgrepoName, TantivyIndex, members_of, rel_partners_at_relRepo_msv};
use skg::types::nodes::complete::Graphnode;
use skg::types::save::{NodeInstruction, SaveNode};
use skg::types::viewnode::{
  Birth,
  Viewnode,
  ViewnodeKind,
  viewforest_root_viewnode};
use skg::types::viewnode::{Vognode, Phantom};
use skg::types::views_state::{OpenViews, ViewState, ViewId};

use std::collections::{BTreeSet, HashMap, HashSet};
use std::error::Error;
use std::fs;
use std::net::{TcpListener, TcpStream};
use std::path::Path;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};

#[test]
fn all_tests
  () -> Result<(), Box<dyn Error>> {
  let fixtures : &str = "tests/repo_sets/fixtures";
  run_with_shared_test_stores (
    "skg-test-repo-sets",
    |s| Box::pin ( async move {
      s . reset ("repo_set_switch_rerenders_views_and_cancels_stale_search_enrichment", fixtures) ?;
      skgrepo_set_switch_rerenders_views_and_cancels_stale_search_enrichment (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("content_view_omits_restricted_contained_nodes", fixtures) ?;
      content_view_omits_restricted_contained_nodes (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset_with_fixture_prep (
        // prepare_git_diff_fixture leaves the public skgrepo with a
        // real worktree-vs-HEAD diff, which this sub-test renders.
        "diff_view_omits_restricted_members_without_content_leak", fixtures,
        |root| prepare_git_diff_fixture (root) ) ?;
      diff_view_omits_restricted_members_without_content_leak (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("search_filters_restricted_repos_before_ranking_and_truncation", fixtures) ?;
      search_filters_restricted_skgrepos_before_ranking_and_truncation (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("restricted_placeholder_in_buffer_does_not_drive_contains", fixtures) ?;
      restricted_placeholder_in_buffer_does_not_drive_contains (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("saving_edits_to_restricted_placeholder_content_are_rejected", fixtures) ?;
      saving_edits_to_restricted_placeholder_content_are_rejected (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("restricted_repo_search_and_save_work_together_end_to_end", fixtures) ?;
      restricted_skgrepo_search_and_save_work_together_end_to_end (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("containerward_expansion_truncates_before_restricted_container", fixtures) ?;
      containerward_expansion_truncates_before_restricted_container (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("mentionerward_expansion_filters_forks_per_branch_and_omits_empty_forks", fixtures) ?;
      mentionerward_expansion_filters_forks_per_branch_and_omits_empty_forks (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("stale_restricted_placeholders_under_folders_save_without_error", fixtures) ?;
      stale_restricted_placeholders_under_folders_save_without_error (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("restricted_subscribee_placeholder_does_not_contribute_to_subscribesTo", fixtures) ?;
      restricted_subscribee_placeholder_does_not_contribute_to_subscribesTo (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("weave_preserves_omitted_restricted_content_members", fixtures) ?;
      weave_preserves_omitted_restricted_content_members (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("restricted_save_preserves_invisible_override_targets", fixtures) ?;
      restricted_save_preserves_invisible_override_targets (
        &s . config, &mut s . tantivy ) . await ?;
      Ok (( )) } )) }

/// PIN (the override-substitution-across-switch case discussed in
/// TODO/strip-restricted-node-fields-progress.org): a drawn override
/// substitute whose skgrepo goes restricted on a skgrepo-set switch
/// becomes an anonymous bare-atom 'restrictedNode'; the rerender draws the
/// original directly (a restricted overrider does not substitute) with
/// NO leak of the overrider's title or id, retaining the overrider's
/// unrestricted descendant; and the container's contains still saves to the
/// original, never to the overrider.
///
/// Fixtures: public 'ovr-sub-container' -> 'ovr-sub-original' (N);
/// private 'ovr-sub-overrider' (R) overrides ovr-sub-original and
/// contains the public 'ovr-sub-child' (D). Under "all" R substitutes
/// for N; switching to "public" makes R restricted.
///
/// Installs the explicit graph handle (override resolution reads
/// it), so it assumes per-test process isolation (nextest), like
/// tests/override_substitution.rs.
#[test]
fn override_substitute_across_skgrepo_switch_anonymizes_and_keeps_original (
) -> Result<(), Box<dyn Error>> {
  run_with_skgrepo_set_test_db (
    "skg-test-ovr-sub-switch",
    "tests/repo_sets/fixtures/skgconfig.toml",
    "/tmp/tantivy-test-ovr-sub-switch",
    |config, tantivy| Box::pin ( async move {
      (
        skg::test_utils::graph_handle_from_config (config) ? );

      // 1. Under "all", R is drawn in place of N (substitution).
      let (view_all, _pids, tree_all)
        : (String, Vec<ID>, Tree<Viewnode>) =
        multi_root_view (
          config, Some (tantivy),
          &[ ID::from ("ovr-sub-container") ], false ) ?;
      assert! (
        view_all . contains ("(overridesHere ovr-sub-original)"),
        "under 'all' the overrider should substitute for N:\n{}",
        view_all );
      assert! (
        view_all . contains ("private overrider title must not leak"),
        "under 'all' the unrestricted overrider is drawn:\n{}", view_all );

      // 2. Switch to "public": R goes restricted; the view re-renders.
      let graph : skg::dbs::in_rust_graph::InRustGraphHandle =
        skg::test_utils::graph_handle_from_config (config) ?;
      let env : skg::types::env::SkgEnv =
        skg::test_utils::skg_env_from_parts (
          config, tantivy, &graph );
      let mut restriction : SkgrepoRestriction =
        SkgrepoRestriction::named (config, SkgrepoSetName::from ("all")) ?;
      let mut views_state : ViewsState =
        ViewsState { diff_mode_enabled : false,
                     open_views        : OpenViews::new () };
      let view_id : ViewId =
        ViewId::SearchView ("ovr-sub" . to_string ());
      views_state . open_views . views . insert (
        view_id . clone (),
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
          handle_skgrepo_set_request (
            &mut server_stream,
            "((request . \"set skgrepo restriction\") (name . \"public\"))",
            &env, &mut views_state, &mut restriction,
            &enrichment_slot, &search_cancelled); } ); } );

      // 3. The re-rendered view: N drawn directly, R anonymized.
      let view_public : String = {
        let forest = views_state . open_views
          . viewid_to_view (&view_id)
          . expect ("the switched view should still be registered");
        viewforest_to_string (forest, config) ? };
      assert! ( view_public . contains ("restrictedNode"),
        "the overrider should become an anonymous restricted vognode:\n{}",
        view_public );
      assert! ( view_public . contains ("ovr-sub-original"),
        "the original N should be drawn directly:\n{}", view_public );
      assert! (
        ! view_public . contains ("private overrider title must not leak"),
        "the restricted overrider's title must not leak:\n{}", view_public );
      assert! ( ! view_public . contains ("ovr-sub-overrider"),
        "the restricted overrider's id must not leak:\n{}", view_public );
      assert! ( ! view_public . contains ("overridesHere"),
        "an anonymous restricted vognode carries no override marker:\n{}",
        view_public );
      assert! ( view_public . contains ("ovr-sub-child"),
        "the overrider's unrestricted descendant is retained:\n{}",
        view_public );

      // 4. Saving the switched view keeps N in the container's
      //    contains, and the restricted overrider writes nothing.
      let public : SkgrepoRestriction =
        SkgrepoRestriction::named (config, SkgrepoSetName::from ("public")) ?;
      let plan = buffer_to_validated_saveplan (
        &view_public, config, Some (&public) )  ? . 1;
      if let Some (c) = plan . node_instructions . iter () . find_map (
        |i| match i {
          NodeInstruction::Save (SaveNode (n))
            if n . pid == ID::from ("ovr-sub-container") => Some (n),
          _ => None } ) {
        assert_eq! ( members_of (& c . contains),
          vec![ ID::from ("ovr-sub-original") ],
          "the container keeps the original in contains, not R" ); }
      assert! (
        ! save_skgids (&plan . node_instructions)
          . contains (&ID::from ("ovr-sub-overrider")),
        "the restricted overrider produces no SaveNode" );
      Ok (( )) } )) }

fn save_skgids (
  instructions : &[NodeInstruction],
) -> Vec<ID> {
  instructions . iter() . filter_map (|instruction| match instruction {
    NodeInstruction::Save (SaveNode (node)) => Some (node . pid . clone()),
    NodeInstruction::Delete (_) => None,
  }) . collect() }

fn saved_node_by_skgid<'a> (
  instructions : &'a [NodeInstruction],
  skgid           : &str,
) -> &'a Graphnode {
  for instruction in instructions {
    if let NodeInstruction::Save (SaveNode (node)) = instruction {
      if node . pid == ID::from (skgid) {
        return node; }}}
  panic! ("SaveNode not found: {}", skgid) }

fn saved_or_graph_node_by_skgid (
  instructions : &[NodeInstruction],
  skgid        : &str,
  graph        : &skg::dbs::in_rust_graph::InRustGraph,
) -> Graphnode {
  instructions . iter () . find_map (|instruction| match instruction {
    NodeInstruction::Save (SaveNode (node)) if node . pid == ID::from (skgid) =>
      Some (node . clone ()),
    _ => None, })
    . or_else (|| graphnode_from_graph (graph, &ID::from (skgid)))
    . unwrap_or_else (|| panic! ("Node not found in plan or graph: {}", skgid))
}

fn viewforest_from_org (
  input : &str,
) -> Result<Tree<Viewnode>, Box<dyn Error>> {
  let unchecked_viewforest =
    org_to_uninterpreted_nodes (input)? . 0;
  Ok ( maybePlaced_to_placed_tree (unchecked_viewforest)? ) }

fn first_child_skgid (
  tree : &Tree<Viewnode>,
) -> NodeId {
  tree . root () . first_child () . unwrap () . id () }

fn true_child_skgids (
  tree         : &Tree<Viewnode>,
  parent_skgid : NodeId,
) -> BTreeSet<ID> {
  tree . get (parent_skgid) . unwrap () . children ()
    . filter_map ( |child| match &child . value () . kind {
      ViewnodeKind::Vognode ( Vognode::Unrestricted (node) )
        => Some (node . skgid . clone ()),
      ViewnodeKind::Vognode (Vognode::Phantom ( Phantom::Diff (p) ))
        => Some (p . skgid . clone ()),
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
fn config_loads_default_skgrepo_set_and_prefix_skgrepo_sets (
) -> Result<(), Box<dyn Error>> {
  // Skgrepo-sets are the prefixes of the privacy order: naming a
  // skgrepo selects it and everything more public. The fixture lists
  // public before private, so "public" selects only itself.
  let config =
    load_config ("tests/repo_sets/fixtures/skgconfig.toml")?;
  assert_eq! (
    config . default_skgrepo_set_name (),
    &SkgrepoSetName::from ("public"));
  assert_eq! (
    config . skgrepo_set_skgrepos (&SkgrepoSetName::from ("public"))?,
    BTreeSet::from ([SkgrepoName::from ("public")]));
  assert_eq! (
    config . skgrepo_set_skgrepos (&SkgrepoSetName::from ("all"))?,
    BTreeSet::from ([
      SkgrepoName::from ("private"),
      SkgrepoName::from ("public")]));
  Ok (( )) }

#[test]
fn config_rejects_reserved_all_skgrepo_and_skgrepo_set_names (
) {
  let skgrepo_all =
    load_config ("tests/repo_sets/fixtures-invalid/repo-all/skgconfig.toml");
  assert! (
    skgrepo_all . is_err (),
    "configured repo named all must be rejected" );
  let skgrepo_set_all =
    load_config ("tests/repo_sets/fixtures-invalid/repo-set-all/skgconfig.toml");
  assert! (
    skgrepo_set_all . is_err (),
	    "a config still defining the retired [[repo_sets]] must be rejected" );
}

async fn skgrepo_set_switch_rerenders_views_and_cancels_stale_search_enrichment (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  // TODO/DONE/full-schema/DONE/9-2_source-set-safety.org: a switch RE-RENDERS
  // open views in place instead of closing them.
      let graph : skg::dbs::in_rust_graph::InRustGraphHandle =
        skg::test_utils::graph_handle_from_config (config) ?;
      let env : skg::types::env::SkgEnv =
        skg::test_utils::skg_env_from_parts (
          config, tantivy, &graph );
      let mut restriction : SkgrepoRestriction =
        SkgrepoRestriction::named (
          config,
          SkgrepoSetName::from ("public"))?;
      let mut views_state : ViewsState =
        ViewsState {
          diff_mode_enabled : false,
          open_views        : OpenViews::new (), };
      let view_id : ViewId =
        ViewId::SearchView ("shared ranking term" . to_string ());
      views_state . open_views . views . insert (
        view_id . clone (),
        ViewState {
          viewforest : Tree::new (viewforest_root_viewnode ()) . into (),
          pids       : HashSet::from ([ID::from ("active-search-hit")]), });
      let enrichment_slot : Arc<Mutex<Option<SearchEnrichmentPayload>>> =
        Arc::new (Mutex::new (Some (SearchEnrichmentPayload {
          runtime        : env . runtime_snapshot (),
          terms          : "shared ranking term" . to_string (),
          search_results : vec![ID::from ("active-search-hit")],
          containerward_role_trees_by_skgid : HashMap::new (),
          graphnodestats : AllGraphnodeStats::empty (),
          include_overPrivateText_telescopes : false, })));
      let search_cancelled : Arc<AtomicBool> =
        Arc::new (AtomicBool::new (false));
      let (mut server_stream, _client_stream) =
        connected_tcp_stream_pair ()?;
      std::thread::scope ( |scope| {
        // The handler is sync and calls block_on internally (as the
        // real connection thread does); it cannot run inside this
        // test's executor, so give it its own thread.
        scope . spawn ( || {
          handle_skgrepo_set_request (
            &mut server_stream,
            "((request . \"set skgrepo restriction\") (name . \"all\"))",
            &env,
            &mut views_state,
            &mut restriction,
            &enrichment_slot,
            &search_cancelled); } ); } );
      assert_eq! (
        restriction . name,
        SkgrepoSetName::from ("all"),
        "repo-set switch should update the skgrepo restriction" );
      assert! (
        views_state . open_views . views . contains_key (&view_id),
        "repo-set switch should KEEP registered views (re-rendered \
         in place), not close them" );
      assert! (
        enrichment_slot . lock () . unwrap () . is_none (),
        "repo-set switch should drop stale search enrichment payloads" );
      assert! (
        search_cancelled . load (Ordering::SeqCst),
        "repo-set switch should cancel in-flight search enrichment" );
      Ok (( )) }

async fn content_view_omits_restricted_contained_nodes (
  config : &SkgConfig,
  _tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  // TODO/DONE/full-schema/DONE/9-2_source-set-safety.org: rendering OMITS
  // restricted children (no placeholders); the weave preserves their
  // memberships at save.
      let restriction : SkgrepoRestriction =
        SkgrepoRestriction::named (
          &config,
          SkgrepoSetName::from ("public"))?;
      let (actual, pids, _viewforest) : (String, Vec<ID>, Tree<Viewnode>) =
        multi_root_view_with_skgrepo_set (
          config, None,
          &[ID::from ("root")],
          false,
          &restriction ) ?;
      assert! (
        ! actual . contains ("private-a"),
        "a restricted contained node must be omitted entirely: {}",
        actual );
      assert! (actual . contains ("active-a"));
      assert! (actual . contains ("active-b"));
      assert! (
        ! actual . contains ("private title must not leak"),
        "restricted content must not reveal its title: {}",
        actual );
      assert! (
        ! pids . contains (&ID::from ("private-a")),
        "an omitted restricted node is not in the view, so not in its pid set: {:?}",
        pids );
      Ok (( )) }

async fn diff_view_omits_restricted_members_without_content_leak (
  config : &SkgConfig,
  _tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      let restriction : SkgrepoRestriction =
        SkgrepoRestriction::named (
          &config,
          SkgrepoSetName::from ("public"))?;
      let (actual, _pids, _viewforest) : (String, Vec<ID>, Tree<Viewnode>) =
        multi_root_view_with_skgrepo_set (
          config, None,
          &[ID::from ("diff-root")],
          true,
          &restriction ) ?;
      // Defense in depth: the connection-level refusals
      // (TODO/DONE/full-schema/DONE/12-2_diff-mode-policy_discussion.org) keep
      // diff mode and restricted skgrepo-sets from combining through
      // the two state doors, but this render seam remains directly
      // constructible (as this test does), so when the modes mix,
      // restricted members are omitted from restricted diff views
      // entirely -- current members and removed-member phantoms
      // alike -- and nothing private leaks.
      for forbidden in [
        "private-new",
        "private-removed",
        "private new title must not leak",
        "private removed title must not leak",
        "private body must not leak",
        "repo private) private",
      ] {
        assert! (
          ! actual . contains (forbidden),
          "restricted diff view leaked '{}': {}",
          forbidden,
          actual ); }
      assert! ( actual . contains ("active-a"),
        "unrestricted content still renders: {}", actual );
      Ok (( )) }

async fn search_filters_restricted_skgrepos_before_ranking_and_truncation (
  config  : &SkgConfig,

  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      let restriction : SkgrepoRestriction =
        SkgrepoRestriction::named (
          &config,
          SkgrepoSetName::from ("public"))?;
      let skgids : Vec<ID> =
        skg::serve::handlers::text_search::search_skgids_for_skgrepo_set_for_test (
          &tantivy,
          &config,
          &restriction,
          "shared ranking term",
          2 )?;
      assert_eq! (
        skgids,
        vec![ID::from ("active-search-hit")],
        "restricted high-scoring hits must be filtered before ranking \
         and display truncation" );
      Ok (( )) }

async fn restricted_placeholder_in_buffer_does_not_drive_contains (
  config : &SkgConfig,
  _tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  // A restricted vognode is write-protected: it emits no save intention
  // for its container. Its presence and position in the container's
  // contains are owned by the disk merge (weave), not the buffer. So
  // reordering the placeholder cannot move its disk member, and a
  // stale placeholder for a node absent from disk is not resurrected.
  // (Disk root.contains = [active-a, private-a, active-b]; private-a's
  // skgrepo is restricted under the "public" set.)
      let restriction : SkgrepoRestriction =
        SkgrepoRestriction::named (
          config, SkgrepoSetName ("public" . to_string ())) ?;
      { // The user drags the placeholder to the end. The save keeps
        // private-a at its DISK position (after active-a), not the
        // buffer position, and writes no SaveNode for it.
        let reordered = indoc! {"
          * (skg (node (id root) (repo public))) root
          ** (skg (node (id active-a) (repo public) writeProtected)) active-a
          ** (skg (node (id active-b) (repo public) writeProtected)) active-b
          ** (skg (restrictedNode (id private-a) (repo private)))
        "};
        let instructions : Vec<NodeInstruction> =
          buffer_to_validated_saveplan (
            reordered, config, Some (&restriction) ) ?
          . 1 . node_instructions;
        let graph = graph_handle_from_config (config)? . load_full ();
        assert_eq! (
          members_of (& saved_or_graph_node_by_skgid (
            &instructions, "root", &graph) . contains),
          vec![ ID::from ("active-a"), ID::from ("private-a"),
                ID::from ("active-b") ],
          "reordering a write-protected placeholder must not move its disk \
           member" );
        assert! (
          ! save_skgids (&instructions) . contains (&ID::from ("private-a")),
          "a restricted vognode must not produce a SaveNode" ); }
      { // A stale placeholder for a node NOT in root's disk contains
        // (private-removed) must not be resurrected into contains; the
        // real invisible member (private-a) is still preserved.
        let stale = indoc! {"
          * (skg (node (id root) (repo public))) root
          ** (skg (node (id active-a) (repo public) writeProtected)) active-a
          ** (skg (node (id active-b) (repo public) writeProtected)) active-b
          ** (skg (restrictedNode (id private-removed) (repo private)))
        "};
        let instructions : Vec<NodeInstruction> =
          buffer_to_validated_saveplan (
            stale, config, Some (&restriction) ) ?
          . 1 . node_instructions;
        let graph = graph_handle_from_config (config)? . load_full ();
        let contains : Vec<ID> =
          members_of (& saved_or_graph_node_by_skgid (
            &instructions, "root", &graph) . contains);
        assert_eq! (
          contains,
          vec![ ID::from ("active-a"), ID::from ("private-a"),
                ID::from ("active-b") ],
          "a stale placeholder absent from disk must not be resurrected" );
        assert! (
          ! contains . contains (&ID::from ("private-removed")),
          "private-removed must not appear in contains" ); }
      Ok (( )) }

async fn saving_edits_to_restricted_placeholder_content_are_rejected (
  config : &SkgConfig,
  _tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      let buffer = indoc! {"
        * (skg (node (id root) (repo public))) root
        ** (skg (restrictedNode (id private-a) (repo private))) edited title
        This body edit should be rejected.
      "};
      let result =
        buffer_to_validated_saveplan (
          buffer, config, None ) ;
      assert! (
        matches! ( result, Err (SaveError::BufferValidationErrors { .. }) ),
        "editing restricted vognode title/body should be rejected: {:?}",
        result );
	      Ok (( )) }

async fn restricted_skgrepo_search_and_save_work_together_end_to_end (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      let restriction : SkgrepoRestriction =
        SkgrepoRestriction::named (
          &config,
          SkgrepoSetName::from ("public"))?;
      let skgids : Vec<ID> =
        skg::serve::handlers::text_search::search_skgids_for_skgrepo_set_for_test (
          tantivy,
          &config,
          &restriction,
          "shared ranking term",
          10 )?;
      assert_eq! (
        skgids,
        vec![ID::from ("active-search-hit")],
        "restricted search should only return unrestricted-repo hits" );
      let (rendered, _pids, _viewforest) : (String, Vec<ID>, Tree<Viewnode>) =
        multi_root_view_with_skgrepo_set (
          config, None,
          &[ID::from ("root")],
          false,
          &restriction ) ?;
      assert! (
        ! rendered . contains ("private-a"),
        "restricted content view must omit restricted members: {}",
        rendered );
      let edited_buffer = indoc! {"
        * (skg (node (id root) (repo public))) root
        ** (skg (node (id active-a) (repo public) writeProtected)) active-a
        ** (skg (node (id active-b) (repo public))) active-b edited through restricted view
      "};
      let instructions : Vec<NodeInstruction> =
        buffer_to_validated_saveplan (
          edited_buffer, config, Some (&restriction) ) ?
        . 1 . node_instructions;
      let graph = graph_handle_from_config (config)? . load_full ();
      assert_eq! (
        members_of (& saved_or_graph_node_by_skgid (
          &instructions, "root", &graph) . contains),
        vec![ ID::from ("active-a"), ID::from ("private-a"),
              ID::from ("active-b") ],
        "restricted save should preserve the omitted restricted member \
         via the weave" );
      assert! (
        ! save_skgids (&instructions) . contains (&ID::from ("private-a")),
        "restricted save should not write restricted-skgrepo nodes" );
      assert_eq! (
        saved_node_by_skgid (&instructions, "active-b") . title,
        "active-b edited through restricted view",
        "restricted save should still write unrestricted-repo edits" );
      Ok (( )) }

#[test]
fn backward_path_truncates_before_first_restricted_node (
) -> Result<(), Box<dyn Error>> {
  let config =
    load_config ("tests/repo_sets/fixtures/skgconfig.toml")?;
  let restriction : SkgrepoRestriction =
    SkgrepoRestriction::named (
      &config,
      SkgrepoSetName::from ("public"))?;
  let graph = graph_handle_from_config (&config)? . load_full ();
  let path : Vec<ID> =
    vec![
      ID::from ("active-container"),
      ID::from ("private-container"),
      ID::from ("active-root-after-private") ];
  assert_eq! (
    filter_path_to_unrestricted_skgrepos_for_test (&graph, &config, &restriction, path)?,
    vec![ID::from ("active-container")],
    "mid-path filtering should keep exactly the unrestricted prefix \
     and stop before the first restricted node" );
  Ok (( )) }

#[test]
fn backward_path_filters_forks_per_branch_and_omits_empty_forks (
) -> Result<(), Box<dyn Error>> {
  let config =
    load_config ("tests/repo_sets/fixtures/skgconfig.toml")?;
  let restriction : SkgrepoRestriction =
    SkgrepoRestriction::named (
      &config,
      SkgrepoSetName::from ("public"))?;
  let graph = graph_handle_from_config (&config)? . load_full ();
  let mixed_branches : BTreeSet<ID> =
    BTreeSet::from ([
      ID::from ("active-fork-branch"),
      ID::from ("private-fork-branch")]);
  assert_eq! (
    filter_branches_to_unrestricted_skgrepos_for_test (
      &graph, &config, &restriction, mixed_branches)?,
    BTreeSet::from ([ID::from ("active-fork-branch")]),
    "partially restricted forks should render only unrestricted branches" );
  let restricted_branches : BTreeSet<ID> =
    BTreeSet::from ([
      ID::from ("private-fork-branch"),
      ID::from ("private-other-branch")]);
  assert! (
    filter_branches_to_unrestricted_skgrepos_for_test (
      &graph, &config, &restriction, restricted_branches)?
    . is_empty (),
    "fully restricted forks should not render an empty fork folder" );
  Ok (( )) }

async fn containerward_expansion_truncates_before_restricted_container (
  config : &SkgConfig,
  _tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      let graph = skg::test_utils::graph_handle_from_config (config)? . load_full ();
      let restriction : SkgrepoRestriction =
        SkgrepoRestriction::named (
          &config,
          SkgrepoSetName::from ("public"))?;
      let mut viewforest : Tree<Viewnode> =
        viewforest_from_org (indoc! {"
          * (skg (node (id child-for-backpath) (repo public))) child-for-backpath
        "})?;
      let child_skgid : NodeId = first_child_skgid (&viewforest);
      build_and_integrate_containerward_role_tree_with_skgrepo_set (
        &mut viewforest,
        child_skgid,
        &graph,
        config,
        Some (&restriction)) ?;
      let child_children : BTreeSet<ID> =
        true_child_skgids (&viewforest, child_skgid);
      assert_eq! (
        child_children,
        BTreeSet::from ([ID::from ("active-container")]),
        "containerward expansion should keep the unrestricted prefix and \
         truncate before the restricted container" );
      let rendered : String =
        viewforest_to_string (&viewforest, config)?;
      assert! (
        ! rendered . contains ("private-container"),
        "restricted container should not render as a placeholder or \
         UnrestrictedVognode: {}",
        rendered );
      assert! (
        ! rendered . contains ("active-root-after-private"),
        "nodes beyond the first restricted container should be unreachable: {}",
        rendered );
      Ok (( )) }

async fn mentionerward_expansion_filters_forks_per_branch_and_omits_empty_forks (
  config : &SkgConfig,
  _tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      let graph = skg::test_utils::graph_handle_from_config (config)? . load_full ();
      let restriction : SkgrepoRestriction =
        SkgrepoRestriction::named (
          &config,
          SkgrepoSetName::from ("public"))?;
      let mut viewforest : Tree<Viewnode> =
        viewforest_from_org (indoc! {"
          * (skg (node (id child-with-fork) (repo public))) child-with-fork
        "})?;
      let child_skgid : NodeId = first_child_skgid (&viewforest);
      integrate_path_that_might_branch_or_cycle_with_skgrepo_set (
        &mut viewforest,
        child_skgid,
        Vec::new (),
        HashSet::from ([
          ID::from ("active-fork-branch"),
          ID::from ("private-fork-branch"),
          ID::from ("private-other-branch")]),
        HashSet::new (),
        &graph,
        config,
        Birth::RoleGraft (RelationRole::MENTIONER),
        Some (&restriction)) ?;
      assert_eq! (
        true_child_skgids (&viewforest, child_skgid),
        BTreeSet::from ([ID::from ("active-fork-branch")]),
        "mentionerward fork expansion should retain unrestricted branches \
         independently and omit restricted branches" );

      let mut empty_fork_viewforest : Tree<Viewnode> =
        viewforest_from_org (indoc! {"
          * (skg (node (id child-with-fork) (repo public))) child-with-fork
        "})?;
      let empty_fork_child_skgid : NodeId =
        first_child_skgid (&empty_fork_viewforest);
      integrate_path_that_might_branch_or_cycle_with_skgrepo_set (
        &mut empty_fork_viewforest,
        empty_fork_child_skgid,
        Vec::new (),
        HashSet::from ([
          ID::from ("private-fork-branch"),
          ID::from ("private-other-branch")]),
        HashSet::new (),
        &graph,
        config,
        Birth::RoleGraft (RelationRole::MENTIONER),
        Some (&restriction)) ?;
      assert! (
        true_child_skgids (&empty_fork_viewforest, empty_fork_child_skgid)
        . is_empty (),
        "all-restricted mentionerward forks should not leave children or \
         empty fork folders" );
      Ok (( )) }

#[test]
fn search_enrichment_truncates_role_tree_before_restricted_container (
) -> Result<(), Box<dyn Error>> {
  let config =
    load_config ("tests/repo_sets/fixtures/skgconfig.toml")?;
  let restriction : SkgrepoRestriction =
    SkgrepoRestriction::named (
      &config,
      SkgrepoSetName::from ("public"))?;
  let mut result_node : Graphnode =
    skg::types::nodes::complete::empty_graphnode ();
  result_node . pid = ID::from ("active-search-hit");
  result_node . title = "unrestricted search hit" . to_string ();
  set_skgrepo_retagging_relRepos ( &mut result_node, &SkgrepoName::from ("public") );
  result_node . aliases = rel_partners_at_relRepo_msv (
    & result_node . home_skgrepo,
    MSV::Specified (vec!["search term" . to_string ()]) );
  let mut unrestricted_container : Graphnode =
    skg::types::nodes::complete::empty_graphnode ();
  unrestricted_container . pid = ID::from ("active-container");
  unrestricted_container . title = "active-container" . to_string ();
  set_skgrepo_retagging_relRepos ( &mut unrestricted_container, &SkgrepoName::from ("public") );
  let mut private_container : Graphnode =
    skg::types::nodes::complete::empty_graphnode ();
  private_container . pid = ID::from ("private-container");
  private_container . title =
    "private container title must not leak" . to_string ();
  set_skgrepo_retagging_relRepos ( &mut private_container, &SkgrepoName::from ("private") );
  let graph = skg::dbs::in_rust_graph::InRustGraph::from_graphnodes (
    &[result_node . clone (), unrestricted_container . clone (),
      private_container . clone ()]);
  let index_dir : &str =
    "/tmp/tantivy-test-repo-sets-search-enrichment-truncation";
  let (tantivy, _count) =
    wipe_then_init_tantivy_db (
      &[ result_node, unrestricted_container, private_container ],
      Path::new (index_dir))?;
  let mut matches_by_skgid =
    skg::serve::handlers::text_search::MatchGroups::new ();
  matches_by_skgid . insert (
    ID::from ("active-search-hit"),
    ( SkgrepoName::from ("public"),
      vec![(1.0, "unrestricted search hit" . to_string ())] ));
  let containerward_role_trees_by_skgid : HashMap<ID, ContainerwardRoleTree> =
    HashMap::from ([(
      ID::from ("active-search-hit"),
      ContainerwardRoleTree::Inner (
        ID::from ("active-search-hit"),
        vec![ContainerwardRoleTree::Inner (
          ID::from ("active-container"),
          vec![ContainerwardRoleTree::Root (
            ID::from ("private-container"))])]))]);
  let rendered : String =
    skg::serve::handlers::text_search
      ::enriched_search_buffer_for_skgrepo_set_for_test (
        &graph,
        "search term",
        &matches_by_skgid,
        &[ID::from ("active-search-hit")],
        &containerward_role_trees_by_skgid,
        &tantivy,
        &config,
        &restriction)?;
  assert! (
    rendered . contains ("active-container"),
    "unrestricted ancestry should render: {}",
    rendered );
  assert! (
    rendered . contains ("(homeRepoHerald ⌂:public)"),
    "enriched search results should show the repo herald at the \
     repo boundary (the restriction-repo root): {}",
    rendered );
  assert! (
    ! rendered . contains ("private-container"),
    "restricted enrichment ancestry should be truncated before the \
     restricted container: {}",
    rendered );
  assert! (
    ! rendered . contains ("private container title must not leak"),
    "restricted enrichment ancestry must not reveal title text: {}",
    rendered );
  if Path::new (index_dir) . exists () {
    fs::remove_dir_all (index_dir)?; }
  Ok (( )) }

#[test]
fn titles_by_skgids_omits_restricted_skgrepo_titles (
) -> Result<(), Box<dyn Error>> {
  let config =
    load_config ("tests/repo_sets/fixtures/skgconfig.toml")?;
  let restriction : SkgrepoRestriction =
    SkgrepoRestriction::named (
      &config,
      SkgrepoSetName::from ("public"))?;
  let titles =
    skg::serve::handlers::titles_by_skgids::titles_by_skgids_for_skgrepo_set_for_test (
      &config,
      &restriction,
      &[ ID::from ("active-a"),
         ID::from ("private-a") ])?;
  assert_eq! (
    titles . get (&ID::from ("active-a")),
    Some (&"active-a" . to_string ()));
  assert! (
    ! titles . contains_key (&ID::from ("private-a")),
    "restricted-skgrepo title lookup must omit private-a" );
  Ok (( )) }

async fn stale_restricted_placeholders_under_folders_save_without_error (
  config : &SkgConfig,
  _tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  // TODO/DONE/full-schema/DONE/9-2_source-set-safety.org: the formerly-unsavable
  // buffer. A buffer rendered before a skgrepo-set switch can hold
  // RestrictedVognodes under folders; saving it must not error.
      let buffer = indoc! {"
        * (skg (node (id root) (repo public))) root
        ** (skg subscriberFolder)
        *** (skg (restrictedNode (id private-a) (repo private)))
        ** (skg (node (id active-b) (repo public) writeProtected)) active-b
      "};
      let restriction : SkgrepoRestriction =
        SkgrepoRestriction::named (
          config, SkgrepoSetName ("public" . to_string ())) ?;
      let result =
        buffer_to_validated_saveplan (
          buffer, config, Some (&restriction) ) ;
      assert! ( result . is_ok (),
        "a RestrictedVognode under a folder must not block saving: {:?}",
        result . err () . map ( |e| format! ("{:?}", e)) );
      Ok (( )) }

async fn restricted_subscribee_placeholder_does_not_contribute_to_subscribesTo (
  config : &SkgConfig,
  _tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  // A restricted vognode emits no subscribesTo membership, just as
  // it emits no contains relationship_axes: 'subscribesTo' is
  // order-meaningful, but the disk merge (weave) owns invisible
  // subscribees, so a buffer-present placeholder must not feed the
  // recorder's subscribeeFolder. (root has no subscribesTo on disk, so the
  // unrestricted member is the only one written.)
      let buffer = indoc! {"
        * (skg (node (id root) (repo public))) root
        ** (skg subscribeeFolder)
        *** (skg (restrictedNode (id private-a) (repo private)))
        *** (skg (node (id active-b) (repo public) writeProtected)) active-b
      "};
      let restriction : SkgrepoRestriction =
        SkgrepoRestriction::named (
          config, SkgrepoSetName ("public" . to_string ())) ?;
      let instructions : Vec<NodeInstruction> =
        buffer_to_validated_saveplan (
          buffer, config, Some (&restriction) )  ?
        . 1 . node_instructions;
      assert_eq! (
        members_of (
          saved_node_by_skgid (&instructions, "root")
            . subscribesTo . or_default () ),
        vec! [ ID::from ("active-b") ],
        "the restricted vognode must not be a subscribee member" );
      Ok (( )) }

async fn weave_preserves_omitted_restricted_content_members (
  config : &SkgConfig,
  _tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  // TODO/DONE/full-schema/DONE/9-2_source-set-safety.org: under a restricted
  // set, a buffer that omits restricted members must not delete them;
  // visible edits (reorder, delete) still land.
      let restriction : SkgrepoRestriction =
        SkgrepoRestriction::named (
          config, SkgrepoSetName ("public" . to_string ())) ?;
      let graph = graph_handle_from_config (config)? . load_full ();
      { // Disk: root contains [active-a, private-a, active-b].
        // The restricted buffer omits private-a; saving must keep it,
        // anchored after active-a.
        let buffer = indoc! {"
          * (skg (node (id root) (repo public))) root
          ** (skg (node (id active-a) (repo public) writeProtected)) active-a
          ** (skg (node (id active-b) (repo public) writeProtected)) active-b
        "};
        let instructions : Vec<NodeInstruction> =
          buffer_to_validated_saveplan (
            buffer, config, Some (&restriction) )  ?
          . 1 . node_instructions;
        assert_eq! (
          members_of (& saved_or_graph_node_by_skgid (
            &instructions, "root", &graph) . contains),
          vec![ ID::from ("active-a"), ID::from ("private-a"),
                ID::from ("active-b") ],
          "omitted restricted member must survive, anchored" ); }
      { // Reordering the visible members carries the anchored
        // invisible member with its anchor.
        let buffer = indoc! {"
          * (skg (node (id root) (repo public))) root
          ** (skg (node (id active-b) (repo public) writeProtected)) active-b
          ** (skg (node (id active-a) (repo public) writeProtected)) active-a
        "};
        let instructions : Vec<NodeInstruction> =
          buffer_to_validated_saveplan (
            buffer, config, Some (&restriction) )  ?
          . 1 . node_instructions;
        assert_eq! (
          members_of (& saved_or_graph_node_by_skgid (
            &instructions, "root", &graph) . contains),
          vec![ ID::from ("active-b"), ID::from ("active-a"),
                ID::from ("private-a") ],
          "the invisible member follows its anchor" ); }
      { // Deleting a visible member lands; the invisible member
        // reattaches leftward (here, to START's successor region).
        let buffer = indoc! {"
          * (skg (node (id root) (repo public))) root
          ** (skg (node (id active-b) (repo public) writeProtected)) active-b
        "};
        let instructions : Vec<NodeInstruction> =
          buffer_to_validated_saveplan (
            buffer, config, Some (&restriction) )  ?
          . 1 . node_instructions;
        assert_eq! (
          members_of (& saved_or_graph_node_by_skgid (
            &instructions, "root", &graph) . contains),
          vec![ ID::from ("private-a"), ID::from ("active-b") ],
          "visible deletion lands; invisible member survives" ); }
      Ok (( )) }


async fn restricted_save_preserves_invisible_override_targets (
  config : &SkgConfig,
  _tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  // The pipeline-level half of the second named regression
  // (TODO/DONE/full-schema/DONE/13_test-rel-matrix.org): a node overrides
  // [ovr-visible, ovr-inactive] where ovr-inactive's skgrepo is
  // restricted. Rendering restricted shows only ovr-visible; the
  // set-difference merge must keep ovr-inactive across a restricted
  // save, even when the visible member is deleted.
      let restriction : SkgrepoRestriction =
        SkgrepoRestriction::named (
          &config, SkgrepoSetName::from ("public") )?;
      let override_set = |node : &Graphnode| -> Vec<ID> {
        match &node . overrides {
          MSV::Specified (skgids) => {
            let mut v : Vec<ID> = members_of (skgids); v . sort (); v }
          MSV::Unspecified => Vec::new (), } };
      { // Unmodified restricted save: the invisible target is preserved.
        // (If the merge reproduces disk exactly the recorder is a no-op and
        // emits no SaveNode -- which is itself preservation.)
        let unmodified = indoc! {"
          * (skg (node (id ovr-owner) (repo public))) ovr-owner
          ** (skg overriddenFolder)
          *** (skg (node (id ovr-visible) (repo public) writeProtected)) ovr-visible
        "};
        let instructions : Vec<NodeInstruction> =
          buffer_to_validated_saveplan (
            unmodified, &config, Some (&restriction) ) ?
          . 1 . node_instructions;
        if let Some (NodeInstruction::Save (SaveNode (recorder))) =
          instructions . iter () . find ( |i| matches! (
            i, NodeInstruction::Save (SaveNode (n))
              if n . pid == ID::from ("ovr-owner") )) {
          assert! (
            override_set (recorder) . contains (&ID::from ("ovr-inactive")),
            "unmodified restricted save dropped the invisible override \
             target: {:?}", recorder . overrides ); } }
      { // Delete the visible member: disk holds exactly [ovr-inactive].
        let deleted = indoc! {"
          * (skg (node (id ovr-owner) (repo public))) ovr-owner
          ** (skg overriddenFolder)
        "};
        let instructions : Vec<NodeInstruction> =
          buffer_to_validated_saveplan (
            deleted, &config, Some (&restriction) ) ?
          . 1 . node_instructions;
        assert_eq! (
          override_set ( saved_node_by_skgid (&instructions, "ovr-owner") ),
          vec![ID::from ("ovr-inactive")],
          "deleting the visible override member must leave exactly the \
           invisible one" ); }
      Ok (( )) }
