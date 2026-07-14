// cargo nextest run --test grouped_sources -E 'test(leak_battery::)'
//
// THE LEAK BATTERY (TODO/user-owned_autofork_chain/5_plan.org, work
// item render-and-gating): a membership's EDGE LEVEL, not just the
// member node's own source, gates whether it renders. Fixtures pin
// the shape the sweep exists to close: a PUBLIC node (N) whose
// PRIVATE section privately contains/subscribes-to another PUBLIC
// node (C) -- a private reading-list entry between two nodes that
// are each individually visible at every level. Without edge-level
// gating this leaks by omission (a public session would still show
// the private membership) or by appearance (inbound surfaces would
// reveal N/S even though the content direction hides them).
//
// Fixture telescope (tests/leak_battery/fixtures):
// - C: public home only ("leak-battery-C").
// - N: public home ("leak-battery-N"), no contains there; a private
//   section (owned/private/N.skg, no title) holds `contains: - C`.
// - S: public home ("leak-battery-S"); a private section
//   (owned/private/S.skg, no title) holds `subscribes_to: - C`.

use ego_tree::{NodeId, Tree};
use std::collections::BTreeSet;
use std::error::Error;

use skg::dbs::filesystem::multiple_nodes::read_all_skg_files_from_sources;
use skg::dbs::in_rust_graph::relation_accessors::{NodeRelation, RelationRole};
use skg::dbs::in_rust_graph::{InRustGraph, install_or_swap_global_handle};
use skg::from_text::buffer_to_viewnodes::uninterpreted::org_to_uninterpreted_nodes;
use skg::org_to_text::viewforest_to_string;
use skg::source_sets::{ActiveSourceSet, SourceSetName, run_with_source_set_test_db};
use skg::test_utils::graph_handle_from_config;
use skg::to_org::expand::backpath::build_and_integrate_containerward_path_with_source_set;
use skg::to_org::render::content_view::multi_root_view_with_source_set;
use skg::types::maybe_placed_viewnode::maybePlaced_to_placed_tree;
use skg::types::misc::ID;
use skg::types::nodes::complete::NodeComplete;
use skg::types::viewnode::{Phantom, RelationCounts, ViewNode, ViewNodeKind, Vognode};
use skg::update_buffer::viewnodestats::set_viewnodestats_in_viewforest;

use std::collections::HashMap;

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

#[test]
fn content_view_of_N_gates_privately_contained_C (
) -> Result<(), Box<dyn Error>> {
  run_with_source_set_test_db (
    "skg-test-leak-battery-content",
    "tests/leak_battery/fixtures/skgconfig.toml",
    "/tmp/tantivy-test-leak-battery-content",
    |config, driver, tantivy| Box::pin ( async move {
      let public : ActiveSourceSet =
        ActiveSourceSet::named (config, SourceSetName::from ("public"))?;
      let all : ActiveSourceSet =
        ActiveSourceSet::named (config, SourceSetName::from ("all"))?;

      // At "public": N's private membership of C must not render --
      // neither C's id nor its title -- even though C itself is a
      // fully public, individually-visible node.
      let (at_public, _pids, _tree) : (String, Vec<ID>, Tree<ViewNode>) =
        multi_root_view_with_source_set (
          driver, config, Some (tantivy),
          &[ ID::from ("N") ], false, &public ) . await ?;
      assert! (
        ! at_public . contains ("leak-battery-C"),
        "a privately-contained public member must not leak by \
         omission-defeat at a public session: {}", at_public );
      assert! (
        at_public . contains ("leak-battery-N"),
        "N itself is public and must render: {}", at_public );

      // At "all": the private membership is visible.
      let (at_all, _pids, _tree) : (String, Vec<ID>, Tree<ViewNode>) =
        multi_root_view_with_source_set (
          driver, config, Some (tantivy),
          &[ ID::from ("N") ], false, &all ) . await ?;
      assert! (
        at_all . contains ("leak-battery-C"),
        "under 'all' the private containment is visible: {}", at_all );
      Ok (( )) } )) }

#[test]
fn inbound_containerward_data_hides_N_at_public (
) -> Result<(), Box<dyn Error>> {
  run_with_source_set_test_db (
    "skg-test-leak-battery-inbound",
    "tests/leak_battery/fixtures/skgconfig.toml",
    "/tmp/tantivy-test-leak-battery-inbound",
    |config, driver, _tantivy| Box::pin ( async move {
      let public : ActiveSourceSet =
        ActiveSourceSet::named (config, SourceSetName::from ("public"))?;
      let all : ActiveSourceSet =
        ActiveSourceSet::named (config, SourceSetName::from ("all"))?;

      // Unit-style pin: the gated in-Rust-graph accessor directly.
      // C's containerward data (who contains C) must not name N at
      // "public" -- the edge's LEVEL (private) is what gates it, not
      // N's own (public) source.
      let nodes : Vec<NodeComplete> =
        read_all_skg_files_from_sources (config)?;
      let graph : InRustGraph =
        InRustGraph::from_nodecompletes (&nodes);
      let inbound_public : Vec<ID> =
        graph . inbound_pids_for_relation_gated (
          &ID::from ("C"), NodeRelation::Contains, Some (&public) );
      assert! (
        ! inbound_public . contains (&ID::from ("N")),
        "N's private containment of C must not surface inbound at \
         public: {:?}", inbound_public );
      let inbound_all : Vec<ID> =
        graph . inbound_pids_for_relation_gated (
          &ID::from ("C"), NodeRelation::Contains, Some (&all) );
      assert! (
        inbound_all . contains (&ID::from ("N")),
        "under 'all' N's containment of C is visible inbound: {:?}",
        inbound_all );

      // Rendered backpath: C's containerward path must truncate
      // before N at "public" (N is grafted at "all").
      install_or_swap_global_handle (
        graph_handle_from_config (config)? );
      {
        let mut viewforest : Tree<ViewNode> =
          viewforest_from_org (
            "* (skg (node (id C) (source public))) leak-battery-C\n" )?;
        let c_id : NodeId = first_child_id (&viewforest);
        build_and_integrate_containerward_path_with_source_set (
          &mut viewforest, c_id, config, driver, Some (&public) ) . await ?;
        let ancestors : BTreeSet<ID> = true_child_ids (&viewforest, c_id);
        assert! (
          ! ancestors . contains (&ID::from ("N")),
          "rendered containerward backpath must not graft N at \
           public: {:?}", ancestors );
        let rendered : String = viewforest_to_string (&viewforest, config)?;
        assert! (
          ! rendered . contains ("leak-battery-N"),
          "N's title must not leak via the rendered backpath at \
           public: {}", rendered );
      }
      {
        let mut viewforest : Tree<ViewNode> =
          viewforest_from_org (
            "* (skg (node (id C) (source public))) leak-battery-C\n" )?;
        let c_id : NodeId = first_child_id (&viewforest);
        build_and_integrate_containerward_path_with_source_set (
          &mut viewforest, c_id, config, driver, Some (&all) ) . await ?;
        let ancestors : BTreeSet<ID> = true_child_ids (&viewforest, c_id);
        assert! (
          ancestors . contains (&ID::from ("N")),
          "under 'all' the rendered containerward backpath grafts \
           N: {:?}", ancestors );
      }
      Ok (( )) } )) }

#[test]
fn subscriberCol_style_inbound_gates_privately_recorded_subscription (
) -> Result<(), Box<dyn Error>> {
  run_with_source_set_test_db (
    "skg-test-leak-battery-subscriber",
    "tests/leak_battery/fixtures/skgconfig.toml",
    "/tmp/tantivy-test-leak-battery-subscriber",
    |config, _driver, _tantivy| Box::pin ( async move {
      let public : ActiveSourceSet =
        ActiveSourceSet::named (config, SourceSetName::from ("public"))?;
      let all : ActiveSourceSet =
        ActiveSourceSet::named (config, SourceSetName::from ("all"))?;
      let nodes : Vec<NodeComplete> =
        read_all_skg_files_from_sources (config)?;
      let graph : InRustGraph =
        InRustGraph::from_nodecompletes (&nodes);

      // C's subscriberCol goal list: 'other_member_pids_gated' at
      // the SUBSCRIBEE role (C's own role -- who subscribes to C).
      // S's subscription is recorded only in S's private section, so
      // it must not appear at "public" even though S itself is a
      // fully public node.
      let subscribers_public : Vec<ID> =
        graph . other_member_pids_gated (
          &ID::from ("C"), RelationRole::SUBSCRIBEE, Some (&public) );
      assert! (
        ! subscribers_public . contains (&ID::from ("S")),
        "S's privately-recorded subscription to C must not surface \
         in C's subscriberCol goal list at public: {:?}",
        subscribers_public );
      let subscribers_all : Vec<ID> =
        graph . other_member_pids_gated (
          &ID::from ("C"), RelationRole::SUBSCRIBEE, Some (&all) );
      assert! (
        subscribers_all . contains (&ID::from ("S")),
        "under 'all' S's subscription to C is visible: {:?}",
        subscribers_all );
      Ok (( )) } )) }

#[test]
fn ancestor_heralds_gate_privately_recorded_relations (
) -> Result<(), Box<dyn Error>> {
  run_with_source_set_test_db (
    "skg-test-leak-battery-heralds",
    "tests/leak_battery/fixtures/skgconfig.toml",
    "/tmp/tantivy-test-leak-battery-heralds",
    |config, _driver, _tantivy| Box::pin ( async move {
      let public : ActiveSourceSet =
        ActiveSourceSet::named (config, SourceSetName::from ("public"))?;
      let all : ActiveSourceSet =
        ActiveSourceSet::named (config, SourceSetName::from ("all"))?;
      install_or_swap_global_handle (
        graph_handle_from_config (config)? );
      // Buffer: C with child S. S subscribes to C, but that edge is
      // recorded only in S's PRIVATE section, so the ancestor-flag
      // pass must not tint S's herald with the 'S' token at public.
      // (Both nodes are individually public; the EDGE is what gates.)
      let herald_of_S = | active : &ActiveSourceSet |
      -> Result<Option<String>, Box<dyn Error>> {
        let mut viewforest : Tree<ViewNode> =
          viewforest_from_org (
            "* (skg (node (id C) (source public))) leak-battery-C\n\
             ** (skg (node (id S) (source public))) leak-battery-S\n" )?;
        for value in viewforest . values_mut () {
          // Give every node counts so the herald pass does not
          // early-return before the ancestor flags (its "no stats ->
          // no heralds" guard). Zero counts render no tokens of
          // their own, so any token present comes from a flag.
          if let ViewNodeKind::Vognode (Vognode::Active (t)) =
            &mut value . kind {
            t . graphStats . rels =
              Some ( RelationCounts::default () ); }}
        set_viewnodestats_in_viewforest (
          &mut viewforest,
          & HashMap::new (),
          & HashMap::new (),
          config,
          Some (active) );
        let c_treeid : NodeId = first_child_id (&viewforest);
        let s_ref = viewforest . get (c_treeid) . unwrap ()
          . first_child () . unwrap ();
        let ViewNodeKind::Vognode (Vognode::Active (t)) =
          & s_ref . value () . kind
        else { return Err ("S is not an Active vognode" . into ()); };
        Ok ( [ t . viewStats . birth_herald . clone (),
               t . viewStats . rels_herald . clone () ]
             . into_iter () . flatten ()
             . reduce ( |a, b| format! ("{} {}", a, b) ) ) };
      let at_public : Option<String> = herald_of_S (&public) ?;
      assert! (
        ! at_public . as_deref () . unwrap_or ("")
          . contains ('S'),
        "S's privately-recorded subscription to its buffer-parent C \
         must not tint an ancestor herald at public: {:?}", at_public );
      let at_all : Option<String> = herald_of_S (&all) ?;
      assert! (
        at_all . as_deref () . unwrap_or ("") . contains ('S'),
        "under 'all' the subscription flags S's herald: {:?}", at_all );
      Ok (( )) } )) }

#[test]
fn a_lowered_edge_is_governed_by_its_new_level (
) {
  // BUG-and-fix_make-edge-more-public.org: after the explicit
  // gesture lowers an edge's privacy to its default, the gated
  // surfaces follow the NEW level -- the edge appears under sets
  // that include that level, while a sibling edge still above its
  // default stays hidden. Lowering to the default cannot leak: by
  // definition both endpoints' homes are at least as public as it.
  use skg::dbs::in_rust_graph::relation_accessors::BinaryRolePosition;
  use skg::types::misc::{PrivaciedMember, SourceName};
  use skg::types::nodes::complete::empty_node_complete;
  let node_at = |pid : &str, source : &str| -> NodeComplete {
    let mut n : NodeComplete = empty_node_complete ();
    n . pid = ID::from (pid);
    n . title = pid . to_string ();
    n . source = SourceName::from (source);
    n };
  let mut owner : NodeComplete = node_at ("owner", "public");
  owner . contains = vec! [
    PrivaciedMember::at ( // as if just lowered to its default
      SourceName::from ("public"), ID::from ("lowered") ),
    PrivaciedMember::at ( // deliberately above its default
      SourceName::from ("private"), ID::from ("kept") ) ];
  let graph : InRustGraph = InRustGraph::from_nodecompletes ( & [
    owner,
    node_at ("lowered", "public"),
    node_at ("kept",    "public") ] );
  let public : ActiveSourceSet = ActiveSourceSet {
    name    : SourceSetName::from ("public"),
    sources : [ SourceName::from ("public") ]
      . into_iter () . collect () };
  let member_role : RelationRole = RelationRole::new (
    NodeRelation::Contains, BinaryRolePosition::Second );
  assert! ( graph . relation_membership_is_visible (
    & ID::from ("owner"), & ID::from ("lowered"), member_role,
    Some (&public) ),
    "an edge lowered to its default renders under the set that \
     includes that default" );
  assert! ( ! graph . relation_membership_is_visible (
    & ID::from ("owner"), & ID::from ("kept"), member_role,
    Some (&public) ),
    "a sibling edge still above its default stays gated" );
  assert! ( graph . relation_membership_is_visible (
    & ID::from ("owner"), & ID::from ("kept"), member_role, None ),
    "the full fold sees everything" ); }
