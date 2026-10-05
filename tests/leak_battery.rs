// cargo nextest run --test grouped_repos -E 'test(leak_battery::)'
//
// THE LEAK BATTERY (TODO/DONE/privacy-telescope/5_plan.org, work
// item render-and-gating): a membership's relRepo, not just the
// member node's own skgrepo, gates whether it renders. Fixtures pin
// the shape the sweep exists to close: a PUBLIC node (N) whose
// PRIVATE section privately contains/subscribesTo another PUBLIC
// node (C) -- a private reading-list entry between two nodes that
// are each individually visible at every skgrepo. Without relRepo
// gating this leaks by omission (a public session would still show
// the private membership) or by appearance (inbound surfaces would
// reveal N/S even though the content direction hides them).
//
// Fixture telescope (tests/leak_battery/fixtures):
// - C: public home only ("leak-battery-C").
// - N: public home ("leak-battery-N"), no contains there; a private
//   section (owned/private/N.skg, no title) holds `contains: - C`.
// - S: public home ("leak-battery-S"); a private section
//   (owned/private/S.skg, no title) holds `subscribesTo: - C`.

use ego_tree::{NodeId, Tree};
use std::collections::BTreeSet;
use std::error::Error;

use skg::dbs::filesystem::multiple_nodes::read_all_skg_files_from_skgrepos;
use skg::dbs::in_rust_graph::relation_accessors::{NodeRelation, RelationRole};
use skg::dbs::in_rust_graph::{InRustGraph};
use skg::from_text::buffer_to_viewnodes::uninterpreted::org_to_uninterpreted_nodes;
use skg::org_to_text::viewforest_to_string;
use skg::skgrepo_sets::{SkgrepoRestriction, SkgrepoSetName, run_with_skgrepo_set_test_db};
use skg::test_utils::graph_handle_from_config;
use skg::to_org::expand::role_tree::build_and_integrate_containerward_role_tree_with_skgrepo_set;
use skg::to_org::render::content_view::multi_root_view_with_skgrepo_set;
use skg::types::maybe_placed_viewnode::maybePlaced_to_placed_tree;
use skg::types::misc::ID;
use skg::types::nodes::complete::Graphnode;
use skg::types::viewnode::{Phantom, RelationCounts, Viewnode, ViewnodeKind, Vognode};
use skg::update_buffer::viewnodestats::set_viewnodestats_in_viewforest;

use std::collections::HashMap;

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

#[test]
fn content_view_of_N_gates_privately_contained_C (
) -> Result<(), Box<dyn Error>> {
  run_with_skgrepo_set_test_db (
    "skg-test-leak-battery-content",
    "tests/leak_battery/fixtures/skgconfig.toml",
    "/tmp/tantivy-test-leak-battery-content",
    |config, tantivy| Box::pin ( async move {
      let public : SkgrepoRestriction =
        SkgrepoRestriction::named (config, SkgrepoSetName::from ("public"))?;
      let all : SkgrepoRestriction =
        SkgrepoRestriction::named (config, SkgrepoSetName::from ("all"))?;

      // At "public": N's private membership of C must not render --
      // neither C's id nor its title -- even though C itself is a
      // fully public, individually-visible node.
      let (at_public, _pids, _tree) : (String, Vec<ID>, Tree<Viewnode>) =
        multi_root_view_with_skgrepo_set (
          config, Some (tantivy),
          &[ ID::from ("N") ], false, &public ) ?;
      assert! (
        ! at_public . contains ("leak-battery-C"),
        "a privately-contained public member must not leak by \
         omission-defeat at a public session: {}", at_public );
      assert! (
        at_public . contains ("leak-battery-N"),
        "N itself is public and must render: {}", at_public );

      // At "all": the private membership is visible.
      let (at_all, _pids, _tree) : (String, Vec<ID>, Tree<Viewnode>) =
        multi_root_view_with_skgrepo_set (
          config, Some (tantivy),
          &[ ID::from ("N") ], false, &all ) ?;
      assert! (
        at_all . contains ("leak-battery-C"),
        "under 'all' the private containment is visible: {}", at_all );
      Ok (( )) } )) }

#[test]
fn inbound_containerward_data_hides_N_at_public (
) -> Result<(), Box<dyn Error>> {
  run_with_skgrepo_set_test_db (
    "skg-test-leak-battery-inbound",
    "tests/leak_battery/fixtures/skgconfig.toml",
    "/tmp/tantivy-test-leak-battery-inbound",
    |config, _tantivy| Box::pin ( async move {
      let public : SkgrepoRestriction =
        SkgrepoRestriction::named (config, SkgrepoSetName::from ("public"))?;
      let all : SkgrepoRestriction =
        SkgrepoRestriction::named (config, SkgrepoSetName::from ("all"))?;

      // Unit-style pin: the gated in-Rust-graph accessor directly.
      // C's containerward data (who contains C) must not name N at
      // "public" -- the relationship's relRepo (private) is what
      // gates it, not N's own (public) skgrepo.
      let nodes : Vec<Graphnode> =
        read_all_skg_files_from_skgrepos (config)?;
      let graph : InRustGraph =
        InRustGraph::from_graphnodes (&nodes);
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

      // Rendered role tree: C's containerward path must truncate
      // before N at "public" (N is grafted at "all").
      let graph_handle = (
        graph_handle_from_config (config)? );
      let graph = graph_handle . load_full ();
      {
        let mut viewforest : Tree<Viewnode> =
          viewforest_from_org (
            "* (skg (node (id C) (repo public))) leak-battery-C\n" )?;
        let c_skgid : NodeId = first_child_skgid (&viewforest);
        build_and_integrate_containerward_role_tree_with_skgrepo_set (
          &mut viewforest, c_skgid, &graph, config, Some (&public) ) ?;
        let ancestors : BTreeSet<ID> = true_child_skgids (&viewforest, c_skgid);
        assert! (
          ! ancestors . contains (&ID::from ("N")),
          "rendered containerward role tree must not graft N at \
           public: {:?}", ancestors );
        let rendered : String = viewforest_to_string (&viewforest, config)?;
        assert! (
          ! rendered . contains ("leak-battery-N"),
          "N's title must not leak via the rendered role tree at \
           public: {}", rendered );
      }
      {
        let mut viewforest : Tree<Viewnode> =
          viewforest_from_org (
            "* (skg (node (id C) (repo public))) leak-battery-C\n" )?;
        let c_skgid : NodeId = first_child_skgid (&viewforest);
        build_and_integrate_containerward_role_tree_with_skgrepo_set (
          &mut viewforest, c_skgid, &graph, config, Some (&all) ) ?;
        let ancestors : BTreeSet<ID> = true_child_skgids (&viewforest, c_skgid);
        assert! (
          ancestors . contains (&ID::from ("N")),
          "under 'all' the rendered containerward role grafts \
           N: {:?}", ancestors );
      }
      Ok (( )) } )) }

#[test]
fn subscriberFolder_style_inbound_gates_privately_recorded_subscription (
) -> Result<(), Box<dyn Error>> {
  run_with_skgrepo_set_test_db (
    "skg-test-leak-battery-subscriber",
    "tests/leak_battery/fixtures/skgconfig.toml",
    "/tmp/tantivy-test-leak-battery-subscriber",
    |config, _tantivy| Box::pin ( async move {
      let public : SkgrepoRestriction =
        SkgrepoRestriction::named (config, SkgrepoSetName::from ("public"))?;
      let all : SkgrepoRestriction =
        SkgrepoRestriction::named (config, SkgrepoSetName::from ("all"))?;
      let nodes : Vec<Graphnode> =
        read_all_skg_files_from_skgrepos (config)?;
      let graph : InRustGraph =
        InRustGraph::from_graphnodes (&nodes);

      // C's subscriberFolder goal list: 'other_member_pids_gated' at
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
         in C's subscriberFolder goal list at public: {:?}",
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
fn default_subscribeeFolder_requires_an_unrestricted_subscription_relationship (
) -> Result<(), Box<dyn Error>> {
  run_with_skgrepo_set_test_db (
    "skg-test-leak-battery-default-subscribee-folder",
    "tests/leak_battery/fixtures/skgconfig.toml",
    "/tmp/tantivy-test-leak-battery-default-subscribee-folder",
    |config, tantivy| Box::pin ( async move {
      let public : SkgrepoRestriction =
        SkgrepoRestriction::named (config, SkgrepoSetName::from ("public"))?;
      let all : SkgrepoRestriction =
        SkgrepoRestriction::named (config, SkgrepoSetName::from ("all"))?;

      // S and C are both public, but S's subscription to C is recorded
      // only in private.  The default folder's existence must follow the
      // relationship skgrepo, not merely the visibility of its endpoints.
      let (at_public, _pids, _tree) : (String, Vec<ID>, Tree<Viewnode>) =
        multi_root_view_with_skgrepo_set (
          config, Some (tantivy),
          &[ ID::from ("S") ], false, &public ) ?;
      assert! (
        ! at_public . contains ("subscribeeFolder"),
        "a restricted subscription must not leave an empty default \
         subscribeeFolder behind:\n{}", at_public );

      let (at_all, _pids, _tree) : (String, Vec<ID>, Tree<Viewnode>) =
        multi_root_view_with_skgrepo_set (
          config, Some (tantivy),
          &[ ID::from ("S") ], false, &all ) ?;
      assert! (
        at_all . contains ("subscribeeFolder"),
        "the unrestricted subscription must create the default folder under all:\n{}",
        at_all );
      assert! (
        at_all . contains ("leak-battery-C"),
        "the unrestricted subscription's member must render under all:\n{}",
        at_all );
      Ok (( )) } )) }

#[test]
fn ancestor_heralds_gate_privately_recorded_relations (
) -> Result<(), Box<dyn Error>> {
  run_with_skgrepo_set_test_db (
    "skg-test-leak-battery-heralds",
    "tests/leak_battery/fixtures/skgconfig.toml",
    "/tmp/tantivy-test-leak-battery-heralds",
    |config, _tantivy| Box::pin ( async move {
      let public : SkgrepoRestriction =
        SkgrepoRestriction::named (config, SkgrepoSetName::from ("public"))?;
      let all : SkgrepoRestriction =
        SkgrepoRestriction::named (config, SkgrepoSetName::from ("all"))?;
      let graph_handle = (
        graph_handle_from_config (config)? );
      let graph = graph_handle . load_full ();
      // Buffer: C with child S. S subscribes to C, but that relationship is
      // recorded only in S's PRIVATE section, so the ancestor-flag
      // pass must not tint S's herald with the 'S' token at public.
      // (Both nodes are individually public; the RELATIONSHIP is what gates.)
      let herald_of_S = | restriction : &SkgrepoRestriction |
      -> Result<Option<String>, Box<dyn Error>> {
        let mut viewforest : Tree<Viewnode> =
          viewforest_from_org (
            "* (skg (node (id C) (repo public))) leak-battery-C\n\
             ** (skg (node (id S) (repo public))) leak-battery-S\n" )?;
        for value in viewforest . values_mut () {
          // Give every node counts so the herald pass does not
          // early-return before the ancestor flags (its "no stats ->
          // no heralds" guard). Zero counts render no tokens of
          // their own, so any token present comes from a flag.
          if let ViewnodeKind::Vognode (Vognode::Unrestricted (t)) =
            &mut value . kind {
            t . graphStats . rels =
              Some ( RelationCounts::default () ); }}
        set_viewnodestats_in_viewforest (
          &mut viewforest,
          &graph,
          & HashMap::new (),
          & HashMap::new (),
          config,
          Some (restriction) );
        let c_treeid : NodeId = first_child_skgid (&viewforest);
        let s_ref = viewforest . get (c_treeid) . unwrap ()
          . first_child () . unwrap ();
        let ViewnodeKind::Vognode (Vognode::Unrestricted (t)) =
          & s_ref . value () . kind
        else { return Err ("S is not an Unrestricted vognode" . into ()); };
        Ok ( t . viewStats . rel_heralds . clone () ) };
      // In the semantic wire the subscription shows as a `subscribes`
      // relation; gated out at public, present under `all`. (S's only
      // subscription here is to its buffer-parent C.)
      let at_public : Option<String> = herald_of_S (&public) ?;
      assert! (
        ! at_public . as_deref () . unwrap_or ("")
          . contains ("subscribesTo"),
        "S's privately-recorded subscription to its buffer-parent C \
         must not tint an ancestor herald at public: {:?}", at_public );
      let at_all : Option<String> = herald_of_S (&all) ?;
      assert! (
        at_all . as_deref () . unwrap_or ("") . contains ("subscribesTo"),
        "under 'all' the subscription flags S's herald: {:?}", at_all );
      Ok (( )) } )) }

#[test]
fn a_lowered_relationship_is_governed_by_its_new_level (
) {
  // BUG-and-fix_make-edge-more-public.org: after the explicit
  // gesture lowers a relationship's privacy to its default, the gated
  // surfaces follow the NEW skgrepo -- the relationship appears under sets
  // that include that skgrepo, while a sibling relationship still more private than its
  // default stays hidden. Lowering to the default cannot leak: by
  // definition both endpoints' homes are at least as public as it.
  use skg::dbs::in_rust_graph::relation_accessors::BinaryRolePosition;
  use skg::types::misc::{RelPartner, SkgrepoName};
  use skg::types::nodes::complete::empty_graphnode;
  let node_at = |pid : &str, skgrepo : &str| -> Graphnode {
    let mut n : Graphnode = empty_graphnode ();
    n . pid = ID::from (pid);
    n . title = pid . to_string ();
    n . home_skgrepo = SkgrepoName::from (skgrepo);
    n };
  let mut recorder : Graphnode = node_at ("recorder", "public");
  recorder . contains = vec! [
    RelPartner::at_relRepo ( // as if just lowered to its default
      SkgrepoName::from ("public"), ID::from ("lowered") ),
    RelPartner::at_relRepo ( // deliberately above its default
      SkgrepoName::from ("private"), ID::from ("kept") ) ];
  let graph : InRustGraph = InRustGraph::from_graphnodes ( & [
    recorder,
    node_at ("lowered", "public"),
    node_at ("kept",    "public") ] );
  let public : SkgrepoRestriction = SkgrepoRestriction {
    name    : SkgrepoSetName::from ("public"),
    skgrepos : [ SkgrepoName::from ("public") ]
      . into_iter () . collect () };
  let member_role : RelationRole = RelationRole::new (
    NodeRelation::Contains, BinaryRolePosition::Second );
  assert! ( graph . relation_membership_is_visible (
    & ID::from ("recorder"), & ID::from ("lowered"), member_role,
    Some (&public) ),
    "an relationship lowered to its default renders under the set that \
     includes that default" );
  assert! ( ! graph . relation_membership_is_visible (
    & ID::from ("recorder"), & ID::from ("kept"), member_role,
    Some (&public) ),
    "a sibling relationship still above its default stays gated" );
  assert! ( graph . relation_membership_is_visible (
    & ID::from ("recorder"), & ID::from ("kept"), member_role, None ),
    "the full fold sees everything" ); }
