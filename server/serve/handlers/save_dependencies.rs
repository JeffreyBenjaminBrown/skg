//! Bounded proof that an old view may be planned against a newer selection.
//!
//! This module is deliberately a pure comparison.  It does not read source
//! files, publish a graph, or change the save gate.  The caller supplies the
//! already selected base/current states and the final save proposal.

use crate::dbs::in_rust_graph::relation_accessors::NodeRelation;
use crate::dbs::in_rust_graph::InRustGraph;
use crate::source_sets::ActiveSourceSet;
use crate::types::misc::{ID, SkgConfig, SourceName, members_of};
use crate::types::nodes::complete::NodeComplete;
use crate::types::nodes::rust::NodeRust;
use crate::types::save::{DefineNode, ForkSpec, SourceMove};
use crate::types::store_state::{PathDigest, SelectedStoreState};
use crate::types::tree::forest::ViewForest;
use crate::types::views_state::{impact_ids_from_viewforest, ViewSaveBase};

use std::collections::{BTreeSet, HashSet, VecDeque};
use std::path::PathBuf;

/// Validate the semantic inputs which save planning reads from a retained
/// view.  A successful result means only that the supplied old view can be
/// planned against the supplied current selection; callers still run the
/// ordinary parser, source, override, scalar, and final filesystem checks.
pub fn validate_save_dependencies (
  base             : &ViewSaveBase,
  current_selected : &SelectedStoreState,
  current_config   : &SkgConfig,
  current_source_set : &ActiveSourceSet,
  accepted_forest  : &ViewForest,
  authored_forest  : &ViewForest,
  final_definitions : &[DefineNode],
  source_moves     : &[SourceMove],
  fork_specs       : &[ForkSpec],
) -> Result<(), String> {
  if base . config != *current_config {
    return Err ("retained save authority config differs from current config"
      . into ()); }
  if base . source_set != current_source_set . name . 0 {
    return Err (format! (
      "retained save authority source-set '{}' differs from current '{}'",
      base . source_set, current_source_set . name . 0)); }
  let expected_sources : BTreeSet<SourceName> = current_config . source_set_sources (
    &current_source_set . name) ?;
  if expected_sources != current_source_set . sources {
    return Err (format! (
      "current source-set '{}' has an inconsistent source membership",
      current_source_set . name)); }

  let base_graph : &InRustGraph = &base . selected . graph;
  let current_graph : &InRustGraph = &current_selected . graph;
  let mut seed_ids : BTreeSet<ID> = impact_ids_from_viewforest (accepted_forest)
    . into_iter () . chain (impact_ids_from_viewforest (authored_forest)
    . into_iter ()) . collect ();
  let mut full_ids : BTreeSet<ID> = BTreeSet::new ();
  let mut output_pids : BTreeSet<ID> = BTreeSet::new ();

  for definition in final_definitions {
    match definition {
      DefineNode::Save (save) => {
        seed_ids . insert (save . 0 . pid . clone ());
        full_ids . insert (save . 0 . pid . clone ());
        output_pids . insert (save . 0 . pid . clone ());
        seed_node_facts (&save . 0, &mut seed_ids); }
      DefineNode::Delete (delete) => {
        seed_ids . insert (delete . id . clone ());
        full_ids . insert (delete . id . clone ());
        output_pids . insert (delete . id . clone ()); }
    }}
  for source_move in source_moves {
    seed_ids . insert (source_move . pid . clone ());
    full_ids . insert (source_move . pid . clone ());
    output_pids . insert (source_move . pid . clone ()); }
  for fork in fork_specs {
    seed_ids . insert (fork . original_id . clone ());
    full_ids . insert (fork . original_id . clone ());
    seed_ids . insert (fork . clone . 0 . pid . clone ());
    full_ids . insert (fork . clone . 0 . pid . clone ());
    output_pids . insert (fork . clone . 0 . pid . clone ());
    seed_node_facts (&fork . clone . 0, &mut seed_ids); }

  let closure : DependencyClosure = dependency_closure (
    base_graph, current_graph, seed_ids, full_ids,
    output_pids . clone ());
  compare_graph_closure (base_graph, current_graph, &closure)?;
  compare_output_paths (
    base, current_selected, current_config, &output_pids)?;
  Ok (( )) }

#[derive(Default)]
struct DependencyClosure {
  /// Raw spellings whose PID resolution is a save-planning input.  Absence is
  /// retained by comparing every spelling in both graphs.
  ids             : BTreeSet<ID>,
  /// Nodes whose structural identity is directly read by save planning.
  structural_pids : BTreeSet<ID>,
  /// Inverse partners contribute only identity and home source facts.
  identity_pids   : BTreeSet<ID>,
  /// Subscription endpoints expose their home, aliases, and contains list.
  subscription_endpoints : BTreeSet<ID>,
  /// Nodes whose full scalar and relation facts are read by the proposal.
  full_pids       : BTreeSet<ID>,
  /// IDs for which direct inverse sets are observable save inputs.
  inverse_seeds   : BTreeSet<ID>,
  /// Inbound textlinks matter when the target is actually written/deleted.
  textlink_inverse_seeds : BTreeSet<ID>,
  /// Override traversal has a seen set so cycles remain finite.
  override_pids   : BTreeSet<ID>,
}

fn dependency_closure (
  base       : &InRustGraph,
  current    : &InRustGraph,
  seed_ids   : BTreeSet<ID>,
  full_ids   : BTreeSet<ID>,
  textlink_ids : BTreeSet<ID>,
) -> DependencyClosure {
  let mut result : DependencyClosure = DependencyClosure {
    ids: seed_ids,
    structural_pids: BTreeSet::new (),
    identity_pids: BTreeSet::new (),
    subscription_endpoints: BTreeSet::new (),
    full_pids: BTreeSet::new (),
    inverse_seeds: BTreeSet::new (),
    textlink_inverse_seeds: textlink_ids,
    override_pids: BTreeSet::new (), };
  for id in full_ids {
    result . ids . insert (id . clone ());
    result . inverse_seeds . insert (id . clone ());
    result . full_pids . insert (id . clone ()); }

  // Initial view and final-output IDs are direct inverse seeds.  Relation
  // members are recorded for PID/endpoint checks, but do not recursively
  // walk their own neighbours.
  let initial_ids : Vec<ID> = result . ids . iter () . cloned () . collect ();
  result . structural_pids . extend (initial_ids . iter () . cloned ());
  for id in initial_ids {
    result . inverse_seeds . insert (id . clone ());
    for graph in [base, current] {
      add_node_facts (
        graph, &id, &mut result . ids, &mut result . identity_pids);
      let Some (pid) = graph . pid_of (&id) else { continue; };
      add_inverse_partners (
        graph, &pid, &mut result . ids, &mut result . identity_pids);
      add_subscription_facts (
        graph, &pid, &mut result . ids,
        &mut result . subscription_endpoints,
        &mut result . identity_pids);
      add_override_chain (graph, &pid, &mut result . ids,
                          &mut result . override_pids);
    }}
  // Newly discovered override members are traversed in both directions.  A
  // separate pass avoids turning ordinary containment into component search.
  let mut queue : VecDeque<ID> = result . override_pids . iter ()
    . cloned () . collect ();
  let mut seen : HashSet<ID> = HashSet::new ();
  while let Some (pid) = queue . pop_front () {
    if !seen . insert (pid . clone ()) { continue; }
    result . ids . insert (pid . clone ());
    for graph in [base, current] {
      let before : usize = result . override_pids . len ();
      add_override_chain (graph, &pid, &mut result . ids,
                          &mut result . override_pids);
      if result . override_pids . len () > before {
        queue . extend (result . override_pids . iter () . filter (
          |candidate| !seen . contains (*candidate)) . cloned ()); }
    }}
  for pid in result . full_pids . clone () {
    result . ids . insert (pid . clone ());
    for graph in [base, current] {
      add_node_facts (
        graph, &pid, &mut result . ids, &mut result . identity_pids);
      if let Some (resolved) = graph . pid_of (&pid) {
        add_inverse_partners (
          graph, &resolved, &mut result . ids,
          &mut result . identity_pids); }
    }}
  result }

fn seed_node_facts (node : &NodeComplete,
                    ids : &mut BTreeSet<ID>) {
  ids . extend (node . all_ids () . cloned ());
  ids . extend (members_of (&node . contains));
  ids . extend (members_of (node . subscribes_to . or_default ()));
  ids . extend (members_of (node . hides_from_its_subscriptions . or_default ()));
  ids . extend (members_of (node . overrides_view_of . or_default ()));
}

fn add_node_facts (
  graph    : &InRustGraph,
  id       : &ID,
  ids      : &mut BTreeSet<ID>,
  identity : &mut BTreeSet<ID>,
) {
  let Some (pid) = graph . pid_of (id) else { return; };
  let Some (node) = graph . get (&pid) else { return; };
  ids . insert (node . pid . clone ());
  ids . extend (node . extra_ids . iter () . cloned ());
  for member in members_of (&node . contains) . into_iter ()
    . chain (members_of (node . subscribes_to . or_default ()) . into_iter ())
    . chain (members_of (node . hides_from_its_subscriptions . or_default ())
      . into_iter ())
    . chain (members_of (node . overrides_view_of . or_default ()) . into_iter ())
    . chain (node . textlinks_to . iter () . cloned ()) {
    ids . insert (member . clone ());
    add_identity_for_id (graph, &member, ids, identity); }
}

fn add_identity_for_id (
  graph    : &InRustGraph,
  id       : &ID,
  ids      : &mut BTreeSet<ID>,
  identity : &mut BTreeSet<ID>,
) {
  ids . insert (id . clone ());
  let Some (pid) = graph . pid_of (id) else { return; };
  identity . insert (pid . clone ());
  ids . insert (pid . clone ());
  if let Some (node) = graph . get (&pid) {
    ids . extend (node . extra_ids . iter () . cloned ()); }
}

fn add_subscription_facts (
  graph     : &InRustGraph,
  pid       : &ID,
  ids       : &mut BTreeSet<ID>,
  endpoints : &mut BTreeSet<ID>,
  identity  : &mut BTreeSet<ID>,
) {
  let Some (node) = graph . get (pid) else { return; };
  for subscribee in members_of (node . subscribes_to . or_default ()) {
    add_identity_for_id (graph, &subscribee, ids, identity);
    let Some (subscribee_pid) = graph . pid_of (&subscribee) else { continue; };
    endpoints . insert (subscribee_pid . clone ());
    if let Some (endpoint) = graph . get (&subscribee_pid) {
      ids . insert (endpoint . pid . clone ());
      ids . extend (endpoint . extra_ids . iter () . cloned ());
      ids . extend (members_of (&endpoint . contains));
      for member in members_of (&endpoint . contains) {
        add_identity_for_id (graph, &member, ids, identity); }
    }} }

fn add_inverse_partners (
  graph    : &InRustGraph,
  pid      : &ID,
  ids      : &mut BTreeSet<ID>,
  identity : &mut BTreeSet<ID>,
) {
  for relation in [NodeRelation::Contains, NodeRelation::TextlinksTo,
                   NodeRelation::Subscribes,
                   NodeRelation::HidesFromItsSubscriptions,
                   NodeRelation::OverridesViewOf] {
    for partner in graph . inbound_pids_for_relation (pid, relation) {
      ids . insert (partner . clone ());
      identity . insert (partner . clone ());
      if let Some (node) = graph . get (&partner) {
        ids . extend (node . extra_ids . iter () . cloned ()); }
    }}
}

fn add_override_chain (
  graph       : &InRustGraph,
  pid         : &ID,
  ids         : &mut BTreeSet<ID>,
  seen        : &mut BTreeSet<ID>,
) {
  seen . insert (pid . clone ());
  ids . insert (pid . clone ());
  for partner in graph . outbound_pids_for_relation (
    pid, NodeRelation::OverridesViewOf) . into_iter ()
    . chain (graph . inbound_pids_for_relation (
      pid, NodeRelation::OverridesViewOf) . into_iter ()) {
    ids . insert (partner . clone ());
    seen . insert (partner); }
}

fn compare_graph_closure (
  base    : &InRustGraph,
  current : &InRustGraph,
  closure : &DependencyClosure,
) -> Result<(), String> {
  for id in &closure . ids {
    let before : Option<ID> = base . pid_of (id);
    let after : Option<ID> = current . pid_of (id);
    if before != after {
      return Err (format! ("save dependency PID resolution changed for {}", id)); }
  }
  let full_pids : BTreeSet<ID> = closure . full_pids . iter ()
    . filter_map (|id| base . pid_of (id) . or_else (|| current . pid_of (id)))
    . collect ();
  let structural_pids : BTreeSet<ID> = closure . structural_pids . iter ()
    . filter_map (|id| base . pid_of (id) . or_else (|| current . pid_of (id)))
    . chain (closure . override_pids . iter () . cloned ())
    . chain (closure . subscription_endpoints . iter () . cloned ())
    . chain (closure . identity_pids . iter () . cloned ())
    . collect ();
  let structural_seed_pids : BTreeSet<ID> = closure . structural_pids . iter ()
    . filter_map (|id| base . pid_of (id) . or_else (|| current . pid_of (id)))
    . collect ();
  for pid in structural_pids {
    let before : Option<&NodeRust> = base . get (&pid);
    let after : Option<&NodeRust> = current . get (&pid);
    if full_pids . contains (&pid) {
      if before != after {
        return Err (format! ("full save dependency changed for {}", pid)); }
    } else {
      if structural_seed_pids . contains (&pid)
        && !same_structural_facts (before, after) {
        return Err (format! ("structural save dependency changed for {}", pid)); }
      if closure . subscription_endpoints . contains (&pid)
        && !same_subscription_endpoint_facts (before, after) {
        return Err (format! (
          "subscription endpoint dependency changed for {}", pid)); }
      if closure . override_pids . contains (&pid)
        && !same_override_chain_facts (before, after) {
        return Err (format! (
          "override-chain dependency changed for {}", pid)); }
      if closure . identity_pids . contains (&pid)
        && !same_identity_facts (before, after) {
        return Err (format! (
          "inverse-partner identity changed for {}", pid)); }
    }
  }
  for id in &closure . inverse_seeds {
    let before : Option<ID> = base . pid_of (id);
    let after : Option<ID> = current . pid_of (id);
    if before != after {
      return Err (format! ("inverse dependency PID changed for {}", id)); }
    let pid : ID = before . unwrap_or_else (|| id . clone ());
    for relation in [NodeRelation::Contains, NodeRelation::TextlinksTo,
                     NodeRelation::Subscribes,
                     NodeRelation::HidesFromItsSubscriptions,
                     NodeRelation::OverridesViewOf] {
      if relation == NodeRelation::TextlinksTo
        && !closure . textlink_inverse_seeds . contains (id) {
        continue; }
      let old : BTreeSet<ID> = base . inbound_pids_for_relation (&pid, relation)
        . into_iter () . collect ();
      let new : BTreeSet<ID> = current . inbound_pids_for_relation (&pid, relation)
        . into_iter () . collect ();
      if old != new {
        return Err (format! (
          "inverse {:?} dependency changed for {}", relation, id)); }
    }}
  Ok (( )) }

fn same_structural_facts (
  before : Option<&NodeRust>,
  after  : Option<&NodeRust>,
) -> bool {
  match (before, after) {
    (None, None) => true,
    (Some (a), Some (b)) =>
      a . pid == b . pid
      && a . source == b . source
      && a . extra_ids == b . extra_ids
      && a . ugly_telescope == b . ugly_telescope
      && a . aliases == b . aliases
      && a . contains == b . contains
      && a . subscribes_to == b . subscribes_to
      && a . hides_from_its_subscriptions == b . hides_from_its_subscriptions
      && a . overrides_view_of == b . overrides_view_of
      && a . misc == b . misc,
    _ => false, } }

fn same_subscription_endpoint_facts (
  before : Option<&NodeRust>,
  after  : Option<&NodeRust>,
) -> bool {
  match (before, after) {
    (None, None) => true,
    (Some (a), Some (b)) =>
      a . pid == b . pid
      && a . source == b . source
      && a . extra_ids == b . extra_ids
      && a . aliases == b . aliases
      && a . contains == b . contains,
    _ => false, } }

fn same_override_chain_facts (
  before : Option<&NodeRust>,
  after  : Option<&NodeRust>,
) -> bool {
  match (before, after) {
    (None, None) => true,
    (Some (a), Some (b)) =>
      a . pid == b . pid
      && a . source == b . source
      && a . extra_ids == b . extra_ids
      && a . overrides_view_of == b . overrides_view_of,
    _ => false, } }

fn same_identity_facts (
  before : Option<&NodeRust>,
  after  : Option<&NodeRust>,
) -> bool {
  match (before, after) {
    (None, None) => true,
    (Some (a), Some (b)) =>
      a . pid == b . pid
      && a . source == b . source
      && a . extra_ids == b . extra_ids,
    _ => false, } }

fn compare_output_paths (
  base          : &ViewSaveBase,
  current       : &SelectedStoreState,
  config        : &SkgConfig,
  output_pids   : &BTreeSet<ID>,
) -> Result<(), String> {
  let mut paths : BTreeSet<PathBuf> = BTreeSet::new ();
  for pid in output_pids {
    for source in config . sources . values () {
      paths . insert (source . path . join (format! ("{}.skg", pid . 0))); }
  }
  for path in paths {
    let old : Option<&PathDigest> = base . selected . manifest . get (&path);
    let new : Option<&PathDigest> = current . manifest . get (&path);
    if old != new {
      return Err (format! ("selected source path changed for {}", path . display())); }
  }
  Ok (( )) }

#[cfg(test)]
mod tests {
  use super::*;
  use crate::types::misc::{MemberAtSource, MSV, SkgfileSource, SourceCatalog,
                           SourceSetName};
  use crate::types::nodes::complete::{FileProperty, empty_node_complete};
  use crate::types::save::{DeleteNode, SaveNode};
  use crate::types::store_state::{PathDigest, SelectedPathManifest};
  use crate::types::tree::forest::ViewForest;
  use crate::types::viewnode::{mk_indefinitive_viewnode, ParentIs};
  use std::sync::Arc;

  fn node (pid : &str, title : &str) -> NodeComplete {
    let mut node : NodeComplete = empty_node_complete ();
    node . pid = ID::new (pid);
    node . title = title . into ();
    node }

  fn state (nodes : &[NodeComplete]) -> SelectedStoreState {
    SelectedStoreState::initial (
      InRustGraph::from_nodecompletes (nodes), SelectedPathManifest::new ()) }

  fn state_with_manifest (
    nodes : &[NodeComplete],
    manifest : SelectedPathManifest,
  ) -> SelectedStoreState {
    SelectedStoreState::initial (
      InRustGraph::from_nodecompletes (nodes), manifest) }

  fn forest_with_id (id : &str) -> ViewForest {
    let mut forest : ViewForest = ViewForest::new ();
    forest . append_root (mk_indefinitive_viewnode (
      ID::new (id), SourceName::from ("main"), "shown" . into (),
      ParentIs::Affected));
    forest }

  #[test]
  fn unrelated_scalar_change_on_structural_partner_is_allowed () {
    let mut ancestor : NodeComplete = node ("ancestor", "old");
    ancestor . contains = vec![MemberAtSource::at_source (
      SourceName::from ("main"), ID::new ("child")),
      MemberAtSource::at_source (SourceName::from ("main"),
                                  ID::new ("unrelated"))];
    let child : NodeComplete = node ("child", "child");
    let mut unrelated : NodeComplete = node ("unrelated", "unrelated");
    unrelated . extra_ids = vec![ID::new ("unrelated-alias")];
    let old : SelectedStoreState = state (&[ancestor . clone (), child . clone (), unrelated]);
    let mut changed_ancestor : NodeComplete = ancestor;
    changed_ancestor . title = "new" . into ();
    changed_ancestor . body = Some ("new body" . into ());
    let mut changed_unrelated : NodeComplete = node ("unrelated", "unrelated changed");
    changed_unrelated . contains = vec![MemberAtSource::at_source (
      SourceName::from ("main"), ID::new ("new-child"))];
    changed_unrelated . extra_ids = vec![ID::new ("unrelated-alias")];
    let new : SelectedStoreState = state (&[changed_ancestor, child . clone (), changed_unrelated]);
    let base : ViewSaveBase = ViewSaveBase {
      selected: Arc::new (old . clone ()),
      config: test_config (), source_set: "all" . into () };
    let forest : ViewForest = forest_with_id ("ancestor");
    let config : SkgConfig = base . config . clone ();
    let result : Result<(), String> = validate_save_dependencies (
      &base, &new, &config, &test_sources (), &forest, &forest,
      &[save (child)], &[], &[]);
    assert! (result . is_ok (), "{:?}", result); }

  #[test]
  fn ordinary_indefinite_structural_change_is_refused () {
    let mut old_ancestor : NodeComplete = node ("ancestor", "old");
    old_ancestor . contains = vec![MemberAtSource::at_source (
      SourceName::from ("main"), ID::new ("child"))];
    let mut current_ancestor : NodeComplete = old_ancestor . clone ();
    current_ancestor . contains . clear ();
    let old : SelectedStoreState = state (&[old_ancestor]);
    let current : SelectedStoreState = state (&[current_ancestor]);
    let config : SkgConfig = test_config ();
    let forest : ViewForest = forest_with_id ("ancestor");
    assert! (validate_save_dependencies (
      &ViewSaveBase { selected: Arc::new (old), config: config . clone (),
                      source_set: "all" . into () },
      &current, &config, &test_sources (), &forest, &forest,
      &[], &[], &[]) . is_err ()); }

  #[test]
  fn unwritten_indefinite_ancestor_ignores_new_inbound_textlink () {
    let mut ancestor : NodeComplete = node ("ancestor", "ancestor");
    ancestor . contains = vec![
      MemberAtSource::at_source (SourceName::from ("main"), ID::new ("child")),
      MemberAtSource::at_source (SourceName::from ("main"), ID::new ("sibling"))];
    let child : NodeComplete = node ("child", "child");
    let sibling : NodeComplete = node ("sibling", "sibling");
    let old : SelectedStoreState = state (&[ancestor . clone (), child . clone (), sibling]);
    let mut linked_sibling : NodeComplete = node ("sibling", "sibling");
    linked_sibling . body = Some ("[[id:ancestor][ancestor]]" . into ());
    let current : SelectedStoreState = state (&[ancestor, child . clone (), linked_sibling]);
    let config : SkgConfig = test_config ();
    let forest : ViewForest = forest_with_id ("ancestor");
    assert! (validate_save_dependencies (
      &ViewSaveBase { selected: Arc::new (old), config: config . clone (),
                      source_set: "all" . into () },
      &current, &config, &test_sources (), &forest, &forest,
      &[save (child)], &[], &[]) . is_ok ()); }

  #[test]
  fn inbound_textlink_to_actual_output_is_refused () {
    let target : NodeComplete = node ("target", "target");
    let source : NodeComplete = node ("source", "source");
    let old : SelectedStoreState = state (&[target . clone (), source]);
    let mut linked_source : NodeComplete = node ("source", "source");
    linked_source . body = Some ("[[id:target][target]]" . into ());
    let current : SelectedStoreState = state (&[target . clone (), linked_source]);
    let config : SkgConfig = test_config ();
    let forest : ViewForest = ViewForest::new ();
    assert! (validate_save_dependencies (
      &ViewSaveBase { selected: Arc::new (old), config: config . clone (),
                      source_set: "all" . into () },
      &current, &config, &test_sources (), &forest, &forest,
      &[save (target)], &[], &[]) . is_err ()); }

  #[test]
  fn multihop_override_chain_is_expanded_without_scalar_dependency () {
    let target : NodeComplete = node ("target", "target");
    let mut first : NodeComplete = node ("first", "first");
    first . overrides_view_of = MSV::Specified (
      vec![MemberAtSource::at_source (SourceName::from ("main"),
                                      target . pid . clone ())]);
    let mut second : NodeComplete = node ("second", "second");
    second . overrides_view_of = MSV::Specified (
      vec![MemberAtSource::at_source (SourceName::from ("main"),
                                      first . pid . clone ())]);
    let old : SelectedStoreState = state (&[target . clone (), first . clone (), second . clone ()]);
    let mut changed_second : NodeComplete = second;
    changed_second . extra_ids = vec![ID::new ("second-alias")];
    let current : SelectedStoreState = state (&[target, first, changed_second]);
    let config : SkgConfig = test_config ();
    let forest : ViewForest = ViewForest::new ();
    assert! (validate_save_dependencies (
      &ViewSaveBase { selected: Arc::new (old), config: config . clone (),
                      source_set: "all" . into () },
      &current, &config, &test_sources (), &forest, &forest,
      &[save (node ("target", "target"))], &[], &[]) . is_err ()); }

  #[test]
  fn mixed_subscription_endpoint_still_gets_seed_structural_check () {
    let mut subscriber : NodeComplete = node ("subscriber", "subscriber");
    subscriber . subscribes_to = MSV::Specified (
      vec![MemberAtSource::at_source (SourceName::from ("main"),
                                      ID::new ("subscribee"))]);
    let subscribee : NodeComplete = node ("subscribee", "subscribee");
    let mut changed : NodeComplete = subscribee . clone ();
    changed . misc = vec![FileProperty::Was_Overloaded];
    let old : SelectedStoreState = state (&[subscriber . clone (), subscribee]);
    let current : SelectedStoreState = state (&[subscriber, changed]);
    let config : SkgConfig = test_config ();
    let forest : ViewForest = forest_with_id ("subscribee");
    assert! (validate_save_dependencies (
      &ViewSaveBase { selected: Arc::new (old), config: config . clone (),
                      source_set: "all" . into () },
      &current, &config, &test_sources (), &forest, &forest,
      &[save (node ("subscriber", "subscriber"))], &[], &[]) . is_err ()); }

  fn test_sources () -> ActiveSourceSet {
    ActiveSourceSet { name: SourceSetName ("all" . into ()),
      sources: [SourceName::from ("main")] . into_iter () . collect () }
  }

  fn test_config () -> SkgConfig {
    let mut sources : SourceCatalog = SourceCatalog::default ();
    sources . insert (SourceName::from ("main"), SkgfileSource {
      name: SourceName::from ("main"), abbreviation: None,
      path: PathBuf::from ("/tmp/save-dependencies-source"),
      user_owns_it: true });
    SkgConfig {
      config_path: PathBuf::new (), data_root: PathBuf::new (),
      sources,
      default_source_set: SourceSetName ("all" . into()),
      owned_folder: "" . into (), tantivy_folder: PathBuf::new (),
      maintenance_archive_folder: PathBuf::new (),
      maintenance_archive_identity: PathBuf::new (), port: 0,
      initial_node_limit: 0, timing_log: false,
      beep_when_server_becomes_available: false, max_ancestry_depth: 0, }
  }

  fn test_config_with_private_source () -> SkgConfig {
    let mut config : SkgConfig = test_config ();
    config . sources . insert (SourceName::from ("private"), SkgfileSource {
      name: SourceName::from ("private"), abbreviation: None,
      path: PathBuf::from ("/tmp/save-dependencies-private"),
      user_owns_it: true });
    config . sources . set_order (
      vec![SourceName::from ("main"), SourceName::from ("private")]);
    config }

  fn test_all_sources_with_private () -> ActiveSourceSet {
    ActiveSourceSet { name: SourceSetName ("all" . into ()),
      sources: [SourceName::from ("main"), SourceName::from ("private")]
        . into_iter () . collect () }
  }

  fn save (node : NodeComplete) -> DefineNode {
    DefineNode::Save (SaveNode (node)) }

  fn delete (id : &str) -> DefineNode {
    DefineNode::Delete (DeleteNode {
      id: ID::new (id), source: SourceName::from ("main") }) }

  #[test]
  fn changed_actual_output_is_refused () {
    let old_node : NodeComplete = node ("output", "old");
    let new_node : NodeComplete = node ("output", "new");
    let old : SelectedStoreState = state (&[old_node . clone ()]);
    let new : SelectedStoreState = state (&[new_node . clone ()]);
    let config : SkgConfig = test_config ();
    let forest : ViewForest = ViewForest::new ();
    let result : Result<(), String> = validate_save_dependencies (
      &ViewSaveBase { selected: Arc::new (old), config: config . clone (),
                      source_set: "all" . into () },
      &new, &config, &test_sources (), &forest, &forest,
      &[save (old_node)], &[], &[]);
    assert! (result . is_err ()); }

  #[test]
  fn new_inbound_override_and_delete_referencer_are_refused () {
    let target : NodeComplete = node ("target", "target");
    let mut referencer : NodeComplete = node ("referencer", "referencer");
    referencer . overrides_view_of = MSV::Specified (
      vec![MemberAtSource::at_source (SourceName::from ("main"),
                                      target . pid . clone ())]);
    let old_target : NodeComplete = target . clone ();
    let old : SelectedStoreState = state (&[target . clone ()]);
    let current : SelectedStoreState = state (&[target . clone (), referencer]);
    let config : SkgConfig = test_config ();
    let forest : ViewForest = ViewForest::new ();
    let result : Result<(), String> = validate_save_dependencies (
      &ViewSaveBase { selected: Arc::new (old), config: config . clone (),
                      source_set: "all" . into () },
      &current, &config, &test_sources (), &forest, &forest,
      &[save (old_target)], &[], &[]);
    assert! (result . is_err ());

    let old : SelectedStoreState = state (&[target . clone ()]);
    let mut container : NodeComplete = node ("container", "c");
    container . contains = vec![MemberAtSource::at_source (
      SourceName::from ("main"), target . pid . clone ())];
    let current_graph : InRustGraph = InRustGraph::from_nodecompletes (
      &[target . clone (), container]);
    let current : SelectedStoreState = SelectedStoreState::initial (
      current_graph, SelectedPathManifest::new ());
    let config : SkgConfig = test_config ();
    let forest : ViewForest = ViewForest::new ();
    let result : Result<(), String> = validate_save_dependencies (
      &ViewSaveBase { selected: Arc::new (old), config: config . clone (),
                      source_set: "all" . into () },
      &current, &config, &test_sources (), &forest, &forest,
      &[delete ("target")], &[], &[]);
    assert! (result . is_err ()); }

  #[test]
  fn dangling_inverse_reference_is_compared_when_target_is_absent () {
    let target : NodeComplete = node ("missing-target", "target");
    let mut referencer : NodeComplete = node ("referencer", "referencer");
    referencer . contains = vec![MemberAtSource::at_source (
      SourceName::from ("main"), target . pid . clone ())];
    let old : SelectedStoreState = state (&[]);
    let current : SelectedStoreState = state (&[referencer]);
    let config : SkgConfig = test_config ();
    let forest : ViewForest = forest_with_id ("missing-target");
    assert! (validate_save_dependencies (
      &ViewSaveBase { selected: Arc::new (old), config: config . clone (),
                      source_set: "all" . into () },
      &current, &config, &test_sources (), &forest, &forest,
      &[], &[], &[]) . is_err ()); }

  #[test]
  fn new_alias_resolution_is_refused () {
    let old_node : NodeComplete = node ("target", "target");
    let mut current_node : NodeComplete = old_node . clone ();
    current_node . extra_ids = vec![ID::new ("new-alias")];
    let old : SelectedStoreState = state (&[old_node . clone ()]);
    let current : SelectedStoreState = state (&[current_node]);
    let config : SkgConfig = test_config ();
    let forest : ViewForest = forest_with_id ("new-alias");
    let result : Result<(), String> = validate_save_dependencies (
      &ViewSaveBase { selected: Arc::new (old), config: config . clone (),
                      source_set: "all" . into () },
      &current, &config, &test_sources (), &forest, &forest,
      &[], &[], &[]);
    assert! (result . is_err ()); }

  #[test]
  fn changed_subscribee_membership_is_refused () {
    let mut subscriber : NodeComplete = node ("subscriber", "subscriber");
    subscriber . subscribes_to = MSV::Specified (
      vec![MemberAtSource::at_source (SourceName::from ("main"),
                                      ID::new ("subscribee"))]);
    let subscribee : NodeComplete = node ("subscribee", "subscribee");
    let mut changed_subscribee : NodeComplete = subscribee . clone ();
    changed_subscribee . contains = vec![MemberAtSource::at_source (
      SourceName::from ("main"), ID::new ("child"))];
    let old : SelectedStoreState = state (&[subscriber . clone (), subscribee]);
    let current : SelectedStoreState = state (&[subscriber . clone (), changed_subscribee]);
    let config : SkgConfig = test_config ();
    let forest : ViewForest = ViewForest::new ();
    let result : Result<(), String> = validate_save_dependencies (
      &ViewSaveBase { selected: Arc::new (old), config: config . clone (),
                      source_set: "all" . into () },
      &current, &config, &test_sources (), &forest, &forest,
      &[save (subscriber)], &[], &[]);
    assert! (result . is_err ()); }

  #[test]
  fn changed_subscribee_home_is_refused () {
    let mut subscriber : NodeComplete = node ("subscriber", "subscriber");
    subscriber . subscribes_to = MSV::Specified (
      vec![MemberAtSource::at_source (SourceName::from ("main"),
                                      ID::new ("subscribee"))]);
    let subscribee : NodeComplete = node ("subscribee", "subscribee");
    let mut moved : NodeComplete = subscribee . clone ();
    moved . source = SourceName::from ("private");
    let old : SelectedStoreState = state (&[subscriber . clone (), subscribee]);
    let current : SelectedStoreState = state (&[subscriber . clone (), moved]);
    let config : SkgConfig = test_config_with_private_source ();
    let forest : ViewForest = ViewForest::new ();
    let result : Result<(), String> = validate_save_dependencies (
      &ViewSaveBase { selected: Arc::new (old), config: config . clone (),
                      source_set: "all" . into () },
      &current, &config, &test_all_sources_with_private (),
      &forest, &forest, &[save (subscriber)], &[], &[]);
    assert! (result . is_err ()); }

  #[test]
  fn fork_original_scalar_is_full_dependency () {
    let original : NodeComplete = node ("foreign", "old");
    let changed : NodeComplete = node ("foreign", "new");
    let clone : NodeComplete = node ("clone", "edited");
    let old : SelectedStoreState = state (&[original]);
    let current : SelectedStoreState = state (&[changed]);
    let config : SkgConfig = test_config ();
    let spec : ForkSpec = ForkSpec {
      clone: SaveNode (clone), original_id: ID::new ("foreign"),
      original_title: "old" . into (), original_source: SourceName::from ("main"),
      source_confirmed: false };
    let forest : ViewForest = ViewForest::new ();
    let result : Result<(), String> = validate_save_dependencies (
      &ViewSaveBase { selected: Arc::new (old), config: config . clone (),
                      source_set: "all" . into () },
      &current, &config, &test_sources (), &forest, &forest,
      &[], &[], &[spec]);
    assert! (result . is_err ()); }

  #[test]
  fn selected_manifest_absence_is_compared_for_outputs () {
    let output : NodeComplete = node ("output", "same");
    let config : SkgConfig = test_config ();
    let path : PathBuf = PathBuf::from ("/tmp/save-dependencies-source/output.skg");
    let old : SelectedStoreState = state_with_manifest (&[output . clone ()],
      [(path . clone (), PathDigest::of_bytes (b"old"))] . into_iter ()
        . collect ());
    let current : SelectedStoreState = state (&[output . clone ()]);
    let forest : ViewForest = ViewForest::new ();
    let result : Result<(), String> = validate_save_dependencies (
      &ViewSaveBase { selected: Arc::new (old), config: config . clone (),
                      source_set: "all" . into () },
      &current, &config, &test_sources (), &forest, &forest,
      &[save (output)], &[], &[]);
    assert! (result . is_err ()); }

  #[test]
  fn source_set_and_config_changes_fail_before_graph_reads () {
    let old_node : NodeComplete = node ("output", "same");
    let old : SelectedStoreState = state (&[old_node . clone ()]);
    let current : SelectedStoreState = state (&[old_node]);
    let config : SkgConfig = test_config ();
    let forest : ViewForest = ViewForest::new ();
    let source_set : ActiveSourceSet = ActiveSourceSet {
      name: SourceSetName ("private" . into ()), sources: BTreeSet::new () };
    assert! (validate_save_dependencies (
      &ViewSaveBase { selected: Arc::new (old . clone ()), config: config . clone (),
                      source_set: "all" . into () },
      &current, &config, &source_set, &forest, &forest,
      &[], &[], &[]) . is_err ());
    let mut changed_config : SkgConfig = config . clone ();
    changed_config . port = 1;
    assert! (validate_save_dependencies (
      &ViewSaveBase { selected: Arc::new (old), config,
                      source_set: "all" . into () },
      &current, &changed_config, &test_sources (), &forest, &forest,
      &[], &[], &[]) . is_err ()); }
}
