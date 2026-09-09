//! Execute an explicit durable recipe after its named selection is ready.
//! All matching, ranking, ancestry and privacy checks use the supplied pair.

use super::{MatchGroups, build_search_viewforest, filter_match_groups_to_active_sources,
  group_matches_by_id, suppressed_result_ids};
use super::render_enriched_search_buffer::{
  insert_containerward_ancestries_into_search_view,
  insert_override_ancestries_into_search_view,
};
use crate::dbs::graph_queries::all_graphnodestats::{
  AllGraphNodeStats, fetch_all_graphnodestats,
};
use crate::dbs::graph_queries::ancestry::{AncestryTree, full_containerward_ancestry};
use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::tantivy::search::{SearchOptions, has_ugly_telescope, search_index};
use crate::maintenance::query_waits::QueryWaitRecipe;
use crate::org_to_text::viewforest_to_string;
use crate::serve::handlers::scalar_release::{
  ScalarReleaseDecision, decide, exclude_ugly_nodes_from_viewforest,
};
use crate::source_sets::{
  ActiveSourceSet, SourceSetName, apply_source_set_to_viewforest,
};
use crate::to_org::util::mark_view_roots_parent_absent;
use crate::types::env::SkgEnv;
use crate::types::misc::ID;
use crate::types::tree::forest::ViewForest;
use crate::update_buffer::{active_ids_in_viewforest, set_viewnodestats_in_viewforest};
use crate::update_buffer::graphnodestats::set_metadata_relationships_in_node_recursive;

use ego_tree::NodeId;
use std::collections::{HashMap, HashSet};
use std::sync::Arc;
use tantivy::{DocAddress, Searcher};

pub(crate) fn execute
( env : &SkgEnv,
  recipe : &QueryWaitRecipe,
) -> Result<(String, Vec<String>), String> {
  let graph : Arc<InRustGraph> = env . in_rust_graph_snapshot ();
  let active : ActiveSourceSet = ActiveSourceSet::named (
    &env . config, SourceSetName::from (recipe . source_set . as_str ()))
    . map_err (|error| error . to_string ())?;
  let include_ugly : bool = active . is_all ()
    || recipe . ugly_choice . as_deref () == Some ("include");
  if !active . is_all () && recipe . ugly_choice . is_none ()
  && has_ugly_telescope (&env . tantivy_index, &env . searcher)
    . map_err (|error| error . to_string ())? {
    return Err ("the target search needs an explicit protected-text choice; submit a new wait choosing include or exclude" . into ()); }
  let options : SearchOptions = SearchOptions {
    regex: recipe . regex, body: recipe . body, operators: recipe . operators,
    exclude_ugly_telescope: !include_ugly, };
  let (matches, searcher) : (Vec<(f32, DocAddress)>, Searcher) = search_index (
    &env . tantivy_index, &env . searcher, &recipe . terms, &options)
    . map_err (|error| error . to_string ())?;
  let groups : MatchGroups = filter_match_groups_to_active_sources (
    group_matches_by_id (matches, searcher, &env . tantivy_index,
      &recipe . terms, &options, Some (&active)), &active);
  let suppressed : HashSet<ID> = suppressed_result_ids (
    &groups, &graph, &env . config, &active);
  let (mut forest, results) : (ViewForest, Vec<ID>) = build_search_viewforest (
    &recipe . terms, &groups, &suppressed);
  if results . is_empty () {
    return Ok (("No matches found.\n" . into (), Vec::new ())); }
  let ancestry : HashMap<ID, AncestryTree> = results . iter () . map (|id|
    (id . clone (), full_containerward_ancestry (
      &graph, id, env . config . max_ancestry_depth, Some (&active)))) . collect ();
  insert_containerward_ancestries_into_search_view (
    &mut forest, &results, &ancestry, &graph, &env . config, &active);
  insert_override_ancestries_into_search_view (
    &mut forest, &results, &active, &graph);
  let rendered_ids : Vec<ID> = active_ids_in_viewforest (&forest);
  let stats : AllGraphNodeStats = fetch_all_graphnodestats (
    &graph, &rendered_ids, Some (&active));
  let root : NodeId = forest . root () . id ();
  set_metadata_relationships_in_node_recursive (
    &graph, &mut forest, root, &stats, &env . config);
  mark_view_roots_parent_absent (&mut forest);
  set_viewnodestats_in_viewforest (
    &graph, &mut forest, &stats . container_to_contents,
    &stats . content_to_containers, &env . config, Some (&active));
  apply_source_set_to_viewforest (&mut forest, &active);
  if !include_ugly { exclude_ugly_nodes_from_viewforest (&mut forest, &graph); }
  let rendered_ids : Vec<ID> = active_ids_in_viewforest (&forest);
  let approved : HashSet<ID> = if include_ugly {
    rendered_ids . iter () . cloned () . collect ()
  } else { HashSet::new () };
  let warnings : Vec<String> = match decide (
      "query-wait-result", &active, &rendered_ids, &graph, &approved) {
    ScalarReleaseDecision::Allow => Vec::new (),
    ScalarReleaseDecision::AllowWithWarning { warning } => vec![warning],
    ScalarReleaseDecision::Challenge { .. } => return Err (
      "the target search failed its protected-text release check" . into ()), };
  let content : String = viewforest_to_string (&forest, &env . config)
    . map_err (|error| error . to_string ())?;
  Ok ((content, warnings)) }

#[cfg(test)]
mod tests {
  use super::*;
  use crate::runtime::query_waits::execution::indexed_query_snapshot;
  use crate::runtime::SelectedRuntimeSnapshot;
  use crate::from_text::buffer_to_viewnodes::uninterpreted::org_to_uninterpreted_viewforest;
  use crate::types::maybe_placed_viewnode::maybePlaced_to_placed_viewforest;
  use crate::types::env::GraphReadSnapshot;
  use crate::types::misc::{MemberAtSource, MSV, SkgConfig, SkgfileSource, SourceName};
  use crate::types::nodes::complete::{NodeComplete, empty_node_complete};
  use crate::types::store_state::SelectedStoreState;
  use std::collections::BTreeSet;

  fn fixture (ugly : bool) -> Arc<SelectedRuntimeSnapshot> {
    let mut config : SkgConfig = SkgConfig::dummyFromSources (Default::default ());
    for name in ["public", "private"] {
      let name : SourceName = SourceName::from (name);
      config . sources . insert (name . clone (), SkgfileSource {
        name: name . clone (), abbreviation: None,
        path: format! ("/nonexistent/{}", name) . into (), user_owns_it: true, }); }
    let mut public : NodeComplete = empty_node_complete ();
    public . pid = ID::from ("public-node");
    public . source = SourceName::from ("public");
    public . title = "Visible needle" . into ();
    public . ugly_telescope = ugly;
    public . body = Some ("retained body" . into ());
    public . aliases = MSV::Specified (vec![MemberAtSource::at_source (
      SourceName::from ("private"), "secretnickname" . into ())]);
    let mut private : NodeComplete = empty_node_complete ();
    private . pid = ID::from ("private-node");
    private . source = SourceName::from ("private");
    private . title = "Hidden needle" . into ();
    let selected : SelectedStoreState = SelectedStoreState::initial (
      InRustGraph::from_nodecompletes (&[public, private]), Default::default ());
    indexed_query_snapshot (GraphReadSnapshot {
      config, selected: selected . graph_base (), cyclic_roots: BTreeSet::new (),
    }) . unwrap ()
  }

  fn recipe (terms : &str, choice : Option<&str>) -> QueryWaitRecipe {
    QueryWaitRecipe {
      terms: terms . into (), regex: false, body: true, operators: false,
      ugly_choice: choice . map (str::to_string), source_set: "public" . into (),
      config_snapshot: "unused in pure renderer" . into (), source_catalog_snapshot: "unused" . into (),
      config_file_blake3: "a" . repeat (64), source_catalog_blake3: "b" . repeat (64),
    }
  }

  #[test]
  fn retained_query_uses_source_filtered_matches_and_ancestry_without_disk () {
    let snapshot : Arc<SelectedRuntimeSnapshot> = fixture (false);
    let (content, warnings) : (String, Vec<String>) = execute (
      &snapshot . env, &recipe ("needle", None)) . unwrap ();
    assert! (content . contains ("Visible needle"));
    assert! (!content . contains ("Hidden needle"));
    assert! (!content . contains ("private-node"));
    assert! (warnings . is_empty ());
    let (parsed, errors, _) = org_to_uninterpreted_viewforest (&content) . unwrap ();
    assert! (errors . is_empty (), "{:?}", errors);
    assert! (maybePlaced_to_placed_viewforest (parsed) . is_ok ());
    let (body_match, _) : (String, Vec<String>) = execute (
      &snapshot . env, &recipe ("retained", None)) . unwrap ();
    assert! (body_match . contains ("Visible needle"));
    let (content, _) : (String, Vec<String>) = execute (
      &snapshot . env, &recipe ("secretnickname", None)) . unwrap ();
    assert_eq! (content, "No matches found.\n");
  }

  #[test]
  fn retained_query_respects_explicit_protected_text_choice_before_matching () {
    let snapshot : Arc<SelectedRuntimeSnapshot> = fixture (true);
    assert! (execute (&snapshot . env, &recipe ("needle", None)) . is_err ());
    let (excluded, _) : (String, Vec<String>) = execute (
      &snapshot . env, &recipe ("needle", Some ("exclude"))) . unwrap ();
    assert_eq! (excluded, "No matches found.\n");
    let (included, warnings) : (String, Vec<String>) = execute (
      &snapshot . env, &recipe ("needle", Some ("include"))) . unwrap ();
    assert! (included . contains ("Visible needle"));
    assert! (!included . contains ("Hidden needle"));
    assert! (!warnings . is_empty ());
  }
}
