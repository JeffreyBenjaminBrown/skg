use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::in_rust_graph::relation_accessors::{
  NodeRelation, UPDATE_RELEVANT_ORDINARY_RELATIONS,
};
use crate::from_text::buffer_to_viewnodes::uninterpreted::org_to_viewforest;
use crate::from_text::buffer_to_viewnodes::add_missing_info::IdResolution;
use crate::save::PreparedSave;
use crate::serve::ViewsState;
use crate::serve::handlers::save_buffer::ClientViewSnapshot;
use crate::types::misc::ID;
use crate::types::tree::forest::ViewForest;
use crate::types::viewnode::{
  PropertyMember, ViewnodeKind, Vognode,
};
use crate::types::views_state::ViewforestId;

use std::collections::{HashMap, HashSet};

/// What a save touches: its touched IDs and their update-relevant
/// neighborhood twice over. The collateral redraw of clean views reads
/// the full neighborhood ('matching_identity'); the dirty-view check
/// reads the neighborhood without links ('matching_identity_ignoring_links'),
/// since a link holds no copy of its target, nor its target of it.
pub(crate) struct SaveAffectedIds {
  raw_touched                : HashSet<ID>,
  neighborhood               : NeighborhoodBeforeAndAfter,
  neighborhood_without_links : NeighborhoodBeforeAndAfter,
}

/// The update-relevant neighborhood of a save's touched IDs over some
/// ordinary relations, in the graph before the save and in its final
/// candidate graph.
struct NeighborhoodBeforeAndAfter {
  before : HashSet<ID>,
  after  : HashSet<ID>,
}

impl SaveAffectedIds {
  pub(crate) fn from_prepared (
    prepared : &PreparedSave,
  ) -> SaveAffectedIds {
    let raw_touched : HashSet<ID> =
      prepared . final_write_identities () . clone ();
    let ordinary_relations_but_links : Vec<NodeRelation> =
      UPDATE_RELEVANT_ORDINARY_RELATIONS . iter () . copied ()
      . filter (|relation| *relation != NodeRelation::LinksTo)
      . collect ();
    SaveAffectedIds {
      neighborhood : NeighborhoodBeforeAndAfter::of_touched (
        prepared, &raw_touched, UPDATE_RELEVANT_ORDINARY_RELATIONS),
      neighborhood_without_links : NeighborhoodBeforeAndAfter::of_touched (
        prepared, &raw_touched, &ordinary_relations_but_links),
      raw_touched, }
  }

  /// The identity under which 'gnid' is touched or in the full
  /// neighborhood, if it is.
  pub(crate) fn matching_identity (
    &self,
    gnid  : &ID,
    before : &InRustGraph,
    after  : &InRustGraph,
  ) -> Option<ID> {
    self . neighborhood . matching_identity (
      &self . raw_touched, gnid, before, after) }

  /// The identity under which 'gnid' is touched or in the neighborhood
  /// without links, if it is.
  pub(crate) fn matching_identity_ignoring_links (
    &self,
    gnid  : &ID,
    before : &InRustGraph,
    after  : &InRustGraph,
  ) -> Option<ID> {
    self . neighborhood_without_links . matching_identity (
      &self . raw_touched, gnid, before, after) }

  pub(crate) fn collateral_viewforest_ids (
    &self,
    saved_viewforest_id : &ViewforestId,
    views_state : &ViewsState,
    before     : &InRustGraph,
    after      : &InRustGraph,
  ) -> Vec<ViewforestId> {
    let mut viewforest_ids : Vec<ViewforestId> = views_state . open_views . views . iter ()
      .filter (|(viewforest_id, state)| {
        *viewforest_id != saved_viewforest_id && state . pids . iter () . any (|gnid|
          self . matching_identity (gnid, before, after) . is_some ()) })
      .map (|(viewforest_id, _)| viewforest_id . clone ())
      .collect ();
    viewforest_ids . sort_by_key (ViewforestId::repr_in_client);
    viewforest_ids
  }
}

impl NeighborhoodBeforeAndAfter {
  fn of_touched (
    prepared           : &PreparedSave,
    raw_touched        : &HashSet<ID>,
    ordinary_relations : &[NodeRelation],
  ) -> NeighborhoodBeforeAndAfter {
    NeighborhoodBeforeAndAfter {
      before : prepared . graph_before_save ()
        . update_relevant_neighborhood_over (
          raw_touched . iter () . cloned (), ordinary_relations),
      after : prepared . final_candidate ()
        . update_relevant_neighborhood_over (
          raw_touched . iter () . cloned (), ordinary_relations), }}

  fn matching_identity (
    &self,
    raw_touched : &HashSet<ID>,
    gnid        : &ID,
    before      : &InRustGraph,
    after       : &InRustGraph,
  ) -> Option<ID> {
    if raw_touched . contains (gnid) {
      return Some (gnid . clone ()); }
    if let Some (canonical) = before . pid_of (gnid) {
      if self . before . contains (&canonical) {
        return Some (canonical); }}
    if let Some (canonical) = after . pid_of (gnid) {
      if self . after . contains (&canonical) {
        return Some (canonical); }}
    None
  }
}

pub(crate) struct DirtyViewConflict {
  pub(crate) viewforest_id : ViewforestId,
  pub(crate) gnids : Vec<ID>,
}

pub(crate) fn dirty_view_conflicts (
  buffer_snapshots   : &[ClientViewSnapshot],
  views_state : &ViewsState,
  affected    : &SaveAffectedIds,
  before      : &InRustGraph,
  after       : &InRustGraph,
) -> Result<Vec<DirtyViewConflict>, String> {
  let mut conflicts : Vec<DirtyViewConflict> = Vec::new ();
  for buffer_snapshot in buffer_snapshots . iter () . filter (|buffer_snapshot| buffer_snapshot . dirty) {
    let baseline : &str = buffer_snapshot . baseline . as_deref () . ok_or_else (||
      format! (
        "Dirty view {} has no verified clean baseline. Run skg-show-unsaved-changes (Emacs) or :SkgShowUnsavedChanges (Neovim), then close or refresh that view before saving.",
        buffer_snapshot . viewforest_id . repr_in_client ())) ?;
    let current : &str = buffer_snapshot . current . as_deref () . ok_or_else (||
      format! ("Dirty view {} has no current text",
               buffer_snapshot . viewforest_id . repr_in_client ())) ?;
    let mut dependencies : HashSet<ID> = dependencies_from_text (
      baseline, &buffer_snapshot . viewforest_id) ?;
    dependencies . extend (dependencies_from_text (
      current, &buffer_snapshot . viewforest_id) ?);
    if let Some (registered) = views_state . open_views
        . viewforest_id_to_view (&buffer_snapshot . viewforest_id)
    { dependencies . extend (dependencies_from_viewforest (registered)); }
    let mut matching : Vec<ID> = dependencies . iter ()
      .filter_map (|gnid| affected . matching_identity_ignoring_links (
        gnid, before, after))
      .collect ();
    matching . sort_by (|left, right| left . 0 . cmp (&right . 0));
    matching . dedup ();
    if ! matching . is_empty () {
      conflicts . push (DirtyViewConflict {
        viewforest_id : buffer_snapshot . viewforest_id . clone (), gnids : matching, }); }}
  Ok (conflicts)
}

fn dependencies_from_text (
  text : &str,
  viewforest_id : &ViewforestId,
) -> Result<HashSet<ID>, String> {
  let (forest, errors, _, _) =
    org_to_viewforest (text, IdResolution::AsWritten)
    .map_err (|error| format! (
      "Cannot inspect dirty view {}: {}. Run its recovery command before saving.",
      viewforest_id . repr_in_client (), error)) ?;
  if ! errors . is_empty () {
    return Err (format! (
      "Cannot inspect dirty view {} because its dependency metadata is invalid: {}. Run its recovery command before saving.",
      viewforest_id . repr_in_client (),
      errors . iter () . map (ToString::to_string)
        .collect::<Vec<String>> () . join ("; "))); }
  Ok (dependencies_from_viewforest (&forest))
}

/// The graphnodes a dirty view holds viewnodes of: occurrences (and the
/// overrides each shows), gnid-only viewnodes, ghosts and ID property
/// members. A merge request or a link does not make its target one: it
/// holds no copy of the target, so a save that changes the target
/// clobbers nothing the view holds.
fn dependencies_from_viewforest (
  forest : &ViewForest,
) -> HashSet<ID> {
  let mut result : HashSet<ID> = HashSet::new ();
  for node in forest . nodes () {
    match &node . value () . kind {
      ViewnodeKind::Vognode (Vognode::Occurrence (occurrence)) => {
        result . insert (occurrence . gnid . clone ());
        result . extend (occurrence . overridesHere . iter () . cloned ()); }
      ViewnodeKind::Vognode (Vognode::GnidOnly (gnid_only)) => {
        result . insert (gnid_only . gnid . clone ()); }
      ViewnodeKind::Vognode (Vognode::Ghost (ghost)) => {
        result . insert (ghost . gnid . clone ()); }
      ViewnodeKind::PropertyMember (
        PropertyMember::ID { gnid, .. }) => {
        result . insert (gnid . clone ()); }
      _ => {}, }}
  result
}

pub(crate) fn format_conflict_error (
  conflicts : &[DirtyViewConflict],
) -> String {
  let by_view : HashMap<String, String> = conflicts . iter ()
    .map (|conflict| (
      conflict . viewforest_id . repr_in_client (),
      conflict . gnids . iter () . map (ToString::to_string)
        .collect::<Vec<String>> () . join (", ")))
    .collect ();
  let mut entries : Vec<(String, String)> = by_view . into_iter () . collect ();
  entries . sort_by (|left, right| left . 0 . cmp (&right . 0));
  format! (
    "NOTHING WAS SAVED: this save conflicts with unsaved dependencies in {}. Archive those edits with skg-show-unsaved-changes (Emacs) or :SkgShowUnsavedChanges (Neovim), close the archived views, and retry. Conflicts: {}",
    entries . iter () . map (|entry| entry . 0 . as_str ())
      .collect::<Vec<&str>> () . join (", "),
    entries . iter () . map (|(viewforest_id, gnids)| format! ("{} [{}]", viewforest_id, gnids))
      .collect::<Vec<String>> () . join ("; "))
}
