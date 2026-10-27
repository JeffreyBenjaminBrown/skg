use crate::dbs::in_rust_graph::InRustGraph;
use crate::types::nodes::rust::GraphnodeInRust;
use crate::from_text::buffer_to_viewnodes::uninterpreted::org_to_viewforest;
use crate::from_text::buffer_to_viewnodes::add_missing_info::IdResolution;
use crate::save::PreparedSave;
use crate::serve::ViewsState;
use crate::serve::handlers::save_buffer::ClientViewSnapshot;
use crate::types::misc::ID;
use crate::types::links::links_from_text;
use crate::types::tree::forest::ViewForest;
use crate::types::viewnode::{
  Editability, NodeEditRequest, PropertyMember, ViewnodeKind, Vognode,
};
use crate::types::views_state::ViewforestId;

use std::collections::{HashMap, HashSet};

pub(crate) struct SaveAffectedIds {
  before      : HashSet<ID>,
  after       : HashSet<ID>,
  raw_touched : HashSet<ID>,
  /// Every ID of each node the save deletes: one the graph before the
  /// save resolves and the graph after it does not.
  pub(crate) deleted : HashSet<ID>,
}

/// Every ID (pid and extra IDs) of each node among 'touched' that
/// 'before' resolves and 'after' does not.
fn ids_of_deleted_nodes (
  touched : &HashSet<ID>,
  before  : &InRustGraph,
  after   : &InRustGraph,
) -> HashSet<ID> {
  touched . iter ()
    . filter ( |gnid| after . pid_of (gnid) . is_none () )
    . filter_map ( |gnid| before . pid_of (gnid) )
    . filter_map ( |pid| before . get (&pid) )
    . flat_map (GraphnodeInRust::ids)
    . collect () }

impl SaveAffectedIds {
  pub(crate) fn from_prepared (
    prepared : &PreparedSave,
  ) -> SaveAffectedIds {
    let raw_touched : HashSet<ID> =
      prepared . final_write_identities () . clone ();
    let before : HashSet<ID> = prepared . graph_before_save ()
      . update_relevant_neighborhood (raw_touched . iter () . cloned ());
    let after : HashSet<ID> = prepared . final_candidate ()
      . update_relevant_neighborhood (raw_touched . iter () . cloned ());
    let deleted : HashSet<ID> = ids_of_deleted_nodes (
      &raw_touched, prepared . graph_before_save (), prepared . final_candidate () );
    SaveAffectedIds { before, after, raw_touched, deleted }
  }

  pub(crate) fn matching_identity (
    &self,
    gnid  : &ID,
    before : &InRustGraph,
    after  : &InRustGraph,
  ) -> Option<ID> {
    if self . raw_touched . contains (gnid) {
      return Some (gnid . clone ()); }
    if let Some (canonical) = before . pid_of (gnid) {
      if self . before . contains (&canonical) {
        return Some (canonical); }}
    if let Some (canonical) = after . pid_of (gnid) {
      if self . after . contains (&canonical) {
        return Some (canonical); }}
    None
  }

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
    // todo | PITFALL : Matching every reference is more conservative than
    // necessary, but much more convenient than classifying dependencies.
    // A save of A and a dirty view of B can conflict merely because both
    // link to X, even when B does not display X's changing herald.
    let mut matching : Vec<ID> = dependencies . iter ()
      .filter_map (|gnid| affected . matching_identity (gnid, before, after))
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
  let mut result : HashSet<ID> = dependencies_from_viewforest (&forest);
  result . extend (links_from_text (text) . into_iter ()
    .map (|link| link . gnid));
  Ok (result)
}

fn dependencies_from_viewforest (
  forest : &ViewForest,
) -> HashSet<ID> {
  let mut result : HashSet<ID> = HashSet::new ();
  for node in forest . nodes () {
    match &node . value () . kind {
      ViewnodeKind::Vognode (Vognode::Occurrence (occurrence)) => {
        result . insert (occurrence . gnid . clone ());
        result . extend (occurrence . overridesHere . iter () . cloned ());
        if let Editability::Editable {
          edit_request : Some (NodeEditRequest::NodeMerge (target)), ..
        } = &occurrence . editability
        { result . insert (target . clone ()); }}
      ViewnodeKind::Vognode (Vognode::Unknown (unknown)) => {
        result . insert (unknown . gnid . clone ()); }
      ViewnodeKind::Vognode (Vognode::Restricted (restricted)) => {
        result . insert (restricted . gnid . clone ()); }
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
