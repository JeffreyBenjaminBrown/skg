use crate::dbs::in_rust_graph::InRustGraph;
use crate::from_text::buffer_to_viewnodes::uninterpreted::org_to_uninterpreted_viewforest;
use crate::save::PreparedSave;
use crate::serve::ViewsState;
use crate::serve::handlers::save_buffer::ClientViewSnapshot;
use crate::types::maybe_placed_viewnode::{
  MpPhantom, MpViewnodeKind, MpVognode,
};
use crate::types::misc::ID;
use crate::types::textlinks::textlinks_from_text;
use crate::types::tree::forest::{MpViewForest, ViewForest};
use crate::types::viewnode::{
  Editability, NodeEditRequest, Phantom, Qual, ViewNodeKind, Vognode,
};
use crate::types::views_state::ViewUri;

use std::collections::{HashMap, HashSet};

pub(crate) struct SaveAffectedIds {
  before      : HashSet<ID>,
  after       : HashSet<ID>,
  raw_touched : HashSet<ID>,
}

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
    SaveAffectedIds { before, after, raw_touched }
  }

  pub(crate) fn matching_identity (
    &self,
    id     : &ID,
    before : &InRustGraph,
    after  : &InRustGraph,
  ) -> Option<ID> {
    if self . raw_touched . contains (id) {
      return Some (id . clone ()); }
    if let Some (canonical) = before . pid_of (id) {
      if self . before . contains (&canonical) {
        return Some (canonical); }}
    if let Some (canonical) = after . pid_of (id) {
      if self . after . contains (&canonical) {
        return Some (canonical); }}
    None
  }

  pub(crate) fn collateral_view_uris (
    &self,
    saved_uri  : &ViewUri,
    views_state : &ViewsState,
    before     : &InRustGraph,
    after      : &InRustGraph,
  ) -> Vec<ViewUri> {
    let mut uris : Vec<ViewUri> = views_state . open_views . views . iter ()
      .filter (|(uri, state)| {
        *uri != saved_uri && state . pids . iter () . any (|id|
          self . matching_identity (id, before, after) . is_some ()) })
      .map (|(uri, _)| uri . clone ())
      .collect ();
    uris . sort_by_key (ViewUri::repr_in_client);
    uris
  }
}

pub(crate) struct DirtyViewConflict {
  pub(crate) uri : ViewUri,
  pub(crate) ids : Vec<ID>,
}

pub(crate) fn dirty_view_conflicts (
  snapshots   : &[ClientViewSnapshot],
  views_state : &ViewsState,
  affected    : &SaveAffectedIds,
  before      : &InRustGraph,
  after       : &InRustGraph,
) -> Result<Vec<DirtyViewConflict>, String> {
  let mut conflicts : Vec<DirtyViewConflict> = Vec::new ();
  for snapshot in snapshots . iter () . filter (|snapshot| snapshot . dirty) {
    let baseline : &str = snapshot . baseline . as_deref () . ok_or_else (||
      format! (
        "Dirty view {} has no verified clean baseline. Run skg-show-unsaved-changes (Emacs) or :SkgShowUnsavedChanges (Neovim), then close or refresh that view before saving.",
        snapshot . uri . repr_in_client ())) ?;
    let current : &str = snapshot . current . as_deref () . ok_or_else (||
      format! ("Dirty view {} has no current text",
               snapshot . uri . repr_in_client ())) ?;
    let mut dependencies : HashSet<ID> = dependencies_from_text (
      baseline, &snapshot . uri) ?;
    dependencies . extend (dependencies_from_text (
      current, &snapshot . uri) ?);
    if let Some (registered) = views_state . open_views
        . viewuri_to_view (&snapshot . uri)
    { dependencies . extend (dependencies_from_registered (registered)); }
    // todo | PITFALL : Matching every reference is more conservative than
    // necessary, but much more convenient than classifying dependencies.
    // A save of A and a dirty view of B can conflict merely because both
    // link to X, even when B does not display X's changing herald.
    let mut matching : Vec<ID> = dependencies . iter ()
      .filter_map (|id| affected . matching_identity (id, before, after))
      .collect ();
    matching . sort_by (|left, right| left . 0 . cmp (&right . 0));
    matching . dedup ();
    if ! matching . is_empty () {
      conflicts . push (DirtyViewConflict {
        uri : snapshot . uri . clone (), ids : matching, }); }}
  Ok (conflicts)
}

fn dependencies_from_text (
  text : &str,
  uri  : &ViewUri,
) -> Result<HashSet<ID>, String> {
  let (forest, errors, _) = org_to_uninterpreted_viewforest (text)
    .map_err (|error| format! (
      "Cannot inspect dirty view {}: {}. Run its recovery command before saving.",
      uri . repr_in_client (), error)) ?;
  if ! errors . is_empty () {
    return Err (format! (
      "Cannot inspect dirty view {} because its dependency metadata is invalid: {}. Run its recovery command before saving.",
      uri . repr_in_client (),
      errors . iter () . map (ToString::to_string)
        .collect::<Vec<String>> () . join ("; "))); }
  let mut result : HashSet<ID> = dependencies_from_uninterpreted (&forest);
  result . extend (textlinks_from_text (text) . into_iter ()
    .map (|textlink| textlink . id));
  Ok (result)
}

fn dependencies_from_uninterpreted (
  forest : &MpViewForest,
) -> HashSet<ID> {
  let mut result : HashSet<ID> = HashSet::new ();
  for node in forest . nodes () {
    match &node . value () . kind {
      MpViewnodeKind::Vognode (MpVognode::Active (active)) => {
        result . extend (active . id . iter () . cloned ());
        result . extend (active . viewStats . overridesHere . iter () . cloned ());
        if let Editability::Definitive {
          edit_request : Some (NodeEditRequest::NodeMerge (target)), ..
        } = &active . editability
        { result . insert (target . clone ()); }}
      MpViewnodeKind::Phantom (MpPhantom::Diff (phantom)) =>
        result . extend (phantom . id . iter () . cloned ()),
      MpViewnodeKind::Phantom (MpPhantom::Deleted (phantom)) => {
        result . insert (phantom . id . clone ()); }
      MpViewnodeKind::Phantom (MpPhantom::Unknown (phantom)) => {
        result . insert (phantom . id . clone ()); }
      MpViewnodeKind::Qual (Qual::ID { id, .. }) => {
        result . insert (id . clone ()); }
      _ => {}, }}
  result
}

fn dependencies_from_registered (
  forest : &ViewForest,
) -> HashSet<ID> {
  let mut result : HashSet<ID> = HashSet::new ();
  for node in forest . nodes () {
    match &node . value () . kind {
      ViewNodeKind::Vognode (Vognode::Active (active)) => {
        result . insert (active . id . clone ());
        result . extend (active . viewStats . overridesHere . iter () . cloned ());
        if let Editability::Definitive {
          edit_request : Some (NodeEditRequest::NodeMerge (target)), ..
        } = &active . editability
        { result . insert (target . clone ()); }}
      ViewNodeKind::Phantom (Phantom::Diff (phantom)) => {
        result . insert (phantom . id . clone ()); }
      ViewNodeKind::Phantom (Phantom::Deleted (phantom)) => {
        result . insert (phantom . id . clone ()); }
      ViewNodeKind::Phantom (Phantom::Unknown (phantom)) => {
        result . insert (phantom . id . clone ()); }
      ViewNodeKind::Qual (Qual::ID { id, .. }) => {
        result . insert (id . clone ()); }
      _ => {}, }}
  result
}

pub(crate) fn format_conflict_error (
  conflicts : &[DirtyViewConflict],
) -> String {
  let by_view : HashMap<String, String> = conflicts . iter ()
    .map (|conflict| (
      conflict . uri . repr_in_client (),
      conflict . ids . iter () . map (ToString::to_string)
        .collect::<Vec<String>> () . join (", ")))
    .collect ();
  let mut entries : Vec<(String, String)> = by_view . into_iter () . collect ();
  entries . sort_by (|left, right| left . 0 . cmp (&right . 0));
  format! (
    "NOTHING WAS SAVED: this save conflicts with unsaved dependencies in {}. Archive those edits with skg-show-unsaved-changes (Emacs) or :SkgShowUnsavedChanges (Neovim), close the archived views, and retry. Conflicts: {}",
    entries . iter () . map (|entry| entry . 0 . as_str ())
      .collect::<Vec<&str>> () . join (", "),
    entries . iter () . map (|(uri, ids)| format! ("{} [{}]", uri, ids))
      .collect::<Vec<String>> () . join ("; "))
}
