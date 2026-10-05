use crate::dbs::in_rust_graph::InRustGraph;
use crate::from_text::buffer_to_viewnodes::uninterpreted::org_to_uninterpreted_viewforest;
use crate::save::PreparedSave;
use crate::serve::ViewsState;
use crate::serve::handlers::save_buffer::ClientViewSnapshot;
use crate::types::maybe_placed_viewnode::{
  MpPhantom, MpViewnodeKind, MpVognode,
};
use crate::types::misc::ID;
use crate::types::links::links_from_text;
use crate::types::tree::forest::{MpViewForest, ViewForest};
use crate::types::viewnode::{
  Editability, NodeEditRequest, Phantom, Property, ViewnodeKind, Vognode,
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
    skgid  : &ID,
    before : &InRustGraph,
    after  : &InRustGraph,
  ) -> Option<ID> {
    if self . raw_touched . contains (skgid) {
      return Some (skgid . clone ()); }
    if let Some (canonical) = before . pid_of (skgid) {
      if self . before . contains (&canonical) {
        return Some (canonical); }}
    if let Some (canonical) = after . pid_of (skgid) {
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
        *uri != saved_uri && state . pids . iter () . any (|skgid|
          self . matching_identity (skgid, before, after) . is_some ()) })
      .map (|(uri, _)| uri . clone ())
      .collect ();
    uris . sort_by_key (ViewUri::repr_in_client);
    uris
  }
}

pub(crate) struct DirtyViewConflict {
  pub(crate) uri : ViewUri,
  pub(crate) skgids : Vec<ID>,
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
        buffer_snapshot . uri . repr_in_client ())) ?;
    let current : &str = buffer_snapshot . current . as_deref () . ok_or_else (||
      format! ("Dirty view {} has no current text",
               buffer_snapshot . uri . repr_in_client ())) ?;
    let mut dependencies : HashSet<ID> = dependencies_from_text (
      baseline, &buffer_snapshot . uri) ?;
    dependencies . extend (dependencies_from_text (
      current, &buffer_snapshot . uri) ?);
    if let Some (registered) = views_state . open_views
        . viewuri_to_view (&buffer_snapshot . uri)
    { dependencies . extend (dependencies_from_registered (registered)); }
    // todo | PITFALL : Matching every reference is more conservative than
    // necessary, but much more convenient than classifying dependencies.
    // A save of A and a dirty view of B can conflict merely because both
    // link to X, even when B does not display X's changing herald.
    let mut matching : Vec<ID> = dependencies . iter ()
      .filter_map (|skgid| affected . matching_identity (skgid, before, after))
      .collect ();
    matching . sort_by (|left, right| left . 0 . cmp (&right . 0));
    matching . dedup ();
    if ! matching . is_empty () {
      conflicts . push (DirtyViewConflict {
        uri : buffer_snapshot . uri . clone (), skgids : matching, }); }}
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
  result . extend (links_from_text (text) . into_iter ()
    .map (|link| link . skgid));
  Ok (result)
}

fn dependencies_from_uninterpreted (
  forest : &MpViewForest,
) -> HashSet<ID> {
  let mut result : HashSet<ID> = HashSet::new ();
  for node in forest . nodes () {
    match &node . value () . kind {
      MpViewnodeKind::Vognode (MpVognode::Active (active)) => {
        result . extend (active . skgid . iter () . cloned ());
        result . extend (active . viewStats . overridesHere . iter () . cloned ());
        if let Editability::Editable {
          edit_request : Some (NodeEditRequest::NodeMerge (target)), ..
        } = &active . editability
        { result . insert (target . clone ()); }}
      MpViewnodeKind::Vognode (MpVognode::Phantom (MpPhantom::Diff (phantom))) =>
        result . extend (phantom . skgid . iter () . cloned ()),
      MpViewnodeKind::Vognode (MpVognode::Phantom (MpPhantom::Deleted (phantom))) => {
        result . insert (phantom . skgid . clone ()); }
      MpViewnodeKind::Vognode (MpVognode::Phantom (MpPhantom::Unknown (phantom))) => {
        result . insert (phantom . skgid . clone ()); }
      MpViewnodeKind::Property (Property::ID { skgid, .. }) => {
        result . insert (skgid . clone ()); }
      _ => {}, }}
  result
}

fn dependencies_from_registered (
  forest : &ViewForest,
) -> HashSet<ID> {
  let mut result : HashSet<ID> = HashSet::new ();
  for node in forest . nodes () {
    match &node . value () . kind {
      ViewnodeKind::Vognode (Vognode::Active (active)) => {
        result . insert (active . skgid . clone ());
        result . extend (active . viewStats . overridesHere . iter () . cloned ());
        if let Editability::Editable {
          edit_request : Some (NodeEditRequest::NodeMerge (target)), ..
        } = &active . editability
        { result . insert (target . clone ()); }}
      ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Diff (phantom))) => {
        result . insert (phantom . skgid . clone ()); }
      ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Deleted (phantom))) => {
        result . insert (phantom . skgid . clone ()); }
      ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Unknown (phantom))) => {
        result . insert (phantom . skgid . clone ()); }
      ViewnodeKind::Property (Property::ID { skgid, .. }) => {
        result . insert (skgid . clone ()); }
      _ => {}, }}
  result
}

pub(crate) fn format_conflict_error (
  conflicts : &[DirtyViewConflict],
) -> String {
  let by_view : HashMap<String, String> = conflicts . iter ()
    .map (|conflict| (
      conflict . uri . repr_in_client (),
      conflict . skgids . iter () . map (ToString::to_string)
        .collect::<Vec<String>> () . join (", ")))
    .collect ();
  let mut entries : Vec<(String, String)> = by_view . into_iter () . collect ();
  entries . sort_by (|left, right| left . 0 . cmp (&right . 0));
  format! (
    "NOTHING WAS SAVED: this save conflicts with unsaved dependencies in {}. Archive those edits with skg-show-unsaved-changes (Emacs) or :SkgShowUnsavedChanges (Neovim), close the archived views, and retry. Conflicts: {}",
    entries . iter () . map (|entry| entry . 0 . as_str ())
      .collect::<Vec<&str>> () . join (", "),
    entries . iter () . map (|(uri, skgids)| format! ("{} [{}]", uri, skgids))
      .collect::<Vec<String>> () . join ("; "))
}
