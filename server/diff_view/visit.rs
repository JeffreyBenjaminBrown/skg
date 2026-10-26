//! Where view completion reads its mode ('CompletionContext::mode',
//! 'server/update_buffer/completion_mode.rs'): once before a node's
//! visit extends anything ('diffView_visit_before_extension') and once
//! after an unrestricted vognode's visit ('diffView_visit_after_extension').
//! ('docs/diff-view.org', "Completion over the straddle".) In diff-view
//! mode, completion changes so:
//! - at an editable node's visit, its children are aligned with the
//!   straddle before the content reconciler runs
//!   ('CompletionMode::align_content_members_before_extension');
//! - content is shown without override substitution;
//! - a changed property folder is skipped at its own visit, since the
//!   per-visit step reconciled it with the union of the straddle; any
//!   other property folder is refreshed from the after-graph there, as
//!   in every view;
//! - an occurrence whose node vanished becomes its ghost when the
//!   before-graph has the node ('vanished_occurrence_becomes_ghost'),
//!   and each ghost's visit adds its old content
//!   ('process_historical_phantom_visit'), unless the user asked to
//!   entombstone it ('(viewRequests tombstone)');
//! - no default partner folders and no hidden-in-subscribee folder
//!   are created;
//! - the node budget bounds only the extensions an editable-view
//!   request makes outside U ('CompletionMode::charges_node_budget');
//! - each unrestricted vognode's visit ends with the per-visit step
//!   ('process_unrestrictedVognode_changes');
//! - the partner folders show the union of the straddle (the
//!   Subscribee folder too, through the generic folder reconciler).
//! The caller then sets the readiness facts
//! ('set_diffView_facts_in_viewforest').

use crate::diff_view::changes::{
  HistoricalPhantomVisit, entombstone_ghost,
  process_historical_phantom_visit, process_unrestrictedVognode_changes};
use crate::types::viewnode::{Viewnode, ViewnodeKind, Vognode};
use crate::update_buffer::completion_mode::CompletionMode;

use ego_tree::{NodeId, Tree};
use std::collections::HashSet;
use std::error::Error;

/// What view completion does at a node's visit, as
/// 'diffView_visit_before_extension' decides.
pub enum DiffViewVisit {
  /// The diff view did the whole visit.
  Done,
  /// Completion visits the node. With 'visit_as_unrestrictedVognode',
  /// the node was a ghost whose node is back, and is now
  /// an editable vognode: completion visits it as one.
  Complete { visit_as_unrestrictedVognode : bool },
}

/// What became of an unrestricted vognode at its visit, before
/// 'diffView_visit_after_extension'.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum NormalNodeUpdate {
  /// The graph lacks its node: it is now a ghost, or a tombstone.
  NodeVanished,
  /// Its update made it something other than an unrestricted vognode.
  NoLongerUnrestricted,
  /// It is still an unrestricted vognode.
  Updated,
}

/// The diff view's part of a visit, before the node extends: outside
/// diff-view mode, an ordinary visit. In it, a changed property
/// folder's visit is done (the per-visit step reconciled it), and any
/// other property folder is visited as in every view, so that one the
/// user requested stays fresh; a ghost the user asked to entombstone
/// becomes a tombstone; any other ghost adds its old
/// content, and is then visited as an editable vognode only if its
/// node is back.
pub fn diffView_visit_before_extension (
  tree                       : &mut Tree<Viewnode>,
  treeid                     : NodeId,
  kind                       : &ViewnodeKind,
  mode                       : CompletionMode<'_>,
) -> Result<DiffViewVisit, Box<dyn Error>> {
  let CompletionMode::DiffView { straddle, buffer_kind } = mode else {
    return Ok ( DiffViewVisit::Complete {
      visit_as_unrestrictedVognode : false } ); };
  let visit_as_unrestrictedVognode : bool = match kind {
    ViewnodeKind::Folder (folder)
      if folder . members_are_properties ()
      && property_folder_is_changed (tree, treeid) =>
      return Ok (DiffViewVisit::Done),
    ViewnodeKind::Vognode (Vognode::Ghost (ghost))
      if ghost . tombstone_request => {
      entombstone_ghost (tree, treeid, straddle) ?;
      return Ok (DiffViewVisit::Done); },
    // A ghost where content stands adds its old content.
    ViewnodeKind::Vognode (Vognode::Ghost (_)) => {
      if process_historical_phantom_visit (
           tree, treeid, straddle, buffer_kind ) ?
        == HistoricalPhantomVisit::Done {
        return Ok (DiffViewVisit::Done); }
      true },
    _ => false, };
  Ok ( DiffViewVisit::Complete { visit_as_unrestrictedVognode } ) }

/// The diff view's part at the end of an unrestricted vognode's visit:
/// the ghost that replaced an occurrence whose node vanished is
/// visited as a ghost; one still unrestricted gets the per-visit step.
/// Nothing outside diff-view mode.
pub fn diffView_visit_after_extension (
  tree                       : &mut Tree<Viewnode>,
  treeid                     : NodeId,
  update                     : NormalNodeUpdate,
  mode                       : CompletionMode<'_>,
  nodes_before_completion    : &HashSet<NodeId>,
  editable_before_completion : &HashSet<NodeId>,
) -> Result<(), Box<dyn Error>> {
  let CompletionMode::DiffView { straddle, buffer_kind } = mode
    else { return Ok (( )); };
  match update {
    NormalNodeUpdate::NodeVanished => {
      // A no-op on a tombstone.
      process_historical_phantom_visit (
        tree, treeid, straddle, buffer_kind ) ?; },
    NormalNodeUpdate::NoLongerUnrestricted => {},
    NormalNodeUpdate::Updated =>
      // Tag the content, show what was removed, and add the change
      // folders, markers and properties.
      process_unrestrictedVognode_changes (
        tree, treeid, straddle, buffer_kind,
        ! nodes_before_completion . contains (&treeid),
        editable_before_completion ) ?, }
  Ok (( )) }

/// Whether the property folder at 'treeid' shows a change: some entry
/// carries a '(rel ...)' part, as the per-visit step leaves on the
/// entries of the union, and removes from a folder whose list no longer
/// differs between the graphs.
fn property_folder_is_changed (
  tree   : &Tree<Viewnode>,
  treeid : NodeId,
) -> bool {
  tree . get (treeid) . is_some_and ( |folder|
    folder . children () . any ( |entry|
      entry . value () . change_facts . change . rel . is_some () )) }
