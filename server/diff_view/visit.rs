//! Where view completion reads the diff-view switch
//! ('CompletionContext::straddle'): once before a node's
//! visit extends anything ('diffView_visit_before_extension') and once
//! after an unrestricted vognode's visit ('diffView_visit_after_extension').
//! ('docs/diff-view.org', "Completion over the straddle".) In a buffer
//! of the diff view, completion changes so:
//! - at an editable node's visit, its children are aligned with the
//!   straddle before the content reconciler runs
//!   ('align_content_members_with_straddle');
//! - content is shown without override substitution;
//! - a changed property folder is skipped at its own visit, since the
//!   per-visit step reconciled it with the union of the straddle; any
//!   other property folder is refreshed from the after-graph there, as
//!   in every view;
//! - a deleted phantom whose node the before-graph has becomes its
//!   historical phantom, and each historical phantom's visit adds
//!   its old content ('process_historical_phantom_visit');
//! - no default partner folders and no hidden-in-subscribee folder
//!   are created;
//! - the node budget bounds only the extensions an editable-view
//!   request makes outside U ('VisitSettings::charges_node_budget');
//! - each unrestricted vognode's visit ends with the per-visit step
//!   ('process_unrestrictedVognode_changes');
//! - the partner folders show the union of the straddle (the
//!   Subscribee folder too, through the generic folder reconciler).
//! The caller then sets the readiness facts
//! ('set_diffView_facts_in_viewforest').

use crate::diff_view::changes::{
  HistoricalPhantomVisit, align_content_members_with_straddle,
  deleted_phantom_becomes_historical, process_historical_phantom_visit,
  process_unrestrictedVognode_changes};
use crate::diff_view::pruned_change_view::prunedChangeView_sets_of_buffer;
use crate::graph_straddle::types::GraphStraddle;
use crate::types::misc::ID;
use crate::types::viewnode::{Phantom, Viewnode, ViewnodeKind, Vognode};
use crate::types::views_state::DiffViewBufferKind;
use crate::update_buffer::reconcile::partner_folder::PartnerFolderGoalSource;

use ego_tree::{NodeId, Tree};
use std::collections::{BTreeSet, HashSet};
use std::error::Error;

/// What view completion does at a node's visit, as
/// 'diffView_visit_before_extension' decides.
pub enum DiffViewVisit<'a> {
  /// The diff view did the whole visit.
  Done,
  /// Completion visits the node, as these settings say.
  Complete (VisitSettings<'a>),
}

/// How view completion visits a node. Outside the diff view, the same
/// at every node.
#[derive(Clone, Copy)]
pub struct VisitSettings<'a> {
  /// The node was a historical phantom whose node is back, and is
  /// now an editable vognode: visit it as one.
  pub visit_as_unrestrictedVognode        : bool,
  /// Whether the Subscribee folder has its own reconciler. In the
  /// diff view the generic partner-folder reconciler fills it.
  pub subscribeeFolder_has_own_reconciler : bool,
  pub partnerFolder_goal_source           : PartnerFolderGoalSource<'a>,
  /// False in the diff view: it shows raw graph facts.
  pub substitution_enabled                : bool,
  /// Whether default partner folders and the hidden-in-subscribee
  /// folder may be created. Not in the diff view.
  pub default_folders_allowed             : bool,
  /// In the diff view, U of the buffer being rendered.
  diffView_u             : Option<&'a BTreeSet<ID>>,
  /// In the diff view, the straddle an editable node's members are
  /// aligned with before the content reconciler runs.
  straddle_to_align_with : Option<&'a GraphStraddle>,
}

/// What became of an unrestricted vognode at its visit, before
/// 'diffView_visit_after_extension'.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum NormalNodeUpdate {
  /// This save deleted its node: it is now a deleted phantom.
  DeletedBySave,
  /// Its update made it something other than an unrestricted vognode.
  NoLongerUnrestricted,
  /// It is still an unrestricted vognode.
  Updated,
}

/// The diff view's part of a visit, before the node extends: outside
/// the diff view, nothing but the ordinary settings. In it, a changed
/// property folder's visit is done (the per-visit step reconciled it),
/// and any other property folder is visited as in every view, so that
/// one the user requested stays fresh; a historical phantom adds its
/// old content, and is then visited as an editable vognode only if its
/// node is back.
pub fn diffView_visit_before_extension<'a> (
  tree                       : &mut Tree<Viewnode>,
  treeid                     : NodeId,
  kind                       : &ViewnodeKind,
  straddle                   : Option<&'a GraphStraddle>,
  diffView_buffer_kind       : Option<DiffViewBufferKind>,
) -> Result<DiffViewVisit<'a>, Box<dyn Error>> {
  let (Some (straddle), Some (diffView_buffer_kind)) =
    (straddle, diffView_buffer_kind) else {
    return Ok ( DiffViewVisit::Complete ( VisitSettings {
      visit_as_unrestrictedVognode        : false,
      subscribeeFolder_has_own_reconciler : true,
      partnerFolder_goal_source           : PartnerFolderGoalSource::LiveGraph,
      substitution_enabled                : true,
      default_folders_allowed             : true,
      diffView_u                          : None,
      straddle_to_align_with              : None, } )); };
  let visit_as_unrestrictedVognode : bool = match kind {
    ViewnodeKind::PropertyFolder (_)
      if property_folder_is_changed (tree, treeid) =>
      return Ok (DiffViewVisit::Done),
    // A historical phantom where content stands adds its old content.
    ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Historical (_))) => {
      if process_historical_phantom_visit (
           tree, treeid, straddle, diffView_buffer_kind ) ?
        == HistoricalPhantomVisit::Done {
        return Ok (DiffViewVisit::Done); }
      true },
    _ => false, };
  Ok ( DiffViewVisit::Complete ( VisitSettings {
    visit_as_unrestrictedVognode,
    subscribeeFolder_has_own_reconciler : false,
    partnerFolder_goal_source           : PartnerFolderGoalSource::GraphStraddle (straddle),
    substitution_enabled                : false,
    default_folders_allowed             : false,
    diffView_u                          :
      Some (& prunedChangeView_sets_of_buffer (straddle, diffView_buffer_kind) . u),
    straddle_to_align_with              : Some (straddle), } )) }

/// The diff view's part at the end of an unrestricted vognode's visit:
/// a node this save deleted that HEAD has is shown as history; one
/// still unrestricted gets the per-visit step. Nothing outside the
/// diff view.
pub fn diffView_visit_after_extension (
  tree                       : &mut Tree<Viewnode>,
  treeid                     : NodeId,
  update                     : NormalNodeUpdate,
  straddle                   : Option<&GraphStraddle>,
  diffView_buffer_kind       : Option<DiffViewBufferKind>,
  nodes_before_completion    : &HashSet<NodeId>,
  editable_before_completion : &HashSet<NodeId>,
) -> Result<(), Box<dyn Error>> {
  let (Some (straddle), Some (diffView_buffer_kind)) =
    (straddle, diffView_buffer_kind)
    else { return Ok (( )); };
  match update {
    NormalNodeUpdate::DeletedBySave =>
      if deleted_phantom_becomes_historical (tree, treeid, straddle) ? {
        process_historical_phantom_visit (
          tree, treeid, straddle, diffView_buffer_kind ) ?; },
    NormalNodeUpdate::NoLongerUnrestricted => {},
    NormalNodeUpdate::Updated =>
      // Tag the content, show what was removed, and add the change
      // folders, markers and properties.
      process_unrestrictedVognode_changes (
        tree, treeid, straddle, diffView_buffer_kind,
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

impl<'a> VisitSettings<'a> {
  /// Whether this extension of 'pid' is charged to the node budget (see
  /// the module comment of 'to_org/extend/editable.rs'). In an ordinary
  /// view, every extension is. The diff view deviates
  /// ('docs/diff-view.org', "Editability in the diff view"): a node of U
  /// is never charged, whether the pruned change view's rule or a request
  /// made it editable, so the budget bounds only the extensions that an
  /// editable-view request makes outside U.
  pub fn charges_node_budget (
    &self,
    pid                      : &ID,
    had_editableViewRequest  : bool,
  ) -> bool {
    match self . diffView_u {
      None    => true,
      Some (u) => had_editableViewRequest && ! u . contains (pid), } }

  /// In the diff view, align an editable node's members with the
  /// straddle, after the editability rule settles its editability and before
  /// the content reconciler runs ('align_content_members_with_straddle').
  pub fn align_content_members_before_extension (
    &self,
    tree   : &mut Tree<Viewnode>,
    treeid : NodeId,
  ) -> Result<(), Box<dyn Error>> {
    match self . straddle_to_align_with {
      None => Ok (( )),
      Some (straddle) =>
        align_content_members_with_straddle (tree, treeid, straddle), } }
}
