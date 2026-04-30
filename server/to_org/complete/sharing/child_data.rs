/// Shared per-child information for sharing-scaffold reconciliation.
///
/// Used by the rerender-time completers for SubscribeeCol,
/// HiddenInSubscribeeCol, and HiddenOutsideOfSubscribeeCol. Each
/// completer used to define a near-identical local copy of this
/// struct and a near-identical `build_*_child_data` helper.

use crate::env::SkgEnv;
use crate::types::git::{ExistenceAxes, MembershipAxes, SourceDiff};
use crate::types::misc::{ID, SourceName};
use crate::types::phantom::{title_for_phantom, phantom_axes};
use crate::types::memory::nodecomplete_from_memory_or_disk;
use crate::types::nodes::complete::NodeComplete;
use crate::types::viewnode::{ViewNode, ViewNodeKind, Birth, mk_indefinitive_viewnode, mk_phantom_viewnode};
use crate::update_buffer::util::complete_relevant_children_in_viewnodetree;

use ego_tree::{NodeId, NodeRef, Tree};
use std::collections::{HashMap, HashSet};
use std::error::Error;

/// Per-child information needed to build a viewnode for a sharing
/// scaffold's child (subscribee, hidden-in-subscribee, or
/// hidden-outside-of-subscribees).
///
/// `phantom: None` => normal indef ContentOf child.
/// `phantom: Some(axes)` => diff-view phantom marking removal.
pub struct ChildData {
  pub source  : SourceName,
  pub title   : String,
  pub phantom : Option<(ExistenceAxes, MembershipAxes)>,
}

/// Build a map from child ID to ChildData for the create-child
/// closure of `complete_relevant_children_in_viewnodetree`.
/// Pre-computes everything so the closure captures only owned
/// data, avoiding conflict with the &mut Tree borrow inside the
/// reconciler.
///
/// `parent_skgid` and `parent_source` are the col's containing
/// node (the subscribee for HiddenIn; the subscriber for
/// SubscribeeCol and HiddenOutsideOf). They feed into
/// `phantom_axes`'s membership-side axes.
pub fn build_child_data (
  tree                           : &Tree<ViewNode>,
  scaffold_node                  : NodeId,
  parent_skgid                   : &ID,
  parent_source                  : &SourceName,
  goal_list                      : &[ID],
  removed_ids                    : &HashSet<ID>,
  source_diffs                   : &Option<HashMap<SourceName, SourceDiff>>,
  deleted_since_head_pid_src_map : &HashMap<ID, SourceName>,
  env                            : &SkgEnv,
) -> Result<HashMap<ID, ChildData>, Box<dyn Error>> {
  let existing_children : HashMap<ID, (SourceName, String)> = {
    let node_ref : NodeRef<ViewNode> =
      tree . get (scaffold_node)
        . ok_or ("build_child_data: node not found") ?;
    let mut m : HashMap<ID, (SourceName, String)> = HashMap::new ();
    for child_ref in node_ref . children () {
      if let ViewNodeKind::True (t) = & child_ref . value () . kind {
        m . insert ( t . id . clone (),
                     ( t . source . clone (),
                       t . title . clone () )); }}
    m };
  let mut result : HashMap<ID, ChildData> = HashMap::new ();
  for child_skgid in goal_list {
    if result . contains_key (child_skgid) { continue; }
    if removed_ids . contains (child_skgid) {
      let child_src : SourceName =
        env . find_source (child_skgid, deleted_since_head_pid_src_map)
        . ok_or_else ( || -> Box<dyn Error> { format! (
          "build_child_data: no source found for {}", child_skgid . 0
        ) . into () } ) ?;
      let axes : (ExistenceAxes, MembershipAxes) =
        phantom_axes ( child_skgid, &child_src,
                       parent_skgid, parent_source,
                       source_diffs . as_ref () );
      let child_title : String =
        title_for_phantom ( child_skgid, &child_src,
                            source_diffs . as_ref (), &env . config );
      result . insert ( child_skgid . clone (),
                        ChildData { source  : child_src,
                                    title   : child_title,
                                    phantom : Some (axes) } );
    } else if let Some ( (s, t) ) = existing_children . get (child_skgid) {
      result . insert ( child_skgid . clone (),
                        ChildData { source  : s . clone (),
                                    title   : t . clone (),
                                    phantom : None } );
    } else {
      let child_src : SourceName =
        env . find_source (child_skgid, deleted_since_head_pid_src_map)
        . ok_or_else ( || -> Box<dyn Error> { format! (
          "build_child_data: no source found for {}", child_skgid . 0
        ) . into () } ) ?;
      let skg : NodeComplete = nodecomplete_from_memory_or_disk (
        &env . config, child_skgid, &child_src ) ?;
      result . insert ( child_skgid . clone (),
                        ChildData { source  : skg . source . clone (),
                                    title   : skg . title . clone (),
                                    phantom : None } ); }}
  Ok (result) }

/// Reconcile a sharing-scaffold's children against a goal list.
///
/// All three rerender-time sharing-scaffold completers
/// (SubscribeeCol, HiddenInSubscribeeCol,
/// HiddenOutsideOfSubscribeeCol) share the same call shape: pre-build
/// per-child data, then call
/// `complete_relevant_children_in_viewnodetree` with identical
/// relevance/key/create closures. Phantom-flagged ChildData entries
/// produce phantom viewnodes; non-phantom entries produce
/// indefinitive ContentOf viewnodes.
pub fn reconcile_sharing_scaffold_children (
  tree          : &mut Tree<ViewNode>,
  scaffold_node : NodeId,
  goal_list     : &[ID],
  child_data    : &HashMap<ID, ChildData>,
  caller_label  : &'static str,
) -> Result<(), Box<dyn Error>> {
  complete_relevant_children_in_viewnodetree (
    tree, scaffold_node,
    |vn : &ViewNode| matches! ( &vn . kind,
                                ViewNodeKind::True (t)
                                if !t . parent_ignores_it () ),
    |vn : &ViewNode| match &vn . kind {
      ViewNodeKind::True (t) => t . id . clone (),
      _ => panic! ( "{}: relevant child not TrueNode", caller_label ) },
    goal_list,
    |id : &ID| {
      let d : &ChildData =
        child_data . get (id) . unwrap_or_else (
          || panic! ( "{}: child data not pre-fetched", caller_label ));
      match d . phantom {
        None =>
          mk_indefinitive_viewnode (
            id . clone (), d . source . clone (),
            d . title . clone (), Birth::ContentOf ),
        Some ((ex, mem)) =>
          mk_phantom_viewnode (
            id . clone (), d . source . clone (),
            d . title . clone (), ex, mem ) } } ) }
