/// Shared per-child information for sharing-scaffold reconciliation.
///
/// Used by the rerender-time completers for SubscribeeCol,
/// HiddenInSubscribeeCol, and HiddenOutsideOfSubscribeeCol. Each
/// completer used to define a near-identical local copy of this
/// struct and a near-identical `build_*_child_data` helper.
///
/// Vocabulary for this module:
///
/// - A `goal_list` is the ordered list of node IDs that a scaffold
///   should present after completion.  The list is computed from the
///   graph and, in diff views, from git-diff state.
/// - A goal child is a child ViewNode whose TrueNode ID appears in
///   that `goal_list`, whether it already existed in the buffer or
///   was created during reconciliation.
/// - A relevant child is one this reconciliation pass is allowed to
///   manage: for sharing scaffolds, a TrueNode marked parentIs=affected.
///   Relevant children whose IDs are not in the goal list are removed
///   or otherwise demoted by the caller-specific cleanup step.
/// - `ChildData` is the pre-fetched title/source/phantom metadata
///   needed to create any missing goal child without querying while
///   the tree is being mutated.

use crate::types::env::SkgEnv;
use crate::types::git::{ExistenceAxes, MembershipAxes, SourceDiff};
use crate::types::misc::{ID, SourceName};
use crate::types::phantom::{title_for_phantom, phantom_axes};
use crate::dbs::node_lookup::nodecomplete_rustFirst_by_pid_and_source;
use crate::types::nodes::complete::NodeComplete;
use crate::types::viewnode::{ViewNode, ViewNodeKind, Vognode, ParentIs, RoleCol, mk_indefinitive_viewnode, mk_phantom_viewnode};
use crate::update_buffer::util::complete_relevant_children_in_viewnodetree;
use crate::update_buffer::util::treat_certain_children;

use ego_tree::{NodeId, NodeRef, Tree};
use std::collections::{HashMap, HashSet};
use std::error::Error;

/// Per-child information needed to build a viewnode for a sharing
/// scaffold's child (subscribee, hidden-in-subscribee, or
/// hidden-outside-of-subscribees).
///
/// `phantom: None` => normal indef child marked ParentIs::Affected.
/// `phantom: Some(axes)` => diff-view phantom marking removal.
pub struct ChildData {
  pub source  : SourceName,
  pub title   : String,
  pub phantom : Option<(ExistenceAxes, MembershipAxes)>,
}

/// Build a map from child ID to ChildData for the create-child
/// closure of `complete_relevant_children_in_viewnodetree`.
/// The sharing completers build this before mutation so their flow
/// stays explicit: read tree and graph facts, compute the goal list,
/// prepare child data, then reconcile scaffold children.
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
      if let ViewNodeKind::Vognode (Vognode::Normal (t))
        = & child_ref . value () . kind
        { m . insert ( t . id . clone (),
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
      let skg : NodeComplete = nodecomplete_rustFirst_by_pid_and_source (
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
/// HiddenOutsideOfSubscribeeCol) share the same call shape: build
/// per-child data before mutation, then call
/// `complete_relevant_children_in_viewnodetree` with identical
/// relevance/key/create closures. Phantom-flagged ChildData entries
/// produce phantom viewnodes; non-phantom entries produce
/// indefinitive viewnodes marked ParentIs::Affected.
pub fn reconcile_sharing_scaffold_children (
  tree          : &mut Tree<ViewNode>,
  scaffold_node : NodeId,
  kind          : RoleCol,
  goal_list     : &[ID],
  child_data    : &HashMap<ID, ChildData>,
) -> Result<(), Box<dyn Error>> {
  let label : &'static str = kind . caller_label ();
  complete_relevant_children_in_viewnodetree (
    tree, scaffold_node,
    |vn : &ViewNode| matches! ( &vn . kind,
                                ViewNodeKind::Vognode (Vognode::Normal (t))
                                if t . parentIs == ParentIs::Affected ),
    |vn : &ViewNode| match &vn . kind {
      ViewNodeKind::Vognode (Vognode::Normal (t))
        => t . id . clone (),
      _ => panic! ( "{}: relevant child not a normal graph node", label ) },
    goal_list,
    |id : &ID| {
      let d : &ChildData =
        child_data . get (id) . unwrap_or_else (
          || panic! ( "{}: child data not pre-fetched", label ));
      match d . phantom {
        None => mk_indefinitive_viewnode ( id . clone (),
                                           d . source . clone (),
                                           d . title . clone (),
                                           ParentIs::Affected ),
        Some ((ex, mem)) =>
          mk_phantom_viewnode (
            id . clone (), d . source . clone (),
            d . title . clone (), ex, mem ) } } ) ?;
  mark_goal_children_as_collectionBranch_members (
    tree, scaffold_node, goal_list) ?;
  Ok (( )) }

/// See this module's header for definition of "goal child".
///
/// This funciton repairs surviving or newly matched goal children whose
/// membership marker is stale, so the scaffold continues to own them
/// as generated collection members.
fn mark_goal_children_as_collectionBranch_members (
  tree          : &mut Tree<ViewNode>,
  scaffold_node : NodeId,
  goal_list     : &[ID],
) -> Result<(), Box<dyn Error>> {
  let goal_set : HashSet<ID> =
    goal_list . iter () . cloned () . collect ();
  treat_certain_children (
    tree, scaffold_node,
    |vn : &ViewNode| match &vn . kind {
      ViewNodeKind::Vognode (Vognode::Normal (t)) =>
        goal_set . contains (&t . id)
        && ! t . should_be_phantom (),
      _ => false },
    |vn : &mut ViewNode| {
      if let ViewNodeKind::Vognode (Vognode::Normal (t))
        = &mut vn . kind
        { t . parentIs = ParentIs::Affected; }} )
    . map_err ( |e| -> Box<dyn Error> { e . into () } ) ?;
  Ok (( )) }
