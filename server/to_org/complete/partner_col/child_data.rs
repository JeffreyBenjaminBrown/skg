/// Shared per-child information for PartnerCol reconciliation.
///
/// Used by the rerender-time completers for SubscribeeCol,
/// HiddenInSubscribeeCol, and HiddenOutsideOfSubscribeeCol.
///
/// Vocabulary for this module:
///
/// - A `goal_list` is the ordered list of node IDs that a col
///   should present after completion.  The list is computed from the
///   graph and, in diff views, from git-diff state.
/// - A goal child is a child ViewNode whose ActiveNode ID appears in
///   that `goal_list`, whether it already existed in the buffer or
///   was created during reconciliation.
/// - A relevant child is one this reconciliation pass is allowed to
///   manage: for PartnerCols, an ActiveNode marked parentIs=affected.
///   Relevant children whose IDs are not in the goal list are removed
///   or otherwise demoted by the caller-specific cleanup step.
/// - `ChildData` is the pre-fetched title/source/phantom metadata
///   needed to create any missing goal child without querying while
///   the tree is being mutated.

use crate::types::env::SkgEnv;
use crate::types::git::{ExistenceAxes, MembershipAxes, Sign, SourceDiff};
use crate::types::misc::{ID, SourceName};
use crate::types::phantom::title_for_phantom;
use crate::dbs::node_lookup::nodecomplete_rustFirst_by_pid_and_source;
use crate::types::nodes::complete::NodeComplete;
use crate::types::viewnode::{ViewNode, ViewNodeKind, Vognode, ParentIs, PartnerCol, mk_indefinitive_viewnode, mk_phantom_viewnode};
use crate::update_buffer::util::{complete_relevant_children_in_viewnodetree, RepairSummary};
use crate::update_buffer::util::treat_certain_children;

use ego_tree::{NodeId, NodeRef, Tree};
use std::collections::{HashMap, HashSet};
use std::error::Error;

/// Per-child information needed to build a viewnode for a sharing
/// col's child (subscribee, hidden-in-subscribee, or
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
/// prepare child data, then reconcile col children.
///
/// `axes_for_removed` supplies each removed member's per-stage diff
/// axes.  The caller must name the relation its col represents when
/// building that closure (outbound cols call `phantom_axes` with
/// their relation; inbound cols read the inverse scan; filter cols
/// compare derived membership), so an axis can never silently come
/// from a different relation involving the same ID.
pub fn build_child_data (
  tree                           : &Tree<ViewNode>,
  col_node                       : NodeId,
  goal_list                      : &[ID],
  removed_ids                    : &HashSet<ID>,
  axes_for_removed               : &dyn Fn (&ID, &SourceName)
                                     -> (ExistenceAxes, MembershipAxes),
  source_diffs                   : &Option<HashMap<SourceName, SourceDiff>>,
  deleted_since_head_pid_src_map : &HashMap<ID, SourceName>,
  env                            : &SkgEnv,
) -> Result<HashMap<ID, ChildData>, Box<dyn Error>> {
  let existing_children : HashMap<ID, (SourceName, String)> = {
    let node_ref : NodeRef<ViewNode> =
      tree . get (col_node)
        . ok_or ("build_child_data: node not found") ?;
    let mut m : HashMap<ID, (SourceName, String)> = HashMap::new ();
    for child_ref in node_ref . children () {
      if let ViewNodeKind::Vognode (Vognode::Active (t))
        = & child_ref . value () . kind
        { m . insert ( t . id . clone (),
                       ( t . source . clone (),
                         t . title . clone () )); }}
    m };
  let mut result : HashMap<ID, ChildData> = HashMap::new ();
  for child_skgid in goal_list {
    if result . contains_key (child_skgid) { continue; }
    if removed_ids . contains (child_skgid) {
      // A removed-member diff-phantom is a *non-Active* viewnode. If its
      // source can't be determined, fall back to the NOT_FOUND sentinel
      // rather than aborting the whole render (TODO/DONE/local-view-update/plan_v2.org §7.6).
      let child_src : SourceName =
        env . find_source (child_skgid, deleted_since_head_pid_src_map)
        . unwrap_or_else ( SourceName::not_found );
      let axes : (ExistenceAxes, MembershipAxes) =
        axes_for_removed ( child_skgid, &child_src );
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

/// Reconcile a PartnerCol's children against a goal list.
///
/// The rerender-time PartnerCol completers
/// (SubscribeeCol, HiddenInSubscribeeCol,
/// HiddenOutsideOfSubscribeeCol) share the same call shape: build
/// per-child data before mutation, then call
/// `complete_relevant_children_in_viewnodetree` with identical
/// relevance/key/create closures. Phantom-flagged ChildData entries
/// produce phantom viewnodes; non-phantom entries produce
/// indefinitive viewnodes marked ParentIs::Affected.
pub fn reconcile_partnerCol_children_against_goal_list (
  tree          : &mut Tree<ViewNode>,
  col_node      : NodeId,
  kind          : PartnerCol,
  goal_list     : &[ID],
  child_data    : &HashMap<ID, ChildData>,
) -> Result<RepairSummary<ID>, Box<dyn Error>> {
  let label : &'static str = kind . caller_label ();
  let summary : RepairSummary<ID> =
    complete_relevant_children_in_viewnodetree (
    tree, col_node,
    // An InactiveNode child is IRRELEVANT
    // (TODO/full-schema/9-2_source-set-safety.org): the goal omits
    // every inactive member, and a retained placeholder already in the
    // col survives as an irrelevant child (preserved as-is, not
    // goal-matched), so it needs no id.
    |vn : &ViewNode| match &vn . kind {
      ViewNodeKind::Vognode (Vognode::Active (t))
        => t . parentIs == ParentIs::Affected,
      _ => false },
    |vn : &ViewNode| match &vn . kind {
      ViewNodeKind::Vognode (Vognode::Active (t))
        => Ok ( t . id . clone () ),
      _ => Err ( format! (
        "{}: relevant child not a normal graph node", label )) },
    goal_list,
    |id : &ID| {
      let d : &ChildData =
        child_data . get (id) . ok_or_else (
          || format! ( "{}: child data not pre-fetched for {}",
                       label, id . 0 )) ?;
      Ok ( match d . phantom {
        None => mk_indefinitive_viewnode ( id . clone (),
                                           d . source . clone (),
                                           d . title . clone (),
                                           ParentIs::Affected ),
        Some ((ex, mem)) =>
          mk_phantom_viewnode (
            id . clone (), d . source . clone (),
            d . title . clone (), ex, mem ) } ) } ) ?;
  mark_goal_children_as_collectionBranch_members (
    tree, col_node, goal_list) ?;
  Ok (summary) }

/// Stamp per-stage membership signs onto a col's existing Active
/// members, from a per-member axes map (an outbound col reads the
/// owner's relation diff via 'outbound_member_axes'; an inbound col
/// reads the inverse scan):
/// - a member PRESENT after both stages gets only its Plus signs
///   ('newM'), mirroring 'mark_membership_on_existing_children's
///   rule for content children (Minus positions are phantoms,
///   handled by the goal list);
/// - an Active child whose net result is REMOVED -- reachable only
///   when a stale saved buffer still holds, as an Active member, a
///   read-only-col member whose edge is gone -- gets the full axes
///   and flips to a phantom, so the rendered buffer cannot show a
///   removed edge as a live member.
pub fn apply_membership_axes_to_col_members (
  tree       : &mut Tree<ViewNode>,
  col_node   : NodeId,
  axes_by_id : &HashMap<ID, MembershipAxes>,
) -> Result<(), Box<dyn Error>> {
  if axes_by_id . is_empty () { return Ok (( )); }
  treat_certain_children (
    tree, col_node,
    |vn : &ViewNode| matches! (
      &vn . kind, ViewNodeKind::Vognode (Vognode::Active (_)) ),
    |vn : &mut ViewNode| {
      if let ViewNodeKind::Vognode (Vognode::Active (t))
        = &mut vn . kind
      { if let Some (m) = axes_by_id . get (&t . id) {
          if m . net_is_present () {
            if m . staged   == Some (Sign::Plus)
              { t . membership . staged   = Some (Sign::Plus); }
            if m . unstaged == Some (Sign::Plus)
              { t . membership . unstaged = Some (Sign::Plus); }
          } else {
            t . membership = *m; }}}
      vn . normal_to_phantom (); // flips only when the axes require
    } )
    . map_err ( |e| -> Box<dyn Error> { e . into () } ) ?;
  Ok (( )) }

/// See this module's header for definition of "goal child".
///
/// This function repairs surviving or newly matched goal children whose
/// membership marker is stale, so the col continues to own them
/// as generated collection members.
fn mark_goal_children_as_collectionBranch_members (
  tree          : &mut Tree<ViewNode>,
  col_node      : NodeId,
  goal_list     : &[ID],
) -> Result<(), Box<dyn Error>> {
  let goal_set : HashSet<ID> =
    goal_list . iter () . cloned () . collect ();
  treat_certain_children (
    tree, col_node,
    |vn : &ViewNode| match &vn . kind {
      ViewNodeKind::Vognode (Vognode::Active (t)) =>
        goal_set . contains (&t . id)
        && ! t . should_be_diffPhantom (),
      _ => false },
    |vn : &mut ViewNode| {
      if let ViewNodeKind::Vognode (Vognode::Active (t))
        = &mut vn . kind
        { t . parentIs = ParentIs::Affected; }} )
    . map_err ( |e| -> Box<dyn Error> { e . into () } ) ?;
  Ok (( )) }
