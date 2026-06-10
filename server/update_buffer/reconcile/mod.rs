// The per-kind reconcilers view completion (complete_nodes_in_level_order in
// complete.rs) dispatches to. There is no preorder/postorder split: each is run
// at its node's own BFS visit.

pub mod aliascol;
pub mod content;
pub mod hiddeninsubscribee_col;
pub mod hiddenoutsideof_subscribeecol;
pub mod id_col;
pub mod partner_col;
pub mod subscribee_col;
pub mod view_requests;

use crate::source_sets::ActiveSourceSet;
use crate::types::misc::{ID, SourceName};
use crate::types::viewnode::{ViewNode, ViewNodeKind, Vognode};
use ego_tree::{NodeId, Tree};
use std::collections::HashSet;

/// TODO/full-schema/9-2_source-set-safety.org: rendering omits
/// inactive members from goal lists (no placeholders).  'retained'
/// names the exceptions: ids this view keeps on screen despite
/// inactivity (an InactiveNode with view-children).  A member whose
/// source cannot be resolved is omitted too: it might be private,
/// and rendering must not leak; saving preserves it regardless (the
/// weave / set-difference merge treat unresolvable as invisible).
pub fn omit_inactive_members (
  goal     : Vec<ID>,
  active   : Option<&ActiveSourceSet>,
  retained : &HashSet<ID>,
  resolve  : impl Fn (&ID) -> Option<SourceName>,
) -> Vec<ID> {
  match active . filter ( |a| ! a . is_all () ) {
    None => goal,
    Some (a) =>
      goal . into_iter ()
      . filter ( |id|
          retained . contains (id)
          || resolve (id)
             . map ( |src| a . contains_source (&src) )
             . unwrap_or (false) )
      . collect (), }}

/// The ids of this node's InactiveNode children that have
/// view-children: the retained case, kept on screen (and in goal
/// lists) because their children have lives of their own.
pub fn retained_inactive_children (
  tree   : &Tree<ViewNode>,
  parent : NodeId,
) -> HashSet<ID> {
  match tree . get (parent) {
    None => HashSet::new (),
    Some (node_ref) =>
      node_ref . children ()
      . filter ( |c| c . has_children ()
                     && matches! ( &c . value () . kind,
                          ViewNodeKind::Vognode (Vognode::Inactive (_)) ) )
      . filter_map ( |c| match &c . value () . kind {
          ViewNodeKind::Vognode (Vognode::Inactive (i))
            => Some (i . id . clone ()),
          _ => None } )
      . collect (), }}
