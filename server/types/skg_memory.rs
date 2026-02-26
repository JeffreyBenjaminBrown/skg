use crate::types::misc::{ID, SourceName};
use crate::types::skgnode::SkgNode;
use crate::types::viewnode::{ViewUri, ViewNode, ViewNodeKind};

use ego_tree::Tree;
use std::collections::{HashMap, HashSet};

//
// Type declarations
//

/// Persistent cross-request cache of all SkgNodes currently displayed
/// in any Emacs view. The pool is the authoritative in-memory store;
/// SkgNodeMap is the per-request transactional layer that shadows it.
/// See also: SkgNodeMap (the per-request working set).
pub struct SkgnodesInMemory {
  pub pool        : HashMap<ID, SkgNode>,
  views           : HashMap<ViewUri, ViewState>, // TODO ? OPTIMIZE:
  // The reverse lookup (PID -> views) is computed by scanning `views` via views_containing(). This is O(views) per call, fine for < 10 views. If the number of views grows large, consider a bijective map (HashMap<ID, HashSet<ViewUri>> maintained alongside this one) for O(1) reverse lookups.
  pub id_resolver : HashMap<ID, (ID, SourceName)>, // TODO ? OPTIMIZE:
  // id_resolver is not yet wired into the render pipeline (which would require threading &mut SkgnodesInMemory through render_initial_forest_bfs and complete_viewtree). If pid_and_source_from_id remains a bottleneck after pool seeding, wire id_resolver into skgnode_and_viewnode_from_id, or apply the batch-query optimization from typedb-batching.org.
}

/// Invariant: all forest mutations must go through register_view /
/// update_view, which maintain pids in sync with the forest.
/// Direct forest mutation would make pids stale.
pub struct ViewState {
  pub forest : Tree<ViewNode>,
  pub pids   : HashSet<ID>, // all the TrueNodes (and DeletedNodes) in the buffer
}

//
// Implementations
//

impl SkgnodesInMemory {
  pub fn new () -> Self {
    SkgnodesInMemory {
      pool        : HashMap::new (),
      views       : HashMap::new (),
      id_resolver : HashMap::new () }}

  pub fn pids_in_view (
    &self,
    uri : &ViewUri,
  ) -> Vec<ID> {
    self . views . get ( uri )
      . map ( |vs| vs . pids . iter () . cloned () . collect () )
      . unwrap_or_default () }

  pub fn forest_in_view (
    &self,
    uri : &ViewUri,
  ) -> Option<&Tree<ViewNode>> {
    self . views . get ( uri )
      . map ( |vs| &vs . forest ) }

  pub fn register_view (
    &mut self,
    uri    : ViewUri,
    forest : Tree<ViewNode>,
    pids   : &[ID],
  ) { let state : ViewState =
        ViewState { forest,
                    pids : pids . iter () . cloned () . collect () };
      self . views . insert ( uri, state ); }

  pub fn update_view (
    &mut self,
    uri        : &ViewUri,
    new_forest : Tree<ViewNode>,
  ) { let pids : HashSet<ID> =
        new_forest . root () . descendants ()
        . filter_map ( |n| match &n . value () . kind {
          ViewNodeKind::True (t)    => Some ( t . id . clone () ),
          ViewNodeKind::Deleted (d) => Some ( d . id . clone () ),
          _ => None } )
        . collect ();
      if let Some (vs)
        = self . views . get_mut (uri)
        { vs . forest = new_forest;
          vs . pids = pids; }
      else { self . views . insert (
               uri . clone (),
               ViewState { forest : new_forest,
                           pids } ); }
      self . gc (); }

  pub fn unregister_view (
    &mut self,
    uri : &ViewUri,
  ) { self . views . remove ( uri );
      self . gc (); }

  pub fn views_containing (
    &self,
    pid : &ID,
  ) -> Vec<ViewUri> {
    self . views . iter ()
      . filter ( |(_, vs)| vs . pids . contains ( pid ) )
      . map ( |(uri, _)| uri . clone () )
      . collect () }

  // SCALING NOTE: gc() is O(views * pids_per_view). For typical usage
  // (< 10 views, < 1000 nodes each) this is negligible. If it becomes
  // a bottleneck, replace with reference counting: increment on
  // register/update, decrement on update/unregister, remove at zero.
  fn gc (&mut self) {
    let referenced : HashSet<ID> =
      self . views . values ()
      . flat_map ( |vs| vs . pids . iter () . cloned () )
      . collect ();
    self . pool . retain ( |pid, _| referenced . contains ( pid ));
    self . id_resolver . retain (
      |_, (target_pid, _)| referenced . contains ( target_pid )); }
}
