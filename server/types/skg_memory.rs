use crate::types::misc::{ID, SourceName};
use crate::types::skgnode::SkgNode;
use crate::types::viewnode::ViewUri;

use std::collections::{HashMap, HashSet};

/// Persistent cross-request cache of all SkgNodes currently displayed
/// in any Emacs view. The pool is the authoritative in-memory store;
/// SkgNodeMap is the per-request transactional layer that shadows it.
/// See also: SkgNodeMap (the per-request working set).
pub struct SkgnodesInMemory {
  pub pool        : HashMap<ID, SkgNode>,
  // TODO ? OPTIMIZE:
  // The reverse lookup (PID -> views) is computed by scanning `views` via views_containing(). This is O(views) per call, fine for < 10 views. If the number of views grows large, consider a bijective map (HashMap<ID, HashSet<ViewUri>> maintained alongside this one) for O(1) reverse lookups.
  views           : HashMap<ViewUri, ViewState>,
  pub id_resolver : HashMap<ID, (ID, SourceName)>,
  // TODO ? OPTIMIZE:
  // id_resolver is not yet wired into the render pipeline (which would require threading &mut SkgnodesInMemory through render_initial_forest_bfs and complete_viewtree). If pid_and_source_from_id remains a bottleneck after pool seeding, wire id_resolver into skgnode_and_viewnode_from_id, or apply the batch-query optimization from typedb-batching.org.
}

pub struct ViewState {
  pub root_ids : Vec<ID>,
  pub pids     : HashSet<ID>, // all the TrueNodes in the buffer
}

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

  pub fn root_ids_in_view (
    &self,
    uri : &ViewUri,
  ) -> Option<&[ID]> {
    self . views . get ( uri )
      . map ( |vs| vs . root_ids . as_slice () ) }

  pub fn register_view (
    &mut self,
    uri      : ViewUri,
    root_ids : Vec<ID>,
    pids     : &[ID],
  ) { let state : ViewState =
        ViewState { root_ids,
                    pids : pids . iter () . cloned () . collect () };
      self . views . insert ( uri, state ); }

  pub fn update_view (
    &mut self,
    uri          : &ViewUri,
    new_root_ids : Vec<ID>,
    new_pids     : &[ID],
  ) { if let Some (vs)
        = self . views . get_mut (uri)
        { vs . root_ids = new_root_ids;
          vs . pids = new_pids . iter () . cloned () . collect (); }
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
