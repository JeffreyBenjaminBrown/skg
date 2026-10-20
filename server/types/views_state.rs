use crate::dbs::in_rust_graph::InRustGraph;
use crate::types::many_to_many::ManyToMany;
use crate::types::tree::forest::ViewForest;
use crate::types::viewnode::ViewnodeKind;
use crate::types::viewnode::Vognode;
use super::misc::ID;

use std::collections::{HashMap, HashSet};

//
// Type declarations
//

/// Identifies a buffer in the client.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum ViewId {
  ContentView  (String), // the root skgid
  SearchView   (String), // query
}

/// Per-connection view bookkeeping. Each entry in 'views' is a
/// currently-open buffer (content or search) with its viewforest and PID
/// set. 'root_ids' is the reverse lookup — which view(s) is this
/// ID a root of.
pub struct OpenViews {
  pub views       : HashMap<ViewId, ViewState>,
  // Includes both (graph) content views and search result views. See the definition of 'ViewId'.
  // TODO ? OPTIMIZE:
  // The reverse lookup (PID -> views) is computed by scanning `views` via views_containing(). This is O(views) per call, fine for < 10 views. If the number of views grows large, consider a bijective map (HashMap<ID, HashSet<ViewId>> maintained alongside this one) for O(1) reverse lookups.

  root_skgids        : ManyToMany<ID, ViewId>, // Maps every root ID (primary + extra) ↔ ViewId. Supports many-to-many because a view can have multiple roots, and an ID could be a root in multiple views. Maintained by register_view / update_view / unregister_view.
}

/// Invariant: all viewforest mutations must go through register_view /
/// update_view, which maintain pids in sync with the viewforest.
/// Direct viewforest mutation would make pids stale.
pub struct ViewState {
  pub viewforest : ViewForest,
  pub pids       : HashSet<ID>, // the Unrestricted vognodes in the buffer (the
                            // kind this view renders meaningfully; see
                            // pids_from_viewforest)
  pub tdv_buffer : Option<TdvBuffer>, // Some for a buffer of the total diff view
}

/// The four buffers of the total diff view
/// ('TODO/total-diff-view/02_plan.org', "One command, four buffers").
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum TdvBuffer {
  AllChanges,
  Merges,
  SkgrepoChanges,
  OrderOnlyChanges,
}

impl TdvBuffer {
  pub const ALL : [TdvBuffer; 4] = [
    TdvBuffer::AllChanges, TdvBuffer::Merges,
    TdvBuffer::SkgrepoChanges, TdvBuffer::OrderOnlyChanges ];

  /// The '(buffer . "...")' of a "total diff view" request; the
  /// client names the buffer "diff-view: " followed by it.
  pub fn wire_name (
    self,
  ) -> &'static str {
    match self {
      TdvBuffer::AllChanges       => "all changes",
      TdvBuffer::Merges           => "merges",
      TdvBuffer::SkgrepoChanges   => "skgrepo changes",
      TdvBuffer::OrderOnlyChanges => "order-only changes", } }

  pub fn from_wire_name (
    name : &str,
  ) -> Option<TdvBuffer> {
    TdvBuffer::ALL . into_iter ()
      . find ( |buffer| buffer . wire_name () == name ) }
}

//
// Implementations
//

impl ViewId {
  /// Serialize to the client string format.
  pub fn repr_in_client ( &self ) -> String {
    match self {
      ViewId::ContentView  (s)  => s . clone (),
      ViewId::SearchView   (q)  => format! ("search:{}", q) } }
  /// Parse a client string to a ViewId.
  pub fn from_client_string ( s : String ) -> ViewId {
    if let Some (query) = s . strip_prefix ("search:") {
      ViewId::SearchView ( query . to_string () )
    } else {
      ViewId::ContentView (s) } }
}

impl OpenViews {
  pub fn new () -> Self {
    OpenViews {
      views       : HashMap::new (),
      root_skgids : ManyToMany::new () }}

  pub fn clear (&mut self) {
    self . views       . clear ();
    self . root_skgids    = ManyToMany::new (); }

  pub fn viewid_to_pids (
    &self,
    view_id : &ViewId,
  ) -> Vec<ID> {
    self . views . get (view_id)
      . map ( |vs| vs . pids . iter () . cloned () . collect () )
      . unwrap_or_default () }

  pub fn viewid_to_view (
    &self,
    view_id : &ViewId,
  ) -> Option<&ViewForest> {
    self . views . get (view_id)
      . map ( |vs| &vs . viewforest ) }

  /// Returns the first (if any exists) CONTENT buffer (not a search
  /// view) for which the ID is a root
  /// (level-1 headline).
  pub fn content_view_id_for_root_skgid (
    &self,
    skgid : &ID,
  ) -> Option<&ViewId> {
    self . root_skgids . get_right (skgid)
      . and_then ( |view_ids| view_ids . iter ()
                   . find ( |u| matches! (
                       u, ViewId::ContentView (_) ))) }

  pub fn register_view (
    &mut self,
    graph  : &InRustGraph,
    view_id : ViewId,
    viewforest : impl Into<ViewForest>,
    pids   : &[ID],
  ) { let viewforest : ViewForest =
        viewforest . into ();
      let rids : HashSet<ID> =
        root_skgids_from_viewforest ( graph, &viewforest );
      for rid in &rids {
        self . root_skgids . insert (
          rid . clone (), view_id . clone () ); }
      let pids : HashSet<ID> =
        pids . iter () . cloned () . collect ();
      let state : ViewState =
        ViewState { viewforest, pids, tdv_buffer : None };
      self . views . insert ( view_id, state ); }

  /// 'register_view' for a buffer of the total diff view.
  pub fn register_tdv_view (
    &mut self,
    graph      : &InRustGraph,
    view_id    : ViewId,
    viewforest : impl Into<ViewForest>,
    pids       : &[ID],
    tdv_buffer : TdvBuffer,
  ) { self . register_view ( graph, view_id . clone (), viewforest, pids );
      if let Some (state) = self . views . get_mut (&view_id) {
        state . tdv_buffer = Some (tdv_buffer); }}

  /// Whether any open view is a buffer of the total diff view.
  pub fn has_tdv_views (
    &self,
  ) -> bool {
    self . views . values () . any ( |state| state . tdv_buffer . is_some () ) }

  pub fn update_view (
    &mut self,
    graph      : &InRustGraph,
    view_id    : &ViewId,
    new_viewforest : impl Into<ViewForest>,
  ) { let new_viewforest : ViewForest =
        new_viewforest . into ();
      let pids : HashSet<ID> =
        pids_from_viewforest ( &new_viewforest );
      self . root_skgids . remove_right (view_id);
      let rids : HashSet<ID> =
        root_skgids_from_viewforest ( graph, &new_viewforest );
      for rid in &rids {
        self . root_skgids . insert (
          rid . clone (), view_id . clone () ); }
      if let Some (vs)
        = self . views . get_mut (view_id)
        { vs . viewforest = new_viewforest;
          vs . pids = pids; }
      else { self . views . insert (
               view_id . clone (),
               ViewState { viewforest : new_viewforest,
                           pids, tdv_buffer : None } ); }}

  pub fn unregister_view (
    &mut self,
    view_id : &ViewId,
  ) { self . root_skgids . remove_right (view_id);
      self . views . remove (view_id); }

  pub fn views_containing (
    &self,
    pid : &ID,
  ) -> Vec<ViewId> {
    self . views . iter ()
      . filter ( |(_, vs)| vs . pids . contains (pid) )
      . map ( |(view_id, _)| view_id . clone () )
      . collect () }
}

//
// Functions
//

/// The pids a view "contains" for collateral detection (views_containing): the
/// primary ids of its Unrestricted vognodes -- the only kind backed by a
/// real, current graphnode that this view renders meaningfully. Restricted
/// placeholders are excluded: they are anonymous placeholders whose rerender shows
/// nothing about the node, so a save touching that node need not re-render this
/// view (its unrestricted descendants register themselves). Deleted / Unknown / Diff
/// phantom are excluded too: they are not graph members. The single source of
/// which kinds count: update_view derives its pids through it, and the de-novo
/// caller (multi_root_view_via_env) computes the pids it passes to register_view
/// through it too.
pub fn pids_from_viewforest (
  viewforest : &ViewForest,
) -> HashSet<ID> {
  viewforest . nodes ()
    . filter_map ( |n| match &n . value () . kind {
      ViewnodeKind::Vognode (Vognode::Unrestricted (t)) =>
        Some ( t . skgid . clone () ),
      _ => None } )
    . collect () }

/// Collect all IDs (primary + extras) for every root
/// -- i.e. every level-1 headline -- in the view.
/// (There can be graph roots at other levels, via non-member (affectsParent false) children;
/// this does not return those.)
///
/// Extra IDs are pulled from the operation's captured in-Rust graph.
fn root_skgids_from_viewforest (
  graph      : &InRustGraph,
  viewforest : &ViewForest,
) -> HashSet<ID> {
  let mut skgids : HashSet<ID> = HashSet::new ();
  for child in viewforest . roots () {
    if let Some (vid) = child . value () . unrestricted_skgid () {
      skgids . insert ( vid . clone () );
      if let Some (pid) = graph . pid_of ( vid ) {
          if let Some (node) = graph . nodes . get (&pid) {
            skgids . insert ( pid . clone () );
            for extra_id in &node . extra_ids {
              skgids . insert ( extra_id . clone () ); }}}}}
  skgids }
