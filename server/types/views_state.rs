use crate::dbs::in_rust_graph::snapshot_global;
use crate::types::many_to_many::ManyToMany;
use crate::types::tree::forest::ViewForest;
use crate::types::viewnode::ViewNodeKind;
use crate::types::viewnode::Vognode;
use super::misc::ID;

use std::collections::{HashMap, HashSet};

//
// Type declarations
//

/// Identifies a buffer in the client.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum ViewUri {
  ContentView  (String), // UUID
  SearchView   (String), // query
  OverrideMenu (String), // the requested (overridden) node's PID. One menu per node: requesting it again switches to the open menu. Deliberately not a ContentView, so an open menu never hijacks 'content_view_uri_for_root_id' -- a raw view of the node and its menu can coexist.
}

/// Per-connection view bookkeeping. Each entry in 'views' is a
/// currently-open buffer (content or search) with its viewforest and PID
/// set. 'root_ids' is the reverse lookup — which view(s) is this
/// ID a root of.
pub struct OpenViews {
  pub views       : HashMap<ViewUri, ViewState>,
  // Includes both (graph) content views and search result views. See the definition of 'ViewUri'.
  // TODO ? OPTIMIZE:
  // The reverse lookup (PID -> views) is computed by scanning `views` via views_containing(). This is O(views) per call, fine for < 10 views. If the number of views grows large, consider a bijective map (HashMap<ID, HashSet<ViewUri>> maintained alongside this one) for O(1) reverse lookups.

  root_ids        : ManyToMany<ID, ViewUri>, // Maps every root ID (primary + extra) ↔ ViewUri. Supports many-to-many because a view can have multiple roots, and an ID could be a root in multiple views. Maintained by register_view / update_view / unregister_view.
}

/// Invariant: all viewforest mutations must go through register_view /
/// update_view, which maintain pids in sync with the viewforest.
/// Direct viewforest mutation would make pids stale.
pub struct ViewState {
  pub viewforest : ViewForest,
  pub pids   : HashSet<ID>, // the Active (Normal) vognodes in the buffer (the
                            // kind this view renders meaningfully; see
                            // pids_from_viewforest)
}

//
// Implementations
//

impl ViewUri {
  /// Serialize to the client string format.
  pub fn repr_in_client ( &self ) -> String {
    match self {
      ViewUri::ContentView  (s) => s . clone (),
      ViewUri::SearchView   (q) => format! ("search:{}", q),
      ViewUri::OverrideMenu (p) => format! ("override-menu:{}", p) } }
  /// Parse a client string to a ViewUri.
  pub fn from_client_string ( s : String ) -> ViewUri {
    if let Some (query) = s . strip_prefix ("search:") {
      ViewUri::SearchView ( query . to_string () )
    } else if let Some (pid) = s . strip_prefix ("override-menu:") {
      ViewUri::OverrideMenu ( pid . to_string () )
    } else {
      ViewUri::ContentView (s) } }
}

impl OpenViews {
  pub fn new () -> Self {
    OpenViews {
      views       : HashMap::new (),
      root_ids    : ManyToMany::new () }}

  pub fn clear (&mut self) {
    self . views       . clear ();
    self . root_ids    = ManyToMany::new (); }

  pub fn viewuri_to_pids (
    &self,
    uri : &ViewUri,
  ) -> Vec<ID> {
    self . views . get (uri)
      . map ( |vs| vs . pids . iter () . cloned () . collect () )
      . unwrap_or_default () }

  pub fn viewuri_to_view (
    &self,
    uri : &ViewUri,
  ) -> Option<&ViewForest> {
    self . views . get (uri)
      . map ( |vs| &vs . viewforest ) }

  /// Returns the first (if any exists) CONTENT buffer (not a search
  /// view, not an override menu) for which the ID is a root
  /// (level-1 headline).
  pub fn content_view_uri_for_root_id (
    &self,
    id : &ID,
  ) -> Option<&ViewUri> {
    self . root_ids . get_right (id)
      . and_then ( |uris| uris . iter ()
                   . find ( |u| matches! (
                       u, ViewUri::ContentView (_) ))) }

  pub fn register_view (
    &mut self,
    uri    : ViewUri,
    viewforest : impl Into<ViewForest>,
    pids   : &[ID],
  ) { let viewforest : ViewForest =
        viewforest . into ();
      let rids : HashSet<ID> =
        root_ids_from_viewforest ( &viewforest );
      for rid in &rids {
        self . root_ids . insert (
          rid . clone (), uri . clone () ); }
      let pids : HashSet<ID> =
        pids . iter () . cloned () . collect ();
      let state : ViewState =
        ViewState { viewforest, pids };
      self . views . insert ( uri, state ); }

  pub fn update_view (
    &mut self,
    uri        : &ViewUri,
    new_viewforest : impl Into<ViewForest>,
  ) { let new_viewforest : ViewForest =
        new_viewforest . into ();
      let pids : HashSet<ID> =
        pids_from_viewforest ( &new_viewforest );
      self . root_ids . remove_right (uri);
      let rids : HashSet<ID> =
        root_ids_from_viewforest ( &new_viewforest );
      for rid in &rids {
        self . root_ids . insert (
          rid . clone (), uri . clone () ); }
      if let Some (vs)
        = self . views . get_mut (uri)
        { vs . viewforest = new_viewforest;
          vs . pids = pids; }
      else { self . views . insert (
               uri . clone (),
               ViewState { viewforest : new_viewforest,
                           pids } ); }}

  pub fn unregister_view (
    &mut self,
    uri : &ViewUri,
  ) { self . root_ids . remove_right (uri);
      self . views . remove (uri); }

  pub fn views_containing (
    &self,
    pid : &ID,
  ) -> Vec<ViewUri> {
    self . views . iter ()
      . filter ( |(_, vs)| vs . pids . contains (pid) )
      . map ( |(uri, _)| uri . clone () )
      . collect () }
}

//
// Functions
//

/// The pids a view "contains" for collateral detection (views_containing): the
/// primary ids of its Active (Normal) vognodes -- the only kind backed by a
/// real, current graph node that this view renders meaningfully. Inactive
/// placeholders are excluded: they are anonymous markers whose rerender shows
/// nothing about the node, so a save touching that node need not re-render this
/// view (its active descendants register themselves). Deleted / Unknown / Diff
/// phantom are excluded too: they are not graph members. The single source of
/// which kinds count: update_view derives its pids through it, and the de-novo
/// caller (multi_root_view_via_env) computes the pids it passes to register_view
/// through it too.
pub fn pids_from_viewforest (
  viewforest : &ViewForest,
) -> HashSet<ID> {
  viewforest . nodes ()
    . filter_map ( |n| match &n . value () . kind {
      ViewNodeKind::Vognode (Vognode::Active (t)) =>
        Some ( t . id . clone () ),
      _ => None } )
    . collect () }

/// Collect all IDs (primary + extras) for every root
/// -- i.e. every level-1 headline -- in the view.
/// (There can be graph roots at other levels, via non-Content parentIs;
/// this does not return those.)
///
/// Extra_ids are pulled from the in-Rust graph. If in-Rust graph isn't
/// initialized (tests that bypass 'init_global_handle_for_first_time_or_panic'), only
/// primary ids are collected — extras aren't available.
fn root_ids_from_viewforest (
  viewforest : &ViewForest,
) -> HashSet<ID> {
  let mut ids : HashSet<ID> = HashSet::new ();
  let graph_snap = snapshot_global ();
  for child in viewforest . roots () {
    if let Some (vid) = child . value () . active_or_diff_phantom_id () {
      ids . insert ( vid . clone () );
      if let Some (graph) = graph_snap . as_ref () {
        if let Some (pid) = graph . pid_of ( vid ) {
          if let Some (node) = graph . nodes . get (&pid) {
            ids . insert ( pid . clone () );
            for extra_id in &node . extra_ids {
              ids . insert ( extra_id . clone () ); }}}}}}
  ids }
