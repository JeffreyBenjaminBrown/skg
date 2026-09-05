use crate::dbs::in_rust_graph::snapshot_global;
use crate::types::many_to_many::ManyToMany;
use crate::types::tree::forest::ViewForest;
use crate::types::viewnode::{Phantom, ViewNodeKind, Vognode};
use crate::maintenance::BufferKind;
use super::misc::ID;

use sexp::{Atom, Sexp};
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
  /// Monotonic server-side base revision. Background render offers name the
  /// revision they cloned and cannot replace a view which advanced meanwhile.
  pub revision : u64,
  pub graph_generation       : u64,
  pub presentation_generation : u64,
  pub client_application_token : u64,
  pub client_buffer_id       : Option<String>,
  pub kind                   : BufferKind,
  pub recipe                 : Option<String>,
  pub root_ids               : HashSet<ID>,
  pub source_set             : String,
  /// The forest/text was deliberately preserved across a graph transition;
  /// generated herald/presentation details may therefore name G0 facts.
  pub presentation_stale     : bool,
  /// Search membership/ranking is never rerun automatically after
  /// maintenance, independently of whether displayed node text was refreshed.
  pub search_stale           : bool,
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
      let revision = self . views . get (&uri)
        . map (|state| state . revision . saturating_add (1))
        . unwrap_or (0);
      let (graph_generation, presentation_generation,
           client_application_token, client_buffer_id, kind, recipe,
           source_set, presentation_stale, search_stale) =
        self . views . get (&uri) . map (|state| (
          state . graph_generation,
          state . presentation_generation,
          state . client_application_token,
          state . client_buffer_id . clone (),
          state . kind . clone (),
          state . recipe . clone (),
          state . source_set . clone (),
          state . presentation_stale,
          state . search_stale,
        )) . unwrap_or_else (|| (
          1, 0, 1, None, default_kind_for_uri (&uri), None,
          "all" . into (), false, false));
      let state : ViewState = ViewState {
        viewforest, pids, revision, root_ids: rids,
        graph_generation,
        presentation_generation,
        client_application_token,
        client_buffer_id,
        kind,
        recipe,
        source_set,
        presentation_stale,
        search_stale,
      };
      self . views . insert ( uri, state ); }

  pub fn register_view_with_authority (
    &mut self,
    uri                     : ViewUri,
    viewforest              : impl Into<ViewForest>,
    pids                    : &[ID],
    graph_generation        : u64,
    presentation_generation : u64,
    client_application_token : u64,
    kind                    : BufferKind,
    source_set              : String,
    recipe                  : Option<String>,
  ) {
    self . register_view (uri . clone (), viewforest, pids);
    let state = self . views . get_mut (&uri)
      . expect ("newly registered view exists");
    state . graph_generation = graph_generation;
    state . presentation_generation = presentation_generation;
    state . client_application_token = client_application_token;
    state . kind = kind;
    state . source_set = source_set;
    state . recipe = recipe;
  }

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
          vs . pids = pids;
          vs . root_ids = rids;
          vs . revision = vs . revision . saturating_add (1); }
      else { self . views . insert (
               uri . clone (),
               ViewState { viewforest : new_viewforest,
                           pids,
                           root_ids: rids,
                           revision: 0,
                           graph_generation: 1,
                           presentation_generation: 0,
                           client_application_token: 1,
                           client_buffer_id: None,
                           kind: default_kind_for_uri (uri),
                           recipe: None,
                           source_set: "all" . into (),
                           presentation_stale: false,
                           search_stale: false } ); }}

  pub fn view_revision (&self, uri : &ViewUri) -> Option<u64> {
    self . views . get (uri) . map (|state| state . revision)
  }

  pub fn set_client_application_authority (
    &mut self,
    uri                     : &ViewUri,
    graph_generation        : u64,
    presentation_generation : u64,
    client_application_token : u64,
  ) -> Result<(), String> {
    let state = self . views . get_mut (uri)
      . ok_or_else (|| format! (
        "view '{}' is not registered", uri . repr_in_client ()))?;
    state . graph_generation = graph_generation;
    state . presentation_generation = presentation_generation;
    state . client_application_token = client_application_token;
    state . presentation_stale = false;
    Ok (( ))
  }

  /// Advance a preserved forest's graph association without claiming it was
  /// rerendered.  This is the maintenance-only orthogonal-view transition.
  pub fn preserve_across_maintenance (
    &mut self,
    uri              : &ViewUri,
    graph_generation : u64,
    search_stale     : bool,
  ) -> Result<(), String> {
    let state = self . views . get_mut (uri)
      . ok_or_else (|| format! (
        "view '{}' is not registered", uri . repr_in_client ()))?;
    state . graph_generation = graph_generation;
    state . presentation_stale = true;
    state . search_stale |= search_stale;
    Ok (( ))
  }

  /// Apply one client-acknowledged background result only if its cloned base
  /// is still current.
  pub fn update_view_if_revision (
    &mut self,
    uri           : &ViewUri,
    base_revision : u64,
    viewforest    : impl Into<ViewForest>,
  ) -> bool {
    if self . view_revision (uri) != Some (base_revision) { return false; }
    self . update_view (uri, viewforest);
    true
  }

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

fn recipe_entry (key : &str, value : &str) -> Sexp {
  Sexp::List (vec![
    Sexp::Atom (Atom::S (key . into ())),
    Sexp::Atom (Atom::S (value . into ())),
  ])
}

pub fn single_root_recipe (root_id : &ID) -> String {
  Sexp::List (vec![
    recipe_entry ("kind", "single-root"),
    recipe_entry ("root-id", &root_id . 0),
  ]) . to_string ()
}

pub fn search_recipe (
  terms       : &str,
  regex       : bool,
  body        : bool,
  operators   : bool,
  ugly_choice : Option<&str>,
) -> String {
  let truth = |value| if value { "true" } else { "nil" };
  let mut entries = vec![
    recipe_entry ("body", truth (body)),
    recipe_entry ("kind", "search"),
    recipe_entry ("operators", truth (operators)),
    recipe_entry ("regex", truth (regex)),
    recipe_entry ("terms", terms),
  ];
  if let Some (choice) = ugly_choice {
    entries . push (recipe_entry ("ugly-choice", choice)); }
  Sexp::List (entries) . to_string ()
}

fn default_kind_for_uri (uri : &ViewUri) -> BufferKind {
  match uri {
    ViewUri::ContentView (_) => BufferKind::ContentView,
    ViewUri::SearchView (_) => BufferKind::SearchView,
    ViewUri::OverrideMenu (_) => BufferKind::OverrideChoiceMenu,
  }
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

/// Every concrete ID visibly represented by a graph-ish node in a retained
/// forest.  This deliberately differs from `pids_from_viewforest`: collateral
/// rendering cares only about current Active nodes, while maintenance impact
/// must also protect a user's view of nodes represented by Deleted, Unknown,
/// and Git-diff phantoms.
///
/// Inactive vognodes remain excluded because their anonymity is a privacy
/// boundary, not missing bookkeeping.
pub fn impact_ids_from_viewforest (
  viewforest : &ViewForest,
) -> HashSet<ID> {
  viewforest . nodes ()
    . filter_map (|node| match &node . value () . kind {
      ViewNodeKind::Vognode (Vognode::Active (active)) =>
        Some (active . id . clone ()),
      ViewNodeKind::Phantom (Phantom::Diff (phantom)) =>
        Some (phantom . id . clone ()),
      ViewNodeKind::Phantom (Phantom::Deleted (phantom)) =>
        Some (phantom . id . clone ()),
      ViewNodeKind::Phantom (Phantom::Unknown (phantom)) =>
        Some (phantom . id . clone ()),
      _ => None,
    })
    . collect ()
}

/// Collect all IDs (primary + extras) for every root
/// -- i.e. every level-1 headline -- in the view.
/// (There can be graph roots at other levels, via non-Content parentIs;
/// this does not return those.)
///
/// Extra_ids are pulled from the in-Rust graph. If in-Rust graph isn't
/// initialized (tests that bypass 'init_global_handle_for_first_time_or_panic'), only
/// primary ids are collected — extras aren't available.
pub(crate) fn root_ids_from_viewforest (
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
