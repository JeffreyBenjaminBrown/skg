use crate::dbs::in_rust_graph::InRustGraph;
use crate::types::many_to_many::ManyToMany;
use crate::types::tree::forest::ViewForest;
use crate::types::viewnode::{Phantom, ViewNodeKind, Vognode};
use crate::maintenance::BufferKind;
use crate::types::env::{GraphReadSnapshot, SkgEnv};
use crate::types::store_state::SelectedGraphBase;
use super::misc::{ID, SkgConfig};

use sexp::{Atom, Sexp};
use std::collections::{HashMap, HashSet};
use uuid::Uuid;

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
#[derive(Clone)]
pub struct ViewState {
  pub incarnation : Uuid,
  /// Exact semantic and path base of the accepted text. Updating a display
  /// generation alone must not replace this older proof input.
  pub save_base : Option<ViewSaveBase>,
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
  /// Read-only query results never acquire authority by becoming locally writable.
  pub writes_admitted        : bool,
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

#[derive(Clone)]
pub struct ViewSaveBase {
  pub selected : SelectedGraphBase,
  pub config : SkgConfig,
  pub source_set : String,
}

impl ViewSaveBase {
  pub fn from_snapshot
  ( snapshot : &GraphReadSnapshot,
    source_set : &str,
  ) -> Self {
    Self { selected: snapshot . selected . clone (),
      config: snapshot . config . clone (), source_set: source_set . into (), } }

  pub fn from_env
  ( env : &SkgEnv,
    source_set : &str,
  ) -> Self {
    Self { selected: env . in_rust_graph . load_full () . graph_base (),
      config: env . config . clone (), source_set: source_set . into (), } }
}

impl ViewState {
  pub fn retain_save_base (
    &mut self,
    base : ViewSaveBase,
  ) -> Result<(), String> {
    if self . graph_generation != base . selected . graph_generation . get ()
    || self . source_set != base . source_set {
      return Err ("accepted view base differs from its graph/source authority" . into ()); }
    self . save_base = Some (base);
    Ok (( )) }

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

  pub(crate) fn snapshot_view (
    &self,
    uri : &ViewUri,
  ) -> OpenViews {
    let mut snapshot : OpenViews = OpenViews::new ();
    let Some (state) : Option<&ViewState> = self . views . get (uri)
      else { return snapshot; };
    snapshot . views . insert (uri . clone (), state . clone ());
    for root_id in &state . root_ids {
      snapshot . root_ids . insert (root_id . clone (), uri . clone ()); }
    snapshot }

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
    graph : &InRustGraph,
    uri    : ViewUri,
    viewforest : impl Into<ViewForest>,
    pids   : &[ID],
  ) { let viewforest : ViewForest =
        viewforest . into ();
      let rids : HashSet<ID> =
        root_ids_from_viewforest (graph, &viewforest );
      for rid in &rids {
        self . root_ids . insert (
          rid . clone (), uri . clone () ); }
      let pids : HashSet<ID> =
        pids . iter () . cloned () . collect ();
      let revision = self . views . get (&uri)
        . map (|state| state . revision . saturating_add (1))
        . unwrap_or (0);
      let incarnation : Uuid = self . views . get (&uri)
        . map (|state| state . incarnation)
        . unwrap_or_else (Uuid::new_v4);
      let (graph_generation, presentation_generation,
           client_application_token, client_buffer_id, kind, recipe,
           source_set, presentation_stale, search_stale, writes_admitted) =
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
          state . writes_admitted,
        )) . unwrap_or_else (|| (
          1, 0, 1, None, default_kind_for_uri (&uri), None,
          "all" . into (), false, false, true));
      let state : ViewState = ViewState {
        incarnation,
        save_base: self . views . get (&uri) . and_then (|state| state . save_base . clone ()),
        viewforest, pids, revision, root_ids: rids,
        graph_generation,
        presentation_generation,
        client_application_token,
        client_buffer_id,
        writes_admitted,
        kind,
        recipe,
        source_set,
        presentation_stale,
        search_stale,
      };
      self . views . insert ( uri, state ); }

  pub fn register_view_with_authority (
    &mut self,
    graph : &InRustGraph,
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
    self . register_view (graph, uri . clone (), viewforest, pids);
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
    graph : &InRustGraph,
    uri        : &ViewUri,
    new_viewforest : impl Into<ViewForest>,
  ) { let new_viewforest : ViewForest =
        new_viewforest . into ();
      let pids : HashSet<ID> =
        pids_from_viewforest ( &new_viewforest );
      self . root_ids . remove_right (uri);
      let rids : HashSet<ID> =
        root_ids_from_viewforest (graph, &new_viewforest );
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
                           incarnation: Uuid::new_v4 (),
                           save_base: None,
                           pids,
                           root_ids: rids,
                           revision: 0,
                           graph_generation: 1,
                           presentation_generation: 0,
                           client_application_token: 1,
                           client_buffer_id: None,
                           writes_admitted: true,
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

  pub fn set_client_application_authority_and_source_set (
    &mut self,
    uri                     : &ViewUri,
    graph_generation        : u64,
    presentation_generation : u64,
    client_application_token : u64,
    source_set              : String,
  ) -> Result<(), String> {
    self . set_client_application_authority (
      uri, graph_generation, presentation_generation,
      client_application_token)?;
    self . views . get_mut (uri)
      . expect ("application authority view remains registered")
      . source_set = source_set;
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
    graph : &InRustGraph,
    uri           : &ViewUri,
    base_revision : u64,
    viewforest    : impl Into<ViewForest>,
  ) -> bool {
    if self . view_revision (uri) != Some (base_revision) { return false; }
    self . update_view (graph, uri, viewforest);
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
/// Extra IDs come from the same graph that supplied the rendered forest.
pub(crate) fn root_ids_from_viewforest (
  graph : &InRustGraph,
  viewforest : &ViewForest,
) -> HashSet<ID> {
  let mut ids : HashSet<ID> = HashSet::new ();
  for child in viewforest . roots () {
    if let Some (vid) = child . value () . active_or_diff_phantom_id () {
      ids . insert ( vid . clone () );
      {
        if let Some (pid) = graph . pid_of ( vid ) {
          if let Some (node) = graph . nodes . get (&pid) {
            ids . insert ( pid . clone () );
            for extra_id in &node . extra_ids {
              ids . insert ( extra_id . clone () ); }}}}}}
  ids }

#[cfg(test)]
mod tests {
  use super::{OpenViews, ViewSaveBase, ViewState, ViewUri};
  use crate::dbs::in_rust_graph::InRustGraph;
  use crate::maintenance::BufferKind;
  use crate::types::misc::SkgConfig;
  use crate::types::store_state::{
    GraphGeneration, ManifestRevision, SelectedGraphBase};
  use crate::types::tree::forest::ViewForest;
  use crate::types::misc::ID;
  use std::collections::{BTreeMap, HashMap, HashSet};
  use std::sync::Arc;
  use uuid::Uuid;

  fn save_base () -> ViewSaveBase {
    let selected : SelectedGraphBase = SelectedGraphBase {
      graph             : Arc::new (crate::dbs::in_rust_graph::InRustGraph::new ()),
      graph_generation  : GraphGeneration::INITIAL,
      manifest_revision : ManifestRevision::INITIAL,
      manifest          : Arc::new (BTreeMap::new ()), };
    ViewSaveBase {
      selected,
      config     : SkgConfig::dummyFromSources (HashMap::new ()),
      source_set : "all" . into (), }
  }

  fn view_state (root_id : ID, revision : u64) -> ViewState {
    let mut root_ids : HashSet<ID> = HashSet::new ();
    root_ids . insert (root_id);
    ViewState {
      incarnation                : Uuid::new_v4 (),
      save_base                 : Some (save_base ()),
      viewforest                : ViewForest::new (),
      pids                      : HashSet::new (),
      revision,
      graph_generation          : 9,
      presentation_generation   : 13,
      client_application_token : 17,
      client_buffer_id          : Some ("buffer" . into ()),
      writes_admitted           : true,
      kind                      : BufferKind::ContentView,
      recipe                    : Some ("recipe" . into ()),
      root_ids,
      source_set                : "all" . into (),
      presentation_stale        : false,
      search_stale              : false, }
  }

  #[test]
  fn snapshot_view_copies_one_independent_view_and_root_reverse_lookup () {
    let uri : ViewUri = ViewUri::ContentView ("chosen" . into ());
    let other_uri : ViewUri = ViewUri::ContentView ("other" . into ());
    let root_id : ID = ID::from ("root");
    let other_root_id : ID = ID::from ("other-root");
    let mut open_views : OpenViews = OpenViews::new ();
    open_views . views . insert (
      uri . clone (), view_state (root_id . clone (), 23));
    open_views . root_ids . insert (root_id . clone (), uri . clone ());
    open_views . views . insert (
      other_uri . clone (), view_state (other_root_id . clone (), 31));
    open_views . root_ids . insert (
      other_root_id . clone (), other_uri . clone ());

    let mut snapshot : OpenViews = open_views . snapshot_view (&uri);
    assert_eq! (snapshot . views . len (), 1);
    let copied : &ViewState = snapshot . views . get (&uri) . unwrap ();
    let original : &ViewState = open_views . views . get (&uri) . unwrap ();
    assert_eq! (copied . revision, original . revision);
    assert_eq! (
      copied . client_application_token,
      original . client_application_token);
    assert_eq! (copied . save_base . as_ref () . unwrap () . source_set, "all");
    assert_eq! (copied . viewforest, original . viewforest);
    assert_eq! (
      snapshot . content_view_uri_for_root_id (&root_id), Some (&uri));
    assert! (snapshot . content_view_uri_for_root_id (&other_root_id) . is_none ());
    let missing_uri : ViewUri = ViewUri::ContentView ("missing" . into ());
    let missing : OpenViews = open_views . snapshot_view (&missing_uri);
    assert! (missing . views . is_empty ());

    snapshot . unregister_view (&uri);
    assert! (snapshot . views . is_empty ());
    assert! (open_views . views . contains_key (&uri));
    assert_eq! (
      open_views . content_view_uri_for_root_id (&root_id), Some (&uri));
  }

  #[test]
  fn view_incarnation_survives_snapshot_and_changes_after_reopen () {
    let graph : InRustGraph = InRustGraph::new ();
    let uri : ViewUri = ViewUri::ContentView ("incarnation" . into ());
    let mut open_views : OpenViews = OpenViews::new ();
    open_views . register_view (
      &graph, uri . clone (), ViewForest::new (), &[]);
    let first : Uuid = open_views . views . get (&uri) . unwrap () . incarnation;
    let snapshot : OpenViews = open_views . snapshot_view (&uri);
    assert_eq! (
      snapshot . views . get (&uri) . unwrap () . incarnation, first);
    open_views . unregister_view (&uri);
    open_views . register_view (&graph, uri . clone (), ViewForest::new (), &[]);
    let reopened : &ViewState = open_views . views . get (&uri) . unwrap ();
    assert_ne! (reopened . incarnation, first);
    assert_eq! (reopened . revision, 0);
    assert_eq! (reopened . graph_generation, 1);
  }
}
