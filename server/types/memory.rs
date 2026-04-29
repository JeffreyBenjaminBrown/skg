use crate::dbs::filesystem::one_node::{nodecomplete_from_id, nodecomplete_from_pid_and_source};
use crate::dbs::memory::snapshot_global;
use crate::types::many_to_many::ManyToMany;
use crate::types::nodes::complete::NodeComplete;
use crate::types::viewnode::{ViewNode, ViewNodeKind};
use super::misc::{ID, SkgConfig, SourceName};
use super::phantom::source_from_disk;

use ego_tree::Tree;
use std::collections::{HashMap, HashSet};
use std::error::Error;
use typedb_driver::TypeDBDriver;

//
// Type declarations
//

/// Identifies a buffer in the client.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum ViewUri {
  ContentView (String), // UUID
  SearchView  (String), // query
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
  pub viewforest : Tree<ViewNode>,
  pub pids   : HashSet<ID>, // all the TrueNodes (and DeletedNodes) in the buffer
}

//
// Implementations
//

impl ViewUri {
  /// Serialize to the client string format.
  pub fn repr_in_client ( &self ) -> String {
    match self {
      ViewUri::ContentView (s) => s . clone (),
      ViewUri::SearchView  (q) => format! ("search:{}", q) } }
  /// Parse a client string to a ViewUri.
  pub fn from_client_string ( s : String ) -> ViewUri {
    if let Some (query) = s . strip_prefix ("search:") {
      ViewUri::SearchView ( query . to_string () )
    } else {
      ViewUri::ContentView (s) } }
  pub fn is_search ( &self ) -> bool {
    matches! ( self, ViewUri::SearchView (_) ) } }

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
  ) -> Option<&Tree<ViewNode>> {
    self . views . get (uri)
      . map ( |vs| &vs . viewforest ) }

  /// Returns the first (if any exists) non-search buffer
  /// for which the ID is a root (level-1 headline).
  pub fn content_view_uri_for_root_id (
    &self,
    id : &ID,
  ) -> Option<&ViewUri> {
    self . root_ids . get_right (id)
      . and_then ( |uris| uris . iter ()
                   . find ( |u| ! u . is_search () )) }

  pub fn register_view (
    &mut self,
    uri    : ViewUri,
    viewforest : Tree<ViewNode>,
    pids   : &[ID],
  ) { let rids : HashSet<ID> =
        root_ids_from_viewforest ( &viewforest );
      for rid in &rids {
        self . root_ids . insert (
          rid . clone (), uri . clone () ); }
      let state : ViewState =
        ViewState { viewforest,
                    pids : pids . iter () . cloned () . collect () };
      self . views . insert ( uri, state ); }

  pub fn update_view (
    &mut self,
    uri        : &ViewUri,
    new_viewforest : Tree<ViewNode>,
  ) { let pids : HashSet<ID> =
        new_viewforest . root () . descendants ()
        . filter_map ( |n| match &n . value () . kind {
          ViewNodeKind::True (t)    => Some ( t . id . clone () ),
          ViewNodeKind::Deleted (d) => Some ( d . id . clone () ),
          _ => None } )
        . collect ();
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
                           pids } ); } }

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

/// Collect all IDs (primary + extras) for every root
/// -- i.e. every level-1 headline -- in the view.
/// (There can be graph roots at other levels, via non-Content birth;
/// this does not return those.)
///
/// Extra_ids are pulled from the in-Rust memory. If memory isn't
/// initialized (tests that bypass 'init_global_handle_for_first_time_or_panic'), only
/// primary ids are collected — extras aren't available.
fn root_ids_from_viewforest (
  viewforest : &Tree<ViewNode>,
) -> HashSet<ID> {
  let mut ids : HashSet<ID> = HashSet::new ();
  let memory = snapshot_global ();
  for child in viewforest . root () . children () {
    if let ViewNodeKind::True (t) = &child . value () . kind {
      ids . insert ( t . id . clone () );
      if let Some (mem) = memory . as_ref () {
        if let Some (pid) = mem . pid_of ( &t . id ) {
          if let Some (node) = mem . nodes . get (&pid) {
            ids . insert ( pid . clone () );
            for extra_id in &node . extra_ids {
              ids . insert ( extra_id . clone () ); }}}}}}
  ids }

/// Memory-first NodeComplete read, disk fallback, id-based.
///
/// Async because the id→(pid, source) resolution goes through
/// 'nodecomplete_from_id', which consults TypeDB when memory is
/// uninitialized. Callers that already have '(pid, source)' in
/// hand should prefer the sync 'nodecomplete_from_memory_or_disk'
/// below.
pub async fn nodecomplete_from_memory_or_disk_async (
  config : &SkgConfig,
  driver : &TypeDBDriver,
  id     : &ID,
) -> Result<NodeComplete, Box<dyn Error>> {
  if let Some (n) = nodecomplete_from_memory (id) {
    return Ok (n); }
  nodecomplete_from_id (config, driver, id) . await }

/// Memory-first NodeComplete read, disk fallback, given an
/// already-resolved '(pid, source)'. Sync — never consults TypeDB,
/// so callers in sync contexts don't have to become async. For the
/// id-only path use 'nodecomplete_from_memory_or_disk_async'.
pub fn nodecomplete_from_memory_or_disk (
  config : &SkgConfig,
  pid    : &ID,
  source : &SourceName,
) -> Result<NodeComplete, Box<dyn Error>> {
  if let Some (n) = nodecomplete_from_memory (pid)
    { return Ok (n); }
  Ok ( nodecomplete_from_pid_and_source (
         config, pid . clone (), source ) ? ) }

/// Synthesize a NodeComplete from the in-Rust memory if the id is
/// there (primary or extra). Returns None if memory isn't
/// initialized or doesn't have the id.
pub fn nodecomplete_from_memory (id: &ID) -> Option<NodeComplete> {
  let graph_snap = snapshot_global () ?;
  let pid : ID = graph_snap . pid_of (id) ?;
  let rust = graph_snap . nodes . get (&pid) ?;
  Some ( NodeComplete {
    pid                          : rust . pid . clone (),
    source                       : rust . source . clone (),
    extra_ids                    : rust . extra_ids . clone (),
    title                        : rust . title . clone (),
    aliases                      : rust . aliases . clone (),
    body                         : rust . body . clone (),
    contains                     : rust . contains . clone (),
    subscribes_to                : rust . subscribes_to . clone (),
    hides_from_its_subscriptions : rust . hides_from_its_subscriptions . clone (),
    overrides_view_of            : rust . overrides_view_of . clone (),
    misc                         : rust . misc . clone (), } ) }

/// Lookup a node's source by trying, in order:
/// - sources_from_siblings
/// - deleted_since_head_pid_src_map
/// - in-Rust memory (handles nodes created in this save that have
///   no disk file yet)
/// - disk (scan source directories for a matching .skg filename)
pub fn find_source_many_ways (
  id                             : &ID,
  sources_from_siblings          : &HashMap<ID, SourceName>, // When 'find_source_many_ways' is called from a parent, the parent's child viewnodes can be scanned for IDs and put here. If that's not done and this is empty, the source will still probably be found.
  deleted_since_head_pid_src_map : &HashMap<ID, SourceName>, // built by deleted_ids_to_source
  config                         : &SkgConfig,
) -> Result<SourceName, String> {
  if let Some (s) = sources_from_siblings . get (id)
    { return Ok ( s . clone () ); }
  if let Some (s) = deleted_since_head_pid_src_map . get (id)
    { return Ok ( s . clone () ); }
  if let Some (( _pid, src )) =
    snapshot_global ()
      . as_deref () . and_then ( |g| g . pid_and_source (id) )
    { return Ok (src); }
  let source : SourceName =
    source_from_disk ( id, config )
    . ok_or_else ( || format! (
      "find_source_many_ways: no source found for {}", id . 0 )) ?;
  Ok (source) }
