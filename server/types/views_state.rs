use crate::dbs::in_rust_graph::InRustGraph;
use crate::types::many_to_many::ManyToMany;
use crate::types::tree::forest::ViewForest;
use crate::types::viewnode::{ViewnodeId, Viewnode, ViewnodeKind, Vognode};
use super::misc::ID;

use ego_tree::{NodeId, NodeMut, Tree};
use std::collections::{HashMap, HashSet};

//
// Type declarations
//

/// Identifies a buffer in the client.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum ViewforestId {
  ContentView  (String), // the root skgid
  SearchView   (String), // query
}

/// Per-connection view bookkeeping. Each entry in 'views' is a
/// currently-open buffer (content or search) with its viewforest and PID
/// set. 'root_ids' is the reverse lookup — which view(s) is this
/// ID a root of.
pub struct OpenViews {
  pub views       : HashMap<ViewforestId, ViewState>,
  // Includes both (graph) content views and search result views. See the definition of 'ViewforestId'.
  // TODO ? OPTIMIZE:
  // The reverse lookup (PID -> views) is computed by scanning `views` via views_containing(). This is O(views) per call, fine for < 10 views. If the number of views grows large, consider a bijective map (HashMap<ID, HashSet<ViewforestId>> maintained alongside this one) for O(1) reverse lookups.

  root_skgids        : ManyToMany<ID, ViewforestId>, // Maps every root ID (primary + extra) ↔ ViewforestId. Supports many-to-many because a view can have multiple roots, and an ID could be a root in multiple views. Maintained by register_view / update_view / unregister_view.
}

/// Invariant: all viewforest mutations must go through register_view /
/// update_view, which maintain pids in sync with the viewforest.
/// Direct viewforest mutation would make pids stale.
pub struct ViewState {
  pub viewforest : ViewForest,
  pub pids       : HashSet<ID>, // the Unrestricted vognodes in the buffer (the
                            // kind this view renders meaningfully; see
                            // pids_from_viewforest)
  pub diffView_buffer_kind : Option<DiffViewBufferKind>, // Some for a buffer of the diff view
  pub viewnode_id_record : ViewnodeIdRecord,
}

/// The viewnode IDs of one buffer: the counter they are assigned from,
/// and the ones the forest holds.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct ViewnodeIdRecord {
  /// Never decreases, so a viewnode ID once used is not used again.
  pub next_viewnode_id_counter : u64,
  /// The viewnode IDs of the forest as last rendered (or registered).
  /// A parsed headline carrying any other is given a fresh one
  /// ('drop_unknown_viewnode_ids_in_parsed_viewforest').
  pub known_viewnode_ids       : HashSet<ViewnodeId>,
}

/// The four buffers of the diff view
/// ('docs/diff-view.org', "The four buffers").
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum DiffViewBufferKind {
  AllChanges,
  Merges,
  SkgrepoChanges,
  OrderOnlyChanges,
}

impl DiffViewBufferKind {
  pub const ALL : [DiffViewBufferKind; 4] = [
    DiffViewBufferKind::AllChanges, DiffViewBufferKind::Merges,
    DiffViewBufferKind::SkgrepoChanges, DiffViewBufferKind::OrderOnlyChanges ];

  /// The '(buffer . "...")' of a "diff view" request; the
  /// client names the buffer "diff-view: " followed by it.
  pub fn wire_name (
    self,
  ) -> &'static str {
    match self {
      DiffViewBufferKind::AllChanges       => "all changes",
      DiffViewBufferKind::Merges           => "merges",
      DiffViewBufferKind::SkgrepoChanges   => "skgrepo changes",
      DiffViewBufferKind::OrderOnlyChanges => "order-only changes", } }

  pub fn from_wire_name (
    name : &str,
  ) -> Option<DiffViewBufferKind> {
    DiffViewBufferKind::ALL . into_iter ()
      . find ( |buffer| buffer . wire_name () == name ) }
}

//
// Implementations
//

impl ViewforestId {
  /// Serialize to the client string format.
  pub fn repr_in_client ( &self ) -> String {
    match self {
      ViewforestId::ContentView  (s)  => s . clone (),
      ViewforestId::SearchView   (q)  => format! ("search:{}", q) } }
  /// Parse a client string to a ViewforestId.
  pub fn from_client_string ( s : String ) -> ViewforestId {
    if let Some (query) = s . strip_prefix ("search:") {
      ViewforestId::SearchView ( query . to_string () )
    } else {
      ViewforestId::ContentView (s) } }
}

impl OpenViews {
  pub fn new () -> Self {
    OpenViews {
      views       : HashMap::new (),
      root_skgids : ManyToMany::new () }}

  pub fn clear (&mut self) {
    self . views       . clear ();
    self . root_skgids    = ManyToMany::new (); }

  pub fn viewforest_id_to_pids (
    &self,
    viewforest_id : &ViewforestId,
  ) -> Vec<ID> {
    self . views . get (viewforest_id)
      . map ( |vs| vs . pids . iter () . cloned () . collect () )
      . unwrap_or_default () }

  pub fn viewforest_id_to_view (
    &self,
    viewforest_id : &ViewforestId,
  ) -> Option<&ViewForest> {
    self . views . get (viewforest_id)
      . map ( |vs| &vs . viewforest ) }

  /// Returns the first (if any exists) content buffer (not a search
  /// view) for which the ID is a root
  /// (level-1 headline).
  pub fn content_viewforest_id_for_root_skgid (
    &self,
    skgid : &ID,
  ) -> Option<&ViewforestId> {
    self . root_skgids . get_right (skgid)
      . and_then ( |viewforest_ids| viewforest_ids . iter ()
                   . find ( |u| matches! (
                       u, ViewforestId::ContentView (_) ))) }

  pub fn register_view (
    &mut self,
    graph  : &InRustGraph,
    viewforest_id : ViewforestId,
    viewforest : impl Into<ViewForest>,
    pids   : &[ID],
  ) { let viewforest : ViewForest =
        viewforest . into ();
      let rids : HashSet<ID> =
        root_skgids_from_viewforest ( graph, &viewforest );
      for rid in &rids {
        self . root_skgids . insert (
          rid . clone (), viewforest_id . clone () ); }
      let pids : HashSet<ID> =
        pids . iter () . cloned () . collect ();
      let viewnode_id_record : ViewnodeIdRecord =
        ViewnodeIdRecord::of_viewforest (&viewforest);
      let state : ViewState =
        ViewState { viewforest, pids, diffView_buffer_kind : None,
                    viewnode_id_record };
      self . views . insert ( viewforest_id, state ); }

  /// 'register_view' for a buffer of the diff view.
  pub fn register_diffView_view (
    &mut self,
    graph                : &InRustGraph,
    viewforest_id        : ViewforestId,
    viewforest           : impl Into<ViewForest>,
    pids                 : &[ID],
    diffView_buffer_kind : DiffViewBufferKind,
  ) { self . register_view ( graph, viewforest_id . clone (), viewforest, pids );
      if let Some (state) = self . views . get_mut (&viewforest_id) {
        state . diffView_buffer_kind = Some (diffView_buffer_kind); }}

  /// Whether any open view is a buffer of the diff view.
  pub fn has_diffView_views (
    &self,
  ) -> bool {
    self . views . values () . any ( |state| state . diffView_buffer_kind . is_some () ) }

  /// The buffer of the diff view that 'viewforest_id' is, if it is one.
  pub fn diffView_buffer_kind_of_view (
    &self,
    viewforest_id : &ViewforestId,
  ) -> Option<DiffViewBufferKind> {
    self . views . get (viewforest_id)
      . and_then ( |state| state . diffView_buffer_kind ) }

  /// A copy of the viewnode IDs of 'viewforest_id', or an empty record when
  /// no such view is open.
  pub fn viewnode_id_record_of (
    &self,
    viewforest_id : &ViewforestId,
  ) -> ViewnodeIdRecord {
    self . views . get (viewforest_id)
      . map ( |state| state . viewnode_id_record . clone () )
      . unwrap_or_default () }

  /// 'viewnode_id_record' is the record the rerender of
  /// 'new_viewforest' assigned its viewnode IDs from.
  pub fn update_view (
    &mut self,
    graph      : &InRustGraph,
    viewforest_id : &ViewforestId,
    new_viewforest : impl Into<ViewForest>,
    viewnode_id_record : ViewnodeIdRecord,
  ) { let new_viewforest : ViewForest =
        new_viewforest . into ();
      let pids : HashSet<ID> =
        pids_from_viewforest ( &new_viewforest );
      self . root_skgids . remove_right (viewforest_id);
      let rids : HashSet<ID> =
        root_skgids_from_viewforest ( graph, &new_viewforest );
      for rid in &rids {
        self . root_skgids . insert (
          rid . clone (), viewforest_id . clone () ); }
      if let Some (vs)
        = self . views . get_mut (viewforest_id)
        { vs . viewforest = new_viewforest;
          vs . pids = pids;
          vs . viewnode_id_record = viewnode_id_record; }
      else { self . views . insert (
               viewforest_id . clone (),
               ViewState { viewforest : new_viewforest,
                           pids, diffView_buffer_kind : None,
                           viewnode_id_record } ); }}

  pub fn unregister_view (
    &mut self,
    viewforest_id : &ViewforestId,
  ) { self . root_skgids . remove_right (viewforest_id);
      self . views . remove (viewforest_id); }

  pub fn views_containing (
    &self,
    pid : &ID,
  ) -> Vec<ViewforestId> {
    self . views . iter ()
      . filter ( |(_, vs)| vs . pids . contains (pid) )
      . map ( |(viewforest_id, _)| viewforest_id . clone () )
      . collect () }
}

impl ViewnodeIdRecord {
  /// The record a view registered with 'viewforest' starts with: the
  /// counter past every viewnode ID it holds, and those viewnode IDs.
  pub fn of_viewforest (
    viewforest : &Tree<Viewnode>,
  ) -> ViewnodeIdRecord {
    let mut record : ViewnodeIdRecord = ViewnodeIdRecord::default ();
    record . advance_counter_past_viewnode_ids_in (viewforest);
    record . record_known_viewnode_ids_in (viewforest);
    record }

  /// After a parse, before any reconciler runs: drop every viewnode ID
  /// the record does not know -- one typed by hand, or brought back by
  /// an old copy of the buffer's text -- so that its headline gets a
  /// fresh one at the end of the completion, and the counter is not
  /// moved by it.
  pub fn drop_unknown_viewnode_ids_in_parsed_viewforest (
    &self,
    viewforest : &mut Tree<Viewnode>,
  ) {
    let treeids : Vec<NodeId> =
      viewforest . root () . descendants () . map ( |node| node . id () ) . collect ();
    for treeid in treeids {
      let mut node_mut : NodeMut<Viewnode> =
        viewforest . get_mut (treeid) . unwrap ();
      let viewnode : &mut Viewnode = node_mut . value ();
      let is_unknown : bool =
        viewnode . viewnode_id . as_ref ()
        . is_some_and ( |viewnode_id| ! self . known_viewnode_ids . contains (viewnode_id) );
      if is_unknown {
        viewnode . viewnode_id = None; }}}

  /// The end-of-completion pass, after the prune and just before the
  /// text is rendered: give a fresh viewnode ID to every viewnode (but the
  /// buffer root) that lacks one, in buffer order, and make the record
  /// know the viewnode ID of every viewnode the forest holds and
  /// nothing else.
  pub fn assign_viewnode_ids (
    &mut self,
    viewforest : &mut Tree<Viewnode>,
  ) {
    self . advance_counter_past_viewnode_ids_in (viewforest);
    let treeids_without_viewnode_ids : Vec<NodeId> =
      viewforest . root () . descendants ()
      . filter ( |node| node . value () . viewnode_id . is_none ()
                        && node . value () . kind != ViewnodeKind::BufferRoot )
      . map ( |node| node . id () )
      . collect ();
    for treeid in treeids_without_viewnode_ids {
      let viewnode_id : ViewnodeId = self . take_next_viewnode_id ();
      viewforest . get_mut (treeid) . unwrap () . value () . viewnode_id = Some (viewnode_id); }
    self . record_known_viewnode_ids_in (viewforest); }

  /// The next viewnode ID from the counter, skipping any that would read as a
  /// number, since a reader of the sexp would turn it into one and
  /// lose its spelling: Emacs and the Rust sexp crate read '05' and
  /// '1e5' as numbers, and the sexp crate, which parses an atom as
  /// Rust's 'f64' does, also reads 'inf' and 'nan'.
  fn take_next_viewnode_id (
    &mut self,
  ) -> ViewnodeId {
    loop {
      let viewnode_id : ViewnodeId = ViewnodeId::from_counter (self . next_viewnode_id_counter);
      self . next_viewnode_id_counter += 1;
      if viewnode_id . 0 . parse::<f64> () . is_err () { return viewnode_id; }} }

  fn advance_counter_past_viewnode_ids_in (
    &mut self,
    viewforest : &Tree<Viewnode>,
  ) {
    for node in viewforest . root () . descendants () {
      if let Some (counter) = node . value () . viewnode_id . as_ref ()
        . and_then (ViewnodeId::counter_value)
      { self . next_viewnode_id_counter =
          self . next_viewnode_id_counter . max ( counter . saturating_add (1) ); }} }

  fn record_known_viewnode_ids_in (
    &mut self,
    viewforest : &Tree<Viewnode>,
  ) {
    self . known_viewnode_ids = viewforest . root () . descendants ()
      . filter_map ( |node| node . value () . viewnode_id . clone () )
      . collect (); }
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
/// (There can be graph roots at other levels, via non-member (nonMember) children;
/// this does not return those.)
///
/// Extra IDs are pulled from the operation's captured in-Rust graph.
fn root_skgids_from_viewforest (
  graph      : &InRustGraph,
  viewforest : &ViewForest,
) -> HashSet<ID> {
  let mut skgids : HashSet<ID> = HashSet::new ();
  for child in viewforest . roots () {
    if let Some (root_skgid) = child . value () . unrestricted_skgid () {
      skgids . insert ( root_skgid . clone () );
      if let Some (pid) = graph . pid_of ( root_skgid ) {
          if let Some (node) = graph . nodes . get (&pid) {
            skgids . insert ( pid . clone () );
            for extra_id in &node . extra_ids {
              skgids . insert ( extra_id . clone () ); }}}}}
  skgids }
