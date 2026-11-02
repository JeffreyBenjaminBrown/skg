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

/// The largest viewnode ID counter a buffer the connection does not
/// hold may carry: half the counter's range, leaving the other half
/// for the fresh viewnode IDs counted past it.
pub const LARGEST_CARRIED_VIEWNODE_ID_COUNTER : u64 = u64::MAX / 2;

/// Identifies a buffer in the client.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum ViewforestId {
  ContentView  (String), // the root gnid
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

  root_gnids        : ManyToMany<ID, ViewforestId>, // Maps every root ID (primary + extra) ↔ ViewforestId. Supports many-to-many because a view can have multiple roots, and an ID could be a root in multiple views. Maintained by register_view / update_view / unregister_view.
}

/// Invariant: all viewforest mutations must go through register_view /
/// update_view, which maintain pids in sync with the viewforest.
/// Direct viewforest mutation would make pids stale.
pub struct ViewState {
  pub viewforest : ViewForest,
  pub pids       : HashSet<ID>, // the occurrences in the buffer (the
                            // kind this view renders meaningfully; see
                            // pids_from_viewforest)
  pub diffView_buffer_kind : Option<DiffViewBufferKind>, // Some for a buffer of the diff view
  pub viewnode_id_record : ViewnodeIdRecord,
  pub extend_at_next_rerender_all : bool, // for a buffer of the diff view a reload named: the next "rerender all views" that rerenders it extends it ('DiffViewBufferRerender::ExtendingWhereAReloadAsked')
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
      root_gnids : ManyToMany::new () }}

  pub fn clear (&mut self) {
    self . views       . clear ();
    self . root_gnids    = ManyToMany::new (); }

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
  pub fn content_viewforest_id_for_root_gnid (
    &self,
    gnid : &ID,
  ) -> Option<&ViewforestId> {
    self . root_gnids . get_right (gnid)
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
        root_gnids_from_viewforest ( graph, &viewforest );
      for rid in &rids {
        self . root_gnids . insert (
          rid . clone (), viewforest_id . clone () ); }
      let pids : HashSet<ID> =
        pids . iter () . cloned () . collect ();
      let viewnode_id_record : ViewnodeIdRecord =
        ViewnodeIdRecord::of_viewforest (&viewforest);
      let state : ViewState =
        ViewState { viewforest, pids, diffView_buffer_kind : None,
                    viewnode_id_record,
                    extend_at_next_rerender_all : false };
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

  /// Record that the open view 'viewforest_id' is a buffer of the
  /// diff view of kind 'diffView_buffer_kind'.
  pub fn mark_as_diffView_buffer (
    &mut self,
    viewforest_id        : &ViewforestId,
    diffView_buffer_kind : DiffViewBufferKind,
  ) {
    if let Some (state) = self . views . get_mut (viewforest_id) {
      state . diffView_buffer_kind = Some (diffView_buffer_kind); }}

  /// Set whether the next "rerender all views" that rerenders the
  /// open view 'viewforest_id' extends it, if it is a buffer of the
  /// diff view ('ViewState::extend_at_next_rerender_all').
  pub fn set_extend_at_next_rerender_all (
    &mut self,
    viewforest_id : &ViewforestId,
    extend        : bool,
  ) {
    if let Some (state) = self . views . get_mut (viewforest_id) {
      state . extend_at_next_rerender_all =
        extend && state . diffView_buffer_kind . is_some (); }}

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

  /// The record a save checks its parsed buffer's viewnode IDs
  /// against: the view's, when 'viewforest_id' names a view this
  /// connection holds; otherwise the buffer's own
  /// ('ViewnodeIdRecord::of_viewforest' of 'parsed_viewforest', whose
  /// parse already dropped every copy's viewnode ID), so each headline
  /// keeps the one it carries and fresh ones start past them all.
  pub fn viewnode_id_record_for_saved_buffer (
    &self,
    viewforest_id     : Option<&ViewforestId>,
    parsed_viewforest : &Tree<Viewnode>,
  ) -> ViewnodeIdRecord {
    viewforest_id
      . and_then ( |viewforest_id| self . views . get (viewforest_id) )
      . map ( |state| state . viewnode_id_record . clone () )
      . unwrap_or_else ( || ViewnodeIdRecord::of_viewforest (parsed_viewforest) ) }

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
      self . root_gnids . remove_right (viewforest_id);
      let rids : HashSet<ID> =
        root_gnids_from_viewforest ( graph, &new_viewforest );
      for rid in &rids {
        self . root_gnids . insert (
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
                           viewnode_id_record,
                           extend_at_next_rerender_all : false } ); }}

  pub fn unregister_view (
    &mut self,
    viewforest_id : &ViewforestId,
  ) { self . root_gnids . remove_right (viewforest_id);
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

  /// A buffer the connection does not hold keeps the viewnode IDs it
  /// carries, and its record counts fresh ones from past the highest
  /// ('viewnode_id_record_for_saved_buffer'). Refuses a buffer
  /// carrying one past 'LARGEST_CARRIED_VIEWNODE_ID_COUNTER' (one typed
  /// by hand, say), which would leave the counter too little room.
  pub fn error_unless_carried_viewnode_ids_leave_room (
    viewforest : &Tree<Viewnode>,
  ) -> Result<(), String> {
    let too_large : Vec<String> = viewforest . root () . descendants ()
      . filter_map ( |node| node . value () . viewnode_id . as_ref () )
      . filter ( |viewnode_id| viewnode_id . counter_value ()
                 . is_some_and ( |counter| counter > LARGEST_CARRIED_VIEWNODE_ID_COUNTER ))
      . map ( |viewnode_id| viewnode_id . 0 . clone () )
      . collect ();
    if too_large . is_empty () { return Ok (( )); }
    Err ( format! (
      "Nothing was saved: the viewnode ID(s) {} are too large. A buffer the server does not hold keeps the viewnode IDs it carries, and each must be at most '{}'. Remove the '(vnid ...)' of each such headline, and the save gives it a fresh one.",
      too_large . iter () . map ( |id| format! ("'{}'", id) )
        . collect::<Vec<String>> () . join (", "),
      ViewnodeId::from_counter (LARGEST_CARRIED_VIEWNODE_ID_COUNTER) )) }

  /// After a parse, before the refresh runs: drop every viewnode ID
  /// the record does not know -- one typed by hand, or brought back by
  /// an old copy of the buffer's text -- so that its headline gets a
  /// fresh one at the refresh's tail, and the counter is not moved by
  /// it.
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

  /// The refresh's last step at its tail, after the prune and just before the
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
      self . next_viewnode_id_counter =
        self . next_viewnode_id_counter . checked_add (1)
        . expect ( "a viewnode ID counter has room: a held view drops the IDs it does not know, and a save refuses an unheld buffer whose IDs leave none ('error_unless_carried_viewnode_ids_leave_room')" );
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
/// primary ids of its occurrences -- the only kind backed by a
/// real, current graphnode that this view renders meaningfully. A tombstone
/// standing for a restricted node shows nothing about the node, so a save
/// touching that node need not re-render this view (its occurrence
/// descendants register themselves). Ghosts and unknowns
/// are excluded too: they are not graph members. The single source of
/// which kinds count: update_view derives its pids through it, and the de-novo
/// caller (multi_root_view_via_runtime) computes the pids it passes to register_view
/// through it too.
pub fn pids_from_viewforest (
  viewforest : &ViewForest,
) -> HashSet<ID> {
  viewforest . nodes ()
    . filter_map ( |n| match &n . value () . kind {
      ViewnodeKind::Vognode (Vognode::Occurrence (t)) =>
        Some ( t . gnid . clone () ),
      _ => None } )
    . collect () }

/// Collect all IDs (primary + extras) for every root
/// -- i.e. every level-1 headline -- in the view.
/// (There can be graph roots at other levels, via noClaim children;
/// this does not return those.)
///
/// Extra IDs are pulled from the operation's captured in-Rust graph.
fn root_gnids_from_viewforest (
  graph      : &InRustGraph,
  viewforest : &ViewForest,
) -> HashSet<ID> {
  let mut gnids : HashSet<ID> = HashSet::new ();
  for child in viewforest . roots () {
    if let Some (root_gnid) = child . value () . occurrence_gnid () {
      gnids . insert ( root_gnid . clone () );
      if let Some (pid) = graph . pid_of ( root_gnid ) {
          if let Some (node) = graph . nodes . get (&pid) {
            gnids . insert ( pid . clone () );
            for extra_id in &node . extra_ids {
              gnids . insert ( extra_id . clone () ); }}}}}
  gnids }
