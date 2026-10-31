//! Buffers of the diff view ('docs/diff-view.org', "Rendering a
//! buffer"): the stub of a fresh one, its first rendering, and the
//! refresh of an open one, with what the "integrate unshown changes"
//! request adds to it. Each is a refresh in diff-view mode
//! ('refresh_viewforest', 'server/refresh/'), over the graph straddle,
//! with the '(ready ...)' change facts read from the store: the first
//! rendering refreshes a stub of the buffer's roots
//! ('diffView_buffer_viewforest'); the rerender pass
//! ('refresh_view' in 'server/rerender_pass.rs') brings the straddle
//! up to date and refreshes what the buffer holds
//! ('refresh_diffView_buffer_over_current_straddle'), and renders the
//! result. Content views have the same three in
//! 'server/update_buffer.rs'.

use crate::diff_view::pruned_change_view::{
  PrunedChangeViewSets, kept_content_origins, nodes_editable_in_diffView,
  prunedChangeView_sets_of_buffer, union_containers};
use crate::diff_view::change_viewnodes::mk_historical_viewnode;
use crate::diff_view::readiness_store::ReadinessStore;
use crate::org_to_text::viewforest_to_string;
use crate::graph_straddle::build::current_graph_straddle_in_slot;
use crate::graph_straddle::types::GraphStraddle;
use crate::refresh::{Truth, refresh_viewforest};
use crate::skgrepo_sets::SkgrepoRestriction;
use crate::types::env::RuntimeGeneration;
use crate::types::misc::ID;
use crate::types::tree::forest::{ViewForest, editable_gnids, in_content_position};
use crate::types::viewnode::{
  Editability, Existence, FoldAndFocus, Folder, Viewnode, ViewnodeKind, Vognode,
  mk_editable_viewnode, viewnode_of};
use crate::types::views_state::{DiffViewBufferKind, ViewnodeIdRecord, pids_from_viewforest};
use crate::rerender_pass::{DiffViewBufferRerender, RerenderPassContext};
use crate::refresh::mode::RefreshMode;

use ego_tree::{NodeId, NodeRef};
use std::collections::{BTreeSet, HashSet, VecDeque};
use std::error::Error;
use std::sync::{Arc, MutexGuard};

/// Refresh 'viewforest', a buffer of kind 'diffView_buffer_kind' of
/// the diff view, over 'straddle', with the '(ready ...)' change facts from
/// 'store'. The goal holds what HEAD alone warrants, the change
/// viewnodes are recomputed in place, and the buffer's sets choose
/// what may be editable ('drawn_occurrence_editability'). The diff view opens only
/// to an unrestricted connection, but a switch rerenders its open
/// buffers under 'restriction', which the goal and the ID rule ask as
/// in a content view: a restricted node is never a ghost.
pub fn refresh_diffView_viewforest (
  runtime              : &RuntimeGeneration,
  straddle             : &GraphStraddle,
  diffView_buffer_kind : DiffViewBufferKind,
  store                : &ReadinessStore,
  restriction          : &SkgrepoRestriction,
  viewforest           : &mut ViewForest,
  viewnode_id_record   : &mut ViewnodeIdRecord,
) -> Result<(), Box<dyn Error>> {
  refresh_viewforest (
    viewforest, viewnode_id_record, &Truth::of_runtime (runtime, restriction),
    RefreshMode::DiffView { straddle,
                            buffer_kind     : diffView_buffer_kind,
                            readiness_store : store },
    runtime . config . initial_node_limit ) }

/// Bring an open buffer of the diff view up to date: bring the
/// straddle in the context's slot up to date, and refresh the buffer
/// over it; it renders nothing. Only for the "integrate unshown
/// changes" request ('DiffViewBufferRerender::IntegratingUnshownChanges')
/// does it make
/// occurrences editable or append roots, both before the one refresh:
/// each node of the recomputed U without an editable occurrence
/// becomes editable at its first content occurrence, and the content
/// origins of the recomputed D that the buffer makes editable nowhere
/// are appended as stub roots, for the refresh to draw. Its standing
/// roots stay.
pub fn refresh_diffView_buffer_over_current_straddle (
  context              : &mut RerenderPassContext<'_>,
  diffView_buffer_kind : DiffViewBufferKind,
  viewforest           : &mut ViewForest,
  viewnode_id_record   : &mut ViewnodeIdRecord,
) -> Result<(), Box<dyn Error>> {
  let runtime : &RuntimeGeneration = &context . runtime;
  let restriction : &SkgrepoRestriction = context . skgrepo_restriction;
  let straddle : Arc<GraphStraddle> =
    current_graph_straddle_in_slot (
      &mut context . graph_straddle, &runtime . config, runtime . graph . clone () )
    . map_err ( |e| format! (
      "Cannot rerender the diff view buffer: {}", e )) ?;
  if context . diffView_buffer_rerender == DiffViewBufferRerender::IntegratingUnshownChanges {
    let standing_roots : BTreeSet<ID> = root_gnids_of_buffer (viewforest);
    ensure_editableOccurrences_exist (&straddle, diffView_buffer_kind, viewforest);
    let new_roots : Vec<ID> = contentOrigins_missing_from_buffer (
      &straddle, diffView_buffer_kind, viewforest, &standing_roots );
    append_contentOrigins_to_buffer (
      runtime, &straddle, diffView_buffer_kind, viewforest, &new_roots ) ?; }
  let store : MutexGuard<ReadinessStore> = context . env . runtime . lock_readiness_store ();
  refresh_diffView_viewforest (
    runtime, &straddle, diffView_buffer_kind, &store, restriction,
    viewforest, viewnode_id_record ) }

/// A buffer of the diff view, rendered: its text, the pids to register
/// it under, and its viewforest.
pub struct RenderedDiffViewBuffer {
  pub text       : String,
  pub pids       : Vec<ID>,
  pub viewforest : ViewForest,
}

/// The buffer of kind 'diffView_buffer_kind' over 'straddle', with
/// roots 'roots' (for a pruned change view, its D in order), and
/// readiness from 'store': the refresh of a stub of its roots
/// ('stub_viewforest_of_roots').
pub fn diffView_buffer_viewforest (
  runtime              : &RuntimeGeneration,
  straddle             : &GraphStraddle,
  diffView_buffer_kind : DiffViewBufferKind,
  store                : &ReadinessStore,
  roots                : &[ID],
) -> Result<RenderedDiffViewBuffer, Box<dyn Error>> {
  let mut viewforest : ViewForest =
    stub_viewforest_of_roots (runtime, straddle, diffView_buffer_kind, roots) ?;
  refresh_diffView_viewforest (
    runtime, straddle, diffView_buffer_kind, store,
    &SkgrepoRestriction::unrestricted (&runtime . config), // the diff view opens unrestricted
    &mut viewforest,
    // A new buffer's first viewnode IDs; 'register_view' reads the record
    // back from the forest.
    &mut ViewnodeIdRecord::default () ) ?;
  let text : String = viewforest_to_string (&viewforest) ?;
  let pids : Vec<ID> =
    pids_from_viewforest (&viewforest) . into_iter () . collect ();
  Ok ( RenderedDiffViewBuffer { text, pids, viewforest } ) }

/// The stub the refresh of a buffer of the diff view starts from:
/// each root as 'append_stub_root' appends it.
fn stub_viewforest_of_roots (
  runtime              : &RuntimeGeneration,
  straddle             : &GraphStraddle,
  diffView_buffer_kind : DiffViewBufferKind,
  roots                : &[ID],
) -> Result<ViewForest, Box<dyn Error>> {
  let mut viewforest : ViewForest = ViewForest::new ();
  for root in roots {
    append_stub_root (
      runtime, straddle, diffView_buffer_kind, &mut viewforest, root ) ?; }
  Ok (viewforest) }

/// Append 'root' to the viewforest as a stub: an editable occurrence,
/// its body folded unless its node is in C (it is context), whose
/// content the refresh draws, editable where it is in U; or, when the
/// after-graph lacks the node, its ghost, whose old content the
/// refresh draws. In a pruned change
/// view, a multiply-contained root gets a folded container folder,
/// which the refresh fills with its containers in either graph and
/// their containerward chains. The order-only buffer, a flat list,
/// gets none. Returns the root's treeid.
fn append_stub_root (
  runtime              : &RuntimeGeneration,
  straddle             : &GraphStraddle,
  diffView_buffer_kind : DiffViewBufferKind,
  viewforest           : &mut ViewForest,
  root                 : &ID,
) -> Result<NodeId, Box<dyn Error>> {
  let buffer_root : NodeId = viewforest . internal_root_gnid ();
  let stub : Viewnode = match runtime . graph . get (root) {
    None => mk_historical_viewnode (straddle, root, None)
      . ok_or_else ( || format! (
        "diffView_buffer_viewforest: root {} is in neither graph", root )) ?,
    Some (node) => {
      let mut stub : Viewnode = mk_editable_viewnode (
        node . pid . clone (), node . home_skgrepo . clone (),
        node . title . clone (), node . body . clone () );
      stub . foldAndFocus . bodyFolded =
        ! prunedChangeView_sets_of_buffer (straddle, diffView_buffer_kind)
          . c . contains (root);
      stub }, };
  let is_occurrence : bool = stub . is_occurrence ();
  let root_treeid : NodeId =
    viewforest . as_internal_tree_mut () . get_mut (buffer_root)
    . ok_or ("diffView_buffer_viewforest: no buffer root") ?
    . append (stub) . id ();
  if is_occurrence
    && diffView_buffer_kind != DiffViewBufferKind::OrderOnlyChanges
    && union_containers (straddle, root) . len () > 1 {
    viewforest . as_internal_tree_mut () . get_mut (root_treeid)
      . ok_or ("diffView_buffer_viewforest: a stub root vanished") ?
      . append ( Viewnode {
        foldAndFocus : FoldAndFocus { folded : true, .. FoldAndFocus::default () },
        .. viewnode_of ( ViewnodeKind::Folder (Folder::Container) ) } ); }
  Ok (root_treeid) }

/// The content origins in D, of the buffer's sets recomputed after
/// the save, that it makes editable nowhere, in ID order. The recomputed
/// R is deduplicated with the buffer's 'standing_roots' pinned as
/// kept, so that a change to a rootlike cycle's membership never adds
/// a second root for a cycle that already stands.
pub fn contentOrigins_missing_from_buffer (
  straddle             : &GraphStraddle,
  diffView_buffer_kind : DiffViewBufferKind,
  viewforest           : &ViewForest,
  standing_roots       : &BTreeSet<ID>,
) -> Vec<ID> {
  let sets : &PrunedChangeViewSets =
    prunedChangeView_sets_of_buffer (straddle, diffView_buffer_kind);
  let containers_of = |gnid : &ID| -> Vec<ID> {
    union_containers (straddle, gnid) };
  let editable : HashSet<ID> = nodes_editable_in_diffView (viewforest);
  kept_content_origins (&sets . r, &sets . u, &containers_of, standing_roots)
    . into_iter ()
    . filter ( |content_origin| ! editable . contains (content_origin)
                                && ! standing_roots . contains (content_origin) )
    . collect () }

/// Append 'content_origins' at the end of the buffer, each stubbed as the first
/// rendering stubs a root ('append_stub_root'), for the refresh to
/// draw.
pub fn append_contentOrigins_to_buffer (
  runtime              : &RuntimeGeneration,
  straddle             : &GraphStraddle,
  diffView_buffer_kind : DiffViewBufferKind,
  viewforest           : &mut ViewForest,
  content_origins      : &[ID],
) -> Result<(), Box<dyn Error>> {
  for content_origin in content_origins {
    append_stub_root (
      runtime, straddle, diffView_buffer_kind, viewforest, content_origin ) ?; }
  Ok (( )) }

/// The IDs of the buffer's roots, whatever kind of vognode each is.
pub fn root_gnids_of_buffer (
  viewforest : &ViewForest,
) -> BTreeSet<ID> {
  viewforest . roots ()
    . filter_map ( |root| match &root . value () . kind {
      ViewnodeKind::Vognode (vognode) => Some ( vognode . gnid () . clone () ),
      _ => None, } )
    . collect () }

/// Before the refresh of a rerender that integrates the changes not
/// editable in the view ('refresh_diffView_buffer_over_current_straddle'): each node of the
/// buffer's recomputed U that has no editable occurrence becomes
/// editable where it stands, at its level-order first content
/// occurrence (a root, a member of a vognode's content, or a removed
/// member). Its children claim membership, as the buffer read back
/// says (under a write-protected parent no claim atom is written); the
/// refresh then makes those the graph does not warrant vanish, gives
/// it its body and draws its content list. A node of U with no occurrence is reached through
/// the node above it on its walk, once that one is editable. A node
/// shown only as a write-protected member of a folder counts as shown
/// nowhere: a folder's members stay write-protected, and if the node
/// is a content origin of D, it is appended as a root
/// ('contentOrigins_missing_from_buffer').
pub fn ensure_editableOccurrences_exist (
  straddle             : &GraphStraddle,
  diffView_buffer_kind : DiffViewBufferKind,
  viewforest           : &mut ViewForest,
) {
  let u : &BTreeSet<ID> =
    & prunedChangeView_sets_of_buffer (straddle, diffView_buffer_kind) . u;
  let mut editable : HashSet<ID> = editable_gnids (viewforest);
  let occurrences : Vec<NodeId> =
    content_occurrences_in_level_order (viewforest);
  for treeid in occurrences {
    if let ViewnodeKind::Vognode (Vognode::Occurrence (occurrence)) =
      &mut viewforest . get_mut (treeid) . unwrap () . value () . kind
    { if ! u . contains (&occurrence . gnid)
         || editable . contains (&occurrence . gnid) { continue; }
      occurrence . editability = Editability::Editable { // the refresh gives it its body
        body : None, edit_request : None };
      editable . insert ( occurrence . gnid . clone () ); }
    else { continue; }
    let children : Vec<NodeId> = viewforest . get (treeid) . unwrap () . children ()
      . map ( |child| child . id () ) . collect ();
    for child in children {
      if matches! ( viewforest . get (child) . unwrap () . value () . kind,
                    ViewnodeKind::Vognode (_) ) {
        viewforest . get_mut (child) . unwrap () . value () . set_claim (true); }}}}

/// The viewforest's occurrences in content position
/// ('in_content_position') that are roots, members or removed members
/// there, in level order. The walk passes through a ghost
/// in content position, to its old content, the removed
/// members under it.
fn content_occurrences_in_level_order (
  viewforest : &ViewForest,
) -> Vec<NodeId> {
  let mut occurrences : Vec<NodeId> = Vec::new ();
  let mut queue : VecDeque<NodeRef<Viewnode>> =
    viewforest . roots () . collect ();
  while let Some (node) = queue . pop_front () {
    if ! in_content_position (node) { continue; }
    match &node . value () . kind {
      ViewnodeKind::Vognode (Vognode::Occurrence (t)) => {
        let is_root_or_member : bool =
          t . claimsMembership
          || node . parent () . is_some_and ( |parent| match parent . value () . kind {
               ViewnodeKind::BufferRoot => true,
               ViewnodeKind::Vognode (
                 Vognode::Occurrence (_)
                 | Vognode::Ghost (_) ) =>
                 node . value () . change_facts . change . rel
                 == Some (Existence::Removed),
               _ => false, } );
        if ! is_root_or_member { continue; }
        occurrences . push ( node . id () ); },
      ViewnodeKind::Vognode (Vognode::Ghost (_)) => {},
      _ => continue, }
    queue . extend ( node . children () ); }
  occurrences }
