//! The pruned change view: the viewforest the "all changes" buffer of
//! the diff view renders ('docs/diff-view.org', "C, R, U and D").
//!
//! C is the set of changed graphnodes. R is the set of content
//! origins reached from C by walking containerward through the union
//! of the graph straddle; U is R, C and every node on those walks; D
//! is R minus the content origins that another kept content origin
//! reaches containerward inside U. The view extends contentward from
//! each d in D: a node in U is shown editable with its whole content
//! list, a member outside U is a write-protected member shown without
//! its content. The refresh draws the viewforest in diff-view mode
//! ('server/refresh/diff_view.rs').
//!
//! The merges and skgrepo-changes buffers render pruned change views over
//! subsets of C ('prunedChangeView_sets_for_merges',
//! 'prunedChangeView_sets_for_skgrepo_changes'). The order-only buffer
//! renders no pruned change view: it is a flat list of the nodes whose
//! only change is the order of a list.

use crate::dbs::in_rust_graph::relation_accessors::RelationRole;
use crate::graph_straddle::types::GraphStraddle;
use crate::diff_view::change_leaves::{
  ChangeLeaves, LeafKey, NodeAspect, RelationshipAspect};
use crate::prominence::climb_containerward_to_content_origins_via;
use crate::diff_view::readiness_store::ReadinessStore;
use crate::diff_view::readiness_requests::set_ready_facts_in_viewforest;
use crate::diff_view::change_viewnodes::mk_historical_viewnode;
use crate::refresh::Refreshed;
use crate::types::env::RuntimeGeneration;
use crate::dbs::in_rust_graph::InRustGraph;
use crate::types::misc::{ID, SkgrepoChange};
use crate::types::tree::forest::{ViewForest, editable_occurrences, in_content_position};
use crate::types::viewnode::{
  ChangeFacts, Editability, Existence, FoldAndFocus, Folder, Viewnode, ViewnodeKind,
  Vognode, mk_editable_viewnode, viewnode_of};
use crate::types::views_state::{DiffViewBufferKind, ViewnodeIdRecord, pids_from_viewforest};
use crate::diff_view::completion::complete_viewforest_with_graph_straddle;
use crate::refresh::corrections::BufferCorrectionPolicy;

use ego_tree::{NodeId, NodeRef};
use std::collections::{BTreeSet, HashMap, HashSet, VecDeque};
use std::error::Error;
use std::sync::OnceLock;

/// The sets that define a pruned change view. See the module comment.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct PrunedChangeViewSets {
  pub c : BTreeSet<ID>,
  pub r : BTreeSet<ID>,
  pub u : BTreeSet<ID>,
  pub d : BTreeSet<ID>,
}

/// The pruned change view sets of each buffer kind, each computed on
/// first use. The graph straddle holds one
/// ('GraphStraddle::prunedChangeView_cache'), so the sets are computed
/// once per straddle and persist with it across requests.
pub struct PrunedChangeViewCache {
  sets_by_buffer_kind :
    HashMap<DiffViewBufferKind, OnceLock<PrunedChangeViewSets>>,
}

impl Default for PrunedChangeViewCache {
  fn default (
  ) -> PrunedChangeViewCache {
    PrunedChangeViewCache {
      sets_by_buffer_kind :
        DiffViewBufferKind::ALL . into_iter ()
        . map ( |diffView_buffer_kind| (diffView_buffer_kind, OnceLock::new ()) )
        . collect () } }
}

/// The sets of the pruned change view that a buffer of the diff view
/// renders. The order-only buffer renders no pruned change view: its sets
/// are empty, so each of its roots is shown with its content as
/// write-protected members without their content. Computed once per
/// straddle ('PrunedChangeViewCache').
pub fn prunedChangeView_sets_of_buffer (
  straddle             : &GraphStraddle,
  diffView_buffer_kind : DiffViewBufferKind,
) -> &PrunedChangeViewSets {
  straddle . prunedChangeView_cache . sets_by_buffer_kind [&diffView_buffer_kind]
    . get_or_init ( || match diffView_buffer_kind {
      DiffViewBufferKind::AllChanges =>
        prunedChangeView_sets (straddle),
      DiffViewBufferKind::Merges =>
        prunedChangeView_sets_for_merges (straddle),
      DiffViewBufferKind::SkgrepoChanges =>
        prunedChangeView_sets_for_skgrepo_changes (straddle),
      DiffViewBufferKind::OrderOnlyChanges =>
        PrunedChangeViewSets::default (), } ) }

/// The sets of the pruned change view over all of C.
pub fn prunedChangeView_sets (
  straddle : &GraphStraddle,
) -> PrunedChangeViewSets {
  prunedChangeView_sets_over ( straddle, straddle . changed_pids () . all () ) }

/// The sets of a pruned change view over the changed nodes 'c'.
pub fn prunedChangeView_sets_over (
  straddle : &GraphStraddle,
  c        : BTreeSet<ID>,
) -> PrunedChangeViewSets {
  let containers_of = |gnid : &ID| -> Vec<ID> {
    union_containers (straddle, gnid) };
  let mut r : BTreeSet<ID> = BTreeSet::new ();
  let mut u : BTreeSet<ID> = c . clone ();
  for changed in &c {
    let (walk, content_origins) : (Vec<ID>, HashSet<ID>) =
      climb_containerward_to_content_origins_via (&containers_of, changed);
    u . extend (walk);
    r . extend (content_origins); }
  u . extend ( r . iter () . cloned () );
  let d : BTreeSet<ID> =
    kept_content_origins (&r, &u, &containers_of, &BTreeSet::new ());
  PrunedChangeViewSets { c, r, u, d } }

/// The sets of the merges buffer's pruned change view, over the nodes
/// involved in nodeMerges since HEAD: for each acquiree (a node of the
/// before-graph only, which an after-graph node lists as an extra ID),
/// the acquiree and its acquirer. The acquiree's text preserver is not
/// in C; it is shown as a new member of the acquirer's content.
pub fn prunedChangeView_sets_for_merges (
  straddle : &GraphStraddle,
) -> PrunedChangeViewSets {
  let mut c : BTreeSet<ID> = BTreeSet::new ();
  for acquiree in &straddle . changed_pids () . out_changed {
    let Some (acquirer) : Option<ID> = straddle . acquirer_of (acquiree)
      else { continue; };
    c . insert ( acquiree . clone () );
    c . insert (acquirer); }
  prunedChangeView_sets_over (straddle, c) }

/// The sets of the skgrepo-changes buffer's pruned change view, over
/// the recorders of the change leaves whose aspect is the home skgrepo
/// or the relRepo: the nodes whose home skgrepo moved, and the
/// recorders of the relationships whose relRepo moved.
pub fn prunedChangeView_sets_for_skgrepo_changes (
  straddle : &GraphStraddle,
) -> PrunedChangeViewSets {
  let leaves : &ChangeLeaves = straddle . change_leaves ();
  let c : BTreeSet<ID> =
    leaves . recorders ()
    . filter ( |recorder|
      leaves . leaves_under (recorder) . iter ()
      . any ( |leaf| leaf_is_a_skgrepo_change (&leaf . key) ) )
    . cloned ()
    . collect ();
  prunedChangeView_sets_over (straddle, c) }

/// A home skgrepo move or a relRepo move.
fn leaf_is_a_skgrepo_change (
  key : &LeafKey,
) -> bool {
  matches! ( key,
             LeafKey::Node { aspect : NodeAspect::HomeSkgrepo, .. }
             | LeafKey::Relationship { aspect : RelationshipAspect::RelRepo, .. } ) }

/// The containers of a node in either graph of the straddle, sorted by ID.
pub fn union_containers (
  straddle : &GraphStraddle,
  gnid    : &ID,
) -> Vec<ID> {
  straddle . partners (gnid, RelationRole::CONTAINER) . into_iter ()
    . map ( |partner| partner . id )
    . collect () }

/// D: process R sorted by ID; discard each content origin whose
/// containerward walk within U reaches a content origin still kept. Of
/// two content origins on a shared cycle, exactly one is kept. The
/// 'pinned' nodes (a buffer's standing roots, at a rerender) are kept
/// before the rest is processed, and never discarded; those not in R
/// are not returned.
fn kept_content_origins (
  r             : &BTreeSet<ID>,
  u             : &BTreeSet<ID>,
  containers_of : &dyn Fn (&ID) -> Vec<ID>,
  pinned        : &BTreeSet<ID>,
) -> BTreeSet<ID> {
  let mut kept : BTreeSet<ID> = r . union (pinned) . cloned () . collect ();
  for content_origin in r . difference (pinned) {
    let reaches_a_kept_content_origin : bool = {
      let mut seen : HashSet<ID> = HashSet::from ([ content_origin . clone () ]);
      let mut frontier : Vec<ID> = vec! [ content_origin . clone () ];
      let mut found : bool = false;
      while let Some (current) = frontier . pop () {
        for container in containers_of (&current) {
          if ! u . contains (&container) || ! seen . insert (container . clone ()) {
            continue; }
          if kept . contains (&container) {
            found = true; break; }
          frontier . push (container); }
        if found { break; }}
      found };
    if reaches_a_kept_content_origin {
      kept . remove (content_origin); }}
  kept . intersection (r) . cloned () . collect () }

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
/// ('stub_viewforest_of_roots'). A first rendering reports no
/// correction.
pub fn diffView_buffer_viewforest (
  runtime              : &RuntimeGeneration,
  straddle             : &GraphStraddle,
  diffView_buffer_kind : DiffViewBufferKind,
  store                : &ReadinessStore,
  roots                : &[ID],
) -> Result<RenderedDiffViewBuffer, Box<dyn Error>> {
  let mut viewforest : ViewForest =
    stub_viewforest_of_roots (runtime, straddle, diffView_buffer_kind, roots) ?;
  let refreshed : Refreshed = complete_viewforest_with_graph_straddle (
    runtime, straddle, diffView_buffer_kind, store, None,
    &BufferCorrectionPolicy::default (), &mut viewforest,
    // A new buffer's first viewnode IDs; 'register_view' reads the record
    // back from the forest.
    &mut ViewnodeIdRecord::default () ) ?;
  let pids : Vec<ID> =
    pids_from_viewforest (&viewforest) . into_iter () . collect ();
  Ok ( RenderedDiffViewBuffer { text : refreshed . text, pids, viewforest } ) }

/// What a buffer of the diff view gets in the refresh's tail:
/// its '(ready ...)' facts, and in the skgrepo-changes buffer each
/// moved vognode's home skgrepo move
/// ('set_homeSkgrepoMove_facts_in_viewforest').
pub fn set_diffView_facts_in_viewforest (
  viewforest           : &mut ViewForest,
  straddle             : &GraphStraddle,
  diffView_buffer_kind : DiffViewBufferKind,
  store                : &ReadinessStore,
  graph                : &InRustGraph,
) {
  set_ready_facts_in_viewforest (
    viewforest, straddle . change_leaves (), store, graph );
  if diffView_buffer_kind == DiffViewBufferKind::SkgrepoChanges {
    set_homeSkgrepoMove_facts_in_viewforest (viewforest, straddle ); }}

/// On each occurrence whose home skgrepo moved: the move,
/// and, when the nearest vognode ancestor whose home moved moved the
/// same way, that ancestor's letter. Computed like
/// 'repoBoundary', against the nearest moved ancestor. On
/// every other occurrence, neither (a rerender may follow
/// a commit that made a move part of HEAD).
pub fn set_homeSkgrepoMove_facts_in_viewforest (
  viewforest : &mut ViewForest,
  straddle   : &GraphStraddle,
) {
  let facts : Vec<(NodeId, Option<SkgrepoChange>, Option<String>)> =
    viewforest . nodes ()
    . filter_map ( |node| {
      let ViewnodeKind::Vognode (Vognode::Occurrence (t)) =
        &node . value () . kind else { return None; };
      let home_skgrepo_move : Option<SkgrepoChange> =
        straddle . node_changes (&t . gnid) . home;
      let moved_like_ancestor : Option<String> =
        home_skgrepo_move . as_ref () . and_then ( |home_skgrepo_move|
          nearest_moved_vognode_ancestor (node, straddle)
          . filter ( |(_, ancestor_move)| ancestor_move == home_skgrepo_move )
          . map ( |(generation, _)| ancestor_letter (generation) ));
      Some (( node . id (), home_skgrepo_move, moved_like_ancestor )) } )
    . collect ();
  for (treeid, home_skgrepo_move, moved_like_ancestor) in facts {
    let mut viewnode = // an occurrence, by the filter above
      viewforest . as_internal_tree_mut () . get_mut (treeid) . unwrap ();
    let facts : &mut ChangeFacts = &mut viewnode . value () . change_facts;
    facts . home_skgrepo_move   = home_skgrepo_move;
    facts . moved_like_ancestor = moved_like_ancestor; }}

/// The ancestor generation (1 = viewparent, counting every viewnode
/// between) and the home skgrepo move of the nearest unrestricted
/// vognode ancestor whose home skgrepo moved.
fn nearest_moved_vognode_ancestor (
  node     : NodeRef<Viewnode>,
  straddle : &GraphStraddle,
) -> Option<(usize, SkgrepoChange)> {
  node . ancestors () . enumerate ()
    . find_map ( |(index, ancestor)| match &ancestor . value () . kind {
      ViewnodeKind::Vognode (Vognode::Occurrence (t)) =>
        straddle . node_changes (&t . gnid) . home
        . map ( |home_skgrepo_move| (index + 1, home_skgrepo_move) ),
      _ => None, } ) }

/// The letter the relationship heralds give an ancestor generation:
/// a for 1 (the viewparent), b for 2, and so on; '{N}' past z.
fn ancestor_letter (
  generation : usize,
) -> String {
  if (1 ..= 26) . contains (&generation) {
    char::from ( b'a' + (generation - 1) as u8 ) . to_string ()
  } else { format! ( "{{{}}}", generation ) }}

/// The nodes a buffer of the diff view makes editable: those it
/// has an editable occurrence of, wherever it sits, and the deleted
/// nodes it shows as ghosts in content position. A
/// ghost in content position counts, since its old
/// content is editable nowhere and shown there. The change-forest
/// leaves these nodes record are the changes editable in the buffer.
pub fn nodes_editable_in_diffView (
  viewforest : &ViewForest,
) -> HashSet<ID> {
  let historical_in_content_position : HashSet<ID> =
    viewforest . nodes () . filter_map ( |node| match &node . value () . kind {
      ViewnodeKind::Vognode (Vognode::Ghost (h))
        if in_content_position (node) =>
        Some ( h . gnid . clone () ),
      _ => None, } )
    . collect ();
  editable_occurrences (viewforest) . into_iter ()
    . chain (historical_in_content_position)
    . collect () }

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
/// draw. Returns their treeids.
pub fn append_contentOrigins_to_buffer (
  runtime              : &RuntimeGeneration,
  straddle             : &GraphStraddle,
  diffView_buffer_kind : DiffViewBufferKind,
  viewforest           : &mut ViewForest,
  content_origins      : &[ID],
) -> Result<Vec<NodeId>, Box<dyn Error>> {
  content_origins . iter ()
    . map ( |content_origin| append_stub_root (
      runtime, straddle, diffView_buffer_kind, viewforest, content_origin ) )
    . collect () }

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
/// editable in the view ('rerender_diffView_buffer'): each node of the
/// buffer's recomputed U that has no editable occurrence becomes
/// editable where it stands, at its level-order first content
/// occurrence (a root, a member of a vognode's content, or a removed
/// member). Its children claim membership, as the buffer read back
/// says (under a write-protected parent no claim atom is written); the
/// refresh then marks 'noClaim' on those the graph does not warrant,
/// gives it its body and draws its content list. A node of U with no occurrence is reached through
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
  let mut editable : HashSet<ID> = editable_occurrences (viewforest);
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
      match &mut viewforest . get_mut (child) . unwrap () . value () . kind {
        ViewnodeKind::Vognode (Vognode::Occurrence (occurrence)) =>
          occurrence . claimsMembership = true,
        ViewnodeKind::Vognode (Vognode::Unknown (unknown)) =>
          unknown . claimsMembership = true,
        ViewnodeKind::Vognode (Vognode::Ghost (ghost)) =>
          ghost . claimsMembership = true,
        _ => {}, }}}}

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
