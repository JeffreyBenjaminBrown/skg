//! The PCSVF ("pruned change-spanning viewforest"): the viewforest
//! the "all changes" buffer of the diff view draws
//! ('docs/diff-view.org', "C, R, U and D").
//!
//! C is the set of changed graphnodes. R is the set of content
//! origins reached from C by walking containerward through the union
//! of the graph straddle; U is R, C and every node on those walks; D
//! is R minus the content origins that another kept content origin
//! reaches containerward inside U. The view unfolds contentward from
//! each d in D: a node in U is drawn editable with its whole content
//! list, a member outside U is a write-protected member drawn without
//! its content. View completion does the drawing, its
//! 'diffView_source' set ('process_unrestrictedVognode_changes').
//!
//! The merges and skgrepo-changes buffers draw PCSVFs over subsets of
//! C ('pcsvf_sets_for_merges', 'pcsvf_sets_for_skgrepo_changes'). The
//! order-only buffer draws no PCSVF: it is a flat list of the nodes
//! whose only change is the order of a list.

use crate::dbs::in_rust_graph::relation_accessors::RelationRole;
use crate::graph_straddle::types::GraphStraddle;
use crate::diff_view::change_leaves::{
  ChangeLeaves, LeafKey, NodeAspect, RelationshipAspect};
use crate::prominence::climb_containerward_to_content_origins_via;
use crate::diff_view::readiness_store::ReadinessStore;
use crate::diff_view::readiness_requests::set_ready_facts_in_viewforest;
use crate::diff_view::change_viewnodes::mk_historical_viewnode;
use crate::diff_view::fresh_drawing::is_unrestricted_removed_member;
use crate::to_org::util::{
  EditableMap, append_plain_stub_root, graphnode_and_viewnode_from_skgid};
use crate::types::env::RuntimeGeneration;
use crate::dbs::in_rust_graph::InRustGraph;
use crate::types::misc::{ID, SkgConfig, SkgrepoChange};
use crate::types::nodes::complete::Graphnode;
use crate::types::tree::forest::{ViewForest, editable_occurrences, in_content_position};
use crate::types::viewnode::{
  AffectsParent, Editability, PartnerFolder, Phantom, Viewnode, ViewnodeKind,
  Vognode, viewnode_of};
use crate::types::views_state::{DiffViewBufferKind, pids_from_viewforest};
use crate::diff_view::completion::complete_viewforest_with_graph_straddle;
use crate::update_buffer::finish_viewforest;

use ego_tree::{NodeId, NodeRef};
use std::collections::{BTreeSet, HashMap, HashSet, VecDeque};
use std::error::Error;
use std::sync::OnceLock;

/// The sets that define a PCSVF. See the module comment.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct PcsvfSets {
  pub c : BTreeSet<ID>,
  pub r : BTreeSet<ID>,
  pub u : BTreeSet<ID>,
  pub d : BTreeSet<ID>,
}

/// What view completion draws a buffer of the diff view from:
/// the graph straddle, and the buffer, whose PCSVF sets it draws.
#[derive(Clone, Copy)]
pub struct DiffViewSource<'a> {
  pub straddle   : &'a GraphStraddle,
  pub diffView_buffer_kind : DiffViewBufferKind,
}

impl<'a> DiffViewSource<'a> {
  pub fn sets (
    &self,
  ) -> &'a PcsvfSets {
    pcsvf_sets_of_buffer (self . straddle, self . diffView_buffer_kind) }
}

/// The PCSVF sets of each buffer kind, each computed on first use.
/// The graph straddle holds one ('GraphStraddle::pcsvf_cache'), so
/// the sets are computed once per straddle and persist with it across
/// requests.
pub struct PcsvfCache {
  sets_by_buffer_kind : HashMap<DiffViewBufferKind, OnceLock<PcsvfSets>>,
}

impl Default for PcsvfCache {
  fn default (
  ) -> PcsvfCache {
    PcsvfCache {
      sets_by_buffer_kind :
        DiffViewBufferKind::ALL . into_iter ()
        . map ( |diffView_buffer_kind| (diffView_buffer_kind, OnceLock::new ()) )
        . collect () } }
}

/// The sets of the PCSVF that a buffer of the diff view draws. The
/// order-only buffer draws no PCSVF: its sets are empty, so each of
/// its roots is drawn with its content as unexpanded write-protected
/// members. Computed once per straddle ('PcsvfCache').
pub fn pcsvf_sets_of_buffer (
  straddle             : &GraphStraddle,
  diffView_buffer_kind : DiffViewBufferKind,
) -> &PcsvfSets {
  straddle . pcsvf_cache . sets_by_buffer_kind [&diffView_buffer_kind]
    . get_or_init ( || match diffView_buffer_kind {
      DiffViewBufferKind::AllChanges       => pcsvf_sets (straddle),
      DiffViewBufferKind::Merges           => pcsvf_sets_for_merges (straddle),
      DiffViewBufferKind::SkgrepoChanges   => pcsvf_sets_for_skgrepo_changes (straddle),
      DiffViewBufferKind::OrderOnlyChanges => PcsvfSets::default (), } ) }

/// The sets of the PCSVF over all of C.
pub fn pcsvf_sets (
  straddle : &GraphStraddle,
) -> PcsvfSets {
  pcsvf_sets_over ( straddle, straddle . changed_pids () . all () ) }

/// The sets of a PCSVF over the changed nodes 'c'.
pub fn pcsvf_sets_over (
  straddle : &GraphStraddle,
  c        : BTreeSet<ID>,
) -> PcsvfSets {
  let containers_of = |skgid : &ID| -> Vec<ID> {
    union_containers (straddle, skgid) };
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
  PcsvfSets { c, r, u, d } }

/// The sets of the merges buffer's PCSVF, over the nodes involved in
/// nodeMerges since HEAD: for each acquiree (a node of the
/// before-graph only, which an after-graph node lists as an extra
/// ID), the acquiree and its acquirer. The acquiree's text preserver
/// is not in C; it is drawn as a new member of the acquirer's content.
pub fn pcsvf_sets_for_merges (
  straddle : &GraphStraddle,
) -> PcsvfSets {
  let mut c : BTreeSet<ID> = BTreeSet::new ();
  for acquiree in &straddle . changed_pids () . out_changed {
    let Some (acquirer) : Option<ID> = straddle . acquirer_of (acquiree)
      else { continue; };
    c . insert ( acquiree . clone () );
    c . insert (acquirer); }
  pcsvf_sets_over (straddle, c) }

/// The sets of the skgrepo-changes buffer's PCSVF, over the recorders
/// of the change leaves whose aspect is the home skgrepo or the
/// relRepo: the nodes whose home skgrepo moved, and the recorders of
/// the relationships whose relRepo moved.
pub fn pcsvf_sets_for_skgrepo_changes (
  straddle : &GraphStraddle,
) -> PcsvfSets {
  let leaves : &ChangeLeaves = straddle . change_leaves ();
  let c : BTreeSet<ID> =
    leaves . recorders ()
    . filter ( |recorder|
      leaves . leaves_under (recorder) . iter ()
      . any ( |leaf| leaf_is_a_skgrepo_change (&leaf . key) ) )
    . cloned ()
    . collect ();
  pcsvf_sets_over (straddle, c) }

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
  skgid    : &ID,
) -> Vec<ID> {
  straddle . partners (skgid, RelationRole::CONTAINER) . into_iter ()
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

/// A buffer of the diff view, drawn: its text, the pids to register
/// it under, and its viewforest.
pub struct DrawnDiffViewBuffer {
  pub text       : String,
  pub pids       : Vec<ID>,
  pub viewforest : ViewForest,
}

/// The buffer 'drawing' names, with roots 'roots' (for a PCSVF, its
/// D in order), drawn with readiness from 'store'.
pub fn diffView_buffer_viewforest (
  runtime : &RuntimeGeneration,
  drawing : DiffViewSource,
  store   : &ReadinessStore,
  roots   : &[ID],
) -> Result<DrawnDiffViewBuffer, Box<dyn Error>> {
  let mut viewforest : ViewForest =
    stub_viewforest_of_roots (runtime, drawing, roots) ?;
  complete_viewforest_with_graph_straddle (runtime, drawing, &mut viewforest) ?;
  set_diffView_facts_in_viewforest (
    &mut viewforest, drawing, store, &runtime . graph );
  let text : String = finish_viewforest (
    &mut viewforest, &runtime . graph, &runtime . config, None,
    Some (&drawing . straddle . before) ) ?;
  let pids : Vec<ID> =
    pids_from_viewforest (&viewforest) . into_iter () . collect ();
  Ok ( DrawnDiffViewBuffer { text, pids, viewforest } ) }

/// What a buffer of the diff view gets after view completion:
/// its '(ready ...)' facts, and in the skgrepo-changes buffer each
/// moved vognode's home move ('set_home_move_facts_in_viewforest').
pub fn set_diffView_facts_in_viewforest (
  viewforest : &mut ViewForest,
  drawing    : DiffViewSource,
  store      : &ReadinessStore,
  graph      : &InRustGraph,
) {
  set_ready_facts_in_viewforest (
    viewforest, drawing . straddle . change_leaves (), store, graph );
  if drawing . diffView_buffer_kind == DiffViewBufferKind::SkgrepoChanges {
    set_home_move_facts_in_viewforest (viewforest, drawing . straddle ); }}

/// On each unrestricted vognode whose home skgrepo moved: the move,
/// and, when the nearest vognode ancestor whose home moved moved the
/// same way, that ancestor's letter. Computed like
/// 'homeSkgrepoAtBoundary', against the nearest moved ancestor. On
/// every other unrestricted vognode, neither (a rerender may follow
/// a commit that made a move part of HEAD).
pub fn set_home_move_facts_in_viewforest (
  viewforest : &mut ViewForest,
  straddle   : &GraphStraddle,
) {
  let facts : Vec<(NodeId, Option<SkgrepoChange>, Option<String>)> =
    viewforest . nodes ()
    . filter_map ( |node| {
      let ViewnodeKind::Vognode (Vognode::Unrestricted (t)) =
        &node . value () . kind else { return None; };
      let home_skgrepo_move : Option<SkgrepoChange> =
        straddle . node_changes (&t . skgid) . home;
      let moved_like_ancestor : Option<String> =
        home_skgrepo_move . as_ref () . and_then ( |home_skgrepo_move|
          nearest_moved_vognode_ancestor (node, straddle)
          . filter ( |(_, ancestor_move)| ancestor_move == home_skgrepo_move )
          . map ( |(generation, _)| ancestor_letter (generation) ));
      Some (( node . id (), home_skgrepo_move, moved_like_ancestor )) } )
    . collect ();
  for (treeid, home_skgrepo_move, moved_like_ancestor) in facts {
    if let ViewnodeKind::Vognode (Vognode::Unrestricted (t)) =
      &mut viewforest . as_internal_tree_mut () . get_mut (treeid)
      . unwrap () . value () . kind {
      t . viewStats . home_skgrepo_move = home_skgrepo_move;
      t . viewStats . moved_like_ancestor = moved_like_ancestor; }}}

/// The ancestor generation (1 = viewparent, counting every viewnode
/// between) and the home move of the nearest unrestricted vognode
/// ancestor whose home skgrepo moved.
fn nearest_moved_vognode_ancestor (
  node     : NodeRef<Viewnode>,
  straddle : &GraphStraddle,
) -> Option<(usize, SkgrepoChange)> {
  node . ancestors () . enumerate ()
    . find_map ( |(index, ancestor)| match &ancestor . value () . kind {
      ViewnodeKind::Vognode (Vognode::Unrestricted (t)) =>
        straddle . node_changes (&t . skgid) . home
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
/// nodes it draws as historical phantoms in content position. A
/// historical phantom in content position counts, since its old
/// content is editable nowhere and shown there. The change-forest
/// leaves these nodes record are the changes editable in the buffer.
pub fn nodes_editable_in_diffView (
  viewforest : &ViewForest,
) -> HashSet<ID> {
  let historical_in_content_position : HashSet<ID> =
    viewforest . nodes () . filter_map ( |node| match &node . value () . kind {
      ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Historical (h)))
        if in_content_position (node) =>
        Some ( h . skgid . clone () ),
      _ => None, } )
    . collect ();
  editable_occurrences (viewforest) . into_iter ()
    . chain (historical_in_content_position)
    . collect () }

/// The stub the PCSVF's view completion starts from: each root drawn
/// as 'append_stub_root' draws one.
fn stub_viewforest_of_roots (
  runtime : &RuntimeGeneration,
  drawing : DiffViewSource,
  roots   : &[ID],
) -> Result<ViewForest, Box<dyn Error>> {
  let mut viewforest : ViewForest = ViewForest::new ();
  let mut stub_editable_map : EditableMap = EditableMap::new ();
  for root in roots {
    append_stub_root (
      runtime, drawing, &mut viewforest, root, &mut stub_editable_map ) ?; }
  Ok (viewforest) }

/// Append 'root' to the viewforest as an editable stub, or, when the
/// after-graph lacks it, as a historical phantom, whose visit draws
/// its old content. In a PCSVF, a multiply-contained root gets a
/// folded container folder, which completion fills with its
/// containers in either graph and their containerward forest. The
/// order-only buffer, a flat list, draws none. Returns the root's
/// treeid.
fn append_stub_root (
  runtime           : &RuntimeGeneration,
  drawing           : DiffViewSource,
  viewforest        : &mut ViewForest,
  root              : &ID,
  stub_editable_map : &mut EditableMap,
) -> Result<NodeId, Box<dyn Error>> {
  let straddle : &GraphStraddle = drawing . straddle;
  let buffer_root : NodeId = viewforest . internal_root_skgid ();
  if straddle . after . get (root) . is_none () {
    let phantom : Viewnode = mk_historical_viewnode (straddle, root, None)
      . ok_or_else ( || format! (
        "diffView_buffer_viewforest: root {} is in neither graph", root )) ?;
    return Ok ( viewforest . as_internal_tree_mut () . get_mut (buffer_root)
                . ok_or ("diffView_buffer_viewforest: no buffer root") ?
                . append (phantom) . id () ); }
  let root_treeid : NodeId = append_plain_stub_root (
    viewforest, root, &runtime . graph, &runtime . config,
    stub_editable_map, None ) ?;
  if drawing . diffView_buffer_kind != DiffViewBufferKind::OrderOnlyChanges
    && union_containers (straddle, root) . len () > 1 {
    viewforest . as_internal_tree_mut () . get_mut (root_treeid)
      . ok_or ("diffView_buffer_viewforest: a stub root vanished") ?
      . append ( Viewnode {
        org_folded : true,
        .. viewnode_of ( ViewnodeKind::PartnerFolder (PartnerFolder::Container) ) } ); }
  Ok (root_treeid) }

/// The content origins in D, of the buffer's sets recomputed after
/// the save, that no viewnode in it draws, in ID order. The recomputed
/// R is deduplicated with the buffer's 'standing_roots' pinned as
/// kept, so that a change to a rootlike cycle's membership never adds
/// a second root for a cycle that already stands.
pub fn contentOrigins_missing_from_buffer (
  drawing        : DiffViewSource,
  viewforest     : &ViewForest,
  standing_roots : &BTreeSet<ID>,
) -> Vec<ID> {
  let sets : &PcsvfSets = drawing . sets ();
  let containers_of = |skgid : &ID| -> Vec<ID> {
    union_containers (drawing . straddle, skgid) };
  let drawn : HashSet<ID> = nodes_editable_in_diffView (viewforest);
  kept_content_origins (&sets . r, &sets . u, &containers_of, standing_roots)
    . into_iter ()
    . filter ( |content_origin| ! drawn . contains (content_origin)
                                && ! standing_roots . contains (content_origin) )
    . collect () }

/// Append 'content_origins' at the end of the buffer, each drawn as the first
/// drawing draws a root ('append_stub_root'), for view completion to
/// complete. Returns their treeids.
pub fn append_contentOrigins_to_buffer (
  runtime         : &RuntimeGeneration,
  drawing         : DiffViewSource,
  viewforest      : &mut ViewForest,
  content_origins : &[ID],
) -> Result<Vec<NodeId>, Box<dyn Error>> {
  let mut stub_editable_map : EditableMap = EditableMap::new ();
  content_origins . iter ()
    . map ( |content_origin| append_stub_root (
      runtime, drawing, viewforest, content_origin, &mut stub_editable_map ) )
    . collect () }

/// The IDs of the buffer's roots, whatever kind of vognode each is.
pub fn root_skgids_of_buffer (
  viewforest : &ViewForest,
) -> BTreeSet<ID> {
  viewforest . roots ()
    . filter_map ( |root| match &root . value () . kind {
      ViewnodeKind::Vognode (vognode) => vognode . skgid () . cloned (),
      _ => None, } )
    . collect () }

/// Before the view completion of a rerender that integrates the
/// changes not editable in the view ('rerender_view'): each node of the buffer's recomputed U that
/// has no editable occurrence becomes editable where it
/// stands, at its BFS-first content occurrence (a root, or a member
/// of a vognode's content), or if it has none at its BFS-first
/// folder occurrence, with its body from the live graph, as a freshly
/// drawn editable viewnode has it. Completion then draws its content
/// list. A node of U with no occurrence is reached through the node
/// above it on its walk, once that one is editable.
pub fn ensure_editableOccurrences_exist (
  drawing    : DiffViewSource,
  viewforest : &mut ViewForest,
  graph      : &InRustGraph,
  config     : &SkgConfig,
) -> Result<(), Box<dyn Error>> {
  let u : &BTreeSet<ID> = & drawing . sets () . u;
  let mut editable : HashSet<ID> = editable_occurrences (viewforest);
  let occurrences : Vec<NodeId> =
    content_occurrences_in_level_order (viewforest) . into_iter ()
    . chain ( folder_occurrences_in_level_order (viewforest) )
    . collect ();
  for treeid in occurrences {
    let skgid : ID = match &viewforest . get (treeid) . unwrap () . value () . kind {
      ViewnodeKind::Vognode (Vognode::Unrestricted (t)) => t . skgid . clone (),
      _ => continue, };
    if ! u . contains (&skgid) || editable . contains (&skgid) { continue; }
    let Some ((graphnode, _)) : Option<(Graphnode, Viewnode)> =
      graphnode_and_viewnode_from_skgid (graph, config, &skgid) ? else { continue; };
    if let ViewnodeKind::Vognode (Vognode::Unrestricted (t)) =
      &mut viewforest . get_mut (treeid) . unwrap () . value () . kind {
      t . editability = Editability::Editable {
        body : graphnode . body, edit_request : None }; }
    editable . insert (skgid); }
  Ok (( )) }

/// The viewforest's unrestricted vognodes in content position
/// ('in_content_position') that are roots or members there, in level
/// order. The walk passes through a historical phantom in content
/// position, to its old content, the removed members under it.
fn content_occurrences_in_level_order (
  viewforest : &ViewForest,
) -> Vec<NodeId> {
  let mut occurrences : Vec<NodeId> = Vec::new ();
  let mut queue : VecDeque<NodeRef<Viewnode>> =
    viewforest . roots () . collect ();
  while let Some (node) = queue . pop_front () {
    if ! in_content_position (node) { continue; }
    match &node . value () . kind {
      ViewnodeKind::Vognode (Vognode::Unrestricted (t)) => {
        let is_root_or_member : bool =
          t . affectsParent == AffectsParent::True
          || node . parent () . is_some_and ( |parent| match parent . value () . kind {
               ViewnodeKind::BufferRoot => true,
               ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Historical (_))) =>
                 is_unrestricted_removed_member ( node . value () ),
               _ => false, } );
        if ! is_root_or_member { continue; }
        occurrences . push ( node . id () ); },
      ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Historical (_))) => {},
      _ => continue, }
    queue . extend ( node . children () ); }
  occurrences }

/// The viewforest's unrestricted vognodes that are members of a
/// partner folder (affectsParent true), in level order.
fn folder_occurrences_in_level_order (
  viewforest : &ViewForest,
) -> Vec<NodeId> {
  let mut occurrences : Vec<NodeId> = Vec::new ();
  let mut queue : VecDeque<NodeRef<Viewnode>> =
    viewforest . roots () . collect ();
  while let Some (node) = queue . pop_front () {
    queue . extend ( node . children () );
    let ViewnodeKind::Vognode (Vognode::Unrestricted (t)) =
      &node . value () . kind else { continue; };
    let in_folder : bool =
      t . affectsParent == AffectsParent::True
      && matches! ( node . parent () . map ( |parent| &parent . value () . kind ),
                    Some (ViewnodeKind::PartnerFolder (_)) );
    if in_folder { occurrences . push ( node . id () ); }}
  occurrences }
