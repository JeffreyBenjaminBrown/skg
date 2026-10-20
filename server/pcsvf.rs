//! The PCSVF ("pruned change-spanning viewforest"): the viewforest
//! the "all changes" buffer of the total diff view draws
//! ('TODO/total-diff-view/02_plan.org', "The PCSVF").
//!
//! C is the set of changed graphnodes. R is the set of content
//! origins reached from C by walking containerward through the union
//! of the graph straddle; U is R, C and every node on those walks; D is R
//! minus the origins that another kept origin reaches containerward
//! inside U. The view unfolds contentward from each d in D: a node in
//! U is drawn editable with its whole content list, a member outside
//! U is a write-protected leaf. View completion does the drawing, its
//! 'diffView_source' set ('process_unrestrictedVognode_changes').
//!
//! The merges and skgrepo-changes buffers draw PCSVFs over subsets of
//! C ('pcsvf_sets_for_merges', 'pcsvf_sets_for_home_moves'). The
//! order-only buffer draws no PCSVF: it is a flat list of the nodes
//! whose only change is the order of a list.

use crate::dbs::in_rust_graph::relation_accessors::RelationRole;
use crate::graph_straddle::types::GraphStraddle;
use crate::prominence::climb_containerward_to_cycle_via;
use crate::readiness_store::ReadinessStore;
use crate::serve::handlers::readiness_requests::set_ready_facts_in_viewforest;
use crate::to_org::render::change_viewnodes::mk_historical_viewnode;
use crate::to_org::util::{
  EditableMap, build_node_branch_minus_content, graphnode_and_viewnode_from_skgid};
use crate::types::env::RuntimeGeneration;
use crate::dbs::in_rust_graph::InRustGraph;
use crate::types::misc::{ID, SkgConfig, SkgrepoName};
use crate::types::nodes::complete::Graphnode;
use crate::types::tree::forest::ViewForest;
use crate::types::viewnode::{
  AffectsParent, Editability, PartnerFolder, Phantom, Viewnode, ViewnodeKind,
  Vognode};
use crate::types::views_state::{DiffViewBufferKind, pids_from_viewforest};
use crate::update_buffer::{complete_viewforest_with_graph_straddle, finish_viewforest};

use ego_tree::{NodeId, NodeRef};
use std::collections::{BTreeSet, HashSet, VecDeque};
use std::error::Error;

/// The sets that define a PCSVF. See the module comment.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct PcsvfSets {
  pub c : BTreeSet<ID>,
  pub r : BTreeSet<ID>,
  pub u : BTreeSet<ID>,
  pub d : BTreeSet<ID>,
}

/// What view completion draws a buffer of the total diff view from:
/// the graph straddle, and the buffer, whose PCSVF sets it draws.
#[derive(Clone, Copy)]
pub struct DiffViewSource<'a> {
  pub straddle   : &'a GraphStraddle,
  pub tdv_buffer : DiffViewBufferKind,
}

impl<'a> DiffViewSource<'a> {
  pub fn sets (
    &self,
  ) -> &'a PcsvfSets {
    self . straddle . pcsvf_sets_of_buffer (self . tdv_buffer) }
}

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
    let (walk, origins) : (Vec<ID>, HashSet<ID>) =
      climb_containerward_to_cycle_via (&containers_of, changed);
    u . extend (walk);
    r . extend (origins); }
  u . extend ( r . iter () . cloned () );
  let d : BTreeSet<ID> =
    kept_origins (&r, &u, &containers_of, &BTreeSet::new ());
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

/// The sets of the skgrepo-changes buffer's PCSVF, over the nodes
/// whose home skgrepo changed.
pub fn pcsvf_sets_for_home_moves (
  straddle : &GraphStraddle,
) -> PcsvfSets {
  let c : BTreeSet<ID> =
    straddle . changed_pids () . out_changed . iter ()
    . filter ( |pid| straddle . node_changes (pid) . home . is_some () )
    . cloned ()
    . collect ();
  pcsvf_sets_over (straddle, c) }

/// The containers of a node in either graph of the straddle, sorted by ID.
pub fn union_containers (
  straddle : &GraphStraddle,
  skgid    : &ID,
) -> Vec<ID> {
  straddle . partners (skgid, RelationRole::CONTAINER) . into_iter ()
    . map ( |partner| partner . id )
    . collect () }

/// D: process R sorted by ID; discard each origin whose containerward
/// walk within U reaches an origin still kept. Of two origins on a
/// shared cycle, exactly one is kept. The 'pinned' nodes (a buffer's
/// standing roots, at a rerender) are kept before the rest is
/// processed, and never discarded; those not in R are not returned.
fn kept_origins (
  r             : &BTreeSet<ID>,
  u             : &BTreeSet<ID>,
  containers_of : &dyn Fn (&ID) -> Vec<ID>,
  pinned        : &BTreeSet<ID>,
) -> BTreeSet<ID> {
  let mut kept : BTreeSet<ID> = r . union (pinned) . cloned () . collect ();
  for origin in r . difference (pinned) {
    let reaches_a_kept_origin : bool = {
      let mut seen : HashSet<ID> = HashSet::from ([ origin . clone () ]);
      let mut frontier : Vec<ID> = vec! [ origin . clone () ];
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
    if reaches_a_kept_origin {
      kept . remove (origin); }}
  kept . intersection (r) . cloned () . collect () }

/// The buffer 'drawing' names, with roots 'roots' (for a PCSVF, its
/// D in order), drawn with readiness from 'store': the buffer text,
/// the pids to register, and the viewforest.
pub fn diffView_buffer_viewforest (
  runtime : &RuntimeGeneration,
  drawing : DiffViewSource,
  store   : &ReadinessStore,
  roots   : &[ID],
) -> Result<(String, Vec<ID>, ViewForest), Box<dyn Error>> {
  let mut viewforest : ViewForest =
    stub_viewforest_of_roots (runtime, drawing, roots) ?;
  complete_viewforest_with_graph_straddle (runtime, drawing, &mut viewforest) ?;
  set_diffView_facts_in_viewforest (
    &mut viewforest, drawing, store, &runtime . graph );
  let text : String = finish_viewforest (
    &mut viewforest, &runtime . graph, &runtime . config, None ) ?;
  let pids : Vec<ID> =
    pids_from_viewforest (&viewforest) . into_iter () . collect ();
  Ok (( text, pids, viewforest )) }

/// What a buffer of the total diff view gets after view completion:
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
  if drawing . tdv_buffer == DiffViewBufferKind::SkgrepoChanges {
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
  let facts : Vec<(NodeId, Option<(SkgrepoName, SkgrepoName)>, Option<String>)> =
    viewforest . nodes ()
    . filter_map ( |node| {
      let ViewnodeKind::Vognode (Vognode::Unrestricted (t)) =
        &node . value () . kind else { return None; };
      let home_skgrepo_move : Option<(SkgrepoName, SkgrepoName)> =
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
) -> Option<(usize, (SkgrepoName, SkgrepoName))> {
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

/// The nodes a buffer of the total diff view makes editable: those it
/// has an editable occurrence of, wherever it sits, and the deleted
/// nodes it draws as historical phantoms in content position. A
/// historical phantom in content position counts, since its old
/// content is editable nowhere and shown there. The change-forest
/// leaves these nodes record are the changes editable in the buffer.
pub fn nodes_editable_in_diffView (
  viewforest : &ViewForest,
) -> HashSet<ID> {
  viewforest . nodes () . filter_map ( |node| match &node . value () . kind {
    ViewnodeKind::Vognode (Vognode::Unrestricted (t))
      if ! t . is_writeProtected () => Some ( t . skgid . clone () ),
    ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Historical (h)))
      if historical_phantom_in_content_position (node) =>
      Some ( h . skgid . clone () ),
    _ => None, } )
    . collect () }

/// Whether a historical phantom stands where content does: under the
/// buffer root or an unrestricted vognode, or in the old content of a
/// historical phantom that does. Not in a partner folder.
fn historical_phantom_in_content_position (
  node : NodeRef<Viewnode>,
) -> bool {
  match node . parent () {
    Some (parent) => match &parent . value () . kind {
      ViewnodeKind::BufferRoot
        | ViewnodeKind::Vognode (Vognode::Unrestricted (_)) => true,
      ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Historical (_))) =>
        historical_phantom_in_content_position (parent),
      _ => false, },
    None => false, }}

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
  let root_treeid : NodeId = build_node_branch_minus_content (
    Some (( viewforest . as_internal_tree_mut (), buffer_root )),
    root, &runtime . graph, &runtime . config, stub_editable_map,
    None ) ?;
  if drawing . tdv_buffer != DiffViewBufferKind::OrderOnlyChanges
    && union_containers (straddle, root) . len () > 1 {
    viewforest . as_internal_tree_mut () . get_mut (root_treeid)
      . ok_or ("diffView_buffer_viewforest: a stub root vanished") ?
      . append ( Viewnode {
        focused : false, folded : true, body_folded : false,
        kind : ViewnodeKind::PartnerFolder (PartnerFolder::Container) } ); }
  Ok (root_treeid) }

/// The origins of the buffer's fresh D that no viewnode in it draws,
/// in ID order. The fresh R is deduplicated with the buffer's
/// 'standing_roots' pinned as kept, so that a change to a rootlike
/// cycle's membership never adds a second root for a cycle that
/// already stands.
pub fn contentOrigins_missing_from_buffer (
  drawing        : DiffViewSource,
  viewforest     : &ViewForest,
  standing_roots : &BTreeSet<ID>,
) -> Vec<ID> {
  let sets : &PcsvfSets = drawing . sets ();
  let containers_of = |skgid : &ID| -> Vec<ID> {
    union_containers (drawing . straddle, skgid) };
  let drawn : HashSet<ID> = nodes_editable_in_diffView (viewforest);
  kept_origins (&sets . r, &sets . u, &containers_of, standing_roots)
    . into_iter ()
    . filter ( |origin| ! drawn . contains (origin)
                        && ! standing_roots . contains (origin) )
    . collect () }

/// Append 'origins' at the end of the buffer, each drawn as the first
/// drawing draws a root ('append_stub_root'), for view completion to
/// complete. Returns their treeids.
pub fn append_contentOrigins_to_buffer (
  runtime    : &RuntimeGeneration,
  drawing    : DiffViewSource,
  viewforest : &mut ViewForest,
  origins    : &[ID],
) -> Result<Vec<NodeId>, Box<dyn Error>> {
  let mut stub_editable_map : EditableMap = EditableMap::new ();
  origins . iter ()
    . map ( |origin| append_stub_root (
      runtime, drawing, viewforest, origin, &mut stub_editable_map ) )
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

/// Before the view completion of a rerender that shows the unshown
/// changes ('rerender_view'): each node of the buffer's fresh U that
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
  let mut editable : HashSet<ID> =
    viewforest . nodes () . filter_map ( |node| match &node . value () . kind {
      ViewnodeKind::Vognode (Vognode::Unrestricted (t))
        if ! t . is_writeProtected () => Some ( t . skgid . clone () ),
      _ => None, } )
    . collect ();
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

/// The viewforest's unrestricted vognodes in content position (a root,
/// or a member of an unrestricted vognode's content), in level order.
fn content_occurrences_in_level_order (
  viewforest : &ViewForest,
) -> Vec<NodeId> {
  let mut occurrences : Vec<NodeId> = Vec::new ();
  let mut queue : VecDeque<NodeRef<Viewnode>> =
    viewforest . roots () . collect ();
  while let Some (node) = queue . pop_front () {
    let ViewnodeKind::Vognode (Vognode::Unrestricted (t)) =
      &node . value () . kind else { continue; };
    let in_content_position : bool =
      match node . parent () . map ( |parent| &parent . value () . kind ) {
        Some (ViewnodeKind::BufferRoot) => true,
        Some (ViewnodeKind::Vognode (Vognode::Unrestricted (_))) =>
          t . affectsParent == AffectsParent::True,
        _ => false, };
    if ! in_content_position { continue; }
    occurrences . push ( node . id () );
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
