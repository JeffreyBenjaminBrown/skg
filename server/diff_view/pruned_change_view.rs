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
use crate::dbs::in_rust_graph::InRustGraph;
use crate::types::misc::{ID, SkgrepoChange};
use crate::types::tree::forest::{ViewForest, editable_gnids, in_content_position};
use crate::types::viewnode::{ChangeFacts, Viewnode, ViewnodeKind, Vognode};
use crate::types::views_state::DiffViewBufferKind;

use ego_tree::{NodeId, NodeRef};
use std::collections::{BTreeSet, HashMap, HashSet};
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
pub(crate) fn kept_content_origins (
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
  editable_gnids (viewforest) . into_iter ()
    . chain (historical_in_content_position)
    . collect () }