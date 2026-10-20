//! Readiness requests, readiness facts and a buffer's readiness
//! summary ('docs/diff-view.org', "Readiness"), the places where the
//! readiness store meets a viewforest. The first two rest on one
//! rule, 'leaves_named_at': which change-forest leaves a headline
//! names.
//!
//! A save consumes the readiness requests of the buffer it saves, in
//! any buffer ('consume_readiness_requests'): after the graph edits
//! succeed, each '(readyRequest ...)' or '(unreadyRequest ...)' sets or
//! clears the marks of the leaves it names, at their fingerprints in
//! the graph straddle brought up to date with the save. A request that
//! names no current change is dropped with a warning. A save that stops
//! for a confirmation consumes nothing.
//!
//! After view completion of a buffer of the diff view, the
//! '(ready ...)' display facts are set from the store
//! ('set_ready_facts_in_viewforest').
//!
//! A save of a buffer of the diff view, and the "integrate unshown
//! changes" request, answer with the readiness summary of the buffer
//! they rerendered ('buffer_readiness_summary_for').

use crate::diff_view::change_leaves::{
  BufferReadinessSummary, ChangeLeaves, LeafKey, NodeAspect, RelationshipAspect};
use crate::diff_view::pcsvf::nodes_editable_in_diffView;
use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::in_rust_graph::relation_accessors::{
  NodeRelation, RelationRole, RelationshipKey};
use crate::graph_straddle::build::current_graph_straddle_in_slot;
use crate::graph_straddle::types::GraphStraddle;
use crate::diff_view::readiness_store::ReadinessStore;
use crate::types::env::{RuntimeGeneration, SkgEnv};
use crate::types::misc::ID;
use crate::types::tree::forest::ViewForest;
use crate::types::viewnode::{
  ChangesetMarkerKind, Phantom, Property, DiffObject,
  ReadinessRequest, Viewnode, ViewnodeKind, Vognode};

use ego_tree::{NodeId, NodeRef};
use std::collections::BTreeSet;
use std::sync::{Arc, MutexGuard};

/// After a save's graph edits succeeded: apply the saved buffer's
/// readiness requests to the store, against the graph straddle in
/// 'graph_straddle_slot' brought up to date with 'runtime', and write the
/// store back. Returns the warnings to show. Builds no straddle when the
/// buffer carries no request.
pub fn consume_readiness_requests (
  viewforest          : &ViewForest,
  env                 : &SkgEnv,
  runtime             : &RuntimeGeneration,
  graph_straddle_slot : &mut Option<Arc<GraphStraddle>>,
) -> Vec<String> {
  let requests : Vec<RequestedReadiness> =
    readiness_requests_in_viewforest (viewforest, &runtime . graph);
  if requests . is_empty () { return Vec::new (); }
  let straddle : Arc<GraphStraddle> =
    match current_graph_straddle_in_slot (
      graph_straddle_slot, &runtime . config, runtime . graph . clone () ) {
      Ok (straddle) => straddle,
      Err (e) => return vec! [ format! (
        "Readiness requests were not applied: {}", e ) ], };
  let mut store : MutexGuard<ReadinessStore> =
    env . runtime . lock_readiness_store ();
  let mut warnings : Vec<String> =
    apply_readiness_requests (&requests, straddle . change_leaves (), &mut store);
  if let Err (e) = store . save (&runtime . config . data_root) {
    warnings . push ( format! (
      "The readiness marks were applied but not written to disk: {}", e )); }
  warnings }

/// One readiness request, read from the saved buffer.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct RequestedReadiness {
  pub ready    : bool,
  pub named    : NamedLeaves,
  /// The headline, for warnings.
  pub headline : String,
}

/// Every readiness request in the saved buffer, with the leaves it
/// names. IDs resolve through 'graph' (the graph after the save),
/// falling back to the ID as written, except a historical phantom's,
/// which names a node of the before-graph.
pub fn readiness_requests_in_viewforest (
  viewforest : &ViewForest,
  graph      : &InRustGraph,
) -> Vec<RequestedReadiness> {
  let pid_of = |skgid : &ID| -> ID {
    graph . pid_of (skgid) . unwrap_or_else ( || skgid . clone () ) };
  viewforest . root () . descendants ()
    . filter_map ( |node| {
      let request : &ReadinessRequest = node . value () . kind . readiness_request () ?;
      let named : NamedLeaves = leaves_named_at (node, &request . objects, &pid_of) ?;
      Some ( RequestedReadiness {
        ready    : request . ready,
        named,
        headline : node . value () . title () . to_string (), } ) } )
    . collect () }

/// What a request on a viewnode names.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct NamedLeaves {
  /// The leaves named outright.
  pub leaves                   : Vec<LeafKey>,
  /// The nodes whose leaves an 'all' request names.
  pub all_of                   : Vec<ID>,
  /// The recorders whose content relationships' leaves of an aspect
  /// a changeset marker's request names.
  pub content_relationships_of : Vec<(ID, RelationshipAspect)>,
}

/// The leaves that a request naming the diff objects 'objects' at
/// 'node' names. No objects on a vognode means both of its objects
/// (only 'rel' on an unknown phantom); a property leaf has one object.
/// None for a viewnode that names no leaf.
fn leaves_named_at (
  node    : NodeRef<Viewnode>,
  objects : &BTreeSet<DiffObject>,
  pid_of  : &dyn Fn (&ID) -> ID,
) -> Option<NamedLeaves> {
  let mut leaves : Vec<LeafKey> = Vec::new ();
  let mut all_of : Vec<ID> = Vec::new ();
  let mut content_relationships_of : Vec<(ID, RelationshipAspect)> =
    Vec::new ();
  match &node . value () . kind {
    ViewnodeKind::Vognode (vognode) => {
      let pid : ID = vognode_leaf_pid (node . value (), pid_of) ?;
      let is_unknown : bool =
        matches! (vognode, Vognode::Phantom (Phantom::Unknown (_)));
      let objects : Vec<DiffObject> =
        if ! objects . is_empty () {
          objects . iter () . copied () . collect ()
        } else if is_unknown { vec! [ DiffObject::Rel ] }
        else { vec! [ DiffObject::Node, DiffObject::Rel ] };
      for object in objects {
        match object {
          DiffObject::Node if ! is_unknown =>
            leaves . push ( LeafKey::Node {
              pid : pid . clone (), aspect : NodeAspect::Existence } ),
          DiffObject::Node => {},
          DiffObject::Rel =>
            leaves . extend ( relationship_to_viewparent (
              node, &pid, pid_of, RelationshipAspect::Existence )),
          DiffObject::All =>
            all_of . push (pid . clone ()), }} },
    ViewnodeKind::Property (property) => {
      // The change-forest leaf of an entry in a folder belongs to the
      // folder's viewparent; the diff view's properties hang
      // under their node.
      let owner_of_folder = || -> Option<ID> {
        Some ( pid_of ( &grandparent_pid (node) ? )) };
      let parent : NodeRef<Viewnode> = node . parent () ?;
      let parent_pid = || -> Option<ID> {
        vognode_leaf_pid (parent . value (), pid_of) };
      match property {
        Property::Alias { text, .. } | Property::RemovedAlias { text, .. } =>
          leaves . push ( LeafKey::Node {
            pid : owner_of_folder () ?,
            aspect : NodeAspect::Alias (text . clone ()) } ),
        Property::ID { skgid, .. } | Property::RemovedID { skgid, .. } =>
          leaves . push ( LeafKey::Node {
            pid : owner_of_folder () ?,
            aspect : NodeAspect::ExtraId (skgid . clone ()) } ),
        Property::Flag { flag, .. } =>
          leaves . push ( LeafKey::Node {
            pid : owner_of_folder () ?,
            aspect : NodeAspect::Flag (*flag) } ),
        Property::OldText { .. } =>
          leaves . push ( LeafKey::Node {
            pid : parent_pid () ?, aspect : NodeAspect::Text } ),
        Property::HomeSkgrepoMove { .. } =>
          leaves . push ( LeafKey::Node {
            pid : parent_pid () ?, aspect : NodeAspect::HomeSkgrepo } ),
        Property::RelRepoMove { .. } =>
          leaves . extend ( relationship_to_viewparent (
            parent, &parent_pid () ?, pid_of,
            RelationshipAspect::RelRepo )),
        Property::ChangesetMarker { kind, .. } => match kind {
          ChangesetMarkerKind::Content =>
            content_relationships_of . push ((
              parent_pid () ?, RelationshipAspect::Existence )),
          ChangesetMarkerKind::RelRepo =>
            content_relationships_of . push ((
              parent_pid () ?, RelationshipAspect::RelRepo )),
          ChangesetMarkerKind::OrderOnly => {}, }, // always ready
        Property::BodyDiff { .. } =>
          return None, }},
    _ => return None, }
  Some ( NamedLeaves { leaves, all_of, content_relationships_of } ) }

/// The leaves of 'forest' that 'named' names.
fn keys_in_forest (
  named  : &NamedLeaves,
  forest : &ChangeLeaves,
) -> Vec<LeafKey> {
  named . leaves . iter () . cloned ()
    . chain ( named . all_of . iter () . flat_map ( |pid|
      forest . leaves_under (pid) . iter ()
        . map ( |leaf| leaf . key . clone () )) )
    . chain ( named . content_relationships_of . iter ()
      . flat_map ( |(pid, aspect)|
        forest . leaves_under (pid) . iter ()
          . map ( |leaf| leaf . key . clone () )
          . filter ( move |key| matches! ( key,
            LeafKey::Relationship {
              relationship : RelationshipKey {
                relation : NodeRelation::Contains, recorder, .. },
              aspect : leaf_aspect }
            if recorder == pid && leaf_aspect == aspect )) ))
    . filter ( |key| forest . leaf (key) . is_some () )
    . collect () }

/// Set the '(ready ...)' display fact of every viewnode that carries
/// one, from 'store': an object is ready when it names some leaf of
/// 'forest' and every leaf it names is marked at its current
/// fingerprint. The order-only marker is always ready.
pub fn set_ready_facts_in_viewforest (
  viewforest : &mut ViewForest,
  forest     : &ChangeLeaves,
  store      : &ReadinessStore,
  graph      : &InRustGraph,
) {
  let pid_of = |skgid : &ID| -> ID {
    graph . pid_of (skgid) . unwrap_or_else ( || skgid . clone () ) };
  let object_is_ready = |node : NodeRef<Viewnode>, objects : &[DiffObject]| -> bool {
    let Some (named) = leaves_named_at (
      node, &objects . iter () . copied () . collect (), &pid_of ) else {
        return false; };
    let keys : Vec<LeafKey> = keys_in_forest (&named, forest);
    ! keys . is_empty () && forest . readiness_of_leaves (store, &keys) };
  let ready_facts : Vec<(NodeId, Option<BTreeSet<DiffObject>>)> =
    viewforest . root () . descendants ()
    . filter_map ( |node| {
      let ready : Option<BTreeSet<DiffObject>> =
        match &node . value () . kind {
          ViewnodeKind::Vognode (
            Vognode::Unrestricted (_)
            | Vognode::Phantom (Phantom::Historical (_)) ) => {
            let objects : BTreeSet<DiffObject> =
              [ DiffObject::Node, DiffObject::Rel ] . into_iter ()
              . filter ( |object| object_is_ready (node, &[ *object ]) )
              . collect ();
            ( ! objects . is_empty () ) . then_some (objects) },
          ViewnodeKind::Vognode (
            Vognode::Phantom (Phantom::Deleted (_) | Phantom::Unknown (_)) ) =>
            object_is_ready (node, &[]) . then (BTreeSet::new),
          ViewnodeKind::Property ( Property::ChangesetMarker {
            kind : ChangesetMarkerKind::OrderOnly, .. } ) =>
            Some (BTreeSet::new ()),
          kind @ ViewnodeKind::Property (_)
            if kind . change_facts () . is_some () =>
            object_is_ready (node, &[]) . then (BTreeSet::new),
          _ => return None, };
      Some (( node . id (), ready )) } )
    . collect ();
  for (treeid, ready) in ready_facts {
    if let Some (change_facts) =
      viewforest . get_mut (treeid) . unwrap () . value () . kind . change_facts_mut () {
      change_facts . ready = ready; }}}

/// The ID a vognode's leaves are keyed by: a historical phantom's as
/// written (a node of the before-graph), any other's resolved by
/// 'pid_of'. None for a restricted vognode or a non-vognode.
fn vognode_leaf_pid (
  viewnode : &Viewnode,
  pid_of   : &dyn Fn (&ID) -> ID,
) -> Option<ID> {
  match &viewnode . kind {
    ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Historical (h))) =>
      Some ( h . skgid . clone () ),
    ViewnodeKind::Vognode (vognode) =>
      Some ( pid_of ( vognode . skgid () ? )),
    _ => None, } }

/// Set or clear the marks the requests name, at their fingerprints
/// in 'forest'. Returns a warning for each request that names no
/// current change.
pub fn apply_readiness_requests (
  requests : &[RequestedReadiness],
  forest   : &ChangeLeaves,
  store    : &mut ReadinessStore,
) -> Vec<String> {
  let mut warnings : Vec<String> = Vec::new ();
  for request in requests {
    let keys : Vec<LeafKey> = keys_in_forest (&request . named, forest);
    if keys . is_empty () {
      warnings . push ( format! (
        "The readiness request on \"{}\" names no current change; it was dropped.",
        request . headline ));
      continue; }
    for key in keys {
      if request . ready {
        let fingerprint : u64 = forest . leaf_fingerprint (&key)
          . expect ("filtered to leaves of the forest");
        store . mark (key, fingerprint);
      } else { store . unmark (&key); }} }
  warnings }

/// The relationship by which a vognode is a member here: its
/// viewparent's content, or the relation of the partner folder it is
/// in, between it and the folder's viewparent. None at a root, or
/// under anything else.
fn relationship_to_viewparent (
  node   : NodeRef<Viewnode>,
  pid    : &ID,
  pid_of : &dyn Fn (&ID) -> ID,
  aspect : RelationshipAspect,
) -> Option<LeafKey> {
  let parent : NodeRef<Viewnode> = node . parent () ?;
  let key = |relation : NodeRelation, recorder : ID, member : ID| -> LeafKey {
    LeafKey::Relationship {
      relationship : RelationshipKey { relation, recorder, member },
      aspect } };
  match &parent . value () . kind {
    ViewnodeKind::Vognode (_) =>
      Some ( key ( NodeRelation::Contains,
                   vognode_leaf_pid (parent . value (), pid_of) ?,
                   pid . clone () )),
    ViewnodeKind::PartnerFolder (folder) => {
      let role : RelationRole = folder . relation_member_role () ?;
      let folder_viewparent : ID = vognode_leaf_pid (
        parent . parent () ? . value (), pid_of ) ?;
      Some ( if role . is_first_role () { // the member records it
               key ( role . relation, pid . clone (), folder_viewparent )
             } else {
               key ( role . relation, folder_viewparent, pid . clone () ) } ) },
    _ => None, } }

/// The ID of the vognode two levels up: the node that owns a
/// property leaf's folder.
fn grandparent_pid (
  node : NodeRef<Viewnode>,
) -> Option<ID> {
  match &node . parent () ? . parent () ? . value () . kind {
    ViewnodeKind::Vognode (vognode) => vognode . skgid () . cloned (),
    _ => None, } }

/// The readiness summary of a buffer of the diff view, from
/// 'straddle' and the marks in the store: the changeset's counts, and
/// how many leaves are recorded by a node that 'viewforest' does not
/// make editable ('nodes_editable_in_diffView').
pub fn buffer_readiness_summary_for (
  env        : &SkgEnv,
  straddle   : &GraphStraddle,
  viewforest : &ViewForest,
) -> BufferReadinessSummary {
  let store : MutexGuard<ReadinessStore> =
    env . runtime . lock_readiness_store ();
  straddle . change_leaves () . buffer_readiness_summary (
    &store, &nodes_editable_in_diffView (viewforest) ) }
