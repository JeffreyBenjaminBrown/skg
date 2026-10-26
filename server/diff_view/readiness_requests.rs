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
//! '(ready ...)' display facts are set from the store, and the
//! '(leaves ...)' facts, which tell the clients what a request on a
//! headline may name, from the same rule
//! ('set_ready_facts_in_viewforest').
//!
//! A save of a buffer of the diff view, and the "integrate unshown
//! changes" request, answer with the readiness summary of the buffer
//! they rerendered ('buffer_readiness_summary_for').

use crate::to_org::extend::containerward_chain::is_in_containerward_chain;
use crate::diff_view::change_leaves::{
  BufferReadinessSummary, ChangeLeaves, LeafKey, NodeAspect, RelationshipAspect};
use crate::diff_view::pruned_change_view::nodes_editable_in_diffView;
use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::in_rust_graph::relation_accessors::{
  NodeRelation, RelationRole, RelationshipKey};
use crate::graph_straddle::build::current_graph_straddle_in_slot;
use crate::graph_straddle::types::GraphStraddle;
use crate::diff_view::readiness_store::ReadinessStore;
use crate::types::env::{RuntimeGeneration, SkgEnv};
use crate::types::errors::{HeadlineName, with_org_subheadlines};
use crate::types::misc::ID;
use crate::types::tree::forest::ViewForest;
use crate::types::viewnode::{
  ChangeFacts, ChangesetMarkerKind, LeavesFact, Phantom, PropertyMember, ChangeViewnode, DiffObject,
  ReadinessRequest, Viewnode, ViewnodeKind, Vognode};

use ego_tree::{NodeId, NodeMut, NodeRef};
use std::collections::{BTreeSet, HashSet};
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
  /// The headline carrying the request, which a warning names.
  pub headline : HeadlineName,
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
      let request : &ReadinessRequest = node . value () . readiness_request . as_ref () ?;
      let named : NamedLeaves = leaves_named_at (node, &request . objects, &pid_of) ?;
      Some ( RequestedReadiness {
        ready    : request . ready,
        named,
        headline : node . value () . headline_name (), } ) } )
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
/// None for a viewnode that names no leaf, among them a node of the
/// containerward chain, which shows no change.
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
    ViewnodeKind::Vognode (_) if is_in_containerward_chain (node) =>
      return None,
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
    ViewnodeKind::PropertyMember (member) => {
      // The change-forest leaf of an entry in a folder belongs to the
      // folder's viewparent.
      let pid_of_folders_viewparent : ID =
        pid_of ( &grandparent_pid (node) ? );
      match member {
        PropertyMember::Alias { text, .. } =>
          leaves . push ( LeafKey::Node {
            pid : pid_of_folders_viewparent,
            aspect : NodeAspect::Alias (text . clone ()) } ),
        PropertyMember::ID { skgid, .. } =>
          leaves . push ( LeafKey::Node {
            pid : pid_of_folders_viewparent,
            aspect : NodeAspect::ExtraId (skgid . clone ()) } ),
        PropertyMember::Flag { flag, .. } =>
          leaves . push ( LeafKey::Node {
            pid : pid_of_folders_viewparent,
            aspect : NodeAspect::Flag (*flag) } ), }},
    ViewnodeKind::ChangeViewnode (change) => {
      // A change viewnode hangs under its node.
      let parent : NodeRef<Viewnode> = node . parent () ?;
      let parent_pid = || -> Option<ID> {
        vognode_leaf_pid (parent . value (), pid_of) };
      match change {
        ChangeViewnode::OldText { .. } =>
          leaves . push ( LeafKey::Node {
            pid : parent_pid () ?, aspect : NodeAspect::Text } ),
        ChangeViewnode::HomeSkgrepoMove { .. } =>
          leaves . push ( LeafKey::Node {
            pid : parent_pid () ?, aspect : NodeAspect::HomeSkgrepo } ),
        ChangeViewnode::RelRepoMove { .. } =>
          leaves . extend ( relationship_to_viewparent (
            parent, &parent_pid () ?, pid_of,
            RelationshipAspect::RelRepo )),
        ChangeViewnode::ChangesetMarker { kind, .. } => match kind {
          ChangesetMarkerKind::Content =>
            content_relationships_of . push ((
              parent_pid () ?, RelationshipAspect::Existence )),
          ChangesetMarkerKind::RelRepo =>
            content_relationships_of . push ((
              parent_pid () ?, RelationshipAspect::RelRepo )),
          ChangesetMarkerKind::OrderOnly => {}, }, // always ready
        ChangeViewnode::BodyDiff { .. } =>
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

/// Set the '(ready ...)' and '(leaves ...)' display facts of every
/// viewnode that carries them. An object is ready when it names some
/// leaf of 'forest' and every leaf it names is marked at its current
/// fingerprint ('store'); the order-only marker is always ready. The
/// '(leaves ...)' fact is 'leaves_fact_at'. On an unrestricted vognode
/// whose leaves fact lists 'all', '(ready ...)' names 'all' when every
/// leaf its node records is marked. A ready changeset marker's '(ready
/// ...)' names 'all'; any other ready viewnode with a single object
/// writes it bare, '(ready)'.
pub fn set_ready_facts_in_viewforest (
  viewforest : &mut ViewForest,
  forest     : &ChangeLeaves,
  store      : &ReadinessStore,
  graph      : &InRustGraph,
) {
  let pid_of = |skgid : &ID| -> ID {
    graph . pid_of (skgid) . unwrap_or_else ( || skgid . clone () ) };
  let object_is_ready = |node : NodeRef<Viewnode>, objects : &[DiffObject]| -> bool {
    let keys : Vec<LeafKey> = keys_named_at (node, objects, forest, &pid_of);
    ! keys . is_empty () && forest . readiness_of_leaves (store, &keys) };
  let facts : Vec<(NodeId, Option<BTreeSet<DiffObject>>, Option<LeavesFact>)> =
    viewforest . root () . descendants ()
    . filter_map ( |node| {
      let leaves : Option<LeavesFact> = leaves_fact_at (node, forest, &pid_of);
      let ready : Option<BTreeSet<DiffObject>> =
        match &node . value () . kind {
          ViewnodeKind::Vognode (
            vognode @ ( Vognode::Unrestricted (_)
                        | Vognode::Phantom (Phantom::Historical (_)) )) => {
            let all_is_listed : bool =
              matches! ( vognode, Vognode::Unrestricted (_) )
              && leaves . as_ref () . is_some_and ( |leaves|
                leaves . objects . contains (&DiffObject::All) );
            let objects : BTreeSet<DiffObject> =
              [ DiffObject::Node, DiffObject::Rel ] . into_iter ()
              . filter ( |object| object_is_ready (node, &[ *object ]) )
              . chain ( ( all_is_listed
                          && object_is_ready (node, &[ DiffObject::All ]) )
                        . then_some (DiffObject::All) )
              . collect ();
            ( ! objects . is_empty () ) . then_some (objects) },
          ViewnodeKind::Vognode (
            Vognode::Phantom (Phantom::Deleted (_) | Phantom::Unknown (_)) ) =>
            object_is_ready (node, &[]) . then (BTreeSet::new),
          ViewnodeKind::ChangeViewnode ( ChangeViewnode::ChangesetMarker { kind } ) =>
            ( *kind == ChangesetMarkerKind::OrderOnly
              || object_is_ready (node, &[]) )
            . then ( || BTreeSet::from ([ DiffObject::All ]) ),
          ViewnodeKind::PropertyMember (_) =>
            object_is_ready (node, &[]) . then (BTreeSet::new),
          ViewnodeKind::ChangeViewnode (change)
            if ! matches! (change, ChangeViewnode::BodyDiff { .. }) =>
            object_is_ready (node, &[]) . then (BTreeSet::new),
          _ => return None, };
      Some (( node . id (), ready, leaves )) } )
    . collect ();
  for (treeid, ready, leaves) in facts {
    let mut node_mut : NodeMut<Viewnode> = viewforest . get_mut (treeid) . unwrap ();
    let change_facts : &mut ChangeFacts = &mut node_mut . value () . change_facts;
    change_facts . ready  = ready;
    change_facts . leaves = leaves; }}

/// The '(leaves N OBJECT...)' fact of 'node', from 'leaves_named_at':
/// on an unrestricted vognode whose node's other properties changed
/// (modified), 'all', beside 'rel' when that names a leaf of 'forest'; on any
/// other unrestricted vognode or a historical phantom, each of 'node'
/// and 'rel' that names a leaf, or else 'all' if its node records any
/// leaf; on anything else, 'all' when a bare request names a commit
/// readiness aggregate (a changeset marker), else no object. N is how many
/// distinct leaves of 'forest' a request naming those objects names.
/// None when that is none.
fn leaves_fact_at (
  node   : NodeRef<Viewnode>,
  forest : &ChangeLeaves,
  pid_of : &dyn Fn (&ID) -> ID,
) -> Option<LeavesFact> {
  let names_some_leaf = |objects : &[DiffObject]| -> bool {
    ! keys_named_at (node, objects, forest, pid_of) . is_empty () };
  let objects : BTreeSet<DiffObject> = match &node . value () . kind {
    ViewnodeKind::Vognode (Vognode::Unrestricted (_))
      if node . value () . change_facts . change . properties =>
      [ DiffObject::Rel ] . into_iter ()
      . filter ( |object| names_some_leaf (&[ *object ]) )
      . chain ([ DiffObject::All ])
      . collect (),
    ViewnodeKind::Vognode (
      Vognode::Unrestricted (_)
      | Vognode::Phantom (Phantom::Historical (_)) ) => {
      let one_leaf_each : BTreeSet<DiffObject> =
        [ DiffObject::Node, DiffObject::Rel ] . into_iter ()
        . filter ( |object| names_some_leaf (&[ *object ]) )
        . collect ();
      if one_leaf_each . is_empty () { BTreeSet::from ([ DiffObject::All ]) }
      else { one_leaf_each } },
    _ => {
      let named : NamedLeaves =
        leaves_named_at (node, &BTreeSet::new (), pid_of) ?;
      if named . all_of . is_empty ()
        && named . content_relationships_of . is_empty ()
      { BTreeSet::new () }
      else { BTreeSet::from ([ DiffObject::All ]) } }, };
  let count : usize = keys_named_at (
    node, &objects . iter () . copied () . collect::<Vec<_>> (),
    forest, pid_of ) . into_iter () . collect::<HashSet<LeafKey>> () . len ();
  ( count > 0 ) . then_some ( LeavesFact { count, objects } ) }

/// The leaves of 'forest' that a request naming 'objects' at 'node'
/// names. Several objects can name one leaf twice ('node' and 'all'
/// both name the existence leaf).
fn keys_named_at (
  node    : NodeRef<Viewnode>,
  objects : &[DiffObject],
  forest  : &ChangeLeaves,
  pid_of  : &dyn Fn (&ID) -> ID,
) -> Vec<LeafKey> {
  leaves_named_at ( node, &objects . iter () . copied () . collect (), pid_of )
    . map ( |named| keys_in_forest (&named, forest) )
    . unwrap_or_default () }

/// The ID a vognode's leaves are keyed by: a historical phantom's as
/// written (a node of the before-graph), any other's resolved by
/// 'pid_of'. None for a non-vognode.
pub(crate) fn vognode_leaf_pid (
  viewnode : &Viewnode,
  pid_of   : &dyn Fn (&ID) -> ID,
) -> Option<ID> {
  match &viewnode . kind {
    ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Historical (h))) =>
      Some ( h . skgid . clone () ),
    ViewnodeKind::Vognode (vognode) =>
      Some ( pid_of ( vognode . skgid () )),
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
      warnings . push ( with_org_subheadlines (
        "A readiness request names no current change; it was dropped.",
        &[ request . headline . clone () ] ));
      continue; }
    for key in keys {
      if request . ready {
        let fingerprint : u64 = forest . leaf_fingerprint (&key)
          . expect ("filtered to leaves of the forest");
        store . mark (key, fingerprint);
      } else { store . unmark (&key); }} }
  warnings }

/// The relationship by which a vognode is shown here, read from where
/// it stands: under a vognode, its viewparent's content; under a
/// partner folder, the relation of the folder, between it and the
/// folder's viewparent. None for a node of the containerward chain
/// ('is_in_containerward_chain'), which stands for no relationship
/// the diff view tracks, at a root, or under anything else.
pub(crate) fn relationship_to_viewparent (
  node   : NodeRef<Viewnode>,
  pid    : &ID,
  pid_of : &dyn Fn (&ID) -> ID,
  aspect : RelationshipAspect,
) -> Option<LeafKey> {
  let parent : NodeRef<Viewnode> = node . parent () ?;
  let key_for_role = |role : RelationRole, partner : ID| -> LeafKey {
    let (recorder, member) : (ID, ID) =
      if role . is_first_role () { (pid . clone (), partner) } // the node records it
      else { (partner, pid . clone ()) };
    LeafKey::Relationship {
      relationship : RelationshipKey { relation : role . relation, recorder, member },
      aspect } };
  match &parent . value () . kind {
    ViewnodeKind::Vognode (_) if is_in_containerward_chain (node) => None,
    ViewnodeKind::Vognode (_) => {
      let viewparent : ID = vognode_leaf_pid (parent . value (), pid_of) ?;
      Some ( key_for_role (RelationRole::CONTENT, viewparent) ) },
    ViewnodeKind::Folder (folder) => { // a property folder has no role
      let role : RelationRole = folder . relation_member_role () ?;
      let folder_viewparent : ID = vognode_leaf_pid (
        parent . parent () ? . value (), pid_of ) ?;
      Some ( key_for_role (role, folder_viewparent) ) },
    _ => None, } }

/// The ID of the vognode two levels up: the node that owns a
/// property leaf's folder.
fn grandparent_pid (
  node : NodeRef<Viewnode>,
) -> Option<ID> {
  match &node . parent () ? . parent () ? . value () . kind {
    ViewnodeKind::Vognode (vognode) => Some ( vognode . skgid () . clone () ),
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
