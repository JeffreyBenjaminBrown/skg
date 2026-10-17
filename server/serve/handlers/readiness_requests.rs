//! A save consumes the readiness requests of the buffer it saves
//! ('TODO/total-diff-view/2_plan.org', "metadata"): after the graph
//! edits succeed, each '(readyRequest ...)' or '(unreadyRequest ...)'
//! sets or clears the marks of the change-forest leaves it names, at
//! their fingerprints in the fresh graph pair. A save that stops for
//! a confirmation consumes nothing.

use crate::change_forest::{
  ChangeForest, LeafKey, NodeAspect, RelationshipAspect};
use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::in_rust_graph::relation_accessors::{
  NodeRelation, RelationRole};
use crate::graph_pair::build::current_graph_pair_in_slot;
use crate::graph_pair::types::GraphPair;
use crate::readiness_store::ReadinessStore;
use crate::types::env::{RuntimeGeneration, SkgEnv};
use crate::types::misc::ID;
use crate::types::tree::forest::ViewForest;
use crate::types::viewnode::{
  Phantom, Property, ReadinessAxis, ReadinessRequest, Viewnode,
  ViewnodeKind, Vognode};

use ego_tree::NodeRef;
use std::sync::{Arc, MutexGuard};

/// After a save's graph edits succeeded: apply the saved buffer's
/// readiness requests to the store, against the graph pair in
/// 'graph_pair_slot' brought up to date with 'runtime', and write the
/// store back. Returns the warnings to show. Builds no pair when the
/// buffer carries no request.
pub fn consume_readiness_requests (
  viewforest      : &ViewForest,
  env             : &SkgEnv,
  runtime         : &RuntimeGeneration,
  graph_pair_slot : &mut Option<Arc<GraphPair>>,
) -> Vec<String> {
  let requests : Vec<RequestedReadiness> =
    readiness_requests_in_viewforest (viewforest, &runtime . graph);
  if requests . is_empty () { return Vec::new (); }
  let pair : Arc<GraphPair> =
    match current_graph_pair_in_slot (
      graph_pair_slot, &runtime . config, runtime . graph . clone () ) {
      Ok (pair) => pair,
      Err (e) => return vec! [ format! (
        "Readiness requests were not applied: {}", e ) ], };
  let mut store : MutexGuard<ReadinessStore> =
    env . runtime . readiness_store . lock ()
    . unwrap_or_else ( |poisoned| poisoned . into_inner () );
  let mut warnings : Vec<String> =
    apply_readiness_requests (&requests, pair . change_forest (), &mut store);
  if let Err (e) = store . save (&runtime . config . data_root) {
    warnings . push ( format! (
      "The readiness marks were applied but not written to disk: {}", e )); }
  warnings }

/// One readiness request, read from the saved buffer.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct RequestedReadiness {
  pub ready    : bool,
  /// The leaves named outright.
  pub leaves   : Vec<LeafKey>,
  /// The nodes whose leaves an 'all' request names.
  pub all_of   : Vec<ID>,
  /// The headline, for warnings.
  pub headline : String,
}

/// Every readiness request in the saved buffer, with the leaves it
/// names. IDs resolve through 'graph' (the graph after the save),
/// falling back to the ID as written.
pub fn readiness_requests_in_viewforest (
  viewforest : &ViewForest,
  graph      : &InRustGraph,
) -> Vec<RequestedReadiness> {
  let pid_of = |skgid : &ID| -> ID {
    graph . pid_of (skgid) . unwrap_or_else ( || skgid . clone () ) };
  viewforest . root () . descendants ()
    . filter_map ( |node| {
      let request : &ReadinessRequest = readiness_request_of (node . value ()) ?;
      let mut leaves : Vec<LeafKey> = Vec::new ();
      let mut all_of : Vec<ID> = Vec::new ();
      match &node . value () . kind {
        ViewnodeKind::Vognode (vognode) => {
          let pid : ID = pid_of ( vognode . skgid () ? );
          let is_unknown : bool =
            matches! (vognode, Vognode::Phantom (Phantom::Unknown (_)));
          let axes : Vec<ReadinessAxis> =
            if ! request . axes . is_empty () {
              request . axes . iter () . copied () . collect ()
            } else if is_unknown { vec! [ ReadinessAxis::Rel ] }
            else { vec! [ ReadinessAxis::Node, ReadinessAxis::Rel ] };
          for axis in axes {
            match axis {
              ReadinessAxis::Node if ! is_unknown =>
                leaves . push ( LeafKey::Node {
                  pid : pid . clone (), aspect : NodeAspect::Existence } ),
              ReadinessAxis::Node => {},
              ReadinessAxis::Rel =>
                leaves . extend ( relationship_to_viewparent (
                  node, &pid, &pid_of )),
              ReadinessAxis::All =>
                all_of . push (pid . clone ()), }} },
        ViewnodeKind::Property (property) => {
          let owner : ID = pid_of ( &grandparent_pid (node) ? );
          let aspect : NodeAspect = match property {
            Property::Alias { text, .. } => NodeAspect::Alias (text . clone ()),
            Property::ID { skgid, .. }   => NodeAspect::ExtraId (skgid . clone ()),
            Property::Flag { flag, .. }  => NodeAspect::Flag (*flag),
            Property::TextChanged { .. } => return None, };
          leaves . push ( LeafKey::Node { pid : owner, aspect } ); },
        _ => return None, }
      Some ( RequestedReadiness {
        ready    : request . ready,
        leaves, all_of,
        headline : node . value () . title () . to_string (), } ) } )
    . collect () }

/// Set or clear the marks the requests name, at their fingerprints
/// in 'forest'. Returns a warning for each request that names no
/// current change.
pub fn apply_readiness_requests (
  requests : &[RequestedReadiness],
  forest   : &ChangeForest,
  store    : &mut ReadinessStore,
) -> Vec<String> {
  let mut warnings : Vec<String> = Vec::new ();
  for request in requests {
    let keys : Vec<LeafKey> =
      request . leaves . iter () . cloned ()
      . chain ( request . all_of . iter () . flat_map ( |pid|
        forest . leaves_under (pid) . iter ()
          . map ( |leaf| leaf . key . clone () )) )
      . filter ( |key| forest . leaf (key) . is_some () )
      . collect ();
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

fn readiness_request_of (
  viewnode : &Viewnode,
) -> Option<&ReadinessRequest> {
  match &viewnode . kind {
    ViewnodeKind::Vognode (Vognode::Unrestricted (t)) =>
      t . readiness_request . as_ref (),
    ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Deleted (d))) =>
      d . readiness_request . as_ref (),
    ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Unknown (u))) =>
      u . readiness_request . as_ref (),
    ViewnodeKind::Property (
      Property::Alias { readiness_request, .. }
      | Property::ID { readiness_request, .. }
      | Property::Flag { readiness_request, .. }) =>
      readiness_request . as_ref (),
    _ => None, } }

/// The relationship by which a vognode is a member here: its
/// viewparent's content, or the relation of the partner folder it is
/// in, whose anchor is the folder's viewparent. None at a root, or
/// under anything else.
fn relationship_to_viewparent (
  node   : NodeRef<Viewnode>,
  pid    : &ID,
  pid_of : &dyn Fn (&ID) -> ID,
) -> Option<LeafKey> {
  let parent : NodeRef<Viewnode> = node . parent () ?;
  let key = |relation : NodeRelation, recorder : ID, member : ID| -> LeafKey {
    LeafKey::Relationship {
      relation, recorder, member,
      aspect : RelationshipAspect::Existence } };
  match &parent . value () . kind {
    ViewnodeKind::Vognode (vognode) =>
      Some ( key ( NodeRelation::Contains,
                   pid_of ( vognode . skgid () ? ), pid . clone () )),
    ViewnodeKind::PartnerFolder (folder) => {
      let role : RelationRole = folder . relation_member_role () ?;
      let anchor : ID = pid_of ( &parent . parent () ? . value ()
                                 . kind . vognode_skgid () ? );
      Some ( if role . is_first_role () { // the member records it
               key ( role . relation, pid . clone (), anchor )
             } else {
               key ( role . relation, anchor, pid . clone () ) } ) },
    _ => None, } }

/// The ID of the vognode two levels up: the node that owns a
/// property leaf's folder.
fn grandparent_pid (
  node : NodeRef<Viewnode>,
) -> Option<ID> {
  node . parent () ? . parent () ? . value () . kind . vognode_skgid () }

impl ViewnodeKind {
  fn vognode_skgid (
    &self,
  ) -> Option<ID> {
    match self {
      ViewnodeKind::Vognode (vognode) => vognode . skgid () . cloned (),
      _ => None, } }
}
