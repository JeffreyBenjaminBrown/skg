//! Processed requests, '(processed ...)' change facts and a buffer's processed
//! summary ('docs/diff-view.org', "Processed marks"), the places where the
//! processed store meets a viewforest. The first two rest on one
//! rule, 'leaves_named_at': which change-forest leaves a headline
//! names.
//!
//! A save consumes the processed requests of the buffer it saves, in
//! any buffer ('consume_processed_requests'): after the graph edits
//! succeed, each '(editRequest (processed ...))' or '(editRequest (unprocessed
//! ...))' sets or
//! clears the marks of the leaves it names, at their fingerprints in
//! the graph straddle brought up to date with the save. A request that
//! names no current change is dropped with a warning. A save that stops
//! for a confirmation consumes nothing. Validation has refused a save
//! in which two headlines name one leaf, one processed and the other
//! unprocessed ('find_contradictory_processed_requests', which reads
//! 'NamedLeafSet').
//!
//! At the tail of the refresh of a buffer of the diff view, the
//! '(processed ...)' change facts are set from the store, and the
//! '(leaves ...)' facts, which tell the clients what a request on a
//! headline may name, from the same rule
//! ('set_processed_facts_in_viewforest').
//!
//! A save of a buffer of the diff view, and the "extend diff view"
//! request, answer with the processed summary of the buffer
//! they rerendered ('buffer_processed_summary_for').

use crate::types::tree::forest::stands_in_containerward_chain;
use crate::diff_view::change_leaves::{
  BufferProcessedSummary, ChangeLeaves, LeafKey, NewNodeWalk, NodeAspect,
  RelationshipAspect, buffer_processed_summary};
use crate::diff_view::pruned_change_view::nodes_editable_in_diffView;
use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::in_rust_graph::relation_accessors::{
  NodeRelation, RelationRole, RelationshipKey};
use crate::graph_straddle::build::current_graph_straddle_in_slot;
use crate::graph_straddle::types::GraphStraddle;
use crate::diff_view::processed_store::ProcessedStore;
use crate::types::env::{RuntimeGeneration, SkgEnv};
use crate::types::errors::{HeadlineName, with_org_subheadlines};
use crate::types::misc::ID;
use crate::types::tree::forest::ViewForest;
use crate::types::viewnode::{
  ChangeFacts, ChangesetMarkerKind, FORKS_FOLDER_MEMBER_ROLES, Folder, LeavesFact,
  PropertyMember, ChangeViewnode, DiffObject, ProcessedRequest, Viewnode, ViewnodeKind,
  Vognode};

use ego_tree::{NodeId, NodeMut, NodeRef};
use std::collections::{BTreeSet, HashSet};
use std::sync::{Arc, MutexGuard};

/// After a save's graph edits succeeded: apply the saved buffer's
/// processed requests to the store, against the graph straddle in
/// 'graph_straddle_slot' brought up to date with 'runtime', and write the
/// store back. Returns the warnings to show. Builds no straddle when the
/// buffer carries no request.
pub fn consume_processed_requests (
  viewforest          : &ViewForest,
  env                 : &SkgEnv,
  runtime             : &RuntimeGeneration,
  graph_straddle_slot : &mut Option<Arc<GraphStraddle>>,
) -> Vec<String> {
  let requests : Vec<RequestedProcessed> =
    processed_requests_in_viewforest (viewforest, &runtime . graph);
  if requests . is_empty () { return Vec::new (); }
  let straddle : Arc<GraphStraddle> =
    match current_graph_straddle_in_slot (
      graph_straddle_slot, &runtime . config, runtime . graph . clone () ) {
      Ok (straddle) => straddle,
      Err (e) => return vec! [ format! (
        "Processed requests were not applied: {}", e ) ], };
  let mut store : MutexGuard<ProcessedStore> =
    env . runtime . lock_processed_store ();
  let mut warnings : Vec<String> =
    apply_processed_requests (&requests, straddle . change_leaves (), &mut store);
  if let Err (e) = store . save ( &runtime . config . processed_marks_path () ) {
    warnings . push ( format! (
      "The processed marks were applied but not written to disk: {}", e )); }
  warnings }

/// Whether some headline of 'viewforest' carries a processed request.
pub fn viewforest_has_processed_requests (
  viewforest : &ViewForest,
) -> bool {
  viewforest . root () . descendants ()
    . any ( |node| node . value () . processed_request . is_some () ) }

/// One processed request, read from the saved buffer.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct RequestedProcessed {
  pub processed : bool,
  pub named    : NamedLeaves,
  /// The headline carrying the request, which a warning names.
  pub headline : HeadlineName,
}

/// Every processed request in the saved buffer, with the leaves it
/// names. IDs resolve through 'graph' (the graph after the save),
/// falling back to the ID as written, except a ghost's,
/// which names a node of the before-graph.
pub fn processed_requests_in_viewforest (
  viewforest : &ViewForest,
  graph      : &InRustGraph,
) -> Vec<RequestedProcessed> {
  let pid_of = |gnid : &ID| -> ID {
    graph . pid_else_raw_id (gnid) };
  viewforest . root () . descendants ()
    . filter_map ( |node| {
      let request : &ProcessedRequest = node . value () . processed_request . as_ref () ?;
      let named : NamedLeaves = leaves_named_at (node, &request . objects, &pid_of) ?;
      Some ( RequestedProcessed {
        processed : request . processed,
        named,
        headline : node . value () . headline_name (), } ) } )
    . collect () }

/// What a request on a viewnode names: sets of leaves that can be told
/// apart without the change forest ('NamedLeafSet').
pub type NamedLeaves = Vec<NamedLeafSet>;

/// A set of change leaves a request names, told without the change
/// forest: one leaf; the leaves a node records but the relationships
/// its headline's 'rel' or a headline under it names ('node' on a
/// vognode); or the content
/// relationships a node records, of one aspect ('rel' on a changeset
/// marker). Which leaves each holds is 'holds'; which of them the
/// forest has, 'keys_in_forest'. The exceptions are the sets only the
/// forest can tell, which without it are taken to hold every leaf they
/// might: 'MembersOwnLeavesOf' ('node' on a changeset marker: the own
/// leaves of the members of the relationships it gathers, each among
/// the content members drawn beside the marker) and
/// 'NewNodesFrom' ('newContent', 'newOutward': the leaves of the new
/// nodes a walk reaches from a node, of the kinds its letters name,
/// 'ChangeLeaves::leaves_of_new_nodes_reached_from').
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum NamedLeafSet {
  Leaf                   (LeafKey),
  OwnLeavesOf            (ID, RelationshipsNamedElsewhere),
  ContentRelationshipsOf (ID, RelationshipAspect),
  MembersOwnLeavesOf     (ID, RelationshipAspect, Vec<ID>),
  NewNodesFrom           (ID, NewNodeWalk, LeafKinds),
}

/// The relationships recorded by the node of a headline that 'node'
/// there does not name, because a headline shows them elsewhere: its
/// own relationship to its viewparent, and that of each member drawn
/// under it, in its content or in a partner folder, whatever their
/// aspect (a relRepo move hangs under the member); and the content
/// relationships of each aspect a changeset marker under it gathers.
#[derive(Clone, Debug, Default, Eq, Hash, PartialEq)]
pub struct RelationshipsNamedElsewhere {
  pub relationships   : Vec<RelationshipKey>,
  pub content_aspects : Vec<RelationshipAspect>,
}

impl RelationshipsNamedElsewhere {
  fn names (
    &self,
    relationship : &RelationshipKey,
    aspect       : RelationshipAspect,
  ) -> bool {
    self . relationships . contains (relationship)
    || ( relationship . relation == NodeRelation::Contains
         && self . content_aspects . contains (&aspect) ) }
}

/// The kinds of leaf of the nodes a walk reaches that a request names:
/// their own leaves ('node'), their relationships ('rel'), or both,
/// which a request naming neither letter beside its walk names.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct LeafKinds {
  pub own           : bool,
  pub relationships : bool,
}

impl LeafKinds {
  /// The kinds the letters among 'objects' name.
  fn named_by_letters_among (
    objects : &[DiffObject],
  ) -> LeafKinds {
    let own : bool = objects . contains (&DiffObject::Node);
    let relationships : bool = objects . contains (&DiffObject::Rel);
    if own || relationships { LeafKinds { own, relationships } }
    else { LeafKinds { own : true, relationships : true } } }

  fn admits (
    &self,
    key : &LeafKey,
  ) -> bool {
    match key {
      LeafKey::Node { .. }         => self . own,
      LeafKey::Relationship { .. } => self . relationships, } }
}

impl NamedLeafSet {
  /// Whether the set holds the leaf 'key', whatever the forest holds:
  /// the one leaf; a leaf its node records that neither the headline's
  /// 'rel' nor a headline under it names; or, for a changeset marker's 'rel', a content relationship
  /// its node records, of its aspect. A set only the forest can tell
  /// might hold any leaf of its kinds.
  pub fn holds (
    &self,
    key : &LeafKey,
  ) -> bool {
    match self {
      NamedLeafSet::Leaf (leaf) => leaf == key,
      NamedLeafSet::OwnLeavesOf (pid, elsewhere) =>
        recorder_of_leaf (key) == pid
        && match key {
          LeafKey::Node { .. } => true,
          LeafKey::Relationship { relationship, aspect } =>
            ! elsewhere . names (relationship, *aspect), },
      NamedLeafSet::ContentRelationshipsOf (pid, aspect) => matches! (
        key,
        LeafKey::Relationship {
          relationship : RelationshipKey {
            relation : NodeRelation::Contains, recorder, .. },
          aspect : leaf_aspect }
        if recorder == pid && leaf_aspect == aspect ),
      NamedLeafSet::MembersOwnLeavesOf (_, _, drawn_members) => matches! (
        key, LeafKey::Node { pid, .. } if drawn_members . contains (pid) ),
      NamedLeafSet::NewNodesFrom (_, _, kinds) => kinds . admits (key), } }

  /// Whether some leaf the set can hold is a node's own, and whether
  /// some is a relationship's.
  fn can_hold_kinds (
    &self,
  ) -> LeafKinds {
    match self {
      NamedLeafSet::Leaf (leaf) => LeafKinds {
        own           : matches! ( leaf, LeafKey::Node { .. } ),
        relationships : matches! ( leaf, LeafKey::Relationship { .. } ), },
      NamedLeafSet::OwnLeavesOf (..) =>
        LeafKinds { own : true, relationships : true },
      NamedLeafSet::ContentRelationshipsOf (..) =>
        LeafKinds { own : false, relationships : true },
      NamedLeafSet::MembersOwnLeavesOf (..) =>
        LeafKinds { own : true, relationships : false },
      NamedLeafSet::NewNodesFrom (_, _, kinds) => *kinds, } }

  /// The leaves both sets name, whatever the forest holds, as the
  /// narrower of the two; None when they can name no leaf in common.
  /// A changeset marker's 'node' might share any own leaf of a member
  /// drawn beside it, and a walk any leaf of its kinds, so validation
  /// refuses a request of the opposite kind beside one that can hold
  /// such a leaf.
  pub fn overlap (
    &self,
    other : &NamedLeafSet,
  ) -> Option<NamedLeafSet> {
    let narrower_within : &dyn Fn (&NamedLeafSet, &NamedLeafSet) -> bool =
      &|narrower : &NamedLeafSet, wider : &NamedLeafSet| -> bool {
      match (narrower, wider) {
        (NamedLeafSet::Leaf (leaf), _) => wider . holds (leaf),
        (NamedLeafSet::OwnLeavesOf (pid, _),
         NamedLeafSet::OwnLeavesOf (wider_pid, _)) =>
          pid == wider_pid, // its own aspects, at least
        (NamedLeafSet::ContentRelationshipsOf (pid, aspect),
         NamedLeafSet::OwnLeavesOf (wider_pid, elsewhere)) =>
          pid == wider_pid
          && ! elsewhere . content_aspects . contains (aspect),
        (NamedLeafSet::OwnLeavesOf (pid, _),
         NamedLeafSet::MembersOwnLeavesOf (_, _, drawn_members)) =>
          drawn_members . contains (pid),
        (NamedLeafSet::MembersOwnLeavesOf (_, _, drawn_members),
         NamedLeafSet::MembersOwnLeavesOf (_, _, wider_drawn_members)) =>
          drawn_members . iter ()
            . any ( |member| wider_drawn_members . contains (member) ),
        (_, NamedLeafSet::MembersOwnLeavesOf (..))
          | (_, NamedLeafSet::NewNodesFrom (..)) => {
          let narrower_kinds : LeafKinds = narrower . can_hold_kinds ();
          let wider_kinds    : LeafKinds = wider    . can_hold_kinds ();
          ( narrower_kinds . own && wider_kinds . own )
          || ( narrower_kinds . relationships && wider_kinds . relationships ) },
        _ => narrower == wider, } };
    if narrower_within (self, other) { Some ( self . clone () ) }
    else if narrower_within (other, self) { Some ( other . clone () ) }
    else { None } }

  /// The leaves of 'forest' the set holds.
  fn keys_in_forest (
    &self,
    forest : &ChangeLeaves,
  ) -> Vec<LeafKey> {
    match self {
      NamedLeafSet::Leaf (leaf) =>
        forest . leaf (leaf) . map ( |_| leaf . clone () ) . into_iter () . collect (),
      NamedLeafSet::OwnLeavesOf (pid, _)
        | NamedLeafSet::ContentRelationshipsOf (pid, _) =>
        forest . leaves_under (pid) . iter ()
          . map ( |leaf| leaf . key . clone () )
          . filter ( |key| self . holds (key) )
          . collect (),
      NamedLeafSet::MembersOwnLeavesOf (pid, aspect, _) => {
        let gathered : NamedLeafSet =
          NamedLeafSet::ContentRelationshipsOf (pid . clone (), *aspect);
        let members : BTreeSet<ID> =
          gathered . keys_in_forest (forest) . into_iter ()
          . filter_map ( |key| match key {
            LeafKey::Relationship { relationship, .. } =>
              Some (relationship . member),
            LeafKey::Node { .. } => None, } )
          . collect ();
        members . iter ()
          . flat_map ( |member| forest . leaves_under (member) . iter () )
          . map ( |leaf| leaf . key . clone () )
          . filter ( |key| matches! ( key, LeafKey::Node { .. } ))
          . collect () },
      NamedLeafSet::NewNodesFrom (pid, walk, kinds) =>
        forest . leaves_of_new_nodes_reached_from (pid, *walk) . into_iter ()
          . map ( |leaf| leaf . key . clone () )
          . filter ( |key| kinds . admits (key) )
          . collect (), } }

  /// The set in words, for a message.
  pub fn describe (
    &self,
  ) -> String {
    match self {
      NamedLeafSet::Leaf (LeafKey::Node { pid, aspect }) => {
        let what : String = match aspect {
          NodeAspect::Existence   => "existence" . to_string (),
          NodeAspect::Text        => "text" . to_string (),
          NodeAspect::HomeSkgrepo => "home skgrepo" . to_string (),
          NodeAspect::Alias (alias) => format! ("alias {:?}", alias),
          NodeAspect::ExtraId (id)  => format! ("extra ID {}", id),
          NodeAspect::Flag (flag)   => format! ("flag {}", flag . wire_name ()), };
        format! ("the {} of node {}", what, pid) },
      NamedLeafSet::Leaf (LeafKey::Relationship { relationship, aspect }) =>
        format! ("the {} of the relationship {} {} {}",
                 relationship_aspect_name (*aspect),
                 relationship . recorder,
                 relationship . relation . relation_name (),
                 relationship . member ),
      NamedLeafSet::OwnLeavesOf (pid, _) =>
        format! ("every change node {} records but the relationships shown on its headline or under it",
                 pid),
      NamedLeafSet::ContentRelationshipsOf (pid, aspect) =>
        format! ("the {} of every content relationship node {} records",
                 relationship_aspect_name (*aspect), pid),
      NamedLeafSet::MembersOwnLeavesOf (pid, aspect, _) =>
        format! ("the own changes of each member whose content relationship's {} changed in node {}",
                 relationship_aspect_name (*aspect), pid),
      NamedLeafSet::NewNodesFrom (pid, walk, kinds) =>
        format! ("the {} of the new nodes {} of node {}",
                 match (kinds . own, kinds . relationships) {
                   (true, false) => "existence",
                   (false, true) => "relationships",
                   _             => "changes", },
                 match walk { NewNodeWalk::Contentward => "contentward",
                              NewNodeWalk::Outward     => "outward", },
                 pid), } }
}

/// The node that records a leaf: a node's own aspect, its node; a
/// relationship, its recorder.
fn recorder_of_leaf (
  leaf : &LeafKey,
) -> &ID {
  match leaf {
    LeafKey::Node { pid, .. } => pid,
    LeafKey::Relationship { relationship, .. } => &relationship . recorder, } }

fn relationship_aspect_name (
  aspect : RelationshipAspect,
) -> &'static str {
  match aspect {
    RelationshipAspect::Existence => "existence",
    RelationshipAspect::RelRepo   => "relRepo", } }

/// The leaves that a request naming the diff objects 'objects' at
/// 'node' names. No objects names what the headline's '(leaves ...)'
/// can list: on a vognode both of its objects (only 'rel' on an
/// unknown viewnode or a restricted viewnode), on a changeset marker
/// 'rel'; a property member and any other change viewnode have one
/// object. On a vognode 'node' names every leaf its node records but
/// the relationships its 'rel' or a headline under it names
/// ('relationships_named_elsewhere'); on a changeset marker, the own
/// leaves of the members of the relationships it gathers. Beside a
/// walk, 'node' and 'rel' also filter what it reaches
/// ('LeafKinds::named_by_letters_among').
/// None for a viewnode that names no leaf, among them a node of the
/// containerward chain, which shows no change.
fn leaves_named_at (
  node    : NodeRef<Viewnode>,
  objects : &BTreeSet<DiffObject>,
  pid_of  : &dyn Fn (&ID) -> ID,
) -> Option<NamedLeaves> {
  let mut named : NamedLeaves = Vec::new ();
  match &node . value () . kind {
    ViewnodeKind::Vognode (_) if stands_in_containerward_chain (node, true) =>
      return None,
    ViewnodeKind::Vognode (vognode) => {
      let pid : ID = vognode_leaf_pid (node . value (), pid_of) ?;
      let is_gnid_only : bool = // no node it shows, one relationship
        matches! (vognode, Vognode::GnidOnly (_));
      let objects : Vec<DiffObject> =
        if ! objects . is_empty () {
          objects . iter () . copied () . collect ()
        } else if is_gnid_only { vec! [ DiffObject::Rel ] }
        else { vec! [ DiffObject::Node, DiffObject::Rel ] };
      let walk_leaf_kinds : LeafKinds =
        LeafKinds::named_by_letters_among (&objects);
      for object in objects {
        match object {
          DiffObject::Node if ! is_gnid_only =>
            named . push ( NamedLeafSet::OwnLeavesOf (
              pid . clone (),
              relationships_named_elsewhere (node, pid_of) )),
          DiffObject::Node => {},
          DiffObject::Rel =>
            named . extend ( relationships_to_viewparent (
              node, &pid, pid_of, RelationshipAspect::Existence )
              . into_iter () . map (NamedLeafSet::Leaf) ),
          DiffObject::NewContent =>
            named . push ( NamedLeafSet::NewNodesFrom (
              pid . clone (), NewNodeWalk::Contentward, walk_leaf_kinds )),
          DiffObject::NewOutward =>
            named . push ( NamedLeafSet::NewNodesFrom (
              pid . clone (), NewNodeWalk::Outward, walk_leaf_kinds )), }} },
    ViewnodeKind::PropertyMember (member) => {
      // The change-forest leaf of a property member belongs to the
      // folder's viewparent.
      let pid_of_folders_viewparent : ID =
        pid_of ( &grandparent_pid (node) ? );
      let aspect : NodeAspect = match member {
        PropertyMember::Alias { text, .. } => NodeAspect::Alias (text . clone ()),
        PropertyMember::ID { gnid, .. }    => NodeAspect::ExtraId (gnid . clone ()),
        PropertyMember::Flag { flag, .. }  => NodeAspect::Flag (*flag), };
      named . push ( NamedLeafSet::Leaf ( LeafKey::Node {
        pid : pid_of_folders_viewparent, aspect } )); },
    ViewnodeKind::ChangeViewnode (change) => {
      // A change viewnode hangs under its node.
      let parent : NodeRef<Viewnode> = node . parent () ?;
      let parent_pid = || -> Option<ID> {
        vognode_leaf_pid (parent . value (), pid_of) };
      match change {
        ChangeViewnode::OldText { .. } =>
          named . push ( NamedLeafSet::Leaf ( LeafKey::Node {
            pid : parent_pid () ?, aspect : NodeAspect::Text } )),
        ChangeViewnode::HomeSkgrepoMove { .. } =>
          named . push ( NamedLeafSet::Leaf ( LeafKey::Node {
            pid : parent_pid () ?, aspect : NodeAspect::HomeSkgrepo } )),
        ChangeViewnode::RelRepoMove { .. } =>
          named . extend ( relationships_to_viewparent (
            parent, &parent_pid () ?, pid_of,
            RelationshipAspect::RelRepo )
            . into_iter () . map (NamedLeafSet::Leaf) ),
        ChangeViewnode::ChangesetMarker { kind } => {
          let aspect : RelationshipAspect = match kind {
            ChangesetMarkerKind::Content   => RelationshipAspect::Existence,
            ChangesetMarkerKind::RelRepo   => RelationshipAspect::RelRepo,
            ChangesetMarkerKind::OrderOnly => return Some (named), }; // no leaf records an order
          let pid : ID = parent_pid () ?;
          if objects . is_empty () || objects . contains (&DiffObject::Rel) {
            named . push ( NamedLeafSet::ContentRelationshipsOf (
              pid . clone (), aspect )); }
          if objects . contains (&DiffObject::Node) {
            let drawn_members : Vec<ID> = parent . children ()
              . filter ( |sibling| matches! ( sibling . value () . kind,
                                              ViewnodeKind::Vognode (_) ))
              . filter_map ( |sibling| vognode_leaf_pid (sibling . value (), pid_of) )
              . collect ();
            named . push ( NamedLeafSet::MembersOwnLeavesOf (
              pid, aspect, drawn_members )); }},
        ChangeViewnode::BodyDiff { .. } =>
          return None, }},
    _ => return None, }
  Some (named) }

/// What 'node' on the vognode 'node' leaves to its 'rel' and the headlines under it
/// ('RelationshipsNamedElsewhere'): the relationship its 'rel' names;
/// that of each vognode under it and of each member of a partner
/// folder under it; and the aspects of the changeset markers under it.
fn relationships_named_elsewhere (
  node   : NodeRef<Viewnode>,
  pid_of : &dyn Fn (&ID) -> ID,
) -> RelationshipsNamedElsewhere {
  let relationship_of = |vognode : NodeRef<Viewnode>| -> Vec<RelationshipKey> {
    let Some (member_pid) : Option<ID> = vognode_leaf_pid (vognode . value (), pid_of)
      else { return Vec::new (); };
    relationships_to_viewparent (
      vognode, &member_pid, pid_of, RelationshipAspect::Existence ) . into_iter ()
    . filter_map ( |leaf| match leaf {
      LeafKey::Relationship { relationship, .. } => Some (relationship),
      LeafKey::Node { .. } => None, } )
    . collect () };
  let mut elsewhere : RelationshipsNamedElsewhere =
    RelationshipsNamedElsewhere::default ();
  elsewhere . relationships . extend ( relationship_of (node) );
  for child in node . children () {
    match &child . value () . kind {
      ViewnodeKind::Vognode (_) =>
        elsewhere . relationships . extend ( relationship_of (child) ),
      ViewnodeKind::Folder (_) =>
        elsewhere . relationships . extend (
          child . children ()
            . filter ( |member| matches! ( member . value () . kind,
                                           ViewnodeKind::Vognode (_) ))
            . flat_map (relationship_of) ),
      ViewnodeKind::ChangeViewnode (
        ChangeViewnode::ChangesetMarker { kind } ) => match kind {
        ChangesetMarkerKind::Content =>
          elsewhere . content_aspects . push (RelationshipAspect::Existence),
        ChangesetMarkerKind::RelRepo =>
          elsewhere . content_aspects . push (RelationshipAspect::RelRepo),
        ChangesetMarkerKind::OrderOnly => {}, },
      _ => {}, }}
  elsewhere }

/// The leaves of 'forest' that 'named' names.
fn keys_in_forest (
  named  : &NamedLeaves,
  forest : &ChangeLeaves,
) -> Vec<LeafKey> {
  named . iter ()
    . flat_map ( |set| set . keys_in_forest (forest) )
    . collect () }

/// Set the '(processed ...)' and '(leaves ...)' change facts of every
/// viewnode that carries them, and clear '(properties changed)' where
/// 'node' names no leaf. An object is processed when it names some
/// leaf of 'forest' and every leaf it names is marked at its current
/// fingerprint ('store'). On a vognode or a changeset marker,
/// '(processed ...)' names the objects of its '(leaves ...)' that are
/// processed; the order-only changeset marker never is, since no leaf
/// records the change it stands for ('UnmarkableChange'); any
/// other processed viewnode has a single object, and writes it bare,
/// '(processed)'. The '(leaves ...)' fact is 'leaves_fact_at'.
/// A modified node's 'M' ('(properties changed)') stands for what
/// 'node' names there ('leaves_named_at'), and so is not drawn where
/// its 'rel' and the headlines under it show every change it records.
pub fn set_processed_facts_in_viewforest (
  viewforest : &mut ViewForest,
  forest     : &ChangeLeaves,
  store      : &ProcessedStore,
  graph      : &InRustGraph,
) {
  let pid_of = |gnid : &ID| -> ID {
    graph . pid_else_raw_id (gnid) };
  let object_is_processed = |node : NodeRef<Viewnode>, objects : &[DiffObject]| -> bool {
    let keys : Vec<LeafKey> = keys_named_at (node, objects, forest, &pid_of);
    ! keys . is_empty () && forest . processed_of_leaves (store, &keys) };
  let processed_among = |node : NodeRef<Viewnode>, leaves : &Option<LeavesFact>|
                        -> Option<BTreeSet<DiffObject>> {
    let objects : BTreeSet<DiffObject> =
      leaves . iter () . flat_map ( |leaves| leaves . objects . iter () )
      . filter ( |object| object_is_processed (node, &[ **object ]) )
      . copied () . collect ();
    ( ! objects . is_empty () ) . then_some (objects) };
  let facts : Vec<(NodeId, Option<BTreeSet<DiffObject>>, Option<LeavesFact>, bool)> =
    viewforest . root () . descendants ()
    . filter_map ( |node| {
      let leaves : Option<LeavesFact> = leaves_fact_at (node, forest, &pid_of);
      let properties : bool =
        node . value () . change_facts . change . properties
        && leaves . as_ref () . is_some_and ( |leaves|
          leaves . objects . contains (&DiffObject::Node) );
      let processed : Option<BTreeSet<DiffObject>> =
        match &node . value () . kind {
          ViewnodeKind::Vognode (
            Vognode::Occurrence (_) | Vognode::Ghost (_) ) =>
            processed_among (node, &leaves),
          ViewnodeKind::Vognode (Vognode::GnidOnly (_)) =>
            object_is_processed (node, &[]) . then (BTreeSet::new),
          ViewnodeKind::ChangeViewnode ( ChangeViewnode::ChangesetMarker {
            kind : ChangesetMarkerKind::OrderOnly } ) => None,
          ViewnodeKind::ChangeViewnode ( ChangeViewnode::ChangesetMarker { .. } ) =>
            processed_among (node, &leaves),
          ViewnodeKind::PropertyMember (_) =>
            object_is_processed (node, &[]) . then (BTreeSet::new),
          ViewnodeKind::ChangeViewnode (change)
            if ! matches! (change, ChangeViewnode::BodyDiff { .. }) =>
            object_is_processed (node, &[]) . then (BTreeSet::new),
          _ => return None, };
      Some (( node . id (), processed, leaves, properties )) } )
    . collect ();
  for (treeid, processed, leaves, properties) in facts {
    let mut node_mut : NodeMut<Viewnode> = viewforest . get_mut (treeid) . unwrap ();
    let change_facts : &mut ChangeFacts = &mut node_mut . value () . change_facts;
    change_facts . processed = processed;
    change_facts . leaves = leaves;
    change_facts . change . properties = properties; }}

/// The '(leaves N OBJECT...)' fact of 'node', from 'leaves_named_at':
/// on an occurrence or a ghost, each of 'node' and 'rel' that names a
/// leaf of 'forest'; on a changeset marker, 'rel' when it names one
/// ('node' there names what its members' own headlines show, so it is
/// never listed); on anything else no object. N is how many distinct
/// leaves of 'forest' a request naming those objects names, or a bare
/// one where none is listed. None when that is none.
fn leaves_fact_at (
  node   : NodeRef<Viewnode>,
  forest : &ChangeLeaves,
  pid_of : &dyn Fn (&ID) -> ID,
) -> Option<LeavesFact> {
  let names_some_leaf = |objects : &[DiffObject]| -> bool {
    ! keys_named_at (node, objects, forest, pid_of) . is_empty () };
  let listable : &[DiffObject] = match &node . value () . kind {
    ViewnodeKind::Vognode (
      Vognode::Occurrence (_)
      | Vognode::Ghost (_) ) => &[ DiffObject::Node, DiffObject::Rel ],
    ViewnodeKind::ChangeViewnode ( ChangeViewnode::ChangesetMarker { .. } ) =>
      &[ DiffObject::Rel ],
    _ => &[], };
  let objects : BTreeSet<DiffObject> =
    listable . iter () . copied ()
    . filter ( |object| names_some_leaf (&[ *object ]) )
    . collect ();
  if objects . is_empty () && ! listable . is_empty () { return None; }
  let count : usize = keys_named_at (
    node, &objects . iter () . copied () . collect::<Vec<_>> (),
    forest, pid_of ) . into_iter () . collect::<HashSet<LeafKey>> () . len ();
  ( count > 0 ) . then_some ( LeavesFact { count, objects } ) }

/// The leaves of 'forest' that a request naming 'objects' at 'node'
/// names. Several objects can name one leaf twice (a walk's 'node'
/// and the headline's own 'node' both name its existence leaf).
fn keys_named_at (
  node    : NodeRef<Viewnode>,
  objects : &[DiffObject],
  forest  : &ChangeLeaves,
  pid_of  : &dyn Fn (&ID) -> ID,
) -> Vec<LeafKey> {
  leaves_named_at ( node, &objects . iter () . copied () . collect (), pid_of )
    . map ( |named| keys_in_forest (&named, forest) )
    . unwrap_or_default () }

/// The ID a vognode's leaves are keyed by: a ghost's as
/// written (a node of the before-graph), any other's resolved by
/// 'pid_of'. None for a non-vognode.
pub(crate) fn vognode_leaf_pid (
  viewnode : &Viewnode,
  pid_of   : &dyn Fn (&ID) -> ID,
) -> Option<ID> {
  match &viewnode . kind {
    ViewnodeKind::Vognode (Vognode::Ghost (h)) =>
      Some ( h . gnid . clone () ),
    ViewnodeKind::Vognode (vognode) =>
      Some ( pid_of ( vognode . gnid () )),
    _ => None, } }

/// Set or clear the marks the requests name, at their fingerprints
/// in 'forest'. Returns a warning for each request that names no
/// current change.
pub fn apply_processed_requests (
  requests : &[RequestedProcessed],
  forest   : &ChangeLeaves,
  store    : &mut ProcessedStore,
) -> Vec<String> {
  let mut warnings : Vec<String> = Vec::new ();
  for request in requests {
    let keys : Vec<LeafKey> = keys_in_forest (&request . named, forest);
    if keys . is_empty () {
      warnings . push ( with_org_subheadlines (
        "A processed request names no current change; it was dropped.",
        &[ request . headline . clone () ] ));
      continue; }
    for key in keys {
      if request . processed {
        let fingerprint : u64 = forest . leaf_fingerprint (&key)
          . expect ("filtered to leaves of the forest");
        store . mark (key, fingerprint);
      } else { store . unmark (&key); }} }
  warnings }

/// The relationships by which a vognode is shown here, read from where
/// it stands: under a vognode, its viewparent's content; under a
/// partner folder, the relation of the folder, between it and the
/// folder's viewparent; under a forks folder, both sides of 'forks'
/// (the viewparent forking it, and it forking the viewparent), so that
/// the leaf of whichever side changed, or of both, is the member's.
/// None for a node of the containerward chain
/// ('stands_in_containerward_chain'), which stands for no relationship
/// the diff view tracks, at a root, or under anything else.
pub(crate) fn relationships_to_viewparent (
  node   : NodeRef<Viewnode>,
  pid    : &ID,
  pid_of : &dyn Fn (&ID) -> ID,
  aspect : RelationshipAspect,
) -> Vec<LeafKey> {
  relationship_keys_to_viewparent (node, pid, pid_of, aspect)
    . unwrap_or_default () }

fn relationship_keys_to_viewparent (
  node   : NodeRef<Viewnode>,
  pid    : &ID,
  pid_of : &dyn Fn (&ID) -> ID,
  aspect : RelationshipAspect,
) -> Option<Vec<LeafKey>> {
  let parent : NodeRef<Viewnode> = node . parent () ?;
  let key_for_role = |role : RelationRole, partner : ID| -> LeafKey {
    let (recorder, member) : (ID, ID) =
      if role . is_first_role () { (pid . clone (), partner) } // the node records it
      else { (partner, pid . clone ()) };
    LeafKey::Relationship {
      relationship : RelationshipKey { relation : role . relation, recorder, member },
      aspect } };
  match &parent . value () . kind {
    ViewnodeKind::Vognode (_) if stands_in_containerward_chain (node, true) => None,
    ViewnodeKind::Vognode (_) => {
      let viewparent : ID = vognode_leaf_pid (parent . value (), pid_of) ?;
      Some ( vec! [ key_for_role (RelationRole::CONTENT, viewparent) ] ) },
    ViewnodeKind::Folder (folder) => { // a property folder has no role
      let roles : Vec<RelationRole> = match folder {
        Folder::Forks => FORKS_FOLDER_MEMBER_ROLES . to_vec (),
        _ => vec! [ folder . relation_member_role () ? ], };
      let folder_viewparent : ID = vognode_leaf_pid (
        parent . parent () ? . value (), pid_of ) ?;
      Some ( roles . into_iter ()
             . map ( |role| key_for_role (role, folder_viewparent . clone ()) )
             . collect () ) },
    _ => None, } }

/// The ID of the vognode two levels up: the node that owns a
/// property member's folder.
fn grandparent_pid (
  node : NodeRef<Viewnode>,
) -> Option<ID> {
  match &node . parent () ? . parent () ? . value () . kind {
    ViewnodeKind::Vognode (vognode) => Some ( vognode . gnid () . clone () ),
    _ => None, } }

/// The processed summary of a buffer of the diff view, from
/// 'straddle' and the marks in the store: the changeset's counts, and
/// how many leaves are recorded by a node that 'viewforest' does not
/// make editable ('nodes_editable_in_diffView').
pub fn buffer_processed_summary_for (
  env        : &SkgEnv,
  straddle   : &GraphStraddle,
  viewforest : &ViewForest,
) -> BufferProcessedSummary {
  let store : MutexGuard<ProcessedStore> =
    env . runtime . lock_processed_store ();
  buffer_processed_summary (
    &env . runtime_snapshot () . config, straddle, &store,
    &nodes_editable_in_diffView (viewforest) ) }
