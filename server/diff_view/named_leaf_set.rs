//! What a processed request names: sets of change-forest leaves
//! that can be told apart without the change forest ('NamedLeafSet').
//! Validation compares them ('NamedLeafSet::overlap',
//! 'find_contradictory_processed_requests'). Which sets a headline's
//! request names is 'leaves_named_at'
//! ('server/diff_view/processed_requests.rs').

use crate::diff_view::change_leaves::{
  ChangeLeaves, LeafKey, NewNodeWalk, NodeAspect, RelationshipAspect};
use crate::dbs::in_rust_graph::relation_accessors::{
  NodeRelation, RelationshipKey};
use crate::types::misc::ID;
use crate::types::viewnode::DiffObject;

use std::collections::BTreeSet;

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
  pub(crate) fn named_by_letters_among (
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
  pub(crate) fn keys_in_forest (
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
          NodeAspect::Flag (flag)   => format! ("flag {}", flag . wire_name ()),
          NodeAspect::OtherChanges  => "other changes" . to_string (), };
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
