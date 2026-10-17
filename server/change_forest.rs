//! The change-forest: the tree of everything commit-readiness can be
//! asked about, built from the graph pair, independent of any view
//! ('TODO/total-diff-view/2_plan.org', "change-forest"). Its roots
//! are the changed graphnodes that own a change; under each come its
//! changed aspects; its leaves are the individual changes a user
//! marks. A relationship's leaves live under its recorder.
//!
//! The readiness of an aggregate (a set of leaves, a file, the whole
//! changeset) is derived here from a 'ReadinessStore'.

use crate::dbs::in_rust_graph::relation_accessors::{
  NodeRelation, RelationRole};
use crate::graph_pair::types::{GraphPair, NodeChanges, Partner, Presence};
use crate::graph_pair::union_walk::ROLES_OF_LISTED_MEMBERS;
use crate::readiness_store::ReadinessStore;
use crate::types::misc::{ID, SkgrepoName, members_msv};
use crate::types::nodes::complete::Flag;
use crate::types::nodes::rust::GraphnodeInRust;

use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

/// One change a user can mark commit-ready.
#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub enum LeafKey {
  Node {
    pid    : ID,
    aspect : NodeAspect, },
  /// The relationship by which 'recorder' lists 'member' under
  /// 'relation'. Seen from either end, it is the same leaf.
  Relationship {
    relation : NodeRelation,
    recorder : ID,
    member   : ID,
    aspect   : RelationshipAspect, },
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub enum NodeAspect {
  Existence,
  Text,
  Home,
  Alias   (String),
  ExtraId (ID),
  Flag    (Flag),
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub enum RelationshipAspect {
  Existence,
  RelRepo,
}

/// A change-forest leaf: its key, the fingerprint of the change it
/// stands for, and the files ((skgrepo, pid) pairs) that record it.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ChangeLeaf {
  pub key         : LeafKey,
  pub fingerprint : u64,
  pub files       : Vec<(SkgrepoName, ID)>,
}

/// What the client shows after a save of a TDV buffer.
/// Order-only files count as changed and ready. Foreign files are
/// counted apart, and never ready.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct ReadinessSummary {
  pub ready_files           : usize,
  pub changed_files         : usize,
  pub foreign_changed_files : usize,
  pub order_only_files      : usize,
}

/// See the module comment.
#[derive(Clone, Debug, Default)]
pub struct ChangeForest {
  /// Each node that owns a leaf, with its leaves.
  leaves_by_owner       : BTreeMap<ID, Vec<ChangeLeaf>>,
  /// Where each leaf is in 'leaves_by_owner': (owner, position).
  index                 : HashMap<LeafKey, (ID, usize)>,
  order_only_pids       : BTreeSet<ID>,
  order_only_files      : BTreeSet<(SkgrepoName, ID)>,
  foreign_changed_files : usize,
}

impl ChangeForest {
  pub fn from_graph_pair (
    pair : &GraphPair,
  ) -> ChangeForest {
    let mut leaves_by_owner : BTreeMap<ID, Vec<ChangeLeaf>> =
      BTreeMap::new ();
    for pid in & pair . changed_pids () . rightward {
      let leaves : Vec<ChangeLeaf> = leaves_owned_by (pair, pid);
      if ! leaves . is_empty () {
        leaves_by_owner . insert (pid . clone (), leaves); }}
    let index : HashMap<LeafKey, (ID, usize)> =
      leaves_by_owner . iter ()
      . flat_map ( |(owner, leaves)|
        leaves . iter () . enumerate () . map (
          move |(position, leaf)|
          ( leaf . key . clone (), (owner . clone (), position) )) )
      . collect ();
    let order_only_pids : BTreeSet<ID> =
      pair . after . nodes . iter ()
      . filter ( |(pid, after)|
        ! leaves_by_owner . contains_key (*pid)
        && pair . before . nodes . get (*pid)
             . is_some_and ( |before| before != *after ))
      . map ( |(pid, _)| pid . clone () )
      . collect ();
    let order_only_files : BTreeSet<(SkgrepoName, ID)> =
      order_only_pids . iter ()
      . filter_map ( |pid| pair . after . nodes . get (pid) )
      . map ( |node| ( node . home_skgrepo . clone (), node . pid . clone () ))
      . collect ();
    ChangeForest { leaves_by_owner, index, order_only_pids,
                   order_only_files,
                   foreign_changed_files : pair . foreign_changed_files } }

  /// The nodes that own a leaf (the forest's roots).
  pub fn owners (
    &self,
  ) -> impl Iterator<Item = &ID> {
    self . leaves_by_owner . keys () }

  pub fn leaf (
    &self,
    key : &LeafKey,
  ) -> Option<&ChangeLeaf> {
    let (owner, position) : &(ID, usize) = self . index . get (key) ?;
    self . leaves_by_owner . get (owner) ? . get (*position) }

  /// None when 'key' is not a leaf of this forest: that change does
  /// not exist (any more).
  pub fn leaf_fingerprint (
    &self,
    key : &LeafKey,
  ) -> Option<u64> {
    self . leaf (key) . map ( |leaf| leaf . fingerprint ) }

  /// The leaves a node owns: its own aspects, and the relationships
  /// it records.
  pub fn leaves_under (
    &self,
    pid : &ID,
  ) -> &[ChangeLeaf] {
    self . leaves_by_owner . get (pid)
      . map ( |leaves| leaves . as_slice () )
      . unwrap_or (&[]) }

  /// The leaves that the file of 'pid' in 'skgrepo' records.
  pub fn leaves_in_file (
    &self,
    skgrepo : &SkgrepoName,
    pid     : &ID,
  ) -> Vec<&ChangeLeaf> {
    let file : (SkgrepoName, ID) = ( skgrepo . clone (), pid . clone () );
    self . leaves_by_owner . values () . flatten ()
      . filter ( |leaf| leaf . files . contains (&file) )
      . collect () }

  /// Every changed file of the owned skgrepos, order-only ones
  /// included, as the pids changed in each skgrepo.
  pub fn files_changed (
    &self,
  ) -> BTreeMap<SkgrepoName, BTreeSet<ID>> {
    let mut files : BTreeMap<SkgrepoName, BTreeSet<ID>> = BTreeMap::new ();
    for (skgrepo, pid) in
      self . leaves_by_owner . values () . flatten ()
      . flat_map ( |leaf| leaf . files . iter () )
      . chain ( self . order_only_files . iter () )
    { files . entry (skgrepo . clone ()) . or_default ()
        . insert (pid . clone ()); }
    files }

  pub fn order_only_pids (
    &self,
  ) -> &BTreeSet<ID> {
    &self . order_only_pids }

  /// Whether every key among 'keys' that is a leaf of this forest is
  /// marked ready with its current fingerprint. Keys that are not
  /// leaves (unchanged things) are never asked.
  pub fn readiness_of_leaves<'a> (
    &self,
    store : &ReadinessStore,
    keys  : impl IntoIterator<Item = &'a LeafKey>,
  ) -> bool {
    keys . into_iter ()
      . filter_map ( |key| self . leaf (key) )
      . all ( |leaf| store . is_ready (&leaf . key, leaf . fingerprint) ) }

  /// A file is ready iff every leaf it records is. A file whose only
  /// change is order has no leaf, and so is ready.
  pub fn readiness_of_file (
    &self,
    store   : &ReadinessStore,
    skgrepo : &SkgrepoName,
    pid     : &ID,
  ) -> bool {
    self . readiness_of_leaves (
      store,
      self . leaves_in_file (skgrepo, pid) . into_iter ()
        . map ( |leaf| &leaf . key )) }

  pub fn readiness_summary (
    &self,
    store : &ReadinessStore,
  ) -> ReadinessSummary {
    let files : Vec<(SkgrepoName, ID)> =
      self . files_changed () . into_iter ()
      . flat_map ( |(skgrepo, pids)|
        pids . into_iter () . map ( move |pid| ( skgrepo . clone (), pid )) )
      . collect ();
    ReadinessSummary {
      ready_files : files . iter ()
        . filter ( |(skgrepo, pid)|
          self . readiness_of_file (store, skgrepo, pid) )
        . count (),
      changed_files         : files . len (),
      foreign_changed_files : self . foreign_changed_files,
      order_only_files      : self . order_only_files . len (), } }
}

impl ReadinessSummary {
  /// Every changed file of the owned skgrepos is ready.
  pub fn is_complete (
    &self,
  ) -> bool {
    self . ready_files == self . changed_files }
}

/// The leaves owned by 'pid': its existence when it is new or
/// deleted, else its changed aspects; then the relationships it
/// records whose existence or relRepo changed.
fn leaves_owned_by (
  pair : &GraphPair,
  pid  : &ID,
) -> Vec<ChangeLeaf> {
  let before : Option<&GraphnodeInRust> = pair . before . get (pid);
  let after  : Option<&GraphnodeInRust> = pair . after  . get (pid);
  let home_file = |node : &GraphnodeInRust| -> Vec<(SkgrepoName, ID)> {
    vec! [ ( node . home_skgrepo . clone (), pid . clone () ) ] };
  let node_leaf = |aspect : NodeAspect, fingerprint : u64,
                   files : Vec<(SkgrepoName, ID)>| -> ChangeLeaf {
    ChangeLeaf { key : LeafKey::Node { pid : pid . clone (), aspect },
                 fingerprint, files } };
  let mut leaves : Vec<ChangeLeaf> = Vec::new ();
  match (before, after) {
    (Some (before), Some (after)) => {
      let changes : NodeChanges = pair . node_changes (pid);
      if changes . text_changed () {
        leaves . push ( node_leaf (
          NodeAspect::Text,
          fingerprint_of (&( &before . title, &before . body,
                             &after  . title, &after  . body )),
          home_file (after) )); }
      if let Some ((old, new)) = &changes . home {
        leaves . push ( node_leaf (
          NodeAspect::Home,
          fingerprint_of (&(old, new)),
          vec! [ ( old . clone (), pid . clone () ),
                 ( new . clone (), pid . clone () ) ] )); }
      for (gained, alias) in
        changes . aliases_gained . iter () . map ( |a| (true, a) )
        . chain ( changes . aliases_lost . iter () . map ( |a| (false, a) ))
      { leaves . push ( node_leaf (
          NodeAspect::Alias (alias . clone ()),
          fingerprint_of (&( ! gained, gained )),
          home_file (after) )); }
      for (gained, extra_id) in
        changes . extra_ids_gained . iter () . map ( |i| (true, i) )
        . chain ( changes . extra_ids_lost . iter () . map ( |i| (false, i) ))
      { leaves . push ( node_leaf (
          NodeAspect::ExtraId (extra_id . clone ()),
          fingerprint_of (&( ! gained, gained )),
          home_file (after) )); }
      for (gained, flag) in
        changes . flags_gained . iter () . map ( |f| (true, f) )
        . chain ( changes . flags_lost . iter () . map ( |f| (false, f) ))
      { leaves . push ( node_leaf (
          NodeAspect::Flag (*flag),
          fingerprint_of (&( ! gained, gained )),
          home_file (after) )); }},
    (before, after) => {
      // A new or deleted node: its existence is one leaf, whose
      // fingerprint covers its own (non-relationship) content.
      let existing : &GraphnodeInRust =
        after . or (before)
        . expect ("a changed node is in one graph or the other");
      leaves . push ( node_leaf (
        NodeAspect::Existence,
        fingerprint_of (&( before . map (own_content_of),
                           after  . map (own_content_of) )),
        home_file (existing) )); }}
  for role in ROLES_OF_LISTED_MEMBERS {
    for partner in pair . partners (pid, role) {
      leaves . extend ( relationship_leaves (pid, role, &partner) ); }}
  leaves }

/// The leaves of the relationship by which 'recorder' lists
/// 'partner' (in the partner's 'role').
fn relationship_leaves (
  recorder : &ID,
  role     : RelationRole,
  partner  : &Partner,
) -> Vec<ChangeLeaf> {
  let key = |aspect : RelationshipAspect| -> LeafKey {
    LeafKey::Relationship {
      relation : role . relation,
      recorder : recorder . clone (),
      member   : partner . id . clone (),
      aspect } };
  let file_in = |skgrepo : &SkgrepoName| -> (SkgrepoName, ID) {
    ( skgrepo . clone (), recorder . clone () ) };
  if partner . presence != Presence::Both {
    let relRepo : &SkgrepoName =
      partner . relRepo_after . as_ref ()
      . or (partner . relRepo_before . as_ref ())
      . expect ("a partner is in one graph or the other");
    vec! [ ChangeLeaf {
      key         : key (RelationshipAspect::Existence),
      fingerprint : fingerprint_of (&( partner . relRepo_before . is_some (),
                                       partner . relRepo_after  . is_some () )),
      files       : vec! [ file_in (relRepo) ], } ]
  } else if partner . relRepo_moved () {
    vec! [ ChangeLeaf {
      key         : key (RelationshipAspect::RelRepo),
      fingerprint : fingerprint_of (&( &partner . relRepo_before,
                                       &partner . relRepo_after )),
      files       : [ &partner . relRepo_before, &partner . relRepo_after ]
                    . into_iter () . flatten () . map (file_in) . collect (), } ]
  } else { Vec::new () }}

/// A node's own content, without its relationships: what a new or
/// deleted node's existence leaf approves.
fn own_content_of (
  node : &GraphnodeInRust,
) -> ( &String, &Option<String>, Vec<String>, &Vec<ID>,
       &Vec<Flag>, &SkgrepoName ) {
  ( &node . title,
    &node . body,
    members_msv (&node . aliases) . or_default () . to_vec (),
    &node . extra_ids,
    &node . flags,
    &node . home_skgrepo ) }

/// The fingerprint of a leaf's before and after values.
fn fingerprint_of<T : Hash> (
  values : &T,
) -> u64 {
  let mut hasher : DefaultHasher = DefaultHasher::new ();
  values . hash (&mut hasher);
  hasher . finish () }

#[cfg(test)]
#[allow(non_snake_case)]
#[path = "../tests/unit/change_forest.rs"]
mod tests;
