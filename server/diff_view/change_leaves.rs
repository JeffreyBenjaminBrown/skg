//! The change-forest: the tree of everything commit-readiness can be
//! asked about, built from the graph straddle, independent of any view
//! ('docs/diff-view.org', "Change-forest leaves"). Its roots
//! are the changed graphnodes that record a change; under each come its
//! changed aspects; its leaves are the individual changes a user
//! marks. A relationship's leaves live under its recorder.
//!
//! The readiness of an aggregate (a set of leaves, a file, the whole
//! changeset) is derived here from a 'ReadinessStore'.

use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::in_rust_graph::relation_accessors::{
  RelationRole, RelationshipKey};
use crate::graph_straddle::types::{GraphStraddle, NodeChanges, Partner, Presence};
use crate::graph_straddle::union_walk::ROLES_OF_LISTED_MEMBERS;
use crate::diff_view::readiness_store::ReadinessStore;
use crate::serve::util::format_string_list_sexp;
use crate::types::misc::{ID, SkgrepoName};
use crate::types::nodes::complete::Flag;
use crate::types::nodes::rust::GraphnodeInRust;

use serde::{Deserialize, Serialize};
use sexp::{Atom, Sexp};
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

/// One change a user can mark commit-ready.
#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub enum LeafKey {
  Node {
    pid    : ID,
    aspect : NodeAspect, },
  /// A relationship. Seen from either end, it is the same leaf.
  /// Flattened, so that a marks file spells it as one object of
  /// 'relation', 'recorder', 'member' and 'aspect'.
  Relationship {
    #[serde(flatten)]
    relationship : RelationshipKey,
    aspect       : RelationshipAspect, },
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, Serialize, Deserialize)]
pub enum NodeAspect {
  Existence,
  Text,
  #[serde(rename = "Home")] // the spelling marks files already on disk use
  HomeSkgrepo,
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
/// stands for, and the files that record it.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ChangeLeaf {
  pub key         : LeafKey,
  pub fingerprint : u64,
  pub files       : Vec<ChangedFile>,
}

/// The '.skg' file of node 'pid' in skgrepo 'skgrepo', as a file that
/// records a change.
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct ChangedFile {
  pub skgrepo : SkgrepoName,
  pub pid     : ID,
}

/// What the client shows after a save of a diff-view buffer.
/// Order-only files count as changed and ready. Foreign files are
/// counted apart, and never ready.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct ReadinessSummary {
  pub ready_files           : usize,
  pub changed_files         : usize,
  pub foreign_changed_files : usize,
  pub order_only_files      : usize,
}

/// The readiness summary a save of a diff-view buffer reports: that of the
/// whole changeset, and how many of its leaves are not editable in
/// the buffer (their recorder is not among the nodes
/// 'nodes_editable_in_diffView' returns for it).
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct BufferReadinessSummary {
  pub summary                   : ReadinessSummary,
  pub changes_not_editable_here : usize,
}

impl BufferReadinessSummary {
  /// '(readiness-summary (ready N) (changed M) (foreign F)
  /// (order-only O) (notEditable K))'.
  pub fn to_sexp (
    &self,
  ) -> Sexp {
    let entry = |key : &str, count : usize| -> Sexp {
      Sexp::List ( vec! [ Sexp::Atom ( Atom::S ( key . to_string () )),
                          Sexp::Atom ( Atom::I ( count as i64 )) ] ) };
    Sexp::List ( vec! [
      Sexp::Atom ( Atom::S ( "readiness-summary" . to_string () )),
      entry ( "ready",       self . summary . ready_files ),
      entry ( "changed",     self . summary . changed_files ),
      entry ( "foreign",     self . summary . foreign_changed_files ),
      entry ( "order-only",  self . summary . order_only_files ),
      entry ( "notEditable", self . changes_not_editable_here ), ] ) }
}

/// The response of a request that can report a readiness summary:
///   (LEADING... (readiness-summary ...) (errors (...)) (warnings (...)))
/// 'leading' are the request's own entries; the summary is there when
/// one is given.
pub fn readiness_response_sexp (
  leading  : Vec<Sexp>,
  summary  : Option<&BufferReadinessSummary>,
  errors   : &[String],
  warnings : &[String],
) -> Sexp {
  Sexp::List (
    leading . into_iter ()
      . chain ( summary . map ( |summary| summary . to_sexp () ))
      . chain ( [ format_string_list_sexp ("errors", errors),
                  format_string_list_sexp ("warnings", warnings) ] )
      . collect () ) }

/// See the module comment.
#[derive(Clone, Debug, Default)]
pub struct ChangeLeaves {
  /// Each node that records a leaf, with its leaves.
  leaves_by_recorder    : BTreeMap<ID, Vec<ChangeLeaf>>,
  /// Where each leaf is in 'leaves_by_recorder': (recorder, position).
  index                 : HashMap<LeafKey, (ID, usize)>,
  order_only_pids       : BTreeSet<ID>,
  order_only_files      : BTreeSet<ChangedFile>,
  foreign_changed_files : usize,
}

impl ChangeLeaves {
  /// Only the nodes that differ between the two graphs, and those
  /// that record a member the two graphs resolve differently, are
  /// compared ('recorders_of_reresolved_members'): no other node can
  /// record a change.
  pub fn from_graph_straddle (
    straddle : &GraphStraddle,
  ) -> ChangeLeaves {
    let differing : BTreeSet<ID> =
      nodes_differing_between_the_two_graphs (straddle);
    let compared : BTreeSet<ID> =
      differing . iter () . cloned ()
      . chain ( recorders_of_reresolved_members (straddle, &differing) )
      . collect ();
    let mut leaves_by_recorder : BTreeMap<ID, Vec<ChangeLeaf>> =
      BTreeMap::new ();
    for pid in &compared {
      let leaves : Vec<ChangeLeaf> = leaves_recorded_by (straddle, pid);
      if ! leaves . is_empty () {
        leaves_by_recorder . insert (pid . clone (), leaves); }}
    let index : HashMap<LeafKey, (ID, usize)> =
      leaves_by_recorder . iter ()
      . flat_map ( |(recorder, leaves)|
        leaves . iter () . enumerate () . map (
          move |(position, leaf)|
          ( leaf . key . clone (), (recorder . clone (), position) )) )
      . collect ();
    let order_only_pids : BTreeSet<ID> =
      differing . into_iter ()
      . filter ( |pid|
        ! leaves_by_recorder . contains_key (pid)
        && straddle . node_presence (pid) == Some (Presence::Both) )
      . collect ();
    let order_only_files : BTreeSet<ChangedFile> =
      order_only_pids . iter ()
      . filter_map ( |pid| straddle . after . nodes . get (pid) )
      . map ( |node| ChangedFile { skgrepo : node . home_skgrepo . clone (),
                                   pid     : node . pid . clone () } )
      . collect ();
    ChangeLeaves { leaves_by_recorder, index, order_only_pids,
                   order_only_files,
                   foreign_changed_files : straddle . foreign_changed_files } }

  /// The nodes that record a leaf (the forest's roots).
  pub fn recorders (
    &self,
  ) -> impl Iterator<Item = &ID> {
    self . leaves_by_recorder . keys () }

  pub fn leaf (
    &self,
    key : &LeafKey,
  ) -> Option<&ChangeLeaf> {
    let (recorder, position) : &(ID, usize) = self . index . get (key) ?;
    self . leaves_by_recorder . get (recorder) ? . get (*position) }

  /// None when 'key' is not a leaf of this forest: that change does
  /// not exist (any more).
  pub fn leaf_fingerprint (
    &self,
    key : &LeafKey,
  ) -> Option<u64> {
    self . leaf (key) . map ( |leaf| leaf . fingerprint ) }

  /// The leaves a node records: its own aspects, and the relationships
  /// it records.
  pub fn leaves_under (
    &self,
    pid : &ID,
  ) -> &[ChangeLeaf] {
    self . leaves_by_recorder . get (pid)
      . map ( |leaves| leaves . as_slice () )
      . unwrap_or (&[]) }

  /// The leaves that 'file' records.
  pub fn leaves_in_file (
    &self,
    file : &ChangedFile,
  ) -> Vec<&ChangeLeaf> {
    self . leaves_by_recorder . values () . flatten ()
      . filter ( |leaf| leaf . files . contains (file) )
      . collect () }

  /// Every changed file of the owned skgrepos, order-only ones
  /// included.
  pub fn files_changed (
    &self,
  ) -> BTreeSet<ChangedFile> {
    self . leaves_by_recorder . values () . flatten ()
      . flat_map ( |leaf| leaf . files . iter () )
      . chain ( self . order_only_files . iter () )
      . cloned ()
      . collect () }

  pub fn order_only_pids (
    &self,
  ) -> &BTreeSet<ID> {
    &self . order_only_pids }

  /// Whether every key among 'keys' that is a leaf of this forest is
  /// marked ready with its current fingerprint. Keys that are not
  /// leaves (unchanged things) are never asked.
  pub fn readiness_of_leaves<'a> (
    &self,
    store         : &ReadinessStore,
    keys  : impl IntoIterator<Item = &'a LeafKey>,
  ) -> bool {
    keys . into_iter ()
      . filter_map ( |key| self . leaf (key) )
      . all ( |leaf| store . is_ready (&leaf . key, leaf . fingerprint) ) }

  /// A file is ready iff every leaf it records is. A file whose only
  /// change is order has no leaf, and so is ready.
  pub fn readiness_of_file (
    &self,
    store : &ReadinessStore,
    file  : &ChangedFile,
  ) -> bool {
    self . readiness_of_leaves (
      store,
      self . leaves_in_file (file) . into_iter ()
        . map ( |leaf| &leaf . key )) }

  pub fn readiness_summary (
    &self,
    store         : &ReadinessStore,
  ) -> ReadinessSummary {
    let files : BTreeSet<ChangedFile> = self . files_changed ();
    ReadinessSummary {
      ready_files : files . iter ()
        . filter ( |file| self . readiness_of_file (store, file) )
        . count (),
      changed_files         : files . len (),
      foreign_changed_files : self . foreign_changed_files,
      order_only_files      : self . order_only_files . len (), } }

  /// The readiness summary, with the count of leaves whose recorder
  /// is not among 'editable_here' (the nodes a buffer makes editable,
  /// 'nodes_editable_in_diffView').
  pub fn buffer_readiness_summary (
    &self,
    store         : &ReadinessStore,
    editable_here : &HashSet<ID>,
  ) -> BufferReadinessSummary {
    BufferReadinessSummary {
      summary                   : self . readiness_summary (store),
      changes_not_editable_here : self . leaves_by_recorder . iter ()
        . filter ( |(recorder, _)| ! editable_here . contains (*recorder) )
        . map ( |(_, leaves)| leaves . len () )
        . sum (), } }
}

impl ReadinessSummary {
  /// Every changed file of the owned skgrepos is ready.
  pub fn is_complete (
    &self,
  ) -> bool {
    self . ready_files == self . changed_files }
}

/// The nodes of either graph that the other lacks or holds unequal.
fn nodes_differing_between_the_two_graphs (
  straddle : &GraphStraddle,
) -> BTreeSet<ID> {
  let in_before : Vec<&ID> = // unequal or only before
    straddle . before . nodes . iter ()
    . filter ( |(pid, before)|
      straddle . after . nodes . get (*pid) != Some (*before) )
    . map ( |(pid, _)| pid )
    . collect ();
  let only_after : Vec<&ID> =
    straddle . after . nodes . keys ()
    . filter ( |pid| ! straddle . before . nodes . contains_key (*pid) )
    . collect ();
  in_before . into_iter () . chain (only_after) . cloned () . collect () }

/// The nodes that list a member whose ID the two graphs resolve to
/// different pids. After a nodeMerge since HEAD, a container of the
/// acquiree that still lists the acquiree's ID is equal in both graphs,
/// but the after-graph resolves that ID to the acquirer, so the
/// container records a changed relationship. An ID resolves
/// differently only when it is an extra ID of some node in one graph
/// and not of the same node in the other, so such IDs are found among
/// the extra-ID differences of the nodes in 'differing' (those that
/// differ between the graphs), and their recorders through each
/// graph's recorderward relmaps.
fn recorders_of_reresolved_members (
  straddle  : &GraphStraddle,
  differing : &BTreeSet<ID>,
) -> BTreeSet<ID> {
  let extra_ids_in = |graph : &InRustGraph, pid : &ID| -> BTreeSet<ID> {
    graph . get (pid)
      . map ( |node| node . extra_ids . iter () . cloned () . collect () )
      . unwrap_or_default () };
  let member_key_in = |graph : &InRustGraph, skgid : &ID| -> ID {
    graph . relationship_member_key (skgid) . into_id () };
  let reresolved : BTreeSet<ID> =
    differing . iter ()
    . flat_map ( |pid|
      extra_ids_in (&straddle . before, pid)
      . symmetric_difference ( &extra_ids_in (&straddle . after, pid) )
      . cloned () . collect::<Vec<ID>> () )
    . filter ( |skgid| member_key_in (&straddle . before, skgid)
                       != member_key_in (&straddle . after, skgid) )
    . collect ();
  let mut recorders : BTreeSet<ID> = BTreeSet::new ();
  for skgid in &reresolved {
    for graph in [ &straddle . before, &straddle . after ] {
      let member : ID = member_key_in (graph, skgid);
      for role in ROLES_OF_LISTED_MEMBERS {
        recorders . extend (
          graph . inbound_pids_for_relation (&member, role . relation) ); }} }
  recorders }

/// The leaves recorded by 'pid': its existence when it is new or
/// deleted, else its changed aspects; then the relationships it
/// records whose existence or relRepo changed.
fn leaves_recorded_by (
  straddle : &GraphStraddle,
  pid      : &ID,
) -> Vec<ChangeLeaf> {
  let before : Option<&GraphnodeInRust> = straddle . before . get (pid);
  let after  : Option<&GraphnodeInRust> = straddle . after  . get (pid);
  let file_in = |skgrepo : &SkgrepoName| -> ChangedFile {
    ChangedFile { skgrepo : skgrepo . clone (), pid : pid . clone () } };
  let home_file = |node : &GraphnodeInRust| -> Vec<ChangedFile> {
    vec! [ file_in (&node . home_skgrepo) ] };
  let node_leaf = |aspect : NodeAspect, fingerprint : u64,
                   files : Vec<ChangedFile>| -> ChangeLeaf {
    ChangeLeaf { key : LeafKey::Node { pid : pid . clone (), aspect },
                 fingerprint, files } };
  let mut leaves : Vec<ChangeLeaf> = Vec::new ();
  match (before, after) {
    (Some (before), Some (after)) => {
      let changes : NodeChanges = straddle . node_changes (pid);
      if changes . text_changed () {
        leaves . push ( node_leaf (
          NodeAspect::Text,
          fingerprint_of (&( &before . title, &before . body,
                             &after  . title, &after  . body )),
          home_file (after) )); }
      if let Some (home) = &changes . home {
        leaves . push ( node_leaf (
          NodeAspect::HomeSkgrepo,
          fingerprint_of (&( &home . old, &home . new )),
          vec! [ file_in (&home . old), file_in (&home . new) ] )); }
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
    for partner in straddle . partners (pid, role) {
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
      relationship : RelationshipKey {
        relation : role . relation,
        recorder : recorder . clone (),
        member   : partner . id . clone () },
      aspect } };
  let file_in = |skgrepo : &SkgrepoName| -> ChangedFile {
    ChangedFile { skgrepo : skgrepo . clone (), pid : recorder . clone () } };
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
    node . alias_texts (),
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
#[path = "../../tests/unit/change_leaves.rs"]
mod tests;
