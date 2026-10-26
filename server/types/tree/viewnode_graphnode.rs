/// Node access utilities for ego_tree::Tree<Viewnode>

use crate::types::misc::ID;
use crate::types::viewnode::{Viewnode, ViewnodeKind};
use crate::types::viewnode::Vognode;

use crate::refresh::ancestry::parent_reads_claims;
use ego_tree::{Tree, NodeId, NodeRef};
use std::collections::{HashMap, HashSet};

/// The ID of this node if it is an occurrence,
/// else that of its nearest occurrence ancestor.
/// Returns an error if no ancestor has an ID (e.g., reached BufferRoot).
pub fn gnid_from_self_or_nearest_ancestor (
  tree    : &Tree<Viewnode>,
  treeid : NodeId,
) -> Result<ID, String> {
  let mut node : NodeRef<Viewnode> =
    tree . get (treeid)
    . ok_or ("id_from_self_or_nearest_ancestor: node not found")?;
  loop {
    match &node . value() . kind {
      ViewnodeKind::Vognode (Vognode::Occurrence (t)) =>
        return Ok (t . gnid . clone ()),
      _ => {} }
    node = node . parent()
      . ok_or ("id_from_self_or_nearest_ancestor: reached root without finding ID")?; }}

/// Find a child node by its ID.
/// Returns the NodeId of the child if found, None otherwise.
pub fn find_child_by_gnid (
  tree          : & Tree<Viewnode>,
  parent_treeid : NodeId,
  target_gnid  : & ID,
) -> Option < NodeId > {
  let singleton : HashSet<ID> =
    std::iter::once( target_gnid . clone() )
    . collect();
  find_children_by_gnids( tree, parent_treeid, &singleton)
    . remove (target_gnid) }

/// Returns a map from ID to NodeId for children that were found.
/// IDs not found as children are not included in the result.
pub fn find_children_by_gnids (
  tree          : & Tree<Viewnode>,
  parent_treeid : NodeId,
  target_gnids : & HashSet < ID >,
) -> HashMap < ID, NodeId > {
  let mut result : HashMap < ID, NodeId > = HashMap::new();
  for child in tree . get (parent_treeid) . unwrap() . children() {
    match &child . value() . kind {
      ViewnodeKind::Vognode (Vognode::Occurrence (t)) =>
        if target_gnids . contains (&t . gnid)
        { result . insert (t . gnid . clone (), child . id()); },
      _ => {} } }
  result }

/// Check if all nodes at the specified generation satisfy the predicate.
/// Returns true if the generation is empty (vacuously true).
/// Negative generations = ancestors; positive = descendants.
/// If skip_non_content, excludes Occurrences that are non-members.
pub fn generation_includes_only<F> (
  tree                : &Tree<Viewnode>,
  treeid : NodeId,
  generation          : i32,
  skip_non_content    : bool,
  predicate           : F,
) -> bool
where F: Fn (&Viewnode) -> bool
{ collect_generation( tree, treeid, generation, skip_non_content )
    . iter()
    . all ( |&gnid| predicate(
      tree . get (gnid) . unwrap() . value() )) }

/// Check if the generation is nonempty and all nodes satisfy the predicate.
/// Negative generations = ancestors; positive = descendants.
/// If skip_non_content, excludes Occurrences that are non-members.
pub fn generation_exists_and_includes<F> (
  tree                : &Tree<Viewnode>,
  treeid : NodeId,
  generation          : i32,
  skip_non_content    : bool,
  predicate           : F,
) -> bool
where F: Fn (&Viewnode) -> bool
{ let nodes = collect_generation(
    tree, treeid, generation, skip_non_content);
  !nodes . is_empty() &&
    nodes . iter() . all(
      |&gnid| predicate(
        tree . get (gnid) . unwrap() . value() )) }

/// Check if the specified generation is empty.
/// Negative generations = ancestors; positive = descendants.
/// If skip_non_content, excludes Occurrences that are non-members.
pub fn generation_does_not_exist (
  tree                : &Tree<Viewnode>,
  treeid : NodeId,
  generation          : i32,
  skip_non_content    : bool,
) -> bool {
  collect_generation( tree, treeid, generation, skip_non_content
                    ) . is_empty() }

/// Collect NodeIds at a specified generation relative to the given node.
/// Negative generation = ancestors (-1 = parent, -2 = grandparent, etc.)
/// Positive generation = descendants (1 = children, 2 = grandchildren, etc.)
/// Generation 0 returns just the node itself.
/// If 'skip_non_content' is true and generation > 0,
///   then we exclude Occurrences that are non-members.
fn collect_generation (
  tree               : &Tree<Viewnode>,
  treeid : NodeId,
  generation         : i32,
  skip_non_content   : bool,
) -> Vec<NodeId> {
  if generation == 0 {
    return vec![treeid]; }
  let Some (node_ref) = tree . get (treeid)
    else { return vec![]; };
  if generation <= 0 {
    let mut current : NodeRef<'_, Viewnode> =
      node_ref;
    for _ in 0..(-generation) {
      match current . parent() {
        Some (parent) => current = parent,
        None => return vec![], }}
    vec![current . id()] }
  else {
    let mut current_gen : Vec<NodeId> =
      vec![treeid];
    for _ in 0..generation {
      let mut next_gen : Vec<NodeId> =
        vec![];
      for gnid in current_gen {
        if let Some (n) = tree . get (gnid) {
          // Under a parent that does not read its children's claims
          // ('parent_reads_claims') no occurrence claims, whatever it
          // carries.
          let claims_are_read : bool = parent_reads_claims (tree, gnid);
          next_gen . extend(
            n . children()
              . filter(|c| !skip_non_content ||
                          // A phantom has no membership field and is implicitly
                          // a member (content), so it is never filtered here.
                          !matches!(&c . value() . kind,
                                    ViewnodeKind::Vognode (Vognode::Occurrence (t))
                                    if ! t . claimsMembership || ! claims_are_read ))
              . map(|c| c . id()) ); }}
      current_gen = next_gen; }
    current_gen } }

/// Check that no sibling satisfies the predicate.
/// Returns true if the node has no siblings,
/// or if the predicate returns false for all siblings.
/// Short-circuits on the first sibling where the predicate returns true.
pub fn siblings_cannot_include<F> (
  tree      : &Tree<Viewnode>,
  treeid : NodeId,
  predicate : F,
) -> bool
where F: Fn (&Viewnode) -> bool
{ let Some (node_ref) = tree . get (treeid)
    else { return true; };
  let Some (parent_ref) = node_ref . parent()
    else { return true; };
  ! parent_ref . children()
      . filter ( |c| c . id() != treeid )
      . any    ( |c| predicate( c . value() )) }
