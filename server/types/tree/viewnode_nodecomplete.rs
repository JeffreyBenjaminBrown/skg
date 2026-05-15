/// Node access utilities for ego_tree::Tree<ViewNode> and Tree<UncheckedViewNode>

use crate::to_org::util::get_id_from_treenode;
use crate::types::views_state::nodecomplete_from_inrustgraph_or_disk;
use crate::types::misc::{ID, MSV, SkgConfig, SourceName};
use crate::types::viewnode::{
    ViewNode, ViewNodeKind, TrueNode, Scaffold,
    viewnode_from_scaffold };
use crate::types::unchecked_viewnode::{
    UncheckedViewNode, UncheckedViewNodeKind };
use crate::types::nodes::complete::NodeComplete;
use crate::types::list::dedup_vector;
use super::generic::{ unique_scaffold_child, write_at_node_in_tree, with_node_mut };

use ego_tree::{Tree, NodeId, NodeRef};
use std::collections::{HashMap, HashSet};
use std::error::Error;

/// Apply a mutating function to the TrueNode at the given tree position.
/// Errors if the node is not found or is not a TrueNode.
pub fn write_at_truenode_in_tree<F, R> (
  tree   : &mut Tree<ViewNode>,
  treeid : NodeId,
  f      : F,
) -> Result<R, String>
where F: FnOnce (&mut TrueNode) -> R {
  write_at_node_in_tree ( tree, treeid, |viewnode| {
    match &mut viewnode . kind {
      ViewNodeKind::True (t) => Ok ( f (t) ),
      _ => Err ( "write_at_truenode_in_tree: expected TrueNode"
                   . to_string () ) }} ) ? }

/// Extract (ID, source) from a TrueNode in the tree.
/// Returns an error if the node is not found or not a TrueNode.
pub fn pid_and_source_from_treenode (
  tree        : &Tree<ViewNode>,
  treeid      : NodeId,
  caller_name : &str,
) -> Result<(ID, SourceName), Box<dyn Error>> {
  let node_ref : NodeRef<ViewNode> =
    tree . get (treeid) . ok_or_else ( ||
      format! ( "{}: node not found", caller_name ) ) ?;
  match &node_ref . value() . kind {
    ViewNodeKind::True (t) =>
      Ok (( t . id . clone(), t . source . clone() )),
    ViewNodeKind::Deleted (d) =>
      Ok (( d . id . clone(), d . source . clone() )),
    _ => Err ( format! ( "{}: expected TrueNode or Deleted",
                         caller_name ) . into() ),
  }}

/// Get the ID from this node if it's an UncheckedTrueNode with an ID,
/// otherwise recursively try ancestors.
/// Returns an error if no ancestor has an ID (e.g., reached BufferRoot).
pub fn id_from_self_or_nearest_ancestor (
  tree    : &Tree<UncheckedViewNode>,
  node_id : NodeId,
) -> Result<ID, String> {
  let mut node : NodeRef<UncheckedViewNode> =
    tree . get (node_id)
    . ok_or ("id_from_self_or_nearest_ancestor: node not found")?;
  loop {
    if let UncheckedViewNodeKind::True (t) = &node . value() . kind {
      if let Some (id) = &t . id {
        return Ok(id . clone()); }}
    node = node . parent()
      . ok_or ("id_from_self_or_nearest_ancestor: reached root without finding ID")?; }}

/// Find the unique child of a node with a given Scaffold.
/// Returns None if no child has the kind,
/// Some(child_id) if exactly one does,
/// or an error if multiple children have it.
pub fn unique_scaffold_child_of_viewnode (
  tree          : &Tree<ViewNode>,
  node_id       : NodeId,
  scaffold_kind : &Scaffold,
) -> Result<Option<NodeId>, Box<dyn Error>> {
  unique_scaffold_child (
    tree,
    node_id,
    scaffold_kind,
    |child : &ViewNode| match &child . kind {
      ViewNodeKind::Scaff (s) => Some (s),
      _ => None,
    })
  . map_err (|e| -> Box<dyn Error> { e . into() }) }

/// Extract PIDs for the subscriber and its subscribees.
pub fn pids_for_subscriber_and_its_subscribees (
  tree    : &Tree<ViewNode>,
  node_id : NodeId,
  config  : &SkgConfig,
) -> Result < ( ID, Vec < ID > ),
              Box<dyn Error> > {
  let (pid, source) : (ID, SourceName) =
    pid_and_source_from_treenode (
      tree, node_id, "pids_for_subscriber_and_its_subscribees" ) ?;
  let nodecomplete : NodeComplete =
    nodecomplete_from_inrustgraph_or_disk ( config, &pid, &source ) ?;
  Ok (( nodecomplete . pid . clone (),
        nodecomplete . subscribes_to . or_default() . to_vec() )) }

/// Extract PIDs for a Subscribee and its grandparent (the subscriber).
/// Expects: subscriber -> SubscribeeCol -> Subscribee (this node)
pub fn pid_for_subscribee_and_its_subscriber_grandparent (
  tree    : &Tree<ViewNode>,
  node_id : NodeId,
  config  : &SkgConfig,
) -> Result < ( ID, ID ), Box<dyn Error> > {
  let subscribee_pid : ID = get_id_from_treenode ( tree, node_id ) ?;
  let node_ref : NodeRef < ViewNode > =
    tree . get (node_id) . ok_or (
      "pid_for_subscribee_and_its_subscriber_grandparent: node not found" ) ?;
  let parent_ref : NodeRef < ViewNode > =
    node_ref . parent ()
    . ok_or ("Subscribee has no parent (SubscribeeCol)") ?;
  if ! matches! ( &parent_ref . value () . kind,
                  ViewNodeKind::Scaff (Scaffold::SubscribeeCol)) {
    return Err ( "Subscribee's parent is not a SubscribeeCol" .
                 into () ); }
  let grandparent_ref : NodeRef < ViewNode > =
    parent_ref . parent ()
    . ok_or ("SubscribeeCol has no parent (subscriber)") ?;
  let (subscriber_id, subscriber_source) : (ID, SourceName) =
    pid_and_source_from_treenode (
      tree, grandparent_ref . id (),
      "pid_for_subscribee_and_its_subscriber_grandparent" ) ?;
  let nodecomplete : NodeComplete =
    nodecomplete_from_inrustgraph_or_disk (
      config, &subscriber_id, &subscriber_source ) ?;
  Ok (( subscribee_pid,
        nodecomplete . pid . clone() )) }

pub fn insert_scaffold_as_child (
  tree          : &mut Tree<ViewNode>,
  parent_id     : NodeId,
  scaffold_kind : Scaffold,
  prepend       : bool, // otherwise, append
) -> Result < NodeId, Box<dyn Error> > {
  let viewnode : ViewNode =
    viewnode_from_scaffold (scaffold_kind);
  let col_id : NodeId = with_node_mut (
    tree, parent_id,
    |mut parent_mut| {
      if prepend { parent_mut . prepend (viewnode) . id () }
      else       { parent_mut . append  (viewnode) . id () } } )
    . map_err ( |e| -> Box<dyn Error> { e . into() } ) ?;
  Ok (col_id) }

/// Collect aliases for a node:
/// - find the unique AliasCol child (error if multiple)
/// - for each Alias child of the AliasCol, collect its title
/// Duplicates are removed (preserving order of first occurrence).
/// Returns None ("no opinion") if no AliasCol found.
/// Returns Some(vec) if AliasCol found, even if empty.
pub fn collect_grandchild_aliases_for_viewnode (
  tree: &Tree<ViewNode>,
  node_id: NodeId,
) -> Result<MSV<String>, String> {
  let alias_col_id : Option<NodeId> =
    unique_scaffold_child_of_viewnode (
      tree, node_id, &Scaffold::AliasCol )
    . map_err ( |e| e . to_string() ) ?;
  match alias_col_id {
    None => Ok (MSV::Unspecified),
    Some (col_id) => {
      let aliases : Vec<String> = {
        let col_ref : NodeRef<ViewNode> =
          tree . get (col_id) . expect ("collect_grandchild_aliases_for_viewnode: AliasCol not found");
        let mut aliases : Vec<String> = Vec::new();
        for alias_child in col_ref . children() {
          { // check for invalid state
            if ! matches!(&alias_child . value() . kind,
                          ViewNodeKind::Scaff(Scaffold::Alias { .. } )) {
              return Err ( format! (
                "AliasCol has non-Alias child with kind: {:?}",
                alias_child . value() . kind )); }}
          aliases . push(
            alias_child . value() . title() . to_string() ); }
        aliases };
      Ok( MSV::Specified(dedup_vector (aliases)) ) }} }

/// Find a child node by its ID.
/// Returns the NodeId of the child if found, None otherwise.
pub fn find_child_by_id (
  tree          : & Tree<ViewNode>,
  parent_treeid : NodeId,
  target_skgid  : & ID,
) -> Option < NodeId > {
  let singleton : HashSet<ID> =
    std::iter::once( target_skgid . clone() )
    . collect();
  find_children_by_ids( tree, parent_treeid, &singleton)
    . remove (target_skgid) }

/// Returns a map from ID to NodeId for children that were found.
/// IDs not found as children are not included in the result.
pub fn find_children_by_ids (
  tree          : & Tree<ViewNode>,
  parent_treeid : NodeId,
  target_skgids : & HashSet < ID >,
) -> HashMap < ID, NodeId > {
  let mut result : HashMap < ID, NodeId > = HashMap::new();
  for child in tree . get (parent_treeid) . unwrap() . children() {
    match &child . value() . kind {
      ViewNodeKind::True (t) =>
        if target_skgids . contains(&t . id)
        { result . insert(t . id . clone(), child . id()); },
      ViewNodeKind::Deleted (d) =>
        if target_skgids . contains(&d . id)
        { result . insert(d . id . clone(), child . id()); },
      _ => {} } }
  result }

/// Check if all nodes at the specified generation satisfy the predicate.
/// Returns true if the generation is empty (vacuously true).
/// Negative generations = ancestors; positive = descendants.
/// If skip_non_content, excludes TrueNodes with birth != ContentOf.
pub fn generation_includes_only<F> (
  tree                : &Tree<UncheckedViewNode>,
  node_id             : NodeId,
  generation          : i32,
  skip_non_content    : bool,
  predicate           : F,
) -> bool
where F: Fn (&UncheckedViewNode) -> bool
{ collect_generation( tree, node_id, generation, skip_non_content )
    . iter()
    . all ( |&id| predicate(
      tree . get (id) . unwrap() . value() )) }

/// Check if the generation is nonempty and all nodes satisfy the predicate.
/// Negative generations = ancestors; positive = descendants.
/// If skip_non_content, excludes TrueNodes with birth != ContentOf.
pub fn generation_exists_and_includes<F> (
  tree                : &Tree<UncheckedViewNode>,
  node_id             : NodeId,
  generation          : i32,
  skip_non_content    : bool,
  predicate           : F,
) -> bool
where F: Fn (&UncheckedViewNode) -> bool
{ let nodes = collect_generation(
    tree, node_id, generation, skip_non_content);
  !nodes . is_empty() &&
    nodes . iter() . all(
      |&id| predicate(
        tree . get (id) . unwrap() . value() )) }

/// Check if the specified generation is empty.
/// Negative generations = ancestors; positive = descendants.
/// If skip_non_content, excludes TrueNodes with birth != ContentOf.
pub fn generation_does_not_exist (
  tree                : &Tree<UncheckedViewNode>,
  node_id             : NodeId,
  generation          : i32,
  skip_non_content    : bool,
) -> bool {
  collect_generation( tree, node_id, generation, skip_non_content
                    ) . is_empty() }

/// Collect NodeIds at a specified generation relative to the given node.
/// Negative generation = ancestors (-1 = parent, -2 = grandparent, etc.)
/// Positive generation = descendants (1 = children, 2 = grandchildren, etc.)
/// Generation 0 returns just the node itself.
/// If 'skip_non_content' is true and generation > 0,
///   then we exclude TrueNodes with birth != ContentOf.
fn collect_generation (
  tree               : &Tree<UncheckedViewNode>,
  node_id            : NodeId,
  generation         : i32,
  skip_non_content   : bool,
) -> Vec<NodeId> {
  if generation == 0 {
    return vec![node_id]; }
  let Some (node_ref) = tree . get (node_id)
    else { return vec![]; };
  if generation <= 0 {
    let mut current : NodeRef<'_, UncheckedViewNode> =
      node_ref;
    for _ in 0..(-generation) {
      match current . parent() {
        Some (parent) => current = parent,
        None => return vec![], }}
    vec![current . id()] }
  else {
    let mut current_gen : Vec<NodeId> =
      vec![node_id];
    for _ in 0..generation {
      let mut next_gen : Vec<NodeId> =
        vec![];
      for id in current_gen {
        if let Some (n) = tree . get (id) {
          next_gen . extend(
            n . children()
              . filter(|c| !skip_non_content ||
                          !matches!(&c . value() . kind,
                                    UncheckedViewNodeKind::True (t)
                                    if t . parent_ignores_it() ))
              . map(|c| c . id()) ); }}
      current_gen = next_gen; }
    current_gen } }

/// Check that no sibling satisfies the predicate.
/// Returns true if the node has no siblings,
/// or if the predicate returns false for all siblings.
/// Short-circuits on the first sibling where the predicate returns true.
pub fn siblings_cannot_include<F> (
  tree      : &Tree<UncheckedViewNode>,
  node_id   : NodeId,
  predicate : F,
) -> bool
where F: Fn (&UncheckedViewNode) -> bool
{ let Some (node_ref) = tree . get (node_id)
    else { return true; };
  let Some (parent_ref) = node_ref . parent()
    else { return true; };
  ! parent_ref . children()
      . filter ( |c| c . id() != node_id )
      . any    ( |c| predicate( c . value() )) }
