/// Node access utilities for ego_tree::Tree<Viewnode> and Tree<MpViewnode>

use crate::to_org::util::get_id_from_treenode;
use crate::dbs::node_lookup::graphnode_rustFirst_by_pid_and_repo;
use crate::dbs::in_rust_graph::InRustGraph;
use crate::types::misc::{ID, MSV, SkgConfig, RepoName};
use crate::types::viewnode::{
    Viewnode, ViewnodeKind, ActiveVognode, AffectsParent };
use crate::types::viewnode::{Vognode, Phantom, PropertyFolder, Property, PartnerFolder};
use crate::types::maybe_placed_viewnode::{
    MpViewnode, MpViewnodeKind };
use crate::types::maybe_placed_viewnode::{MpVognode, MpPhantom};
use crate::types::nodes::complete::Graphnode;
use crate::types::list::dedup_vector;
use super::generic::{ unique_non_vognode_child, write_at_node_in_tree, with_node_mut };

use ego_tree::{Tree, NodeId, NodeRef};
use std::collections::{HashMap, HashSet};
use std::error::Error;

/// Apply a mutating function to the ActiveVognode at the given tree position.
/// Errors if the node is not found or is not an ActiveVognode.
pub fn write_at_activeVognode_in_tree<F, R> (
  tree   : &mut Tree<Viewnode>,
  treeid : NodeId,
  f      : F,
) -> Result<R, String>
where F: FnOnce (&mut ActiveVognode) -> R {
  write_at_node_in_tree (
    tree, treeid,
    |viewnode| { match &mut viewnode . kind {
      // TODO/DONE/local-view-update/plan_v2.org §11: a phantom is not an ActiveVognode (it carries a slim PhantomDiff), so
      // this Normal-only mutator cannot apply to one (a phantom has no
      // view_requests/editability/etc).
      ViewnodeKind::Vognode (Vognode::Active (t))
        => Ok ( f (t) ),
      _ => Err ( "write_at_activeVognode_in_tree: expected ActiveVognode"
                   . to_string () ) }} ) ? }

/// Extract (ID, repo) from a non-phantom vognode that carries both.
/// Returns an error if the node is not found or cannot provide both fields.
pub fn pid_and_repo_from_treenode (
  tree        : &Tree<Viewnode>,
  treeid      : NodeId,
  caller_name : &str,
) -> Result<(ID, RepoName), Box<dyn Error>> {
  let node_ref : NodeRef<Viewnode> =
    tree . get (treeid) . ok_or_else ( ||
      format! ( "{}: node not found", caller_name ) ) ?;
  match &node_ref . value() . kind {
    ViewnodeKind::Vognode (v) if v . is_graph_member () =>
      v . pid_and_repo ()
      . map ( |(pid, repo)| (pid . clone (), repo . clone ()) )
      . ok_or_else (|| format!(
        "{}: vognode has no repo", caller_name ) . into () ),
    _ => Err ( format! (
      "{}: expected a non-phantom vognode with PID and repo",
      caller_name ) . into() ),
  }}

/// Get the ID from this node if it's an MpActiveVognode with an ID,
/// otherwise recursively try ancestors.
/// Returns an error if no ancestor has an ID (e.g., reached BufferRoot).
pub fn id_from_self_or_nearest_ancestor (
  tree    : &Tree<MpViewnode>,
  node_id : NodeId,
) -> Result<ID, String> {
  let mut node : NodeRef<MpViewnode> =
    tree . get (node_id)
    . ok_or ("id_from_self_or_nearest_ancestor: node not found")?;
  loop {
    match &node . value() . kind {
      MpViewnodeKind::Vognode (MpVognode::Active (t)) =>
        { if let Some (id) = &t . id { return Ok(id . clone()); }}
      MpViewnodeKind::Vognode (MpVognode::Phantom (MpPhantom::Diff (p))) =>
        { if let Some (id) = &p . id { return Ok(id . clone()); }}
      _ => {} }
    node = node . parent()
      . ok_or ("id_from_self_or_nearest_ancestor: reached root without finding ID")?; }}

/// Find the unique child of a node with a given non-vognode kind.
/// Returns None if no child has the kind,
/// Some(child_id) if exactly one does,
/// or an error if multiple children have it.
pub fn unique_non_vognode_child_of_viewnode (
  tree          : &Tree<Viewnode>,
  node_id       : NodeId,
  non_vognode_kind : &ViewnodeKind,
) -> Result<Option<NodeId>, Box<dyn Error>> {
  unique_non_vognode_child (
    tree,
    node_id,
    non_vognode_kind,
    |child : &Viewnode| Some (&child . kind))
  . map_err (|e| -> Box<dyn Error> { e . into() }) }

/// Extract PIDs for a Subscribee and its grandparent (the subscriber).
/// Expects: subscriber -> SubscribeeFolder -> Subscribee (this node)
pub fn pid_for_subscribee_and_its_subscriber_grandparent (
  tree    : &Tree<Viewnode>,
  node_id : NodeId,
  graph   : &InRustGraph,
  config  : &SkgConfig,
) -> Result < ( ID, ID ), Box<dyn Error> > {
  let subscribee_pid : ID = get_id_from_treenode ( tree, node_id ) ?;
  let node_ref : NodeRef < Viewnode > =
    tree . get (node_id) . ok_or (
      "pid_for_subscribee_and_its_subscriber_grandparent: node not found" ) ?;
  let parent_ref : NodeRef < Viewnode > =
    node_ref . parent ()
    . ok_or ("Subscribee has no parent (SubscribeeFolder)") ?;
  if ! matches! ( &parent_ref . value () . kind,
                  ViewnodeKind::PartnerFolder (PartnerFolder::Subscribee)) {
    return Err ( "Subscribee's parent is not a SubscribeeFolder" .
                 into () ); }
  let grandparent_ref : NodeRef < Viewnode > =
    parent_ref . parent ()
    . ok_or ("SubscribeeFolder has no parent (subscriber)") ?;
  let (subscriber_id, subscriber_repo) : (ID, RepoName) =
    pid_and_repo_from_treenode (
      tree, grandparent_ref . id (),
      "pid_for_subscribee_and_its_subscriber_grandparent" ) ?;
  let graphnode : Graphnode =
    graphnode_rustFirst_by_pid_and_repo (
      graph, config, &subscriber_id, &subscriber_repo ) ?;
  Ok (( subscribee_pid,
        graphnode . pid . clone() )) }

pub fn insert_non_vognode_as_child (
  tree          : &mut Tree<Viewnode>,
  parent_id     : NodeId,
  non_vognode_kind : ViewnodeKind,
  prepend       : bool, // otherwise, append
) -> Result < NodeId, Box<dyn Error> > {
  let viewnode : Viewnode =
    Viewnode {
      focused     : false,
      folded      : false,
      body_folded : false,
      kind        : non_vognode_kind };
  let folder_id : NodeId = with_node_mut (
    tree, parent_id,
    |mut parent_mut| {
      if prepend { parent_mut . prepend (viewnode) . id () }
      else       { parent_mut . append  (viewnode) . id () } } )
    . map_err ( |e| -> Box<dyn Error> { e . into() } ) ?;
  Ok (folder_id) }

/// Collect aliases for a node:
/// - find the unique AliasFolder child (error if multiple)
/// - for each Alias child of the AliasFolder, collect its title
/// Duplicates are removed (preserving order of first occurrence).
/// Returns None ("no opinion") if no AliasFolder found.
/// Returns Some(vec) if AliasFolder found, even if empty.
pub fn collect_grandchild_aliases_for_viewnode (
  tree: &Tree<Viewnode>,
  node_id: NodeId,
) -> Result<MSV<String>, String> {
  let alias_folder_id : Option<NodeId> =
    unique_non_vognode_child_of_viewnode (
      tree, node_id, &ViewnodeKind::PropertyFolder (PropertyFolder::Alias) )
    . map_err ( |e| e . to_string() ) ?;
  match alias_folder_id {
    None => Ok (MSV::Unspecified),
    Some (folder_id) => {
      let aliases : Vec<String> = {
        let folder_ref : NodeRef<Viewnode> =
          tree . get (folder_id) . expect ("collect_grandchild_aliases_for_viewnode: AliasFolder not found");
        let mut aliases : Vec<String> = Vec::new();
        for alias_child in folder_ref . children() {
          { // check for invalid state
            if ! matches!(&alias_child . value() . kind,
                          ViewnodeKind::Property (Property::Alias { .. } )) {
              return Err ( format! (
                "AliasFolder has non-Alias child with kind: {:?}",
                alias_child . value() . kind )); }}
          aliases . push(
            alias_child . value() . title() . to_string() ); }
        aliases };
      Ok( MSV::Specified(dedup_vector (aliases)) ) }} }

/// Find a child node by its ID.
/// Returns the NodeId of the child if found, None otherwise.
pub fn find_child_by_id (
  tree          : & Tree<Viewnode>,
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
  tree          : & Tree<Viewnode>,
  parent_treeid : NodeId,
  target_skgids : & HashSet < ID >,
) -> HashMap < ID, NodeId > {
  let mut result : HashMap < ID, NodeId > = HashMap::new();
  for child in tree . get (parent_treeid) . unwrap() . children() {
    match &child . value() . kind {
      ViewnodeKind::Vognode (Vognode::Active (t)) =>
        if target_skgids . contains (&t . id)
        { result . insert (t . id . clone (), child . id()); },
      ViewnodeKind::Vognode (Vognode::Phantom (p @ (Phantom::Diff (_)
                                  | Phantom::Deleted (_)))) =>
        if target_skgids . contains (p . id ())
        { result . insert (p . id () . clone (), child . id()); },
      _ => {} } }
  result }

/// Check if all nodes at the specified generation satisfy the predicate.
/// Returns true if the generation is empty (vacuously true).
/// Negative generations = ancestors; positive = descendants.
/// If skip_non_content, excludes ActiveVognodes with affectsParent != Affected.
pub fn generation_includes_only<F> (
  tree                : &Tree<MpViewnode>,
  node_id             : NodeId,
  generation          : i32,
  skip_non_content    : bool,
  predicate           : F,
) -> bool
where F: Fn (&MpViewnode) -> bool
{ collect_generation( tree, node_id, generation, skip_non_content )
    . iter()
    . all ( |&id| predicate(
      tree . get (id) . unwrap() . value() )) }

/// Check if the generation is nonempty and all nodes satisfy the predicate.
/// Negative generations = ancestors; positive = descendants.
/// If skip_non_content, excludes ActiveVognodes with affectsParent != Affected.
pub fn generation_exists_and_includes<F> (
  tree                : &Tree<MpViewnode>,
  node_id             : NodeId,
  generation          : i32,
  skip_non_content    : bool,
  predicate           : F,
) -> bool
where F: Fn (&MpViewnode) -> bool
{ let nodes = collect_generation(
    tree, node_id, generation, skip_non_content);
  !nodes . is_empty() &&
    nodes . iter() . all(
      |&id| predicate(
        tree . get (id) . unwrap() . value() )) }

/// Check if the specified generation is empty.
/// Negative generations = ancestors; positive = descendants.
/// If skip_non_content, excludes ActiveVognodes with affectsParent != Affected.
pub fn generation_does_not_exist (
  tree                : &Tree<MpViewnode>,
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
///   then we exclude ActiveVognodes with affectsParent != Affected.
fn collect_generation (
  tree               : &Tree<MpViewnode>,
  node_id            : NodeId,
  generation         : i32,
  skip_non_content   : bool,
) -> Vec<NodeId> {
  if generation == 0 {
    return vec![node_id]; }
  let Some (node_ref) = tree . get (node_id)
    else { return vec![]; };
  if generation <= 0 {
    let mut current : NodeRef<'_, MpViewnode> =
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
                          // TODO/DONE/local-view-update/plan_v2.org §11: a phantom has no affectsParent and is implicitly
                          // Affected (content), so it is never filtered here.
                          !matches!(&c . value() . kind,
                                    MpViewnodeKind::Vognode (MpVognode::Active (t))
                                    if t . affectsParent != AffectsParent::True ))
              . map(|c| c . id()) ); }}
      current_gen = next_gen; }
    current_gen } }

/// Check that no sibling satisfies the predicate.
/// Returns true if the node has no siblings,
/// or if the predicate returns false for all siblings.
/// Short-circuits on the first sibling where the predicate returns true.
pub fn siblings_cannot_include<F> (
  tree      : &Tree<MpViewnode>,
  node_id   : NodeId,
  predicate : F,
) -> bool
where F: Fn (&MpViewnode) -> bool
{ let Some (node_ref) = tree . get (node_id)
    else { return true; };
  let Some (parent_ref) = node_ref . parent()
    else { return true; };
  ! parent_ref . children()
      . filter ( |c| c . id() != node_id )
      . any    ( |c| predicate( c . value() )) }
