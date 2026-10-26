/// PURPOSE: Draw the containerward chain under the members of a
/// partner folder ('Folder::members_carry_containerward_chain'):
/// under each member, its containers as write-protected non-members;
/// under each of those, its containers; and so on breadth-first to
/// 'max_roleTree_depth', up to the roots.
/// A node the walk met before (a cycle or a diamond) is shown once
/// more, as a leaf ('ContainerwardRoleTree::Repeated').
/// The folder's reconciler reconciles each member's chain at every
/// visit of the folder ('reconcile_containerward_chain_under_member'),
/// so a member it adds gets its chain; no request extends a chain,
/// and a request on a chain node for its containers draws a
/// containers folder under it like any other. A chain node carries no
/// change facts and takes no commit-readiness marker
/// ('is_in_containerward_chain').

use crate::dbs::in_rust_graph::containerward_role_tree::{ ContainerwardRoleTree, containerward_role_trees_by_skgid_from_skgids, containers_in_inRustGraph};
use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::in_rust_graph::relation_accessors::{NodeRelation, RelationRole};
use crate::graph_straddle::types::GraphStraddle;
use crate::skgrepo_sets::SkgrepoRestriction;
use crate::to_org::util::graphnode_and_viewnode_from_skgid;
use crate::types::misc::{ID, SkgConfig};
use crate::types::phantom::home_from_disk;
use crate::types::viewnode::{ DiffChange, Viewnode, ViewnodeKind, Vognode, mk_unknown_viewnode, mk_writeProtected_from_viewnode };

use ego_tree::{NodeId, NodeRef, Tree};
use std::collections::{HashMap, HashSet};
use std::error::Error;

/// Where the chain finds a node's containers: in the live graph, or in
/// the union of a graph straddle. Viewnodes are built from the live
/// graph either way.
#[derive(Clone, Copy)]
pub enum ContainerwardChainPartners<'a> {
  /// The walk is ungated; what the skgrepo restriction hides is
  /// left out where it would be shown, and the chain stops there.
  LiveGraph,
  GraphStraddle (&'a GraphStraddle),
}

impl ContainerwardChainPartners<'_> {
  /// The containerward role tree of each of 'skgids', whose children
  /// are its containers.
  fn containerward_role_trees (
    &self,
    graph     : &InRustGraph,
    skgids    : &[ID],
    max_depth : usize,
  ) -> HashMap<ID, ContainerwardRoleTree> {
    match self {
      ContainerwardChainPartners::LiveGraph =>
        containerward_role_trees_by_skgid_from_skgids (
          &containers_in_inRustGraph (graph), skgids, max_depth ),
      ContainerwardChainPartners::GraphStraddle (straddle) =>
        containerward_role_trees_by_skgid_from_skgids (
          &|node : &ID, role : RelationRole| -> HashSet<ID> {
            straddle . partners (node, role) . into_iter ()
              . map ( |partner| partner . id )
              . collect () },
          skgids, max_depth ), } }
}

/// Draw the chain under each of 'members', each an unrestricted
/// vognode: its containers' role trees, prepended so that the first
/// container comes first. Anything else among 'members' is skipped.
pub fn attach_containerward_chains_under_members (
  tree        : &mut Tree<Viewnode>,
  members     : &[NodeId],
  graph       : &InRustGraph,
  config      : &SkgConfig,
  restriction : Option<&SkgrepoRestriction>,
  partners    : ContainerwardChainPartners,
) -> Result<(), Box<dyn Error>> {
  let members_and_skgids : Vec<(NodeId, ID)> =
    members . iter ()
    . filter_map ( |member|
      tree . get (*member) . and_then ( |node|
        match &node . value () . kind {
          ViewnodeKind::Vognode (Vognode::Unrestricted (t)) =>
            Some (( *member, t . skgid . clone () )),
          _ => None, } ))
    . collect ();
  if members_and_skgids . is_empty () { return Ok (( )); }
  let skgids : Vec<ID> =
    members_and_skgids . iter () . map ( |(_, skgid)| skgid . clone () ) . collect ();
  let role_trees_by_skgid : HashMap<ID, ContainerwardRoleTree> =
    partners . containerward_role_trees (
      graph, &skgids, config . max_roleTree_depth );
  for (member, skgid) in &members_and_skgids {
    if let Some (ContainerwardRoleTree::Inner ( _, containers )) =
      role_trees_by_skgid . get (skgid)
    { for container in containers . iter () . rev () {
        insert_containerward_chain_recursive (
          container, *member, tree, graph, config, restriction ) ?; }} }
  Ok (( )) }

/// Prepend 'node' under 'parent' as a chain node, and under it, its
/// own containers, each prepended in reverse so that they keep their
/// order. Where a node is hidden, its branch of the chain stops.
fn insert_containerward_chain_recursive (
  node        : &ContainerwardRoleTree,
  parent      : NodeId,
  tree        : &mut Tree<Viewnode>,
  graph       : &InRustGraph,
  config      : &SkgConfig,
  restriction : Option<&SkgrepoRestriction>,
) -> Result<(), Box<dyn Error>> {
  let Some (chain_node) : Option<NodeId> =
    prepend_containerward_chain_node (
      tree, parent, node . skgid (), graph, config, restriction ) ?
    else { return Ok (( )); };
  if let ContainerwardRoleTree::Inner ( _, containers ) = node {
    for container in containers . iter () . rev () {
      insert_containerward_chain_recursive (
        container, chain_node, tree, graph, config, restriction ) ?; }}
  Ok (( )) }

/// Prepend 'container' under 'parent', which it contains, as a
/// write-protected non-member, or an unknown phantom if no graph
/// knows it. None, and nothing prepended, when the skgrepo
/// restriction hides the container or the relationship: the
/// containment is recorded by the container, perhaps in a more
/// private skgrepo than either node's home ('docs/telescopes.org',
/// "What lives where").
fn prepend_containerward_chain_node (
  tree        : &mut Tree<Viewnode>,
  parent      : NodeId,
  container   : &ID,
  graph       : &InRustGraph,
  config      : &SkgConfig,
  restriction : Option<&SkgrepoRestriction>,
) -> Result<Option<NodeId>, Box<dyn Error>> {
  if let Some (restriction) = restriction {
    if ! restriction . is_all () {
      if let Some (skgrepo) = graph . pid_and_skgrepo (container)
        . map ( |(_, skgrepo)| skgrepo )
        . or_else ( || home_from_disk (container, config) )
      { if ! restriction . contains_skgrepo (&skgrepo)
        { return Ok (None); }} }
    let parent_skgid : Option<ID> =
      tree . get (parent) . and_then ( |node| match &node . value () . kind {
        ViewnodeKind::Vognode (Vognode::Unrestricted (t)) => Some (t . skgid . clone ()),
        _ => None, } );
    if let Some (parent_skgid) = parent_skgid {
      let relationship_unrestricted : bool =
        graph . relRepo (container, NodeRelation::Contains, &parent_skgid)
        . map ( |skgrepo| restriction . contains_skgrepo (&skgrepo) )
        . unwrap_or (false);
      if ! relationship_unrestricted { return Ok (None); }} }
  let viewnode : Viewnode = match
    graphnode_and_viewnode_from_skgid (graph, config, container) ? {
      Some ((_graphnode, viewnode)) =>
        mk_writeProtected_from_viewnode (
          viewnode, false ) ?,
      None => mk_unknown_viewnode (container . clone ()), };
  Ok ( Some ( tree . get_mut (parent)
              . ok_or ("prepend_containerward_chain_node: parent not found") ?
              . prepend (viewnode)
              . id () )) }

/// Whether 'node' is a chain node: a non-member vognode under a
/// member of a partner folder whose members carry the chain
/// ('Folder::members_carry_containerward_chain'), or under
/// another chain node.
pub fn is_in_containerward_chain (
  node : NodeRef<Viewnode>,
) -> bool {
  let mut current : NodeRef<Viewnode> = node;
  loop {
    let ViewnodeKind::Vognode (vognode) = &current . value () . kind
      else { return false; };
    let is_nonMember : bool = match vognode {
      Vognode::Unrestricted (t) => ! t . claimsMembership,
      _ => true, };
    if ! is_nonMember { return false; }
    let Some (parent) : Option<NodeRef<Viewnode>> = current . parent ()
      else { return false; };
    let ViewnodeKind::Vognode (_) = &parent . value () . kind
      else { return false; };
    let parent_is_a_chain_carrying_folder_member : bool =
      parent . parent () . is_some_and ( |grandparent| matches! (
        &grandparent . value () . kind,
        ViewnodeKind::Folder (kind)
          if kind . members_carry_containerward_chain () ));
    if parent_is_a_chain_carrying_folder_member { return true; }
    current = parent; }}

/// Whether 'node' is a chain node as the walk draws it: write-protected
/// (or a phantom, for a container it cannot show), carrying no change
/// facts. So a non-member the user typed
/// under a folder's member, which is editable, is not one,
/// and neither is what that member's own visit adds there (a removed
/// member, a historical phantom), which carries change facts.
pub fn is_drawn_chain_node (
  node : NodeRef<Viewnode>,
) -> bool {
  let viewnode : &Viewnode = node . value ();
  is_in_containerward_chain (node)
    && viewnode . editable_skgid () . is_none ()
    && viewnode . change_facts . change == DiffChange::default ()
    && ! matches! ( viewnode . kind,
                    ViewnodeKind::Vognode (Vognode::Ghost (_)) ) }
