/// PURPOSE: Draw the containerward chain under the members of a
/// partner folder ('Folder::members_carry_containerward_chain'):
/// under each member, its containers as write-protected non-members;
/// under each of those, its containers; and so on breadth-first to
/// 'max_roleTree_depth', up to the roots.
/// A node the walk met before (a cycle or a diamond) is shown once
/// more, as a leaf ('ContainerwardRoleTree::Repeated').
/// The refresh draws the chain under a member it draws into such a
/// folder ('after_drawing' in 'server/refresh/ladder.rs'), and never
/// reinstates a chain node once drawn; no request extends a chain,
/// and a request on a chain node for its containers draws a
/// containers folder under it like any other. A chain node carries no
/// change facts and takes no commit-readiness marker
/// ('stands_in_containerward_chain').

use crate::dbs::in_rust_graph::containerward_role_tree::{ ContainerwardRoleTree, containerward_role_trees_by_gnid_from_gnids, containers_in_inRustGraph};
use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::in_rust_graph::relation_accessors::{NodeRelation, RelationRole};
use crate::graph_straddle::types::GraphStraddle;
use crate::skgrepo_sets::SkgrepoRestriction;
use crate::to_org::util::graphnode_and_viewnode_from_gnid;
use crate::types::misc::{ID, SkgConfig};
use crate::types::phantom::home_from_disk;
use crate::types::viewnode::{ Viewnode, ViewnodeKind, Vognode, mk_unknown_viewnode, mk_writeProtected_from_viewnode };

use ego_tree::{NodeId, Tree};
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
  /// The containerward role tree of each of 'gnids', whose children
  /// are its containers.
  fn containerward_role_trees (
    &self,
    graph     : &InRustGraph,
    gnids    : &[ID],
    max_depth : usize,
  ) -> HashMap<ID, ContainerwardRoleTree> {
    match self {
      ContainerwardChainPartners::LiveGraph =>
        containerward_role_trees_by_gnid_from_gnids (
          &containers_in_inRustGraph (graph), gnids, max_depth ),
      ContainerwardChainPartners::GraphStraddle (straddle) =>
        containerward_role_trees_by_gnid_from_gnids (
          &|node : &ID, role : RelationRole| -> HashSet<ID> {
            straddle . partners (node, role) . into_iter ()
              . map ( |partner| partner . id )
              . collect () },
          gnids, max_depth ), } }
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
  let members_and_gnids : Vec<(NodeId, ID)> =
    members . iter ()
    . filter_map ( |member|
      tree . get (*member) . and_then ( |node|
        match &node . value () . kind {
          ViewnodeKind::Vognode (Vognode::Occurrence (t)) =>
            Some (( *member, t . gnid . clone () )),
          _ => None, } ))
    . collect ();
  if members_and_gnids . is_empty () { return Ok (( )); }
  let gnids : Vec<ID> =
    members_and_gnids . iter () . map ( |(_, gnid)| gnid . clone () ) . collect ();
  let role_trees_by_gnid : HashMap<ID, ContainerwardRoleTree> =
    partners . containerward_role_trees (
      graph, &gnids, config . max_roleTree_depth );
  for (member, gnid) in &members_and_gnids {
    if let Some (ContainerwardRoleTree::Inner ( _, containers )) =
      role_trees_by_gnid . get (gnid)
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
      tree, parent, node . gnid (), graph, config, restriction ) ?
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
    let parent_gnid : Option<ID> =
      tree . get (parent) . and_then ( |node| match &node . value () . kind {
        ViewnodeKind::Vognode (Vognode::Occurrence (t)) => Some (t . gnid . clone ()),
        _ => None, } );
    if let Some (parent_gnid) = parent_gnid {
      let relationship_unrestricted : bool =
        graph . relRepo (container, NodeRelation::Contains, &parent_gnid)
        . map ( |skgrepo| restriction . contains_skgrepo (&skgrepo) )
        . unwrap_or (false);
      if ! relationship_unrestricted { return Ok (None); }} }
  let viewnode : Viewnode = match
    graphnode_and_viewnode_from_gnid (graph, config, container) ? {
      Some ((_graphnode, viewnode)) =>
        mk_writeProtected_from_viewnode (
          viewnode, false ) ?,
      None => mk_unknown_viewnode (container . clone ()), };
  Ok ( Some ( tree . get_mut (parent)
              . ok_or ("prepend_containerward_chain_node: parent not found") ?
              . prepend (viewnode)
              . id () )) }

