//! The search buffer's enrichment ('docs/api-and-formats.org',
//! "Text search"): under each hit, folded, its containers folder, each
//! member carrying the containerward chain, then the folder of the
//! nodes that override it and the folder of the nodes it overrides.
//! A folder with no member is not drawn.

use crate::dbs::tantivy::title_and_skgrepo_by_skgid;
use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::in_rust_graph::relation_accessors::NodeRelation;
use crate::dbs::in_rust_graph::containerward_role_tree::ContainerwardRoleTree;
use crate::skgrepo_sets::SkgrepoRestriction;
use crate::types::misc::{ID, SkgConfig, SkgrepoName, TantivyIndex};
use crate::types::viewnode::{FoldAndFocus, PartnerFolder, Viewnode, ViewnodeKind, mk_writeProtected_viewnode, viewnode_of};
use crate::types::viewnode::Vognode;

use ego_tree::{NodeId, NodeMut, NodeRef, Tree};
use std::collections::{HashMap, HashSet};

/// Under each level-1 hit of the search viewforest that is among
/// 'search_results', prepend, folded, its containers folder, its
/// overrider folder and its overridden folder, in that order, each
/// only if it has a member.
pub fn insert_partner_folders_into_search_view (
  viewforest                        : &mut Tree<Viewnode>,
  graph                             : &InRustGraph,
  search_results                    : &[ID],
  containerward_role_trees_by_skgid : &HashMap<ID, ContainerwardRoleTree>,
  tantivy_index                     : &TantivyIndex,
  config                            : &SkgConfig,
  restriction                       : &SkgrepoRestriction,
) {
  let hits : Vec<(NodeId, ID)> = {
    let root_ref : NodeRef<Viewnode> = viewforest . root ();
    root_ref . children ()
    . filter_map ( |c| match &c . value () . kind {
      ViewnodeKind::Vognode (Vognode::Unrestricted (t))
        if search_results . contains (&t . skgid)
        => Some (( c . id (), t . skgid . clone () )),
      _ => None } )
    . collect () };
  for (hit_treeid, hit_skgid) in &hits {
    // Prepended last-first, so that they stand in the order named.
    for kind in [ PartnerFolder::Overridden, PartnerFolder::Overrider ] {
      let members : Vec<(ID, SkgrepoName, String)> =
        override_folder_members (graph, hit_skgid, kind, restriction);
      if members . is_empty () { continue; }
      let folder : NodeId = prepend_folded_folder (viewforest, *hit_treeid, kind);
      for (skgid, skgrepo, title) in members {
        viewforest . get_mut (folder) . unwrap () . append (
          mk_writeProtected_viewnode (
            skgid, skgrepo, title, true )); }}
    if let Some (ContainerwardRoleTree::Inner ( _, containers )) =
      containerward_role_trees_by_skgid . get (hit_skgid)
    { let folder : NodeId =
        prepend_folded_folder (viewforest, *hit_treeid, PartnerFolder::Container);
      for container in containers . iter () . rev () {
        insert_containerward_tree (
          container, hit_skgid, folder, true,
          viewforest, graph, tantivy_index, config, restriction ); }
      let is_empty : bool =
        viewforest . get (folder) . unwrap () . children () . next () . is_none ();
      if is_empty { viewforest . get_mut (folder) . unwrap () . detach (); }} }}

/// Prepend a folded partner folder of 'kind' under 'parent'.
fn prepend_folded_folder (
  viewforest : &mut Tree<Viewnode>,
  parent     : NodeId,
  kind       : PartnerFolder,
) -> NodeId {
  viewforest . get_mut (parent) . unwrap () . prepend ( Viewnode {
    foldAndFocus : FoldAndFocus { folded : true, .. FoldAndFocus::default () },
    .. viewnode_of ( ViewnodeKind::PartnerFolder (kind) ) } ) . id () }

/// The members of the hit's overrider folder (the nodes that override
/// it) or overridden folder (the nodes it overrides): their IDs,
/// skgrepos and titles, less those whose override relationship or own
/// skgrepo the restriction hides.
fn override_folder_members (
  graph       : &InRustGraph,
  hit         : &ID,
  kind        : PartnerFolder,
  restriction : &SkgrepoRestriction,
) -> Vec<(ID, SkgrepoName, String)> {
  override_relatives (graph, hit, kind, restriction) . into_iter ()
    . filter_map ( |relative| graph . nodes . get (&relative)
      . map ( |node| ( relative . clone (), node . home_skgrepo . clone (),
                       node . title . clone () )) )
    . collect () }

/// The IDs of the hit's override relatives that a folder of 'kind'
/// shows (see 'override_folder_members').
fn override_relatives (
  graph       : &InRustGraph,
  hit         : &ID,
  kind        : PartnerFolder,
  restriction : &SkgrepoRestriction,
) -> Vec<ID> {
  let relatives : Vec<ID> = match kind {
    PartnerFolder::Overridden =>
      graph . outbound_pids_for_relation_gated (
        hit, NodeRelation::Overrides, Some (restriction) ),
    _ =>
      graph . inbound_pids_for_relation_gated (
        hit, NodeRelation::Overrides, Some (restriction) ), };
  relatives . into_iter ()
    . filter ( |relative| graph . nodes . get (relative)
      . is_some_and ( |node| restriction . contains_skgrepo (&node . home_skgrepo) ))
    . collect () }

/// Every ID the override folders under the given results would show.
/// The enrichment thread unions these into the graphStats pre-fetch so
/// the folder members get their relationship heralds; without this
/// they would render herald-less, since the folders do not exist yet
/// when the pre-fetch runs.
pub fn collect_override_folder_member_skgids (
  graph          : &InRustGraph,
  search_results : &[ID],
  restriction    : &SkgrepoRestriction,
) -> HashSet<ID> {
  search_results . iter ()
    . flat_map ( |hit|
      [ PartnerFolder::Overrider, PartnerFolder::Overridden ] . into_iter ()
      . flat_map ( |kind| override_relatives (graph, hit, kind, restriction) ) )
    . collect () }

/// Prepend 'node', a container of 'contained_skgid', under
/// 'parent_treeid' as a write-protected vognode claiming membership or not (a
/// member of the containers folder, or below that a chain node), and
/// under it, recursively, its own containers as chain nodes. A node
/// whose containment or skgrepo the restriction hides is left out,
/// and its branch stops.
fn insert_containerward_tree (
  node            : &ContainerwardRoleTree,
  contained_skgid : &ID,
  parent_treeid   : NodeId,
  claimsMembership : bool,
  viewforest      : &mut Tree<Viewnode>,
  graph           : &InRustGraph,
  tantivy_index   : &TantivyIndex,
  config          : &SkgConfig,
  restriction     : &SkgrepoRestriction,
) {
  if ! restriction . is_all () {
    // relRepo gating ('docs/telescopes.org', "What lives where"): a
    // private membership must not surface through the enrichment even
    // when both nodes are public. The relationship's recorder is the
    // container.
    let rel_is_visible : bool =
      graph . relRepo (
        node . skgid (), NodeRelation::Contains, contained_skgid )
      . map ( |skgrepo| restriction . contains_skgrepo (&skgrepo) )
      . unwrap_or (true); // unknown relationship: fall through to the
                          // node-repo gate below, as before
    if ! rel_is_visible { return; }}
  let child_treeid : NodeId = match
    prepend_containing_child_from_tantivy (
      node . skgid (), parent_treeid, claimsMembership,
      viewforest, tantivy_index, config, restriction ) {
        Some (child_treeid) => child_treeid,
        None => return, };
  if let ContainerwardRoleTree::Inner ( _, children ) = node {
    for child in children . iter () . rev () {
      insert_containerward_tree (
        child, node . skgid (), child_treeid, false,
        viewforest, graph, tantivy_index, config, restriction ); } } }

/// Looks up a node's title and skgrepo from Tantivy, and prepends it
/// under the given parent as a write-protected vognode: a member of a
/// containers folder, or a chain node, a non-member. Returns the new
/// child's NodeId.
fn prepend_containing_child_from_tantivy (
  skgid         : &ID, // what to prepend
  parent_treeid : NodeId, // where to prepend
  claimsMembership : bool,
  viewforest        : &mut Tree<Viewnode>,
  tantivy_index : &TantivyIndex,
  _config       : &SkgConfig,
  restriction   : &SkgrepoRestriction,
) -> Option<NodeId> {
  let viewnode : Viewnode =
    match title_and_skgrepo_by_skgid ( tantivy_index, skgid ) {
      Some ((title, skgrepo)) => {
        if ! restriction . contains_skgrepo (&skgrepo) {
          return None;
        } else {
          mk_writeProtected_viewnode (
            skgid . clone (), skgrepo, title, claimsMembership ) }},
      None =>
        mk_writeProtected_viewnode (
          skgid . clone (), SkgrepoName::from ("search"),
          skgid . as_str () . to_string (), claimsMembership ) };
  let mut parent_mut : NodeMut<Viewnode> =
    viewforest . get_mut (parent_treeid) . unwrap ();
  Some ( parent_mut . prepend (viewnode) . id () ) }
