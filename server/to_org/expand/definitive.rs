use crate::skgrepo_sets::ActiveSkgRepoSet;
use crate::to_org::expand::aliases::build_and_integrate_aliases_view_then_drop_request;
use crate::to_org::expand::role_tree::build_and_integrate_role_tree_then_drop_request;
use crate::to_org::expand::folder_request::build_and_integrate_folder_then_drop_request;
use crate::to_org::expand::flags::build_and_integrate_flags_then_drop_request;
use crate::to_org::util::{ EditableMap, Finalizable, get_skgid_from_treenode, makeWriteProtectedAndClobber, activeVognode_in_tree_is_writeProtected };
use crate::types::misc::{ID, SkgConfig, SkgRepoName};
use crate::types::git::SkgRepoDiff;
use crate::types::viewnode::{ Viewnode, ViewnodeKind, ViewRequest, FolderRelation, Editability, AffectsParent };
use crate::types::viewnode::Vognode;
use crate::types::nodes::complete::Graphnode;
use crate::dbs::node_lookup::graphnode_graphFirst_by_pid_and_skgrepo;
use crate::dbs::in_rust_graph::InRustGraph;
use crate::types::tree::viewnode_graphnode::{write_at_activeVognode_in_tree, pid_and_skgrepo_from_treenode};

use ego_tree::{Tree, NodeId, NodeRef};
use std::collections::HashMap;
use std::error::Error;

pub fn execute_view_requests (
  viewforest         : &mut Tree<Viewnode>,
  requests           : Vec < (NodeId, ViewRequest) >,
  graph              : &crate::dbs::in_rust_graph::InRustGraph,
  config             : &SkgConfig,
  errors             : &mut Vec < String >,
  active_skgrepo_set : Option<&ActiveSkgRepoSet>,
  skgrepo_diffs      : &Option<HashMap<SkgRepoName, SkgRepoDiff>>,
) -> Result < (), Box<dyn Error> > {
  for (treeid, request) in requests {
    match request {
      ViewRequest::Folder (FolderRelation::Aliases) => {
        build_and_integrate_aliases_view_then_drop_request (
          viewforest, treeid, graph, config, errors )
 ?; },
      ViewRequest::Folder (rel) => {
        build_and_integrate_folder_then_drop_request (
          viewforest, treeid, graph, rel, config, errors,
          active_skgrepo_set, skgrepo_diffs ) ?; },
      ViewRequest::RoleTree (role) => {
        // Relation-generic: every partner role routes through the one
        // role-tree engine (container, mentioner, and the seven new
        // roles alike). A view-ROOT's container request is handled
        // separately (finish_viewforest) and removed before this pass.
        build_and_integrate_role_tree_then_drop_request (
          viewforest, treeid, graph, role, config, errors,
          active_skgrepo_set ) ?; },
      ViewRequest::Flags => {
        build_and_integrate_flags_then_drop_request (
          viewforest, treeid, graph, config, errors ) ?; },
      ViewRequest::Definitive =>
        // View completion (dispatch_node_update) settles every Editable
        // request at the node's own visit (apply_editable_draw_rule, the TODO/DONE/local-view-update/plan_v2.org §5.2
        // draw rule + TODO/DONE/local-view-update/plan_v2.org §5.3 cascade), before this post-content view-request pass
        // runs, so none should reach here. Fail loudly if one does, rather than
        // silently dropping it.
        return Err ( "execute_view_requests: a ViewRequest::Definitive survived \
          to the view-request pass; it should have been consumed by the draw \
          rule at the node's visit" . into () ),
      ViewRequest::Fork =>
        // A Fork request is consumed on the SAVE path (fork detection),
        // and 'extract_view_requests' strips it before this render-time
        // pass; reaching here means that stripping was bypassed.
        return Err ( "execute_view_requests: a ViewRequest::Fork survived \
          to the view-request pass; it should have been consumed on the \
          save path and stripped before rendering" . into () ), }}
  Ok (( )) }

/// The result of applying the TODO/DONE/local-view-update/plan_v2.org §5.2 Tentative/Final draw rule to a node that
/// carries a 'ViewRequest::Definitive' (a user DVR, or a TODO/DONE/local-view-update/plan_v2.org §5.3 cascade DVR).
pub enum DrawOutcome {
  /// An existing Final occurrence of this id won: the DVR was dropped and
  /// the node left write-protected. No content expansion should follow.
  Deferred,
  /// The node was made Final (and any prior Tentative occurrence of its id
  /// write-protectd). The caller draws its content next via the TODO/DONE/local-view-update/plan_v2.org §5.3 cascade.
  MadeFinal,
}

/// Apply the TODO/DONE/local-view-update/plan_v2.org §5.2 draw rule for a node carrying
/// 'ViewRequest::Definitive', WITHOUT expanding its content:
/// - defer to an existing Final occurrence (drop the DVR, stay
///   write-protected) -> 'DrawOutcome::Deferred';
/// - otherwise write-protect any prior Tentative occurrence of the id, mark
///   this node Final (resyncing title/body from disk), register it in the map,
///   and clear the request -> 'DrawOutcome::MadeFinal'.
/// Content drawing is the caller's job (the TODO/DONE/local-view-update/plan_v2.org §5.3 cascade), so the rule settles
/// Final-ness before view completion (complete_nodes_in_level_order) draws
/// (and cascades) content.
pub fn apply_editable_draw_rule (
  viewforest : &mut Tree<Viewnode>,
  treeid     : NodeId,
  graph      : &InRustGraph,
  config     : &SkgConfig,
  visited    : &mut EditableMap,
) -> Result < DrawOutcome, Box<dyn Error> > {
  let node_pid : ID = get_skgid_from_treenode (
    viewforest, treeid ) ?;
  if let Some (&prior) = visited . get (& node_pid) {
    if prior . is_final () && prior . treeid () != treeid {
      // TODO/DONE/local-view-update/plan_v2.org §5.2: an existing Final occurrence wins; discard this DVR and make
      // the node write-protected. (Setting write-protected matters for a TODO/DONE/local-view-update/plan_v2.org §5.3
      // cascade DVR landing on a freshly-created editable child whose id
      // is already Final elsewhere; for a user DVR on an already-write-protected
      // node it is a no-op. The expand step then clobbers/refreshes it.)
      write_at_activeVognode_in_tree (
        viewforest, treeid,
        |t| { t . view_requests . remove (& ViewRequest::Definitive);
              t . editability = Editability::WriteProtected; } )
        . map_err ( |e| -> Box<dyn Error> { e . into() } ) ?;
      return Ok ( DrawOutcome::Deferred ); }
    if prior . treeid () != treeid {
      writeProtect_content_subtree ( viewforest,
                                     prior . treeid (),
                                     visited, graph, config ) ?; }}
  { // Remove request, mark editable, replace title/body, add to visited.
    write_at_activeVognode_in_tree (
      viewforest, treeid, |t| {
        t . view_requests . remove (& ViewRequest::Definitive);
        t . editability = Editability::Editable {
          body         : None,
          edit_request : None }; } )
      . map_err ( |e| -> Box<dyn Error> { e . into() } ) ?;
    from_disk_replace_title_body_and_graphnode (
      viewforest, treeid, graph, config ) ?;
    // A DVR target is Final (TODO/DONE/local-view-update/plan_v2.org §5.2): later DVRs for this ID defer to it.
    visited . insert ( node_pid . clone(), Finalizable::Final (treeid) ); }
  Ok ( DrawOutcome::MadeFinal ) }

/// Does two things:
/// - Mark a node, and its entire content subtree, as write-protected.
/// - Remove them from `visited`.
/// Only recurses into non-ignored ActiveVognode children;
///   ignored and non-vognode children persist unchanged.
/// TODO : This will need complication to properly handle
///   sharing-related nodes among the input node's descendents.
fn writeProtect_content_subtree (
  tree    : &mut Tree<Viewnode>,
  treeid  : NodeId,
  visited : &mut EditableMap,
  graph   : &InRustGraph,
  config  : &SkgConfig,
) -> Result < (), Box<dyn Error> > {
  let (node_pid, content_child_treeids)
    : (ID, Vec <NodeId>) =
    { let node_ref : NodeRef < Viewnode > =
        tree . get (treeid) . ok_or (
          "writeProtect_content_subtree: NodeId not in tree" ) ?;
      let node_pid : ID =
        get_skgid_from_treenode ( tree, treeid ) ?;
      let content_child_treeids : Vec < NodeId > =
        node_ref . children ()
        . filter ( |c| matches! ( &c . value() . kind,
                                  ViewnodeKind::Vognode (Vognode::Active (t))
                                  if t . affectsParent == AffectsParent::True ))
        . map ( |c| c . id () )
        . collect ();
      (node_pid, content_child_treeids) };
  if ! activeVognode_in_tree_is_writeProtected ( tree, treeid ) ? {
    visited . remove (&node_pid);
    makeWriteProtectedAndClobber ( tree, treeid, graph, config ) ?; }
  for child_treeid in content_child_treeids { // recurse
    writeProtect_content_subtree (
      tree, child_treeid, visited, graph, config ) ?; }
  Ok (( )) }

/// Fetches Graphnode from the in-Rust graph or disk.
/// Updates title and body.
/// Preserves all other Viewnode data.
fn from_disk_replace_title_body_and_graphnode (
  tree    : &mut Tree<Viewnode>,
  treeid  : NodeId,
  graph   : &InRustGraph,
  config  : &SkgConfig,
) -> Result < (), Box<dyn Error> > {
  let (pid, src) : (ID, SkgRepoName) =
    pid_and_skgrepo_from_treenode ( tree, treeid,
      "from_disk_replace_title_body_and_graphnode" ) ?;
  let graphnode : Graphnode = graphnode_graphFirst_by_pid_and_skgrepo (
    graph, config, &pid, &src ) ?;
  let title : String = graphnode . title . clone();
  if title . is_empty () {
    return Err ( format! ( "Graphnode {} has empty title", pid ) . into () ); }
  let body : Option < String > = graphnode . body . clone ();
  write_at_activeVognode_in_tree
    ( tree, treeid,
      |t| { t . title = title;
            if let Editability::Editable { body: ref mut b, .. }
              = t . editability
              { *b = body; }} )
    . map_err ( |e| -> Box<dyn Error> { e . into() } ) ?;
  Ok (( )) }
