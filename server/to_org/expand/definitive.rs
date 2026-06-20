use crate::source_sets::ActiveSourceSet;
use crate::to_org::expand::aliases::build_and_integrate_aliases_view_then_drop_request;
use crate::to_org::expand::backpath::{ build_and_integrate_containerward_view_then_drop_request_with_source_set, build_and_integrate_sourceward_view_then_drop_request_with_source_set};
use crate::to_org::util::{ DefinitiveMap, Finalizable, get_id_from_treenode, makeIndefinitiveAndClobber, activeNode_in_tree_is_indefinitive };
use crate::types::misc::{ID, SkgConfig, SourceName};
use crate::dbs::in_rust_graph::relation_accessors::RelationRole;
use crate::types::viewnode::{ ViewNode, ViewNodeKind, ViewRequest, ColRelation, IndefOrDef, ParentIs };
use crate::types::viewnode::Vognode;
use crate::types::nodes::complete::NodeComplete;
use crate::dbs::node_lookup::nodecomplete_rustFirst_by_pid_and_source;
use crate::types::tree::viewnode_nodecomplete::{write_at_activeNode_in_tree, pid_and_source_from_treenode};

use ego_tree::{Tree, NodeId, NodeRef};
use std::error::Error;
use typedb_driver::TypeDBDriver;

pub async fn execute_view_requests (
  viewforest    : &mut Tree<ViewNode>,
  requests      : Vec < (NodeId, ViewRequest) >,
  config        : &SkgConfig,
  typedb_driver : &TypeDBDriver,
  errors        : &mut Vec < String >,
  active_source_set : Option<&ActiveSourceSet>,
) -> Result < (), Box<dyn Error> > {
  for (node_id, request) in requests {
    match request {
      ViewRequest::Col (ColRelation::Aliases) => {
        build_and_integrate_aliases_view_then_drop_request (
          viewforest, node_id, config, typedb_driver, errors )
          . await ?; },
      ViewRequest::Col (rel) =>
        // Populated overrides/hides/subscribes PartnerCols are built in
        // step 4; no client requests them yet.
        return Err ( format! (
          "execute_view_requests: Col({}) not yet implemented",
          rel . relname () ) . into () ),
      ViewRequest::Path (role) if role == RelationRole::CONTAINER => {
        build_and_integrate_containerward_view_then_drop_request_with_source_set (
          viewforest, node_id, config, typedb_driver, errors,
          active_source_set )
          . await ?; },
      ViewRequest::Path (role) if role == RelationRole::LINK_SOURCE => {
        build_and_integrate_sourceward_view_then_drop_request_with_source_set (
          viewforest, node_id, config, typedb_driver, errors,
          active_source_set )
          . await ?; },
      ViewRequest::Path (role) =>
        // The other seven partner roles are built in step 5; no client
        // requests them yet.
        return Err ( format! (
          "execute_view_requests: Path({}) not yet implemented",
          role . rolename () ) . into () ),
      ViewRequest::Definitive =>
        // View completion (dispatch_node_update) settles every Definitive
        // request at the node's own visit (apply_definitive_draw_rule, the TODO/DONE/local-view-update/plan_v2.org §5.2
        // draw rule + TODO/DONE/local-view-update/plan_v2.org §5.3 cascade), before this post-content view-request pass
        // runs, so none should reach here. Fail loudly if one does, rather than
        // silently dropping it.
        return Err ( "execute_view_requests: a ViewRequest::Definitive survived \
          to the view-request pass; it should have been consumed by the draw \
          rule at the node's visit" . into () ), }}
  Ok (( )) }

/// The result of applying the TODO/DONE/local-view-update/plan_v2.org §5.2 Tentative/Final draw rule to a node that
/// carries a 'ViewRequest::Definitive' (a user DVR, or a TODO/DONE/local-view-update/plan_v2.org §5.3 cascade DVR).
pub enum DrawOutcome {
  /// An existing Final occurrence of this id won: the DVR was dropped and
  /// the node left indefinitive. No content expansion should follow.
  Deferred,
  /// The node was made Final (and any prior Tentative occurrence of its id
  /// indefinitized). The caller draws its content next via the TODO/DONE/local-view-update/plan_v2.org §5.3 cascade.
  MadeFinal,
}

/// Apply the TODO/DONE/local-view-update/plan_v2.org §5.2 draw rule for a node carrying
/// 'ViewRequest::Definitive', WITHOUT expanding its content:
/// - defer to an existing Final occurrence (drop the DVR, stay
///   indefinitive) -> 'DrawOutcome::Deferred';
/// - otherwise indefinitize any prior Tentative occurrence of the id, mark
///   this node Final (resyncing title/body from disk), register it in the map,
///   and clear the request -> 'DrawOutcome::MadeFinal'.
/// Content drawing is the caller's job (the TODO/DONE/local-view-update/plan_v2.org §5.3 cascade), so the rule settles
/// Final-ness before view completion (complete_nodes_in_level_order) draws
/// (and cascades) content.
pub fn apply_definitive_draw_rule (
  viewforest : &mut Tree<ViewNode>,
  node_id    : NodeId,
  config     : &SkgConfig,
  visited    : &mut DefinitiveMap,
) -> Result < DrawOutcome, Box<dyn Error> > {
  let node_pid : ID = get_id_from_treenode (
    viewforest, node_id ) ?;
  if let Some (&prior) = visited . get (& node_pid) {
    if prior . is_final () && prior . node_id () != node_id {
      // TODO/DONE/local-view-update/plan_v2.org §5.2: an existing Final occurrence wins; discard this DVR and make
      // the node indefinitive. (Setting indefinitive matters for a TODO/DONE/local-view-update/plan_v2.org §5.3
      // cascade DVR landing on a freshly-created definitive child whose id
      // is already Final elsewhere; for a user DVR on an already-indefinitive
      // node it is a no-op. The expand step then clobbers/refreshes it.)
      write_at_activeNode_in_tree (
        viewforest, node_id,
        |t| { t . view_requests . remove (& ViewRequest::Definitive);
              t . indef_or_def = IndefOrDef::Indefinitive; } )
        . map_err ( |e| -> Box<dyn Error> { e . into() } ) ?;
      return Ok ( DrawOutcome::Deferred ); }
    if prior . node_id () != node_id {
      indefinitize_content_subtree ( viewforest,
                                     prior . node_id (),
                                     visited, config ) ?; }}
  { // Remove request, mark definitive, replace title/body, add to visited.
    write_at_activeNode_in_tree (
      viewforest, node_id, |t| {
        t . view_requests . remove (& ViewRequest::Definitive);
        t . indef_or_def = IndefOrDef::Definitive {
          body         : None,
          edit_request : None }; } )
      . map_err ( |e| -> Box<dyn Error> { e . into() } ) ?;
    from_disk_replace_title_body_and_nodecomplete (
      viewforest, node_id, config ) ?;
    // A DVR target is Final (TODO/DONE/local-view-update/plan_v2.org §5.2): later DVRs for this ID defer to it.
    visited . insert ( node_pid . clone(), Finalizable::Final (node_id) ); }
  Ok ( DrawOutcome::MadeFinal ) }

/// Does two things:
/// - Mark a node, and its entire content subtree, as indefinitive.
/// - Remove them from `visited`.
/// Only recurses into non-ignored ActiveNode children;
///   ignored and scaffold children persist unchanged.
/// TODO : This will need complication to properly handle
///   sharing-related nodes among the input node's descendents.
fn indefinitize_content_subtree (
  tree    : &mut Tree<ViewNode>,
  node_id : NodeId,
  visited : &mut DefinitiveMap,
  config  : &SkgConfig,
) -> Result < (), Box<dyn Error> > {
  let (node_pid, content_child_treeids)
    : (ID, Vec <NodeId>) =
    { let node_ref : NodeRef < ViewNode > =
        tree . get (node_id) . ok_or (
          "indefinitize_content_subtree: NodeId not in tree" ) ?;
      let node_pid : ID =
        get_id_from_treenode ( tree, node_id ) ?;
      let content_child_treeids : Vec < NodeId > =
        node_ref . children ()
        . filter ( |c| matches! ( &c . value() . kind,
                                  ViewNodeKind::Vognode (Vognode::Active (t))
                                  if t . parentIs == ParentIs::Affected ))
        . map ( |c| c . id () )
        . collect ();
      (node_pid, content_child_treeids) };
  if ! activeNode_in_tree_is_indefinitive ( tree, node_id ) ? {
    visited . remove (&node_pid);
    makeIndefinitiveAndClobber ( tree, node_id, config ) ?; }
  for child_treeid in content_child_treeids { // recurse
    indefinitize_content_subtree (
      tree, child_treeid, visited, config ) ?; }
  Ok (( )) }

/// Fetches NodeComplete from the in-Rust graph or disk.
/// Updates title and body.
/// Preserves all other ViewNode data.
fn from_disk_replace_title_body_and_nodecomplete (
  tree    : &mut Tree<ViewNode>,
  node_id : NodeId,
  config  : &SkgConfig,
) -> Result < (), Box<dyn Error> > {
  let (pid, src) : (ID, SourceName) =
    pid_and_source_from_treenode ( tree, node_id,
      "from_disk_replace_title_body_and_nodecomplete" ) ?;
  let nodecomplete : NodeComplete = nodecomplete_rustFirst_by_pid_and_source (
    config, &pid, &src ) ?;
  let title : String = nodecomplete . title . clone();
  if title . is_empty () {
    return Err ( format! ( "NodeComplete {} has empty title", pid ) . into () ); }
  let body : Option < String > = nodecomplete . body . clone ();
  write_at_activeNode_in_tree
    ( tree, node_id,
      |t| { t . title = title;
            if let IndefOrDef::Definitive { body: ref mut b, .. }
              = t . indef_or_def
              { *b = body; }} )
    . map_err ( |e| -> Box<dyn Error> { e . into() } ) ?;
  Ok (( )) }
