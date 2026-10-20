//! Which occurrence of a node is editable, and the view requests.
//!
//! A buffer holds at most one editable occurrence of a graphnode; every
//! other occurrence of it is write-protected (see the glossary's
//! "editable, write-protected"). View completion decides which, as its
//! level-order BFS ('complete_nodes_in_level_order' in
//! 'server/update_buffer/complete.rs') visits the occurrences, by four
//! rules:
//!
//! - The editable map ('EditableMap' in 'server/to_org/util.rs'),
//!   built afresh at every completion, records which node has claimed
//!   an editable occurrence, and where. An occurrence an editable-view
//!   request made editable claims its node as 'Finalizable::Final'; any
//!   other editable occurrence claims it as 'Finalizable::Tentative'.
//!   Only an editable occurrence claims, so editability can move from
//!   one rerender to the next.
//! - The draw rule: an occurrence without a request is editable when
//!   its node is unclaimed, and write-protected when the BFS reached
//!   another occurrence first
//!   ('make_writeProtected_if_repeat_then_extend_editable_map'). An
//!   occurrence carrying an editable-view request ('ViewRequest::Editable',
//!   C-c s d) defers to a Final occurrence elsewhere, becoming
//!   write-protected and losing the request; otherwise it becomes
//!   editable and Final, and a Tentative occurrence elsewhere is
//!   write-protected with its whole content subtree
//!   ('apply_editable_draw_rule'). Write-protecting an occurrence makes
//!   its member children non-members, since it writes no content.
//! - The cascade: a Final occurrence hands an editable-view request to
//!   each of its member content children
//!   ('pass_editableViewRequest_to_content_children' in
//!   'server/update_buffer/reconcile/content.rs'), each of which applies
//!   the draw rule at its own visit and cascades in turn. So a request
//!   on an editable occurrence means "unfold this subtree editable".
//! - The node budget ('CompletionContext::node_budget') starts at
//!   'initial_node_limit' at each completion. Each expansion of an
//!   editable vognode's content costs one; a folder fills its members
//!   for free, and a content list is drawn whole or not at all. Once the
//!   budget is spent, every later occurrence other than a root is drawn
//!   write-protected and loses its request, which is what bounds the
//!   cascade. The diff view charges only some expansions; see
//!   'docs/diff-view.org', "Editability in the diff view".
//!
//! This file also dispatches the view requests that remain after the
//! node's visit ('execute_view_requests'), and applies the write-protect
//! requests ('apply_writeProtect_request', C-c s w and C-c s W).

use crate::skgrepo_sets::SkgrepoRestriction;
use crate::to_org::expand::aliases::build_and_integrate_aliases_view_then_drop_request;
use crate::to_org::expand::role_tree::build_and_integrate_role_tree_then_drop_request;
use crate::to_org::expand::folder_request::build_and_integrate_folder_then_drop_request;
use crate::to_org::expand::flags::build_and_integrate_flags_then_drop_request;
use crate::to_org::util::{ EditableMap, Finalizable, get_skgid_from_viewnode_at, makeWriteProtectedAndClobber, make_member_children_nonMembers, unrestrictedVognode_in_tree_is_writeProtected };
use crate::types::misc::{ID, SkgConfig, SkgrepoName};
use crate::types::viewnode::{ Viewnode, ViewRequest, FolderRelation, Editability };
use crate::types::nodes::complete::Graphnode;
use crate::dbs::node_lookup::graphnode_graphFirst_by_pid_and_skgrepo;
use crate::dbs::in_rust_graph::InRustGraph;
use crate::types::tree::viewnode_graphnode::{write_at_unrestrictedVognode_in_tree, pid_and_skgrepo_from_viewnode_at};

use ego_tree::{Tree, NodeId};
use std::error::Error;

pub fn execute_view_requests (
  viewforest         : &mut Tree<Viewnode>,
  requests           : Vec < (NodeId, ViewRequest) >,
  graph              : &crate::dbs::in_rust_graph::InRustGraph,
  config             : &SkgConfig,
  errors             : &mut Vec < String >,
  skgrepo_restriction : Option<&SkgrepoRestriction>,
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
          skgrepo_restriction ) ?; },
      ViewRequest::RoleTree (role) => {
        // Relation-generic: every partner role routes through the one
        // role-tree engine (container, mentioner, and the seven new
        // roles alike). A view-ROOT's container request is handled
        // separately (finish_viewforest) and removed before this pass.
        build_and_integrate_role_tree_then_drop_request (
          viewforest, treeid, graph, role, config, errors,
          skgrepo_restriction ) ?; },
      ViewRequest::Flags => {
        build_and_integrate_flags_then_drop_request (
          viewforest, treeid, graph, config, errors ) ?; },
      ViewRequest::Editable =>
        // View completion (dispatch_node_update) settles every Editable
        // request at the node's own visit (apply_editable_draw_rule, the
        // draw rule and the cascade), before this post-content view-request pass
        // runs, so none should reach here. Fail loudly if one does, rather than
        // silently dropping it.
        return Err ( "execute_view_requests: a ViewRequest::Editable survived \
          to the view-request pass; it should have been consumed by the draw \
          rule at the node's visit" . into () ),
      ViewRequest::WriteProtected | ViewRequest::WriteProtectedSubtree =>
        // Like an Editable request, consumed at the node's visit
        // (apply_writeProtect_request), before this pass.
        return Err ( "execute_view_requests: a write-protect request survived \
          to the view-request pass; it should have been consumed at the \
          node's visit" . into () ),
      ViewRequest::Fork =>
        // A Fork request is consumed on the SAVE path (fork detection),
        // and 'extract_view_requests' strips it before this render-time
        // pass; reaching here means that stripping was bypassed.
        return Err ( "execute_view_requests: a ViewRequest::Fork survived \
          to the view-request pass; it should have been consumed on the \
          save path and stripped before rendering" . into () ), }}
  Ok (( )) }

/// The result of applying the Tentative/Final draw rule (see the module
/// comment) to a node that carries a 'ViewRequest::Editable' (a user
/// EVR, or one the cascade passed down).
pub enum DrawOutcome {
  /// An existing Final occurrence of this id won: the EVR was dropped and
  /// the node left write-protected. No content expansion should follow.
  Deferred,
  /// The node was made Final (and any prior Tentative occurrence of its id
  /// write-protectd). The caller draws its content next, and cascades.
  MadeFinal,
}

/// Apply the draw rule (see the module comment) for a node carrying
/// 'ViewRequest::Editable', WITHOUT expanding its content:
/// - defer to an existing Final occurrence (drop the EVR, stay
///   write-protected) -> 'DrawOutcome::Deferred';
/// - otherwise write-protect any prior Tentative occurrence of the id, mark
///   this node Final (resyncing title/body from disk), register it in the map,
///   and clear the request -> 'DrawOutcome::MadeFinal'.
/// Content drawing is the caller's job (and so is the cascade), so the rule settles
/// Final-ness before view completion (complete_nodes_in_level_order) draws
/// (and cascades) content.
pub fn apply_editable_draw_rule (
  viewforest : &mut Tree<Viewnode>,
  treeid     : NodeId,
  graph      : &InRustGraph,
  config     : &SkgConfig,
  visited    : &mut EditableMap,
) -> Result < DrawOutcome, Box<dyn Error> > {
  let node_pid : ID = get_skgid_from_viewnode_at (
    viewforest, treeid ) ?;
  if let Some (&prior) = visited . get (& node_pid) {
    if prior . is_final () && prior . treeid () != treeid {
      // An existing Final occurrence wins; discard this EVR and make
      // the node write-protected. (Setting write-protected matters for a
      // cascade EVR landing on a freshly-created editable child whose id
      // is already Final elsewhere; for a user EVR on an already-write-protected
      // node it is a no-op. The expand step then clobbers/refreshes it.)
      write_at_unrestrictedVognode_in_tree (
        viewforest, treeid,
        |t| { t . view_requests . remove (& ViewRequest::Editable);
              t . editability = Editability::WriteProtected; } )
        . map_err ( |e| -> Box<dyn Error> { e . into() } ) ?;
      return Ok ( DrawOutcome::Deferred ); }
    if prior . treeid () != treeid {
      writeProtect_content_subtree ( viewforest,
                                     prior . treeid (),
                                     visited, graph, config ) ?; }}
  { // Remove request, mark editable, replace title/body, add to visited.
    write_at_unrestrictedVognode_in_tree (
      viewforest, treeid, |t| {
        t . view_requests . remove (& ViewRequest::Editable);
        t . editability = Editability::Editable {
          body         : None,
          edit_request : None }; } )
      . map_err ( |e| -> Box<dyn Error> { e . into() } ) ?;
    from_disk_replace_title_body_and_graphnode (
      viewforest, treeid, graph, config ) ?;
    // A EVR target is Final: later EVRs for this ID defer to it.
    visited . insert ( node_pid . clone(), Finalizable::Final (treeid) ); }
  Ok ( DrawOutcome::MadeFinal ) }

/// Apply a write-protect request -- 'ViewRequest::WriteProtected' or
/// 'ViewRequest::WriteProtectedSubtree' -- that the occurrence at
/// 'treeid' carries, and consume it. Call at the occurrence's visit,
/// before it expands, so that the expansion takes the write-protected
/// path and draws nothing under it.
/// - The plain form write-protects the occurrence and makes each of its
///   direct unrestricted children a non-member; the children keep
///   their own editability and subtrees.
/// - The subtree form does that to the whole content subtree
///   ('writeProtect_content_subtree').
/// An editable-view request on the same occurrence goes too: the
/// validator refuses a user's writing both, so one here was passed
/// down by the cascade, and the user's explicit request wins.
/// On an already write-protected occurrence this only consumes the
/// request, since a write-protected occurrence holds no member.
pub fn apply_writeProtect_request (
  tree    : &mut Tree<Viewnode>,
  treeid  : NodeId,
  graph   : &InRustGraph,
  config  : &SkgConfig,
  visited : &mut EditableMap,
) -> Result < (), Box<dyn Error> > {
  let (requested, subtree) : (bool, bool) =
    write_at_unrestrictedVognode_in_tree (
      tree, treeid,
      |t| { let requested : bool =
              t . view_requests . remove (& ViewRequest::WriteProtected);
            let subtree : bool =
              t . view_requests . remove (& ViewRequest::WriteProtectedSubtree);
            if requested || subtree {
              t . view_requests . remove (& ViewRequest::Editable); }
            ( requested || subtree, subtree ) } )
    . map_err ( |e| -> Box<dyn Error> { e . into() } ) ?;
  if subtree {
    writeProtect_content_subtree (
      tree, treeid, visited, graph, config ) ?;
  } else if requested {
    writeProtect_occurrence ( tree, treeid, visited, graph, config ) ?;
    make_member_children_nonMembers ( tree, treeid ) ?; }
  Ok (( )) }

/// Does three things:
/// - Mark a node, and its entire content subtree, as write-protected.
/// - Make each member in the subtree a non-member,
///   since a write-protected node writes no content.
/// - Remove them from `visited`.
/// Only recurses into non-ignored UnrestrictedVognode children;
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
  writeProtect_occurrence ( tree, treeid, visited, graph, config ) ?;
  for child_treeid in make_member_children_nonMembers ( tree, treeid ) ? {
    writeProtect_content_subtree ( // recurse
      tree, child_treeid, visited, graph, config ) ?; }
  Ok (( )) }

/// Write-protect one editable occurrence (resyncing it from the
/// graph), and drop its entry from `visited` if the entry names it,
/// so that an editable copy can be drawn elsewhere. An entry naming
/// another occurrence is that copy, and stays.
fn writeProtect_occurrence (
  tree    : &mut Tree<Viewnode>,
  treeid  : NodeId,
  visited : &mut EditableMap,
  graph   : &InRustGraph,
  config  : &SkgConfig,
) -> Result < (), Box<dyn Error> > {
  if unrestrictedVognode_in_tree_is_writeProtected ( tree, treeid ) ? {
    return Ok (( )); }
  let node_pid : ID =
    get_skgid_from_viewnode_at ( tree, treeid ) ?;
  if visited . get (&node_pid)
       . is_some_and ( |entry| entry . treeid () == treeid ) {
    visited . remove (&node_pid); }
  makeWriteProtectedAndClobber ( tree, treeid, graph, config ) ?;
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
  let (pid, src) : (ID, SkgrepoName) =
    pid_and_skgrepo_from_viewnode_at ( tree, treeid,
      "from_disk_replace_title_body_and_graphnode" ) ?;
  let graphnode : Graphnode = graphnode_graphFirst_by_pid_and_skgrepo (
    graph, config, &pid, &src ) ?;
  let title : String = graphnode . title . clone();
  if title . is_empty () {
    return Err ( format! ( "Graphnode {} has empty title", pid ) . into () ); }
  let body : Option < String > = graphnode . body . clone ();
  write_at_unrestrictedVognode_in_tree
    ( tree, treeid,
      |t| { t . title = title;
            if let Editability::Editable { body: ref mut b, .. }
              = t . editability
              { *b = body; }} )
    . map_err ( |e| -> Box<dyn Error> { e . into() } ) ?;
  Ok (( )) }
