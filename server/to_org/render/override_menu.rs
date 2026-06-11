//! The override-choice buffer ("the menu",
//! TODO/full-schema/11_override-rendering-and-navigation.org): when
//! a single-root content view names an overridden node, the server
//! offers the chain of overriders instead of silently choosing.
//! The menu is an ordinary Skg buffer of indefinitive nodes -- the
//! requested node as root, each overrider drawn as an Independent
//! child of what it overrides, following 'overrides_view_of' from
//! overridden to overrider recursively, ALL edges including foreign
//! (the source herald distinguishes ownership; 'overridesParent'
//! supplies the "Op" markers). Overriders from inactive sources are
//! omitted, with their branches. A branch stops at the first
//! repeated ID; the standard stats pass marks that repeat with the
//! 'cycle' viewstat, since the repeat is by construction also an
//! org-ancestor.

use crate::dbs::in_rust_graph::InRustGraph;
use crate::source_sets::ActiveSourceSet;
use crate::types::env::SkgEnv;
use crate::types::misc::ID;
use crate::types::tree::forest::ViewForest;
use crate::types::viewnode::{
  ParentIs, ViewNode, mk_indefinitive_viewnode};
use crate::types::views_state::pids_from_viewforest;
use crate::update_buffer::finish_viewforest;

use ego_tree::{NodeId, Tree};
use std::collections::HashSet;
use std::error::Error;
use std::sync::Arc;

/// Build the override-choice buffer for 'pid' (already resolved to
/// a primary ID). Returns None when no overrider of 'pid' is
/// visible under the active source-set -- a menu with no choices
/// would be noise, and the caller falls through to a normal render.
pub async fn override_menu_view (
  env    : &SkgEnv,
  pid    : &ID,
  active : Option<&ActiveSourceSet>,
) -> Result < Option<(String, Vec<ID>, Tree<ViewNode>)>,
              Box<dyn Error> > {
  let graph : Arc<InRustGraph> =
    env . in_rust_graph . load_full ();
  if visible_overriders (&graph, active, pid) . is_empty () {
    return Ok (None); }
  let Some (root_node) = graph . nodes . get (pid)
    else { return Ok (None); }; // unknown id: let the normal render path explain
  let mut viewforest : ViewForest = ViewForest::new ();
  let root_treeid : NodeId =
    viewforest . append_root (
      mk_indefinitive_viewnode (
        pid . clone (),
        root_node . source . clone (),
        root_node . title . clone (),
        ParentIs::Absent ));
  { let mut path : HashSet<ID> =
      HashSet::from ( [ pid . clone () ] );
    add_overrider_branches (
      &mut viewforest, root_treeid,
      &graph, active, pid, &mut path ); }
  let rendered : String =
    finish_viewforest (
      &mut viewforest, &env . config, &env . driver,
      active ) . await ?;
  let pids : Vec<ID> =
    pids_from_viewforest ( &viewforest )
    . into_iter () . collect ();
  Ok ( Some ((
    rendered, pids, viewforest . into_internal_tree () )) ) }

/// Append, under 'treeid' (the viewnode for 'pid'), one indefinitive
/// Independent child per visible overrider of 'pid', recursing into
/// each overrider that is not already on the current path. A
/// repeated ID is still drawn (so the stats pass can mark it
/// 'cycle') but its branch stops there.
fn add_overrider_branches (
  viewforest : &mut ViewForest,
  treeid     : NodeId,
  graph      : &InRustGraph,
  active     : Option<&ActiveSourceSet>,
  pid        : &ID,
  path       : &mut HashSet<ID>,
) {
  for overrider in visible_overriders (graph, active, pid) {
    let Some (node) = graph . nodes . get (&overrider)
      else { continue; };
    let child_treeid : NodeId = {
      let Some (mut node_mut) = viewforest . get_mut (treeid)
        else { continue; };
      node_mut . append (
        mk_indefinitive_viewnode (
          overrider . clone (),
          node . source . clone (),
          node . title . clone (),
          ParentIs::Independent ))
        . id () };
    if path . insert ( overrider . clone () ) {
      add_overrider_branches (
        viewforest, child_treeid,
        graph, active, &overrider, path );
      path . remove (&overrider); }}}

/// The overriders of 'pid' the menu shows: every edge, user-owned
/// and foreign alike, except those from inactive sources (omitted
/// with their branches). Sorted by ID so menus render
/// deterministically.
fn visible_overriders (
  graph  : &InRustGraph,
  active : Option<&ActiveSourceSet>,
  pid    : &ID,
) -> Vec<ID> {
  let mut result : Vec<ID> = Vec::new ();
  if let Some (overriders) = graph . overriders_of . get (pid) {
    for overrider in overriders {
      if let Some (node) = graph . nodes . get (overrider) {
        let visible : bool =
          active
          . map ( |a| a . is_all ()
                  || a . contains_source (&node . source) )
          . unwrap_or (true);
        if visible {
          result . push ( overrider . clone () ); }}}}
  result . sort ();
  result }
