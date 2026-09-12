/// Fulfill a '(viewRequests (col RELNAME))' request: build BOTH cols of
/// the relation, each POPULATED from the graph, reusing the de-novo
/// PartnerCol generators. The WRITABLE col of the relation is created
/// even when empty (its editable "add here" surface); the READ-ONLY
/// cols are built only when populated (decision A -- an empty read-only
/// col is pruned by 'is_self_deletable_when_empty').
///
/// 'aliases' is handled by the AliasCol builder ('expand/aliases.rs'),
/// not here -- the dispatch in 'execute_view_requests' routes it there.

use crate::source_sets::ActiveSourceSet;
use crate::dbs::in_rust_graph::InRustGraph;
use crate::to_org::complete::partner_col::{
  maybe_add_one_partnerCol, maybe_add_subscribeeCol_branch };
use crate::to_org::util::remove_completed_view_request;
use crate::types::git::SourceDiff;
use crate::types::misc::{SkgConfig, SourceName};
use crate::types::viewnode::{ViewNode, ViewRequest, ColRelation, PartnerCol};

use ego_tree::{NodeId, Tree};
use std::collections::HashMap;
use std::error::Error;

pub fn build_and_integrate_col_then_drop_request (
  tree          : &mut Tree<ViewNode>,
  node_id       : NodeId,
  graph         : &InRustGraph,
  rel           : ColRelation,
  config        : &SkgConfig,
  errors        : &mut Vec < String >,
  active_source_set : Option<&ActiveSourceSet>,
) -> Result < (), Box<dyn Error> > {
  let result : Result<(), Box<dyn Error>> =
    build_and_integrate_col (
      tree, node_id, rel, graph, config, active_source_set );
  remove_completed_view_request (
    tree, node_id,
    ViewRequest::Col (rel),
    "Failed to build collection view",
    errors, result ) }

/// Build the relation's cols. Idempotent: each generator skips a col
/// that already exists (e.g. one content completion already added), so
/// a populated relation's cols are not doubled, while the empty
/// writable col is still forced in.
fn build_and_integrate_col (
  tree    : &mut Tree<ViewNode>,
  node_id : NodeId,
  rel     : ColRelation,
  graph   : &InRustGraph,
  config  : &SkgConfig,
  active_source_set : Option<&ActiveSourceSet>,
) -> Result < (), Box<dyn Error> > {
  // Not a diff view: a Col request never runs in git-diff mode.
  let no_diffs : Option<HashMap<SourceName, SourceDiff>> = None;
  match rel {
    ColRelation::Aliases =>
      // The dispatch routes Col(Aliases) to the AliasCol builder; it
      // never reaches this function.
      return Err (
        "build_and_integrate_col: aliases is built by the AliasCol \
         builder, not here" . into () ),
    ColRelation::Overrides => {
      // overriddenCol (writable) -- forced empty; overriderCol (read-only).
      maybe_add_one_partnerCol (
        tree, node_id, PartnerCol::Overridden, config, graph,
        active_source_set, &no_diffs, true ) ?;
      maybe_add_one_partnerCol (
        tree, node_id, PartnerCol::Overrider, config, graph,
        active_source_set, &no_diffs, false ) ?; },
    ColRelation::Hides => {
      // Both sides read-only: hiding is editable only from a
      // subscribee-as-such, never from a hider/hidden col.
      maybe_add_one_partnerCol (
        tree, node_id, PartnerCol::Hider, config, graph,
        active_source_set, &no_diffs, false ) ?;
      maybe_add_one_partnerCol (
        tree, node_id, PartnerCol::Hidden, config, graph,
        active_source_set, &no_diffs, false ) ?; },
    ColRelation::Subscribes => {
      // subscribeeCol (writable) -- forced empty; subscriberCol (read-only).
      maybe_add_subscribeeCol_branch (
        tree, node_id, graph, config,
        active_source_set, &no_diffs, true ) ?;
      maybe_add_one_partnerCol (
        tree, node_id, PartnerCol::Subscriber, config, graph,
        active_source_set, &no_diffs, false ) ?; }, }
  Ok (( )) }
