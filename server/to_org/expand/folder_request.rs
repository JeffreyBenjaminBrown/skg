/// Fulfill a '(viewRequests (folder RELNAME))' request: build BOTH folders of
/// the relation, each POPULATED from the graph, reusing the de-novo
/// PartnerFolder generators. The EDITABLE folder of the relation is created
/// even when empty (its editable "add here" surface); the WRITE-PROTECTED
/// folders are built only when populated in the worktree or, in diff mode,
/// on the HEAD side (decision A -- a write-protected folder empty on both sides is
/// pruned by 'is_self_deletable_when_empty').
///
/// 'aliases' is handled by the AliasFolder builder ('expand/aliases.rs'),
/// not here -- the dispatch in 'execute_view_requests' routes it there.

use crate::skgrepo_sets::SkgrepoRestriction;
use crate::dbs::in_rust_graph::InRustGraph;
use crate::to_org::complete::partner_folder::{
  maybe_add_one_partnerFolder, maybe_add_subscribeeFolder_branch };
use crate::to_org::util::remove_completed_view_request;
use crate::types::git::SkgrepoDiff;
use crate::types::misc::{SkgConfig, SkgrepoName};
use crate::types::viewnode::{Viewnode, ViewRequest, FolderRelation, PartnerFolder};

use ego_tree::{NodeId, Tree};
use std::collections::HashMap;
use std::error::Error;

pub fn build_and_integrate_folder_then_drop_request (
  tree               : &mut Tree<Viewnode>,
  treeid             : NodeId,
  graph              : &InRustGraph,
  rel                : FolderRelation,
  config             : &SkgConfig,
  errors             : &mut Vec < String >,
  skgrepo_restriction : Option<&SkgrepoRestriction>,
  skgrepo_diffs      : &Option<HashMap<SkgrepoName, SkgrepoDiff>>,
) -> Result < (), Box<dyn Error> > {
  let result : Result<(), Box<dyn Error>> =
    build_and_integrate_folder (
      tree, treeid, rel, graph, config, skgrepo_restriction,
      skgrepo_diffs );
  remove_completed_view_request (
    tree, treeid,
    ViewRequest::Folder (rel),
    "Failed to build folder view",
    errors, result ) }

/// Build the relation's folders. Idempotent: each generator skips a folder
/// that already exists (e.g. one content completion already added), so
/// a populated relation's folders are not doubled, while the empty
/// editable folder is still forced in.
fn build_and_integrate_folder (
  tree               : &mut Tree<Viewnode>,
  treeid             : NodeId,
  rel                : FolderRelation,
  graph              : &InRustGraph,
  config             : &SkgConfig,
  skgrepo_restriction : Option<&SkgrepoRestriction>,
  skgrepo_diffs      : &Option<HashMap<SkgrepoName, SkgrepoDiff>>,
) -> Result < (), Box<dyn Error> > {
  match rel {
    FolderRelation::Aliases =>
      // The dispatch routes Folder(Aliases) to the AliasFolder builder; it
      // never reaches this function.
      return Err (
        "build_and_integrate_folder: aliases is built by the AliasFolder \
         builder, not here" . into () ),
    FolderRelation::Overrides => {
      // overriddenFolder (writable) -- forced empty; overriderFolder (write-protected).
      maybe_add_one_partnerFolder (
        tree, treeid, PartnerFolder::Overridden, config, graph,
        skgrepo_restriction, skgrepo_diffs, true ) ?;
      maybe_add_one_partnerFolder (
        tree, treeid, PartnerFolder::Overrider, config, graph,
        skgrepo_restriction, skgrepo_diffs, false ) ?; },
    FolderRelation::HidesFromSubs => {
      // Both sides write-protected: hiding is editable only from a
      // subscribee-as-such, never from a hider/hidden folder.
      maybe_add_one_partnerFolder (
        tree, treeid, PartnerFolder::Hider, config, graph,
        skgrepo_restriction, skgrepo_diffs, false ) ?;
      maybe_add_one_partnerFolder (
        tree, treeid, PartnerFolder::Hidden, config, graph,
        skgrepo_restriction, skgrepo_diffs, false ) ?; },
    FolderRelation::SubscribesTo => {
      // subscribeeFolder (writable) -- forced empty; subscriberFolder (write-protected).
      maybe_add_subscribeeFolder_branch (
        tree, treeid, graph, config,
        skgrepo_restriction, skgrepo_diffs, true ) ?;
      maybe_add_one_partnerFolder (
        tree, treeid, PartnerFolder::Subscriber, config, graph,
        skgrepo_restriction, skgrepo_diffs, false ) ?; }, }
  Ok (( )) }
