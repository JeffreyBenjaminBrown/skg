/// Fulfill a '(viewRequests (folder RELNAME))' request: build BOTH folders of
/// the relation, each POPULATED from the graph, reusing the de-novo
/// PartnerFolder generators. The WRITABLE folder of the relation is created
/// even when empty (its editable "add here" surface); the WRITE-PROTECTED
/// folders are built only when populated in the worktree or, in diff mode,
/// on the HEAD side (decision A -- a write-protected folder empty on both sides is
/// pruned by 'is_self_deletable_when_empty').
///
/// 'aliases' is handled by the AliasFolder builder ('expand/aliases.rs'),
/// not here -- the dispatch in 'execute_view_requests' routes it there.

use crate::repo_sets::ActiveRepoSet;
use crate::dbs::in_rust_graph::InRustGraph;
use crate::to_org::complete::partner_folder::{
  maybe_add_one_partnerFolder, maybe_add_subscribeeFolder_branch };
use crate::to_org::util::remove_completed_view_request;
use crate::types::git::RepoDiff;
use crate::types::misc::{SkgConfig, RepoName};
use crate::types::viewnode::{ViewNode, ViewRequest, FolderRelation, PartnerFolder};

use ego_tree::{NodeId, Tree};
use std::collections::HashMap;
use std::error::Error;

pub fn build_and_integrate_folder_then_drop_request (
  tree          : &mut Tree<ViewNode>,
  node_id       : NodeId,
  graph         : &InRustGraph,
  rel           : FolderRelation,
  config        : &SkgConfig,
  errors        : &mut Vec < String >,
  active_repo_set : Option<&ActiveRepoSet>,
  repo_diffs : &Option<HashMap<RepoName, RepoDiff>>,
) -> Result < (), Box<dyn Error> > {
  let result : Result<(), Box<dyn Error>> =
    build_and_integrate_folder (
      tree, node_id, rel, graph, config, active_repo_set,
      repo_diffs );
  remove_completed_view_request (
    tree, node_id,
    ViewRequest::Folder (rel),
    "Failed to build folder view",
    errors, result ) }

/// Build the relation's folders. Idempotent: each generator skips a folder
/// that already exists (e.g. one content completion already added), so
/// a populated relation's folders are not doubled, while the empty
/// writable folder is still forced in.
fn build_and_integrate_folder (
  tree    : &mut Tree<ViewNode>,
  node_id : NodeId,
  rel     : FolderRelation,
  graph   : &InRustGraph,
  config  : &SkgConfig,
  active_repo_set : Option<&ActiveRepoSet>,
  repo_diffs : &Option<HashMap<RepoName, RepoDiff>>,
) -> Result < (), Box<dyn Error> > {
  match rel {
    FolderRelation::Aliases =>
      // The dispatch routes Folder(Aliases) to the AliasFolder builder; it
      // never reaches this function.
      return Err (
        "build_and_integrate_folder: aliases is built by the AliasFolder \
         builder, not here" . into () ),
    FolderRelation::OverridesViewOf => {
      // overriddenFolder (writable) -- forced empty; overriderFolder (write-protected).
      maybe_add_one_partnerFolder (
        tree, node_id, PartnerFolder::Overridden, config, graph,
        active_repo_set, repo_diffs, true ) ?;
      maybe_add_one_partnerFolder (
        tree, node_id, PartnerFolder::Overrider, config, graph,
        active_repo_set, repo_diffs, false ) ?; },
    FolderRelation::HidesFromItsSubscriptions => {
      // Both sides write-protected: hiding is editable only from a
      // subscribee-as-such, never from a hider/hidden folder.
      maybe_add_one_partnerFolder (
        tree, node_id, PartnerFolder::Hider, config, graph,
        active_repo_set, repo_diffs, false ) ?;
      maybe_add_one_partnerFolder (
        tree, node_id, PartnerFolder::Hidden, config, graph,
        active_repo_set, repo_diffs, false ) ?; },
    FolderRelation::SubscribesTo => {
      // subscribeeFolder (writable) -- forced empty; subscriberFolder (write-protected).
      maybe_add_subscribeeFolder_branch (
        tree, node_id, graph, config,
        active_repo_set, repo_diffs, true ) ?;
      maybe_add_one_partnerFolder (
        tree, node_id, PartnerFolder::Subscriber, config, graph,
        active_repo_set, repo_diffs, false ) ?; }, }
  Ok (( )) }
