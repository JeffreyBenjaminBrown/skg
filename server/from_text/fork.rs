//! Fork support. Editing a foreign node N (one in a repo the user
//! does not own) is read as a request to CLONE it: the clone C lives in
//! an owned repo, copies N's edited title/body/contains, subscribes
//! to N and overrides N. N itself is left untouched.
//!
//! Detection happens in 'apply_foreign_policy' (validate.rs): a foreign
//! SaveNode whose buffer content differs from disk is a fork candidate
//! rather than a 'ModifiedForeignNode' error. This module resolves the
//! clone's owned repo (from N's nearest owned ancestor in the view)
//! and builds C's SaveNode. The confirmation-gating and the commit live
//! in the save handler.

use crate::dbs::in_rust_graph::override_invariants::existing_user_owned_overrider_of;
use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::filesystem::multiple_nodes::read_all_skg_files_from_repos;
use crate::org_to_text::metadata_value_atom;
use crate::repo_sets::ActiveRepoSet;
use crate::types::errors::BufferValidationError;
use crate::types::misc::{ID, MSV, SkgConfig, RepoName, members_of, rel_partners_at_relRepo};
use crate::types::nodes::complete::{
  Flag, NodeComplete, flag_is_true};
use crate::types::save::{ForkSpec, SaveNode};
use crate::types::tree::forest::ViewForest;
use crate::types::viewnode::{ViewnodeKind, Vognode};

use std::collections::{HashMap, HashSet};

/// For every FOREIGN vognode in the view, the repo of its nearest
/// vognode ancestor, recorded IFF that ancestor is an OWNED Active
/// vognode. A fork's clone C must live in an owned repo; the foreign
/// node N's own repo is write-protected, so C inherits from N's IMMEDIATE
/// container context -- the nearest vognode ancestor reached by skipping
/// only non-vognodes (folders, etc.). The walk STOPS at that nearest vognode
/// ancestor and never passes it: if the ancestor is foreign (or
/// inactive), nothing is inferred (the repo then defaults, or the user
/// sets it in the confirmation buffer). Inferring a distant owned node
/// reached by skipping a foreign ancestor would be wrong -- a clone
/// belongs in the repo of the node that actually contains N here.
/// A foreign node drawn at several positions can have different nearest
/// ancestors; the first OWNED one reached in preorder wins -- monogamy
/// means at most one clone anyway, and the user can override the choice
/// in the confirmation buffer.
pub fn owned_ancestor_repos_for_foreign_vognodes (
  viewforest : &ViewForest,
  config     : &SkgConfig,
) -> HashMap<ID, RepoName> {
  let mut map : HashMap<ID, RepoName> = HashMap::new ();
  for node in viewforest . nodes () {
    let ViewnodeKind::Vognode (Vognode::Active (t)) = & node . value () . kind
      else { continue; };
    if config . user_owns_repo (& t . home_repo) { continue; } // not foreign
    let mut current = node;
    while let Some (parent) = current . parent () {
      match & parent . value () . kind {
        ViewnodeKind::Vognode (Vognode::Active (pt)) => {
          // N's nearest vognode ancestor: record its repo IFF owned,
          // then stop -- never walk past it.
          if config . user_owns_repo (& pt . home_repo) {
            map . entry ( t . id . clone () )
              . or_insert_with ( || pt . home_repo . clone () ); }
          break; }
        ViewnodeKind::Vognode (Vognode::Inactive (_)) =>
          // An inactive vognode is a real container boundary too (and
          // never an owned repo): infer nothing.
          break,
        _ =>
          // A non-vognode (folder, etc.): skip it and keep walking rootward.
          { current = parent; }} }}
  map }

/// A NEW node that INHERITED a foreign repo (the user typed a bare
/// headline under a foreign parent -- neither id nor repo) is not a
/// foreign-creation error: appending it modifies the foreign parent's
/// contains, which FORKS the parent, and the new node belongs in the
/// CLONE's repo, exactly as a new node under an owned parent lands
/// in that parent's repo. This maps each such new node to the pid
/// of the foreign node whose fork it rides: its nearest Active
/// vognode ancestor that is not itself such a new node (a chain of
/// new headlines climbs to the first non-new node), skipping
/// non-vognodes. No entry is recorded when the anchor is missing,
/// Inactive, or (impossibly, since the repo was inherited down the
/// chain) owned -- the node is then still judged a foreign creation.
/// The DefineNode rewrite driven by this map happens in
/// 'validate_and_filter_foreign_instructions', where the ForkSpecs
/// (hence the clones' resolved repos) exist.
pub fn new_foreign_nodes_adopting_clone_repos (
  viewforest : &ViewForest,
  new_nodes_with_inherited_repos : &HashSet<ID>,
  config     : &SkgConfig,
) -> HashMap<ID, ID> {
  let mut map : HashMap<ID, ID> = HashMap::new ();
  for node in viewforest . nodes () {
    let ViewnodeKind::Vognode (Vognode::Active (t)) = & node . value () . kind
      else { continue; };
    if ! new_nodes_with_inherited_repos . contains (& t . id)
      { continue; }
    if config . user_owns_repo (& t . home_repo)
      { continue; } // an owned inheritance is an ordinary creation
    let mut current = node;
    while let Some (parent) = current . parent () {
      match & parent . value () . kind {
        ViewnodeKind::Vognode (Vognode::Active (pt)) => {
          if new_nodes_with_inherited_repos . contains (& pt . id) {
            // Another new headline in the same chain: keep climbing.
            current = parent;
            continue; }
          if ! config . user_owns_repo (& pt . home_repo) {
            map . insert ( t . id . clone (), pt . id . clone () ); }
          break; }
        ViewnodeKind::Vognode (Vognode::Inactive (_)) =>
          break,
        _ =>
          // A non-vognode (folder, etc.): skip it and keep walking rootward.
          { current = parent; }} }}
  map }

/// Everything clone-repo resolution can draw on, in priority order
/// (user_set > explicit_child > inferred_ancestor > default). The
/// first two are user SPECIFICATIONS: a repo resolved from them is
/// 'repo_confirmed', and the confirmation buffer shows it as
/// settled. The last two are guesses: the buffer then shows the
/// PICK-A-REPO placeholder plus the guess as a suggestion, and the
/// client asks the user to choose before approving.
pub struct CloneRepoInputs {
  pub user_set          : HashMap<ID, RepoName>, // What the user chose in the confirmation buffer, riding back on the approve re-save's fork-repos field.
  pub explicit_child    : HashMap<ID, RepoName>, // What the saved metadata already specified, via explicit repos on N's new children ('explicit_new_child_repos_for_foreign_vognodes').
  pub inferred_ancestor : HashMap<ID, RepoName>, // N's nearest owned ancestor in the view ('owned_ancestor_repos_for_foreign_vognodes') -- always active.
  pub default           : Option<RepoName>,      // The caller's active-aware config-first owned repo.
}

/// Build the ForkSpec for a fork of the foreign buffer node N,
/// resolving C's owned repo per 'CloneRepoInputs'. Every fork
/// carries a concrete owned repo unless the user owns NO repo at
/// all -- only then does 'ForkRepoUnresolved' fire. (The chosen
/// repo is validated owned + active later, in
/// 'validate_fork_specs'; resolution here only fills it. The default
/// is active-aware so that, under a restricted repo-set with both
/// an inactive and an active owned repo, the fork still reaches the
/// confirmation buffer rather than dead-ending on
/// 'ForkRepoInactive'.)
pub fn fork_spec_from_buffer_node (
  buffer_node   : &NodeComplete,
  disk_title    : &str, // N's original title (before the edit), for the confirmation buffer's child line.
  disk_contains : &[ID], // N's original contains (before the edit); children the edit deleted become the clone's hides.
  repos       : &CloneRepoInputs,
) -> Result<ForkSpec, BufferValidationError> {
  let specified : Option<RepoName> =
    repos . user_set . get (& buffer_node . pid) . cloned ()
    . or_else ( || repos . explicit_child . get (& buffer_node . pid)
                   . cloned () );
  let repo_confirmed : bool =
    specified . is_some ();
  let clone_repo : RepoName =
    specified
    . or_else ( || repos . inferred_ancestor
                   . get (& buffer_node . pid) . cloned () )
    . or_else ( || repos . default . clone () )
    . ok_or_else ( || BufferValidationError::ForkRepoUnresolved (
        buffer_node . pid . clone () )) ?;
  Ok ( build_fork_clone (
    buffer_node, disk_title, disk_contains, clone_repo,
    repo_confirmed ) ) }

/// The clone-repo specification Case 2 of TODO/fork-fixes.org asks
/// for: when the fork-forcing edit is the addition of NEW children
/// whose metadata EXPLICITLY names an owned repo, the user has
/// already said where this material belongs, so the clone of the
/// foreign parent goes there too and the confirmation flow does not
/// ask again. Per foreign node N, this records the one owned repo
/// its new explicit-repo immediate Active children agree on;
/// nothing is recorded when they disagree (ambiguous -- the flow then
/// asks) or when there are none. 'new_nodes_with_explicit_repos' is
/// enrichment's new-nodes set MINUS its inherited-repo set: only a
/// repo the user actually typed counts as a specification.
pub fn explicit_new_child_repos_for_foreign_vognodes (
  viewforest : &ViewForest,
  new_nodes_with_explicit_repos : &HashSet<ID>,
  config     : &SkgConfig,
) -> HashMap<ID, RepoName> {
  let mut map : HashMap<ID, RepoName> = HashMap::new ();
  let mut ambiguous : HashSet<ID> = HashSet::new ();
  for node in viewforest . nodes () {
    let ViewnodeKind::Vognode (Vognode::Active (t)) = & node . value () . kind
      else { continue; };
    if config . user_owns_repo (& t . home_repo)
      { continue; } // only a foreign node forks
    for child in node . children () {
      let ViewnodeKind::Vognode (Vognode::Active (ct))
        = & child . value () . kind
        else { continue; };
      if ! new_nodes_with_explicit_repos . contains (& ct . id)
        { continue; }
      if ! config . user_owns_repo (& ct . home_repo)
        { continue; } // an explicitly-foreign new child is an error elsewhere, not a specification
      match map . get (& t . id) {
        Some (prior) if prior != & ct . home_repo =>
          { ambiguous . insert ( t . id . clone () ); }
        _ =>
          { map . insert ( t . id . clone (),
                           ct . home_repo . clone () ); }} }}
  for id in &ambiguous { map . remove (id); }
  map }

/// The sentinel repo the confirmation buffer pre-fills for a
/// clone-to-be whose repo the user has not SPECIFIED (neither in
/// the saved metadata nor in a prior confirmation round). It must be
/// replaced by a real owned repo before approving: the client
/// prompts for one per placeholder-carrying clone (or the user sets
/// it with C-c s s), and refuses to approve while any remains (the
/// server would reject it as an unknown repo anyway). Must match
/// 'skg-fork-repo-placeholder' in
/// [[../../elisp/skg-request-save.el]].
pub const FORK_REPO_PLACEHOLDER : &str = "PICK-A-REPO";

/// Build the fork-confirmation buffer. Its head is an org headline
/// whose BODY holds the explanation (foldable; a long '#' comment
/// block annoyed in practice -- TODO/fork-fixes.org Case 2). Then two
/// levels per fork, because one headline cannot honestly stand for
/// both the original N and the (possibly re-titled) clone C:
///
///   * <edited title>     -- the CLONE-TO-BE C: the user's edited title,
///                           NO id (none yet). Its (repo ...) is the
///                           resolved repo when repo_confirmed, else
///                           the PICK-A-REPO placeholder, preceded by a
///                           one-line suggestion comment the client
///                           offers as the prompt's default.
///   ** <original title>  -- the ORIGINAL N that C overrides: its real id,
///                           real repo, write-protected,
///                           affectsParent=false, marked "pO".
///
/// The client shows this and asks the user to approve (re-save the
/// origin with the chosen repos) or decline (kill the buffer),
/// prompting first for each placeholder. Every fork headline carries
/// real (skg ...) metadata so the buffer stays navigable -- the usual
/// ID-stack-push / search commands work on it.
pub fn build_fork_confirmation_buffer (
  fork_specs : &[ForkSpec],
) -> String {
  let mut out : String = String::new ();
  out . push_str (
    "* Fork confirmation -- what this buffer is\n\
     Forking turns a node into an editable clone, in a repo you\n\
     own, that subscribes to and overrides the original. Each\n\
     top-level headline below is a clone-to-be; its child is the\n\
     original it forks (real id, marked \"pO\": its visible parent\n\
     overrides it).\n\
     APPROVE with C-c C-c: the origin buffer is re-saved, committing\n\
     each fork into the repo its clone-to-be shows. A clone still\n\
     showing PICK-A-REPO needs a real repo first: Emacs prompts\n\
     for each, or set one yourself with C-c s s on the clone-to-be's\n\
     headline.\n\
     DECLINE with C-c C-k (or kill this buffer): nothing is written.\n" );
  for spec in fork_specs {
    let shown_repo : &str =
      if spec . repo_confirmed {
        // The user already specified it; show it as settled.
        spec . clone . 0 . home_repo . 0 . as_str ()
      } else {
        out . push_str ( & format! (
          "# Suggested repo for the clone below: {}\n",
          spec . clone . 0 . home_repo ));
        FORK_REPO_PLACEHOLDER };
    out . push_str ( & format! (
      "* (skg (node (repo {}) (viewStats (homeRepoHerald {})))) {}\n",
      metadata_value_atom (shown_repo),
      metadata_value_atom (&format! ("⌂:{}", shown_repo)),
      spec . clone . 0 . title ));
    out . push_str ( & format! (
      "** (skg (node (id {}) (repo {}) (affectsParent false) writeProtected \
       (viewStats parentOverrides))) {}\n",
      spec . original_id . 0,
      metadata_value_atom (&spec . original_repo),
      spec . original_title )); }
  out }

/// Reject any fork that monogamy or the repo-set forbids. Run after
/// the clones are built (their repos resolved) but before the save
/// commits anything:
/// - *monogamy*: a node may have at most one user-owned overrider, so
///   forking an N the user has already forked would violate it. Detect
///   the existing clone against the LIVE graph and reject with
///   'ForkAlreadyExists' (naming it) rather than letting the raw
///   MultipleUserOwnedOverriders fire at commit. A monogamy-blocked fork
///   is not also reported for its repo.
/// - *owned*: the clone's resolved repo must be one the user owns.
///   Inference and the default only ever yield owned repos, but a
///   user-set repo (typed, or hand-edited) might not be -- reject with
///   'ForkRepoNotOwned'.
/// - *repo-set*: the clone's owned repo must be ACTIVE under the
///   active repo-set. Under a restricted set the user is not meant to
///   touch inactive repos, and an invisible clone is never created
///   silently; reject with 'ForkRepoInactive'.
///
/// 'restricted_repo_set' is None when nothing is restricted (the set
/// 'all'). The monogamy check uses the explicit save-planning graph; the
/// commit-time invariant check remains a defense in depth.
pub fn validate_fork_specs_in_graph (
  fork_specs            : &[ForkSpec],
  graph                 : &crate::dbs::in_rust_graph::InRustGraph,
  config                : &SkgConfig,
  restricted_repo_set : Option<&ActiveRepoSet>,
) -> Vec<BufferValidationError> {
  let mut errors : Vec<BufferValidationError> = Vec::new ();
  for spec in fork_specs {
    if let Some (existing) = existing_user_owned_overrider_of (
        config, graph, & spec . original_id )
      { errors . push (
          BufferValidationError::ForkAlreadyExists (
            spec . original_id . clone (), existing ));
        continue; }
    let clone_repo : &RepoName = & spec . clone . 0 . home_repo;
    if ! config . user_owns_repo (clone_repo) {
      errors . push (
        BufferValidationError::ForkRepoNotOwned (
          spec . original_id . clone (),
          clone_repo . clone () ));
      continue; }
    let active : bool =
      restricted_repo_set
      . map_or ( true, |a| a . contains_repo (clone_repo) );
    if ! active {
      errors . push (
        BufferValidationError::ForkRepoInactive (
          spec . original_id . clone (),
          clone_repo . clone () )); }}
  errors }

pub fn validate_fork_specs (
  fork_specs            : &[ForkSpec],
  config                : &SkgConfig,
  restricted_repo_set : Option<&ActiveRepoSet>,
) -> Vec<BufferValidationError> {
  let graph = match read_all_skg_files_from_repos (config) {
    Ok (nodes) => InRustGraph::from_nodecompletes (&nodes),
    Err (e) => return vec! [BufferValidationError::Other (
      format! ("Could not read graph for fork validation: {}", e))], };
  validate_fork_specs_in_graph (
    fork_specs, &graph, config, restricted_repo_set ) }

/// Construct the clone C from the edited foreign buffer node N and a
/// resolved owned repo. C copies N's title/body/contains (the
/// edited buffer values -- N is already disk-supplemented, so no disk
/// fetch is needed, unlike nodeMerge whose acquiree is only an ID
/// reference), subscribes_to = [N] and overrides_view_of = [N], a
/// fresh pid, and the owned repo. C's hides are the children the
/// forking edit DELETED (disk_contains minus the edited contains):
/// the user dismissed them, so they must not reappear as
/// unintegrated subscribed content. (Children the clone keeps need no
/// hide -- the display rule already excludes C's own contains from
/// its subscribee-as-such view.)
/// contains is stored RAW (the child IDs as the buffer collected them);
/// override substitution applies at render time.
pub fn build_fork_clone (
  buffer_node   : &NodeComplete,
  disk_title    : &str, // N's original (pre-edit) title, kept for the confirmation buffer's child line.
  disk_contains : &[ID], // N's original (pre-edit) contains.
  clone_repo  : RepoName,
  repo_confirmed : bool, // whether clone_repo was user-SPECIFIED (vs inferred or defaulted)
) -> ForkSpec {
  let buffer_contains_ids : Vec<ID> =
    members_of (& buffer_node . contains);
  let clone : NodeComplete = NodeComplete {
    title         : buffer_node . title . clone (),
    overPrivateText_telescope : false,
    aliases       : MSV::Unspecified,
    pid           : ID ( uuid::Uuid::new_v4 () . to_string () ),
    extra_ids     : Vec::new (),
    body          : buffer_node . body . clone (),
    contains      : rel_partners_at_relRepo (
      &clone_repo, buffer_contains_ids . clone () ),
    subscribes_to : MSV::Specified ( rel_partners_at_relRepo (
      &clone_repo, vec! [ buffer_node . pid . clone () ] )),
    hides_from_its_subscriptions : MSV::Specified ( rel_partners_at_relRepo (
      &clone_repo,
      // The children the forking edit deleted.
      disk_contains . iter ()
        . filter ( |id| ! buffer_contains_ids . contains (id) )
        . cloned () . collect () )),
    overrides_view_of : MSV::Specified ( rel_partners_at_relRepo (
      &clone_repo, vec! [ buffer_node . pid . clone () ] )),
    // The clone preserves the original node's search-matching choice, but
    // importer provenance flags do not describe the newly-created clone.
    misc          : if flag_is_true (
      &buffer_node . misc, Flag::NoSearchMatching)
      { vec![Flag::NoSearchMatching] }
      else { Vec::new () },
    home_repo        : clone_repo,
  };
  ForkSpec {
    clone           : SaveNode (clone),
    original_id     : buffer_node . pid . clone (),
    // The ORIGINAL title (N's disk title), distinct from the clone's
    // edited title above -- the two-level confirmation buffer shows both.
    original_title  : disk_title . to_string (),
    original_repo : buffer_node . home_repo . clone (),
    repo_confirmed,
  }}

#[cfg(test)]
#[allow(non_snake_case)]
#[path = "../../tests/unit/fork.rs"]
mod tests;
