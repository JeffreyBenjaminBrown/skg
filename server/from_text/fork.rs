//! Fork support. Editing a foreign node N (one in a skgrepo the user
//! does not own) is read as a request to CLONE it: the clone C lives in
//! an owned skgrepo, copies N's edited title/body/contains, subscribes
//! to N and overrides N. N itself is left untouched.
//!
//! Detection happens in 'apply_foreign_policy' (validate.rs): a foreign
//! SaveNode whose buffer content differs from disk is a fork candidate
//! rather than a 'ModifiedForeignNode' error. This module resolves the
//! clone's owned repo (from N's nearest owned ancestor in the view)
//! and builds C's SaveNode. The confirmation-gating and the skgsave-commit live
//! in the save handler.

use crate::dbs::in_rust_graph::override_invariants::existing_owned_overrider_of;
use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::filesystem::multiple_nodes::read_all_skg_files_from_skgrepos;
use crate::org_to_text::metadata_value_atom;
use crate::skgrepo_sets::SkgrepoRestriction;
use crate::types::errors::BufferValidationError;
use crate::types::misc::{ID, MSV, SkgConfig, SkgrepoName, members_of, rel_partners_at_relRepo};
use crate::types::nodes::complete::{
  Flag, Graphnode, flag_is_true};
use crate::types::save::{ForkSpec, SaveNode};
use crate::types::tree::forest::ViewForest;
use crate::types::viewnode::{ViewnodeKind, Vognode};

use std::collections::{HashMap, HashSet};

/// For every FOREIGN vognode in the view, the skgrepo of its nearest
/// vognode ancestor, recorded IFF that ancestor is an OWNED Unrestricted
/// vognode. A fork's clone C must live in an owned skgrepo; the foreign
/// node N's own repo is write-protected, so C inherits from N's IMMEDIATE
/// container context -- the nearest vognode ancestor reached by skipping
/// only non-vognodes (folders, etc.). The walk STOPS at that nearest vognode
/// ancestor and never passes it: if the ancestor is foreign (or
/// restricted), nothing is inferred (the skgrepo then defaults, or the user
/// sets it in the confirmation buffer). Inferring a distant owned node
/// reached by skipping a foreign ancestor would be wrong -- a clone
/// belongs in the skgrepo of the node that actually contains N here.
/// A foreign node drawn at several positions can have different nearest
/// ancestors; the first OWNED one reached in preorder wins -- monogamy
/// means at most one clone anyway, and the user can override the choice
/// in the confirmation buffer.
pub fn owned_ancestor_skgrepos_for_foreign_vognodes (
  viewforest : &ViewForest,
  config     : &SkgConfig,
) -> HashMap<ID, SkgrepoName> {
  let mut map : HashMap<ID, SkgrepoName> = HashMap::new ();
  for node in viewforest . nodes () {
    let ViewnodeKind::Vognode (Vognode::Unrestricted (t)) = & node . value () . kind
      else { continue; };
    if config . skgrepo_is_owned (& t . home_skgrepo) { continue; } // not foreign
    let mut current = node;
    while let Some (parent) = current . parent () {
      match & parent . value () . kind {
        ViewnodeKind::Vognode (Vognode::Unrestricted (pt)) => {
          // N's nearest vognode ancestor: record its skgrepo IFF owned,
          // then stop -- never walk past it.
          if config . skgrepo_is_owned (& pt . home_skgrepo) {
            map . entry ( t . skgid . clone () )
              . or_insert_with ( || pt . home_skgrepo . clone () ); }
          break; }
        ViewnodeKind::Vognode (Vognode::Restricted (_)) =>
          // A restricted vognode is a real container boundary too (and
          // never an owned skgrepo): infer nothing.
          break,
        _ =>
          // A non-vognode (folder, etc.): skip it and keep walking rootward.
          { current = parent; }} }}
  map }

/// A NEW node that INHERITED a foreign skgrepo (the user typed a bare
/// headline under a foreign parent -- neither id nor skgrepo) is not a
/// foreign-creation error: appending it modifies the foreign parent's
/// contains, which FORKS the parent, and the new node belongs in the
/// CLONE's skgrepo, exactly as a new node under an owned parent lands
/// in that parent's skgrepo. This maps each such new node to the pid
/// of the foreign node whose fork it rides: its nearest Unrestricted
/// vognode ancestor that is not itself such a new node (a chain of
/// new headlines climbs to the first non-new node), skipping
/// non-vognodes. No entry is recorded when the anchor is missing,
/// Restricted, or (impossibly, since the skgrepo was inherited down the
/// chain) owned -- the node is then still judged a foreign creation.
/// The NodeInstruction rewrite driven by this map happens in
/// 'validate_and_filter_foreign_instructions', where the ForkSpecs
/// (hence the clones' resolved skgrepos) exist.
pub fn new_foreign_nodes_adopting_clone_skgrepos (
  viewforest                        : &ViewForest,
  new_nodes_with_inherited_skgrepos : &HashSet<ID>,
  config                            : &SkgConfig,
) -> HashMap<ID, ID> {
  let mut map : HashMap<ID, ID> = HashMap::new ();
  for node in viewforest . nodes () {
    let ViewnodeKind::Vognode (Vognode::Unrestricted (t)) = & node . value () . kind
      else { continue; };
    if ! new_nodes_with_inherited_skgrepos . contains (& t . skgid)
      { continue; }
    if config . skgrepo_is_owned (& t . home_skgrepo)
      { continue; } // an owned inheritance is an ordinary creation
    let mut current = node;
    while let Some (parent) = current . parent () {
      match & parent . value () . kind {
        ViewnodeKind::Vognode (Vognode::Unrestricted (pt)) => {
          if new_nodes_with_inherited_skgrepos . contains (& pt . skgid) {
            // Another new headline in the same chain: keep climbing.
            current = parent;
            continue; }
          if ! config . skgrepo_is_owned (& pt . home_skgrepo) {
            map . insert ( t . skgid . clone (), pt . skgid . clone () ); }
          break; }
        ViewnodeKind::Vognode (Vognode::Restricted (_)) =>
          break,
        _ =>
          // A non-vognode (folder, etc.): skip it and keep walking rootward.
          { current = parent; }} }}
  map }

/// Everything clone-repo resolution can draw on, in priority order
/// (user_set > explicit_child > inferred_ancestor > default). The
/// first two are user SPECIFICATIONS: a skgrepo resolved from them is
/// 'repo_confirmed', and the confirmation buffer shows it as
/// settled. The last two are guesses: the buffer then shows the
/// PICK-A-REPO placeholder plus the guess as a suggestion, and the
/// client asks the user to choose before approving.
pub struct CloneSkgrepoInputs {
  pub user_set          : HashMap<ID, SkgrepoName>, // What the user chose in the confirmation buffer, riding back on the approve re-save's fork-repos field.
  pub explicit_child    : HashMap<ID, SkgrepoName>, // What the saved metadata already specified, via explicit skgrepos on N's new children ('explicit_new_child_repos_for_foreign_vognodes').
  pub inferred_ancestor : HashMap<ID, SkgrepoName>, // N's nearest owned ancestor in the view ('owned_ancestor_repos_for_foreign_vognodes') -- always unrestricted.
  pub default           : Option<SkgrepoName>,      // The caller's active-aware config-first owned skgrepo.
}

/// Build the ForkSpec for a fork of the foreign buffer node N,
/// resolving C's owned repo per 'CloneRepoInputs'. Every fork
/// carries a concrete owned skgrepo unless the user owns NO skgrepo at
/// all -- only then does 'ForkRepoUnresolved' fire. (The chosen
/// skgrepo is validated owned + unrestricted later, in
/// 'validate_fork_specs'; resolution here only fills it. The default
/// is active-aware so that, under a restricted skgrepo-set with both
/// a restricted and an unrestricted owned skgrepo, the fork still reaches the
/// confirmation buffer rather than dead-ending on
/// 'ForkRepoRestricted'.)
pub fn fork_spec_from_buffer_node (
  buffer_node   : &Graphnode,
  disk_title    : &str, // N's original title (before the edit), for the confirmation buffer's child line.
  disk_contains : &[ID], // N's original contains (before the edit); children the edit deleted become the clone's hides.
  skgrepos      : &CloneSkgrepoInputs,
) -> Result<ForkSpec, BufferValidationError> {
  let specified : Option<SkgrepoName> =
    skgrepos . user_set . get (& buffer_node . pid) . cloned ()
    . or_else ( || skgrepos . explicit_child . get (& buffer_node . pid)
                   . cloned () );
  let skgrepo_confirmed : bool =
    specified . is_some ();
  let clone_skgrepo : SkgrepoName =
    specified
    . or_else ( || skgrepos . inferred_ancestor
                   . get (& buffer_node . pid) . cloned () )
    . or_else ( || skgrepos . default . clone () )
    . ok_or_else ( || BufferValidationError::ForkSkgrepoUnresolved (
        buffer_node . pid . clone () )) ?;
  Ok ( build_fork_clone (
    buffer_node, disk_title, disk_contains, clone_skgrepo,
    skgrepo_confirmed ) ) }

/// The clone-repo specification Case 2 of TODO/fork-fixes.org asks
/// for: when the fork-forcing edit is the addition of NEW children
/// whose metadata EXPLICITLY names an owned skgrepo, the user has
/// already said where this material belongs, so the clone of the
/// foreign parent goes there too and the confirmation flow does not
/// ask again. Per foreign node N, this records the one owned skgrepo
/// its new explicit-repo immediate Unrestricted children agree on;
/// nothing is recorded when they disagree (ambiguous -- the flow then
/// asks) or when there are none. 'new_nodes_with_explicit_repos' is
/// enrichment's new-nodes set MINUS its inherited-repo set: only a
/// skgrepo the user actually typed counts as a specification.
pub fn explicit_new_child_skgrepos_for_foreign_vognodes (
  viewforest                       : &ViewForest,
  new_nodes_with_explicit_skgrepos : &HashSet<ID>,
  config                           : &SkgConfig,
) -> HashMap<ID, SkgrepoName> {
  let mut map       : HashMap<ID, SkgrepoName> = HashMap::new ();
  let mut ambiguous : HashSet<ID> = HashSet::new ();
  for node in viewforest . nodes () {
    let ViewnodeKind::Vognode (Vognode::Unrestricted (t)) = & node . value () . kind
      else { continue; };
    if config . skgrepo_is_owned (& t . home_skgrepo)
      { continue; } // only a foreign node forks
    for child in node . children () {
      let ViewnodeKind::Vognode (Vognode::Unrestricted (ct))
        = & child . value () . kind
        else { continue; };
      if ! new_nodes_with_explicit_skgrepos . contains (& ct . skgid)
        { continue; }
      if ! config . skgrepo_is_owned (& ct . home_skgrepo)
        { continue; } // an explicitly-foreign new child is an error elsewhere, not a specification
      match map . get (& t . skgid) {
        Some (prior) if prior != & ct . home_skgrepo =>
          { ambiguous . insert ( t . skgid . clone () ); }
        _ =>
          { map . insert ( t . skgid . clone (),
                           ct . home_skgrepo . clone () ); }} }}
  for skgid in &ambiguous { map . remove (skgid); }
  map }

/// The sentinel skgrepo the confirmation buffer pre-fills for a
/// clone-to-be whose skgrepo the user has not SPECIFIED (neither in
/// the saved metadata nor in a prior confirmation round). It must be
/// replaced by a real owned skgrepo before approving: the client
/// prompts for one per placeholder-carrying clone (or the user sets
/// it with C-c s s), and refuses to approve while any remains (the
/// server would reject it as an unknown skgrepo anyway). Must match
/// 'skg-fork-repo-placeholder' in
/// [[../../elisp/skg-request-save.el]].
pub const FORK_SKGREPO_PLACEHOLDER : &str = "PICK-A-REPO";

/// Build the fork-confirmation buffer. Its head is an org headline
/// whose BODY holds the explanation (foldable; a long '#' comment
/// block annoyed in practice -- TODO/fork-fixes.org Case 2). Then two
/// levels per fork, because one headline cannot honestly stand for
/// both the original N and the (possibly re-titled) clone C:
///
///   * <edited title>     -- the CLONE-TO-BE C: the user's edited title,
///                           NO id (none yet). Its (repo ...) is the
///                           resolved skgrepo when repo_confirmed, else
///                           the PICK-A-REPO placeholder, preceded by a
///                           one-line suggestion comment the client
///                           offers as the prompt's default.
///   ** <original title>  -- the ORIGINAL N that C overrides: its real id,
///                           real skgrepo, write-protected,
///                           affectsParent=false, marked "pO".
///
/// The client shows this and asks the user to approve (re-save the
/// source buffer with the chosen skgrepos) or decline (kill the buffer),
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
     original it forks (real id, marked \"pO\": its viewparent\n\
     overrides it).\n\
     APPROVE with C-c C-c: the source buffer is re-saved, skgsave-committing\n\
     each fork into the repo its clone-to-be shows. A clone still\n\
     showing PICK-A-REPO needs a real repo first: Emacs prompts\n\
     for each, or set one yourself with C-c s s on the clone-to-be's\n\
     headline.\n\
     DECLINE with C-c C-k (or kill this buffer): nothing is written.\n" );
  for spec in fork_specs {
    let shown_skgrepo : &str =
      if spec . skgrepo_confirmed {
        // The user already specified it; show it as settled.
        spec . clone . 0 . home_skgrepo . 0 . as_str ()
      } else {
        out . push_str ( & format! (
          "# Suggested repo for the clone below: {}\n",
          spec . clone . 0 . home_skgrepo ));
        FORK_SKGREPO_PLACEHOLDER };
    out . push_str ( & format! (
      "* (skg (node (repo {}) (viewStats (homeRepoHerald {})))) {}\n",
      metadata_value_atom (shown_skgrepo),
      metadata_value_atom (&format! ("⌂:{}", shown_skgrepo)),
      spec . clone . 0 . title ));
    out . push_str ( & format! (
      "** (skg (node (id {}) (repo {}) (affectsParent false) writeProtected \
       (viewStats parentOverrides))) {}\n",
      spec . original_skgid . 0,
      metadata_value_atom (&spec . original_skgrepo),
      spec . original_title )); }
  out }

/// Reject any fork that monogamy or the skgrepo-set forbids. Run after
/// the clones are built (their skgrepos resolved) but before the save
/// skgsave-commits anything:
/// - *monogamy*: a node may have at most one owned overrider, so
///   forking an N the user has already forked would violate it. Detect
///   the existing clone against the LIVE graph and reject with
///   'ForkAlreadyExists' (naming it) rather than letting the raw
///   MultipleOwnedOverriders fire at skgsave-commit. A monogamy-blocked fork
///   is not also reported for its skgrepo.
/// - *owned*: the clone's resolved skgrepo must be one the user owns.
///   Inference and the default only ever yield owned skgrepos, but a
///   user-set skgrepo (typed, or hand-edited) might not be -- reject with
///   'ForkRepoNotOwned'.
/// - *skgrepo-set*: the clone's owned skgrepo must be UNRESTRICTED under the
///   skgrepo restriction. Under a restricted set the user is not meant to
///   touch restricted skgrepos, and an invisible clone is never created
///   silently; reject with 'ForkRepoRestricted'.
///
/// 'restricted_repo_set' is None when nothing is restricted (the set
/// 'all'). The monogamy check uses the explicit save-planning graph; the
/// skgsave-commit-time invariant check remains a defense in depth.
pub fn validate_fork_specs_in_graph (
  fork_specs             : &[ForkSpec],
  graph                  : &crate::dbs::in_rust_graph::InRustGraph,
  config                 : &SkgConfig,
  skgrepo_restriction    : Option<&SkgrepoRestriction>,
) -> Vec<BufferValidationError> {
  let mut errors : Vec<BufferValidationError> = Vec::new ();
  for spec in fork_specs {
    if let Some (existing) = existing_owned_overrider_of (
        config, graph, & spec . original_skgid )
      { errors . push (
          BufferValidationError::ForkAlreadyExists (
            spec . original_skgid . clone (), existing ));
        continue; }
    let clone_skgrepo : &SkgrepoName = & spec . clone . 0 . home_skgrepo;
    if ! config . skgrepo_is_owned (clone_skgrepo) {
      errors . push (
        BufferValidationError::ForkSkgrepoNotOwned (
          spec . original_skgid . clone (),
          clone_skgrepo . clone () ));
      continue; }
    let unrestricted : bool =
      skgrepo_restriction
      . map_or ( true, |a| a . contains_skgrepo (clone_skgrepo) );
    if ! unrestricted {
      errors . push (
        BufferValidationError::ForkSkgrepoRestricted (
          spec . original_skgid . clone (),
          clone_skgrepo . clone () )); }}
  errors }

pub fn validate_fork_specs (
  fork_specs             : &[ForkSpec],
  config                 : &SkgConfig,
  skgrepo_restriction    : Option<&SkgrepoRestriction>,
) -> Vec<BufferValidationError> {
  let graph = match read_all_skg_files_from_skgrepos (config) {
    Ok (nodes) => InRustGraph::from_graphnodes (&nodes),
    Err (e) => return vec! [BufferValidationError::Other (
      format! ("Could not read graph for fork validation: {}", e))], };
  validate_fork_specs_in_graph (
    fork_specs, &graph, config, skgrepo_restriction ) }

/// Construct the clone C from the edited foreign buffer node N and a
/// resolved owned skgrepo. C copies N's title/body/contains (the
/// edited buffer values -- N is already disk-supplemented, so no disk
/// fetch is needed, unlike nodeMerge whose acquiree is only an ID
/// reference), subscribesTo = [N] and overrides = [N], a
/// fresh pid, and the owned skgrepo. C's hides are the children the
/// forking edit DELETED (disk_contains minus the edited contains):
/// the user dismissed them, so they must not reappear as
/// unintegrated subscribed content. (Children the clone keeps need no
/// hide -- the display rule already excludes C's own contains from
/// its subscribee-as-such view.)
/// contains is stored RAW (the child IDs as the buffer collected them);
/// override substitution applies at render time.
pub fn build_fork_clone (
  buffer_node       : &Graphnode,
  disk_title        : &str, // N's original (pre-edit) title, kept for the confirmation buffer's child line.
  disk_contains     : &[ID], // N's original (pre-edit) contains.
  clone_skgrepo     : SkgrepoName,
  skgrepo_confirmed : bool, // whether clone_repo was user-SPECIFIED (vs inferred or defaulted)
) -> ForkSpec {
  let buffer_contains_skgids : Vec<ID> =
    members_of (& buffer_node . contains);
  let clone : Graphnode = Graphnode {
    title         : buffer_node . title . clone (),
    overPrivateText_telescope : false,
    aliases       : MSV::Unspecified,
    pid           : ID ( uuid::Uuid::new_v4 () . to_string () ),
    extra_ids     : Vec::new (),
    body          : buffer_node . body . clone (),
    contains      : rel_partners_at_relRepo (
      &clone_skgrepo, buffer_contains_skgids . clone () ),
    subscribesTo  : MSV::Specified ( rel_partners_at_relRepo (
      &clone_skgrepo, vec! [ buffer_node . pid . clone () ] )),
    hidesFromSubs : MSV::Specified ( rel_partners_at_relRepo (
      &clone_skgrepo,
      // The children the forking edit deleted.
      disk_contains . iter ()
        . filter ( |skgid| ! buffer_contains_skgids . contains (skgid) )
        . cloned () . collect () )),
    overrides : MSV::Specified ( rel_partners_at_relRepo (
      &clone_skgrepo, vec! [ buffer_node . pid . clone () ] )),
    // The clone preserves the original node's search-matching choice, but
    // importer provenance flags do not describe the newly-created clone.
    flags          : if flag_is_true (
      &buffer_node . flags, Flag::NoSearchMatching)
      { vec![Flag::NoSearchMatching] }
      else { Vec::new () },
    home_skgrepo        : clone_skgrepo,
  };
  ForkSpec {
    clone           : SaveNode (clone),
    original_skgid  : buffer_node . pid . clone (),
    // The ORIGINAL title (N's disk title), distinct from the clone's
    // edited title above -- the two-level confirmation buffer shows both.
    original_title   : disk_title . to_string (),
    original_skgrepo : buffer_node . home_skgrepo . clone (),
    skgrepo_confirmed,
  }}

#[cfg(test)]
#[allow(non_snake_case)]
#[path = "../../tests/unit/fork.rs"]
mod tests;
