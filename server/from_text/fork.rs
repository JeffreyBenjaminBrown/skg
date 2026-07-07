//! Fork support. Editing a foreign node N (one in a source the user
//! does not own) is read as a request to CLONE it: the clone C lives in
//! an owned source, copies N's edited title/body/contains, subscribes
//! to N and overrides N. N itself is left untouched.
//!
//! Detection happens in 'apply_foreign_policy' (validate.rs): a foreign
//! SaveNode whose buffer content differs from disk is a fork candidate
//! rather than a 'ModifiedForeignNode' error. This module resolves the
//! clone's owned source (from N's nearest owned ancestor in the view)
//! and builds C's SaveNode. The confirmation-gating and the commit live
//! in the save handler.

use crate::dbs::in_rust_graph::override_invariants::existing_user_owned_overrider_of;
use crate::dbs::in_rust_graph::snapshot_global;
use crate::source_sets::ActiveSourceSet;
use crate::types::errors::BufferValidationError;
use crate::types::misc::{ID, MSV, SkgConfig, SourceName};
use crate::types::nodes::complete::NodeComplete;
use crate::types::save::{ForkSpec, SaveNode};
use crate::types::tree::forest::ViewForest;
use crate::types::viewnode::{ViewNodeKind, Vognode};

use std::collections::{HashMap, HashSet};

/// For every FOREIGN vognode in the view, the source of its nearest
/// vognode ancestor, recorded IFF that ancestor is an OWNED Active
/// vognode. A fork's clone C must live in an owned source; the foreign
/// node N's own source is read-only, so C inherits from N's IMMEDIATE
/// container context -- the nearest vognode ancestor reached by skipping
/// only scaffolds (cols, etc.). The walk STOPS at that nearest vognode
/// ancestor and never passes it: if the ancestor is foreign (or
/// inactive), nothing is inferred (the source then defaults, or the user
/// sets it in the confirmation buffer). Inferring a distant owned node
/// reached by skipping a foreign ancestor would be wrong -- a clone
/// belongs in the source of the node that actually contains N here.
/// A foreign node drawn at several positions can have different nearest
/// ancestors; the first OWNED one reached in preorder wins -- monogamy
/// means at most one clone anyway, and the user can override the choice
/// in the confirmation buffer.
pub fn owned_ancestor_sources_for_foreign_vognodes (
  viewforest : &ViewForest,
  config     : &SkgConfig,
) -> HashMap<ID, SourceName> {
  let mut map : HashMap<ID, SourceName> = HashMap::new ();
  for node in viewforest . nodes () {
    let ViewNodeKind::Vognode (Vognode::Active (t)) = & node . value () . kind
      else { continue; };
    if config . user_owns_source (& t . source) { continue; } // not foreign
    let mut current = node;
    while let Some (parent) = current . parent () {
      match & parent . value () . kind {
        ViewNodeKind::Vognode (Vognode::Active (pt)) => {
          // N's nearest vognode ancestor: record its source IFF owned,
          // then stop -- never walk past it.
          if config . user_owns_source (& pt . source) {
            map . entry ( t . id . clone () )
              . or_insert_with ( || pt . source . clone () ); }
          break; }
        ViewNodeKind::Vognode (Vognode::Inactive (_)) =>
          // An inactive vognode is a real container boundary too (and
          // never an owned source): infer nothing.
          break,
        _ =>
          // A scaffold (col, etc.): skip it and keep walking rootward.
          { current = parent; }} }}
  map }

/// A NEW node that INHERITED a foreign source (the user typed a bare
/// headline under a foreign parent -- neither id nor source) is not a
/// foreign-creation error: appending it modifies the foreign parent's
/// contains, which FORKS the parent, and the new node belongs in the
/// CLONE's source, exactly as a new node under an owned parent lands
/// in that parent's source. This maps each such new node to the pid
/// of the foreign node whose fork it rides: its nearest Active
/// vognode ancestor that is not itself such a new node (a chain of
/// new headlines climbs to the first non-new node), skipping
/// scaffolds. No entry is recorded when the anchor is missing,
/// Inactive, or (impossibly, since the source was inherited down the
/// chain) owned -- the node is then still judged a foreign creation.
/// The DefineNode rewrite driven by this map happens in
/// 'validate_and_filter_foreign_instructions', where the ForkSpecs
/// (hence the clones' resolved sources) exist.
pub fn new_foreign_nodes_adopting_clone_sources (
  viewforest : &ViewForest,
  new_nodes_with_inherited_sources : &HashSet<ID>,
  config     : &SkgConfig,
) -> HashMap<ID, ID> {
  let mut map : HashMap<ID, ID> = HashMap::new ();
  for node in viewforest . nodes () {
    let ViewNodeKind::Vognode (Vognode::Active (t)) = & node . value () . kind
      else { continue; };
    if ! new_nodes_with_inherited_sources . contains (& t . id)
      { continue; }
    if config . user_owns_source (& t . source)
      { continue; } // an owned inheritance is an ordinary creation
    let mut current = node;
    while let Some (parent) = current . parent () {
      match & parent . value () . kind {
        ViewNodeKind::Vognode (Vognode::Active (pt)) => {
          if new_nodes_with_inherited_sources . contains (& pt . id) {
            // Another new headline in the same chain: keep climbing.
            current = parent;
            continue; }
          if ! config . user_owns_source (& pt . source) {
            map . insert ( t . id . clone (), pt . id . clone () ); }
          break; }
        ViewNodeKind::Vognode (Vognode::Inactive (_)) =>
          break,
        _ =>
          // A scaffold (col, etc.): skip it and keep walking rootward.
          { current = parent; }} }}
  map }

/// Everything clone-source resolution can draw on, in priority order
/// (user_set > explicit_child > inferred_ancestor > default). The
/// first two are user SPECIFICATIONS: a source resolved from them is
/// 'source_confirmed', and the confirmation buffer shows it as
/// settled. The last two are guesses: the buffer then shows the
/// PICK-A-SOURCE placeholder plus the guess as a suggestion, and the
/// client asks the user to choose before approving.
pub struct CloneSourceInputs {
  pub user_set          : HashMap<ID, SourceName>, // What the user chose in the confirmation buffer, riding back on the approve re-save's fork-sources field.
  pub explicit_child    : HashMap<ID, SourceName>, // What the saved metadata already specified, via explicit sources on N's new children ('explicit_new_child_sources_for_foreign_vognodes').
  pub inferred_ancestor : HashMap<ID, SourceName>, // N's nearest owned ancestor in the view ('owned_ancestor_sources_for_foreign_vognodes') -- always active.
  pub default           : Option<SourceName>,      // The caller's active-aware config-first owned source.
}

/// Build the ForkSpec for a fork of the foreign buffer node N,
/// resolving C's owned source per 'CloneSourceInputs'. Every fork
/// carries a concrete owned source unless the user owns NO source at
/// all -- only then does 'ForkSourceUnresolved' fire. (The chosen
/// source is validated owned + active later, in
/// 'validate_fork_specs'; resolution here only fills it. The default
/// is active-aware so that, under a restricted source-set with both
/// an inactive and an active owned source, the fork still reaches the
/// confirmation buffer rather than dead-ending on
/// 'ForkSourceInactive'.)
pub fn fork_spec_from_buffer_node (
  buffer_node   : &NodeComplete,
  disk_title    : &str, // N's original title (before the edit), for the confirmation buffer's child line.
  disk_contains : &[ID], // N's original contains (before the edit); children the edit deleted become the clone's hides.
  sources       : &CloneSourceInputs,
) -> Result<ForkSpec, BufferValidationError> {
  let specified : Option<SourceName> =
    sources . user_set . get (& buffer_node . pid) . cloned ()
    . or_else ( || sources . explicit_child . get (& buffer_node . pid)
                   . cloned () );
  let source_confirmed : bool =
    specified . is_some ();
  let clone_source : SourceName =
    specified
    . or_else ( || sources . inferred_ancestor
                   . get (& buffer_node . pid) . cloned () )
    . or_else ( || sources . default . clone () )
    . ok_or_else ( || BufferValidationError::ForkSourceUnresolved (
        buffer_node . pid . clone () )) ?;
  Ok ( build_fork_clone (
    buffer_node, disk_title, disk_contains, clone_source,
    source_confirmed ) ) }

/// The clone-source specification Case 2 of TODO/fork-fixes.org asks
/// for: when the fork-forcing edit is the addition of NEW children
/// whose metadata EXPLICITLY names an owned source, the user has
/// already said where this material belongs, so the clone of the
/// foreign parent goes there too and the confirmation flow does not
/// ask again. Per foreign node N, this records the one owned source
/// its new explicitly-sourced immediate Active children agree on;
/// nothing is recorded when they disagree (ambiguous -- the flow then
/// asks) or when there are none. 'new_nodes_with_explicit_sources' is
/// enrichment's new-nodes set MINUS its inherited-source set: only a
/// source the user actually typed counts as a specification.
pub fn explicit_new_child_sources_for_foreign_vognodes (
  viewforest : &ViewForest,
  new_nodes_with_explicit_sources : &HashSet<ID>,
  config     : &SkgConfig,
) -> HashMap<ID, SourceName> {
  let mut map : HashMap<ID, SourceName> = HashMap::new ();
  let mut ambiguous : HashSet<ID> = HashSet::new ();
  for node in viewforest . nodes () {
    let ViewNodeKind::Vognode (Vognode::Active (t)) = & node . value () . kind
      else { continue; };
    if config . user_owns_source (& t . source)
      { continue; } // only a foreign node forks
    for child in node . children () {
      let ViewNodeKind::Vognode (Vognode::Active (ct))
        = & child . value () . kind
        else { continue; };
      if ! new_nodes_with_explicit_sources . contains (& ct . id)
        { continue; }
      if ! config . user_owns_source (& ct . source)
        { continue; } // an explicitly-foreign new child is an error elsewhere, not a specification
      match map . get (& t . id) {
        Some (prior) if prior != & ct . source =>
          { ambiguous . insert ( t . id . clone () ); }
        _ =>
          { map . insert ( t . id . clone (),
                           ct . source . clone () ); }} }}
  for id in &ambiguous { map . remove (id); }
  map }

/// The sentinel source the confirmation buffer pre-fills for a
/// clone-to-be whose source the user has not SPECIFIED (neither in
/// the saved metadata nor in a prior confirmation round). It must be
/// replaced by a real owned source before approving: the client
/// prompts for one per placeholder-carrying clone (or the user sets
/// it with C-c s s), and refuses to approve while any remains (the
/// server would reject it as an unknown source anyway). Must match
/// 'skg-fork-source-placeholder' in
/// [[../../elisp/skg-request-save.el]].
pub const FORK_SOURCE_PLACEHOLDER : &str = "PICK-A-SOURCE";

/// Build the fork-confirmation buffer. Its head is an org headline
/// whose BODY holds the explanation (foldable; a long '#' comment
/// block annoyed in practice -- TODO/fork-fixes.org Case 2). Then two
/// levels per fork, because one headline cannot honestly stand for
/// both the original N and the (possibly re-titled) clone C:
///
///   * <edited title>     -- the CLONE-TO-BE C: the user's edited title,
///                           NO id (none yet). Its (source ...) is the
///                           resolved source when source_confirmed, else
///                           the PICK-A-SOURCE placeholder, preceded by a
///                           one-line suggestion comment the client
///                           offers as the prompt's default.
///   ** <original title>  -- the ORIGINAL N that C overrides: its real id,
///                           real source, indefinitive,
///                           parentIs=independent, marked "pO".
///
/// The client shows this and asks the user to approve (re-save the
/// origin with the chosen sources) or decline (kill the buffer),
/// prompting first for each placeholder. Every fork headline carries
/// real (skg ...) metadata so the buffer stays navigable -- the usual
/// ID-stack-push / search commands work on it.
pub fn build_fork_confirmation_buffer (
  fork_specs : &[ForkSpec],
) -> String {
  let mut out : String = String::new ();
  out . push_str (
    "* Fork confirmation -- what this buffer is\n\
     Forking turns a node into an editable clone, in a source you\n\
     own, that subscribes to and overrides the original. Each\n\
     top-level headline below is a clone-to-be; its child is the\n\
     original it forks (real id, marked \"pO\": its visible parent\n\
     overrides it).\n\
     APPROVE with C-c C-c: the origin buffer is re-saved, committing\n\
     each fork into the source its clone-to-be shows. A clone still\n\
     showing PICK-A-SOURCE needs a real source first: Emacs prompts\n\
     for each, or set one yourself with C-c s s on the clone-to-be's\n\
     headline.\n\
     DECLINE with C-c C-k (or kill this buffer): nothing is written.\n" );
  for spec in fork_specs {
    let shown_source : &str =
      if spec . source_confirmed {
        // The user already specified it; show it as settled.
        spec . clone . 0 . source . 0 . as_str ()
      } else {
        out . push_str ( & format! (
          "# Suggested source for the clone below: {}\n",
          spec . clone . 0 . source ));
        FORK_SOURCE_PLACEHOLDER };
    out . push_str ( & format! (
      "* (skg (node (source {}) (viewStats (sourceHerald ⌂:{})))) {}\n",
      shown_source, shown_source,
      spec . clone . 0 . title ));
    out . push_str ( & format! (
      "** (skg (node (id {}) (source {}) (parentIs independent) indef \
       (viewStats parentOverrides))) {}\n",
      spec . original_id . 0, spec . original_source,
      spec . original_title )); }
  out }

/// Reject any fork that monogamy or the source-set forbids. Run after
/// the clones are built (their sources resolved) but before the save
/// commits anything:
/// - *monogamy*: a node may have at most one user-owned overrider, so
///   forking an N the user has already forked would violate it. Detect
///   the existing clone against the LIVE graph and reject with
///   'ForkAlreadyExists' (naming it) rather than letting the raw
///   MultipleUserOwnedOverriders fire at commit. A monogamy-blocked fork
///   is not also reported for its source.
/// - *owned*: the clone's resolved source must be one the user owns.
///   Inference and the default only ever yield owned sources, but a
///   user-set source (typed, or hand-edited) might not be -- reject with
///   'ForkSourceNotOwned'.
/// - *source-set*: the clone's owned source must be ACTIVE under the
///   active source-set. Under a restricted set the user is not meant to
///   touch inactive sources, and an invisible clone is never created
///   silently; reject with 'ForkSourceInactive'.
///
/// 'restricted_source_set' is None when nothing is restricted (the set
/// 'all'); the monogamy graph is the process-global snapshot, absent
/// only in tests that bypass it (then monogamy is left to the commit-time
/// invariant check).
pub fn validate_fork_specs (
  fork_specs            : &[ForkSpec],
  config                : &SkgConfig,
  restricted_source_set : Option<&ActiveSourceSet>,
) -> Vec<BufferValidationError> {
  let mut errors : Vec<BufferValidationError> = Vec::new ();
  let graph_snap = snapshot_global ();
  for spec in fork_specs {
    if let Some (graph) = graph_snap . as_deref () {
      if let Some (existing) = existing_user_owned_overrider_of (
        config, graph, & spec . original_id )
      { errors . push (
          BufferValidationError::ForkAlreadyExists (
            spec . original_id . clone (), existing ));
        continue; }}
    let clone_source : &SourceName = & spec . clone . 0 . source;
    if ! config . user_owns_source (clone_source) {
      errors . push (
        BufferValidationError::ForkSourceNotOwned (
          spec . original_id . clone (),
          clone_source . clone () ));
      continue; }
    let active : bool =
      restricted_source_set
      . map_or ( true, |a| a . contains_source (clone_source) );
    if ! active {
      errors . push (
        BufferValidationError::ForkSourceInactive (
          spec . original_id . clone (),
          clone_source . clone () )); }}
  errors }

/// Construct the clone C from the edited foreign buffer node N and a
/// resolved owned source. C copies N's title/body/contains (the
/// edited buffer values -- N is already disk-supplemented, so no disk
/// fetch is needed, unlike nodeMerge whose acquiree is only an ID
/// reference), subscribes_to = [N] and overrides_view_of = [N], a
/// fresh pid, and the owned source. C's hides are the children the
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
  clone_source  : SourceName,
  source_confirmed : bool, // whether clone_source was user-SPECIFIED (vs inferred or defaulted)
) -> ForkSpec {
  let clone : NodeComplete = NodeComplete {
    title         : buffer_node . title . clone (),
    aliases       : MSV::Unspecified,
    source        : clone_source,
    pid           : ID ( uuid::Uuid::new_v4 () . to_string () ),
    extra_ids     : Vec::new (),
    body          : buffer_node . body . clone (),
    contains      : buffer_node . contains . clone (),
    subscribes_to : MSV::Specified ( vec! [ buffer_node . pid . clone () ] ),
    hides_from_its_subscriptions : MSV::Specified (
      // The children the forking edit deleted.
      disk_contains . iter ()
        . filter ( |id| ! buffer_node . contains . contains (id) )
        . cloned () . collect () ),
    overrides_view_of : MSV::Specified ( vec! [ buffer_node . pid . clone () ] ),
    misc          : Vec::new (),
  };
  ForkSpec {
    clone           : SaveNode (clone),
    original_id     : buffer_node . pid . clone (),
    // The ORIGINAL title (N's disk title), distinct from the clone's
    // edited title above -- the two-level confirmation buffer shows both.
    original_title  : disk_title . to_string (),
    original_source : buffer_node . source . clone (),
    source_confirmed,
  }}

#[cfg(test)]
#[allow(non_snake_case)]
#[path = "../../tests/unit/fork.rs"]
mod tests;
