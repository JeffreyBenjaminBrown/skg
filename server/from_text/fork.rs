//! Fork support. Editing a foreign node N (one in a skgrepo the user
//! does not own) is read as a request to clone it: the clone C lives in
//! an owned skgrepo, copies N's edited title/body/contains, subscribes
//! to N and overrides N. N itself is left untouched.
//!
//! Detection happens in 'apply_foreign_policy' (validate.rs): a foreign
//! SaveNode whose buffer content differs from disk is a fork candidate
//! rather than a 'ModifiedForeignNode' error. This module resolves the
//! clone's owned repo (from N's nearest owned ancestor in the view)
//! and builds C's SaveNode. The confirmation-gating and the skgsave-commit live
//! in the save handler.

use crate::dbs::in_rust_graph::mirrors::{
  MIRROR_RELATION, Mirror, Mirrors, mirror_shown_among, mirrors_of,
  perspective_owner_of_tree_containing};
use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::filesystem::multiple_nodes::read_all_skg_files_from_skgrepos;
use crate::org_to_text::skgRepos_form;
use crate::skgrepo_sets::SkgrepoRestriction;
use crate::types::errors::BufferValidationError;
use crate::types::misc::{ID, MSV, RelPartner, SkgConfig, SkgrepoName, SkgrepoOwner, members_of, rel_partners_at_relRepo};
use crate::types::nodes::complete::{
  Flag, Graphnode, flag_is_true};
use crate::types::save::{ForkSpec, SaveNode};
use crate::types::tree::forest::ViewForest;
use crate::types::viewnode::{Viewnode, ViewnodeKind, Vognode};
use crate::types::views_state::OverridesFollowed;

use ego_tree::NodeRef;
use std::collections::{HashMap, HashSet};
use std::iter::once;

/// For every foreign vognode in the view, the skgrepo of its nearest
/// vognode ancestor, recorded iff that ancestor is an owned
/// occurrence. A fork's clone C must live in an owned skgrepo; the foreign
/// node N's own repo is write-protected, so C inherits from N's immediate
/// container context -- the nearest vognode ancestor reached by skipping
/// only non-vognodes (folders, etc.). The walk stops at that nearest vognode
/// ancestor and never passes it: if the ancestor is foreign (or
/// restricted), nothing is inferred (the skgrepo then defaults, or the user
/// sets it in the confirmation buffer). Inferring a distant owned node
/// reached by skipping a foreign ancestor would be wrong -- a clone
/// belongs in the skgrepo of the node that actually contains N here.
/// A foreign node shown at several positions can have different nearest
/// ancestors; the first owned one reached in preorder wins, since one
/// fork makes one clone, and the user can override the choice in the
/// confirmation buffer.
/// A write-protected ancestor's skgrepo is read from its ID
/// (WRITEPROTECTED_SKGREPOS_AFTER_SAVE): what it shows lags the move when
/// the same buffer moved that node's editable occurrence.
pub fn owned_ancestor_skgrepos_for_foreign_vognodes (
  viewforest                         : &ViewForest,
  writeProtected_skgrepos_after_save : &HashMap<ID, SkgrepoName>,
  config                             : &SkgConfig,
) -> HashMap<ID, SkgrepoName> {
  let mut map : HashMap<ID, SkgrepoName> = HashMap::new ();
  for node in viewforest . nodes () {
    let ViewnodeKind::Vognode (Vognode::Occurrence (t)) = & node . value () . kind
      else { continue; };
    if config . skgrepo_is_owned (& t . home_skgrepo) { continue; } // not foreign
    let mut current = node;
    while let Some (parent) = current . parent () {
      match & parent . value () . kind {
        ViewnodeKind::Vognode (Vognode::Occurrence (pt)) => {
          // N's nearest vognode ancestor: record its skgrepo iff owned,
          // then stop -- never walk past it.
          let ancestor_skgrepo : &SkgrepoName =
            if pt . is_writeProtected () {
              writeProtected_skgrepos_after_save . get (& pt . gnid)
              . unwrap_or (& pt . home_skgrepo)
            } else { & pt . home_skgrepo };
          if config . skgrepo_is_owned (ancestor_skgrepo) {
            map . entry ( t . gnid . clone () )
              . or_insert_with ( || ancestor_skgrepo . clone () ); }
          break; }
        ViewnodeKind::Tombstone =>
          // A tombstone is a container boundary too (it may stand for
          // a restricted node): infer nothing.
          break,
        _ =>
          // A non-vognode (folder, etc.): skip it and keep walking rootward.
          { current = parent; }} }}
  map }

/// A new node that inherited a foreign skgrepo (the user typed a bare
/// headline under a foreign parent -- neither id nor skgrepo) is not a
/// foreign-creation error: appending it modifies the foreign parent's
/// contains, which forks the parent, and the new node belongs in the
/// clone's skgrepo, exactly as a new node under an owned parent lands
/// in that parent's skgrepo. This maps each such new node to the pid
/// of the foreign node whose fork it rides: its nearest
/// occurrence ancestor that is not itself such a new node (a chain of
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
    let ViewnodeKind::Vognode (Vognode::Occurrence (t)) = & node . value () . kind
      else { continue; };
    if ! new_nodes_with_inherited_skgrepos . contains (& t . gnid)
      { continue; }
    if config . skgrepo_is_owned (& t . home_skgrepo)
      { continue; } // an owned inheritance is an ordinary creation
    let mut current = node;
    while let Some (parent) = current . parent () {
      match & parent . value () . kind {
        ViewnodeKind::Vognode (Vognode::Occurrence (pt)) => {
          if new_nodes_with_inherited_skgrepos . contains (& pt . gnid) {
            // Another new headline in the same chain: keep climbing.
            current = parent;
            continue; }
          if ! config . skgrepo_is_owned (& pt . home_skgrepo) {
            map . insert ( t . gnid . clone (), pt . gnid . clone () ); }
          break; }
        ViewnodeKind::Tombstone =>
          break,
        _ =>
          // A non-vognode (folder, etc.): skip it and keep walking rootward.
          { current = parent; }} }}
  map }

/// Everything clone-repo resolution can use, in priority order
/// (user_set > explicit_child > inferred_ancestor > default). The
/// first two are user specifications: a skgrepo resolved from them is
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
/// carries a concrete owned skgrepo unless the user owns no skgrepo at
/// all -- only then does 'ForkRepoUnresolved' fire. (The chosen
/// skgrepo is validated owned + unrestricted later, in
/// 'validate_fork_specs'; resolution here only fills it. The default
/// is active-aware so that, under a skgrepo restriction with both
/// a restricted and an unrestricted owned skgrepo, the fork still reaches the
/// confirmation buffer rather than dead-ending on
/// 'ForkRepoRestricted'.)
pub fn fork_spec_from_buffer_node (
  buffer_node         : &Graphnode,
  disk_title          : &str, // N's original title (before the edit), for the confirmation buffer's child line.
  disk_contains       : &[RelPartner<ID>], // N's original contains records (before the edit); children the edit deleted become the clone's hides.
  skgrepos            : &CloneSkgrepoInputs,
  config              : &SkgConfig,
  skgrepo_restriction : &SkgrepoRestriction,
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
    skgrepo_confirmed, config, skgrepo_restriction ) ) }

/// What 'docs/forks.org', "Adding new content under a foreign node",
/// describes: when the fork-forcing edit is the addition of new children
/// whose metadata explicitly names an owned skgrepo, the user has
/// already said where this material belongs, so the clone of the
/// foreign parent goes there too and the confirmation flow does not
/// ask again. Per foreign node N, this records the one owned skgrepo
/// its new explicit-repo immediate occurrence children agree on;
/// nothing is recorded when they disagree (ambiguous -- the flow then
/// asks) or when there are none. 'new_nodes_with_explicit_repos' is
/// the parse's new-nodes set minus its inherited-repo set
/// ('WhatTheParseSupplied'): only a skgrepo the user actually typed counts
/// as a specification.
pub fn explicit_new_child_skgrepos_for_foreign_vognodes (
  viewforest                       : &ViewForest,
  new_nodes_with_explicit_skgrepos : &HashSet<ID>,
  config                           : &SkgConfig,
) -> HashMap<ID, SkgrepoName> {
  let mut map       : HashMap<ID, SkgrepoName> = HashMap::new ();
  let mut ambiguous : HashSet<ID> = HashSet::new ();
  for node in viewforest . nodes () {
    let ViewnodeKind::Vognode (Vognode::Occurrence (t)) = & node . value () . kind
      else { continue; };
    if config . skgrepo_is_owned (& t . home_skgrepo)
      { continue; } // only a foreign node forks
    for child in node . children () {
      let ViewnodeKind::Vognode (Vognode::Occurrence (ct))
        = & child . value () . kind
        else { continue; };
      if ! new_nodes_with_explicit_skgrepos . contains (& ct . gnid)
        { continue; }
      if ! config . skgrepo_is_owned (& ct . home_skgrepo)
        { continue; } // an explicitly-foreign new child is an error elsewhere, not a specification
      match map . get (& t . gnid) {
        Some (prior) if prior != & ct . home_skgrepo =>
          { ambiguous . insert ( t . gnid . clone () ); }
        _ =>
          { map . insert ( t . gnid . clone (),
                           ct . home_skgrepo . clone () ); }} }}
  for gnid in &ambiguous { map . remove (gnid); }
  map }

/// The sentinel skgrepo the confirmation buffer pre-fills for a
/// clone-to-be whose skgrepo the user has not specified (neither in
/// the saved metadata nor in a prior confirmation round). It must be
/// replaced by a real owned skgrepo before approving: the client
/// prompts for one per placeholder-carrying clone (or the user sets
/// it with C-c s s), and refuses to approve while any remains (the
/// server would reject it as an unknown skgrepo anyway). Must match
/// 'skg-fork-repo-placeholder' in
/// [[../../elisp/skg-request-save.el]].
pub const FORK_SKGREPO_PLACEHOLDER : &str = "PICK-A-REPO";

/// Build the fork-confirmation buffer. Its head is an org headline
/// whose body holds the explanation (foldable; a long '#' comment
/// block annoyed in practice). Then two
/// levels per fork, because one headline cannot honestly stand for
/// both the original N and the (possibly re-titled) clone C:
///
///   * <edited title>     -- the clone-to-be C: the user's edited title,
///                           no gnid (none yet). Its nodeRepo is the
///                           resolved skgrepo when repo_confirmed, else
///                           the PICK-A-REPO placeholder, preceded by a
///                           one-line suggestion comment the client
///                           offers as the prompt's default.
///   ** <original title>  -- the original N that C overrides: its real gnid,
///                           real skgrepo, write-protected,
///                           noClaim.
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
     original it forks (real id; the clone above it overrides\n\
     it).\n\
     APPROVE with C-c C-c: the source buffer is re-saved, skgsave-committing\n\
     each fork into the repo its clone-to-be shows. A clone still\n\
     showing PICK-A-REPO needs a real repo first: the client prompts\n\
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
    out . push_str ( &fork_clone_headline (
      &SkgrepoName::from (shown_skgrepo), &spec . clone . 0 . title ));
    out . push_str ( &forked_original_headline (
      &spec . original_gnid, &spec . original_skgrepo,
      &spec . original_title )); }
  out }

/// The headline of a clone-to-be, in the metadata grammar: a node
/// with no gnid yet, its home skgrepo, and the fact that it stands at
/// a skgrepo boundary (it is a root).
fn fork_clone_headline (
  skgrepo : &SkgrepoName,
  title   : &str,
) -> String {
  let parts : Vec<String> =
    once ( "node" . to_string () )
    . chain ( skgRepos_form ( Some (skgrepo), None ))
    . chain ( once ( "(view repoBoundary)" . to_string () ))
    . collect ();
  format! ( "* (skg {}) {}\n", parts . join (" "), title ) }

/// The headline of the original a clone-to-be forks, in the metadata
/// grammar: a write-protected node that claims no membership in its
/// viewparent, the clone.
fn forked_original_headline (
  gnid    : &ID,
  skgrepo : &SkgrepoName,
  title   : &str,
) -> String {
  let parts : Vec<String> =
    [ "node" . to_string (),
      format! ( "(gnid {})", gnid . 0 ) ]
    . into_iter ()
    . chain ( skgRepos_form ( Some (skgrepo), None ))
    . chain ( [ "noClaim" . to_string (),
                "writeProtected" . to_string () ] )
    . collect ();
  format! ( "** (skg {}) {}\n", parts . join (" "), title ) }

/// Reject any fork the fork operation's guard or the skgrepo restriction
/// forbids. Run after the clones are built (their skgrepos resolved) but
/// before the save skgsave-commits anything:
/// - *already forked*: if the user already owns a fork of N
///   ('existing_owned_fork_of'), reject with 'ForkAlreadyExists',
///   naming it. Editing a foreign note shown raw that silently made a
///   second clone would more often be an accident than not; a
///   deliberate second fork is made by having another owned note fork
///   N, which nothing refuses. A fork refused so is not also reported
///   for its skgrepo.
/// - *owned*: the clone's resolved skgrepo must be one the user owns.
///   Inference and the default only ever yield owned skgrepos, but a
///   user-set skgrepo (typed, or hand-edited) might not be -- reject with
///   'ForkRepoNotOwned'.
/// - *skgrepo restriction*: the clone's owned skgrepo must be unrestricted under the
///   skgrepo restriction. Under a restricted set the user is not meant to
///   touch restricted skgrepos, and an invisible clone is never created
///   silently; reject with 'ForkRepoRestricted'.
///
/// The already-forked check reads the explicit save-planning graph.
pub fn validate_fork_specs_in_graph (
  fork_specs             : &[ForkSpec],
  graph                  : &crate::dbs::in_rust_graph::InRustGraph,
  config                 : &SkgConfig,
  skgrepo_restriction    : &SkgrepoRestriction,
) -> Vec<BufferValidationError> {
  let mut errors : Vec<BufferValidationError> = Vec::new ();
  for spec in fork_specs {
    if let Some (existing) = existing_owned_fork_of (
        config, graph, & spec . original_gnid )
      { errors . push (
          BufferValidationError::ForkAlreadyExists (
            spec . original_gnid . clone (), existing ));
        continue; }
    let clone_skgrepo : &SkgrepoName = & spec . clone . 0 . home_skgrepo;
    if ! config . skgrepo_is_owned (clone_skgrepo) {
      errors . push (
        BufferValidationError::ForkSkgrepoNotOwned (
          spec . original_gnid . clone (),
          clone_skgrepo . clone () ));
      continue; }
    let unrestricted : bool =
      skgrepo_restriction . contains_skgrepo (clone_skgrepo);
    if ! unrestricted {
      errors . push (
        BufferValidationError::ForkSkgrepoRestricted (
          spec . original_gnid . clone (),
          clone_skgrepo . clone () )); }}
  errors }

/// A note the user owns that forks the note 'original' names, if any
/// (the least pid among several). 'original' may be a primary or an
/// extra ID.
pub fn existing_owned_fork_of (
  config   : &SkgConfig,
  graph    : &InRustGraph,
  original : &ID,
) -> Option<ID> {
  let pid : ID = graph . pid_else_raw_id (original);
  graph . inbound_pids_for_relation (&pid, MIRROR_RELATION) . into_iter ()
    . find ( |forker| graph . nodes . get (forker) . is_some_and (
      |node| config . skgrepo_is_owned (&node . home_skgrepo) )) }

pub fn validate_fork_specs (
  fork_specs             : &[ForkSpec],
  config                 : &SkgConfig,
  skgrepo_restriction    : &SkgrepoRestriction,
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
/// forking edit deleted (disk_contains minus the edited contains):
/// the user dismissed them, so they must not reappear as
/// unintegrated subscribed content. (Children the clone keeps need no
/// hide -- the display rule already excludes C's own contains from
/// its subscribee-as-such view.)
/// contains is stored raw (the child IDs as the buffer collected them);
/// override substitution applies at render time.
/// .
/// The clone copies only the contains records the skgrepo restriction
/// shows: what the user could not see, the user did not fork. The rest
/// is N's still, and since C subscribes to N it shows under C as
/// unintegrated content after a skgrepo widening. Each copied record,
/// and each hide, stands at the more private of the clone's home and
/// the relRepo of the record it comes from, when that relRepo is owned
/// (a foreign section's privacy says nothing of an owned one), so that
/// a private membership never lands in a more public section.
pub fn build_fork_clone (
  buffer_node         : &Graphnode,
  disk_title          : &str, // N's original (pre-edit) title, kept for the confirmation buffer's child line.
  disk_contains       : &[RelPartner<ID>], // N's original (pre-edit) contains records.
  clone_skgrepo       : SkgrepoName,
  skgrepo_confirmed   : bool, // whether clone_repo was user-specified (vs inferred or defaulted)
  config              : &SkgConfig,
  skgrepo_restriction : &SkgrepoRestriction,
) -> ForkSpec {
  let floored = |record : &RelPartner<ID>| -> RelPartner<ID> {
    RelPartner::at_relRepo (
      if config . skgrepo_is_owned (&record . relRepo) {
        config . more_private_of (
          record . relRepo . clone (), clone_skgrepo . clone () )
      } else { clone_skgrepo . clone () },
      record . member . clone () ) };
  let visible = |record : &&RelPartner<ID>| -> bool {
    SkgrepoRestriction::record_is_visible (
      skgrepo_restriction, &record . relRepo ) };
  let buffer_contains : Vec<RelPartner<ID>> =
    buffer_node . contains . iter () . filter (visible) . map (floored) . collect ();
  let buffer_contains_gnids : Vec<ID> =
    members_of (&buffer_contains);
  let clone : Graphnode = Graphnode {
    title         : buffer_node . title . clone (),
    overPrivateText_telescope : false,
    aliases       : MSV::Unspecified,
    pid           : ID ( uuid::Uuid::new_v4 () . to_string () ),
    extra_ids     : Vec::new (),
    body          : buffer_node . body . clone (),
    contains      : buffer_contains,
    subscribesTo  : MSV::Specified ( rel_partners_at_relRepo (
      &clone_skgrepo, vec! [ buffer_node . pid . clone () ] )),
    hidesFromSubs : MSV::Specified (
      // The children the forking edit deleted.
      disk_contains . iter ()
        . filter (visible)
        . filter ( |record| ! buffer_contains_gnids . contains (&record . member) )
        . map (floored) . collect () ),
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
    original_gnid  : buffer_node . pid . clone (),
    // The original title (N's disk title), distinct from the clone's
    // edited title above -- the two-level confirmation buffer shows both.
    original_title   : disk_title . to_string (),
    original_skgrepo : buffer_node . home_skgrepo . clone (),
    skgrepo_confirmed,
  }}

/// One warning per fork the saved view will go on hiding: a fork of a
/// node edited, below a root, in a tree whose perspective
/// ('perspective_owner_of_tree_containing') will show another mirror
/// of it than the clone ('mirror_shown_once_cloned'), as a view rooted
/// at another's note does unless switched to the user's perspective.
/// There the rerender draws that mirror, without the edit, so without
/// a word the edit would seem lost: the clone is in the user's
/// skgrepo, and no note contains it. (A fork of a root re-roots the
/// view at the clone, so it shows.)
pub fn forks_the_view_will_not_show (
  viewforest         : &ViewForest,
  graph              : &InRustGraph,
  config             : &SkgConfig,
  restriction        : &SkgrepoRestriction,
  overrides_followed : &OverridesFollowed,
  fork_specs         : &[ForkSpec],
) -> Vec<String> {
  let mut warnings : Vec<String> = Vec::new ();
  for spec in fork_specs {
    let edited : Option<NodeRef<Viewnode>> =
      viewforest . nodes () . find ( |node|
        node . value () . editable_gnid () . is_some_and ( |gnid|
          graph . pid_else_raw_id (gnid) == spec . original_gnid ));
    let Some (edited) : Option<NodeRef<Viewnode>> = edited else { continue; };
    let is_root : bool = edited . parent () . is_some_and ( |parent|
      matches! ( parent . value () . kind, ViewnodeKind::BufferRoot ));
    if is_root { continue; }
    let owner : SkgrepoOwner = perspective_owner_of_tree_containing (
      config, graph, overrides_followed, edited );
    let shown : ID = mirror_shown_once_cloned (config, graph, restriction, &owner, spec);
    if shown == spec . clone . 0 . pid { continue; }
    let perspective : String = match &owner {
      SkgrepoOwner::User => "your perspective" . to_string (),
      SkgrepoOwner::Foreign (name) =>
        format! ("the perspective of the owner of '{}'", name . 0), };
    let what_it_shows : String =
      if shown == spec . original_gnid { "the original" . to_string () }
      else { format! ( "'{}', another of its mirrors",
                       graph . nodes . get (&shown)
                       . map ( |node| node . title . clone () )
                       . unwrap_or_else ( || shown . 0 . clone () )) };
    warnings . push ( format! (
      "Your edit of '{}' forked it into your skgrepo '{}', but this view \
       is drawn there from {}, so it goes on showing {} there, without \
       your edit. To see your fork, switch the view's perspective (C-c v o).",
      spec . original_title, spec . clone . 0 . home_skgrepo, perspective,
      what_it_shows )); }
  warnings }

/// The mirror 'owner''s perspective will show in place of the original
/// 'spec' forks, once the clone is saved: the original's mirrors, and
/// the clone, which forks it.
fn mirror_shown_once_cloned (
  config      : &SkgConfig,
  graph       : &InRustGraph,
  restriction : &SkgrepoRestriction,
  owner       : &SkgrepoOwner,
  spec        : &ForkSpec,
) -> ID {
  let clone : &Graphnode = &spec . clone . 0;
  let mut mirrors : Mirrors = mirrors_of (graph, restriction, &spec . original_gnid);
  mirrors . members . push ( Mirror {
    pid : clone . pid . clone (), home : clone . home_skgrepo . clone (), distance : 1 } );
  mirrors . forks . insert (( clone . pid . clone (), spec . original_gnid . clone () ));
  mirror_shown_among (config, owner, &spec . original_gnid, &mirrors) }

#[cfg(test)]
#[allow(non_snake_case)]
#[path = "../../tests/unit/fork.rs"]
mod tests;
