/// The 'buffer' referred to here
/// is a Skg buffer from the Emacs client,
/// read by the Rust server when the user saves it.
/// The sole purpose of all the sub-libraries in 'from_text::'
/// is the function 'buffer_to_validated_saveplan'
/// defined here.

pub mod buffer_to_viewnodes;
pub mod fork;
pub mod write_protected_edits;
pub mod local_instruction_collection;
pub mod supplement_from_disk;
pub mod weave;
pub mod validate;

use crate::nodeMerge::nodeMergeInstructionTriple::nodeMerge_instructions_from_pairs;
use crate::skgrepo_sets::ActiveSkgRepoSet;
use crate::types::errors::{BufferValidationError, SaveError};
use crate::types::misc::{ID, SkgConfig, members_of};
use crate::types::save::{NodeMerge, NodeInstruction, SavePlan};
use crate::types::maybe_placed_viewnode::maybePlaced_to_placed_viewforest;
use crate::types::tree::forest::{MpViewForest, ViewForest};

use buffer_to_viewnodes::uninterpreted::org_to_uninterpreted_viewforest;
use buffer_to_viewnodes::add_missing_info::{
  add_missing_info_to_viewforest_in_graph,
  na_affectsParent_under_visible_parent_becomes_isContainer,
  EnrichmentProvenance};
use fork::{
  CloneSkgRepoInputs,
  explicit_new_child_skgrepos_for_foreign_vognodes,
  fork_spec_from_buffer_node,
  new_foreign_nodes_adopting_clone_skgrepos,
  owned_ancestor_skgrepos_for_foreign_vognodes,
  validate_fork_specs_in_graph};
use local_instruction_collection::NonmergeSavePlan;
use validate::{validate_and_filter_foreign_instructions, validate_no_simultaneous_move_and_nodeMerge};

use crate::dbs::node_lookup::graphnode_graphFirst_by_pid_and_skgrepo;
use crate::types::nodes::complete::Graphnode;
use crate::types::viewnode::{ViewnodeKind, Vognode, ViewRequest};
use std::collections::{HashMap, HashSet};
use crate::types::misc::SkgRepoName;
use crate::types::save::ForkSpec;

/// Save preparation deliberately validates at several
/// data-maturity stages:
/// - raw org parse: errors only visible before tree construction;
/// - metadata-filled maybePlaced tree: global/local buffer structure;
/// - placed, role-aware viewforest: saved-view role policy;
/// - disk-supplemented NodeInstructions: foreign write policy;
/// - non-nodeMerge plus nodeMerge plan: cross-plan repo-move/nodeMerge policy.
///
/// Returns the saved view, the plan derived from it, and nonfatal
/// parse warnings (e.g. discarded folder headline text, destined for
/// 'SaveResponse.warnings'). View and plan are
/// kept apart (TODO/DONE/local-view-update/plan_v2.org §11): the graph-mutation
/// step consumes only the SavePlan; the rerender step consumes the ViewForest
/// (plus the plan's PIDs, for collateral selection). One parse produces both.
pub fn buffer_to_validated_saveplan_in_graph (
  buffer_text        : &str,
  graph              : &crate::dbs::in_rust_graph::InRustGraph,
  config             : &SkgConfig,
  active_skgrepo_set : Option<&ActiveSkgRepoSet>,
) -> Result<(ViewForest, SavePlan, Vec<String>), SaveError> {
  // No user-set clone skgrepos: every fork's skgrepo resolves by
  // inference-else-default. The fork-confirmation re-save uses the
  // _with_fork_repos entry below.
  buffer_to_validated_saveplan_with_fork_skgrepos_in_graph (
    buffer_text, graph, config, active_skgrepo_set, &HashMap::new () )
    }

/// As 'buffer_to_validated_saveplan', but with the per-fork clone
/// skgrepos the user chose in the confirmation buffer ('fork_repos',
/// keyed by each forked node N's pid). These take priority over the
/// inferred/default skgrepo when each clone's skgrepo is resolved.
pub fn buffer_to_validated_saveplan_with_fork_skgrepos_in_graph (
  buffer_text        : &str,
  graph              : &crate::dbs::in_rust_graph::InRustGraph,
  config             : &SkgConfig,
  active_skgrepo_set : Option<&ActiveSkgRepoSet>,
  fork_skgrepos      : &HashMap<ID, SkgRepoName>,
) -> Result<(ViewForest, SavePlan, Vec<String>), SaveError> {
  buffer_to_validated_saveplan_with_fork_skgrepos_and_previous_view_in_graph (
    buffer_text, graph, config, active_skgrepo_set, fork_skgrepos, None ) }

/// As 'buffer_to_validated_saveplan_with_fork_repos_in_graph', while also
/// comparing an open view's last server-rendered forest. This detects edits to
/// data that a write-protected occurrence would otherwise silently ignore.
pub fn buffer_to_validated_saveplan_with_fork_skgrepos_and_previous_view_in_graph (
  buffer_text : &str,
  graph       : &crate::dbs::in_rust_graph::InRustGraph,
  config      : &SkgConfig,
  active_skgrepo_set : Option<&ActiveSkgRepoSet>,
  fork_skgrepos : &HashMap<ID, SkgRepoName>,
  previous_viewforest : Option<&ViewForest>,
) -> Result<(ViewForest, SavePlan, Vec<String>), SaveError> {
  let restricted_skgrepo_set : Option<&ActiveSkgRepoSet> =
    // The set 'all' restricts nothing; downstream stages treat None
    // as "no restriction", so normalize here, once.
    active_skgrepo_set . filter ( |a| ! a . is_all () );
  let ( mut maybePlaced_viewforest, parsing_errors, parsing_warnings )
    : ( MpViewForest, Vec<BufferValidationError>, Vec<String> )
    = { let _span : tracing::span::EnteredSpan = tracing::info_span!(
          "org_to_uninterpreted_viewforest" ). entered();
        // parse the raw buffer
        org_to_uninterpreted_viewforest (buffer_text) }
          . map_err (SaveError::ParseError) ?;
  let enrichment : EnrichmentProvenance =
    { let _span : tracing::span::EnteredSpan = tracing::info_span!(
        "add_missing_info_to_viewforest" ). entered();
      // Metadata filling must precede maybePlaced-tree validation,
      // because those validators compare nodes by pid,
      // and expect skgrepos to be inherited/resolved.
      add_missing_info_to_viewforest_in_graph (
        & mut maybePlaced_viewforest, graph )
      } . map_err (SaveError::DatabaseError) ?;
  na_affectsParent_under_visible_parent_becomes_isContainer (
    &mut maybePlaced_viewforest );
  { // If saving is impossible, don't.
    let mut validation_errors : Vec<BufferValidationError> =
      { let _span : tracing::span::EnteredSpan = tracing::info_span!(
          "find_buffer_errors_for_saving" ). entered();
        crate::from_text::buffer_to_viewnodes::validate_tree
          ::find_buffer_errors_for_saving_in_graph (
          & maybePlaced_viewforest, graph, config )
 } . map_err (SaveError::DatabaseError) ?;
    validation_errors . extend (parsing_errors);
    if ! validation_errors . is_empty () {
      // Warnings always accompany errors (decided 2026-06-12): the
      // parse-time warnings collected so far ride out with the abort.
      return Err ( SaveError::BufferValidationErrors {
        errors   : validation_errors,
        warnings : parsing_warnings, } ); }}
  let mut viewforest : ViewForest =
    { let _span : tracing::span::EnteredSpan = tracing::info_span!(
        "maybePlaced_to_placed_viewforest" ). entered();
      maybePlaced_to_placed_viewforest (maybePlaced_viewforest) }
        . map_err ( |e| SaveError::ParseError (e) ) ?;
  if let Some (previous) = previous_viewforest {
    let errors : Vec<BufferValidationError> =
      write_protected_edits
      ::errors_and_normalize_new_writeProtected_occurrences (
        &mut viewforest, previous );
    if ! errors . is_empty () {
      return Err ( SaveError::BufferValidationErrors {
        errors,
        warnings : parsing_warnings, } ); }}
  else {
    let errors = write_protected_edits
      ::flags_surface_errors_against_graph (&viewforest, graph);
    if ! errors . is_empty () {
      return Err ( SaveError::BufferValidationErrors {
        errors,
        warnings : parsing_warnings, } ); }}
  let ( nonmerge_plan, nodeMerge_acquisitions )
    : ( NonmergeSavePlan, Vec<(ID, ID)> )
    = crate::from_text::local_instruction_collection
      ::extract_nonmergeSavePlan_locally_in_graph (
        &viewforest, graph, config, restricted_skgrepo_set )
 . map_err (SaveError::DatabaseError) ?;
  { // A flag preference is never an implicit-fork gesture.
    // Validate while its side-channel identity is still available; the
    // ordinary foreign-write filter below sees only supplemented SaveNodes.
    let mut errors : Vec<BufferValidationError> = Vec::new ();
    for target in &nonmerge_plan . flag_targets {
      match graph . pid_and_skgrepo (target) {
        None => errors . push (
          BufferValidationError::FlagEditOnUnknownNode (
            target . clone () )),
        Some ((_pid, skgrepo)) if ! config . skgrepo_is_owned (&skgrepo) =>
          errors . push (
            BufferValidationError::FlagEditOnForeignNode (
              target . clone (), skgrepo )),
        Some (_) => {}, }}
    if ! errors . is_empty () {
      return Err (SaveError::BufferValidationErrors {
        errors, warnings : parsing_warnings . clone (), }); }}
  let nodeMerge_instructions : Vec<NodeMerge> =
    // NodeMerge extraction only plans mutations; it does not mutate the saved
    // viewforest. After a successful commit, the common edit-request
    // consumption boundary in update_views_after_save clears this request
    // together with every other `(editRequest ...)` carrier.
    { let _span : tracing::span::EnteredSpan = tracing::info_span!(
        "nodeMerge_instructions_from_pairs" ). entered();
      nodeMerge_instructions_from_pairs (
        &nodeMerge_acquisitions, graph, config )
 } . map_err (SaveError::DatabaseError) ?;
  // C's repo is inferred from N's nearest OWNED vognode ancestor in
  // the view. The flat NodeInstructions have lost that ancestry, so resolve
  // it here, where the placed viewforest is live, keyed by foreign pid.
  let owned_ancestor_skgrepo : HashMap<ID, SkgRepoName> =
    owned_ancestor_skgrepos_for_foreign_vognodes (&viewforest, config);
  let adopt_clone_skgrepo : HashMap<ID, ID> = {
    // Bare new headlines under a foreign node are not foreign-creation
    // errors; they ride that node's fork, adopting its clone's skgrepo.
    // Ancestry is likewise only visible here, while the viewforest is
    // live; only enrichment knew which nodes were new and repoless.
    let new_with_inherited_skgrepo : HashSet<ID> =
      enrichment . new_nodes
      . intersection (& enrichment . inherited_skgrepo_nodes)
      . cloned () . collect ();
    new_foreign_nodes_adopting_clone_skgrepos (
      &viewforest, & new_with_inherited_skgrepo, config ) };
  let explicit_child_skgrepo : HashMap<ID, SkgRepoName> = {
    // A clone skgrepo the user already SPECIFIED, via explicit owned
    // skgrepos on the forked node's new children (fork-fixes Case 2):
    // the confirmation flow shows it as settled instead of asking.
    let new_with_explicit_skgrepo : HashSet<ID> =
      enrichment . new_nodes
      . difference (& enrichment . inherited_skgrepo_nodes)
      . cloned () . collect ();
    explicit_new_child_skgrepos_for_foreign_vognodes (
      &viewforest, & new_with_explicit_skgrepo, config ) };
  let default_clone_skgrepo : Option<SkgRepoName> = {
    // The active-aware default for a fork whose skgrepo can be neither
    // user-set nor inferred: prefer the CONFIG-FIRST owned skgrepo that
    // is ACTIVE under the restricted set, so the fork reaches the
    // confirmation buffer (where the user can rotate it) instead of
    // dead-ending on ForkRepoInactive when an inactive owned skgrepo
    // happens to sort first. Falls back to the config-first owned skgrepo
    // -- then ForkRepoInactive fires only when the user owns no ACTIVE
    // skgrepo at all (the genuine "activate one first" case), and
    // ForkRepoUnresolved only when the user owns no skgrepo at all.
    let owned_in_order : Vec<SkgRepoName> =
      config . owned_skgrepos_in_config_order ();
    let active_owned : Option<SkgRepoName> = restricted_skgrepo_set . and_then (
      |active| owned_in_order . iter ()
        . find ( |name| active . contains_skgrepo (name) )
        . cloned () );
    active_owned . or_else ( || owned_in_order . into_iter () . next () ) };
  let clone_skgrepo_inputs : CloneSkgRepoInputs = CloneSkgRepoInputs {
    user_set          : fork_skgrepos . clone (),
    explicit_child    : explicit_child_skgrepo,
    inferred_ancestor : owned_ancestor_skgrepo,
    default           : default_clone_skgrepo, };
  let ( node_instructions, fork_specs )
    : ( Vec<NodeInstruction>, Vec<ForkSpec> ) =
    { let _span : tracing::span::EnteredSpan = tracing::info_span!(
        "validate_and_filter_foreign_instructions" ). entered();
      validate_and_filter_foreign_instructions (
        nonmerge_plan . node_instructions,
        &nodeMerge_instructions,
        graph,
        &clone_skgrepo_inputs,
        &adopt_clone_skgrepo,
        config )
 } . map_err ( |errors| SaveError::BufferValidationErrors {
        errors, warnings : parsing_warnings . clone () } ) ?;
  validate_no_simultaneous_move_and_nodeMerge (
    &nonmerge_plan . skgrepo_moves, &nodeMerge_instructions )
    . map_err ( |errors| SaveError::BufferValidationErrors {
      errors, warnings : parsing_warnings . clone () } ) ?;
  let fork_specs : Vec<ForkSpec> = {
    // Explicit 'skg-fork-node' gesture: a node carrying ViewRequest::Fork
    // is an OWNED node the user asked to fork. Unlike the implicit
    // foreign fork (where N's own save is dropped), N keeps its own save
    // (it is owned); we only ADD the clone C overriding N. C copies N's
    // saved disk snapshot -- the client refuses to fork a dirty buffer,
    // so disk == what the user sees. These specs join the implicit ones
    // for the shared confirmation / commit pipeline below.
    let mut specs : Vec<ForkSpec> = fork_specs;
    specs . extend (
      explicit_fork_specs_from_viewforest (
        &viewforest, graph, config, &clone_skgrepo_inputs )
      . map_err ( |errors| SaveError::BufferValidationErrors {
          errors, warnings : parsing_warnings . clone () } ) ? );
    specs };
  { // Reject forks monogamy or the skgrepo-set forbids (before any
    // commit). Monogamy reads the live graph; the skgrepo-set check uses
    // the active set.
    let fork_errors : Vec<BufferValidationError> =
      validate_fork_specs_in_graph (
        &fork_specs, graph, config, restricted_skgrepo_set);
    if ! fork_errors . is_empty () {
      return Err ( SaveError::BufferValidationErrors {
        errors   : fork_errors,
        warnings : parsing_warnings . clone () } ); }}
  let warnings : Vec<String> = {
    let mut warnings : Vec<String> = parsing_warnings;
    warnings . extend ( nonmerge_plan . warnings );
    warnings . extend (
      dead_link_warnings ( graph, &node_instructions, &fork_specs ) );
    warnings };
  Ok (( viewforest,
        SavePlan {
          node_instructions,
          nodeMerge_instructions,
          skgrepo_moves : nonmerge_plan . skgrepo_moves,
          fork_specs,
          post_commit_notice_candidates :
            nonmerge_plan . post_commit_notice_candidates },
        warnings )) }

/// Transitional compatibility for callers not yet carrying a generation.
pub fn buffer_to_validated_saveplan (
  buffer_text        : &str,
  config             : &SkgConfig,
  active_skgrepo_set : Option<&ActiveSkgRepoSet>,
) -> Result<(ViewForest, SavePlan, Vec<String>), SaveError> {
  let nodes = crate::dbs::filesystem::multiple_nodes
    ::read_all_skg_files_from_skgrepos (config)
    . map_err (|e| SaveError::DatabaseError (Box::new (e)))?;
  let graph = crate::dbs::in_rust_graph::InRustGraph::from_graphnodes (&nodes);
  buffer_to_validated_saveplan_in_graph (
    buffer_text, &graph, config, active_skgrepo_set ) }

/// Transitional compatibility for the fork-confirmation surface.
pub fn buffer_to_validated_saveplan_with_fork_skgrepos (
  buffer_text        : &str,
  config             : &SkgConfig,
  active_skgrepo_set : Option<&ActiveSkgRepoSet>,
  fork_skgrepos      : &HashMap<ID, SkgRepoName>,
) -> Result<(ViewForest, SavePlan, Vec<String>), SaveError> {
  let nodes = crate::dbs::filesystem::multiple_nodes
    ::read_all_skg_files_from_skgrepos (config)
    . map_err (|e| SaveError::DatabaseError (Box::new (e)))?;
  let graph = crate::dbs::in_rust_graph::InRustGraph::from_graphnodes (&nodes);
  buffer_to_validated_saveplan_with_fork_skgrepos_in_graph (
    buffer_text, &graph, config, active_skgrepo_set, fork_skgrepos ) }

/// One nonfatal warning per DANGLING link this save writes: a
/// '[[id:X][label]]' in a saved title or body where X is neither in
/// the graph nor created by this same save (TODO/more.org, "Warn the
/// user when they make dangling links"). Only nodes the save actually
/// writes are scanned -- the noop filter has already dropped
/// unchanged ones -- so an old dangling link warns again only when its
/// carrier is edited. The explicit save-planning graph makes this check use
/// the same snapshot as every other validation stage.
fn dead_link_warnings (
  graph             : &crate::dbs::in_rust_graph::InRustGraph,
  node_instructions : &[NodeInstruction],
  fork_specs        : &[ForkSpec],
) -> Vec<String> {
  use crate::types::links::links_from_node;
  let saved_nodes : Vec<&Graphnode> =
    node_instructions . iter ()
    . filter_map ( |dn| match dn {
        crate::types::save::NodeInstruction::Save (
          crate::types::save::SaveNode (n) ) => Some (n),
        _ => None } )
    . chain ( fork_specs . iter () . map ( |spec| & spec . clone . 0 ) )
    . collect ();
  let created_this_save : HashSet<&ID> = {
    let mut skgids : HashSet<&ID> = HashSet::new ();
    for node in &saved_nodes {
      skgids . insert (& node . pid);
      skgids . extend ( node . extra_ids . iter () ); }
    skgids };
  let mut warnings : Vec<String> = Vec::new ();
  for node in &saved_nodes {
    for link in links_from_node (node) {
      if created_this_save . contains (& link . skgid) { continue; }
      if graph . pid_of (& link . skgid) . is_some () { continue; }
      warnings . push ( format! (
        "Dangling link: node {} links to absent id {} (label {:?}).",
        node . pid . 0, link . skgid . 0, link . label )); }}
  warnings }

/// Build a ForkSpec for each node carrying 'ViewRequest::Fork' (the
/// explicit 'skg-fork-node' gesture, for an OWNED node). The clone C is
/// built from N's current disk/graph snapshot (not the buffer): the
/// client refuses to fork a dirty buffer, so disk == what the user sees,
/// and N's own owned save proceeds separately. The clone skgrepo resolves
/// user-set-else-config-first-owned: the shared 'CloneRepoInputs'
/// carries the explicit-child and inferred-ancestor maps too, but both
/// are keyed by FOREIGN pids, so an owned fork target never hits them.
/// D2's 'validate_fork_view_requests' has already rejected an unsaved
/// or already-forked target, so this only builds.
fn explicit_fork_specs_from_viewforest (
  viewforest           : &ViewForest,
  graph                : &crate::dbs::in_rust_graph::InRustGraph,
  config               : &SkgConfig,
  clone_skgrepo_inputs : &CloneSkgRepoInputs,
) -> Result<Vec<ForkSpec>, Vec<BufferValidationError>> {
  let mut specs  : Vec<ForkSpec> = Vec::new ();
  let mut errors : Vec<BufferValidationError> = Vec::new ();
  let mut seen   : HashSet<ID> = HashSet::new ();
  for node in viewforest . nodes () {
    let ViewnodeKind::Vognode (Vognode::Active (t)) = & node . value () . kind
      else { continue; };
    if ! t . view_requests . contains (& ViewRequest::Fork) { continue; }
    let pid : &ID = & t . skgid;
    // 'viewforest.nodes()' walks the ego_tree arena, which retains
    // detached copies left by placement; dedup so one forked node yields
    // one spec. (D2 already rejected a genuine second fork request.)
    if ! seen . insert (pid . clone ()) { continue; }
    let snapshot : Graphnode =
      match graphnode_graphFirst_by_pid_and_skgrepo (
        graph, config, pid, & t . home_skgrepo ) {
        Ok (nc) => nc,
        Err (e) => {
          errors . push ( BufferValidationError::Other ( format! (
            "Cannot fork node {}: {}", pid . 0, e )));
          continue; }};
    match fork_spec_from_buffer_node (
      // The snapshot serves as both the clone template and the disk
      // state, so the disk-contains diff is empty: an explicit fork
      // deletes nothing, hence hides nothing.
      & snapshot, & snapshot . title,
      & members_of ( & snapshot . contains ),
      clone_skgrepo_inputs )
    { Ok (spec) => specs . push (spec),
      Err (e)   => errors . push (e), }}
  if ! errors . is_empty () { return Err (errors); }
  Ok (specs) }
