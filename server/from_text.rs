//! The 'buffer' referred to here
//! is a Skg buffer from the Emacs client,
//! read by the Rust server when the user saves it.
//! The sole purpose of all the sub-libraries in 'from_text::'
//! is the function 'buffer_to_validated_saveplan'
//! defined here.

pub mod buffer_to_viewnodes;
pub mod fork;
pub mod write_protected_edits;
pub mod local_fieldintent_collection;
pub mod strandeds;
pub mod supplement_from_disk;
pub mod weave;
pub mod validate;

use crate::skgrepo_sets::SkgrepoRestriction;
use crate::types::errors::{BufferValidationError, SaveError};
use crate::types::misc::{ID, SkgConfig};
use crate::types::save::{NodeInstruction, SavePlan, SkgrepoMove, UneditedStaleGraphnode};
use crate::serve::handlers::save_staleness::{
  WhatTheBaselineShows, without_unedited_stale_saves};
use crate::types::tree::forest::ViewForest;

use buffer_to_viewnodes::uninterpreted::org_to_viewforest;
use buffer_to_viewnodes::add_missing_info::IdResolution;
use buffer_to_viewnodes::add_missing_info::WhatTheParseSupplied;
use fork::{
  CloneSkgrepoInputs,
  explicit_new_child_skgrepos_for_foreign_vognodes,
  fork_spec_from_buffer_node,
  new_foreign_nodes_adopting_clone_skgrepos,
  owned_ancestor_skgrepos_for_foreign_vognodes,
  validate_fork_specs_in_graph};
use local_fieldintent_collection::NonmergeSavePlan;
use validate::{
  refuse_records_landing_restricted,
  validate_and_filter_foreign_instructions,
  validate_no_simultaneous_delete_and_nodeMerge,
  validate_no_simultaneous_move_and_nodeMerge};

use crate::dbs::node_lookup::graphnode_graphFirst_by_pid_and_skgrepo;
use crate::types::nodes::complete::Graphnode;
use crate::types::viewnode::{ViewnodeKind, Vognode, ViewRequest};
use std::collections::{HashMap, HashSet};
use crate::types::misc::SkgrepoName;
use crate::types::save::ForkSpec;

/// Save preparation deliberately validates at several
/// data-maturity stages:
/// - raw org parse: errors only visible before tree construction;
/// - the parsed viewforest, its missing gnids and skgrepos supplied:
///   global/local buffer structure, then saved-view role policy;
/// - disk-supplemented NodeInstructions: unedited stale graphnodes
///   left out ('without_unedited_stale_saves'), then foreign write
///   policy;
/// - non-nodeMerge plan plus the merge pairs: cross-plan
///   repo-move/nodeMerge and delete/nodeMerge policy.
///
/// The plan carries the merge pairs, not the NodeMerge triples: those
/// are built in 'prepare_save_under_mutation_gate', from the graph as
/// the save's ordinary nodeInstructions leave it.
///
/// Returns the saved view, the plan derived from it, and nonfatal
/// parse warnings (e.g. discarded folder headline text, destined for
/// 'SaveResponse.warnings'). View and plan are
/// kept apart: the graph-mutation
/// step consumes only the SavePlan; the rerender step consumes the ViewForest
/// (plus the plan's PIDs, for collateral selection). One parse produces both.
pub fn buffer_to_validated_saveplan_in_graph (
  buffer_text        : &str,
  graph              : &crate::dbs::in_rust_graph::InRustGraph,
  config             : &SkgConfig,
  skgrepo_restriction : &SkgrepoRestriction,
) -> Result<(ViewForest, SavePlan, Vec<String>), SaveError> {
  // No user-set clone skgrepos: every fork's skgrepo resolves by
  // inference-else-default. The fork-confirmation re-save uses the
  // _with_fork_repos entry below. No baseline: nothing is stale.
  buffer_to_validated_saveplan_with_fork_skgrepos_in_graph (
    buffer_text, graph, config, skgrepo_restriction, &HashMap::new (),
    &WhatTheBaselineShows::default () )
    }

/// As 'buffer_to_validated_saveplan', but with the per-fork clone
/// skgrepos the user chose in the confirmation buffer ('fork_repos',
/// keyed by each forked node N's pid). These take priority over the
/// inferred/default skgrepo when each clone's skgrepo is resolved.
/// 'baseline' is what the saved buffer's baseline shows
/// ('what_the_baseline_shows' in
/// 'server/serve/handlers/save_staleness.rs'): of its writes, the
/// copies of the graphnodes the graph changed after the buffer was
/// rendered, the plan leaves out the ones the buffer did not edit; and
/// the write-protected check excuses an occurrence that shows a title
/// or home skgrepo as one of the baseline's occurrences does.
pub fn buffer_to_validated_saveplan_with_fork_skgrepos_in_graph (
  buffer_text        : &str,
  graph              : &crate::dbs::in_rust_graph::InRustGraph,
  config             : &SkgConfig,
  skgrepo_restriction : &SkgrepoRestriction,
  fork_skgrepos      : &HashMap<ID, SkgrepoName>,
  baseline           : &WhatTheBaselineShows,
) -> Result<(ViewForest, SavePlan, Vec<String>), SaveError> {
  let ( viewforest, parsing_errors, parsing_warnings, supplied )
    : ( ViewForest, Vec<BufferValidationError>, Vec<String>,
        WhatTheParseSupplied )
    = { let _span : tracing::span::EnteredSpan = tracing::info_span!(
          "org_to_viewforest" ). entered();
        // Parse the raw buffer, supplying missing gnids and skgrepos,
        // which the validators need: they compare nodes by pid,
        // and expect skgrepos to be inherited/resolved.
        org_to_viewforest (buffer_text, IdResolution::ThroughGraph (graph)) }
          . map_err (SaveError::ParseError) ?;
  { // If saving is impossible, don't.
    let mut validation_errors : Vec<BufferValidationError> =
      { let _span : tracing::span::EnteredSpan = tracing::info_span!(
          "find_buffer_errors_for_saving" ). entered();
        crate::from_text::buffer_to_viewnodes::validate_tree
          ::find_buffer_errors_for_saving_in_graph (
          & viewforest, graph, config )
 } . map_err (SaveError::DatabaseError) ?;
    validation_errors . extend (parsing_errors);
    if ! validation_errors . is_empty () {
      // Warnings always accompany errors (decided 2026-06-12): the
      // parse-time warnings collected so far ride out with the abort.
      return Err ( SaveError::BufferValidationErrors {
        errors   : validation_errors,
        warnings : parsing_warnings, } ); }}
  { let errors : Vec<BufferValidationError> =
      write_protected_edits::errors_for_writeProtected_occurrences (
        &viewforest, graph, skgrepo_restriction, &baseline . occurrences );
    if ! errors . is_empty () {
      return Err ( SaveError::BufferValidationErrors {
        errors,
        warnings : parsing_warnings, } ); }}
  let ( nonmerge_plan, nodeMerge_acquisitions )
    : ( NonmergeSavePlan, Vec<(ID, ID)> )
    = crate::from_text::local_fieldintent_collection
      ::extract_nonmergeSavePlan_locally_in_graph (
        &viewforest, graph, config, skgrepo_restriction )
 . map_err (SaveError::DatabaseError) ?;
  let ( nonmerge_plan, unedited_stale_graphnodes )
    : ( NonmergeSavePlan, Vec<UneditedStaleGraphnode> ) =
  { // Before any validator judges them: an unedited stale graphnode
    // is neither forked nor moved back, and never refuses the save.
    let (node_instructions, skgrepo_moves, unedited)
      : (Vec<NodeInstruction>, Vec<SkgrepoMove>, Vec<UneditedStaleGraphnode>) =
      without_unedited_stale_saves (
        graph, &baseline . writes,
        nonmerge_plan . node_instructions, nonmerge_plan . skgrepo_moves );
    ( NonmergeSavePlan { node_instructions, skgrepo_moves, .. nonmerge_plan },
      unedited ) };
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
  let nodeMerge_pairs : Vec<(ID, ID)> =
    // A merge request only plans mutations; it does not mutate the saved
    // viewforest. After a successful skgsave-commit, the common edit-request
    // consumption boundary in update_views_after_save clears this request
    // together with every other `(editRequest ...)` carrier.
    // The acquiree is resolved from the ID as written (possibly an
    // extra ID) to its pid, so every later comparison is by pid.
    nodeMerge_acquisitions . into_iter ()
    . map ( |(acquirer, acquiree)| (
      graph . pid_else_raw_id (&acquirer),
      graph . pid_else_raw_id (&acquiree) ))
    . collect ();
  // C's repo is inferred from N's nearest owned vognode ancestor in
  // the view. The flat NodeInstructions have lost that ancestry, so resolve
  // it here, where the placed viewforest is live, keyed by foreign pid.
  let owned_ancestor_skgrepo : HashMap<ID, SkgrepoName> =
    owned_ancestor_skgrepos_for_foreign_vognodes (
      &viewforest,
      & supplied . writeProtected_skgrepos_after_save,
      config );
  let adopt_clone_skgrepo : HashMap<ID, ID> = {
    // Bare new headlines under a foreign node are not foreign-creation
    // errors; they ride that node's fork, adopting its clone's skgrepo.
    // Ancestry is likewise only visible here, while the viewforest is
    // live; only the parse knew which nodes were new and repoless.
    let new_with_inherited_skgrepo : HashSet<ID> =
      supplied . new_nodes
      . intersection (& supplied . inherited_skgrepo_nodes)
      . cloned () . collect ();
    new_foreign_nodes_adopting_clone_skgrepos (
      &viewforest, & new_with_inherited_skgrepo, config ) };
  let explicit_child_skgrepo : HashMap<ID, SkgrepoName> = {
    // A clone skgrepo the user already specified, via explicit owned
    // skgrepos on the forked node's new children:
    // the confirmation flow shows it as settled instead of asking.
    let new_with_explicit_skgrepo : HashSet<ID> =
      supplied . new_nodes
      . difference (& supplied . inherited_skgrepo_nodes)
      . cloned () . collect ();
    explicit_new_child_skgrepos_for_foreign_vognodes (
      &viewforest, & new_with_explicit_skgrepo, config ) };
  let default_clone_skgrepo : Option<SkgrepoName> = {
    // The active-aware default for a fork whose skgrepo can be neither
    // user-set nor inferred: prefer the config-first owned skgrepo that
    // is unrestricted under the restricted set, so the fork reaches the
    // confirmation buffer (where the user can rotate it) instead of
    // dead-ending on ForkRepoRestricted when a restricted owned skgrepo
    // happens to sort first. Falls back to the config-first owned skgrepo
    // -- then ForkRepoRestricted fires only when the user owns no unrestricted
    // skgrepo at all (the genuine "activate one first" case), and
    // ForkRepoUnresolved only when the user owns no skgrepo at all.
    let owned_in_order : Vec<SkgrepoName> =
      config . owned_skgrepos_in_config_order ();
    let unrestricted_owned : Option<SkgrepoName> =
      owned_in_order . iter ()
      . find ( |name| skgrepo_restriction . contains_skgrepo (name) )
      . cloned ();
    unrestricted_owned . or_else ( || owned_in_order . into_iter () . next () ) };
  let clone_skgrepo_inputs : CloneSkgrepoInputs = CloneSkgrepoInputs {
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
        &nodeMerge_pairs,
        graph,
        &clone_skgrepo_inputs,
        &adopt_clone_skgrepo,
        config,
        skgrepo_restriction )
 } . map_err ( |errors| SaveError::BufferValidationErrors {
        errors, warnings : parsing_warnings . clone () } ) ?;
  { // A record that would land where the skgrepo restriction keeps
    // it out of sight is refused, before anything is written.
    let errors : Vec<BufferValidationError> =
      refuse_records_landing_restricted (
        &node_instructions, graph, skgrepo_restriction );
    if ! errors . is_empty () {
      return Err ( SaveError::BufferValidationErrors {
        errors, warnings : parsing_warnings . clone () } ); }}
  validate_no_simultaneous_move_and_nodeMerge (
    &nonmerge_plan . skgrepo_moves, &nodeMerge_pairs )
    . map_err ( |errors| SaveError::BufferValidationErrors {
      errors, warnings : parsing_warnings . clone () } ) ?;
  validate_no_simultaneous_delete_and_nodeMerge (
    &node_instructions, &nodeMerge_pairs, graph )
    . map_err ( |errors| SaveError::BufferValidationErrors {
      errors, warnings : parsing_warnings . clone () } ) ?;
  let fork_specs : Vec<ForkSpec> = {
    // Explicit 'skg-fork-node' gesture: a node carrying ViewRequest::Fork
    // is an owned node the user asked to fork. Unlike the implicit
    // foreign fork (where N's own save is dropped), N keeps its own save
    // (it is owned); we only add the clone C overriding N. C copies N's
    // saved state on disk -- the client refuses to fork a dirty buffer,
    // so disk == what the user sees. These specs join the implicit ones
    // for the shared confirmation / skgsave-commit pipeline below.
    let mut specs : Vec<ForkSpec> = fork_specs;
    specs . extend (
      explicit_fork_specs_from_viewforest (
        &viewforest, graph, config, &clone_skgrepo_inputs,
        skgrepo_restriction )
      . map_err ( |errors| SaveError::BufferValidationErrors {
          errors, warnings : parsing_warnings . clone () } ) ? );
    specs };
  { // Reject forks monogamy or the skgrepo restriction forbids (before
    // any skgsave-commit). Monogamy reads the live graph; the other check
    // reads the skgrepo restriction.
    let fork_errors : Vec<BufferValidationError> =
      validate_fork_specs_in_graph (
        &fork_specs, graph, config, skgrepo_restriction);
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
          nodeMerge_pairs,
          skgrepo_moves : nonmerge_plan . skgrepo_moves,
          fork_specs,
          post_skgsave_commit_notice_candidates :
            nonmerge_plan . post_skgsave_commit_notice_candidates,
          unedited_stale_graphnodes },
        warnings )) }

/// Transitional compatibility for callers not yet carrying a generation.
pub fn buffer_to_validated_saveplan (
  buffer_text        : &str,
  config             : &SkgConfig,
  skgrepo_restriction : &SkgrepoRestriction,
) -> Result<(ViewForest, SavePlan, Vec<String>), SaveError> {
  let nodes = crate::dbs::filesystem::multiple_nodes
    ::read_all_skg_files_from_skgrepos (config)
    . map_err (|e| SaveError::DatabaseError (Box::new (e)))?;
  let graph = crate::dbs::in_rust_graph::InRustGraph::from_graphnodes (&nodes);
  buffer_to_validated_saveplan_in_graph (
    buffer_text, &graph, config, skgrepo_restriction ) }

/// Transitional compatibility for the fork-confirmation surface.
pub fn buffer_to_validated_saveplan_with_fork_skgrepos (
  buffer_text        : &str,
  config             : &SkgConfig,
  skgrepo_restriction : &SkgrepoRestriction,
  fork_skgrepos      : &HashMap<ID, SkgrepoName>,
) -> Result<(ViewForest, SavePlan, Vec<String>), SaveError> {
  let nodes = crate::dbs::filesystem::multiple_nodes
    ::read_all_skg_files_from_skgrepos (config)
    . map_err (|e| SaveError::DatabaseError (Box::new (e)))?;
  let graph = crate::dbs::in_rust_graph::InRustGraph::from_graphnodes (&nodes);
  buffer_to_validated_saveplan_with_fork_skgrepos_in_graph (
    buffer_text, &graph, config, skgrepo_restriction, fork_skgrepos,
    &WhatTheBaselineShows::default () ) }

/// One nonfatal warning per dangling link this save writes: a
/// '[[id:X][label]]' in a saved title or body where X is neither in
/// the graph nor created by this same save. Only nodes the save actually
/// writes are scanned -- the noop filter has already dropped
/// unchanged ones -- so an old dangling link warns again only when its
/// carrier is edited. The explicit save-planning graph makes this check use
/// the same graph snapshot as every other validation stage.
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
    let mut gnids : HashSet<&ID> = HashSet::new ();
    for node in &saved_nodes {
      gnids . insert (& node . pid);
      gnids . extend ( node . extra_ids . iter () ); }
    gnids };
  let mut warnings : Vec<String> = Vec::new ();
  for node in &saved_nodes {
    for link in links_from_node (node) {
      if created_this_save . contains (& link . gnid) { continue; }
      if graph . pid_of (& link . gnid) . is_some () { continue; }
      warnings . push ( format! (
        "Dangling link: node {} links to absent id {} (label {:?}).",
        node . pid . 0, link . gnid . 0, link . label )); }}
  warnings }

/// Build a ForkSpec for each node carrying 'ViewRequest::Fork' (the
/// explicit 'skg-fork-node' gesture, for an owned node). The clone C is
/// built from N's current disk/graph snapshot (not the buffer): the
/// client refuses to fork a dirty buffer, so disk == what the user sees,
/// and N's own owned save proceeds separately. The clone skgrepo resolves
/// user-set-else-config-first-owned: the shared 'CloneRepoInputs'
/// carries the explicit-child and inferred-ancestor maps too, but both
/// are keyed by foreign pids, so an owned fork target never hits them.
/// D2's 'validate_fork_view_requests' has already rejected an unsaved
/// or already-forked target, and a second request for one, so this
/// only builds.
fn explicit_fork_specs_from_viewforest (
  viewforest           : &ViewForest,
  graph                : &crate::dbs::in_rust_graph::InRustGraph,
  config               : &SkgConfig,
  clone_skgrepo_inputs : &CloneSkgrepoInputs,
  skgrepo_restriction  : &SkgrepoRestriction,
) -> Result<Vec<ForkSpec>, Vec<BufferValidationError>> {
  let mut specs  : Vec<ForkSpec> = Vec::new ();
  let mut errors : Vec<BufferValidationError> = Vec::new ();
  for node in viewforest . nodes () {
    let ViewnodeKind::Vognode (Vognode::Occurrence (t)) = & node . value () . kind
      else { continue; };
    if ! t . view_requests . contains (& ViewRequest::Fork) { continue; }
    let pid : &ID = & t . gnid;
    let graph_snapshot : Graphnode =
      match graphnode_graphFirst_by_pid_and_skgrepo (
        graph, config, pid, & t . home_skgrepo ) {
        Ok (nc) => nc,
        Err (e) => {
          errors . push ( BufferValidationError::Other ( format! (
            "Cannot fork node {}: {}", pid . 0, e )));
          continue; }};
    match fork_spec_from_buffer_node (
      // The graph snapshot serves as both the clone template and the disk
      // state, so the disk-contains diff is empty: an explicit fork
      // deletes nothing, hence hides nothing.
      & graph_snapshot, & graph_snapshot . title,
      & graph_snapshot . contains,
      clone_skgrepo_inputs, config, skgrepo_restriction )
    { Ok (spec) => specs . push (spec),
      Err (e)   => errors . push (e), }}
  if ! errors . is_empty () { return Err (errors); }
  Ok (specs) }
