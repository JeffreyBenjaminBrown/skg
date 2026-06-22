/// The 'buffer' referred to here
/// is a Skg buffer from the Emacs client,
/// read by the Rust server when the user saves it.
/// The sole purpose of all the sub-libraries in 'from_text::'
/// is the function 'buffer_to_validated_saveplan'
/// defined here.

pub mod buffer_to_viewnodes;
pub mod fork;
pub mod local_instruction_collection;
pub mod supplement_from_disk;
pub mod weave;
pub mod validate;

use crate::nodeMerge::nodeMergeInstructionTriple::nodeMerge_instructions_from_pairs;
use crate::source_sets::ActiveSourceSet;
use crate::types::errors::{BufferValidationError, SaveError};
use crate::types::misc::{ID, SkgConfig};
use crate::types::save::{NodeMerge, DefineNode, SavePlan};
use crate::types::maybe_placed_viewnode::maybePlaced_to_placed_viewforest;
use crate::types::tree::forest::{MpViewForest, ViewForest};

use buffer_to_viewnodes::uninterpreted::org_to_uninterpreted_viewforest;
use buffer_to_viewnodes::add_missing_info::{
  add_missing_info_to_viewforest,
  absent_parentIs_under_visible_parent_becomes_isContainer};
use buffer_to_viewnodes::validate_tree::find_buffer_errors_for_saving;
use fork::{
  fork_spec_from_buffer_node,
  owned_ancestor_sources_for_foreign_vognodes,
  validate_fork_specs};
use local_instruction_collection::{
  extract_nonmergeSavePlan_locally, NonmergeSavePlan };
use validate::{validate_and_filter_foreign_instructions, validate_no_simultaneous_move_and_nodeMerge};

use crate::dbs::node_lookup::nodecomplete_rustFirst_by_pid_and_source;
use crate::types::nodes::complete::NodeComplete;
use crate::types::viewnode::{ViewNodeKind, Vognode, ViewRequest};
use std::collections::{HashMap, HashSet};
use crate::types::misc::SourceName;
use crate::types::save::ForkSpec;
use typedb_driver::TypeDBDriver;

/// Save preparation deliberately validates at several
/// data-maturity stages:
/// - raw org parse: errors only visible before tree construction;
/// - metadata-filled maybePlaced tree: global/local buffer structure;
/// - placed, role-aware viewforest: saved-view role policy;
/// - disk-supplemented DefineNodes: foreign write policy;
/// - non-nodeMerge plus nodeMerge plan: cross-plan source-move/nodeMerge policy.
///
/// Returns the saved view, the plan derived from it, and nonfatal
/// parse warnings (e.g. discarded col headline text, destined for
/// 'SaveResponse.warnings'). View and plan are
/// kept apart (TODO/DONE/local-view-update/plan_v2.org §11): the graph-mutation
/// step consumes only the SavePlan; the rerender step consumes the ViewForest
/// (plus the plan's PIDs, for collateral selection). One parse produces both.
pub async fn buffer_to_validated_saveplan (
  buffer_text : &str,
  config      : &SkgConfig,
  driver      : &TypeDBDriver,
  active_source_set : Option<&ActiveSourceSet>,
) -> Result<(ViewForest, SavePlan, Vec<String>), SaveError> {
  // No user-set clone sources: every fork's source resolves by
  // inference-else-default. The fork-confirmation re-save uses the
  // _with_fork_sources entry below.
  buffer_to_validated_saveplan_with_fork_sources (
    buffer_text, config, driver, active_source_set, &HashMap::new () )
    . await }

/// As 'buffer_to_validated_saveplan', but with the per-fork clone
/// sources the user chose in the confirmation buffer ('fork_sources',
/// keyed by each forked node N's pid). These take priority over the
/// inferred/default source when each clone's source is resolved.
pub async fn buffer_to_validated_saveplan_with_fork_sources (
  buffer_text : &str,
  config      : &SkgConfig,
  driver      : &TypeDBDriver,
  active_source_set : Option<&ActiveSourceSet>,
  fork_sources : &HashMap<ID, SourceName>,
) -> Result<(ViewForest, SavePlan, Vec<String>), SaveError> {
  let restricted_source_set : Option<&ActiveSourceSet> =
    // The set 'all' restricts nothing; downstream stages treat None
    // as "no restriction", so normalize here, once.
    active_source_set . filter ( |a| ! a . is_all () );
  let ( mut maybePlaced_viewforest, parsing_errors, parsing_warnings )
    : ( MpViewForest, Vec<BufferValidationError>, Vec<String> )
    = { let _span : tracing::span::EnteredSpan = tracing::info_span!(
          "org_to_uninterpreted_viewforest" ). entered();
        // parse the raw buffer
        org_to_uninterpreted_viewforest (buffer_text) }
          . map_err (SaveError::ParseError) ?;
  { let _span : tracing::span::EnteredSpan = tracing::info_span!(
      "add_missing_info_to_viewforest" ). entered();
    // Metadata filling must precede maybePlaced-tree validation,
    // because those validators compare nodes by pid,
    // and expect sources to be inherited/resolved.
    add_missing_info_to_viewforest (
      & mut maybePlaced_viewforest, & config . db_name, driver )
    . await } . map_err (SaveError::DatabaseError) ?;
  absent_parentIs_under_visible_parent_becomes_isContainer (
    &mut maybePlaced_viewforest );
  { // If saving is impossible, don't.
    let mut validation_errors : Vec<BufferValidationError> =
      { let _span : tracing::span::EnteredSpan = tracing::info_span!(
          "find_buffer_errors_for_saving" ). entered();
        find_buffer_errors_for_saving (
          & maybePlaced_viewforest, config, driver )
        . await } . map_err (SaveError::DatabaseError) ?;
    validation_errors . extend (parsing_errors);
    if ! validation_errors . is_empty () {
      // Warnings always accompany errors (decided 2026-06-12): the
      // parse-time warnings collected so far ride out with the abort.
      return Err ( SaveError::BufferValidationErrors {
        errors   : validation_errors,
        warnings : parsing_warnings, } ); }}
  let viewforest : ViewForest =
    { let _span : tracing::span::EnteredSpan = tracing::info_span!(
        "maybePlaced_to_placed_viewforest" ). entered();
      maybePlaced_to_placed_viewforest (maybePlaced_viewforest) }
        . map_err ( |e| SaveError::ParseError (e) ) ?;
  let ( nonmerge_plan, nodeMerge_acquisitions )
    : ( NonmergeSavePlan, Vec<(ID, ID)> )
    = extract_nonmergeSavePlan_locally (
        &viewforest, config, driver, restricted_source_set )
      . await . map_err (SaveError::DatabaseError) ?;
  let nodeMerge_instructions : Vec<NodeMerge> =
    // PITFALL: The edit_requests consumed here remain in viewforest until cleared by expand_true_content_at_activeNode, during complete_viewforest. NodeMerge extraction only plans nodeMerge mutations; it does not mutate the saved viewforest.
    { let _span : tracing::span::EnteredSpan = tracing::info_span!(
        "nodeMerge_instructions_from_pairs" ). entered();
      nodeMerge_instructions_from_pairs (
        &nodeMerge_acquisitions, config, driver )
      . await } . map_err (SaveError::DatabaseError) ?;
  // C's source is inferred from N's nearest OWNED vognode ancestor in
  // the view. The flat DefineNodes have lost that ancestry, so resolve
  // it here, where the placed viewforest is live, keyed by foreign pid.
  let owned_ancestor_source : HashMap<ID, SourceName> =
    owned_ancestor_sources_for_foreign_vognodes (&viewforest, config);
  let default_clone_source : Option<SourceName> = {
    // The active-aware default for a fork whose source can be neither
    // user-set nor inferred: prefer the CONFIG-FIRST owned source that
    // is ACTIVE under the restricted set, so the fork reaches the
    // confirmation buffer (where the user can rotate it) instead of
    // dead-ending on ForkSourceInactive when an inactive owned source
    // happens to sort first. Falls back to the config-first owned source
    // -- then ForkSourceInactive fires only when the user owns no ACTIVE
    // source at all (the genuine "activate one first" case), and
    // ForkSourceUnresolved only when the user owns no source at all.
    let owned_in_order : Vec<SourceName> =
      config . owned_sources_in_config_order ();
    let active_owned : Option<SourceName> = restricted_source_set . and_then (
      |active| owned_in_order . iter ()
        . find ( |name| active . contains_source (name) )
        . cloned () );
    active_owned . or_else ( || owned_in_order . into_iter () . next () ) };
  let ( define_nodes, fork_specs )
    : ( Vec<DefineNode>, Vec<ForkSpec> ) =
    { let _span : tracing::span::EnteredSpan = tracing::info_span!(
        "validate_and_filter_foreign_instructions" ). entered();
      validate_and_filter_foreign_instructions (
        nonmerge_plan . define_nodes,
        &nodeMerge_instructions,
        &owned_ancestor_source,
        fork_sources,
        default_clone_source . as_ref (),
        config,
        driver )
      . await } . map_err ( |errors| SaveError::BufferValidationErrors {
        errors, warnings : parsing_warnings . clone () } ) ?;
  validate_no_simultaneous_move_and_nodeMerge (
    &nonmerge_plan . source_moves, &nodeMerge_instructions )
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
        &viewforest, config, fork_sources,
        default_clone_source . as_ref () )
      . map_err ( |errors| SaveError::BufferValidationErrors {
          errors, warnings : parsing_warnings . clone () } ) ? );
    specs };
  { // Reject forks monogamy or the source-set forbids (before any
    // commit). Monogamy reads the live graph; the source-set check uses
    // the active set.
    let fork_errors : Vec<BufferValidationError> =
      validate_fork_specs (&fork_specs, config, restricted_source_set);
    if ! fork_errors . is_empty () {
      return Err ( SaveError::BufferValidationErrors {
        errors   : fork_errors,
        warnings : parsing_warnings . clone () } ); }}
  let warnings : Vec<String> = {
    let mut warnings : Vec<String> = parsing_warnings;
    warnings . extend ( nonmerge_plan . warnings );
    warnings };
  Ok (( viewforest,
        SavePlan {
          define_nodes,
          nodeMerge_instructions,
          source_moves : nonmerge_plan . source_moves,
          fork_specs },
        warnings )) }

/// Build a ForkSpec for each node carrying 'ViewRequest::Fork' (the
/// explicit 'skg-fork-node' gesture, for an OWNED node). The clone C is
/// built from N's current disk/graph snapshot (not the buffer): the
/// client refuses to fork a dirty buffer, so disk == what the user sees,
/// and N's own owned save proceeds separately. The clone source resolves
/// user-set-else-config-first-owned; unlike the implicit foreign fork
/// there is NO owned-ancestor inference (an empty inference map). D2's
/// 'validate_fork_view_requests' has already rejected an unsaved or
/// already-forked target, so this only builds.
fn explicit_fork_specs_from_viewforest (
  viewforest      : &ViewForest,
  config          : &SkgConfig,
  user_set_source : &HashMap<ID, SourceName>,
  default_source  : Option<&SourceName>,
) -> Result<Vec<ForkSpec>, Vec<BufferValidationError>> {
  let no_inference : HashMap<ID, SourceName> = HashMap::new ();
  let mut specs  : Vec<ForkSpec> = Vec::new ();
  let mut errors : Vec<BufferValidationError> = Vec::new ();
  let mut seen   : HashSet<ID> = HashSet::new ();
  for node in viewforest . nodes () {
    let ViewNodeKind::Vognode (Vognode::Active (t)) = & node . value () . kind
      else { continue; };
    if ! t . view_requests . contains (& ViewRequest::Fork) { continue; }
    let pid : &ID = & t . id;
    // 'viewforest.nodes()' walks the ego_tree arena, which retains
    // detached copies left by placement; dedup so one forked node yields
    // one spec. (D2 already rejected a genuine second fork request.)
    if ! seen . insert (pid . clone ()) { continue; }
    let snapshot : NodeComplete =
      match nodecomplete_rustFirst_by_pid_and_source (
        config, pid, & t . source ) {
        Ok (nc) => nc,
        Err (e) => {
          errors . push ( BufferValidationError::Other ( format! (
            "Cannot fork node {}: {}", pid . 0, e )));
          continue; }};
    match fork_spec_from_buffer_node (
      & snapshot, & snapshot . title, & no_inference,
      user_set_source, default_source )
    { Ok (spec) => specs . push (spec),
      Err (e)   => errors . push (e), }}
  if ! errors . is_empty () { return Err (errors); }
  Ok (specs) }
