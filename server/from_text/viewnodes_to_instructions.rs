pub mod to_naive_instructions;
pub mod reconcile_same_id_instructions;
pub mod classify;
pub mod subscribee_hiderel_intents;

use classify::{ SaveRole, viewforest_with_saveroles, ViewNode_in_Role };
use crate::dbs::filesystem::one_node::optnodecomplete_from_id;
use crate::types::errors::BufferValidationError;
use crate::types::misc::{ID, SkgConfig};
use crate::types::nodes::complete::NodeComplete;
use crate::types::save::{DefineNode, SaveNode, SourceMove};
use crate::types::viewnode::{ IndefOrDef, ViewNode, ViewNodeKind };
use crate::types::views_state::nodecomplete_from_in_rust_graph;
use subscribee_hiderel_intents::{ SubscribeeHiderelIntent, subscribee_hiderel_intents_from_candidates, };
use super::supplement_from_disk::{ canonicalize_ids_from_disk, detect_source_move, supplement_unspecified_fields_from_disk, };
use super::validate::buffernode_differs_from_disknode;
use to_naive_instructions::{ collect_intent_candidates, naive_node_edit_intents_from_candidates, IntentCandidate, reconcile_nodeEditIntents, NodeEditIntent, NodeSaveIntent, SameIdReconciledNodeEditIntents, };

use ego_tree::Tree;
use std::collections::{HashMap, HashSet};
use std::error::Error;
use typedb_driver::TypeDBDriver;

/// The user intends changes to the graph.
/// Skg derives all of those intentions from one of these.
/// .
/// Caveats:
/// - Disk supplementation reads disk/DB to fill unspecified fields
///   and detect source moves, so not every field is from SaveAuthority.
/// - Later persistence adds derived instructions that are
///   not extracted from the buffer:
///   - delete-propagation cleanup appends cleanup SaveNodes in server/save.rs
///   - merge neighbor SaveNodes are derived from
///     merge instructions plus graph/DB state
pub(crate) struct SaveAuthority {
  role_viewforest : Tree<ViewNode_in_Role>, // The whole buffer/view.
  candidates      : Vec<IntentCandidate>, // Some of the nodes from the buffer.
}

impl SaveAuthority {
  pub(crate) fn from_viewforest (
    viewforest : &Tree<ViewNode>,
  ) -> Result<SaveAuthority, String> {
    let role_viewforest : Tree<ViewNode_in_Role> =
      viewforest_with_saveroles (viewforest) ?;
    let candidates : Vec<IntentCandidate> =
      collect_intent_candidates (&role_viewforest) ?;
    Ok (SaveAuthority {
      role_viewforest,
      candidates,
    }) }

  pub(crate) fn role_viewforest (
    &self,
  ) -> &Tree<ViewNode_in_Role> {
    &self . role_viewforest }

  pub(crate) fn candidates (
    &self,
  ) -> &[IntentCandidate] {
    &self . candidates }
}

struct SaveExtraction {
  node_edit_intents  : Vec<NodeEditIntent>,
  hiderel_intents : Vec<SubscribeeHiderelIntent>,
}

pub struct NonmergeSavePlan {
  pub define_nodes : Vec<DefineNode>,
  pub source_moves : Vec<SourceMove>,
}

struct Definenodes_with_Sourcemoves {
  instructions : Vec<DefineNode>,
  source_moves : Vec<SourceMove>,
}

struct Definenode_with_Opt_Sourcemove {
  instruction : DefineNode,
  source_move : Option<SourceMove>,
}

impl Definenodes_with_Sourcemoves {
  fn with_capacity (
    capacity : usize,
  ) -> Definenodes_with_Sourcemoves {
    Definenodes_with_Sourcemoves {
      instructions : Vec::with_capacity (capacity),
      source_moves : Vec::new(),
    }}

  fn push (
    &mut self,
    node : Definenode_with_Opt_Sourcemove,
  ) {
    self . instructions . push (node . instruction);
    if let Some (sm) = node . source_move {
      let sm : SourceMove = sm;
      self . source_moves . push (sm); }}
}

/// Supplanted by 'extract_nonmergeSavePlan_from_authority'.
/// It now exists only to avoid test churn.
pub async fn extract_nonmergeSavePlan (
  viewforest : &Tree<ViewNode>, // "viewforest" = tree with BufferRoot
  config : &SkgConfig,
  driver : &TypeDBDriver,
) -> Result<NonmergeSavePlan, Box<dyn Error>> {
  let _span : tracing::span::EnteredSpan = tracing::info_span!(
    "extract_nonmergeSavePlan" ). entered();
  let extraction_forest : SaveAuthority =
    SaveAuthority::from_viewforest (viewforest)
    . map_err ( |e| -> Box<dyn Error> { e . into() } ) ?;
  extract_nonmergeSavePlan_from_authority (
    &extraction_forest, config, driver ) . await
}

/// Reconciles duplicates via 'reconcile_same_id_instructions'
/// and supplements None fields with data from disk.
/// Indefinitive nodes produce no instructions.
pub(crate) async fn extract_nonmergeSavePlan_from_authority (
  save_authority : &SaveAuthority,
  config         : &SkgConfig,
  driver         : &TypeDBDriver,
) -> Result<NonmergeSavePlan, Box<dyn Error>> {
  let _span : tracing::span::EnteredSpan = tracing::info_span!(
    "extract_nonmergeSavePlan_from_authority" ). entered();
  let role_viewforest : &Tree<ViewNode_in_Role> =
    save_authority . role_viewforest ();
  let candidates : &[IntentCandidate] =
    save_authority . candidates ();
  validate_no_title_or_body_edit_in_subscribeeAsSuch (
    role_viewforest, config, driver ) . await ?;
  let extracted : SaveExtraction =
    extract_save_intents (role_viewforest, candidates)?;
  let intents_without_dups : SameIdReconciledNodeEditIntents =
    reconcile_nodeEditIntents (extracted . node_edit_intents)
    . map_err ( |e| -> Box<dyn Error> { e . into() } ) ?;
  let intents_with_visibility : SameIdReconciledNodeEditIntents =
    apply_hiderels_from_intents (
      intents_without_dups,
      &extracted . hiderel_intents,
      config,
      driver ) . await ?;
  let with_disk : Definenodes_with_Sourcemoves =
    build_disk_supplemented_define_nodes (
      intents_with_visibility . into_ordered_intents(),
      config,
      driver ) . await ?;
  let changed_instructions : Vec<DefineNode> =
    filter_wouldbe_noop_definenodes (with_disk . instructions);
  Ok (NonmergeSavePlan {
    define_nodes : changed_instructions,
    source_moves : with_disk . source_moves,
  }) }

fn extract_save_intents (
  role_viewforest : &Tree<ViewNode_in_Role>,
  candidates      : &[IntentCandidate],
) -> Result<SaveExtraction, Box<dyn Error>> {
  let hiderel_intents : Vec<SubscribeeHiderelIntent> =
    subscribee_hiderel_intents_from_candidates (
      role_viewforest, candidates)
    . map_err ( |e| -> Box<dyn Error> { e . into() } ) ?;
  let node_edit_intents : Vec<NodeEditIntent> =
    naive_node_edit_intents_from_candidates (
      role_viewforest, candidates)
    . map_err ( |e| -> Box<dyn Error> { e . into() } ) ?;
  Ok (SaveExtraction {
    node_edit_intents,
    hiderel_intents,
  }) }

async fn validate_no_title_or_body_edit_in_subscribeeAsSuch (
  role_viewforest : &Tree<ViewNode_in_Role>,
  config          : &SkgConfig,
  driver          : &TypeDBDriver,
) -> Result<(), Box<dyn Error>> {
  // Requires a placed, role-classified viewforest: only then do we
  // know which definitive nodes are being edited as subscribees.
  for node_ref in role_viewforest . nodes() {
    if ! matches!(
      node_ref . value() . role,
      SaveRole::AsSubscribee { .. })
    { continue; }
    let ViewNodeKind::True (t) =
      &node_ref . value() . viewnode . kind
    else { continue; };
    let IndefOrDef::Definitive { body, .. } =
      &t . indef_or_def
    else { continue; };
    let Some (from_disk) =
      optnodecomplete_from_id (
        config, driver, &t . id) . await ?
    else { continue; };
    if t . title != from_disk . title || *body != from_disk . body {
      return Err (Box::new (BufferValidationError::Other (
        format!(
          "Cannot edit title/body for node {} in subscribee-as-such position. View the node as itself to edit title/body.",
          t . id )))); }}
  Ok (( )) }

async fn build_disk_supplemented_define_nodes (
  intents : Vec<NodeEditIntent>,
  config  : &SkgConfig,
  driver  : &TypeDBDriver,
) -> Result<Definenodes_with_Sourcemoves, Box<dyn Error>> {
  let mut result : Definenodes_with_Sourcemoves =
    Definenodes_with_Sourcemoves::with_capacity (intents . len());
  for intent in intents {
    let supplemented : Definenode_with_Opt_Sourcemove =
      supplement_nodeeditintent_from_disk (
        intent, config, driver ) . await ?;
    result . push (supplemented); }
  Ok (result) }

async fn supplement_nodeeditintent_from_disk (
  intent : NodeEditIntent,
  config : &SkgConfig,
  driver : &TypeDBDriver,
) -> Result<Definenode_with_Opt_Sourcemove, Box<dyn Error>> {
  match intent {
    NodeEditIntent::Delete (_) =>
      Ok (Definenode_with_Opt_Sourcemove {
        instruction : intent . into_define_node()
          . map_err ( |e| -> Box<dyn Error> { e . into() } ) ?,
        source_move : None,
      }),
    _ => supplement_saveintent_from_disk (
      intent . save_intent()
        . map_err ( |e| -> Box<dyn Error> { e . into() } ) ?,
      config, driver ) . await,
  }}

async fn supplement_saveintent_from_disk (
  from_buffer : NodeSaveIntent,
  config      : &SkgConfig,
  driver      : &TypeDBDriver,
) -> Result<Definenode_with_Opt_Sourcemove, Box<dyn Error>> {
  let pid : ID =
    from_buffer . pid . clone();
  let from_disk : Option<NodeComplete> =
    optnodecomplete_from_id (config, driver, &pid) . await ?;
  match from_disk {
    None =>
      Ok (Definenode_with_Opt_Sourcemove {
        instruction :
          NodeEditIntent::GraphSave (from_buffer) . into_define_node()
          . map_err ( |e| -> Box<dyn Error> { e . into() } ) ?,
        source_move : None,
      }),
    Some (disk_node) => {
      let disk_node : NodeComplete = disk_node;
      let mut from_buffer : NodeSaveIntent = from_buffer;
      from_buffer . fill_unspecified_contains (
        &disk_node . contains);
      let from_buffer : NodeComplete =
        from_buffer . into_nodecomplete();
      let canonicalized : NodeComplete =
        canonicalize_ids_from_disk (from_buffer, &disk_node) ?;
      let maybe_move : Option<SourceMove> =
        detect_source_move ( config,  &pid,
                             &canonicalized . source,
                             &disk_node . source) ?;
      let supplemented : NodeComplete =
        supplement_unspecified_fields_from_disk (
          canonicalized, &disk_node);
      Ok (Definenode_with_Opt_Sourcemove {
        instruction : DefineNode::Save (SaveNode (supplemented)),
        source_move : maybe_move,
      }) }}}

/// Interpret view-children under definitive AsSubscribee branches as
/// subscriber edits to "hides" relationships.
///
/// At this point graph edit intents have been same-ID reconciled, but
/// have not yet been converted to DefineNodes or supplemented from
/// disk. That lets inferred hides become subscriber node intents
/// before final save instruction construction.
///
/// Policy for subscriber R viewing subscribee E:
///
/// - If N is graph-content of subscribee E,
///   and E has a view-child with ID N and Birth=ContentOf,
///   then N is intended to be visible through this subscription,
///   so it is removed from the hides of subscriber R.
/// - If N is graph-content of E, E has no view-child with ID N and
///   Birth=ContentOf, and N is not graph-content of R after the save,
///   then N is hidden from R.
/// - If N is a Birth=ContentOf view-child of E but is not
///   graph-content of E, then it is not a hiderel edit.
///   This does not touch it. The completion/rerender pipeline
///   will change it to Independent.
async fn apply_hiderels_from_intents (
  mut node_edit_intents : SameIdReconciledNodeEditIntents,
  vis_intents     : &[SubscribeeHiderelIntent],
  config          : &SkgConfig,
  driver          : &TypeDBDriver,
) -> Result<SameIdReconciledNodeEditIntents, Box<dyn Error>> {
  validate_no_overlapping_subscribee_hiderel_conflicts (
    vis_intents, config, driver ) . await ?;
  for intent in vis_intents {
    let Some (subscribee_from_disk) =
      optnodecomplete_from_id (
        config, driver, &intent . subscribee ) . await ?
    else { continue; };
    let Some (subscriber_from_disk) =
      optnodecomplete_from_id (
        config, driver, &intent . subscriber ) . await ?
    else { continue; };
    if ! source_is_owned (config, &subscriber_from_disk . source) {
      continue; }

    let subscriber_contains : HashSet<ID> =
      node_edit_intents . subscriber_contains_after_save (
        &subscriber_from_disk );
    let visible_content : HashSet<ID> =
      intent . visible_content . iter() . cloned() . collect();
    let inferred_hides : Vec<ID> =
      subscribee_from_disk . contains . iter()
      . filter ( |id| ! visible_content . contains (*id) )
      . filter ( |id| ! subscriber_contains . contains (*id) )
      . cloned()
      . collect();
    let inferred_unhides : Vec<ID> =
      subscribee_from_disk . contains . iter()
      . filter ( |id| visible_content . contains (*id) )
      . filter ( |id| subscriber_from_disk . hides_from_its_subscriptions
                      . or_default() . contains (*id) )
      . cloned() . collect();
    if inferred_hides . is_empty() && inferred_unhides . is_empty()
      { continue; }
    node_edit_intents . apply_hiderel_delta_to_subscriber (
      subscriber_from_disk,
      &inferred_hides,
      &inferred_unhides ); }
  Ok (node_edit_intents) }

/// Checks one subscriber + content node
/// against all subscribee branches under the subscriber.
/// Rejects the save if one subscribee says "hide it"
/// and another says "show it".
async fn validate_no_overlapping_subscribee_hiderel_conflicts (
  intents : &[SubscribeeHiderelIntent],
  config  : &SkgConfig,
  driver  : &TypeDBDriver,
) -> Result<(), Box<dyn Error>> {
  // Requires subscribee hiderel intents plus disk contains lists:
  // the conflict is per subscriber/subscribee-content pair, not just
  // per visible child shown in the buffer.
  let mut seen : HashMap<(ID, ID), bool> = HashMap::new();
  for intent in intents {
    let intent : &SubscribeeHiderelIntent = intent;
    let subscribee_from_disk : NodeComplete =
      match optnodecomplete_from_id (
        config, driver, &intent . subscribee ) . await ?
      { Some (subscribee_from_disk) => subscribee_from_disk,
        None                        => continue, };
    let visible_content : HashSet<ID> =
      intent . visible_content . iter() . cloned() . collect();
    for content_id in subscribee_from_disk . contains {
      let content_id : ID = content_id;
      let key : (ID, ID) =
        (intent . subscriber . clone(), content_id . clone());
      let visible : bool = visible_content . contains (&content_id);
      match seen . get (&key) {
        Some (previous_visible) => {
          let previous_visible : &bool = previous_visible;
          if *previous_visible != visible {
            return Err (Box::new (BufferValidationError::Other (
              format!( "Conflicting subscribee visibility edits for subscriber {} and content {}",
                        intent . subscriber, content_id )) )); }},
        None => {}, }
      seen . insert (key, visible); }}
  Ok (( )) }

fn source_is_owned (
  config : &SkgConfig,
  source : &crate::types::misc::SourceName,
) -> bool {
  config . sources . get (source)
    . map ( |s| s . user_owns_it )
    . unwrap_or (false) }

/// Filters out Save instructions that would be no-ops,
/// because they match the pre-save in-Rust graph entry
/// (nothing changed). Delete instructions and new nodes (not yet
/// in the in-Rust graph) are kept. This runs after disk
/// supplementation so unspecified fields have already been restored
/// to their disk values before comparison.
fn filter_wouldbe_noop_definenodes (
  instructions : Vec<DefineNode>,
) -> Vec<DefineNode> {
  let initial_count : usize = instructions . len();
  let filtered : Vec<DefineNode> = instructions
    . into_iter()
    . filter(|instr| match instr {
      DefineNode::Save(SaveNode (node)) => {
        match nodecomplete_from_in_rust_graph (&node . pid) {
          Some (pre_save) =>
            buffernode_differs_from_disknode (node, &pre_save),
          None => true, }}
      DefineNode::Delete (_) => true, })
    . collect();
  let removed_count : usize = initial_count - filtered . len();
  tracing::debug!("filter_wouldbe_noop_definenodes: \
             kept {} of {} instructions ({} unchanged filtered out)",
            filtered . len(), initial_count, removed_count);
  filtered }
