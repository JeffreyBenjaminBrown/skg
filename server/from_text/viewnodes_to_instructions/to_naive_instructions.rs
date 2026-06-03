use crate::from_text::viewnodes_to_instructions::classify::{
  viewforest_with_saveroles, SaveRole, ViewNode_in_Role };
use crate::types::viewnode::EditRequest;
use crate::types::git::Sign;
use crate::types::viewnode::{ViewNode, ViewNodeKind, TrueNode, IndefOrDef, InactiveNode, ParentIs, RoleCol};
use crate::types::viewnode::{Vognode, QualCol, Qual};
use crate::types::misc::{ID, MSV, SourceName};
use crate::types::nodes::complete::{FileProperty, NodeComplete};
use crate::types::save::{DefineNode, SaveNode, DeleteNode};
use crate::types::tree::generic::{
  read_at_node_in_tree, unique_scaffold_child };
use crate::types::tree::forest::tree_forest_root_ids;
use crate::types::list::dedup_vector;
use ego_tree::{NodeId, NodeRef, Tree};
use std::collections::{HashMap, HashSet};

/// What the user appears to intend for this node.
/// Might eventually become a DefineNode.
/// Uses MSV values in the Save variant (whereas DefineNode uses
/// SaveNode, which uses NodeComplete, which specifies all values).
pub(crate) enum NodeIntent {
  Save   (NodeSaveIntent),
  Delete (DeleteNode), // DefineNode uses the same DeleteNode type
}

pub(crate) struct NodeSaveIntent {
  pub(crate) pid               : ID,
  source                       : SourceName,
  title                        : String,
  body                         : Option<String>,
  pub(crate) contains          : MSV<ID>,
  extra_ids                    : Vec<ID>,
  aliases                      : MSV<String>,
  subscribes_to                : MSV<ID>,
  hides_from_its_subscriptions : MSV<ID>,
  overrides_view_of            : MSV<ID>,
  misc                         : Vec<FileProperty>,
}

/// Node edit intents for the entire saved buffer.
pub(crate) struct SameIdReconciledNodeIntents {
  order  : Vec<ID>,
  by_pid : HashMap<ID, NodeIntent>, }

/// What one can discern from a node without examining its neighbors.
struct NodeIntentLocal {
  pid    : ID,
  source : SourceName,
  kind   : NodeIntentLocalAction, }

enum NodeIntentLocalAction {
  Save { title : String,
         body  : Option<String>, },
  Delete, }

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct DefinenodeCandidate {
  pub(crate) treeid : NodeId,
  pub(crate) kind   : DefinenodeCandidateKind, }

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum DefinenodeCandidateKind {
  Ordinary,
  Subscribee { subscriber : ID },
  Overridden { overrider : ID }, }

impl NodeIntent {
  pub(crate) fn pid (
    &self,
  ) -> &ID {
    match self {
      NodeIntent::Save (intent) => &intent . pid,
      NodeIntent::Delete (intent) => &intent . id, }}

  pub(crate) fn apply_hiderel_delta (
    &mut self,
    base_hides       : &MSV<ID>,
    inferred_hides   : &[ID],
    inferred_unhides : &[ID],
  ) {
    match self {
      NodeIntent::Save (intent)
        => intent . apply_hiderel_delta (
             base_hides, inferred_hides, inferred_unhides),
      NodeIntent::Delete (_) => {}, }}

  pub(crate) fn graph_save_from_nodecomplete (
    node : NodeComplete,
  ) -> NodeIntent {
    NodeIntent::Save (NodeSaveIntent {
      pid                          : node . pid,
      source                       : node . source,
      title                        : node . title,
      body                         : node . body,
      contains                     : MSV::Specified (node . contains),
      extra_ids                    : node . extra_ids,
      aliases                      : node . aliases,
      subscribes_to                : node . subscribes_to,
      hides_from_its_subscriptions :
        node . hides_from_its_subscriptions,
      overrides_view_of            : node . overrides_view_of,
      misc                         : node . misc,
    }) }

  pub(crate) fn save_intent (
    self,
  ) -> Result<NodeSaveIntent, String> {
    match self {
      NodeIntent::Save (intent) =>
        Ok (intent),
      NodeIntent::Delete (_) =>
        Err ("Delete intent does not contain a SaveNode" . to_string()),
    }}

  pub(crate) fn into_define_node (
    self,
  ) -> Result<DefineNode, String> {
    match self {
      NodeIntent::Delete (intent)
        => Ok (DefineNode::Delete (intent)),
      NodeIntent::Save (intent)
        => Ok (DefineNode::Save (SaveNode (
          intent . into_nodecomplete() ))) }}
}

impl NodeSaveIntent {
  pub(crate) fn fill_unspecified_contains (
    &mut self,
    contains : &[ID],
  ) {
    if self . contains . is_unspecified() {
      self . contains =
        MSV::Specified (contains . to_vec()); }}

  pub(crate) fn into_nodecomplete (
    self,
  ) -> NodeComplete {
    NodeComplete {
      title                        : self . title,
      aliases                      : self . aliases,
      source                       : self . source,
      pid                          : self . pid,
      extra_ids                    : self . extra_ids,
      body                         : self . body,
      contains                     :
        self . contains . or_default() . to_vec(),
      subscribes_to                : self . subscribes_to,
      hides_from_its_subscriptions :
        self . hides_from_its_subscriptions,
      overrides_view_of            : self . overrides_view_of,
      misc                         : self . misc,
    }}

  fn apply_hiderel_delta (
    &mut self,
    base_hides       : &MSV<ID>,
    inferred_hides   : &[ID],
    inferred_unhides : &[ID],
  ) {
    let mut hides : Vec<ID> =
      if self . hides_from_its_subscriptions . is_unspecified() {
        base_hides . or_default() . to_vec()
      } else {
        self . hides_from_its_subscriptions . or_default() . to_vec()
      };
    hides . retain ( |id| ! inferred_unhides . contains (id) );
    for id in inferred_hides {
      if ! hides . contains (id) {
        hides . push (id . clone()); }}
    self . hides_from_its_subscriptions =
      MSV::Specified (hides); }}

impl SameIdReconciledNodeIntents {
  fn from_groups (
    order      : Vec<ID>,
    mut groups : HashMap<ID, Vec<NodeIntent>>,
  ) -> Result<SameIdReconciledNodeIntents, String> {
    let mut by_pid : HashMap<ID, NodeIntent> =
      HashMap::new();
    for pid in &order {
      let group : Vec<NodeIntent> =
        groups . remove (pid)
        . ok_or ( "SameIdReconciledNodeIntents::from_groups: missing group" . to_string())?;
      by_pid . insert (
        pid . clone(),
        reconcile_nodeIntents_with_same_ID (group)?); }
    Ok (SameIdReconciledNodeIntents { order, by_pid }) }

  pub(crate) fn into_ordered_intents (
    self,
  ) -> Vec<NodeIntent> {
    let SameIdReconciledNodeIntents { order, mut by_pid, }
      = self;
    order . into_iter()
      . filter_map ( |pid| by_pid . remove (&pid) )
      . collect() }

  pub(crate) fn subscriber_contains_after_save (
    &self,
    subscriber_from_disk : &NodeComplete,
  ) -> HashSet<ID> {
    match self . by_pid . get (&subscriber_from_disk . pid) {
      Some (NodeIntent::Save (intent)) =>
        intent . contains . or_default() . iter() . cloned() . collect(),
      _ =>
        subscriber_from_disk . contains . iter() . cloned() . collect(),
    }}

  pub(crate) fn apply_hiderel_delta_to_subscriber (
    &mut self,
    subscriber        : NodeComplete,
    inferred_hides   : &[ID],
    inferred_unhides : &[ID],
  ) {
    if let Some (intent) =
      self . by_pid . get_mut (&subscriber . pid)
    { intent . apply_hiderel_delta (
        &subscriber . hides_from_its_subscriptions,
        inferred_hides,
        inferred_unhides);
      return; }
    let mut intent : NodeIntent =
      NodeIntent::graph_save_from_nodecomplete (
        subscriber . clone());
    intent . apply_hiderel_delta (
      &subscriber . hides_from_its_subscriptions,
      inferred_hides,
      inferred_unhides);
    let pid : ID =
      subscriber . pid;
    self . order . push (pid . clone());
    self . by_pid . insert (pid, intent); }}

/// Converts a viewforest of ViewNodes to preliminary DefineNodes.
///
/// "Naive" means the instructions are not yet reconciled with
/// same-ID duplicates and are not supplemented from disk. The
/// extraction itself may still use interpreted save roles and
/// internal edit intents.
pub fn naive_saveinstructions_from_tree (
  viewforest: Tree<ViewNode> // Legacy type. ViewForest is preferred, but changing this here would be hard.
) -> Result<Vec<DefineNode>, String> {
  let intents : Vec<NodeIntent> =
    naive_node_edit_intents_from_viewforest (&viewforest)?;
  intents
    . into_iter()
    . map (NodeIntent::into_define_node)
    . collect() }

pub(crate) fn naive_node_edit_intents_from_viewforest (
  viewforest : &Tree<ViewNode>,
) -> Result<Vec<NodeIntent>, String> {
  let role_viewforest : Tree<ViewNode_in_Role> =
    viewforest_with_saveroles (viewforest)?;
  naive_node_edit_intents_from_role_viewforest (
    &role_viewforest) }

pub(crate) fn naive_node_edit_intents_from_role_viewforest (
  role_viewforest : &Tree<ViewNode_in_Role>,
) -> Result<Vec<NodeIntent>, String> {
  let candidates : Vec<DefinenodeCandidate> =
    collect_savenode_candidates (
      role_viewforest)?;
  naive_node_edit_intents_from_candidates (
    role_viewforest, &candidates) }

/// "Naive" in the sense that the output still needs to be:
/// - reconciled across duplicate appearances of the same PID
///   ('reconcile_nodeIntents' does that)
/// - enriched by subscribee-as-such visibility/hiderel inference
///   ('apply_hiderels_from_intents' does that)
/// - supplemented from disk ('build_disk_supplemented_define_nodes')
/// - filtered for no-op saves
/// - ignorant of the special hiderel interpretation in the case of
///   DefinenodeCandidateKind::Subscribee
///   ('ubscribee_hiderel_intents_from_candidates' does that)
pub(crate) fn naive_node_edit_intents_from_candidates (
  role_viewforest : &Tree<ViewNode_in_Role>,
  candidates      : &[DefinenodeCandidate],
) -> Result<Vec<NodeIntent>, String> {
  let candidate_ids : Vec<NodeId> =
    candidates . iter()
    . filter_map (|candidate| match &candidate . kind {
      DefinenodeCandidateKind::Ordinary =>
        Some (candidate . treeid),
      DefinenodeCandidateKind::Subscribee { .. } =>
        None,
      DefinenodeCandidateKind::Overridden { .. } =>
        None,
    })
    . collect();
  let basics_by_node_id : HashMap<NodeId, NodeIntentLocal> =
    collect_node_edit_basics (role_viewforest, &candidate_ids)?;
  let save_candidate_ids : Vec<NodeId> =
    candidate_ids . iter()
    . filter_map (|candidate_id| {
      basics_by_node_id . get (candidate_id)
        . and_then (|basics| match basics . kind {
          NodeIntentLocalAction::Save { .. } =>
            Some (*candidate_id),
          NodeIntentLocalAction::Delete =>
            None, } ) } )
    . collect();
  let contains_by_node_id : HashMap<NodeId, Vec<ID>> =
    collect_contains_by_node_id (
      role_viewforest, &save_candidate_ids)?;
  let aliases_by_node_id : HashMap<NodeId, MSV<String>> =
    collect_aliases_by_node_id (
      role_viewforest, &save_candidate_ids)?;
  let subscribees_by_node_id : HashMap<NodeId, MSV<ID>> =
    collect_subscribees_by_node_id (
      role_viewforest, &save_candidate_ids)?;
  let overridden_by_node_id : HashMap<NodeId, MSV<ID>> =
    collect_overridden_by_node_id (
      role_viewforest, &save_candidate_ids)?;
  assemble_node_edit_intents (
    candidate_ids,
    basics_by_node_id,
    contains_by_node_id,
    aliases_by_node_id,
    subscribees_by_node_id,
    overridden_by_node_id) }

/// Ensures there is at most one save *or* one delete per ID.
/// Groups intens by ID, then on each group calls
/// reconcile_one_id_node_edit_intents.
#[allow(non_snake_case)]
pub(crate) fn reconcile_nodeIntents (
  intents : Vec<NodeIntent>,
) -> Result<SameIdReconciledNodeIntents, String> {
  let mut grouped : HashMap<ID, Vec<NodeIntent>> =
    HashMap::new();
  let mut order : Vec<ID> =
    Vec::new();
  for intent in intents {
    let intent : NodeIntent = intent;
    let pid : ID =
      intent . pid() . clone();
    if ! grouped . contains_key (&pid) {
      order . push (pid . clone()); }
    grouped . entry (pid)
      . or_insert_with (Vec::new)
      . push (intent); }
  SameIdReconciledNodeIntents::from_groups (order, grouped) }

/// ASSUMES the inputs all share an ID.
#[allow(non_snake_case)]
fn reconcile_nodeIntents_with_same_ID (
  intents : Vec<NodeIntent>,
) -> Result<NodeIntent, String> {
  let mut optSave : Option<NodeIntent> = None;
  let mut optDelete : Option<NodeIntent> = None;
  for intent in intents {
    let intent : NodeIntent = intent;
    match intent {
      NodeIntent::Save (_) => {
        if optSave . is_some() {
          return Err (
              "Multiple ordinary save instructions for same ID"
                . to_string()); }
        optSave = Some (intent); },
      NodeIntent::Delete (_) => {
        if optDelete . is_none() {
          optDelete = Some (intent); }} }}
  if optDelete . is_some() && optSave . is_some() {
    return Err ( "Cannot have both Delete and Save for same ID" . to_string()); }
  match optDelete {
    Some (delete) => { return Ok (delete); },
    None => {}, }
  match optSave {
    Some (save) => { return Ok (save); },
    None => {}, }
  Err ( "No delete and no save instruction found. This should not be possible." . to_string()) }

/// Returns buffer-order positions that can own save-intent extraction.
///
/// Later passes split save/delete details. Here we only decide
/// which tree positions are meaningful for intent extraction:
///
/// - Definitive ordinary TrueNodes own graph edits for themselves.
/// - Definitive subscribee-as-such TrueNodes own the
///   save-time subscribe-hiderel surface for their subscriber.
/// - Display scaffolds are not intent roots, and most of their
///   descendants are interpreted by scaffold-specific collectors or
///   ignored as presentation state.
///
/// Keeping this as a separate ordered pass lets the following
/// collectors share one candidate list without each rediscovering the
/// same traversal/pruning policy.
pub(crate) fn collect_savenode_candidates (
  tree  : &Tree<ViewNode_in_Role>,
) -> Result<Vec<DefinenodeCandidate>, String> {
  fn maybe_collect_candidate_and_recurse (
    tree    : &Tree<ViewNode_in_Role>,
    node_id : NodeId,
    result  : &mut Vec<DefinenodeCandidate>
  ) -> Result<(), String> {

    /// Defined separately because it's called in two places.
    fn recurse_on_children (
      tree: &Tree<ViewNode_in_Role>,
      node_id: NodeId,
      result: &mut Vec<DefinenodeCandidate>
    ) -> Result<(), String> {
      for child_treeid in
        { let child_treeids: Vec<NodeId> =
            tree . get (node_id) . unwrap() . children()
            . map(|c| c . id()) . collect();
          child_treeids }
        { maybe_collect_candidate_and_recurse(
            tree, child_treeid, result)?; }
      Ok(( )) }

    let node_kind: ViewNodeKind = read_at_node_in_tree(
        tree, node_id, |node| node . viewnode . kind . clone())?;
    let role : SaveRole =
      read_at_node_in_tree (
        tree, node_id, |node| node . role . clone())?;
    match node_kind {
      ViewNodeKind::PartnerCol (RoleCol::Subscribee) =>
        recurse_on_children( tree, node_id, result )?,
      ViewNodeKind::QualCol (_)
        | ViewNodeKind::Qual (_)
        | ViewNodeKind::PartnerCol (_)
        | ViewNodeKind::BufferRoot => {},
      ViewNodeKind::Vognode (Vognode::Deleted (_)) =>
        recurse_on_children( tree, node_id, result )?,
      ViewNodeKind::DeadScaffold =>
        recurse_on_children( tree, node_id, result )?,
      ViewNodeKind::Vognode (Vognode::Inactive (_)) => {},
      ViewNodeKind::Vognode (Vognode::Unknown (_)) =>
        // An UnknownNode is a placeholder for a missing referent. It cannot generate save instructions itself, but its descendents might, so we recurse.
        recurse_on_children( tree, node_id, result )?,
      ViewNodeKind::Vognode (Vognode::Normal (t)) => {
        let candidate_kind : Option<DefinenodeCandidateKind> =
          match role {
            SaveRole::Ordinary =>
              Some (DefinenodeCandidateKind::Ordinary),
            SaveRole::Subscribee { subscriber } =>
              Some (DefinenodeCandidateKind::Subscribee {
                subscriber }),
            SaveRole::Overridden { overrider } =>
              Some (DefinenodeCandidateKind::Overridden {
                overrider }),
            _ => None };
        if let Some (kind) = candidate_kind {
          if ! t . is_indefinitive() {
            result . push (DefinenodeCandidate {
              treeid : node_id,
              kind } ); }
          recurse_on_children( tree, node_id, result )?; }},
      ViewNodeKind::Vognode (Vognode::DiffPhantom (_)) => {} }
    Ok (( )) }

  let mut result: Vec<DefinenodeCandidate> = Vec::new();
  for root_child in tree_forest_root_ids (tree) {
    maybe_collect_candidate_and_recurse (
      tree, root_child, &mut result ) ?; }
  Ok (result) }

fn collect_node_edit_basics (
  tree          : &Tree<ViewNode_in_Role>,
  candidate_ids : &[NodeId],
) -> Result<HashMap<NodeId, NodeIntentLocal>, String> {
  let mut result : HashMap<NodeId, NodeIntentLocal> =
    HashMap::new();
  for candidate_id in candidate_ids {
    let true_node : TrueNode =
      read_at_node_in_tree (
        tree,
        *candidate_id,
        |node| match &node . viewnode . kind {
          ViewNodeKind::Vognode (Vognode::Normal (t) )
            => Ok (t . clone()),
          _ => Err ( "intent candidate was not a TrueNode"
                      . to_string() ), } )??;
    let basics : NodeIntentLocal =
      node_edit_basics_from_treenode (&true_node)?;
    result . insert (*candidate_id, basics); }
  Ok (result) }

fn node_edit_basics_from_treenode (
  t : &TrueNode
) -> Result<NodeIntentLocal, String> {
  match &t . indef_or_def {
    IndefOrDef::Indefinitive =>
      Err (
        "intent candidate was indefinitive; this should have been filtered earlier"
          . to_string()),
    IndefOrDef::Definitive { body, edit_request } => {
      if *edit_request == Some (EditRequest::Delete) {
        return Ok (NodeIntentLocal {
          pid    : t . id . clone(),
          source : t . source . clone(),
          kind   : NodeIntentLocalAction::Delete,
        }); }
      Ok (NodeIntentLocal {
        pid    : t . id . clone(),
        source : t . source . clone(),
        kind   : NodeIntentLocalAction::Save {
          title : t . title . clone(),
          body  : body . clone(),
        },
      }) }}}

fn collect_contains_by_node_id (
  tree               : &Tree<ViewNode_in_Role>,
  save_candidate_ids : &[NodeId],
) -> Result<HashMap<NodeId, Vec<ID>>, String> {
  let mut result : HashMap<NodeId, Vec<ID>> =
    HashMap::new();
  for candidate_id in save_candidate_ids {
    let node_ref : NodeRef<ViewNode_in_Role> =
      tree . get (*candidate_id) . ok_or(
        "collect_contains_by_node_id: node not found")?;
    result . insert (
      *candidate_id,
      collect_contents_to_save_from_children (&node_ref)); }
  Ok (result) }

fn collect_aliases_by_node_id (
  tree               : &Tree<ViewNode_in_Role>,
  save_candidate_ids : &[NodeId],
) -> Result<HashMap<NodeId, MSV<String>>, String> {
  let mut result : HashMap<NodeId, MSV<String>> =
    HashMap::new();
  for candidate_id in save_candidate_ids {
    result . insert (
      *candidate_id,
      collect_grandchild_aliases_for_viewnodeInRole (tree, *candidate_id)?); }
  Ok (result) }

fn collect_subscribees_by_node_id (
  tree               : &Tree<ViewNode_in_Role>,
  save_candidate_ids : &[NodeId],
) -> Result<HashMap<NodeId, MSV<ID>>, String> {
  let mut result : HashMap<NodeId, MSV<ID>> =
    HashMap::new();
  for candidate_id in save_candidate_ids {
    result . insert (
      *candidate_id,
      collect_subscribees (tree, *candidate_id)?); }
  Ok (result) }

/// The type signature says it all.
fn collect_overridden_by_node_id (
  tree               : &Tree<ViewNode_in_Role>,
  save_candidate_ids : &[NodeId],
) -> Result<HashMap<NodeId, // each key is a TrueNode
                    MSV<ID>>, // what the tree says that key overrides
            String> {
  let mut result : HashMap<NodeId, MSV<ID>> =
    HashMap::new();
  for candidate_id in save_candidate_ids {
    result . insert (
      *candidate_id,
      collect_members_from_child_relation_col (
        tree, *candidate_id,
        RoleCol::Overridden,
        "OverriddenCol")?); }
  Ok (result) }

fn assemble_node_edit_intents (
  candidate_ids              : Vec<NodeId>,
  mut basics_by_node_id      : HashMap<NodeId, NodeIntentLocal>,
  mut contains_by_node_id    : HashMap<NodeId, Vec<ID>>,
  mut aliases_by_node_id     : HashMap<NodeId, MSV<String>>,
  mut subscribees_by_node_id : HashMap<NodeId, MSV<ID>>,
  mut overridden_by_node_id  : HashMap<NodeId, MSV<ID>>,
) -> Result<Vec<NodeIntent>, String> {
  let mut result : Vec<NodeIntent> =
    Vec::new();
  for candidate_id in candidate_ids {
    let basics : NodeIntentLocal =
      basics_by_node_id . remove (&candidate_id)
      . ok_or (
        "assemble_node_edit_intents: missing node edit basics")?;
    match basics . kind {
      NodeIntentLocalAction::Delete =>
        result . push (NodeIntent::Delete (DeleteNode {
          id     : basics . pid,
          source : basics . source, } )),
      NodeIntentLocalAction::Save { title, body } =>
        { let intent : NodeSaveIntent =
            NodeSaveIntent {
              pid                          : basics . pid,
              source                       : basics . source,
              title,
              body,
              contains                     :
                contains_for_save_intent (
                  candidate_id, &mut contains_by_node_id)?,
              extra_ids                    : vec![],
              aliases                      :
                aliases_by_node_id . remove (&candidate_id) . ok_or (
                  "assemble_node_edit_intents: missing aliases")?,
              subscribes_to                :
                subscribees_by_node_id . remove (&candidate_id) . ok_or (
                  "assemble_node_edit_intents: missing subscribees")?,
              hides_from_its_subscriptions : MSV::Unspecified,
              overrides_view_of            :
                overridden_by_node_id . remove (&candidate_id) . ok_or (
                  "assemble_node_edit_intents: missing overridden")?,
              misc                         : Vec::new(), };
          result . push (NodeIntent::Save (intent)); }}}
  Ok (result) }

fn contains_for_save_intent (
  candidate_id            : NodeId,
  contains_by_node_id     : &mut HashMap<NodeId, Vec<ID>>,
) -> Result<MSV<ID>, String> {
  let contains : Vec<ID> =
    contains_by_node_id . remove (&candidate_id) . ok_or (
      "assemble_node_edit_intents: missing contains")?;
  Ok (MSV::Specified (contains)) }

/// Treats the input tree as the source of truth; does not read dbs.
/// Returns None if no SubscribeeCol found,
///   because in this case the user has expressed no opinion.
/// Returns Some(vec) if SubscribeeCol found.
///   Empty means the user wants no subscribees.
///   Deduplicates the output, preserving order of first occurrence.
fn collect_subscribees (
  tree: &Tree<ViewNode_in_Role>,
  node_id: NodeId,
) -> Result<MSV<ID>, String> {
  let subscribee_col_id : Option<NodeId> =
    unique_scaffold_child (
      tree, node_id, &ViewNodeKind::PartnerCol (RoleCol::Subscribee),
      scaffold_from_viewnodeInRole )?;
  match subscribee_col_id {
    None => Ok (MSV::Unspecified),
    Some (col_id) => {
      let subscribees : Vec<ID> = {
        let col_ref : NodeRef<ViewNode_in_Role> = tree . get (col_id) . expect(
          "collect_subscribees: SubscribeeCol not found");
        let mut subscribees : Vec<ID> = Vec::new();
        for subscribeecol_child in col_ref . children() {
          let subscribeecol_child : NodeRef<ViewNode_in_Role> =
            subscribeecol_child;
          let child_node : &ViewNode =
            &subscribeecol_child . value() . viewnode;
          match &child_node . kind {
            ViewNodeKind::Vognode (Vognode::Normal (t)) => {
              if matches!(
                   subscribeecol_child . value() . role,
                   SaveRole::Subscribee { .. })
                 && member_counts_for_relation_collection (t)
              { subscribees . push(t . id . clone()); }},
            ViewNodeKind::Vognode (Vognode::DiffPhantom (_))
              => continue,
            ViewNodeKind::PartnerCol (RoleCol::HiddenOutsideOfSubscribee)
              => continue, // valid child of SubscribeeCol, but not a subscribee
            ViewNodeKind::Vognode (Vognode::Deleted (_))
              | ViewNodeKind::DeadScaffold
              | ViewNodeKind::Vognode (Vognode::Inactive (_))
              | ViewNodeKind::Vognode (Vognode::Unknown (_))
              => continue, // inert in this context
            ViewNodeKind::QualCol (_)
              | ViewNodeKind::Qual (_)
              | ViewNodeKind::PartnerCol (_)
              | ViewNodeKind::BufferRoot => return Err(format!(
                "SubscribeeCol has unexpected non-vognode child: {:?}",
                child_node . kind)), }}
        subscribees };
      Ok( MSV::Specified(dedup_vector (subscribees)) ) }} }

/// Collect members from a writable relation collection scaffold S
/// where S is a child of the input node_id.
///
/// Absence of the collection means the buffer expresses no opinion
/// about the relation field.  A present-but-empty collection is an
/// explicit empty set, so it must stay `MSV::Specified(vec![])`.
fn collect_members_from_child_relation_col (
  tree     : &Tree<ViewNode_in_Role>,
  node_id  : NodeId, // this is the truenode; the collection is its child
  roleCol  : RoleCol,
  label    : &str,
) -> Result<MSV<ID>, String> {
  let col_id : Option<NodeId> =
    unique_scaffold_child (
      tree, node_id, &ViewNodeKind::PartnerCol (roleCol),
      scaffold_from_viewnodeInRole )?;
  match col_id {
    None => Ok (MSV::Unspecified),
    Some (col_id) => {
      let col_ref : NodeRef<ViewNode_in_Role> = tree . get (col_id)
        . ok_or_else (|| format!("{} not found", label))?;
      let mut members : Vec<ID> = Vec::new();
      for child in col_ref . children() {
        match &child . value() . viewnode . kind {
          ViewNodeKind::Vognode (Vognode::Normal (t)) => {
            if member_counts_for_relation_collection (t) {
              members . push (t . id . clone ()); }},
          ViewNodeKind::Vognode (Vognode::DiffPhantom (_))
            | ViewNodeKind::Vognode (Vognode::Deleted (_))
            | ViewNodeKind::DeadScaffold
            | ViewNodeKind::Vognode (Vognode::Inactive (_))
            | ViewNodeKind::Vognode (Vognode::Unknown (_))
            => continue,
          ViewNodeKind::QualCol (_)
            | ViewNodeKind::Qual (_)
            | ViewNodeKind::PartnerCol (_)
            | ViewNodeKind::BufferRoot => return Err (format!(
              "{} has unexpected non-vognode child: {:?}",
              label, child . value() . viewnode . kind)), }}
      Ok (MSV::Specified (dedup_vector (members))) }} }

fn member_counts_for_relation_collection (
  t : &TrueNode,
) -> bool {
  t . parentIs == ParentIs::Affected
    && !t . should_be_phantom ()
    && !matches!( t . edit_request (),
                  Some (&EditRequest::Delete)) }

/// The following kinds of TrueNode children
/// should be excluded from their parent's content:
/// - anything with parentIs != Affected (display-only relative to parent)
/// - any phantom content ('Removed' or 'RemovedHere')
/// - anything about to be deleted
fn collect_contents_to_save_from_children<'a> (
  node_ref: &NodeRef<'a, ViewNode_in_Role>,
) -> Vec<ID> {
  let mut contents: Vec<ID> =
    Vec::new();
  for child_ref in node_ref . children() {
    let child_ref : NodeRef<ViewNode_in_Role> = child_ref;
    let child : &ViewNode = &child_ref . value() . viewnode;
    match &child . kind {
      ViewNodeKind::Vognode (Vognode::Normal (t)) => {
           if matches!(
             child_ref . value() . role,
             SaveRole::Ordinary)
           && t . parentIs == ParentIs::Affected
           && ! t . should_be_phantom ()
           && ! matches!( t . edit_request (),
                          Some (&EditRequest::Delete))
        { contents . push( t . id . clone() ); }},
      ViewNodeKind::Vognode (Vognode::Inactive (i)) => {
        if ! inactiveNode_is_phantom (i) {
          contents . push ( i . id . clone () ); }},
      _ => continue }}
  contents }

fn inactiveNode_is_phantom (
  inactive : &InactiveNode,
) -> bool {
  inactive . membership . staged == Some (Sign::Minus)
  || inactive . membership . unstaged == Some (Sign::Minus)
}

#[allow(non_snake_case)]
fn scaffold_from_viewnodeInRole (
  node : &ViewNode_in_Role,
) -> Option<&ViewNodeKind> {
  Some (&node . viewnode . kind) }

#[allow(non_snake_case)]
fn collect_grandchild_aliases_for_viewnodeInRole (
  tree    : &Tree<ViewNode_in_Role>,
  node_id : NodeId,
) -> Result<MSV<String>, String> {
  let alias_col_id : Option<NodeId> =
    unique_scaffold_child (
      tree, node_id,
      &ViewNodeKind::QualCol (QualCol::Alias),
      scaffold_from_viewnodeInRole )?;
  match alias_col_id {
    None => Ok (MSV::Unspecified),
    Some (col_id) => {
      let col_ref : NodeRef<ViewNode_in_Role> =
        tree . get (col_id) . expect (
          "collect_grandchild_aliases_for_viewnodeInRole: AliasCol not found");
      let mut aliases : Vec<String> = Vec::new();
      for alias_child in col_ref . children() {
        let alias_child : NodeRef<ViewNode_in_Role> = alias_child;
        if ! matches!(
          &alias_child . value() . viewnode . kind,
          ViewNodeKind::Qual (Qual::Alias { .. }))
        { return Err (format!(
            "AliasCol has non-Alias child with kind: {:?}",
            alias_child . value() . viewnode . kind)); }
        aliases . push (
          alias_child . value() . viewnode . title() . to_string()); }
      Ok (MSV::Specified (dedup_vector (aliases))) }}}
