use crate::from_text::viewnodes_to_instructions::classify::{
  viewforest_with_saveroles, SaveRole, ViewNode_in_Role };
use crate::types::viewnode::EditRequest;
use crate::types::viewnode::{ViewNode, ViewNodeKind, Scaffold, TrueNode, IndefOrDef};
use crate::types::misc::{ID, MSV, SourceName};
use crate::types::nodes::complete::{FileProperty, NodeComplete};
use crate::types::save::{DefineNode, SaveNode, DeleteNode};
use crate::types::tree::generic::{
  read_at_node_in_tree, unique_scaffold_child };
use crate::types::list::dedup_vector;
use ego_tree::{NodeId, NodeRef, Tree};
use std::collections::{HashMap, HashSet};

/// What the user appears to intend for this node.
/// Might eventually become a DefineNode.
/// Uses MSV values in the Save variant (whereas DefineNode uses
/// SaveNode, which uses NodeComplete, which specifies all values).
pub(crate) enum NodeIntent {
  Save   (NodeSaveIntent),
  Delete (DeleteNode), /// DefineNode uses the same DeleteNode type
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

struct NodeEditMinimal {
  pid    : ID,
  source : SourceName,
  kind   : NodeEditMinimalAction, }

/// What one can discern from the node
/// without considering its neighbors.
enum NodeEditMinimalAction {
  Save { title : String,
         body  : Option<String>, },
  Delete, }

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum SavenodeCandidateKind {
  OrdinaryTrueNode,
  SubscribeeAsSuch { subscriber : ID }, }

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct SavenodeCandidate {
  pub(crate) treeid : NodeId,
  pub(crate) kind   : SavenodeCandidateKind, }

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
        reconcile_nodeEditIntents_with_same_ID (group)?); }
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
  viewforest: Tree<ViewNode> // "viewforest" = tree with BufferRoot
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
  let candidates : Vec<SavenodeCandidate> =
    collect_savenode_candidates (
      role_viewforest)?;
  naive_node_edit_intents_from_candidates (
    role_viewforest, &candidates) }

/// "Naive" in the sense that the output still needs to be:
/// - reconciled across duplicate appearances of the same PID
///   ('reconcile_nodeEditIntents' does that)
/// - enriched by subscribee-as-such visibility/hiderel inference
///   ('apply_hiderels_from_intents' does that)
/// - supplemented from disk ('build_disk_supplemented_define_nodes')
/// - filtered for no-op saves
/// - ignorant of the special hiderel interpretation in the case of
///   SavenodeCandidateKind::SubscribeeAsSuch
///   ('ubscribee_hiderel_intents_from_candidates' does that)
pub(crate) fn naive_node_edit_intents_from_candidates (
  role_viewforest : &Tree<ViewNode_in_Role>,
  candidates      : &[SavenodeCandidate],
) -> Result<Vec<NodeIntent>, String> {
  let candidate_ids : Vec<NodeId> =
    candidates . iter()
    . filter_map (|candidate| match &candidate . kind {
      SavenodeCandidateKind::OrdinaryTrueNode =>
        Some (candidate . treeid),
      SavenodeCandidateKind::SubscribeeAsSuch { .. } =>
        None,
    })
    . collect();
  let basics_by_node_id : HashMap<NodeId, NodeEditMinimal> =
    collect_node_edit_basics (role_viewforest, &candidate_ids)?;
  let save_candidate_ids : Vec<NodeId> =
    candidate_ids . iter()
    . filter_map (|candidate_id| {
      basics_by_node_id . get (candidate_id)
        . and_then (|basics| match basics . kind {
          NodeEditMinimalAction::Save { .. } =>
            Some (*candidate_id),
          NodeEditMinimalAction::Delete =>
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
  assemble_node_edit_intents (
    candidate_ids,
    basics_by_node_id,
    contains_by_node_id,
    aliases_by_node_id,
    subscribees_by_node_id) }

/// Ensures there is at most one save *or* one delete per ID.
/// Groups intens by ID, then on each group calls
/// reconcile_one_id_node_edit_intents.
#[allow(non_snake_case)]
pub(crate) fn reconcile_nodeEditIntents (
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
fn reconcile_nodeEditIntents_with_same_ID (
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
) -> Result<Vec<SavenodeCandidate>, String> {
  fn maybe_collect_candidate_and_recurse (
    tree    : &Tree<ViewNode_in_Role>,
    node_id : NodeId,
    result  : &mut Vec<SavenodeCandidate>
  ) -> Result<(), String> {

    /// Defined separately because it's called in two places.
    fn recurse_on_children (
      tree: &Tree<ViewNode_in_Role>,
      node_id: NodeId,
      result: &mut Vec<SavenodeCandidate>
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
      ViewNodeKind::Scaff (Scaffold::BufferRoot) =>
        recurse_on_children( tree, node_id, result )?,
      ViewNodeKind::Scaff (Scaffold::SubscribeeCol) =>
        recurse_on_children( tree, node_id, result )?,
      ViewNodeKind::Scaff (_) => {
        // Display-only scaffolds do not own graph edits. Do not recurse through them: children under scaffolds such as AliasCol, IDCol, HiddenInSubscribeeCol, and diff display scaffolds are interpreted by their specific collectors (ancestors in the view) or ignored as presentation-only state.
      },
      ViewNodeKind::Deleted (_) =>
        recurse_on_children( tree, node_id, result )?,
      ViewNodeKind::DeletedScaff (_) =>
        recurse_on_children( tree, node_id, result )?,
      ViewNodeKind::Unknown (_) =>
        // An UnknownNode is a placeholder for a missing referent. It cannot generate save instructions itself, but its descendents might, so we recurse.
        recurse_on_children( tree, node_id, result )?,
      ViewNodeKind::True (t) => {
        let candidate_kind : Option<SavenodeCandidateKind> =
          match role {
            SaveRole::Truenode =>
              Some (SavenodeCandidateKind::OrdinaryTrueNode),
            SaveRole::TruenodeAsSubscribee { subscriber } =>
              Some (SavenodeCandidateKind::SubscribeeAsSuch {
                subscriber,
              }),
            _ => None,
          };
        if let Some (kind) = candidate_kind {
          if ! t . is_indefinitive() {
            result . push (SavenodeCandidate {
              treeid : node_id,
              kind,
            }); }
          recurse_on_children( tree, node_id, result )?; }}}
    Ok(( )) }

  let mut result: Vec<SavenodeCandidate> = Vec::new();
  let root_id : NodeId = tree . root() . id();
  maybe_collect_candidate_and_recurse (
    tree, root_id, &mut result ) ?;
  Ok (result) }

fn collect_node_edit_basics (
  tree          : &Tree<ViewNode_in_Role>,
  candidate_ids : &[NodeId],
) -> Result<HashMap<NodeId, NodeEditMinimal>, String> {
  let mut result : HashMap<NodeId, NodeEditMinimal> =
    HashMap::new();
  for candidate_id in candidate_ids {
    let true_node : TrueNode =
      read_at_node_in_tree (
        tree,
        *candidate_id,
        |node| match &node . viewnode . kind {
          ViewNodeKind::True (t) => Ok (t . clone()),
          _ => Err ( "intent candidate was not a TrueNode"
                      . to_string() ), } )??;
    let basics : NodeEditMinimal =
      node_edit_basics_from_treenode (&true_node)?;
    result . insert (*candidate_id, basics); }
  Ok (result) }

fn node_edit_basics_from_treenode (
  t : &TrueNode
) -> Result<NodeEditMinimal, String> {
  match &t . indef_or_def {
    IndefOrDef::Indefinitive =>
      Err (
        "intent candidate was indefinitive; this should have been filtered earlier"
          . to_string()),
    IndefOrDef::Definitive { body, edit_request } => {
      if *edit_request == Some (EditRequest::Delete) {
        return Ok (NodeEditMinimal {
          pid    : t . id . clone(),
          source : t . source . clone(),
          kind   : NodeEditMinimalAction::Delete,
        }); }
      Ok (NodeEditMinimal {
        pid    : t . id . clone(),
        source : t . source . clone(),
        kind   : NodeEditMinimalAction::Save {
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

fn assemble_node_edit_intents (
  candidate_ids              : Vec<NodeId>,
  mut basics_by_node_id      : HashMap<NodeId, NodeEditMinimal>,
  mut contains_by_node_id    : HashMap<NodeId, Vec<ID>>,
  mut aliases_by_node_id     : HashMap<NodeId, MSV<String>>,
  mut subscribees_by_node_id : HashMap<NodeId, MSV<ID>>,
) -> Result<Vec<NodeIntent>, String> {
  let mut result : Vec<NodeIntent> =
    Vec::new();
  for candidate_id in candidate_ids {
    let basics : NodeEditMinimal =
      basics_by_node_id . remove (&candidate_id)
      . ok_or (
        "assemble_node_edit_intents: missing node edit basics")?;
    match basics . kind {
      NodeEditMinimalAction::Delete =>
        result . push (NodeIntent::Delete (DeleteNode {
          id     : basics . pid,
          source : basics . source, } )),
      NodeEditMinimalAction::Save { title, body } =>
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
              overrides_view_of            : MSV::Unspecified,
              misc                         : Vec::new(),
            };
          result . push (NodeIntent::Save (intent)); },
    }}
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
      tree, node_id, &Scaffold::SubscribeeCol,
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
            ViewNodeKind::True (t) => {
              if matches!(
                   subscribeecol_child . value() . role,
                   SaveRole::TruenodeAsSubscribee { .. })
                 && !t . parent_ignores_it()
              { subscribees . push(t . id . clone()); }},
            ViewNodeKind::Scaff (Scaffold::HiddenOutsideOfSubscribeeCol) =>
              continue, // valid child of SubscribeeCol, but not a subscribee
            ViewNodeKind::Deleted (_) |
            ViewNodeKind::DeletedScaff (_) |
            ViewNodeKind::Unknown (_) =>
              continue, // inert in this context
            ViewNodeKind::Scaff (s) => return Err(format!( "SubscribeeCol has unexpected Scaffold child: {:?}", s)), }}
        subscribees };
      Ok( MSV::Specified(dedup_vector (subscribees)) ) }} }

/// The following kinds of TrueNode children
/// should be excluded from their parent's content:
/// - anything with birth != ContentOf (i.e. parent_ignores_it)
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
      ViewNodeKind::True (t) => {
        // In diff view, skip phantom nodes.
        if matches!(
             child_ref . value() . role,
             SaveRole::Truenode)
           && ! t . parent_ignores_it()
           && ! t . is_phantom ()
           && ! matches!( t . edit_request (),
                          Some (&EditRequest::Delete))
        { contents . push( t . id . clone() ); }},
      _ => continue }}
  contents }

#[allow(non_snake_case)]
fn scaffold_from_viewnodeInRole (
  node : &ViewNode_in_Role,
) -> Option<&Scaffold> {
  match &node . viewnode . kind {
    ViewNodeKind::Scaff (s) => Some (s),
    _ => None,
  }}

#[allow(non_snake_case)]
fn collect_grandchild_aliases_for_viewnodeInRole (
  tree    : &Tree<ViewNode_in_Role>,
  node_id : NodeId,
) -> Result<MSV<String>, String> {
  let alias_col_id : Option<NodeId> =
    unique_scaffold_child (
      tree, node_id, &Scaffold::AliasCol,
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
          ViewNodeKind::Scaff (Scaffold::Alias { .. }))
        { return Err (format!(
            "AliasCol has non-Alias child with kind: {:?}",
            alias_child . value() . viewnode . kind)); }
        aliases . push (
          alias_child . value() . viewnode . title() . to_string()); }
      Ok (MSV::Specified (dedup_vector (aliases))) }}}
