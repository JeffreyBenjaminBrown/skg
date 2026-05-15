use crate::from_text::viewnodes_to_instructions::classify::{
  viewforest_with_save_roles, SaveRole, ViewNode_in_Role };
use crate::types::viewnode::EditRequest;
use crate::types::viewnode::{ViewNode, ViewNodeKind, Scaffold, TrueNode, IndefOrDef};
use crate::types::misc::{ID, MSV, SourceName};
use crate::types::nodes::complete::NodeComplete;
use crate::types::save::{DefineNode, SaveNode, DeleteNode};
use crate::types::tree::generic::{
  read_at_node_in_tree, unique_scaffold_child };
use crate::types::list::dedup_vector;
use ego_tree::{NodeId, NodeRef, Tree};
use std::collections::HashMap;

pub(crate) enum NodeEditIntent {
  Save   (NodeSaveIntent),
  Delete (NodeDeleteIntent),
}

pub(crate) struct NodeSaveIntent {
  pid                          : ID,
  source                       : SourceName,
  title                        : String,
  body                         : Option<String>,
  contains                     : Vec<ID>,
  pub(crate) children_affect_only_hiding : bool, // Usually false. True for subscribees (where they appear *as* subscribees, i.e. as a child of a SubscribeeCol). When this is true, the node's children are treated as a set (order is ignored). Anything the subscribee contains that is absent here will be hidden from the subscriber (unless it is content of the subscriber, because nothing should hide its own content). Anything present here that the subscriber does not contain is invalid -- even if the user owns the subscribee, they can't edit it where it appears *as* a subscribee -- so their relationship to this node will be changed to Independent.
  aliases                      : MSV<String>,
  subscribes_to                : MSV<ID>,
  hides_from_its_subscriptions : MSV<ID>,
  overrides_view_of            : MSV<ID>,
}

pub(crate) struct NodeDeleteIntent {
  pid    : ID,
  source : SourceName,
}

struct NodeEditMinimal {
  pid    : ID,
  source : SourceName,
  kind   : NodeEditMinimalAction,
}

enum NodeEditMinimalAction {
  Save {
    title : String,
    body  : Option<String>,
  },
  Delete,
}

impl NodeEditIntent {
  pub(crate) fn pid (
    &self,
  ) -> &ID {
    match self {
      NodeEditIntent::Save (intent) => &intent . pid,
      NodeEditIntent::Delete (intent) => &intent . pid,
    }}

  pub(crate) fn children_affect_only_hiding (
    &self,
  ) -> bool {
    match self {
      NodeEditIntent::Save (intent) =>
        intent . children_affect_only_hiding,
      NodeEditIntent::Delete (_) => false,
    }}

  pub(crate) fn into_define_node (
    self,
  ) -> Result<DefineNode, String> {
    match self {
      NodeEditIntent::Delete (intent) =>
        Ok (DefineNode::Delete (DeleteNode {
          id     : intent . pid,
          source : intent . source,
        })),
      NodeEditIntent::Save (intent) =>
        Ok (DefineNode::Save (SaveNode (NodeComplete {
          title                        : intent . title,
          aliases                      : intent . aliases,
          source                       : intent . source,
          pid                          : intent . pid,
          extra_ids                    : vec![],
          body                         : intent . body,
          contains                     : intent . contains,
          subscribes_to                : intent . subscribes_to,
          hides_from_its_subscriptions :
            intent . hides_from_its_subscriptions,
          overrides_view_of            : intent . overrides_view_of,
          misc                         : Vec::new(),
        }))) }}
}

/// Converts a viewforest of ViewNodes to preliminary DefineNodes.
///
/// "Naive" means the instructions are not yet reconciled with
/// same-ID duplicates and are not supplemented from disk. The
/// extraction itself may still use interpreted save roles and
/// internal edit intents.
pub fn naive_saveinstructions_from_tree (
  viewforest: Tree<ViewNode> // "viewforest" = tree with BufferRoot
) -> Result<Vec<DefineNode>, String> {
  let intents : Vec<NodeEditIntent> =
    naive_node_edit_intents_from_viewforest (&viewforest)?;
  intents
    . into_iter()
    . map (NodeEditIntent::into_define_node)
    . collect() }

pub(crate) fn naive_node_edit_intents_from_viewforest (
  viewforest : &Tree<ViewNode>,
) -> Result<Vec<NodeEditIntent>, String> {
  let role_viewforest : Tree<ViewNode_in_Role> =
    viewforest_with_save_roles (viewforest)?;
  let candidate_ids : Vec<NodeId> =
    collect_preliminary_intent_candidate_ids (
      &role_viewforest)?;
  let basics_by_node_id : HashMap<NodeId, NodeEditMinimal> =
    collect_node_edit_basics (&role_viewforest, &candidate_ids)?;
  let save_candidate_ids : Vec<NodeId> =
    candidate_ids . iter()
    . filter_map (|candidate_id| {
      basics_by_node_id . get (candidate_id)
        . and_then (|basics| match basics . kind {
          NodeEditMinimalAction::Save { .. } =>
            Some (*candidate_id),
          NodeEditMinimalAction::Delete =>
            None,
        })})
    . collect();
  let contains_by_node_id : HashMap<NodeId, Vec<ID>> =
    collect_contains_by_node_id (
      &role_viewforest, &save_candidate_ids)?;
  let aliases_by_node_id : HashMap<NodeId, MSV<String>> =
    collect_aliases_by_node_id (
      &role_viewforest, &save_candidate_ids)?;
  let subscribees_by_node_id : HashMap<NodeId, MSV<ID>> =
    collect_subscribees_by_node_id (
      &role_viewforest, &save_candidate_ids)?;
  assemble_node_edit_intents (
    &role_viewforest,
    candidate_ids,
    basics_by_node_id,
    contains_by_node_id,
    aliases_by_node_id,
    subscribees_by_node_id) }

/// Two NodeEditIntents can conflict if they have the same ID.
/// This groups them by ID, and then on each group calls
/// reconcile_one_id_node_edit_intents.
#[allow(non_snake_case)]
pub(crate) fn reconcile_nodeEditIntents (
  intents : Vec<NodeEditIntent>,
) -> Result<Vec<NodeEditIntent>, String> {
  let mut grouped : HashMap<ID, Vec<NodeEditIntent>> =
    HashMap::new();
  for intent in intents {
    let intent : NodeEditIntent = intent;
    grouped
      . entry (intent . pid() . clone())
      . or_insert_with (Vec::new)
      . push (intent); }
  let mut result : Vec<NodeEditIntent> =
    Vec::new();
  for (pid, group) in grouped {
    let _pid : ID = pid;
    let group : Vec<NodeEditIntent> = group;
    result . push (
      reconcile_nodeEditIntents_with_same_ID (group)?); }
  Ok (result) }

/// ASSUMES the inputs all share an ID.
#[allow(non_snake_case)]
fn reconcile_nodeEditIntents_with_same_ID (
  intents : Vec<NodeEditIntent>,
) -> Result<NodeEditIntent, String> {
  let mut ordinary_save : Option<NodeEditIntent> = None;
  let mut visibility_save : Option<NodeEditIntent> = None;
  let mut delete : Option<NodeEditIntent> = None;
  for intent in intents {
    let intent : NodeEditIntent = intent;
    match intent {
      NodeEditIntent::Save (_) => {
        if intent . children_affect_only_hiding() {
          if visibility_save . is_some() {
            return Err (
              "Multiple direct AsSubscribee save instructions for same ID"
                . to_string()); }
          visibility_save = Some (intent);
        } else {
          if ordinary_save . is_some() {
            return Err (
              "Multiple ordinary save instructions for same ID"
                . to_string()); }
          ordinary_save = Some (intent); }},
      NodeEditIntent::Delete (_) => {
        if delete . is_none() {
          delete = Some (intent); }}}}
  if delete . is_some() && (ordinary_save . is_some()
                           || visibility_save . is_some()) {
    return Err (
      "Cannot have both Delete and Save for same ID" . to_string()); }
  match delete {
    Some (delete) => {
      let delete : NodeEditIntent = delete;
      return Ok (delete); },
    None => {},
  }
  match ordinary_save {
    Some (ordinary_save) => {
      let ordinary_save : NodeEditIntent = ordinary_save;
      return Ok (ordinary_save); },
    None => {},
  }
  let visibility_save : NodeEditIntent =
    visibility_save . ok_or (
      "No delete and no save instruction found. This should not be possible."
        . to_string())?;
  Ok (visibility_save) }

fn collect_preliminary_intent_candidate_ids (
  tree  : &Tree<ViewNode_in_Role>,
) -> Result<Vec<NodeId>, String> {
  fn maybe_collect_candidate_and_recurse (
    tree    : &Tree<ViewNode_in_Role>,
    node_id : NodeId,
    result  : &mut Vec<NodeId>
  ) -> Result<(), String> {

    /// Defined separately because it's called in two places.
    fn recurse_on_children (
      tree: &Tree<ViewNode_in_Role>,
      node_id: NodeId,
      result: &mut Vec<NodeId>
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
      ViewNodeKind::True (_) => {
        if role == SaveRole::OrdinaryTrueNode
           || matches! (role, SaveRole::AsSubscribee { .. })
        { result . push (node_id);
          recurse_on_children( tree, node_id, result )?; }}}
    Ok(( )) }

  let mut result: Vec<NodeId> = Vec::new();
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
          _ => Err (
            "preliminary intent candidate was not a TrueNode"
              . to_string()),
        })??;
    if let Some (basics) =
      node_edit_basics_from_treenode (&true_node)
    { result . insert (*candidate_id, basics); }}
  Ok (result) }

fn node_edit_basics_from_treenode (
  t : &TrueNode
) -> Option<NodeEditMinimal> {
  match &t . indef_or_def {
    IndefOrDef::Indefinitive =>
      None,
    IndefOrDef::Definitive { body, edit_request } => {
      if *edit_request == Some (EditRequest::Delete) {
        return Some (NodeEditMinimal {
          pid    : t . id . clone(),
          source : t . source . clone(),
          kind   : NodeEditMinimalAction::Delete,
        }); }
      Some (NodeEditMinimal {
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
  tree                   : &Tree<ViewNode_in_Role>,
  candidate_ids          : Vec<NodeId>,
  mut basics_by_node_id  : HashMap<NodeId, NodeEditMinimal>,
  mut contains_by_node_id : HashMap<NodeId, Vec<ID>>,
  mut aliases_by_node_id : HashMap<NodeId, MSV<String>>,
  mut subscribees_by_node_id : HashMap<NodeId, MSV<ID>>,
) -> Result<Vec<NodeEditIntent>, String> {
  let mut result : Vec<NodeEditIntent> =
    Vec::new();
  for candidate_id in candidate_ids {
    let Some (basics) =
      basics_by_node_id . remove (&candidate_id)
    else { continue; };
    match basics . kind {
      NodeEditMinimalAction::Delete =>
        result . push (NodeEditIntent::Delete (NodeDeleteIntent {
          pid    : basics . pid,
          source : basics . source,
        })),
      NodeEditMinimalAction::Save { title, body } =>
        result . push (NodeEditIntent::Save (NodeSaveIntent {
          pid                          : basics . pid,
          source                       : basics . source,
          title,
          body,
          contains                     :
            contains_by_node_id . remove (&candidate_id) . ok_or (
              "assemble_node_edit_intents: missing contains")?,
          children_affect_only_hiding :
            matches!(
              tree . get (candidate_id) . unwrap() . value() . role,
              SaveRole::AsSubscribee { .. }),
          aliases                      :
            aliases_by_node_id . remove (&candidate_id) . ok_or (
              "assemble_node_edit_intents: missing aliases")?,
          subscribes_to                :
            subscribees_by_node_id . remove (&candidate_id) . ok_or (
              "assemble_node_edit_intents: missing subscribees")?,
        hides_from_its_subscriptions : MSV::Unspecified,
        overrides_view_of            : MSV::Unspecified,
        })),
    }}
  Ok (result) }

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
                   SaveRole::AsSubscribee { .. })
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
             SaveRole::OrdinaryTrueNode)
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
