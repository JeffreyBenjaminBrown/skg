use crate::from_text::viewnodes_to_instructions::classify::{
  classify_save_roles, SaveRole, SaveRoleMap };
use crate::types::viewnode::EditRequest;
use crate::types::viewnode::{ViewNode, ViewNodeKind, Scaffold, TrueNode, IndefOrDef};
use crate::types::misc::{ID, MSV, SourceName};
use crate::types::nodes::complete::NodeComplete;
use crate::types::save::{DefineNode, SaveNode, DeleteNode};
use crate::types::tree::generic::read_at_node_in_tree;
use crate::types::tree::viewnode_nodecomplete::{ collect_grandchild_aliases_for_viewnode, unique_scaffold_child };
use crate::types::list::dedup_vector;
use ego_tree::{NodeId, NodeRef, Tree};
use std::collections::HashMap;

enum NodeEditIntent {
  Save   (NodeSaveIntent),
  Delete (NodeDeleteIntent),
}

struct NodeSaveIntent {
  pid                          : ID,
  source                       : SourceName,
  title                        : String,
  body                         : Option<String>,
  contains                     : Vec<ID>,
  aliases                      : MSV<String>,
  subscribes_to                : MSV<ID>,
  hides_from_its_subscriptions : MSV<ID>,
  overrides_view_of            : MSV<ID>,
}

struct NodeDeleteIntent {
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
  fn into_define_node (
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
  let roles : SaveRoleMap =
    classify_save_roles (&viewforest)?;
  let intents : Vec<NodeEditIntent> =
    naive_node_edit_intents_from_tree (&viewforest, &roles)?;
  intents
    . into_iter()
    . map (NodeEditIntent::into_define_node)
    . collect() }

fn naive_node_edit_intents_from_tree (
  viewforest : &Tree<ViewNode>,
  roles      : &SaveRoleMap,
) -> Result<Vec<NodeEditIntent>, String> {
  let candidate_ids : Vec<NodeId> =
    collect_preliminary_intent_candidate_ids (
      viewforest, roles)?;
  let basics_by_node_id : HashMap<NodeId, NodeEditMinimal> =
    collect_node_edit_basics (viewforest, &candidate_ids)?;
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
      viewforest, &save_candidate_ids, roles)?;
  let aliases_by_node_id : HashMap<NodeId, MSV<String>> =
    collect_aliases_by_node_id (
      viewforest, &save_candidate_ids)?;
  let subscribees_by_node_id : HashMap<NodeId, MSV<ID>> =
    collect_subscribees_by_node_id (
      viewforest, &save_candidate_ids, roles)?;
  assemble_node_edit_intents (
    candidate_ids,
    basics_by_node_id,
    contains_by_node_id,
    aliases_by_node_id,
    subscribees_by_node_id) }

fn collect_preliminary_intent_candidate_ids (
  tree  : &Tree<ViewNode>,
  roles : &SaveRoleMap,
) -> Result<Vec<NodeId>, String> {
  fn maybe_collect_candidate_and_recurse (
    tree    : &Tree<ViewNode>,
    node_id : NodeId,
    roles   : &SaveRoleMap,
    result  : &mut Vec<NodeId>
  ) -> Result<(), String> {

    /// Defined separately because it's called in two places.
    fn recurse_on_children (
      tree: &Tree<ViewNode>,
      node_id: NodeId,
      roles: &SaveRoleMap,
      result: &mut Vec<NodeId>
    ) -> Result<(), String> {
      for child_treeid in
        { let child_treeids: Vec<NodeId> =
            tree . get (node_id) . unwrap() . children()
            . map(|c| c . id()) . collect();
          child_treeids }
        { maybe_collect_candidate_and_recurse(
            tree, child_treeid, roles, result)?; }
      Ok(( )) }

    let node_kind: ViewNodeKind = read_at_node_in_tree(
        tree, node_id, |node| node . kind . clone())?;
    let role : SaveRole =
      roles . get (&node_id) . ok_or (
        "maybe_defineonenode_and_maybe_recurse: node has no SaveRole")?
      . clone();
    match node_kind {
      ViewNodeKind::Scaff (Scaffold::BufferRoot) =>
        recurse_on_children( tree, node_id, roles, result )?,
      ViewNodeKind::Scaff (Scaffold::SubscribeeCol) =>
        recurse_on_children( tree, node_id, roles, result )?,
      ViewNodeKind::Scaff (_) => {
        // TODO ? Recurse into more Scaffolds.
      },
      ViewNodeKind::Deleted (_) =>
        recurse_on_children( tree, node_id, roles, result )?,
      ViewNodeKind::DeletedScaff (_) =>
        recurse_on_children( tree, node_id, roles, result )?,
      ViewNodeKind::Unknown (_) =>
        // Inert: an UnknownNode is a placeholder for a missing
        // referent and generates no save instruction. Recurse so
        // that any user-edited descendants are not silently lost.
        recurse_on_children( tree, node_id, roles, result )?,
      ViewNodeKind::True (_) => {
        if role == SaveRole::OrdinaryTrueNode
           || matches! (role, SaveRole::AsSubscribee { .. })
        { result . push (node_id);
          recurse_on_children( tree, node_id, roles, result )?; }}}
    Ok(( )) }

  let mut result: Vec<NodeId> = Vec::new();
  let root_id : NodeId = tree . root() . id();
  maybe_collect_candidate_and_recurse (
    tree, root_id, roles, &mut result ) ?;
  Ok (result) }

fn collect_node_edit_basics (
  tree          : &Tree<ViewNode>,
  candidate_ids : &[NodeId],
) -> Result<HashMap<NodeId, NodeEditMinimal>, String> {
  let mut result : HashMap<NodeId, NodeEditMinimal> =
    HashMap::new();
  for candidate_id in candidate_ids {
    let true_node : TrueNode =
      read_at_node_in_tree (
        tree,
        *candidate_id,
        |node| match &node . kind {
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
  tree               : &Tree<ViewNode>,
  save_candidate_ids : &[NodeId],
  roles              : &SaveRoleMap,
) -> Result<HashMap<NodeId, Vec<ID>>, String> {
  let mut result : HashMap<NodeId, Vec<ID>> =
    HashMap::new();
  for candidate_id in save_candidate_ids {
    let node_ref : NodeRef<ViewNode> =
      tree . get (*candidate_id) . ok_or(
        "collect_contains_by_node_id: node not found")?;
    result . insert (
      *candidate_id,
      collect_contents_to_save_from_children (&node_ref, roles)); }
  Ok (result) }

fn collect_aliases_by_node_id (
  tree               : &Tree<ViewNode>,
  save_candidate_ids : &[NodeId],
) -> Result<HashMap<NodeId, MSV<String>>, String> {
  let mut result : HashMap<NodeId, MSV<String>> =
    HashMap::new();
  for candidate_id in save_candidate_ids {
    result . insert (
      *candidate_id,
      collect_grandchild_aliases_for_viewnode (tree, *candidate_id)?); }
  Ok (result) }

fn collect_subscribees_by_node_id (
  tree               : &Tree<ViewNode>,
  save_candidate_ids : &[NodeId],
  roles              : &SaveRoleMap,
) -> Result<HashMap<NodeId, MSV<ID>>, String> {
  let mut result : HashMap<NodeId, MSV<ID>> =
    HashMap::new();
  for candidate_id in save_candidate_ids {
    result . insert (
      *candidate_id,
      collect_subscribees (tree, *candidate_id, roles)?); }
  Ok (result) }

fn assemble_node_edit_intents (
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
  tree: &Tree<ViewNode>,
  node_id: NodeId,
  roles: &SaveRoleMap,
) -> Result<MSV<ID>, String> {
  let subscribee_col_id : Option<NodeId> =
    unique_scaffold_child (
      tree, node_id, &Scaffold::SubscribeeCol )
    . map_err ( |e| e . to_string() ) ?;
  match subscribee_col_id {
    None => Ok (MSV::Unspecified),
    Some (col_id) => {
      let subscribees : Vec<ID> = {
        let col_ref : NodeRef<ViewNode> = tree . get (col_id) . expect(
          "collect_subscribees: SubscribeeCol not found");
        let mut subscribees : Vec<ID> = Vec::new();
        for subscribeecol_child in col_ref . children() {
          let child_node : &ViewNode = subscribeecol_child . value();
          match &child_node . kind {
            ViewNodeKind::True (t) => {
              if matches!(
                   roles . get (&subscribeecol_child . id()),
                   Some (SaveRole::AsSubscribee { .. }))
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
  node_ref: &NodeRef<'a, ViewNode>,
  roles: &SaveRoleMap,
) -> Vec<ID> {
  let mut contents: Vec<ID> =
    Vec::new();
  for child_ref in node_ref . children() {
    let child : &ViewNode = child_ref . value();
    match &child . kind {
      ViewNodeKind::True (t) => {
        // In diff view, skip phantom nodes.
        if matches!(
             roles . get (&child_ref . id()),
             Some (SaveRole::OrdinaryTrueNode))
           && ! t . parent_ignores_it()
           && ! t . is_phantom ()
           && ! matches!( t . edit_request (),
                          Some (&EditRequest::Delete))
        { contents . push( t . id . clone() ); }},
      _ => continue }}
  contents }
