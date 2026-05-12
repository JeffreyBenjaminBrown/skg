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

struct NodeEditIntent {
  pid                          : ID,
  source                       : SourceName,
  delete                       : bool,
  title                        : Option<String>,
  body                         : Option<String>,
  contains                     : Vec<ID>,
  aliases                      : MSV<String>,
  subscribes_to                : MSV<ID>,
  hides_from_its_subscriptions : MSV<ID>,
  overrides_view_of            : MSV<ID>,
}

impl NodeEditIntent {
  fn into_define_node (
    self,
  ) -> Result<DefineNode, String> {
    if self . delete {
      return Ok (DefineNode::Delete (DeleteNode {
        id     : self . pid,
        source : self . source,
      })); }
    let title : String =
      self . title . ok_or (
        "NodeEditIntent save is missing title")?;
    Ok (DefineNode::Save (SaveNode (NodeComplete {
      title,
      aliases                      : self . aliases,
      source                       : self . source,
      pid                          : self . pid,
      extra_ids                    : vec![],
      body                         : self . body,
      contains                     : self . contains,
      subscribes_to                : self . subscribes_to,
      hides_from_its_subscriptions :
        self . hides_from_its_subscriptions,
      overrides_view_of            : self . overrides_view_of,
      misc                         : Vec::new(),
    } ))) }
}

/// Converts a viewforest of ViewNodes to preliminary DefineNodes.
///
/// "Naive" means the instructions are not yet reconciled with
/// same-ID duplicates and are not supplemented from disk. The
/// extraction itself may still use interpreted save roles and
/// internal edit intents.
pub fn naive_saveinstructions_from_tree (
  mut viewforest: Tree<ViewNode> // "viewforest" = tree with BufferRoot
) -> Result<Vec<DefineNode>, String> {
  let roles : SaveRoleMap =
    classify_save_roles (&viewforest)?;
  let intents : Vec<NodeEditIntent> =
    naive_node_edit_intents_from_tree (&mut viewforest, &roles)?;
  intents
    . into_iter()
    . map (NodeEditIntent::into_define_node)
    . collect() }

fn naive_node_edit_intents_from_tree (
  viewforest : &mut Tree<ViewNode>,
  roles      : &SaveRoleMap,
) -> Result<Vec<NodeEditIntent>, String> {

  /// May append a NodeEditIntent to 'result', and might recurse.
  /// Skips some nodes, because:
  /// - indefinitive nodes don't generate intents
  /// - aliases     are handled by
  ///   'collect_grandchild_aliases_for_viewnode'
  /// - subscribees are handled by
  ///   'collect_subscribees'
  fn maybe_defineonenode_and_maybe_recurse (
    tree    : &mut Tree<ViewNode>,
    node_id : NodeId,
    roles   : &SaveRoleMap,
    result  : &mut Vec<NodeEditIntent>
  ) -> Result<(), String> {

    /// Defined separately because it's called in two places.
    fn recurse_on_children (
      tree: &mut Tree<ViewNode>,
      node_id: NodeId,
      roles: &SaveRoleMap,
      result: &mut Vec<NodeEditIntent>
    ) -> Result<(), String> {
      for child_treeid in
        { let child_treeids: Vec<NodeId> =
            tree . get (node_id) . unwrap() . children()
            . map(|c| c . id()) . collect();
          child_treeids }
        { maybe_defineonenode_and_maybe_recurse(
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
      ViewNodeKind::True (t) => {
        if role == SaveRole::OrdinaryTrueNode
           || matches! (role, SaveRole::AsSubscribee { .. })
        { if let Some (intent)
            = maybe_intent_from_treenode (
                tree, node_id, roles, &t )?
            { result . push (intent); }
          recurse_on_children( tree, node_id, roles, result )?; }}}
    Ok(( )) }

  let mut result: Vec<NodeEditIntent> = Vec::new();
  let root_id : NodeId = viewforest . root() . id();
  maybe_defineonenode_and_maybe_recurse (
    viewforest, root_id, roles, &mut result ) ?;
  Ok (result) }

/// Returns Some(delete intent) or Some(save intent) for definitive nodes.
/// Returns None for indefinitive nodes.
fn maybe_intent_from_treenode (
  tree    : &Tree<ViewNode>,
  node_id : NodeId,
  roles   : &SaveRoleMap,
  t       : &TrueNode
) -> Result<Option<NodeEditIntent>, String> {
  match &t . indef_or_def {
    IndefOrDef::Indefinitive => return Ok (None),
    IndefOrDef::Definitive { body, edit_request } => {
      if *edit_request == Some (EditRequest::Delete) {
        return Ok(Some(NodeEditIntent {
          pid                          : t . id . clone(),
          source                       : t . source . clone(),
          delete                       : true,
          title                        : None,
          body                         : None,
          contains                     : Vec::new(),
          aliases                      : MSV::Unspecified,
          subscribes_to                : MSV::Unspecified,
          hides_from_its_subscriptions : MSV::Unspecified,
          overrides_view_of            : MSV::Unspecified,
        } )); }
      let node_ref : NodeRef<ViewNode> =
        tree . get (node_id) . ok_or(
          "maybe_intent_from_treenode: node not found")?;
      Ok(Some(NodeEditIntent {
        pid           : t . id . clone(),
        source        : t . source . clone(),
        delete        : false,
        title         : Some (t . title . clone()),
        body          : body . clone(),
        contains      :
          collect_contents_to_save_from_children (&node_ref, roles),
        aliases       : collect_grandchild_aliases_for_viewnode(
          tree, node_id)?,
        subscribes_to :
          collect_subscribees( tree, node_id, roles )?,
        hides_from_its_subscriptions : MSV::Unspecified,
        overrides_view_of            : MSV::Unspecified,
      })) }}}

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
