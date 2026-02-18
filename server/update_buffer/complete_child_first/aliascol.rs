use crate::types::git::{FieldDiffStatus, NodeChanges, SourceDiff, node_changes_for_truenode};
use crate::types::list::Diff_Item;
use crate::types::misc::{ID, SourceName};
use crate::types::skgnode::SkgNode;
use crate::types::skgnodemap::SkgNodeMap;
use crate::types::viewnode::{ViewNode, ViewNodeKind, Scaffold};
use crate::types::tree::generic::{pid_and_source_from_ancestor, read_at_ancestor_in_tree};
use crate::update_buffer::util::{complete_relevant_children_in_viewnodetree, treat_certain_children};
use ego_tree::{NodeId, Tree};
use std::collections::HashMap;
use std::error::Error;

/// Reconciles an AliasCol's children against
///   the aliases on disk (via the map) for its parent TrueNode.
///
/// Per the spec in buffer-update.org:
/// - Verify this node is an AliasCol
/// - Verify its parent is a TrueNode
/// - Fetch the corresponding SkgNode from the map
/// - Read its aliases into 'aliases'
/// - Partition the AliasCol's children into:
///   - TrueNodes with parent_ignores=true
///   - Alias scaffold nodes
///   (Error if any child does not fit these categories.)
/// - Reorder children: ignored TrueNodes first, then Alias nodes
/// - Among the Alias children, discard any not in 'aliases'
/// - Create new Alias nodes for values in 'aliases' not already present
/// - Order the final Alias children to match the order in 'aliases'
pub fn completeAliasCol (
  tree             : &mut Tree<ViewNode>,
  map              : &SkgNodeMap,
  aliascol_node_id : NodeId,
  source_diffs     : &Option<HashMap<SourceName, SourceDiff>>,
) -> Result<(), Box<dyn Error>> {
  { let is_aliascol : bool = // barf if not an aliascol
      read_at_ancestor_in_tree(
        tree, aliascol_node_id, 0,
        |viewnode| matches!( &viewnode.kind,
                            ViewNodeKind::Scaff( Scaffold::AliasCol )) )
      .map_err( |e| -> Box<dyn Error> { e.into() } )?;
    if !is_aliascol { return Err(
      "completeAliasCol: Node is not an AliasCol".into() ); }}
  let (parent_pid, parent_source) : (ID, SourceName) =
    pid_and_source_from_ancestor(
      tree, aliascol_node_id, 1,
      "completeAliasCol" ) ?;
  let parent_skgnode : &SkgNode =
    map.get( &parent_pid )
    .ok_or( "completeAliasCol: Parent SkgNode not in map" )?;
  let node_changes : Option<&NodeChanges> =
    node_changes_for_truenode(
      source_diffs, &parent_pid, &parent_source );
  let (goal_list, diff_map)
    : (Vec<String>, HashMap<String, Option<FieldDiffStatus>>)
    = match node_changes {
      None => { // No git diff view: Easy.
        let goals : Vec<String> =
          parent_skgnode.aliases.clone().unwrap_or_default();
        ( goals, HashMap::new() ) }
      Some( nc ) => { // Git diff view.
        let mut goals : Vec<String> = Vec::new();
        let mut dmap : HashMap<String, Option<FieldDiffStatus>> =
          HashMap::new();
        for entry in &nc.aliases_diff {
          let (text, diff) : (String, Option<FieldDiffStatus>) =
            match entry {
              Diff_Item::Unchanged( t ) =>
                ( t.clone(), None ),
              Diff_Item::New( t ) =>
                ( t.clone(), Some( FieldDiffStatus::New ) ),
              Diff_Item::Removed( t ) =>
                ( t.clone(), Some( FieldDiffStatus::Removed ) ), };
          goals.push( text.clone() );
          dmap.insert( text, diff ); }
        ( goals, dmap ) } };
  let is_alias : fn(&ViewNode) -> bool =
    // relevance to complete_relevant_children
    |viewnode| matches!( &viewnode.kind,
                        ViewNodeKind::Scaff( Scaffold::Alias { .. } ) );
  let view_alias_text : fn(&ViewNode) -> String =
    |viewnode| match &viewnode.kind {
      ViewNodeKind::Scaff( Scaffold::Alias { text, .. } ) =>
        text.clone(),
      _ => unreachable!(), }; // relevance means Scaffold::Alias
  let create_alias = |text: &String| -> ViewNode {
    let diff : Option<FieldDiffStatus> =
      diff_map.get( text ).copied().flatten();
    ViewNode {
      focused : false,
      folded  : false,
      kind    : ViewNodeKind::Scaff(
        Scaffold::Alias { text : text.clone(),
                          diff } ), } };
  complete_relevant_children_in_viewnodetree(
    tree,
    aliascol_node_id,
    is_alias,
    view_alias_text,
    &goal_list,
    create_alias )?;
  treat_certain_children( // Currently unreachable: validation rejects TrueNode children under AliasCol. Left here in case validation is later relaxed.
      tree, aliascol_node_id,
      |vn : &ViewNode| matches!( &vn.kind, ViewNodeKind::True( _ )),
      |vn : &mut ViewNode| {
        if let ViewNodeKind::True( ref mut t ) = vn.kind {
          t.parent_ignores = true; }},
    ).map_err( |e| -> Box<dyn Error> { e.into() } )?;
  Ok( () ) }
