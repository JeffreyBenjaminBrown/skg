use crate::to_org::util::skgnode_and_orgnode_from_id;
use crate::types::misc::{ID, SkgConfig};
use crate::types::skgnode::SkgNode;
use crate::types::orgnode::{OrgNodeKind, OrgNode};
use crate::types::tree::{NodePair, PairTree};
use crate::types::tree::generic::{
  read_at_node_in_tree, write_at_node_in_tree, with_node_mut };

use ego_tree::{NodeId, NodeRef};
use std::collections::{HashSet, HashMap};
use std::error::Error;
use typedb_driver::TypeDBDriver;

/// Completes the 'content' of a definitive node's children,
/// using the associated SkgNode's 'contains' field as a reference,
/// maintaining the correct order and creating any that need it.
///
/// ASSUMES: The node's "id" field is its PID.
pub async fn completeAndReorder_childrenOf_definitiveOrgnode (
  tree    : &mut PairTree,
  node_id : NodeId,
  config  : &SkgConfig,
  driver  : &TypeDBDriver,
) -> Result < (), Box<dyn Error> > {
  let content_from_disk_as_list : Option<Vec<ID>> =
    read_at_node_in_tree ( tree, node_id, |np|
      np . mskgnode . as_ref ()
      . map ( |s| s . contains . clone () )
    ) ? . ok_or ( "SkgNode should exist" ) ?;
  let content_from_disk_as_set : HashSet<ID> =
    content_from_disk_as_list . as_ref ()
    . map ( |v| v . iter () . cloned () . collect () )
    . unwrap_or_default ();
  let ( mut content_skgid_to_treeid, // longer-term content map
        mut non_content_child_treeids )
    : ( HashMap < ID, NodeId >,
        Vec < NodeId > ) =
    { let ( content_child_treeids, // short-term content vector
            non_content_child_treeids )
        : ( Vec < NodeId >, Vec < NodeId > )
        = partition_nonignored_true_content_from_other_children (
            tree,
            node_id ) ?;
      let content_skgid_to_treeid : HashMap < ID, NodeId > =
        // PITFALL: This map can initially include non-content. Any such items are next removed by 'mark_parentignores_and_move_invalidated_content'.
          map_skgid_to_treeid ( tree, &content_child_treeids ) ?;
      ( content_skgid_to_treeid,
        non_content_child_treeids ) };

  mark_parentignores_and_move_invalidated_content ( // Move false content from content_skgid_to_treeid to non_content_child_treeids.
    tree,
    &content_from_disk_as_set,
    &mut content_skgid_to_treeid,
    &mut non_content_child_treeids ) ?;

  let mut complete_list_of_content_branch_treeids : Vec < NodeId > =
    Vec::new ();
  if let Some(should_contain) = & content_from_disk_as_list {
    for disk_skgid in should_contain {
      if let Some ( existing_treeid )
      = content_skgid_to_treeid . get ( disk_skgid )
      { // It's already in the tree. Push it on unchanged. It might be a deep branch.
        complete_list_of_content_branch_treeids . push (
          *existing_treeid );
      } else { // It's not in the tree. Push on a new leaf.
        // PITFALL: The preorder DFS traversal for complete_or_restore_each_node_in_branch lets us add a child without considering grandchildren yet.
        complete_list_of_content_branch_treeids . push (
          append_new_definitive_nodepair_to_children_based_on_skgid (
            tree, node_id, disk_skgid, config, driver
          ) . await ? ); }}
    // 'complete_list_of_content_branch_treeids' is now "complete" as in "missing no content". BUT a branch of it could still be incomplete, until 'complete_or_restore_each_node_in_branch' recurses into it.
  }

  reorder_children (
    tree,
    node_id,
    & non_content_child_treeids,
    & complete_list_of_content_branch_treeids ) ?;
  Ok (( )) }

/// PURPOSE: Separate a node's non-ignored true content from its other
/// children.
///
/// SOME TERMS:
/// - "False" content would look like content based on
/// the orgnode tree, but without corresponding content relationships
/// in the dbs. Maybe it can't happen, but the types don't prevent it.
/// - 'Non-intentional' content would be nodes that the parent *does*
/// contain, but marked as parentIgnores. We assume such nodes
/// are copies the user wants to keep in view without affecting
/// the graph. Therefore, when populating a definitive node's contents,
/// they will be ignored. A non-ignored duplicate is made if necessary.
///
/// RETURNS (content_child_ids, non_content_child_ids).
fn partition_nonignored_true_content_from_other_children (
  tree    : &PairTree,
  node_id : NodeId,
) -> Result < ( Vec <NodeId>, // intentional true content
                Vec <NodeId> ), // others
              Box<dyn Error> > {
  let mut content_child_ids : Vec < NodeId > =
    Vec::new ();
  let mut non_content_child_ids : Vec < NodeId > =
    Vec::new ();
  let node_ref : NodeRef < NodePair > =
    tree . get ( node_id )
    . ok_or ( "Node not found in tree" ) ?;
  for child in node_ref . children () {
    if matches! ( &child . value () . orgnode . kind,
                  OrgNodeKind::True(t)
                  if !t.parent_ignores )
    { content_child_ids . push ( child . id () );
    } else {
      non_content_child_ids . push ( child . id () ); }}
  Ok (( content_child_ids, non_content_child_ids )) }

/// Builds a map from each node's Skg ID to its tree ID.
/// Errors if any node lacks an ID.
///
/// PITFALL: If the same ID appeared twice in the list,
/// the resulting map would not be well-defined.
/// But 'find_buffer_errors_for_saving' prevents that.
fn map_skgid_to_treeid (
  tree          : &PairTree,
  child_treeids : &[NodeId],
) -> Result < HashMap < ID, NodeId >, Box<dyn Error> > {
  let mut result : HashMap < ID, NodeId > = HashMap::new ();
  for child_treeid in child_treeids {
    let child_pid : ID =
      read_at_node_in_tree ( tree, *child_treeid, |np|
        match &np . orgnode . kind
        { OrgNodeKind::True(t) => t.id_opt.clone(),
          OrgNodeKind::Scaff(_) => None }
      ) ? . ok_or ( "Content child has no ID" ) ?;
    result . insert ( child_pid, *child_treeid ); }
  Ok ( result ) }

/// PURPOSE : If an entry in 'supposedly_content'
/// is not truly content, then mark it 'parentIgnores'.
/// and move it to 'non_content'.
///
/// MOTIVATION: Content can become invalidated (i.e. non-content)
/// if a separate buffer edited the invalidated content's container.
fn mark_parentignores_and_move_invalidated_content (
  tree               : &mut PairTree,
  truly_content      : &HashSet<ID>, // immutable, used to judge
  supposedly_content : &mut HashMap<ID, NodeId>, // might lose some
  non_content        : &mut Vec<NodeId>, // might correspondingly gain some
) -> Result<(), Box<dyn Error>> {
  let invalid_content_treeids : Vec<NodeId> =
    supposedly_content . iter ()
    . filter ( |(pid, _)| ! truly_content . contains ( *pid ) )
    . map ( |(_, treeid)| *treeid )
    . collect ();
  for invalid_treeid in & invalid_content_treeids {
    let child_pid : ID =
      write_at_node_in_tree ( tree, *invalid_treeid, |np| {
        let OrgNodeKind::True(t) = &mut np . orgnode . kind
          else { return Err ( "Not-really-content child is not a TrueNode" ) };
        t . parent_ignores = true;
        Ok ( t . id_opt . clone () . unwrap () )
      } ) ? ?;
    supposedly_content . remove ( & child_pid );
    non_content . push ( *invalid_treeid ); }
  Ok (()) }

/// Create a new Content node from disk (using 'disk_id')
/// and append it to the children of 'parent_nid'.
async fn append_new_definitive_nodepair_to_children_based_on_skgid (
  tree       : &mut PairTree,
  parent_nid : NodeId,
  skgid      : &ID,
  config     : &SkgConfig,
  driver     : &TypeDBDriver,
) -> Result < NodeId, Box<dyn Error> > {
  let ( skgnode, orgnode ) : ( SkgNode, OrgNode ) =
    skgnode_and_orgnode_from_id (
      config, driver, skgid ) . await ?;
  let new_child_id : NodeId = with_node_mut (
    tree, parent_nid,
    ( |mut parent_mut|
      parent_mut . append ( NodePair { mskgnode : Some(skgnode),
                                       orgnode  : orgnode } )
      . id () )) ?;
  Ok ( new_child_id ) }

/// Reorder a node's children by detaching all
/// and re-appending in desired order.
fn reorder_children (
  tree          : &mut PairTree,
  parent_treeid : NodeId,
  first         : &Vec < NodeId >,
  second        : &Vec < NodeId >,
) -> Result < (), Box<dyn Error> > {
  let mut desired_order : Vec < NodeId > =
    Vec::new ();
  desired_order . extend ( first );
  desired_order . extend ( second );

  for child_treeid in & desired_order {
    // Detach all children
    with_node_mut ( tree, *child_treeid,
                    |mut child_mut| child_mut . detach () ) ?; }

  for child_treeid in & desired_order {
    // Re-append in desired order.
    // ('append_id' preserves entire subtrees.)
    with_node_mut (
      tree, parent_treeid,
      |mut parent_mut| {
        parent_mut . append_id ( *child_treeid ); } ) ?; }
  Ok (( )) }
