use crate::dbs::typedb::nodes::create_node;
use crate::dbs::typedb::relationships::insert_relationship_from_list;
use crate::dbs::typedb::util::ConceptRowStream;
use crate::dbs::typedb::util::extract_payload_from_typedb_string_rep;
use crate::types::misc::ID;
use crate::types::save::Merge;
use crate::types::skgnode::SkgNode;

use futures::StreamExt;
use std::collections::HashSet;
use std::error::Error;
use typedb_driver::{TypeDBDriver, Transaction, TransactionType};
use typedb_driver::answer::{QueryAnswer, ConceptRow};

/// Merges nodes in TypeDB by applying Merge.
/// All merges are batched in a single transaction.
pub(super) async fn merge_nodes_in_typedb (
  db_name            : &str,
  driver             : &TypeDBDriver,
  merge_instructions : &[Merge],
) -> Result < (), Box<dyn Error> > {
  if merge_instructions.is_empty() {
    return Ok (( )); }
  let tx : Transaction = driver.transaction(
    db_name, TransactionType::Write).await?;
  for merge in merge_instructions {
    let ( acquiree_text_preserver,
          updated_acquirer,
          (acquiree_id, _acquiree_source) )
      : (&SkgNode, &SkgNode, (&ID, _))
      = merge.targets_from_merge();
    merge_one_node_in_typedb (
      &tx,
      acquiree_text_preserver,
      updated_acquirer,
      acquiree_id,
    ).await?; }
  tx.commit().await?;
  Ok (( )) }

/// Adds the processing for a single merge to the transaction.
async fn merge_one_node_in_typedb(
  tx: &Transaction,
  acquiree_text_preserver : &SkgNode,
  updated_acquirer        : &SkgNode,
  acquiree_id             : &ID,
) -> Result<(), Box<dyn Error>> {
  let acquirer_id  : &ID = updated_acquirer        . primary_id()?;
  let preserver_id : &ID = acquiree_text_preserver . primary_id()?;
  create_node( // Create the text preserver node before using it.
    // PITFALL: Rerouting to a nonexistent node fails silently.
    acquiree_text_preserver, tx).await?;

  { // Reroute relationships.
    reroute_relationships_for_merge (
      tx, acquiree_id, acquirer_id,
      "contains", "container", "contained" ). await ?;
    reroute_relationships_for_merge (
      tx, acquiree_id, acquirer_id,
      "contains", "contained", "container" ). await ?;
    reroute_relationships_for_merge ( // Because the preserver has the acquiree's text, it links to everything the acquiree linked to.
      tx, acquiree_id, preserver_id,
      "textlinks_to", "source", "dest" ). await ?;
    reroute_relationships_for_merge (
      tx, acquiree_id, acquirer_id,
      "textlinks_to", "dest", "source" ). await ?;
    reroute_relationships_for_merge (
      tx, acquiree_id, acquirer_id,
      "subscribes", "subscriber", "subscribee" ). await ?;
    reroute_relationships_for_merge (
      tx, acquiree_id, acquirer_id,
      "subscribes", "subscribee", "subscriber" ). await ?;
    drop_relationships_for_merge (
      // Whether the acquiree was hidden from N says nothing about
      // whether the acquirer should be hidden from N,
      // since it has information the acquiree did not.
      // We assume it should not be so hidden.
      tx, acquiree_id,
      "hides_from_its_subscriptions", "hidden", "hider" ). await ?;
    reroute_what_acquiree_hides (
      // PITFALL: Must happen after rerouting 'contains',
      // to know which hide relatinoships to filter out.
      tx, acquirer_id, acquiree_id,
      ( &updated_acquirer.contains.as_ref().map(
        |v| v.iter().cloned().collect()).unwrap_or_default()
      )) . await ?;
    reroute_relationships_for_merge (
      tx, acquiree_id, acquirer_id,
      "overrides_view_of", "replacement", "replaced" ). await ?;
    drop_relationships_for_merge (
      // If the acquiree was replaced by something else,
      // we don't know whether the acquirer should be.
      // It probably shouldn't, since if it were,
      // the user probably would not be editing it.
      tx, acquiree_id,
      "overrides_view_of", "replaced", "replacement" ). await ?; }

  { // Give the acquirer more IDs.
    create_and_connect_extra_id(
      tx, acquirer_id, acquiree_id).await?;
    reroute_extra_ids_for_merge( // PITFALL: Relies on TypeDB constraints. See the comment above its definition for details.
      tx, acquiree_id, acquirer_id).await?; }

  tx.query( format!( // Delete the acquiree.
               r#"match $node isa node, has id "{}"; delete $node;"#,
               acquiree_id.as_str()
          )).await.map_err( |e|
             format!( "Failed to delete acquiree node '{}': {}",
                      acquiree_id.as_str(), e))?;
  insert_relationship_from_list( // add preserver to acquirer's content
    acquirer_id.as_str(), &vec![preserver_id.clone()],
    "contains", "container", "contained", tx
  ).await?;
  Ok (( )) }

/// Move extra_ids from old_node to new_node.
/// 'extra_id' entities are preserved; only relationships change.
/// ASSUMES:
/// - An ID can only belong to one thing (node or extra_id).
///   It can't be owned by both types, or by two of either type.
/// - An extra_id can only be in one has_extra_id relationship.
/// The TypeDB schema enforces both assumptions. Without them,
/// the acquirer could already have a relationship to one of these
/// extra_ids, so this could then create a duplicate relationship.
async fn reroute_extra_ids_for_merge(
  tx: &Transaction,
  old_id: &ID,
  new_id: &ID,
) -> Result<(), Box<dyn Error>> {
  tx.query(format!(
    r#"match
         $old_node isa node, has id "{}";
         $new_node isa node, has id "{}";
         $e isa extra_id;
         $old_rel isa has_extra_id (node: $old_node,
                                    extra_id: $e);
       delete $old_rel;
       insert
         $new_rel isa has_extra_id (node: $new_node,
                                    extra_id: $e);"#,
    old_id.as_str(),
    new_id.as_str()
  )).await?;
  Ok(( )) }

/// Creates an extra_id entity for the given ID value
/// and links it to the node.
async fn create_and_connect_extra_id(
  tx: &Transaction,
  node_id: &ID,
  extra_id_value: &ID,
) -> Result<(), Box<dyn Error>> {
  tx.query(format!(
    r#"match
         $node isa node, has id "{}";
       insert
         $e isa extra_id, has id "{}";
         $r isa has_extra_id (node: $node, extra_id: $e);"#,
    node_id.as_str(),
    extra_id_value.as_str()
  )).await?;
  Ok(())
}

/// Drops relationships where old_id plays old_role.
/// This is used when the old node is being merged and we don't want
/// to preserve certain relationships.
async fn drop_relationships_for_merge (
  tx            : &Transaction,
  old_id        : &ID,
  relation_name : &str,
  old_role      : &str,
  other_role    : &str,
) -> Result < (), Box<dyn Error> > {
  tx.query ( {
    let query : String =
      format! (
        r#"match
             $old_node isa node, has id "{}";
             $other isa node;
             $rel isa {} ({}: $old_node, {}: $other);
           delete $rel;"#,
        old_id.as_str (),
        relation_name,
        old_role,
        other_role );
    query } ). await ?;
  Ok (( )) }

/// Given a 'relation_name' between 'old_id' playing 'old_role'
/// and 'other_id' playing 'other_role',
/// deletes that relationship and makes a corresponding one
/// substituting 'new_id' for 'old_id'.
async fn reroute_relationships_for_merge (
  tx            : &Transaction,
  old_id        : &ID,
  new_id        : &ID,
  relation_name : &str,
  old_role      : &str,
  other_role    : &str,
) -> Result < (), Box<dyn Error> > {
  tx.query ( {
    let query : String =
      format! (
        r#"match
             $old_node isa node, has id "{}";
             $new_node isa node, has id "{}";
             $other isa node;
             $old_rel isa {} ({}: $old_node, {}: $other);
           delete $old_rel;
           insert
             $new_rel isa {} ({}: $new_node, {}: $other);"#,
        old_id.as_str (),
        new_id.as_str (),
        relation_name,
        old_role,
        other_role,
        relation_name,
        old_role,
        other_role );
    query } ). await ?;
  Ok (( )) }

/// Reroute what the acquiree hides.
/// Transfer if hidden node not in acquirer's contents.
/// Otherwise just drop the relationship.
async fn reroute_what_acquiree_hides (
  tx                       : &Transaction,
  acquirer_id              : &ID,
  acquiree_id              : &ID,
  acquirer_final_contains  : &HashSet<ID>,
) -> Result < (), Box<dyn Error> > {
  drop_relationships_for_merge (
    tx, acquiree_id,
    "hides_from_its_subscriptions", "hidden", "hider" ). await ?;
  let answer : QueryAnswer = tx.query( {
    let what_acquiree_hides : String = format!(
      r#"match
           $acquiree isa node, has id "{}";
           $r isa hides_from_its_subscriptions (hider: $acquiree,
                                                hidden: $hidden);
           $hidden has id $hidden_id;
         select $hidden_id;"#,
      acquiree_id.as_str() );
    what_acquiree_hides } ).await?;
  let mut stream : ConceptRowStream = answer.into_rows();
  while let Some(row_result) = stream.next().await {
    let row : ConceptRow = row_result?;
    if let Some(concept) = row.get("hidden_id")? {
      let hidden_id : ID = ID( {
        let hidden_id_str : String =
          extract_payload_from_typedb_string_rep(
            &concept.to_string());
        hidden_id_str } );
      if !acquirer_final_contains.contains(&hidden_id) {
        // If this ID is not in acquirer's final contains,
        // then transfer the relationship.
        tx.query( {
          let transfer_query : String = format!(
            r#"match
                 $acquirer isa node, has id "{}";
                 $hidden isa node, has id "{}";
                 $acquiree isa node, has id "{}";
                 $old_r isa hides_from_its_subscriptions (hider: $acquiree, hidden: $hidden);
               delete $old_r;
               insert
                 $new_r isa hides_from_its_subscriptions (hider: $acquirer, hidden: $hidden);"#,
            acquirer_id.as_str(),
            hidden_id.as_str(),
            acquiree_id.as_str() );
          transfer_query } ).await?;
      } else {
        // Just delete, b/c acquirer shouldn't hite its own content.
        tx.query ( {
          let delete_query : String = format!(
            r#"match
                 $acquiree isa node, has id "{}";
                 $hidden isa node, has id "{}";
                 $old_r isa hides_from_its_subscriptions (hider: $acquiree, hidden: $hidden);
               delete $old_r;"#,
            acquiree_id.as_str(),
            hidden_id.as_str() );
          delete_query } ). await ?; }} }
  Ok (( )) }
