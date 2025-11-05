use crate::types::{MergeInstructionTriple, SkgNode, ID};
use crate::typedb::nodes::create_node;
use crate::typedb::relationships::create_relationships_from_node;
use crate::typedb::util::extract_payload_from_typedb_string_rep;
use futures::StreamExt;
use std::collections::HashSet;
use std::error::Error;
use typedb_driver::{TypeDBDriver, Transaction, TransactionType};
use typedb_driver::answer::QueryAnswer;

/// Merges nodes in TypeDB by applying MergeInstructionTriple.
/// All merges are batched in a single transaction.
pub(super) async fn merge_nodes_in_typedb (
  db_name            : &str,
  driver             : &TypeDBDriver,
  merge_instructions : &[MergeInstructionTriple],
) -> Result < (), Box<dyn Error> > {
  if merge_instructions.is_empty() {
    return Ok (( )); }
  let tx : Transaction = driver.transaction(
    db_name, TransactionType::Write).await?;
  for merge in merge_instructions {
    merge_one_node_in_typedb (
      &tx,
      &merge.acquiree_text_preserver.0,
      &merge.updated_acquirer.0,
      &merge.acquiree_to_delete.0,
    ).await?; }
  tx.commit().await?;
  Ok (( )) }

/// Adds the processing for a single merge to the transaction.
async fn merge_one_node_in_typedb(
  tx: &Transaction,
  acquiree_text_preserver : &SkgNode,
  updated_acquirer        : &SkgNode,
  acquiree                : &SkgNode,
) -> Result<(), Box<dyn Error>> {
  let acquirer_id  : &ID = &updated_acquirer        . ids[0];
  let acquiree_id  : &ID = &acquiree                . ids[0];
  let preserver_id : &ID = &acquiree_text_preserver . ids[0];

  { // Reroute relationships.
    reroute_relationships_for_merge (
      tx, acquiree_id, acquirer_id,
      "contains", "container", "contained" ). await ?;
    reroute_relationships_for_merge (
      tx, acquiree_id, acquirer_id,
      "contains", "contained", "container" ). await ?;
    reroute_relationships_for_merge (
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

  delete_extra_ids_for_node(
    tx, acquiree_id).await?;
  tx.query( format!( // Delete acquiree node
    r#"match
         $node isa node, has id "{}";
       delete $node;"#,
    acquiree_id.as_str()
  )) .await
    .map_err(|e| format!("Failed to delete acquiree node '{}': {}",
                         acquiree_id.as_str(), e))?;

  create_node(acquiree_text_preserver, tx).await?;
  create_relationships_from_node(
    acquiree_text_preserver, tx )
    . await
    . map_err ( |e| format!("Failed to create relationships for acquiree_text_preserver: {}", e ))?;
  let acquiree_text_preserver_id : &ID =
    &acquiree_text_preserver.ids[0];
  let contains_query : String = format!(
    r#"match
         $acquirer isa node, has id "{}";
         $preserver isa node, has id "{}";
       insert
         $contains_rel isa contains (container: $acquirer,
                                     contained: $preserver);"#,
    acquirer_id.as_str(),
    acquiree_text_preserver_id.as_str() );
  tx.query(contains_query).await?;

  for extra_id_value in &acquiree.ids {
    // Add extra_ids to acquirer.
    // PITFALL: Even the acquiree's PID becomomes an acquirer extra_id.
    // PITFALL: Should happen after deleting acquiree's extra_ids.
    let query : String = format!(
      r#"
      match
        $acquirer isa node, has id "{}";
      insert
        $e isa extra_id, has id "{}";
        $r isa has_extra_id
          ( node: $acquirer,
            extra_id: $e );"#,
      acquirer_id.as_str(),
      extra_id_value.as_str() );
    tx.query(query).await?; }
  Ok (( )) }

/// Deletes all extra_ids associated with a node.
async fn delete_extra_ids_for_node(
  tx: &Transaction,
  node_id: &ID,
) -> Result<(), Box<dyn Error>> {
  // Find all extra_ids (as strings) associated with the node.
  let extra_ids_query : String = format!(
    r#"match
         $node isa node, has id "{}";
         $e isa extra_id;
         $rel isa has_extra_id (node: $node, extra_id: $e);
         $e has id $extra_id_value;
       select $extra_id_value;"#,
    node_id.as_str() );
  let answer : QueryAnswer =
    tx.query ( extra_ids_query ). await ?;
  let mut extra_id_values : Vec<String> = Vec::new();
  let mut rows = answer.into_rows();
  while let Some(row_res) = rows.next().await {
    let row = row_res?;
    if let Some(concept) = row.get("extra_id_value")? {
      let extra_id_value : String =
        extract_payload_from_typedb_string_rep(
          &concept.to_string());
      extra_id_values.push(extra_id_value); }}
  for extra_id_value in extra_id_values {
    // Delete each extra_id
    let delete_extra_id_query : String = format!(
      r#"match
           $e isa extra_id, has id "{}";
         delete $e;"#,
      extra_id_value );
    tx . query(delete_extra_id_query) . await?; }
  Ok(( )) }

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
  tx.query ( query ). await ?;
  Ok (( )) }

/// Reroutes relationships where old_id plays old_role
/// to have new_id play that role instead.
/// Deletes the old relationship and creates a new one.
async fn reroute_relationships_for_merge (
  tx            : &Transaction,
  old_id        : &ID,
  new_id        : &ID,
  relation_name : &str,
  old_role      : &str,
  other_role    : &str,
) -> Result < (), Box<dyn Error> > {
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
  tx.query ( query ). await ?;
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
  let what_acquiree_hides : String = format!(
    r#"match
         $acquiree isa node, has id "{}";
         $r isa hides_from_its_subscriptions (hider: $acquiree,
                                              hidden: $hidden);
         $hidden has id $hidden_id;
       select $hidden_id;"#,
    acquiree_id.as_str() );
  let answer : QueryAnswer = tx.query(what_acquiree_hides).await?;
  let mut stream = answer.into_rows();
  while let Some(row_result) = stream.next().await {
    let row = row_result?;
    if let Some(concept) = row.get("hidden_id")? {
      let hidden_id_str : String =
        extract_payload_from_typedb_string_rep(&concept.to_string());
      let hidden_id : ID = ID(hidden_id_str);
      if !acquirer_final_contains.contains(&hidden_id) {
        // If this ID is not in acquirer's final contains,
        // then transfer the relationship.
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
        tx.query(transfer_query).await?;
      } else {
        // Just delete, b/c acquirer shouldn't hite its own content.
        let delete_query : String = format!(
          r#"match
               $acquiree isa node, has id "{}";
               $hidden isa node, has id "{}";
               $old_r isa hides_from_its_subscriptions (hider: $acquiree, hidden: $hidden);
             delete $old_r;"#,
          acquiree_id.as_str(),
          hidden_id.as_str() );
        tx.query ( delete_query ). await ?; }} }
  Ok (( )) }
