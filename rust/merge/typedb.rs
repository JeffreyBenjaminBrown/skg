use crate::types::{SaveInstruction, SkgNode, ID};
use crate::typedb::nodes::create_node;
use crate::typedb::util::extract_payload_from_typedb_string_rep;
use futures::StreamExt;
use std::collections::HashSet;
use std::error::Error;
use typedb_driver::{TypeDBDriver, Transaction, TransactionType};
use typedb_driver::answer::QueryAnswer;

/// Merges nodes in TypeDB by applying merge SaveInstructions.
/// All merges are batched in a single transaction.
pub(crate) async fn merge_nodes_in_typedb (
  db_name      : &str,
  driver       : &TypeDBDriver,
  _instructions : &[SaveInstruction],
  acquiree_text_preservers : Vec<&SaveInstruction>,
  updated_acquirers : Vec<&SaveInstruction>,
  deleted_acquirees : Vec<&SaveInstruction>,
) -> Result < (), Box<dyn Error> > {
  if acquiree_text_preservers.is_empty() {
    return Ok (( )); }
  let tx : Transaction = driver.transaction(
    db_name, TransactionType::Write).await?;
  for i in 0..acquiree_text_preservers.len() {
    process_merge_in_typedb(
      &tx,
      &acquiree_text_preservers[i].0,
      &updated_acquirers[i].0,
      &deleted_acquirees[i].0,
    ).await?; }
  tx.commit().await?;
  Ok (( )) }

/// Adds the processing for a single merge to the transaction.
async fn process_merge_in_typedb(
  tx: &Transaction,
  acquiree_text_preserver: &SkgNode,
  updated_acquirer: &SkgNode,
  acquiree: &SkgNode,
) -> Result<(), Box<dyn Error>> {
  let acquirer_id : &ID = &updated_acquirer.ids[0];
  let acquiree_id : &ID = &acquiree.ids[0];
  reroute_contains(tx, acquirer_id, acquiree_id).await?;
  reroute_subscribes(tx, acquirer_id, acquiree_id).await?;
  reroute_overrides(tx, acquirer_id, acquiree_id).await?;
  reroute_hides(
    tx, acquirer_id, acquiree_id,
    &updated_acquirer.contains.iter().cloned().collect()
  ). await ?;

  // Delete acquiree's extra_ids BEFORE deleting the node
  delete_extra_ids_for_node(tx, acquiree_id).await?;

  tx.query( format!( // Delete acquiree node
    r#"match
         $node isa node, has id "{}";
       delete $node;"#,
    acquiree_id.as_str()
  )) .await?;

  // Create the node that saves the acquiree's text
  create_node(acquiree_text_preserver, tx).await?;

  // Acquirer should contain acquiree_text_preserver
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
    // Add extra_ids to acquirer (all acquiree IDs become extra_ids).
    // This must happen AFTER deleting the acquiree's extra_ids.
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
  // find all extra_ids (as strings) associated with the node
  let extra_ids_query : String = format!(
    r#"match
         $node isa node, has id "{}";
         $e isa extra_id;
         $rel isa has_extra_id (node: $node, extra_id: $e);
         $e has id $extra_id_value;
       select $extra_id_value;"#,
    node_id.as_str() );
  let answer : QueryAnswer = tx.query(extra_ids_query).await?;
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
    let delete_extra_id_query : String = format!(
      // Delete each extra_id
      r#"match
           $e isa extra_id, has id "{}";
         delete $e;"#,
      extra_id_value );
    tx.query(delete_extra_id_query).await?; }
  Ok(( )) }

/// Reroute contains relationships (bilateral transfer).
async fn reroute_contains(
  tx: &Transaction,
  acquirer_id: &ID,
  acquiree_id: &ID,
) -> Result<(), Box<dyn Error>> {

  // Acquiree was container → acquirer is now container
  let query1 : String = format!(
    r#"match
         $acquiree isa node, has id "{}";
         $acquirer isa node, has id "{}";
         $c isa node;
         $contains_rel isa contains (container: $acquiree, contained: $c);
       delete $contains_rel;
       insert
         $new_contains_rel isa contains (container: $acquirer, contained: $c);"#,
    acquiree_id.as_str(),
    acquirer_id.as_str()
  );
  tx.query(query1).await?;

  // Acquiree was contained → acquirer is now contained
  let query2 : String = format!(
    r#"match
         $acquiree2 isa node, has id "{}";
         $acquirer2 isa node, has id "{}";
         $container isa node;
         $contains_rel2 isa contains (container: $container, contained: $acquiree2);
       delete $contains_rel2;
       insert
         $new_contains_rel2 isa contains (container: $container, contained: $acquirer2);"#,
    acquiree_id.as_str(),
    acquirer_id.as_str()
  );
  tx.query(query2).await?;

  Ok(())
}

/// Reroute subscribes relationships (bilateral transfer).
async fn reroute_subscribes(
  tx: &Transaction,
  acquirer_id: &ID,
  acquiree_id: &ID,
) -> Result<(), Box<dyn Error>> {

  // Acquiree was subscriber → acquirer is now subscriber
  let query1 : String = format!(
    r#"match
         $acquiree_sub isa node, has id "{}";
         $acquirer_sub isa node, has id "{}";
         $s isa node;
         $sub_rel isa subscribes (subscriber: $acquiree_sub, subscribee: $s);
       delete $sub_rel;
       insert
         $new_sub_rel isa subscribes (subscriber: $acquirer_sub, subscribee: $s);"#,
    acquiree_id.as_str(),
    acquirer_id.as_str()
  );
  tx.query(query1).await?;

  // Acquiree was subscribee → acquirer is now subscribee
  let query2 : String = format!(
    r#"match
         $acquiree_subee isa node, has id "{}";
         $acquirer_subee isa node, has id "{}";
         $s2 isa node;
         $subee_rel isa subscribes (subscriber: $s2, subscribee: $acquiree_subee);
       delete $subee_rel;
       insert
         $new_subee_rel isa subscribes (subscriber: $s2, subscribee: $acquirer_subee);"#,
    acquiree_id.as_str(),
    acquirer_id.as_str()
  );
  tx.query(query2).await?;

  Ok(())
}

/// Reroute overrides_view_of relationships (conditional).
async fn reroute_overrides(
  tx: &Transaction,
  acquirer_id: &ID,
  acquiree_id: &ID,
) -> Result<(), Box<dyn Error>> {

  // Acquiree was replacement → acquirer is now replacement (TRANSFER)
  let query1 : String = format!(
    r#"match
         $acquiree isa node, has id "{}";
         $acquirer isa node, has id "{}";
         $replaced_node isa node;
         $rel_old isa overrides_view_of (replacement: $acquiree, replaced: $replaced_node);
       delete $rel_old;
       insert
         $rel_new isa overrides_view_of (replacement: $acquirer, replaced: $replaced_node);"#,
    acquiree_id.as_str(),
    acquirer_id.as_str()
  );
  tx.query(query1).await?;

  // Acquiree was replaced → DROP (do not recreate)
  let query2 : String = format!(
    r#"match
         $acquiree isa node, has id "{}";
         $replacement_node isa node;
         $rel_drop isa overrides_view_of (replacement: $replacement_node, replaced: $acquiree);
       delete $rel_drop;"#,
    acquiree_id.as_str()
  );
  tx.query(query2).await?;

  Ok(())
}

/// Reroute hides_from_its_subscriptions relationships (most complex).
async fn reroute_hides(
  tx: &Transaction,
  acquirer_id: &ID,
  acquiree_id: &ID,
  acquirer_final_contains: &HashSet<ID>,
) -> Result<(), Box<dyn Error>> {

  // Acquiree is hidden → DROP (do not recreate)
  let query1 : String = format!(
    r#"match
         $acquiree isa node, has id "{}";
         $h isa node;
         $r isa hides_from_its_subscriptions (hider: $h, hidden: $acquiree);
       delete $r;"#,
    acquiree_id.as_str()
  );
  tx.query(query1).await?;

  // Acquiree is hider → transfer ONLY if hidden node NOT in acquirer's contains
  // First, query all hidden nodes
  let query_hidden : String = format!(
    r#"match
         $acquiree isa node, has id "{}";
         $r isa hides_from_its_subscriptions (hider: $acquiree, hidden: $hidden);
         $hidden has id $hidden_id;
       select $hidden_id;"#,
    acquiree_id.as_str()
  );

  let answer : QueryAnswer = tx.query(query_hidden).await?;
  let mut stream = answer.into_rows();

  while let Some(row_result) = stream.next().await {
    let row = row_result?;
    if let Some(concept) = row.get("hidden_id")? {
      let hidden_id_str : String =
        extract_payload_from_typedb_string_rep(&concept.to_string());
      let hidden_id : ID = ID(hidden_id_str);

      // Check if this ID is in acquirer's final contains
      if !acquirer_final_contains.contains(&hidden_id) {
        // Transfer this relationship
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
          acquiree_id.as_str()
        );
        tx.query(transfer_query).await?;
      } else {
        // Just delete (do not recreate - can't hide your own content)
        let delete_query : String = format!(
          r#"match
               $acquiree isa node, has id "{}";
               $hidden isa node, has id "{}";
               $old_r isa hides_from_its_subscriptions (hider: $acquiree, hidden: $hidden);
             delete $old_r;"#,
          acquiree_id.as_str(),
          hidden_id.as_str()
        );
        tx.query(delete_query).await?;
      }
    }
  }

  Ok(())
}
