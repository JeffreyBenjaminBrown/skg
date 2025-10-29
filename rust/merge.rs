use crate::file_io::{read_node, write_node};
use crate::types::{SaveInstruction, SkgConfig, OrgNode, SkgNode, NodeSaveAction, ID, NodeRequest};
use crate::util::path_from_pid;
use crate::typedb::nodes::create_node;
use crate::typedb::util::extract_payload_from_typedb_string_rep;
use crate::tantivy::update_index_from_saveinstructions;
use ego_tree::Tree;
use futures::StreamExt;
use std::collections::HashSet;
use std::error::Error;
use std::fs;
use typedb_driver::{TypeDBDriver, Transaction, TransactionType};
use typedb_driver::answer::QueryAnswer;

/// Creates SaveInstructions for merge operations from an orgnode forest.
///
/// For each node with a merge instruction, this creates three SaveInstructions:
/// - A new acquiree_text_preserver containing the acquiree's title and body
/// - An updated acquirer node with modified contents and extra IDs
/// - A deletion instruction for the acquiree node
///
/// TODO: This is slightly inefficient. It would be faster to collect a list
/// of orgnodes with merge instructions during one of the other walks of the forest.
pub async fn saveinstructions_from_the_merges_in_an_orgnode_forest(
  forest: &[Tree<OrgNode>],
  config: &SkgConfig,
  _driver: &TypeDBDriver,
) -> Result<Vec<SaveInstruction>, Box<dyn Error>> {
  let mut instructions: Vec<SaveInstruction> = Vec::new();

  // Walk the forest to find nodes with merge requests
  for tree in forest {
    for edge in tree.root().traverse() {
      if let ego_tree::iter::Edge::Open(node_ref) = edge {
        let node: &OrgNode = node_ref.value();

        // Check if this node has merge requests
        for request in &node.metadata.code.nodeRequests {
          if let NodeRequest::Merge(acquiree_id) = request {
            let acquirer_id = node.metadata.id.as_ref()
              .ok_or("Node with merge request must have an ID")?;

            // Fetch acquirer and acquiree from disk
            let acquirer_from_disk: SkgNode =
              read_node(&path_from_pid(config, acquirer_id.clone()))?;
            let acquiree_from_disk: SkgNode =
              read_node(&path_from_pid(config, acquiree_id.clone()))?;

            // Create acquiree_text_preserver node
            let acquiree_text_preserver: SkgNode = create_acquiree_text_preserver(
              &acquiree_from_disk);
            let acquiree_text_preserver_id: &ID = &acquiree_text_preserver.ids[0];

            // Append acquiree's IDs to acquirer's
            let mut updated_acquirer: SkgNode =
              acquirer_from_disk.clone();
            updated_acquirer.ids = acquirer_from_disk.ids.clone();
            for id in &acquiree_from_disk.ids {
              if !updated_acquirer.ids.contains(id) {
                updated_acquirer.ids.push(id.clone( )); }}

            // Update contains: [acquiree_text_preserver] + acquirer's old + acquiree's old
            let mut new_contains: Vec<ID> = vec![acquiree_text_preserver_id.clone()];
            new_contains.extend(acquirer_from_disk.contains.clone());
            new_contains.extend(acquiree_from_disk.contains.clone());
            updated_acquirer.contains = new_contains;

            // Add the three SaveInstructions
            instructions.push((
              acquiree_text_preserver,
              NodeSaveAction { indefinitive: false,
                               toDelete: false } ));
            instructions.push((
              updated_acquirer,
              NodeSaveAction { indefinitive: false,
                               toDelete: false } ));
            instructions.push((
              acquiree_from_disk,
              NodeSaveAction { indefinitive: false,
                               toDelete: true } )); }} }} }
  Ok(instructions) }

/// Create a MERGED node from the acquiree's data
fn create_merged_node(acquiree: &SkgNode) -> SkgNode {
  SkgNode {
    title: format!("MERGED: {}", acquiree.title),
    aliases: None,
    ids: vec![ID(uuid::Uuid::new_v4().to_string())],
    body: acquiree.body.clone(),
    contains: vec![],
    subscribes_to: Some(vec![]),
    hides_from_its_subscriptions: Some(vec![]),
    overrides_view_of: Some(vec![]),
  }}

/// Merges nodes in the graph by applying merge SaveInstructions.
/// Updates three systems in order:
///   1) TypeDB
///   2) Filesystem
///   3) Tantivy
/// PITFALL: If any but the first step fails,
///   the resulting system state is invalid.
pub async fn merge_nodes_in_graph (
  instructions  : Vec<SaveInstruction>,
  config        : SkgConfig,
  tantivy_index : &tantivy::Index,
  driver        : &TypeDBDriver,
) -> Result < (), Box<dyn Error> > {

  println!(
    "Merging nodes in TypeDB, FS, and Tantivy, in that order ..." );
  let db_name : &str = &config.db_name;
  { println!( "1) Merging in TypeDB database '{}' ...", db_name );
    merge_nodes_in_typedb (
      db_name,
      driver,
      &instructions ). await ?;
    println!( "   TypeDB merge complete." ); }
  { println!( "2) Merging in filesystem ..." );
    merge_nodes_in_fs (
      instructions.clone (), config.clone () ) ?;
    println!( "   Filesystem merge complete." ); }
  { println!( "3) Merging in Tantivy ..." );
    merge_nodes_in_tantivy (
      &instructions, tantivy_index ) ?;
    println!( "   Tantivy merge complete." ); }
  Ok (( )) }

/// Merges nodes in TypeDB by applying merge SaveInstructions.
async fn merge_nodes_in_typedb (
  db_name      : &str,
  driver       : &TypeDBDriver,
  instructions : &[SaveInstruction],
) -> Result < (), Box<dyn Error> > {

  // Categorize merge instructions
  let (acquiree_text_preservers, updated_acquirers, deleted_acquirees) : (
    Vec<&SaveInstruction>,
    Vec<&SaveInstruction>,
    Vec<&SaveInstruction>
  ) = categorize_merge_instructions(instructions);

  if acquiree_text_preservers.is_empty() {
    return Ok(()); // No merges to process
  }

  // Process each merge
  for i in 0..acquiree_text_preservers.len() {
    let acquiree_text_preserver : &SkgNode = &acquiree_text_preservers[i].0;
    let updated_acquirer : &SkgNode = &updated_acquirers[i].0;
    let acquiree : &SkgNode = &deleted_acquirees[i].0;

    let acquirer_id : &ID = &updated_acquirer.ids[0];
    let acquiree_id : &ID = &acquiree.ids[0];

    // Open transaction for this merge
    let tx : Transaction = driver.transaction(
      db_name, TransactionType::Write).await?;

    // Compute acquirer's final contains for relationship filtering
    let acquirer_final_contains : HashSet<ID> =
      updated_acquirer.contains.iter().cloned().collect();

    // Reroute relationships BEFORE deleting acquiree
    reroute_contains(&tx, acquirer_id, acquiree_id).await?;
    reroute_subscribes(&tx, acquirer_id, acquiree_id).await?;
    reroute_overrides(&tx, acquirer_id, acquiree_id).await?;
    reroute_hides(&tx, acquirer_id, acquiree_id, &acquirer_final_contains).await?;

    // Delete acquiree's extra_ids BEFORE deleting the node
    delete_extra_ids_for_node(&tx, acquiree_id).await?;

    // Delete acquiree node
    tx.query( format!(
      r#"match
           $node isa node, has id "{}";
         delete $node;"#,
      acquiree_id.as_str()
    )).await?;

    // Create the node that saves the acquiree's text
    create_node(acquiree_text_preserver, &tx).await?;

    // Add extra_ids to acquirer (all acquiree IDs become extra_ids)
    // This must happen AFTER deleting the acquiree's extra_ids
    for extra_id_value in &acquiree.ids {
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
        extra_id_value.as_str()
      );
      tx.query(query).await?;
    }

    tx.commit().await?;
  }

  Ok(())
}

/// Merges nodes in filesystem by applying merge SaveInstructions.
fn merge_nodes_in_fs (
  instructions : Vec<SaveInstruction>,
  config       : SkgConfig,
) -> Result < (), Box<dyn Error> > {

  // Categorize instructions
  let (merged_nodes, updated_acquirers, deleted_acquirees) : (
    Vec<&SaveInstruction>,
    Vec<&SaveInstruction>,
    Vec<&SaveInstruction>
  ) = categorize_merge_instructions(&instructions);

  if merged_nodes.is_empty() {
    return Ok(());
  }

  // Process each merge
  for i in 0..merged_nodes.len() {
    let merged_node : &SkgNode = &merged_nodes[i].0;
    let updated_acquirer : &SkgNode = &updated_acquirers[i].0;
    let acquiree : &SkgNode = &deleted_acquirees[i].0;

    // Write MERGED node to disk
    let merged_path : String =
      path_from_pid(&config, merged_node.ids[0].clone());
    write_node(merged_node, &merged_path)?;

    // Compute final relationship fields for acquirer
    let acquirer_final_contains : HashSet<ID> =
      updated_acquirer.contains.iter().cloned().collect();

    let mut acquirer_to_write : SkgNode = updated_acquirer.clone();

    // Combine subscribes_to
    acquirer_to_write.subscribes_to = Some(
      updated_acquirer.subscribes_to.clone().unwrap_or_default()
        .into_iter()
        .chain(acquiree.subscribes_to.clone().unwrap_or_default())
        .collect()
    );

    // Combine hides_from_its_subscriptions (with filtering)
    let mut combined_hides : Vec<ID> = Vec::new();
    for list in [&updated_acquirer.hides_from_its_subscriptions,
                 &acquiree.hides_from_its_subscriptions] {
      if let Some(hides_list) = list {
        for hidden_id in hides_list {
          if !acquirer_final_contains.contains(hidden_id)
             && !combined_hides.contains(hidden_id) {
            combined_hides.push(hidden_id.clone());
          }
        }
      }
    }
    acquirer_to_write.hides_from_its_subscriptions = Some(combined_hides);

    // Combine overrides_view_of
    acquirer_to_write.overrides_view_of = Some(
      updated_acquirer.overrides_view_of.clone().unwrap_or_default()
        .into_iter()
        .chain(acquiree.overrides_view_of.clone().unwrap_or_default())
        .collect()
    );

    // Write updated acquirer to disk
    let acquirer_path : String =
      path_from_pid(&config, acquirer_to_write.ids[0].clone());
    write_node(&acquirer_to_write, &acquirer_path)?;

    // Delete acquiree from disk
    let acquiree_path : String =
      path_from_pid(&config, acquiree.ids[0].clone());
    fs::remove_file(&acquiree_path)?;
  }

  Ok(())
}

/// Merges nodes in Tantivy by applying merge SaveInstructions.
fn merge_nodes_in_tantivy (
  instructions : &[SaveInstruction],
  index        : &tantivy::Index,
) -> Result < (), Box<dyn Error> > {

  // The tantivy module already has a function that handles SaveInstructions!
  // It will:
  // - Delete all IDs from the index (including the acquiree)
  // - Add back non-deleted nodes (MERGED and updated acquirer)
  // - Skip deleted nodes (acquiree)

  use crate::types::TantivyIndex;
  use std::sync::Arc;

  // We need to get the fields from the index, but we don't have the TantivyIndex struct here.
  // Let's use update_index_from_saveinstructions but we need to construct a TantivyIndex.
  // Actually, looking at the signature, we're receiving a tantivy::Index, not a TantivyIndex.
  // We need to get the schema fields.

  // Let me look at the schema - it should have "id" and "title_or_alias" fields
  let schema : tantivy::schema::Schema = index.schema();
  let id_field : tantivy::schema::Field =
    schema.get_field("id")
      .ok_or("Missing 'id' field in Tantivy schema")?;
  let title_or_alias_field : tantivy::schema::Field =
    schema.get_field("title_or_alias")
      .ok_or("Missing 'title_or_alias' field in Tantivy schema")?;

  let tantivy_index : TantivyIndex = TantivyIndex {
    index: Arc::new(index.clone()),
    id_field,
    title_or_alias_field,
  };

  update_index_from_saveinstructions(instructions, &tantivy_index)?;

  Ok(())
}

//
// Helper functions
//

/// Deletes all extra_ids associated with a node.
async fn delete_extra_ids_for_node(
  tx: &Transaction,
  node_id: &ID,
) -> Result<(), Box<dyn Error>> {
  // Query to find all extra_ids associated with the node
  let extra_ids_query : String = format!(
    r#"match
         $node isa node, has id "{}";
         $e isa extra_id;
         $rel isa has_extra_id (node: $node, extra_id: $e);
         $e has id $extra_id_value;
       select $extra_id_value;"#,
    node_id.as_str()
  );

  let answer : QueryAnswer = tx.query(extra_ids_query).await?;
  let mut extra_id_values : Vec<String> = Vec::new();
  let mut rows = answer.into_rows();
  while let Some(row_res) = rows.next().await {
    let row = row_res?;
    if let Some(concept) = row.get("extra_id_value")? {
      let extra_id_value : String =
        extract_payload_from_typedb_string_rep(
          &concept.to_string());
      extra_id_values.push(extra_id_value);
    }
  }

  // Delete each extra_id
  for extra_id_value in extra_id_values {
    let delete_extra_id_query : String = format!(
      r#"match
           $e isa extra_id, has id "{}";
         delete $e;"#,
      extra_id_value
    );
    tx.query(delete_extra_id_query).await?;
  }

  Ok(())
}

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

/// Categorizes merge SaveInstructions into their three components.
/// Returns (merged_nodes, updated_acquirers, deleted_acquirees).
fn categorize_merge_instructions(
  instructions: &[SaveInstruction]
) -> (Vec<&SaveInstruction>, Vec<&SaveInstruction>, Vec<&SaveInstruction>) {
  let mut merged_nodes: Vec<&SaveInstruction> = Vec::new();
  let mut updated_acquirers: Vec<&SaveInstruction> = Vec::new();
  let mut deleted_acquirees: Vec<&SaveInstruction> = Vec::new();

  let mut i: usize = 0;
  while i < instructions.len() {
    let instr: &SaveInstruction = &instructions[i];

    if instr.0.title.starts_with("MERGED: ") {
      // This is a MERGED node
      merged_nodes.push(instr);

      // Next should be updated acquirer
      if i + 1 < instructions.len() {
        updated_acquirers.push(&instructions[i + 1]);
      }

      // Next should be deleted acquiree
      if i + 2 < instructions.len() {
        deleted_acquirees.push(&instructions[i + 2]);
      }

      i += 3;
    } else {
      i += 1;
    }
  }

  (merged_nodes, updated_acquirers, deleted_acquirees)
}
