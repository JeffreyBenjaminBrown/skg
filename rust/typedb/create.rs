use std::error::Error;
use std::fs;
use std::io;
use std::path::Path;
use typedb_driver::{
    TypeDBDriver,
    TransactionType,
};

use crate::types::{ID, SkgNode};
use crate::file_io::read_skgnode_from_path;

pub async fn make_db_destroying_earlier_one (
  data_folder : &str,
  db_name : &str,
  driver : &TypeDBDriver
) -> Result<(), Box<dyn Error>> {
  let skg_nodes : Vec<SkgNode> =
    read_skg_files( data_folder )?;
  println!( "{} .skg files were read", skg_nodes.len() );
  // If any of the following needs a transaction, it opens a new one.
  // Thus all nodes are created before any relationships,
  // ensuring that all members of the relationship tp be made exist.
  make_empty_db_destroying_earlier_one (
    db_name, &driver             ) . await?;
  define_schema (
    db_name, &driver             ) . await?;
  create_nodes (
    db_name, &driver, &skg_nodes ) . await?;
  create_all_relationships (
    db_name, &driver, &skg_nodes ) . await?;
  Ok (()) }

pub async fn make_empty_db_destroying_earlier_one (
  db_name : &str,
  driver : &TypeDBDriver
)-> Result<(), Box<dyn Error>> {
  let databases = driver.databases();
  if databases.contains(db_name).await? {
    println!("Deleting existing database '{}'...",
             db_name);
    let database = databases.get(db_name).await?;
    database.delete().await?;
  }
  println!("Creating database '{}'...", db_name);
  databases.create(db_name).await?;
  Ok (()) }

pub async fn define_schema (
  db_name : &str,
  driver : &TypeDBDriver
)-> Result<(), Box<dyn Error>> {
  let tx = driver.transaction(
    db_name, TransactionType::Schema).await?;
  let schema = fs::read_to_string
    ("schema.tql")
    .expect("Failed to read TypeDB schema file");
  println!("Defining schema ...");
  tx.query(schema).await?;
  tx.commit().await?;
  Ok (()) }

pub async fn create_nodes (
  db_name : &str,
  driver : &TypeDBDriver,
  skg_nodes : &Vec<SkgNode>
)-> Result<(), Box<dyn Error>> {
  let tx = driver.transaction(
    db_name, TransactionType::Write).await?;
  println!("Creating nodes ...");
  for node in skg_nodes {
    create_node(node, &tx).await?; }
  tx.commit().await?;
  Ok (()) }

pub async fn create_all_relationships (
  db_name : &str,
  driver : &TypeDBDriver,
  skg_nodes : &Vec<SkgNode>
)-> Result<(), Box<dyn Error>> {
  let tx = driver.transaction(
    db_name, TransactionType::Write).await?;
  println!("Creating relationships ...");
  for node in skg_nodes {
    create_relationships_from_node(node, &tx).await?; }
  tx.commit().await?;
  Ok (()) }

pub fn read_skg_files
  <P: AsRef<Path>>
  (dir_path: P)
   -> io::Result<Vec<SkgNode>> {
    let mut skg_nodes = Vec::new();
    let entries : std::fs::ReadDir = // an iterator
      fs::read_dir(dir_path)? ;
    for entry in entries {
      let entry : std::fs::DirEntry = entry?;
      let path = entry.path();
      if ( path.is_file() &&
           path.extension().map_or(false, |ext| ext == "skg") ) {
        let node = read_skgnode_from_path(&path)?;
        skg_nodes.push(node); } }
    Ok (skg_nodes) }

pub async fn create_node(
  node: &SkgNode,
  tx: &typedb_driver::Transaction
) -> Result<(), Box<dyn Error>> {
  let path_str : String =
    node.path.to_string_lossy().to_string();
  if node.ids.is_empty() {
    return Err( format!( "Node {} has no IDs.",
                          path_str )
                . into() ); }
  let primary_id = node.ids[0].as_str();
  let insert_node_query = format!(
    r#"insert $n isa node,
            has id "{}",
            has path "{}";"#,
    primary_id,
    path_str );
  tx.query(insert_node_query).await?;
  insert_extra_ids ( &node, tx ) . await?; // PITFALL: This creates has_extra_id relationships, so you might expect it to belong in `create_relationships_from_node`. But it's important that these relationships be created before any others, because the others might refer to nodes via their `extra_id`s. They are basically optional attributes of a node; they have no meaning beyond being another way to refer to a node.
  Ok (()) }

pub async fn create_relationships_from_node(
  node: &SkgNode,
  tx: &typedb_driver::Transaction
) -> Result<(), Box<dyn Error>> {
  let primary_id = node.ids[0].as_str();
  insert_comment_rel ( &node, tx ) . await?;
  insert_from_list( primary_id,
                    &node.nodes_contained,
                    "contains",
                    "container",
                    "contained",
                    tx ).await?;

  insert_from_list(
    primary_id,
    &node.links.iter ()
      . map ( |link| ID::from(link.id.clone()) )
      . collect::<Vec<ID>>(),
    "links_to",
    "source",
    "dest",
    tx
  ).await?;

  insert_from_list( primary_id,
                    &node.nodes_subscribed,
                    "subscribes",
                    "subscriber",
                    "subscribee",
                    tx ).await?;
  insert_from_list( primary_id,
                    &node.nodes_unsubscribed,
                    "unsubscribes",
                    "unsubscriber",
                    "unsubscribee",
                    tx ).await?;
  Ok (()) }

pub async fn insert_extra_ids (
  node : &SkgNode,
  tx: &typedb_driver::Transaction
) -> Result<(), Box<dyn Error>> {
  if node.ids.len() > 1 {
    let primary_id = node.ids[0].as_str();
    let extra_ids: Vec<&ID> =
      node.ids.iter().skip(1).collect();
    for extra_id in extra_ids {
      tx.query(
        format!( r#"
                    match
                        $n isa node, has id "{}";
                    insert
                        $e isa extra_id, has id "{}";
                        $r isa has_extra_id
                           (node: $n, extra_id: $e);"#,
                    primary_id,
                    extra_id.as_str() ) )
        . await?; } }
  Ok (()) }

pub async fn insert_comment_rel(
  node: &SkgNode,
  tx: &typedb_driver::Transaction
) -> Result<(), Box<dyn Error>> {
  let primary_id = node.ids[0].as_str();
    if let Some(commented_id) = &node.comments_on {
      tx.query (
        format!( r#"
                    match
                        $commenter isa node, has id "{}";
                        {{ $commentee isa node, has id "{}"; }} or
                        {{ $commentee isa node;
                           $e isa extra_id, has id "{}";
                           $rel isa has_extra_id (node: $commentee,
                                                  extra_id: $e); }} ;
                    insert
                        $r isa comments_on
                          (commenter: $commenter,
                           commentee: $commentee);"#,
                    primary_id,
                    commented_id.as_str(),
                    commented_id.as_str()
        ) ) . await?; }
  Ok (()) }

pub async fn insert_from_list(
  primary_id: &str,
  id_list: &Vec<ID>,   // This would be node.nodes_contained, etc.
  relation_name: &str, // "contains", "subscribes", etc.
  from_role: &str,     // "container", "subscriber", etc.
  to_role: &str,       // "contained", "subscribee", etc.
  tx: &typedb_driver::Transaction
) -> Result<(), Box<dyn Error>> {
  for target_id in id_list {
    tx.query (
      format! ( r#"
                match
                  $from isa node, has id "{}";
                  {{ $to isa node, has id "{}"; }} or
                  {{ $to isa node;
                     $e isa extra_id, has id "{}";
                     $rel isa has_extra_id (node: $to,
                                            extra_id: $e); }};
                insert
                  $r isa {}
                    ({}: $from,
                     {}: $to);"#,
                primary_id,
                target_id.as_str(),
                target_id.as_str(),
                relation_name,
                from_role,
                to_role ) ).await?; }
  Ok (()) }
