// TODO | PITFALL:
// Deletes any existing TypeDB database named `skg-test`,

use futures::StreamExt;
use std::error::Error;
use std::fs;
use std::io;
use std::path::Path;
use typedb_driver::{
    TypeDBDriver,
    TransactionType,
    Credentials,
    DriverOptions,
};

use skg::types::{ID, SkgNode, SkgNodeProperty};
use skg::file_io::read_skgnode_from_path;

#[tokio::main]
async fn main() -> Result<(), Box<dyn Error>> {
    do_typedb().await
}

async fn do_typedb() -> Result<(), Box<dyn Error>> {
    let skg_nodes : Vec<SkgNode> =
	read_skg_files("tests/typedb/fixtures")?;
    println!( "Done: Read {} .skg files", skg_nodes.len() );
    let driver = TypeDBDriver::new(
	"127.0.0.1:1729",
	Credentials::new("admin", "password"),
	DriverOptions::new(false, None)?
    ).await?;
    let db_name = "skg-test";

    // If any of the following needs a transaction, it opens a new one.
    // Thus all nodes are created before any relationships,
    // ensuring that all members of the relationship tp be made exist.
    make_fresh_db_destroying_earlier_one (
	db_name, &driver             ) . await?;
    define_schema (
	db_name, &driver             ) . await?;
    create_nodes (
	db_name, &driver, &skg_nodes ) . await?;
    create_all_relationships (
	db_name, &driver, &skg_nodes ) . await?;

    print_all_of_some_binary_rel (
	db_name,
	&driver,
	r#" match
          $container isa node, has id $container_id;
          $contained isa node, has id $contained_id;
          $rel isa contains (container: $container,
                             contained: $contained);
          select $container_id, $contained_id;"#,
	"contains",
	"container_id",
	"contained_id" ).await?;

    print_all_of_some_binary_rel (
	db_name,
	&driver,
	r#" match
          $subscriber isa node, has id $from;
          $subscribee isa node, has id $to;
          $rel isa subscribed_to (subscriber: $subscriber,
                                  subscribee: $subscribee);
          select $from, $to;"#,
	"subscribed_to",
	"from",
	"to" ).await?;

    print_all_of_some_binary_rel (
	db_name,
	&driver,
	r#" match
          $unsubscriber isa node, has id $from;
          $unsubscribee isa node, has id $to;
          $rel isa unsubscribed_from (unsubscriber: $unsubscriber,
                                      unsubscribee: $unsubscribee);
          select $from, $to;"#,
	"unsubscribed_to",
	"from",
	"to" ).await?;

    println! (
	"The node containing the node with ID '2' has ID: {}",
	find_container_of_node_with_id (
	    db_name, &driver, "2" ) . await? );
    Ok(()) }

async fn make_fresh_db_destroying_earlier_one (
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

async fn define_schema (
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

async fn create_nodes (
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

async fn create_all_relationships (
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

fn read_skg_files <P: AsRef<Path>> (dir_path: P)
				    -> io::Result<Vec<SkgNode>> {
    let mut skg_nodes = Vec::new();
    let entries : std::fs::ReadDir = // an iterator
	fs::read_dir(dir_path)? ;
    for entry in entries
    { let entry : std::fs::DirEntry = entry?;
      let path = entry.path();
      if ( path.is_file() &&
	   path.extension().map_or(false, |ext| ext == "skg") )
      { println!("Reading file: {}", path.display());
	let node = read_skgnode_from_path(&path)?;
	skg_nodes.push(node); } }
    Ok (skg_nodes) }

async fn create_node(
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
    Ok (()) }

async fn create_relationships_from_node(
    node: &SkgNode,
    tx: &typedb_driver::Transaction
) -> Result<(), Box<dyn Error>> {
    let primary_id = node.ids[0].as_str();
    insert_extra_ids   ( &node, tx ) . await?;
    insert_comment_rel ( &node, tx ) . await?;
    insert_from_list( primary_id,
		      &node.nodes_contained,
		      "contains",
		      "container",
		      "contained",
		      tx ).await?;
    insert_from_list( primary_id,
		      &node.nodes_subscribed,
		      "subscribed_to",
		      "subscriber",
		      "subscribee",
		      tx ).await?;
    insert_from_list( primary_id,
		      &node.nodes_unsubscribed,
		      "unsubscribed_from",
		      "unsubscriber",
		      "unsubscribee",
		      tx ).await?;
    Ok (()) }

async fn insert_comment_rel(
    node: &SkgNode,
    tx: &typedb_driver::Transaction
) -> Result<(), Box<dyn Error>> {
    let primary_id = node.ids[0].as_str();
    for property in &node.properties {
	// TODO | PITFALL: This is inefficient.
	// Better to have the file store a properties list,
	// but the SkgNode represent each as a standalone field,
	// usually empty.
        if let SkgNodeProperty::CommentsOn(commented_id) = property {
	    tx.query (
		format!( r#"
                    match
                        $commenter isa node, has id "{}";
                        $commentee isa node, has id "{}";
                    insert
                        $r isa comments_on
                          (commenter: $commenter,
                           commentee: $commentee);"#,
                    primary_id,
                    commented_id.as_str()
		) ) . await?; } }
    Ok (()) }

async fn insert_extra_ids (
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
                        $r isa extra_id (node: $n, id: $e);
                        $e isa id "{}";"#,
		    primary_id,
		    extra_id.as_str() ) )
		. await?; } }
    Ok (()) }

async fn insert_from_list(
    primary_id: &str,
    id_list: &Vec<ID>,   // This would be node.nodes_contained, etc.
    relation_name: &str, // "contains", "subscribed_to", etc.
    from_role: &str,     // "container", "subscriber", etc.
    to_role: &str,       // "contained", "subscribee", etc.
    tx: &typedb_driver::Transaction
) -> Result<(), Box<dyn Error>> {
    for target_id in id_list {
        tx.query (
	    format! ( r#"
                match
                  $from isa node, has id "{}";
                  $to isa node, has id "{}";
                insert
                  $r isa {}
                    ({}: $from,
                     {}: $to);"#,
                primary_id,
                target_id.as_str(),
                relation_name,
                from_role,
                to_role ) ).await?; }
    Ok (()) }

async fn print_all_of_some_binary_rel (
    db_name: &str,
    driver: &TypeDBDriver,
    query: &str,
    rel_name: &str,
    member1_variable: &str, // PITFALL: Must correspond to `query`.
    member2_variable: &str, // PITFALL: Must correspond to `query`.
) -> Result<(), Box<dyn Error>> {
    let tx = driver.transaction(
        db_name, TransactionType::Read).await?;
    let answer = tx.query(query).await?;
    let mut stream = answer.into_rows();
    println!( "All '{}' relationships in the database:",
	       rel_name);
    while let Some(row_result) = stream.next().await {
        let row = row_result?;
        let id1 = match row.get(member1_variable)? {
            Some(c) => c.to_string(),
            None => "unknown".to_string()
        };
        let id2 = match row.get(member2_variable)? {
            Some(c) => c.to_string(),
            None => "unknown".to_string()
        };
        println!("  Node '{}' {} node '{}'", id1, rel_name, id2); }
    println! ();
    Ok (()) }

async fn find_container_of_node_with_id (
    db_name : &str,
    driver : &TypeDBDriver,
    target_id: &str
) -> Result<String, Box<dyn Error>> {
    let tx = driver.transaction(
	db_name, TransactionType::Read).await?;
    let answer = tx.query(
	format!( r#" match
                       $container isa node, has id $container_id;
                       $contained isa node, has id "{}";
                       $rel isa contains (container: $container,
                                          contained: $contained);
                       select $container_id;"#,
		     target_id ) ) . await?;
    let mut stream = answer.into_rows();
    if let Some(row_result) = stream.next().await {
        let row = row_result?;
        if let Some(concept) = row.get("container_id")? {
            return Ok(concept.to_string()); } }
    Err(format!("No container found for node with ID '{}'",
		target_id).into()) }
