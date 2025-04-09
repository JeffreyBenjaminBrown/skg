// PITFALL: Deletes any existing TypeDB database named `skg-test`,

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

use skg::types::{SkgNode};
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
    print_all_contains_relationships(
	db_name, &driver             ) . await?;
    let container_id = find_container_of_node_with_id(
	db_name, &driver, "2"        ) . await?;

    println!("The ID of the node that contains node with ID '2' is: {}", container_id);
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
    let primary_id = // create_node() ensures this exists
	node.ids[0].as_str();
    for contained_id in &node.nodes_contained {
        tx.query(
	    format!( r#"
              match
                $container isa node, has id "{}";
                $contained isa node, has id "{}";
              insert
                $r isa contains
                  (container: $container,
                   contained: $contained);"#,
            primary_id,
            contained_id.as_str() )
	).await?; }
    // TODO : Handle extra IDs, subscribed nodes, unsubscribed nodes, and the comments_on property
    Ok(()) }

async fn print_all_contains_relationships (
    db_name : &str,
    driver : &TypeDBDriver
) -> Result<(), Box<dyn Error>> {
    let tx = driver.transaction(
	db_name, TransactionType::Read).await?;
    let query = r#"match
        $container isa node, has id $container_id;
        $contained isa node, has id $contained_id;
        $rel isa contains (container: $container, contained: $contained);
        select $container_id, $contained_id;"#;
    let query_answer = tx.query(query).await?;
    let mut stream = query_answer.into_rows();
    println!("All 'contains' relationships in the database:");
    while let Some(row_result) = stream.next().await {
        let row = row_result?;
        // Use String instead of &str to avoid the lifetime issues
        let container_id = match row.get("container_id")? {
            Some(c) => c.to_string(),
            None => "unknown".to_string(),
        };
        let contained_id = match row.get("contained_id")? {
            Some(c) => c.to_string(),
            None => "unknown".to_string(),
        };
        println!("  Node '{}' contains node '{}'", container_id, contained_id);
    }
    println!();
    Ok(())
}

async fn find_container_of_node_with_id(
    db_name : &str,
    driver : &TypeDBDriver,
    target_id: &str
) -> Result<String, Box<dyn Error>> {
    let tx = driver.transaction(
	db_name, TransactionType::Read).await?;
    let query = format!(
        r#"match
            $container isa node, has id $container_id;
            $contained isa node, has id "{}";
            $rel isa contains (container: $container,
                               contained: $contained);
            select $container_id;"#,
        target_id
    );

    let query_answer = tx.query(query).await?;
    let mut stream = query_answer.into_rows();

    if let Some(row_result) = stream.next().await {
        let row = row_result?;
        if let Some(concept) = row.get("container_id")? {
            return Ok(concept.to_string());
        }
    }

    Err(format!("No container found for node with ID '{}'", target_id).into())
}
