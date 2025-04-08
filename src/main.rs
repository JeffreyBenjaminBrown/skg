use std::error::Error;
use std::fs;
use std::path::Path;
use typedb_driver::{
    TypeDBDriver,
    TransactionType,
    Credentials,
    DriverOptions,
};
use std::io;
use futures::StreamExt;

// Import from your crate
use skg::types::{SkgNode, SkgNodeProperty};
use skg::file_io::read_skgnode_from_path;

async fn run_typedb_process() -> Result<(), Box<dyn Error>> {
    // 1. Read all .skg files from tests/typedb/fixtures/
    let fixtures_path = "tests/typedb/fixtures";
    let skg_nodes = read_skg_files(fixtures_path)?;

    println!("Read {} .skg files from {}", skg_nodes.len(), fixtures_path);

    // 2. Connect to TypeDB and create a database called 'skg'
    let credentials = Credentials::new("admin", "password");
    let driver_options = DriverOptions::new(false, None)?;
    let driver = TypeDBDriver::new("127.0.0.1:1729", credentials, driver_options).await?;

    // Check if database exists, if not create it
    let db_name = "skg";
    let databases = driver.databases();
    if !databases.contains(db_name).await? {
        println!("Creating database '{}'...", db_name);
        databases.create(db_name).await?;
    } else {
        println!("Database '{}' already exists", db_name);
    }

    // 3. Create schema and populate database with data
    // Create schema transaction
    let schema_tx = driver.transaction(db_name, TransactionType::Schema).await?;

    // Define the schema
    let schema = r#"
  define
    attribute id, value string;
    attribute path, value string;
    entity node,
        owns id @key,
        owns path @unique,
        plays contains:container,
        plays contains:contained,
        plays comments_on:commenter,
        plays comments_on:commentee,
        plays subscribed_to:subscriber,
        plays subscribed_to:subscribee,
        plays unsubscribed_from:unsubscriber,
        plays unsubscribed_from:unsubscribee,
        plays extra_id:node;
    relation contains,
        relates container,
        relates contained @card(1..1);
    relation comments_on,
        relates commenter @card(1..1),
        relates commentee;
    relation subscribed_to,
        relates subscriber,
        relates subscribee;
    relation unsubscribed_from,
        relates unsubscriber,
        relates unsubscribee;
    relation extra_id,
        relates node,
        relates id @card(1..1);
    "#;

    schema_tx.query(schema).await?;
    schema_tx.commit().await?;
    println!("Schema defined successfully");

    // Create data transaction
    let data_tx = driver.transaction(db_name, TransactionType::Write).await?;

    // Populate data from skg_nodes
    for node in &skg_nodes {
        populate_node(node, &data_tx).await?;
    }

    data_tx.commit().await?;
    println!("Data transaction committed successfully");

    // 4. Query to find the container of node with ID "2"
    let query_tx = driver.transaction(db_name, TransactionType::Read).await?;
    let container_id = find_container_of_node_with_id("2", &query_tx).await?;

    println!("The ID of the node that contains node with ID '2' is: {}", container_id);

    Ok(())
}

fn read_skg_files<P: AsRef<Path>>(dir_path: P) -> io::Result<Vec<SkgNode>> {
    let mut skg_nodes = Vec::new();

    let entries = fs::read_dir(dir_path)?;
    for entry in entries {
        let entry = entry?;
        let path = entry.path();

        if path.is_file() && path.extension().map_or(false, |ext| ext == "skg") {
	    println!("Reading file: {}", path.display());
            let node = read_skgnode_from_path(&path)?;
            skg_nodes.push(node);
        }
    }

    Ok(skg_nodes)
}

async fn populate_node(node: &SkgNode, tx: &typedb_driver::Transaction) -> Result<(), Box<dyn Error>> {
    // First, create the node entity with its primary ID and path
    if node.ids.is_empty() {
        return Err("Node must have at least one ID".into());
    }

    let primary_id = node.ids[0].as_str();
    let path_str = node.path.to_string_lossy().to_string();

    // Create the node entity with primary ID and path attributes
    let insert_node_query = format!(
        r#"insert $n isa node,
            has id "{}",
            has path "{}";"#,
        primary_id,
        path_str
    );

    tx.query(insert_node_query).await?;

    // Handle extra IDs (if any)
    if node.ids.len() > 1 {
        for extra_id in node.ids.iter().skip(1) {
            let extra_id_query = format!(
                r#"match
                    $n isa node, has id "{}";
                insert
                    $r(node: $n, extra_id: $e) isa extra_id;
                    $e isa extra_id, has extra_id "{}";"#,
                primary_id,
                extra_id.as_str()
            );

            tx.query(extra_id_query).await?;
        }
    }

    // Handle contained nodes
    for contained_id in &node.nodes_contained {
        let contains_query = format!(
            r#"match
                $container isa node, has id "{}";
                $contained isa node, has id "{}";
            insert
                $r(container: $container, contained: $contained) isa contains;"#,
            primary_id,
            contained_id.as_str()
        );

        tx.query(contains_query).await?;
    }

    // Handle subscribed nodes
    for subscribed_id in &node.nodes_subscribed {
        let subscribed_query = format!(
            r#"match
                $subscriber isa node, has id "{}";
                $subscribee isa node, has id "{}";
            insert
                $r(subscriber: $subscriber, subscribee: $subscribee) isa subscribed_to;"#,
            primary_id,
            subscribed_id.as_str()
        );

        tx.query(subscribed_query).await?;
    }

    // Handle unsubscribed nodes
    for unsubscribed_id in &node.nodes_unsubscribed {
        let unsubscribed_query = format!(
            r#"match
                $unsubscriber isa node, has id "{}";
                $unsubscribee isa node, has id "{}";
            insert
                $r(unsubscriber: $unsubscriber, unsubscribee: $unsubscribee) isa unsubscribed_from;"#,
            primary_id,
            unsubscribed_id.as_str()
        );

        tx.query(unsubscribed_query).await?;
    }

    // Handle comments_on property
    for property in &node.properties {
        if let SkgNodeProperty::CommentsOn(commented_id) = property {
            let comments_query = format!(
                r#"match
                    $commenter isa node, has id "{}";
                    $commentee isa node, has id "{}";
                insert
                    $r(commenter: $commenter, commentee: $commentee) isa comments_on;"#,
                primary_id,
                commented_id.as_str()
            );

            tx.query(comments_query).await?;
        }
        // NoTantivyIndex is not relevant to TypeDB
    }

    Ok(())
}

async fn find_container_of_node_with_id(target_id: &str, tx: &typedb_driver::Transaction) -> Result<String, Box<dyn Error>> {
    // Query to find the container of the node with ID "2"
    let query = format!(
        r#"match
            $container isa node, has id $container_id;
            $contained isa node, has id "{}";
            $rel(container: $container, contained: $contained) isa contains;
            get $container_id;"#,
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

// Main function with tokio runtime
#[tokio::main]
async fn main() -> Result<(), Box<dyn Error>> {
    run_typedb_process().await
}
