use crate::typedb::nodes::create_all_nodes;
use crate::typedb::relationships::create_all_relationships;
use crate::types::{Node, SkgConfig};
use crate::file_io::read_skg_files;

use futures::executor::block_on;
use std::error::Error;
use std::fs;
use std::sync::Arc;
use typedb_driver::{
  Credentials,
  DriverOptions,
  Transaction,
  TransactionType,
  TypeDBDriver,
};


/// A helper function for tests.
pub async fn populate_test_db_from_fixtures (
  data_folder: &str,
  db_name: &str,
  driver: &TypeDBDriver
) -> Result<(), Box<dyn Error>> {
  let nodes: Vec<Node> =
    read_skg_files(data_folder)?;
  overwrite_new_empty_db (
    db_name, driver ). await ?;
  define_schema (
    db_name, driver ). await?;
  create_all_nodes (
    db_name, driver, &nodes ). await ?;
  create_all_relationships (
    db_name, driver, &nodes ). await ?;
  Ok (( )) }

pub fn initialize_typedb_from_nodes (
  config : & SkgConfig,
  nodes: &[Node],
) -> Arc<TypeDBDriver> {
  // Connects to the TypeDB server,
  // then populates it with the provided Nodes.

  println!("Initializing TypeDB database...");
  let driver: TypeDBDriver = block_on ( async {
    TypeDBDriver::new(
      "127.0.0.1:1729",
      Credentials::new("admin", "password"),
      DriverOptions::new(false, None).unwrap() )
      .await
      .unwrap_or_else(|e| {
        eprintln!("Error connecting to TypeDB: {}", e);
        std::process::exit(1);
      } )
  } );

  block_on ( async {
    // Recreate the database from scratch
    if let Err (e) = overwrite_new_empty_db (
      & config . db_name,
      & driver,
    ) . await {
      eprintln! ( "Failed to create empty database: {}", e );
      std::process::exit(1);
    }

    if let Err (e) = define_schema (
      & config . db_name,
      & driver,
    ) . await {
      eprintln! ( "Failed to define schema: {}", e );
      std::process::exit(1);
    }

    if let Err (e) = create_all_nodes (
      & config . db_name,
      & driver,
      nodes,
    ) . await {
      eprintln! ( "Failed to create nodes: {}", e );
      std::process::exit(1);
    }

    if let Err (e) = create_all_relationships (
      & config . db_name,
      & driver,
      nodes,
    ) . await {
      eprintln! ( "Failed to create relationships: {}", e );
      std::process::exit(1);
    }
  } );
  println!("TypeDB database initialized successfully.");
  Arc::new( driver ) }

async fn overwrite_new_empty_db (
  // Destroys the db named `db_name` if it exists,
  // then makes a new, empty one.
  db_name : &str,
  driver  : &TypeDBDriver
) -> Result < (), Box<dyn Error> > {

  let databases = driver.databases ();
  if databases.contains (db_name) . await ? {
    println! ( "Deleting existing database '{}'...",
                db_name );
    let database = databases.get (db_name) . await ?;
    database . delete () . await ?; }
  println! ( "Creating empty database '{}'...", db_name );
  databases.create (db_name) . await ?;
  Ok (()) }

pub async fn define_schema (
  db_name : &str,
  driver  : &TypeDBDriver
)-> Result < (), Box<dyn Error> > {

  let tx : Transaction =
    driver.transaction ( db_name,
                         TransactionType::Schema )
    . await ?;
  let schema : String = fs::read_to_string
    ("schema.tql")
    . expect ( "Failed to read TypeDB schema file" );
  println! ( "Defining schema ..." );
  tx.query (schema) . await ?;
  tx.commit () . await ?;
  Ok (()) }
