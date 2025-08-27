pub mod nodes;
pub mod relationships;

pub use nodes::create_all_nodes;
pub use relationships::create_all_relationships;

use crate::types::{FileNode};
use crate::file_io::read_skg_files;

use std::error::Error;
use std::fs;
use typedb_driver::{
  Transaction,
  TransactionType,
  TypeDBDriver,
};

pub async fn overwrite_and_populate_new_db (
  // Reads files.
  // Destroys earlier db, if any.
  // Makes new db.
  // Loads files into it.
  data_folder : &str,
  db_name     : &str,
  driver      : &TypeDBDriver
) -> Result < (), Box<dyn Error> > {

  let filenodes : Vec <FileNode> =
    read_skg_files ( data_folder ) ?;
  println! ( "{} .skg files were read",
             filenodes.len () );
  // If any of the following needs a transaction, it opens a new one.
  // Thus all nodes are created before any relationships,
  // ensuring that all members of the relationship to be made exist.
  overwrite_new_empty_db (
    db_name, &driver             ) . await?;
  define_schema (
    db_name, &driver             ) . await?;
  create_all_nodes (
    db_name, &driver, &filenodes ) . await?;
  create_all_relationships (
    db_name, &driver, &filenodes ) . await?;
  Ok (()) }

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
