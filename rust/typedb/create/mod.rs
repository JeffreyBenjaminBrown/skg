pub mod nodes;
pub mod relationships;

pub use nodes::create_all_nodes;
pub use relationships::create_all_relationships;

use crate::types::{FileNode};
use crate::file_io::read_filenode;

use std::error::Error;
use std::{io, fs, path::Path};
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

pub fn read_skg_files
  <P : AsRef<Path> > (
    dir_path : P )
  -> io::Result < Vec<FileNode> >
{ // Reads all relevant files from the path.

  let mut filenodes = Vec::new ();
  let entries : std::fs::ReadDir = // an iterator
    fs::read_dir (dir_path) ?;
  for entry in entries {
    let entry : std::fs::DirEntry = entry ?;
    let path = entry.path () ;
    if ( path.is_file () &&
         path . extension () . map_or (
           false,                // None => no extension found
           |ext| ext == "skg") ) // Some
    { let node = read_filenode (&path) ?;
      filenodes.push (node); }}
  Ok (filenodes) }
