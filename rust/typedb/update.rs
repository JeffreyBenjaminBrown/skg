use std::error::Error;
use typedb_driver::TypeDBDriver;

use crate::typedb::nodes::create_only_nodes_with_no_ids_present;
use crate::typedb::relationships::create_all_relationships;
use crate::typedb::relationships::delete_out_links;
use crate::types::{FileNode, ID};

/// Update the DB from a batch of `FileNode`s:
/// 1) Create only nodes whose IDs are not present, via
///      create_only_nodes_with_no_ids_present
/// 2) Delete all outbound `contains` from those nodes, via
///      delete_out_links
///    PITFALL: Only the primary ID from each FileNode is used.
/// 3) Recreate all relationships for those nodes, via
///      create_all_relationships
pub async fn update_nodes_and_relationships (
    db_name: &str,
    driver: &TypeDBDriver,
    filenodes: &Vec<FileNode>,
) -> Result<(), Box<dyn Error>> {

  create_only_nodes_with_no_ids_present (
    db_name, driver, filenodes ). await ?;
  let primary_ids : Vec<ID> =
    filenodes . iter ()
    . filter_map ( |n|
                    n . ids
                    . get(0) // TODO: Consider other IDs too?
                    . cloned() )
    . collect ();
  if !primary_ids.is_empty () {
    delete_out_links (
      db_name, driver,
      &primary_ids,
      "contains",
      "container" ). await ?; }
  create_all_relationships (
    db_name, driver, filenodes ). await ?;
  Ok (( )) }
