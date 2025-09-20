use crate::file_io::{read_node, path_from_pid};
use crate::typedb::search::pid_from_id;
use crate::types::{ID, SkgConfig, SkgNode};

use std::error::Error;
use std::io;
use typedb_driver::TypeDBDriver;

pub fn newline_to_space ( s: &str ) -> String {
  s.replace ( '\n', " " ) }

pub fn org_bullet ( level: usize ) -> String {
  "*" . repeat ( level.max ( 1 )) }

/// Helper function to get a node's title from its ID.
pub async fn get_node_title (
  driver  : &TypeDBDriver,
  config  : &SkgConfig,
  node_id : &ID,
) -> Result<String, Box<dyn Error>> {

  let path = path_from_pid (
    config,
    pid_from_id (
      & config.db_name, driver, node_id
    ). await ? );
  let node : SkgNode =
    read_node (path) ?;
  if node.title.is_empty () {
    return Err ( Box::new ( io::Error::new (
      io::ErrorKind::InvalidData,
      format! ("SkgNode with ID {} has an empty title",
               node_id ),
    )) ); }
  Ok (node.title) }
