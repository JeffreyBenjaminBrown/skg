use crate::file_io::read_filenode;
use crate::typedb::search::{
  find_rootish_container,
  filepath_from_id_via_typedb, };
use crate::types::{ID, FileNode};

use std::collections::HashSet;
use std::error::Error;
use std::io;
use typedb_driver::TypeDBDriver;
use std::fmt::Write as _; // for the write! macro


/// Given the id `focus`,
/// identifies its rootish container (`find_rootish_container()`),
/// and builds a view from that `root`
/// by recursively following the `content` relationship.
pub async fn single_root_content_view (
  db_name : &str,
  driver  : &TypeDBDriver,
  focus   : &ID,
) -> Result < String, Box<dyn Error> > {

  let root_id : ID = find_rootish_container (
    db_name, driver, focus ) . await ?;
  let mut visited : HashSet<ID> = HashSet::new();
  let org : String =
    org_from_node_recursive (
      db_name, driver, &root_id, focus, &mut visited, 1
    ) . await ?;
  Ok (org) }

/// Recursively render a node and its branches into Org.
/// `level` controls the number of leading `*` on the heading line.
async fn org_from_node_recursive (
  db_name : &str,
  driver  : &TypeDBDriver,
  node_id : &ID,
  focus   : &ID,
  visited : &mut HashSet<ID>,
  level   : usize,
) -> Result<String, Box<dyn Error>> {
  let path : String = filepath_from_id_via_typedb (
      db_name, driver, node_id ). await ?;
  let filenode : FileNode = read_filenode ( path )?;
  if filenode.title.is_empty () {
    return Err ( Box::new ( io::Error::new (
      io::ErrorKind::InvalidData,
      format! ( "Node with ID {} has an empty title",
                 node_id ),
    )) ); }
  if visited.contains (node_id) {
    return Ok ( format_repeated_node (
      node_id, level, & filenode.title )); }
  visited.insert ( node_id.clone () );
  let mut out = String::new();
  write! ( &mut out,
            "{} <<id:{}>> {}\n",
            org_bullet (level),
            node_id,
            newline_to_space ( // Over-cautious, because the title should contain no newlines, but probably cheap.
              & filenode.title )) ?;
  if let Some (text) = & filenode.body {
    // Ensure a newline separates the body from the next heading.
    out.push_str ( text );
    if !text.ends_with ( '\n' ) {
      out.push('\n'); }}
  for child_id in &filenode.contains { // Recurse at next level.
    let child = Box::pin (
      org_from_node_recursive (
        db_name, driver, child_id, focus, visited, level + 1
      )) . await? ;
    out.push_str ( &child ); }
  Ok (out) }

/// When a node is encountered again in the same document:
/// Produce a heading with a `repeated` herald and a short body.
/// Do not recurse into children.
fn format_repeated_node (
  node_id: &ID,
  level: usize,
  title: &String
) -> String {

  let mut s = String::new ();
  write! ( &mut s,
            "{} <<id:{},repeated>> {}\nRepeated, probably above. Edit there, not here.\n",
            org_bullet (level),
            node_id,
            newline_to_space ( title ))
    . unwrap ();
  s }

fn org_bullet ( level: usize ) -> String {
  "*" . repeat ( level.max ( 1 )) }

fn newline_to_space ( s: &str ) -> String {
  s.replace ( '\n', " " ) }
