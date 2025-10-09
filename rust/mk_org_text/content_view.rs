use crate::file_io::read_node;
use crate::util::path_from_pid;
use crate::typedb::search::{
  climb_containerward_and_fetch_rootish_context,
  pid_from_id, };
use crate::types::{SkgNode, ID, SkgConfig, OrgNode};
use crate::types::orgnode::default_metadata;
use crate::mk_org_text::util::newline_to_space;
use crate::mk_org_text::orgnode::render_org_node_from_text;

use std::collections::HashSet;
use std::error::Error;
use std::io;
use typedb_driver::TypeDBDriver;

/// Given the id `focus`,
/// identifies its context (`climb_containerward_and_fetch_rootish_context()`),
/// and builds a view from that root,
/// by recursively following the `content` relationship.
pub async fn single_root_view (
  driver  : &TypeDBDriver,
  config  : &SkgConfig,
  focus   : &ID,
) -> Result < String, Box<dyn Error> > {

  let root_id : ID = climb_containerward_and_fetch_rootish_context (
    & config . db_name,
    driver , focus
  ) . await ?;
  let mut visited : HashSet<ID> = HashSet::new();
  let org : String =
    org_from_node_recursive (
      driver, config,
      &root_id, focus, &mut visited, 1
    ) . await ?;
  Ok (org) }

/// Just like 'single_root_view',
/// except it builds a forest rather than a tree.
pub async fn multi_root_view (
  driver  : &TypeDBDriver,
  config  : &SkgConfig,
  focii   : &[ID],
) -> Result < String, Box<dyn Error> > {

  let mut result : String = String::new();
  let mut visited : HashSet<ID> = HashSet::new();
  for focus in focii {
    let root_id : ID =
      climb_containerward_and_fetch_rootish_context (
        & config . db_name,
        driver , focus
      ) . await ?;
    let org : String =
      org_from_node_recursive (
        driver, config,
        &root_id, focus, &mut visited, 1
      ) . await ?;
    result . push_str ( & org ); }
  Ok ( result ) }

/// Recursively render a node and its branches into Org.
/// `level` controls the number of leading `*` on the headline.
pub async fn org_from_node_recursive (
  driver  : &TypeDBDriver,
  config  : &SkgConfig,
  node_id : &ID,
  focus   : &ID,
  visited : &mut HashSet<ID>,
  level   : usize,
) -> Result<String, Box<dyn Error>> {

  let pid : ID =
    pid_from_id ( & config . db_name,
                    driver,
                    node_id,
  ). await ?
    . ok_or_else ( || format! (
      "ID '{}' not found in database", node_id ) ) ?;
  let path : String = path_from_pid (
    &config, pid );
  let node : SkgNode = read_node ( path )?;
  if node.title.is_empty () {
    return Err ( Box::new ( io::Error::new (
      io::ErrorKind::InvalidData,
      format! ( "SkgNode with ID {} has an empty title",
                 node_id ),
    )) ); }
  if visited.contains (node_id) {
    return Ok ( format_repeated_node (
      node_id, level, & node.title )); }
  visited.insert ( node_id.clone () );
  let orgnode2 : OrgNode =
    OrgNode {
      metadata : { let mut md = default_metadata ();
                   md.id = Some ( node_id.clone () );
                   md },
      title : newline_to_space ( &node.title ),
      // Over-cautious, because the title should contain no newlines, but probably cheap.
      body : node.body.clone (), };
  let mut out : String =
    render_org_node_from_text (
      level,
      &orgnode2
    );
  for child_id in &node.contains { // Recurse at next level.
    let child = Box::pin (
      org_from_node_recursive (
        driver, config,
        child_id, focus, visited, level + 1
      )) . await? ;
    out.push_str ( &child ); }
  Ok (out) }

/// When a node is encountered again in the same document:
/// Produce a headline with a `repeated` herald and a short body.
/// Do not recurse into children.
pub fn format_repeated_node (
  node_id : &ID,
  level   : usize,
  title   : &String
) -> String {
  let orgnode2 : OrgNode =
    OrgNode {
      metadata : { let mut md = default_metadata ();
                   md.id = Some ( node_id.clone () );
                   md.repeat = true;
                   md },
      title : newline_to_space ( title ),
      body : Some (
        "Repeated, probably above. Edit there, not here."
          . to_string () ), };
  render_org_node_from_text (
    level,
    &orgnode2 ) }
