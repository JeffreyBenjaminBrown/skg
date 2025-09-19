use crate::file_io::{read_node,path_from_pid};
use crate::typedb::search::{
  climb_containerward_and_fetch_rootish_context,
  containerward_path,
  pid_from_id, };
use crate::types::{MetadataItem, OrgNodeType};
use crate::types::{SkgNode, ID, SkgConfig};
use crate::render::util::{newline_to_space, org_bullet};

use std::collections::HashSet;
use std::error::Error;
use std::io;
use typedb_driver::TypeDBDriver;
use std::fmt::Write as _; // for the write! macro

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

  let path : String = path_from_pid (
    &config,
    pid_from_id ( & config . db_name,
                  driver,
                  node_id,
    ). await ? );
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
  let mut out = String::new();
  write! ( &mut out,
            "{} <skg<id:{}>> {}\n",
            org_bullet (level),
            node_id,
            newline_to_space ( // Over-cautious, because the title should contain no newlines, but probably cheap.
              & node.title )) ?;
  if let Some (text) = & node.body {
    // Ensure a newline separates the body from the next headline.
    out.push_str ( text );
    if !text.ends_with ( '\n' ) {
      out.push('\n'); }}
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
  node_id: &ID,
  level: usize,
  title: &String
) -> String {

  let mut s = String::new ();
  write! ( &mut s,
            "{} <skg<id:{},repeated>> {}\nRepeated, probably above. Edit there, not here.\n",
            org_bullet (level),
            node_id,
            newline_to_space ( title ))
    . unwrap ();
  s }

pub fn aliases_to_org (
  aliases : Vec<String>,
  level   : usize,
) -> String {
  let header_level : usize = level + 1;
  let alias_level  : usize = level + 2;
  let aliases_metadata = MetadataItem::Type(
    OrgNodeType::Aliases);
  let mut result : String =
    format! ( "{} <skg<{}>>\n",
              "*".repeat ( header_level ),
              aliases_metadata );
  for alias in aliases {
    result.push_str (
      & format! ( "{} {}",
                  "*".repeat ( alias_level ),
                  alias )) ;
    result.push ( '\n' ); }
  result }

/// Unlike single-root content view, which generates an entire buffer,
/// this generates some new text that gets inserted into the buffer.
/// It is intended to replace the headline (and body if present)
/// from which it is called, but not that headline's children.
/// That's because the replacement might include a 'cycle' tag,
/// whereas the original probably did not have one.
pub async fn containerward_org_view (
  driver         : &TypeDBDriver,
  config         : &SkgConfig,
  terminus       : &ID,
  terminus_level : usize,
) -> Result<String, Box<dyn Error>> {

  let (path, cycle_node, branches)
    : (Vec<ID>, Option<ID>, HashSet<ID>)
    = containerward_path (
      &config.db_name,
      driver,
      terminus ). await ?;
  let mut result_acc : String = String::new();
  for (i, node_id) in path.iter().enumerate() { // Render the path.
    let current_level = terminus_level + i;
    let node_title: String = get_node_title (
      // PITFALL This might look inefficient, because didn't containerward_path already fetch the node? No, it didn't. It queried TypeDB for connectivity, without reading the full node from disk.
      driver, config, node_id ). await ?;
    let metadata: String =
      if cycle_node.as_ref() == Some(node_id) {
        // This one is the cycle node.
        format!("id:{},cycle", node_id)
      } else { format!("id:{}", node_id) };
    write! (
      &mut result_acc,
      "{} <skg<{}>> {}\n",
      org_bullet (current_level),
      metadata,
      newline_to_space (& node_title) )?;
    if i == 0 {
      // If it exists, render the body
      // of *only* the terminus node (first in path).
      let terminus_node =
        get_full_node ( driver, config, node_id ). await ?;
      if let Some (ref body) = terminus_node.body {
        result_acc.push_str(body);
        if !body.ends_with('\n') {
          result_acc.push('\n'); }} }}
  let branch_or_cycle_level : usize = // The last level, if there are branches or a cycle. Otherwise, one more than the last level.
    terminus_level + path.len();
  if ! branches.is_empty () {
    // There are branches, one of which might be a cycle.
    for branch_id in & branches {
      let node_title: String = get_node_title (
        driver, config, branch_id ). await ?;
      let metadata: String =
        if cycle_node.as_ref() == Some (branch_id) {
          format! ("id:{},cycle", branch_id)
        } else { format!("id:{}", branch_id) };
      write! (
        &mut result_acc,
        "{} <skg<{}>> {}\n",
        org_bullet (branch_or_cycle_level),
        metadata,
        newline_to_space (&node_title)
      )?; }}
  else if let Some (ref cycle_id) = cycle_node {
    // There's a cycle but no branches.
    let node_title: String = get_node_title (
      driver, config, cycle_id ). await ?;
    let metadata : String =
      format! ("id:{},cycle", cycle_id);
    write! (
      &mut result_acc,
      "{} <skg<{}>> {}\n",
      org_bullet (branch_or_cycle_level),
      metadata,
      newline_to_space (&node_title)
    ) ?; }
  Ok (result_acc) }

/// Helper function to get a node's title from its ID.
async fn get_node_title (
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

/// Helper function to get a full node from its ID
async fn get_full_node (
  driver  : &TypeDBDriver,
  config  : &SkgConfig,
  node_id : &ID,
) -> Result<SkgNode, Box<dyn Error>> {

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
  Ok (node) }
