use crate::file_io::{read_node, path_from_pid};
use crate::typedb::search::{
  containerward_path,
  pid_from_id, };
use crate::types::{ID, SkgConfig, SkgNode};
use crate::render::util::{newline_to_space, org_bullet, get_node_title};

use std::collections::HashSet;
use std::error::Error;
use typedb_driver::TypeDBDriver;
use std::fmt::Write as _; // for the write! macro

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
    let metadata: String = {
      let mut parts = vec![format!("id:{}", node_id)];
      if cycle_node.as_ref() == Some(node_id) {
        // The cycle node needs extra metadata.
        parts.push("cycle".to_string());
      }
      if i > 0 { // The 'containsOrgParent' type.
        // All nodes except the terminus (first in path) get this type.
        // PITFALL: If there is a second appearance of the terminus,
        // later in the containerward path,
        // that appearance *should* (and does) get this type.
        parts.push("type:containsOrgParent".to_string()); }
      parts.join(",") };
    write! (
      & mut result_acc,
      "{} <skg<{}>> {}\n",
      org_bullet (current_level),
      metadata,
      newline_to_space (& node_title) )?;
    if i == 0 {
      // If it exists, render the body
      // of *only* the terminus (first in path) node.
      let path = path_from_pid (
        config,
        pid_from_id (
          & config.db_name, driver, node_id
        ). await ? );
      let terminus_node : SkgNode = read_node (path) ?;
      if let Some (ref body) = terminus_node.body {
        result_acc.push_str(body);
        if !body.ends_with('\n') {
          result_acc.push('\n'); }} }}
  let branch_or_cycle_level : usize = // The last level, if there are branches and/or a cycle. Otherwise, one more than the last level.
    terminus_level + path.len();
  if ! branches.is_empty () {
    // There are branches, one of which might be a cycle.
    for branch_id in & branches {
      let node_title: String = get_node_title (
        driver, config, branch_id ). await ?;
      let metadata: String = {
        let mut parts = vec![format!("id:{}", branch_id)];
        if cycle_node.as_ref() == Some (branch_id) {
          parts.push("cycle".to_string());
        }
        parts.push("type:containsOrgParent".to_string());
        parts.join(",")
      };
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
    let metadata : String = {
      let mut parts = vec![
        format!("id:{}", cycle_id), "cycle".to_string() ];
      parts.push("type:containsOrgParent".to_string());
      parts.join(",")
    };
    write! (
      &mut result_acc,
      "{} <skg<{}>> {}\n",
      org_bullet (branch_or_cycle_level),
      metadata,
      newline_to_space (&node_title)
    ) ?; }
  Ok (result_acc) }
