use crate::file_io::read_node_from_id;
use crate::render::util::newline_to_space;
use crate::render::orgnode::render_org_node_from_text;
use crate::typedb::search::path_containerward_to_end_cycle_and_or_branches;
use crate::types::{ID, SkgConfig, OrgNode, OrgnodeMetadata, RelToOrgParent};

use std::collections::HashSet;
use std::error::Error;
use typedb_driver::TypeDBDriver;

/// Unlike single-root content view, which generates an entire buffer,
/// this generates some new text that gets inserted into the buffer.
/// It is intended to replace the headline (and body if present)
/// from which it is called, but not that headline's children.
/// That's because the replacement might include a 'cycle' tag,
/// whereas the original probably did not have one.
pub async fn containerward_org_view (
  driver         : &TypeDBDriver,
  config         : &SkgConfig,
  terminus       : &ID, // you could think of it as the origin, but everything else contains it, so I thought terminus was clearer
  terminus_level : usize,
) -> Result<String, Box<dyn Error>> {

  let (path, cycle_node, branches)
    : (Vec<ID>, Option<ID>, HashSet<ID>)
    = path_containerward_to_end_cycle_and_or_branches (
      &config.db_name,
      driver,
      terminus ). await ?;
  let mut result_acc = String::new();

  result_acc.push_str(
    & render_linear_portion_of_path(
      config, driver, &path, &cycle_node, terminus_level
    ). await ?);
  let branch_or_cycle_level = terminus_level + path.len();
  if ! branches.is_empty() {
    // There are branches, one of which might be a cycle.
    result_acc.push_str (
      & render_branches (
        config, driver, &branches, &cycle_node, branch_or_cycle_level
      ). await ?);
  } else if let Some(ref cycle_id) = cycle_node {
    // There's a cycle and no branches.
    result_acc.push_str (
      & render_terminating_cycle_when_no_branches (
        config, driver, cycle_id, &cycle_node, branch_or_cycle_level
      ). await ?); }
  Ok (result_acc) }

async fn render_linear_portion_of_path (
  config: &SkgConfig,
  driver: &TypeDBDriver,
  path: &[ID],
  cycle_node: &Option<ID>,
  terminus_level: usize,
) -> Result<String, Box<dyn Error>> {

  if path.is_empty() {
    return Err(Box::new(std::io::Error::new(
      std::io::ErrorKind::InvalidInput,
      "render_linear_portion_of_path called with empty path",
    )) ); }
  let mut result = String::new();
  for (i, node_id) in path.iter().enumerate() {
    let node = read_node_from_id (
      config, driver, node_id ). await ?;
    let orgnode2 : OrgNode =
      OrgNode {
        metadata : metadata_for_element_of_path (
          node_id, cycle_node, i == 0 ),
        title : newline_to_space ( & node.title ),
        body :
          // Render body only for origin of path.
          if i == 0 { node.body.clone () }
          else { None },
      };
    result.push_str (
      & render_org_node_from_text (
        terminus_level + i,
        &orgnode2 )); }
  Ok (result) }

async fn render_branches (
  config: &SkgConfig,
  driver: &TypeDBDriver,
  branches: &HashSet<ID>,
  cycle_node: &Option<ID>,
  branch_level: usize,
) -> Result<String, Box<dyn Error>> {
  let mut result = String::new();

  for branch_id in branches {
    let node = read_node_from_id (
      config, driver, branch_id ). await ?;
    let orgnode2 : OrgNode =
      OrgNode {
        metadata : metadata_for_element_of_path (
          branch_id, cycle_node, false ),
        title : newline_to_space ( & node.title ),
        body : None,
      };
    result.push_str (
      & render_org_node_from_text (
        branch_level,
        &orgnode2 )); }
  Ok (result) }

async fn render_terminating_cycle_when_no_branches(
  config: &SkgConfig,
  driver: &TypeDBDriver,
  cycle_id: &ID,
  cycle_node: &Option<ID>,
  cycle_level: usize,
) -> Result<String, Box<dyn Error>> {
  let node = read_node_from_id (
    config, driver, cycle_id ). await ?;
  let orgnode2 : OrgNode =
    OrgNode {
      metadata : metadata_for_element_of_path (
        cycle_id, cycle_node, false ),
      title : newline_to_space ( & node.title ),
      body : None,
    };
  Ok ( render_org_node_from_text (
    cycle_level,
    &orgnode2 )) }

fn metadata_for_element_of_path (
  node_id     : &ID,
  cycle_node  : &Option<ID>,
  is_terminus : bool
) -> OrgnodeMetadata {
  OrgnodeMetadata {
    id : Some ( node_id.clone () ),
    relToOrgParent :
      if is_terminus {
        RelToOrgParent::Content
      } else {
        // All nodes except the terminus (first in path)
        // contain their org parent.
        // PITFALL: If there is a second appearance of the terminus,
        // later in the containerward path,
        // that appearance *should* (and does) get this type.
        RelToOrgParent::Container
      },
    cycle : cycle_node.as_ref () == Some ( node_id ),
    focused : false,
    folded : false,
    mightContainMore : ! is_terminus,
    repeat : false,
    toDelete : false,
  } }
