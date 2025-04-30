use futures::StreamExt;
use std::error::Error;
use std::io;
use typedb_driver::{
    TypeDBDriver,
    TransactionType,
};

use crate::file_io::read_skgnode_from_path;
use crate::types::ID;

pub async fn path_to_root_container (
  // Returns the path from the given node to the root container
  // The first element is the original node, and the last is the root.
  db_name: &str,
  driver: &TypeDBDriver,
  node: &ID
) -> Result<Vec<ID>, Box<dyn Error>> {
  let mut path = vec![node.clone()];
  let mut current_node = node.clone();
  loop {
    match find_container_of (
      db_name, driver, &current_node) . await {
      Ok(container_id) => {
        // Found a container, so add it to the path.
        path.push(container_id.clone());
        current_node = container_id;
      }, Err(_) => {
        // No container found; this is the root.
        break; } } }
  Ok (path) }

pub async fn find_container_of (
  db_name : &str,
  driver : &TypeDBDriver,
  node: &ID
) -> Result<ID, Box<dyn Error>> {
  let tx = driver.transaction(
    db_name, TransactionType::Read).await?;
  let answer = tx.query(
    format!( r#" match
                   $container isa node, has id $container_id;
                   $contained isa node, has id "{}";
                   $rel isa contains (container: $container,
                                      contained: $contained);
                 select $container_id;"#,
                 node ) ) . await?;
  let mut stream = answer.into_rows();
  if let Some(row_result) = stream.next().await {
    let row = row_result?;
    if let Some(concept) = row.get("container_id")? {
      return Ok ( ID (
        extract_payload_from_typedb_string_rep(
          &concept.to_string() ) ) ); } }
  Err(format!("No container found for node with ID '{}'",
              node).into()) }

pub async fn get_filepath_from_node (
  db_name: &str,
  driver: &TypeDBDriver,
  node_id: &ID
) -> Result<String, Box<dyn Error>> {
  let tx = driver.transaction(
    db_name, TransactionType::Read).await?;
  let query = format!(
    r#"match
      $node isa node,
            has path $path;
      {{ $node has id "{}"; }} or
      {{ $e isa extra_id, has id "{}";
         $rel isa has_extra_id (node: $node,
                                extra_id: $e); }} ;
      select $path;"#,
    node_id,
    node_id );
  let answer = tx.query(query).await?;
  let mut stream = answer.into_rows();
  if let Some(row_result) = stream.next().await {
    let row = row_result?;
    if let Some(concept) = row.get("path")? {
      return Ok(extract_payload_from_typedb_string_rep(
        &concept.to_string())); } }
  Err(format!("No path found for node with ID '{}'",
              node_id).into ()) }

pub async fn single_document_view(
  /*Given a node, this finds its root container, and returns
an s-expression representing a document built from there.

Properties (tags) in the resulting s-expression include:
    `view` : `single document`
    `content` : a length-1 list of nodes.
  where each "node" contains the following:
    `heading` : the text of a heading (bullet)
    `focused` : absent almost everywhere, but `t` for the node which the document was summoned in order to view.
    `body` : possibly absent, the text just under the bullet.
    `content`: possibly absent, a list of nodes.
  Thus the document is recursive. */
  db_name: &str,
  driver: &TypeDBDriver,
  focus: &ID
) -> Result<String, Box<dyn Error>> {
  let path = path_to_root_container(
    db_name, driver, focus) . await?;
  let root_id = path.last() . ok_or_else(
    || io::Error::new(
      io::ErrorKind::InvalidData,
      format!("Empty path returned for node '{}'", focus)
    ))?;
  let sexpr = format!(
    "((view . \"single document\")\n (content . ({})))",
    recursive_s_expression_from_node(
      db_name, driver, root_id, focus, &mut Vec::new() )
      . await?);
  Ok (sexpr) }

async fn recursive_s_expression_from_node(
  db_name: &str,
  driver: &TypeDBDriver,
  node_id: &ID,
  focus: &ID,
  visited: &mut Vec<ID>
) -> Result<String, Box<dyn Error>> {
  let path = get_filepath_from_node(
    db_name, driver, node_id).await?;
  let node = read_skgnode_from_path ( path ) ?;
  let heading = node.titles.first()
    .ok_or_else(|| io::Error::new(
      io::ErrorKind::InvalidData,
      format!("Node with ID {} has no titles",
              node_id)
    ))? . to_string();

  if visited.iter().any(|id| id == node_id) { // was already visited
    let node_sexpr = format!(
      "(id . \"{}\")\n  (heading . \"{}\")\n  (body . \"repeated above\")\n  (repeated . t)",
      node_id,
      escape_string_for_s_expression(&heading)
    );
    return Ok(node_sexpr);
  }

  visited.push ( node_id.clone() );
  let mut node_sexpr = format!(
    "(id . \"{}\")\n  (heading . \"{}\")",
    node_id,
    escape_string_for_s_expression ( &heading ) );
  if node_id == focus {
    node_sexpr = format!(
      "{}\n  (focused . t)",
      node_sexpr // PITFALL: self-referential
    ); }
  if let Some(text) = &node.body {
    // Only happens if body is present.
    node_sexpr = format!(
      "{}\n  (body . \"{}\")",
      node_sexpr, // PITFALL: self-referential
      escape_string_for_s_expression(text)); }
  if !node.contains.is_empty() { // recursion
    let mut contained_sexpr = Vec::new();
    for contained_id in &node.contains {
      let contained_node = Box::pin(
        recursive_s_expression_from_node(
          db_name, driver, contained_id, focus, visited
        ) ) . await?;
      contained_sexpr.push(format!("({})",
                                   contained_node)); }
    if !contained_sexpr.is_empty() {
      let content_str = contained_sexpr.join("\n     ");
      node_sexpr = format!(
        "{}\n  (content . (\n     {}\n  ))",
        node_sexpr,
        content_str); } }
  Ok (node_sexpr) }

fn escape_string_for_s_expression(s: &str) -> String {
  s . replace("\\", "\\\\") . replace("\"", "\\\"") }

pub fn extract_payload_from_typedb_string_rep(
  attribute_str: &str)
  -> String {
  if let Some(start) = attribute_str.find('"') {
    if let Some(end) = attribute_str[start + 1..] . find('"') {
      return attribute_str [start + 1 .. start + 1 + end]
        . to_string(); } }
  panic!( "Failed to extract payload from TypeDB output: {}",
           attribute_str) }
