use futures::StreamExt;
use std::error::Error;
use std::io;
use typedb_driver::{
    TypeDBDriver,
    TransactionType,
};

use crate::file_io::read_skgnode_from_path;

pub async fn path_to_root_container (
  // Returns the path from the given node to the root container
  // The first element is the original node, and the last is the root.
  db_name: &str,
  driver: &TypeDBDriver,
  node: &str
) -> Result<Vec<String>, Box<dyn Error>> {
  let mut path = vec![node.to_string()];
  let mut current_node = node.to_string();
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
  node: &str
) -> Result<String, Box<dyn Error>> {
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
      return Ok(extract_id_from_typedb_string_rep(
        &concept.to_string())); } }
  Err(format!("No container found for node with ID '{}'",
              node).into()) }

pub async fn get_container_path_from_node (
  db_name: &str,
  driver: &TypeDBDriver,
  node_id: &str
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
      return Ok(extract_id_from_typedb_string_rep(
        &concept.to_string())); } }
  Err(format!("No path found for node with ID '{}'",
              node_id).into ()) }

pub async fn recursive_s_expression_from_node(
  /*Given a node, this finds its root container, and returns
an s-expression representing a document built from there.

Important properties of that s-expression include:
    `view` : `single document`
    `content` : a length-1 list of nodes.
  where each "node" contains the following:
    `headline` : the text of a headline (bullet)
    `focused` : absent everywhere except for one node, the node which the document was summoned in order to view.
    `unindexed_text` : possibly absent, the text just under the bullet.
    `content`: possibly absent, a list of nodes.
  Thus the document is recursive. */
  db_name: &str,
  driver: &TypeDBDriver,
  focus: &str
) -> Result<String, Box<dyn Error>> {
  let path = path_to_root_container(db_name, driver, focus).await?;
  let root_id = path.last()
    .ok_or_else(
      || io::Error::new(
        io::ErrorKind::InvalidData,
        format!("Empty path returned for node '{}'", focus)
      ))?;
  let sexpr = format!(
    "((view . \"single document\")\n (content . ({})))",
    helps_recursive_s_expression_from_node(
      db_name, driver, root_id, focus).await?);
  Ok (sexpr) }

async fn helps_recursive_s_expression_from_node(
  db_name: &str,
  driver: &TypeDBDriver,
  node_id: &str,
  focus: &str
) -> Result<String, Box<dyn Error>> {
  let path = get_container_path_from_node(
    db_name, driver, node_id).await?;
  let node = read_skgnode_from_path ( path ) ?;
  let headline = node.titles.first()
    .ok_or_else(|| io::Error::new(
      io::ErrorKind::InvalidData,
      format!("Node with ID {} has no titles",
              node_id)
    ))? . to_string();
  let mut node_sexpr = format!(
    "(\"id\" . \"{}\")\n  (\"headline\" . \"{}\")",
    node_id,
    escape_string_for_s_expression ( &headline ) );
  if node_id == focus {
    node_sexpr = format!(
      "{}\n  (\"focused\" . t)",
      node_sexpr // PITFALL: self-referential
    ); }
  if let Some(text) = &node.unindexed_text {
    // Only happens if unindexed_text is present.
    node_sexpr = format!(
      "{}\n  (\"unindexed_text\" . \"{}\")",
      node_sexpr, // PITFALL: self-referential
      escape_string_for_s_expression(text)); }
  if !node.nodes_contained.is_empty() { // recursion
    let mut contained_sexpr = Vec::new();
    for contained_id in &node.nodes_contained {
      let contained_node = Box::pin(
        helps_recursive_s_expression_from_node(
          db_name, driver, contained_id, focus)).await?;
      contained_sexpr.push(format!("({})",
                                   contained_node)); }
    if !contained_sexpr.is_empty() {
      let content_str = contained_sexpr.join("\n     ");
      node_sexpr = format!(
        "{}\n  (\"content\" . (\n     {}\n  ))",
        node_sexpr,
        content_str); } }
  Ok (node_sexpr) }

fn escape_string_for_s_expression(s: &str) -> String {
  s . replace("\\", "\\\\") . replace("\"", "\\\"") }

pub fn extract_id_from_typedb_string_rep(
  attribute_str: &str)
  -> String {
  if let Some(start) = attribute_str.find('"') {
    if let Some(end) = attribute_str[start + 1..] . find('"') {
      return attribute_str [start + 1 .. start + 1 + end]
        . to_string(); } }
  panic!( "Failed to extract ID from TypeDB output: {}",
           attribute_str) }
