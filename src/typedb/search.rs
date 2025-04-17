use futures::StreamExt;
use std::error::Error;
use std::io;
use typedb_driver::{
    TypeDBDriver,
    TransactionType,
};

use crate::file_io::read_skgnode_from_path;

pub async fn find_node_containing_node (
  db_name : &str,
  driver : &TypeDBDriver,
  target_id: &str
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
                 target_id ) ) . await?;
  let mut stream = answer.into_rows();
  if let Some(row_result) = stream.next().await {
    let row = row_result?;
    if let Some(concept) = row.get("container_id")? {
      return Ok(extract_id_from_typedb_string_rep(
        &concept.to_string())); } }
  Err(format!("No container found for node with ID '{}'",
              target_id).into()) }

pub async fn get_path_from_node_id (
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
    db_name: &str,
    driver: &TypeDBDriver,
    node_id: &str
) -> Result<String, Box<dyn Error>> {
  let root_node = helps_recursive_s_expression_from_node (
    db_name, driver, node_id ).await?;
  let sexpr = format!(
    "((view . \"single document\")\n (content . ({})))",
    root_node );
  Ok (sexpr) }

async fn helps_recursive_s_expression_from_node(
  db_name: &str,
  driver: &TypeDBDriver,
  node_id: &str
) -> Result<String, Box<dyn Error>> {
  let path = get_path_from_node_id(
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
  if !node.nodes_contained.is_empty() { // recursion
    let mut contained_sexpr = Vec::new();
    for contained_id in &node.nodes_contained {
      let contained_node = Box::pin(
        helps_recursive_s_expression_from_node(
          db_name, driver, contained_id)).await?;
      contained_sexpr.push(format!("({})", contained_node)); }
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
