use std::collections::HashSet;
use futures::StreamExt;
use std::error::Error;
use std::io;
use typedb_driver::{
  answer::{ConceptRow, QueryAnswer},
  Transaction,
  TransactionType,
  TypeDBDriver,
};

use crate::file_io::read_filenode;
use crate::types::{ID, FileNode};

pub async fn path_to_root_container (
  // Runs a series of TypeDB queries.
  // Returns a path through the graph,
  // from the given node (first in returned vector)
  // to the root container (last in returned vector).
  db_name : &str,
  driver  : &TypeDBDriver,
  node    : &ID
) -> Result < Vec<ID>,
              Box<dyn Error> > {

  let mut path : Vec<ID> =
    vec! [ node.clone () ];
  let mut seen : HashSet<ID> =
    HashSet::from ( [ node.clone() ] );
  let mut current_node : ID =
    node.clone ();
  loop {
    // TODO: This is inefficient. Better to use a single TypeDB query.
    match find_container_of (
      db_name, driver, &current_node) . await {
      Ok (container_id) => {
        if seen.contains ( &container_id ) {
          // It's a duplicate. Return without re-including duplicate.
          break; }
        else { // Found a container. Add it to `path` and `seen`.
          path.push ( container_id.clone () );
          seen.insert ( container_id.clone () );
          current_node = container_id; } },
      Err(_) => {
        // No container found, so this is the root.
        break; } } }
  Ok (path) }

pub async fn find_container_of (
  // Runs a single TypeDB query.
  // Returns the containing node's ID.
  db_name : &str,
  driver  : &TypeDBDriver,
  node    : &ID
) -> Result < ID, Box<dyn Error> > {

  let tx : Transaction = driver.transaction (
    db_name, TransactionType::Read ). await ?;
  let answer : QueryAnswer = tx.query (
    format!( r#" match
                   $container isa node, has id $container_id;
                   $contained isa node, has id "{}";
                   $rel isa contains ( container: $container,
                                       contained: $contained );
                 select $container_id;"#,
                 node ) ) . await?;
  let mut stream = answer.into_rows ();
  if let Some (row_result) = stream . next () . await {
    let row : ConceptRow = row_result ?;
    if let Some (concept) = row.get ("container_id") ? {
      return Ok ( ID (
        extract_payload_from_typedb_string_rep (
          &concept . to_string () )) ); }}
  Err ( format! ( "No container found for node with ID '{}'",
                   node ). into() ) }

pub async fn get_filepath_from_node (
  // Runs a single TypeDB query.
  // Returns the node's filepath.
  db_name : &str,
  driver  : &TypeDBDriver,
  node_id : &ID
) -> Result < String, Box<dyn Error> > {

  let tx : Transaction =
    driver.transaction (
      db_name, TransactionType::Read
    ). await ?;
  let query = format! (
    r#"match
      $node isa node,
            has path $path;
      {{ $node has id "{}"; }} or
      {{ $e isa extra_id, has id "{}";
         $rel isa has_extra_id ( node: $node,
                                 extra_id: $e ); }} ;
      select $path;"#,
    node_id,
    node_id );
  let answer : QueryAnswer = tx.query ( query ). await ?;
  let mut stream = answer.into_rows ();
  if let Some (row_result) = stream . next () . await {
    let row : ConceptRow = row_result ?;
    if let Some (concept) = row.get ("path") ? {
      return Ok (extract_payload_from_typedb_string_rep (
        &concept . to_string () )); }}
  Err ( format! ( "No path found for node with ID '{}'",
                   node_id).into ()) }

pub async fn single_document_view (
  /*Given a node (the `focus` argument),
this finds its root container (the `root` variable),
and returns an s-expression representing a document
with `root` as its root.

Properties (tags) in the resulting s-expression include:
    `view`    : `single document`
    `content` : a length-1 list of nodes.
  where each "node" contains the following:
    `heading` : the text of a heading (bullet)
    `focused` : absent almost everywhere, but `t` for the node which the document was summoned in order to view.
    `body`    : possibly absent, the text just under the bullet.
    `content` : possibly absent, a list of nodes.
  The `content` field enables recursion. */
  db_name : &str,
  driver  : &TypeDBDriver,
  focus   : &ID,
) -> Result < String, Box<dyn Error> > {

  let path : Vec<ID> =
    path_to_root_container (
      db_name, driver, focus
    ). await ?;
  let root_id : &ID = path . last () . ok_or_else (
    || io::Error::new (
      io::ErrorKind::InvalidData,
      format! ( "Empty path returned for node '{}'",
                 focus )
    )) ?;
  let sexpr = format! (
    "((view . \"single document\")\n (content . ({})))",
    s_expression_from_node_recursive (
      db_name, driver, root_id, focus,
      &mut HashSet::new () )
      . await ?);
  Ok (sexpr) }

async fn s_expression_from_node_recursive (
  db_name : &str,
  driver  : &TypeDBDriver,
  root    : &ID,
  focus   : &ID,
  visited : &mut HashSet<ID>
) -> Result < String, Box<dyn Error> > {

  let path : String = get_filepath_from_node (
    db_name, driver, root ). await ?;
  let filenode : FileNode = read_filenode ( path ) ?;
  if filenode . title . is_empty () {
    return Err (
      Box::new (
        io::Error::new (
          io::ErrorKind::InvalidData,
          format! ( "Node with ID {} has an empty title",
                     root )) )); }
  if visited.contains (root) {
    return Ok (
      format_repeated_node_sexpr (
        root, &filenode.title )); }
  visited . insert ( root.clone () );
  { // Recursively process children, *before* finishing root s-exp.
    let mut child_sexprs : Vec<String> = Vec::new();
    for contained_id in &filenode.contains {
      let contained_node : String = Box::pin (
        s_expression_from_node_recursive (
          db_name, driver, contained_id, focus, visited
        )). await ?;
      child_sexprs . push ( format! ( "({})",
                                      contained_node )); }
    Ok ( format_node_sexpr (
      root, &filenode, focus, child_sexprs )) }}

fn format_node_sexpr (
  /* Returns an sexp with:
  - id,
  - heading,
  - focused, if applicable
  - body, if applicable
  - contents, if applicable, each of which looks the same
  */
  node_id      : &ID,
  filenode     : &FileNode,
  focus        : &ID,
  child_sexprs : Vec<String>
) -> String {

  let heading : String =
    escape_string_for_s_expression ( &filenode.title );
  let mut node_sexpr = format!(
    "(id . \"{}\")\n  (heading . \"{}\")",
    node_id, heading );
  if node_id == focus {
    node_sexpr = format! (
      "{}\n  (focused . t)",
      node_sexpr); }
  if let Some (text) = &filenode.body {
    node_sexpr = format! (
      "{}\n  (body . \"{}\")",
      node_sexpr,
      escape_string_for_s_expression (text) ); }
  if !child_sexprs.is_empty () {
    let content_str = child_sexprs.join("\n     ");
    node_sexpr = format!(
      "{}\n  (content . (\n     {}\n  ))",
      node_sexpr,
      content_str ); }
  node_sexpr }

fn format_repeated_node_sexpr (
  node_id: &ID,
  title: &str
) -> String {
  // If `node_id` appears earlier in the same document,
  // then the document should repeat it,
  // but should also indicate that it is a repeat,
  // and should not recurse into it a second time.

  format! (
    "(id . \"{}\")\n  (heading . \"{}\")\n  (body . \"Repeated above. Edit there, not here.\")\n  (repeated . t)",
    node_id,
    escape_string_for_s_expression (title)
  ) }

fn escape_string_for_s_expression (
  // TODO: Why do I use this?
  s : &str )
  -> String
{ s
  . replace ("\\", "\\\\")
  . replace ("\"", "\\\"") }

pub fn extract_payload_from_typedb_string_rep (
  // Returns the string it finds
  // between the first and the second quotation marks.
  attribute_str : &str )
  -> String {

  if let Some (first_quote_pos) =
    attribute_str                          . find ('"')
  { if let Some (second_quote_pos) =
    attribute_str [first_quote_pos + 1 ..] . find ('"')
    { return attribute_str [ first_quote_pos + 1 ..
                             first_quote_pos + 1 + second_quote_pos ]
        . to_string (); }}
  panic! ( "Failed to extract payload from TypeDB output: {}",
            attribute_str ) }
