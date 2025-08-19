use std::collections::HashSet;
use std::error::Error;
use std::io;
use typedb_driver::TypeDBDriver;

use crate::file_io::read_filenode;
use crate::typedb::search::{
  find_rootish_container,
  filepath_from_id, };
use crate::types::{ID, FileNode};


/*Given a node (the `focus` argument),
this finds its root container (the `root` variable),
and returns an s-expression representing a document
with `root` as its root.
.
Properties (tags) in the resulting s-expression include:
    `view`    : `single document`
    `content` : a length-1 list of nodes.
  where each "node" contains the following:
    `heading` : the text of a heading (bullet)
    `focused` : absent almost everywhere, but `t` for the node which the document was summoned in order to view.
    `body`    : possibly absent, the text just under the bullet.
    `content` : possibly absent, a list of nodes.
  The `content` field enables recursion. */
pub async fn single_document_view (
  db_name : &str,
  driver  : &TypeDBDriver,
  focus   : &ID,
) -> Result < String, Box<dyn Error> > {

  let root_id : ID = find_rootish_container (
      db_name, driver, focus ) . await ?;
  let sexpr = format! (
    "((view . \"single document\")\n (content . ({})))",
    s_expression_from_node_recursive (
      db_name, driver, &root_id, focus,
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

  let path : String = filepath_from_id (
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
