use crate::viewdata::set_metadata_relationship_viewdata_in_forest;
use crate::from_text::buffer_to_orgnode_forest_and_save_instructions;
use crate::merge::merge_nodes;
use crate::org_to_text::orgnode_forest_to_string;
use crate::save::update_graph_minus_merges;
use crate::serve::util::{ format_buffer_response_sexp, read_length_prefixed_content, send_response};
use crate::to_org::complete::contents::completeAndRestoreForest_collectingViewRequests;
use crate::to_org::expand::definitive::execute_view_requests;
use crate::to_org::util::forest_root_pair;
use crate::types::errors::SaveError;
use crate::types::misc::{ID, SkgConfig, TantivyIndex};
use crate::types::orgnode_new::OrgNode;
use crate::types::save::{SaveInstruction, MergeInstructionTriple, format_save_error_as_org};
use crate::types::skgnode::SkgNode;
use crate::types::tree::{NodePair, PairTree};

use ego_tree::{Tree, NodeId, NodeMut};
use futures::executor::block_on;
use sexp::{Sexp, Atom};
use std::collections::HashMap;
use std::error::Error;
use std::io::{BufReader, Write};
use std::net::TcpStream;
use typedb_driver::TypeDBDriver;

/// Rust's response to Emacs for a save operation.
/// Contains the regenerated buffer content and any warnings/errors.
pub struct SaveResponse {
  pub buffer_content : String,
  pub errors         : Vec < String >,
}

impl SaveResponse {
  /// Format the response as an s-expression.
  /// Format: ((content "...") (errors ("error1" "error2" ...)))
  fn to_sexp_string ( &self ) -> String {
    format_buffer_response_sexp (
      & self . buffer_content,
      & self . errors ) }}

/* Handles save buffer requests from Emacs.
.
- Reads the buffer content with length prefix.
- Puts that text through `update_from_and_rerender_buffer`.
- Sends that back to Emacs (with a length prefix). */
pub fn handle_save_buffer_request (
  reader        : &mut BufReader <TcpStream>,
  stream        : &mut TcpStream,
  typedb_driver : &TypeDBDriver,
  config        : &SkgConfig,
  tantivy_index : &TantivyIndex ) {

  match read_length_prefixed_content (reader) {
    Ok (initial_buffer_content) => {
      match block_on(
        update_from_and_rerender_buffer (
          // Most of the work happens here.
          & initial_buffer_content,
          typedb_driver, config, tantivy_index ))
      { Ok (save_response) =>
        { // S-exp response format: ((content "...") (errors (...)))
          stream . write_all (
              { let response_sexp : String =
                  save_response . to_sexp_string ();
                let header : String =
                  format! ( "Content-Length: {}\r\n\r\n",
                               response_sexp . len () );
                format! ( "{}{}", header, response_sexp ) }
              . as_bytes() ). unwrap ();
          stream . flush() . unwrap (); }
        Err (err) => {
          // Check if this is a SaveError that should be formatted for the client
          if let Some(save_error) = err.downcast_ref::<SaveError>() {
            stream.write_all(
              { let response_sexp : String =
                  { let response : Sexp =
                      empty_response_sexp (
                        & { let error_buffer_content : String =
                              format_save_error_as_org(save_error);
                            error_buffer_content } );
                    response }
                  . to_string ();
                let full_response : String =
                  format! (
                    "{}{}",
                    { let header : String =
                        format! ( "Content-Length: {}\r\n\r\n",
                                  response_sexp . len ( ));
                      header },
                    response_sexp );
                full_response
              } .as_bytes( )) . unwrap();
            stream.flush().unwrap();
          } else {
            let error_msg : String =
              format!("Error processing buffer content: {}", err);
            println!("{}", error_msg);
            send_response(stream, &error_msg);
          }} }}
    Err(err) => {
      let error_msg : String =
        format! ("Error reading buffer content: {}", err );
      println! ( "{}", error_msg );
      send_response ( stream, &error_msg ); }} }

/// Create an s-expression with nil content and an error message.
fn empty_response_sexp (
  error_buffer_content : &str
) -> Sexp {
  Sexp::List ( vec! [
    Sexp::List ( vec! [
      Sexp::Atom ( Atom::S ( "content" . to_string () )),
      Sexp::Atom ( Atom::S ( "nil" . to_string () )) ] ),
    Sexp::List ( vec! [
      Sexp::Atom ( Atom::S ( "errors" . to_string () )),
      Sexp::List ( vec! [
        Sexp::Atom ( Atom::S (
          error_buffer_content . to_string () )) ] ) ] ) ] ) }

/// Update dbs and filesystem, and generate text for a new org-buffer.
/// Steps:
/// - Build an orgnode forest and save instructions,
///   via `buffer_to_orgnode_forest_and_save_instructions`.
/// - Update the graph:
///   - 'update_graph_minus_merges'
///   - 'merge_nodes'
/// - Modify the orgnode forest:
///   - 'pair_orgnode_forest_with_save_instructions'
///   - 'completeAndRestoreForest_collectingViewRequests'
///   - 'execute_view_requests'
///   - 'set_metadata_relationship_viewdata_in_forest'
/// - Return a content view, via 'orgnode_forest_to_string'.
pub async fn update_from_and_rerender_buffer (
  org_buffer_text : &str,
  typedb_driver   : &TypeDBDriver,
  config          : &SkgConfig,
  tantivy_index   : &TantivyIndex
) -> Result<SaveResponse, Box<dyn Error>> {
  let (orgnode_forest, save_instructions, mergeInstructions)
    : ( Tree<OrgNode>,
        Vec<SaveInstruction>,
        Vec<MergeInstructionTriple> )
    = buffer_to_orgnode_forest_and_save_instructions (
      org_buffer_text, config, typedb_driver )
    . await . map_err (
      |e| Box::new(e) as Box<dyn Error> ) ?;
  if orgnode_forest.root().children().next().is_none() { return Err (
    "Nothing to save found in org_buffer_text" . into( )); }

  update_graph_minus_merges (
    save_instructions.clone(),
    config.clone(),
    tantivy_index,
    typedb_driver ) . await ?;
  merge_nodes (
    mergeInstructions,
    config.clone(),
    tantivy_index,
    typedb_driver ) . await ?;

  let mut errors : Vec < String > = Vec::new ();
  let mut paired_forest : PairTree =
    pair_orgnode_forest_with_save_instructions (
      // Definitive nodes get Some(skgnode), indefinitive get None.
      & orgnode_forest,
      & save_instructions );
  { // modify the paired forest before re-rendering it
    let (mut visited, view_requests) =
      completeAndRestoreForest_collectingViewRequests (
        &mut paired_forest,
        config,
        typedb_driver ) . await ?;
    execute_view_requests ( // PITFALL: Should follow completion.
      // Why: If a content child added during completion matches the head of the path to be integrated for a view request, then the path will be integrated there (where treatment=Content), instead of creating a duplicate child with treatment=ParentIgnores.
      &mut paired_forest,
      view_requests,
      config,
      typedb_driver,
      &mut visited,
      &mut errors ) . await ?; }
  set_metadata_relationship_viewdata_in_forest (
    &mut paired_forest,
    config,
    typedb_driver ) . await ?;

  let buffer_content : String =
    orgnode_forest_to_string ( & paired_forest ) ?;

  Ok ( SaveResponse { buffer_content, errors } ) }

/// Converts a OrgNode forest to a PairTree forest
/// (both represented as Trees, via ForestRoot).
///
/// Definitive nodes that generated SaveInstructions get Some(skgnode).
/// Indefinitive nodes (views) get None.
fn pair_orgnode_forest_with_save_instructions (
  orgnode_tree : &Tree<OrgNode>,
  instructions : &[SaveInstruction],
) -> PairTree {
  let skgnode_map : HashMap<ID, SkgNode> =
    instructions . iter ()
    . filter_map ( |(skgnode, _action)| {
      skgnode . ids . first ()
        . map ( |pid| (pid.clone(), skgnode.clone()) ) } )
    . collect ();
  let mut pair_tree : PairTree = Tree::new (
    // PITFALL: Discards the forest's root OrgNode.
    forest_root_pair () );
  let forest_root_treeid : NodeId = pair_tree . root () . id ();
  for tree_root in orgnode_tree.root().children() {
    add_paired_subtree_as_child (
      &mut pair_tree,
      forest_root_treeid,
      orgnode_tree, tree_root . id (),
      &skgnode_map ); }
  pair_tree }

/// Add a OrgNode subtree as a child of a parent in the PairTree,
/// pairing each node with its SkgNode from the map.
fn add_paired_subtree_as_child (
  pair_tree      : &mut PairTree,
  parent_treeid  : NodeId,
  orgnode_tree   : &Tree<OrgNode>,
  orgnode_treeid : NodeId,
  skgnode_map    : &HashMap<ID, SkgNode>,
) {
  let orgnode : OrgNode =
    orgnode_tree . get ( orgnode_treeid ) . unwrap ()
    . value () . clone ();
  let mskgnode : Option<SkgNode> =
    orgnode . id ()
    . and_then (
      |id| skgnode_map . get (id) . cloned () );
  let new_treeid : NodeId = {
    let mut parent_mut : NodeMut < _ > =
      pair_tree . get_mut ( parent_treeid ) . unwrap ();
    parent_mut . append ( // add new node
      NodePair { mskgnode, orgnode } ) . id () };
  { // recurse in new node
    let child_treeids : Vec < NodeId > =
      orgnode_tree . get ( orgnode_treeid ) . unwrap ()
      . children () . map ( |c| c . id () ) . collect ();
    for child_treeid in child_treeids {
      add_paired_subtree_as_child (
        pair_tree, new_treeid,
        orgnode_tree, child_treeid,
        skgnode_map ); }} }
