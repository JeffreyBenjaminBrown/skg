use crate::viewdata::set_metadata_relationship_viewdata_in_forest;
use crate::from_text::buffer_to_orgnode_forest_and_save_instructions;
use crate::merge::merge_nodes;
use crate::org_to_text::orgnode_forest_to_string;
use crate::save::update_graph_minus_merges;
use crate::serve::util::{ format_buffer_response_sexp, read_length_prefixed_content, send_response};
use crate::to_org::complete::contents::complete_or_restore_each_node_in_branch;
use crate::to_org::expand::collect_view_requests::collectViewRequestsFromForest;
use crate::to_org::expand::definitive::execute_view_requests;
use crate::to_org::util::DefinitiveMap;
use crate::types::errors::SaveError;
use crate::types::misc::{SkgConfig, TantivyIndex};
use crate::types::orgnode::{OrgNode, ViewRequest};
use crate::types::save::{SaveInstruction, MergeInstructionTriple, format_save_error_as_org};
use crate::types::skgnode::{SkgNodeMap, skgnode_map_from_save_instructions};

use ego_tree::{Tree, NodeId};
use futures::executor::block_on;
use sexp::{Sexp, Atom};
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

/// PURPOSE: Process the buffer that a user wants to save.
/// - "save": Update dbs and filesystem.
/// - "rerender": Create a new buffer for the user.
/// ERRORS: If the buffer is invalid.
/// COMPLEX:
/// - Validation must happen at many stages.
/// - Merges must follow the execution of other save instructions, because the user may have updated one of the nodes to be merged.
/// - completeAndRestoreForest_collectingViewRequests is complex.
/// - execute_view_requests is complex, because it attempts to integrate existing branches before generating new ones
pub async fn update_from_and_rerender_buffer (
  org_buffer_text : &str,
  typedb_driver   : &TypeDBDriver,
  config          : &SkgConfig,
  tantivy_index   : &TantivyIndex
) -> Result<SaveResponse, Box<dyn Error>> {
  let (forest, save_instructions, mergeInstructions)
    : ( Tree<OrgNode>,
        Vec<SaveInstruction>,
        Vec<MergeInstructionTriple> )
    = buffer_to_orgnode_forest_and_save_instructions (
      org_buffer_text, config, typedb_driver )
    . await . map_err (
      |e| Box::new(e) as Box<dyn Error> ) ?;
  if forest.root().children().next().is_none() { return Err (
    "Nothing to save found in org_buffer_text" . into( )); }

  { // update the graph
    update_graph_minus_merges (
      save_instructions.clone(),
      config.clone(),
      tantivy_index,
      typedb_driver ) . await ?;
    merge_nodes (
      mergeInstructions,
      config.clone(),
      tantivy_index,
      typedb_driver ) . await ?; }

  { // update the view and return it to the client
    let mut errors : Vec < String > = Vec::new ();
    let mut skgnode_map : SkgNodeMap =
      skgnode_map_from_save_instructions ( & save_instructions );
    let mut forest_mut : Tree<OrgNode> = forest;
    { // mutate it before re-rendering it
      let mut visited : DefinitiveMap = DefinitiveMap::new();
      let forest_root_id : NodeId = forest_mut.root().id();
      complete_or_restore_each_node_in_branch (
        &mut forest_mut,
        &mut skgnode_map,
        forest_root_id,
        config,
        typedb_driver,
        &mut visited ). await ?;
      let view_requests : Vec < (NodeId, ViewRequest) > =
        collectViewRequestsFromForest (
          & forest_mut ) ?;
      execute_view_requests ( // PITFALL: Must follow completion.
        // Why: If a content child added during completion matches the head of the path to be integrated for a view request, then the path will be integrated there (where treatment=Content), instead of creating a duplicate child with treatment=ParentIgnores.
        &mut forest_mut,
        &mut skgnode_map,
        view_requests,
        config,
        typedb_driver,
        &mut visited,
        &mut errors ). await ?;
      set_metadata_relationship_viewdata_in_forest (
        &mut forest_mut,
        config,
        typedb_driver ). await ?; }
    let buffer_content : String =
      orgnode_forest_to_string ( & forest_mut ) ?;
    Ok ( SaveResponse { buffer_content, errors } ) }}
