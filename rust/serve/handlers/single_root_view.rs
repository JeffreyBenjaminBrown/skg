use crate::to_org::render::content_view::single_root_view;
use crate::serve::util::{
  send_response,
  send_response_with_length_prefix,
  format_buffer_response_sexp};
use crate::types::sexp::extract_v_from_kv_pair_in_sexp;
use crate::types::misc::SkgConfig;
use crate::types::ID;

use futures::executor::block_on;
use sexp::Sexp;
use std::net::TcpStream; // handles two-way communication
use typedb_driver::TypeDBDriver;

/// Gets a node id from the request,
/// generates an org view of that id's content (recursively),
/// and sends the response to Emacs (length-prefixed).
/// Response format: ((content "...") (errors ("error1" "error2" ...)))
pub fn handle_single_root_view_request (
  stream        : &mut TcpStream,
  request       : &str,
  typedb_driver : &TypeDBDriver,
  config        : &SkgConfig,
) {

  match node_id_from_single_root_view_request ( request ) {
    Ok ( node_id ) => {
      let response_sexp : String = block_on ( async {
        match single_root_view (
          typedb_driver,
          config,
          &node_id ) . await
        { Ok ( buffer_content ) =>
            format_buffer_response_sexp (
              & buffer_content,
              & vec![] ),
          Err (e) => {
            // If we fail to generate the view, return error in content
            let error_content : String = format!(
              "Error generating document: {}", e);
            format_buffer_response_sexp (
              & error_content,
              & vec![] ) }} } );
      send_response_with_length_prefix (
        stream,
        & response_sexp ); },
    Err ( err ) => {
      let error_msg : String = format!(
        "Error extracting node ID: {}", err);
      println! ( "{}", error_msg ) ;
      send_response ( stream, &error_msg ); } } }

pub fn node_id_from_single_root_view_request (
  request : &str
) -> Result<ID, String> {
  let sexp : Sexp =
    sexp::parse ( request )
    . map_err ( |e| format! (
      "Failed to parse S-expression: {}", e ) ) ?;
  extract_v_from_kv_pair_in_sexp ( &sexp, "id" )
    . map(ID) }
