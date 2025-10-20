use crate::mk_org_text::containerward_org_view;
use crate::serve::util::send_response;
use crate::serve::util::send_response_with_length_prefix;
use crate::serve::parse_headline_from_sexp;
use crate::types::SkgConfig;

use futures::executor::block_on;
use std::net::TcpStream;
use typedb_driver::TypeDBDriver;

/// Extracts parameters from a containerward view request,
/// generates the containerward org view,
/// and sends the response with length prefix.
pub fn handle_containerward_view_request (
  stream        : &mut TcpStream,
  request       : &str,
  typedb_driver : &TypeDBDriver,
  config        : &SkgConfig,
) {

  match parse_headline_from_sexp ( request ) {
    Ok ( (orgnode_md, level, _title) ) => {
      match orgnode_md.id {
        Some ( id ) => {
          let generated_view : String = block_on ( async {
            match containerward_org_view (
              typedb_driver,
              config,
              &id,
              level ) . await
            { Ok  (s) => s,
              Err (e) => format!(
                "Error generating containerward view: {}", e), }} );
          send_response_with_length_prefix (
            stream,
            & generated_view ); },
        None => {
          let error_msg : String =
            "No ID found in headline metadata".to_string ();
          println! ( "{}", error_msg ) ;
          send_response ( stream, &error_msg ); }} },
    Err ( err ) => {
      let error_msg : String = format!(
        "Error extracting containerward view parameters: {}",
        err);
      println! ( "{}", error_msg ) ;
      send_response ( stream, &error_msg ); }}
}
