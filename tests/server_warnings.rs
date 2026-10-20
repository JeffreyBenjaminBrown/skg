// The startup warnings: 'load_config' warns about the top-level keys
// of skgconfig.toml it does not know, and the "server warnings"
// request serves what the server recorded.
// Fixture: 'tests/server_warnings/fixtures/skgconfig.toml', a config
// with no skgrepos that sets an old spelling and an unknown key.

use skg::dbs::filesystem::not_nodes::load_config;
use skg::serve::handlers::server_warnings::handle_server_warnings_request;
use skg::serve::protocol::RequestType;
use skg::test_utils::read_lp_message;
use skg::types::misc::SkgConfig;

use std::io::BufReader;
use std::net::{TcpListener, TcpStream};

#[test]
fn an_old_key_spelling_is_warned_about_by_its_new_spelling_and_the_config_still_loads (
) -> Result<(), Box<dyn std::error::Error>> {
  let (config, warnings) : (SkgConfig, Vec<String>) =
    load_config ("tests/server_warnings/fixtures/skgconfig.toml") ?;
  assert_eq! (config . max_roleTree_depth, 20,
              "the old spelling has no effect, so the default holds");
  assert_eq! (warnings . len (), 2, "{:?}", warnings);
  let about_the_old_spelling : &String =
    warnings . iter ()
    . find ( |warning| warning . contains ("'max_role_tree_depth'") )
    . expect ("a warning names the old spelling");
  assert! (about_the_old_spelling . contains ("'max_roleTree_depth'"),
           "{}", about_the_old_spelling);
  assert! (warnings . iter ()
           . any ( |warning| warning . contains ("'no_such_key'") ),
           "{:?}", warnings);
  Ok (( )) }

#[test]
fn the_server_warnings_request_answers_with_each_recorded_warning (
) -> Result<(), Box<dyn std::error::Error>> {
  assert_eq! (RequestType::from_client_string ("server warnings"),
              Ok (RequestType::ServerWarnings));
  let warnings : Vec<String> = vec! [
    "skgconfig.toml sets 'a', a key skg does not know, so it has no effect." . to_string (),
    "a \"quoted\" word" . to_string () ];
  assert_eq! (
    served_warnings_response (&warnings) ?,
    "((response-type server-warnings) (warnings (\"skgconfig.toml sets 'a', a key skg does not know, so it has no effect.\" \"a \\\"quoted\\\" word\")))" );
  assert_eq! (
    served_warnings_response (&[]) ?,
    "((response-type server-warnings) (warnings ()))" );
  Ok (( )) }

/// What 'handle_server_warnings_request' writes for WARNINGS, read
/// back as one length-prefixed message.
fn served_warnings_response (
  warnings : &[String],
) -> Result<String, Box<dyn std::error::Error>> {
  let listener : TcpListener = TcpListener::bind ("127.0.0.1:0") ?;
  let mut write_end : TcpStream =
    TcpStream::connect (listener . local_addr () ?) ?;
  let (read_end, _) : (TcpStream, std::net::SocketAddr) =
    listener . accept () ?;
  handle_server_warnings_request (&mut write_end, warnings);
  drop (write_end);
  let mut reader : BufReader<TcpStream> = BufReader::new (read_end);
  read_lp_message (&mut reader) }
