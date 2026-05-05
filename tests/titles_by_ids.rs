use skg::dbs::init::wipe_then_init_tantivy_db;
use skg::serve::handlers::titles_by_ids::handle_titles_by_ids_request;
use skg::test_utils::read_lp_message;
use skg::types::misc::{ID, MSV, SourceName, TantivyIndex};
use skg::types::nodes::complete::{empty_node_complete, NodeComplete};

use std::error::Error;
use std::net::{TcpListener, TcpStream};
use std::path::Path;

#[test]
fn titles_by_ids_handler_sends_parseable_titles (
) -> Result<(), Box<dyn Error>> {
  let mut node : NodeComplete =
    empty_node_complete ();
  node . pid =
    ID::new ("11111111-1111-4111-8111-111111111111");
  node . title =
    "The Real Title, with spaces" . to_string ();
  node . aliases =
    MSV::Specified (vec!["Alias One" . to_string ()]);
  node . source =
    SourceName::from ("main");
  let (tantivy_index, _indexed_count) : (TantivyIndex, usize) =
    wipe_then_init_tantivy_db (
      &vec![node],
      Path::new ("/tmp/tantivy-test-titles-by-ids-handler"))?;
  let listener : TcpListener =
    TcpListener::bind ("127.0.0.1:0")?;
  let addr =
    listener . local_addr ()?;
  let client : TcpStream =
    TcpStream::connect (addr)?;
  let (mut server, _peer) =
    listener . accept ()?;
  let request : &str =
    "((request . \"titles by ids\") \
      (ids \"11111111-1111-4111-8111-111111111111\"))";
  handle_titles_by_ids_request (
    &mut server,
    request,
    &tantivy_index );
  drop (server);
  let mut reader =
    std::io::BufReader::new (client);
  let response : String =
    read_lp_message (&mut reader)?;
  let parsed : sexp::Sexp =
    sexp::parse (&response)?;
  let response_text : String =
    parsed . to_string ();
  assert! (
    response_text . contains ("titles-by-ids"),
    "response should be tagged as titles-by-ids: {}",
    response_text );
  assert! (
    response_text . contains ("The Real Title, with spaces"),
    "response should preserve the title: {}",
    response_text );
  Ok (( )) }
