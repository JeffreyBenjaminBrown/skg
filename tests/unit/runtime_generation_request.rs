//! Unit tests for the 'runtime generation' endpoint
//! ('runtime_generation_response'; 'docs/api-and-formats.org',
//! "Runtime generation").

use super::runtime_generation_response;
use crate::serve::protocol::RequestType;

#[test]
fn runtime_generation_request_answers_the_number_quoted () {
  assert_eq! ( RequestType::from_client_string ("runtime generation"),
               Ok (RequestType::RuntimeGeneration) );
  assert_eq! ( runtime_generation_response (1762000000000042),
               r#"((response-type runtime-generation) (content "1762000000000042"))"# ); }
