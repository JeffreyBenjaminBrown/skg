use crate::serve::util::send_response_with_length_prefix;

use std::net::TcpStream;

pub(crate) trait ResponseSink {
  fn emit (&mut self, response : &str) -> std::io::Result<()>;
}

impl ResponseSink for TcpStream {
  fn emit (&mut self, response : &str) -> std::io::Result<()> {
    send_response_with_length_prefix (self, response)
  }
}

impl ResponseSink for Vec<String> {
  fn emit (&mut self, response : &str) -> std::io::Result<()> {
    self . push (response . to_string ());
    Ok (( ))
  }
}
