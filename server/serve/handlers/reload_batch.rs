//! Process-wide brackets for long, externally driven worktree batches.
//!
//! `data/bash/pull-all.sh` opens one control connection, begins a bracket,
//! pulls repositories serially, and closes the same token in its exit trap.
//! Tokens are connection-owned: one driver cannot end another driver's
//! bracket, and losing the connection releases its tokens.

use crate::serve::protocol::TcpToClient;
use crate::serve::util::{
  send_response_with_length_prefix,
  tag_sexp_response,
  value_from_request_sexp,
};

use sexp::{Atom, Sexp};
use std::collections::HashSet;
use std::net::TcpStream;
use std::sync::{Mutex, OnceLock};
use uuid::Uuid;

#[derive(Default)]
struct ReloadBatchRegistry {
  active : HashSet<String>, }

impl ReloadBatchRegistry {
  fn begin (&mut self) -> String {
    let token = Uuid::new_v4 () . to_string ();
    assert! (self . active . insert (token . clone ()),
             "fresh reload-batch UUID collided");
    token
  }

  fn end (&mut self, token : &str) -> bool {
    self . active . remove (token)
  }

  fn active_count (&self) -> usize { self . active . len () }
}

static RELOAD_BATCHES : OnceLock<Mutex<ReloadBatchRegistry>> = OnceLock::new ();

fn registry () -> &'static Mutex<ReloadBatchRegistry> {
  RELOAD_BATCHES . get_or_init (|| Mutex::new (ReloadBatchRegistry::default ()))
}

pub fn reload_batch_active () -> bool {
  registry () . lock () . expect ("reload-batch mutex poisoned")
    . active_count () > 0
}

pub fn handle_begin_reload_batch_request (
  stream       : &mut TcpStream,
  owned_tokens : &mut HashSet<String>,
) {
  let (token, active_count) = {
    let mut state = registry () . lock ()
      . expect ("reload-batch mutex poisoned");
    let token = state . begin ();
    let active_count = state . active_count ();
    (token, active_count) };
  owned_tokens . insert (token . clone ());
  send_batch_response (
    stream, &token, active_count,
    "Reload batch opened; observations will be retained and deferred.");
}

pub fn handle_end_reload_batch_request (
  stream       : &mut TcpStream,
  request      : &str,
  owned_tokens : &mut HashSet<String>,
) {
  let token = match value_from_request_sexp ("batch-token", request) {
    Ok (token) => token,
    Err (error) => {
      send_batch_response (stream, "nil", active_reload_batch_count (),
                           &format! ("Cannot end reload batch: {}", error));
      return; }};
  if ! owned_tokens . remove (&token) {
    send_batch_response (
      stream, &token, active_reload_batch_count (),
      "Cannot end reload batch: this connection does not own that token.");
    return; }
  let active_count = {
    let mut state = registry () . lock ()
      . expect ("reload-batch mutex poisoned");
    state . end (&token);
    state . active_count () };
  send_batch_response (
    stream, &token, active_count,
    if active_count == 0 {
      "Reload batch closed; retained observations may now reconcile."
    } else {
      "Reload batch closed; another reload batch remains active."
    });
}

pub fn release_connection_reload_batches (owned_tokens : &mut HashSet<String>) {
  if owned_tokens . is_empty () { return; }
  let mut state = registry () . lock ()
    . expect ("reload-batch mutex poisoned");
  for token in owned_tokens . drain () {
    state . end (&token); }
  tracing::warn! (
    active_reload_batches = state . active_count (),
    "reload-batch control connection closed; released its tokens");
}

fn active_reload_batch_count () -> usize {
  registry () . lock () . expect ("reload-batch mutex poisoned")
    . active_count ()
}

fn send_batch_response (
  stream       : &mut TcpStream,
  token        : &str,
  active_count : usize,
  message      : &str,
) {
  let atom = |value : &str| Sexp::Atom (Atom::S (value . into ()));
  let payload = Sexp::List (vec![
    Sexp::List (vec![atom ("content"), atom (message)]),
    Sexp::List (vec![atom ("batch-token"), atom (token)]),
    Sexp::List (vec![
      atom ("active-count"), Sexp::Atom (Atom::I (active_count as i64))]),
  ]) . to_string ();
  send_response_with_length_prefix (
    stream, &tag_sexp_response (TcpToClient::ReloadBatch, &payload));
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn brackets_are_tokenized_and_overlap () {
    let mut state = ReloadBatchRegistry::default ();
    let first = state . begin ();
    let second = state . begin ();
    assert_ne! (first, second);
    assert_eq! (state . active_count (), 2);
    assert! (! state . end ("not-a-token"));
    assert! (state . end (&first));
    assert_eq! (state . active_count (), 1);
    assert! (state . end (&second));
    assert_eq! (state . active_count (), 0); }
}
