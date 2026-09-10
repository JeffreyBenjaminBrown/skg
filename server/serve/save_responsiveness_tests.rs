//! Same-connection progress while an ordinary save worker is paused.

use super::handle_connection;
use crate::runtime::interactive_session::InteractiveSession;
use crate::runtime::query_waits::query_test_runtime;
use crate::runtime::{SelectedRuntimeSnapshot, ServerRuntime};
use crate::runtime::save_operations::socket_tests::Client;
use crate::types::misc::SourceName;
use crate::types::views_state::ViewUri;

use sexp::{Atom, Sexp};
use sha2::{Digest, Sha256};
use std::fs;
use std::net::TcpListener;
use std::sync::mpsc::{sync_channel, Receiver, SyncSender};
use std::sync::{Arc, MutexGuard};
use std::thread::{self, JoinHandle};
use std::path::PathBuf;

#[test]
fn tcp_held_save_keeps_status_and_readonly_query_responsive () {
  for close_target in [false, true] { held_save_with_readonly_query (close_target); }
}

fn held_save_with_readonly_query (close_target : bool) {
  let (temp, runtime, _fixture_operation) :
    (tempfile::TempDir, Arc<ServerRuntime>, String) = query_test_runtime ();
  let listener : TcpListener = TcpListener::bind ("127.0.0.1:0") . unwrap ();
  let port : u16 = listener . local_addr () . unwrap () . port ();
  let server_runtime : Arc<ServerRuntime> = Arc::clone (&runtime);
  let server : JoinHandle<()> = thread::spawn (move || {
    let (stream, _) = listener . accept () . unwrap ();
    handle_connection (stream, server_runtime);
  });
  let mut client : Client = Client::connect_port (port);
  let selected : Arc<SelectedRuntimeSnapshot> = runtime . selected_snapshot ();
  runtime . with_writer_env (|env| *env = selected . env . clone ());
  let (release_sender, holder) : (SyncSender<()>, JoinHandle<()>) =
    hold_writer (&runtime);
  let body : String = save_body ("Held save");
  let (request, operation_id, target_uri) : (String, String, String) =
    new_empty_save_request (&runtime, &body);

  client . send (&request, Some (&body));
  let yielded : Sexp = until_yield (&mut client);
  assert_eq! (field (&yielded, "response-type"), "request-yield", "{}", yielded);
  assert_eq! (field (&yielded, "frame-kind"), "request-yield", "{}", yielded);
  let original_request_id : String = field (&yielded, "request-id");

  client . send ("((request . \"maintenance status\"))", None);
  let status : Sexp = terminal (&mut client);
  assert_eq! (field (&status, "response-type"), "maintenance-status", "{}", status);
  assert_eq! (field (&status, "graph-write-admission"), "closed", "{}", status);

  let query_uri : String = format! ("query:held-save:{}", uuid::Uuid::new_v4 ());
  client . send (&format! (
    "((request . \"single root content view\") (id . \"needle\") (view-uri . \"{}\") (fresh-view . \"true\") (requested-view-write-authority . \"read-only\"))",
    query_uri), None);
  let query : Sexp = terminal (&mut client);
  assert_eq! (field (&query, "view-write-authority"), "read-only", "{}", query);
  assert! (field (&query, "content") . contains ("Retained needle"), "{}", query);

  if close_target {
    client . send (&format! (
      "((request . \"close view\") (view-uri . \"{}\"))", target_uri), None);
    let closed : Sexp = terminal (&mut client);
    assert_eq! (field (&closed, "response-type"), "close-view", "{}", closed);
  }

  release_sender . send (()) . unwrap ();
  holder . join () . unwrap ();
  let completed : Sexp = terminal (&mut client);
  assert_eq! (field (&completed, "response-type"), "save-result", "{}", completed);
  assert_eq! (field (&completed, "request-id"), original_request_id, "{}", completed);
  assert_eq! (field (&completed, "operation-id"), operation_id, "{}", completed);
  assert_eq! (field (&completed, "save-operation-state"), "committed", "{}", completed);
  assert_eq! (field (&completed, "requires-fresh-view"),
    if close_target { "true" } else { "" }, "{}", completed);
  if !close_target {
    assert! (field (&completed, "content") . contains ("Held save"), "{}", completed);
    assert_eq! (field (&completed, "view-write-authority"), "editable", "{}", completed);
  }

  let query_view_uri : ViewUri = ViewUri::from_client_string (query_uri);
  let target_view_uri : ViewUri = ViewUri::from_client_string (target_uri);
  let interactive : MutexGuard<'_, InteractiveSession> =
    runtime . interactive . lock () . unwrap ();
  assert! (interactive . views . open_views . views . contains_key (&query_view_uri));
  assert_eq! (interactive . views . open_views . views . contains_key (&target_view_uri), !close_target);
  drop (interactive);
  let source_path : PathBuf = selected . env . config . sources
    . get (&SourceName::from ("owned")) . unwrap () . path . clone ();
  let saved_bytes : Vec<u8> = fs::read (source_path . join ("needle.skg")) . unwrap ();
  assert! (String::from_utf8_lossy (&saved_bytes) . contains ("Held save"));
  drop (client);
  server . join () . unwrap ();
  drop (temp);
}

#[test]
fn tcp_held_save_close_of_initiating_view_does_not_resurrect_it () {
  let (temp, runtime, _fixture_operation) :
    (tempfile::TempDir, Arc<ServerRuntime>, String) = query_test_runtime ();
  let listener : TcpListener = TcpListener::bind ("127.0.0.1:0") . unwrap ();
  let port : u16 = listener . local_addr () . unwrap () . port ();
  let server_runtime : Arc<ServerRuntime> = Arc::clone (&runtime);
  let server : JoinHandle<()> = thread::spawn (move || {
    let (stream, _) = listener . accept () . unwrap ();
    handle_connection (stream, server_runtime);
  });
  let mut client : Client = Client::connect_port (port);
  let selected : Arc<SelectedRuntimeSnapshot> = runtime . selected_snapshot ();
  runtime . with_writer_env (|env| *env = selected . env . clone ());
  let view_uri : String = format! ("closed-save-{}", uuid::Uuid::new_v4 ());
  let view : Sexp = editable_view (&mut client, &view_uri);
  let body : String = field (&view, "content") . replace ("Retained needle", "Closed save");
  let (request, operation_id) : (String, String) = registered_save_request (
    &runtime, &view_uri, &body, &view);
  let (release_sender, holder) : (SyncSender<()>, JoinHandle<()>) =
    hold_writer (&runtime);

  client . send (&request, Some (&body));
  let yielded : Sexp = until_yield (&mut client);
  assert_eq! (field (&yielded, "response-type"), "request-yield", "{}", yielded);
  assert_eq! (field (&yielded, "frame-kind"), "request-yield", "{}", yielded);

  client . send (&format! (
    "((request . \"close view\") (view-uri . \"{}\"))", view_uri), None);
  let closed : Sexp = terminal (&mut client);
  assert_eq! (field (&closed, "response-type"), "close-view", "{}", closed);

  release_sender . send (()) . unwrap ();
  holder . join () . unwrap ();
  let completed : Sexp = terminal (&mut client);
  assert_eq! (field (&completed, "response-type"), "save-result", "{}", completed);
  assert_eq! (field (&completed, "operation-id"), operation_id, "{}", completed);
  assert_eq! (field (&completed, "save-operation-state"), "committed", "{}", completed);
  assert_eq! (field (&completed, "requires-fresh-view"), "true", "{}", completed);

  let uri : ViewUri = ViewUri::from_client_string (view_uri);
  let interactive : MutexGuard<'_, InteractiveSession> =
    runtime . interactive . lock () . unwrap ();
  assert! (!interactive . views . open_views . views . contains_key (&uri));
  drop (interactive);
  let source_path : PathBuf = selected . env . config . sources
    . get (&SourceName::from ("owned")) . unwrap () . path . clone ();
  let saved_bytes : Vec<u8> = fs::read (source_path . join ("needle.skg")) . unwrap ();
  assert! (String::from_utf8_lossy (&saved_bytes) . contains ("Closed save"));
  drop (client);
  server . join () . unwrap ();
  drop (temp);
}

fn hold_writer (runtime : &Arc<ServerRuntime>) -> (SyncSender<()>, JoinHandle<()>) {
  let (entered_sender, entered_receiver) : (SyncSender<()>, Receiver<()>) =
    sync_channel (0);
  let (release_sender, release_receiver) : (SyncSender<()>, Receiver<()>) =
    sync_channel (0);
  let held_runtime : Arc<ServerRuntime> = Arc::clone (runtime);
  let holder : JoinHandle<()> = thread::spawn (move || {
    held_runtime . with_writer_env (|_| {
      entered_sender . send (()) . unwrap ();
      release_receiver . recv () . unwrap ();
    });
  });
  entered_receiver . recv () . unwrap ();
  (release_sender, holder)
}

fn save_body (title : &str) -> String {
  format! ("* (skg (node (id needle) (source owned))) {}\n", title)
}

fn new_empty_save_request (
  runtime : &Arc<ServerRuntime>,
  body    : &str,
) -> (String, String, String) {
  let operation_id : String = uuid::Uuid::new_v4 () . to_string ();
  let target_uri : String = format! ("new-empty-save-{}", uuid::Uuid::new_v4 ());
  let selected : Arc<SelectedRuntimeSnapshot> = runtime . selected_snapshot ();
  let intent : String = format! (
    "((request . \"save buffer\") (view-uri . \"{}\") (client-buffer-id . \"{}\") (view-kind . \"new-empty-content-view\") (graph-generation . {}) (server-revision . 0) (client-application-token . 1) (server-session-id . \"{}\") (operation-id . \"{}\"))",
    target_uri, uuid::Uuid::new_v4 (),
    selected . selected . graph_generation . get (),
    runtime . server_session_id (), operation_id);
  let (request, operation_id) : (String, String) =
    request_with_fingerprint (intent, body, operation_id);
  (request, operation_id, target_uri)
}

fn registered_save_request (
  runtime  : &Arc<ServerRuntime>,
  view_uri : &str,
  body     : &str,
  view     : &Sexp,
) -> (String, String) {
  let operation_id : String = uuid::Uuid::new_v4 () . to_string ();
  let intent : String = format! (
    "((request . \"save buffer\") (view-uri . \"{}\") (client-buffer-id . \"{}\") (view-kind . \"content-view\") (graph-generation . {}) (server-revision . {}) (client-application-token . {}) (server-session-id . \"{}\") (operation-id . \"{}\"))",
    view_uri, uuid::Uuid::new_v4 (), field (view, "graph-generation"),
    field (view, "server-revision"), field (view, "client-application-token"),
    runtime . server_session_id (), operation_id);
  request_with_fingerprint (intent, body, operation_id)
}

fn request_with_fingerprint (
  intent       : String,
  body         : &str,
  operation_id : String,
) -> (String, String) {
  let mut digest : Sha256 = Sha256::new ();
  digest . update (intent . as_bytes ());
  digest . update ([0]);
  digest . update (body . as_bytes ());
  let fingerprint : String = format! ("{:x}", digest . finalize ());
  let request : String = format! (
    "{} (request-base-fingerprint . \"{}\"))",
    &intent [..intent . len () - 1], fingerprint);
  (request, operation_id)
}

fn editable_view (client : &mut Client, view_uri : &str) -> Sexp {
  client . send (&format! (
    "((request . \"single root content view\") (id . \"needle\") (view-uri . \"{}\") (fresh-view . \"true\") (override-choice . \"bypass\") (requested-view-write-authority . \"editable\"))",
    view_uri), None);
  let view : Sexp = terminal (client);
  assert_eq! (field (&view, "view-write-authority"), "editable", "{}", view);
  view
}

fn field (response : &Sexp, key : &str) -> String {
  let Sexp::List (fields) : &Sexp = response else { return String::new (); };
  fields . iter () . find_map (|field| match field {
    Sexp::List (pair) if pair . len () == 2
      && pair[0] == Sexp::Atom (Atom::S (key . into ())) =>
      match &pair[1] {
        Sexp::Atom (Atom::S (value)) => Some (value . clone ()),
        Sexp::Atom (Atom::I (value)) => Some (value . to_string ()),
        _ => None,
      },
    _ => None,
  }) . unwrap_or_default ()
}

fn terminal (client : &mut Client) -> Sexp {
  loop {
    let response : Sexp = client . receive () . unwrap ();
    if ! field (&response, "terminal-status") . is_empty () { return response; }
  }
}

fn until_yield (client : &mut Client) -> Sexp {
  loop {
    let response : Sexp = client . receive () . unwrap ();
    if field (&response, "response-type") == "request-yield" {
      return response; }
    assert_eq! (field (&response, "response-type"), "save-lock", "{}", response);
  }
}
