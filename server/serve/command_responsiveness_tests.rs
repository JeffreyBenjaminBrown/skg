//! Same-connection progress while a durable command worker is paused.

use super::handle_connection;
use crate::runtime::query_waits::query_test_runtime;
use crate::runtime::save_operations::SaveOperation;
use crate::runtime::save_operations::socket_tests::Client;
use crate::runtime::{MutationControl, SelectedRuntimeSnapshot, ServerRuntime};
use crate::source_sets::ActiveSourceSet;
use sexp::{Atom, Sexp};
use std::net::TcpListener;
use std::sync::mpsc::{sync_channel, Receiver, SyncSender};
use std::sync::Arc;
use std::time::{Duration, Instant};
use std::thread::{self, JoinHandle};
use sha2::{Digest, Sha256};

#[test]
fn tcp_durable_command_yields_for_status_query_and_duplicate_replay () {
  let (_temp, runtime, _fixture_operation) :
    (tempfile::TempDir, Arc<ServerRuntime>, String) = query_test_runtime ();
  let listener : TcpListener = TcpListener::bind ("127.0.0.1:0") . unwrap ();
  let port : u16 = listener . local_addr () . unwrap () . port ();
  let server_runtime : Arc<ServerRuntime> = Arc::clone (&runtime);
  let server : JoinHandle<()> = thread::spawn (move || {
    let (stream, _) = listener . accept () . unwrap ();
    handle_connection (stream, server_runtime);
  });
  let mut client : Client = Client::connect_port (port);

  // The query fixture publishes G2 directly; align the writer adapter with
  // that selected snapshot before exercising a new durable mutation.
  let selected : Arc<SelectedRuntimeSnapshot> = runtime . selected_snapshot ();
  runtime . with_writer_env (|env| *env = selected . env . clone ());

  let (release_sender, holder) : (SyncSender<()>, JoinHandle<()>) = hold_writer (&runtime);

  let operation : String = uuid::Uuid::new_v4 () . to_string ();
  let original : String = format! (
    "((request . \"recompute cyclic roots\") (operation-id . \"{}\"))",
    operation);
  client . send (&original, None);
  let yielded : Sexp = client . receive () . unwrap ();
  assert_eq! (get (&yielded, "response-type"), "request-yield", "{}", yielded);
  assert_eq! (get (&yielded, "frame-kind"), "request-yield", "{}", yielded);
  let original_request_id : String = get (&yielded, "request-id");
  assert! (! original_request_id . is_empty (), "{}", yielded);
  assert_eq! (get (&yielded, "server-session-id"), runtime . server_session_id ());
  assert! (get (&yielded, "terminal-status") . is_empty (), "{}", yielded);

  client . send ("((request . \"maintenance status\"))", None);
  let status : Sexp = terminal (&mut client);
  assert_eq! (get (&status, "response-type"), "maintenance-status", "{}", status);
  assert_eq! (get (&status, "graph-write-admission"), "closed", "{}", status);

  let view_uri : String = format! ("query:responsiveness:{}", uuid::Uuid::new_v4 ());
  client . send (&format! (
    "((request . \"single root content view\") (id . \"needle\") (view-uri . \"{}\") (fresh-view . \"true\") (requested-view-write-authority . \"read-only\"))",
    view_uri), None);
  let view : Sexp = terminal (&mut client);
  assert_eq! (get (&view, "view-write-authority"), "read-only", "{}", view);
  assert! (get (&view, "content") . contains ("Retained needle"), "{}", view);

  let competing : String = format! (
    "((request . \"recompute cyclic roots\") (operation-id . \"{}\"))",
    uuid::Uuid::new_v4 ());
  client . send (&competing, None);
  let refused : Sexp = terminal (&mut client);
  assert_eq! (get (&refused, "save-operation-state"), "blocked", "{}", refused);

  client . send (&original, None);
  let duplicate_before_prepare : Sexp = terminal (&mut client);
  assert_eq! (get (&duplicate_before_prepare, "save-operation-state"),
    "blocked", "{}", duplicate_before_prepare);

  // A different endpoint using the same UUID must be refused without
  // creating a save journal entry that could poison the command in flight.
  let save_body : String = "* (skg (node (id collision) (source main))) save\n".into ();
  let (save_request, save_operation) : (String, SaveOperation) = save_collision_request (
    &operation, &runtime, &save_body);
  client . send (&save_request, Some (&save_body));
  let save_collision : Sexp = terminal (&mut client);
  assert_eq! (get (&save_collision, "response-type"), "save-result",
    "{}", save_collision);
  assert_eq! (get (&save_collision, "save-operation-state"), "blocked",
    "{}", save_collision);
  assert! (save_collision . to_string () . contains ("already reserved"),
    "{}", save_collision);
  assert! (save_operation . status () . unwrap () . is_none ());

  let distinct_operation_id : String = uuid::Uuid::new_v4 () . to_string ();
  let distinct_body : String =
    "* (skg (node (id distinct-collision) (source main))) save\n" . into ();
  let (distinct_request, distinct_operation) : (String, SaveOperation) =
    save_collision_request (&distinct_operation_id, &runtime, &distinct_body);
  client . send (&distinct_request, Some (&distinct_body));
  let distinct_refused : Sexp = terminal (&mut client);
  assert_eq! (get (&distinct_refused, "response-type"), "save-result",
    "{}", distinct_refused);
  assert_eq! (get (&distinct_refused, "save-operation-state"), "refused",
    "{}", distinct_refused);
  let recorded_response : String = distinct_operation . recorded_response ()
    . unwrap () . expect ("distinct refusal should be durably recorded");
  let recorded : Sexp = sexp::parse (&recorded_response) . unwrap ();
  assert_eq! (get (&recorded, "save-operation-state"), "refused", "{}", recorded);
  assert_eq! (get (&recorded, "operation-id"), distinct_operation_id, "{}", recorded);
  assert_eq! (get (&recorded, "request-base-fingerprint"),
    get (&distinct_refused, "request-base-fingerprint"), "{}", recorded);
  assert_eq! (get (&recorded, "content"), get (&distinct_refused, "content"), "{}", recorded);

  release_sender . send (()) . unwrap ();
  holder . join () . unwrap ();

  let completed : Sexp = terminal (&mut client);
  assert_eq! (get (&completed, "response-type"), "recompute-cyclic-roots", "{}", completed);
  assert_eq! (get (&completed, "request-id"), original_request_id, "{}", completed);
  assert_eq! (get (&completed, "operation-id"), operation, "{}", completed);
  assert_eq! (get (&completed, "save-operation-state"), "committed", "{}", completed);

  client . send (&original, None);
  let replay : Sexp = terminal (&mut client);
  assert_eq! (get (&replay, "operation-id"), operation, "{}", replay);
  assert_eq! (get (&replay, "save-operation-state"), "committed", "{}", replay);

  let open : Sexp = maintenance_status (&mut client);
  assert_eq! (get (&open, "graph-write-admission"), "open", "{}", open);

  // Exercise the other durable command through the same held-writer path.
  let selected : Arc<SelectedRuntimeSnapshot> = runtime . selected_snapshot ();
  runtime . with_writer_env (|env| *env = selected . env . clone ());
  let (release_sender, holder) : (SyncSender<()>, JoinHandle<()>) = hold_writer (&runtime);
  let whitespace_operation : String = uuid::Uuid::new_v4 () . to_string ();
  let whitespace : String = format! (
    "((request . \"strip body whitespace\") (operation-id . \"{}\"))",
    whitespace_operation);
  client . send (&whitespace, None);
  let whitespace_yielded : Sexp = client . receive () . unwrap ();
  assert_eq! (get (&whitespace_yielded, "response-type"), "request-yield",
    "{}", whitespace_yielded);
  assert_eq! (get (&whitespace_yielded, "frame-kind"), "request-yield",
    "{}", whitespace_yielded);
  let held : Sexp = maintenance_status (&mut client);
  assert_eq! (get (&held, "graph-write-admission"), "closed", "{}", held);
  release_sender . send (()) . unwrap ();
  holder . join () . unwrap ();
  let whitespace_completed : Sexp = terminal (&mut client);
  assert_eq! (get (&whitespace_completed, "response-type"),
    "strip-body-whitespace", "{}", whitespace_completed);
  assert_eq! (get (&whitespace_completed, "operation-id"),
    whitespace_operation, "{}", whitespace_completed);
  assert_eq! (get (&whitespace_completed, "save-operation-state"),
    "committed", "{}", whitespace_completed);
  let open : Sexp = maintenance_status (&mut client);
  assert_eq! (get (&open, "graph-write-admission"), "open", "{}", open);

  client . send (&whitespace, None);
  let whitespace_replay : Sexp = terminal (&mut client);
  assert_eq! (get (&whitespace_replay, "operation-id"),
    whitespace_operation, "{}", whitespace_replay);
  assert_eq! (get (&whitespace_replay, "save-operation-state"),
    "committed", "{}", whitespace_replay);
  drop (client);
  server . join () . unwrap ();
}

#[test]
fn tcp_durable_command_disconnect_settles_reserved_operation () {
  let (_temp, runtime, _fixture_operation) :
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
  let active : ActiveSourceSet = runtime . interactive . lock () . unwrap ()
    . active_source_set . clone ();
  let operation_id : String = uuid::Uuid::new_v4 () . to_string ();
  let request : String = format! (
    "((request . \"recompute cyclic roots\") (operation-id . \"{}\") (server-session-id . \"{}\"))",
    operation_id, runtime . server_session_id ());
  let operation : SaveOperation = SaveOperation::from_command (
    &request, &selected . env . config, &active) . unwrap ();
  let (release_sender, holder) : (SyncSender<()>, JoinHandle<()>) = hold_writer (&runtime);

  client . send (&request, None);
  let yielded : Sexp = client . receive () . unwrap ();
  assert_eq! (get (&yielded, "response-type"), "request-yield", "{}", yielded);
  assert_eq! (get (&yielded, "frame-kind"), "request-yield", "{}", yielded);
  drop (client);
  server . join () . unwrap ();

  let during : Arc<SelectedRuntimeSnapshot> = runtime . selected_snapshot ();
  assert! (runtime . reserve_mutation (
    "disconnect-probe", during . selected . graph_generation,
    during . selected . manifest_revision) . is_err ());

  release_sender . send (()) . unwrap ();
  holder . join () . unwrap ();
  let deadline : Instant = Instant::now () + Duration::from_secs (5);
  let response : String = loop {
    match operation . recorded_response () {
      Ok (Some (response)) => break response,
      Ok (None) | Err (_) => {
        assert! (Instant::now () < deadline, "disconnected command did not settle");
        thread::sleep (Duration::from_millis (10));
      }
    }
  };
  let settled : Sexp = sexp::parse (&response) . unwrap ();
  assert_eq! (get (&settled, "operation-id"), operation_id, "{}", settled);
  assert_eq! (get (&settled, "save-operation-state"), "committed", "{}", settled);
  // The durable result is recorded before the worker releases its reservation.
  while runtime . publication_with_mutation () . 4 . is_some () {
    assert! (Instant::now () < deadline, "settled command retained its reservation");
    thread::sleep (Duration::from_millis (10));
  }
  let after : Arc<SelectedRuntimeSnapshot> = runtime . selected_snapshot ();
  let released : MutationControl = runtime . reserve_mutation (
    "disconnect-after", after . selected . graph_generation,
    after . selected . manifest_revision) . unwrap ();
  released . finish () . unwrap ();
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

fn maintenance_status (client : &mut Client) -> Sexp {
  client . send ("((request . \"maintenance status\"))", None);
  terminal (client)
}

fn save_collision_request (
  operation_id : &str,
  runtime      : &Arc<ServerRuntime>,
  body         : &str,
) -> (String, SaveOperation) {
  let selected : Arc<SelectedRuntimeSnapshot> = runtime . selected_snapshot ();
  let session : &str = runtime . server_session_id ();
  let intent : String = format! (
    "((request . \"save buffer\") (view-uri . \"collision-view\") (client-buffer-id . \"collision-buffer\") (view-kind . \"new-empty-content-view\") (graph-generation . {}) (server-revision . 0) (client-application-token . 1) (server-session-id . \"{}\") (operation-id . \"{}\"))",
    selected . selected . graph_generation . get (), session, operation_id);
  let mut digest : Sha256 = Sha256::new ();
  digest . update (intent . as_bytes ());
  digest . update ([0]);
  digest . update (body . as_bytes ());
  let fingerprint : String = format! ("{:x}", digest . finalize ());
  let request : String = format! ("{} (request-base-fingerprint . \"{}\"))",
    &intent [..intent . len () - 1], fingerprint);
  let active : ActiveSourceSet = runtime . interactive . lock () . unwrap ()
    . active_source_set . clone ();
  let operation : SaveOperation = SaveOperation::from_request (
    &request, body, &selected . env . config, &active) . unwrap ();
  (request, operation)
}

fn get (response : &Sexp, key : &str) -> String {
  let Sexp::List (fields) : &Sexp = response else { return String::new (); };
  fields . iter () . find_map (|field| match field {
    Sexp::List (pair) if pair . len () == 2
      && pair[0] == Sexp::Atom (Atom::S (key . into ())) =>
      match &pair[1] {
        Sexp::Atom (Atom::S (value)) => Some (value . clone ()) ,
        Sexp::Atom (Atom::I (value)) => Some (value . to_string ()) ,
        _ => None,
      },
    _ => None,
  }) . unwrap_or_default ()
}

fn terminal (client : &mut Client) -> Sexp {
  loop {
    let response : Sexp = client . receive () . unwrap_or_else (|error| {
      panic! ("terminal response was not delivered: {}", error)
    });
    if get (&response, "terminal-status") != "" { return response; }
  }
}
