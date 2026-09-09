//! Actual TCP registration and lost-ACK replay, sharing the save wire client.

use super::handle_connection;
use crate::maintenance::query_waits::{QueryWaitRecord, QueryWaitResult, QueryWaitTarget};
use crate::runtime::query_waits::query_test_runtime;
use crate::runtime::ServerRuntime;
use crate::runtime::save_operations::socket_tests::Client;
use crate::types::sexp::atom_to_string;
use crate::types::views_state::ViewUri;

use sexp::{Atom, Sexp};
use sha2::{Digest, Sha256};
use std::net::{SocketAddr, TcpListener, TcpStream};
use std::sync::Arc;
use std::thread::JoinHandle;

#[test]
fn query_wait_tcp_reconnect_replays_exact_result_and_ack_keeps_it_read_only () {
  let (_temp, runtime, fixture_operation) :
    (tempfile::TempDir, Arc<ServerRuntime>, String) = query_test_runtime ();
  let fixture : QueryWaitRecord = runtime . maintenance_snapshot () . query_waits
    . get (&fixture_operation) . unwrap () . clone ();
  runtime . start_query_wait_worker () . unwrap ();
  let listener : TcpListener = TcpListener::bind ("127.0.0.1:0") . unwrap ();
  let port : u16 = listener . local_addr () . unwrap () . port ();
  let server_runtime = Arc::clone (&runtime);
  let server : JoinHandle<()> = std::thread::spawn (move || {
    for _ in 0..2 {
      let (stream, _) = listener . accept () . unwrap ();
      handle_connection (stream, Arc::clone (&server_runtime));
    }
  });
  let operation : String = uuid::Uuid::new_v4 () . to_string ();
  let buffer : String = uuid::Uuid::new_v4 () . to_string ();
  let uri : String = format! ("search:wait:{}", operation);
  let recipe : String = "((kind . \"text-search\") (terms . \"needle\") (regex . \"false\") (body . \"true\") (operators . \"false\") (ugly-telescopes . \"default\") (source-set . \"all\"))" . into ();
  let recipe_digest : String = digest (&recipe);
  let mut fields : Vec<Sexp> = vec![field ("request", "query wait"), field ("query-operation-id", &operation),
    field ("outcome-kind", "graph-publication"), field ("query-recipe", &recipe),
    field ("query-recipe-digest", &recipe_digest), field ("view-uri", &uri), field ("client-buffer-id", &buffer),
    field ("source-set", "all"), field ("client-application-token", "1"), field ("graph-generation", "0"),
    field ("presentation-generation", "0"), field ("server-revision", "0"),
    field ("base-content-sha256", &digest ("Waiting for reconciliation.\n")), field ("destination-state", "clean")];
  let QueryWaitTarget::Incident { incident_id, epoch } = fixture . target else { unreachable! (); };
  fields . extend ([field ("incident-id", incident_id . as_str ()), field ("maintenance-epoch", &epoch . get () . to_string ())]);
  let request : String = Sexp::List (fields) . to_string ();
  let mut client : Client = Client::connect_port (port);
  client . send (&request, None);
  let accepted : Sexp = client . terminal ();
  assert_eq! (get (&accepted, "response-type"), "query-wait-status", "{}", accepted);
  assert_ne! (get (&accepted, "status"), "refused", "{}", accepted);
  let first : Sexp = next_result (&mut client);
  assert_eq! (get (&first, "expected-graph-generation"), "0");
  assert_eq! (get (&first, "graph-generation"), "2");
  assert_eq! (get (&first, "view-write-authority"), "read-only");
  assert_eq! (get (&first, "result-digest"), digest (&get (&first, "content")));
  assert! (get (&first, "content") . contains ("Retained needle"));
  drop (client);

  let mut client : Client = Client::connect_port (port);
  client . send (&Sexp::List (vec![field ("request", "query wait status"),
    field ("query-operation-id", &operation)]) . to_string (), None);
  let status : Sexp = client . terminal ();
  assert_eq! (get (&status, "query-recipe"), recipe);
  assert_eq! (get (&status, "query-recipe-digest"), recipe_digest);
  let mut applied : Vec<Sexp> = vec![field ("query-operation-id", &operation), field ("view-uri", &uri),
    field ("client-buffer-id", &buffer), field ("source-set", "all"), field ("destination-state", "clean"),
    field ("base-content-sha256", &get (&first, "result-digest")),
    field ("query-recipe-digest", &recipe_digest),
    field ("client-application-token", &get (&first, "resulting-client-application-token")),
    field ("graph-generation", &get (&first, "graph-generation")),
    field ("presentation-generation", &get (&first, "presentation-generation")),
    field ("server-revision", &get (&first, "server-revision"))];
  let mut request : Vec<Sexp> = applied . clone ();
  request . push (field ("request", "query wait status"));
  client . send (&Sexp::List (request) . to_string (), None);
  assert_eq! (get (&client . terminal (), "status"), "ready");
  let replay : Sexp = next_result (&mut client);
  assert_eq! (get (&replay, "content"), get (&first, "content"));
  assert_eq! (get (&replay, "result-digest"), get (&first, "result-digest"));
  applied . extend ([field ("request", "query wait applied"), field ("applied", "true"),
    field ("result-digest", &get (&replay, "result-digest"))]);
  let ack : String = Sexp::List (applied) . to_string ();
  for _ in 0..2 {
    client . send (&ack, None);
    let acknowledged : Sexp = client . terminal ();
    assert_eq! (get (&acknowledged, "response-type"), "query-wait-applied", "{}", acknowledged);
    assert_eq! (get (&acknowledged, "status"), "delivered");
  }
  let interactive = runtime . interactive . lock () . unwrap ();
  let view = interactive . views . open_views . views . get (&ViewUri::from_client_string (uri)) . unwrap ();
  assert! (!view . writes_admitted);
  assert! (view . save_base . is_none ());
  assert_eq! (view . client_application_token, 2);
  assert_eq! (view . revision, 1);
  drop (interactive);
  drop (client);
  server . join () . unwrap ();
}

fn next_result (client : &mut Client) -> Sexp {
  for _ in 0..30 {
    let response : Sexp = client . receive () . unwrap ();
    if get (&response, "response-type") == "query-wait-result" { return response; }
    if get (&response, "response-type") == "query-wait-status" {
      assert! (!matches! (get (&response, "status") . as_str (), "failed" | "blocked" | "refused"), "{}", response);
    }
  }
  panic! ("query result was not delivered");
}

fn get (response : &Sexp, key : &str) -> String {
  let Sexp::List (fields) : &Sexp = response else { return String::new (); };
  fields . iter () . find_map (|field| match field {
    Sexp::List (pair) if pair . len () == 2 && pair[0] == Sexp::Atom (Atom::S (key . into ())) =>
      atom_to_string (&pair[1]) . ok (),
    _ => None,
  }) . unwrap_or_default ()
}
fn digest (value : &str) -> String { format! ("{:x}", Sha256::digest (value . as_bytes ())) }
fn field (key : &str, value : &str) -> Sexp {
  Sexp::List (vec![Sexp::Atom (Atom::S (key . into ())), Sexp::Atom (Atom::S ("." . into ())), Sexp::Atom (Atom::S (value . into ()))])
}

#[test]
fn query_wait_tcp_destination_mismatch_and_dirty_buffer_withhold_result () {
  let (_temp, runtime, fixture_operation) :
    (tempfile::TempDir, Arc<ServerRuntime>, String) = query_test_runtime ();
  let fixture : QueryWaitRecord = runtime . maintenance_snapshot () . query_waits
    . get (&fixture_operation) . unwrap () . clone ();
  let listener : TcpListener = TcpListener::bind ("127.0.0.1:0") . unwrap ();
  let port : u16 = listener . local_addr () . unwrap () . port ();
  let server_runtime : Arc<ServerRuntime> = Arc::clone (&runtime);
  let server : JoinHandle<()> = std::thread::spawn (move || {
    let (stream, _peer) : (TcpStream, SocketAddr) =
      listener . accept () . unwrap ();
    handle_connection (stream, server_runtime);
  });
  let mut client : Client = Client::connect_port (port);
  runtime . start_query_wait_worker () . unwrap ();

  let mismatch_operation : String = uuid::Uuid::new_v4 () . to_string ();
  let mismatch_buffer : String = uuid::Uuid::new_v4 () . to_string ();
  client . send (&registration_request (
    &fixture, &mismatch_operation, &mismatch_buffer, "clean"), None);
  let accepted : Sexp = terminal_without_result (&mut client);
  assert_ne! (get (&accepted, "status"), "refused", "{}", accepted);
  client . send (&destination_status_request (
    &mismatch_operation, &mismatch_buffer, "999", "clean"), None);
  let mismatch : Sexp = terminal_without_result (&mut client);
  assert_ne! (get (&mismatch, "status"), "refused", "{}", mismatch);
  wait_until_ready (&mut client, &mismatch_operation);
  assert_no_query_view (&runtime, &mismatch_operation);

  let dirty_operation : String = uuid::Uuid::new_v4 () . to_string ();
  let dirty_buffer : String = uuid::Uuid::new_v4 () . to_string ();
  client . send (&registration_request (
    &fixture, &dirty_operation, &dirty_buffer, "clean"), None);
  let accepted : Sexp = terminal_without_result (&mut client);
  assert_ne! (get (&accepted, "status"), "refused", "{}", accepted);
  client . send (&destination_status_request (
    &dirty_operation, &dirty_buffer, "0", "dirty"), None);
  let dirty : Sexp = terminal_without_result (&mut client);
  assert_ne! (get (&dirty, "status"), "refused", "{}", dirty);
  wait_until_ready (&mut client, &dirty_operation);
  assert_no_query_view (&runtime, &dirty_operation);
  drop (client);
  server . join () . unwrap ();
}

#[test]
fn query_wait_tcp_cancelled_ready_result_rejects_ack_without_view () {
  let (_temp, runtime, fixture_operation) :
    (tempfile::TempDir, Arc<ServerRuntime>, String) = query_test_runtime ();
  let fixture : QueryWaitRecord = runtime . maintenance_snapshot () . query_waits
    . get (&fixture_operation) . unwrap () . clone ();
  let listener : TcpListener = TcpListener::bind ("127.0.0.1:0") . unwrap ();
  let port : u16 = listener . local_addr () . unwrap () . port ();
  let server_runtime : Arc<ServerRuntime> = Arc::clone (&runtime);
  let server : JoinHandle<()> = std::thread::spawn (move || {
    let (stream, _peer) : (TcpStream, SocketAddr) =
      listener . accept () . unwrap ();
    handle_connection (stream, server_runtime);
  });
  let mut client : Client = Client::connect_port (port);
  let operation : String = uuid::Uuid::new_v4 () . to_string ();
  let buffer : String = uuid::Uuid::new_v4 () . to_string ();
  client . send (&registration_request (
    &fixture, &operation, &buffer, "clean"), None);
  let accepted : Sexp = terminal_without_result (&mut client);
  assert_ne! (get (&accepted, "status"), "refused", "{}", accepted);
  client . send (&destination_status_request (
    &operation, &buffer, "999", "clean"), None);
  let mismatch : Sexp = terminal_without_result (&mut client);
  assert_ne! (get (&mismatch, "status"), "refused", "{}", mismatch);
  runtime . start_query_wait_worker () . unwrap ();
  let ready : Sexp = wait_until_ready (&mut client, &operation);
  assert_eq! (get (&ready, "status"), "ready", "{}", ready);
  let stored : QueryWaitRecord = runtime . maintenance_snapshot () . query_waits
    . get (&operation) . unwrap () . clone ();
  let result : QueryWaitResult =
    stored . result . clone () . expect ("ready query result");

  client . send (&Sexp::List (vec![field ("request", "query wait cancel"),
    field ("query-operation-id", &operation)]) . to_string (), None);
  let cancelled : Sexp = terminal_without_result (&mut client);
  assert_eq! (get (&cancelled, "status"), "cancelled", "{}", cancelled);
  let applied : String = applied_request (&stored, &result);
  client . send (&applied, None);
  let refused : Sexp = terminal_without_result (&mut client);
  assert_eq! (get (&refused, "status"), "refused", "{}", refused);
  assert_no_query_view (&runtime, &operation);
  drop (client);
  server . join () . unwrap ();
}

fn registration_request (
  fixture       : &QueryWaitRecord,
  operation_id  : &str,
  buffer_id     : &str,
  destination_state : &str,
) -> String {
  let recipe : String = "((kind . \"text-search\") (terms . \"needle\") (regex . \"false\") (body . \"true\") (operators . \"false\") (ugly-telescopes . \"default\") (source-set . \"all\"))" . into ();
  let recipe_digest : String = digest (&recipe);
  let QueryWaitTarget::Incident { incident_id, epoch } = &fixture . target else { unreachable! (); };
  Sexp::List (vec![field ("request", "query wait"), field ("query-operation-id", operation_id),
    field ("incident-id", incident_id . as_str ()), field ("maintenance-epoch", &epoch . get () . to_string ()),
    field ("outcome-kind", "graph-publication"), field ("query-recipe", &recipe),
    field ("query-recipe-digest", &recipe_digest), field ("view-uri", &format! ("search:wait:{}", operation_id)),
    field ("client-buffer-id", buffer_id), field ("source-set", "all"),
    field ("client-application-token", "1"), field ("graph-generation", "0"),
    field ("presentation-generation", "0"), field ("server-revision", "0"),
    field ("base-content-sha256", &digest ("Waiting for reconciliation.\n")),
    field ("destination-state", destination_state)]) . to_string ()
}

fn destination_status_request (
  operation_id : &str,
  buffer_id    : &str,
  graph        : &str,
  state        : &str,
) -> String {
  Sexp::List (vec![field ("request", "query wait status"), field ("query-operation-id", operation_id),
    field ("view-uri", &format! ("search:wait:{}", operation_id)), field ("client-buffer-id", buffer_id),
    field ("source-set", "all"), field ("client-application-token", "1"), field ("graph-generation", graph),
    field ("presentation-generation", "0"), field ("server-revision", "0"),
    field ("base-content-sha256", &digest ("Waiting for reconciliation.\n")), field ("destination-state", state)]) . to_string ()
}

fn wait_until_ready (client : &mut Client, operation_id : &str) -> Sexp {
  for _ in 0..100 {
    client . send (&Sexp::List (vec![field ("request", "query wait status"),
      field ("query-operation-id", operation_id)]) . to_string (), None);
    let status : Sexp = terminal_without_result (client);
    if get (&status, "status") == "ready" { return status; }
    assert_ne! (get (&status, "status"), "refused", "{}", status);
    std::thread::sleep (std::time::Duration::from_millis (10));
  }
  panic! ("query wait did not become ready")
}

fn terminal_without_result (client : &mut Client) -> Sexp {
  loop {
    let response : Sexp = client . receive () . unwrap ();
    assert_ne! (get (&response, "response-type"), "query-wait-result", "unexpected result: {}", response);
    if get (&response, "terminal-status") == "complete" { return response; }
  }
}

fn applied_request (record : &QueryWaitRecord, result : &QueryWaitResult) -> String {
  let recipe : &str = record . client_recipe . as_deref () . unwrap ();
  Sexp::List (vec![field ("request", "query wait applied"), field ("query-operation-id", &record . operation_id),
    field ("applied", "true"), field ("view-uri", &record . destination . view_uri),
    field ("client-buffer-id", record . destination . client_buffer_id . as_deref () . unwrap ()),
    field ("source-set", &result . source_set), field ("destination-state", "clean"),
    field ("base-content-sha256", &result . content_sha256), field ("query-recipe-digest", &digest (recipe)),
    field ("client-application-token", &record . destination . base_application_token . checked_add (1) . unwrap () . to_string ()),
    field ("graph-generation", &result . graph_generation . to_string ()),
    field ("presentation-generation", &result . presentation_generation . to_string ()),
    field ("server-revision", &record . destination . base_server_revision . checked_add (1) . unwrap () . to_string ()),
    field ("result-digest", &result . content_sha256)]) . to_string ()
}

fn assert_no_query_view (runtime : &Arc<ServerRuntime>, operation_id : &str) {
  let uri : ViewUri = ViewUri::from_client_string (format! ("search:wait:{}", operation_id));
  assert! (!runtime . interactive . lock () . unwrap () . views . open_views
    . views . contains_key (&uri));
}
