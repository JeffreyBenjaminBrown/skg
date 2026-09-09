//! Real TCP saves and fresh-process recovery. Fault hooks exist only in the
//! library test binary; every crash terminates without unwinding or cleanup.

use super::{commit_recovered_source_effects, recover_source_effects_before_startup};
use crate::context::{ContextComputation, compute_and_store_context_types};
use crate::dbs::filesystem::not_nodes::load_config;
use crate::dbs::init::{InitContextHandoff, initialize_dbs};
use crate::runtime::ServerRuntime;
use crate::runtime::save_operations::StartupSaveRecovery;
use crate::serve::{prepare_runtime, serve};
use crate::types::env::SkgEnv;
use crate::types::misc::SkgConfig;
use crate::types::nodes::complete::NodeComplete;
use crate::types::sexp::atom_to_string;
use crate::types::store_state::SelectedStoreState;

use sexp::{Atom, Sexp};
use sha2::{Digest, Sha256};
use std::fs::{self, File};
use std::io::{BufRead, BufReader, Read, Write};
use std::net::{TcpListener, TcpStream};
use std::path::PathBuf;
use std::process::{Child, Command, ExitStatus, Stdio};
use std::sync::Arc;
use std::time::{Duration, Instant};

#[cfg(unix)]
use std::os::unix::process::ExitStatusExt;

const WORKER_ROOT : &str = "SKG_SOCKET_SAVE_TEST_ROOT";
const CRASH_POINT : &str = "SKG_SOCKET_SAVE_TEST_CRASH";
const REPORT_HOLD : &str = "SKG_SOCKET_SAVE_TEST_REPORT_HOLD";
const REBUILD_HOLD : &str = "SKG_SOCKET_SAVE_TEST_REBUILD_HOLD";
const WORKER_NAME : &str =
  "runtime::save_operations::socket_tests::socket_process_worker";

struct Fixture {
  root : PathBuf,
  port : u16,
}

struct ServerProcess {
  child : Child,
  log : PathBuf,
}

struct Client {
  stream : TcpStream,
  reader : BufReader<TcpStream>,
  session : String,
  generation : u64,
}

#[test]
fn socket_saves_recover_at_every_authorized_boundary () {
  for boundary in ["after-staging", "after-authorization", "after-path-1",
                   "after-path-2", "after-application", "after-publication",
                   "after-commit"] {
    let fixture : Fixture = Fixture::new ();
    let mut process : ServerProcess = fixture . start (boundary);
    let mut client : Client = Client::connect (&fixture);
    let old_session : String = client . session . clone ();
    let (request, fingerprint, operation) : (String, String, String) =
      client . save_request ("Alpha", "Beta");
    client . send (&request, Some (&save_body ("Alpha", "Beta")));
    while let Ok (response) = client . receive () {
      assert_ne! (field (&response, "save-operation-state") . as_deref (),
        Some ("committed"), "{} acknowledged before process death", boundary); }
    let status : ExitStatus = process . child . wait () . unwrap ();
    #[cfg(unix)]
    assert_eq! (status . signal (), Some (libc::SIGKILL),
      "{} did not reach its crash hook: {}", boundary, process . logs ());
    #[cfg(not(unix))]
    assert! (!status . success ());
    if boundary == "after-staging" || boundary == "after-authorization" {
      assert! (!fixture . node ("a") . exists ());
      assert! (!fixture . node ("b") . exists ()); }
    if boundary == "after-path-1" {
      assert_ne! (fixture . node ("a") . exists (), fixture . node ("b") . exists (),
        "the crash must interrupt a real multi-file batch"); }
    drop (client);
    drop (process);
    let _restarted : ServerProcess = fixture . start ("none");
    let mut recovered : Client = Client::connect (&fixture);
    assert_ne! (recovered . session, old_session);
    let response : Sexp = recovered . operation_status (&operation, &fingerprint);
    let expected : &str = if boundary == "after-staging" { "refused" } else { "committed" };
    assert_eq! (field (&response, "state") . as_deref (), Some (expected), "{}: {}", boundary, response);
    if boundary == "after-staging" {
      assert! (!fixture . node ("a") . exists ());
      assert! (!fixture . node ("b") . exists ());
    } else {
      assert! (fs::read_to_string (fixture . node ("a")) . unwrap () . contains ("Alpha"));
      assert! (fs::read_to_string (fixture . node ("b")) . unwrap () . contains ("Beta")); }
    let (new_request, _, _) : (String, String, String) =
      recovered . save_request ("NewerAlpha", "NewerBeta");
    recovered . send (&new_request, Some (&save_body ("NewerAlpha", "NewerBeta")));
    let newer : Sexp = recovered . terminal ();
    assert_eq! (field (&newer, "save-operation-state") . as_deref (), Some ("committed"), "{}", newer);
    let newest_a : Vec<u8> = fs::read (fixture . node ("a")) . unwrap ();
    let newest_b : Vec<u8> = fs::read (fixture . node ("b")) . unwrap ();
    recovered . send (&request, Some (&save_body ("Alpha", "Beta")));
    let duplicate : Sexp = recovered . terminal ();
    assert_eq! (field (&duplicate, "operation-id") . as_deref (), Some (operation . as_str ()));
    assert_eq! (fs::read (fixture . node ("a")) . unwrap (), newest_a,
      "{} replayed an old save over newer bytes", boundary);
    assert_eq! (fs::read (fixture . node ("b")) . unwrap (), newest_b);
  }
}

#[test]
fn socket_protocol_mismatch_cannot_inherit_a_previous_client () {
  let fixture : Fixture = Fixture::new ();
  let _process : ServerProcess = fixture . start ("none");
  let mut stream : TcpStream = TcpStream::connect (("127.0.0.1", fixture . port)) . unwrap ();
  stream . set_read_timeout (Some (Duration::from_secs (3))) . unwrap ();
  stream . write_all (b"((request . \"verify connection\") (request-id . \"old\") (role . \"interactive\") (protocol-version . 1))\n") . unwrap ();
  let response : Sexp = read_response (&mut BufReader::new (stream)) . unwrap ();
  assert! (field (&response, "content") . unwrap () . contains ("incompatible Skg protocol"));
  let mut client : Client = Client::connect (&fixture);
  let (request, _, _) : (String, String, String) = client . save_request ("Alpha", "Beta");
  client . send (&request, Some (&save_body ("Alpha", "Beta")));
  assert_eq! (field (&client . terminal (), "save-operation-state") . as_deref (), Some ("committed"));
}

#[test]
fn barrier_query_remains_read_only_after_admission_reopens () {
  let fixture : Fixture = Fixture::new ();
  let _process : ServerProcess = fixture . start ("none");
  let mut client : Client = Client::connect (&fixture);
  let (request, _, _) : (String, String, String) = client . save_request ("Alpha", "Beta");
  client . send (&request, Some (&save_body ("Alpha", "Beta")));
  let saved : Sexp = client . terminal ();
  assert_eq! (field (&saved, "save-operation-state") . as_deref (), Some ("committed"));
  client . send ("((request . \"begin maintenance\") (origin . \"explicit-partial-reload\") (ids \"a\"))", None);
  let offer : Sexp = client . terminal ();
  let incident : String = field (&offer, "allocated-incident-id") . unwrap_or_else (|| panic! ("maintenance incident: {}", offer));
  let epoch : String = field (&offer, "maintenance-epoch") . expect ("maintenance epoch");
  let uri : String = uuid::Uuid::new_v4 () . to_string ();
  client . send (&format! (
    "((request . \"single root content view\") (id . \"a\") (view-uri . \"{}\") (fresh-view . \"true\") (requested-view-write-authority . \"editable\"))", uri), None);
  let view : Sexp = client . terminal ();
  assert_eq! (field (&view, "view-write-authority") . as_deref (), Some ("read-only"), "{}", view);
  assert_eq! (field (&view, "graph-write-admission") . as_deref (), Some ("closed"));
  client . send (&format! (
    "((request . \"cancel maintenance\") (incident-id . \"{}\") (maintenance-epoch . {}))", incident, epoch), None);
  let cancelled : Sexp = client . terminal ();
  assert_eq! (field (&cancelled, "status") . as_deref (), Some ("cancelled-before-archive"), "{}", cancelled);
  client . send ("((request . \"maintenance status\"))", None);
  assert_eq! (field (&client . terminal (), "graph-write-admission") . as_deref (), Some ("open"));
  let body : String = field (&view, "content") . expect ("read-only content") . replace ("Alpha", "UnauthorizedAlpha");
  let intent : String = format! (
    "((request . \"save buffer\") (view-uri . \"{}\") (client-buffer-id . \"{}\") (view-kind . \"content-view\") (graph-generation . {}) (server-revision . {}) (client-application-token . {}) (server-session-id . \"{}\") (operation-id . \"{}\"))",
    uri, uuid::Uuid::new_v4 (), field (&view, "graph-generation") . unwrap (),
    field (&view, "server-revision") . unwrap (), field (&view, "client-application-token") . unwrap (),
    client . session, uuid::Uuid::new_v4 ());
  let mut digest : Sha256 = Sha256::new ();
  digest . update (intent . as_bytes ()); digest . update ([0]); digest . update (body . as_bytes ());
  let forged : String = format! ("{} (request-base-fingerprint . \"{:x}\"))", &intent[..intent . len () - 1], digest . finalize ());
  client . send (&forged, Some (&body));
  let refused : Sexp = client . terminal ();
  assert_eq! (field (&refused, "save-operation-state") . as_deref (), Some ("refused"), "{}", refused);
  let bytes : String = fs::read_to_string (fixture . node ("a")) . unwrap ();
  assert! (bytes . contains ("Alpha") && !bytes . contains ("UnauthorizedAlpha"));
}

#[test]
fn socket_registered_old_editable_views_cross_an_unrelated_save () {
  let fixture : Fixture = Fixture::new ();
  let _process : ServerProcess = fixture . start ("none");
  let mut client : Client = Client::connect (&fixture);
  let (initial_request, _, _) : (String, String, String) =
    client . save_request ("Alpha", "Beta");
  client . send (&initial_request, Some (&save_body ("Alpha", "Beta")));
  let initial : Sexp = client . terminal ();
  assert_eq! (field (&initial, "save-operation-state") . as_deref (),
    Some ("committed"), "{}", initial);

  let uri_a : String = format! ("view-a-{}", uuid::Uuid::new_v4 ());
  let uri_b : String = format! ("view-b-{}", uuid::Uuid::new_v4 ());
  let view_a : Sexp = editable_content_view (&mut client, "a", &uri_a);
  let view_b : Sexp = editable_content_view (&mut client, "b", &uri_b);
  let a_generation : u64 = view_field (&view_a, "graph-generation");
  let a_revision : u64 = view_field (&view_a, "server-revision");
  let a_token : u64 = view_field (&view_a, "client-application-token");
  let a_body : String = view_content (&view_a) . replace ("Alpha", "OlderAlpha");
  let b_body : String = view_content (&view_b) . replace ("Beta", "NewerBeta");
  let b_result : Sexp = save_registered_view (
    &mut client, &uri_b, a_buffer_id (), view_field (&view_b, "graph-generation"),
    view_field (&view_b, "server-revision"),
    view_field (&view_b, "client-application-token"), &b_body);
  assert_eq! (field (&b_result, "save-operation-state") . as_deref (),
    Some ("committed"), "{}", b_result);
  let newer_b_bytes : Vec<u8> = fs::read (fixture . node ("b")) . unwrap ();
  let a_result : Sexp = save_registered_view (
    &mut client, &uri_a, a_buffer_id (), a_generation, a_revision, a_token, &a_body);
  assert_eq! (field (&a_result, "save-operation-state") . as_deref (),
    Some ("committed"), "{}", a_result);
  let a_bytes : String = fs::read_to_string (fixture . node ("a")) . unwrap ();
  let b_bytes : Vec<u8> = fs::read (fixture . node ("b")) . unwrap ();
  assert! (a_bytes . contains ("OlderAlpha"), "{}", a_bytes);
  assert_eq! (b_bytes, newer_b_bytes, "A save changed newer B bytes");
  assert! (String::from_utf8_lossy (&b_bytes) . contains ("NewerBeta"));
  let search : Sexp = client . text_search ("NewerBeta");
  assert! (field (&search, "content") . unwrap_or_default ()
    . contains ("NewerBeta"), "{}", search);
}

#[test]
fn socket_registered_old_view_is_refused_after_same_pid_save () {
  let fixture : Fixture = Fixture::new ();
  let _process : ServerProcess = fixture . start ("none");
  let mut client : Client = Client::connect (&fixture);
  let (initial_request, _, _) : (String, String, String) =
    client . save_request ("Alpha", "Beta");
  client . send (&initial_request, Some (&save_body ("Alpha", "Beta")));
  let initial : Sexp = client . terminal ();
  assert_eq! (field (&initial, "save-operation-state") . as_deref (),
    Some ("committed"), "{}", initial);

  let old_uri : String = format! ("old-a-{}", uuid::Uuid::new_v4 ());
  let fresh_uri : String = format! ("fresh-a-{}", uuid::Uuid::new_v4 ());
  let old_view : Sexp = editable_content_view (&mut client, "a", &old_uri);
  let fresh_view : Sexp = editable_content_view (&mut client, "a", &fresh_uri);
  let fresh_body : String = view_content (&fresh_view)
    . replace ("Alpha", "FreshAlpha");
  let fresh_result : Sexp = save_registered_view (
    &mut client, &fresh_uri, a_buffer_id (),
    view_field (&fresh_view, "graph-generation"),
    view_field (&fresh_view, "server-revision"),
    view_field (&fresh_view, "client-application-token"), &fresh_body);
  assert_eq! (field (&fresh_result, "save-operation-state") . as_deref (),
    Some ("committed"), "{}", fresh_result);
  let before_refused_a : Vec<u8> = fs::read (fixture . node ("a")) . unwrap ();
  let before_refused_b : Vec<u8> = fs::read (fixture . node ("b")) . unwrap ();
  let old_body : String = view_content (&old_view) . replace ("Alpha", "OldAlpha");
  let refused : Sexp = save_registered_view (
    &mut client, &old_uri, a_buffer_id (),
    view_field (&old_view, "graph-generation"),
    view_field (&old_view, "server-revision"),
    view_field (&old_view, "client-application-token"), &old_body);
  assert_eq! (field (&refused, "save-operation-state") . as_deref (),
    Some ("refused"), "{}", refused);
  let refusal_text : String = refused . to_string ();
  assert! (refusal_text . contains ("save dependency"), "{}", refused);
  assert_eq! (fs::read (fixture . node ("a")) . unwrap (), before_refused_a);
  assert_eq! (fs::read (fixture . node ("b")) . unwrap (), before_refused_b);
}

#[test]
fn socket_child_save_survives_unwritten_parent_title_change () {
  let fixture : Fixture = Fixture::new ();
  let _process : ServerProcess = fixture . start ("none");
  let mut client : Client = Client::connect (&fixture);
  let initial_body : String = parent_child_body ("Parent", "Child");
  let (initial_request, _, _) : (String, String, String) =
    client . save_arbitrary_request (&initial_body);
  client . send (&initial_request, Some (&initial_body));
  let initial : Sexp = client . terminal ();
  assert_eq! (field (&initial, "save-operation-state") . as_deref (),
    Some ("committed"), "{}", initial);

  let parent_uri : String = format! ("parent-{}", uuid::Uuid::new_v4 ());
  let g2_parent_uri : String = format! ("parent-g2-{}", uuid::Uuid::new_v4 ());
  let parent_view : Sexp = editable_content_view (&mut client, "parent", &parent_uri);
  let g2_parent_view : Sexp = editable_content_view (&mut client, "parent", &g2_parent_uri);
  let parent_body : String = view_content (&g2_parent_view)
    . replace ("Parent", "ParentG2");
  let parent_result : Sexp = save_registered_view (
    &mut client, &g2_parent_uri, a_buffer_id (),
    view_field (&g2_parent_view, "graph-generation"),
    view_field (&g2_parent_view, "server-revision"),
    view_field (&g2_parent_view, "client-application-token"), &parent_body);
  assert_eq! (field (&parent_result, "save-operation-state") . as_deref (),
    Some ("committed"), "{}", parent_result);
  let stale_view_body : String = view_content (&parent_view);
  let child_body : String = stale_view_body
    . replace ("(node (id parent) (source main)",
      "(node (id parent) (source main) indef")
    . replace ("Child", "ChildG1");
  assert! (child_body . contains ("Parent"), "parent title was lost: {}", child_body);
  assert! (child_body . contains ("(id parent) (source main) indef"),
    "parent was not made indefinite: {}", child_body);
  assert! (child_body . contains ("ChildG1"), "child edit was lost: {}", child_body);
  let child_result : Sexp = save_registered_view (
    &mut client, &parent_uri, a_buffer_id (),
    view_field (&parent_view, "graph-generation"),
    view_field (&parent_view, "server-revision"),
    view_field (&parent_view, "client-application-token"), &child_body);
  assert_eq! (field (&child_result, "save-operation-state") . as_deref (),
    Some ("committed"), "{}", child_result);
  let parent_bytes : String = fs::read_to_string (
    fixture . node ("parent")) . unwrap ();
  let child_bytes : String = fs::read_to_string (
    fixture . node ("child")) . unwrap ();
  assert! (parent_bytes . contains ("ParentG2"), "{}", parent_bytes);
  assert! (child_bytes . contains ("ChildG1"), "{}", child_bytes);
}

#[test]
fn socket_report_recovery_reconstructs_old_pair_after_restart () {
  let fixture : Fixture = Fixture::new ();
  let hold : PathBuf = fixture . root . join ("report-staging-hold");
  let mut process : ServerProcess = fixture . start_with_report_hold (&hold);
  let mut client : Client = Client::connect (&fixture);
  let (initial_request, _, _) : (String, String, String) =
    client . save_request ("InitialAlpha", "InitialBeta");
  client . send (&initial_request,
    Some (&save_body ("InitialAlpha", "InitialBeta")));
  let initial_result : Sexp = client . terminal ();
  assert_eq! (field (&initial_result, "save-operation-state") . as_deref (),
    Some ("committed"), "{}", initial_result);

  client . send ("((request . \"begin maintenance\") (origin . \"explicit-partial-reload\") (ids \"a\"))", None);
  let offer : Sexp = client . terminal ();
  let incident : String = field (&offer, "allocated-incident-id")
    . expect ("maintenance incident");
  let epoch : String = field (&offer, "maintenance-epoch")
    . expect ("maintenance epoch");
  let g0 : u64 = field (&offer, "g0-graph-generation")
    . expect ("G0 graph generation") . parse () . unwrap ();

  client . send (&format! (
    "((request . \"maintenance locked census\") (incident-id . \"{}\") (maintenance-epoch . {}) (client-constructor-admission . \"closed\"))",
    incident, epoch), None);
  let locked : Sexp = client . terminal ();
  assert_eq! (field (&locked, "status") . as_deref (),
    Some ("locked-census-accepted-publish-initial-archive"), "{}", locked);
  let manifest_sha256 : String = write_empty_initial_archive (&offer);
  client . send (&format! (
    "((request . \"maintenance archive ready\") (incident-id . \"{}\") (maintenance-epoch . {}) (lock-census-sha256 . \"{}\") (manifest-sha256 . \"{}\"))",
    incident, epoch, empty_lock_census_sha256 (), manifest_sha256), None);
  let archive_ready : Sexp = client . terminal ();
  assert_eq! (field (&archive_ready, "status") . as_deref (),
    Some ("archive-ready"), "{}", archive_ready);

  let initial_a : String = fs::read_to_string (fixture . node ("a")) . unwrap ();
  let modified_a : String = initial_a . replace ("InitialAlpha", "G1Alpha");
  assert_ne! (modified_a, initial_a);
  fs::write (fixture . node ("a"), modified_a) . unwrap ();
  client . send (&format! (
    "((request . \"run maintenance origin\") (incident-id . \"{}\") (maintenance-epoch . {}))",
    incident, epoch), None);
  let origin_started : Sexp = client . terminal ();
  assert_eq! (field (&origin_started, "status") . as_deref (),
    Some ("origin-operation-started"), "{}", origin_started);
  wait_for_path (&hold . with_extension ("ready"));

  client . send (&format! (
    "((request . \"maintenance status\") (incident-id . \"{}\") (maintenance-epoch . {}))",
    incident, epoch), None);
  let held : Sexp = client . terminal ();
  assert_eq! (field (&held, "phase") . as_deref (), Some ("presenting"), "{}", held);
  let g1 : u64 = field (&held, "g1-graph-generation")
    . expect ("G1 graph generation") . parse () . unwrap ();
  assert_eq! (g1, g0 + 1, "{}", held);
  client . generation = field (&held, "current-graph-generation")
    . expect ("current G1 graph generation") . parse () . unwrap ();
  assert_eq! (client . generation, g1);

  let (g2_request, _, _) : (String, String, String) =
    client . save_request ("G2Alpha", "G2Beta");
  client . send (&g2_request, Some (&save_body ("G2Alpha", "G2Beta")));
  let g2_result : Sexp = client . terminal ();
  assert_eq! (field (&g2_result, "save-operation-state") . as_deref (),
    Some ("committed"), "{}", g2_result);
  let g2_status : Sexp = {
    client . send (&format! (
      "((request . \"maintenance status\") (incident-id . \"{}\") (maintenance-epoch . {}))",
      incident, epoch), None);
    client . terminal () };
  let g2 : u64 = field (&g2_status, "current-graph-generation")
    . expect ("current G2 graph generation") . parse () . unwrap ();
  assert! (g2 > g1, "{}", g2_status);
  let g2_a : Vec<u8> = fs::read (fixture . node ("a")) . unwrap ();
  let g2_b : Vec<u8> = fs::read (fixture . node ("b")) . unwrap ();
  assert! (String::from_utf8_lossy (&g2_a) . contains ("G2Alpha"));
  assert! (String::from_utf8_lossy (&g2_b) . contains ("G2Beta"));

  #[cfg(unix)]
  unsafe { libc::kill (process . child . id () as i32, libc::SIGKILL); }
  #[cfg(not(unix))]
  process . child . kill () . unwrap ();
  let status : ExitStatus = process . child . wait () . unwrap ();
  #[cfg(unix)]
  assert_eq! (status . signal (), Some (libc::SIGKILL), "{}", process . logs ());
  drop (client);
  drop (process);

  let _restarted : ServerProcess = fixture . start ("none");
  let mut recovered : Client = Client::connect (&fixture);
  let restarted_generation : u64 = recovered . generation;
  recovered . send (&format! (
    "((request . \"maintenance status\") (incident-id . \"{}\") (maintenance-epoch . {}))",
    incident, epoch), None);
  let report : Sexp = recovered . terminal ();
  assert_eq! (field (&report, "status") . as_deref (), Some ("active"), "{}", report);
  assert_eq! (field (&report, "phase") . as_deref (), Some ("finalizing-archive"), "{}", report);
  assert_eq! (field (&report, "g1-graph-generation") . as_deref (),
    Some (g1 . to_string () . as_str ()), "{}", report);
  assert_eq! (field (&report, "g0-graph-generation") . as_deref (),
    Some (g0 . to_string () . as_str ()), "{}", report);
  assert_eq! (field (&report, "live-authority-retired") . as_deref (),
    Some ("true"), "{}", report);
  assert_eq! (field (&report, "current-graph-generation") . as_deref (),
    Some (restarted_generation . to_string () . as_str ()), "{}", report);
  assert_eq! (fs::read (fixture . node ("a")) . unwrap (), g2_a);
  assert_eq! (fs::read (fixture . node ("b")) . unwrap (), g2_b);

  let view_uri : String = format! ("view:test-{}", uuid::Uuid::new_v4 ());
  recovered . send (&format! (
    "((request . \"single root content view\") (id . \"a\") (view-uri . \"{}\") (fresh-view . \"true\") (override-choice . \"bypass\") (requested-view-write-authority . \"editable\"))",
    view_uri), None);
  let view : Sexp = recovered . receive () . unwrap ();
  assert_eq! (field (&view, "view-write-authority") . as_deref (),
    Some ("editable"), "{}", view);
  let view_body : String = field (&view, "content")
    . expect ("fresh editable view content") . replace ("G2Alpha", "FreshAlpha");
  assert_ne! (view_body, field (&view, "content") . unwrap ());
  let fresh_buffer_id : String = uuid::Uuid::new_v4 () . to_string ();
  let (fresh_request, _, _) : (String, String, String) =
    recovered . save_request_with_authority (
      &view_uri, &fresh_buffer_id,
      field (&view, "graph-generation") . expect ("fresh view graph")
        . parse () . unwrap (),
      field (&view, "server-revision") . expect ("fresh view revision")
        . parse () . unwrap (),
      field (&view, "client-application-token")
        . expect ("fresh view application token") . parse () . unwrap (),
      &view_body);
  recovered . send (&fresh_request, Some (&view_body));
  let fresh_result : Sexp = recovered . terminal ();
  assert_eq! (field (&fresh_result, "save-operation-state") . as_deref (),
    Some ("committed"), "{}", fresh_result);

  assert! (fs::read_to_string (fixture . node ("a")) . unwrap ()
    . contains ("FreshAlpha"));
  let search : Sexp = recovered . text_search ("G2Beta");
  assert! (field (&search, "content") . unwrap_or_default ()
    . contains ("G2Beta"), "{}", search);
}

#[test]
fn socket_full_rebuild_serves_reads_status_and_prompt_save_refusal () {
  let fixture : Fixture = Fixture::new ();
  let hold : PathBuf = fixture . root . join ("rebuild-hold");
  let _process : ServerProcess = fixture . start_with_options ("none", None, Some (&hold));
  let mut client : Client = Client::connect (&fixture);
  let (request, _, _) : (String, String, String) = client . save_request ("Alpha", "Beta");
  client . send (&request, Some (&save_body ("Alpha", "Beta")));
  assert_eq! (field (&client . terminal (), "save-operation-state") . as_deref (), Some ("committed"));
  let before : Vec<u8> = fs::read (fixture . node ("a")) . unwrap ();
  client . send ("((request . \"begin maintenance\") (origin . \"full-rebuild\"))", None);
  let offer : Sexp = client . terminal ();
  let incident : String = field (&offer, "allocated-incident-id") . expect ("incident");
  let epoch : String = field (&offer, "maintenance-epoch") . expect ("epoch");
  client . generation = field (&offer, "g0-graph-generation") . unwrap () . parse () . unwrap ();
  client . send (&format! (
    "((request . \"maintenance locked census\") (incident-id . \"{}\") (maintenance-epoch . {}) (client-constructor-admission . \"closed\"))",
    incident, epoch), None);
  assert_eq! (field (&client . terminal (), "status") . as_deref (),
    Some ("locked-census-accepted-publish-initial-archive"));
  let checksum : String = write_empty_initial_archive (&offer);
  client . send (&format! (
    "((request . \"maintenance archive ready\") (incident-id . \"{}\") (maintenance-epoch . {}) (lock-census-sha256 . \"{}\") (manifest-sha256 . \"{}\"))",
    incident, epoch, empty_lock_census_sha256 (), checksum), None);
  let archived : Sexp = client . terminal ();
  assert_eq! (field (&archived, "status") . as_deref (), Some ("archive-ready"), "{}", archived);
  client . send (&format! (
    "((request . \"run maintenance origin\") (incident-id . \"{}\") (maintenance-epoch . {}))",
    incident, epoch), None);
  assert_eq! (field (&client . terminal (), "status") . as_deref (), Some ("origin-operation-started"));
  wait_for_path (&hold . with_extension ("ready"));
  let status_request : String = format! (
    "((request . \"maintenance status\") (incident-id . \"{}\") (maintenance-epoch . {}))", incident, epoch);
  let status_started : Instant = Instant::now ();
  client . send (&status_request, None);
  let status : Sexp = client . terminal ();
  let status_elapsed : Duration = status_started . elapsed ();
  assert! (status_elapsed < Duration::from_secs (1), "status stalled: {:?}", status_elapsed);
  assert_eq! (field (&status, "rebuilding") . as_deref (), Some ("true"), "{}", status);
  assert_eq! (field (&status, "graph-write-admission") . as_deref (), Some ("closed"));
  let (refused_request, _, _) : (String, String, String) = client . save_request ("Unwanted", "Beta");
  let save_started : Instant = Instant::now ();
  client . send (&refused_request, Some (&save_body ("Unwanted", "Beta")));
  let refusal : Sexp = client . terminal ();
  let save_elapsed : Duration = save_started . elapsed ();
  assert! (save_elapsed < Duration::from_secs (1), "refusal stalled: {:?}", save_elapsed);
  assert_eq! (field (&refusal, "save-operation-state") . as_deref (), Some ("refused"), "{}", refusal);
  assert_eq! (fs::read (fixture . node ("a")) . unwrap (), before);
  let query_started : Instant = Instant::now ();
  client . send (&format! (
    "((request . \"single root content view\") (id . \"a\") (view-uri . \"{}\") (fresh-view . \"true\") (requested-view-write-authority . \"editable\"))",
    uuid::Uuid::new_v4 ()), None);
  let view : Sexp = client . terminal ();
  let query_elapsed : Duration = query_started . elapsed ();
  assert! (query_elapsed < Duration::from_secs (1), "query stalled: {:?}", query_elapsed);
  assert_eq! (field (&view, "view-write-authority") . as_deref (), Some ("read-only"), "{}", view);
  assert! (field (&view, "content") . unwrap_or_default () . contains ("Alpha"));
  let search : Sexp = client . text_search ("Beta");
  assert! (field (&search, "content") . unwrap_or_default () . contains ("Beta"), "{}", search);
  assert_eq! (field (&search, "view-write-authority") . as_deref (), Some ("read-only"));
  fs::write (hold . with_extension ("release"), b"release") . unwrap ();
  let publication_started : Instant = Instant::now ();
  loop {
    client . send (&status_request, None);
    let result : Sexp = client . terminal ();
    if field (&result, "graph-write-admission") . as_deref () == Some ("open") {
      assert_ne! (field (&result, "rebuilding") . as_deref (), Some ("true"));
      break; }
    assert! (publication_started . elapsed () < Duration::from_secs (10), "{}", result);
    std::thread::sleep (Duration::from_millis (20)); }
  eprintln! ("held full rebuild: status={:?}, save refusal={:?}, view={:?}, publication after release={:?}",
    status_elapsed, save_elapsed, query_elapsed, publication_started . elapsed ());
}

#[test]
fn socket_process_worker () {
  let Some (root) : Option<std::ffi::OsString> = std::env::var_os (WORKER_ROOT)
    else { return; };
  let root : PathBuf = root . into ();
  let config : SkgConfig = load_config (root . join ("config.toml") . to_str () . unwrap ()) . unwrap ();
  let recovery : StartupSaveRecovery = recover_source_effects_before_startup (&config) . unwrap ();
  let (env, handoff, _) : (SkgEnv, InitContextHandoff, Vec<NodeComplete>) = initialize_dbs (&config);
  let computation : ContextComputation = compute_and_store_context_types (
    &env . tantivy_index, &handoff . had_id_set, &handoff . all_node_ids,
    &handoff . link_dests, &handoff . map_to_content, &handoff . map_to_containers) . unwrap ();
  let old : Arc<SelectedStoreState> = env . in_rust_graph . load_full ();
  env . in_rust_graph . store (Arc::new (old . with_cyclic_roots (computation . cyclic_roots)));
  commit_recovered_source_effects (recovery,
    env . in_rust_graph . load_full () . graph_generation . get ()) . unwrap ();
  let runtime : Arc<ServerRuntime> = prepare_runtime (env) . unwrap ();
  let listener : TcpListener = TcpListener::bind (("127.0.0.1", config . port)) . unwrap ();
  fs::write (root . join ("ready"), b"ready") . unwrap ();
  serve (runtime, listener) . unwrap ();
}

pub(crate) fn crash_point (boundary : &str) {
  if std::env::var_os (WORKER_ROOT) . is_none ()
    || std::env::var (CRASH_POINT) . ok () . as_deref () != Some (boundary)
  { return; }
  #[cfg(unix)]
  unsafe { libc::kill (libc::getpid (), libc::SIGKILL); }
  std::process::abort ();
}

/// Test-only barrier used to leave a committed G1 with durable evidence while
/// report settlement planning is paused. The parent test owns the control
/// files; the child never fabricates a candidate or journal record.
pub(crate) fn hold_maintenance_report () {
  hold_at (REPORT_HOLD); }

pub(crate) fn hold_maintenance_rebuild () {
  hold_at (REBUILD_HOLD); }

fn hold_at (
  variable : &str,
) {
  let Some (control) : Option<PathBuf> = std::env::var_os (variable)
    . map (PathBuf::from) else { return; };
  fs::write (control . with_extension ("ready"), b"ready") . unwrap ();
  while ! control . with_extension ("release") . exists () {
    std::thread::sleep (Duration::from_millis (10)); }
}

impl Fixture {
  fn new () -> Self {
    let root : PathBuf = std::env::temp_dir () . join (format! (
      "skg-socket-save-{}", uuid::Uuid::new_v4 ()));
    fs::create_dir_all (root . join ("owned/main")) . unwrap ();
    let listener : TcpListener = TcpListener::bind (("127.0.0.1", 0)) . unwrap ();
    let port : u16 = listener . local_addr () . unwrap () . port ();
    fs::write (root . join ("config.toml"), format! (
      "port = {}\nbeep_when_server_becomes_available = false\ntantivy_folder = {:?}\nmaintenance_archive_folder = {:?}\n[[sources]]\nname = \"main\"\npath = {:?}\n",
      port, root . join ("index"), root . join ("archives"), root . join ("owned/main"))) . unwrap ();
    Self { root, port }
  }

  fn node (&self, id : &str) -> PathBuf { self . root . join (format! ("owned/main/{}.skg", id)) }

  fn start (&self, boundary : &str) -> ServerProcess {
    self . start_with_options (boundary, None, None)
  }

  fn start_with_report_hold (&self, hold : &PathBuf) -> ServerProcess {
    self . start_with_options ("none", Some (hold), None)
  }

  fn start_with_options (
    &self,
    boundary : &str,
    report_hold : Option<&PathBuf>,
    rebuild_hold : Option<&PathBuf>,
  ) -> ServerProcess {
    let _ = fs::remove_file (self . root . join ("ready"));
    let log : PathBuf = self . root . join (format! ("server-{}.log", boundary));
    let output : File = File::create (&log) . unwrap ();
    let mut command : Command = Command::new (
      std::env::current_exe () . unwrap ());
    command . args (["--exact", WORKER_NAME, "--nocapture"])
      . env (WORKER_ROOT, &self . root) . env (CRASH_POINT, boundary)
      . env ("XDG_STATE_HOME", self . root . join ("private-state"));
    if let Some (hold) = report_hold {
      command . env (REPORT_HOLD, hold); }
    if let Some (hold) = rebuild_hold {
      command . env (REBUILD_HOLD, hold); }
    let child : Child = command
      . stdout (Stdio::from (output . try_clone () . unwrap ()))
      . stderr (Stdio::from (output)) . spawn () . unwrap ();
    let mut process : ServerProcess = ServerProcess { child, log };
    let deadline : Instant = Instant::now () + Duration::from_secs (15);
    while !self . root . join ("ready") . exists () {
      assert! (process . child . try_wait () . unwrap () . is_none (), "startup failed: {}", process . logs ());
      assert! (Instant::now () < deadline, "startup timed out: {}", process . logs ());
      std::thread::sleep (Duration::from_millis (10)); }
    process
  }
}

fn wait_for_path (path : &PathBuf) {
  let deadline : Instant = Instant::now () + Duration::from_secs (15);
  while ! path . exists () {
    assert! (Instant::now () < deadline,
      "timed out waiting for {}", path . display ());
    std::thread::sleep (Duration::from_millis (10)); }
}

impl Drop for Fixture {
  fn drop (&mut self) {
    if !std::thread::panicking () { let _ = fs::remove_dir_all (&self . root); }
    else { eprintln! ("retained socket fixture {}", self . root . display ()); }
  }
}

impl ServerProcess {
  fn logs (&self) -> String { fs::read_to_string (&self . log) . unwrap_or_default () }
}

impl Drop for ServerProcess {
  fn drop (&mut self) { let _ = self . child . kill (); let _ = self . child . wait (); }
}

impl Client {
  fn connect (fixture : &Fixture) -> Self {
    let stream : TcpStream = TcpStream::connect (("127.0.0.1", fixture . port)) . unwrap ();
    stream . set_read_timeout (Some (Duration::from_secs (5))) . unwrap ();
    let reader : BufReader<TcpStream> = BufReader::new (stream . try_clone () . unwrap ());
    let mut client : Self = Self { stream, reader, session: String::new (), generation: 1 };
    client . send ("((request . \"verify connection\") (role . \"interactive\") (protocol-version . 2) (client-kind . \"neovim\") (client-version . \"test\") (client-session-id . \"socket-client\") (archive-format-version . 1) (native-undo-kind . \"nvim\") (native-undo-version . \"test\") (source-set . \"server-default\"))", None);
    let hello : Sexp = client . terminal ();
    client . session = field (&hello, "server-session-id") . expect ("hello session");
    client . generation = field (&hello, "graph-generation") . expect ("hello graph") . parse () . unwrap ();
    client . send ("((request . \"client census\"))", Some ("()"));
    let census : Sexp = client . terminal ();
    assert_eq! (field (&census, "response-type") . as_deref (), Some ("client-census"), "{}", census);
    client
  }

  fn save_request (&self, a : &str, b : &str) -> (String, String, String) {
    let operation : String = uuid::Uuid::new_v4 () . to_string ();
    let intent : String = format! (
      "((request . \"save buffer\") (view-uri . \"{}\") (client-buffer-id . \"{}\") (view-kind . \"new-empty-content-view\") (graph-generation . {}) (server-revision . 0) (client-application-token . 1) (server-session-id . \"{}\") (operation-id . \"{}\"))",
      uuid::Uuid::new_v4 (), uuid::Uuid::new_v4 (), self . generation, self . session, operation);
    self . save_request_from_intent (
      intent, &save_body (a, b), operation)
  }

  fn save_arbitrary_request (&self, body : &str) -> (String, String, String) {
    let operation : String = uuid::Uuid::new_v4 () . to_string ();
    let intent : String = format! (
      "((request . \"save buffer\") (view-uri . \"{}\") (client-buffer-id . \"{}\") (view-kind . \"new-empty-content-view\") (graph-generation . {}) (server-revision . 0) (client-application-token . 1) (server-session-id . \"{}\") (operation-id . \"{}\"))",
      uuid::Uuid::new_v4 (), uuid::Uuid::new_v4 (), self . generation,
      self . session, operation);
    self . save_request_from_intent (intent, body, operation)
  }

  fn save_request_with_authority (
    &self,
    view_uri : &str,
    buffer_id : &str,
    graph_generation : u64,
    server_revision : u64,
    application_token : u64,
    body : &str,
  ) -> (String, String, String) {
    let operation : String = uuid::Uuid::new_v4 () . to_string ();
    let intent : String = format! (
      "((request . \"save buffer\") (view-uri . \"{}\") (client-buffer-id . \"{}\") (view-kind . \"content-view\") (graph-generation . {}) (server-revision . {}) (client-application-token . {}) (server-session-id . \"{}\") (operation-id . \"{}\"))",
      view_uri, buffer_id, graph_generation, server_revision,
      application_token, self . session, operation);
    self . save_request_from_intent (intent, body, operation)
  }

  fn save_request_from_intent (
    &self,
    intent : String,
    body : &str,
    operation : String,
  ) -> (String, String, String) {
    let mut digest : Sha256 = Sha256::new ();
    digest . update (intent . as_bytes ()); digest . update ([0]);
    digest . update (body . as_bytes ());
    let fingerprint : String = format! ("{:x}", digest . finalize ());
    (format! ("{} (request-base-fingerprint . \"{}\"))", &intent [..intent . len () - 1], fingerprint), fingerprint, operation)
  }

  fn send (&mut self, request : &str, body : Option<&str>) {
    let session : String = if self . session . is_empty () || request . contains ("server-session-id") {
      String::new ()
    } else { format! (" (server-session-id . \"{}\")", self . session) };
    let header : String = format! ("{}{} (request-id . \"{}\"))\n",
      &request [..request . len () - 1], session, uuid::Uuid::new_v4 ());
    self . stream . write_all (header . as_bytes ()) . unwrap ();
    if let Some (body) = body {
      self . stream . write_all (format! ("Content-Length: {}\r\n\r\n", body . len ()) . as_bytes ()) . unwrap ();
      self . stream . write_all (body . as_bytes ()) . unwrap (); }
    self . stream . flush () . unwrap ();
  }

  fn receive (&mut self) -> Result<Sexp, String> { read_response (&mut self . reader) }

  fn text_search (&mut self, terms : &str) -> Sexp {
    self . send (&format! (
      "((request . \"text search\") (terms . \"{}\") (regex . \"false\") (body . \"true\") (operators . \"false\") (requested-view-write-authority . \"read-only\"))",
      terms), None);
    self . receive () . unwrap ()
  }

  fn terminal (&mut self) -> Sexp {
    loop {
      let response : Sexp = self . receive () . unwrap ();
      if field (&response, "terminal-status") . is_some () { return response; }
    }
  }

  fn operation_status (&mut self, operation : &str, fingerprint : &str) -> Sexp {
    self . send (&format! ("((request . \"save operation status\") (operation-id . \"{}\") (request-base-fingerprint . \"{}\"))", operation, fingerprint), None);
    self . terminal ()
  }
}

fn save_body (a : &str, b : &str) -> String {
  format! ("* (skg (node (id a) (source main))) {}\n* (skg (node (id b) (source main))) {}\n", a, b)
}

fn parent_child_body (parent : &str, child : &str) -> String {
  format! ("* (skg (node (id parent) (source main))) {}\n** (skg (node (id child) (source main))) {}\n", parent, child)
}

fn editable_content_view (
  client : &mut Client,
  id     : &str,
  uri    : &str,
) -> Sexp {
  client . send (&format! (
    "((request . \"single root content view\") (id . \"{}\") (view-uri . \"{}\") (fresh-view . \"true\") (override-choice . \"bypass\") (requested-view-write-authority . \"editable\"))",
    id, uri), None);
  let view : Sexp = client . terminal ();
  assert_eq! (field (&view, "view-write-authority") . as_deref (),
    Some ("editable"), "{}", view);
  view
}

fn view_content (view : &Sexp) -> String {
  field (view, "content") . expect ("editable view content")
}

fn view_field (view : &Sexp, name : &str) -> u64 {
  field (view, name) . expect (name) . parse () . expect (name)
}

fn a_buffer_id () -> String {
  uuid::Uuid::new_v4 () . to_string ()
}

fn save_registered_view (
  client            : &mut Client,
  view_uri          : &str,
  buffer_id         : String,
  graph_generation  : u64,
  server_revision   : u64,
  application_token : u64,
  body              : &str,
) -> Sexp {
  let (request, _, _) : (String, String, String) =
    client . save_request_with_authority (
      view_uri, &buffer_id, graph_generation, server_revision,
      application_token, body);
  client . send (&request, Some (body));
  client . terminal ()
}

fn quoted (value : &str) -> String {
  format! ("\"{}\"", value . replace ('\\', "\\\\")
    . replace ('\"', "\\\"") . replace ('\n', "\\n"))
}

fn artifact_record (path : &str, bytes : &[u8]) -> String {
  format! ("((path {}) (bytes {}) (sha256 \"{:x}\"))",
    quoted (path), bytes . len (), Sha256::digest (bytes))
}

fn write_empty_initial_archive (offer : &Sexp) -> String {
  let archive_root : PathBuf = PathBuf::from (
    field (offer, "maintenance-archive-identity")
      . expect ("maintenance archive identity"));
  let directory_name : String = field (offer, "archive-directory-name")
    . expect ("archive directory name");
  let directory : PathBuf = archive_root . join (&directory_name);
  fs::create_dir_all (directory . join ("buffer-snapshots")) . unwrap ();
  fs::create_dir_all (directory . join ("interrupted-buffers")) . unwrap ();
  #[cfg(unix)]
  {
    use std::os::unix::fs::PermissionsExt;
    for path in [
      directory . clone (),
      directory . join ("buffer-snapshots"),
      directory . join ("interrupted-buffers"),
    ] {
      fs::set_permissions (path, fs::Permissions::from_mode (0o700))
        . unwrap (); }
  }
  let incident : &[u8] = b"* Test incident\n";
  let interrupted : &[u8] = b"* Interrupted buffers\n";
  fs::write (directory . join ("incident.org"), incident) . unwrap ();
  fs::write (directory . join ("interrupted-buffers/README.org"), interrupted)
    . unwrap ();
  let manifest : String = format! (concat! (
    "((archive-format-version 1) (manifest-kind \"initial\") ",
    "(incident-id {}) (maintenance-epoch {}) (origin {}) ",
    "(started-at-utc {}) (archive-directory-name {}) ",
    "(client-kind \"neovim\") (client-version \"socket-test\") ",
    "(client-session-id \"socket-client\") ",
    "(client-archive-identity \"socket-test-empty-archive\") ",
    "(server-archive-identity {}) (source-set \"all\") ",
    "(g0-graph-generation {}) (g0-manifest-revision {}) ",
    "(directory-sync \"socket-test\") (artifacts ({} {})) ",
    "(buffers ()) (initial-status \"prepared-for-publication\"))\n"),
    quoted (&field (offer, "allocated-incident-id")
      . expect ("incident ID")),
    field (offer, "maintenance-epoch") . expect ("epoch"),
    quoted (&field (offer, "origin") . expect ("origin")),
    quoted (&field (offer, "started-at-utc") . expect ("started at")),
    quoted (&directory_name), quoted (&archive_root . to_string_lossy ()),
    field (offer, "g0-graph-generation") . expect ("G0 generation"),
    field (offer, "g0-manifest-revision") . expect ("G0 revision"),
    artifact_record ("incident.org", incident),
    artifact_record ("interrupted-buffers/README.org", interrupted));
  fs::write (directory . join ("manifest.initial.sexp"), manifest . as_bytes ())
    . unwrap ();
  let manifest_sha256 : String = format! ("{:x}", Sha256::digest (
    manifest . as_bytes ())); 
  let marker : String = format! (
    "((archive-format-version 1) (incident-id {}) (manifest-sha256 {}))\n",
    quoted (&field (offer, "allocated-incident-id")
      . expect ("incident ID")), quoted (&manifest_sha256));
  fs::write (directory . join ("ARCHIVE-READY"), marker . as_bytes ())
    . unwrap ();
  #[cfg(unix)]
  for path in [
    directory . join ("incident.org"),
    directory . join ("interrupted-buffers/README.org"),
    directory . join ("manifest.initial.sexp"),
    directory . join ("ARCHIVE-READY"),
  ] {
    use std::os::unix::fs::PermissionsExt;
    fs::set_permissions (path, fs::Permissions::from_mode (0o600))
      . unwrap (); }
  manifest_sha256
}

fn empty_lock_census_sha256 () -> String {
  format! ("{:x}", Sha256::digest ([]))
}

fn field (response : &Sexp, name : &str) -> Option<String> {
  let Sexp::List (fields) : &Sexp = response else { return None; };
  fields . iter () . find_map (|field| match field {
    Sexp::List (parts) if parts . len () == 2 => match &parts [0] {
      Sexp::Atom (Atom::S (key)) if key == name => atom_to_string (&parts [1]) . ok (),
      _ => None,
    },
    _ => None,
  })
}

fn read_response (reader : &mut BufReader<TcpStream>) -> Result<Sexp, String> {
  let mut length : Option<usize> = None;
  loop {
    let mut header : String = String::new ();
    if reader . read_line (&mut header) . map_err (|error| error . to_string ())? == 0 {
      return Err ("server closed the socket" . into ()); }
    if header == "\r\n" { break; }
    if let Some (bytes) = header . strip_prefix ("Content-Length: ") {
      length = Some (bytes . trim () . parse () . map_err (|_| "invalid frame length")?); }
  }
  let mut bytes : Vec<u8> = vec![0; length . ok_or ("missing frame length")?];
  reader . read_exact (&mut bytes) . map_err (|error| error . to_string ())?;
  sexp::parse (std::str::from_utf8 (&bytes) . map_err (|error| error . to_string ())?)
    . map_err (|error| error . to_string ())
}
