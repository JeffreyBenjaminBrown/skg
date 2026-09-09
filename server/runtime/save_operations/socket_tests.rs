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
    let _ = fs::remove_file (self . root . join ("ready"));
    let log : PathBuf = self . root . join (format! ("server-{}.log", boundary));
    let output : File = File::create (&log) . unwrap ();
    let child : Child = Command::new (std::env::current_exe () . unwrap ())
      . args (["--exact", WORKER_NAME, "--nocapture"])
      . env (WORKER_ROOT, &self . root) . env (CRASH_POINT, boundary)
      . env ("XDG_STATE_HOME", self . root . join ("private-state"))
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
    let mut digest : Sha256 = Sha256::new ();
    digest . update (intent . as_bytes ()); digest . update ([0]);
    digest . update (save_body (a, b) . as_bytes ());
    let fingerprint : String = format! ("{:x}", digest . finalize ());
    (format! ("{} (request-base-fingerprint . \"{}\"))", &intent [..intent . len () - 1], fingerprint), fingerprint, operation)
  }

  fn send (&mut self, request : &str, body : Option<&str>) {
    let header : String = format! ("{} (request-id . \"{}\"))\n",
      &request [..request . len () - 1], uuid::Uuid::new_v4 ());
    self . stream . write_all (header . as_bytes ()) . unwrap ();
    if let Some (body) = body {
      self . stream . write_all (format! ("Content-Length: {}\r\n\r\n", body . len ()) . as_bytes ()) . unwrap ();
      self . stream . write_all (body . as_bytes ()) . unwrap (); }
    self . stream . flush () . unwrap ();
  }

  fn receive (&mut self) -> Result<Sexp, String> { read_response (&mut self . reader) }

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
