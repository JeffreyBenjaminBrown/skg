//! Retained-session, single-worker background view rendering.
//!
//! A graph transition replaces the remaining queue rather than appending to
//! it.  Workers never touch TCP or `OpenViews`; they return an immutable
//! proposal which the connection thread scalar-gates and offers to the
//! client.  The server forest advances only after the client ACKs that exact
//! operation against the revision from which it was rendered.

use crate::serve::ViewsState;
use crate::git_ops::presentation_signature::{
  PresentationSignature, presentation_signature,
};
use crate::serve::handlers::scalar_release::{
  ScalarReleaseDecision, decide,
};
use crate::serve::protocol::TcpToClient;
use crate::serve::util::{
  send_response_with_length_prefix,
  tag_server_push_sexp_response,
  tag_text_response,
  value_from_request_sexp,
};
use crate::source_sets::ActiveSourceSet;
use crate::types::env::SkgEnv;
use crate::types::misc::ID;
use crate::types::save::DefineNode;
use crate::types::store_state::GraphGeneration;
use crate::types::tree::forest::ViewForest;
use crate::types::views_state::ViewUri;
use crate::update_buffer::{
  RenderCancellationTicket,
  active_ids_in_viewforest,
  render_background_view,
};
use crate::update_buffer::source_switch::convert_and_prune_for_source_switch;

use futures::executor::block_on;
use sexp::{Atom, Sexp};
use std::collections::{HashMap, HashSet};
use std::net::TcpStream;
use std::sync::Arc;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::mpsc::{self, Receiver, Sender, TryRecvError};
use std::thread;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct RenderGeneration {
  pub graph        : GraphGeneration,
  pub presentation : u64,
}

#[derive(Clone)]
struct BatchContext {
  generation        : RenderGeneration,
  epoch              : u64,
  env                : SkgEnv,
  define_nodes       : Vec<DefineNode>,
  diff_mode_enabled  : bool,
  active_source_set  : ActiveSourceSet,
  scalar_approved    : HashSet<ID>,
  operation          : String,
  source_switch      : bool,
  create_partnerCols : bool,
}

struct WorkerResult {
  uri           : ViewUri,
  base_revision : u64,
  base_view_graph_generation : u64,
  base_view_presentation_generation : u64,
  base_client_application_token : u64,
  client_buffer_id : Option<String>,
  base_source_set : String,
  generation    : RenderGeneration,
  epoch         : u64,
  result        : Result<(ViewForest, String, Vec<String>), String>,
}

#[derive(Clone)]
struct PendingOffer {
  root_graph : Option<std::sync::Arc<crate::dbs::in_rust_graph::InRustGraph>>,
  uri           : ViewUri,
  base_revision : u64,
  base_view_graph_generation : u64,
  base_view_presentation_generation : u64,
  base_client_application_token : u64,
  client_buffer_id : Option<String>,
  base_source_set : String,
  resulting_source_set : String,
  generation    : RenderGeneration,
  viewforest    : Option<ViewForest>,
  authorization_pids : Vec<ID>,
  render_error  : bool,
  batch_epoch   : Option<u64>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ViewApplicationOffer {
  pub operation_id : String,
  pub uri           : ViewUri,
  pub client_buffer_id : Option<String>,
  pub base_revision : u64,
  pub base_graph_generation : u64,
  pub base_presentation_generation : u64,
  pub expected_client_application_token : u64,
  pub resulting_graph_generation : u64,
  pub resulting_presentation_generation : u64,
  pub base_source_set : String,
  pub resulting_source_set : String,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct QueuedRefresh {
  pub operation   : String,
  pub generation  : RenderGeneration,
  pub view_uris   : Vec<ViewUri>,
}

impl QueuedRefresh {
  pub fn operation_id (&self) -> String {
    format! ("refresh-{}-{}-{}",
      self . generation . graph . get (),
      self . generation . presentation,
      self . operation)
  }

  pub fn payload (&self) -> String {
    Sexp::List (vec![
      pair ("reason", atom (&self . operation)),
      pair ("graph-generation", Sexp::Atom (Atom::I (
        self . generation . graph . get () as i64))),
      pair ("presentation-generation", Sexp::Atom (Atom::I (
        self . generation . presentation as i64))),
      pair ("queued-view-uris", Sexp::List (self . view_uris . iter ()
        . map (|uri| atom (&uri . repr_in_client ())) . collect ())),
    ]) . to_string ()
  }

  pub fn send (&self, stream : &mut TcpStream) -> std::io::Result<()> {
    if self . view_uris . is_empty () { return Ok (( )); }
    send_response_with_length_prefix (stream,
      &tag_server_push_sexp_response (
        TcpToClient::RefreshQueued, &self . operation_id (),
        &self . payload ()))
  }
}

pub struct CollateralScheduler {
  epoch          : Arc<AtomicU64>,
  batch          : Option<BatchContext>,
  queue          : Vec<ViewUri>,
  visit_sequence : HashMap<ViewUri, u64>,
  in_flight      : Option<ViewUri>,
  pending_offer  : Option<(String, PendingOffer)>,
  next_operation : u64,
  presentation_generation : u64,
  presentation_signature  : Option<PresentationSignature>,
  sender         : Sender<WorkerResult>,
  receiver       : Receiver<WorkerResult>,
}

impl CollateralScheduler {
  pub fn new () -> Self {
    let (sender, receiver) = mpsc::channel ();
    Self {
      epoch: Arc::new (AtomicU64::new (0)),
      batch: None,
      queue: Vec::new (),
      visit_sequence: HashMap::new (),
      in_flight: None,
      pending_offer: None,
      next_operation: 0,
      presentation_generation: 0,
      presentation_signature: None,
      sender,
      receiver,
    }
  }

  /// Replace all obsolete work after a successful graph transition.
  pub fn replace_after_transition (
    &mut self,
    saved_uri           : Option<&ViewUri>,
    views_state         : &mut ViewsState,
    env                 : &SkgEnv,
    define_nodes        : &[DefineNode],
    active_source_set   : &ActiveSourceSet,
    scalar_approved     : &HashSet<ID>,
  ) -> QueuedRefresh {
    self . replace_queue (
      saved_uri, views_state, env, define_nodes, active_source_set,
      scalar_approved, "background-rerender", false, false)
  }

  /// Replace obsolete queued rerenders with one explicit retained-session
  /// batch.  Existing offered text remains an exact obligation: the new batch
  /// waits for its ACK and then clones whichever forest that ACK made current.
  pub fn replace_for_explicit_rerender (
    &mut self,
    views_state       : &mut ViewsState,
    env               : &SkgEnv,
    active_source_set : &ActiveSourceSet,
    scalar_approved   : &HashSet<ID>,
    operation         : &str,
    source_switch     : bool,
  ) -> QueuedRefresh {
    self . replace_queue (
      None, views_state, env, &[], active_source_set, scalar_approved,
      operation, source_switch, source_switch)
  }

  fn replace_queue (
    &mut self,
    saved_uri           : Option<&ViewUri>,
    views_state         : &mut ViewsState,
    env                 : &SkgEnv,
    define_nodes        : &[DefineNode],
    active_source_set   : &ActiveSourceSet,
    scalar_approved     : &HashSet<ID>,
    operation           : &str,
    source_switch       : bool,
    create_partnerCols  : bool,
  ) -> QueuedRefresh {
    let epoch = self . epoch . fetch_add (1, Ordering::AcqRel) + 1;
    self . queue = views_state . open_views . views . keys ()
      . filter (|uri| saved_uri != Some (*uri))
      . cloned ()
      . collect ();
    self . sort_queue ();
    for uri in &self . queue {
      if let Some (state) = views_state . open_views . views . get_mut (uri) {
        state . presentation_stale = true; }}
    let generation = RenderGeneration {
      graph: env . in_rust_graph . load_full () . graph_generation,
      presentation: self . presentation_generation,
    };
    self . batch = Some (BatchContext {
      generation,
      epoch,
      env: env . clone (),
      define_nodes: define_nodes . to_vec (),
      diff_mode_enabled: views_state . diff_mode_enabled,
      active_source_set: active_source_set . clone (),
      scalar_approved: scalar_approved . clone (),
      operation: operation . into (),
      source_switch,
      create_partnerCols,
    });
    let mut uris = self . queue . clone ();
    uris . sort_by_key (ViewUri::repr_in_client);
    QueuedRefresh {
      operation: operation . into (),
      generation,
      view_uris: uris,
    }
  }

  pub fn seed_presentation (&mut self, env : &SkgEnv) -> Result<(), String> {
    self . presentation_signature = Some (presentation_signature (&env . config)?);
    Ok (( ))
  }

  pub fn presentation_generation (&self) -> u64 {
    self . presentation_generation
  }

  pub fn presentation_signature_hex (&self) -> Option<String> {
    self . presentation_signature . map (PresentationSignature::to_hex)
  }

  /// Observe HEAD/index independently of worktree selection. A changed
  /// signature rerenders every view only while diff mode is enabled.
  pub fn observe_presentation (
    &mut self,
    views_state       : &mut ViewsState,
    env               : &SkgEnv,
    active_source_set : &ActiveSourceSet,
  ) -> Result<(bool, Option<QueuedRefresh>), String> {
    let signature = presentation_signature (&env . config)?;
    let previous = self . presentation_signature . replace (signature);
    if previous == Some (signature) { return Ok ((false, None)); }
    if previous . is_none () { return Ok ((false, None)); }
    self . presentation_generation = self . presentation_generation
      . checked_add (1) . ok_or ("presentation generation exhausted")?;
    let queued = if views_state . diff_mode_enabled {
      Some (self . replace_queue (
        None, views_state, env, &[], active_source_set, &HashSet::new (),
        "git-presentation-rerender", false, false))
    } else { None };
    Ok ((true, queued))
  }

  /// A foreground operation takes priority over collateral rendering.  The current worker is
  /// made obsolete and its URI returns to the newest queue.
  pub fn preempt (&mut self) {
    let Some (uri) = self . in_flight . clone () else { return; };
    let epoch = self . epoch . fetch_add (1, Ordering::AcqRel) + 1;
    if let Some (batch) = &mut self . batch { batch . epoch = epoch; }
    if ! self . queue . contains (&uri) { self . queue . push (uri); }
    self . sort_queue ();
  }

  pub fn note_visit (&mut self, uri : ViewUri, sequence : u64) {
    let entry = self . visit_sequence . entry (uri) . or_insert (0);
    *entry = (*entry) . max (sequence);
    self . sort_queue ();
  }

  /// On an idle tick, consume at most one completed result or start at most
  /// one worker.  Thus background activity can never delay request parsing.
  pub fn pump (
    &mut self,
    stream      : &mut TcpStream,
    views_state : &ViewsState,
  ) {
    match self . receiver . try_recv () {
      Ok (finished) => {
        self . in_flight = None;
        self . offer_if_current (stream, views_state, finished);
        return; }
      Err (TryRecvError::Disconnected) => {
        tracing::error! ("collateral render worker channel disconnected");
        return; }
      Err (TryRecvError::Empty) => {}
    }
    if self . in_flight . is_some () || self . pending_offer . is_some () {
      return; }
    let Some (uri) = self . queue . pop () else { return; };
    let Some (batch) = self . batch . clone () else { return; };
    let Some (state) = views_state . open_views . views . get (&uri) else {
      return; };
    let viewforest = state . viewforest . clone ();
    let base_revision = state . revision;
    let base_view_graph_generation = state . graph_generation;
    let base_view_presentation_generation = state . presentation_generation;
    let base_client_application_token = state . client_application_token;
    let client_buffer_id = state . client_buffer_id . clone ();
    let base_source_set = state . source_set . clone ();
    let input_ids = active_ids_in_viewforest (&viewforest);
    match decide (
      &batch . operation, &batch . active_source_set, &input_ids,
      &batch . env . in_rust_graph_snapshot (), &batch . scalar_approved)
    {
      ScalarReleaseDecision::Challenge { pids, prompt, .. } => {
        self . offer_challenge (
          stream, &uri, base_revision, base_view_graph_generation,
          base_view_presentation_generation,
          base_client_application_token,
          client_buffer_id, base_source_set, &batch, pids, &prompt);
        return; }
      ScalarReleaseDecision::AllowWithWarning { .. }
      | ScalarReleaseDecision::Allow => {}
    }
    self . in_flight = Some (uri . clone ());
    let sender = self . sender . clone ();
    let cancellation = RenderCancellationTicket::new (
      self . epoch . clone (), batch . epoch);
    thread::spawn (move || {
      let mut viewforest = viewforest;
      let result = if batch . source_switch {
        convert_and_prune_for_source_switch (
          viewforest . as_internal_tree_mut (), &batch . active_source_set)
          . map_err (|error| error . to_string ())
      } else { Ok (( )) } . and_then (|_| block_on (render_background_view (
          viewforest,
          &batch . define_nodes,
          &batch . env,
          batch . diff_mode_enabled,
          Some (&batch . active_source_set),
          batch . create_partnerCols,
          cancellation)));
      let _ = sender . send (WorkerResult {
        uri,
        base_revision,
        base_view_graph_generation,
        base_view_presentation_generation,
        base_client_application_token,
        client_buffer_id,
        base_source_set,
        generation: batch . generation,
        epoch: batch . epoch,
        result,
      });
    });
  }

  fn offer_if_current (
    &mut self,
    stream      : &mut TcpStream,
    views_state : &ViewsState,
    finished    : WorkerResult,
  ) {
    let Some (batch) = self . batch . clone () else { return; };
    if finished . epoch != batch . epoch
       || finished . generation != batch . generation
       || self . epoch . load (Ordering::Acquire) != finished . epoch
       || views_state . open_views . view_revision (&finished . uri)
          != Some (finished . base_revision)
       || views_state . open_views . views . get (&finished . uri)
          . map (|state| state . client_application_token)
          != Some (finished . base_client_application_token)
       || views_state . open_views . views . get (&finished . uri)
          . map (|state| state . graph_generation)
          != Some (finished . base_view_graph_generation)
       || views_state . open_views . views . get (&finished . uri)
          . map (|state| state . presentation_generation)
          != Some (finished . base_view_presentation_generation)
       || views_state . open_views . views . get (&finished . uri)
          . map (|state| state . source_set . as_str ())
          != Some (finished . base_source_set . as_str ())
       || batch . env . in_rust_graph . load_full () . graph_generation
          != finished . generation . graph
    { return; }
    let (viewforest, content, mut warnings) = match finished . result {
      Ok (rendered) => rendered,
      Err (ref error) => {
        if error == "obsolete background render cancelled" { return; }
        tracing::warn! (uri = %finished . uri . repr_in_client (),
                        %error, "background view render failed");
        self . offer_failure (
          stream, &finished, &batch, error);
        return; }
    };
    let output_ids = active_ids_in_viewforest (&viewforest);
    match decide (
      &batch . operation, &batch . active_source_set, &output_ids,
      &batch . env . in_rust_graph_snapshot (), &batch . scalar_approved)
    {
      ScalarReleaseDecision::Challenge { pids, prompt, .. } => {
        let uri = finished . uri . clone ();
        self . offer_challenge (
          stream, &uri, finished . base_revision,
          finished . base_view_graph_generation,
          finished . base_view_presentation_generation,
          finished . base_client_application_token,
          finished . client_buffer_id,
          finished . base_source_set,
          &batch, pids, &prompt);
        return; }
      ScalarReleaseDecision::AllowWithWarning { warning } =>
        warnings . push (warning),
      ScalarReleaseDecision::Allow => {}
    }
    let operation_id = self . fresh_operation_id ();
    let payload = offer_payload (
      &finished . uri, &content, finished . generation,
      finished . base_revision, finished . base_view_graph_generation,
      finished . base_view_presentation_generation,
      finished . base_client_application_token,
      finished . client_buffer_id . as_deref (),
      &finished . base_source_set,
      &batch . active_source_set . name . 0,
      &warnings, None, None);
    if send_response_with_length_prefix (stream,
      &tag_server_push_sexp_response (
        TcpToClient::CollateralView, &operation_id, &payload)) . is_err () {
      if ! self . queue . contains (&finished . uri) {
        self . queue . push (finished . uri); }
      self . sort_queue ();
      return; }
    self . pending_offer = Some ((operation_id, PendingOffer {
      root_graph: Some (batch . env . in_rust_graph_snapshot ()),
      uri: finished . uri,
      base_revision: finished . base_revision,
      base_view_graph_generation: finished . base_view_graph_generation,
      base_view_presentation_generation:
        finished . base_view_presentation_generation,
      base_client_application_token:
        finished . base_client_application_token,
      client_buffer_id: finished . client_buffer_id,
      base_source_set: finished . base_source_set,
      resulting_source_set: batch . active_source_set . name . 0 . clone (),
      generation: finished . generation,
      viewforest: Some (viewforest),
      authorization_pids: Vec::new (),
      render_error: false,
      batch_epoch: Some (finished . epoch),
    }));
  }

  fn offer_challenge (
    &mut self,
    stream : &mut TcpStream,
    uri    : &ViewUri,
    base_revision : u64,
    base_view_graph_generation : u64,
    base_view_presentation_generation : u64,
    base_client_application_token : u64,
    client_buffer_id : Option<String>,
    base_source_set : String,
    batch  : &BatchContext,
    pids   : Vec<ID>,
    prompt : &str,
  ) {
    let operation_id = self . fresh_operation_id ();
    let payload = offer_payload (
      uri, "", batch . generation, base_revision,
      base_view_graph_generation, base_view_presentation_generation,
      base_client_application_token, client_buffer_id . as_deref (),
      &base_source_set,
      &batch . active_source_set . name . 0,
      &[], Some ((pids . clone (), prompt)), None);
    if send_response_with_length_prefix (stream,
      &tag_server_push_sexp_response (
        TcpToClient::CollateralView, &operation_id, &payload)) . is_err () {
      if ! self . queue . contains (uri) {
        self . queue . push (uri . clone ()); }
      self . sort_queue ();
      return; }
    self . pending_offer = Some ((operation_id, PendingOffer {
      root_graph: None,
      uri: uri . clone (),
      base_revision,
      base_view_graph_generation,
      base_view_presentation_generation,
      base_client_application_token,
      client_buffer_id,
      base_source_set,
      resulting_source_set: batch . active_source_set . name . 0 . clone (),
      generation: batch . generation,
      viewforest: None,
      authorization_pids: pids,
      render_error: false,
      batch_epoch: Some (batch . epoch),
    }));
  }

  fn offer_failure (
    &mut self,
    stream   : &mut TcpStream,
    finished : &WorkerResult,
    batch    : &BatchContext,
    error    : &str,
  ) {
    let operation_id = self . fresh_operation_id ();
    let payload = offer_payload (
      &finished . uri, "", finished . generation,
      finished . base_revision, finished . base_view_graph_generation,
      finished . base_view_presentation_generation,
      finished . base_client_application_token,
      finished . client_buffer_id . as_deref (),
      &finished . base_source_set,
      &batch . active_source_set . name . 0,
      &[], None, Some (error));
    if send_response_with_length_prefix (stream,
      &tag_server_push_sexp_response (
        TcpToClient::CollateralView, &operation_id, &payload)) . is_err () {
      if ! self . queue . contains (&finished . uri) {
        self . queue . push (finished . uri . clone ()); }
      self . sort_queue ();
      return; }
    self . pending_offer = Some ((operation_id, PendingOffer {
      root_graph: None,
      uri: finished . uri . clone (),
      base_revision: finished . base_revision,
      base_view_graph_generation: finished . base_view_graph_generation,
      base_view_presentation_generation:
        finished . base_view_presentation_generation,
      base_client_application_token:
        finished . base_client_application_token,
      client_buffer_id: finished . client_buffer_id . clone (),
      base_source_set: finished . base_source_set . clone (),
      resulting_source_set: batch . active_source_set . name . 0 . clone (),
      generation: finished . generation,
      viewforest: None,
      authorization_pids: Vec::new (),
      render_error: true,
      batch_epoch: Some (finished . epoch),
    }));
  }

  /// Stage text returned by a foreground continuation without advancing the
  /// retained forest.  The same exact ACK path as collateral work owns the
  /// eventual forest/token transition.
  pub fn stage_view_application (
    &mut self,
    graph : &std::sync::Arc<crate::dbs::in_rust_graph::InRustGraph>,
    views_state : &ViewsState,
    uri         : &ViewUri,
    generation  : RenderGeneration,
    viewforest  : impl Into<ViewForest>,
  ) -> Result<ViewApplicationOffer, String> {
    let viewforest = viewforest . into ();
    if self . pending_offer . is_some () {
      return Err ("another view application is awaiting acknowledgement"
        . into ()); }
    let state = views_state . open_views . views . get (uri)
      . ok_or_else (|| format! (
        "view '{}' closed before application could be staged",
        uri . repr_in_client ()))?;
    let operation_id = self . fresh_operation_id ();
    let offer = ViewApplicationOffer {
      operation_id: operation_id . clone (),
      uri: uri . clone (),
      client_buffer_id: state . client_buffer_id . clone (),
      base_revision: state . revision,
      base_graph_generation: state . graph_generation,
      base_presentation_generation: state . presentation_generation,
      expected_client_application_token: state . client_application_token,
      resulting_graph_generation: generation . graph . get (),
      resulting_presentation_generation: generation . presentation,
      base_source_set: state . source_set . clone (),
      resulting_source_set: state . source_set . clone (),
    };
    self . pending_offer = Some ((operation_id, PendingOffer {
      root_graph: Some (graph . clone ()),
      uri: uri . clone (),
      base_revision: state . revision,
      base_view_graph_generation: state . graph_generation,
      base_view_presentation_generation: state . presentation_generation,
      base_client_application_token: state . client_application_token,
      client_buffer_id: state . client_buffer_id . clone (),
      base_source_set: state . source_set . clone (),
      resulting_source_set: state . source_set . clone (),
      generation,
      viewforest: Some (viewforest),
      authorization_pids: Vec::new (),
      render_error: false,
      batch_epoch: None,
    }));
    Ok (offer)
  }

  pub fn handle_apply_ack (
    &mut self,
    stream      : &mut TcpStream,
    request     : &str,
    views_state : &mut ViewsState,
  ) {
    let result = (|| -> Result<&'static str, String> {
      let operation_id = value_from_request_sexp ("operation-id", request)?;
      let uri = ViewUri::from_client_string (
        value_from_request_sexp ("view-uri", request)?);
      let applied = parse_bool_field (request, "applied")?;
      let authorized = parse_bool_field (request, "authorized")?;
      let graph_generation = parse_u64_field (request, "graph-generation")?;
      let presentation_generation =
        parse_u64_field (request, "presentation-generation")?;
      let base_revision = parse_u64_field (
        request, "viewforest-base-revision")?;
      let resulting_revision = parse_u64_field (
        request, "resulting-server-revision")?;
      let base_graph_generation = parse_u64_field (
        request, "view-base-graph-generation")?;
      let base_presentation_generation = parse_u64_field (
        request, "view-base-presentation-generation")?;
      let expected_client_token = parse_u64_field (
        request, "expected-client-application-token")?;
      let resulting_client_token = parse_u64_field (
        request, "resulting-client-application-token")?;
      let client_token = parse_u64_field (request, "client-token")?;
      let base_source_set = value_from_request_sexp (
        "view-base-source-set", request)?;
      let resulting_source_set = value_from_request_sexp (
        "resulting-source-set", request)?;
      let Some ((pending_id, pending)) = self . pending_offer . clone ()
      else { return Err ("No collateral offer is pending" . into ()); };
      if pending_id != operation_id || pending . uri != uri {
        return Err ("Collateral ACK does not name the pending operation"
          . into ()); }
      if pending . generation . graph . get () != graph_generation
         || pending . generation . presentation != presentation_generation
         || pending . base_revision != base_revision
         || pending . base_revision . saturating_add (1)
            != resulting_revision
         || pending . base_view_graph_generation != base_graph_generation
         || pending . base_view_presentation_generation
            != base_presentation_generation
         || pending . base_client_application_token != expected_client_token
         || pending . base_client_application_token . saturating_add (1)
            != resulting_client_token
         || pending . base_source_set != base_source_set
         || pending . resulting_source_set != resulting_source_set
      {
        return Err ("Collateral ACK changed its render identity" . into ()); }
      if let Some (buffer_id) = &pending . client_buffer_id {
        if value_from_request_sexp ("client-buffer-id", request)?
           != buffer_id . as_str ()
        {
          return Err ("Collateral ACK changed its client buffer" . into ()); }}
      if applied && authorized {
        return Err ("Collateral ACK cannot apply text and authorize a retry"
          . into ()); }
      if client_token != if applied {
          resulting_client_token
        } else { pending . base_client_application_token }
      {
        return Err (format! (
          "Collateral ACK changed its resulting client token to {}",
          client_token)); }
      if pending . render_error {
        if applied || authorized {
          return Err ("A failed render can only be acknowledged" . into ()); }
        mark_stale_if_still_pending_base (views_state, &pending);
        self . pending_offer = None;
        return Ok ("client acknowledged failed view rendering"); }
      if pending . viewforest . is_none () {
        if applied {
          return Err ("Cannot apply a text-free authorization challenge"
            . into ()); }
        if ! authorized {
          mark_stale_if_still_pending_base (views_state, &pending);
          self . pending_offer = None;
          return Ok ("client declined protected view rendering"); }
        current_view_for_pending (views_state, &pending)?;
        let Some (epoch) = pending . batch_epoch else {
          return Err ("Authorization challenge has no render batch" . into ()); };
        let Some (batch) = &mut self . batch else {
          return Err ("Authorization challenge lost its render batch" . into ()); };
        if batch . epoch != epoch
        || batch . generation != pending . generation
        || batch . active_source_set . name . 0 != pending . resulting_source_set
        {
          mark_stale_if_still_pending_base (views_state, &pending);
          self . pending_offer = None;
          return Ok ("protected rendering was superseded before approval"); }
        batch . scalar_approved . extend (
          pending . authorization_pids . iter () . cloned ());
        if ! self . queue . contains (&pending . uri) {
          self . queue . push (pending . uri . clone ()); }
        self . sort_queue ();
        self . pending_offer = None;
        return Ok ("protected view rendering authorized for retry");
      }
      if ! applied {
        mark_stale_if_still_pending_base (views_state, &pending);
        self . pending_offer = None;
        return Ok ("client rejected obsolete or dirty view"); }
      let _ = current_view_for_pending (views_state, &pending)?;
      let viewforest = pending . viewforest
        . expect ("application offer carries a forest");
      if ! views_state . open_views . update_view_if_revision (
          pending . root_graph . as_deref ()
            . ok_or ("application offer lost its selected graph")?,
          &pending . uri, pending . base_revision, viewforest)
      { return Err ("Collateral view advanced before its ACK" . into ()); }
      views_state . open_views
        . set_client_application_authority_and_source_set (
        &pending . uri,
        pending . generation . graph . get (),
        pending . generation . presentation,
        resulting_client_token,
        pending . resulting_source_set)?;
      // A newer replacement batch may have queued this URI while the exact
      // older offer was awaiting its ACK.  Applying that offer repairs its
      // own generation, but must not erase the newer retained refresh debt.
      if self . queue . contains (&pending . uri) {
        views_state . open_views . views . get_mut (&pending . uri)
          . expect ("acknowledged queued view remains registered")
          . presentation_stale = true; }
      self . pending_offer = None;
      Ok ("collateral view application acknowledged")
    })();
    let _ = match result {
      Ok (message) => send_response_with_length_prefix (stream,
        &tag_text_response (TcpToClient::CollateralApplied, message)),
      Err (error) => send_response_with_length_prefix (stream,
        &tag_text_response (TcpToClient::Error, &error)),
    };
  }

  fn fresh_operation_id (&mut self) -> String {
    self . next_operation = self . next_operation . saturating_add (1);
    format! ("collateral-{}", self . next_operation)
  }

  fn sort_queue (&mut self) {
    self . queue . sort_by (|a, b| {
      let av = self . visit_sequence . get (a) . copied () . unwrap_or (0);
      let bv = self . visit_sequence . get (b) . copied () . unwrap_or (0);
      av . cmp (&bv) . then_with (||
        b . repr_in_client () . cmp (&a . repr_in_client ()))
    });
  }
}

fn current_view_for_pending<'a> (
  views_state : &'a ViewsState,
  pending     : &PendingOffer,
) -> Result<&'a crate::types::views_state::ViewState, String> {
  let current = views_state . open_views . views . get (&pending . uri)
    . ok_or_else (|| "Collateral view closed before its ACK" . to_string ())?;
  if current . revision != pending . base_revision
  || current . graph_generation != pending . base_view_graph_generation
  || current . presentation_generation
     != pending . base_view_presentation_generation
  || current . client_application_token
     != pending . base_client_application_token
  || current . source_set != pending . base_source_set
  || (pending . client_buffer_id . is_some ()
      && current . client_buffer_id != pending . client_buffer_id)
  {
    return Err ("Collateral view authority advanced before its ACK"
      . into ()); }
  Ok (current)
}

fn mark_stale_if_still_pending_base (
  views_state : &mut ViewsState,
  pending     : &PendingOffer,
) {
  if current_view_for_pending (views_state, pending) . is_ok () {
    views_state . open_views . views . get_mut (&pending . uri)
      . expect ("validated pending view remains registered")
      . presentation_stale = true; }
}

fn parse_u64_field (request : &str, field : &str) -> Result<u64, String> {
  value_from_request_sexp (field, request)? . parse::<u64> ()
    . map_err (|_| format! ("Collateral ACK has invalid {}", field))
}

fn parse_bool_field (request : &str, field : &str) -> Result<bool, String> {
  match value_from_request_sexp (field, request)? . as_str () {
    "true" => Ok (true),
    "nil" => Ok (false),
    _ => Err (format! ("Collateral ACK has invalid {}", field)),
  }
}

fn atom (value : &str) -> Sexp {
  Sexp::Atom (Atom::S (value . to_string ()))
}

fn pair (key : &str, value : Sexp) -> Sexp {
  Sexp::List (vec! [atom (key), value])
}

fn offer_payload (
  uri           : &ViewUri,
  content       : &str,
  generation    : RenderGeneration,
  base_revision : u64,
  base_view_graph_generation : u64,
  base_view_presentation_generation : u64,
  base_client_application_token : u64,
  client_buffer_id : Option<&str>,
  base_source_set : &str,
  resulting_source_set : &str,
  warnings      : &[String],
  challenge     : Option<(Vec<ID>, &str)>,
  render_error  : Option<&str>,
) -> String {
  let mut fields = vec! [
    pair ("view-uri", atom (&uri . repr_in_client ())),
    pair ("graph-generation", Sexp::Atom (Atom::I (
      generation . graph . get () as i64))),
    pair ("presentation-generation", Sexp::Atom (Atom::I (
      generation . presentation as i64))),
    pair ("viewforest-base-revision", Sexp::Atom (Atom::I (
      base_revision as i64))),
    pair ("view-base-graph-generation", Sexp::Atom (Atom::I (
      base_view_graph_generation as i64))),
    pair ("view-base-presentation-generation", Sexp::Atom (Atom::I (
      base_view_presentation_generation as i64))),
    pair ("resulting-server-revision", Sexp::Atom (Atom::I (
      base_revision . saturating_add (1) as i64))),
    pair ("expected-client-application-token", Sexp::Atom (Atom::I (
      base_client_application_token as i64))),
    pair ("resulting-client-application-token", Sexp::Atom (Atom::I (
      base_client_application_token . saturating_add (1) as i64))),
    pair ("view-base-source-set", atom (base_source_set)),
    pair ("resulting-source-set", atom (resulting_source_set)),
    pair ("warnings", Sexp::List (warnings . iter ()
      . map (|warning| atom (warning)) . collect ())),
  ];
  if let Some (buffer_id) = client_buffer_id {
    fields . push (pair ("client-buffer-id", atom (buffer_id))); }
  if let Some (error) = render_error {
    fields . push (pair ("render-error", atom (error)));
  } else if let Some ((pids, prompt)) = challenge {
    fields . push (pair ("needs-authorization", atom ("true")));
    fields . push (pair ("pids", Sexp::List (pids . iter ()
      . map (|pid| atom (pid . as_str ())) . collect ())));
    fields . push (pair ("prompt", atom (prompt)));
  } else {
    fields . push (pair ("content", atom (content)));
  }
  Sexp::List (fields) . to_string ()
}

pub fn add_application_offer_to_response (
  response : &str,
  offer    : &ViewApplicationOffer,
) -> Result<String, String> {
  let Sexp::List (mut fields) = sexp::parse (response)
    . map_err (|error| format! ("invalid view response: {}", error))?
  else { return Err ("view response is not a list" . into ()); };
  fields . extend (vec![
    pair ("operation-id", atom (&offer . operation_id)),
    pair ("view-uri", atom (&offer . uri . repr_in_client ())),
    pair ("graph-generation", Sexp::Atom (Atom::I (
      offer . resulting_graph_generation as i64))),
    pair ("presentation-generation", Sexp::Atom (Atom::I (
      offer . resulting_presentation_generation as i64))),
    pair ("viewforest-base-revision", Sexp::Atom (Atom::I (
      offer . base_revision as i64))),
    pair ("resulting-server-revision", Sexp::Atom (Atom::I (
      offer . base_revision . saturating_add (1) as i64))),
    pair ("view-base-graph-generation", Sexp::Atom (Atom::I (
      offer . base_graph_generation as i64))),
    pair ("view-base-presentation-generation", Sexp::Atom (Atom::I (
      offer . base_presentation_generation as i64))),
    pair ("expected-client-application-token", Sexp::Atom (Atom::I (
      offer . expected_client_application_token as i64))),
    pair ("resulting-client-application-token", Sexp::Atom (Atom::I (
      offer . expected_client_application_token . saturating_add (1) as i64))),
    pair ("view-base-source-set", atom (&offer . base_source_set)),
    pair ("resulting-source-set", atom (&offer . resulting_source_set)),
  ]);
  if let Some (buffer_id) = &offer . client_buffer_id {
    fields . push (pair ("client-buffer-id", atom (buffer_id))); }
  Ok (Sexp::List (fields) . to_string ())
}

#[cfg(test)]
mod tests {
  use super::*;

  fn ack_request (base_graph_generation : u64) -> String {
    format! (concat! (
      "((operation-id . \"collateral-1\") (view-uri . \"view-uri\") ",
      "(applied . \"nil\") (authorized . \"nil\") ",
      "(graph-generation . \"2\") (presentation-generation . \"9\") ",
      "(viewforest-base-revision . \"4\") ",
      "(resulting-server-revision . \"5\") ",
      "(view-base-graph-generation . \"{}\") ",
      "(view-base-presentation-generation . \"7\") ",
      "(expected-client-application-token . \"12\") ",
      "(resulting-client-application-token . \"13\") ",
      "(client-token . \"12\") (view-base-source-set . \"private\") ",
      "(resulting-source-set . \"all\"))"),
      base_graph_generation)
  }

  fn scheduler_with_pending_application () -> CollateralScheduler {
    let mut scheduler = CollateralScheduler::new ();
    scheduler . next_operation = 1;
    scheduler . pending_offer = Some (("collateral-1" . into (), PendingOffer {
      root_graph: Some (std::sync::Arc::new (crate::dbs::in_rust_graph::InRustGraph::new ())),
      uri: ViewUri::ContentView ("view-uri" . into ()),
      base_revision: 4,
      base_view_graph_generation: 1,
      base_view_presentation_generation: 7,
      base_client_application_token: 12,
      client_buffer_id: None,
      base_source_set: "private" . into (),
      resulting_source_set: "all" . into (),
      generation: RenderGeneration {
        graph: GraphGeneration::INITIAL . successor (), presentation: 9,
      },
      viewforest: Some (ViewForest::new ()),
      authorization_pids: Vec::new (),
      render_error: false,
      batch_epoch: Some (1),
    }));
    scheduler
  }

  #[test]
  fn queue_is_most_recently_visited_first () {
    let mut scheduler = CollateralScheduler::new ();
    let older = ViewUri::ContentView ("older" . into ());
    let newer = ViewUri::ContentView ("newer" . into ());
    scheduler . queue = vec! [newer . clone (), older . clone ()];
    scheduler . note_visit (older . clone (), 2);
    scheduler . note_visit (newer . clone (), 9);
    assert_eq! (scheduler . queue . pop (), Some (newer));
    assert_eq! (scheduler . queue . pop (), Some (older));
  }

  #[test]
  fn foreground_preemption_cancels_but_does_not_overlap_workers () {
    let mut scheduler = CollateralScheduler::new ();
    let uri = ViewUri::ContentView ("rendering" . into ());
    scheduler . in_flight = Some (uri . clone ());
    scheduler . preempt ();
    assert_eq! (scheduler . in_flight, Some (uri . clone ()));
    assert! (scheduler . queue . contains (&uri));
    assert_eq! (scheduler . epoch . load (Ordering::Acquire), 1);
  }

  #[test]
  fn offer_names_complete_base_and_resulting_application_authority () {
    let uri = ViewUri::ContentView ("view-uri" . into ());
    let payload = offer_payload (
      &uri,
      "* rendered\n",
      RenderGeneration {
        graph: GraphGeneration::INITIAL . successor (),
        presentation: 9,
      },
      4,
      GraphGeneration::INITIAL . get (),
      7,
      12,
      Some ("client-buffer"),
      "private",
      "all",
      &[], None, None,
    );
    let Sexp::List (fields) = sexp::parse (&payload) . unwrap () else {
      panic! ("offer payload is not a list"); };
    let field = |key : &str| fields . iter () . find_map (|entry| match entry {
      Sexp::List (parts) if parts . len () == 2
        && parts[0] == atom (key) => crate::types::sexp::atom_to_string (
          &parts[1]) . ok (),
      _ => None,
    });
    assert_eq! (field ("viewforest-base-revision") . as_deref (), Some ("4"));
    assert_eq! (field ("resulting-server-revision") . as_deref (), Some ("5"));
    assert_eq! (field ("view-base-graph-generation") . as_deref (), Some ("1"));
    assert_eq! (field ("view-base-presentation-generation") . as_deref (), Some ("7"));
    assert_eq! (field ("expected-client-application-token") . as_deref (), Some ("12"));
    assert_eq! (field ("resulting-client-application-token") . as_deref (), Some ("13"));
    assert_eq! (field ("client-buffer-id") . as_deref (), Some ("client-buffer"));
    assert_eq! (field ("view-base-source-set") . as_deref (), Some ("private"));
    assert_eq! (field ("resulting-source-set") . as_deref (), Some ("all"));
  }

  #[test]
  fn queued_refresh_names_generation_reason_and_every_view () {
    let refresh = QueuedRefresh {
      operation: "git-presentation-rerender" . into (),
      generation: RenderGeneration {
        graph: GraphGeneration::INITIAL . successor (), presentation: 8,
      },
      view_uris: vec![
        ViewUri::ContentView ("view" . into ()),
        ViewUri::SearchView ("terms" . into ()),
      ],
    };
    let payload = refresh . payload ();
    assert! (payload . contains ("(reason git-presentation-rerender)"));
    assert! (payload . contains ("(graph-generation 2)"));
    assert! (payload . contains ("(presentation-generation 8)"));
    assert! (payload . contains ("(queued-view-uris (view search:terms))"));
  }

  #[test]
  fn exact_rejection_settles_even_after_the_offer_was_superseded () {
    use std::net::{TcpListener, TcpStream};

    let listener = TcpListener::bind ("127.0.0.1:0") . unwrap ();
    let mut client = TcpStream::connect (listener . local_addr () . unwrap ())
      . unwrap ();
    let (mut server, _) = listener . accept () . unwrap ();
    let mut scheduler = scheduler_with_pending_application ();
    let mut views = ViewsState {
      diff_mode_enabled: false,
      open_views: crate::types::views_state::OpenViews::new (),
    };
    scheduler . handle_apply_ack (
      &mut server, &ack_request (1), &mut views);
    use std::io::Read;
    let mut response = [0u8; 512];
    let read = client . read (&mut response) . unwrap ();
    let response = String::from_utf8_lossy (&response [..read]);
    assert! (scheduler . pending_offer . is_none (), "{}", response);
    assert! (response . contains ("client rejected obsolete or dirty view"));
  }

  #[test]
  fn exact_rejection_marks_an_unchanged_view_stale () {
    use std::net::{TcpListener, TcpStream};

    let listener = TcpListener::bind ("127.0.0.1:0") . unwrap ();
    let _client = TcpStream::connect (listener . local_addr () . unwrap ())
      . unwrap ();
    let (mut server, _) = listener . accept () . unwrap ();
    let mut scheduler = scheduler_with_pending_application ();
    let uri = ViewUri::ContentView ("view-uri" . into ());
    let mut views = ViewsState {
      diff_mode_enabled: false,
      open_views: crate::types::views_state::OpenViews::new (),
    };
    views . open_views . register_view_with_authority (
      &crate::dbs::in_rust_graph::InRustGraph::new (), uri . clone (), ViewForest::new (), &[], 1, 7, 12,
      crate::maintenance::BufferKind::ContentView,
      "private" . into (), None);
    views . open_views . views . get_mut (&uri) . unwrap () . revision = 4;
    scheduler . handle_apply_ack (
      &mut server, &ack_request (1), &mut views);
    assert! (views . open_views . views [&uri] . presentation_stale);
    assert! (scheduler . pending_offer . is_none ());
  }

  #[test]
  fn changed_rejection_identity_preserves_the_pending_offer () {
    use std::net::{TcpListener, TcpStream};

    let listener = TcpListener::bind ("127.0.0.1:0") . unwrap ();
    let _client = TcpStream::connect (listener . local_addr () . unwrap ())
      . unwrap ();
    let (mut server, _) = listener . accept () . unwrap ();
    let mut scheduler = scheduler_with_pending_application ();
    let mut views = ViewsState {
      diff_mode_enabled: false,
      open_views: crate::types::views_state::OpenViews::new (),
    };
    scheduler . handle_apply_ack (
      &mut server, &ack_request (99), &mut views);
    assert! (scheduler . pending_offer . is_some ());
  }
}
