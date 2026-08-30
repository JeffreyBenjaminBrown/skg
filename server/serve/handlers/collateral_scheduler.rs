//! Connection-local, single-worker background view rendering.
//!
//! A graph transition replaces the remaining queue rather than appending to
//! it.  Workers never touch TCP or `OpenViews`; they return an immutable
//! proposal which the connection thread scalar-gates and offers to the
//! client.  The server forest advances only after the client ACKs that exact
//! operation against the revision from which it was rendered.

use crate::serve::ViewsState;
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
}

struct WorkerResult {
  uri           : ViewUri,
  base_revision : u64,
  generation    : RenderGeneration,
  epoch         : u64,
  result        : Result<(ViewForest, String, Vec<String>), String>,
}

struct PendingOffer {
  uri           : ViewUri,
  base_revision : u64,
  generation    : RenderGeneration,
  viewforest    : Option<ViewForest>,
}

pub struct CollateralScheduler {
  epoch          : Arc<AtomicU64>,
  batch          : Option<BatchContext>,
  queue          : Vec<ViewUri>,
  visit_sequence : HashMap<ViewUri, u64>,
  in_flight      : Option<ViewUri>,
  pending_offer  : Option<(String, PendingOffer)>,
  next_operation : u64,
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
      sender,
      receiver,
    }
  }

  /// Replace all obsolete work after a successful graph transition.
  pub fn replace_after_transition (
    &mut self,
    saved_uri           : Option<&ViewUri>,
    views_state         : &ViewsState,
    env                 : &SkgEnv,
    define_nodes        : &[DefineNode],
    active_source_set   : &ActiveSourceSet,
    scalar_approved     : &HashSet<ID>,
  ) {
    let epoch = self . epoch . fetch_add (1, Ordering::AcqRel) + 1;
    self . pending_offer = None;
    self . queue = views_state . open_views . views . keys ()
      . filter (|uri| saved_uri != Some (*uri))
      . cloned ()
      . collect ();
    self . sort_queue ();
    self . batch = Some (BatchContext {
      generation: RenderGeneration {
        graph: env . in_rust_graph . load_full () . graph_generation,
        presentation: 0,
      },
      epoch,
      env: env . clone (),
      define_nodes: define_nodes . to_vec (),
      diff_mode_enabled: views_state . diff_mode_enabled,
      active_source_set: active_source_set . clone (),
      scalar_approved: scalar_approved . clone (),
    });
  }

  /// A foreground operation gets first use of TypeDB.  The current worker is
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
    let input_ids = active_ids_in_viewforest (&viewforest);
    match decide (
      "background-rerender", &batch . active_source_set, &input_ids,
      &batch . env . in_rust_graph_snapshot (), &batch . scalar_approved)
    {
      ScalarReleaseDecision::Challenge { pids, prompt, .. } => {
        self . offer_challenge (
          stream, &uri, base_revision, &batch, pids, &prompt);
        return; }
      ScalarReleaseDecision::AllowWithWarning { .. }
      | ScalarReleaseDecision::Allow => {}
    }
    self . in_flight = Some (uri . clone ());
    let sender = self . sender . clone ();
    let cancellation = RenderCancellationTicket::new (
      self . epoch . clone (), batch . epoch);
    thread::spawn (move || {
      let result = block_on (render_background_view (
        viewforest,
        &batch . define_nodes,
        &batch . env,
        batch . diff_mode_enabled,
        Some (&batch . active_source_set),
        cancellation));
      let _ = sender . send (WorkerResult {
        uri,
        base_revision,
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
    let Some (batch) = &self . batch else { return; };
    if finished . epoch != batch . epoch
       || finished . generation != batch . generation
       || self . epoch . load (Ordering::Acquire) != finished . epoch
       || views_state . open_views . view_revision (&finished . uri)
          != Some (finished . base_revision)
       || batch . env . in_rust_graph . load_full () . graph_generation
          != finished . generation . graph
    { return; }
    let (viewforest, content, mut warnings) = match finished . result {
      Ok (rendered) => rendered,
      Err (error) => {
        if error != "obsolete background render cancelled" {
          tracing::warn! (uri = %finished . uri . repr_in_client (),
                          %error, "background view render failed"); }
        return; }
    };
    let output_ids = active_ids_in_viewforest (&viewforest);
    match decide (
      "background-rerender", &batch . active_source_set, &output_ids,
      &batch . env . in_rust_graph_snapshot (), &batch . scalar_approved)
    {
      ScalarReleaseDecision::Challenge { pids, prompt, .. } => {
        let uri = finished . uri . clone ();
        let batch = batch . clone ();
        self . offer_challenge (
          stream, &uri, finished . base_revision, &batch, pids, &prompt);
        return; }
      ScalarReleaseDecision::AllowWithWarning { warning } =>
        warnings . push (warning),
      ScalarReleaseDecision::Allow => {}
    }
    let operation_id = self . fresh_operation_id ();
    let payload = offer_payload (
      &finished . uri, &content, finished . generation,
      finished . base_revision, &warnings, None);
    send_response_with_length_prefix (stream,
      &tag_server_push_sexp_response (
        TcpToClient::CollateralView, &operation_id, &payload));
    self . pending_offer = Some ((operation_id, PendingOffer {
      uri: finished . uri,
      base_revision: finished . base_revision,
      generation: finished . generation,
      viewforest: Some (viewforest),
    }));
  }

  fn offer_challenge (
    &mut self,
    stream : &mut TcpStream,
    uri    : &ViewUri,
    base_revision : u64,
    batch  : &BatchContext,
    pids   : Vec<ID>,
    prompt : &str,
  ) {
    let operation_id = self . fresh_operation_id ();
    let payload = offer_payload (
      uri, "", batch . generation, base_revision, &[],
      Some ((pids, prompt)));
    send_response_with_length_prefix (stream,
      &tag_server_push_sexp_response (
        TcpToClient::CollateralView, &operation_id, &payload));
    self . pending_offer = Some ((operation_id, PendingOffer {
      uri: uri . clone (),
      base_revision,
      generation: batch . generation,
      viewforest: None,
    }));
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
      let applied = value_from_request_sexp ("applied", request)? == "true";
      let graph_generation = parse_u64_field (request, "graph-generation")?;
      let presentation_generation =
        parse_u64_field (request, "presentation-generation")?;
      let base_revision = parse_u64_field (
        request, "viewforest-base-revision")?;
      let _client_token = parse_u64_field (request, "client-token")?;
      let Some ((pending_id, pending)) = self . pending_offer . take ()
      else { return Err ("No collateral offer is pending" . into ()); };
      if pending_id != operation_id || pending . uri != uri {
        self . pending_offer = Some ((pending_id, pending));
        return Err ("Collateral ACK does not name the pending operation"
          . into ()); }
      if pending . generation . graph . get () != graph_generation
         || pending . generation . presentation != presentation_generation
         || pending . base_revision != base_revision
      {
        return Err ("Collateral ACK changed its render identity" . into ()); }
      if ! applied { return Ok ("client rejected obsolete or dirty view"); }
      let Some (viewforest) = pending . viewforest else {
        return Err ("Cannot apply a text-free authorization challenge"
          . into ()); };
      let current_graph = self . batch . as_ref ()
        . map (|batch| batch . generation . graph);
      if current_graph != Some (pending . generation . graph) {
        return Err ("Collateral offer belongs to an obsolete graph generation"
          . into ()); }
      if ! views_state . open_views . update_view_if_revision (
          &pending . uri, pending . base_revision, viewforest)
      { return Err ("Collateral view advanced before its ACK" . into ()); }
      Ok ("collateral view application acknowledged")
    })();
    match result {
      Ok (message) => send_response_with_length_prefix (stream,
        &tag_text_response (TcpToClient::CollateralApplied, message)),
      Err (error) => send_response_with_length_prefix (stream,
        &tag_text_response (TcpToClient::Error, &error)),
    }
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

fn parse_u64_field (request : &str, field : &str) -> Result<u64, String> {
  value_from_request_sexp (field, request)? . parse::<u64> ()
    . map_err (|_| format! ("Collateral ACK has invalid {}", field))
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
  warnings      : &[String],
  challenge     : Option<(Vec<ID>, &str)>,
) -> String {
  let mut fields = vec! [
    pair ("view-uri", atom (&uri . repr_in_client ())),
    pair ("graph-generation", Sexp::Atom (Atom::I (
      generation . graph . get () as i64))),
    pair ("presentation-generation", Sexp::Atom (Atom::I (
      generation . presentation as i64))),
    pair ("viewforest-base-revision", Sexp::Atom (Atom::I (
      base_revision as i64))),
    pair ("warnings", Sexp::List (warnings . iter ()
      . map (|warning| atom (warning)) . collect ())),
  ];
  if let Some ((pids, prompt)) = challenge {
    fields . push (pair ("needs-authorization", atom ("true")));
    fields . push (pair ("pids", Sexp::List (pids . iter ()
      . map (|pid| atom (pid . as_str ())) . collect ())));
    fields . push (pair ("prompt", atom (prompt)));
  } else {
    fields . push (pair ("content", atom (content)));
  }
  Sexp::List (fields) . to_string ()
}

#[cfg(test)]
mod tests {
  use super::*;

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
}
