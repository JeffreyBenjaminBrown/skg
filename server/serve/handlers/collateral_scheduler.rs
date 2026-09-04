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
  base_view_graph_generation : u64,
  base_view_presentation_generation : u64,
  base_client_application_token : u64,
  client_buffer_id : Option<String>,
  generation    : RenderGeneration,
  epoch         : u64,
  result        : Result<(ViewForest, String, Vec<String>), String>,
}

struct PendingOffer {
  uri           : ViewUri,
  base_revision : u64,
  base_view_graph_generation : u64,
  base_view_presentation_generation : u64,
  base_client_application_token : u64,
  client_buffer_id : Option<String>,
  generation    : RenderGeneration,
  viewforest    : Option<ViewForest>,
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
    views_state         : &ViewsState,
    env                 : &SkgEnv,
    define_nodes        : &[DefineNode],
    active_source_set   : &ActiveSourceSet,
    scalar_approved     : &HashSet<ID>,
  ) {
    self . replace_queue (
      saved_uri, views_state, env, define_nodes, active_source_set,
      scalar_approved);
  }

  fn replace_queue (
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
        presentation: self . presentation_generation,
      },
      epoch,
      env: env . clone (),
      define_nodes: define_nodes . to_vec (),
      diff_mode_enabled: views_state . diff_mode_enabled,
      active_source_set: active_source_set . clone (),
      scalar_approved: scalar_approved . clone (),
    });
  }

  pub fn seed_presentation (&mut self, env : &SkgEnv) -> Result<(), String> {
    self . presentation_signature = Some (presentation_signature (&env . config)?);
    Ok (( ))
  }

  pub fn presentation_generation (&self) -> u64 {
    self . presentation_generation
  }

  /// Observe HEAD/index independently of worktree selection. A changed
  /// signature rerenders every view only while diff mode is enabled.
  pub fn observe_presentation (
    &mut self,
    views_state       : &ViewsState,
    env               : &SkgEnv,
    active_source_set : &ActiveSourceSet,
  ) -> Result<bool, String> {
    let signature = presentation_signature (&env . config)?;
    let previous = self . presentation_signature . replace (signature);
    if previous == Some (signature) { return Ok (false); }
    if previous . is_none () { return Ok (false); }
    self . presentation_generation = self . presentation_generation
      . checked_add (1) . ok_or ("presentation generation exhausted")?;
    if views_state . diff_mode_enabled {
      self . replace_queue (
        None, views_state, env, &[], active_source_set, &HashSet::new ()); }
    Ok (true)
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
    let base_view_graph_generation = state . graph_generation;
    let base_view_presentation_generation = state . presentation_generation;
    let base_client_application_token = state . client_application_token;
    let client_buffer_id = state . client_buffer_id . clone ();
    let input_ids = active_ids_in_viewforest (&viewforest);
    match decide (
      "background-rerender", &batch . active_source_set, &input_ids,
      &batch . env . in_rust_graph_snapshot (), &batch . scalar_approved)
    {
      ScalarReleaseDecision::Challenge { pids, prompt, .. } => {
        self . offer_challenge (
          stream, &uri, base_revision, base_view_graph_generation,
          base_view_presentation_generation,
          base_client_application_token,
          client_buffer_id, &batch, pids, &prompt);
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
        base_view_graph_generation,
        base_view_presentation_generation,
        base_client_application_token,
        client_buffer_id,
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
       || views_state . open_views . views . get (&finished . uri)
          . map (|state| state . client_application_token)
          != Some (finished . base_client_application_token)
       || views_state . open_views . views . get (&finished . uri)
          . map (|state| state . graph_generation)
          != Some (finished . base_view_graph_generation)
       || views_state . open_views . views . get (&finished . uri)
          . map (|state| state . presentation_generation)
          != Some (finished . base_view_presentation_generation)
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
          stream, &uri, finished . base_revision,
          finished . base_view_graph_generation,
          finished . base_view_presentation_generation,
          finished . base_client_application_token,
          finished . client_buffer_id,
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
      finished . client_buffer_id . as_deref (), &warnings, None);
    if send_response_with_length_prefix (stream,
      &tag_server_push_sexp_response (
        TcpToClient::CollateralView, &operation_id, &payload)) . is_err () {
      return; }
    self . pending_offer = Some ((operation_id, PendingOffer {
      uri: finished . uri,
      base_revision: finished . base_revision,
      base_view_graph_generation: finished . base_view_graph_generation,
      base_view_presentation_generation:
        finished . base_view_presentation_generation,
      base_client_application_token:
        finished . base_client_application_token,
      client_buffer_id: finished . client_buffer_id,
      generation: finished . generation,
      viewforest: Some (viewforest),
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
    batch  : &BatchContext,
    pids   : Vec<ID>,
    prompt : &str,
  ) {
    let operation_id = self . fresh_operation_id ();
    let payload = offer_payload (
      uri, "", batch . generation, base_revision,
      base_view_graph_generation, base_view_presentation_generation,
      base_client_application_token, client_buffer_id . as_deref (), &[],
      Some ((pids, prompt)));
    if send_response_with_length_prefix (stream,
      &tag_server_push_sexp_response (
        TcpToClient::CollateralView, &operation_id, &payload)) . is_err () {
      return; }
    self . pending_offer = Some ((operation_id, PendingOffer {
      uri: uri . clone (),
      base_revision,
      base_view_graph_generation,
      base_view_presentation_generation,
      base_client_application_token,
      client_buffer_id,
      generation: batch . generation,
      viewforest: None,
      batch_epoch: Some (batch . epoch),
    }));
  }

  /// Stage text returned by a foreground continuation without advancing the
  /// retained forest.  The same exact ACK path as collateral work owns the
  /// eventual forest/token transition.
  pub fn stage_view_application (
    &mut self,
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
    };
    self . pending_offer = Some ((operation_id, PendingOffer {
      uri: uri . clone (),
      base_revision: state . revision,
      base_view_graph_generation: state . graph_generation,
      base_view_presentation_generation: state . presentation_generation,
      base_client_application_token: state . client_application_token,
      client_buffer_id: state . client_buffer_id . clone (),
      generation,
      viewforest: Some (viewforest),
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
      let applied = value_from_request_sexp ("applied", request)? == "true";
      let graph_generation = parse_u64_field (request, "graph-generation")?;
      let presentation_generation =
        parse_u64_field (request, "presentation-generation")?;
      let base_revision = parse_u64_field (
        request, "viewforest-base-revision")?;
      let client_token = parse_u64_field (request, "client-token")?;
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
      let Some (current) = views_state . open_views . views . get (&uri) else {
        return Err ("Collateral view closed before its ACK" . into ()); };
      if current . revision != pending . base_revision
      || current . graph_generation
         != pending . base_view_graph_generation
      || current . presentation_generation
         != pending . base_view_presentation_generation
      || current . client_application_token
         != pending . base_client_application_token
      || (pending . client_buffer_id . is_some ()
          && current . client_buffer_id != pending . client_buffer_id)
      {
        return Err ("Collateral view authority advanced before its ACK"
          . into ()); }
      if ! applied {
        if client_token != pending . base_client_application_token {
          return Err ("Rejected collateral ACK changed the client token"
            . into ()); }
        return Ok ("client rejected obsolete or dirty view"); }
      let resulting_client_token = pending . base_client_application_token
        . checked_add (1)
        . ok_or ("client application token exhausted")?;
      if client_token != resulting_client_token {
        return Err (format! (
          "Collateral ACK token {} is not the required next token {}",
          client_token, resulting_client_token)); }
      let Some (viewforest) = pending . viewforest else {
        return Err ("Cannot apply a text-free authorization challenge"
          . into ()); };
      if let Some (epoch) = pending . batch_epoch {
        let current_batch = self . batch . as_ref ()
          . map (|batch| (batch . epoch, batch . generation . graph));
        if current_batch != Some ((epoch, pending . generation . graph)) {
          return Err (
            "Collateral offer belongs to an obsolete graph generation"
              . into ()); }}
      if ! views_state . open_views . update_view_if_revision (
          &pending . uri, pending . base_revision, viewforest)
      { return Err ("Collateral view advanced before its ACK" . into ()); }
      views_state . open_views . set_client_application_authority (
        &pending . uri,
        pending . generation . graph . get (),
        pending . generation . presentation,
        resulting_client_token)?;
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
  base_view_graph_generation : u64,
  base_view_presentation_generation : u64,
  base_client_application_token : u64,
  client_buffer_id : Option<&str>,
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
    pair ("warnings", Sexp::List (warnings . iter ()
      . map (|warning| atom (warning)) . collect ())),
  ];
  if let Some (buffer_id) = client_buffer_id {
    fields . push (pair ("client-buffer-id", atom (buffer_id))); }
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
  ]);
  if let Some (buffer_id) = &offer . client_buffer_id {
    fields . push (pair ("client-buffer-id", atom (buffer_id))); }
  Ok (Sexp::List (fields) . to_string ())
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
      &[],
      None,
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
  }
}
