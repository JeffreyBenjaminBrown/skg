//! Partial-rebuild integration at the otherwise generic TCP connection seam.

use crate::runtime::ServerRuntime;
use crate::runtime::interactive_session::InteractiveSession;
use crate::serve::handlers::reload_batch::{
  reconciliation_generation,
  release_connection_reload_batches,
};
use crate::serve::protocol::{RequestType, TcpToClient};
use crate::serve::util::{
  request_context_active,
  send_response_with_length_prefix,
  tag_server_push_sexp_response,
};

use sexp::{Atom, Sexp};
use std::collections::HashSet;
use std::net::TcpStream;

pub fn current_reconciliation_generation () -> u64 {
  reconciliation_generation ()
}

pub fn request_allowed_before_census (request_type : RequestType) -> bool {
  matches! (request_type,
    RequestType::VerifyConnection
    | RequestType::ClientCensus
    | RequestType::ClientCensusTexts
    | RequestType::MaintenanceLockedCensus)
}

pub fn request_requires_collateral_preemption (
  request_type : RequestType,
) -> bool {
  !matches! (request_type,
    RequestType::ApplyCollateral
    | RequestType::ViewVisited
    | RequestType::ObservePresentation
    | RequestType::ClientCensus
    | RequestType::ClientCensusTexts)
}

pub fn handle_idle_maintenance_events (
  stream                         : &mut TcpStream,
  runtime                        : &ServerRuntime,
  interactive_connection         : bool,
  snapshot_requested             : bool,
  seen_reconciliation_generation : &mut u64,
) {
  if request_context_active () { return; }
  let reconciliation = reconciliation_generation ();
  if interactive_connection
  && reconciliation > *seen_reconciliation_generation
  {
    let payload = Sexp::List (vec![
      Sexp::List (vec![
        Sexp::Atom (Atom::S ("content" . into ())),
        Sexp::Atom (Atom::S (
          "External reload batch closed; run one exact full manifest sweep"
            . into ())),
      ]),
      Sexp::List (vec![
        Sexp::Atom (Atom::S ("sweep-generation" . into ())),
        Sexp::Atom (Atom::I (reconciliation as i64)),
      ]),
    ]) . to_string ();
    let _ = send_response_with_length_prefix (
      stream, &tag_server_push_sexp_response (
        TcpToClient::ReconciliationReady,
        &format! ("reconciliation-{}", reconciliation),
        &payload));
    *seen_reconciliation_generation = reconciliation;
  }
  if !interactive_connection { return; }
  let mut server_event_send_failed = false;
  loop {
    let event = runtime . interactive . lock () . unwrap ()
      . queued_server_events . pop_front ();
    let Some (event) = event else { break; };
    let response_type = match event . frame_kind . as_str () {
      "maintenance-offer" => TcpToClient::MaintenanceOffer,
      "maintenance-status" => TcpToClient::MaintenanceStatus,
      "refresh-queued" => TcpToClient::RefreshQueued,
      other => {
        tracing::error! (frame_kind = other,
          "discarding unknown queued server event kind");
        continue; }
    };
    if send_response_with_length_prefix (
      stream, &tag_server_push_sexp_response (
        response_type, &event . operation_id, &event . payload)) . is_err ()
    {
      runtime . interactive . lock () . unwrap ()
        . queued_server_events . push_front (event);
      server_event_send_failed = true;
      break;
    }
  }
  let maintenance_locked = runtime . maintenance . lock () . unwrap ()
    . state . policy () . maintenance_locked;
  if collateral_pump_allowed (
      snapshot_requested, server_event_send_failed, maintenance_locked)
  {
    let mut interactive = runtime . interactive . lock () . unwrap ();
    let InteractiveSession { views, collateral_scheduler, .. } =
      &mut *interactive;
    collateral_scheduler . pump (stream, views);
  }
}

pub fn finish_connection_maintenance (
  runtime                   : &ServerRuntime,
  interactive_connection    : bool,
  owned_reload_batch_tokens : &mut HashSet<String>,
) {
  release_connection_reload_batches (runtime, owned_reload_batch_tokens);
  if !interactive_connection { return; }
  runtime . maintenance . lock () . unwrap () . disconnected ();
  runtime . persist_maintenance_state ();
  if let Ok (mut interactive) = runtime . interactive . lock () {
    if let Some (client) = &mut interactive . attached_client {
      client . census_complete = false; }}
}

fn collateral_pump_allowed (
  snapshot_requested  : bool,
  server_event_failed : bool,
  maintenance_locked  : bool,
) -> bool {
  !snapshot_requested && !server_event_failed && !maintenance_locked
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn collateral_pump_waits_for_snapshot_event_and_maintenance_boundaries () {
    assert! (collateral_pump_allowed (false, false, false));
    assert! (!collateral_pump_allowed (true, false, false));
    assert! (!collateral_pump_allowed (false, true, false));
    assert! (!collateral_pump_allowed (false, false, true));
  }
}
