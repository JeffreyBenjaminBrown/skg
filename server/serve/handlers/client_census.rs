//! Two-step client census and retained-view reattachment.

use crate::from_text::buffer_to_viewnodes::uninterpreted::org_to_uninterpreted_viewforest;
use crate::maintenance::BufferKind;
use crate::runtime::interactive_session::{CensusDescriptor, InteractiveSession};
use crate::serve::protocol::TcpToClient;
use crate::serve::util::{
  read_length_prefixed_content,
  send_response_with_length_prefix,
  tag_sexp_response,
};
use crate::types::env::SkgEnv;
use crate::types::maybe_placed_viewnode::maybePlaced_to_placed_viewforest;
use crate::types::sexp::extract_v_from_kv_pair_in_sexp;
use crate::types::views_state::{ViewUri, pids_from_viewforest};

use sexp::{Atom, Sexp};
use sha2::{Digest, Sha256};
use std::collections::HashSet;
use std::io::BufReader;
use std::net::TcpStream;

pub fn handle_client_census_request (
  reader       : &mut BufReader<TcpStream>,
  stream       : &mut TcpStream,
  env          : &SkgEnv,
  interactive  : &mut InteractiveSession,
  writes_allowed : bool,
) {
  let result = (|| -> Result<String, String> {
    let payload = read_length_prefixed_content (reader)
      . map_err (|error| format! ("could not read client census: {}", error))?;
    let descriptors = parse_descriptors (&payload)?;
    let current_generation = env . in_rust_graph . load_full ()
      . graph_generation . get ();
    let mut live_uris : HashSet<ViewUri> = HashSet::new ();
    let mut text_required : Vec<String> = Vec::new ();
    let mut stale : Vec<String> = Vec::new ();
    interactive . pending_census_texts . clear ();
    interactive . live_census = descriptors . iter () . map (|descriptor|
      (descriptor . buffer_id . clone (), descriptor . clone ())) . collect ();

    for descriptor in descriptors {
      let Some (uri) = descriptor . view_uri . clone () else { continue; };
      if !live_uris . insert (uri . clone ()) {
        return Err (format! (
          "client census names view '{}' more than once",
          uri . repr_in_client ())); }
      match interactive . views . open_views . views . get_mut (&uri) {
        Some (state)
          if state . graph_generation == descriptor . graph_generation
          && state . revision == descriptor . server_revision
          && state . client_application_token == descriptor . application_token =>
        {
          state . client_buffer_id = Some (descriptor . buffer_id . clone ());
        }
        Some (_) => stale . push (descriptor . buffer_id . clone ()),
        None if descriptor . graph_generation == current_generation => {
          text_required . push (descriptor . buffer_id . clone ());
          interactive . pending_census_texts . insert (
            descriptor . buffer_id . clone (), descriptor);
        }
        None => stale . push (descriptor . buffer_id . clone ()),
      }
    }

    let absent_server_views : Vec<ViewUri> = interactive . views . open_views
      . views . keys () . filter (|uri| !live_uris . contains (*uri))
      . cloned () . collect ();
    for uri in absent_server_views {
      interactive . views . open_views . unregister_view (&uri); }

    let complete = text_required . is_empty ();
    if let Some (client) = &mut interactive . attached_client {
      client . census_complete = complete; }
    Ok (census_response (
      complete, writes_allowed && complete, &text_required, &stale))
  })();
  send_result (stream, result);
}

pub fn handle_client_census_texts_request (
  reader       : &mut BufReader<TcpStream>,
  stream       : &mut TcpStream,
  env          : &SkgEnv,
  interactive  : &mut InteractiveSession,
  writes_allowed : bool,
) {
  let result = (|| -> Result<String, String> {
    let payload = read_length_prefixed_content (reader)
      . map_err (|error| format! ("could not read census texts: {}", error))?;
    let records = parse_text_records (&payload)?;
    let mut restored : Vec<String> = Vec::new ();
    let mut stale : Vec<String> = Vec::new ();
    for (buffer_id, last_fetched, current) in records {
      let Some (descriptor) = interactive . pending_census_texts
        . remove (&buffer_id)
      else {
        return Err (format! (
          "census texts include unrequested buffer '{}'", buffer_id)); };
      if descriptor . graph_generation != env . in_rust_graph . load_full ()
        . graph_generation . get ()
      {
        stale . push (buffer_id);
        continue; }
      if sha256 (&last_fetched) != descriptor . last_fetched_sha256
         || sha256 (&current) != descriptor . current_sha256
      {
        stale . push (buffer_id);
        continue; }
      let Some (uri) = descriptor . view_uri . clone () else {
        continue; };
      let (maybe_placed, parse_errors, _) = org_to_uninterpreted_viewforest (
        &last_fetched).map_err (|error| format! (
          "could not reconstruct '{}': {}", buffer_id, error))?;
      if !parse_errors . is_empty () {
        stale . push (buffer_id);
        continue; }
      let viewforest = maybePlaced_to_placed_viewforest (maybe_placed)
        . map_err (|error| format! (
          "could not place reconstructed '{}': {}", buffer_id, error))?;
      let pids : Vec<_> = pids_from_viewforest (&viewforest)
        . into_iter () . collect ();
      interactive . views . open_views . register_view_with_authority (
        uri . clone (), viewforest, &pids,
        descriptor . graph_generation,
        0,
        descriptor . application_token,
        parse_kind (&descriptor . kind)?,
        None);
      interactive . views . open_views . views . get_mut (&uri)
        . expect ("reconstructed census view exists")
        . revision = descriptor . server_revision;
      interactive . views . open_views . views . get_mut (&uri)
        . expect ("reconstructed census view exists")
        . client_buffer_id = Some (descriptor . buffer_id . clone ());
      restored . push (descriptor . buffer_id);
    }
    stale . extend (
      interactive . pending_census_texts . keys () . cloned ());
    interactive . pending_census_texts . clear ();
    if let Some (client) = &mut interactive . attached_client {
      client . census_complete = true; }
    let mut response = census_response (
      true, writes_allowed, &[], &stale);
    let Ok (Sexp::List (mut fields)) = sexp::parse (&response) else {
      unreachable! (); };
    fields . push (list_field ("restored-buffer-ids", &restored));
    response = Sexp::List (fields) . to_string ();
    Ok (response)
  })();
  send_result (stream, result);
}

fn parse_descriptors (payload : &str) -> Result<Vec<CensusDescriptor>, String> {
  let parsed = sexp::parse (payload)
    . map_err (|error| format! ("invalid census S-expression: {}", error))?;
  let records = match parsed {
    Sexp::List (records) => records,
    Sexp::Atom (Atom::S (nil)) if nil == "nil" => Vec::new (),
    _ => return Err ("client census must be a list" . into ()),
  };
  let mut result = Vec::new ();
  let mut ids = HashSet::new ();
  for record in records {
    if !matches! (record, Sexp::List (_)) {
      return Err ("each census descriptor must be a list" . into ()); }
    let buffer_id = field (&record, "buffer-id")?;
    if !ids . insert (buffer_id . clone ()) {
      return Err (format! ("duplicate census buffer-id '{}'", buffer_id)); }
    let uri = field (&record, "view-uri")?;
    let dirty = field (&record, "dirty")? == "true";
    result . push (CensusDescriptor {
      buffer_id,
      kind: field (&record, "kind")?,
      view_uri: if uri == "nil" { None }
                else { Some (ViewUri::from_client_string (uri)) },
      graph_generation: unsigned_field (&record, "graph-generation")?,
      server_revision: unsigned_field (&record, "server-revision")?,
      application_token: unsigned_field (&record, "application-token")?,
      dirty,
      // A pre-extension peer which omits this field is conservative: every
      // dirty record might carry native undo which must not be discarded.
      undo_required: field (&record, "undo-required")
        . map (|value| value == "true") . unwrap_or (dirty),
      last_fetched_sha256: field (&record, "last-fetched-sha256")?,
      current_sha256: field (&record, "current-sha256")?,
    });
  }
  Ok (result)
}

fn parse_text_records (
  payload : &str,
) -> Result<Vec<(String, String, String)>, String> {
  let parsed = sexp::parse (payload)
    . map_err (|error| format! ("invalid census text S-expression: {}", error))?;
  let Sexp::List (records) = parsed else {
    return Err ("census texts must be a list" . into ()); };
  records . into_iter () . map (|record| Ok ((
    field (&record, "buffer-id")?,
    field (&record, "last-fetched")?,
    field (&record, "current")?,
  ))) . collect ()
}

fn field (record : &Sexp, key : &str) -> Result<String, String> {
  extract_v_from_kv_pair_in_sexp (record, key)
}

fn unsigned_field (record : &Sexp, key : &str) -> Result<u64, String> {
  field (record, key)? . parse::<u64> ()
    . map_err (|_| format! ("census field '{}' must be unsigned", key))
}

fn parse_kind (kind : &str) -> Result<BufferKind, String> {
  match kind {
    "content-view" => Ok (BufferKind::ContentView),
    "new-empty-content-view" => Ok (BufferKind::NewEmptyContentView),
    "search-view" => Ok (BufferKind::SearchView),
    "override-choice-menu" => Ok (BufferKind::OverrideChoiceMenu),
    other => Err (format! (
      "buffer kind '{}' cannot be reconstructed as a live view", other)),
  }
}

fn sha256 (text : &str) -> String {
  format! ("{:x}", Sha256::digest (text . as_bytes ()))
}

fn census_response (
  complete      : bool,
  write_enabled : bool,
  text_required : &[String],
  stale         : &[String],
) -> String {
  Sexp::List (vec![
    atom_field ("census-complete", if complete { "true" } else { "nil" }),
    atom_field ("write-enabled", if write_enabled { "true" } else { "nil" }),
    list_field ("text-required-buffer-ids", text_required),
    list_field ("stale-buffer-ids", stale),
  ]) . to_string ()
}

fn atom_field (key : &str, value : &str) -> Sexp {
  Sexp::List (vec![
    Sexp::Atom (Atom::S (key . into ())),
    Sexp::Atom (Atom::S (value . into ())),
  ])
}

fn list_field (key : &str, values : &[String]) -> Sexp {
  Sexp::List (vec![
    Sexp::Atom (Atom::S (key . into ())),
    Sexp::List (values . iter () . map (|value|
      Sexp::Atom (Atom::S (value . clone ()))) . collect ()),
  ])
}

fn send_result (stream : &mut TcpStream, result : Result<String, String>) {
  let response = match result {
    Ok (payload) => tag_sexp_response (TcpToClient::ClientCensus, &payload),
    Err (error) => crate::serve::util::tag_terminal_text_response (
      TcpToClient::Error, "failed", &error),
  };
  let _ = send_response_with_length_prefix (stream, &response);
}
