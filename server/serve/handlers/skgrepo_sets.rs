use crate::rerender_pass::DiffViewBufferRerender;
use crate::serve::ViewsState;
use crate::serve::rerender_stream::{
  PreparedRerenders,
  authorize_prepared_rerenders,
  RerenderRequest, RerenderSelector, prepare_rerender_views,
  stream_empty_rerender,
  stream_prepared_rerenders};
use crate::serve::handlers::text_release::approved_pids_from_request;
use crate::serve::protocol::{RequestType, TcpToClient};
use crate::serve::util::{
  request_type_from_request,
  send_response_with_length_prefix,
  value_from_request_sexp};
use crate::sexp_string::quote_sexp_string;
use crate::skgrepo_sets::SkgrepoRestriction;
use crate::types::env::{RuntimeGeneration, SkgEnv};
use crate::types::misc::SkgrepoName;
use crate::types::misc::SkgConfig;

use std::net::TcpStream;
use std::sync::Arc;

pub fn handle_skgrepo_set_request (
  stream           : &mut TcpStream,
  request          : &str,
  env              : &SkgEnv,
  views_state      : &mut ViewsState,
  skgrepo_restriction : &mut SkgrepoRestriction,
) {
  let runtime = env . runtime_snapshot ();
  match request_type_from_request (request) {
    Ok (RequestType::ListSkgrepoSets) =>
      send_skgrepo_sets_response (stream, &runtime . config, skgrepo_restriction),
    Ok (RequestType::SkgrepoRestriction) =>
      send_skgrepo_restriction_response (stream, skgrepo_restriction),
    Ok (RequestType::SetSkgrepoRestriction) =>
      set_skgrepo_restriction (
        stream, request, env, runtime, views_state,
        skgrepo_restriction ),
    Ok (_) =>
      // Reachable only from malformed requests no current client
      // sends, but either client may have locked buffers and set its
      // stream-in-progress guard (Emacs's 'skg--stream-in-progress',
      // Neovim's 'stream_in_progress' in 'nvim/lua/skg/lock.lua')
      // before any switch request, so even these paths
      // answer in the unwinding shape.
      refuse_unwinding (
        stream, skgrepo_restriction, "not a request to switch the skgrepo restriction"),
    Err (e) =>
      refuse_unwinding (stream, skgrepo_restriction, &e), }}

/// The request names the most private owned skgrepo to show
/// ('name'), whether to show the foreign skgrepos ('foreign-repos',
/// "shown" or "hidden"), or both; what it leaves out stays as it was.
///
/// Under a skgrepo restriction (the API doc's "Restricted-skgrepo nodes"): a switch
/// re-renders open views in place rather than closing them. The
/// refresh brings each view under the new skgrepo restriction: an occurrence
/// of a node the skgrepo restriction now keeps out becomes a restricted
/// viewnode where its record is visible, and vanishes elsewhere; a
/// member whose record it keeps out vanishes; and what a skgrepo
/// widening reveals is drawn. A folder the
/// skgrepo restriction empties stays, empty, as any empty editable folder
/// does. The rest keeps its
/// viewnode IDs. Results stream via
/// the rerender-all message flow (lock, per-view, done).
fn set_skgrepo_restriction (
  stream           : &mut TcpStream,
  request          : &str,
  env              : &SkgEnv,
  runtime          : Arc<RuntimeGeneration>,
  views_state      : &mut ViewsState,
  skgrepo_restriction : &mut SkgrepoRestriction,
) {
  let restriction : SkgrepoRestriction =
    match requested_skgrepo_restriction (
      request, &runtime . config, skgrepo_restriction ) {
      Ok (restriction) => restriction,
      Err (e) => {
        refuse_unwinding (stream, skgrepo_restriction, &e);
        return; }};
  let mut prepared : PreparedRerenders =
    prepare_rerender_views (
      env, runtime, views_state,
      RerenderRequest {
        restriction : &restriction,
        selector    : RerenderSelector::All,
        diffView_buffer_rerender : DiffViewBufferRerender::RefreshOnly, } );
  if ! authorize_prepared_rerenders (
    stream, &mut prepared, &restriction,
    "repo-set-switch-rerender",
    &approved_pids_from_request (request) ) {
    return; }
  *skgrepo_restriction = restriction;
  send_skgrepo_restriction_response (stream, skgrepo_restriction);
  stream_prepared_rerenders (stream, views_state, prepared); }

/// The skgrepo restriction a switch request asks for: its 'name' (the
/// most private owned skgrepo to show) and its 'foreign-repos'
/// ("shown" or "hidden"), each defaulting to the current one's.
fn requested_skgrepo_restriction (
  request : &str,
  config  : &SkgConfig,
  current : &SkgrepoRestriction,
) -> Result<SkgrepoRestriction, String> {
  if let Err (e) = sexp::parse (request) {
    return Err ( format! ("Failed to parse S-expression: {}", e) ); }
  let name : Option<SkgrepoName> =
    value_from_request_sexp ("name", request) . ok ()
    . map (SkgrepoName::from);
  let foreign_shown : Option<bool> =
    match value_from_request_sexp ("foreign-repos", request) . ok ()
    . as_deref () {
      None           => None,
      Some ("shown")  => Some (true),
      Some ("hidden") => Some (false),
      Some (other)   => return Err ( format! (
        "'foreign-repos' must be \"shown\" or \"hidden\", not {:?}.",
        other )), };
  if name . is_none () && foreign_shown . is_none () {
    return Err ( "A skgrepo restriction switch names a repo ('name'), whether foreign repos are shown ('foreign-repos'), or both." . to_string () ); }
  SkgrepoRestriction::named_with_foreign_shown (
    config,
    name . unwrap_or_else ( || current . most_private_shown . clone () ),
    foreign_shown . unwrap_or ( current . foreign_shown ))
    . map_err ( |e| e . to_string () ) }

/// How the wire says whether foreign skgrepos are shown.
fn foreign_repos_word (
  restriction : &SkgrepoRestriction,
) -> &'static str {
  if restriction . foreign_shown { "shown" } else { "hidden" } }

fn send_skgrepo_sets_response (
  stream      : &mut TcpStream,
  config      : &SkgConfig,
  restriction : &SkgrepoRestriction,
) {
  // The skgrepo restriction's rung is one owned skgrepo, the most
  // private one shown, so the choices are the owned skgrepos
  // themselves, in privacy order (each meaning "this skgrepo and
  // every more public owned one"), the last of them no skgrepo
  // restriction when foreign skgrepos are shown. Order is
  // meaningful; do not sort.
  let names : Vec<String> =
    config . privacy_ladder () . iter ()
    . map ( |name| name . 0 . clone () )
    . collect ();
  let names_sexp : String =
    names . iter ()
    . map ( |name| quote_sexp_string (name) )
    . collect::<Vec<String>> ()
    . join (" ");
  let response : String =
    format! (
      "((response-type {}) (restriction {}) (foreign-repos {}) (sets ({})))",
      TcpToClient::SkgrepoSets . repr_in_client (),
      quote_sexp_string (&restriction . most_private_shown . 0),
      quote_sexp_string ( foreign_repos_word (restriction) ),
      names_sexp );
  send_response_with_length_prefix (stream, &response); }

/// The skgrepo restriction, as the most private owned skgrepo it shows
/// and whether it shows the foreign ones. The clients say whether that
/// is no skgrepo restriction (" (no restriction)" appended to the
/// config's most private owned skgrepo, foreign repos shown); the
/// response carries no words for them to show.
fn send_skgrepo_restriction_response (
  stream : &mut TcpStream,
  restriction : &SkgrepoRestriction,
) {
  let response : String =
    format! (
      "((response-type {}) (restriction {}) (foreign-repos {}))",
      TcpToClient::SkgrepoRestriction . repr_in_client (),
      quote_sexp_string (&restriction . most_private_shown . 0),
      quote_sexp_string ( foreign_repos_word (restriction) ));
  send_response_with_length_prefix (stream, &response); }

/// The unwinding refusal shape (the quiet shape): the endpoint's
/// normal skgrepo-restriction response-type carrying explanatory text
/// in 'content' and the unchanged skgrepo restriction, followed by an empty rerender
/// stream.  Each client locks all Skg buffers and sets its
/// stream-in-progress guard before sending a switch request; a
/// response-type it has no handler for would leave it wedged, so
/// refusals and errors alike must answer in this shape.
fn refuse_unwinding (
  stream : &mut TcpStream,
  restriction : &SkgrepoRestriction,
  msg    : &str,
) {
  let response : String =
    format! (
      "((response-type {}) (restriction {}) (foreign-repos {}) (content {}))",
      TcpToClient::SkgrepoRestriction . repr_in_client (),
      quote_sexp_string (&restriction . most_private_shown . 0),
      quote_sexp_string ( foreign_repos_word (restriction) ),
      quote_sexp_string (msg));
  send_response_with_length_prefix (stream, &response);
  stream_empty_rerender (stream); }
