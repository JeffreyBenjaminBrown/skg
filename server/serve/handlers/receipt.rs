//! The "receipt" request ('docs/api-and-formats.org', "Receipt"):
//! plan a read receipt ('server/receipt.rs'), and unless the request
//! is a preview, write it as one batch and rerender the open views
//! that show what it changed. Every answer is a "receipt" response
//! followed by a rerender stream, empty for a preview or a refusal.
//! The request is itself the approval of the fork it may make, so no
//! fork confirmation is asked.

use crate::dbs::in_rust_graph::mirrors::mirror_pids_ignoring_restriction;
use crate::dbs::in_rust_graph::new_handle;
use crate::receipt::{
  DEFAULT_RECEIPT_TITLE, ReceiptPlan, ReceiptRefusal, ReceiptRequest, plan_receipt};
use crate::rerender_pass::DiffViewBufferRerender;
use crate::save::update_graph_minus_nodeMerges_with_hoist_approval;
use crate::serve::ViewsState;
use crate::serve::protocol::TcpToClient;
use crate::serve::rerender_stream::{
  PreparedRerenders, RerenderRequest, RerenderSelector,
  leave_out_views_needing_text_release, prepare_rerender_views,
  stream_empty_rerender, stream_prepared_rerenders};
use crate::serve::util::{
  format_string_list_sexp, send_response_with_length_prefix,
  sexp_to_client_string, tag_sexp_response, value_from_request_sexp};
use crate::skgrepo_sets::SkgrepoRestriction;
use crate::types::env::SkgEnv;
use crate::types::misc::{ID, SkgrepoName};
use crate::types::sexp::extract_string_list_from_sexp;
use crate::types::views_state::ViewforestId;
use crate::util::path_from_pid_and_skgrepo;

use futures::executor::block_on;
use sexp::{Atom, Sexp};
use std::collections::HashSet;
use std::net::TcpStream;

pub fn handle_receipt_request (
  stream              : &mut TcpStream,
  request             : &str,
  env                 : &mut SkgEnv,
  views_state         : &mut ViewsState,
  skgrepo_restriction : &SkgrepoRestriction,
) {
  let (receipt_request, preview, excluded) :
    (ReceiptRequest, bool, HashSet<ViewforestId>) =
    match receipt_request_from (request) {
      Ok (parsed) => parsed,
      Err (e) => return refuse (stream, &ReceiptRefusal::Plain (e)), };
  // Planning and writing are one mutation, so that the plan is the
  // graph's when it is written.
  let mutation_gate = env . mutation_gate ();
  let mutation_guard = block_on (mutation_gate . lock ());
  let runtime = env . runtime_snapshot ();
  let plan : ReceiptPlan = match plan_receipt (
    &runtime . graph, &runtime . config, skgrepo_restriction, &receipt_request )
  { Ok (plan) => plan,
    Err (refusal) => return refuse (stream, &refusal), };
  if preview {
    drop (mutation_guard);
    send_response_with_length_prefix (
      stream, &tag_sexp_response (
        TcpToClient::Receipt, &plan_response (&plan, true) ));
    stream_empty_rerender (stream);
    return; }
  let working_graph = new_handle ( (*runtime . graph) . clone () );
  if let Err (e) = update_graph_minus_nodeMerges_with_hoist_approval (
    plan . node_instructions (), &[], (*runtime . config) . clone (),
    &runtime . tantivy_index, &working_graph, &HashSet::new () )
  { return refuse (stream, &ReceiptRefusal::Plain (
      what_a_failed_write_left (&plan, &runtime . config, &e . to_string ()) )); }
  env . runtime . swap_in (
    runtime . config . clone (), working_graph . load_full (),
    runtime . tantivy_index . clone ());
  drop (mutation_guard);
  send_response_with_length_prefix (
    stream, &tag_sexp_response (
      TcpToClient::Receipt, &plan_response (&plan, false) ));
  stream_rerender_views_showing_the_receipt (
    stream, env, views_state, skgrepo_restriction, &receipt_request . item,
    &plan, &excluded ); }

/// The request's fields: '(item . ID)', '(text . TEXT)', and
/// optionally '(title . TITLE)', '(mailbox . ID)', '(fork-into .
/// SKGREPO)', '(hide . "false")', '(preview . "true")' and
/// '(exclude-view-ids "V" ...)', the views a rerender must leave
/// alone (the client's buffers with unsaved edits).
fn receipt_request_from (
  request : &str,
) -> Result<(ReceiptRequest, bool, HashSet<ViewforestId>), String> {
  let optional = |key : &str| -> Option<String> {
    value_from_request_sexp (key, request) . ok ()
      . filter ( |value| ! value . is_empty () ) };
  let item : ID = ID::from (
    value_from_request_sexp ("item", request)
      . map_err ( |_| "A receipt request names its item: (item . ID)." . to_string () ) ? );
  let parsed : Sexp = sexp::parse (request) . map_err ( |e| e . to_string () ) ?;
  let excluded : HashSet<ViewforestId> =
    if request . contains ("(exclude-view-ids") {
      extract_string_list_from_sexp (&parsed, "exclude-view-ids") ?
        . into_iter () . map (ViewforestId::from_client_string) . collect ()
    } else { HashSet::new () };
  Ok (( ReceiptRequest {
          item,
          text      : value_from_request_sexp ("text", request) . unwrap_or_default (),
          title     : optional ("title")
                      . unwrap_or_else ( || DEFAULT_RECEIPT_TITLE . to_string () ),
          mailbox   : optional ("mailbox") . map (ID::from),
          fork_into : optional ("fork-into") . map ( |s| SkgrepoName::from (s . as_str ()) ),
          hide      : optional ("hide") . as_deref () != Some ("false"), },
        optional ("preview") . as_deref () == Some ("true"),
        excluded )) }

/// '((preview "true"|"false") (lines (...)) (receipt-id ID) (holder-id ID))'.
fn plan_response (
  plan    : &ReceiptPlan,
  preview : bool,
) -> String {
  sexp_to_client_string ( &Sexp::List ( vec! [
    pair ( "preview", if preview { "true" } else { "false" } ),
    format_string_list_sexp ( "lines", &plan . lines ),
    pair ( "receipt-id", plan . receipt . pid . as_str () ),
    pair ( "holder-id", plan . holder . pid . as_str () ),
    pair ( "holder-is-new-fork",
           if plan . new_fork . is_some () { "true" } else { "false" } ), ] )) }

/// A refusal, and the empty rerender stream every answer is followed
/// by: '((refusal TEXT))', with '(choose-skgrepo (S ...))' or
/// '(choose-mailbox (ID ...)) (choose-mailbox-titles (TITLE ...))'
/// where the reader must choose.
fn refuse (
  stream  : &mut TcpStream,
  refusal : &ReceiptRefusal,
) {
  let mut entries : Vec<Sexp> = vec! [ pair ("refusal", refusal . message ()) ];
  match refusal {
    ReceiptRefusal::Plain (_) => {},
    ReceiptRefusal::ChooseSkgrepo { choices, .. } =>
      entries . push ( format_string_list_sexp (
        "choose-skgrepo",
        &choices . iter () . map ( |s| s . to_string () ) . collect::<Vec<String>> () )),
    ReceiptRefusal::ChooseMailbox { choices, .. } => {
      entries . push ( format_string_list_sexp (
        "choose-mailbox",
        &choices . iter () . map ( |(pid, _)| pid . to_string () ) . collect::<Vec<String>> () ));
      entries . push ( format_string_list_sexp (
        "choose-mailbox-titles",
        &choices . iter () . map ( |(_, title)| title . clone () ) . collect::<Vec<String>> () )); }, }
  send_response_with_length_prefix (
    stream, &tag_sexp_response (
      TcpToClient::Receipt, &sexp_to_client_string ( &Sexp::List (entries) )));
  stream_empty_rerender (stream); }

/// What a failed write left. The files are written in the plan's
/// order, each whole, and the first failure stops the rest; the
/// in-Rust graph is unchanged, and the server reloads, when it
/// notices them, the files that were written.
fn what_a_failed_write_left (
  plan   : &ReceiptPlan,
  config : &crate::types::misc::SkgConfig,
  error  : &str,
) -> String {
  let files : Vec<String> = [
    ( "the receipt", Some (&plan . receipt) ),
    ( if plan . new_fork . is_some () { "the new fork" } else { "the note it goes under" },
      Some (&plan . holder) ),
    ( "your fork of the mailbox, with the hide", plan . mailbox_fork . as_ref () ), ]
    . into_iter ()
    . filter_map ( |(what, node)| node . map ( |node| format! (
      "{}, {}", what,
      path_from_pid_and_skgrepo (config, &node . home_skgrepo, node . pid . clone ())
        . unwrap_or_else ( |_| node . pid . to_string () ) )))
    . collect ();
  format! (
    "The receipt's write failed: {}. It writes these files in this order, \
     each whole, and stops at the first that fails, so those before the \
     failing one were written and the rest were not: {}. The in-Rust graph \
     is unchanged; the server reloads the written files when it notices them.",
    error, files . join ("; ") ) }

/// Rerender the open views, but those excluded, that show the item or
/// a mirror of it, or any note the receipt wrote, or a mirror of the
/// mailbox, whose forks folders show the hide.
fn stream_rerender_views_showing_the_receipt (
  stream              : &mut TcpStream,
  env                 : &SkgEnv,
  views_state         : &mut ViewsState,
  skgrepo_restriction : &SkgrepoRestriction,
  item                : &ID,
  plan                : &ReceiptPlan,
  excluded            : &HashSet<ViewforestId>,
) {
  let runtime = env . runtime_snapshot ();
  let seeds : HashSet<ID> =
    std::iter::once ( runtime . graph . pid_else_raw_id (item) )
    . chain ( plan . written_pids () )
    . collect ();
  let affected : HashSet<ID> =
    mirror_pids_ignoring_restriction (&runtime . graph, &seeds);
  let mut prepared : PreparedRerenders = prepare_rerender_views (
    env, runtime, views_state,
    RerenderRequest {
      restriction : skgrepo_restriction,
      selector    : RerenderSelector::Where ( Box::new (
        |viewforest_id, state| ! excluded . contains (viewforest_id)
          && state . pids . iter () . any ( |pid| affected . contains (pid) ))),
      diffView_buffer_rerender : DiffViewBufferRerender::RefreshOnly, } );
  leave_out_views_needing_text_release (
    &mut prepared, skgrepo_restriction, "receipt-rerender" );
  stream_prepared_rerenders (stream, views_state, prepared); }

fn pair (
  key   : &str,
  value : &str,
) -> Sexp {
  Sexp::List ( vec! [
    Sexp::Atom ( Atom::S ( key . to_string () )),
    Sexp::Atom ( Atom::S ( value . to_string () )) ] ) }
