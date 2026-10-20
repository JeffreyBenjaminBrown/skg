//! The "rerender all views" request: rerender every open view, or
//! every one the request does not exclude, through the shared
//! rerender pipeline ('server/serve/rerender_stream.rs').

use crate::serve::ViewsState;
use crate::serve::handlers::text_release::approved_pids_from_request;
use crate::serve::protocol::TcpToClient;
use crate::serve::rerender_stream::{
  PreparedRerenders, authorize_prepared_rerenders,
  prepare_rerender_views, stream_prepared_rerenders};
use crate::serve::util::{ format_errors_warnings_sexp, format_lock_views_sexp, send_response_with_length_prefix, tag_sexp_response};
use crate::skgrepo_sets::SkgrepoRestriction;
use crate::types::env::SkgEnv;
use crate::types::views_state::ViewId;
use crate::update_buffer::RerenderOptions;

use std::net::TcpStream;
use std::collections::HashSet;

pub fn handle_rerender_all_views_request (
  stream     : &mut TcpStream,
  request    : &str,
  env        : &SkgEnv,
  views_state : &mut ViewsState,
  skgrepo_restriction : &SkgrepoRestriction,
) {
  let excluded : HashSet<ViewId> =
    match excluded_view_ids_from_request (request) {
      Ok (excluded) => excluded,
      Err (error) => {
        send_response_with_length_prefix (
          stream, &tag_sexp_response (TcpToClient::RerenderLock,
            &format_lock_views_sexp (&[])));
        send_response_with_length_prefix (
          stream, &tag_sexp_response (TcpToClient::RerenderDone,
            &format_errors_warnings_sexp (&[error], &[])));
        return; } };
  stream_rerender_views_excluding (
    stream, env, views_state, skgrepo_restriction,
    &approved_pids_from_request (request), &excluded); }

fn excluded_view_ids_from_request (
  request : &str,
) -> Result<HashSet<ViewId>, String> {
  let parsed = sexp::parse (request) . map_err (|error| error . to_string ())?;
  let present : bool = match &parsed {
    sexp::Sexp::List (items) => items . iter () . any (|item|
      matches! (item, sexp::Sexp::List (fields)
        if matches! (fields . first (), Some (sexp::Sexp::Atom (
          sexp::Atom::S (name))) if name == "exclude-view-ids"))),
    _ => false, };
  if ! present { return Ok (HashSet::new ()); }
  crate::types::sexp::extract_string_list_from_sexp (
    &parsed, "exclude-view-ids")
    .map (|view_ids| view_ids . into_iter ()
      .map (ViewId::from_client_string) . collect ())
}

#[cfg(test)]
mod import_exclusion_tests {
  use super::*;

  #[test]
  fn dirty_view_exclusions_are_parsed_and_malformed_lists_fail_closed () {
    let request : &str = "((request . \"rerender all views\") (exclude-view-ids \"dirty-view_id\"))";
    let excluded : HashSet<ViewId> =
      excluded_view_ids_from_request (request) . unwrap ();
    assert! (excluded . contains (&ViewId::from_client_string (
      "dirty-view_id" . to_string ())));
    assert! (excluded_view_ids_from_request (
      "((request . \"rerender all views\") (exclude-view-ids (bad)))")
      . is_err ());
  }
}

fn stream_rerender_views_excluding (
  stream : &mut TcpStream,
  env : &SkgEnv,
  views_state : &mut ViewsState,
  skgrepo_restriction : &SkgrepoRestriction,
  approved_pids : &HashSet<crate::types::misc::ID>,
  excluded : &HashSet<ViewId>,
) {
  let mut prepared : PreparedRerenders = prepare_rerender_views (
    env, views_state, Some (skgrepo_restriction), None,
    RerenderOptions::ordinary );
  prepared . view_ids . retain (|view_id| ! excluded . contains (view_id));
  prepared . views . retain (|view| ! excluded . contains (&view . view_id));
  if ! authorize_prepared_rerenders (
    stream, &mut prepared, Some (skgrepo_restriction),
    "rerender-all-views", approved_pids) {
    return; }
  stream_prepared_rerenders (stream, views_state, prepared);
}
