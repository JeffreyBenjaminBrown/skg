//! The "rerender views" request: rerender the open views it names,
//! through the shared rerender pipeline ('server/serve/rerender_stream.rs').
//! It writes nothing. A save whose rerender would release text from
//! overPrivateText telescopes under a skgrepo restriction withholds
//! it, and its challenge names the views withheld
//! ('withhold_save_rerender' in 'server/rerender_pass.rs'); the
//! clients approve by sending this request for them, with the PIDs the
//! challenge listed.

use crate::rerender_pass::DiffViewBufferRerender;
use crate::serve::ViewsState;
use crate::serve::handlers::text_release::approved_pids_from_request;
use crate::serve::rerender_stream::{
  PreparedRerenders, RerenderRequest, RerenderSelector,
  authorize_prepared_rerenders, prepare_rerender_views,
  stream_empty_rerender_with_error, stream_prepared_rerenders};
use crate::skgrepo_sets::SkgrepoRestriction;
use crate::types::env::SkgEnv;
use crate::types::sexp::extract_string_list_from_sexp;
use crate::types::views_state::ViewforestId;

use sexp::Sexp;
use std::collections::HashSet;
use std::net::TcpStream;

pub fn handle_rerender_views_request (
  stream              : &mut TcpStream,
  request             : &str,
  env                 : &SkgEnv,
  views_state         : &mut ViewsState,
  skgrepo_restriction : &SkgrepoRestriction,
) {
  let named : Vec<ViewforestId> =
    match named_viewforest_ids_from_request (request) {
      Ok (named) => named,
      Err (error) => {
        stream_empty_rerender_with_error (stream, error);
        return; } };
  let named_set : HashSet<ViewforestId> =
    named . iter () . cloned () . collect ();
  let mut prepared : PreparedRerenders = prepare_rerender_views (
    env, env . runtime_snapshot (), views_state,
    RerenderRequest {
      restriction : skgrepo_restriction,
      selector    : RerenderSelector::Where ( Box::new (
        |viewforest_id, _state| named_set . contains (viewforest_id) )),
      diffView_buffer_rerender : DiffViewBufferRerender::RefreshOnly, } );
  for viewforest_id in &named {
    if ! views_state . open_views . views . contains_key (viewforest_id) {
      prepared . errors . push ( format! (
        "View {}: not open on this connection",
        viewforest_id . repr_in_client () )); }}
  if ! authorize_prepared_rerenders (
    stream, &mut prepared, skgrepo_restriction,
    "rerender-views", &approved_pids_from_request (request) ) {
    return; }
  stream_prepared_rerenders (stream, views_state, prepared); }

/// The request's '(view-ids "VIEW_ID" ...)', which it requires.
fn named_viewforest_ids_from_request (
  request : &str,
) -> Result<Vec<ViewforestId>, String> {
  let parsed : Sexp =
    sexp::parse (request) . map_err (|error| error . to_string ()) ?;
  extract_string_list_from_sexp (&parsed, "view-ids")
    . map ( |viewforest_ids| viewforest_ids . into_iter ()
            . map (ViewforestId::from_client_string) . collect () ) }

#[cfg(test)]
mod named_views_tests {
  use super::*;

  #[test]
  fn the_named_views_are_parsed_and_a_request_naming_none_fails () {
    assert_eq! (
      named_viewforest_ids_from_request (
        "((request . \"rerender views\") (view-ids \"a-view\" \"b-view\"))" ),
      Ok ( vec! [ ViewforestId::from_client_string ("a-view" . to_string ()),
                  ViewforestId::from_client_string ("b-view" . to_string ()) ] ));
    assert! ( named_viewforest_ids_from_request (
      "((request . \"rerender views\"))" ) . is_err () );
  }
}
