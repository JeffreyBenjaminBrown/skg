//! PURPOSE: Tell a client, unprompted, what the repo watcher
//! reloaded ('server/repo_watch/'): one "repo-files-reloaded"
//! message, '((reloaded ((SKGREPO N) ...)) (warnings ("..." ...))
//! (view-ids ("..." ...)))', naming each skgrepo and how many of its
//! files were reloaded, what could not be, and the open views of the
//! connection that show a changed node. The client reports the counts
//! briefly when there are no warnings, shows the warnings as it shows
//! the startup warnings, and asks for a rerender of those views it
//! holds no unsaved edits in ("rerender all views", excluding the
//! others). Each buffer of the diff view the message names is marked
//! so that that rerender also extends it to the changes it does not
//! show ('ViewState::extend_at_next_rerender_all'), as the "extend
//! diff view" request would; under a skgrepo restriction, which that
//! request refuses, none is marked.
//!
//! The connection's loop sends it only between requests, when no
//! request has arrived for its read timeout, so it never falls inside
//! another response. A skgrepo the connection's skgrepo restriction
//! keeps out is not named, and a problem that concerns one is not
//! told.

use crate::repo_watch::reload::RepoChangeNotice;
use crate::serve::ViewsState;
use crate::serve::protocol::TcpToClient;
use crate::serve::util::{send_response_with_length_prefix, sexp_to_client_string, tag_sexp_response};
use crate::skgrepo_sets::SkgrepoRestriction;
use crate::types::misc::SkgrepoName;
use crate::types::views_state::{OpenViews, ViewforestId};

use sexp::{Atom, Sexp};
use std::collections::BTreeSet;
use std::net::TcpStream;
use std::sync::Arc;
use tokio::sync::broadcast::Receiver;
use tokio::sync::broadcast::error::TryRecvError;

/// Send the client one message for every notice 'receiver' holds,
/// if any says something this connection may hear.
pub fn deliver_repo_change_notices (
  stream      : &mut TcpStream,
  receiver    : &mut Receiver<Arc<RepoChangeNotice>>,
  views_state : &mut ViewsState,
  restriction : &SkgrepoRestriction,
) {
  let (notices, missed_some) : (Vec<Arc<RepoChangeNotice>>, bool) =
    received_notices (receiver);
  if notices . is_empty () && ! missed_some { return; }
  if missed_some
    || notices . iter () . any ( |notice| notice . foreign_files_reloaded ) {
    // The diff view reads foreign skgrepos from their worktree.
    views_state . graph_straddle = None; }
  let reloaded : Vec<(SkgrepoName, usize)> =
    files_reloaded_per_skgrepo (&notices, restriction);
  let warnings : Vec<String> = {
    let mut warnings : Vec<String> = notices . iter ()
      . flat_map ( |notice| warnings_of_notice (notice, restriction) )
      . collect ();
    if missed_some {
      warnings . push ( "Files that changed on disk were reloaded more often than this connection could follow; every open view is rerendered." . to_string () ); }
    warnings };
  let view_ids : Vec<ViewforestId> =
    views_to_rerender (&notices, missed_some, views_state);
  if reloaded . is_empty () && warnings . is_empty () && view_ids . is_empty () {
    return; }
  if restriction . is_unrestricted () {
    for viewforest_id in &view_ids {
      views_state . open_views . set_extend_at_next_rerender_all (
        viewforest_id, true ); }}
  send_response_with_length_prefix (
    stream, & repo_files_reloaded_response (
      &reloaded, &warnings, &view_ids )); }

/// The notices waiting, and whether some were missed because the
/// connection fell too far behind.
fn received_notices (
  receiver : &mut Receiver<Arc<RepoChangeNotice>>,
) -> (Vec<Arc<RepoChangeNotice>>, bool) {
  let mut notices : Vec<Arc<RepoChangeNotice>> = Vec::new ();
  let mut missed_some : bool = false;
  loop {
    match receiver . try_recv () {
      Ok (notice) => notices . push (notice),
      Err (TryRecvError::Lagged (_)) => missed_some = true,
      Err (TryRecvError::Empty) | Err (TryRecvError::Closed) => break, }}
  (notices, missed_some) }

/// How many files each skgrepo 'restriction' shows had reloaded,
/// summed over 'notices', in the order the notices first name them.
pub fn files_reloaded_per_skgrepo (
  notices     : &[Arc<RepoChangeNotice>],
  restriction : &SkgrepoRestriction,
) -> Vec<(SkgrepoName, usize)> {
  let mut reloaded : Vec<(SkgrepoName, usize)> = Vec::new ();
  for (skgrepo, count) in notices . iter ()
    . flat_map ( |notice| notice . files_reloaded . iter () )
    . filter ( |(skgrepo, _)| restriction . contains_skgrepo (skgrepo) )
  { match reloaded . iter_mut () . find ( |(seen, _)| seen == skgrepo ) {
      Some ((_, total)) => *total += count,
      None => reloaded . push (( skgrepo . clone (), *count )), }}
  reloaded }

/// The warnings 'notice' tells a connection under 'restriction': each
/// problem whose skgrepos it all shows.
pub fn warnings_of_notice (
  notice      : &RepoChangeNotice,
  restriction : &SkgrepoRestriction,
) -> Vec<String> {
  notice . problems . iter ()
    . filter ( |problem| problem . skgrepos . iter ()
               . all ( |skgrepo| restriction . contains_skgrepo (skgrepo) ))
    . map ( |problem| problem . message . clone () )
    . collect () }

/// The connection's open views a rerender should bring up to date:
/// those that show a changed node, and every buffer of the diff view
/// when anything was reloaded (what the diff view shows depends on
/// every file); every open view if notices were missed. Sorted.
fn views_to_rerender (
  notices     : &[Arc<RepoChangeNotice>],
  missed_some : bool,
  views_state : &ViewsState,
) -> Vec<ViewforestId> {
  let open_views : &OpenViews = &views_state . open_views;
  let anything_reloaded : bool = notices . iter ()
    . any ( |notice| notice . generation . is_some () );
  let selected : BTreeSet<String> = open_views . views . iter ()
    . filter ( |(_, state)|
      missed_some
      || ( anything_reloaded && state . diffView_buffer_kind . is_some () )
      || notices . iter () . any ( |notice|
           notice . changed_pids . iter ()
           . any ( |pid| state . pids . contains (pid) )) )
    . map ( |(viewforest_id, _)| viewforest_id . repr_in_client () )
    . collect ();
  selected . into_iter () . map (ViewforestId::from_client_string) . collect () }

/// '((response-type repo-files-reloaded) (reloaded ((SKGREPO N)
/// ...)) (warnings ("..." ...)) (view-ids (ID ...)))'.
pub fn repo_files_reloaded_response (
  reloaded : &[(SkgrepoName, usize)],
  warnings : &[String],
  view_ids : &[ViewforestId],
) -> String {
  let payload : Sexp = Sexp::List ( vec! [
    Sexp::List ( vec! [
      Sexp::Atom ( Atom::S ( "reloaded" . to_string () )),
      Sexp::List ( reloaded . iter ()
                   . map ( |(skgrepo, count)| Sexp::List ( vec! [
                     Sexp::Atom ( Atom::S ( skgrepo . 0 . clone () )),
                     Sexp::Atom ( Atom::I ( *count as i64 )), ] ))
                   . collect () ), ] ),
    Sexp::List ( vec! [
      Sexp::Atom ( Atom::S ( "warnings" . to_string () )),
      Sexp::List ( warnings . iter ()
                   . map ( |warning| Sexp::Atom ( Atom::S ( warning . clone () )) )
                   . collect () ), ] ),
    Sexp::List ( vec! [
      Sexp::Atom ( Atom::S ( "view-ids" . to_string () )),
      Sexp::List ( view_ids . iter ()
                   . map ( |view_id| Sexp::Atom ( Atom::S ( view_id . repr_in_client () )) )
                   . collect () ), ] ), ] );
  tag_sexp_response (
    TcpToClient::RepoFilesReloaded,
    & sexp_to_client_string (&payload) ) }
