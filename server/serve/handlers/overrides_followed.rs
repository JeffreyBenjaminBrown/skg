//! The "set overrides followed" request: switch the perspective of
//! one open view, whose overrides it follows ('OverridesFollowed'),
//! and rerender it. The request names a perspective, or asks for the
//! next one in the view's cycle ('perspectives_of_view').
//!
//! The answer is an 'overrides-followed' message naming the view, the
//! perspective it now shows, and that perspective as a message to the
//! user names it, followed by a rerender stream of that one view. A
//! refusal answers in the same shape, carrying the view's unchanged
//! perspective and its reason in 'content', followed by an empty
//! rerender stream, since each client locks its buffers and sets its
//! stream-in-progress guard before it sends the request. Under a
//! skgrepo restriction, a rerender that would release overPrivateText
//! text is challenged ('authorize_prepared_rerenders') before the view
//! changes: the challenge and an empty rerender stream replace the
//! answer, and the view keeps its perspective.
//!
//! The clients refuse to send it for a buffer with unsaved edits,
//! which the rerender would replace.

use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::in_rust_graph::override_resolution::{
  override_follower_of_tree_containing, visible_overriders_with_owners_of};
use crate::org_to_text::viewforest_to_string;
use crate::rerender_pass::{
  DiffViewBufferRerender, RerenderMode, RerenderPassContext, refresh_view};
use crate::serve::ViewsState;
use crate::serve::handlers::text_release::approved_pids_from_request;
use crate::serve::protocol::TcpToClient;
use crate::serve::rerender_stream::{
  PreparedRerenders, RerenderRequest, RerenderSelector,
  authorize_prepared_rerenders, prepare_rerender_views,
  stream_empty_rerender, stream_prepared_rerenders};
use crate::serve::util::{
  send_response_with_length_prefix, value_from_request_sexp,
  viewforest_id_from_request};
use crate::sexp_string::quote_sexp_string;
use crate::skgrepo_sets::SkgrepoRestriction;
use crate::types::env::{RuntimeGeneration, SkgEnv};
use crate::types::misc::{ID, SkgConfig, SkgrepoOwner};
use crate::types::viewnode::{Viewnode, ViewnodeKind, Vognode, Warrant};
use crate::types::tree::forest::ViewForest;
use crate::types::views_state::{OverridesFollowed, ViewState, ViewforestId, ViewnodeIdRecord};

use ego_tree::NodeRef;
use std::iter::once;
use std::net::TcpStream;
use std::str::Chars;
use std::sync::Arc;

/// How a message names the root authors' perspective where its
/// authors go unnamed ('perspective_name').
const ROOT_AUTHORS_UNNAMED : &str = "each root author's overrides";

/// What a "set overrides followed" request asks for.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum RequestedPerspective {
  /// The perspective after the view's own in its cycle, as the cycle
  /// is computed now ('perspective_after').
  Next,
  /// This one, which must be in the view's cycle.
  Named (OverridesFollowed),
}

pub fn handle_set_overrides_followed_request (
  stream              : &mut TcpStream,
  request             : &str,
  env                 : &SkgEnv,
  views_state         : &mut ViewsState,
  skgrepo_restriction : &SkgrepoRestriction,
) {
  let runtime : Arc<RuntimeGeneration> = env . runtime_snapshot ();
  let viewforest_id : ViewforestId =
    match viewforest_id_from_request (request) {
      Ok (viewforest_id) => viewforest_id,
      Err (_) => {
        refuse_unwinding (
          stream, "", &OverridesFollowed::default (),
          ROOT_AUTHORS_UNNAMED,
          "The request names no view ('view-id')." );
        return; }};
  let current : OverridesFollowed =
    views_state . open_views . overrides_followed_by_view (&viewforest_id);
  let requested : OverridesFollowed =
    match requested_perspective (request, &runtime . config)
      . and_then ( |requested| perspective_to_show (
        views_state, env, &runtime, skgrepo_restriction,
        &viewforest_id, &current, requested )) {
      Ok (requested) => requested,
      Err (reason) => {
        let current_name : String = views_state . open_views . views
          . get (&viewforest_id)
          . map ( |state| perspective_name (&current, state, &runtime) )
          . unwrap_or_else ( || ROOT_AUTHORS_UNNAMED . to_string () );
        refuse_unwinding (
          stream, &viewforest_id . repr_in_client (), &current,
          &current_name, &reason );
        return; }};
  let requested_name : String = perspective_name (
    &requested,
    views_state . open_views . views . get (&viewforest_id)
      . expect ("perspective_to_show found the view"),
    &runtime );
  // The rerender reads the view's perspective from what the
  // connection holds, so it is set first, and restored unless the
  // rerender is released.
  views_state . open_views . set_overrides_followed (
    &viewforest_id, requested . clone ());
  let mut prepared : PreparedRerenders = prepare_rerender_views (
    env, runtime . clone (), views_state,
    RerenderRequest {
      restriction : skgrepo_restriction,
      selector    : RerenderSelector::One (viewforest_id . clone ()),
      diffView_buffer_rerender : DiffViewBufferRerender::RefreshOnly, } );
  if prepared . views . is_empty () {
    views_state . open_views . set_overrides_followed (
      &viewforest_id, current . clone ());
    let current_name : String = perspective_name (
      &current,
      views_state . open_views . views . get (&viewforest_id)
        . expect ("perspective_to_show found the view"),
      &runtime );
    refuse_unwinding (
      stream, &viewforest_id . repr_in_client (), &current,
      &current_name, &prepared . errors . join ("\n") );
    return; }
  if ! authorize_prepared_rerenders (
    stream, &mut prepared, skgrepo_restriction,
    "overrides-followed-switch", &approved_pids_from_request (request) ) {
    views_state . open_views . set_overrides_followed (&viewforest_id, current);
    return; }
  send_overrides_followed_response (
    stream, &viewforest_id . repr_in_client (), &requested,
    &requested_name, None );
  stream_prepared_rerenders (stream, views_state, prepared); }

/// The request's '(overrides-followed . "NAME")', which it requires:
/// "next", or a perspective's wire name ('OverridesFollowed::wire_name').
pub fn requested_perspective (
  request : &str,
  config  : &SkgConfig,
) -> Result<RequestedPerspective, String> {
  let name : String =
    value_from_request_sexp ("overrides-followed", request)
    . map_err ( |_| "The request says nothing of whose overrides to follow ('overrides-followed')." . to_string () ) ?;
  if name == "next" { return Ok (RequestedPerspective::Next); }
  OverridesFollowed::from_wire_name (&name, config)
    . map (RequestedPerspective::Named)
    . ok_or_else ( || format! (
      "'overrides-followed' must be \"next\", \"root authors\", \"user\" or \"owner of SKGREPO\" (a foreign skgrepo), not {:?}.",
      name )) }

/// The perspective the view 'viewforest_id', now showing 'current',
/// is to show, or why it cannot be switched: the connection does not
/// hold it; it is a buffer of the diff view (which substitutes no
/// overriders); "next" finds no perspective but the one shown; or a
/// named perspective is not in the view's cycle. The root authors'
/// is always in the cycle, so the way back is never refused.
fn perspective_to_show (
  views_state   : &ViewsState,
  env           : &SkgEnv,
  runtime       : &Arc<RuntimeGeneration>,
  restriction   : &SkgrepoRestriction,
  viewforest_id : &ViewforestId,
  current       : &OverridesFollowed,
  requested     : RequestedPerspective,
) -> Result<OverridesFollowed, String> {
  let Some (state) : Option<&ViewState> =
    views_state . open_views . views . get (viewforest_id)
  else { return Err ( format! (
    "The server does not hold the view {} (it was restarted, or the client reconnected, since the view was drawn). Save the buffer, then switch again.",
    viewforest_id . repr_in_client () )); };
  if state . diffView_buffer_kind . is_some () {
    return Err ( "The diff view shows no overriders in place of what they override, so it follows no one's overrides." . to_string () ); }
  let cycle : Vec<OverridesFollowed> =
    perspectives_of_view (state, env, runtime, restriction) ?;
  match requested {
    RequestedPerspective::Next => {
      let next : OverridesFollowed =
        perspective_after (current, &cycle, &runtime . config);
      if &next == current {
        return Err ( "No one's overrides would show this buffer differently from its root authors'." . to_string () ); }
      Ok (next) },
    RequestedPerspective::Named (named) => {
      if ! cycle . contains (&named) {
        return Err ( format! (
          "{} would show this buffer as its root authors' do, so it is not one of its perspectives.",
          capitalized ( &perspective_name (&named, state, runtime) ))); }
      Ok (named) }, }}

/// The view's cycle of perspectives, computed afresh at each request:
/// first the root authors', then each owner whose perspective draws
/// the view otherwise than the root authors' do, in the order
/// 'perspective_rank' gives. Each is judged by drawing the view from
/// it, from what the connection holds, and comparing the text with
/// the root authors' drawing ('view_text_from'), so a perspective that
/// would change nothing is left out exactly. The owners tried are
/// those 'candidate_owners' finds in the root authors' drawing and in
/// the view's own, so that a member found from the root authors' is
/// found from every perspective, whichever path led there.
pub fn perspectives_of_view (
  state       : &ViewState,
  env         : &SkgEnv,
  runtime     : &Arc<RuntimeGeneration>,
  restriction : &SkgrepoRestriction,
) -> Result<Vec<OverridesFollowed>, String> {
  let (root_authors_text, root_authors_forest) : (String, ViewForest) =
    view_text_from (&OverridesFollowed::RootAuthors, state, env, runtime, restriction) ?;
  let mut candidates : Vec<SkgrepoOwner> =
    candidate_owners (&root_authors_forest, &runtime . config, &runtime . graph, restriction);
  for owner in candidate_owners (&state . viewforest, &runtime . config, &runtime . graph, restriction) {
    if ! candidates . contains (&owner) { candidates . push (owner); }}
  let mut owners : Vec<OverridesFollowed> = Vec::new ();
  for owner in candidates {
    let perspective : OverridesFollowed = OverridesFollowed::Owner (owner);
    let (text, _forest) : (String, ViewForest) =
      view_text_from (&perspective, state, env, runtime, restriction) ?;
    if text != root_authors_text { owners . push (perspective); }}
  owners . sort_by_key ( |perspective| perspective_rank (perspective, &runtime . config) );
  Ok ( once (OverridesFollowed::RootAuthors) . chain (owners) . collect () ) }

/// The view 'state' drawn from 'perspective', in memory, from what the
/// connection holds for it, as its rerender would draw it: the text,
/// and the forest. Nothing the connection holds changes.
fn view_text_from (
  perspective : &OverridesFollowed,
  state       : &ViewState,
  env         : &SkgEnv,
  runtime     : &Arc<RuntimeGeneration>,
  restriction : &SkgrepoRestriction,
) -> Result<(String, ViewForest), String> {
  let mut forest : ViewForest = state . viewforest . clone ();
  let mut record : ViewnodeIdRecord = state . viewnode_id_record . clone ();
  let mut context : RerenderPassContext = RerenderPassContext::new (
    env, runtime . clone (), restriction, None, DiffViewBufferRerender::RefreshOnly );
  refresh_view (
    &mut forest, &mut record, &mut context,
    RerenderMode::ContentView { overrides_followed : perspective . clone () } )
    . map_err ( |error| error . to_string () ) ?;
  let text : String = viewforest_to_string (&forest)
    . map_err ( |error| error . to_string () ) ?;
  Ok (( text, forest )) }

/// The owners whose perspective might draw 'forest' otherwise than
/// the root authors' do: the user, and the owner of each visible
/// overrider of each original 'forest' draws at a position where
/// substitution acts ('substitute_overriders' and 'substitute_roots'
/// in 'server/refresh/visit.rs'). Every other owner would show each
/// original raw there, as some candidate (the user, where the user
/// overrides nothing) already does. A position drawn only beneath a
/// node another perspective puts in place is not read, so an owner
/// whose overrides apply only there is found from that perspective.
fn candidate_owners (
  forest      : &ViewForest,
  config      : &SkgConfig,
  graph       : &InRustGraph,
  restriction : &SkgrepoRestriction,
) -> Vec<SkgrepoOwner> {
  let mut owners : Vec<SkgrepoOwner> = vec! [ SkgrepoOwner::User ];
  for node in forest . nodes () {
    let Some (original) : Option<ID> = original_at_substitution_position (graph, node)
      else { continue; };
    for (_overrider, owner) in visible_overriders_with_owners_of (
      config, graph, restriction, &original ) {
      if ! owners . contains (&owner) { owners . push (owner); }} }
  owners }

/// The original a substitution at 'node' would stand for, if 'node'
/// is a position where substitution acts: an occurrence that is a
/// root of the view, or that stands as content of an editable
/// occurrence, which stands for its '(overridesHere N)', or, a root
/// or the graph relating it so, for itself.
fn original_at_substitution_position (
  graph : &InRustGraph,
  node  : NodeRef<Viewnode>,
) -> Option<ID> {
  let parent : NodeRef<Viewnode> = node . parent () ?;
  let at_a_root : bool = matches! ( parent . value () . kind, ViewnodeKind::BufferRoot );
  if ! at_a_root { parent . value () . editable_gnid () ?; }
  let ViewnodeKind::Vognode (Vognode::Occurrence (occurrence)) : &ViewnodeKind =
    &node . value () . kind else { return None; };
  match &occurrence . overridesHere {
    Some (original) => Some ( original . clone () ),
    None => ( at_a_root || occurrence . viewStats . warrant == Warrant::ByGraph )
      . then ( || graph . pid_else_raw_id (&occurrence . gnid) ), }}

/// Where 'perspective' stands in every cycle: the root authors' first,
/// then the user's, then each foreign owner's in the declaration order
/// of the skgrepo that names it. Every cycle is ordered so, so the
/// perspective after one is well defined even when the cycle changes
/// between requests.
fn perspective_rank (
  perspective : &OverridesFollowed,
  config      : &SkgConfig,
) -> usize {
  match perspective {
    OverridesFollowed::RootAuthors => 0,
    OverridesFollowed::Owner (SkgrepoOwner::User) => 1,
    OverridesFollowed::Owner (SkgrepoOwner::Foreign (name)) =>
      2 + config . skgrepos_in_declaration_order () . iter ()
        . position ( |declared| declared == name )
        . unwrap_or (usize::MAX - 2), }}

/// The perspective after 'current' in 'cycle' (ordered by
/// 'perspective_rank', the root authors' first): the first member
/// ranked after 'current', else, wrapping, the root authors'. Since
/// the cycle is computed afresh at each request, 'current' may have
/// left it; the next is then still the first member ranked after it.
pub fn perspective_after (
  current : &OverridesFollowed,
  cycle   : &[OverridesFollowed],
  config  : &SkgConfig,
) -> OverridesFollowed {
  let current_rank : usize = perspective_rank (current, config);
  cycle . iter ()
    . find ( |member| perspective_rank (member, config) > current_rank )
    . cloned ()
    . unwrap_or (OverridesFollowed::RootAuthors) }

/// How a message to the user names 'perspective' in the view 'state':
/// "your overrides", "NAME's overrides", or "each root author's
/// overrides" with the root authors named, in the order of their
/// first roots ("each root author's overrides (NAME's, yours)").
pub fn perspective_name (
  perspective : &OverridesFollowed,
  state       : &ViewState,
  runtime     : &RuntimeGeneration,
) -> String {
  match perspective {
    OverridesFollowed::Owner (owner) =>
      OverridesFollowed::name_of_owners_overrides (owner),
    OverridesFollowed::RootAuthors => {
      let mut authors : Vec<SkgrepoOwner> = Vec::new ();
      for root in state . viewforest . roots () {
        let author : SkgrepoOwner = override_follower_of_tree_containing (
          &runtime . config, &runtime . graph,
          &OverridesFollowed::RootAuthors, root );
        if ! authors . contains (&author) { authors . push (author); }}
      let names : Vec<String> = authors . iter ()
        . map ( |author| match author {
          SkgrepoOwner::User => "yours" . to_string (),
          SkgrepoOwner::Foreign (name) => format! ("{}'s", name . 0), } )
        . collect ();
      if names . is_empty () { ROOT_AUTHORS_UNNAMED . to_string () }
      else { format! ( "{} ({})", ROOT_AUTHORS_UNNAMED, names . join (", ") ) }}, }}

fn capitalized (
  text : &str,
) -> String {
  let mut characters : Chars = text . chars ();
  match characters . next () {
    Some (first) => first . to_uppercase () . chain (characters) . collect (),
    None => String::new (), }}

/// The refusal: the answer carrying the view's unchanged perspective
/// and 'reason', then an empty rerender stream, which unlocks the
/// client's buffers and clears its stream-in-progress guard.
fn refuse_unwinding (
  stream             : &mut TcpStream,
  view_id            : &str,
  overrides_followed : &OverridesFollowed,
  perspective_name   : &str,
  reason             : &str,
) {
  send_overrides_followed_response (
    stream, view_id, overrides_followed, perspective_name, Some (reason) );
  stream_empty_rerender (stream); }

/// The 'overrides-followed' message: the view, the perspective it
/// shows, that perspective's name for the user, and, for a refusal,
/// why.
fn send_overrides_followed_response (
  stream             : &mut TcpStream,
  view_id            : &str,
  overrides_followed : &OverridesFollowed,
  perspective_name   : &str,
  refusal            : Option<&str>,
) {
  let content : String = refusal
    . map ( |reason| format! (" (content {})", quote_sexp_string (reason)) )
    . unwrap_or_default ();
  send_response_with_length_prefix ( stream, & format! (
    "((response-type {}) (view-id {}) (overrides-followed {}) (perspective-name {}){})",
    TcpToClient::OverridesFollowed . repr_in_client (),
    quote_sexp_string (view_id),
    quote_sexp_string ( &overrides_followed . wire_name () ),
    quote_sexp_string (perspective_name),
    content )); }

#[cfg(test)]
mod tests {
  use super::*;
  use crate::types::misc::SkgrepoName;
  use std::collections::HashMap;

  fn foreign (
    name : &str,
  ) -> OverridesFollowed {
    OverridesFollowed::Owner ( SkgrepoOwner::Foreign ( SkgrepoName::from (name) )) }

  /// The next perspective is the first in the cycle ranked after the
  /// current one, wrapping to the root authors'. A current perspective
  /// that has left the cycle (it would now show nothing differently)
  /// is followed by the first member ranked after it, so no member is
  /// skipped and the root authors' are always reached.
  #[test]
  fn the_next_perspective_follows_the_rank_even_when_the_current_one_left_the_cycle () {
    let mut config : SkgConfig = SkgConfig::dummyFromSkgrepos (HashMap::new ());
    config . skgrepo_order = [ "main", "alice", "bob", "carol" ] . iter ()
      . map ( |name| SkgrepoName::from (*name) ) . collect ();
    let cycle : Vec<OverridesFollowed> = vec! [
      OverridesFollowed::RootAuthors, OverridesFollowed::user (),
      foreign ("alice"), foreign ("carol") ];
    assert_eq! ( perspective_after (&OverridesFollowed::RootAuthors, &cycle, &config),
                 OverridesFollowed::user () );
    assert_eq! ( perspective_after (&OverridesFollowed::user (), &cycle, &config),
                 foreign ("alice") );
    assert_eq! ( perspective_after (&foreign ("carol"), &cycle, &config),
                 OverridesFollowed::RootAuthors );
    assert_eq! ( perspective_after (&foreign ("bob"), &cycle, &config),
                 foreign ("carol"), "bob left the cycle; carol is ranked after him" );
    assert_eq! ( perspective_after (&foreign ("bob"), &cycle [..2], &config),
                 OverridesFollowed::RootAuthors );
    assert_eq! ( perspective_after (&OverridesFollowed::RootAuthors,
                                    &[ OverridesFollowed::RootAuthors ], &config),
                 OverridesFollowed::RootAuthors,
                 "a cycle of one leads nowhere; the request is refused" ); }
}
