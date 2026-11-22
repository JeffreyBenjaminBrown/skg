//! The "set perspective" request: switch the perspective of one open
//! view ('Perspective') and rerender it. 'docs/api-and-formats.org',
//! "Set perspective", gives the request, its answer, its refusals,
//! the overPrivateText challenge, and the view's cycle of
//! perspectives ('perspective_to_show').

use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::in_rust_graph::mirrors::{
  Mirror, mirrors_of, perspective_skgrepo_of_tree_containing};
use crate::org_to_text::viewforest_to_string;
use crate::rerender_pass::{
  DiffViewBufferRerender, RerenderMode, RerenderPassContext, RerenderedView,
  refresh_view};
use crate::serve::ViewsState;
use crate::serve::handlers::text_release::approved_pids_from_request;
use crate::serve::protocol::TcpToClient;
use crate::serve::rerender_stream::{
  PreparedRerenders, authorize_prepared_rerenders,
  stream_empty_rerender, stream_prepared_rerenders};
use crate::serve::util::{
  send_response_with_length_prefix, value_from_request_sexp,
  viewforest_id_from_request};
use crate::sexp_string::quote_sexp_string;
use crate::skgrepo_sets::SkgrepoRestriction;
use crate::types::env::{RuntimeGeneration, SkgEnv};
use crate::types::config::SkgConfig;
use crate::types::misc::{ID, SkgrepoName};
use crate::types::viewnode::{Viewnode, ViewnodeKind, Vognode, Warrant};
use crate::types::tree::forest::ViewForest;
use crate::types::views_state::{Perspective, ViewState, ViewforestId, ViewnodeIdRecord};

use ego_tree::NodeRef;
use std::net::TcpStream;
use std::str::Chars;
use std::sync::Arc;

/// How a message names the root homes' perspective where its
/// skgrepos go unnamed ('perspective_name').
const ROOT_HOMES_UNNAMED : &str = "the perspective of each root's home skgrepo";

/// What a "set perspective" request asks for.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum RequestedPerspective {
  /// The perspective after the view's own in its cycle, as the cycle
  /// is computed now ('first_ranked_after').
  Next,
  /// This one, which must be in the view's cycle.
  Named (Perspective),
}

pub fn handle_set_perspective_request (
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
          stream, "", &Perspective::default (),
          ROOT_HOMES_UNNAMED,
          "The request names no view ('view-id')." );
        return; }};
  let current : Perspective =
    views_state . open_views . perspective_of_view (&viewforest_id);
  let (requested, drawn) : (Perspective, RerenderedView) =
    match requested_perspective (request, &runtime . config)
      . and_then ( |requested| perspective_to_show (
        views_state, env, &runtime, skgrepo_restriction,
        &viewforest_id, &current, requested )) {
      Ok (shown) => shown,
      Err (reason) => {
        let current_name : String = views_state . open_views . views
          . get (&viewforest_id)
          . map ( |state| perspective_name (&current, state, &runtime) )
          . unwrap_or_else ( || ROOT_HOMES_UNNAMED . to_string () );
        refuse_unwinding (
          stream, &viewforest_id . repr_in_client (), &current,
          &current_name, &reason );
        return; }};
  let requested_name : String = perspective_name (
    &requested,
    views_state . open_views . views . get (&viewforest_id)
      . expect ("perspective_to_show found the view"),
    &runtime );
  // The view as the new perspective draws it was drawn to choose that
  // perspective; it is the rerender, and is not drawn again.
  let mut prepared : PreparedRerenders = PreparedRerenders::of_one_content_view (
    runtime . clone (), skgrepo_restriction, drawn );
  if ! authorize_prepared_rerenders (
    stream, &mut prepared, skgrepo_restriction,
    "perspective-switch", &approved_pids_from_request (request) ) {
    return; }
  views_state . open_views . set_perspective (
    &viewforest_id, requested . clone ());
  send_perspective_response (
    stream, &viewforest_id . repr_in_client (), &requested,
    &requested_name, None );
  stream_prepared_rerenders (stream, views_state, prepared); }

/// The request's '(perspective . "NAME")', which it requires:
/// "next", or a perspective's wire name ('Perspective::wire_name').
pub fn requested_perspective (
  request : &str,
  config  : &SkgConfig,
) -> Result<RequestedPerspective, String> {
  let name : String =
    value_from_request_sexp ("perspective", request)
    . map_err ( |_| "The request names no perspective ('perspective')." . to_string () ) ?;
  if name == "next" { return Ok (RequestedPerspective::Next); }
  Perspective::from_wire_name (&name, config)
    . map (RequestedPerspective::Named)
    . ok_or_else ( || format! (
      "'perspective' must be \"next\", \"root homes\" or \"skgrepo SKGREPO\" (a skgrepo the config lists), not {:?}.",
      name )) }

/// The perspective the view 'viewforest_id', now showing 'current',
/// is to show, and the view drawn from it, which is its rerender; or
/// why it cannot be switched. 'docs/api-and-formats.org', "Set
/// perspective", states the refusals and the cycle. Each skgrepo is
/// judged by drawing the view from it and comparing the text with the
/// root homes' drawing ('view_drawn_from'); the skgrepos tried are
/// those 'candidate_skgrepos' finds in the root homes' drawing and in
/// the view's own, in the order 'perspective_rank' gives; "next" stops
/// at the first in the cycle ('first_ranked_after'); a named
/// perspective draws only itself.
fn perspective_to_show (
  views_state   : &ViewsState,
  env           : &SkgEnv,
  runtime       : &Arc<RuntimeGeneration>,
  restriction   : &SkgrepoRestriction,
  viewforest_id : &ViewforestId,
  current       : &Perspective,
  requested     : RequestedPerspective,
) -> Result<(Perspective, RerenderedView), String> {
  let Some (state) : Option<&ViewState> =
    views_state . open_views . views . get (viewforest_id)
  else { return Err ( format! (
    "The server does not hold the view {} (it was restarted, or the client reconnected, since the view was drawn). Save the buffer, then switch again.",
    viewforest_id . repr_in_client () )); };
  if state . diffView_buffer_kind . is_some () {
    return Err ( "The diff view shows no mirror in place of another, so it has no perspective." . to_string () ); }
  let draw = |perspective : &Perspective| -> Result<RerenderedView, String> {
    view_drawn_from (perspective, viewforest_id, state, env, runtime, restriction) };
  let root_homes : RerenderedView = draw (&Perspective::RootHomes) ?;
  let mut candidates : Vec<SkgrepoName> =
    candidate_skgrepos (&root_homes . viewforest, &runtime . graph, restriction);
  for skgrepo in candidate_skgrepos (&state . viewforest, &runtime . graph, restriction) {
    if ! candidates . contains (&skgrepo) { candidates . push (skgrepo); }}
  let candidates : Vec<Perspective> =
    candidates . into_iter () . map (Perspective::Skgrepo) . collect ();
  let in_the_cycle = |perspective : &Perspective|
    -> Result<Option<RerenderedView>, String> {
    let drawn : RerenderedView = draw (perspective) ?;
    Ok ( ( drawn . text != root_homes . text ) . then_some (drawn) ) };
  match requested {
    RequestedPerspective::Next =>
      match first_ranked_after (current, candidates, &runtime . config, in_the_cycle) ? {
        Some (next) => Ok (next),
        None if *current == Perspective::RootHomes =>
          Err ( format! ( "No other perspective would show this buffer differently from {}.",
                          ROOT_HOMES_UNNAMED )),
        None => Ok (( Perspective::RootHomes, root_homes )), },
    RequestedPerspective::Named (Perspective::RootHomes) =>
      Ok (( Perspective::RootHomes, root_homes )),
    RequestedPerspective::Named (named) => {
      let drawn : Option<RerenderedView> =
        if candidates . contains (&named) { in_the_cycle (&named) ? } else { None };
      match drawn {
        Some (drawn) => Ok (( named, drawn )),
        None => Err ( format! (
          "{} would show this buffer as {} does, so it is not one of its perspectives.",
          capitalized ( &perspective_name (&named, state, runtime) ),
          ROOT_HOMES_UNNAMED )), }}, }}

/// The view 'state', 'viewforest_id', drawn from 'perspective', in
/// memory, from what the connection holds for it, exactly as its
/// rerender would draw it ('rerender_open_view'). Nothing the
/// connection holds changes.
fn view_drawn_from (
  perspective   : &Perspective,
  viewforest_id : &ViewforestId,
  state         : &ViewState,
  env           : &SkgEnv,
  runtime       : &Arc<RuntimeGeneration>,
  restriction   : &SkgrepoRestriction,
) -> Result<RerenderedView, String> {
  let mut viewforest : ViewForest = state . viewforest . clone ();
  let mut viewnode_id_record : ViewnodeIdRecord = state . viewnode_id_record . clone ();
  let mut context : RerenderPassContext = RerenderPassContext::new (
    env, runtime . clone (), restriction, None, DiffViewBufferRerender::RefreshOnly );
  refresh_view (
    &mut viewforest, &mut viewnode_id_record, &mut context,
    RerenderMode::ContentView { perspective : perspective . clone () } )
    . map_err ( |error| error . to_string () ) ?;
  let text : String = viewforest_to_string (&viewforest)
    . map_err ( |error| error . to_string () ) ?;
  Ok ( RerenderedView { viewforest_id : viewforest_id . clone (),
                        text, viewforest, viewnode_id_record } ) }

/// The skgrepos whose perspective might draw 'forest' otherwise than
/// the root homes' does: the home of each visible mirror of each
/// original 'forest' draws at a position where substitution acts
/// ('substitute_mirrors' and 'substitute_roots' in
/// 'server/refresh/visit.rs'). A skgrepo with no mirror of any such
/// original ranks each one's mirrors by declaration order alone; its
/// perspective is not offered, since it mostly draws what a candidate
/// does. A position drawn only beneath a node another perspective puts
/// in place is not read, so a skgrepo whose mirrors stand only there
/// is found from that perspective.
fn candidate_skgrepos (
  forest      : &ViewForest,
  graph       : &InRustGraph,
  restriction : &SkgrepoRestriction,
) -> Vec<SkgrepoName> {
  let mut skgrepos : Vec<SkgrepoName> = Vec::new ();
  for node in forest . nodes () {
    let Some (original) : Option<ID> = original_at_substitution_position (graph, node)
      else { continue; };
    for Mirror { home, .. } in mirrors_of (graph, restriction, &original, None) . members {
      if ! skgrepos . contains (&home) { skgrepos . push (home); }} }
  skgrepos }

/// The original a substitution at 'node' would stand for, if 'node'
/// is a position where substitution acts: an occurrence that is a
/// root of the view, or that stands as content of an editable
/// occurrence, which stands for its '(inPlaceOf N)', or, a root
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
  match &occurrence . in_place_of {
    Some (original) => Some ( original . clone () ),
    None => ( at_a_root || occurrence . viewStats . warrant == Warrant::ByGraph )
      . then ( || graph . pid_else_raw_id (&occurrence . gnid) ), }}

/// Where 'perspective' stands in every cycle: the root homes' first,
/// then each skgrepo's in declaration order. Every cycle is ordered
/// so, so the perspective after one is well defined even when the
/// cycle changes between requests.
fn perspective_rank (
  perspective : &Perspective,
  config      : &SkgConfig,
) -> usize {
  match perspective {
    Perspective::RootHomes => 0,
    Perspective::Skgrepo (skgrepo) =>
      1 + config . skgrepos_in_declaration_order () . iter ()
        . position ( |declared| declared == skgrepo )
        . unwrap_or (usize::MAX - 1), }}

/// The perspective after 'current' in the cycle, if it is not the
/// root homes': the first of 'candidates' ranked after 'current'
/// ('perspective_rank') that 'in_the_cycle' admits, with what it
/// returned (the view drawn from it). Candidates are tried in rank
/// order, and none after the first admitted, so a press draws only
/// the candidates between 'current' and the next. None means the next
/// wraps to the root homes'. Since the cycle is computed afresh at
/// each request, 'current' may have left it; the next is then still
/// the first member ranked after it.
pub fn first_ranked_after<T> (
  current      : &Perspective,
  candidates   : Vec<Perspective>,
  config       : &SkgConfig,
  mut in_the_cycle : impl FnMut (&Perspective) -> Result<Option<T>, String>,
) -> Result<Option<(Perspective, T)>, String> {
  let current_rank : usize = perspective_rank (current, config);
  let mut after : Vec<Perspective> = candidates . into_iter ()
    . filter ( |candidate| perspective_rank (candidate, config) > current_rank )
    . collect ();
  after . sort_by_key ( |candidate| perspective_rank (candidate, config) );
  for candidate in after {
    if let Some (found) = in_the_cycle (&candidate) ? {
      return Ok ( Some (( candidate, found )) ); }}
  Ok (None) }

/// How a message to the user names 'perspective' in the view 'state':
/// "the perspective of LABEL" ('Perspective::name_of_skgrepos_perspective'),
/// or "the perspective of each root's home skgrepo" with those
/// skgrepos named, in the order of their first roots ("the perspective
/// of each root's home skgrepo (tracy, main)").
pub fn perspective_name (
  perspective : &Perspective,
  state       : &ViewState,
  runtime     : &RuntimeGeneration,
) -> String {
  match perspective {
    Perspective::Skgrepo (skgrepo) =>
      Perspective::name_of_skgrepos_perspective (skgrepo, &runtime . config),
    Perspective::RootHomes => {
      let mut homes : Vec<SkgrepoName> = Vec::new ();
      for root in state . viewforest . roots () {
        let home : SkgrepoName = perspective_skgrepo_of_tree_containing (
          &runtime . config, &runtime . graph,
          &Perspective::RootHomes, root );
        if ! homes . contains (&home) { homes . push (home); }}
      let labels : Vec<String> = homes . iter ()
        . map ( |home| runtime . config . skgrepos . get (home)
                . map ( |configured| configured . herald_label () . to_string () )
                . unwrap_or_else ( || home . 0 . clone () ))
        . collect ();
      if labels . is_empty () { ROOT_HOMES_UNNAMED . to_string () }
      else { format! ( "{} ({})", ROOT_HOMES_UNNAMED, labels . join (", ") ) }}, }}

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
  perspective        : &Perspective,
  perspective_name   : &str,
  reason             : &str,
) {
  send_perspective_response (
    stream, view_id, perspective, perspective_name, Some (reason) );
  stream_empty_rerender (stream); }

/// The 'perspective' message: the view, the perspective it
/// shows, that perspective's name for the user, and, for a refusal,
/// why.
fn send_perspective_response (
  stream             : &mut TcpStream,
  view_id            : &str,
  perspective        : &Perspective,
  perspective_name   : &str,
  refusal            : Option<&str>,
) {
  let content : String = refusal
    . map ( |reason| format! (" (content {})", quote_sexp_string (reason)) )
    . unwrap_or_default ();
  send_response_with_length_prefix ( stream, & format! (
    "((response-type {}) (view-id {}) (perspective {}) (perspective-name {}){})",
    TcpToClient::Perspective . repr_in_client (),
    quote_sexp_string (view_id),
    quote_sexp_string ( &perspective . wire_name () ),
    quote_sexp_string (perspective_name),
    content )); }

#[cfg(test)]
mod tests {
  use super::*;
  use std::collections::HashMap;

  fn skgrepo (
    name : &str,
  ) -> Perspective {
    Perspective::Skgrepo ( SkgrepoName::from (name) ) }

  /// 'first_ranked_after' as its doc says, a current perspective that
  /// has left the cycle included, the candidates given in no order.
  #[test]
  fn the_next_perspective_follows_the_rank_even_when_the_current_one_left_the_cycle () {
    let mut config : SkgConfig = SkgConfig::dummyFromSkgrepos (HashMap::new ());
    config . skgrepo_order = [ "main", "alice", "bob", "carol" ] . iter ()
      . map ( |name| SkgrepoName::from (*name) ) . collect ();
    let candidates : Vec<Perspective> = vec! [
      skgrepo ("carol"), skgrepo ("bob"), skgrepo ("main"), skgrepo ("alice") ];
    let next = |current : &Perspective, cycle : &[Perspective]|
      -> (Perspective, Vec<Perspective>) {
      let mut tried : Vec<Perspective> = Vec::new ();
      let found : Option<(Perspective, ())> = first_ranked_after (
        current, candidates . clone (), &config,
        |candidate| { tried . push ( candidate . clone () );
                      Ok ( cycle . contains (candidate) . then_some (()) ) } )
        . unwrap ();
      ( found . map ( |(next, ())| next ) . unwrap_or (Perspective::RootHomes),
        tried ) };
    let cycle : Vec<Perspective> = vec! [
      Perspective::RootHomes, skgrepo ("main"),
      skgrepo ("alice"), skgrepo ("carol") ];
    assert_eq! ( next (&Perspective::RootHomes, &cycle),
                 ( skgrepo ("main"), vec! [ skgrepo ("main") ] ),
                 "only the next is drawn" );
    assert_eq! ( next (&skgrepo ("main"), &cycle) . 0, skgrepo ("alice") );
    assert_eq! ( next (&skgrepo ("carol"), &cycle),
                 ( Perspective::RootHomes, vec! [] ),
                 "nothing is ranked after the last; nothing is drawn" );
    assert_eq! ( next (&skgrepo ("alice"), &cycle),
                 ( skgrepo ("carol"), vec! [ skgrepo ("bob"), skgrepo ("carol") ] ),
                 "bob, not in the cycle, is drawn and passed over" );
    assert_eq! ( next (&skgrepo ("bob"), &cycle) . 0,
                 skgrepo ("carol"), "bob left the cycle; carol is ranked after it" );
    assert_eq! ( next (&skgrepo ("bob"), &cycle [..2]) . 0,
                 Perspective::RootHomes );
    assert_eq! ( next (&Perspective::RootHomes, &[ Perspective::RootHomes ]) . 0,
                 Perspective::RootHomes,
                 "a cycle of one leads nowhere; the request is refused" ); }
}
