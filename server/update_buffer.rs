//! Bringing views up to date with the graph. Every view is refreshed
//! ('refresh_viewforest', 'server/refresh/'): a content view in
//! content-view mode, a buffer of the diff view in diff-view mode,
//! over the graph straddle ('server/diff_view/completion.rs'):
//! - 'render_initial_view' renders a new content view, the refresh of
//!   a stub of its roots, each asking for its containers folder;
//! - 'diffView_buffer_viewforest'
//!   ('server/diff_view/pruned_change_view.rs') renders a buffer of the
//!   diff view, the refresh of a stub of its roots;
//! - 'rerender_view' brings a view that already exists up to date
//!   (after a save, collaterally, after a commit, or at a skgrepo-set
//!   switch), from what it holds;
//! - 'update_views_after_save' rerenders the saved view and then the
//!   collateral ones, streaming each to the client.

pub mod completion_mode;
pub mod reconcile;
pub mod graphnodestats;
pub mod util;
pub mod viewnodestats;

pub use graphnodestats::{
  set_graphnodestats_in_viewforest,
  set_graphnodestats_in_viewforest_with_skgrepo_set};
pub use viewnodestats::set_viewnodestats_in_viewforest;

use crate::dbs::in_rust_graph::InRustGraph;
use crate::refresh::{Refreshed, Truth, refresh_content_viewforest};
use crate::refresh::buffer_corrections::{BufferCorrectionPolicy, BufferCorrection, ViewBufferCorrections};
use crate::diff_view::change_leaves::BufferReadinessSummary;
use crate::diff_view::completion::rerender_diffView_buffer;
use crate::diff_view::readiness_requests::buffer_readiness_summary_for;
use crate::graph_straddle::types::GraphStraddle;
use crate::types::env::{RuntimeGeneration, SkgEnv};
use crate::serve::ViewsState;
use crate::serve::handlers::save_buffer::SaveResponse;
use crate::serve::handlers::text_release::{
  TextReleaseDecision, challenge_response, decide,
};
use crate::serve::protocol::TcpToClient;
use crate::serve::rerender_stream::write_graph_straddle_back;
use crate::serve::util::{ format_single_view_sexp, send_response_with_length_prefix, tag_sexp_response};
use crate::skgrepo_sets::SkgrepoRestriction;
use crate::types::views_state::{DiffViewBufferKind, ViewforestId, ViewnodeIdRecord};
use crate::types::misc::ID;
use crate::types::save::ForkSpec;
use crate::types::tree::forest::ViewForest;
use crate::types::viewnode::{
  Editability, Viewnode, ViewnodeKind, mk_editable_viewnode, mk_unknown_viewnode};
use crate::types::viewnode::{FolderRelation, Vognode, ViewRequest};

use ego_tree::NodeId;
use std::collections::HashSet;
use std::error::Error;
use std::sync::Arc;
use std::time::Instant;

pub struct RerenderAfterSaveContext<'a> {
  pub env           : &'a SkgEnv,
  pub runtime       : Arc<RuntimeGeneration>,
  pub graph_snap    : Arc<InRustGraph>,
  pub errors        : Vec<String>,
  pub warnings      : Vec<String>,
  pub skgrepo_restriction            : Option<&'a SkgrepoRestriction>,
  /// The graph straddle a buffer of the diff view is refreshed
  /// over, brought up to date by 'current_graph_straddle_in_slot' when
  /// the first such buffer is rerendered. A caller that holds the
  /// connection's straddle sets it, and keeps what it becomes.
  pub graph_straddle                 : Option<Arc<GraphStraddle>>,
  /// Which changes the rerenders report as no buffer corrections: a
  /// save's rerender sets the nodes it deleted, a skgrepo-set switch
  /// whether the restriction changed; every other rerender reports
  /// every change.
  pub buffer_correction_policy       : BufferCorrectionPolicy,
}

impl<'a> RerenderAfterSaveContext<'a> {
  fn with_runtime (
    env : &'a SkgEnv,
    runtime : Arc<RuntimeGeneration>,
    skgrepo_restriction : Option<&'a SkgrepoRestriction>,
  ) -> RerenderAfterSaveContext<'a> {
    RerenderAfterSaveContext {
      env,
      graph_snap : runtime . graph . clone (),
      runtime,
      errors : Vec::new (),
      warnings : Vec::new (),
      skgrepo_restriction,
      graph_straddle : None,
      buffer_correction_policy : BufferCorrectionPolicy::default (),
    }}

  pub fn without_save (
    env                : &'a SkgEnv,
    skgrepo_restriction : Option<&'a SkgrepoRestriction>,
  ) -> RerenderAfterSaveContext<'a> {
    RerenderAfterSaveContext::with_runtime (
      env, env . runtime_snapshot (), skgrepo_restriction ) }

  pub fn without_save_with_runtime (
    env : &'a SkgEnv,
    runtime : Arc<RuntimeGeneration>,
    skgrepo_restriction : Option<&'a SkgrepoRestriction>,
  ) -> RerenderAfterSaveContext<'a> {
    RerenderAfterSaveContext::with_runtime (
      env, runtime, skgrepo_restriction ) }
}

struct RenderedCollateralView {
  viewforest_id  : ViewforestId,
  text           : String,
  buffer_corrections    : Vec<BufferCorrection>,
  viewforest     : ViewForest,
  viewnode_id_record : ViewnodeIdRecord,
}

/// PURPOSE:
/// Updates the rendered views and ViewsState
/// for each view affected by the save.
/// Starts with the saved view itself, not the collateral ones,
/// to minimize any freeze the user might experience.
/// A headline standing for a node in 'deleted_by_the_save' that the
/// rerender entombstones is no buffer correction.
/// .
/// ASSUMES:
/// the graph was already updated. The reverse order would be bad.
pub fn update_views_after_save (
  stream                      : &mut std::net::TcpStream,
  saved_view                  : ViewForest,
  collateral_viewforest_ids   : Vec<ViewforestId>,
  env                         : &SkgEnv,
  runtime                     : Arc<RuntimeGeneration>,
  viewforest_id_from_request_result : &Result<ViewforestId, String>,
  views_state                 : &mut ViewsState,
  skgrepo_restriction         : Option<&SkgrepoRestriction>,
  text_approved_pids        : &HashSet<ID>,
  fork_specs                  : &[ForkSpec],
  deleted_by_the_save         : HashSet<ID>,
) -> Result<SaveResponse, Box<dyn Error>> {
  let mut context : RerenderAfterSaveContext =
    // Snapshot the in-Rust graph once for this save's rerender pass.
    RerenderAfterSaveContext::with_runtime (
      env, runtime, skgrepo_restriction );
  context . buffer_correction_policy =
    BufferCorrectionPolicy::after_a_save_deleting (deleted_by_the_save);
  context . graph_straddle = views_state . graph_straddle . clone ();
  let saved_diffView_buffer_kind : Option<DiffViewBufferKind> =
    viewforest_id_from_request_result . as_ref () . ok ()
    . and_then ( |viewforest_id| views_state . open_views . diffView_buffer_kind_of_view (viewforest_id) );
  let mut saved_view_mut : ViewForest = saved_view;
  let mut saved_viewnode_id_record : ViewnodeIdRecord =
    // The parsed buffer carries viewnode IDs; the view's record says which
    // it knows, before the refresh runs.
    viewforest_id_from_request_result . as_ref () . ok ()
    . map ( |viewforest_id| views_state . open_views . viewnode_id_record_of (viewforest_id) )
    . unwrap_or_default ();
  saved_viewnode_id_record . drop_unknown_viewnode_ids_in_parsed_viewforest (&mut saved_view_mut);
  replace_saved_view_fork_roots (&mut saved_view_mut, fork_specs) ?;
  // The skgsave-commit has happened, so every `(editRequest ...)` in the
  // submitted view is now consumed input. Clear all request carriers at this
  // one boundary before the refresh can echo any of them into the response.
  // `viewRequests` remain: those are fulfilled by the refresh below.
  saved_view_mut . consume_edit_requests_after_save ();
  // Gate the occurrences already in the forests before rendering, so even a
  // rendering error cannot echo protected title/body text. A second decision
  // below covers nodes introduced by the refresh.
  if let Some (restriction) = skgrepo_restriction {
    let mut input_candidates : Vec<ID> =
      occurrence_gnids_in_viewforest (&saved_view_mut);
    for viewforest_id in &collateral_viewforest_ids {
      if let Some (viewforest) = views_state . open_views
          . viewforest_id_to_view (viewforest_id) {
        input_candidates . extend (
          occurrence_gnids_in_viewforest (viewforest) ); }}
    let release : TextReleaseDecision = decide (
      "save-rerender", restriction, &input_candidates,
      &context . graph_snap, text_approved_pids );
    if matches! (release, TextReleaseDecision::Challenge { .. }) {
      return Ok ( SaveResponse {
        saved_view          : String::new (),
        errors              : Vec::new (),
        warnings            : Vec::new (),
        save_point_position : None,
        fork_confirmation   : None,
        hoist_confirmation  : None,
        strandeds_confirmation : None,
        text_release_confirmation : challenge_response (&release),
        readiness_summary   : None,
        buffer_corrections         : Vec::new (),
      } ); }}
  let saved : Refreshed =
    { let _span : tracing::span::EnteredSpan = tracing::info_span!(
        "rerender_view (saved)" ). entered();
      rerender_view ( // saved view first; collateral ones later
        &mut saved_view_mut,
        &mut saved_viewnode_id_record,
        &mut context,
        RerenderOptions::ordinary (saved_diffView_buffer_kind) ) } ?;
  let readiness_summary : Option<BufferReadinessSummary> =
    saved_diffView_buffer_kind . and ( context . graph_straddle . as_ref () )
    . map ( |straddle| buffer_readiness_summary_for (
      env, straddle, &saved_view_mut ) );
  let mut collateral_views : Vec<RenderedCollateralView> = Vec::new ();
  for curi in &collateral_viewforest_ids {
    match rerender_collateral_view (
      curi . clone (), views_state, &mut context )
    { Ok (rendered) => collateral_views . push (rendered),
      Err (e) => context . errors . push (e), }}
  // Everything textual is now staged in memory. Decide before changing the
  // open-view registry or streaming the first view.
  let mut release_candidates : Vec<ID> =
    occurrence_gnids_in_viewforest (&saved_view_mut);
  for collateral in &collateral_views {
    release_candidates . extend (
      occurrence_gnids_in_viewforest (&collateral . viewforest) ); }
  if let Some (restriction) = skgrepo_restriction {
    let release : TextReleaseDecision = decide (
      "save-rerender", restriction, &release_candidates,
      &context . graph_snap, text_approved_pids );
    match release {
      decision @ TextReleaseDecision::Challenge { .. } => {
        return Ok ( SaveResponse {
          saved_view          : String::new (),
          errors              : Vec::new (),
          warnings            : Vec::new (),
          save_point_position : None,
          fork_confirmation   : None,
          hoist_confirmation  : None,
          strandeds_confirmation : None,
          text_release_confirmation : challenge_response (&decision),
          readiness_summary   : None,
          buffer_corrections         : Vec::new (),
        } ); }
      TextReleaseDecision::AllowWithWarning { warning } =>
        context . warnings . push (warning),
      TextReleaseDecision::Allow => {}, }}
  write_graph_straddle_back (views_state, context . graph_straddle . take ());
  let mut buffer_corrections : Vec<ViewBufferCorrections> = Vec::new ();
  if let Ok (viewforest_id) = viewforest_id_from_request_result {
    buffer_corrections . extend ( ViewBufferCorrections::of (viewforest_id, saved . buffer_corrections) );
    views_state . open_views . update_view (
      &context . graph_snap, viewforest_id, saved_view_mut, saved_viewnode_id_record );
    for rendered in collateral_views {
      buffer_corrections . extend ( ViewBufferCorrections::of (
        &rendered . viewforest_id, rendered . buffer_corrections ));
      views_state . open_views . update_view (
        &context . graph_snap,
        &rendered . viewforest_id, rendered . viewforest,
        rendered . viewnode_id_record );
      send_response_with_length_prefix (
        stream,
        & tag_sexp_response (
          TcpToClient::CollateralView,
          & format_single_view_sexp (
            &rendered . viewforest_id, &rendered . text) )); }}
  Ok ( SaveResponse {
    saved_view          : saved . text,
    errors              : context . errors,
    warnings            : context . warnings,
    save_point_position : None,
    fork_confirmation   : None,
    hoist_confirmation  : None,
    strandeds_confirmation : None,
    text_release_confirmation : None,
    readiness_summary,
    buffer_corrections, } ) }

fn occurrence_gnids_in_viewforest (
  viewforest : &ViewForest,
) -> Vec<ID> {
  viewforest . nodes ()
    .filter_map ( |node| match &node . value () . kind {
      ViewnodeKind::Vognode (Vognode::Occurrence (occurrence)) =>
        Some ( occurrence . gnid . clone () ),
      _ => None,
    } )
    .collect ()
}

/// A root is intentionally rendered raw when the user ordinarily opens an
/// overridden node.  The buffer which *created* a fork is the exception: once
/// the save's skgsave-commit happens, continuing to show its foreign root would make the next
/// edit attempt another fork.  Replace only roots named by this save's exact
/// ForkSpecs; content occurrences are substituted by the refresh.
fn replace_saved_view_fork_roots (
  viewforest : &mut ViewForest,
  fork_specs : &[ForkSpec],
) -> Result<(), Box<dyn Error>> {
  if fork_specs . is_empty () { return Ok (()); }
  let root_gnids : Vec<NodeId> = viewforest . root_gnids ();
  for root_gnid in root_gnids {
    let original : Option<ID> =
      viewforest . get (root_gnid) . and_then (|root| match
        &root . value () . kind {
          ViewnodeKind::Vognode (Vognode::Occurrence (occurrence)) =>
            Some (occurrence . gnid . clone ()),
          _ => None, });
    let Some (original) = original else { continue; };
    let Some (spec) = fork_specs . iter ()
      . find (|spec| spec . original_gnid == original)
    else { continue; };
    let clone = & spec . clone . 0;
    let mut root = viewforest . get_mut (root_gnid)
      . ok_or ("replace_saved_view_fork_roots: root not found") ?;
    if let ViewnodeKind::Vognode (Vognode::Occurrence (occurrence)) =
      &mut root . value () . kind
    { occurrence . gnid = clone . pid . clone ();
      occurrence . home_skgrepo = clone . home_skgrepo . clone ();
      occurrence . title = clone . title . clone ();
      if let Editability::Editable { body, .. } = &mut occurrence . editability
      { *body = clone . body . clone (); }
      occurrence . view_requests . remove (&ViewRequest::Fork);
      occurrence . overridesHere = Some (original); }}
  Ok (())
}

fn rerender_collateral_view (
  viewforest_id : ViewforestId,
  views_state : &ViewsState,
  context     : &mut RerenderAfterSaveContext<'_>,
) -> Result<RenderedCollateralView, String> {
  let mut viewforest : ViewForest = match
    views_state . open_views . viewforest_id_to_view (&viewforest_id) {
      Some (f) => f . clone (),
      None => {
        return Err ( format! (
          "Collateral view {}: no viewforest found",
          viewforest_id . repr_in_client () )); } };
  let mut viewnode_id_record : ViewnodeIdRecord =
    views_state . open_views . viewnode_id_record_of (&viewforest_id);
  let diffView_buffer_kind : Option<DiffViewBufferKind> =
    views_state . open_views . diffView_buffer_kind_of_view (&viewforest_id);
  let refreshed : Refreshed =
    { let _span : tracing::span::EnteredSpan =
        tracing::info_span!( "rerender_view (collateral)" ). entered();
      rerender_view (
        &mut viewforest,
        &mut viewnode_id_record,
        context,
        RerenderOptions::ordinary (diffView_buffer_kind)
      ) . map_err (
        |e| format!( "Collateral view {}: {}",
                      viewforest_id . repr_in_client (), e)) ? };
  Ok (RenderedCollateralView {
    viewforest_id,
    text        : refreshed . text,
    buffer_corrections : refreshed . buffer_corrections,
    viewforest,
    viewnode_id_record,
  }) }

/// Render a fresh content view: the refresh of a stub
/// ('stub_of_fresh_content_view') in content-view mode, which draws
/// each root's content within the node budget and fulfils the stub's
/// folder requests. Returns the viewforest and its text, with every
/// viewnode given a vnid from a new record. A fresh view reports no
/// buffer correction: there was nothing the user could have made otherwise.
pub fn render_initial_view (
  runtime     : &RuntimeGeneration,
  root_gnids : &[ID],
  restriction : Option<&SkgrepoRestriction>,
) -> Result<(ViewForest, String), Box<dyn Error>> {
  let mut viewforest : ViewForest = stub_of_fresh_content_view (
    root_gnids, &runtime . graph );
  let refreshed : Refreshed = refresh_content_viewforest (
    &mut viewforest, &mut ViewnodeIdRecord::default (),
    &Truth::of_runtime (runtime, restriction),
    &BufferCorrectionPolicy::default () ) ?;
  Ok (( viewforest, refreshed . text )) }

/// One root per requested node: an editable occurrence of the node
/// asking for its containers folder ('(folder containers)') and to be
/// the node's editable occurrence ('editableView'), which extends its
/// content and gives each occurrence that extension draws editable its
/// default folders ('add_default_folders' in 'server/refresh/ladder.rs'),
/// as a fresh view always has. A node requested twice is editable at
/// its first root only. A gnid no node answers to is an unknown, which
/// the refresh entombstones.
pub(crate) fn stub_of_fresh_content_view (
  root_gnids : &[ID],
  graph       : &InRustGraph,
) -> ViewForest {
  let mut viewforest : ViewForest = ViewForest::new ();
  let mut editable : HashSet<ID> = HashSet::new ();
  for gnid in root_gnids {
    let root : Viewnode =
      match graph . pid_of (gnid) . and_then ( |pid| graph . get (&pid) ) {
        None => mk_unknown_viewnode ( gnid . clone () ),
        Some (node) => {
          let mut root : Viewnode = mk_editable_viewnode (
            node . pid . clone (), node . home_skgrepo . clone (),
            node . title . clone (), node . body . clone () );
          if let ViewnodeKind::Vognode (Vognode::Occurrence (occurrence)) =
            &mut root . kind
          { occurrence . view_requests . insert (
              ViewRequest::Folder (FolderRelation::Containers) );
            if editable . insert ( node . pid . clone () ) {
              occurrence . view_requests . insert (ViewRequest::Editable);
            } else { occurrence . write_protect (); }}
          root }, };
    viewforest . append_root (root); }
  viewforest }

/// How 'rerender_view' treats a view, beyond what its context says.
/// Each case of rerender has its own constructor, which names every
/// field.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct RerenderOptions {
  /// Some for a buffer of the diff view.
  pub diffView_buffer                       : Option<DiffViewBufferKind>,
  /// True only for the "integrate unshown changes" request; matters
  /// only in a buffer of the diff view.
  pub integrate_unshown_changes             : bool,
}

impl RerenderOptions {
  /// Every rerender of an open view but the one below: the saved view
  /// and the collateral views of a save, "rerender all views", a
  /// skgrepo-set switch, the cleanup after deleting references to an
  /// absent node, and the diff view's buffers after a git-commit.
  pub fn ordinary (
    diffView_buffer : Option<DiffViewBufferKind>,
  ) -> RerenderOptions {
    RerenderOptions {
      diffView_buffer,
      integrate_unshown_changes             : false, } }

  /// The buffer of the diff view named by an "integrate unshown
  /// changes" request.
  pub fn for_integrating_unshown_changes (
    diffView_buffer : Option<DiffViewBufferKind>,
  ) -> RerenderOptions {
    RerenderOptions {
      diffView_buffer,
      integrate_unshown_changes             : true, } }
}

/// Bring an open view up to date with the graph and render it,
/// returning its text and its buffer corrections. It is refreshed
/// ('refresh_viewforest'), from what it holds and the graph as they
/// are now: a content view ('options . diffView_buffer' None) in
/// content-view mode; a buffer of the diff view in diff-view mode,
/// over the graph straddle, brought up to date in 'context'
/// ('rerender_diffView_buffer', which says what the "integrate unshown
/// changes" request, 'options . integrate_unshown_changes', adds).
pub fn rerender_view (
  viewforest    : &mut ViewForest,
  viewnode_id_record : &mut ViewnodeIdRecord, // the view's, which assigns the viewnode IDs its new viewnodes get
  context       : &mut RerenderAfterSaveContext<'_>,
  options       : RerenderOptions,
) -> Result<Refreshed, Box<dyn Error>> {
  let t_rerender : Instant = Instant::now ();
  let _span : tracing::span::EnteredSpan = tracing::info_span!(
    "refresh_viewforest" ). entered();
  let result : Refreshed = match options . diffView_buffer {
    None => refresh_content_view (
      viewforest, viewnode_id_record, context ) ?,
    Some (diffView_buffer_kind) => rerender_diffView_buffer (
      &mut context . graph_straddle, &context . runtime, context . env,
      context . skgrepo_restriction, diffView_buffer_kind,
      options . integrate_unshown_changes,
      &context . buffer_correction_policy,
      viewforest, viewnode_id_record ) ?, };
  tracing::debug!("rerender_view: done ({:.3}s)",
            t_rerender . elapsed () . as_secs_f64 ());
  Ok (result) }

/// The refresh of an open content view; see 'rerender_view'.
fn refresh_content_view (
  viewforest         : &mut ViewForest,
  viewnode_id_record : &mut ViewnodeIdRecord,
  context            : &mut RerenderAfterSaveContext<'_>,
) -> Result<Refreshed, Box<dyn Error>> {
  refresh_content_viewforest (
    viewforest, viewnode_id_record,
    &Truth::of_runtime (&context . runtime, context . skgrepo_restriction),
    &context . buffer_correction_policy ) }
