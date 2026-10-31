//! Content views: the stub of a fresh one, its first rendering, and
//! the refresh of an open one. Every view is refreshed
//! ('refresh_viewforest', 'server/refresh/'), a content view in
//! content-view mode:
//! - 'render_initial_view' renders a new content view, the refresh of
//!   a stub of its roots ('stub_of_fresh_content_view'), each asking
//!   for its containers folder;
//! - 'refresh_content_view' brings an open one up to date (after a
//!   save, collaterally, after a commit, or at a switch of the skgrepo
//!   restriction), from what it holds; the rerender pass
//!   ('server/rerender_pass.rs') calls it and renders the result.
//! A buffer of the diff view has the same three in
//! 'server/diff_view/buffer_rerender.rs'.

use crate::dbs::in_rust_graph::InRustGraph;
use crate::org_to_text::viewforest_to_string;
use crate::refresh::{Truth, refresh_content_viewforest};
use crate::rerender_pass::RerenderPassContext;
use crate::types::env::RuntimeGeneration;
use crate::skgrepo_sets::SkgrepoRestriction;
use crate::types::views_state::{ViewnodeIdRecord, pids_from_viewforest};
use crate::types::misc::ID;
use crate::types::tree::forest::ViewForest;
use crate::types::viewnode::{
  Viewnode, ViewnodeKind, mk_editable_viewnode, mk_unknown_viewnode};
use crate::types::viewnode::{FolderRelation, Vognode, ViewRequest};

use ego_tree::Tree;
use std::collections::HashSet;
use std::error::Error;

/// The text of a fresh content view of 'root_gnids', the pids the
/// caller registers for it, and its viewforest: the roots' content,
/// their containers folders and their default folders. The de-novo
/// render ('render_initial_view') is the refresh of a stub forest of
/// the requested roots, each asking for its containers folder, which
/// is drawn with the containerward chain.
pub(crate) fn multi_root_view_via_runtime (
  runtime            : &RuntimeGeneration,
  root_gnids        : &[ID],
  skgrepo_restriction : &SkgrepoRestriction,
) -> Result < (String, Vec<ID>, Tree<Viewnode>), Box<dyn Error> > {
  let (viewforest, buffer_content) : (ViewForest, String) =
    render_initial_view ( runtime, root_gnids, skgrepo_restriction ) ?;
  // The pids the caller registers for this view -- its occurrences,
  // via the one shared source of which kinds count
  // (views_state::pids_from_viewforest), the same helper update_view uses post-save.
  let pids : Vec<ID> =
    pids_from_viewforest (&viewforest)
      . into_iter () . collect ();
  Ok ((buffer_content, pids, viewforest . into_internal_tree ())) }

/// Render a fresh content view: the refresh of a stub
/// ('stub_of_fresh_content_view') in content-view mode, which draws
/// each root's content within the node budget and fulfils the stub's
/// folder requests. Returns the viewforest and its text, with every
/// viewnode given a vnid from a new record.
pub fn render_initial_view (
  runtime     : &RuntimeGeneration,
  root_gnids : &[ID],
  restriction : &SkgrepoRestriction,
) -> Result<(ViewForest, String), Box<dyn Error>> {
  let mut viewforest : ViewForest = stub_of_fresh_content_view (
    root_gnids, &runtime . graph );
  refresh_content_viewforest (
    &mut viewforest, &mut ViewnodeIdRecord::default (),
    &Truth::of_runtime (runtime, restriction) ) ?;
  let text : String = viewforest_to_string (&viewforest) ?;
  Ok (( viewforest, text )) }

/// One root per requested node: an editable occurrence of the node
/// asking for its containers folder ('(folder containers)') and to be
/// the node's editable occurrence ('editableView'), which extends its
/// content and gives each occurrence that extension draws editable its
/// default folders ('add_default_folders' in 'server/refresh/visit.rs'),
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

/// The refresh of an open content view, against the pass's runtime
/// generation and under its skgrepo restriction ('refresh_view' in
/// 'server/rerender_pass.rs').
pub(crate) fn refresh_content_view (
  viewforest         : &mut ViewForest,
  viewnode_id_record : &mut ViewnodeIdRecord,
  context            : &mut RerenderPassContext<'_>,
) -> Result<(), Box<dyn Error>> {
  refresh_content_viewforest (
    viewforest, viewnode_id_record,
    &Truth::of_runtime (&context . runtime, context . skgrepo_restriction) ) }
