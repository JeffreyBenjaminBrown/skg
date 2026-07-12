use crate::herald_tokens::assemble_counts_only;
use crate::types::git::MembershipAxes;
use crate::types::misc::SkgConfig;
use crate::types::tree::forest::ViewForest;
use crate::types::viewnode::{
  ViewNode, ViewNodeKind, Vognode, Phantom, Qual, QualCol, ActiveNode, PhantomDiff,
  PhantomDeleted, PhantomUnknown, EditRequest, GraphNodeStats,
  ParentIs,
};

use ego_tree::{NodeRef, Tree};
use std::error::Error;

/// PURPOSE: Render a view forest to org-mode text.
/// Each forest root starts at level 1.
///
/// ASSUMES: metadata has already been enriched with relationship data.
pub trait ViewForestRenderRoots {
  fn render_roots<'a> (
    &'a self,
  ) -> Result<Box<dyn Iterator<Item = NodeRef<'a, ViewNode>> + 'a>,
              Box<dyn Error>>;
}

impl ViewForestRenderRoots for ViewForest {
  fn render_roots<'a> (
    &'a self,
  ) -> Result<Box<dyn Iterator<Item = NodeRef<'a, ViewNode>> + 'a>,
              Box<dyn Error>> {
    Ok (Box::new (self . roots ())) }
}

impl ViewForestRenderRoots for Tree<ViewNode> {
  fn render_roots<'a> (
    &'a self,
  ) -> Result<Box<dyn Iterator<Item = NodeRef<'a, ViewNode>> + 'a>,
              Box<dyn Error>> {
    let root_ref : NodeRef<ViewNode> = self . root ();
    let is_viewforest_root : bool =
      matches! (
        & root_ref . value () . kind,
        ViewNodeKind::BufferRoot);
    if ! is_viewforest_root {
      return Err (
        "viewforest_to_string: root is not a BufferRoot" . into() ); }
    Ok (Box::new (root_ref . children ())) }
}

pub fn viewforest_to_string <R> (
  viewforest : &R,
  config : &SkgConfig,
) -> Result < String, Box<dyn Error> >
where R : ViewForestRenderRoots + ?Sized {
  render_view_roots_to_string (
    viewforest . render_roots () ?, config ) }

/// Compatibility helper for callers that still hold the old
/// single-tree representation with an internal BufferRoot.
pub fn viewforest_tree_to_string (
  viewforest : &Tree<ViewNode>,
  config : &SkgConfig,
) -> Result < String, Box<dyn Error> > {
  let root_ref : NodeRef<ViewNode> = viewforest . root ();
  let is_viewforest_root : bool =
    matches! (
      & root_ref . value () . kind,
      ViewNodeKind::BufferRoot);
  if ! is_viewforest_root {
    return Err (
      "viewforest_tree_to_string: root is not a BufferRoot" . into() ); }
  render_view_roots_to_string (
    root_ref . children (), config ) }

fn render_view_roots_to_string <'a> (
  roots  : impl Iterator<Item = NodeRef<'a, ViewNode>>,
  config : &SkgConfig,
) -> Result < String, Box<dyn Error> > {
  fn render_node_subtree_to_org (
    node_ref : NodeRef < ViewNode >,
    level    : usize,
    config   : &SkgConfig,
  ) -> Result < String, Box<dyn Error> > {
    let viewnode : &ViewNode = node_ref . value ();
    let mut out : String =
      viewnode_to_text ( level, viewnode, config )?;
    for child in node_ref . children () {
      out . push_str (
        & render_node_subtree_to_org (
          child,
          level + 1,
          config )? ); }
    Ok (out) }
  let mut result : String =
    String::new ();
  for child in roots {
    result . push_str (
      & render_node_subtree_to_org ( child, 1, config )? ); }
  Ok (result) }

/// Renders an ViewNode as org-mode formatted text.
/// Not recursive -- just stars, metadata, title, and maybe a body.
/// ERRORS: If viewnode is a BufferRoot.
pub fn viewnode_to_text (
  level    : usize,
  viewnode : &ViewNode,
  config   : &SkgConfig,
) -> Result < String, Box<dyn Error> > {
  let metadata_str : String =
    viewnode_to_string (viewnode, config)?;
  let title : &str = viewnode . title ();
  let body : Option < &String > = viewnode . body ();
  if metadata_str . is_empty () && title . is_empty () {
    panic! (
      "viewnode_to_text called with both empty metadata and empty title"
    ); }
  let mut result : String =
    String::new ();
  result . push_str (
    &org_bullet (level));
  if ! metadata_str . is_empty () {
    result . push (' ');
    result . push_str ("(skg ");
    result . push_str (&metadata_str);
    result . push (')'); }
  if ! title . is_empty () {
    result . push (' ');
    result . push_str (title); }
  result . push ('\n');
  if let Some (body_text) = body {
    if ! body_text . is_empty () {
      result . push_str (body_text);
      if ! body_text . ends_with ('\n') {
        result . push ('\n'); }} }
  Ok (result) }

pub fn viewnode_to_string (
  viewnode : &ViewNode,
  config   : &SkgConfig,
) -> Result < String, Box<dyn Error> > {
  match &viewnode . kind {
    ViewNodeKind::QualCol (col) =>
      qualcol_metadata_to_string (
        viewnode . focused, viewnode . folded,
        viewnode . body_folded, col ),
    ViewNodeKind::Qual (qual) =>
      qual_metadata_to_string (
        viewnode . focused, viewnode . folded,
        viewnode . body_folded, qual ),
    ViewNodeKind::PartnerCol (partnerCol) =>
      Ok ( non_vognode_atom_metadata_to_string (
        viewnode . focused, viewnode . folded,
        viewnode . body_folded, partnerCol . repr_in_client () ) ),
    ViewNodeKind::BufferRoot =>
      Err ( "viewnode_to_string: BufferRoot should never be rendered" . into () ),
    ViewNodeKind::DeadScaffold =>
      Ok ( deleted_scaff_metadata_to_string (
        viewnode . focused, viewnode . folded,
        viewnode . body_folded )),
    ViewNodeKind::Vognode (Vognode::Active (activeNode)) =>
      Ok ( activeNode_metadata_to_string (
        viewnode . focused, viewnode . folded,
        viewnode . body_folded, activeNode, config )),
    ViewNodeKind::Phantom (Phantom::Diff (phantom)) =>
      Ok ( phantomDiff_metadata_to_string (
        viewnode . focused, viewnode . folded,
        viewnode . body_folded, phantom, config )),
    ViewNodeKind::Phantom (Phantom::Deleted (deleted_node)) =>
      Ok ( phantomDeleted_metadata_to_string (
        viewnode . focused, viewnode . folded,
        viewnode . body_folded, deleted_node )),
    ViewNodeKind::Phantom (Phantom::Unknown (unknown_node)) =>
      Ok ( phantomUnknown_metadata_to_string (
        viewnode . focused, viewnode . folded,
        viewnode . body_folded, unknown_node )),
    ViewNodeKind::Vognode (Vognode::Inactive (_)) =>
      Ok ( inactive_node_metadata_to_string (
        viewnode . focused, viewnode . folded,
        viewnode . body_folded )) } }

fn non_vognode_atom_metadata_to_string (
  focused     : bool,
  folded      : bool,
  body_folded : bool,
  atom        : &str,
) -> String {
  let mut parts : Vec < String > = Vec::new ();
  if focused     { parts . push ( "focused"    . to_string () ); }
  if folded      { parts . push ( "folded"     . to_string () ); }
  if body_folded { parts . push ( "bodyFolded" . to_string () ); }
  parts . push (atom . to_string ());
  parts . join (" ") }

fn qualcol_metadata_to_string (
  focused     : bool,
  folded      : bool,
  body_folded : bool,
  col         : &QualCol,
) -> Result < String, Box<dyn Error> > {
  Ok ( non_vognode_atom_metadata_to_string (
    focused, folded, body_folded, col . repr_in_client () ) ) }

fn qual_metadata_to_string (
  focused     : bool,
  folded      : bool,
  body_folded : bool,
  qual        : &Qual,
) -> Result < String, Box<dyn Error> > {
  let mut parts : Vec < String > = Vec::new ();
  if focused     { parts . push ( "focused"    . to_string () ); }
  if folded      { parts . push ( "folded"     . to_string () ); }
  if body_folded { parts . push ( "bodyFolded" . to_string () ); }
  match qual {
    Qual::Alias { membership, .. } => {
      parts . push ( "alias" . to_string () );
      append_membership_stage_forms (&mut parts, membership); }
    Qual::TextChanged { staged, unstaged } => {
      let mut tags : Vec<&'static str> = Vec::new ();
      if *staged   { tags . push ("staged"); }
      if *unstaged { tags . push ("unstaged"); }
      if tags . is_empty ()
      { parts . push ( "textChanged" . to_string () ); }
      else
      { parts . push ( format! ( "(textChanged {})", tags . join (" ") )); } }
    Qual::ID { membership, .. } => {
      parts . push ( "id" . to_string () );
      append_membership_stage_forms (&mut parts, membership); }
  }
  Ok ( parts . join (" ")) }

/// Emit '(staged AXES)' and/or '(unstaged AXES)' for a MembershipAxes.
/// Adds nothing if neither stage has a change.
fn append_membership_stage_forms (
  parts      : &mut Vec<String>,
  membership : &MembershipAxes,
) {
  if let Some (atom) = membership . staged_atom ()
    { parts . push ( format! ( "(staged {})", atom ) ); }
  if let Some (atom) = membership . unstaged_atom ()
    { parts . push ( format! ( "(unstaged {})", atom ) ); } }

/// Render metadata for an ActiveNode:
///   (skg [focused] [folded] (node ...))
fn activeNode_metadata_to_string (
  focused     : bool,
  folded      : bool,
  body_folded : bool,
  activeNode   : & ActiveNode,
  config      : & SkgConfig,
) -> String {
  fn node_sexp (
    activeNode : & ActiveNode,
    config    : & SkgConfig,
  ) -> String {
    fn birth_herald ( activeNode : & ActiveNode ) -> Option < String > {
      activeNode . viewStats . birth_herald . as_ref () . map (
        |s| format! ("(birthHerald {})", quote_herald (s)) ) }
    fn rels_herald ( activeNode : & ActiveNode ) -> Option < String > {
      activeNode . viewStats . rels_herald . as_ref () . map (
        |s| format! ("(rels {})", quote_herald (s)) ) }
    fn view_stats (
      activeNode : & ActiveNode,
      config    : & SkgConfig,
    ) -> Option < String > {
      let mut parts : Vec < String > = Vec::new ();
      if activeNode . viewStats . cycle {
        parts . push ( "cycle" . to_string () ); }
      if let Some (ref original) =
        activeNode . viewStats . overridesHere {
        parts . push ( format! ("(overridesHere {})",
                                 original . 0 )); }
      if let Some (ref level) =
        activeNode . viewStats . rel_source {
        parts . push ( format! ("(relSource {})", level )); }
      if activeNode . viewStats . sourceAtBoundary {
        if let Some (src_config)
        = config . sources . get ( &activeNode . source )
        { parts . push ( format! ("(sourceHerald ⌂:{})",
                                  src_config . herald_label () )); }}
      if parts . is_empty () { None }
      else { Some ( format! (
               "(viewStats {})", parts . join (" ") )) }}
    fn edit_request ( activeNode : & ActiveNode
                    ) -> Option < String > {
      activeNode . edit_request () . map ( | edit_req | {
        let edit_str : String = match edit_req {
          EditRequest::NodeMerge (id) => format! ( "(merge {})", id . 0 ),
          EditRequest::Delete => "delete" . to_string () };
        format! ( "(editRequest {})", edit_str ) } ) }
    fn view_requests ( activeNode : & ActiveNode
                     ) -> Option < String > {
      if activeNode . view_requests . is_empty () { return None; }
      let mut request_strings : Vec < String > =
        activeNode . view_requests . iter ()
          . map ( | req | req . to_string () )
          . collect ();
      request_strings . sort ();
      Some ( format! ( "(viewRequests {})",
                       request_strings . join (" ") )) }
    fn staged_axes ( activeNode : & ActiveNode ) -> Option < String > {
      let mut atoms : Vec<&'static str> = Vec::new ();
      if let Some (a) = activeNode . existence  . staged_atom ()
        { atoms . push (a); }
      if let Some (a) = activeNode . membership . staged_atom ()
        { atoms . push (a); }
      if atoms . is_empty () { None }
      else { Some ( format! ( "(staged {})", atoms . join (" "))) } }
    fn unstaged_axes ( activeNode : & ActiveNode ) -> Option < String > {
      let mut atoms : Vec<&'static str> = Vec::new ();
      if let Some (a) = activeNode . existence  . unstaged_atom ()
        { atoms . push (a); }
      if let Some (a) = activeNode . membership . unstaged_atom ()
        { atoms . push (a); }
      if atoms . is_empty () { None }
      else { Some ( format! ( "(unstaged {})", atoms . join (" "))) } }
    fn not_in_git_atom ( activeNode : & ActiveNode ) -> Option < String > {
      if activeNode . not_in_git { Some ("notInGit" . to_string ()) }
      else                      { None } }
    let mut parts : Vec < String > =
      vec! [ "node" . to_string () ];
    parts . push ( format! ( "(id {})", activeNode . id . 0 ));
    parts . push ( format! ( "(source {})", activeNode . source ));
    // ParentIs::Affected is left implicit because it is the default
    // membership relation.
    match activeNode . parentIs {
      ParentIs::Affected => {},
      ParentIs::Absent =>
        parts . push ( "(parentIs absent)" . to_string () ),
      ParentIs::Independent =>
        parts . push ( "(parentIs independent)" . to_string () ) }
    if activeNode . is_indefinitive () {
      // "indef" is short for "indefinitive" -- a read-only view of
      // a node (see IndefOrDef in types/viewnode.rs). The metadata
      // sexp uses only this short form on both emission and parsing.
      parts . push ( "indef" . to_string () );
      if activeNode . viewStats . hidden_body {
        // The rendering is hiding a body (herald "B" on the ☮).
        parts . push ( "hiddenBody" . to_string () ); }}
    if let Some (s) = birth_herald (activeNode)
    { parts . push (s); }
    if let Some (s) = rels_herald (activeNode)
    { parts . push (s); }
    if let Some (s) = view_stats (activeNode, config)
    { parts . push (s); }
    if let Some (s) = edit_request (activeNode)
    { parts . push (s); }
    if let Some (s) = view_requests (activeNode)
    { parts . push (s); }
    if let Some (s) = staged_axes (activeNode)
    { parts . push (s); }
    if let Some (s) = unstaged_axes (activeNode)
    { parts . push (s); }
    if let Some (s) = not_in_git_atom (activeNode)
    { parts . push (s); }
    format! ( "({})", parts . join (" ")) }
  let mut parts : Vec < String > = Vec::new ();
  if focused     { parts . push ( "focused"    . to_string () ); }
  if folded      { parts . push ( "folded"     . to_string () ); }
  if body_folded { parts . push ( "bodyFolded" . to_string () ); }
  parts . push ( node_sexp (activeNode, config));
  parts . join (" ") }

/// Render metadata for a PhantomDiff (TODO/DONE/local-view-update/plan_v2.org §11). The root atom is
/// `diffPhantom`, distinct from the `node` atom an ActiveNode emits, so the
/// client can tell a moved/removed phantom apart from a live node without
/// inferring it from the diff axes. A phantom is always indefinitive (so always
/// emits `indef` and never a body, editRequest, or viewRequests) and its
/// parentIs is implicit Affected and birth Unremarkable (so neither atom
/// appears, and graphStats is rendered as if Affected / Unremarkable). It
/// carries no viewStats. What remains: id, source, indef, graphStats, the
/// staged/unstaged diff axes, and notInGit.
fn phantomDiff_metadata_to_string (
  focused     : bool,
  folded      : bool,
  body_folded : bool,
  phantom     : & PhantomDiff,
  config      : & SkgConfig,
) -> String {
  fn node_sexp (
    phantom : & PhantomDiff,
    config  : & SkgConfig,
  ) -> String {
    let mut parts : Vec < String > =
      vec! [ "diffPhantom" . to_string () ];
    parts . push ( format! ( "(id {})", phantom . id . 0 ));
    parts . push ( format! ( "(source {})", phantom . source ));
    // parentIs is implicit Affected and birth Unremarkable on a phantom, so
    // neither atom is emitted; both are passed as such to graphnodestats.
    parts . push ( "indef" . to_string () );
    if let Some (s) = phantom_rels_atom (& phantom . graphStats)
    { parts . push (s); }
    { let mut atoms : Vec<&'static str> = Vec::new ();
      if let Some (a) = phantom . existence  . staged_atom () { atoms . push (a); }
      if let Some (a) = phantom . membership . staged_atom () { atoms . push (a); }
      if ! atoms . is_empty ()
      { parts . push ( format! ( "(staged {})", atoms . join (" "))); } }
    { let mut atoms : Vec<&'static str> = Vec::new ();
      if let Some (a) = phantom . existence  . unstaged_atom () { atoms . push (a); }
      if let Some (a) = phantom . membership . unstaged_atom () { atoms . push (a); }
      if ! atoms . is_empty ()
      { parts . push ( format! ( "(unstaged {})", atoms . join (" "))); } }
    if phantom . not_in_git
    { parts . push ( "notInGit" . to_string () ); }
    let _ = config; // reserved for parity with activeNode_metadata_to_string
    format! ( "({})", parts . join (" ")) }
  let mut parts : Vec < String > = Vec::new ();
  if focused     { parts . push ( "focused"    . to_string () ); }
  if folded      { parts . push ( "folded"     . to_string () ); }
  if body_folded { parts . push ( "bodyFolded" . to_string () ); }
  parts . push ( node_sexp (phantom, config));
  parts . join (" ") }

/// Render metadata for a PhantomDeleted:
///   (skg [focused] [folded] (deleted (id X) (source S)))
fn phantomDeleted_metadata_to_string (
  focused      : bool,
  folded       : bool,
  body_folded  : bool,
  deleted_node : &PhantomDeleted,
) -> String {
  let mut parts : Vec < String > = Vec::new ();
  if focused     { parts . push ( "focused"    . to_string () ); }
  if folded      { parts . push ( "folded"     . to_string () ); }
  if body_folded { parts . push ( "bodyFolded" . to_string () ); }
  parts . push ( format! ( "(deleted (id {}) (source {}))",
                            deleted_node . id . 0,
                            deleted_node . source ));
  parts . join (" ") }

/// Render metadata for an PhantomUnknown:
///   (skg [focused] [folded] (unknown (id X)))
/// Triggered when a referenced ID resolved to nothing in any db.
fn phantomUnknown_metadata_to_string (
  focused      : bool,
  folded       : bool,
  body_folded  : bool,
  unknown_node : &PhantomUnknown,
) -> String {
  let mut parts : Vec < String > = Vec::new ();
  if focused     { parts . push ( "focused"    . to_string () ); }
  if folded      { parts . push ( "folded"     . to_string () ); }
  if body_folded { parts . push ( "bodyFolded" . to_string () ); }
  parts . push ( format! ( "(unknown (id {}))",
                            unknown_node . id . 0 ));
  parts . join (" ") }

/// Render an inactive placeholder as the bare atom 'inactiveNode',
/// like the other dataless scaffold markers (aliasCol, subscribeeCol,
/// ...). It carries no id/source/etc. -- those describe content the
/// user hid by restricting the source-set, so emitting them would leak
/// (see InactiveNode).
fn inactive_node_metadata_to_string (
  focused       : bool,
  folded        : bool,
  body_folded   : bool,
) -> String {
  let mut parts : Vec < String > = Vec::new ();
  if focused     { parts . push ( "focused"    . to_string () ); }
  if folded      { parts . push ( "folded"     . to_string () ); }
  if body_folded { parts . push ( "bodyFolded" . to_string () ); }
  parts . push ( "inactiveNode" . to_string () );
  parts . join (" ") }

/// Render metadata for a DeletedScaff:
///   (skg [focused] [folded] (deletedScaffold kindString))
fn deleted_scaff_metadata_to_string (
  focused     : bool,
  folded      : bool,
  body_folded : bool,
) -> String {
  let mut parts : Vec < String > = Vec::new ();
  if focused     { parts . push ( "focused"    . to_string () ); }
  if folded      { parts . push ( "folded"     . to_string () ); }
  if body_folded { parts . push ( "bodyFolded" . to_string () ); }
  parts . push ( "(deletedScaffold deadScaffold)" . to_string () );
  parts . join (" ") }

/// The blue relationship-herald atom for a phantom: counts-only tokens
/// (no ancestor flags, no birth, no view position) plus =Ak= / =Ik=.
fn phantom_rels_atom (
  gs : &GraphNodeStats,
) -> Option < String > {
  gs . rels . as_ref ()
    . and_then ( |counts| assemble_counts_only (
      counts, gs . aliases, gs . extra_ids ) )
    . map ( |s| format! ("(rels {})", quote_herald (&s)) ) }

/// Quote an assembled herald string for the metadata sexp. The strings
/// contain spaces and parens (e.g. =2(1,1)L=), so they MUST be quoted;
/// both the Rust 'sexp' parser and Emacs 'read' accept quoted strings.
fn quote_herald (
  s : &str,
) -> String {
  format! ( "\"{}\"",
            s . replace ('\\', "\\\\") . replace ('"', "\\\"") ) }

fn org_bullet ( level: usize ) -> String {
  "*" . repeat ( level . max (1)) }
