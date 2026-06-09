use crate::types::git::MembershipAxes;
use crate::types::misc::SkgConfig;
use crate::types::tree::forest::ViewForest;
use crate::types::viewnode::{
  ViewNode, ViewNodeKind, Vognode, Qual, QualCol, TrueNode, DiffPhantomNode,
  DeletedNode, UnknownNode, InactiveNode, EditRequest, GraphNodeStats,
  Birth, ParentIs,
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
    ViewNodeKind::PartnerCol (roleCol) =>
      Ok ( non_vognode_atom_metadata_to_string (
        viewnode . focused, viewnode . folded,
        viewnode . body_folded, roleCol . repr_in_client () ) ),
    ViewNodeKind::BufferRoot =>
      Err ( "viewnode_to_string: BufferRoot should never be rendered" . into () ),
    ViewNodeKind::DeadScaffold =>
      Ok ( deleted_scaff_metadata_to_string (
        viewnode . focused, viewnode . folded,
        viewnode . body_folded )),
    ViewNodeKind::Vognode (Vognode::Normal (true_node)) =>
      Ok ( true_node_metadata_to_string (
        viewnode . focused, viewnode . folded,
        viewnode . body_folded, true_node, config )),
    ViewNodeKind::Vognode (Vognode::DiffPhantom (phantom)) =>
      Ok ( diffphantom_metadata_to_string (
        viewnode . focused, viewnode . folded,
        viewnode . body_folded, phantom, config )),
    ViewNodeKind::Vognode (Vognode::Deleted (deleted_node)) =>
      Ok ( deleted_node_metadata_to_string (
        viewnode . focused, viewnode . folded,
        viewnode . body_folded, deleted_node )),
    ViewNodeKind::Vognode (Vognode::Unknown (unknown_node)) =>
      Ok ( unknown_node_metadata_to_string (
        viewnode . focused, viewnode . folded,
        viewnode . body_folded, unknown_node )),
    ViewNodeKind::Vognode (Vognode::Inactive (inactive_node)) =>
      Ok ( inactive_node_metadata_to_string (
        viewnode . focused, viewnode . folded,
        viewnode . body_folded, inactive_node )) } }

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

/// Render metadata for a TrueNode:
///   (skg [focused] [folded] (node ...))
fn true_node_metadata_to_string (
  focused     : bool,
  folded      : bool,
  body_folded : bool,
  true_node   : & TrueNode,
  config      : & SkgConfig,
) -> String {
  fn node_sexp (
    true_node : & TrueNode,
    config    : & SkgConfig,
  ) -> String {
    fn graph_stats ( true_node : & TrueNode ) -> Option < String > {
      graphnodestats_to_sexp ( & true_node . graphStats,
                                 true_node . parentIs,
                                 true_node . birth ) }
    fn view_stats (
      true_node : & TrueNode,
      config    : & SkgConfig,
    ) -> Option < String > {
      let mut parts : Vec < String > = Vec::new ();
      if true_node . viewStats . cycle {
        parts . push ( "cycle" . to_string () ); }
      if true_node . viewStats . parentIsContent {
        parts . push ( "containsParent" . to_string () ); }
      if true_node . viewStats . sourceAtBoundary {
        if let Some (src_config)
        = config . sources . get ( &true_node . source )
        { parts . push ( format! ("(sourceHerald ⌂:{})",
                                  src_config . herald_label () )); }}
      if parts . is_empty () { None }
      else { Some ( format! (
               "(viewStats {})", parts . join (" ") )) }}
    fn edit_request ( true_node : & TrueNode
                    ) -> Option < String > {
      true_node . edit_request () . map ( | edit_req | {
        let edit_str : String = match edit_req {
          EditRequest::Merge (id) => format! ( "(merge {})", id . 0 ),
          EditRequest::Delete => "delete" . to_string () };
        format! ( "(editRequest {})", edit_str ) } ) }
    fn view_requests ( true_node : & TrueNode
                     ) -> Option < String > {
      if true_node . view_requests . is_empty () { return None; }
      let mut request_strings : Vec < String > =
        true_node . view_requests . iter ()
          . map ( | req | req . to_string () )
          . collect ();
      request_strings . sort ();
      Some ( format! ( "(viewRequests {})",
                       request_strings . join (" ") )) }
    fn staged_axes ( true_node : & TrueNode ) -> Option < String > {
      let mut atoms : Vec<&'static str> = Vec::new ();
      if let Some (a) = true_node . existence  . staged_atom ()
        { atoms . push (a); }
      if let Some (a) = true_node . membership . staged_atom ()
        { atoms . push (a); }
      if atoms . is_empty () { None }
      else { Some ( format! ( "(staged {})", atoms . join (" "))) } }
    fn unstaged_axes ( true_node : & TrueNode ) -> Option < String > {
      let mut atoms : Vec<&'static str> = Vec::new ();
      if let Some (a) = true_node . existence  . unstaged_atom ()
        { atoms . push (a); }
      if let Some (a) = true_node . membership . unstaged_atom ()
        { atoms . push (a); }
      if atoms . is_empty () { None }
      else { Some ( format! ( "(unstaged {})", atoms . join (" "))) } }
    fn not_in_git_atom ( true_node : & TrueNode ) -> Option < String > {
      if true_node . not_in_git { Some ("notInGit" . to_string ()) }
      else                      { None } }
    let mut parts : Vec < String > =
      vec! [ "node" . to_string () ];
    parts . push ( format! ( "(id {})", true_node . id . 0 ));
    parts . push ( format! ( "(source {})", true_node . source ));
    // ParentIs::Affected is left implicit because it is the default
    // membership relation.
    match true_node . parentIs {
      ParentIs::Affected => {},
      ParentIs::Absent =>
        parts . push ( "(parentIs absent)" . to_string () ),
      ParentIs::Independent =>
        parts . push ( "(parentIs independent)" . to_string () ) }
    match true_node . birth {
      Birth::Unremarkable => {},
      Birth::ContainsParent =>
        parts . push ( "(birth containsParent)" . to_string () ),
      Birth::LinksToParent =>
        parts . push ( "(birth linksToParent)" . to_string () ) }
    if true_node . is_indefinitive () {
      // "indef" is short for "indefinitive" -- a read-only view of
      // a node (see IndefOrDef in types/viewnode.rs). The metadata
      // sexp uses only this short form on both emission and parsing.
      parts . push ( "indef" . to_string () ); }
    if let Some (s) = graph_stats (true_node)
    { parts . push (s); }
    if let Some (s) = view_stats (true_node, config)
    { parts . push (s); }
    if let Some (s) = edit_request (true_node)
    { parts . push (s); }
    if let Some (s) = view_requests (true_node)
    { parts . push (s); }
    if let Some (s) = staged_axes (true_node)
    { parts . push (s); }
    if let Some (s) = unstaged_axes (true_node)
    { parts . push (s); }
    if let Some (s) = not_in_git_atom (true_node)
    { parts . push (s); }
    format! ( "({})", parts . join (" ")) }
  let mut parts : Vec < String > = Vec::new ();
  if focused     { parts . push ( "focused"    . to_string () ); }
  if folded      { parts . push ( "folded"     . to_string () ); }
  if body_folded { parts . push ( "bodyFolded" . to_string () ); }
  parts . push ( node_sexp (true_node, config));
  parts . join (" ") }

/// Render metadata for a DiffPhantomNode (TODO/DONE/local-view-update/plan_v2.org §11). A phantom is always
/// indefinitive (so always emits `indef` and never a body, editRequest, or
/// viewRequests) and its parentIs is implicit Affected and birth Unremarkable
/// (so neither atom appears, and graphStats is rendered as if Affected /
/// Unremarkable). It carries no viewStats. What remains: id, source, indef,
/// graphStats, the staged/unstaged diff axes, and notInGit. This is
/// byte-identical to what the old shared TrueNode renderer produced for a
/// phantom (verified: no phantom ever carried parentIs/birth/viewStats/
/// viewRequests in any oracle).
fn diffphantom_metadata_to_string (
  focused     : bool,
  folded      : bool,
  body_folded : bool,
  phantom     : & DiffPhantomNode,
  config      : & SkgConfig,
) -> String {
  fn node_sexp (
    phantom : & DiffPhantomNode,
    config  : & SkgConfig,
  ) -> String {
    let mut parts : Vec < String > =
      vec! [ "node" . to_string () ];
    parts . push ( format! ( "(id {})", phantom . id . 0 ));
    parts . push ( format! ( "(source {})", phantom . source ));
    // parentIs is implicit Affected and birth Unremarkable on a phantom, so
    // neither atom is emitted; both are passed as such to graphnodestats.
    parts . push ( "indef" . to_string () );
    if let Some (s) = graphnodestats_to_sexp (
      & phantom . graphStats, ParentIs::Affected, Birth::Unremarkable )
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
    let _ = config; // reserved for parity with true_node_metadata_to_string
    format! ( "({})", parts . join (" ")) }
  let mut parts : Vec < String > = Vec::new ();
  if focused     { parts . push ( "focused"    . to_string () ); }
  if folded      { parts . push ( "folded"     . to_string () ); }
  if body_folded { parts . push ( "bodyFolded" . to_string () ); }
  parts . push ( node_sexp (phantom, config));
  parts . join (" ") }

/// Render metadata for a DeletedNode:
///   (skg [focused] [folded] (deleted (id X) (source S)))
fn deleted_node_metadata_to_string (
  focused      : bool,
  folded       : bool,
  body_folded  : bool,
  deleted_node : &DeletedNode,
) -> String {
  let mut parts : Vec < String > = Vec::new ();
  if focused     { parts . push ( "focused"    . to_string () ); }
  if folded      { parts . push ( "folded"     . to_string () ); }
  if body_folded { parts . push ( "bodyFolded" . to_string () ); }
  parts . push ( format! ( "(deleted (id {}) (source {}))",
                            deleted_node . id . 0,
                            deleted_node . source ));
  parts . join (" ") }

/// Render metadata for an UnknownNode:
///   (skg [focused] [folded] (unknownNode (id X)))
/// Triggered when a referenced ID resolved to nothing in any db.
fn unknown_node_metadata_to_string (
  focused      : bool,
  folded       : bool,
  body_folded  : bool,
  unknown_node : &UnknownNode,
) -> String {
  let mut parts : Vec < String > = Vec::new ();
  if focused     { parts . push ( "focused"    . to_string () ); }
  if folded      { parts . push ( "folded"     . to_string () ); }
  if body_folded { parts . push ( "bodyFolded" . to_string () ); }
  parts . push ( format! ( "(unknownNode (id {}))",
                            unknown_node . id . 0 ));
  parts . join (" ") }

fn inactive_node_metadata_to_string (
  focused       : bool,
  folded        : bool,
  body_folded   : bool,
  inactive_node : &InactiveNode,
) -> String {
  let mut parts : Vec < String > = Vec::new ();
  if focused     { parts . push ( "focused"    . to_string () ); }
  if folded      { parts . push ( "folded"     . to_string () ); }
  if body_folded { parts . push ( "bodyFolded" . to_string () ); }
  let mut inactive_parts : Vec<String> =
    vec! [
      "inactiveNode" . to_string (),
      format! ("(id {})", inactive_node . id . 0),
      format! ("(source {})", inactive_node . source) ];
  append_membership_stage_forms (
    &mut inactive_parts,
    &inactive_node . membership );
  parts . push ( format! (
    "({})", inactive_parts . join (" ") ));
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

fn graphnodestats_to_sexp (
  gs       : &GraphNodeStats,
  parentIs : ParentIs,
  birth    : Birth,
) -> Option < String > {
  let mut parts : Vec < String > = Vec::new ();
  if let Some (ref c) = gs . containRels {
    // Per-parentIs suppression of the containers count. The client
    // INTERCs (containers N) and (contents M) into a combined
    // herald with `{` between them; here, the server merely decides
    // whether each raw atom is worth emitting.
    // - Affected   : hide the common case of exactly 1 container
    //                (every content-child has at least one
    //                container; the number only matters when it's
    //                0 or >1).
    // - Independent|Absent: hide the common case of 0 containers (a view
    //                       root is typically standalone).
    // - Non-default birth provenance: always show (the birth metadata
    //                indicates that the node is *related* to the view
    //                root via the graph, so the count is always
    //                informative).
    let show_containers : bool =
      if birth != Birth::Unremarkable { true
      } else { match parentIs {
        ParentIs::Affected                       => c . containers != 1,
        ParentIs::Independent | ParentIs::Absent => c . containers != 0 }};
    let show_contents : bool = c . contents != 0;
    if show_containers {
      parts . push ( format! ("(containers {})", c . containers) ); }
    if show_contents {
      parts . push ( format! ("(contents {})", c . contents) ); } }
  if let Some (ref l) = gs . linksourceRels {
    // Client INTERCs these two into a linksHerald via `→`.
    if l . sources_with_content != 0 {
      parts . push ( format! ("(linksInFromContainers {})",
                              l . sources_with_content) ); }
    if l . sources_without_content != 0 {
      parts . push ( format! ("(linksInFromLeaves {})",
                              l . sources_without_content) ); } }
  if gs . aliasing {
    parts . push ( "aliasing" . to_string () ); }
  if gs . extraIDs {
    parts . push ( "extraIDs" . to_string () ); }
  if gs . overriding {
    parts . push ( "overriding" . to_string () ); }
  if gs . subscribing {
    parts . push ( "subscribing" . to_string () ); }
  if parts . is_empty () { None }
  else { Some ( format! (
           "(graphStats {})", parts . join (" ") )) }}

fn org_bullet ( level: usize ) -> String {
  "*" . repeat ( level . max (1)) }
