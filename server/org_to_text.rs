use crate::types::misc::{SkgConfig, SkgfileSource};
use crate::types::viewnode::{ ViewNode, ViewNodeKind, Scaffold, ScaffoldKind, TrueNode, DeletedNode, EditRequest, GraphNodeStats, Birth };

use ego_tree::{NodeRef, Tree};
use std::error::Error;

/// PURPOSE: Render a "forest" -- a tree with BufferRoot at root
/// -- to org-mode text.
/// BufferRoot is not rendered; its children start at level 1.
///
/// ASSUMES: metadata has already been enriched with relationship data.
/// ERRORS: if root is not a BufferRoot.
pub fn viewnode_forest_to_string (
  forest : &Tree<ViewNode>,
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
  let root_ref : NodeRef<ViewNode> = forest . root ();
  let is_forest_root : bool =
    matches! (
      & root_ref . value () . kind,
      ViewNodeKind::Scaff (Scaffold::BufferRoot));
  if ! is_forest_root {
    return Err (
      "viewnode_forest_to_string: root is not a BufferRoot" . into() ); }
  let mut result : String =
    String::new ();
  for child in root_ref . children () {
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
    ViewNodeKind::Scaff (scaffold) =>
      scaffold_metadata_to_string (
        viewnode . focused, viewnode . folded, scaffold ),
    ViewNodeKind::True (true_node) =>
      Ok ( true_node_metadata_to_string (
        viewnode . focused, viewnode . folded, true_node, config )),
    ViewNodeKind::Deleted (deleted_node) =>
      Ok ( deleted_node_metadata_to_string (
        viewnode . focused, viewnode . folded, deleted_node )),
    ViewNodeKind::DeletedScaff (kind) =>
      Ok ( deleted_scaff_metadata_to_string (
        viewnode . focused, viewnode . folded, kind )) }}

/// Render metadata for a Scaffold:
///   (skg [focused] [folded] scaffoldKind)
/// where scaffoldKind is a bare atom.
/// ERRORS: if scaffold is BufferRoot.
fn scaffold_metadata_to_string (
  focused  : bool,
  folded   : bool,
  scaffold : &Scaffold
) -> Result < String, Box<dyn Error> > {
  let mut parts : Vec < String > = Vec::new ();
  if focused { parts . push ( "focused" . to_string () ); }
  if folded  { parts . push ( "folded" . to_string () ); }
  match scaffold {
    Scaffold::Alias { diff, .. } => {
      parts . push ( "alias" . to_string () );
      if let Some (d) = diff {
        parts . push ( format! ( "(diff {})", d . repr_in_client() ) ); }}
    Scaffold::AliasCol =>
      parts . push ( "aliasCol" . to_string () ),
    Scaffold::BufferRoot =>
      return Err ( "scaffold_metadata_to_string: BufferRoot should never be rendered" . into () ),
    Scaffold::HiddenInSubscribeeCol =>
      parts . push ( "hiddenInSubscribeeCol" . to_string () ),
    Scaffold::HiddenOutsideOfSubscribeeCol =>
      parts . push ( "hiddenOutsideOfSubscribeeCol" . to_string () ),
    Scaffold::SubscribeeCol =>
      parts . push ( "subscribeeCol" . to_string () ),
    Scaffold::TextChanged =>
      parts . push ( "textChanged" . to_string () ),
    Scaffold::IDCol =>
      parts . push ( "idCol" . to_string () ),
    Scaffold::ID { diff, .. } => {
      parts . push ( "id" . to_string () );
      if let Some (d) = diff {
        parts . push ( format! ( "(diff {})", d . repr_in_client() ) ); }}
  }
  Ok ( parts . join (" ")) }

/// Render metadata for a TrueNode:
///   (skg [focused] [folded] (node ...))
fn true_node_metadata_to_string (
  focused   : bool,
  folded    : bool,
  true_node : & TrueNode,
  config    : & SkgConfig,
) -> String {
  fn node_sexp (
    true_node : & TrueNode,
    config    : & SkgConfig,
  ) -> String {
    fn graph_stats ( true_node : & TrueNode ) -> Option < String > {
      let herald_should_be_rooty : bool =
        true_node . birth != Birth::ContentOf;
      graphnodestats_to_sexp (
        & true_node . graphStats, herald_should_be_rooty ) }
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
    fn diff_status ( true_node : & TrueNode ) -> Option < String > {
      true_node . diff . as_ref () . map ( | d |
        format! ( "(diff {})", d . repr_in_client () )) }
    let mut parts : Vec < String > =
      vec! [ "node" . to_string () ];
    parts . push ( format! ( "(id {})", true_node . id . 0 ));
    parts . push ( format! ( "(source {})", true_node . source ));
    match true_node . birth {
      Birth::ContentOf => {},
      Birth::Independent =>
        parts . push ( "(birth independent)" . to_string () ),
      Birth::ContainerOf =>
        parts . push ( "(birth containerOf)" . to_string () ),
      Birth::LinksTo =>
        parts . push ( "(birth linksTo)" . to_string () ) }
    if true_node . is_indefinitive () {
      parts . push ( "indefinitive" . to_string () ); }
    if let Some (s) = graph_stats (true_node)
    { parts . push (s); }
    if let Some (s) = view_stats (true_node, config)
    { parts . push (s); }
    if let Some (s) = edit_request (true_node)
    { parts . push (s); }
    if let Some (s) = view_requests (true_node)
    { parts . push (s); }
    if let Some (s) = diff_status (true_node)
    { parts . push (s); }
    format! ( "({})", parts . join (" ")) }
  let mut parts : Vec < String > = Vec::new ();
  if focused { parts . push ( "focused" . to_string () ); }
  if folded  { parts . push ( "folded" . to_string () ); }
  parts . push ( node_sexp (true_node, config));
  parts . join (" ") }

/// Render metadata for a DeletedNode:
///   (skg [focused] [folded] (deleted (id X) (source S)))
fn deleted_node_metadata_to_string (
  focused      : bool,
  folded       : bool,
  deleted_node : &DeletedNode,
) -> String {
  let mut parts : Vec < String > = Vec::new ();
  if focused { parts . push ( "focused" . to_string () ); }
  if folded  { parts . push ( "folded" . to_string () ); }
  parts . push ( format! ( "(deleted (id {}) (source {}))",
                            deleted_node . id . 0,
                            deleted_node . source ));
  parts . join (" ") }

/// Render metadata for a DeletedScaff:
///   (skg [focused] [folded] (deletedScaffold kindString))
fn deleted_scaff_metadata_to_string (
  focused : bool,
  folded  : bool,
  kind    : &ScaffoldKind,
) -> String {
  let mut parts : Vec < String > = Vec::new ();
  if focused { parts . push ( "focused" . to_string () ); }
  if folded  { parts . push ( "folded" . to_string () ); }
  parts . push ( format! ( "(deletedScaffold {})",
                           kind . repr_in_client () ) );
  parts . join (" ") }

fn graphnodestats_to_sexp (
  gs                     : &GraphNodeStats,
  herald_should_be_rooty : bool,
) -> Option < String > {
  let mut parts : Vec < String > = Vec::new ();
  if let Some (ref c) = gs . containRels {
    if herald_should_be_rooty || c . containers != 1 {
      parts . push ( format! ("(containers {})", c . containers) ); }
    if c . contents != 0 {
      parts . push ( format! ("(contents {})", c . contents) ); }
    let herald : Option<String> =
      if herald_should_be_rooty { Some ( c . herald_for_non_content() ) }
      else                      {        c . herald_for_content() };
    if let Some (h) = herald {
      parts . push ( format! ("(containsHerald {})", h) ); }}
  if let Some (ref l) = gs . linksourceRels {
    if l . sources_with_content != 0 {
      parts . push ( format! ("(linksInFromContainers {})",
                              l . sources_with_content) ); }
    if l . sources_without_content != 0 {
      parts . push ( format! ("(linksInFromLeaves {})",
                              l . sources_without_content) ); }
    if let Some (h) = l . herald() {
      parts . push ( format! ("(linksHerald {})", h) ); }}
  if gs . aliasing {
    parts . push ( "aliasing" . to_string () ); }
  if gs . extraIDs {
    parts . push ( "extraIDs" . to_string () ); }
  if gs . overriding {
    parts . push ( "overriding" . to_string () ); }
  if gs . subscribing {
    parts . push ( "subscribing" . to_string () ); }
  if let Some (ref cp) = gs . containerwardPath {
    if cp . length > 0 || cp . forks > 1 || cp . cycles {
      parts . push (
        format! ( "(containerwardPath {})",
                  cp . to_display_atom () )); }}
  if parts . is_empty () { None }
  else { Some ( format! (
           "(graphStats {})", parts . join (" ") )) }}

fn org_bullet ( level: usize ) -> String {
  "*" . repeat ( level . max (1)) }
