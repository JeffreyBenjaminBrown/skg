use crate::types::viewnode::{
    ViewNode, ViewNodeKind, Scaffold, TrueNode, EditRequest,
};

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
) -> Result < String, Box<dyn Error> > {
  fn render_node_subtree_to_org (
    node_ref : NodeRef < ViewNode >,
    level    : usize,
  ) -> Result < String, Box<dyn Error> > {
    let viewnode : &ViewNode = node_ref . value ();
    let mut out : String =
      viewnode_to_text ( level, viewnode )?;
    for child in node_ref . children () {
      out . push_str (
        & render_node_subtree_to_org (
          child,
          level + 1 )? ); }
    Ok ( out ) }
  let root_ref : NodeRef<ViewNode> = forest . root ();
  let is_forest_root : bool =
    matches! (
      & root_ref . value () . kind,
      ViewNodeKind::Scaff ( Scaffold::BufferRoot ));
  if ! is_forest_root {
    return Err (
      "viewnode_forest_to_string: root is not a BufferRoot".into() ); }
  let mut result : String =
    String::new ();
  for child in root_ref . children () {
    result . push_str (
      & render_node_subtree_to_org ( child, 1 )? ); }
  Ok ( result ) }

/// Renders an ViewNode as org-mode formatted text.
/// Not recursive -- just stars, metadata, title, and maybe a body.
/// ERRORS: If viewnode is a BufferRoot.
pub fn viewnode_to_text (
  level    : usize,
  viewnode : &ViewNode
) -> Result < String, Box<dyn Error> > {
  let metadata_str : String =
    viewnode_to_string ( viewnode )?;
  let title : &str = match &viewnode . kind {
    ViewNodeKind::True  ( t ) => &t . title,
    ViewNodeKind::Scaff ( s ) => s . title (),
  };
  let body : Option < &String > = match &viewnode . kind {
    ViewNodeKind::True  ( t ) => t . body . as_ref (),
    ViewNodeKind::Scaff ( _ ) => None,
  };
  if metadata_str . is_empty () && title . is_empty () {
    panic! (
      "viewnode_to_text called with both empty metadata and empty title"
    ); }
  let mut result : String =
    String::new ();
  result . push_str (
    &org_bullet ( level ));
  if ! metadata_str . is_empty () {
    result . push ( ' ' );
    result . push_str ( "(skg " );
    result . push_str ( &metadata_str );
    result . push ( ')' ); }
  if ! title . is_empty () {
    result . push ( ' ' );
    result . push_str ( title ); }
  result . push ( '\n' );
  if let Some ( body_text ) = body {
    if ! body_text . is_empty () {
      result . push_str ( body_text );
      if ! body_text . ends_with ( '\n' ) {
        result . push ( '\n' ); }} }
  Ok ( result ) }

pub fn viewnode_to_string (
  viewnode : &ViewNode
) -> Result < String, Box<dyn Error> > {
  match &viewnode . kind {
    ViewNodeKind::Scaff ( scaffold ) =>
      scaffold_metadata_to_string ( viewnode . focused, viewnode . folded, scaffold ),
    ViewNodeKind::True ( true_node ) =>
      Ok ( true_node_metadata_to_string ( viewnode . focused, viewnode . folded, true_node )),
  }}

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
      if let Some ( d ) = diff {
        parts . push ( format! ( "(diff {})", d.repr_in_client() ) ); }}
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
      if let Some ( d ) = diff {
        parts . push ( format! ( "(diff {})", d.repr_in_client() ) ); }}
  }
  Ok ( parts . join ( " " )) }

/// Render metadata for a TrueNode:
///   (skg [focused] [folded] (node ...))
fn true_node_metadata_to_string (
  focused   : bool,
  folded    : bool,
  true_node : & TrueNode
) -> String {
  fn node_sexp ( true_node : & TrueNode ) -> String {
    fn graph_stats ( true_node : & TrueNode ) -> Option < String > {
      let mut parts : Vec < String > = Vec::new ();
      if true_node . graphStats . numContainers != Some ( 1 ) {
        if let Some ( count ) = true_node . graphStats . numContainers {
          parts . push ( format! ( "(containers {})", count )); }}
      if true_node . graphStats . numContents != Some ( 0 ) {
        if let Some ( count ) = true_node . graphStats . numContents {
          parts . push ( format! ( "(contents {})", count )); }}
      if true_node . graphStats . numLinksIn != Some ( 0 ) {
        if let Some ( count ) = true_node . graphStats . numLinksIn {
          parts . push ( format! ( "(linksIn {})", count )); }}
      if true_node . graphStats . aliasing {
        parts . push ( "aliasing" . to_string () ); }
      if true_node . graphStats . extraIDs {
        parts . push ( "extraIDs" . to_string () ); }
      if true_node . graphStats . overriding {
        parts . push ( "overriding" . to_string () ); }
      if true_node . graphStats . subscribing {
        parts . push ( "subscribing" . to_string () ); }
      if parts . is_empty () { None }
      else { Some ( format! ( "(graphStats {})", parts . join ( " " ))) }}
    fn view_stats ( true_node : & TrueNode ) -> Option < String > {
      let mut parts : Vec < String > = Vec::new ();
      if true_node . viewStats . cycle {
        parts . push ( "cycle" . to_string () ); }
      if ! true_node . viewStats . parentIsContainer {
        parts . push ( "notInParent" . to_string () ); }
      if true_node . viewStats . parentIsContent {
        parts . push ( "containsParent" . to_string () ); }
      if parts . is_empty () { None }
      else { Some ( format! ( "(viewStats {})", parts . join ( " " ))) }}
    fn edit_request ( true_node : & TrueNode ) -> Option < String > {
      true_node . edit_request . as_ref () . map ( | edit_req | {
        let edit_str : String = match edit_req {
          EditRequest::Merge ( id ) => format! ( "(merge {})", id . 0 ),
          EditRequest::Delete => "delete" . to_string () };
        format! ( "(editRequest {})", edit_str ) })}
    fn view_requests ( true_node : & TrueNode ) -> Option < String > {
      if true_node . view_requests . is_empty () { return None; }
      let mut request_strings : Vec < String > =
        true_node . view_requests . iter ()
          . map ( | req | req . to_string () )
          . collect ();
      request_strings . sort ();
      Some ( format! ( "(viewRequests {})", request_strings . join ( " " ))) }
    fn diff_status ( true_node : & TrueNode ) -> Option < String > {
      true_node . diff . as_ref () . map ( | d |
        format! ( "(diff {})", d . repr_in_client () )) }
    let mut parts : Vec < String > =
      vec! [ "node" . to_string () ];
    parts . push ( format! ( "(id {})", true_node . id . 0 ));
    parts . push ( format! ( "(source {})", true_node . source ));
    if true_node . parent_ignores {
      parts . push ( "parentIgnores" . to_string () ); }
    if true_node . indefinitive {
      parts . push ( "indefinitive" . to_string () ); }
    if let Some ( s ) = graph_stats ( true_node )
    { parts . push ( s ); }
    if let Some ( s ) = view_stats ( true_node )
    { parts . push ( s ); }
    if let Some ( s ) = edit_request ( true_node )
    { parts . push ( s ); }
    if let Some ( s ) = view_requests ( true_node )
    { parts . push ( s ); }
    if let Some ( s ) = diff_status ( true_node )
    { parts . push ( s ); }
    format! ( "({})", parts . join ( " " )) }
  let mut parts : Vec < String > = Vec::new ();
  if focused { parts . push ( "focused" . to_string () ); }
  if folded  { parts . push ( "folded" . to_string () ); }
  parts . push ( node_sexp ( true_node ));
  parts . join ( " " ) }

fn org_bullet ( level: usize ) -> String {
  "*" . repeat ( level.max ( 1 )) }
