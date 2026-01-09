use crate::types::orgnode::{
    EffectOnParent, OrgNode, OrgNodeKind, Scaffold, TrueNode,
};
use crate::types::tree::{NodePair, PairTree};

use ego_tree::NodeRef;
use std::error::Error;

/// PURPOSE: Render a "forest" -- a tree with ForestRoot at root
/// -- to org-mode text.
/// ForestRoot is not rendered; its children start at level 1.
///
/// ASSUMES: metadata has already been enriched with relationship data.
/// ERRORS: if root is not a ForestRoot.
pub fn orgnode_forest_to_string (
  forest : &PairTree,
) -> Result < String, Box<dyn Error> > {
  fn render_node_subtree_to_org (
    node_ref : NodeRef < NodePair >,
    level    : usize,
  ) -> String {
    let orgnode : &OrgNode =
      &node_ref . value () .orgnode;
    let mut out : String =
      orgnode_to_text ( level, orgnode );
    for child in node_ref . children () {
      out . push_str (
        & render_node_subtree_to_org (
          child,
          level + 1 )); }
    out }
  let root_ref = forest . root ();
  let is_forest_root : bool = {
    let root_orgnode : &OrgNode =
      &root_ref . value () .orgnode;
    matches! (
      & root_orgnode . kind,
      OrgNodeKind::Scaff ( Scaffold::ForestRoot )) };
  if ! is_forest_root {
    return Err (
      "orgnode_forest_to_string: root is not a ForestRoot".into() ); }
  let mut result : String =
    String::new ();
  for child in root_ref . children () {
    result . push_str (
      & render_node_subtree_to_org ( child, 1 )); }
  Ok ( result ) }

/// Renders an OrgNode as org-mode formatted text.
/// Not recursive -- just stars, metadata, title, and maybe a body.
pub fn orgnode_to_text (
  level   : usize,
  orgnode : &OrgNode
) -> String {
  let metadata_str : String =
    orgnode_to_string ( orgnode );
  let title : &str = match &orgnode . kind {
    OrgNodeKind::True  ( t ) => &t . title,
    OrgNodeKind::Scaff ( s ) => s . title (),
  };
  let body : Option < &String > = match &orgnode . kind {
    OrgNodeKind::True  ( t ) => t . body . as_ref (),
    OrgNodeKind::Scaff ( _ ) => None,
  };
  if metadata_str . is_empty () && title . is_empty () {
    panic! (
      "orgnode_to_text called with both empty metadata and empty title"
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
  result }

pub fn orgnode_to_string (
  orgnode : &OrgNode
) -> String {
  match &orgnode . kind {
    OrgNodeKind::Scaff ( scaffold ) =>
      scaffold_metadata_to_string ( orgnode . focused, orgnode . folded, scaffold ),
    OrgNodeKind::True ( true_node ) =>
      true_node_metadata_to_string ( orgnode . focused, orgnode . folded, true_node ),
  }}

/// Render metadata for a Scaffold.
/// Scaffolds have minimal metadata: just interp, and maybe focused/folded.
fn scaffold_metadata_to_string (
  focused  : bool,
  folded   : bool,
  scaffold : &Scaffold
) -> String {
  let mut parts : Vec < String > = Vec::new ();

  // Build view s-expr (only focused/folded, no relationships)
  let mut view_parts : Vec < String > = Vec::new ();
  if focused { view_parts . push ( "focused" . to_string () ); }
  if folded  { view_parts . push ( "folded" . to_string () ); }
  if ! view_parts . is_empty () {
    parts . push ( format! ( "(view {})", view_parts . join ( " " ))); }

  // Build code s-expr
  let mut code_parts : Vec < String > = Vec::new ();
  // Scaffolds always have non-Content interp
  code_parts . push ( format! ( "(interp {})", scaffold . interp_str () ));
  if ! code_parts . is_empty () {
    parts . push ( format! ( "(code {})", code_parts . join ( " " ))); }

  parts . join ( " " ) }

/// Render metadata for a TrueNode.
fn true_node_metadata_to_string (
  focused   : bool,
  folded    : bool,
  true_node : & TrueNode
) -> String {
  let mut parts : Vec < String > = Vec::new ();

  if let Some ( ref id ) = true_node . id {
    parts . push ( format! ( "(id {})", id . 0 )); }
  if let Some ( ref source ) = true_node . source {
    parts . push ( format! ( "(source {})", source )); }

  // Build view s-expr
  let mut view_parts : Vec < String > = Vec::new ();
  if true_node . cycle {
    view_parts . push ( "cycle" . to_string () ); }
  if focused {
    view_parts . push ( "focused" . to_string () ); }
  if folded {
    view_parts . push ( "folded" . to_string () ); }

  // Build rels s-expr
  let mut rel_parts : Vec < String > = Vec::new ();
  if ! true_node . relationships . parentIsContainer {
    rel_parts . push ( "notInParent" . to_string () ); }
  if true_node . relationships . parentIsContent {
    rel_parts . push ( "containsParent" . to_string () ); }
  if true_node . relationships . numContainers != Some ( 1 ) {
    if let Some ( count ) = true_node . relationships . numContainers {
      rel_parts . push ( format! ( "(containers {})", count )); }}
  if true_node . relationships . numContents != Some ( 0 ) {
    if let Some ( count ) = true_node . relationships . numContents {
      rel_parts . push ( format! ( "(contents {})", count )); }}
  if true_node . relationships . numLinksIn != Some ( 0 ) {
    if let Some ( count ) = true_node . relationships . numLinksIn {
      rel_parts . push ( format! ( "(linksIn {})", count )); }}

  if ! rel_parts . is_empty () {
    view_parts . push ( format! ( "(rels {})", rel_parts . join ( " " ))); }

  if ! view_parts . is_empty () {
    parts . push ( format! ( "(view {})", view_parts . join ( " " ))); }

  // Build code s-expr
  let mut code_parts : Vec < String > = Vec::new ();
  // Only emit interp if not Content
  if true_node . effect_on_parent != EffectOnParent::Content {
    let interp_str = match true_node . effect_on_parent {
      EffectOnParent::Content              => "content",
      EffectOnParent::Subscribee           => "subscribee",
      EffectOnParent::ParentIgnores        => "parentIgnores",
      EffectOnParent::HiddenFromSubscribees => "hiddenFromSubscribees",
    };
    code_parts . push ( format! ( "(interp {})", interp_str )); }
  if true_node . indefinitive {
    code_parts . push ( "indefinitive" . to_string () ); }

  // Handle editRequest
  if let Some ( ref edit_req ) = true_node . edit_request {
    code_parts . push ( edit_req . to_string () ); }

  // Build viewRequests s-expr
  if ! true_node . view_requests . is_empty () {
    let mut request_strings : Vec < String > =
      true_node . view_requests . iter ()
        . map ( | req | req . to_string () )
        . collect ();
    request_strings . sort ();
    code_parts . push ( format! ( "(viewRequests {})", request_strings . join ( " " ))); }

  if ! code_parts . is_empty () {
    parts . push ( format! ( "(code {})", code_parts . join ( " " ))); }

  parts . join ( " " ) }

fn org_bullet ( level: usize ) -> String {
  "*" . repeat ( level.max ( 1 )) }
