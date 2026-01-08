use crate::types::orgnode::{OrgNode, orgnodemd_to_string};
use crate::types::orgnode_new::{
    EffectOnParent, NewOrgNode, OrgNodeKind, Scaffold, ScaffoldKind, TrueNode,
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
///
/// TRANSITION NOTE: Converts orgnode to NewOrgNode at render time.
/// This ensures we use new types for rendering while orgnode remains
/// the authoritative source (since it gets modified post-construction).
/// Once all mutations use NewOrgNode, we can switch to reading new_orgnode directly.
pub fn orgnode_forest_to_string (
  forest : &PairTree,
) -> Result < String, Box<dyn Error> > {
  fn render_node_subtree_to_org (
    node_ref : NodeRef < NodePair >,
    level    : usize,
  ) -> String {
    // Use new_orgnode directly - it's the authoritative source
    let new_orgnode : &NewOrgNode =
      node_ref . value () . orgnode_new ();
    let mut out : String =
      new_orgnode_to_text ( level, new_orgnode );
    for child in node_ref . children () {
      out . push_str (
        & render_node_subtree_to_org (
          child,
          level + 1 )); }
    out }
  let root_ref = forest . root ();
  let is_forest_root : bool = {
    let root_new_orgnode : &NewOrgNode =
      root_ref . value () . orgnode_new ();
    matches! (
      & root_new_orgnode . kind,
      OrgNodeKind::Scaff ( Scaffold { kind : ScaffoldKind::ForestRoot } )) };
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
    orgnodemd_to_string ( &orgnode.metadata );
  if ( metadata_str . is_empty() &&
       orgnode.title   . is_empty() ) {
    panic! (
      "orgnode_to_text called with both empty metadata and empty title"
    ); }
  let mut result : String =
    String::new ();
  result . push_str (
    // Leading bullet is mandatory.
    &org_bullet ( level ));
  if ! metadata_str.is_empty () {
    // Maybe add metadata.
    result . push ( ' ' );
    result . push_str ( "(skg " );
    result . push_str ( &metadata_str );
    result . push ( ')' ); }
  if ! orgnode.title.is_empty () {
    // Maybe add title.
    // PITFALL: Title can be missing, for the right metadata.
    result . push ( ' ' );
    result . push_str ( &orgnode.title ); }
  result . push ( '\n' );
  if let Some ( ref body_text ) = orgnode.body {
    // Maybe add body
    if ! body_text . is_empty () {
      result . push_str ( body_text );
      if ! body_text . ends_with ( '\n' ) {
        result . push ( '\n' ); }} }
  result }

fn org_bullet ( level: usize ) -> String {
  "*" . repeat ( level.max ( 1 )) }

//
// New rendering functions for NewOrgNode
//

/// Renders a NewOrgNode as org-mode formatted text.
/// Not recursive -- just stars, metadata, title, and maybe a body.
pub fn new_orgnode_to_text (
  level   : usize,
  orgnode : &NewOrgNode
) -> String {
  let metadata_str : String =
    new_orgnodemd_to_string ( orgnode );
  let title : &str = match &orgnode . kind {
    OrgNodeKind::True  ( t ) => &t . title,
    OrgNodeKind::Scaff ( s ) => s . kind . title (),
  };
  let body : Option < &String > = match &orgnode . kind {
    OrgNodeKind::True  ( t ) => t . body . as_ref (),
    OrgNodeKind::Scaff ( _ ) => None,
  };
  if metadata_str . is_empty () && title . is_empty () {
    panic! (
      "new_orgnode_to_text called with both empty metadata and empty title"
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

/// Renders NewOrgNode metadata as a string suitable for org-mode display.
/// Returns string like "(id abc123) (view ...) (code ...)" etc.
/// Must produce identical output to orgnodemd_to_string for equivalent data.
pub fn new_orgnodemd_to_string (
  orgnode : &NewOrgNode
) -> String {
  match &orgnode . kind {
    OrgNodeKind::Scaff ( scaffold ) =>
      scaffold_metadata_to_string ( orgnode . focused, orgnode . folded, scaffold ),
    OrgNodeKind::True ( true_node ) =>
      true_node_metadata_to_string ( orgnode . focused, orgnode . folded, true_node ),
  } }

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
  code_parts . push ( format! ( "(interp {})", scaffold . kind . interp_str () ));
  if ! code_parts . is_empty () {
    parts . push ( format! ( "(code {})", code_parts . join ( " " ))); }

  parts . join ( " " ) }

/// Render metadata for a TrueNode.
fn true_node_metadata_to_string (
  focused   : bool,
  folded    : bool,
  true_node : &TrueNode
) -> String {
  let mut parts : Vec < String > = Vec::new ();

  if let Some ( ref id ) = true_node . id {
    parts . push ( format! ( "(id {})", id . 0 )); }
  if let Some ( ref source ) = true_node . source {
    parts . push ( format! ( "(source {})", source )); }

  // Build view s-expr
  let mut view_parts : Vec < String > = Vec::new ();
  if true_node . view_data . cycle {
    view_parts . push ( "cycle" . to_string () ); }
  if focused {
    view_parts . push ( "focused" . to_string () ); }
  if folded {
    view_parts . push ( "folded" . to_string () ); }

  // Build rels s-expr
  let mut rel_parts : Vec < String > = Vec::new ();
  if ! true_node . view_data . relationships . parentIsContainer {
    rel_parts . push ( "notInParent" . to_string () ); }
  if true_node . view_data . relationships . parentIsContent {
    rel_parts . push ( "containsParent" . to_string () ); }
  if true_node . view_data . relationships . numContainers != Some ( 1 ) {
    if let Some ( count ) = true_node . view_data . relationships . numContainers {
      rel_parts . push ( format! ( "(containers {})", count )); }}
  if true_node . view_data . relationships . numContents != Some ( 0 ) {
    if let Some ( count ) = true_node . view_data . relationships . numContents {
      rel_parts . push ( format! ( "(contents {})", count )); }}
  if true_node . view_data . relationships . numLinksIn != Some ( 0 ) {
    if let Some ( count ) = true_node . view_data . relationships . numLinksIn {
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

//
// Tests
//

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::misc::ID;
    use crate::types::orgnode::{
        default_metadata, forest_root_orgnode, EditRequest, ViewRequest, Interp,
    };
    use crate::types::orgnode_new::from_old_orgnode;

    /// Test that rendering a converted OrgNode produces identical output.
    fn assert_render_identical ( old : &OrgNode, level : usize ) {
        let old_text = orgnode_to_text ( level, old );
        let new = from_old_orgnode ( old );
        let new_text = new_orgnode_to_text ( level, &new );
        assert_eq! ( old_text, new_text,
            "Rendering mismatch for {:?}", old . metadata . code . interp );
    }

    #[test]
    fn test_render_forest_root () {
        let old = forest_root_orgnode ();
        let new = from_old_orgnode ( &old );
        let old_text = orgnode_to_text ( 1, &old );
        let new_text = new_orgnode_to_text ( 1, &new );
        assert_eq! ( old_text, new_text );
    }

    #[test]
    fn test_render_content_basic () {
        let mut md = default_metadata ();
        md . id = Some ( ID::from ( "abc123" ));
        md . source = Some ( "test.skg" . to_string ());
        let old = OrgNode {
            metadata : md,
            title    : "Test Title" . to_string (),
            body     : Some ( "Body text\n" . to_string ()),
        };
        assert_render_identical ( &old, 2 );
    }

    #[test]
    fn test_render_content_with_view_data () {
        let mut md = default_metadata ();
        md . id = Some ( ID::from ( "view123" ));
        md . viewData . focused = true;
        md . viewData . folded = true;
        md . viewData . cycle = true;
        let old = OrgNode {
            metadata : md,
            title    : "With View Data" . to_string (),
            body     : None,
        };
        assert_render_identical ( &old, 1 );
    }

    #[test]
    fn test_render_content_with_relationships () {
        let mut md = default_metadata ();
        md . id = Some ( ID::from ( "rel123" ));
        md . viewData . relationships . parentIsContainer = false;
        md . viewData . relationships . parentIsContent = true;
        md . viewData . relationships . numContainers = Some ( 3 );
        md . viewData . relationships . numContents = Some ( 5 );
        md . viewData . relationships . numLinksIn = Some ( 2 );
        let old = OrgNode {
            metadata : md,
            title    : "With Relationships" . to_string (),
            body     : None,
        };
        assert_render_identical ( &old, 1 );
    }

    #[test]
    fn test_render_content_indefinitive () {
        let mut md = default_metadata ();
        md . id = Some ( ID::from ( "indef123" ));
        md . code . indefinitive = true;
        let old = OrgNode {
            metadata : md,
            title    : "Indefinitive" . to_string (),
            body     : None,
        };
        assert_render_identical ( &old, 1 );
    }

    #[test]
    fn test_render_content_with_edit_request () {
        let mut md = default_metadata ();
        md . id = Some ( ID::from ( "del123" ));
        md . code . editRequest = Some ( EditRequest::Delete );
        let old = OrgNode {
            metadata : md,
            title    : "To Delete" . to_string (),
            body     : None,
        };
        assert_render_identical ( &old, 1 );
    }

    #[test]
    fn test_render_content_with_view_requests () {
        let mut md = default_metadata ();
        md . id = Some ( ID::from ( "vr123" ));
        md . code . viewRequests . insert ( ViewRequest::Aliases );
        md . code . viewRequests . insert ( ViewRequest::Containerward );
        let old = OrgNode {
            metadata : md,
            title    : "With View Requests" . to_string (),
            body     : None,
        };
        assert_render_identical ( &old, 1 );
    }

    #[test]
    fn test_render_subscribee () {
        let mut md = default_metadata ();
        md . id = Some ( ID::from ( "sub123" ));
        md . code . interp = Interp::Subscribee;
        let old = OrgNode {
            metadata : md,
            title    : "Subscribee Node" . to_string (),
            body     : None,
        };
        assert_render_identical ( &old, 2 );
    }

    #[test]
    fn test_render_parent_ignores () {
        let mut md = default_metadata ();
        md . id = Some ( ID::from ( "pi123" ));
        md . code . interp = Interp::ParentIgnores;
        let old = OrgNode {
            metadata : md,
            title    : "Parent Ignores" . to_string (),
            body     : None,
        };
        assert_render_identical ( &old, 2 );
    }

    #[test]
    fn test_render_hidden_from_subscribees () {
        let mut md = default_metadata ();
        md . id = Some ( ID::from ( "hfs123" ));
        md . code . interp = Interp::HiddenFromSubscribees;
        let old = OrgNode {
            metadata : md,
            title    : "Hidden From Subscribees" . to_string (),
            body     : None,
        };
        assert_render_identical ( &old, 2 );
    }

    #[test]
    fn test_render_alias_col () {
        let mut md = default_metadata ();
        md . code . interp = Interp::AliasCol;
        let old = OrgNode {
            metadata : md,
            title    : "its aliases" . to_string (), // canonical scaffold title
            body     : None,
        };
        assert_render_identical ( &old, 2 );
    }

    #[test]
    fn test_render_alias () {
        let mut md = default_metadata ();
        md . code . interp = Interp::Alias;
        let old = OrgNode {
            metadata : md,
            title    : "My Alias" . to_string (),
            body     : None,
        };
        assert_render_identical ( &old, 3 );
    }

    #[test]
    fn test_render_subscribee_col () {
        let mut md = default_metadata ();
        md . code . interp = Interp::SubscribeeCol;
        let old = OrgNode {
            metadata : md,
            title    : "it subscribes to these" . to_string (), // canonical scaffold title
            body     : None,
        };
        assert_render_identical ( &old, 2 );
    }

    #[test]
    fn test_render_hidden_outside_col () {
        let mut md = default_metadata ();
        md . code . interp = Interp::HiddenOutsideOfSubscribeeCol;
        let old = OrgNode {
            metadata : md,
            title    : "hidden from all subscriptions" . to_string (), // canonical scaffold title
            body     : None,
        };
        assert_render_identical ( &old, 3 );
    }

    #[test]
    fn test_render_hidden_in_col () {
        let mut md = default_metadata ();
        md . code . interp = Interp::HiddenInSubscribeeCol;
        let old = OrgNode {
            metadata : md,
            title    : "hidden from this subscription" . to_string (), // canonical scaffold title
            body     : None,
        };
        assert_render_identical ( &old, 3 );
    }
}
