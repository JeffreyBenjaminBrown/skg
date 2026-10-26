use crate::refresh::ancestry::parent_reads_claims;
use crate::serve::util::sexp_to_client_string;
use crate::types::misc::{SkgConfig, SkgrepoName};
use crate::types::tree::forest::ViewForest;
use crate::types::viewnode::{
  ChangeFacts, FoldAndFocus, ReadinessRequest,
  Viewnode, ViewnodeKind, Vognode, PropertyMember, ChangeViewnode, UnrestrictedVognode,
  Ghost, Unknown, NodeEditRequest,
  RelationshipEditRequest,
  Warrant,
};

use ego_tree::{NodeRef, Tree};
use sexp::{Atom, Sexp};
use std::error::Error;
use std::iter::once;

/// Render a metadata value as one S-expression atom. Most IDs and skgrepo
/// names print in the familiar bare form; values containing whitespace or
/// other S-expression syntax are quoted and escaped by the `sexp` crate.
pub(crate) fn metadata_value_atom (
  value : &str,
) -> String {
  sexp_to_client_string ( &Sexp::Atom ( Atom::S (value . to_string ()) )) }

/// PURPOSE: Render a view forest to org-mode text.
/// Each forest root starts at level 1.
///
/// ASSUMES: metadata has already been enriched with relationship data.
pub trait ViewForestRenderRoots {
  fn render_roots<'a> (
    &'a self,
  ) -> Result<Box<dyn Iterator<Item = NodeRef<'a, Viewnode>> + 'a>,
              Box<dyn Error>>;
}

impl ViewForestRenderRoots for ViewForest {
  fn render_roots<'a> (
    &'a self,
  ) -> Result<Box<dyn Iterator<Item = NodeRef<'a, Viewnode>> + 'a>,
              Box<dyn Error>> {
    Ok (Box::new (self . roots ())) }
}

impl ViewForestRenderRoots for Tree<Viewnode> {
  fn render_roots<'a> (
    &'a self,
  ) -> Result<Box<dyn Iterator<Item = NodeRef<'a, Viewnode>> + 'a>,
              Box<dyn Error>> {
    let root_ref : NodeRef<Viewnode> = self . root ();
    let is_viewforest_root : bool =
      matches! (
        & root_ref . value () . kind,
        ViewnodeKind::BufferRoot);
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
  viewforest : &Tree<Viewnode>,
  config : &SkgConfig,
) -> Result < String, Box<dyn Error> > {
  let root_ref : NodeRef<Viewnode> = viewforest . root ();
  let is_viewforest_root : bool =
    matches! (
      & root_ref . value () . kind,
      ViewnodeKind::BufferRoot);
  if ! is_viewforest_root {
    return Err (
      "viewforest_tree_to_string: root is not a BufferRoot" . into() ); }
  render_view_roots_to_string (
    root_ref . children (), config ) }

fn render_view_roots_to_string <'a> (
  roots  : impl Iterator<Item = NodeRef<'a, Viewnode>>,
  config : &SkgConfig,
) -> Result < String, Box<dyn Error> > {
  fn render_node_subtree_to_org (
    node_ref : NodeRef < Viewnode >,
    level    : usize,
    config   : &SkgConfig,
  ) -> Result < String, Box<dyn Error> > {
    let claims_are_read : bool = // under any other parent no child claims
      node_ref . parent () . is_some_and ( |parent|
        parent_reads_claims ( node_ref . tree (), parent . id () ));
    let mut out : String =
      viewnode_to_text_where_it_stands (
        level, node_ref . value (), claims_are_read )?;
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

/// Renders an Viewnode as org-mode formatted text.
/// Not recursive -- just stars, metadata, title, and maybe a body.
/// Standing alone, it is read as standing where its claim is read.
/// ERRORS: If viewnode is a BufferRoot.
pub fn viewnode_to_text (
  level    : usize,
  viewnode : &Viewnode,
  _config  : &SkgConfig,
) -> Result < String, Box<dyn Error> > {
  viewnode_to_text_where_it_stands (level, viewnode, true) }

/// 'viewnode_to_text' for a viewnode whose viewparent reads its
/// claim ('parent_reads_claims') when 'claims_are_read'. Elsewhere it
/// carries no 'noClaim': no child there claims, whatever it carries.
fn viewnode_to_text_where_it_stands (
  level           : usize,
  viewnode        : &Viewnode,
  claims_are_read : bool,
) -> Result < String, Box<dyn Error> > {
  let metadata_str : String =
    viewnode_to_string_where_it_stands (viewnode, claims_are_read)?;
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
      result . push_str (&crate::body_view_escape::encode_body_for_view (body_text));
      if ! body_text . ends_with ('\n') {
        result . push ('\n'); }} }
  Ok (result) }

/// The parts of a viewnode's '(skg ...)' wrapper, its viewnode ID
/// first:
///   (vnid X) [focused] [folded] [bodyFolded] KIND DECLARED...
///            [(view FACT...)] [(diff FACT...)]
/// every part standing at the top level, the derived facts gathered
/// in '(view ...)' and '(diff ...)'.
/// Standing alone, the viewnode is read as standing where its claim is
/// read.
pub fn viewnode_to_string (
  viewnode : &Viewnode,
  _config  : &SkgConfig,
) -> Result < String, Box<dyn Error> > {
  viewnode_to_string_where_it_stands (viewnode, true) }

/// 'viewnode_to_string' for a viewnode whose claim is read where it
/// stands when 'claims_are_read'; otherwise it carries no 'noClaim'.
fn viewnode_to_string_where_it_stands (
  viewnode        : &Viewnode,
  claims_are_read : bool,
) -> Result < String, Box<dyn Error> > {
  let parts_but_the_viewnode_id : String =
    viewnode_to_string_without_viewnode_id (viewnode, claims_are_read) ?;
  Ok ( match &viewnode . viewnode_id {
    None      => parts_but_the_viewnode_id,
    Some (viewnode_id) => format! ("(vnid {}) {}", viewnode_id, parts_but_the_viewnode_id), } ) }

fn viewnode_to_string_without_viewnode_id (
  viewnode        : &Viewnode,
  claims_are_read : bool,
) -> Result < String, Box<dyn Error> > {
  let facts   : &ChangeFacts = &viewnode . change_facts;
  let request : &Option<ReadinessRequest> = &viewnode . readiness_request;
  match &viewnode . kind {
    ViewnodeKind::Folder (folder) =>
      Ok ( non_vognode_atom_metadata_to_string (
        viewnode . foldAndFocus, folder . repr_in_client () )),
    ViewnodeKind::PropertyMember (member) =>
      Ok ( propertyMember_metadata_to_string (
        viewnode . foldAndFocus, member, claims_are_read,
        viewnode . is_ghostWarranted (), facts, request )),
    ViewnodeKind::ChangeViewnode (change) =>
      Ok ( changeViewnode_metadata_to_string (
        viewnode . foldAndFocus, change, facts, request )),
    ViewnodeKind::BufferRoot =>
      Err ( "viewnode_to_string: BufferRoot should never be rendered" . into () ),
    ViewnodeKind::Tombstone =>
      Ok ( tombstone_metadata_to_string (
        viewnode . foldAndFocus )),
    ViewnodeKind::Vognode (Vognode::Unrestricted (unrestrictedVognode)) =>
      Ok ( unrestrictedVognode_metadata_to_string (
        viewnode . foldAndFocus, unrestrictedVognode, claims_are_read, facts, request )),
    ViewnodeKind::Vognode (Vognode::Unknown (unknown_node)) =>
      Ok ( phantomUnknown_metadata_to_string (
        viewnode . foldAndFocus, unknown_node, claims_are_read, facts, request )),
    ViewnodeKind::Vognode (Vognode::Ghost (ghost)) =>
      Ok ( ghost_metadata_to_string (
        viewnode . foldAndFocus, ghost, claims_are_read, facts, request )), } }

/// The position atoms a headline carries, in their order on the wire.
fn foldAndFocus_atoms (
  foldAndFocus : FoldAndFocus,
) -> Vec < String > {
  [ (foldAndFocus . focused,    "focused"),
    (foldAndFocus . folded,     "folded"),
    (foldAndFocus . bodyFolded, "bodyFolded") ]
  . into_iter ()
  . filter ( |(carried, _)| *carried )
  . map ( |(_, atom)| atom . to_string () )
  . collect () }

fn non_vognode_atom_metadata_to_string (
  foldAndFocus    : FoldAndFocus,
  atom            : &str,
) -> String {
  let mut parts : Vec < String > = foldAndFocus_atoms (foldAndFocus);
  parts . push (atom . to_string ());
  parts . join (" ") }

fn propertyMember_metadata_to_string (
  foldAndFocus    : FoldAndFocus,
  member          : &PropertyMember,
  claims_are_read : bool,
  ghostWarranted  : bool,
  facts           : &ChangeFacts,
  request         : &Option<ReadinessRequest>,
) -> String {
  let mut parts : Vec < String > = foldAndFocus_atoms (foldAndFocus);
  let isWarranted : Warrant = match member {
    PropertyMember::Alias { isWarranted, .. }
      | PropertyMember::ID { isWarranted, .. }
      | PropertyMember::Flag { isWarranted, .. } => *isWarranted, };
  let warrant_atom : Option<String> = // a ghost warrant first: HEAD warrants it
    if ghostWarranted || isWarranted == Warrant::ByHead {
      Some ( "ghostWarrant" . to_string () ) }
    else if isWarranted == Warrant::No { Some ( "noWarrant" . to_string () ) }
    else { None };
  match member {
    PropertyMember::Alias { relRepo, relationshipEditRequest,
                            claimsMembership, .. } => {
      parts . push ( "alias" . to_string () );
      parts . extend ( skgRepos_form ( None, relRepo . as_ref () ));
      parts . extend ( noClaim_atom (*claimsMembership, claims_are_read) );
      parts . extend ( editRequest_form (
        relationshipEditRequest . as_ref () . map (relationshipEditRequest_element),
        request )); }
    PropertyMember::ID { claimsMembership, .. } => {
      parts . push ( "id" . to_string () );
      parts . extend ( noClaim_atom (*claimsMembership, claims_are_read) );
      parts . extend ( editRequest_form ( None, request )); }
    PropertyMember::Flag { flag, .. } => {
      parts . push ( format! ("(flag {})", flag . wire_name ()) );
      parts . extend ( editRequest_form ( None, request )); } }
  parts . extend ( view_form ( warrant_atom . into_iter () . collect () ));
  parts . extend ( diff_form (facts) );
  parts . join (" ") }

fn changeViewnode_metadata_to_string (
  foldAndFocus    : FoldAndFocus,
  change          : &ChangeViewnode,
  facts           : &ChangeFacts,
  request         : &Option<ReadinessRequest>,
) -> String {
  let mut parts : Vec < String > = foldAndFocus_atoms (foldAndFocus);
  match change {
    ChangeViewnode::OldText { title, body } =>
      parts . push ( format! ( "(oldText {})",
        match (title . is_some (), body . is_some ()) {
          (true, true)  => "titleAndBody",
          (true, false) => "title",
          _             => "body", } )),
    ChangeViewnode::BodyDiff { .. } => {
      parts . push ( "bodyDiff" . to_string () );
      return parts . join (" "); }
    ChangeViewnode::HomeSkgrepoMove { skgrepo_change }
      | ChangeViewnode::RelRepoMove { skgrepo_change } =>
      parts . push ( format! ( "({} (old {}) (new {}))",
        change . repr_in_client (),
        metadata_value_atom (&skgrepo_change . old),
        metadata_value_atom (&skgrepo_change . new) )),
    ChangeViewnode::ChangesetMarker { kind } =>
      parts . push ( format! ( "(changesetMarker {})", kind . wire_name () )), }
  parts . extend ( editRequest_form ( None, request ));
  parts . extend ( diff_form (facts) );
  parts . join (" ") }

/// '(skgRepos [(nodeRepo R)] [(relRepo R)])': the home skgrepo of the
/// node a headline stands for, and the skgrepo recording its
/// relationship to its viewparent, which is written only when it
/// differs from the default ('default_relRepo'). None when both are
/// absent.
pub(crate) fn skgRepos_form (
  nodeRepo : Option<&SkgrepoName>,
  relRepo  : Option<&SkgrepoName>,
) -> Option<String> {
  let repos : Vec<String> =
    nodeRepo . map ( |skgrepo| format! (
      "(nodeRepo {})", metadata_value_atom (skgrepo) ))
    . into_iter ()
    . chain ( relRepo . map ( |skgrepo| format! (
      "(relRepo {})", metadata_value_atom (skgrepo) )) )
    . collect ();
  if repos . is_empty () { None }
  else { Some ( format! ( "(skgRepos {})", repos . join (" ") )) } }

/// 'noClaim' for a child that claims no membership where its claim is
/// read; a claimant, and any child where no claim is read, carries
/// nothing.
fn noClaim_atom (
  claimsMembership : bool,
  claims_are_read  : bool,
) -> Option<String> {
  ( claims_are_read && ! claimsMembership ) . then ( || "noClaim" . to_string () ) }

/// '(relRepo R)' or 'restoreRel', inside '(editRequest ...)'.
fn relationshipEditRequest_element (
  request : &RelationshipEditRequest,
) -> String {
  match request {
    RelationshipEditRequest::RelRepo (skgrepo) => format! (
      "(relRepo {})", metadata_value_atom (skgrepo) ),
    RelationshipEditRequest::RestoreRel =>
      "restoreRel" . to_string (), } }

/// '(editRequest [REQUEST] [(ready OBJECT...) | (unready OBJECT...)])':
/// the headline's one edit request, if any, then its readiness
/// request, if any. None when it has neither.
fn editRequest_form (
  edit_request : Option<String>,
  readiness    : &Option<ReadinessRequest>,
) -> Option<String> {
  let elements : Vec<String> =
    edit_request . into_iter ()
    . chain ( readiness . as_ref () . map ( |request| sexp_form (
      request . form_head (),
      request . objects . iter () . map ( |object| object . wire_name () )
      . collect () )))
    . collect ();
  ( ! elements . is_empty () ) . then ( || format! (
    "(editRequest {})", elements . join (" ") )) }

/// '(HEAD ATOM...)', or '(HEAD)' when there are no atoms.
fn sexp_form (
  head  : &str,
  atoms : Vec<&str>,
) -> String {
  if atoms . is_empty () { format! ("({})", head) }
  else { format! ("({} {})", head, atoms . join (" ")) } }

/// '(view FACT...)', the derived facts of a headline's position, or
/// None when there are none: '(view)' is never written.
fn view_form (
  facts : Vec<String>,
) -> Option<String> {
  ( ! facts . is_empty () ) . then ( || format! (
    "(view {})", facts . join (" ") )) }

/// The diff view's change facts for a viewnode, '(diff (node
/// added|removed) (rel added|removed) (properties changed) (leaves N
/// OBJECT...) (ready OBJECT...) (homeSkgrepoMove (old R) (new R))
/// (movedLikeAncestor X))', each part present only when it has
/// something to say (the first three only when true). None outside
/// the diff view, and wherever no part has: '(diff)' is never
/// written.
fn diff_form (
  facts : &ChangeFacts,
) -> Option<String> {
  let mut parts : Vec<String> = Vec::new ();
  parts . extend ( facts . change . forms () );
  if let Some (leaves) = &facts . leaves {
    let count : String = leaves . count . to_string ();
    parts . push ( sexp_form ( "leaves",
      once ( count . as_str () )
      . chain ( leaves . objects . iter () . map ( |object| object . wire_name () ))
      . collect () )); }
  if let Some (objects) = &facts . ready {
    parts . push ( sexp_form ( "ready",
      objects . iter () . map ( |object| object . wire_name () ) . collect () )); }
  if let Some (home_skgrepo_move) = &facts . home_skgrepo_move {
    parts . push ( format! (
      "(homeSkgrepoMove (old {}) (new {}))",
      metadata_value_atom (&home_skgrepo_move . old),
      metadata_value_atom (&home_skgrepo_move . new) )); }
  if let Some (letter) = &facts . moved_like_ancestor {
    parts . push ( format! (
      "(movedLikeAncestor {})", metadata_value_atom (letter) )); }
  ( ! parts . is_empty () ) . then ( || format! (
    "(diff {})", parts . join (" ") )) }

/// Render metadata for an UnrestrictedVognode:
///   [focused] [folded] [bodyFolded] node (gnid X)
///   (skgRepos (nodeRepo R) [(relRepo R)]) [noClaim] [writeProtected]
///   [(overridesHere X)] [(editRequest ...)] [(viewRequests ...)]
///   [(view [omittedBody] [(rels ...)] [(props ...)] [(birth ...)]
///          [noWarrant] [cycle] [repoBoundary])]
///   [(diff ...)]
fn unrestrictedVognode_metadata_to_string (
  foldAndFocus    : FoldAndFocus,
  unrestrictedVognode : & UnrestrictedVognode,
  claims_are_read     : bool,
  facts               : & ChangeFacts,
  request             : & Option<ReadinessRequest>,
) -> String {
  fn node_parts (
    unrestrictedVognode : & UnrestrictedVognode,
    claims_are_read     : bool,
    facts     : & ChangeFacts,
    request   : & Option<ReadinessRequest>,
  ) -> Vec < String > {
    fn view_facts (
      unrestrictedVognode : & UnrestrictedVognode,
    ) -> Vec < String > {
      let stats = &unrestrictedVognode . viewStats;
      let atoms = |atoms : Vec<(bool, &str)>| -> Vec<String> {
        atoms . into_iter ()
          . filter ( |(carried, _)| *carried )
          . map ( |(_, atom)| atom . to_string () )
          . collect () };
      [ // The rendering is hiding a body (herald "B" on the ☮).
        atoms ( vec! [ ( unrestrictedVognode . is_writeProtected ()
                         && stats . omitted_body, "omittedBody" ) ] ),
        // The semantic (rels ...), (props ...) and (birth ...) sexps,
        // assembled in the viewnodestats pass
        // (server/herald_tokens.rs); emitted verbatim.
        stats . rel_heralds . forms () . cloned () . collect (),
        atoms ( vec! [ ( stats . isWarranted == Warrant::ByHead, "ghostWarrant" ),
                       ( stats . isWarranted == Warrant::No, "noWarrant" ),
                       ( stats . cycle,                       "cycle" ),
                       ( stats . repoBoundary,                "repoBoundary" ) ] ) ]
        . concat () }
    fn edit_request ( unrestrictedVognode : & UnrestrictedVognode
                    ) -> Option < String > {
      if let Some (request) = &unrestrictedVognode . relationshipEditRequest {
        return Some ( relationshipEditRequest_element (request) ); }
      unrestrictedVognode . edit_request () . map ( | edit_req | {
        match edit_req {
          NodeEditRequest::NodeMerge (skgid) => format! ( "(merge {})", skgid . 0 ),
          NodeEditRequest::Delete => "delete" . to_string (),
          NodeEditRequest::SetFlag { flag, value } =>
            format! ( "(flag {} {})",
              flag . wire_name (), value ),
          // Parsed into fields outside 'Editability', never kept here.
          NodeEditRequest::RestoreNode => "restoreNode" . to_string (),
          NodeEditRequest::RestoreRel  => "restoreRel" . to_string () } } ) }
    fn view_requests ( unrestrictedVognode : & UnrestrictedVognode
                     ) -> Option < String > {
      if unrestrictedVognode . view_requests . is_empty () { return None; }
      let mut request_strings : Vec < String > =
        unrestrictedVognode . view_requests . iter ()
          . map ( | req | req . to_string () )
          . collect ();
      request_strings . sort ();
      Some ( format! ( "(viewRequests {})",
                       request_strings . join (" ") )) }
    let mut parts : Vec < String > =
      vec! [ "node" . to_string () ];
    parts . push ( format! ( "(gnid {})", unrestrictedVognode . skgid . 0 ));
    parts . extend ( skgRepos_form (
      Some (&unrestrictedVognode . home_skgrepo),
      unrestrictedVognode . viewStats . relRepo . as_ref () ));
    parts . extend ( noClaim_atom (unrestrictedVognode . claimsMembership, claims_are_read) );
    if unrestrictedVognode . is_writeProtected () {
      // `writeProtected` means "write-protected" -- a view of a node that
      // cannot be edited (see Editability in types/viewnode.rs). The metadata
      // sexp uses only this short form on both emission and parsing.
      parts . push ( "writeProtected" . to_string () ); }
    if let Some (ref original) =
      unrestrictedVognode . overridesHere {
      parts . push ( format! ("(overridesHere {})",
                               original . 0 )); }
    parts . extend ( editRequest_form (
      edit_request (unrestrictedVognode), request ));
    parts . extend ( view_requests (unrestrictedVognode) );
    parts . extend ( view_form ( view_facts (unrestrictedVognode) ));
    parts . extend ( diff_form (facts) );
    parts }
  let mut parts : Vec < String > = foldAndFocus_atoms (foldAndFocus);
  parts . extend ( node_parts (unrestrictedVognode, claims_are_read, facts, request));
  parts . join (" ") }

/// Render metadata for a ghost:
///   [focused] [folded] [bodyFolded] ghost (gnid X)
///   (skgRepos (nodeRepo R)) [noClaim]
///   [(editRequest [restoreNode] [READINESS])]
///   [(viewRequests tombstone)] [(view noWarrant)] [(diff ...)]
fn ghost_metadata_to_string (
  foldAndFocus    : FoldAndFocus,
  ghost           : &Ghost,
  claims_are_read : bool,
  facts           : &ChangeFacts,
  request         : &Option<ReadinessRequest>,
) -> String {
  let mut parts : Vec < String > = foldAndFocus_atoms (foldAndFocus);
  parts . extend (
    [ "ghost" . to_string (),
      format! ("(gnid {})", ghost . skgid . 0) ]
    . into_iter ()
    . chain ( skgRepos_form ( Some (&ghost . home_skgrepo), None ))
    . chain ( noClaim_atom (ghost . claimsMembership, claims_are_read) )
    . chain ( editRequest_form (
      ghost . restoreNode_request . then ( || "restoreNode" . to_string () ),
      request ))
    . chain ( ghost . tombstone_request . then (
      || "(viewRequests tombstone)" . to_string () ))
    . chain ( view_form (
      ( ghost . isWarranted == Warrant::No ) . then ( || "noWarrant" . to_string () )
      . into_iter () . collect () ))
    . chain ( diff_form (facts) ) );
  parts . join (" ") }

/// Render metadata for an unknown:
///   [focused] [folded] [bodyFolded] unknown (gnid X)
///   [(skgRepos (relRepo R))] [noClaim]
///   [(editRequest [(relRepo R)] [READINESS])] [(diff ...)]
/// Triggered when a referenced ID resolved to nothing in any db.
fn phantomUnknown_metadata_to_string (
  foldAndFocus    : FoldAndFocus,
  unknown_node    : &Unknown,
  claims_are_read : bool,
  facts           : &ChangeFacts,
  request         : &Option<ReadinessRequest>,
) -> String {
  let mut parts : Vec < String > = foldAndFocus_atoms (foldAndFocus);
  parts . push ( "unknown" . to_string () );
  parts . push ( format! ("(gnid {})", unknown_node . skgid . 0) );
  parts . extend ( skgRepos_form ( None, unknown_node . relRepo . as_ref () ));
  parts . extend ( noClaim_atom (unknown_node . claimsMembership, claims_are_read) );
  parts . extend ( editRequest_form (
    unknown_node . relationshipEditRequest . as_ref ()
      . map (relationshipEditRequest_element),
    request ));
  parts . extend ( diff_form (facts) );
  parts . join (" ") }

/// Render metadata for a tombstone:
///   (skg [focused] [folded] tombstone)
fn tombstone_metadata_to_string (
  foldAndFocus    : FoldAndFocus,
) -> String {
  let mut parts : Vec < String > = foldAndFocus_atoms (foldAndFocus);
  parts . push ( "tombstone" . to_string () );
  parts . join (" ") }

fn org_bullet ( level: usize ) -> String {
  "*" . repeat ( level . max (1)) }
