use crate::serve::util::sexp_to_client_string;
use crate::types::misc::SkgConfig;
use crate::types::tree::forest::ViewForest;
use crate::types::viewnode::{
  ChangeFacts, DiffObject, ReadinessRequest,
  Viewnode, ViewnodeKind, Vognode, Phantom, Property, UnrestrictedVognode,
  PhantomDeleted, PhantomHistorical, PhantomUnknown, NodeEditRequest,
  AffectsParent,
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
    let viewnode : &Viewnode = node_ref . value ();
    let mut out : String =
      viewnode_to_text_with_folder_facts (
        level, viewnode, folder_change_facts (node_ref), config )?;
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
/// ERRORS: If viewnode is a BufferRoot.
pub fn viewnode_to_text (
  level    : usize,
  viewnode : &Viewnode,
  config   : &SkgConfig,
) -> Result < String, Box<dyn Error> > {
  viewnode_to_text_with_folder_facts (level, viewnode, None, config) }

/// The diff view's facts about a folder, derived from its
/// members when the buffer is rendered: it is drawn because of a
/// change when some member carries one, and ready when every such
/// member's change is ready. Rendered '(KIND changed)' in place of
/// the bare folder atom, then '(ready)'.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct FolderChangeFacts {
  pub ready : bool,
}

/// None unless 'node_ref' is a folder some member of which carries a
/// change: a presence sign, or (on a member) a relRepo move.
pub fn folder_change_facts (
  node_ref : NodeRef<Viewnode>,
) -> Option<FolderChangeFacts> {
  if ! matches! ( &node_ref . value () . kind,
                  ViewnodeKind::PartnerFolder (_)
                  | ViewnodeKind::PropertyFolder (_) ) {
    return None; }
  let member_changes : Vec<bool> = // whether each changed member is ready
    node_ref . children ()
    . filter_map ( |member| {
      let relRepo_move_ready : Option<bool> =
        member . children () . find_map ( |child| match &child . value () . kind {
          ViewnodeKind::Property (Property::RelRepoMove { change_facts, .. }) =>
            Some ( change_facts . ready . is_some () ),
          _ => None, } );
      let presence_ready : Option<bool> = match &member . value () . kind {
        ViewnodeKind::Vognode (Vognode::Unrestricted (t)) =>
          rel_change_ready (&t . change_facts),
        ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Unknown (u))) =>
          single_change_ready (&u . change_facts),
        ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Historical (h))) =>
          rel_change_ready (&h . change_facts),
        ViewnodeKind::Property (
          Property::Alias { change_facts, .. }
          | Property::ID { change_facts, .. }
          | Property::Flag { change_facts, .. }
          | Property::RemovedAlias { change_facts, .. }
          | Property::RemovedID { change_facts, .. } ) =>
          single_change_ready (change_facts),
        _ => None, };
      match (presence_ready, relRepo_move_ready) {
        (None, None) => None,
        (a, b) => Some ( a . unwrap_or (true) && b . unwrap_or (true) ), } } )
    . collect ();
  if member_changes . is_empty () { return None; }
  Some ( FolderChangeFacts {
    ready : member_changes . iter () . all ( |ready| *ready ) } ) }

/// Some (whether ready) when the relationship object changed.
fn rel_change_ready (
  change_facts : &ChangeFacts,
) -> Option<bool> {
  change_facts . presence . rel . map ( |_| change_facts . ready . as_ref ()
    . is_some_and ( |objects| objects . contains (&DiffObject::Rel) )) }

/// Some (whether ready) when a single-object leaf changed.
fn single_change_ready (
  change_facts : &ChangeFacts,
) -> Option<bool> {
  let changed : bool = change_facts . presence . node . is_some ()
    || change_facts . presence . rel . is_some ();
  changed . then ( || change_facts . ready . is_some () ) }

fn viewnode_to_text_with_folder_facts (
  level        : usize,
  viewnode     : &Viewnode,
  folder_facts : Option<FolderChangeFacts>,
  config       : &SkgConfig,
) -> Result < String, Box<dyn Error> > {
  let metadata_str : String =
    viewnode_to_string_with_folder_facts (viewnode, folder_facts, config)?;
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

pub fn viewnode_to_string (
  viewnode : &Viewnode,
  config   : &SkgConfig,
) -> Result < String, Box<dyn Error> > {
  viewnode_to_string_with_folder_facts (viewnode, None, config) }

fn viewnode_to_string_with_folder_facts (
  viewnode     : &Viewnode,
  folder_facts : Option<FolderChangeFacts>,
  config       : &SkgConfig,
) -> Result < String, Box<dyn Error> > {
  match &viewnode . kind {
    ViewnodeKind::PropertyFolder (folder) =>
      Ok ( folder_atom_metadata_to_string (
        viewnode . focused, viewnode . org_folded,
        viewnode . org_body_folded, folder . repr_in_client (), folder_facts )),
    ViewnodeKind::Property (property) =>
      property_metadata_to_string (
        viewnode . focused, viewnode . org_folded,
        viewnode . org_body_folded, property ),
    ViewnodeKind::PartnerFolder (partnerFolder) =>
      Ok ( folder_atom_metadata_to_string (
        viewnode . focused, viewnode . org_folded,
        viewnode . org_body_folded, partnerFolder . repr_in_client (),
        folder_facts )),
    ViewnodeKind::BufferRoot =>
      Err ( "viewnode_to_string: BufferRoot should never be rendered" . into () ),
    ViewnodeKind::DeadViewnode =>
      Ok ( dead_viewnode_metadata_to_string (
        viewnode . focused, viewnode . org_folded,
        viewnode . org_body_folded )),
    ViewnodeKind::Vognode (Vognode::Unrestricted (unrestrictedVognode)) =>
      Ok ( unrestrictedVognode_metadata_to_string (
        viewnode . focused, viewnode . org_folded,
        viewnode . org_body_folded, unrestrictedVognode, config )),
    ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Deleted (deleted_node))) =>
      Ok ( phantomDeleted_metadata_to_string (
        viewnode . focused, viewnode . org_folded,
        viewnode . org_body_folded, deleted_node )),
    ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Unknown (unknown_node))) =>
      Ok ( phantomUnknown_metadata_to_string (
        viewnode . focused, viewnode . org_folded,
        viewnode . org_body_folded, unknown_node )),
    ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Historical (historical))) =>
      Ok ( phantomHistorical_metadata_to_string (
        viewnode . focused, viewnode . org_folded,
        viewnode . org_body_folded, historical )),
    ViewnodeKind::Vognode (Vognode::Restricted (_)) =>
      Ok ( restricted_node_metadata_to_string (
        viewnode . focused, viewnode . org_folded,
        viewnode . org_body_folded )) } }

fn non_vognode_atom_metadata_to_string (
  focused         : bool,
  org_folded      : bool,
  org_body_folded : bool,
  atom            : &str,
) -> String {
  let mut parts : Vec < String > = Vec::new ();
  if focused     { parts . push ( "focused"    . to_string () ); }
  if org_folded  { parts . push ( "folded"     . to_string () ); }
  if org_body_folded { parts . push ( "bodyFolded" . to_string () ); }
  parts . push (atom . to_string ());
  parts . join (" ") }

/// A folder's atom, or, when it is drawn because of a change,
/// '(ATOM changed)' and '(ready)' if that change is ready.
fn folder_atom_metadata_to_string (
  focused         : bool,
  org_folded      : bool,
  org_body_folded : bool,
  atom            : &str,
  folder_facts    : Option<FolderChangeFacts>,
) -> String {
  match folder_facts {
    None => non_vognode_atom_metadata_to_string (
      focused, org_folded, org_body_folded, atom ),
    Some (facts) => {
      let mut metadata : String = non_vognode_atom_metadata_to_string (
        focused, org_folded, org_body_folded, &format! ("({} changed)", atom) );
      if facts . ready { metadata . push_str (" (ready)"); }
      metadata }, } }

fn property_metadata_to_string (
  focused         : bool,
  org_folded      : bool,
  org_body_folded : bool,
  property        : &Property,
) -> Result < String, Box<dyn Error> > {
  let mut parts : Vec < String > = Vec::new ();
  if focused     { parts . push ( "focused"    . to_string () ); }
  if org_folded  { parts . push ( "folded"     . to_string () ); }
  if org_body_folded { parts . push ( "bodyFolded" . to_string () ); }
  match property {
    Property::Alias { relRepo, relRepo_request,
                      change_facts, readiness_request, .. } => {
      parts . push ( "alias" . to_string () );
      if let Some (skgrepo) = relRepo {
        parts . push ( format! (
          "(relRepo {})", metadata_value_atom (skgrepo) ) ); }
      if let Some (skgrepo) = relRepo_request {
        parts . push ( format! (
          "(editRequest (relRepo {}))",
          metadata_value_atom (skgrepo) ) ); }
      parts . extend ( change_fact_forms (change_facts, readiness_request) ); }
    Property::ID { change_facts, readiness_request, .. } => {
      parts . push ( "id" . to_string () );
      parts . extend ( change_fact_forms (change_facts, readiness_request) ); }
    Property::Flag { flag, change_facts, readiness_request, .. } => {
      parts . push ( format! ("(flag {})", flag . wire_name ()) );
      parts . extend ( change_fact_forms (change_facts, readiness_request) ); }
    Property::RemovedAlias { change_facts, readiness_request, .. }
      | Property::RemovedID { change_facts, readiness_request, .. } => {
      parts . push ( property . repr_in_client () . to_string () );
      parts . extend ( change_fact_forms (change_facts, readiness_request) ); }
    Property::OldText { title, body, change_facts, readiness_request } => {
      parts . push ( format! ( "(oldText {})",
        match (title . is_some (), body . is_some ()) {
          (true, true)  => "titleAndBody",
          (true, false) => "title",
          _             => "body", } ));
      parts . extend ( change_fact_forms (change_facts, readiness_request) ); }
    Property::BodyDiff { .. } =>
      parts . push ( "bodyDiff" . to_string () ),
    Property::HomeSkgrepoMove { skgrepo_change, change_facts, readiness_request }
      | Property::RelRepoMove { skgrepo_change, change_facts, readiness_request } => {
      parts . push ( format! ( "({} (old {}) (new {}))",
        property . repr_in_client (),
        metadata_value_atom (&skgrepo_change . old),
        metadata_value_atom (&skgrepo_change . new) ));
      parts . extend ( change_fact_forms (change_facts, readiness_request) ); }
    Property::ChangesetMarker { kind, change_facts, readiness_request } => {
      parts . push ( format! ( "(changesetMarker {})", kind . wire_name () ));
      parts . extend ( change_fact_forms (change_facts, readiness_request) ); }
  }
  Ok ( parts . join (" ")) }

/// The diff view's forms for one viewnode: '(diff ...)',
/// '(ready ...)', '(leaves ...)', and its readiness request. Empty
/// outside the diff view.
fn change_fact_forms (
  facts   : &ChangeFacts,
  request : &Option<ReadinessRequest>,
) -> Vec<String> {
  let form = |head : &str, atoms : Vec<&str>| -> String {
    if atoms . is_empty () { format! ("({})", head) }
    else { format! ("({} {})", head, atoms . join (" ")) } };
  let mut forms : Vec<String> = Vec::new ();
  let presence : Vec<&'static str> = facts . presence . atoms ();
  if ! presence . is_empty () {
    forms . push ( form ("diff", presence) ); }
  if let Some (objects) = &facts . ready {
    forms . push ( form ( "ready",
      objects . iter () . map ( |object| object . wire_name () ) . collect () )); }
  if let Some (leaves) = &facts . leaves {
    let count : String = leaves . count . to_string ();
    forms . push ( form ( "leaves",
      once ( count . as_str () )
      . chain ( leaves . objects . iter () . map ( |object| object . wire_name () ))
      . collect () )); }
  if let Some (request) = request {
    forms . push ( form ( request . form_head (),
      request . objects . iter () . map ( |object| object . wire_name () )
      . collect () )); }
  forms }

/// Render metadata for an UnrestrictedVognode:
///   (skg [focused] [folded] (node ...))
fn unrestrictedVognode_metadata_to_string (
  focused             : bool,
  org_folded          : bool,
  org_body_folded     : bool,
  unrestrictedVognode : & UnrestrictedVognode,
  config              : & SkgConfig,
) -> String {
  fn node_sexp (
    unrestrictedVognode : & UnrestrictedVognode,
    config    : & SkgConfig,
  ) -> String {
    fn rels_herald ( unrestrictedVognode : & UnrestrictedVognode ) -> Option < String > {
      // The semantic (rels ...) sexp, assembled in the viewnodestats
      // pass (server/herald_tokens.rs); emitted verbatim.
      unrestrictedVognode . viewStats . rel_heralds . clone () }
    fn view_stats (
      unrestrictedVognode : & UnrestrictedVognode,
      config    : & SkgConfig,
    ) -> Option < String > {
      let mut parts : Vec < String > = Vec::new ();
      if unrestrictedVognode . viewStats . cycle {
        parts . push ( "cycle" . to_string () ); }
      if let Some (ref original) =
        unrestrictedVognode . viewStats . overridesHere {
        parts . push ( format! ("(overridesHere {})",
                                 original . 0 )); }
      if let Some (ref skgrepo) =
        unrestrictedVognode . viewStats . relRepo {
        parts . push ( format! (
          "(relRepo {})", metadata_value_atom (skgrepo) )); }
      if unrestrictedVognode . viewStats . homeSkgrepoAtBoundary {
        if let Some (src_config)
        = config . skgrepos . get ( &unrestrictedVognode . home_skgrepo )
        { parts . push ( format! (
            "(homeRepoHerald {})",
            metadata_value_atom (
              & format! ("⌂:{}", src_config . herald_label ()) ) ) ); }}
      if let Some (home_skgrepo_move) =
        &unrestrictedVognode . viewStats . home_skgrepo_move {
        parts . push ( format! (
          "(homeSkgrepoMove (old {}) (new {}))",
          metadata_value_atom (&home_skgrepo_move . old),
          metadata_value_atom (&home_skgrepo_move . new) )); }
      if let Some (letter) =
        &unrestrictedVognode . viewStats . moved_like_ancestor {
        parts . push ( format! (
          "(movedLikeAncestor {})", metadata_value_atom (letter) )); }
      if parts . is_empty () { None }
      else { Some ( format! (
               "(viewStats {})", parts . join (" ") )) }}
    fn edit_request ( unrestrictedVognode : & UnrestrictedVognode
                    ) -> Option < String > {
      if let Some (skgrepo) = &unrestrictedVognode . relRepo_request {
        return Some ( format! (
          "(editRequest (relRepo {}))",
          metadata_value_atom (skgrepo) ) ); }
      unrestrictedVognode . edit_request () . map ( | edit_req | {
        let edit_str : String = match edit_req {
          NodeEditRequest::NodeMerge (skgid) => format! ( "(merge {})", skgid . 0 ),
          NodeEditRequest::Delete => "delete" . to_string (),
          NodeEditRequest::SetFlag { flag, value } =>
            format! ( "(flag {} {})",
              flag . wire_name (), value ) };
        format! ( "(editRequest {})", edit_str ) } ) }
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
    parts . push ( format! ( "(id {})", unrestrictedVognode . skgid . 0 ));
    parts . push ( format! (
      "(repo {})", metadata_value_atom (&unrestrictedVognode . home_skgrepo) ));
    // AffectsParent::True is left implicit because it is the default
    // membership relation.
    match unrestrictedVognode . affectsParent {
      AffectsParent::True => {},
      AffectsParent::NA =>
        parts . push ( "(affectsParent na)" . to_string () ),
      AffectsParent::False =>
        parts . push ( "(affectsParent false)" . to_string () ) }
    if unrestrictedVognode . is_writeProtected () {
      // `writeProtected` means "write-protected" -- a view of a node that
      // cannot be edited (see Editability in types/viewnode.rs). The metadata
      // sexp uses only this short form on both emission and parsing.
      parts . push ( "writeProtected" . to_string () );
      if unrestrictedVognode . viewStats . omitted_body {
        // The rendering is hiding a body (herald "B" on the ☮).
        parts . push ( "omittedBody" . to_string () ); }}
    if let Some (s) = rels_herald (unrestrictedVognode)
    { parts . push (s); }
    if let Some (s) = view_stats (unrestrictedVognode, config)
    { parts . push (s); }
    if let Some (s) = edit_request (unrestrictedVognode)
    { parts . push (s); }
    if let Some (s) = view_requests (unrestrictedVognode)
    { parts . push (s); }
    parts . extend ( change_fact_forms (
      &unrestrictedVognode . change_facts,
      &unrestrictedVognode . readiness_request ));
    format! ( "({})", parts . join (" ")) }
  let mut parts : Vec < String > = Vec::new ();
  if focused     { parts . push ( "focused"    . to_string () ); }
  if org_folded  { parts . push ( "folded"     . to_string () ); }
  if org_body_folded { parts . push ( "bodyFolded" . to_string () ); }
  parts . push ( node_sexp (unrestrictedVognode, config));
  parts . join (" ") }

/// Render metadata for a PhantomDeleted:
///   (skg [focused] [folded] (deleted (id X) (repo S)))
fn phantomDeleted_metadata_to_string (
  focused         : bool,
  org_folded      : bool,
  org_body_folded : bool,
  deleted_node    : &PhantomDeleted,
) -> String {
  let mut parts : Vec < String > = Vec::new ();
  if focused     { parts . push ( "focused"    . to_string () ); }
  if org_folded  { parts . push ( "folded"     . to_string () ); }
  if org_body_folded { parts . push ( "bodyFolded" . to_string () ); }
  let deleted_parts : Vec<String> =
    [ format! ("(id {})", deleted_node . skgid . 0),
      format! ("(repo {})",
               metadata_value_atom (&deleted_node . home_skgrepo)) ]
    . into_iter ()
    . chain ( change_fact_forms (
      &deleted_node . change_facts, &deleted_node . readiness_request ))
    . collect ();
  parts . push ( format! ( "(deleted {})", deleted_parts . join (" ") ));
  parts . join (" ") }

/// Render metadata for a PhantomHistorical:
///   (skg [focused] [folded] (historical (id X) (repo S) ...))
fn phantomHistorical_metadata_to_string (
  focused         : bool,
  org_folded      : bool,
  org_body_folded : bool,
  historical      : &PhantomHistorical,
) -> String {
  let mut parts : Vec < String > = Vec::new ();
  if focused     { parts . push ( "focused"    . to_string () ); }
  if org_folded  { parts . push ( "folded"     . to_string () ); }
  if org_body_folded { parts . push ( "bodyFolded" . to_string () ); }
  let historical_parts : Vec<String> =
    [ format! ("(id {})", historical . skgid . 0),
      format! ("(repo {})",
               metadata_value_atom (&historical . home_skgrepo)) ]
    . into_iter ()
    . chain ( change_fact_forms (
      &historical . change_facts, &historical . readiness_request ))
    . collect ();
  parts . push ( format! ( "(historical {})", historical_parts . join (" ") ));
  parts . join (" ") }

/// Render metadata for a PhantomUnknown:
///   (skg [focused] [folded] (unknown (id X)))
/// Triggered when a referenced ID resolved to nothing in any db.
fn phantomUnknown_metadata_to_string (
  focused         : bool,
  org_folded      : bool,
  org_body_folded : bool,
  unknown_node    : &PhantomUnknown,
) -> String {
  let mut parts : Vec < String > = Vec::new ();
  if focused     { parts . push ( "focused"    . to_string () ); }
  if org_folded  { parts . push ( "folded"     . to_string () ); }
  if org_body_folded { parts . push ( "bodyFolded" . to_string () ); }
  let mut unknown_parts : Vec<String> = vec! [
    format! ("(id {})", unknown_node . skgid . 0) ];
  if let Some (skgrepo) = &unknown_node . relRepo {
    unknown_parts . push ( format! (
      "(viewStats (relRepo {}))", metadata_value_atom (skgrepo)) ); }
  if let Some (skgrepo) = &unknown_node . relRepo_request {
    unknown_parts . push ( format! (
      "(editRequest (relRepo {}))", metadata_value_atom (skgrepo)) ); }
  unknown_parts . extend ( change_fact_forms (
    &unknown_node . change_facts, &unknown_node . readiness_request ));
  parts . push ( format! ( "(unknown {})", unknown_parts . join (" ") ) );
  parts . join (" ") }

/// Render a restricted vognode as the bare atom 'restrictedNode',
/// like the other dataless non-vognode atoms (aliasFolder, subscribeeFolder,
/// ...). It carries no id/repo/etc. -- those describe content the
/// user hid by restricting the skgrepo-set, so emitting them would leak
/// (see RestrictedVognode).
fn restricted_node_metadata_to_string (
  focused         : bool,
  org_folded      : bool,
  org_body_folded : bool,
) -> String {
  let mut parts : Vec < String > = Vec::new ();
  if focused     { parts . push ( "focused"    . to_string () ); }
  if org_folded  { parts . push ( "folded"     . to_string () ); }
  if org_body_folded { parts . push ( "bodyFolded" . to_string () ); }
  parts . push ( "restrictedNode" . to_string () );
  parts . join (" ") }

/// Render metadata for a DeadViewnode:
///   (skg [focused] [folded] deadViewnode)
fn dead_viewnode_metadata_to_string (
  focused         : bool,
  org_folded      : bool,
  org_body_folded : bool,
) -> String {
  let mut parts : Vec < String > = Vec::new ();
  if focused     { parts . push ( "focused"    . to_string () ); }
  if org_folded  { parts . push ( "folded"     . to_string () ); }
  if org_body_folded { parts . push ( "bodyFolded" . to_string () ); }
  parts . push ( "deadViewnode" . to_string () );
  parts . join (" ") }

fn org_bullet ( level: usize ) -> String {
  "*" . repeat ( level . max (1)) }
