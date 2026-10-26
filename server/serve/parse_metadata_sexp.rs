/// PURPOSE: Parse (skg ...) metadata s-expressions from org headlines.
///
/// Format ('METADATA_GRAMMAR'): one shape for every headline,
///   (skg (vnid X) [focused] [folded] [bodyFolded] KIND DECLARED...
///        [(view FACT...)] [(diff FACT...)])
/// KIND a bare atom, or for a few kinds a small form naming its
/// parameter, and every part beside it at the top level, in any order.
/// '(view ...)' and '(diff ...)' hold the derived facts the server
/// writes, which nobody reads back: the parser skips each whole.
/// The kinds and the declared parts each takes ('parts_of_kind'):
///   node [(gnid ID)] [(skgRepos [(nodeRepo REPO)] [(relRepo REPO)])]
///        [noClaim]          ; the occurrence claims no membership
///        [writeProtected]   ; marks the occurrence write-protected
///        [(overridesHere ID)]
///        [(editRequest [<delete | (merge ID) | (relRepo REPO)
///                       | (flag FLAG true|false) | restoreRel>]
///                      [(ready OBJECT...) | (unready OBJECT...)])]
///        [(viewRequests REQUEST...)]
///     where OBJECT is node, rel or all. A headline naming no kind is
///     a node.
///   ghost (gnid ID) (skgRepos (nodeRepo REPO))
///         [(editRequest [restoreNode] [READINESS])]
///         [(viewRequests tombstone)]
///   unknown (gnid ID) [(skgRepos (relRepo REPO))] [noClaim]
///           [(editRequest [(relRepo REPO)] [READINESS])]
///   alias [(skgRepos (relRepo REPO))] [noClaim]
///         [(editRequest [(relRepo REPO)] [READINESS])]
///   id [noClaim] [(editRequest READINESS)],
///   (flag hadId | wasOverloaded | noSearchMatching) [(editRequest READINESS)],
///   and the diff view's own viewnodes
///     (oldText titleAndBody | title | body)
///     (homeSkgrepoMove (old REPO) (new REPO))
///     (relRepoMove (old REPO) (new REPO))
///     (changesetMarker content | relRepo | orderOnly)
///   each of which may carry '(editRequest READINESS)';
///   bodyDiff, tombstone, forestRoot and the folders' bare atoms,
///   which carry nothing.
/// READINESS is '(ready OBJECT...)' or '(unready OBJECT...)'.
/// A node, an alias, an id and a flag may carry '(view ...)'; every
/// kind that takes an editRequest may carry '(diff ...)'.
/// 'restoreRel' asks to restore a removed membership; where it may
/// stand the save checks ('restoreRel_stands_where_it_may', local.rs),
/// and the save drops one that names no relationship HEAD records. It
/// may sit beside 'writeProtected'.
/// The viewnode ID, '(vnid X)', X being one or more lowercase letters
/// and digits, comes first when written.
/// A retired spelling ('(node ...)', '(ghost ...)' or '(unknown ...)' as
/// a form, '(deleted ...)', '(historical ...)', '(id X)', '(repo R)', a
/// top-level '(relRepo R)', '(FOLDER changed)', '(rels ...)',
/// '(viewStats ...)', 'omittedBody' or '(homeRepoHerald ...)' outside
/// '(view ...)', '(readyRequest ...)', '(unreadyRequest ...)',
/// 'nonMember', 'restrictedNode') is refused with a message naming the
/// grammar.

use crate::types::sexp::atom_to_string;
use crate::types::misc::{ID, SkgrepoChange, SkgrepoName};
use crate::types::errors::{BufferValidationError, HeadlineName, with_org_subheadlines};
use crate::types::nodes::complete::Flag;
use crate::types::viewnode::{
  ChangeFacts, DiffObject, FoldAndFocus, ReadinessRequest,
  GraphnodeStats, ViewnodeStats, NodeEditRequest, ViewRequest, FolderRelation,
  ChangesetMarkerKind, PropertyMember, ChangeViewnode, Folder,
  Ghost, Unknown,
  Editability, RelationshipEditRequest, ViewnodeId, Warrant,
};
use crate::types::viewnode::{
    Viewnode, ViewnodeKind, UnrestrictedVognode,
    Vognode, viewnode_of,
};

use sexp::Sexp;
use std::collections::{BTreeSet, HashSet};
use uuid::Uuid;

//
// Parsing-internal types
//

/// Intermediate parsed metadata. Converted to Viewnode via viewnode_from_metadata().
/// It holds what the headline says, so an occurrence's gnid and skgrepo
/// may be absent; the parse completes them ('org_to_viewforest').
#[derive(Debug, Clone, PartialEq)]
pub struct ViewnodeMetadata {
  pub foldAndFocus: FoldAndFocus,
  pub viewnode_id: Option<ViewnodeId>,
  // None means vognode, Some means non-vognode.
  pub non_vognode: Option<ViewnodeKind>,
  // UnrestrictedVognode fields (ignored if non-vognode is Some)
  pub skgid: Option<ID>,
  pub home_skgrepo: Option<SkgrepoName>,
  // False when the headline carries 'noClaim': on a node, an alias,
  // an id or an unknown.
  pub claimsMembership: bool,
  pub writeProtected: bool,
  pub graphStats: GraphnodeStats,
  pub viewStats: ViewnodeStats,
  pub overridesHere: Option<ID>,
  pub edit_request: Option<NodeEditRequest>,
  pub relRepo_request: Option<SkgrepoName>,
  pub view_requests: HashSet<ViewRequest>,
  // The readiness request inside '(editRequest ...)', on whichever
  // headline carries it.
  pub readiness_request: Option<ReadinessRequest>,
  pub property_relRepo : Option<SkgrepoName>,
  pub property_relRepo_request : Option<SkgrepoName>,
  // When true, this is a ghost (id and skgrepo are used).
  pub is_ghost: bool,
  // When true, this is a tombstone.
  pub is_tombstone: bool,
  // When Some, this is a Unknown (a phantom for a missing
  // referent). Carries only the id; no skgrepo/title/body apply.
  pub unknown_node_skgid: Option<ID>,
  pub unknown_relRepo : Option<SkgrepoName>,
  pub unknown_relRepo_request : Option<SkgrepoName>,
}

impl ViewnodeMetadata {
  /// Whether the headline is an occurrence: no other kind is named.
  pub fn names_an_occurrence (&self) -> bool {
    self . non_vognode . is_none ()
    && ! self . is_ghost
    && ! self . is_tombstone
    && self . unknown_node_skgid . is_none () }

  /// What a message says of the headline that wrote this metadata and
  /// TITLE: the IDs it writes, not any the parse supplies.
  pub fn headline_name (
    &self,
    title : &str,
  ) -> HeadlineName {
    HeadlineName::of (
      self . viewnode_id . as_ref (),
      self . skgid . as_ref ()
        . or ( self . unknown_node_skgid . as_ref () ),
      title ) }}

pub fn default_metadata() -> ViewnodeMetadata {
  ViewnodeMetadata {
    foldAndFocus: FoldAndFocus::default(),
    viewnode_id: None,
    non_vognode: None,
    skgid: None,
    home_skgrepo: None,
    claimsMembership: true,
    writeProtected: false,
    graphStats: GraphnodeStats::default(),
    viewStats: ViewnodeStats::default(),
    overridesHere: None,
    edit_request: None,
    relRepo_request: None,
    view_requests: HashSet::new(),
    readiness_request: None,
    property_relRepo : None,
    property_relRepo_request : None,
    is_ghost: false,
    is_tombstone: false,
    unknown_node_skgid: None,
    unknown_relRepo: None,
    unknown_relRepo_request: None, }}

/// Create a Viewnode from parsed metadata components.
/// This is the bridge between parsing (ViewnodeMetadata) and runtime (Viewnode).
/// An occurrence without a gnid gets a fresh one. One without a
/// skgrepo gets 'SkgrepoName::not_found', which validation refuses;
/// the parse gives it a real one first wherever one can be found
/// ('complete_occurrence_skgrepo').
/// Returns (Viewnode, error, warning):
/// - error if a Non-vognode has a body;
/// - warning if a folder header (PropertyFolder or PartnerFolder) has nonempty
///   title text, which the server discards: folder headlines are
///   titleless server-side (heralds supply their labels), so any
///   text there is a user edit that cannot be saved. Property leaves
///   are exempt -- their title is their data.
pub fn viewnode_from_metadata (
  metadata : &ViewnodeMetadata,
  title    : String,
  body     : Option < String >,
) -> ( Viewnode, Option < BufferValidationError >, Option < String > ) {
  let headline : HeadlineName =
    metadata . headline_name (&title);
  let (kind, error, warning)
    : (ViewnodeKind, Option<BufferValidationError>, Option<String>)
    = if let Some (ref uid) = metadata . unknown_node_skgid {
        ( ViewnodeKind::Vognode (Vognode::Unknown (
            Unknown {
              skgid           : uid . clone (),
              relRepo         : metadata . unknown_relRepo . clone (),
              relationshipEditRequest : metadata . unknown_relRepo_request . clone ()
                . map (RelationshipEditRequest::RelRepo),
              claimsMembership : metadata . claimsMembership,
            } )),
          if body . is_some () || ! title . is_empty () {
            Some ( BufferValidationError::Other ( with_org_subheadlines (
              "Unknown phantom content cannot be edited.",
              &[ headline . clone () ] )))
          } else { None },
          None )
      } else if metadata . is_tombstone {
        ( ViewnodeKind::Tombstone, None, None )
      } else if metadata . is_ghost {
        ( ViewnodeKind::Vognode (Vognode::Ghost ( Ghost {
            skgid : metadata . skgid . clone ()
                    . unwrap_or_else ( || ID::from ("")),
            home_skgrepo : metadata . home_skgrepo . clone ()
                           . unwrap_or_else ( || SkgrepoName::from ("")),
            title,
            body,
            restoreNode_request : matches! (
              metadata . edit_request,
              Some (NodeEditRequest::RestoreNode) ),
            tombstone_request : metadata . view_requests
              . contains (&ViewRequest::Tombstone),
            claimsMembership : metadata . claimsMembership,
            isWarranted : Warrant::ByGraph, // a derived fact: the refresh sets it
          } )), None, None )
      } else if let Some ( ref non_vognode ) = metadata . non_vognode {
        let is_flags_folder = matches! (non_vognode,
          ViewnodeKind::Folder (Folder::Flags { .. }));
        let is_flag = matches! (non_vognode,
          ViewnodeKind::PropertyMember (PropertyMember::Flag { .. }));
        let carries_text : bool = // the diff view's text properties
          matches! (non_vognode,
            ViewnodeKind::ChangeViewnode (
              ChangeViewnode::OldText { .. } | ChangeViewnode::BodyDiff { .. } ));
        let error : Option<BufferValidationError> =
          if body . is_some () && ! is_flags_folder && ! is_flag
             && ! carries_text {
            Some ( BufferValidationError::Body_of_NonVognode (
              title . clone (),
              kind_error_label (non_vognode),
              headline . clone () ))
          } else { None };
        let folder_title_warning : Option<String> =
          if ! title . is_empty ()
            && ! is_flags_folder
            && matches! ( non_vognode,
                          ViewnodeKind::Folder (_) )
          { Some ( with_org_subheadlines (
              &format! ( "Headline text on a {} is not saved; discarded: {:?}",
                         kind_error_label (non_vognode),
                         title ),
              &[ headline . clone () ] )) }
          else { None };
        let non_vognode_with_title : ViewnodeKind = match non_vognode {
          // Use headline title for string
          ViewnodeKind::PropertyMember (PropertyMember::Alias { .. }) =>
            ViewnodeKind::PropertyMember (PropertyMember::Alias {
              text : title . clone (),
              relRepo : metadata . property_relRepo . clone (),
              relationshipEditRequest : metadata . property_relRepo_request . clone ()
                . map (RelationshipEditRequest::RelRepo),
              claimsMembership : metadata . claimsMembership,
              isWarranted : Warrant::ByGraph }),
          ViewnodeKind::PropertyMember (PropertyMember::ID { .. }) =>
            ViewnodeKind::PropertyMember (PropertyMember::ID {
              skgid : title . clone () . into (),
              claimsMembership : metadata . claimsMembership,
              isWarranted : Warrant::ByGraph }),
          ViewnodeKind::PropertyMember (PropertyMember::Flag { flag, .. }) =>
            ViewnodeKind::PropertyMember (PropertyMember::flag (
              *flag, title . clone (), body . clone () )),
          ViewnodeKind::Folder (Folder::Flags { .. }) =>
            ViewnodeKind::Folder (Folder::Flags {
              title : title . clone (), body : body . clone () }),
          // The parser marks which aspects an OldText shows; its text
          // comes from the headline and the body.
          ViewnodeKind::ChangeViewnode (ChangeViewnode::OldText {
            title : title_changed, body : body_changed, .. }) =>
            ViewnodeKind::ChangeViewnode (ChangeViewnode::OldText {
              title : title_changed . as_ref () . map ( |_| title . clone () ),
              body  : body_changed . as_ref ()
                      . map ( |_| body . clone () . unwrap_or_default () ) }),
          ViewnodeKind::ChangeViewnode (ChangeViewnode::BodyDiff { .. }) =>
            ViewnodeKind::ChangeViewnode (ChangeViewnode::BodyDiff {
              diff : body . clone () . unwrap_or_default () }),
          ViewnodeKind::ChangeViewnode (ChangeViewnode::HomeSkgrepoMove { skgrepo_change, .. }) =>
            ViewnodeKind::ChangeViewnode (ChangeViewnode::HomeSkgrepoMove {
              skgrepo_change    : skgrepo_change . clone () }),
          ViewnodeKind::ChangeViewnode (ChangeViewnode::RelRepoMove { skgrepo_change, .. }) =>
            ViewnodeKind::ChangeViewnode (ChangeViewnode::RelRepoMove {
              skgrepo_change    : skgrepo_change . clone () }),
          ViewnodeKind::ChangeViewnode (ChangeViewnode::ChangesetMarker { kind, .. }) =>
            ViewnodeKind::ChangeViewnode (ChangeViewnode::ChangesetMarker {
              kind : *kind }),
          other => other . clone () };
        ( non_vognode_with_title, error, folder_title_warning )
      } else {
      // UnrestrictedVognode
      { let relationshipEditRequest : Option<RelationshipEditRequest> =
          // lives outside 'Editability'
          if matches! ( metadata . edit_request,
                        Some (NodeEditRequest::RestoreRel) )
          { Some (RelationshipEditRequest::RestoreRel) }
          else { metadata . relRepo_request . clone ()
                 . map (RelationshipEditRequest::RelRepo) };
        let edit_request : Option<NodeEditRequest> =
          if relationshipEditRequest == Some (RelationshipEditRequest::RestoreRel) { None }
          else { metadata . edit_request . clone () };
        let editability : Editability =
          if metadata . writeProtected
          { Editability::WriteProtected }
          else
          { Editability::Editable {
              body,
              edit_request : edit_request . clone () } };
        // An edit_request on a write-protected node has nowhere to live
        // (Editability::WriteProtected carries none), so the user's
        // instruction to delete or merge would silently vanish. Emit a
        // validation error instead so the save is rejected with a
        // clear message. We can only report this when the id is
        // known; if it isn't, other validations cover the missing-id
        // case.
        // (A relRepo request is not among these: it belongs to the
        // relationship from the view-parent, so it is the view-parent's
        // instruction. 'writeProtected_relRepo_request_errors' checks
        // that some editable node collects it. Nor is a restoreRel
        // request, which likewise concerns that relationship.)
        let error : Option<BufferValidationError> =
          if     metadata . writeProtected
              && edit_request . is_some ()
          { metadata . skgid . clone ()
            . map ( |skgid| BufferValidationError::EditRequestOnWriteProtectedOccurrence (
                      skgid, headline . clone () )) }
          else { None };
        let t : UnrestrictedVognode = UnrestrictedVognode {
            title,
            skgid            : metadata . skgid . clone ()
                               . unwrap_or_else ( || ID (
                                 Uuid::new_v4 () . to_string () )),
            home_skgrepo     : metadata . home_skgrepo . clone ()
                               . unwrap_or_else (SkgrepoName::not_found),
            claimsMembership : metadata . claimsMembership,
            overridesHere    : metadata . overridesHere . clone (),
            graphStats       : metadata . graphStats . clone (),
            viewStats        : metadata . viewStats . clone (),
            relationshipEditRequest,
            view_requests    : metadata . view_requests . clone (),
            editability, };
        let node_kind : ViewnodeKind =
          ViewnodeKind::Vognode ( Vognode::Unrestricted (t) );
        ( node_kind,
          error, None ) }
    };
  ( Viewnode { foldAndFocus      : metadata . foldAndFocus,
                 kind,
                 // derived facts, which the parser skips
                 change_facts      : ChangeFacts::default (),
                 readiness_request : metadata . readiness_request . clone (),
                 viewnode_id       : metadata . viewnode_id . clone () },
    error, warning ) }

fn kind_error_label (
  kind : &ViewnodeKind,
) -> String {
  match kind {
    ViewnodeKind::Folder (folder) =>
      folder . repr_in_client () . to_string (),
    ViewnodeKind::PropertyMember (member) =>
      member . repr_in_client () . to_string (),
    ViewnodeKind::ChangeViewnode (change) =>
      change . repr_in_client () . to_string (),
    ViewnodeKind::BufferRoot =>
      "forestRoot" . to_string (),
    ViewnodeKind::Tombstone =>
      "tombstone" . to_string (),
    ViewnodeKind::Vognode (_) =>
      viewnode_of ( kind . clone () ) . error_label (), } }


/// The grammar every headline's metadata follows, named by the
/// parser's refusal of a retired spelling.
pub const METADATA_GRAMMAR : &str =
  "(skg (vnid X) [focused] [folded] [bodyFolded] KIND DECLARED... \
   [(view FACT...)] [(diff FACT...)]), \
   KIND a bare atom (node, ghost, unknown, alias, id, tombstone, a folder, ...) \
   and every declared part beside it at the top level, among them (gnid X), \
   (skgRepos (nodeRepo R) [(relRepo R)]) and (editRequest ...), \
   which holds the readiness requests (ready OBJECT...) and (unready OBJECT...)";

/// The refusal of a retired spelling.
fn retired_spelling (
  spelling : &str,
) -> String {
  format! ( "'{}' is a retired metadata spelling; the grammar is {}",
            spelling, METADATA_GRAMMAR ) }

/// The kind a headline's metadata names.
#[derive(Debug, Clone)]
enum MetadataKind {
  Node,
  Ghost,
  Unknown,
  Tombstone,
  Other (ViewnodeKind),
}

/// The parts a kind accepts, beside the position atoms and the
/// viewnode ID, which every kind accepts.
fn parts_of_kind (
  kind : &MetadataKind,
) -> &'static [&'static str] {
  match kind {
    MetadataKind::Node =>
      &[ "gnid", "skgRepos", "noClaim", "writeProtected",
         "overridesHere", "editRequest", "viewRequests", "view", "diff" ],
    MetadataKind::Ghost =>
      &[ "gnid", "skgRepos", "noClaim", "editRequest", "viewRequests",
         "view", "diff" ],
    MetadataKind::Unknown =>
      &[ "gnid", "skgRepos", "noClaim", "editRequest", "diff" ],
    MetadataKind::Tombstone => &[],
    MetadataKind::Other (ViewnodeKind::PropertyMember (
      PropertyMember::Alias { .. } )) =>
      &[ "skgRepos", "noClaim", "editRequest", "view", "diff" ],
    MetadataKind::Other (ViewnodeKind::PropertyMember (
      PropertyMember::ID { .. } )) =>
      &[ "noClaim", "editRequest", "view", "diff" ],
    MetadataKind::Other (ViewnodeKind::PropertyMember (
      PropertyMember::Flag { .. } )) =>
      &[ "editRequest", "view", "diff" ],
    MetadataKind::Other (
      ViewnodeKind::ChangeViewnode (
        ChangeViewnode::OldText { .. }
        | ChangeViewnode::HomeSkgrepoMove { .. }
        | ChangeViewnode::RelRepoMove { .. }
        | ChangeViewnode::ChangesetMarker { .. } )) =>
      &[ "editRequest", "diff" ],
    MetadataKind::Other (_) => &[], } }

/// The name of 'kind' in an error message.
fn kind_name (
  kind : &MetadataKind,
) -> String {
  match kind {
    MetadataKind::Node      => "node" . to_string (),
    MetadataKind::Ghost     => "ghost" . to_string (),
    MetadataKind::Unknown   => "unknown" . to_string (),
    MetadataKind::Tombstone => "tombstone" . to_string (),
    MetadataKind::Other (kind) => kind_error_label (kind), } }

/// Parse metadata from org-mode headline into ViewnodeMetadata.
/// See file header comment for full syntax.
pub fn parse_metadata_to_viewnodemd (
  sexp_str : &str
) -> Result<ViewnodeMetadata, String> {
  let parsed : Sexp =
    sexp::parse (sexp_str)
    . map_err ( |e| format! ( "Failed to parse metadata as s-expression: {}", e ) ) ?;
  let elements : &[Sexp] = // what follows the symbol 'skg'
    match &parsed {
      Sexp::List (items) => {
        if items . is_empty () {
          return Err ( "Empty metadata s-expression" . to_string () ); }
        &items[1..] },
      _ => return Err ( "Expected metadata to be a list" . to_string () ), };
  let mut result : ViewnodeMetadata = default_metadata ();
  let mut kind : Option<MetadataKind> = None;
  let mut parts : Vec<(String, &Sexp)> = Vec::new ();
  for element in elements {
    let (name, kind_named) : (String, Option<MetadataKind>) =
      classify_metadata_element (element, &mut result) ?;
    if let Some (named) = kind_named {
      if let Some (earlier) = &kind {
        return Err ( format! ( "A headline has one kind; this one names {} and {}",
                               kind_name (earlier), kind_name (&named) )); }
      kind = Some (named);
    } else if ! name . is_empty () {
      if parts . iter () . any ( |(seen, _)| *seen == name ) {
        return Err ( format! ( "{} may appear only once", name )); }
      parts . push ( (name, element) ); }}
  let kind : MetadataKind = kind . unwrap_or (MetadataKind::Node);
  for (name, _) in &parts {
    if ! parts_of_kind (&kind) . contains ( &name . as_str () ) {
      return Err ( format! ( "A headline of kind {} does not take {}",
                             kind_name (&kind), name )); }}
  for (name, element) in &parts {
    read_metadata_part (&kind, name, element, &mut result) ?; }
  match kind {
    MetadataKind::Node => {},
    MetadataKind::Ghost => result . is_ghost = true,
    MetadataKind::Unknown =>
      if result . unknown_node_skgid . is_none () {
        return Err ( "An unknown requires its (gnid X)" . to_string () ); },
    MetadataKind::Tombstone => result . is_tombstone = true,
    MetadataKind::Other (other) => result . non_vognode = Some (other), }
  check_where_the_edit_request_sits (&result) ?;
  Ok (result) }

/// What one element of '(skg ...)' is: a position atom or the
/// viewnode ID, read into 'metadata' at once (the name returned is
/// empty); the kind (returned second, with its parameters read); or
/// a part, whose name is returned, to be read once the kind is
/// known. Refuses a retired spelling and anything unknown.
fn classify_metadata_element (
  element  : &Sexp,
  metadata : &mut ViewnodeMetadata,
) -> Result<(String, Option<MetadataKind>), String> {
  let part = |name : &str| -> Result<(String, Option<MetadataKind>), String> {
    Ok (( name . to_string (), None )) };
  let a_kind = |kind : MetadataKind| -> Result<(String, Option<MetadataKind>), String> {
    Ok (( String::new (), Some (kind) )) };
  match element {
    Sexp::Atom (_) => {
      let atom : String = atom_to_string (element) ?;
      match atom . as_str () {
        "focused"    => { metadata . foldAndFocus . focused = true;
                          Ok (( String::new (), None )) },
        "folded"     => { metadata . foldAndFocus . folded = true;
                          Ok (( String::new (), None )) },
        "bodyFolded" => { metadata . foldAndFocus . bodyFolded = true;
                          Ok (( String::new (), None )) },
        "noClaim" | "writeProtected" => part (&atom),
        "node"       => a_kind (MetadataKind::Node),
        "ghost"      => a_kind (MetadataKind::Ghost),
        "unknown"    => a_kind (MetadataKind::Unknown),
        "tombstone"  => a_kind (MetadataKind::Tombstone),
        "alias"      => a_kind ( MetadataKind::Other (
          ViewnodeKind::PropertyMember (
            PropertyMember::alias ( String::new (), None, None ) ))),
        "id"         => a_kind ( MetadataKind::Other (
          ViewnodeKind::PropertyMember (
            PropertyMember::id ( ID::default () ) ))),
        "forestRoot" => a_kind ( MetadataKind::Other (ViewnodeKind::BufferRoot) ),
        "bodyDiff"   => a_kind ( MetadataKind::Other (
          ViewnodeKind::ChangeViewnode (
            ChangeViewnode::BodyDiff { diff : String::new () } ))),
        "nonMember" | "restrictedNode" | "omittedBody" =>
          Err ( retired_spelling (&atom) ),
        folder_atom if folder_kind_of_atom (folder_atom) . is_some () =>
          a_kind ( MetadataKind::Other (
            folder_kind_of_atom (folder_atom) . expect ("a folder") )),
        _ => Err ( format! ( "Unknown top-level value: {}", atom )), }},
    Sexp::List (items) if ! items . is_empty () => {
      let head : String = atom_to_string ( &items[0] ) ?;
      let args : &[Sexp] = &items[1..];
      match head . as_str () {
        "vnid" => {
          if metadata . viewnode_id . is_some () {
            return Err ( "a viewnode ID may appear only once" . to_string () ); }
          metadata . viewnode_id = Some ( ViewnodeId::parse (
            & single_argument (&head, args) ? ) ? );
          Ok (( String::new (), None )) },
        "gnid" | "skgRepos" | "overridesHere"
          | "editRequest" | "viewRequests" | "view" | "diff" => part (&head),
        "oldText" => {
          let aspect : String = single_argument (&head, args) ?;
          let (title_changed, body_changed) : (bool, bool) =
            match aspect . as_str () {
              "titleAndBody" => (true,  true),
              "title"        => (true,  false),
              "body"         => (false, true),
              other => return Err ( format! (
                "Unknown oldText aspect: {}", other )), };
          a_kind ( MetadataKind::Other ( ViewnodeKind::ChangeViewnode (
            ChangeViewnode::OldText {
              title : title_changed . then (String::new),
              body  : body_changed  . then (String::new) } ))) },
        "homeSkgrepoMove" | "relRepoMove" => {
          let skgrepo_change : SkgrepoChange =
            parse_old_and_new_skgrepos (&head, args) ?;
          a_kind ( MetadataKind::Other ( ViewnodeKind::ChangeViewnode (
            if head == "homeSkgrepoMove" {
              ChangeViewnode::HomeSkgrepoMove { skgrepo_change }
            } else {
              ChangeViewnode::RelRepoMove { skgrepo_change } } ))) },
        "changesetMarker" => {
          let name : String = single_argument (&head, args) ?;
          let kind : ChangesetMarkerKind =
            ChangesetMarkerKind::from_wire_name (&name)
            . ok_or_else ( || format! (
              "Unknown changesetMarker kind: {}", name )) ?;
          a_kind ( MetadataKind::Other ( ViewnodeKind::ChangeViewnode (
            ChangeViewnode::ChangesetMarker { kind } ))) },
        "flag" => {
          let name : String = single_argument (&head, args)
            . map_err ( |_| "flag requires exactly one flag name" . to_string () ) ?;
          let flag : Flag = Flag::from_wire_name (&name)
            . ok_or_else (|| format! ("Unknown flag: {}", name)) ?;
          a_kind ( MetadataKind::Other ( ViewnodeKind::PropertyMember (
            PropertyMember::flag ( flag, String::new (), None ) ))) },
        "node" | "ghost" | "unknown" | "deleted" | "historical"
          | "id" | "repo" | "relRepo" | "rels" | "viewStats"
          | "homeRepoHerald" | "readyRequest" | "unreadyRequest" =>
          Err ( retired_spelling ( &format! ( "({} ...)", head )) ),
        folder_atom if folder_kind_of_atom (folder_atom) . is_some () =>
          Err ( retired_spelling ( &format! ( "({} ...)", folder_atom )) ),
        _ => Err ( format! ( "Unknown metadata key: {}", head )), }},
    _ => Err ( format! ( "Unexpected element in metadata sexp: {:?}",
                         element )), }}

/// Read the part named 'name', 'element', of a headline of kind
/// 'kind' into 'metadata'. 'parts_of_kind' has already said the kind
/// takes it.
fn read_metadata_part (
  kind     : &MetadataKind,
  name     : &str,
  element  : &Sexp,
  metadata : &mut ViewnodeMetadata,
) -> Result<(), String> {
  let args : &[Sexp] = match element {
    Sexp::List (items) => &items[1..],
    _ => &[], };
  match name {
    "noClaim" =>
      // the occurrence claims no membership; a claimant carries nothing
      metadata . claimsMembership = false,
    "writeProtected" =>
      metadata . writeProtected = true,
    "view" | "diff" => {}, // derived facts, skipped whole
    "gnid" => {
      let gnid : ID = ID::from ( single_argument (name, args) ? );
      if matches! (kind, MetadataKind::Unknown) {
        metadata . unknown_node_skgid = Some (gnid);
      } else { metadata . skgid = Some (gnid); }},
    "skgRepos" => {
      let (nodeRepo, relRepo) : (Option<SkgrepoName>, Option<SkgrepoName>) =
        parse_skgRepos (args) ?;
      match kind {
        MetadataKind::Node => {
          metadata . home_skgrepo = nodeRepo;
          metadata . viewStats . relRepo = relRepo; },
        MetadataKind::Ghost => {
          if relRepo . is_some () || nodeRepo . is_none () {
            return Err ( "A ghost's skgRepos holds only (nodeRepo R)"
                         . to_string () ); }
          metadata . home_skgrepo = nodeRepo; },
        _ => {
          if nodeRepo . is_some () || relRepo . is_none () {
            return Err ( format! (
              "The skgRepos of a headline of kind {} holds only (relRepo R)",
              kind_name (kind) )); }
          if matches! (kind, MetadataKind::Unknown) {
            metadata . unknown_relRepo = relRepo;
          } else { metadata . property_relRepo = relRepo; }}, }},
    "overridesHere" =>
      // Load-bearing: save extraction round-trips the original ID
      // through it.
      metadata . overridesHere = Some ( ID::from (
        single_argument (name, args) ? )),
    "editRequest" => match kind {
      MetadataKind::Node | MetadataKind::Ghost => {
        // Where a ghost's request may sit is checked once the whole
        // metadata is read ('check_where_the_edit_request_sits').
        parse_editrequest_sexp ( args, metadata ) ?;
        if matches! (kind, MetadataKind::Ghost)
           && metadata . relRepo_request . is_some () {
          return Err ( "A ghost accepts no edit request but restoreNode"
                       . to_string () ); }},
      _ => {
        let mut request_metadata : ViewnodeMetadata = default_metadata ();
        parse_editrequest_sexp ( args, &mut request_metadata ) ?;
        if let Some (message) = misplaced_restore_request_message (
             &request_metadata . edit_request, false ) {
          return Err (message); }
        let takes_a_relRepo_request : bool =
          matches! ( kind,
            MetadataKind::Unknown
            | MetadataKind::Other ( ViewnodeKind::PropertyMember (
                PropertyMember::Alias { .. } )));
        if request_metadata . edit_request . is_some ()
           || ( request_metadata . relRepo_request . is_some ()
                && ! takes_a_relRepo_request ) {
          return Err ( format! (
            "A headline of kind {} accepts no edit request but {}",
            kind_name (kind),
            if takes_a_relRepo_request { "(relRepo R) and a readiness request" }
            else { "a readiness request" } )); }
        metadata . readiness_request = request_metadata . readiness_request;
        if matches! (kind, MetadataKind::Unknown) {
          metadata . unknown_relRepo_request = request_metadata . relRepo_request;
        } else {
          metadata . property_relRepo_request = request_metadata . relRepo_request; }}, },
    "viewRequests" =>
      parse_viewrequests_sexp ( args, &mut metadata . view_requests ) ?,
    other => return Err ( format! ( "Unknown metadata part: {}", other )), }
  Ok (( )) }

/// What the parser says to a restoreRel on a ghost.
pub const RESTORE_REL_ON_A_DELETED_NODE : &str =
  "This node is deleted; restore it first (skg-restore-deleted-node)";
/// What the parser says to a restoreNode anywhere but on a ghost.
pub const RESTORE_NODE_ON_A_NODE_NOT_DELETED : &str =
  "Only a node deleted since HEAD can be restored; restoreNode belongs on a ghost (skg-restore-deleted-node)";
/// What the parser says to a tombstone request anywhere but on a ghost.
pub const TOMBSTONE_REQUEST_ON_A_NON_GHOST : &str =
  "Only a ghost can be entombstoned; the tombstone view request belongs on a ghost (skg-request-ghost-tombstone)";
/// What the parser says to a restoreRel anywhere but on a removed
/// membership.
pub const RESTORE_REL_WITHOUT_A_REMOVED_MEMBERSHIP : &str =
  "Only a removed membership can be restored; restoreRel belongs on a removed member, or on a folder member carrying (rel removed) (skg-restore-removed-membership)";

/// The refusal of a restore request on a headline that cannot carry
/// it, given whether the headline is a ghost; None for
/// any other request.
fn misplaced_restore_request_message (
  request       : &Option<NodeEditRequest>,
  is_ghost : bool,
) -> Option<String> {
  match request {
    Some (NodeEditRequest::RestoreNode) =>
      Some ( RESTORE_NODE_ON_A_NODE_NOT_DELETED . to_string () ),
    Some (NodeEditRequest::RestoreRel) if is_ghost =>
      Some ( RESTORE_REL_ON_A_DELETED_NODE . to_string () ),
    Some (NodeEditRequest::RestoreRel) =>
      Some ( RESTORE_REL_WITHOUT_A_REMOVED_MEMBERSHIP . to_string () ),
    _ => None, } }

/// A ghost carries no edit request but restoreNode, and no view
/// request but tombstone, which nothing else carries; restoreRel sits
/// only on an unrestricted vognode, and the save checks its position.
/// The other requests keep the places their own checks give them.
fn check_where_the_edit_request_sits (
  metadata : &ViewnodeMetadata,
) -> Result<(), String> {
  let is_unrestricted_vognode : bool =
    metadata . names_an_occurrence ();
  if metadata . view_requests . contains (&ViewRequest::Tombstone)
     && ! metadata . is_ghost
  { return Err ( TOMBSTONE_REQUEST_ON_A_NON_GHOST . to_string () ); }
  if metadata . is_ghost
     && metadata . view_requests . iter ()
        . any ( |request| *request != ViewRequest::Tombstone )
  { return Err ( "A ghost accepts no view request but tombstone"
                 . to_string () ); }
  match &metadata . edit_request {
    Some (NodeEditRequest::RestoreNode) if metadata . is_ghost =>
      Ok (( )),
    Some (NodeEditRequest::RestoreRel) if is_unrestricted_vognode =>
      Ok (( )),
    request @ Some (NodeEditRequest::RestoreNode | NodeEditRequest::RestoreRel) =>
      Err ( misplaced_restore_request_message (
        request, metadata . is_ghost )
            . expect ("a restore request") ),
    Some (_) if metadata . is_ghost =>
      Err ( "A ghost accepts no edit request but restoreNode"
            . to_string () ),
    _ => Ok (( )), } }

/// The folder a bare folder atom names.
fn folder_kind_of_atom (
  atom : &str,
) -> Option<ViewnodeKind> {
  Folder::from_client_string (atom) . map (ViewnodeKind::Folder) }


/// The one atom argument of the form whose head is 'head'.
fn single_argument (
  head : &str,
  args : &[Sexp],
) -> Result<String, String> {
  match args {
    [arg] => atom_to_string (arg),
    _ => Err ( format! ( "{} requires exactly one argument", head )), } }

/// The skgrepos of '(HEAD (old OLD) (new NEW))', whose arguments are 'args'.
fn parse_old_and_new_skgrepos (
  head : &str,
  args : &[Sexp],
) -> Result<SkgrepoChange, String> {
  let mut old : Option<SkgrepoName> = None;
  let mut new : Option<SkgrepoName> = None;
  for arg in args {
    let Sexp::List (pair) = arg else {
      return Err ( format! ( "{} takes only (old REPO) and (new REPO)", head )); };
    let key : String = match pair . first () {
      Some (key) => atom_to_string (key) ?,
      None => return Err ( format! ( "{} has an empty form", head )), };
    let slot : &mut Option<SkgrepoName> = match key . as_str () {
      "old" => &mut old,
      "new" => &mut new,
      other => return Err ( format! ( "Unknown {} key: {}", head, other )), };
    if slot . is_some () {
      return Err ( format! ( "{} names {} twice", head, key )); }
    *slot = Some ( SkgrepoName::from (
      single_argument (&key, &pair [1..]) ? )); }
  match (old, new) {
    (Some (old), Some (new)) => Ok ( SkgrepoChange { old, new } ),
    _ => Err ( format! ( "{} requires both (old REPO) and (new REPO)", head )), } }

/// The skgrepos of '(skgRepos [(nodeRepo R)] [(relRepo R)])', whose
/// parts are 'parts': the home skgrepo of the node a headline stands
/// for, and the skgrepo recording its relationship to its viewparent.
fn parse_skgRepos (
  parts : &[Sexp],
) -> Result<(Option<SkgrepoName>, Option<SkgrepoName>), String> {
  let mut nodeRepo : Option<SkgrepoName> = None;
  let mut relRepo  : Option<SkgrepoName> = None;
  for part in parts {
    let Sexp::List (pair) = part else {
      return Err ( "skgRepos holds only (nodeRepo R) and (relRepo R)"
                   . to_string () ); };
    let key : String = match pair . first () {
      Some (key) => atom_to_string (key) ?,
      None => return Err ( "skgRepos has an empty form" . to_string () ), };
    let slot : &mut Option<SkgrepoName> = match key . as_str () {
      "nodeRepo" => &mut nodeRepo,
      "relRepo"  => &mut relRepo,
      other => return Err ( format! ( "Unknown skgRepos key: {}", other )), };
    if slot . is_some () {
      return Err ( format! ( "skgRepos names {} twice", key )); }
    *slot = Some ( SkgrepoName::from (
      single_argument (&key, &pair [1..]) ? )); }
  if nodeRepo . is_none () && relRepo . is_none () {
    return Err ( "skgRepos holds (nodeRepo R), (relRepo R) or both"
                 . to_string () ); }
  Ok (( nodeRepo, relRepo )) }

/// Parse the (editRequest ...) s-expression contents: at most one
/// edit request, and at most one readiness request, '(ready
/// OBJECT...)' or '(unready OBJECT...)'.
fn parse_editrequest_sexp (
  items : &[Sexp],
  metadata : &mut ViewnodeMetadata
) -> Result<(), String> {
  if items . is_empty () {
    return Err ( "editRequest requires a request" . to_string () ); }
  let mut edit_requests : usize = 0;
  for element in items {
    if let Some (request) = readiness_request_of (element) ? {
      if metadata . readiness_request . is_some () {
        return Err ( "A viewnode may carry only one readiness request"
                     . to_string () ); }
      metadata . readiness_request = Some (request);
      continue; }
    edit_requests += 1;
    if edit_requests > 1 {
      return Err ( "editRequest holds at most one request beside a readiness request"
                   . to_string () ); }
    parse_one_edit_request (element, metadata) ?; }
  Ok (( )) }

/// The readiness request 'element' is, if it is '(ready OBJECT...)'
/// or '(unready OBJECT...)'.
fn readiness_request_of (
  element : &Sexp,
) -> Result<Option<ReadinessRequest>, String> {
  let Sexp::List (items) = element else { return Ok (None); };
  let Some (head) : Option<&Sexp> = items . first () else { return Ok (None); };
  let head : String = atom_to_string (head) ?;
  if head != "ready" && head != "unready" { return Ok (None); }
  let atoms : Vec<String> =
    items [1..] . iter () . map (atom_to_string) . collect::<Result<_, _>> () ?;
  Ok ( Some ( ReadinessRequest {
    ready   : head == "ready",
    objects : readiness_objects_from_atoms (&atoms) ?, } )) }

/// Parse one edit request inside (editRequest ...).
fn parse_one_edit_request (
  element : &Sexp,
  metadata : &mut ViewnodeMetadata
) -> Result<(), String> {
  match element {
    Sexp::List (subitems) if subitems . len () == 2 => {
      let key : String =
        atom_to_string ( &subitems[0] ) ?;
      if key == "merge" {
        let id_str : String =
          atom_to_string ( &subitems[1] ) ?;
        metadata . edit_request = Some (
          NodeEditRequest::NodeMerge ( ID::from (id_str)));
      } else if key == "relRepo" {
        let skgrepo : String = atom_to_string ( &subitems[1] ) ?;
        metadata . relRepo_request =
          Some ( SkgrepoName::from (skgrepo) );
      } else {
        return Err ( format! ( "Unknown editRequest key: {}", key )); }
    },
    Sexp::List (subitems) if subitems . len () == 3 => {
      let key : String = atom_to_string (&subitems [0]) ?;
      if key != "flag" {
        return Err ( format! (
          "Unknown three-part editRequest key: {}", key )); }
      let flag_name : String = atom_to_string (&subitems [1]) ?;
      let flag : Flag =
        Flag::from_wire_name (&flag_name)
        . ok_or_else (|| format! (
          "Unknown flag: {}", flag_name )) ?;
      if ! flag . is_mutable () {
        return Err ( format! (
          "Flag {} is write-protected provenance and cannot be changed",
          flag_name )); }
      let value_name : String = atom_to_string (&subitems [2]) ?;
      let value : bool = match value_name . as_str () {
        "true"  => true,
        "false" => false,
        _ => return Err ( format! (
          "Flag value must be true or false, got: {}", value_name )), };
      metadata . edit_request = Some (
        NodeEditRequest::SetFlag { flag, value });
    },
    Sexp::Atom (_) => {
      let bare_value : String =
        atom_to_string (element) ?;
      match bare_value . as_str () {
        "delete" => metadata . edit_request = Some (NodeEditRequest::Delete),
        "restoreNode" =>
          metadata . edit_request = Some (NodeEditRequest::RestoreNode),
        "restoreRel" =>
          metadata . edit_request = Some (NodeEditRequest::RestoreRel),
        _ => {
          return Err ( format! ( "Unknown editRequest value: {}",
                                  bare_value )); }} },
    _ => { return Err ( "Unexpected element in editRequest"
                         . to_string () ); }}
  Ok (( )) }


/// Parse the (viewRequests ...) s-expression and update viewRequests.
/// Each request is either a bare atom ('editableView',
/// 'writeProtectedView', 'writeProtectedSubtreeView', 'fork',
/// 'flags'), or a nested
/// '(folder RELNAME)' form.
fn parse_viewrequests_sexp (
  items : &[Sexp],
  requests : &mut HashSet<ViewRequest>
) -> Result<(), String> {
  for request_element in items {
    let request : ViewRequest = match request_element {
      Sexp::Atom (_) => {
        let atom : String = atom_to_string (request_element) ?;
        if atom == "editableView" { ViewRequest::Editable }
        else if atom == "writeProtectedView" { ViewRequest::WriteProtected }
        else if atom == "writeProtectedSubtreeView" { ViewRequest::WriteProtectedSubtree }
        else if atom == "fork" { ViewRequest::Fork }
        else if atom == "flags" { ViewRequest::Flags }
        else if atom == "tombstone" { ViewRequest::Tombstone }
        else { return Err ( format! (
          "Invalid view request atom: {}", atom )); } },
      Sexp::List (sub) if sub . len () == 2 => {
        let head : String = atom_to_string ( &sub[0] ) ?;
        let arg  : String = atom_to_string ( &sub[1] ) ?;
        match head . as_str () {
          "folder"  => ViewRequest::Folder (
            FolderRelation::from_relname (&arg)
              . ok_or_else ( || format! (
                "Invalid folder relname: {}", arg )) ?),
          _ => return Err ( format! (
            "Unknown view request form: ({} ...)", head )), } },
      _ => return Err (
        "Unexpected element in viewRequests (expected 'editableView' \
         or '(folder ...)')" . to_string () ), };
    requests . insert (request); }
  Ok (( )) }


fn readiness_objects_from_atoms (
  atoms : &[String],
) -> Result<BTreeSet<DiffObject>, String> {
  atoms . iter ()
    . map ( |atom| DiffObject::from_wire_name (atom)
            . ok_or_else ( || format! ( "Unknown diff object: {}", atom )) )
    . collect () }
