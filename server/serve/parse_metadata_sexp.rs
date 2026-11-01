//! PURPOSE: Parse (skg ...) metadata s-expressions from org headlines.
//!
//! Format ('METADATA_GRAMMAR'): one shape for every headline,
//!   (skg (vnid X) [focused] [folded] [bodyFolded] KIND DECLARED...
//!        [(view FACT...)] [(diff FACT...)])
//! KIND a bare atom, or for a few kinds a small form naming its
//! parameter, and every part beside it at the top level, in any order.
//! '(view ...)' and '(diff ...)' hold the derived facts the server
//! writes, which nobody reads back: the parser skips each whole.
//! The kinds and the declared parts each takes. The fields a user sets
//! (noClaim, writeProtected, the requests of '(editRequest ...)',
//! '(viewRequests ...)') a kind takes as 'shared/metadata-fields.json'
//! lists them for it ('metadata_fields.rs'); the other parts as
//! 'parts_the_table_leaves_out' says:
//!   node [(gnid ID)] [(skgRepos [(nodeRepo REPO)] [(relRepo REPO)])]
//!        [noClaim]          ; the occurrence claims no membership
//!        [writeProtected]   ; marks the occurrence write-protected
//!        [(overridesHere ID)]
//!        [(editRequest [<delete | (merge ID) | (relRepo REPO)
//!                       | (flag FLAG true|false) | restoreRel>]
//!                      [(ready OBJECT...) | (unready OBJECT...)])]
//!        [(viewRequests REQUEST...)]
//!     where OBJECT is node, rel or all. A headline naming no kind is
//!     a node.
//!   ghost (gnid ID) [(skgRepos (nodeRepo REPO))] [noClaim]
//!         [(editRequest [restoreNode] [READINESS])]
//!   unknown (gnid ID) [(skgRepos (relRepo REPO))] [noClaim]
//!           [(editRequest [(relRepo REPO)] [READINESS])]
//!   restricted, the restricted viewnode, with the parts an
//!     unknown viewnode takes
//!   alias [(skgRepos (relRepo REPO))] [noClaim]
//!         [(editRequest [(relRepo REPO)] [READINESS])]
//!   id [noClaim] [(editRequest READINESS)],
//!   (flag hadId | wasOverloaded | noSearchMatching) [(editRequest READINESS)],
//!   and the diff view's own viewnodes
//!     (oldText titleAndBody | title | body)
//!     (homeSkgrepoMove (old REPO) (new REPO))
//!     (relRepoMove (old REPO) (new REPO))
//!     (changesetMarker content | relRepo | orderOnly)
//!   each of which may carry '(editRequest READINESS)';
//!   bodyDiff, tombstone, forestRoot and the folders' bare atoms,
//!   which carry nothing.
//! READINESS is '(ready OBJECT...)' or '(unready OBJECT...)'.
//! A node, a ghost, an alias, an id and a flag may carry '(view ...)';
//! every kind that takes an editRequest may carry '(diff ...)'.
//! 'restoreRel' asks to restore a removed membership; where it may
//! stand the save checks ('restoreRel_stands_where_it_may', local.rs),
//! and the save drops one that names no relationship HEAD records. It
//! may sit beside 'writeProtected'.
//! The viewnode ID, '(vnid X)', X being one or more lowercase letters
//! and digits, comes first when written.
//! A retired spelling ('(node ...)', '(ghost ...)' or '(unknown ...)' as
//! a form, '(deleted ...)', '(historical ...)', '(id X)', '(repo R)', a
//! top-level '(relRepo R)', '(FOLDER changed)', '(rels ...)',
//! '(viewStats ...)', 'omittedBody' or '(homeRepoHerald ...)' outside
//! '(view ...)', '(readyRequest ...)', '(unreadyRequest ...)',
//! 'nonMember', 'restrictedNode') is refused with a message naming the
//! grammar.

use crate::serve::metadata_fields::{
  kind_takes_field, kind_takes_value, values_written_for_a_message};
use crate::types::sexp::atom_to_string;
use crate::types::misc::{ID, SkgrepoChange, SkgrepoName};
use crate::types::errors::{BufferValidationError, HeadlineName, with_org_subheadlines};
use crate::types::nodes::complete::Flag;
use crate::types::viewnode::{
  ChangeFacts, DiffObject, FoldAndFocus, ReadinessRequest,
  GraphnodeStats, ViewnodeStats, NodeEditRequest, ViewRequest, FolderRelation,
  ChangesetMarkerKind, PropertyMember, ChangeViewnode, Folder,
  Ghost, GnidOnlyKind, GnidOnlyVognode,
  Editability, RelationshipEditRequest, ViewnodeId, Warrant,
};
use crate::types::viewnode::{
    Viewnode, ViewnodeKind, Occurrence,
    Vognode,
};

use sexp::Sexp;
use std::collections::{BTreeSet, HashSet};
use uuid::Uuid;

//
// Parsing-internal types
//

/// What a headline's metadata says: the parts every headline may
/// carry, and its kind ('kind'), which carries the rest.
/// Converted to a Viewnode by 'viewnode_from_metadata'.
#[derive(Debug, Clone, PartialEq)]
pub struct ViewnodeMetadata {
  pub foldAndFocus      : FoldAndFocus,
  pub viewnode_id       : Option<ViewnodeId>,
  /// The readiness request inside '(editRequest ...)', on whichever
  /// headline carries it.
  pub readiness_request : Option<ReadinessRequest>,
  // The derived facts. The parser skips '(view ...)' and '(diff ...)',
  // so all it reads of them is an occurrence's relRepo, from its
  // '(skgRepos ...)'.
  pub graphStats        : GraphnodeStats,
  pub viewStats         : ViewnodeStats,
  /// The kind the headline names, with the parts that kind takes.
  pub kind              : MetadataKind,
}

/// The kind a headline's metadata names, one variant per kind of
/// headline that 'shared/metadata-fields.json' lists, each carrying
/// the parts that kind's headline writes.
#[derive(Debug, Clone, PartialEq)]
pub enum MetadataKind {
  Occurrence     (OccurrenceMetadata),
  Ghost          (GhostMetadata),
  GnidOnly       (GnidOnlyMetadata), // an unknown viewnode or
                                     // a restricted viewnode
  Alias          (AliasMetadata),
  ID             { claimsMembership : bool },
  Flag           (Flag),
  ChangeViewnode (ChangeViewnode),
  Folder         (Folder),
  Tombstone,
  BufferRoot,
}

/// What an occurrence's headline says. It may leave out its gnid and
/// its home skgrepo; the parse completes them ('org_to_viewforest').
#[derive(Debug, Clone, PartialEq)]
pub struct OccurrenceMetadata {
  pub gnid             : Option<ID>,
  pub home_skgrepo     : Option<SkgrepoName>,
  /// False when the headline carries 'noClaim'.
  pub claimsMembership : bool,
  pub writeProtected   : bool,
  /// Load-bearing: save extraction round-trips the original ID
  /// through it.
  pub overridesHere    : Option<ID>,
  pub edit_request     : Option<OccurrenceEditRequest>,
  pub view_requests    : HashSet<ViewRequest>,
}

impl Default for OccurrenceMetadata {
  /// A bare occurrence: what a headline with no metadata says.
  fn default () -> Self {
    OccurrenceMetadata {
      gnid             : None,
      home_skgrepo     : None,
      claimsMembership : true,
      writeProtected   : false,
      overridesHere    : None,
      edit_request     : None,
      view_requests    : HashSet::new (), } }
}

/// An occurrence's one edit request: about its node (delete, merge,
/// flag), which only an editable occurrence can carry out, or about
/// its relationship to its viewparent (relRepo, restoreRel), which a
/// write-protected one carries too.
#[derive(Debug, Clone, PartialEq)]
pub enum OccurrenceEditRequest {
  OfTheNode         (NodeEditRequest),
  OfTheRelationship (RelationshipEditRequest),
}

/// What a ghost's headline says.
#[derive(Debug, Clone, PartialEq)]
pub struct GhostMetadata {
  pub gnid                : ID,
  /// The refresh supplies HEAD's where the headline leaves it out.
  pub home_skgrepo        : Option<SkgrepoName>,
  pub claimsMembership    : bool,
  pub restoreNode_request : bool,
}

/// What a gnid-only vognode's headline says: its kind, its gnid, the
/// record's relRepo, a relRepo request and its claim. A headline that
/// writes no gnid is refused.
#[derive(Debug, Clone, PartialEq)]
pub struct GnidOnlyMetadata {
  pub kind             : GnidOnlyKind,
  pub gnid             : ID,
  pub relRepo          : Option<SkgrepoName>,
  pub relRepo_request  : Option<SkgrepoName>,
  pub claimsMembership : bool,
}

/// What an alias's headline says beside its text: the record's
/// relRepo, a relRepo request and its claim.
#[derive(Debug, Clone, PartialEq)]
pub struct AliasMetadata {
  pub relRepo          : Option<SkgrepoName>,
  pub relRepo_request  : Option<SkgrepoName>,
  pub claimsMembership : bool,
}

impl MetadataKind {
  /// The atom the kind is known by, in the metadata and in
  /// 'shared/metadata-fields.json'. A message names the kind by it.
  pub fn atom (&self) -> &'static str {
    match self {
      MetadataKind::Occurrence (_)          => "node",
      MetadataKind::Ghost (_)               => "ghost",
      MetadataKind::GnidOnly (gnid_only)    => gnid_only . kind . wire_atom (),
      MetadataKind::Alias (_)               => "alias",
      MetadataKind::ID { .. }               => "id",
      MetadataKind::Flag (_)                => "flag",
      MetadataKind::ChangeViewnode (change) => change . repr_in_client (),
      MetadataKind::Folder (folder)         => folder . repr_in_client (),
      MetadataKind::Tombstone               => "tombstone",
      MetadataKind::BufferRoot              => "forestRoot", } }

  /// The gnid the headline writes: an occurrence's, if it writes one,
  /// a ghost's or a gnid-only vognode's. None for any other kind.
  pub fn gnid (&self) -> Option<&ID> {
    match self {
      MetadataKind::Occurrence (occurrence) => occurrence . gnid . as_ref (),
      MetadataKind::Ghost (ghost)           => Some (&ghost . gnid),
      MetadataKind::GnidOnly (gnid_only)    => Some (&gnid_only . gnid),
      _ => None, } }
}

impl ViewnodeMetadata {
  /// What a message says of the headline that wrote this metadata and
  /// TITLE: the IDs it writes, not any the parse supplies.
  pub fn headline_name (
    &self,
    title : &str,
  ) -> HeadlineName {
    HeadlineName::of (
      self . viewnode_id . as_ref (),
      self . kind . gnid (),
      title ) }}

pub fn default_metadata() -> ViewnodeMetadata {
  ViewnodeMetadata {
    foldAndFocus      : FoldAndFocus::default (),
    viewnode_id       : None,
    readiness_request : None,
    graphStats        : GraphnodeStats::default (),
    viewStats         : ViewnodeStats::default (),
    kind              : MetadataKind::Occurrence (
                          OccurrenceMetadata::default () ), }}

/// Create a Viewnode from parsed metadata components.
/// This is the bridge between parsing (ViewnodeMetadata) and runtime (Viewnode).
/// Each kind's arm builds its viewnode from its own parts.
/// Returns (Viewnode, error, warning):
/// - error if a Non-vognode has a body;
/// - warning if a folder header (a property or partner folder) has nonempty
///   title text, which the server discards: folder headlines are
///   titleless server-side (heralds supply their labels), so any
///   text there is a user edit that cannot be saved. Property members
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
    = match &metadata . kind {
      MetadataKind::Occurrence (occurrence) =>
        occurrence_from_metadata (
          occurrence, metadata, title, body, &headline ),
      MetadataKind::Ghost (ghost) =>
        ( ViewnodeKind::Vognode (Vognode::Ghost ( Ghost {
            gnid : ghost . gnid . clone (),
            home_skgrepo : ghost . home_skgrepo . clone ()
                           . unwrap_or_else ( || SkgrepoName::from ("")),
            title,
            body,
            restoreNode_request : ghost . restoreNode_request,
            claimsMembership : ghost . claimsMembership,
            warrant : Warrant::ByGraph, // a derived fact: the refresh sets it
          } )), None, None ),
      MetadataKind::GnidOnly (gnid_only) => {
        let relationshipEditRequest : Option<RelationshipEditRequest> =
          gnid_only . relRepo_request . clone ()
          . map (RelationshipEditRequest::RelRepo);
        ( ViewnodeKind::Vognode ( Vognode::GnidOnly ( GnidOnlyVognode {
            relationshipEditRequest,
            .. GnidOnlyVognode::new (
              gnid_only . kind,
              gnid_only . gnid . clone (),
              gnid_only . relRepo . clone (),
              gnid_only . claimsMembership,
              Warrant::ByGraph ) } )), // a derived fact: the refresh sets it
          if body . is_some () || ! title . is_empty () {
            Some ( BufferValidationError::Other ( with_org_subheadlines (
              &format! ( "{} content cannot be edited.",
                         gnid_only . kind . wire_atom () ),
              &[ headline . clone () ] )))
          } else { None },
          None ) },
      MetadataKind::Alias (alias) => {
        let error : Option<BufferValidationError> =
          body_refusal (&body, &title, "alias", &headline);
        ( ViewnodeKind::PropertyMember (PropertyMember::Alias {
            text : title, // the headline's title is the alias
            relRepo : alias . relRepo . clone (),
            relationshipEditRequest : alias . relRepo_request . clone ()
              . map (RelationshipEditRequest::RelRepo),
            claimsMembership : alias . claimsMembership,
            warrant : Warrant::ByGraph }),
          error, None ) },
      MetadataKind::ID { claimsMembership } => {
        let error : Option<BufferValidationError> =
          body_refusal (&body, &title, "id", &headline);
        ( ViewnodeKind::PropertyMember (PropertyMember::ID {
            gnid : title . into (), // the headline's title is the ID
            claimsMembership : *claimsMembership,
            warrant : Warrant::ByGraph }),
          error, None ) },
      MetadataKind::Flag (flag) =>
        ( ViewnodeKind::PropertyMember (
            PropertyMember::flag ( *flag, title, body )),
          None, None ),
      MetadataKind::ChangeViewnode (change) =>
        change_viewnode_from_metadata (change, title, body, &headline),
      MetadataKind::Folder (Folder::Flags { .. }) =>
        ( ViewnodeKind::Folder ( Folder::Flags { title, body } ),
          None, None ),
      // Its text is the illegal sections', which the refresh redraws
      // from the graph; nothing saves it.
      MetadataKind::Folder (Folder::IllegalEdit { .. }) =>
        ( ViewnodeKind::Folder ( Folder::IllegalEdit { title, body } ),
          None, None ),
      MetadataKind::Folder (folder) => {
        let error : Option<BufferValidationError> =
          body_refusal (&body, &title, folder . repr_in_client (), &headline);
        let warning : Option<String> =
          if title . is_empty () { None }
          else { Some ( with_org_subheadlines (
            &format! ( "Headline text on a {} is not saved; discarded: {:?}",
                       folder . repr_in_client (),
                       title ),
            &[ headline . clone () ] )) };
        ( ViewnodeKind::Folder ( folder . clone () ), error, warning ) },
      MetadataKind::Tombstone =>
        ( ViewnodeKind::Tombstone, None, None ),
      MetadataKind::BufferRoot => {
        let error : Option<BufferValidationError> =
          body_refusal (&body, &title, "forestRoot", &headline);
        ( ViewnodeKind::BufferRoot, error, None ) }, };
  ( Viewnode { foldAndFocus      : metadata . foldAndFocus,
                 kind,
                 // derived facts, which the parser skips
                 change_facts      : ChangeFacts::default (),
                 readiness_request : metadata . readiness_request . clone (),
                 viewnode_id       : metadata . viewnode_id . clone () },
    error, warning ) }

/// The occurrence an occurrence's metadata, title and body say.
/// One without a gnid gets a fresh one. One without a skgrepo gets
/// 'SkgrepoName::not_found', which validation refuses; the parse
/// gives it a real one first wherever one can be found
/// ('complete_occurrence_skgrepo').
fn occurrence_from_metadata (
  occurrence : &OccurrenceMetadata,
  metadata   : &ViewnodeMetadata,
  title      : String,
  body       : Option<String>,
  headline   : &HeadlineName,
) -> (ViewnodeKind, Option<BufferValidationError>, Option<String>) {
  let (edit_request, relationshipEditRequest)
    : (Option<NodeEditRequest>, Option<RelationshipEditRequest>)
    = // the latter lives outside 'Editability'
    match &occurrence . edit_request {
      Some (OccurrenceEditRequest::OfTheNode (request)) =>
        ( Some (request . clone ()), None ),
      Some (OccurrenceEditRequest::OfTheRelationship (request)) =>
        ( None, Some (request . clone ()) ),
      None => ( None, None ), };
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
    if occurrence . writeProtected && edit_request . is_some ()
    { occurrence . gnid . clone ()
      . map ( |gnid| BufferValidationError::EditRequestOnWriteProtectedOccurrence (
                gnid, headline . clone () )) }
    else { None };
  let editability : Editability =
    if occurrence . writeProtected
    { Editability::WriteProtected }
    else
    { Editability::Editable { body, edit_request } };
  ( ViewnodeKind::Vognode ( Vognode::Occurrence ( Occurrence {
      title,
      gnid             : occurrence . gnid . clone ()
                         . unwrap_or_else ( || ID (
                           Uuid::new_v4 () . to_string () )),
      home_skgrepo     : occurrence . home_skgrepo . clone ()
                         . unwrap_or_else (SkgrepoName::not_found),
      claimsMembership : occurrence . claimsMembership,
      overridesHere    : occurrence . overridesHere . clone (),
      graphStats       : metadata . graphStats . clone (),
      viewStats        : metadata . viewStats . clone (),
      relationshipEditRequest,
      view_requests    : occurrence . view_requests . clone (),
      editability, } )),
    error, None ) }

/// The change viewnode a change viewnode's metadata, title and body
/// say. An OldText's text comes from the headline and the body (the
/// parser marks only which aspects it shows), a BodyDiff's from the
/// body; any other refuses a body.
fn change_viewnode_from_metadata (
  change   : &ChangeViewnode,
  title    : String,
  body     : Option<String>,
  headline : &HeadlineName,
) -> (ViewnodeKind, Option<BufferValidationError>, Option<String>) {
  match change {
    ChangeViewnode::OldText { title : title_changed, body : body_changed } =>
      ( ViewnodeKind::ChangeViewnode ( ChangeViewnode::OldText {
          title : title_changed . as_ref () . map ( |_| title ),
          body  : body_changed . as_ref ()
                  . map ( |_| body . unwrap_or_default () ) } ),
        None, None ),
    ChangeViewnode::BodyDiff { .. } =>
      ( ViewnodeKind::ChangeViewnode ( ChangeViewnode::BodyDiff {
          diff : body . unwrap_or_default () } ),
        None, None ),
    other =>
      ( ViewnodeKind::ChangeViewnode ( other . clone () ),
        body_refusal (&body, &title, other . repr_in_client (), headline),
        None ), } }

/// The refusal of body text under a headline of a kind that has no
/// body, known by its atom 'kind_atom'.
fn body_refusal (
  body      : &Option<String>,
  title     : &str,
  kind_atom : &str,
  headline  : &HeadlineName,
) -> Option<BufferValidationError> {
  body . is_some () . then ( || BufferValidationError::Body_of_NonVognode (
    title . to_string (),
    kind_atom . to_string (),
    headline . clone () )) }


/// The grammar every headline's metadata follows, named by the
/// parser's refusal of a retired spelling.
pub const METADATA_GRAMMAR : &str =
  "(skg (vnid X) [focused] [folded] [bodyFolded] KIND DECLARED... \
   [(view FACT...)] [(diff FACT...)]), \
   KIND a bare atom (node, ghost, unknown, restricted, alias, id, tombstone, a folder, ...) \
   and every declared part beside it at the top level, among them (gnid X), \
   (skgRepos (nodeRepo R) [(relRepo R)]) and (editRequest ...), \
   which holds the readiness requests (ready OBJECT...) and (unready OBJECT...)";

/// The refusal of a retired spelling.
fn retired_spelling (
  spelling : &str,
) -> String {
  format! ( "'{}' is a retired metadata spelling; the grammar is {}",
            spelling, METADATA_GRAMMAR ) }

/// The kind a headline's atom names, before the parse reads the parts
/// beside it: a kind with parts of its own by name, any other whole.
enum NamedKind {
  Occurrence,
  Ghost,
  GnidOnly (GnidOnlyKind),
  Alias,
  ID,
  WithoutParts (MetadataKind),
}

impl NamedKind {
  /// The atom the kind is known by ('MetadataKind::atom').
  fn atom (&self) -> &'static str {
    match self {
      NamedKind::Occurrence     => "node",
      NamedKind::Ghost          => "ghost",
      NamedKind::GnidOnly (kind) => kind . wire_atom (),
      NamedKind::Alias          => "alias",
      NamedKind::ID             => "id",
      NamedKind::WithoutParts (kind) => kind . atom (), } }

  /// The parts the kind takes that 'shared/metadata-fields.json' does
  /// not list for it, the table listing only what a user sets: those
  /// the server writes, and, on a kind whose only request is a
  /// readiness request, the '(editRequest ...)' that holds it, which
  /// the readiness commands write.
  fn parts_the_table_leaves_out (&self) -> &'static [&'static str] {
    match self {
      NamedKind::Occurrence =>
        &[ "gnid", "skgRepos", "overridesHere", "view", "diff" ],
      NamedKind::Ghost =>
        &[ "gnid", "skgRepos", "view", "diff" ],
      NamedKind::GnidOnly (_) =>
        &[ "gnid", "skgRepos", "diff" ],
      NamedKind::Alias =>
        &[ "skgRepos", "view", "diff" ],
      NamedKind::ID
        | NamedKind::WithoutParts ( MetadataKind::Flag (_) ) =>
        &[ "editRequest", "view", "diff" ],
      NamedKind::WithoutParts ( MetadataKind::ChangeViewnode (
        ChangeViewnode::BodyDiff { .. } )) => &[],
      NamedKind::WithoutParts ( MetadataKind::ChangeViewnode (_) ) =>
        &[ "editRequest", "diff" ],
      NamedKind::WithoutParts (_) => &[], } }

  /// Whether the kind takes the part 'name': a field the table lists
  /// for it, or a part the table leaves out. The one rule that
  /// refuses a part the kind does not take.
  fn takes_part (
    &self,
    name : &str,
  ) -> bool {
    kind_takes_field ( self . atom (), name )
    || self . parts_the_table_leaves_out () . contains (&name) }
}

/// The one edit request '(editRequest ...)' holds beside a readiness
/// request, as written, before the kind's variant takes it.
enum WrittenEditRequest {
  ForAnOccurrence (OccurrenceEditRequest),
  RestoreNode, // a ghost's
}

impl WrittenEditRequest {
  /// The request's value in 'shared/metadata-fields.json'.
  fn value_in_the_table (&self) -> &'static str {
    match self {
      WrittenEditRequest::ForAnOccurrence (
        OccurrenceEditRequest::OfTheNode (request) ) => match request {
          NodeEditRequest::Delete         => "delete",
          NodeEditRequest::NodeMerge (_)  => "merge",
          NodeEditRequest::SetFlag { .. } => "flag", },
      WrittenEditRequest::ForAnOccurrence (
        OccurrenceEditRequest::OfTheRelationship (request) ) => match request {
          RelationshipEditRequest::RelRepo (_) => "relRepo",
          RelationshipEditRequest::RestoreRel  => "restoreRel", },
      WrittenEditRequest::RestoreNode => "restoreNode", } }
}

/// The parts a headline writes beside its kind, read before the
/// kind's variant takes them.
#[derive(Default)]
struct WrittenParts {
  gnid           : Option<ID>,
  skgRepos       : Option<( Option<SkgrepoName>,   // its nodeRepo
                            Option<SkgrepoName> )>, // its relRepo
  noClaim        : bool,
  writeProtected : bool,
  overridesHere  : Option<ID>,
  edit_request   : Option<WrittenEditRequest>,
  view_requests  : HashSet<ViewRequest>,
}

/// Parse metadata from org-mode headline into ViewnodeMetadata.
/// See file header comment for full syntax.
/// In order: the elements are sorted into the kind and its parts; the
/// one rule ('NamedKind::takes_part') refuses a part the kind does not
/// take; the parts are read; the kind's variant takes them.
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
  let mut named : Option<NamedKind> = None;
  let mut parts : Vec<(String, &Sexp)> = Vec::new ();
  for element in elements {
    let (name, kind_named) : (String, Option<NamedKind>) =
      classify_metadata_element (element, &mut result) ?;
    if let Some (kind_named) = kind_named {
      if let Some (earlier) = &named {
        return Err ( format! ( "A headline has one kind; this one names {} and {}",
                               earlier . atom (), kind_named . atom () )); }
      named = Some (kind_named);
    } else if ! name . is_empty () {
      if parts . iter () . any ( |(seen, _)| *seen == name ) {
        return Err ( format! ( "{} may appear only once", name )); }
      parts . push ( (name, element) ); }}
  let named : NamedKind = named . unwrap_or (NamedKind::Occurrence);
  for (name, _) in &parts {
    if ! named . takes_part (name) {
      return Err ( format! ( "A headline of kind {} does not take {}",
                             named . atom (), name )); }}
  let mut written : WrittenParts = WrittenParts::default ();
  for (name, element) in &parts {
    read_metadata_part (&named, name, element, &mut written, &mut result) ?; }
  result . kind = kind_with_its_parts (named, written, &mut result . viewStats) ?;
  Ok (result) }

/// What one element of '(skg ...)' is: a position atom or the
/// viewnode ID, read into 'metadata' at once (the name returned is
/// empty); the kind (returned second, with its parameters read); or
/// a part, whose name is returned, to be read once the kind is
/// known. Refuses a retired spelling and anything unknown.
fn classify_metadata_element (
  element  : &Sexp,
  metadata : &mut ViewnodeMetadata,
) -> Result<(String, Option<NamedKind>), String> {
  let part = |name : &str| -> Result<(String, Option<NamedKind>), String> {
    Ok (( name . to_string (), None )) };
  let a_kind = |kind : NamedKind| -> Result<(String, Option<NamedKind>), String> {
    Ok (( String::new (), Some (kind) )) };
  let whole = |kind : MetadataKind| -> Result<(String, Option<NamedKind>), String> {
    Ok (( String::new (), Some ( NamedKind::WithoutParts (kind) ) )) };
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
        "node"       => a_kind (NamedKind::Occurrence),
        "ghost"      => a_kind (NamedKind::Ghost),
        "unknown"    => a_kind ( NamedKind::GnidOnly (GnidOnlyKind::Unknown) ),
        "restricted" => a_kind ( NamedKind::GnidOnly (GnidOnlyKind::Restricted) ),
        "alias"      => a_kind (NamedKind::Alias),
        "id"         => a_kind (NamedKind::ID),
        "tombstone"  => whole (MetadataKind::Tombstone),
        "forestRoot" => whole (MetadataKind::BufferRoot),
        "bodyDiff"   => whole ( MetadataKind::ChangeViewnode (
          ChangeViewnode::BodyDiff { diff : String::new () } )),
        "nonMember" | "restrictedNode" | "omittedBody" =>
          Err ( retired_spelling (&atom) ),
        folder_atom if Folder::from_client_string (folder_atom) . is_some () =>
          whole ( MetadataKind::Folder (
            Folder::from_client_string (folder_atom) . expect ("a folder") )),
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
          whole ( MetadataKind::ChangeViewnode ( ChangeViewnode::OldText {
            title : title_changed . then (String::new),
            body  : body_changed  . then (String::new) } )) },
        "homeSkgrepoMove" | "relRepoMove" => {
          let skgrepo_change : SkgrepoChange =
            parse_old_and_new_skgrepos (&head, args) ?;
          whole ( MetadataKind::ChangeViewnode (
            if head == "homeSkgrepoMove" {
              ChangeViewnode::HomeSkgrepoMove { skgrepo_change }
            } else {
              ChangeViewnode::RelRepoMove { skgrepo_change } } )) },
        "changesetMarker" => {
          let name : String = single_argument (&head, args) ?;
          let kind : ChangesetMarkerKind =
            ChangesetMarkerKind::from_wire_name (&name)
            . ok_or_else ( || format! (
              "Unknown changesetMarker kind: {}", name )) ?;
          whole ( MetadataKind::ChangeViewnode (
            ChangeViewnode::ChangesetMarker { kind } )) },
        "flag" => {
          let name : String = single_argument (&head, args)
            . map_err ( |_| "flag requires exactly one flag name" . to_string () ) ?;
          let flag : Flag = Flag::from_wire_name (&name)
            . ok_or_else (|| format! ("Unknown flag: {}", name)) ?;
          whole ( MetadataKind::Flag (flag) ) },
        "node" | "ghost" | "unknown" | "deleted" | "historical"
          | "id" | "repo" | "relRepo" | "rels" | "viewStats"
          | "homeRepoHerald" | "readyRequest" | "unreadyRequest" =>
          Err ( retired_spelling ( &format! ( "({} ...)", head )) ),
        folder_atom if Folder::from_client_string (folder_atom) . is_some () =>
          Err ( retired_spelling ( &format! ( "({} ...)", folder_atom )) ),
        _ => Err ( format! ( "Unknown metadata key: {}", head )), }},
    _ => Err ( format! ( "Unexpected element in metadata sexp: {:?}",
                         element )), }}

/// Read the part named 'name', 'element', into 'written', or, for a
/// readiness request, into 'metadata'. The kind 'named' takes the part
/// ('NamedKind::takes_part'); an edit request the table does not list
/// for it is refused here.
fn read_metadata_part (
  named    : &NamedKind,
  name     : &str,
  element  : &Sexp,
  written  : &mut WrittenParts,
  metadata : &mut ViewnodeMetadata,
) -> Result<(), String> {
  let args : &[Sexp] = match element {
    Sexp::List (items) => &items[1..],
    _ => &[], };
  match name {
    "noClaim" =>
      // the headline claims no membership; a claiming one carries nothing
      written . noClaim = true,
    "writeProtected" =>
      written . writeProtected = true,
    "view" | "diff" => {}, // derived facts, skipped whole
    "gnid" =>
      written . gnid = Some ( ID::from ( single_argument (name, args) ? )),
    "skgRepos" =>
      written . skgRepos = Some ( parse_skgRepos (args) ? ),
    "overridesHere" =>
      written . overridesHere = Some ( ID::from (
        single_argument (name, args) ? )),
    "editRequest" => {
      let (request, readiness_request)
        : (Option<WrittenEditRequest>, Option<ReadinessRequest>)
        = parse_editrequest_sexp (args) ?;
      if let Some (request) = &request {
        check_the_kind_takes_the_edit_request (named . atom (), request) ?; }
      written . edit_request = request;
      metadata . readiness_request = readiness_request; },
    "viewRequests" =>
      parse_viewrequests_sexp ( args, &mut written . view_requests ) ?,
    other => return Err ( format! ( "Unknown metadata part: {}", other )), }
  Ok (( )) }

/// Refuses an edit request that 'shared/metadata-fields.json' does not
/// list for the kind known by 'kind_atom': a restore request with the
/// words the clients show, any other with what the kind takes.
fn check_the_kind_takes_the_edit_request (
  kind_atom : &str,
  request   : &WrittenEditRequest,
) -> Result<(), String> {
  if kind_takes_value ( kind_atom, "editRequest",
                        request . value_in_the_table () ) {
    return Ok (( )); }
  Err ( match request {
    WrittenEditRequest::RestoreNode =>
      RESTORE_NODE_ON_A_NODE_NOT_DELETED . to_string (),
    WrittenEditRequest::ForAnOccurrence (
      OccurrenceEditRequest::OfTheRelationship (
        RelationshipEditRequest::RestoreRel )) =>
      if kind_atom == "ghost" { RESTORE_REL_ON_A_DELETED_NODE . to_string () }
      else { RESTORE_REL_WITHOUT_A_REMOVED_MEMBERSHIP . to_string () },
    _ => edit_request_refusal (kind_atom), } ) }

/// The refusal of an edit request the kind known by 'kind_atom' does
/// not take, naming what it takes.
fn edit_request_refusal (
  kind_atom : &str,
) -> String {
  let mut taken : Vec<String> =
    values_written_for_a_message (kind_atom, "editRequest");
  taken . push ( "a readiness request" . to_string () );
  format! ( "A headline of kind {} accepts no edit request but {}",
            kind_atom, taken . join (" and ") ) }

/// The kind 'named' with the parts 'written' gives it: one arm per
/// kind, each taking the parts its variant carries. An occurrence's
/// relRepo, a derived fact, goes to 'viewStats'.
fn kind_with_its_parts (
  named     : NamedKind,
  written   : WrittenParts,
  viewStats : &mut ViewnodeStats,
) -> Result<MetadataKind, String> {
  let atom : &'static str = named . atom ();
  let claimsMembership : bool = ! written . noClaim;
  match named {
    NamedKind::Occurrence => {
      let (nodeRepo, relRepo) : (Option<SkgrepoName>, Option<SkgrepoName>) =
        written . skgRepos . unwrap_or ((None, None));
      viewStats . relRepo = relRepo;
      Ok ( MetadataKind::Occurrence ( OccurrenceMetadata {
        gnid           : written . gnid,
        home_skgrepo   : nodeRepo,
        claimsMembership,
        writeProtected : written . writeProtected,
        overridesHere  : written . overridesHere,
        edit_request   : match written . edit_request {
          None => None,
          Some (WrittenEditRequest::ForAnOccurrence (request)) => Some (request),
          Some (WrittenEditRequest::RestoreNode) => return Err (
            RESTORE_NODE_ON_A_NODE_NOT_DELETED . to_string () ), },
        view_requests  : written . view_requests, } )) },
    NamedKind::Ghost => {
      let home_skgrepo : Option<SkgrepoName> =
        match written . skgRepos {
          None => None,
          Some (( Some (nodeRepo), None )) => Some (nodeRepo),
          Some (_) => return Err ( "A ghost's skgRepos holds only (nodeRepo R)"
                                   . to_string () ), };
      let restoreNode_request : bool =
        match written . edit_request {
          None => false,
          Some (WrittenEditRequest::RestoreNode) => true,
          Some (_) => return Err ( edit_request_refusal (atom) ), };
      let gnid : ID = written . gnid . ok_or_else (
        || "A ghost requires its (gnid X)" . to_string () ) ?;
      Ok ( MetadataKind::Ghost ( GhostMetadata {
        gnid, home_skgrepo, claimsMembership, restoreNode_request } )) },
    NamedKind::GnidOnly (kind) => {
      let relRepo : Option<SkgrepoName> =
        relRepo_alone (atom, written . skgRepos) ?;
      let relRepo_request : Option<SkgrepoName> =
        relRepo_request_alone (atom, written . edit_request) ?;
      let gnid : ID = written . gnid . ok_or_else ( || match kind {
        GnidOnlyKind::Unknown    => "An unknown viewnode requires its (gnid X)",
        GnidOnlyKind::Restricted => "A restricted viewnode requires its (gnid X)", }
        . to_string () ) ?;
      Ok ( MetadataKind::GnidOnly ( GnidOnlyMetadata {
        kind, gnid, relRepo, relRepo_request, claimsMembership } )) },
    NamedKind::Alias =>
      Ok ( MetadataKind::Alias ( AliasMetadata {
        relRepo         : relRepo_alone (atom, written . skgRepos) ?,
        relRepo_request : relRepo_request_alone (atom, written . edit_request) ?,
        claimsMembership } )),
    NamedKind::ID =>
      Ok ( MetadataKind::ID { claimsMembership } ),
    NamedKind::WithoutParts (kind) =>
      Ok (kind), } }

/// The record's relRepo, from the '(skgRepos ...)' of a headline of the
/// kind known by 'kind_atom', which holds only that.
fn relRepo_alone (
  kind_atom : &str,
  skgRepos  : Option<(Option<SkgrepoName>, Option<SkgrepoName>)>,
) -> Result<Option<SkgrepoName>, String> {
  match skgRepos {
    None => Ok (None),
    Some (( None, Some (relRepo) )) => Ok ( Some (relRepo) ),
    Some (_) => Err ( format! (
      "The skgRepos of a headline of kind {} holds only (relRepo R)",
      kind_atom )), } }

/// The relRepo request of a headline of the kind known by 'kind_atom',
/// whose only edit request it is.
fn relRepo_request_alone (
  kind_atom    : &str,
  edit_request : Option<WrittenEditRequest>,
) -> Result<Option<SkgrepoName>, String> {
  match edit_request {
    None => Ok (None),
    Some ( WrittenEditRequest::ForAnOccurrence (
      OccurrenceEditRequest::OfTheRelationship (
        RelationshipEditRequest::RelRepo (skgrepo) ))) => Ok ( Some (skgrepo) ),
    Some (_) => Err ( edit_request_refusal (kind_atom) ), } }

/// What the parser says to a restoreRel on a ghost.
pub const RESTORE_REL_ON_A_DELETED_NODE : &str =
  "This node is deleted; restore it first (skg-restore-deleted-node)";
/// What the parser says to a restoreNode anywhere but on a ghost.
pub const RESTORE_NODE_ON_A_NODE_NOT_DELETED : &str =
  "Only a node deleted since HEAD can be restored; restoreNode belongs on a ghost (skg-restore-deleted-node)";
/// What the parser says to a restoreRel anywhere but on a removed
/// membership.
pub const RESTORE_REL_WITHOUT_A_REMOVED_MEMBERSHIP : &str =
  "Only a removed membership can be restored; restoreRel belongs on a removed member, or on a folder member carrying (rel removed) (skg-restore-removed-membership)";

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
) -> Result<(Option<WrittenEditRequest>, Option<ReadinessRequest>), String> {
  if items . is_empty () {
    return Err ( "editRequest requires a request" . to_string () ); }
  let mut edit_request : Option<WrittenEditRequest> = None;
  let mut readiness_request : Option<ReadinessRequest> = None;
  for element in items {
    if let Some (request) = readiness_request_of (element) ? {
      if readiness_request . is_some () {
        return Err ( "A viewnode may carry only one readiness request"
                     . to_string () ); }
      readiness_request = Some (request);
      continue; }
    if edit_request . is_some () {
      return Err ( "editRequest holds at most one request beside a readiness request"
                   . to_string () ); }
    edit_request = Some ( parse_one_edit_request (element) ? ); }
  Ok (( edit_request, readiness_request )) }

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
) -> Result<WrittenEditRequest, String> {
  let of_the_node = |request : NodeEditRequest| -> WrittenEditRequest {
    WrittenEditRequest::ForAnOccurrence (
      OccurrenceEditRequest::OfTheNode (request) ) };
  let of_the_relationship = |request : RelationshipEditRequest| -> WrittenEditRequest {
    WrittenEditRequest::ForAnOccurrence (
      OccurrenceEditRequest::OfTheRelationship (request) ) };
  match element {
    Sexp::List (subitems) if subitems . len () == 2 => {
      let key : String =
        atom_to_string ( &subitems[0] ) ?;
      if key == "merge" {
        let id_str : String =
          atom_to_string ( &subitems[1] ) ?;
        Ok ( of_the_node ( NodeEditRequest::NodeMerge ( ID::from (id_str))))
      } else if key == "relRepo" {
        let skgrepo : String = atom_to_string ( &subitems[1] ) ?;
        Ok ( of_the_relationship ( RelationshipEditRequest::RelRepo (
          SkgrepoName::from (skgrepo) )))
      } else {
        Err ( format! ( "Unknown editRequest key: {}", key )) } },
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
      Ok ( of_the_node ( NodeEditRequest::SetFlag { flag, value } ))
    },
    Sexp::Atom (_) => {
      let bare_value : String =
        atom_to_string (element) ?;
      match bare_value . as_str () {
        "delete"      => Ok ( of_the_node (NodeEditRequest::Delete) ),
        "restoreNode" => Ok (WrittenEditRequest::RestoreNode),
        "restoreRel"  => Ok ( of_the_relationship (RelationshipEditRequest::RestoreRel) ),
        _ => Err ( format! ( "Unknown editRequest value: {}",
                             bare_value )), } },
    _ => Err ( "Unexpected element in editRequest" . to_string () ), } }

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
