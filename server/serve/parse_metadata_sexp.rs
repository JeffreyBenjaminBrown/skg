/// PURPOSE: Parse (skg ...) metadata s-expressions from org headlines.
///
/// Format:
///   Non-vognodes: (skg [focused] [folded] nonVognodeKind)
///   UnrestrictedVognodes: (skg [focused] [folded]
///                   (node [(id ID)]
///                         [(repo REPO)]
///                         [noClaim]          ; the occurrence claims no membership
///                         [writeProtected]   ; marks the occurrence write-protected
///                         [cycle]
///                         [(stats [containsParent]
///                                 [(containers N)]
///                                 [(contents N)]
///                                 [(linksIn N)])]
///                         [(editRequest <delete | (merge ID) | (relRepo REPO)
///                                       | (flag FLAG true|false)
///                                       | restoreRel>)]
///                         [(viewRequests REQUEST...)]
///                         [(diff [(node added|removed)]
///                                [(rel added|removed)]
///                                [(properties changed)]
///                                [(leaves N OBJECT...)]
///                                [(ready OBJECT...)]
///                                [(readyRequest OBJECT...) | (unreadyRequest OBJECT...)])]))
///   where OBJECT is node, rel or all. The '(diff ...)' form may also
///   appear in (deleted ...), (unknown ...) and (historical ...), and
///   at top level on an alias, id or flag property and on the
///   diff view's properties other than bodyDiff. A changed folder,
///   '(KIND changed)', may carry '(diff (ready))' at top level, and
///   nothing else in it.
///   'restoreRel' is valid only on a node carrying '(rel removed)': a
///   removed member, or a folder member whose relationship to the
///   folder's viewparent was removed. Which of those the headline is
///   depends on where it stands, which the save checks
///   ('validate_restoreRel_position', local.rs). It may sit beside
///   'writeProtected'.
///   The diff view's own viewnodes:
///     (historical (id ID) (repo REPO) [(editRequest restoreNode)] ...)
///                                            a historical phantom
///     (oldText titleAndBody | title | body)  the text at HEAD
///     bodyDiff
///     (homeSkgrepoMove (old REPO) (new REPO))
///     (relRepoMove (old REPO) (new REPO))
///     (changesetMarker content | relRepo | orderOnly)
///     acquiredByFolder, acquiredFolder
///   Every kind may end with its viewnode ID, '(vnid X)', X being one or
///   more lowercase letters and digits.

use crate::types::sexp::atom_to_string;
use crate::types::misc::{ID, SkgrepoChange, SkgrepoName};
use crate::types::errors::{BufferValidationError, HeadlineName, with_org_subheadlines};
use crate::types::nodes::complete::Flag;
use crate::types::viewnode::{
  ChangeFacts, DiffObject, Existence, FoldAndFocus, ReadinessRequest,
  GraphnodeStats, ViewnodeStats, NodeEditRequest, ViewRequest, FolderRelation,
  ChangesetMarkerKind, PropertyMember, ChangeViewnode, Folder, PhantomDeleted,
  PhantomHistorical, PhantomUnknown,
  Editability, RelationshipEditRequest, ViewnodeId,
};
use crate::types::maybe_placed_viewnode::{
    MpViewnode, MpViewnodeKind, MpUnrestrictedVognode,
    MpVognode, MpPhantom,
};

use sexp::Sexp;
use std::collections::{BTreeSet, HashSet};

//
// Parsing-internal types
//

/// Intermediate parsed metadata. Converted to Viewnode via viewnode_from_metadata().
#[derive(Debug, Clone, PartialEq)]
pub struct ViewnodeMetadata {
  pub foldAndFocus: FoldAndFocus,
  pub viewnode_id: Option<ViewnodeId>,
  // None means vognode, Some means non-vognode.
  pub non_vognode: Option<MpViewnodeKind>,
  // UnrestrictedVognode fields (ignored if non-vognode is Some)
  pub skgid: Option<ID>,
  pub home_skgrepo: Option<SkgrepoName>,
  pub claimsMembership: bool,
  pub writeProtected: bool,
  pub graphStats: GraphnodeStats,
  pub viewStats: ViewnodeStats,
  pub overridesHere: Option<ID>,
  pub edit_request: Option<NodeEditRequest>,
  pub relRepo_request: Option<SkgrepoName>,
  pub view_requests: HashSet<ViewRequest>,
  // The parts of the diff view's '(diff ...)' form, its facts and
  // readiness request, on whichever vognode, phantom or property
  // leaf carries it.
  pub change_facts: ChangeFacts,
  pub readiness_request: Option<ReadinessRequest>,
  pub property_relRepo : Option<SkgrepoName>,
  pub property_relRepo_request : Option<SkgrepoName>,
  // When true, this is a PhantomDeleted (id and skgrepo are used).
  pub is_deleted_node: bool,
  // When true, this is a PhantomHistorical (id and skgrepo are used).
  pub is_historical_node: bool,
  // When true, this is a tombstone.
  pub is_tombstone: bool,
  // When Some, this is a PhantomUnknown (a phantom for a missing
  // referent). Carries only the id; no skgrepo/title/body apply.
  pub unknown_node_skgid: Option<ID>,
  pub unknown_relRepo : Option<SkgrepoName>,
  pub unknown_relRepo_request : Option<SkgrepoName>,
}

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
    change_facts: ChangeFacts::default(),
    readiness_request: None,
    property_relRepo : None,
    property_relRepo_request : None,
    is_deleted_node: false,
    is_historical_node: false,
    is_tombstone: false,
    unknown_node_skgid: None,
    unknown_relRepo: None,
    unknown_relRepo_request: None, }}

/// Create an MpViewnode from parsed metadata components.
/// This is the bridge between parsing (ViewnodeMetadata) and runtime (MpViewnode).
/// Returns (MpViewnode, error, warning):
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
) -> ( MpViewnode, Option < BufferValidationError >, Option < String > ) {
  let headline : HeadlineName =
    HeadlineName::of (
      metadata . viewnode_id . as_ref (),
      metadata . skgid . as_ref ()
        . or ( metadata . unknown_node_skgid . as_ref () ),
      &title );
  let (kind, error, warning)
    : (MpViewnodeKind, Option<BufferValidationError>, Option<String>)
    = if let Some (ref uid) = metadata . unknown_node_skgid {
        ( MpViewnodeKind::Vognode (MpVognode::Phantom (
            MpPhantom::Unknown (
              PhantomUnknown {
                skgid           : uid . clone (),
                relRepo         : metadata . unknown_relRepo . clone (),
                relationshipEditRequest : metadata . unknown_relRepo_request . clone ()
                  . map (RelationshipEditRequest::RelRepo),
              } ) )),
          if body . is_some () || ! title . is_empty () {
            Some ( BufferValidationError::Other ( with_org_subheadlines (
              "Unknown phantom content cannot be edited.",
              &[ headline . clone () ] )))
          } else { None },
          None )
      } else if metadata . is_tombstone {
        ( MpViewnodeKind::Tombstone, None, None )
      } else if metadata . is_historical_node {
        ( MpViewnodeKind::Vognode (MpVognode::Phantom (
            MpPhantom::Historical ( PhantomHistorical {
              skgid : metadata . skgid . clone ()
                      . unwrap_or_else ( || ID::from ("")),
              home_skgrepo : metadata . home_skgrepo . clone ()
                             . unwrap_or_else ( || SkgrepoName::from ("")),
              title,
              body,
              restoreNode_request : matches! (
                metadata . edit_request,
                Some (NodeEditRequest::RestoreNode) ),
            } ) )), None, None )
      } else if metadata . is_deleted_node {
        ( MpViewnodeKind::Vognode (MpVognode::Phantom (
            MpPhantom::Deleted ( PhantomDeleted {
            skgid     : metadata . skgid . clone ()
                       . unwrap_or_else ( || ID::from ("")),
            home_skgrepo : metadata . home_skgrepo . clone ()
                       . unwrap_or_else ( || SkgrepoName::from ("")),
            title,
            body,
          } ) )), None, None )
      } else if let Some ( ref non_vognode ) = metadata . non_vognode {
        let is_flags_folder = matches! (non_vognode,
          MpViewnodeKind::Folder (Folder::Flags { .. }));
        let is_flag = matches! (non_vognode,
          MpViewnodeKind::PropertyMember (PropertyMember::Flag { .. }));
        let carries_text : bool = // the diff view's text properties
          matches! (non_vognode,
            MpViewnodeKind::ChangeViewnode (
              ChangeViewnode::OldText { .. } | ChangeViewnode::BodyDiff { .. } ));
        let error : Option<BufferValidationError> =
          if body . is_some () && ! is_flags_folder && ! is_flag
             && ! carries_text {
            Some ( BufferValidationError::Body_of_NonVognode (
              title . clone (),
              maybeplaced_kind_error_label (non_vognode),
              headline . clone () ))
          } else { None };
        let folder_title_warning : Option<String> =
          if ! title . is_empty ()
            && ! is_flags_folder
            && matches! ( non_vognode,
                          MpViewnodeKind::Folder (_) )
          { Some ( with_org_subheadlines (
              &format! ( "Headline text on a {} is not saved; discarded: {:?}",
                         maybeplaced_kind_error_label (non_vognode),
                         title ),
              &[ headline . clone () ] )) }
          else { None };
        let non_vognode_with_title : MpViewnodeKind = match non_vognode {
          // Use headline title for string
          MpViewnodeKind::PropertyMember (PropertyMember::Alias { .. }) =>
            MpViewnodeKind::PropertyMember (PropertyMember::Alias {
                              text: title . clone (),
                              relRepo:
                                metadata . property_relRepo . clone (),
                              relationshipEditRequest:
                                metadata . property_relRepo_request . clone ()
                                . map (RelationshipEditRequest::RelRepo) }),
          MpViewnodeKind::PropertyMember (PropertyMember::ID { .. }) =>
            MpViewnodeKind::PropertyMember (PropertyMember::ID {
                              skgid: title . clone () . into () }),
          MpViewnodeKind::PropertyMember (PropertyMember::Flag { flag, .. }) =>
            MpViewnodeKind::PropertyMember (PropertyMember::Flag {
              flag : *flag,
              title    : title . clone (),
              body     : body . clone () }),
          MpViewnodeKind::Folder (Folder::Flags { .. }) =>
            MpViewnodeKind::Folder (Folder::Flags {
              title : title . clone (), body : body . clone () }),
          // The parser marks which aspects an OldText shows; its text
          // comes from the headline and the body.
          MpViewnodeKind::ChangeViewnode (ChangeViewnode::OldText {
            title : title_changed, body : body_changed, .. }) =>
            MpViewnodeKind::ChangeViewnode (ChangeViewnode::OldText {
              title : title_changed . as_ref () . map ( |_| title . clone () ),
              body  : body_changed . as_ref ()
                      . map ( |_| body . clone () . unwrap_or_default () ) }),
          MpViewnodeKind::ChangeViewnode (ChangeViewnode::BodyDiff { .. }) =>
            MpViewnodeKind::ChangeViewnode (ChangeViewnode::BodyDiff {
              diff : body . clone () . unwrap_or_default () }),
          MpViewnodeKind::ChangeViewnode (ChangeViewnode::HomeSkgrepoMove { skgrepo_change, .. }) =>
            MpViewnodeKind::ChangeViewnode (ChangeViewnode::HomeSkgrepoMove {
              skgrepo_change    : skgrepo_change . clone () }),
          MpViewnodeKind::ChangeViewnode (ChangeViewnode::RelRepoMove { skgrepo_change, .. }) =>
            MpViewnodeKind::ChangeViewnode (ChangeViewnode::RelRepoMove {
              skgrepo_change    : skgrepo_change . clone () }),
          MpViewnodeKind::ChangeViewnode (ChangeViewnode::ChangesetMarker { kind, .. }) =>
            MpViewnodeKind::ChangeViewnode (ChangeViewnode::ChangesetMarker {
              kind : *kind }),
          other => other . clone () };
        ( non_vognode_with_title, error, folder_title_warning )
      } else {
      // MpUnrestrictedVognode
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
        let t : MpUnrestrictedVognode = MpUnrestrictedVognode {
            title,
            skgid            : metadata . skgid . clone (),
            home_skgrepo     : metadata . home_skgrepo . clone (),
            claimsMembership : metadata . claimsMembership,
            overridesHere    : metadata . overridesHere . clone (),
            graphStats       : metadata . graphStats . clone (),
            viewStats        : metadata . viewStats . clone (),
            relationshipEditRequest,
            view_requests    : metadata . view_requests . clone (),
            editability, };
        let node_kind : MpViewnodeKind =
          MpViewnodeKind::Vognode ( MpVognode::Unrestricted (t) );
        ( node_kind,
          error, None ) }
    };
  let never_carries_a_change : bool = // so the facts parsed are dropped
    matches! ( kind,
               MpViewnodeKind::Folder (_)
               | MpViewnodeKind::BufferRoot
               | MpViewnodeKind::Tombstone
               | MpViewnodeKind::ChangeViewnode (ChangeViewnode::BodyDiff { .. }) );
  ( MpViewnode { foldAndFocus      : metadata . foldAndFocus,
                 kind,
                 change_facts      :
                   if never_carries_a_change { ChangeFacts::default () }
                   else { metadata . change_facts . clone () },
                 readiness_request :
                   if never_carries_a_change { None }
                   else { metadata . readiness_request . clone () },
                 viewnode_id       : metadata . viewnode_id . clone () },
    error, warning ) }

fn maybeplaced_kind_error_label (
  kind : &MpViewnodeKind,
) -> String {
  match kind {
    MpViewnodeKind::Folder (folder) =>
      folder . repr_in_client () . to_string (),
    MpViewnodeKind::PropertyMember (member) =>
      member . repr_in_client () . to_string (),
    MpViewnodeKind::ChangeViewnode (change) =>
      change . repr_in_client () . to_string (),
    MpViewnodeKind::BufferRoot =>
      "forestRoot" . to_string (),
    MpViewnodeKind::Tombstone =>
      "tombstone" . to_string (),
    MpViewnodeKind::Vognode (_) =>
      MpViewnode { kind : kind . clone (),
                   .. MpViewnode::default () }
      . error_label (), } }


/// Parse metadata from org-mode headline into ViewnodeMetadata.
/// See file header comment for full syntax.
pub fn parse_metadata_to_viewnodemd (
  sexp_str : &str
) -> Result<ViewnodeMetadata, String> {
  let mut result : ViewnodeMetadata =
    default_metadata ();

  let parsed : Sexp =
    sexp::parse (sexp_str)
    . map_err ( |e| format! ( "Failed to parse metadata as s-expression: {}", e ) ) ?;

  // Extract the list of elements from (skg ...)
  let elements : &[Sexp] =
    match &parsed {
      Sexp::List (items) => {
        // First element should be the symbol 'skg'
        if items . is_empty () {
          return Err ( "Empty metadata s-expression" . to_string () ); }
        // Skip the 'skg' symbol and return the rest
        &items[1..]
      },
      _ => return Err ( "Expected metadata to be a list" . to_string () ),
    };

  let mut top_level_diff : bool = false;
  // Process each element
  for element in elements {
    match element {
      Sexp::List (items) if items . len () >= 1 => {
        let first : String =
          atom_to_string ( &items[0] ) ?;
        match first . as_str () {
          "node" => {
            parse_node_sexp ( &items[1..], &mut result ) ?; },
          "deleted" => {
            result . is_deleted_node = true;
            parse_deleted_sexp ( &items[1..], &mut result ) ?; },
          "historical" => {
            // Same grammar as (deleted ...).
            result . is_historical_node = true;
            parse_deleted_sexp ( &items[1..], &mut result ) ?; },
          "oldText" => {
            let aspect : String = single_argument (&first, &items[1..]) ?;
            let (title_changed, body_changed) : (bool, bool) =
              match aspect . as_str () {
                "titleAndBody" => (true,  true),
                "title"        => (true,  false),
                "body"         => (false, true),
                other => return Err ( format! (
                  "Unknown oldText aspect: {}", other )), };
            result . non_vognode = Some ( MpViewnodeKind::ChangeViewnode (
              ChangeViewnode::OldText {
                title : title_changed . then (String::new),
                body  : body_changed  . then (String::new) } )); },
          "homeSkgrepoMove" | "relRepoMove" => {
            let skgrepo_change : SkgrepoChange =
              parse_old_and_new_skgrepos (&first, &items[1..]) ?;
            result . non_vognode = Some ( MpViewnodeKind::ChangeViewnode (
              if first == "homeSkgrepoMove" {
                ChangeViewnode::HomeSkgrepoMove { skgrepo_change }
              } else {
                ChangeViewnode::RelRepoMove { skgrepo_change } } )); },
          "changesetMarker" => {
            let name : String = single_argument (&first, &items[1..]) ?;
            let kind : ChangesetMarkerKind =
              ChangesetMarkerKind::from_wire_name (&name)
              . ok_or_else ( || format! (
                "Unknown changesetMarker kind: {}", name )) ?;
            result . non_vognode = Some ( MpViewnodeKind::ChangeViewnode (
              ChangeViewnode::ChangesetMarker { kind } )); },
          "unknown" => {
            // (unknown (id X)) -- placeholder for a referenced
            // node with no record anywhere. No skgrepo/title/body.
            parse_unknownnode_sexp ( &items[1..], &mut result ) ?; },
          "relRepo" => {
            if items . len () != 2 {
              return Err (
                "relRepo requires exactly one repo name"
                . to_string () ); }
            if result . property_relRepo . is_some () {
              return Err ( "Alias relRepo may appear only once"
                           . to_string () ); }
            result . property_relRepo = Some ( SkgrepoName::from (
              atom_to_string (&items [1]) ? )); },
          "editRequest" => {
            let mut request_metadata : ViewnodeMetadata = default_metadata ();
            parse_editrequest_sexp (
              &items[1..], &mut request_metadata ) ?;
            if let Some (message) = misplaced_restore_request_message (
                 &request_metadata . edit_request, false ) {
              return Err (message); }
            if request_metadata . edit_request . is_some () {
              return Err ( "Only Alias may carry a top-level editRequest relRepo"
                           . to_string () ); }
            if result . property_relRepo_request . is_some () {
              return Err ( "Alias editRequest may appear only once"
                           . to_string () ); }
            result . property_relRepo_request =
              request_metadata . relRepo_request; },
          "vnid" => {
            if result . viewnode_id . is_some () {
              return Err ( "a viewnode ID may appear only once" . to_string () ); }
            result . viewnode_id = Some ( ViewnodeId::parse (
              & single_argument (&first, &items[1..]) ? ) ? ); },
          "diff" => {
            // At top level, valid only on a property leaf, and on a
            // changed folder holding only '(ready)' (checked below).
            parse_diff_form ( &items[1..], &mut result ) ?;
            top_level_diff = true; },
          "flag" => {
            if items . len () != 2 {
              return Err ("flag requires exactly one flag name"
                          . to_string ()); }
            let name : String = atom_to_string (&items[1]) ?;
            let flag : Flag = Flag::from_wire_name (&name)
              . ok_or_else (|| format! ("Unknown flag: {}", name)) ?;
            result . non_vognode = Some (MpViewnodeKind::PropertyMember (
              PropertyMember::Flag {
                flag, title: String::new (), body: None })); },
          // Note: "alias" as a list like (alias "string") is no longer supported.
          // Use bare "alias" atom instead - the alias string comes from headline title.
          // Legacy format detection - reject with helpful error
          "id" | "repo" | "view" | "code" => {
            return Err ( format! (
              "Legacy metadata format detected (found '{}' at top level). \
               The new format uses (skg [focused] [folded] (node ...)) for UnrestrictedVognodes \
               and (skg [focused] [folded] nonVognodeKind) for Non-vognodes.",
              first )); },
          folder_atom if folder_kind_of_atom (folder_atom) . is_some () => {
            // '(KIND changed)': a folder the diff view shows
            // because of a change. A display fact, like its '(diff
            // (ready))'.
            if items . len () != 2
              || atom_to_string (&items[1]) ? != "changed" {
              return Err ( format! (
                "A folder form must be ({} changed)", folder_atom )); }
            result . non_vognode = folder_kind_of_atom (folder_atom); },
          _ => { return Err ( format! ( "Unknown metadata key: {}",
                                         first )); }} },
      Sexp::Atom (_) => {
        let bare_value : String =
          atom_to_string (element) ?;
        match bare_value . as_str () {
          "focused"  => result . foldAndFocus . focused = true,
          "folded"   => result . foldAndFocus . folded = true,
          "bodyFolded" => result . foldAndFocus . bodyFolded = true,
          // Non-vognode kinds as bare atoms (alias/id string comes from title in viewnode_from_metadata)
          "alias"    => result . non_vognode = Some ( MpViewnodeKind::PropertyMember ( PropertyMember::Alias { text: String::new(), relRepo: None, relationshipEditRequest: None } ) ),
          "forestRoot" => result . non_vognode = Some (MpViewnodeKind::BufferRoot),
          "bodyDiff" =>
            result . non_vognode = Some (MpViewnodeKind::ChangeViewnode (
              ChangeViewnode::BodyDiff { diff : String::new () } )),
          "id" =>
            result . non_vognode = Some ( MpViewnodeKind::PropertyMember ( PropertyMember::ID { skgid: ID::default() } ) ),
          "tombstone" => result . is_tombstone = true,
          folder_atom if folder_kind_of_atom (folder_atom) . is_some () =>
            result . non_vognode = folder_kind_of_atom (folder_atom),
          _ => {
            return Err ( format! ( "Unknown top-level value: {}",
                                    bare_value )); }} },
      _ => { return Err ( format! (
        "Unexpected element in metadata sexp: {}",
        sexp_str )); }} }
  if ( result . property_relRepo . is_some ()
       || result . property_relRepo_request . is_some () )
     && ! matches! ( result . non_vognode,
                     Some (MpViewnodeKind::PropertyMember (PropertyMember::Alias { .. })) )
  { return Err ( "relRepo and its editRequest are valid only on Alias properties"
                 . to_string () ); }
  let is_folder : bool = matches! ( result . non_vognode,
    Some ( MpViewnodeKind::Folder (_) ));
  let holds_only_an_objectless_ready : bool =
    result . change_facts == ChangeFacts {
      ready : Some (BTreeSet::new ()), .. ChangeFacts::default () }
    && result . readiness_request . is_none ();
  if top_level_diff && is_folder && ! holds_only_an_objectless_ready
  { return Err ( "A folder's diff may hold only an objectless (ready)"
                 . to_string () ); }
  if top_level_diff && ! is_folder
     && ! matches! ( result . non_vognode,
                     Some ( MpViewnodeKind::PropertyMember (_)
                            | MpViewnodeKind::ChangeViewnode (
                              ChangeViewnode::OldText { .. }
                              | ChangeViewnode::HomeSkgrepoMove { .. }
                              | ChangeViewnode::RelRepoMove { .. }
                              | ChangeViewnode::ChangesetMarker { .. } )) )
  { return Err ( "A top-level diff is valid only on alias, id and flag properties, the diff view's properties other than bodyDiff, and changed folders"
                 . to_string () ); }
  check_where_the_edit_request_sits (&result) ?;
  Ok (result) }

/// What the parser says to a restoreRel on a historical phantom.
pub const RESTORE_REL_ON_A_DELETED_NODE : &str =
  "This node is deleted; restore it first (skg-restore-deleted-node)";
/// What the parser says to a restoreNode anywhere but on a historical
/// phantom.
pub const RESTORE_NODE_ON_A_NODE_NOT_DELETED : &str =
  "Only a node deleted since HEAD can be restored; restoreNode belongs on a historical phantom (skg-restore-deleted-node)";
/// What the parser says to a restoreRel anywhere but on a removed
/// membership.
pub const RESTORE_REL_WITHOUT_A_REMOVED_MEMBERSHIP : &str =
  "Only a removed membership can be restored; restoreRel belongs on a removed member, or on a folder member carrying (rel removed) (skg-restore-removed-membership)";

/// The refusal of a restore request on a headline that cannot carry
/// it, given whether the headline is a historical phantom; None for
/// any other request.
fn misplaced_restore_request_message (
  request       : &Option<NodeEditRequest>,
  is_historical : bool,
) -> Option<String> {
  match request {
    Some (NodeEditRequest::RestoreNode) =>
      Some ( RESTORE_NODE_ON_A_NODE_NOT_DELETED . to_string () ),
    Some (NodeEditRequest::RestoreRel) if is_historical =>
      Some ( RESTORE_REL_ON_A_DELETED_NODE . to_string () ),
    Some (NodeEditRequest::RestoreRel) =>
      Some ( RESTORE_REL_WITHOUT_A_REMOVED_MEMBERSHIP . to_string () ),
    _ => None, } }

/// A phantom carries no edit request but a historical phantom's
/// restoreNode; restoreRel sits only on an unrestricted vognode that
/// carries '(rel removed)', and the save checks its position. The other requests
/// keep the places their own checks give them.
fn check_where_the_edit_request_sits (
  metadata : &ViewnodeMetadata,
) -> Result<(), String> {
  let is_phantom : bool =
    metadata . is_deleted_node || metadata . is_historical_node;
  let is_unrestricted_vognode : bool =
    metadata . non_vognode . is_none ()
    && ! is_phantom
    && ! metadata . is_tombstone
    && metadata . unknown_node_skgid . is_none ();
  match &metadata . edit_request {
    Some (NodeEditRequest::RestoreNode) if metadata . is_historical_node =>
      Ok (( )),
    Some (NodeEditRequest::RestoreRel)
      if is_unrestricted_vognode
      && metadata . change_facts . change . rel == Some (Existence::Removed) =>
      Ok (( )),
    request @ Some (NodeEditRequest::RestoreNode | NodeEditRequest::RestoreRel) =>
      Err ( misplaced_restore_request_message (
        request, metadata . is_historical_node )
            . expect ("a restore request") ),
    Some (_) if is_phantom =>
      Err ( "A deleted node accepts no edit request but restoreNode, on a historical phantom"
            . to_string () ),
    _ => Ok (( )), } }

/// The folder a bare folder atom names.
fn folder_kind_of_atom (
  atom : &str,
) -> Option<MpViewnodeKind> {
  Folder::from_client_string (atom) . map (MpViewnodeKind::Folder) }


/// Parse the (node ...) s-expression contents.
fn parse_node_sexp (
  items : &[Sexp],
  metadata : &mut ViewnodeMetadata
) -> Result<(), String> {
  for element in items {
    match element {
      Sexp::List (subitems) if subitems . len () >= 1 => {
        let key : String =
          atom_to_string ( &subitems[0] ) ?;
        match key . as_str () {
          "id" => {
            if subitems . len () != 2 {
              return Err ( "id requires exactly one value" . to_string () ); }
            let value : String =
              atom_to_string ( &subitems[1] ) ?;
            metadata . skgid = Some ( ID::from (value)); },
          "repo" => {
            if subitems . len () != 2 {
              return Err ( "repo requires exactly one value" . to_string () ); }
            let value : String =
              atom_to_string ( &subitems[1] ) ?;
            metadata . home_skgrepo = Some ( SkgrepoName::from (value) ); },
          // Semantic relationship facts are display-only; the view
          // regenerates them.
          "rels" => {},
          "viewStats" => {
            parse_viewstats_sexp (
              &subitems[1..], &mut metadata . viewStats,
              &mut metadata . overridesHere ) ?; },
          "editRequest" => {
            if metadata . edit_request . is_some ()
               || metadata . relRepo_request . is_some () {
              return Err ( "node editRequest may appear only once"
                           . to_string () ); }
            parse_editrequest_sexp ( &subitems[1..], metadata ) ?; },
          "viewRequests" => {
            parse_viewrequests_sexp (
              &subitems[1..], &mut metadata . view_requests ) ?; },
          "diff" =>
            parse_diff_form ( &subitems[1..], metadata ) ?,
          _ => { return Err ( format! ( "Unknown node key: {}",
                                         key )); }} },
      Sexp::Atom (_) => {
        let bare_value : String =
          atom_to_string (element) ?;
        match bare_value . as_str () {
          // A `writeProtected` atom makes this occurrence write-protected.
          // The server emits and accepts this exact atom (see org_to_text.rs).
          "writeProtected" =>
            metadata . writeProtected = true,
          // A 'noClaim' atom says this occurrence does not claim
          // membership; a claimant carries nothing.
          "noClaim" =>
            metadata . claimsMembership = false,
          "omittedBody" =>
            // Display-only (like rels): the view
            // regenerates it, so accept and discard.
            {},
          _ => {
            return Err ( format! ( "Unknown node value: {}",
                                    bare_value )); }} },
      _ => { return Err ( "Unexpected element in node"
                           . to_string () ); }} }
  Ok (( )) }

/// Parse the (unknown (id X)) s-expression contents.
/// Sets metadata.unknown_node_id when an id is found.
fn parse_unknownnode_sexp (
  items    : &[Sexp],
  metadata : &mut ViewnodeMetadata,
) -> Result<(), String> {
  for element in items {
    match element {
      Sexp::List (subitems) if ! subitems . is_empty () => {
        let key : String =
          atom_to_string ( &subitems[0] ) ?;
        match key . as_str () {
          "id" => {
            if subitems . len () != 2 {
              return Err ( "unknown id requires exactly one value" . to_string () ); }
            if metadata . unknown_node_skgid . is_some () {
              return Err ( "unknown id may appear only once" . to_string () ); }
            let value : String =
              atom_to_string ( &subitems[1] ) ?;
            metadata . unknown_node_skgid =
              Some ( ID::from (value)); },
          "viewStats" => {
            if metadata . unknown_relRepo . is_some () {
              return Err ( "unknown viewStats may appear only once" . to_string () ); }
            let mut stats : ViewnodeStats = ViewnodeStats::default ();
            let mut overridesHere : Option<ID> = None;
            parse_viewstats_sexp ( &subitems[1..], &mut stats, &mut overridesHere ) ?;
            if stats . relRepo . is_none ()
               || stats . cycle || overridesHere . is_some () {
              return Err ( "Unknown viewStats supports only relRepo"
                           . to_string () ); }
            metadata . unknown_relRepo = stats . relRepo; },
          "editRequest" => {
            if metadata . unknown_relRepo_request . is_some () {
              return Err ( "unknown editRequest may appear only once"
                           . to_string () ); }
            let mut request_metadata : ViewnodeMetadata = default_metadata ();
            parse_editrequest_sexp (
              &subitems[1..], &mut request_metadata ) ?;
            if let Some (message) = misplaced_restore_request_message (
                 &request_metadata . edit_request, false ) {
              return Err (message); }
            if request_metadata . edit_request . is_some () {
              return Err ( "Unknown supports only an editRequest relRepo"
                           . to_string () ); }
            metadata . unknown_relRepo_request =
              request_metadata . relRepo_request; },
          "diff" =>
            parse_diff_form ( &subitems[1..], metadata ) ?,
          _ => { return Err ( format! (
            "Unknown 'unknown' key: {}", key )); }} },
      _ => { return Err ( "Unexpected element in unknown sexp"
                           . to_string () ); }} }
  Ok (( )) }

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

/// Parse the (deleted (id X) (repo S)) s-expression contents.
fn parse_deleted_sexp (
  items    : &[Sexp],
  metadata : &mut ViewnodeMetadata,
) -> Result<(), String> {
  for element in items {
    match element {
      Sexp::List (subitems) if subitems . len () >= 1 => {
        let key : String =
          atom_to_string ( &subitems[0] ) ?;
        match key . as_str () {
          "id" => {
            if subitems . len () != 2 {
              return Err ( "deleted id requires exactly one value" . to_string () ); }
            let value : String =
              atom_to_string ( &subitems[1] ) ?;
            metadata . skgid = Some ( ID::from (value)); },
          "repo" => {
            if subitems . len () != 2 {
              return Err ( "deleted repo requires exactly one value" . to_string () ); }
            let value : String =
              atom_to_string ( &subitems[1] ) ?;
            metadata . home_skgrepo = Some ( SkgrepoName::from (value)); },
          "editRequest" => {
            // Where a phantom's request may sit is checked once the
            // whole metadata is read ('check_where_the_edit_request_sits').
            if metadata . edit_request . is_some () {
              return Err ( "node editRequest may appear only once"
                           . to_string () ); }
            parse_editrequest_sexp ( &subitems[1..], metadata ) ?;
            if metadata . relRepo_request . is_some () {
              return Err ( "A deleted node accepts no edit request but restoreNode, on a historical phantom"
                           . to_string () ); }},
          "diff" =>
            parse_diff_form ( &subitems[1..], metadata ) ?,
          _ => { return Err ( format! ( "Unknown deleted key: {}",
                                         key )); }} },
      _ => { return Err ( "Unexpected element in deleted sexp"
                           . to_string () ); }} }
  Ok (( )) }


/// Parse the (viewStats ...) s-expression contents: the bare-atom
/// 'cycle' stat, the discarded bare atom 'noWarrant', and the keyed 'overridesHere' / 'homeRepoHerald' /
/// 'homeSkgrepoMove' / 'movedLikeAncestor' sub-forms. Relationship stats are semantic display-only '(rels ...)'
/// facts, parsed and discarded at node level instead.
fn parse_viewstats_sexp (
  items         : &[Sexp],
  stats         : &mut ViewnodeStats,
  overridesHere : &mut Option<ID>, // an occurrence's, which it carries outside its stats
) -> Result<(), String> {
  for element in items {
    match element {
      Sexp::Atom (_) => {
        let bare_value : String =
          atom_to_string (element) ?;
        match bare_value . as_str () {
          "cycle"          => stats . cycle = true,
          "noWarrant"      => {}, // output-only, silently discard
          _ => {
            return Err ( format! ( "Unknown viewStats value: {}",
                                    bare_value )); }} },
      Sexp::List (form) if form . first () . is_some_and (
        |head| matches! ( atom_to_string (head) . as_deref (),
                          Ok ("homeSkgrepoMove") | Ok ("movedLikeAncestor") )) => {
        // Output-only facts of the diff view's skgrepo-changes
        // buffer, recomputed after completion: silently discard.
      },
      Sexp::List (kv_pair) if kv_pair . len () == 2 => {
        let key : String = atom_to_string ( &kv_pair[0] ) ?;
        match key . as_str () {
          "homeRepoHerald" => {}, // output-only, silently discard
          "overridesHere" => {
            // Load-bearing, unlike the other view stats: save
            // extraction round-trips the original ID through it.
            let value : String =
              atom_to_string ( &kv_pair[1] ) ?;
            *overridesHere = Some ( ID::from (value)); },
          "relRepo" => {
            // Display-only skgrepo fact.  A save request must instead
            // appear as (editRequest (relRepo REPO)).
            let value : String =
              atom_to_string ( &kv_pair[1] ) ?;
            stats . relRepo = Some ( SkgrepoName::from (value)); },
          _ => { return Err ( format! (
            "Unknown viewStats key: {}", key )); }} },
      _ => { return Err ( "Unexpected element in viewStats"
                           . to_string () ); }} }
  Ok (( )) }


/// Parse the (editRequest ...) s-expression contents.
fn parse_editrequest_sexp (
  items : &[Sexp],
  metadata : &mut ViewnodeMetadata
) -> Result<(), String> {
  if items . len () != 1 {
    return Err ( "editRequest requires exactly one request" . to_string () ); }
  for element in items {
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
                           . to_string () ); }} }
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


/// Parse the diff view's '(diff PART...)' form, whose parts are
/// 'args', into 'metadata'. It may appear once.
fn parse_diff_form (
  args     : &[Sexp],
  metadata : &mut ViewnodeMetadata,
) -> Result<(), String> {
  if metadata . change_facts != ChangeFacts::default ()
     || metadata . readiness_request . is_some () {
    return Err ( "diff may appear only once" . to_string () ); }
  for part in args {
    match part {
      Sexp::List (items) if ! items . is_empty () => {
        let head : String = atom_to_string (&items[0]) ?;
        parse_change_fact_part ( &head, &items[1..], metadata ) ?; },
      _ => return Err ( "Each part of diff must be a list" . to_string () ), }}
  Ok (( )) }

/// Parse one part of the diff view's '(diff ...)' form -- '(node
/// added|removed)', '(rel added|removed)', '(properties changed)',
/// '(leaves N OBJECT...)', '(ready OBJECT...)', '(readyRequest
/// OBJECT...)' or '(unreadyRequest OBJECT...)' -- whose head is 'key'
/// and whose arguments are 'args', into 'metadata'. Each may appear once; a viewnode carries at most
/// one readiness request. '(leaves ...)' is accepted and ignored: it
/// serves only the clients' gestures, and the view derives it again.
fn parse_change_fact_part (
  key      : &str,
  args     : &[Sexp],
  metadata : &mut ViewnodeMetadata,
) -> Result<(), String> {
  let atoms : Vec<String> =
    args . iter () . map (atom_to_string) . collect::<Result<_, _>> () ?;
  match key {
    "node" | "rel" => {
      let existence : Existence = match atoms . as_slice () {
        [ atom ] => Existence::from_wire_name (atom),
        _ => None, }
        . ok_or_else ( || format! (
          "{} must be ({} added) or ({} removed)", key, key, key )) ?;
      let slot : &mut Option<Existence> =
        if key == "node" { &mut metadata . change_facts . change . node }
        else { &mut metadata . change_facts . change . rel };
      if slot . replace (existence) . is_some () {
        return Err ( format! ( "{} may appear only once", key )); }},
    "properties" => {
      if atoms != [ "changed" ] {
        return Err ( "properties must be (properties changed)" . to_string () ); }
      if metadata . change_facts . change . properties {
        return Err ( "properties may appear only once" . to_string () ); }
      metadata . change_facts . change . properties = true; },
    "ready" => {
      if metadata . change_facts . ready . is_some () {
        return Err ( "ready may appear only once" . to_string () ); }
      metadata . change_facts . ready =
        Some ( readiness_objects_from_atoms (&atoms) ? ); },
    "leaves" => {},
    "readyRequest" | "unreadyRequest" => {
      if metadata . readiness_request . is_some () {
        return Err ( "A viewnode may carry only one readiness request"
                     . to_string () ); }
      metadata . readiness_request = Some ( ReadinessRequest {
        ready   : key == "readyRequest",
        objects : readiness_objects_from_atoms (&atoms) ?, } ); },
    other => return Err ( format! ( "Unknown part of diff: {}", other )), }
  Ok (( )) }

fn readiness_objects_from_atoms (
  atoms : &[String],
) -> Result<BTreeSet<DiffObject>, String> {
  atoms . iter ()
    . map ( |atom| DiffObject::from_wire_name (atom)
            . ok_or_else ( || format! ( "Unknown diff object: {}", atom )) )
    . collect () }
