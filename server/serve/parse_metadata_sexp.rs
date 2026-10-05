/// PURPOSE: Parse (skg ...) metadata s-expressions from org headlines.
///
/// Format:
///   Non-vognodes: (skg [focused] [folded] nonVognodeKind)
///   UnrestrictedVognodes: (skg [focused] [folded]
///                   (node [(id ID)]
///                         [(repo REPO)]
///                         [(affectsParent true|false|na)]
///                         [(birth roleGraft ROLENAME)]
///                         [writeProtected]   ; marks the occurrence write-protected
///                         [cycle]
///                         [(stats [containsParent]
///                                 [(containers N)]
///                                 [(contents N)]
///                                 [(linksIn N)])]
///                         [(editRequest <delete | (merge ID)>)]
///                         [(viewRequests REQUEST...)]))

use crate::types::sexp::atom_to_string;
use crate::types::misc::{ID, SkgRepoName};
use crate::types::errors::BufferValidationError;
use crate::types::nodes::complete::Flag;
use crate::types::git::{NodeAxes, RelationshipAxes, Sign};
use crate::types::viewnode::{
  GraphnodeStats, ViewnodeStats, NodeEditRequest, ViewRequest, FolderRelation,
  Property, PropertyFolder, PartnerFolder, PhantomDeleted, RestrictedVognode, PhantomUnknown,
  Birth, Editability, AffectsParent,
};
use crate::dbs::in_rust_graph::relation_accessors::RelationRole;
use crate::types::maybe_placed_viewnode::{
    MpViewnode, MpViewnodeKind, MpUnrestrictedVognode, MpPhantomDiff,
    MpVognode, MpPhantom,
};

use sexp::Sexp;
use std::collections::HashSet;

//
// Parsing-internal types
//

/// Intermediate parsed metadata. Converted to Viewnode via viewnode_from_metadata().
#[derive(Debug, Clone, PartialEq)]
pub struct ViewnodeMetadata {
  pub focused: bool,
  pub folded: bool,
  pub body_folded: bool,
  // None means vognode, Some means non-vognode.
  pub non_vognode: Option<MpViewnodeKind>,
  // UnrestrictedVognode fields (ignored if non-vognode is Some)
  pub skgid: Option<ID>,
  pub home_skgrepo: Option<SkgRepoName>,
  pub affectsParent: AffectsParent,
  pub birth: Birth,
  pub writeProtected: bool,
  pub graphStats: GraphnodeStats,
  pub viewStats: ViewnodeStats,
  pub edit_request: Option<NodeEditRequest>,
  pub relRepo_request: Option<SkgRepoName>,
  pub view_requests: HashSet<ViewRequest>,
  pub unrestrictedVognode_node_axes : NodeAxes,
  pub unrestrictedVognode_relationship_axes : RelationshipAxes,
  pub unrestrictedVognode_not_in_git : bool,
  pub property_relationship_axes : RelationshipAxes,
  pub property_relRepo : Option<SkgRepoName>,
  pub property_relRepo_request : Option<SkgRepoName>,
  pub textchanged_staged   : bool,
  pub textchanged_unstaged : bool,
  // When true, this is a PhantomDeleted (id and skgrepo are used).
  pub is_deleted_node: bool,
  // When true, this is a DeadViewnode.
  pub is_dead_viewnode: bool,
  // When Some, this is a PhantomUnknown (a phantom for a missing
  // referent). Carries only the id; no skgrepo/title/body apply.
  pub unknown_node_skgid: Option<ID>,
  pub unknown_relRepo : Option<SkgRepoName>,
  pub unknown_relRepo_request : Option<SkgRepoName>,
  // When true, this is a restricted-skgrepo placeholder: an anonymous,
  // dataless atom (see RestrictedVognode). It carries no id/repo/etc.
  pub is_restricted_node : bool,
  // When true, this is a PhantomDiff. It carries the same fields as a
  // node (id/repo/write-protected/graphStats/diff axes), parsed via
  // parse_node_sexp, but emits and is recognized by its own root atom
  // 'diffPhantom' rather than being inferred from the diff axes.
  pub is_diff_phantom : bool,
}

pub fn default_metadata() -> ViewnodeMetadata {
  ViewnodeMetadata {
    focused: false,
    folded: false,
    body_folded: false,
    non_vognode: None,
    skgid: None,
    home_skgrepo: None,
    affectsParent: AffectsParent::True,
    birth: Birth::Unremarkable,
    writeProtected: false,
    graphStats: GraphnodeStats::default(),
    viewStats: ViewnodeStats::default(),
    edit_request: None,
    relRepo_request: None,
    view_requests: HashSet::new(),
    unrestrictedVognode_node_axes : NodeAxes::default(),
    unrestrictedVognode_relationship_axes : RelationshipAxes::default(),
    unrestrictedVognode_not_in_git : false,
    property_relationship_axes : RelationshipAxes::default(),
    property_relRepo : None,
    property_relRepo_request : None,
    textchanged_staged   : false,
    textchanged_unstaged : false,
    is_deleted_node: false,
    is_dead_viewnode: false,
    unknown_node_skgid: None,
    unknown_relRepo: None,
    unknown_relRepo_request: None,
    is_restricted_node: false,
    is_diff_phantom: false, }}

/// Create an MpViewnode from parsed metadata components.
/// This is the bridge between parsing (ViewnodeMetadata) and runtime (MpViewnode).
/// Returns (MpViewnode, error, warning):
/// - error if a Non-vognode has a body;
/// - warning if a folder header (PropertyFolder or PartnerFolder) has nonempty
///   title text, which the server discards: folder headlines are
///   titleless server-side (heralds supply their labels), so any
///   text there is a user edit that cannot be saved. Property leaves
///   are exempt -- their title IS their data.
pub fn viewnode_from_metadata (
  metadata : &ViewnodeMetadata,
  title    : String,
  body     : Option < String >,
) -> ( MpViewnode, Option < BufferValidationError >, Option < String > ) {
  let (kind, error, warning)
    : (MpViewnodeKind, Option<BufferValidationError>, Option<String>)
    = if let Some (ref uid) = metadata . unknown_node_skgid {
        ( MpViewnodeKind::Vognode (MpVognode::Phantom (
            MpPhantom::Unknown (
              PhantomUnknown {
                skgid           : uid . clone (),
                relRepo         : metadata . unknown_relRepo . clone (),
                relRepo_request : metadata . unknown_relRepo_request . clone (),
              } ) )),
          if body . is_some () || ! title . is_empty () {
            Some ( BufferValidationError::Other (
              "Unknown phantom content cannot be edited" . to_string () ))
          } else { None },
          None )
      } else if metadata . is_restricted_node {
        let error : Option<BufferValidationError> =
          if body . is_some ()
          || ! title . is_empty () {
            Some ( BufferValidationError::Other (
              "Restricted vognode content cannot be edited"
              . to_string () ))
          } else { None };
        ( MpViewnodeKind::Vognode (
            MpVognode::Restricted ( RestrictedVognode ) ),
          error, None )
      } else if metadata . is_dead_viewnode {
        ( MpViewnodeKind::DeadViewnode, None, None )
      } else if metadata . is_deleted_node {
        ( MpViewnodeKind::Vognode (MpVognode::Phantom (
            MpPhantom::Deleted ( PhantomDeleted {
            skgid     : metadata . skgid . clone ()
                       . unwrap_or_else ( || ID::from ("")),
            home_skgrepo : metadata . home_skgrepo . clone ()
                       . unwrap_or_else ( || SkgRepoName::from ("")),
            title,
            body,
          } ) )), None, None )
      } else if let Some ( ref non_vognode ) = metadata . non_vognode {
        let is_flags_folder = matches! (non_vognode,
          MpViewnodeKind::PropertyFolder (PropertyFolder::Flags { .. }));
        let is_flag = matches! (non_vognode,
          MpViewnodeKind::Property (Property::Flag { .. }));
        let error : Option<BufferValidationError> =
          if body . is_some () && ! is_flags_folder && ! is_flag {
            Some ( BufferValidationError::Body_of_NonVognode (
              title . clone (),
              maybeplaced_kind_error_label (non_vognode) ))
          } else { None };
        let folder_title_warning : Option<String> =
          if ! title . is_empty ()
            && ! is_flags_folder
            && matches! ( non_vognode,
                          MpViewnodeKind::PropertyFolder (_)
                          | MpViewnodeKind::PartnerFolder (_) )
          { Some ( format! (
              "Headline text on a {} is not saved; discarded: {:?}",
              maybeplaced_kind_error_label (non_vognode),
              title )) }
          else { None };
        let non_vognode_with_title : MpViewnodeKind = match non_vognode {
          // Use headline title for string and apply non-vognode relationship axes
          MpViewnodeKind::Property (Property::Alias { .. }) =>
            MpViewnodeKind::Property (Property::Alias {
                              text: title . clone (),
                              relRepo:
                                metadata . property_relRepo . clone (),
                              relRepo_request:
                                metadata . property_relRepo_request . clone (),
                              relationship_axes: metadata . property_relationship_axes }),
          MpViewnodeKind::Property (Property::ID { .. }) =>
            MpViewnodeKind::Property (Property::ID {
                              skgid: title . clone () . into (),
                              relationship_axes: metadata . property_relationship_axes }),
          MpViewnodeKind::Property (Property::Flag { flag, .. }) =>
            MpViewnodeKind::Property (Property::Flag {
              flag : *flag,
              title    : title . clone (),
              body     : body . clone () }),
          MpViewnodeKind::PropertyFolder (PropertyFolder::Flags { .. }) =>
            MpViewnodeKind::PropertyFolder (PropertyFolder::Flags {
              title : title . clone (), body : body . clone () }),
          MpViewnodeKind::Property (Property::TextChanged { .. }) =>
            MpViewnodeKind::Property (Property::TextChanged {
                              staged   : metadata . textchanged_staged,
                              unstaged : metadata . textchanged_unstaged }),
          other => other . clone () };
        ( non_vognode_with_title, error, folder_title_warning )
      } else {
      // MpUnrestrictedVognode
      { let editability : Editability =
          if metadata . writeProtected
          { Editability::WriteProtected }
          else
          { Editability::Editable {
              body,
              edit_request : metadata . edit_request . clone () } };
        // An edit_request on a write-protected node has nowhere to live
        // (Editability::WriteProtected carries none), so the user's
        // instruction to delete or merge would silently vanish. Emit a
        // validation error instead so the save is rejected with a
        // clear message. We can only report this when the id is
        // known; if it isn't, other validations cover the missing-id
        // case.
        let error : Option<BufferValidationError> =
          if     metadata . writeProtected
              && metadata . edit_request . is_some ()
          { metadata . skgid . clone ()
            . map ( BufferValidationError::EditRequestOnWriteProtectedOccurrence ) }
          else if metadata . writeProtected
               && metadata . relRepo_request . is_some ()
          { Some ( BufferValidationError::Other (
              "relRepo request on a write-protected node"
              . to_string () )) }
          else { None };
        let t : MpUnrestrictedVognode = MpUnrestrictedVognode {
            title,
            skgid            : metadata . skgid . clone (),
            home_skgrepo     : metadata . home_skgrepo . clone (),
            affectsParent         : metadata . affectsParent,
            birth            : metadata . birth,
            graphStats       : metadata . graphStats . clone (),
            viewStats        : metadata . viewStats . clone (),
            relRepo_request : metadata . relRepo_request . clone (),
            view_requests    : metadata . view_requests . clone (),
            node_axes        : metadata . unrestrictedVognode_node_axes,
            relationship_axes       : metadata . unrestrictedVognode_relationship_axes,
            not_in_git       : metadata . unrestrictedVognode_not_in_git,
            editability, };
        let node_kind : MpViewnodeKind =
          if metadata . is_diff_phantom
          { // TODO/DONE/local-view-update/plan_v2.org §11: a phantom carries only the slim MpPhantomDiff. The
            // root atom 'diffPhantom' (not the diff axes) decides this, so a
            // live node carrying e.g. removedR stays a Vognode. The
            // EditRequestOnWriteProtectedOccurrence validation above already fired if this
            // phantom (write-protected) carried an edit_request, so dropping
            // editability/affectsParent/etc. here loses nothing.
            MpViewnodeKind::Vognode (MpVognode::Phantom (
              MpPhantom::Diff (
                MpPhantomDiff::from_unrestrictedVognode (t) ))) }
          else
          { MpViewnodeKind::Vognode ( MpVognode::Unrestricted (t) ) };
        ( node_kind,
          error, None ) }
    };
  ( MpViewnode { focused     : metadata . focused,
                       folded      : metadata . folded,
                       body_folded : metadata . body_folded,
                       kind },
    error, warning ) }

fn maybeplaced_kind_error_label (
  kind : &MpViewnodeKind,
) -> String {
  match kind {
    MpViewnodeKind::PropertyFolder (folder) =>
      folder . repr_in_client () . to_string (),
    MpViewnodeKind::Property (property) =>
      property . repr_in_client () . to_string (),
    MpViewnodeKind::PartnerFolder (partnerFolder) =>
      partnerFolder . repr_in_client () . to_string (),
    MpViewnodeKind::BufferRoot =>
      "forestRoot" . to_string (),
    MpViewnodeKind::DeadViewnode =>
      "deadViewnode" . to_string (),
    MpViewnodeKind::Vognode (_) =>
      MpViewnode {
        focused     : false,
        folded      : false,
        body_folded : false,
        kind        : kind . clone (),
      } . error_label (), } }


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

  // Process each element
  for element in elements {
    match element {
      Sexp::List (items) if items . len () >= 1 => {
        let first : String =
          atom_to_string ( &items[0] ) ?;
        match first . as_str () {
          "node" => {
            parse_node_sexp ( &items[1..], &mut result ) ?; },
          "diffPhantom" => {
            // (diffPhantom ...) -- a moved/removed phantom in git-diff
            // mode. Same field grammar as (node ...) (id/repo/write-protected/
            // graphStats/diff axes), but its own root atom so the client
            // and round-trip never infer phantom-ness from the diff axes.
            parse_node_sexp ( &items[1..], &mut result ) ?;
            result . is_diff_phantom = true; },
          "deleted" => {
            result . is_deleted_node = true;
            parse_deleted_sexp ( &items[1..], &mut result ) ?; },
          "unknown" => {
            // (unknown (id X)) -- placeholder for a referenced
            // node with no record anywhere. No skgrepo/title/body.
            parse_unknownnode_sexp ( &items[1..], &mut result ) ?; },
          "restrictedNode" => {
            parse_restrictednode_sexp ( &items[1..], &mut result ) ?; },
          "staged" => {
            // (staged ATOMS) at top level is for Properties (Alias/ID).
            apply_axis_atoms_to_property (
              &items[1..],
              true,  // staged
              &mut result . property_relationship_axes ) ?; },
          "unstaged" => {
            apply_axis_atoms_to_property (
              &items[1..],
              false, // unstaged
              &mut result . property_relationship_axes ) ?; },
          "relRepo" => {
            if items . len () != 2 {
              return Err (
                "relRepo requires exactly one repo name"
                . to_string () ); }
            if result . property_relRepo . is_some () {
              return Err ( "Alias relRepo may appear only once"
                           . to_string () ); }
            result . property_relRepo = Some ( SkgRepoName::from (
              atom_to_string (&items [1]) ? )); },
          "editRequest" => {
            let mut request_metadata : ViewnodeMetadata = default_metadata ();
            parse_editrequest_sexp (
              &items[1..], &mut request_metadata ) ?;
            if request_metadata . edit_request . is_some () {
              return Err ( "Only Alias may carry a top-level editRequest relRepo"
                           . to_string () ); }
            if result . property_relRepo_request . is_some () {
              return Err ( "Alias editRequest may appear only once"
                           . to_string () ); }
            result . property_relRepo_request =
              request_metadata . relRepo_request; },
          "textChanged" => {
            // (textChanged STAGE_TAGS) for the TextChanged property.
            result . non_vognode = Some (
              MpViewnodeKind::Property (
                Property::TextChanged { staged: false, unstaged: false } ) );
            for tag in &items[1..] {
              let tag_str : String = atom_to_string (tag) ?;
              match tag_str . as_str () {
                "staged"   => result . textchanged_staged   = true,
                "unstaged" => result . textchanged_unstaged = true,
                other => return Err ( format! (
                  "Unknown textChanged stage tag: {}", other )), } } },
          "flag" => {
            if items . len () != 2 {
              return Err ("flag requires exactly one flag name"
                          . to_string ()); }
            let name : String = atom_to_string (&items[1]) ?;
            let flag : Flag = Flag::from_wire_name (&name)
              . ok_or_else (|| format! ("Unknown flag: {}", name)) ?;
            result . non_vognode = Some (MpViewnodeKind::Property (
              Property::Flag {
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
          _ => { return Err ( format! ( "Unknown metadata key: {}",
                                         first )); }} },
      Sexp::Atom (_) => {
        let bare_value : String =
          atom_to_string (element) ?;
        match bare_value . as_str () {
          "focused"  => result . focused = true,
          "folded"   => result . folded = true,
          "bodyFolded" => result . body_folded = true,
          // A restricted vognode is a dataless bare atom
          // (see RestrictedVognode). The legacy field-bearing list form
          // '(restrictedNode ...)' is still tolerated by the List arm
          // above so a stale buffer round-trips.
          "restrictedNode" => result . is_restricted_node = true,
          // Non-vognode kinds as bare atoms (alias/id string comes from title in viewnode_from_metadata)
          "alias"    => result . non_vognode = Some ( MpViewnodeKind::Property ( Property::Alias { text: String::new(), relRepo: None, relRepo_request: None, relationship_axes: RelationshipAxes::default() } ) ),
          "aliasFolder" => result . non_vognode = Some (MpViewnodeKind::PropertyFolder (PropertyFolder::Alias)),
          "flagsFolder" => result . non_vognode = Some (
            MpViewnodeKind::PropertyFolder (PropertyFolder::flags ())),
          "forestRoot" => result . non_vognode = Some (MpViewnodeKind::BufferRoot),
          "hiddenInSubscribeeFolder" =>
            result . non_vognode = Some (MpViewnodeKind::PartnerFolder (PartnerFolder::HiddenInSubscribee)),
          "hiddenOutsideOfSubscribeeFolder" =>
            result . non_vognode = Some (MpViewnodeKind::PartnerFolder (PartnerFolder::HiddenOutsideOfSubscribee)),
          "hiddenFolder" =>
            result . non_vognode = Some (MpViewnodeKind::PartnerFolder (PartnerFolder::Hidden)),
          "hiderFolder" =>
            result . non_vognode = Some (MpViewnodeKind::PartnerFolder (PartnerFolder::Hider)),
          "overriddenFolder" =>
            result . non_vognode = Some (MpViewnodeKind::PartnerFolder (PartnerFolder::Overridden)),
          "overriderFolder" =>
            result . non_vognode = Some (MpViewnodeKind::PartnerFolder (PartnerFolder::Overrider)),
          "subscriberFolder" =>
            result . non_vognode = Some (MpViewnodeKind::PartnerFolder (PartnerFolder::Subscriber)),
          "subscribeeFolder" =>
            result . non_vognode = Some (MpViewnodeKind::PartnerFolder (PartnerFolder::Subscribee)),
          "textChanged" =>
            result . non_vognode = Some (
              MpViewnodeKind::Property (
                Property::TextChanged { staged: false, unstaged: false } ) ),
          "idFolder" =>
            result . non_vognode = Some (MpViewnodeKind::PropertyFolder (PropertyFolder::ID)),
          "id" =>
            result . non_vognode = Some ( MpViewnodeKind::Property ( Property::ID { skgid: ID::default(), relationship_axes: RelationshipAxes::default() } ) ),
          "deadViewnode" => result . is_dead_viewnode = true,
          _ => {
            return Err ( format! ( "Unknown top-level value: {}",
                                    bare_value )); }} },
      _ => { return Err ( format! (
        "Unexpected element in metadata sexp: {}",
        sexp_str )); }} }
  if ( result . property_relRepo . is_some ()
       || result . property_relRepo_request . is_some () )
     && ! matches! ( result . non_vognode,
                     Some (MpViewnodeKind::Property (Property::Alias { .. })) )
  { return Err ( "relRepo and its editRequest are valid only on Alias properties"
                 . to_string () ); }
  Ok (result) }


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
            metadata . home_skgrepo = Some ( SkgRepoName::from (value) ); },
          // Semantic relationship / birth facts are display-only.
          // The client strips them before save; the view regenerates them.
          "rels" => {},
          "viewStats" => {
            parse_viewstats_sexp ( &subitems[1..], &mut metadata . viewStats ) ?; },
          "editRequest" => {
            if metadata . edit_request . is_some ()
               || metadata . relRepo_request . is_some () {
              return Err ( "node editRequest may appear only once"
                           . to_string () ); }
            parse_editrequest_sexp ( &subitems[1..], metadata ) ?; },
          "viewRequests" => {
            parse_viewrequests_sexp (
              &subitems[1..], &mut metadata . view_requests ) ?; },
          "affectsParent" => {
            if subitems . len () != 2 {
              return Err ( "affectsParent requires exactly one value" . to_string () ); }
            let value : String =
              atom_to_string ( &subitems[1] ) ?;
            metadata . affectsParent = match value . as_str () {
              "true"  => AffectsParent::True,
              "false" => AffectsParent::False,
              "na"    => AffectsParent::NA,
              _ => return Err ( format! (
                "Invalid affectsParent value: {}", value )), }; },
          "staged" => {
            apply_axis_atoms_to_unrestrictedVognode (
              &subitems[1..],
              true,  // staged
              &mut metadata . unrestrictedVognode_node_axes,
              &mut metadata . unrestrictedVognode_relationship_axes ) ?; },
          "unstaged" => {
            apply_axis_atoms_to_unrestrictedVognode (
              &subitems[1..],
              false, // unstaged
              &mut metadata . unrestrictedVognode_node_axes,
              &mut metadata . unrestrictedVognode_relationship_axes ) ?; },
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
          "omittedBody" =>
            // Display-only (like rels): the view
            // regenerates it, so accept and discard.
            {},
          "notInGit" =>
            metadata . unrestrictedVognode_not_in_git = true,
          _ => {
            return Err ( format! ( "Unknown node value: {}",
                                    bare_value )); }} },
      _ => { return Err ( "Unexpected element in node"
                           . to_string () ); }} }
  Ok (( )) }

/// Apply a sequence of axis atoms (addedN, deletedN, addedR, removedR) to
/// an UnrestrictedVognode's node and relationship axes for the given stage.
fn apply_axis_atoms_to_unrestrictedVognode (
  atoms      : &[Sexp],
  is_staged  : bool,
  node_axes  : &mut NodeAxes,
  relationship_axes : &mut RelationshipAxes,
) -> Result<(), String> {
  for atom in atoms {
    let atom_str : String = atom_to_string (atom) ?;
    let (axis, sign) : (char, Sign) =
      Sign::parse_axis_atom (&atom_str)
        . ok_or_else ( || format! (
          "Unknown axis atom: {}", atom_str )) ?;
    let slot : &mut Option<Sign> = match (axis, is_staged) {
      ('N', true)  => &mut node_axes . staged,
      ('N', false) => &mut node_axes . unstaged,
      ('R', true)  => &mut relationship_axes . staged,
      ('R', false) => &mut relationship_axes . unstaged,
      _ => unreachable!(), };
    *slot = Some (sign); }
  Ok (( )) }

/// Apply axis atoms (addedR, removedR only) to a Non-vognode's relationship axes.
fn apply_axis_atoms_to_property (
  atoms      : &[Sexp],
  is_staged  : bool,
  relationship_axes : &mut RelationshipAxes,
) -> Result<(), String> {
  for atom in atoms {
    let atom_str : String = atom_to_string (atom) ?;
    let (axis, sign) : (char, Sign) =
      Sign::parse_axis_atom (&atom_str)
        . ok_or_else ( || format! (
          "Unknown axis atom: {}", atom_str )) ?;
    if axis != 'R' {
      return Err ( format! (
        "Property (alias/id) only supports R axis atoms, got: {}",
        atom_str )); }
    let slot : &mut Option<Sign> =
      if is_staged { &mut relationship_axes . staged }
      else         { &mut relationship_axes . unstaged };
    *slot = Some (sign); }
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
            parse_viewstats_sexp ( &subitems[1..], &mut stats ) ?;
            if stats . relRepo . is_none ()
               || stats . cycle || stats . overridesHere . is_some () {
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
            if request_metadata . edit_request . is_some () {
              return Err ( "Unknown supports only an editRequest relRepo"
                           . to_string () ); }
            metadata . unknown_relRepo_request =
              request_metadata . relRepo_request; },
          _ => { return Err ( format! (
            "Unknown 'unknown' key: {}", key )); }} },
      _ => { return Err ( "Unexpected element in unknown sexp"
                           . to_string () ); }} }
  Ok (( )) }

/// Parse the LEGACY list form '(restrictedNode ...)'. The server now
/// emits the bare atom 'restrictedNode' (handled in the atom arm), but an
/// older server's field-bearing list form is tolerated here -- its
/// children (id/repo/membership/overridesHere) are discarded -- so a
/// stale buffer still round-trips. A restricted vognode is an
/// anonymous, dataless atom (see RestrictedVognode).
fn parse_restrictednode_sexp (
  _items   : &[Sexp],
  metadata : &mut ViewnodeMetadata,
) -> Result<(), String> {
  metadata . is_restricted_node = true;
  Ok (( )) }

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
            metadata . home_skgrepo = Some ( SkgRepoName::from (value)); },
          _ => { return Err ( format! ( "Unknown deleted key: {}",
                                         key )); }} },
      _ => { return Err ( "Unexpected element in deleted sexp"
                           . to_string () ); }} }
  Ok (( )) }


/// Parse the (viewStats ...) s-expression contents: the bare-atom
/// 'cycle' stat and the keyed 'overridesHere' / 'homeRepoHerald'
/// sub-forms. Relationship stats are semantic display-only '(rels ...)'
/// facts, parsed and discarded at node level instead.
fn parse_viewstats_sexp (
  items : &[Sexp],
  stats : &mut ViewnodeStats
) -> Result<(), String> {
  for element in items {
    match element {
      Sexp::Atom (_) => {
        let bare_value : String =
          atom_to_string (element) ?;
        match bare_value . as_str () {
          "cycle"          => stats . cycle = true,
          _ => {
            return Err ( format! ( "Unknown viewStats value: {}",
                                    bare_value )); }} },
      Sexp::List (kv_pair) if kv_pair . len () == 2 => {
        let key : String = atom_to_string ( &kv_pair[0] ) ?;
        match key . as_str () {
          "homeRepoHerald" => {}, // output-only, silently discard
          "overridesHere" => {
            // LOAD-BEARING, unlike the other view stats: save
            // extraction round-trips the original ID through it.
            let value : String =
              atom_to_string ( &kv_pair[1] ) ?;
            stats . overridesHere = Some ( ID::from (value)); },
          "relRepo" => {
            // Display-only skgrepo fact.  A save request must instead
            // appear as (editRequest (relRepo REPO)).
            let value : String =
              atom_to_string ( &kv_pair[1] ) ?;
            stats . relRepo = Some ( SkgRepoName::from (value)); },
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
            Some ( SkgRepoName::from (skgrepo) );
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
          _ => {
            return Err ( format! ( "Unknown editRequest value: {}",
                                    bare_value )); }} },
      _ => { return Err ( "Unexpected element in editRequest"
                           . to_string () ); }} }
  Ok (( )) }


/// Parse the (viewRequests ...) s-expression and update viewRequests.
/// Each request is either the bare atom 'editableView', or a nested
/// '(folder RELNAME)' / '(roleTree ROLENAME)' form.
fn parse_viewrequests_sexp (
  items : &[Sexp],
  requests : &mut HashSet<ViewRequest>
) -> Result<(), String> {
  for request_element in items {
    let request : ViewRequest = match request_element {
      Sexp::Atom (_) => {
        let atom : String = atom_to_string (request_element) ?;
        if atom == "editableView" { ViewRequest::Editable }
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
          "roleTree" => ViewRequest::RoleTree (
            RelationRole::from_rolename (&arg)
              . ok_or_else ( || format! (
                "Invalid path rolename: {}", arg )) ?),
          _ => return Err ( format! (
            "Unknown view request form: ({} ...)", head )), } },
      _ => return Err (
        "Unexpected element in viewRequests (expected 'editableView' \
         or '(folder ...)' / '(roleTree ...)')" . to_string () ), };
    requests . insert (request); }
  Ok (( )) }
