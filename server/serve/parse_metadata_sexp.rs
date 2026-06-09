/// PURPOSE: Parse (skg ...) metadata s-expressions from org headlines.
///
/// Format:
///   Scaffolds: (skg [focused] [folded] scaffoldKind)
///   TrueNodes: (skg [focused] [folded]
///                   (node [(id ID)]
///                         [(source SOURCE)]
///                         [(parentIs affected|independent|absent)]
///                         [(birth linksToParent|containsParent)]
///                         [indef]   ; short for "indefinitive"
///                         [cycle]
///                         [(stats [containsParent]
///                                 [(containers N)]
///                                 [(contents N)]
///                                 [(linksIn N)])]
///                         [(editRequest <delete | (merge ID)>)]
///                         [(viewRequests REQUEST...)]))

use crate::types::sexp::atom_to_string;
use crate::types::misc::{ID, SourceName};
use crate::types::errors::BufferValidationError;
use crate::types::git::{ExistenceAxes, MembershipAxes, Sign};
use crate::types::viewnode::{
  GraphNodeStats, ViewNodeStats, EditRequest, ViewRequest,
  Qual, QualCol, RoleCol, DeletedNode, InactiveNode, UnknownNode,
  Birth, IndefOrDef, NodeContainRels, NodeLinksourceRels, ParentIs,
};
use crate::types::maybe_placed_viewnode::{
    MpViewnode, MpViewnodeKind, MpTruenode, MpDiffPhantomNode,
    MpVognode,
};

use sexp::Sexp;
use std::collections::HashSet;
use std::str::FromStr;

//
// Parsing-internal types
//

/// Intermediate parsed metadata. Converted to ViewNode via viewnode_from_metadata().
#[derive(Debug, Clone, PartialEq)]
pub struct ViewnodeMetadata {
  pub focused: bool,
  pub folded: bool,
  pub body_folded: bool,
  // None means vognode, Some means non-vognode.
  pub non_vognode: Option<MpViewnodeKind>,
  // TrueNode fields (ignored if scaffold is Some)
  pub id: Option<ID>,
  pub source: Option<SourceName>,
  pub parentIs: ParentIs,
  pub birth: Birth,
  pub indefinitive: bool,
  pub graphStats: GraphNodeStats,
  pub viewStats: ViewNodeStats,
  pub edit_request: Option<EditRequest>,
  pub view_requests: HashSet<ViewRequest>,
  pub truenode_existence  : ExistenceAxes,
  pub truenode_membership : MembershipAxes,
  pub truenode_not_in_git : bool,
  pub scaffold_membership : MembershipAxes,
  pub textchanged_staged   : bool,
  pub textchanged_unstaged : bool,
  // When true, this is a DeletedNode (id and source are used).
  pub is_deleted_node: bool,
  // When true, this is a deleted non-vognode placeholder.
  pub is_dead_scaffold: bool,
  // When Some, this is an UnknownNode (a placeholder for a missing
  // referent). Carries only the id; no source/title/body apply.
  pub unknown_node_id: Option<ID>,
  // When true, this is an inactive-source placeholder. It carries id,
  // source, and relationship-position membership axes, but no editable
  // node content.
  pub is_inactive_node : bool,
  pub inactive_membership : MembershipAxes,
}

pub fn default_metadata() -> ViewnodeMetadata {
  ViewnodeMetadata {
    focused: false,
    folded: false,
    body_folded: false,
    non_vognode: None,
    id: None,
    source: None,
    parentIs: ParentIs::Affected,
    birth: Birth::Unremarkable,
    indefinitive: false,
    graphStats: GraphNodeStats::default(),
    viewStats: ViewNodeStats::default(),
    edit_request: None,
    view_requests: HashSet::new(),
    truenode_existence  : ExistenceAxes::default(),
    truenode_membership : MembershipAxes::default(),
    truenode_not_in_git : false,
    scaffold_membership : MembershipAxes::default(),
    textchanged_staged   : false,
    textchanged_unstaged : false,
    is_deleted_node: false,
    is_dead_scaffold: false,
    unknown_node_id: None,
    is_inactive_node: false,
    inactive_membership: MembershipAxes::default(), }}

/// Create an MpViewnode from parsed metadata components.
/// This is the bridge between parsing (ViewnodeMetadata) and runtime (MpViewnode).
/// Returns (MpViewnode, Option<BufferValidationError>) - error if Scaffold has body.
pub fn viewnode_from_metadata (
  metadata : &ViewnodeMetadata,
  title    : String,
  body     : Option < String >,
) -> ( MpViewnode, Option < BufferValidationError > ) {
  let (kind, error)
    : (MpViewnodeKind, Option<BufferValidationError>)
    = if let Some (ref uid) = metadata . unknown_node_id {
        ( MpViewnodeKind::Vognode (
            MpVognode::Unknown (
              UnknownNode { id: uid . clone () } ) ), None )
      } else if metadata . is_inactive_node {
        let error : Option<BufferValidationError> =
          if body . is_some ()
          || ! title . is_empty () {
            Some ( BufferValidationError::Other (
              "Inactive placeholder content cannot be edited"
              . to_string () ))
          } else { None };
        ( MpViewnodeKind::Vognode (
            MpVognode::Inactive (
              InactiveNode {
              id         : metadata . id . clone ()
                           . unwrap_or_else ( || ID::from ("")),
              source     : metadata . source . clone ()
                           . unwrap_or_else ( || SourceName::from ("")),
              membership : metadata . inactive_membership } ) ),
          error )
      } else if metadata . is_dead_scaffold {
        ( MpViewnodeKind::DeadScaffold, None )
      } else if metadata . is_deleted_node {
        ( MpViewnodeKind::Vognode (
            MpVognode::Deleted ( DeletedNode {
            id     : metadata . id . clone ()
                       . unwrap_or_else ( || ID::from ("")),
            source : metadata . source . clone ()
                       . unwrap_or_else ( || SourceName::from ("")),
            title,
            body,
          } ) ), None )
      } else if let Some ( ref non_vognode ) = metadata . non_vognode {
        let error : Option<BufferValidationError> =
          if body . is_some () {
            Some ( BufferValidationError::Body_of_Scaffold (
              title . clone (),
              maybeplaced_kind_error_label (non_vognode) ))
          } else { None };
        let non_vognode_with_title : MpViewnodeKind = match non_vognode {
          // Use headline title for string and apply scaffold membership axes
          MpViewnodeKind::Qual (Qual::Alias { .. }) =>
            MpViewnodeKind::Qual (Qual::Alias {
                              text: title . clone (),
                              membership: metadata . scaffold_membership }),
          MpViewnodeKind::Qual (Qual::ID { .. }) =>
            MpViewnodeKind::Qual (Qual::ID {
                              id: title . clone () . into (),
                              membership: metadata . scaffold_membership }),
          MpViewnodeKind::Qual (Qual::TextChanged { .. }) =>
            MpViewnodeKind::Qual (Qual::TextChanged {
                              staged   : metadata . textchanged_staged,
                              unstaged : metadata . textchanged_unstaged }),
          other => other . clone () };
        ( non_vognode_with_title, error )
      } else {
      // MpTruenode
      { let indef_or_def : IndefOrDef =
          if metadata . indefinitive
          { IndefOrDef::Indefinitive }
          else
          { IndefOrDef::Definitive {
              body,
              edit_request : metadata . edit_request . clone () } };
        // An edit_request on an indefinitive node has nowhere to live
        // (IndefOrDef::Indefinitive carries none), so the user's
        // instruction to delete or merge would silently vanish. Emit a
        // validation error instead so the save is rejected with a
        // clear message. We can only report this when the id is
        // known; if it isn't, other validations cover the missing-id
        // case.
        let error : Option<BufferValidationError> =
          if     metadata . indefinitive
              && metadata . edit_request . is_some ()
          { metadata . id . clone ()
            . map ( BufferValidationError::EditRequestOnIndefinitive ) }
          else { None };
        let t : MpTruenode = MpTruenode {
            title,
            id               : metadata . id . clone (),
            source           : metadata . source . clone (),
            parentIs         : metadata . parentIs,
            birth            : metadata . birth,
            graphStats       : metadata . graphStats . clone (),
            viewStats        : metadata . viewStats . clone (),
            view_requests    : metadata . view_requests . clone (),
            existence        : metadata . truenode_existence,
            membership       : metadata . truenode_membership,
            not_in_git       : metadata . truenode_not_in_git,
            indef_or_def, };
        let vognode =
          if t . should_be_diffPhantom ()
          { // TODO/DONE/local-view-update/plan_v2.org §11: a phantom carries only the slim MpDiffPhantomNode. The
            // EditRequestOnIndefinitive validation above already fired if this
            // phantom (indefinitive) carried an edit_request, so dropping
            // indef_or_def/parentIs/etc. here loses nothing.
            MpVognode::DiffPhantom (
              MpDiffPhantomNode::from_truenode (t) ) }
          else
          { MpVognode::Normal (t) };
        ( MpViewnodeKind::Vognode (vognode),
          error ) }
    };
  ( MpViewnode { focused     : metadata . focused,
                       folded      : metadata . folded,
                       body_folded : metadata . body_folded,
                       kind },
    error ) }

fn maybeplaced_kind_error_label (
  kind : &MpViewnodeKind,
) -> String {
  match kind {
    MpViewnodeKind::QualCol (col) =>
      col . repr_in_client () . to_string (),
    MpViewnodeKind::Qual (qual) =>
      qual . repr_in_client () . to_string (),
    MpViewnodeKind::PartnerCol (roleCol) =>
      roleCol . repr_in_client () . to_string (),
    MpViewnodeKind::BufferRoot =>
      "forestRoot" . to_string (),
    MpViewnodeKind::DeadScaffold =>
      "deadScaffold" . to_string (),
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
          "deleted" => {
            result . is_deleted_node = true;
            parse_deleted_sexp ( &items[1..], &mut result ) ?; },
          "unknownNode" => {
            // (unknownNode (id X)) -- placeholder for a referenced
            // node with no record anywhere. No source/title/body.
            parse_unknownnode_sexp ( &items[1..], &mut result ) ?; },
          "inactiveNode" => {
            parse_inactivenode_sexp ( &items[1..], &mut result ) ?; },
          "staged" => {
            // (staged ATOMS) at top level is for Quals (Alias/ID).
            apply_axis_atoms_to_membership_scaffold (
              &items[1..],
              true,  // staged
              &mut result . scaffold_membership ) ?; },
          "unstaged" => {
            apply_axis_atoms_to_membership_scaffold (
              &items[1..],
              false, // unstaged
              &mut result . scaffold_membership ) ?; },
          "textChanged" => {
            // (textChanged STAGE_TAGS) for the TextChanged qual.
            result . non_vognode = Some (
              MpViewnodeKind::Qual (
                Qual::TextChanged { staged: false, unstaged: false } ) );
            for tag in &items[1..] {
              let tag_str : String = atom_to_string (tag) ?;
              match tag_str . as_str () {
                "staged"   => result . textchanged_staged   = true,
                "unstaged" => result . textchanged_unstaged = true,
                other => return Err ( format! (
                  "Unknown textChanged stage tag: {}", other )), } } },
          "deletedScaffold" => {
            if items . len () != 2 {
              return Err ( "deletedScaffold requires exactly one kind value" . to_string () ); }
            let _kind_str : String =
              atom_to_string ( &items[1] ) ?;
            result . is_dead_scaffold = true; },
          // Note: "alias" as a list like (alias "string") is no longer supported.
          // Use bare "alias" atom instead - the alias string comes from headline title.
          // Legacy format detection - reject with helpful error
          "id" | "source" | "view" | "code" => {
            return Err ( format! (
              "Legacy metadata format detected (found '{}' at top level). \
               The new format uses (skg [focused] [folded] (node ...)) for TrueNodes \
               and (skg [focused] [folded] scaffoldKind) for Scaffolds.",
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
          // Scaffold kinds as bare atoms (alias/id string comes from title in viewnode_from_metadata)
          "alias"    => result . non_vognode = Some ( MpViewnodeKind::Qual ( Qual::Alias { text: String::new(), membership: MembershipAxes::default() } ) ),
          "aliasCol" => result . non_vognode = Some (MpViewnodeKind::QualCol (QualCol::Alias)),
          "forestRoot" => result . non_vognode = Some (MpViewnodeKind::BufferRoot),
          "hiddenInSubscribeeCol" =>
            result . non_vognode = Some (MpViewnodeKind::PartnerCol (RoleCol::HiddenInSubscribee)),
          "hiddenOutsideOfSubscribeeCol" =>
            result . non_vognode = Some (MpViewnodeKind::PartnerCol (RoleCol::HiddenOutsideOfSubscribee)),
          "hiddenCol" =>
            result . non_vognode = Some (MpViewnodeKind::PartnerCol (RoleCol::Hidden)),
          "hiderCol" =>
            result . non_vognode = Some (MpViewnodeKind::PartnerCol (RoleCol::Hider)),
          "overriddenCol" =>
            result . non_vognode = Some (MpViewnodeKind::PartnerCol (RoleCol::Overridden)),
          "overriderCol" =>
            result . non_vognode = Some (MpViewnodeKind::PartnerCol (RoleCol::Overrider)),
          "subscriberCol" =>
            result . non_vognode = Some (MpViewnodeKind::PartnerCol (RoleCol::Subscriber)),
          "subscribeeCol" =>
            result . non_vognode = Some (MpViewnodeKind::PartnerCol (RoleCol::Subscribee)),
          "textChanged" =>
            result . non_vognode = Some (
              MpViewnodeKind::Qual (
                Qual::TextChanged { staged: false, unstaged: false } ) ),
          "idCol" =>
            result . non_vognode = Some (MpViewnodeKind::QualCol (QualCol::ID)),
          "id" =>
            result . non_vognode = Some ( MpViewnodeKind::Qual ( Qual::ID { id: ID::default(), membership: MembershipAxes::default() } ) ),
          "deletedScaffold" =>
            return Err ( "deletedScaffold as bare atom is no longer supported; use (deletedScaffold kindString)" . to_string () ),
          _ => {
            return Err ( format! ( "Unknown top-level value: {}",
                                    bare_value )); }} },
      _ => { return Err ( format! (
        "Unexpected element in metadata sexp: {}",
        sexp_str )); }} }
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
            metadata . id = Some ( ID::from (value)); },
          "source" => {
            if subitems . len () != 2 {
              return Err ( "source requires exactly one value" . to_string () ); }
            let value : String =
              atom_to_string ( &subitems[1] ) ?;
            metadata . source = Some ( SourceName::from (value) ); },
          "graphStats" => {
            parse_graphstats_sexp ( &subitems[1..], &mut metadata . graphStats ) ?; },
          "viewStats" => {
            parse_viewstats_sexp ( &subitems[1..], &mut metadata . viewStats ) ?; },
          "editRequest" => {
            parse_editrequest_sexp ( &subitems[1..], metadata ) ?; },
          "viewRequests" => {
            parse_viewrequests_sexp (
              &subitems[1..], &mut metadata . view_requests ) ?; },
          "parentIs" => {
            if subitems . len () != 2 {
              return Err ( "parentIs requires exactly one value" . to_string () ); }
            let value : String =
              atom_to_string ( &subitems[1] ) ?;
            metadata . parentIs = match value . as_str () {
              "affected"    => ParentIs::Affected,
              "independent" => ParentIs::Independent,
              "absent"      => ParentIs::Absent,
              _ => return Err ( format! (
                "Invalid parentIs value: {}", value )), }; },
          "birth" => {
            if subitems . len () != 2 {
              return Err ( "birth requires exactly one value" . to_string () ); }
            let value : String =
              atom_to_string ( &subitems[1] ) ?;
            metadata . birth = match value . as_str () {
              "linksToParent"    => Birth::LinksToParent,
              "containsParent"   => Birth::ContainsParent,
              _ => return Err ( format! (
                "Invalid birth value: {}", value )), }; },
          "staged" => {
            apply_axis_atoms_to_truenode (
              &subitems[1..],
              true,  // staged
              &mut metadata . truenode_existence,
              &mut metadata . truenode_membership ) ?; },
          "unstaged" => {
            apply_axis_atoms_to_truenode (
              &subitems[1..],
              false, // unstaged
              &mut metadata . truenode_existence,
              &mut metadata . truenode_membership ) ?; },
          _ => { return Err ( format! ( "Unknown node key: {}",
                                         key )); }} },
      Sexp::Atom (_) => {
        let bare_value : String =
          atom_to_string (element) ?;
        match bare_value . as_str () {
          // "indef" is short for "indefinitive". The server emits
          // and accepts only the abbreviated form (see org_to_text.rs).
          "indef" =>
            metadata . indefinitive = true,
          "notInGit" =>
            metadata . truenode_not_in_git = true,
          _ => {
            return Err ( format! ( "Unknown node value: {}",
                                    bare_value )); }} },
      _ => { return Err ( "Unexpected element in node"
                           . to_string () ); }} }
  Ok (( )) }

/// Apply a sequence of axis atoms (newX, removedX, newM, removedM) to
/// a TrueNode's existence and membership axes for the given stage.
fn apply_axis_atoms_to_truenode (
  atoms      : &[Sexp],
  is_staged  : bool,
  existence  : &mut ExistenceAxes,
  membership : &mut MembershipAxes,
) -> Result<(), String> {
  for atom in atoms {
    let atom_str : String = atom_to_string (atom) ?;
    let (axis, sign) : (char, Sign) =
      Sign::parse_axis_atom (&atom_str)
        . ok_or_else ( || format! (
          "Unknown axis atom: {}", atom_str )) ?;
    let slot : &mut Option<Sign> = match (axis, is_staged) {
      ('X', true)  => &mut existence  . staged,
      ('X', false) => &mut existence  . unstaged,
      ('M', true)  => &mut membership . staged,
      ('M', false) => &mut membership . unstaged,
      _ => unreachable!(), };
    *slot = Some (sign); }
  Ok (( )) }

/// Apply axis atoms (newM, removedM only) to a Scaffold's membership.
fn apply_axis_atoms_to_membership_scaffold (
  atoms      : &[Sexp],
  is_staged  : bool,
  membership : &mut MembershipAxes,
) -> Result<(), String> {
  for atom in atoms {
    let atom_str : String = atom_to_string (atom) ?;
    let (axis, sign) : (char, Sign) =
      Sign::parse_axis_atom (&atom_str)
        . ok_or_else ( || format! (
          "Unknown axis atom: {}", atom_str )) ?;
    if axis != 'M' {
      return Err ( format! (
        "Scaffold (alias/id) only supports M axis atoms, got: {}",
        atom_str )); }
    let slot : &mut Option<Sign> =
      if is_staged { &mut membership . staged }
      else         { &mut membership . unstaged };
    *slot = Some (sign); }
  Ok (( )) }

/// Parse the (unknownNode (id X)) s-expression contents.
/// Sets metadata.unknown_node_id when an id is found.
fn parse_unknownnode_sexp (
  items    : &[Sexp],
  metadata : &mut ViewnodeMetadata,
) -> Result<(), String> {
  for element in items {
    match element {
      Sexp::List (subitems) if subitems . len () == 2 => {
        let key : String =
          atom_to_string ( &subitems[0] ) ?;
        match key . as_str () {
          "id" => {
            let value : String =
              atom_to_string ( &subitems[1] ) ?;
            metadata . unknown_node_id =
              Some ( ID::from (value)); },
          _ => { return Err ( format! (
            "Unknown unknownNode key: {}", key )); }} },
      _ => { return Err ( "Unexpected element in unknownNode sexp"
                           . to_string () ); }} }
  Ok (( )) }

/// Parse the (inactiveNode (id X) (source S) [(staged M)] [(unstaged M)])
/// s-expression contents.
fn parse_inactivenode_sexp (
  items    : &[Sexp],
  metadata : &mut ViewnodeMetadata,
) -> Result<(), String> {
  let mut id : Option<ID> = None;
  let mut source : Option<SourceName> = None;
  let mut membership : MembershipAxes =
    MembershipAxes::default ();
  for element in items {
    match element {
      Sexp::List (subitems) if subitems . len () >= 1 => {
        let key : String =
          atom_to_string ( &subitems[0] ) ?;
        match key . as_str () {
          "id" => {
            if subitems . len () != 2 {
              return Err (
                "inactiveNode id requires exactly one value"
                . to_string () ); }
            let value : String =
              atom_to_string ( &subitems[1] ) ?;
            id = Some ( ID::from (value)); },
          "source" => {
            if subitems . len () != 2 {
              return Err (
                "inactiveNode source requires exactly one value"
                . to_string () ); }
            let value : String =
              atom_to_string ( &subitems[1] ) ?;
            source = Some ( SourceName::from (value)); },
          "staged" => {
            apply_axis_atoms_to_membership_scaffold (
              &subitems[1..], true, &mut membership ) ?; },
          "unstaged" => {
            apply_axis_atoms_to_membership_scaffold (
              &subitems[1..], false, &mut membership ) ?; },
          _ => { return Err ( format! (
            "Unknown inactiveNode key: {}", key )); }} },
      _ => { return Err (
        "Unexpected element in inactiveNode sexp"
        . to_string () ); }}}
  metadata . id =
    Some ( id . ok_or (
      "inactiveNode requires an id" . to_string () )? );
  metadata . source =
    Some ( source . ok_or (
      "inactiveNode requires a source" . to_string () )? );
  metadata . is_inactive_node = true;
  metadata . inactive_membership = membership;
  Ok (( )) }

/// Parse the (deleted (id X) (source S)) s-expression contents.
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
            metadata . id = Some ( ID::from (value)); },
          "source" => {
            if subitems . len () != 2 {
              return Err ( "deleted source requires exactly one value" . to_string () ); }
            let value : String =
              atom_to_string ( &subitems[1] ) ?;
            metadata . source = Some ( SourceName::from (value)); },
          _ => { return Err ( format! ( "Unknown deleted key: {}",
                                         key )); }} },
      _ => { return Err ( "Unexpected element in deleted sexp"
                           . to_string () ); }} }
  Ok (( )) }


/// Parse the (graphStats ...) s-expression contents.
fn parse_graphstats_sexp (
  items : &[Sexp],
  stats : &mut GraphNodeStats
) -> Result<(), String> {
  // Accumulate containers/contents to build containRels.
  let mut saw_containers : Option<usize> = None;
  let mut saw_contents   : Option<usize> = None;
  // Accumulate linksIn counts to build linksourceRels.
  let mut saw_links_from_containers : Option<usize> = None;
  let mut saw_links_from_leaves     : Option<usize> = None;
  for element in items {
    match element {
      Sexp::Atom (_) => {
        let key : String = atom_to_string (element) ?;
        match key . as_str () {
          "aliasing"    => { stats . aliasing    = true; },
          "extraIDs"    => { stats . extraIDs    = true; },
          "overriding"  => { stats . overriding  = true; },
          "subscribing" => { stats . subscribing = true; },
          _ => { return Err ( format! (
            "Unknown graphStats atom: {}", key )); }} },
      Sexp::List (kv_pair) if kv_pair . len () == 2 => {
        let key : String =
          atom_to_string ( &kv_pair[0] ) ?;
        let value : String =
          atom_to_string ( &kv_pair[1] ) ?;
        match key . as_str () {
          "containers" => {
            saw_containers = Some (
              value . parse::<usize>()
                . map_err ( |_| format! (
                  "Invalid containers value: {}", value )) ? ); },
          "contents" => {
            saw_contents = Some (
              value . parse::<usize>()
                . map_err ( |_| format! (
                  "Invalid contents value: {}", value )) ? ); },
          "linksInFromContainers" => {
            saw_links_from_containers = Some (
              value . parse::<usize>()
                . map_err ( |_| format! (
                  "Invalid linksInFromContainers value: {}", value )) ? ); },
          "linksInFromLeaves" => {
            saw_links_from_leaves = Some (
              value . parse::<usize>()
                . map_err ( |_| format! (
                  "Invalid linksInFromLeaves value: {}", value )) ? ); },
          // Herald fields are output-only; silently discard on parse.
          "containsHerald" | "linksHerald" => {},
          // Legacy linksIn field: silently discard.
          "linksIn" => {},
          _ => { return Err ( format! ( "Unknown graphStats key: {}",
                                         key )); }} },
      _ => { return Err ( "Unexpected element in graphStats"
                           . to_string () ); }} }
  if saw_containers . is_some ()
    || saw_contents . is_some () {
    // Build containRels if either contain field was present.
    let cr : &mut NodeContainRels =
      stats . containRels . get_or_insert (
        NodeContainRels { containers : 1, contents : 0 } );
    if let Some (c) = saw_containers { cr . containers = c; }
    if let Some (c) = saw_contents   { cr . contents   = c; }}
  if saw_links_from_containers . is_some ()
    || saw_links_from_leaves . is_some () {
    // Build linksourceRels if either link field was present.
    let lr : &mut NodeLinksourceRels =
      stats . linksourceRels . get_or_insert (
        NodeLinksourceRels { sources_with_content    : 0,
                             sources_without_content : 0 } );
    if let Some (c) = saw_links_from_containers
      { lr . sources_with_content    = c; }
    if let Some (c) = saw_links_from_leaves
      { lr . sources_without_content = c; }}
  Ok (( )) }

/// Parse the (viewStats ...) s-expression contents.
/// Only handles bare atoms (cycle, containsParent).
fn parse_viewstats_sexp (
  items : &[Sexp],
  stats : &mut ViewNodeStats
) -> Result<(), String> {
  for element in items {
    match element {
      Sexp::Atom (_) => {
        let bare_value : String =
          atom_to_string (element) ?;
        match bare_value . as_str () {
          "cycle"          => stats . cycle = true,
          "containsParent" => stats . parentIsContent   = true,
          _ => {
            return Err ( format! ( "Unknown viewStats value: {}",
                                    bare_value )); }} },
      Sexp::List (kv_pair) if kv_pair . len () == 2 => {
        let key : String = atom_to_string ( &kv_pair[0] ) ?;
        match key . as_str () {
          "sourceHerald" => {}, // output-only, silently discard
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
  for element in items {
    match element {
      Sexp::List (subitems) if subitems . len () == 2 => {
        let key : String =
          atom_to_string ( &subitems[0] ) ?;
        if key == "merge" {
          let id_str : String =
            atom_to_string ( &subitems[1] ) ?;
          metadata . edit_request = Some (
            EditRequest::Merge ( ID::from (id_str)));
        } else {
          return Err ( format! ( "Unknown editRequest key: {}", key )); }
      },
      Sexp::Atom (_) => {
        let bare_value : String =
          atom_to_string (element) ?;
        match bare_value . as_str () {
          "delete" => metadata . edit_request = Some (EditRequest::Delete),
          _ => {
            return Err ( format! ( "Unknown editRequest value: {}",
                                    bare_value )); }} },
      _ => { return Err ( "Unexpected element in editRequest"
                           . to_string () ); }} }
  Ok (( )) }


/// Parse the (viewRequests ...) s-expression and update viewRequests.
fn parse_viewrequests_sexp (
  items : &[Sexp],
  requests : &mut HashSet<ViewRequest>
) -> Result<(), String> {
  for request_element in items {
    match request_element {
      Sexp::Atom (_) => {
        let request_str : String =
          atom_to_string (request_element) ?;
        let request : ViewRequest =
          ViewRequest::from_str (&request_str)
          . map_err (
            | e | format! ( "Invalid view request: {}", e )) ?;
        requests . insert (request); },
      _ => { return Err (
        "Unexpected element in viewRequests (expected atoms)"
          . to_string () ); }} }
  Ok (( )) }
