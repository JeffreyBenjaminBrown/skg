/// PURPOSE: Parse (skg ...) metadata s-expressions from org headlines.
///
/// Format:
///   Scaffolds: (skg [focused] [folded] scaffoldKind)
///   ActiveNodes: (skg [focused] [folded]
///                   (node [(id ID)]
///                         [(source SOURCE)]
///                         [(parentIs affected|independent|absent)]
///                         [(birth backpath ROLENAME)]
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
  GraphNodeStats, ViewNodeStats, EditRequest, ViewRequest, ColRelation,
  Qual, QualCol, PartnerCol, PhantomDeleted, InactiveNode, PhantomUnknown,
  Birth, IndefOrDef, ParentIs,
};
use crate::dbs::in_rust_graph::relation_accessors::RelationRole;
use crate::types::maybe_placed_viewnode::{
    MpViewnode, MpViewnodeKind, MpActiveNode, MpPhantomDiff,
    MpVognode, MpPhantom,
};

use sexp::Sexp;
use std::collections::HashSet;

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
  // ActiveNode fields (ignored if scaffold is Some)
  pub id: Option<ID>,
  pub source: Option<SourceName>,
  pub parentIs: ParentIs,
  pub birth: Birth,
  pub indefinitive: bool,
  pub graphStats: GraphNodeStats,
  pub viewStats: ViewNodeStats,
  pub edit_request: Option<EditRequest>,
  pub view_requests: HashSet<ViewRequest>,
  pub activeNode_existence  : ExistenceAxes,
  pub activeNode_membership : MembershipAxes,
  pub activeNode_not_in_git : bool,
  pub scaffold_membership : MembershipAxes,
  pub textchanged_staged   : bool,
  pub textchanged_unstaged : bool,
  // When true, this is a PhantomDeleted (id and source are used).
  pub is_deleted_node: bool,
  // When true, this is a deleted non-vognode placeholder.
  pub is_dead_scaffold: bool,
  // When Some, this is an PhantomUnknown (a placeholder for a missing
  // referent). Carries only the id; no source/title/body apply.
  pub unknown_node_id: Option<ID>,
  // When true, this is an inactive-source placeholder: an anonymous,
  // dataless marker (see InactiveNode). It carries no id/source/etc.
  pub is_inactive_node : bool,
  // When true, this is a PhantomDiff. It carries the same fields as a
  // node (id/source/indef/graphStats/diff axes), parsed via
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
    id: None,
    source: None,
    parentIs: ParentIs::Affected,
    birth: Birth::Unremarkable,
    indefinitive: false,
    graphStats: GraphNodeStats::default(),
    viewStats: ViewNodeStats::default(),
    edit_request: None,
    view_requests: HashSet::new(),
    activeNode_existence  : ExistenceAxes::default(),
    activeNode_membership : MembershipAxes::default(),
    activeNode_not_in_git : false,
    scaffold_membership : MembershipAxes::default(),
    textchanged_staged   : false,
    textchanged_unstaged : false,
    is_deleted_node: false,
    is_dead_scaffold: false,
    unknown_node_id: None,
    is_inactive_node: false,
    is_diff_phantom: false, }}

/// Create an MpViewnode from parsed metadata components.
/// This is the bridge between parsing (ViewnodeMetadata) and runtime (MpViewnode).
/// Returns (MpViewnode, error, warning):
/// - error if a Scaffold has a body;
/// - warning if a col header (QualCol or PartnerCol) has nonempty
///   title text, which the server discards: col headlines are
///   titleless server-side (heralds supply their labels), so any
///   text there is a user edit that cannot be saved. Qual leaves
///   are exempt -- their title IS their data.
pub fn viewnode_from_metadata (
  metadata : &ViewnodeMetadata,
  title    : String,
  body     : Option < String >,
) -> ( MpViewnode, Option < BufferValidationError >, Option < String > ) {
  let (kind, error, warning)
    : (MpViewnodeKind, Option<BufferValidationError>, Option<String>)
    = if let Some (ref uid) = metadata . unknown_node_id {
        ( MpViewnodeKind::Phantom (
            MpPhantom::Unknown (
              PhantomUnknown { id: uid . clone () } ) ), None, None )
      } else if metadata . is_inactive_node {
        let error : Option<BufferValidationError> =
          if body . is_some ()
          || ! title . is_empty () {
            Some ( BufferValidationError::Other (
              "Inactive placeholder content cannot be edited"
              . to_string () ))
          } else { None };
        ( MpViewnodeKind::Vognode (
            MpVognode::Inactive ( InactiveNode ) ),
          error, None )
      } else if metadata . is_dead_scaffold {
        ( MpViewnodeKind::DeadScaffold, None, None )
      } else if metadata . is_deleted_node {
        ( MpViewnodeKind::Phantom (
            MpPhantom::Deleted ( PhantomDeleted {
            id     : metadata . id . clone ()
                       . unwrap_or_else ( || ID::from ("")),
            source : metadata . source . clone ()
                       . unwrap_or_else ( || SourceName::from ("")),
            title,
            body,
          } ) ), None, None )
      } else if let Some ( ref non_vognode ) = metadata . non_vognode {
        let error : Option<BufferValidationError> =
          if body . is_some () {
            Some ( BufferValidationError::Body_of_Scaffold (
              title . clone (),
              maybeplaced_kind_error_label (non_vognode) ))
          } else { None };
        let col_title_warning : Option<String> =
          if ! title . is_empty ()
            && matches! ( non_vognode,
                          MpViewnodeKind::QualCol (_)
                          | MpViewnodeKind::PartnerCol (_) )
          { Some ( format! (
              "Headline text on a {} is not saved; discarded: {:?}",
              maybeplaced_kind_error_label (non_vognode),
              title )) }
          else { None };
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
        ( non_vognode_with_title, error, col_title_warning )
      } else {
      // MpActiveNode
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
        let t : MpActiveNode = MpActiveNode {
            title,
            id               : metadata . id . clone (),
            source           : metadata . source . clone (),
            parentIs         : metadata . parentIs,
            birth            : metadata . birth,
            graphStats       : metadata . graphStats . clone (),
            viewStats        : metadata . viewStats . clone (),
            view_requests    : metadata . view_requests . clone (),
            existence        : metadata . activeNode_existence,
            membership       : metadata . activeNode_membership,
            not_in_git       : metadata . activeNode_not_in_git,
            indef_or_def, };
        let node_kind : MpViewnodeKind =
          if metadata . is_diff_phantom
          { // TODO/DONE/local-view-update/plan_v2.org §11: a phantom carries only the slim MpPhantomDiff. The
            // root atom 'diffPhantom' (not the diff axes) decides this, so a
            // live node carrying e.g. removedM stays a Vognode. The
            // EditRequestOnIndefinitive validation above already fired if this
            // phantom (indefinitive) carried an edit_request, so dropping
            // indef_or_def/parentIs/etc. here loses nothing.
            MpViewnodeKind::Phantom (
              MpPhantom::Diff (
                MpPhantomDiff::from_activeNode (t) )) }
          else
          { MpViewnodeKind::Vognode ( MpVognode::Active (t) ) };
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
    MpViewnodeKind::QualCol (col) =>
      col . repr_in_client () . to_string (),
    MpViewnodeKind::Qual (qual) =>
      qual . repr_in_client () . to_string (),
    MpViewnodeKind::PartnerCol (partnerCol) =>
      partnerCol . repr_in_client () . to_string (),
    MpViewnodeKind::BufferRoot =>
      "forestRoot" . to_string (),
    MpViewnodeKind::DeadScaffold =>
      "deadScaffold" . to_string (),
    MpViewnodeKind::Vognode (_) | MpViewnodeKind::Phantom (_) =>
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
            // mode. Same field grammar as (node ...) (id/source/indef/
            // graphStats/diff axes), but its own root atom so the client
            // and round-trip never infer phantom-ness from the diff axes.
            parse_node_sexp ( &items[1..], &mut result ) ?;
            result . is_diff_phantom = true; },
          "deleted" => {
            result . is_deleted_node = true;
            parse_deleted_sexp ( &items[1..], &mut result ) ?; },
          "unknown" => {
            // (unknown (id X)) -- placeholder for a referenced
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
               The new format uses (skg [focused] [folded] (node ...)) for ActiveNodes \
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
          // An inactive placeholder is a dataless bare-atom marker
          // (see InactiveNode). The legacy field-bearing list form
          // '(inactiveNode ...)' is still tolerated by the List arm
          // above so a stale buffer round-trips.
          "inactiveNode" => result . is_inactive_node = true,
          // Scaffold kinds as bare atoms (alias/id string comes from title in viewnode_from_metadata)
          "alias"    => result . non_vognode = Some ( MpViewnodeKind::Qual ( Qual::Alias { text: String::new(), membership: MembershipAxes::default() } ) ),
          "aliasCol" => result . non_vognode = Some (MpViewnodeKind::QualCol (QualCol::Alias)),
          "forestRoot" => result . non_vognode = Some (MpViewnodeKind::BufferRoot),
          "hiddenInSubscribeeCol" =>
            result . non_vognode = Some (MpViewnodeKind::PartnerCol (PartnerCol::HiddenInSubscribee)),
          "hiddenOutsideOfSubscribeeCol" =>
            result . non_vognode = Some (MpViewnodeKind::PartnerCol (PartnerCol::HiddenOutsideOfSubscribee)),
          "hiddenCol" =>
            result . non_vognode = Some (MpViewnodeKind::PartnerCol (PartnerCol::Hidden)),
          "hiderCol" =>
            result . non_vognode = Some (MpViewnodeKind::PartnerCol (PartnerCol::Hider)),
          "overriddenCol" =>
            result . non_vognode = Some (MpViewnodeKind::PartnerCol (PartnerCol::Overridden)),
          "overriderCol" =>
            result . non_vognode = Some (MpViewnodeKind::PartnerCol (PartnerCol::Overrider)),
          "subscriberCol" =>
            result . non_vognode = Some (MpViewnodeKind::PartnerCol (PartnerCol::Subscriber)),
          "subscribeeCol" =>
            result . non_vognode = Some (MpViewnodeKind::PartnerCol (PartnerCol::Subscribee)),
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
          // The relationship / birth heralds are display-only strings
          // (assembled in Rust, echoed by the rule table). Emacs strips
          // them before save; if a stale buffer still carries them, we
          // accept and discard -- the view regenerates them.
          "rels" | "birthHerald" => {},
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
          "staged" => {
            apply_axis_atoms_to_activeNode (
              &subitems[1..],
              true,  // staged
              &mut metadata . activeNode_existence,
              &mut metadata . activeNode_membership ) ?; },
          "unstaged" => {
            apply_axis_atoms_to_activeNode (
              &subitems[1..],
              false, // unstaged
              &mut metadata . activeNode_existence,
              &mut metadata . activeNode_membership ) ?; },
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
          "hiddenBody" =>
            // Display-only (like rels/birthHerald): the view
            // regenerates it, so accept and discard.
            {},
          "notInGit" =>
            metadata . activeNode_not_in_git = true,
          _ => {
            return Err ( format! ( "Unknown node value: {}",
                                    bare_value )); }} },
      _ => { return Err ( "Unexpected element in node"
                           . to_string () ); }} }
  Ok (( )) }

/// Apply a sequence of axis atoms (newX, removedX, newM, removedM) to
/// an ActiveNode's existence and membership axes for the given stage.
fn apply_axis_atoms_to_activeNode (
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

/// Parse the (unknown (id X)) s-expression contents.
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
            "Unknown 'unknown' key: {}", key )); }} },
      _ => { return Err ( "Unexpected element in unknown sexp"
                           . to_string () ); }} }
  Ok (( )) }

/// Parse the LEGACY list form '(inactiveNode ...)'. The server now
/// emits the bare atom 'inactiveNode' (handled in the atom arm), but an
/// older server's field-bearing list form is tolerated here -- its
/// children (id/source/membership/overridesHere) are discarded -- so a
/// stale buffer still round-trips. An inactive placeholder is an
/// anonymous, dataless marker (see InactiveNode).
fn parse_inactivenode_sexp (
  _items   : &[Sexp],
  metadata : &mut ViewnodeMetadata,
) -> Result<(), String> {
  metadata . is_inactive_node = true;
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


/// Parse the (viewStats ...) s-expression contents: the bare-atom
/// 'cycle' stat and the keyed 'overridesHere' / 'sourceHerald'
/// sub-forms. (The relationship stats are now display-only herald
/// strings, parsed -- and discarded -- as the 'rels'/'birthHerald'
/// atoms instead.)
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
          _ => {
            return Err ( format! ( "Unknown viewStats value: {}",
                                    bare_value )); }} },
      Sexp::List (kv_pair) if kv_pair . len () == 2 => {
        let key : String = atom_to_string ( &kv_pair[0] ) ?;
        match key . as_str () {
          "sourceHerald" => {}, // output-only, silently discard
          "overridesHere" => {
            // LOAD-BEARING, unlike the other view stats: save
            // extraction round-trips the original ID through it.
            let value : String =
              atom_to_string ( &kv_pair[1] ) ?;
            stats . overridesHere = Some ( ID::from (value)); },
          "relSource" => {
            // LOAD-BEARING (see ViewNodeStats::rel_source): the
            // buffer's explicit privacy level for this position's
            // binding edge, round-tripped from
            // skg-privatize-relationship into save extraction, which
            // feeds save-leveling's sticky-else-default resolution
            // above its floor.
            let value : String =
              atom_to_string ( &kv_pair[1] ) ?;
            stats . rel_source = Some ( SourceName::from (value)); },
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
            EditRequest::NodeMerge ( ID::from (id_str)));
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
/// Each request is either the bare atom 'definitiveView', or a nested
/// '(col RELNAME)' / '(path ROLENAME)' form.
fn parse_viewrequests_sexp (
  items : &[Sexp],
  requests : &mut HashSet<ViewRequest>
) -> Result<(), String> {
  for request_element in items {
    let request : ViewRequest = match request_element {
      Sexp::Atom (_) => {
        let atom : String = atom_to_string (request_element) ?;
        if atom == "definitiveView" { ViewRequest::Definitive }
        else if atom == "fork" { ViewRequest::Fork }
        else { return Err ( format! (
          "Invalid view request atom: {}", atom )); } },
      Sexp::List (sub) if sub . len () == 2 => {
        let head : String = atom_to_string ( &sub[0] ) ?;
        let arg  : String = atom_to_string ( &sub[1] ) ?;
        match head . as_str () {
          "col"  => ViewRequest::Col (
            ColRelation::from_relname (&arg)
              . ok_or_else ( || format! (
                "Invalid col relname: {}", arg )) ?),
          "path" => ViewRequest::Path (
            RelationRole::from_rolename (&arg)
              . ok_or_else ( || format! (
                "Invalid path rolename: {}", arg )) ?),
          _ => return Err ( format! (
            "Unknown view request form: ({} ...)", head )), } },
      _ => return Err (
        "Unexpected element in viewRequests (expected 'definitiveView' \
         or '(col ...)' / '(path ...)')" . to_string () ), };
    requests . insert (request); }
  Ok (( )) }
