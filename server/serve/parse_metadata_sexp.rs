/// PURPOSE: Parse (skg ...) metadata s-expressions from org headlines.
///
/// WARNING: I had Claude auto-refactor a lot of code,
/// just after commit bb8da95e14f1243d54926e58de612f7b1969487a,
/// to improve the type hierarchy. The old types have persisted
/// in this module. Many of the names involved are bad.
///
/// TODO ? Rewrite this whole thing from scratch.

use crate::types::sexp::atom_to_string;
use crate::types::misc::ID;
use crate::types::errors::BufferValidationError;
use crate::types::orgnode::{TrueNodeStats, EditRequest, ViewRequest};
use crate::types::orgnode::{
    OrgNode, OrgNodeKind, Scaffold, ScaffoldKind, TrueNode,
};

use sexp::Sexp;
use std::collections::HashSet;
use std::str::FromStr;

//
// Parsing-internal types
// These are used by the parser to produce OrgNode.
//

/// Intermediate parsed metadata. Converted to OrgNode via from_parsed().
#[derive(Debug, Clone, PartialEq)]
pub struct OrgnodeMetadata {
  pub id: Option<ID>,
  pub source: Option<String>,
  pub focused: bool,
  pub folded: bool,
  pub cycle: bool,
  pub stats: TrueNodeStats,
  pub code: OrgnodeCode,
}

/// Code-related metadata (determines how node is saved).
#[derive(Debug, Clone, PartialEq)]
pub struct OrgnodeCode {
  pub interp: Interp,
  pub parent_ignores: bool,
  pub indefinitive: bool,
  pub editRequest: Option<EditRequest>,
  pub viewRequests: HashSet<ViewRequest>,
}

/// Interpretation of a node in the tree.
/// Internal to parsing; scaffold variants map to Scaffold, TrueNode is default.
#[derive(Debug, Clone, PartialEq)]
pub enum Interp {
  TrueNode, // Default for non-scaffold nodes
  ForestRoot,
  AliasCol,
  Alias,
  SubscribeeCol,
  HiddenOutsideOfSubscribeeCol,
  HiddenInSubscribeeCol,
}

impl Interp {
  /// Convert scaffold Interps to Scaffold. Returns None for TrueNode.
  fn to_scaffold ( &self, title : &str ) -> Option < Scaffold > {
    match self {
      Interp::TrueNode => None,
      Interp::ForestRoot =>
        Some ( Scaffold::ForestRoot ),
      Interp::AliasCol =>
        Some ( Scaffold::AliasCol ),
      Interp::Alias =>
        Some ( Scaffold::Alias ( title.to_string() )),
      Interp::SubscribeeCol =>
        Some ( Scaffold::SubscribeeCol ),
      Interp::HiddenOutsideOfSubscribeeCol =>
        Some ( Scaffold::HiddenOutsideOfSubscribeeCol ),
      Interp::HiddenInSubscribeeCol =>
        Some ( Scaffold::HiddenInSubscribeeCol ),
    }}
}

impl Default for OrgnodeCode {
  fn default() -> Self {
    OrgnodeCode {
      interp: Interp::TrueNode,
      parent_ignores: false,
      indefinitive: false,
      editRequest: None,
      viewRequests: HashSet::new(),
    }
  }
}

pub fn default_metadata() -> OrgnodeMetadata {
  OrgnodeMetadata {
    id: None,
    source: None,
    focused: false,
    folded: false,
    cycle: false,
    stats: TrueNodeStats::default(),
    code: OrgnodeCode::default(),
  }
}

/// Create an OrgNode from parsed metadata components.
/// This is the bridge between parsing (OrgnodeMetadata) and runtime (OrgNode).
/// Returns (OrgNode, Option<BufferValidationError>) - error if Scaffold has body.
pub fn from_parsed (
  metadata : &OrgnodeMetadata,
  title    : String,
  body     : Option < String >,
) -> ( OrgNode, Option < BufferValidationError > ) {
  let interp = &metadata . code . interp;
  let (kind, error) =
    if let Some ( scaffold ) = interp . to_scaffold ( &title ) {
      let error = if body . is_some () {
        Some ( BufferValidationError::Body_of_Scaffold (
          title . clone (),
          scaffold . repr_in_client () ))
      } else { None };
      ( OrgNodeKind::Scaff ( scaffold ), error )
    } else {
      // Interp::TrueNode
      ( OrgNodeKind::True ( TrueNode {
          title,
          body,
          id_opt           : metadata . id . clone (),
          source_opt       : metadata . source . clone (),
          parent_ignores   : metadata . code . parent_ignores,
          indefinitive     : metadata . code . indefinitive,
          cycle            : metadata . cycle,
          stats            : metadata . stats . clone (),
          edit_request     : metadata . code . editRequest . clone (),
          view_requests    : metadata . code . viewRequests . clone (), } ),
        None )
    };
  ( OrgNode { focused : metadata.focused,
              folded  : metadata.folded,
              kind },
    error ) }


/// Parse metadata from org-mode headline into OrgnodeMetadata.
/// Format: "(skg (id xyz) (view ...) (code (requests ...) ...))"
/// Takes the full s-expression including the "(skg ...)" wrapper.
pub fn parse_metadata_to_orgnodemd (
  sexp_str : &str
) -> Result<OrgnodeMetadata, String> {
  let mut result : OrgnodeMetadata =
    default_metadata ();

  let parsed : Sexp =
    sexp::parse ( sexp_str )
    . map_err ( |e| format! ( "Failed to parse metadata as s-expression: {}", e ) ) ?;

  // Extract the list of elements from (skg ...)
  let elements : &[Sexp] =
    match &parsed {
      Sexp::List ( items ) => {
        // First element should be the symbol 'skg'
        if items . is_empty () {
          return Err ( "Empty metadata s-expression".to_string () ); }
        // Skip the 'skg' symbol and return the rest
        &items[1..]
      },
      _ => return Err ( "Expected metadata to be a list".to_string () ),
    };

  // Process each element
  for element in elements {
    match element {
      Sexp::List ( items ) if items . len () >= 1 => {
        let first : String =
          atom_to_string ( &items[0] ) ?;
        match first . as_str () {
          "id" => {
            if items . len () != 2 {
              return Err ( "id requires exactly one value".to_string () ); }
            let value : String =
              atom_to_string ( &items[1] ) ?;
            result.id = Some ( ID::from ( value )); },
          "source" => {
            if items . len () != 2 {
              return Err ( "source requires exactly one value".to_string () ); }
            let value : String =
              atom_to_string ( &items[1] ) ?;
            result.source = Some ( value ); },
          "view" => {
            parse_view_sexp ( &items[1..], &mut result ) ?; },
          "code" => {
            parse_code_sexp ( &items[1..], &mut result . code ) ?; },
          _ => { return Err ( format! ( "Unknown metadata key: {}",
                                         first )); }} },
      _ => { return Err ( format! (
        "Unexpected element '{}' in metadata sexp: {}",
        element, sexp_str )); }} }
  Ok ( result ) }


/// Parse the (view ...) s-expression and update metadata.
fn parse_view_sexp (
  items : &[Sexp],
  metadata : &mut OrgnodeMetadata
) -> Result<(), String> {
  for view_element in items {
    match view_element {
      Sexp::List ( subitems ) if subitems . len () >= 2 => {
        let key : String =
          atom_to_string ( &subitems[0] ) ?;
        if key == "rels" {
          parse_rels_sexp ( &subitems[1..], &mut metadata . stats ) ?;
        } else {
          return Err ( format! ( "Unknown view key: {}", key )); }
      },
      Sexp::Atom ( _ ) => {
        let bare_value : String =
          atom_to_string ( view_element ) ?;
        match bare_value . as_str () {
          "cycle"    => metadata . cycle = true,
          "focused"  => metadata . focused = true,
          "folded"   => metadata . folded = true,
          _ => {
            return Err ( format! ( "Unknown view value: {}",
                                    bare_value )); }} },
      _ => { return Err ( "Unexpected element in view"
                           . to_string () ); }} }
  Ok (( )) }

/// Parse the (code ...) s-expression and update code.
fn parse_code_sexp (
  items : &[Sexp],
  code : &mut OrgnodeCode
) -> Result<(), String> {
  for code_element in items {
    match code_element {
      Sexp::List ( kv_pair ) if kv_pair . len () == 2 => {
        let key : String =
          atom_to_string ( &kv_pair[0] ) ?;
        match key . as_str () {
          "interp" => {
            let value : String =
              atom_to_string ( &kv_pair[1] ) ?;
            if let Some ( scaffold ) = Scaffold::from_repr_in_client (
                 &value, "" ) // title unused for kind detection
            { match scaffold.kind() { // Try scaffold strings first
                ScaffoldKind::Alias =>
                  code . interp = Interp::Alias,
                ScaffoldKind::AliasCol =>
                  code . interp = Interp::AliasCol,
                ScaffoldKind::ForestRoot =>
                  code . interp = Interp::ForestRoot,
                ScaffoldKind::HiddenInSubscribeeCol =>
                  code . interp = Interp::HiddenInSubscribeeCol,
                ScaffoldKind::HiddenOutsideOfSubscribeeCol =>
                  code . interp = Interp::HiddenOutsideOfSubscribeeCol,
                ScaffoldKind::SubscribeeCol =>
                  code . interp = Interp::SubscribeeCol, }}
            else if value == "parentIgnores" {
              // TrueNode with parent_ignores=true
              code . interp = Interp::TrueNode;
              code . parent_ignores = true; }
            else if value == "content"
                 || value == "subscribee"
                 || value == "hiddenFromSubscribees"
            { return Err ( format! (
                "Legacy interp value '{}' is no longer supported. \
                 Remove the (interp ...) clause; these are now implicit from tree structure.",
                value )); }
            else { return Err ( format! ( "Unknown interp value: {}", value )) }; },
          "merge" => {
            // (merge id) sets editRequest to Merge(id)
            let id_str : String = atom_to_string ( &kv_pair[1] ) ?;
            code . editRequest = Some (
              EditRequest::Merge ( ID::from ( id_str ))); },
          "viewRequests" => {
            parse_viewrequests_sexp (
              &kv_pair[1..], &mut code . viewRequests ) ?; },
          _ => { return Err ( format! ( "Unknown code key: {}",
                                         key )); }} },
      Sexp::Atom ( _ ) => {
        let bare_value : String =
          atom_to_string ( code_element ) ?;
        match bare_value . as_str () {
          "indefinitive" => code . indefinitive = true,
          "toDelete"     => code . editRequest =
            Some ( EditRequest::Delete ),
          _ => { return Err ( format! ( "Unknown code value: {}",
                                         bare_value )); }} },
      _ => { return Err ( "Unexpected element in code"
                           . to_string () ); }} }
  Ok (( )) }


/// Parse the (rels ...) s-expression and update stats.
fn parse_rels_sexp (
  items : &[Sexp],
  stats : &mut TrueNodeStats
) -> Result<(), String> {
  for rel_element in items {
    match rel_element {
      Sexp::List ( kv_pair ) if kv_pair . len () == 2 => {
        let rel_key : String =
          atom_to_string ( &kv_pair[0] ) ?;
        let rel_value : String =
          atom_to_string ( &kv_pair[1] ) ?;
        match rel_key . as_str () {
          "containers" => {
            stats . numContainers = Some (
              rel_value.parse::<usize>()
                . map_err ( |_| format! (
                  "Invalid containers value: {}", rel_value )) ? ); },
          "contents" => {
            stats . numContents = Some (
              rel_value.parse::<usize>()
                . map_err ( |_| format! (
                  "Invalid contents value: {}", rel_value )) ? ); },
          "linksIn" => {
            stats . numLinksIn = Some (
              rel_value.parse::<usize>()
                . map_err ( |_| format! (
                  "Invalid linksIn value: {}", rel_value )) ? ); },
          _ => { return Err ( format! ( "Unknown rels key: {}",
                                         rel_key )); }} },
      Sexp::Atom ( _ ) => {
        let bare_value : String =
          atom_to_string ( rel_element ) ?;
        match bare_value . as_str () {
          "notInParent"    => stats . parentIsContainer = false,
          "containsParent" => stats . parentIsContent   = true,
          _ => {
            return Err ( format! ( "Unknown rels value: {}",
                                    bare_value )); }} },
      _ => { return Err ( "Unexpected element in rels"
                           . to_string () ); }} }
  Ok (( )) }

/// Parse the (viewRequests ...) s-expression and update viewRequests.
fn parse_viewrequests_sexp (
  items : &[Sexp],
  requests : &mut HashSet<ViewRequest>
) -> Result<(), String> {
  for request_element in items {
    match request_element {
      Sexp::Atom ( _ ) => {
        let request_str : String =
          atom_to_string ( request_element ) ?;
        let request : ViewRequest =
          ViewRequest::from_str ( &request_str )
          . map_err (
            | e | format! ( "Invalid view request: {}", e )) ?;
        requests . insert ( request ); },
      _ => { return Err (
        "Unexpected element in viewRequests (expected atoms)"
          . to_string () ); }} }
  Ok (( )) }
