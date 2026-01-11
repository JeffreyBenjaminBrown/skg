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
use crate::types::orgnode::{TruenodeRelationships, EditRequest, ViewRequest};
use crate::types::orgnode::{
    OrgNode, OrgNodeKind, Scaffold, TrueNode, EffectOnParent,
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
  pub relationships: TruenodeRelationships,
  pub code: OrgnodeCode,
}

/// Code-related metadata (determines how node is saved).
#[derive(Debug, Clone, PartialEq)]
pub struct OrgnodeCode {
  pub interp: Interp,
  pub indefinitive: bool,
  pub editRequest: Option<EditRequest>,
  pub viewRequests: HashSet<ViewRequest>,
}

/// Interpretation of a node in the tree.
/// Internal to parsing; external code uses OrgNodeKind.
#[derive(Debug, Clone, PartialEq)]
pub enum Interp {
  ForestRoot,
  Content,
  AliasCol,
  Alias,
  ParentIgnores,
  SubscribeeCol,
  Subscribee,
  HiddenOutsideOfSubscribeeCol,
  HiddenInSubscribeeCol,
  HiddenFromSubscribees,
}

impl Interp {
  /// Convert scaffold Interps to Scaffold. Returns None for TrueNode interps.
  fn to_scaffold ( &self, title : &str ) -> Option < Scaffold > {
    match self {
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
      _ => None,
    }}

  /// Convert TrueNode Interps to EffectOnParent. Returns None for scaffold interps.
  fn to_effect_on_parent ( &self ) -> Option < EffectOnParent > {
    match self {
      Interp::Content =>
        Some ( EffectOnParent::Content ),
      Interp::Subscribee =>
        Some ( EffectOnParent::Subscribee ),
      Interp::ParentIgnores =>
        Some ( EffectOnParent::ParentIgnores ),
      Interp::HiddenFromSubscribees =>
        Some ( EffectOnParent::HiddenFromSubscribees ),
      _ => None,
    }}
}

impl Default for OrgnodeCode {
  fn default() -> Self {
    OrgnodeCode {
      interp: Interp::Content,
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
    relationships: TruenodeRelationships::default(),
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
        Some ( match &scaffold {
          Scaffold::AliasCol =>
            BufferValidationError::Body_of_AliasCol (
              title.clone () ),
          Scaffold::Alias ( _ ) =>
            BufferValidationError::Body_of_Alias (
              title.clone () ),
          _ => BufferValidationError::Other ( format! ( "Scaffold {:?} should not have a body", scaffold ) ), } )
      } else { None };
      ( OrgNodeKind::Scaff ( scaffold ), error )
    } else if let Some ( effect ) = interp . to_effect_on_parent () {
      ( OrgNodeKind::True ( TrueNode {
          title,
          body,
          id_opt           : metadata . id . clone (),
          source_opt       : metadata . source . clone (),
          effect_on_parent : effect,
          indefinitive     : metadata . code . indefinitive,
          cycle            : metadata . cycle,
          relationships    : metadata . relationships . clone (),
          edit_request     : metadata . code . editRequest . clone (),
          view_requests    : metadata . code . viewRequests . clone (), } ),
        None )
    } else { panic! ( "Invalid Interp: {:?}", interp ) };
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
          parse_rels_sexp ( &subitems[1..], &mut metadata . relationships ) ?;
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
            code . interp = match value . as_str () {
              "alias"         => Interp::Alias,
              "aliasCol"      => Interp::AliasCol,
              "content"       => Interp::Content,
              "parentIgnores" => Interp::ParentIgnores,
              "subscribeeCol" => Interp::SubscribeeCol,
              "subscribee"    => Interp::Subscribee,
              "hiddenOutsideOfSubscribeeCol" => Interp::HiddenOutsideOfSubscribeeCol,
              "hiddenInSubscribeeCol"        => Interp::HiddenInSubscribeeCol,
              "hiddenFromSubscribees"        => Interp::HiddenFromSubscribees,
              _ => return Err (
                format! ( "Unknown interp value: {}", value )),
            }; },
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


/// Parse the (rels ...) s-expression and update relationships.
fn parse_rels_sexp (
  items : &[Sexp],
  relationships : &mut TruenodeRelationships
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
            relationships . numContainers = Some (
              rel_value.parse::<usize>()
                . map_err ( |_| format! (
                  "Invalid containers value: {}", rel_value )) ? ); },
          "contents" => {
            relationships . numContents = Some (
              rel_value.parse::<usize>()
                . map_err ( |_| format! (
                  "Invalid contents value: {}", rel_value )) ? ); },
          "linksIn" => {
            relationships . numLinksIn = Some (
              rel_value.parse::<usize>()
                . map_err ( |_| format! (
                  "Invalid linksIn value: {}", rel_value )) ? ); },
          _ => { return Err ( format! ( "Unknown rels key: {}",
                                         rel_key )); }} },
      Sexp::Atom ( _ ) => {
        let bare_value : String =
          atom_to_string ( rel_element ) ?;
        match bare_value . as_str () {
          "notInParent"    => relationships . parentIsContainer = false,
          "containsParent" => relationships . parentIsContent   = true,
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
