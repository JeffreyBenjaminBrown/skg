/// PURPOSE: Parse (skg ...) metadata s-expressions from org headlines.
///
/// Format:
///   Scaffolds: (skg [focused] [folded] scaffoldKind)
///   TrueNodes: (skg [focused] [folded] (node [(id ID)]
///                                            [(source SOURCE)]
///                                            [parentIgnores]
///                                            [indefinitive]
///                                            [cycle]
///                                            [(stats [notInParent]
///                                                    [containsParent]
///                                                    [(containers N)]
///                                                    [(contents N)]
///                                                    [(linksIn N)])]
///                                            [(editRequest <delete | (merge ID)>)]
///                                            [(viewRequests REQUEST...)]))

use crate::types::sexp::atom_to_string;
use crate::types::misc::ID;
use crate::types::errors::BufferValidationError;
use crate::types::orgnode::{TrueNodeStats, EditRequest, ViewRequest};
use crate::types::orgnode::{
    OrgNode, OrgNodeKind, Scaffold, TrueNode,
};

use sexp::Sexp;
use std::collections::HashSet;
use std::str::FromStr;

//
// Parsing-internal types
//

/// Intermediate parsed metadata. Converted to OrgNode via orgnode_from_metadata().
#[derive(Debug, Clone, PartialEq)]
pub struct OrgnodeMetadata {
  pub focused: bool,
  pub folded: bool,
  // None means TrueNode, Some means Scaffold
  pub scaffold: Option<Scaffold>,
  // TrueNode fields (ignored if scaffold is Some)
  pub id: Option<ID>,
  pub source: Option<String>,
  pub parent_ignores: bool,
  pub indefinitive: bool,
  pub cycle: bool,
  pub stats: TrueNodeStats,
  pub edit_request: Option<EditRequest>,
  pub view_requests: HashSet<ViewRequest>,
}

pub fn default_metadata() -> OrgnodeMetadata {
  OrgnodeMetadata {
    focused: false,
    folded: false,
    scaffold: None,
    id: None,
    source: None,
    parent_ignores: false,
    indefinitive: false,
    cycle: false,
    stats: TrueNodeStats::default(),
    edit_request: None,
    view_requests: HashSet::new(),
  }
}

/// Create an OrgNode from parsed metadata components.
/// This is the bridge between parsing (OrgnodeMetadata) and runtime (OrgNode).
/// Returns (OrgNode, Option<BufferValidationError>) - error if Scaffold has body.
pub fn orgnode_from_metadata (
  metadata : &OrgnodeMetadata,
  title    : String,
  body     : Option < String >,
) -> ( OrgNode, Option < BufferValidationError > ) {
  let (kind, error) =
    if let Some ( ref scaffold ) = metadata . scaffold {
      let error = if body . is_some () {
        Some ( BufferValidationError::Body_of_Scaffold (
          title . clone (),
          scaffold . repr_in_client () ))
      } else { None };
      // For Alias, use the headline title as the alias string
      let scaffold_with_title = match scaffold {
        Scaffold::Alias ( _ ) => Scaffold::Alias ( title . clone () ),
        other => other . clone (),
      };
      ( OrgNodeKind::Scaff ( scaffold_with_title ), error )
    } else {
      // TrueNode
      ( OrgNodeKind::True ( TrueNode {
          title,
          body,
          id_opt           : metadata . id . clone (),
          source_opt       : metadata . source . clone (),
          parent_ignores   : metadata . parent_ignores,
          indefinitive     : metadata . indefinitive,
          cycle            : metadata . cycle,
          stats            : metadata . stats . clone (),
          edit_request     : metadata . edit_request . clone (),
          view_requests    : metadata . view_requests . clone (), } ),
        None )
    };
  ( OrgNode { focused : metadata.focused,
              folded  : metadata.folded,
              kind },
    error ) }


/// Parse metadata from org-mode headline into OrgnodeMetadata.
/// See file header comment for full syntax.
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
          "node" => {
            parse_node_sexp ( &items[1..], &mut result ) ?; },
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
      Sexp::Atom ( _ ) => {
        let bare_value : String =
          atom_to_string ( element ) ?;
        match bare_value . as_str () {
          "focused"  => result . focused = true,
          "folded"   => result . folded = true,
          // Scaffold kinds as bare atoms (alias string comes from title in orgnode_from_metadata)
          "alias"    => result . scaffold = Some ( Scaffold::Alias ( String::new () )),
          "aliasCol" => result . scaffold = Some ( Scaffold::AliasCol ),
          "forestRoot" => result . scaffold = Some ( Scaffold::ForestRoot ),
          "hiddenInSubscribeeCol" =>
            result . scaffold = Some ( Scaffold::HiddenInSubscribeeCol ),
          "hiddenOutsideOfSubscribeeCol" =>
            result . scaffold = Some ( Scaffold::HiddenOutsideOfSubscribeeCol ),
          "subscribeeCol" =>
            result . scaffold = Some ( Scaffold::SubscribeeCol ),
          _ => {
            return Err ( format! ( "Unknown top-level value: {}",
                                    bare_value )); }} },
      _ => { return Err ( format! (
        "Unexpected element in metadata sexp: {}",
        sexp_str )); }} }
  Ok ( result ) }


/// Parse the (node ...) s-expression contents.
fn parse_node_sexp (
  items : &[Sexp],
  metadata : &mut OrgnodeMetadata
) -> Result<(), String> {
  for element in items {
    match element {
      Sexp::List ( subitems ) if subitems . len () >= 1 => {
        let key : String =
          atom_to_string ( &subitems[0] ) ?;
        match key . as_str () {
          "id" => {
            if subitems . len () != 2 {
              return Err ( "id requires exactly one value".to_string () ); }
            let value : String =
              atom_to_string ( &subitems[1] ) ?;
            metadata . id = Some ( ID::from ( value )); },
          "source" => {
            if subitems . len () != 2 {
              return Err ( "source requires exactly one value".to_string () ); }
            let value : String =
              atom_to_string ( &subitems[1] ) ?;
            metadata . source = Some ( value ); },
          "stats" => {
            parse_stats_sexp ( &subitems[1..], &mut metadata . stats ) ?; },
          "editRequest" => {
            parse_editrequest_sexp ( &subitems[1..], metadata ) ?; },
          "viewRequests" => {
            parse_viewrequests_sexp (
              &subitems[1..], &mut metadata . view_requests ) ?; },
          _ => { return Err ( format! ( "Unknown node key: {}",
                                         key )); }} },
      Sexp::Atom ( _ ) => {
        let bare_value : String =
          atom_to_string ( element ) ?;
        match bare_value . as_str () {
          "parentIgnores" => metadata . parent_ignores = true,
          "indefinitive"  => metadata . indefinitive = true,
          "cycle"         => metadata . cycle = true,
          _ => {
            return Err ( format! ( "Unknown node value: {}",
                                    bare_value )); }} },
      _ => { return Err ( "Unexpected element in node"
                           . to_string () ); }} }
  Ok (( )) }


/// Parse the (stats ...) s-expression contents.
fn parse_stats_sexp (
  items : &[Sexp],
  stats : &mut TrueNodeStats
) -> Result<(), String> {
  for element in items {
    match element {
      Sexp::List ( kv_pair ) if kv_pair . len () == 2 => {
        let key : String =
          atom_to_string ( &kv_pair[0] ) ?;
        let value : String =
          atom_to_string ( &kv_pair[1] ) ?;
        match key . as_str () {
          "containers" => {
            stats . numContainers = Some (
              value.parse::<usize>()
                . map_err ( |_| format! (
                  "Invalid containers value: {}", value )) ? ); },
          "contents" => {
            stats . numContents = Some (
              value.parse::<usize>()
                . map_err ( |_| format! (
                  "Invalid contents value: {}", value )) ? ); },
          "linksIn" => {
            stats . numLinksIn = Some (
              value.parse::<usize>()
                . map_err ( |_| format! (
                  "Invalid linksIn value: {}", value )) ? ); },
          _ => { return Err ( format! ( "Unknown stats key: {}",
                                         key )); }} },
      Sexp::Atom ( _ ) => {
        let bare_value : String =
          atom_to_string ( element ) ?;
        match bare_value . as_str () {
          "notInParent"    => stats . parentIsContainer = false,
          "containsParent" => stats . parentIsContent   = true,
          _ => {
            return Err ( format! ( "Unknown stats value: {}",
                                    bare_value )); }} },
      _ => { return Err ( "Unexpected element in stats"
                           . to_string () ); }} }
  Ok (( )) }


/// Parse the (editRequest ...) s-expression contents.
fn parse_editrequest_sexp (
  items : &[Sexp],
  metadata : &mut OrgnodeMetadata
) -> Result<(), String> {
  for element in items {
    match element {
      Sexp::List ( subitems ) if subitems . len () == 2 => {
        let key : String =
          atom_to_string ( &subitems[0] ) ?;
        if key == "merge" {
          let id_str : String =
            atom_to_string ( &subitems[1] ) ?;
          metadata . edit_request = Some (
            EditRequest::Merge ( ID::from ( id_str )));
        } else {
          return Err ( format! ( "Unknown editRequest key: {}", key )); }
      },
      Sexp::Atom ( _ ) => {
        let bare_value : String =
          atom_to_string ( element ) ?;
        match bare_value . as_str () {
          "delete" => metadata . edit_request = Some ( EditRequest::Delete ),
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


/// Parse a string literal like "foo" from an s-expression.
fn parse_string_literal ( sexp : &Sexp ) -> Result<String, String> {
  match sexp {
    Sexp::Atom ( sexp::Atom::S ( s )) => Ok ( s . clone () ),
    _ => Err ( format! ( "Expected string literal, got: {:?}", sexp )),
  }
}
