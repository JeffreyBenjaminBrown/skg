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
use crate::types::misc::{ID, SourceName};
use crate::types::errors::BufferValidationError;
use crate::types::git::{NodeDiffStatus, FieldDiffStatus};
use crate::types::viewnode::{GraphNodeStats, ViewNodeStats, EditRequest, ViewRequest, Scaffold};
use crate::types::unchecked_viewnode::{
    UncheckedViewNode, UncheckedViewNodeKind, UncheckedTrueNode,
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
  // None means TrueNode, Some means Scaffold
  pub scaffold: Option<Scaffold>,
  // TrueNode fields (ignored if scaffold is Some)
  pub id: Option<ID>,
  pub source: Option<SourceName>,
  pub parent_ignores: bool,
  pub indefinitive: bool,
  pub graphStats: GraphNodeStats,
  pub viewStats: ViewNodeStats,
  pub edit_request: Option<EditRequest>,
  pub view_requests: HashSet<ViewRequest>,
  pub truenode_diff: Option<NodeDiffStatus>,
  pub scaffold_diff: Option<FieldDiffStatus>,
}

pub fn default_metadata() -> ViewnodeMetadata {
  ViewnodeMetadata {
    focused: false,
    folded: false,
    scaffold: None,
    id: None,
    source: None,
    parent_ignores: false,
    indefinitive: false,
    graphStats: GraphNodeStats::default(),
    viewStats: ViewNodeStats::default(),
    edit_request: None,
    view_requests: HashSet::new(),
    truenode_diff: None,
    scaffold_diff: None, }}

/// Create an UncheckedViewNode from parsed metadata components.
/// This is the bridge between parsing (ViewnodeMetadata) and runtime (UncheckedViewNode).
/// Returns (UncheckedViewNode, Option<BufferValidationError>) - error if Scaffold has body.
pub fn viewnode_from_metadata (
  metadata : &ViewnodeMetadata,
  title    : String,
  body     : Option < String >,
) -> ( UncheckedViewNode, Option < BufferValidationError > ) {
  let (kind, error)
    : (UncheckedViewNodeKind, Option<BufferValidationError>)
    = if let Some ( ref scaffold ) = metadata . scaffold {
        let error : Option<BufferValidationError> =
          if body . is_some () {
            Some ( BufferValidationError::Body_of_Scaffold (
              title . clone (),
              scaffold . repr_in_client () ))
          } else { None };
        let scaffold_with_title : Scaffold = match scaffold {
          // Use headline title for string and apply scaffold_diff
          Scaffold::Alias { .. } => Scaffold::Alias {
                              text: title . clone (),
                              diff: metadata . scaffold_diff },
          Scaffold::ID    { .. } => Scaffold::ID {
                              id: title . clone () . into (),
                              diff: metadata . scaffold_diff },
          other => other . clone () };
        ( UncheckedViewNodeKind::Scaff ( scaffold_with_title ), error )
      } else {
      // UncheckedTrueNode
      ( UncheckedViewNodeKind::True ( UncheckedTrueNode {
          title,
          body,
          id_opt           : metadata . id . clone (),
          source_opt       : metadata . source . clone (),
          parent_ignores   : metadata . parent_ignores,
          indefinitive     : metadata . indefinitive,
          graphStats       : metadata . graphStats . clone (),
          viewStats        : metadata . viewStats . clone (),
          edit_request     : metadata . edit_request . clone (),
          view_requests    : metadata . view_requests . clone (),
          diff             : metadata . truenode_diff, } ),
        None )
    };
  ( UncheckedViewNode { focused : metadata.focused,
                       folded  : metadata.folded,
                       kind },
    error ) }


/// Parse metadata from org-mode headline into ViewnodeMetadata.
/// See file header comment for full syntax.
pub fn parse_metadata_to_viewnodemd (
  sexp_str : &str
) -> Result<ViewnodeMetadata, String> {
  let mut result : ViewnodeMetadata =
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
          "diff" => {
            // (diff <value>) at top level is for Scaffolds (Alias/ID)
            if items . len () != 2 {
              return Err ( "diff requires exactly one value".to_string () ); }
            let value : String =
              atom_to_string ( &items[1] ) ?;
            result . scaffold_diff = Some (
              FieldDiffStatus::from_client_string ( &value )
                . ok_or_else ( || format! ( "Invalid FieldDiffStatus value: {}", value ))? ); },
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
          // Scaffold kinds as bare atoms (alias/id string comes from title in viewnode_from_metadata)
          "alias"    => result . scaffold = Some ( Scaffold::Alias { text: String::new(), diff: None } ),
          "aliasCol" => result . scaffold = Some ( Scaffold::AliasCol ),
          "forestRoot" => result . scaffold = Some ( Scaffold::BufferRoot ),
          "hiddenInSubscribeeCol" =>
            result . scaffold = Some ( Scaffold::HiddenInSubscribeeCol ),
          "hiddenOutsideOfSubscribeeCol" =>
            result . scaffold = Some ( Scaffold::HiddenOutsideOfSubscribeeCol ),
          "subscribeeCol" =>
            result . scaffold = Some ( Scaffold::SubscribeeCol ),
          "textChanged" =>
            result . scaffold = Some ( Scaffold::TextChanged ),
          "idCol" =>
            result . scaffold = Some ( Scaffold::IDCol ),
          "id" =>
            result . scaffold = Some ( Scaffold::ID { id: ID::default(), diff: None } ),
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
  metadata : &mut ViewnodeMetadata
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
            metadata . source = Some ( SourceName::from ( value ) ); },
          "graphStats" => {
            parse_graphstats_sexp ( &subitems[1..], &mut metadata . graphStats ) ?; },
          "viewStats" => {
            parse_viewstats_sexp ( &subitems[1..], &mut metadata . viewStats ) ?; },
          "editRequest" => {
            parse_editrequest_sexp ( &subitems[1..], metadata ) ?; },
          "viewRequests" => {
            parse_viewrequests_sexp (
              &subitems[1..], &mut metadata . view_requests ) ?; },
          "diff" => {
            if subitems . len () != 2 {
              return Err ( "diff requires exactly one value".to_string () ); }
            let value : String =
              atom_to_string ( &subitems[1] ) ?;
            metadata . truenode_diff = Some (
              NodeDiffStatus::from_client_string ( &value )
                . ok_or_else ( || format! ( "Invalid NodeDiffStatus value: {}", value ))? ); },
          _ => { return Err ( format! ( "Unknown node key: {}",
                                         key )); }} },
      Sexp::Atom ( _ ) => {
        let bare_value : String =
          atom_to_string ( element ) ?;
        match bare_value . as_str () {
          "parentIgnores" => metadata . parent_ignores = true,
          "indefinitive"  => metadata . indefinitive = true,
          _ => {
            return Err ( format! ( "Unknown node value: {}",
                                    bare_value )); }} },
      _ => { return Err ( "Unexpected element in node"
                           . to_string () ); }} }
  Ok (( )) }


/// Parse the (graphStats ...) s-expression contents.
/// Only handles key-value pairs (containers, contents, linksIn).
fn parse_graphstats_sexp (
  items : &[Sexp],
  stats : &mut GraphNodeStats
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
          _ => { return Err ( format! ( "Unknown graphStats key: {}",
                                         key )); }} },
      _ => { return Err ( "Unexpected element in graphStats (expected key-value pairs)"
                           . to_string () ); }} }
  Ok (( )) }

/// Parse the (viewStats ...) s-expression contents.
/// Only handles bare atoms (cycle, notInParent, containsParent).
fn parse_viewstats_sexp (
  items : &[Sexp],
  stats : &mut ViewNodeStats
) -> Result<(), String> {
  for element in items {
    match element {
      Sexp::Atom ( _ ) => {
        let bare_value : String =
          atom_to_string ( element ) ?;
        match bare_value . as_str () {
          "cycle"          => stats . cycle = true,
          "notInParent"    => stats . parentIsContainer = false,
          "containsParent" => stats . parentIsContent   = true,
          _ => {
            return Err ( format! ( "Unknown viewStats value: {}",
                                    bare_value )); }} },
      _ => { return Err ( "Unexpected element in viewStats (expected bare atoms)"
                           . to_string () ); }} }
  Ok (( )) }


/// Parse the (editRequest ...) s-expression contents.
fn parse_editrequest_sexp (
  items : &[Sexp],
  metadata : &mut ViewnodeMetadata
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
