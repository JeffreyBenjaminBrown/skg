/// PURPOSE: Parse (skg ...) metadata s-expressions from org headlines.

use crate::media::sexp::atom_to_string;
use crate::types::misc::ID;
use crate::types::orgnode::{OrgnodeMetadata, OrgnodeViewData, OrgnodeCode, OrgnodeRelationships, Interp, EditRequest, ViewRequest, default_metadata};

use sexp::Sexp;
use std::collections::HashSet;
use std::str::FromStr;


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
            parse_view_sexp ( &items[1..], &mut result . viewData ) ?; },
          "code" => {
            parse_code_sexp ( &items[1..], &mut result . code ) ?; },
          _ => { return Err ( format! ( "Unknown metadata key: {}",
                                         first )); }} },
      _ => { return Err ( format! (
        "Unexpected element '{}' in metadata sexp: {}",
        element, sexp_str )); }} }
  Ok ( result ) }


/// Parse the (view ...) s-expression and update viewData.
fn parse_view_sexp (
  items : &[Sexp],
  view_data : &mut OrgnodeViewData
) -> Result<(), String> {
  for view_element in items {
    match view_element {
      Sexp::List ( subitems ) if subitems . len () >= 2 => {
        let key : String =
          atom_to_string ( &subitems[0] ) ?;
        if key == "rels" {
          parse_rels_sexp ( &subitems[1..], &mut view_data . relationships ) ?;
        } else {
          return Err ( format! ( "Unknown view key: {}", key )); }
      },
      Sexp::Atom ( _ ) => {
        let bare_value : String =
          atom_to_string ( view_element ) ?;
        match bare_value . as_str () {
          "cycle"    => view_data . cycle = true,
          "focused"  => view_data . focused = true,
          "folded"   => view_data . folded = true,
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
  relationships : &mut OrgnodeRelationships
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
