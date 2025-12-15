use crate::serve::util::send_response;
use crate::media::sexp::extract_v_from_kv_pair_in_sexp;
use crate::media::tantivy::search_index;
use crate::types::orgnode::{OrgNode, Interp, default_metadata};
use crate::types::misc::TantivyIndex;
use crate::org_to_text::orgnode_to_text;

use sexp::Sexp;
use std::collections::HashMap;
use std::net::TcpStream; // handles two-way communication
use tantivy::{Document, Searcher};

type MatchGroups =
  HashMap < String,              // ID
            Vec < ( f32,         // score
                    String ) >>; // title or alias

/// Extracts search terms from request,
/// finds matching titles,
/// and sends a corresponding document to Emacs.
pub fn handle_title_matches_request (
  stream        : &mut TcpStream,
  request       : &str,
  tantivy_index : &TantivyIndex ) {

  match search_terms_from_request ( request ) {
    Ok ( search_terms ) => {
      send_response (
        stream,
        & generate_title_matches_response (
          &search_terms,
          tantivy_index ) ); },
    Err ( err ) => {
      let error_msg : String =
        format! (
          "Error extracting search terms: {}", err );
      println! ( "{}", error_msg ) ;
      send_response (
        stream, &error_msg ); } } }

/// Runs `search_index`.
/// Returns an org-mode formatted string with grouped results by ID.
/// The resulting buffer looks something like this:
///   * (skg type:searchResult) second
///   ** score: 1.29, [[id:5a][imperfect test second]]
///   *** score: 1.17, [[id:5a][perfect match test second]]
///   ** score: 0.98, [[id:2a][This is a second test file.]]
///   ** score: 0.98, [[id:7f2d15e3-2d6e-4670-9700-fb6baabd6062][I am adding a second child.]]
/// That is, the root says what was searched for,
/// each level-2 headline is a distinct matching node,
/// and each level-3 headline is a distinct matching alias
/// for its parent, if any exist.
pub fn generate_title_matches_response (
  search_terms  : &str,
  tantivy_index : &TantivyIndex)
  -> String {

  match search_index ( tantivy_index,
                       search_terms ) {
    Ok (( best_matches, searcher )) => {
      if best_matches.is_empty () {
        "No matches found.".to_string ()
      } else {
        format_matches_as_org_mode (
          search_terms,
          group_matches_by_id (
            best_matches,
            searcher,
            tantivy_index )) }},
    Err(e) => {
      format!("Error searching index: {}", e) }} }

fn group_matches_by_id (
  best_matches  : Vec < (f32, tantivy::DocAddress) >,
  searcher      : Searcher,
  tantivy_index : &TantivyIndex )
  -> MatchGroups {

  let mut result_acc : MatchGroups =
    HashMap::new();
  for (score, doc_address) in best_matches {
    match searcher.doc (doc_address) {
      Ok (retrieved_doc) => {
        let retrieved_doc : Document = retrieved_doc;
        let id_opt : Option < String > =
          retrieved_doc
            . get_first ( tantivy_index.id_field )
            . and_then ( |v| v.as_text() )
            . map ( |s| s.to_string() );
        let title_opt : Option < String > =
          retrieved_doc
            . get_first ( tantivy_index.title_or_alias_field )
            . and_then ( |v| v.as_text() )
            . map ( |s| s.to_string() );
        if let (Some(id), Some(title)) = (id_opt, title_opt) {
          result_acc
            . entry ( id )
            . or_insert_with ( Vec::new )
            . push (( score, title )); }},
      Err (e) => { eprintln! (
        "Error retrieving document: {}", e ); }} }
  result_acc }

/// Formats grouped matches as an org-mode document.
/// Sorts IDs by best score, and matches within each ID by score.
fn format_matches_as_org_mode (
  search_terms  : &str,
  matches_by_id : MatchGroups )
  -> String {

  let mut result : String =
    String::new();
  let search_root_node : OrgNode =
    OrgNode {
      metadata : { let mut md = default_metadata ();
                   md.code.interp = Interp::ParentIgnores;
                   md },
      title : search_terms.to_string (),
      // The unique level-1 headline states the search terms.
      body : None, };
  result.push_str (
    & orgnode_to_text (
      1,
      &search_root_node ));
  let mut id_entries // Not a MatchGroups, b/c Vec != HashMap
    : Vec < ( String,               // ID
              Vec < ( f32,          // score
                      String ) >) > // title or alias
    = ( matches_by_id.into_iter()
        . map ( | (id, mut matches) | {
          // Sort matches within each ID by score
          // (descending, os the first is the best).
          matches . sort_by( |a, b|
                              b.0.partial_cmp (&a.0) . unwrap() );
          (id, matches) } )
        . collect() );
  id_entries.sort_by ( |a, b| {
    // Sort IDs by each one's best (and now first) match score.
    let score_a : f32 =
      a.1.first() . map( |(s, _)| *s) . unwrap_or (0.0);
    let score_b : f32 =
      b.1.first() . map( |(s, _)| *s) . unwrap_or (0.0);
    score_b . partial_cmp (&score_a) . unwrap () } );
  for (id, matches) in id_entries {
    // First (best) match becomes level-2 headline
    let (score, title) = &matches[0];
    let match_node : OrgNode =
      OrgNode {
        metadata : default_metadata (),
        title : format! (
          "score: {:.2}, [[id:{}][{}]]",
          score, id, title ),
        body : None, };
    result.push_str (
      & orgnode_to_text (
        2,
        &match_node ));
    for (score, title) in matches.iter().skip(1) {
      // The rest, if any, become level-3 headlines.
      let alias_match_node : OrgNode =
        OrgNode {
          metadata : default_metadata (),
          title : format! (
            "score: {:.2}, [[id:{}][{}]]",
            score, id, title ),
          body : None, };
      result.push_str (
        & orgnode_to_text (
          3,
          &alias_match_node )); }}
  result }

pub fn search_terms_from_request (
  request : &str
) -> Result<String, String> {
  let sexp : Sexp =
    sexp::parse ( request )
    . map_err ( |e| format! (
      "Failed to parse S-expression: {}", e ) ) ?;
  extract_v_from_kv_pair_in_sexp ( &sexp, "terms" ) }
