use crate::serve::util::search_terms_from_request;
use crate::serve::util::send_response;
use crate::tantivy::search_index;
use crate::types::TantivyIndex;

use std::net::TcpStream; // handles two-way communication
use tantivy::{Document};


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
      let error_msg = format! (
        "Error extracting search terms: {}", err );
      println! ( "{}", error_msg ) ;
      send_response (
        stream, &error_msg ); } } }

/// Runs `search_index`.
/// If matches are found, returns a String
/// with a match score and a title on each line.
fn generate_title_matches_response (
  search_terms  : &str,
  tantivy_index : &TantivyIndex)
  -> String {

  match search_index (
    tantivy_index,
    search_terms ) {
    Ok (( best_matches, searcher )) => {
      if best_matches.is_empty () {
        "No matches found.".to_string ()
      } else {
        let mut titles = Vec::new();
        for (score, doc_address) in best_matches {
          match searcher.doc (doc_address) {
            // searcher.doc fetches a Document.
            // (Document is an alias of the TantivyDocument type.)
            // Each Document here is just two fields,
            // "title" and "path" (as of <2025-08-08 Fri>).
            Ok (retrieved_doc) => {
              let retrieved_doc : Document = retrieved_doc;
              if let Some (title_value) = retrieved_doc
                . get_first ( tantivy_index.title_field )
              { if let Some ( title_text ) =
                title_value.as_text ()
                { titles.push ( format! (
                  "{:.2}: {}",
                  score,
                  title_text ) ); } } },
            Err (e) => { eprintln! (
              "Error retrieving document: {}", e ); } } }
        titles.join("\n") } },
    Err(e) => { format!("Error searching index: {}", e) } } }
