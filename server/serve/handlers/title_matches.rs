mod render_enriched_search_buffer;
use render_enriched_search_buffer::{
  enrich_with_containerward_paths,
  compute_paths_for_search_results};

/// PITFALL: Uses two layers of truncation.
/// Tantivy truncates its search after (unimaginable) 1e5 results.
/// The server (outside of Tantivy) then sorts those results
/// to surface roots and other high-value gometries,
/// which are truncated for display.

use crate::consts::SEARCH_DISPLAY_LIMIT;
use crate::context::ContextOriginType;
use crate::dbs::tantivy::search_index;
use crate::dbs::typedb::paths::PathToFirstNonlinearity;
use crate::org_to_text::viewnode_to_text;
use crate::serve::util::{
  send_response_with_length_prefix,
  tag_text_response};
use crate::types::misc::{TantivyIndex, SkgConfig, ID, SourceName};
use crate::types::sexp::extract_v_from_kv_pair_in_sexp;
use crate::types::viewnode::{
  ViewNode, ViewNodeKind, TrueNode, Scaffold,
  default_truenode};

use sexp::{Sexp, Atom};
use std::collections::HashMap;
use std::net::TcpStream;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};
use tantivy::{Document, Searcher};
use typedb_driver::TypeDBDriver;

pub type MatchGroups =
  HashMap < String,              // ID
            Vec < ( f32,         // score (after multiplier)
                    String ) >>; // title or alias

/// Provides two responses, one fast and one slow.
/// The slower one is 'enriched'
/// with containerward paths at each search hit,
/// and is processed in the background -- the user need not await it.
pub fn handle_title_matches_request (
  stream           : &mut TcpStream,
  request          : &str,
  tantivy_index    : &TantivyIndex,
  typedb_driver    : &Arc<TypeDBDriver>,
  config           : &SkgConfig,
  enrichment_slot  : &Arc<Mutex<Option<String>>>,
  search_cancelled : &Arc<AtomicBool>,
) {
  match search_terms_from_request (request) {
    Ok (search_terms) => {
      // --- Phase 1: immediate results without paths ---
      match search_index ( tantivy_index,
                           &search_terms ) {
        Ok (( best_matches, searcher )) => {
          if best_matches . is_empty () {
            send_response_with_length_prefix (
              stream,
              & tag_text_response (
                "search-results",
                "No matches found." ));
            return; }
          let matches_by_id : MatchGroups =
            group_matches_by_id (
              best_matches,
              searcher,
              tantivy_index );
          let (result, result_ids) : (String, Vec<ID>) =
            format_matches_as_org_mode (
              &search_terms,
              matches_by_id );
          send_response_with_length_prefix (
            // Sends phase 1 tagged LP response
            stream,
            & tag_text_response (
              "search-results", &result ));
          spawn_enrichment_thread (
            // --- Phase 2: background path computation ---
            enrichment_slot, search_cancelled,
            typedb_driver, config, tantivy_index,
            &search_terms, &result_ids, &result ); },
        Err (e) => {
          send_response_with_length_prefix (
            stream,
            & tag_text_response (
              "search-results",
              & format! ("Error searching index: {}", e) )); } } },
    Err (err) => {
      let error_msg : String =
        format! (
          "Error extracting search terms: {}", err );
      println! ( "{}", error_msg ) ;
      send_response_with_length_prefix (
        stream,
        & tag_text_response (
          "search-results", &error_msg )); } } }

/// Spawn a background thread to compute containerward paths
/// and write the enriched results to the shared slot.
/// Clears any stale enrichment and resets the cancellation flag
/// before spawning.
fn spawn_enrichment_thread (
  enrichment_slot  : &Arc<Mutex<Option<String>>>,
  search_cancelled : &Arc<AtomicBool>,
  typedb_driver    : &Arc<TypeDBDriver>,
  config           : &SkgConfig,
  tantivy_index    : &TantivyIndex,
  search_terms     : &str,
  result_ids       : &[ID],
  base_org         : &str,
) {
  // P8: clear stale enrichment before spawning
  { let mut guard : std::sync::MutexGuard<Option<String>> =
      enrichment_slot . lock () . unwrap ();
    *guard = None; }
  search_cancelled . store (false, Ordering::SeqCst);
  let slot_clone    : Arc<Mutex<Option<String>>> =
    Arc::clone (enrichment_slot);
  let cancel_clone  : Arc<AtomicBool> = Arc::clone (search_cancelled);
  let driver_clone  : Arc<TypeDBDriver> = Arc::clone (typedb_driver);
  let config_clone  : SkgConfig = config . clone ();
  let tantivy_clone : TantivyIndex = tantivy_index . clone ();
  let terms_clone   : String = search_terms . to_string ();
  let ids_clone     : Vec<ID> = result_ids . to_vec ();
  let base_org      : String = base_org . to_string ();
  std::thread::spawn ( move || {
    println! ("search enrichment: thread started for {} IDs",
              ids_clone . len ());
    let paths_by_id
      : HashMap < ID, Vec < PathToFirstNonlinearity > > =
      compute_paths_for_search_results (
        &ids_clone, &driver_clone, &config_clone );
    println! ("search enrichment: paths computed ({} entries)",
              paths_by_id . len ());
    if cancel_clone . load (Ordering::SeqCst) {
      println! ("search enrichment: cancelled after paths");
      return; }
    let enriched : String =
      enrich_with_containerward_paths (
        &base_org, &ids_clone, &paths_by_id,
        &tantivy_clone );
    if cancel_clone . load (Ordering::SeqCst) {
      println! ("search enrichment: cancelled after enrich");
      return; }
    let tagged : String =
      mk_search_enrichment_sexp (
        &terms_clone, &enriched );
    println! ("search enrichment: writing to slot ({} bytes)",
              tagged . len ());
    let mut guard : std::sync::MutexGuard<Option<String>> =
      slot_clone . lock () . unwrap ();
    *guard = Some (tagged); } ); }

/// Build the tagged s-exp for a search enrichment payload.
/// Format: (("response-type" "search-enrichment")
///          ("terms" "TERMS") ("content" "ORG"))
fn mk_search_enrichment_sexp (
  terms   : &str,
  content : &str,
) -> String {
  Sexp::List ( vec! [
    Sexp::List ( vec! [
      Sexp::Atom ( Atom::S ( "response-type" . to_string () )),
      Sexp::Atom ( Atom::S ( "search-enrichment" . to_string () )), ] ),
    Sexp::List ( vec! [
      Sexp::Atom ( Atom::S ( "terms"   . to_string () )),
      Sexp::Atom ( Atom::S ( terms     . to_string () )), ] ),
    Sexp::List ( vec! [
      Sexp::Atom ( Atom::S ( "content" . to_string () )),
      Sexp::Atom ( Atom::S ( content   . to_string () )), ] ),
  ] ) . to_string () }

/// Groups raw Tantivy results by ID.
/// Applies context-based score multipliers:
/// each result's BM25 score is multiplied by the multiplier
/// corresponding to its context_origin_type.
/// Non-origins keep their raw score (multiplier = 1).
pub fn group_matches_by_id (
  best_matches  : Vec < (f32, tantivy::DocAddress) >,
  searcher      : Searcher,
  tantivy_index : &TantivyIndex,
) -> MatchGroups {
  let mut result_acc : MatchGroups =
    HashMap::new();
  for (score, doc_address) in best_matches {
    match searcher . doc (doc_address) {
      Ok (retrieved_doc) => {
        let retrieved_doc : Document = retrieved_doc;
        let id_opt : Option < String > =
          retrieved_doc
            . get_first ( tantivy_index . id_field )
            . and_then ( |v| v . as_text() )
            . map ( |s| s . to_string() );
        let title_opt : Option < String > =
          retrieved_doc
            . get_first ( tantivy_index . title_or_alias_field )
            . and_then ( |v| v . as_text() )
            . map ( |s| s . to_string() );
        let multiplier : f32 =
          retrieved_doc
            . get_first ( tantivy_index . context_origin_type_field )
            . and_then ( |v| v . as_text () )
            . and_then ( ContextOriginType::from_label )
            . map_or ( 1.0, |t| t . multiplier() );
        let adjusted_score : f32 = score * multiplier;
        if let (Some (id), Some (title)) = (id_opt, title_opt) {
          result_acc
            . entry (id)
            . or_insert_with (Vec::new)
            . push (( adjusted_score, title )); }},
      Err (e) => { eprintln! (
        "Error retrieving document: {}", e ); }} }
  result_acc }

/// Formats grouped matches as an org-mode document.
/// Sorts IDs by best (adjusted) score, and matches within each ID by score.
/// Returns the formatted string and the ordered list of result IDs.
pub fn format_matches_as_org_mode (
  search_terms  : &str,
  matches_by_id : MatchGroups,
) -> (String, Vec<ID>) {
  let mut result : String =
    String::new();
  result . push_str (
    & viewnode_to_text (
      1,
      & ViewNode {
        focused : false,
        folded  : false,
        kind    : ViewNodeKind::True (
          // The unique level-1 headline states the search terms.
          TrueNode {
            parent_ignores : true,
            .. default_truenode (
              ID::from ("search-results"),
              SourceName::from ("search"),
              search_terms . to_string() ) } ), } )
    . expect ("TrueNode rendering never fails"));
  let mut id_entries
    : Vec < ( String,               // ID
              Vec < ( f32,          // score
                      String ) >) > // title or alias
    = ( matches_by_id . into_iter()
        . map ( | (id, mut matches) | {
          // Sort matches within each ID by score
          // (descending, so the first is the best).
          matches . sort_by( |a, b|
                              b . 0 . partial_cmp (&a . 0) . unwrap() );
          (id, matches) } )
        . collect() );
  id_entries . sort_by ( |a, b| {
    let score_a : f32 =
      a . 1 . first () . map ( |(s, _)| *s ) . unwrap_or (0.0);
    let score_b : f32 =
      b . 1 . first () . map ( |(s, _)| *s ) . unwrap_or (0.0);
    score_b . partial_cmp (& score_a)
    . unwrap_or (std::cmp::Ordering::Equal) } );
  let mut result_ids : Vec < ID > = Vec::new ();
  for (id, matches) in id_entries . into_iter()
    . take (SEARCH_DISPLAY_LIMIT) {
    result_ids . push ( ID::from ( id . clone () ) );
    // First (best) match becomes level-2 headline
    let (score, title) : &(f32, String) = &matches[0];
    result . push_str (
      & viewnode_to_text (
        2,
        & ViewNode {
          focused : false,
          folded  : false,
          kind    : ViewNodeKind::True (
            TrueNode {
              indefinitive : true,
              .. default_truenode (
                ID::from(id . clone()),
                SourceName::from ("search"),
                format! ( "score: {:.2}, [[id:{}][{}]]",
                          score, id, title )) } ), } )
      . expect ("TrueNode rendering never fails"));
    if matches . len() > 1 {
      // Present any matching aliases (or the title, if an alias was the best match) as children of an AliasCol scaffold.
      result . push_str (
        & viewnode_to_text (
          3,
          & ViewNode {
            focused : false,
            folded  : true,
            kind    : ViewNodeKind::Scaff (
              Scaffold::AliasCol ), } )
        . expect ("AliasCol rendering never fails"));
      for (_score, title) in matches . iter() . skip (1) {
        result . push_str (
          & viewnode_to_text (
            4,
            & ViewNode {
              focused : false,
              folded  : false,
              kind    : ViewNodeKind::Scaff (
                Scaffold::Alias {
                  text : title . clone(),
                  diff : None } ), } )
          . expect ("Alias rendering never fails")); } } }
  (result, result_ids) }

pub fn search_terms_from_request (
  request : &str
) -> Result<String, String> {
  extract_v_from_kv_pair_in_sexp (
    & { let sexp : Sexp =
          sexp::parse (request)
          . map_err ( |e| format! (
            "Failed to parse S-expression: {}", e ) ) ?;
        sexp },
    "terms" ) }
