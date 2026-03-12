pub mod render_enriched_search_buffer;

/// PITFALL: Uses two layers of truncation.
/// Tantivy truncates its search after (unimaginable) 1e5 results.
/// The server (outside of Tantivy) then sorts those results
/// to surface roots and other high-value gometries,
/// which are truncated for display.

use crate::consts::SEARCH_DISPLAY_LIMIT;
use crate::context::ContextOriginType;
use crate::dbs::tantivy::search_index;
use crate::dbs::typedb::ancestry::{AncestryTree, full_containerward_ancestry};
use crate::dbs::typedb::search::all_graphnodestats::{ AllGraphNodeStats, fetch_all_graphnodestats};
use crate::org_to_text::viewnode_forest_to_string;
use crate::serve::ConnectionState;
use crate::serve::protocol::TcpToClient;
use crate::serve::util::{ send_response_with_length_prefix, tag_text_response};
use crate::types::memory::skgnode_from_map_or_disk;
use crate::types::misc::{TantivyIndex, SkgConfig, ID, SourceName};
use crate::types::sexp::extract_v_from_kv_pair_in_sexp;
use crate::types::memory::ViewUri;
use crate::types::viewnode::{ ViewNode, ViewNodeKind, Scaffold, forest_root_viewnode, mk_indefinitive_viewnode};

use ego_tree::{Tree, NodeId, NodeMut};
use sexp::{Sexp, Atom};
use std::collections::{HashMap, HashSet};
use std::net::TcpStream;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex, MutexGuard};
use tantivy::{Document, Searcher};
use typedb_driver::TypeDBDriver;

/// "Rooty" nodes have the most interesting graph structure:
/// literal roots, cycle-roots, link targets,
/// and things that had an ID when imported.
/// The default search scope is Rooty.
pub enum SearchScope { Rooty, Everything }

/// Maps each ID to search hits (plural -- IDs can have aliases,
/// so one ID might get multiple matches).
/// The score incorporates a context-based multiplier:
/// Each result's BM25 score from Tantivy is multiplied by
/// the multiplier corresponding to its context_origin_type.
/// Non-origins keep their raw score (multiplier = 1).
pub type MatchGroups =
  HashMap < ID, ( SourceName,
                  Vec < ( f32,           // score (after multiplier)
                          String ) >) >; // title or alias

/// Structured enrichment data passed through the slot,
/// replacing the raw rendered String.
pub struct SearchEnrichmentPayload {
  pub terms          : String,
  pub search_results : Vec<ID>,
  pub ancestry_by_id : HashMap<ID, AncestryTree>,
  pub graphnodestats : AllGraphNodeStats,
}

/// Provides two responses, one fast and one slow.
/// The slower one is 'enriched'
/// with containerward paths and graphnodestats at each search hit,
/// and is processed in the background -- the user need not await it.
pub fn handle_title_matches_request (
  stream           : &mut TcpStream,
  request          : &str,
  tantivy_index    : &TantivyIndex,
  typedb_driver    : &Arc<TypeDBDriver>,
  config           : &SkgConfig,
  enrichment_slot  : &Arc<Mutex<Option<SearchEnrichmentPayload>>>,
  search_cancelled : &Arc<AtomicBool>,
  conn_state       : &mut ConnectionState,
) {
  let parsed_sexp : Result < Sexp, String > =
    sexp::parse (request)
    . map_err ( |e| format! (
      "Failed to parse S-expression: {}", e ) );
  let sexp : Sexp = match parsed_sexp {
    Ok (s) => s,
    Err (err) => {
      tracing::error! ( "{}", err );
      send_response_with_length_prefix (
        stream,
        & tag_text_response (
          TcpToClient::SearchResults, &err ));
      return; } };
  let search_terms : Result < String, String > =
    extract_v_from_kv_pair_in_sexp ( &sexp, "terms" );
  let scope : SearchScope =
    match extract_v_from_kv_pair_in_sexp ( &sexp, "scope" )
          . unwrap_or ( "rooty" . to_string () )
          . as_str ()
    { "everywhere" => SearchScope::Everything,
      _            => SearchScope::Rooty };
  match search_terms {
    Ok (search_terms) => {
      // --- Phase 1: immediate results without paths ---
      match search_index ( tantivy_index,
                           &search_terms ) {
        Ok (( best_matches, searcher )) => {
          if best_matches . is_empty () {
            send_response_with_length_prefix (
              stream,
              & tag_text_response (
                TcpToClient::SearchResults,
                "No matches found." ));
            return; }
          let matches_by_id : MatchGroups =
            group_matches_by_id (
              best_matches,
              searcher,
              tantivy_index,
              &scope );
          let (forest, search_results) : (Tree<ViewNode>, Vec<ID>) =
            build_search_forest (
              &search_terms,
              &matches_by_id );
          let rendered : String =
            // Render first, before register_view moves the forest
            viewnode_forest_to_string ( &forest )
            . expect ("search forest rendering never fails");
          for (id, (source, _)) in &matches_by_id {
            // Populate pool with SkgNodes for each result ID.
            let _ = skgnode_from_map_or_disk (
              id, source,
              &mut conn_state . memory . pool, config ); }
          let uri : ViewUri =
            ViewUri::SearchView ( search_terms . clone () );
          if conn_state . memory . views . contains_key (&uri) {
            // A previous search with the same terms exists.
            // Clean up its pool entries before overwriting.
            conn_state . memory . unregister_view (&uri); }
          conn_state . memory . register_view (
            uri, forest, &search_results );
          send_response_with_length_prefix (
            // phase 1 (unenriched) tagged LP response
            stream,
            & tag_text_response (
              TcpToClient::SearchResults, &rendered ));
          spawn_enrichment_thread (
            // phase 2 (enriched) search results, backgrounded
            enrichment_slot, search_cancelled,
            typedb_driver, config,
            &search_terms, &search_results ); },
        Err (e) => {
          send_response_with_length_prefix (
            stream,
            & tag_text_response (
              TcpToClient::SearchResults,
              & format! ("Error searching index: {}", e) )); }} },
    Err (err) => {
      let error_msg : String =
        format! (
          "Error extracting search terms: {}", err );
      tracing::error! ( "{}", error_msg ) ;
      send_response_with_length_prefix (
        stream,
        & tag_text_response (
          TcpToClient::SearchResults, &error_msg )); }} }

/// Spawn a background thread to compute containerward acnestries
/// and graphnodestats, then write the structured payload
/// to the shared slot. Clears any stale enrichment and resets
/// the cancellation flag before spawning.
fn spawn_enrichment_thread (
  enrichment_slot  : &Arc<Mutex<Option<SearchEnrichmentPayload>>>,
  search_cancelled : &Arc<AtomicBool>,
  typedb_driver    : &Arc<TypeDBDriver>,
  config           : &SkgConfig,
  search_terms     : &str,
  search_results   : &[ID],
) {
  { // Clear stale enrichment before spawning.
    // todo ? Instead, permit multiple enrichments for different search result buffers to coexist.
    let mut guard : MutexGuard<Option<SearchEnrichmentPayload>> =
      enrichment_slot . lock () . unwrap ();
    *guard = None; }
  search_cancelled . store (false, Ordering::SeqCst);
  let slot_clone    : Arc<Mutex<Option<SearchEnrichmentPayload>>> =
    Arc::clone (enrichment_slot);
  let cancel_clone  : Arc<AtomicBool>   = Arc::clone (search_cancelled);
  let driver_clone  : Arc<TypeDBDriver> = Arc::clone (typedb_driver);
  let config_clone  : SkgConfig         = config . clone ();
  let terms_clone   : String            = search_terms . to_string ();
  let ids_clone     : Vec<ID>           = search_results . to_vec ();
  let max_depth : usize = config . max_ancestry_depth;
  std::thread::spawn ( move || {
    tracing::info! ("search enrichment: thread started for {} IDs",
              ids_clone . len ());
    let ancestry_by_id : HashMap<ID, AncestryTree> =
      ancestry_by_id_from_ids (
        &ids_clone, &config_clone . db_name,
        &driver_clone, max_depth );
    tracing::info! ("search enrichment: ancestry computed ({} entries)",
              ancestry_by_id . len ());
    if cancel_clone . load (Ordering::SeqCst) {
      tracing::info! ("search enrichment: cancelled after ancestry");
      return; }
    let all_enriched_ids : Vec<ID> = {
      // Collect result IDs + every ID from ancestry trees.
      let mut id_set : HashSet<ID> = HashSet::new ();
      for id in &ids_clone {
        id_set . insert ( id . clone () ); }
      for tree in ancestry_by_id . values () {
        collect_ids_from_ancestry_node ( tree, &mut id_set ); }
      id_set . into_iter () . collect () };
    let graphnodestats : AllGraphNodeStats =
      futures::executor::block_on (
        fetch_all_graphnodestats (
          &config_clone . db_name,
          &driver_clone,
          &all_enriched_ids ) )
      . unwrap_or_else ( |e| {
        tracing::warn! ("search enrichment: graphnodestats failed: {}", e);
        AllGraphNodeStats::empty () } );
    tracing::info! ("search enrichment: graphnodestats fetched for {} IDs",
              all_enriched_ids . len ());
    if cancel_clone . load (Ordering::SeqCst) {
      tracing::info! ("search enrichment: cancelled after graphnodestats");
      return; }
    tracing::info! ("search enrichment: writing payload to slot");
    let mut guard : MutexGuard<Option<SearchEnrichmentPayload>> =
      slot_clone . lock () . unwrap ();
    *guard = Some ( SearchEnrichmentPayload {
      terms          : terms_clone,
      search_results     : ids_clone,
      ancestry_by_id,
      graphnodestats } ); } ); }

/// Compute full containerward ancestry for each ID in parallel.
/// IDs whose ancestry lookup fails are logged and omitted.
fn ancestry_by_id_from_ids (
  ids       : &[ID],
  db_name   : &str,
  driver    : &TypeDBDriver,
  max_depth : usize,
) -> HashMap<ID, AncestryTree> {
  let futures : Vec<_> =
    ids . iter ()
    . map ( |id|
      full_containerward_ancestry (
        db_name, driver, id, max_depth ) )
    . collect ();
  let results : Vec<Result<AncestryTree,
                           Box<dyn std::error::Error>>> =
    futures::executor::block_on (
      futures::future::join_all (futures) );
  let mut map : HashMap<ID, AncestryTree> =
    HashMap::new ();
  for ( id, result ) in ids . iter ()
                        . zip ( results )
  { match result {
      Ok (tree) => { map . insert ( id . clone (), tree ); },
      Err (e) => {
        tracing::warn! (
          "search enrichment: ancestry for {} failed: {}",
          id, e ); } } }
  map }

fn collect_ids_from_ancestry_node(
  node   : &AncestryTree,
  id_set : &mut HashSet<ID>,
) {
  id_set . insert ( node . id () . clone () );
  if let AncestryTree::Inner ( _, children ) = node {
    for child in children {
      collect_ids_from_ancestry_node ( child, id_set ); } } }

/// Build the tagged s-exp for a search enrichment payload.
/// Format: (("response-type" "search-enrichment")
///          ("terms" "TERMS") ("content" "ORG"))
pub fn mk_search_enrichment_sexp (
  terms   : &str,
  content : &str,
) -> String {
  Sexp::List ( vec! [
    Sexp::List ( vec! [
      Sexp::Atom ( Atom::S ( "response-type" . to_string () )),
      Sexp::Atom ( Atom::S ( TcpToClient::SearchEnrichment
                             . repr_in_client () . to_string () )), ] ),
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
  scope         : &SearchScope,
) -> MatchGroups {
  let mut result_acc : MatchGroups =
    HashMap::new();
  for (score, doc_address) in best_matches {
    match searcher . doc (doc_address) {
      Ok (retrieved_doc) => {
        let retrieved_doc : Document = retrieved_doc;
        let id_opt : Option < ID > =
          retrieved_doc
            . get_first ( tantivy_index . id_field )
            . and_then ( |v| v . as_text() )
            . map ( |s| ID::from (s) );
        let title_opt : Option < String > =
          retrieved_doc
            . get_first ( tantivy_index . title_or_alias_field )
            . and_then ( |v| v . as_text() )
            . map ( |s| s . to_string() );
        let source : SourceName =
          SourceName::from (
            retrieved_doc
              . get_first ( tantivy_index . source_field )
              . and_then ( |v| v . as_text () )
              . unwrap_or ("") );
        let origin_type : Option < ContextOriginType > =
          retrieved_doc
            . get_first ( tantivy_index . context_origin_type_field )
            . and_then ( |v| v . as_text () )
            . and_then ( ContextOriginType::from_label );
        if matches! ( scope, SearchScope::Rooty ) {
          match origin_type {
            None => continue,
            Some ( ContextOriginType::MultiContained ) => continue,
            _ => {} } }
        let multiplier : f32 =
          origin_type . map_or ( 1.0, |t| t . multiplier() );
        let adjusted_score : f32 = score * multiplier;
        if let (Some (id), Some (title)) = (id_opt, title_opt) {
          result_acc
            . entry (id)
            . or_insert_with ( || (source, Vec::new ()) )
            . 1
            . push (( adjusted_score, title )); }},
      Err (e) => { tracing::error! (
        "Error retrieving document: {}", e ); }} }
  result_acc }

/// Builds a Tree<ViewNode> representing the search results.
/// Returns the forest and the ordered list of result IDs.
///
/// Tree structure:
///   BufferRoot
///   ├── TrueNode per result (level 1, indefinitive, parent_ignores)
///   │   └── AliasCol + Alias children (if aliases matched)
///   └── ...
pub fn build_search_forest (
  _search_terms : &str,
  matches_by_id : &MatchGroups,
) -> (Tree<ViewNode>, Vec<ID>) {
  let mut forest : Tree<ViewNode> =
    Tree::new ( forest_root_viewnode () );
  let mut id_entries : Vec < ( &ID,
                               &SourceName,
                               &Vec < ( f32, String ) > ) > =
    matches_by_id . iter ()
    . map ( |(id, (source, matches))| // flatten
             (id, source, matches) )
    . collect ();
  id_entries . sort_by ( |a, b| { // sort by best score (descending)
    let score_a : f32 =
      a . 2 . first () . map ( |(s, _)| *s ) . unwrap_or (0.0);
    let score_b : f32 =
      b . 2 . first () . map ( |(s, _)| *s ) . unwrap_or (0.0);
    score_b . partial_cmp (& score_a)
    . unwrap_or (std::cmp::Ordering::Equal) } );
  let mut search_results : Vec < ID > = Vec::new ();
  for (id, source, matches) in id_entries . iter ()
        . take (SEARCH_DISPLAY_LIMIT)
    { search_results . push ( (*id) . clone () );
      let mut sorted_matches : Vec < &(f32, String) > =
        // We borrow from matches_by_id.
        // Sort matches by score descending for display.
        matches . iter () . collect ();
      sorted_matches . sort_by ( |a, b|
        b . 0 . partial_cmp (&a . 0) . unwrap () );
      let (_score, title) : &(f32, String) = sorted_matches [0];
      let result_treeid : NodeId = {
        let mut root_mut : NodeMut<ViewNode> =
          forest . root_mut ();
        root_mut . append (
          mk_indefinitive_viewnode (
            (*id) . clone (),
            (*source) . clone (),
            title . clone (),
            true ) ) // parent_ignores
        . id () };
      if sorted_matches . len () > 1 {
        // We bury all but the best match in an AliasCol.
        // PITFALL: The title might not be the best match,
        // in which case this makes it look like an alias.
        let aliascol_id : NodeId = {
          let mut result_mut : NodeMut<ViewNode> =
            forest . get_mut (result_treeid) . unwrap ();
          result_mut . append ( ViewNode {
            focused : false,
            folded  : true,
            kind    : ViewNodeKind::Scaff (
              Scaffold::AliasCol ) } )
          . id () };
        for (_score, title) in sorted_matches . iter () . skip (1) {
          let mut aliascol_mut : NodeMut<ViewNode> =
            forest . get_mut (aliascol_id) . unwrap ();
          aliascol_mut . append ( ViewNode {
            focused : false,
            folded  : false,
            kind    : ViewNodeKind::Scaff (
              Scaffold::Alias {
                text : title . clone (),
                diff : None } ) } ); }} }
  (forest, search_results) }
