pub mod render_enriched_search_buffer;
mod coverage;

use coverage::{CoverageMatcher, build_coverage_matcher, coverage_factor};

/// PITFALL: Uses two layers of truncation.
/// Tantivy truncates its search after (unimaginable) 1e5 results.
/// The server (outside of Tantivy) then sorts those results
/// to surface roots and other high-value gometries,
/// which are truncated for display.

use crate::consts::SEARCH_DISPLAY_LIMIT;
use crate::context::ContextOriginType;
use crate::dbs::tantivy::background_writer::wait_for_tantivy_writes_idle;
use crate::dbs::tantivy::search::{SearchOptions, search_index};
use crate::dbs::typedb::ancestry::{ AncestryTree, ancestry_by_id_from_ids_async};
use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::in_rust_graph::relation_accessors::NodeRelation;
use crate::dbs::typedb::search::all_graphnodestats::{
  AllGraphNodeStats,
  fetch_all_graphnodestats_with_source_set};
use crate::types::env::SkgEnv;
use crate::org_to_text::viewforest_to_string;
use crate::update_buffer::set_viewnodestats_in_viewforest;
use crate::serve::ViewsState;
use crate::serve::protocol::TcpToClient;
use crate::serve::util::{ send_response_with_length_prefix, tag_text_response};
use crate::types::git::MembershipAxes;
use crate::types::views_state::ViewUri;
use crate::types::misc::{TantivyIndex, SkgConfig, ID, SourceName};
use crate::source_sets::{ActiveSourceSet, search_ids_for_source_set_for_test as search_ids_for_source_set_for_test_impl};
use crate::types::sexp::extract_v_from_kv_pair_in_sexp;
use crate::types::tree::forest::ViewForest;
use crate::types::viewnode::{ ViewNode, ViewNodeKind, ParentIs, mk_indefinitive_viewnode};
use crate::types::viewnode::{QualCol, Qual};

use ego_tree::{NodeId, NodeMut};
use sexp::{Sexp, Atom};
use std::collections::{HashMap, HashSet};
use std::net::TcpStream;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex, MutexGuard};
use tantivy::{TantivyDocument, Searcher};
use tantivy::schema::document::Value;
use typedb_driver::TypeDBDriver;

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

pub fn search_ids_for_source_set_for_test (
  tantivy_index : &TantivyIndex,
  config        : &SkgConfig,
  active        : &ActiveSourceSet,
  terms         : &str,
  limit         : usize,
) -> Result<Vec<ID>, Box<dyn std::error::Error>> {
  search_ids_for_source_set_for_test_impl (
    tantivy_index, config, active, terms, limit ) }

pub fn enriched_search_buffer_for_source_set_for_test (
  terms          : &str,
  matches_by_id  : &MatchGroups,
  search_results : &[ID],
  ancestry_by_id : &HashMap<ID, AncestryTree>,
  tantivy_index  : &TantivyIndex,
  config         : &SkgConfig,
  active         : &ActiveSourceSet,
) -> Result<String, Box<dyn std::error::Error>> {
  let (mut viewforest, _ids) : (ViewForest, Vec<ID>) =
    build_search_viewforest (terms, matches_by_id, &HashSet::new ());
  render_enriched_search_buffer::insert_containerward_ancestries_into_search_view (
    &mut viewforest,
    search_results,
    ancestry_by_id,
    tantivy_index,
    config,
    active );
  render_enriched_search_buffer::insert_override_ancestries_into_search_view (
    &mut viewforest,
    search_results,
    active );
  set_viewnodestats_in_viewforest (
    // Mirror the production enrichment path (handle_snapshot_response):
    // compute view-relative stats so the rendered buffer carries the
    // sourceHerald at source boundaries. Empty containment maps suffice
    // here -- sourceAtBoundary is derived from the tree alone; the maps
    // only feed the containsParent stat, which this test does not assert.
    &mut viewforest,
    & HashMap::new (),
    & HashMap::new (),
    config,
    Some (active) );
  Ok ( viewforest_to_string ( &viewforest, config )? ) }

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
pub fn handle_text_search_request (
  stream           : &mut TcpStream,
  request          : &str,
  env              : &SkgEnv,
  enrichment_slot  : &Arc<Mutex<Option<SearchEnrichmentPayload>>>,
  search_cancelled : &Arc<AtomicBool>,
  views_state       : &mut ViewsState,
  active            : &ActiveSourceSet,
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
  let search_opts : SearchOptions =
    SearchOptions {
      regex     : bool_key ( &sexp, "regex" ),
      body      : bool_key ( &sexp, "body" ),
      operators : bool_key ( &sexp, "operators" ), };
  match search_terms {
    Ok (search_terms) => {
      // Wait for any in-flight background save-index writes to commit, so
      // the search reflects every save issued so far (read-your-writes).
      wait_for_tantivy_writes_idle ();
      // --- Phase 1: immediate results without paths ---
      match search_index ( &env . tantivy_index,
                           &search_terms,
                           &search_opts ) {
        Ok (( best_matches, searcher )) => {
          if best_matches . is_empty () {
            send_response_with_length_prefix (
              stream,
              & tag_text_response (
                TcpToClient::SearchResults,
                "No matches found." ));
            return; }
          let matches_by_id : MatchGroups =
            filter_match_groups_to_active_sources (
              group_matches_by_id (
              best_matches,
              searcher,
              &env . tantivy_index,
              &search_terms,
              &search_opts,
              Some (active) ),
              active );
          if matches_by_id . is_empty () {
            send_response_with_length_prefix (
              stream,
              & tag_text_response (
                TcpToClient::SearchResults,
                "No matches found." ));
            return; }
          let suppressed : HashSet<ID> =
            suppressed_result_ids (
              &matches_by_id,
              & env . in_rust_graph_snapshot (),
              &env . config,
              active );
          let (viewforest, search_results) : (ViewForest, Vec<ID>) =
            build_search_viewforest (
              &search_terms,
              &matches_by_id,
              &suppressed );
          let rendered : String =
            // Render first, before register_view moves the viewforest
            viewforest_to_string ( &viewforest, &env . config )
            . expect ("search viewforest rendering never fails");
          let uri : ViewUri =
            ViewUri::SearchView ( search_terms . clone () );
          if views_state . open_views . views . contains_key (&uri) {
            // Replace prior search with the same terms.
            views_state . open_views . unregister_view (&uri); }
          views_state . open_views . register_view (
            uri, viewforest, &search_results );
          send_response_with_length_prefix (
            // phase 1 (unenriched) tagged LP response
            stream,
            & tag_text_response (
              TcpToClient::SearchResults, &rendered ));
          spawn_enrichment_thread (
            // phase 2 (enriched) search results, backgrounded
            enrichment_slot, search_cancelled,
            &env . driver, &env . config,
            &search_terms, &search_results, active ); },
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

/// Read a boolean axis flag from the request sexp. Absent, empty, or
/// any non-"true" string is treated as false.
fn bool_key (
  sexp : &Sexp,
  key  : &str,
) -> bool {
  extract_v_from_kv_pair_in_sexp ( sexp, key )
    . unwrap_or_default ()
    == "true" }

fn filter_match_groups_to_active_sources (
  matches_by_id : MatchGroups,
  active        : &ActiveSourceSet,
) -> MatchGroups {
  if active . is_all () {
    return matches_by_id; }
  matches_by_id . into_iter ()
    . filter ( |(_, (source, _))|
      active . contains_source (source) )
    . collect () }

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
  active           : &ActiveSourceSet,
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
  let active_clone  : ActiveSourceSet   = active . clone ();
  let terms_clone   : String            = search_terms . to_string ();
  let ids_clone     : Vec<ID>           = search_results . to_vec ();
  let max_depth : usize = config . max_ancestry_depth;
  std::thread::spawn ( move || {
    tracing::info! ("search enrichment: thread started for {} IDs",
              ids_clone . len ());
    let ancestry_by_id : HashMap<ID, AncestryTree> =
      futures::executor::block_on (
        ancestry_by_id_from_ids_async (
          &ids_clone, &config_clone . db_name,
          &driver_clone, max_depth ));
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
        fetch_all_graphnodestats_with_source_set (
          &config_clone . db_name,
          &driver_clone,
          &all_enriched_ids,
          Some (&active_clone) ) )
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

fn collect_ids_from_ancestry_node(
  node   : &AncestryTree,
  id_set : &mut HashSet<ID>,
) {
  id_set . insert ( node . id () . clone () );
  if let AncestryTree::Inner ( _, children ) = node {
    for child in children {
      collect_ids_from_ancestry_node ( child, id_set ); }}}

/// Build the tagged s-exp for a search enrichment payload.
/// Format: (("response-type" "search-enrichment")
///          ("terms" "TERMS") ("content" "ORG") ("warnings" ()))
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
    Sexp::List ( vec! [
      Sexp::Atom ( Atom::S ( "warnings" . to_string () )),
      Sexp::List ( vec! [] ), ] ),
  ] ) . to_string () }

/// Groups raw Tantivy results by ID, applying score adjustments
/// in this order:
///
/// - Coverage multiplier: how many of the user's query terms
///   appear in this match-doc's searchable title
///   (title_or_alias_field), as a substring in literal mode or as
///   a case-insensitive regex match in regex mode. Multiplied as
///   (matched/total)^SEARCH_COVERAGE_EXPONENT, so at the default
///   exponent of 2 a hit that covers every term gets 1x and one
///   that covers half gets 0.25x. Skipped (factor=1) in operators
///   mode, where the user has expressed explicit MUST/MUSTNOT
///   semantics that don't translate cleanly to "fraction
///   matched".
/// - Context multiplier (Root, CycleMember, Dest, HadID,
///   MultiContained, or 1.0 for none).
///
/// adjusted_score = bm25_score * coverage * context_multiplier
pub fn group_matches_by_id (
  best_matches  : Vec < (f32, tantivy::DocAddress) >,
  searcher      : Searcher,
  tantivy_index : &TantivyIndex,
  search_terms  : &str,
  search_opts   : &SearchOptions,
  active        : Option<&ActiveSourceSet>,
) -> MatchGroups {
  let matcher : CoverageMatcher = // pre-build once
    build_coverage_matcher (search_terms, search_opts);
  let mut result_acc : MatchGroups =
    HashMap::new();
  for (score, doc_address) in best_matches {
    match searcher . doc (doc_address) {
      Ok (retrieved_doc) => {
        let retrieved_doc : TantivyDocument = retrieved_doc;
        let id_opt : Option < ID > =
          retrieved_doc
            . get_first ( tantivy_index . id_field )
            . and_then ( |v| v . as_str() )
            . map ( |s| ID::from (s) );
        // Prefer raw_title (un-reduced, only on is_title="true"
        // docs) so a textlink in the title shows as
        // `[[id:X][label]]` in search results. For alias-doc hits
        // raw_title is empty/absent, so fall back to
        // title_or_alias, which holds the alias literal.
        let title_opt : Option < String > = {
          let raw : Option<String> =
            retrieved_doc
              . get_first ( tantivy_index . raw_title_field )
              . and_then ( |v| v . as_str () )
              . map ( |s| s . to_string () )
              . filter ( |s| ! s . is_empty () );
          raw . or_else ( || retrieved_doc
            . get_first ( tantivy_index . title_or_alias_field )
            . and_then ( |v| v . as_str() )
            . map ( |s| s . to_string() )) };
        // Read title_or_alias separately for coverage counting --
        // it's the field the index actually matched against.
        let searchable_title : String =
          retrieved_doc
            . get_first ( tantivy_index . title_or_alias_field )
            . and_then ( |v| v . as_str () )
            . map ( |s| s . to_string () )
            . unwrap_or_default ();
        let source : SourceName =
          SourceName::from (
            retrieved_doc
              . get_first ( tantivy_index . source_field )
              . and_then ( |v| v . as_str () )
              . unwrap_or ("") );
        if let Some (a) = active {
          // Per-DOCUMENT source filtering, BEFORE grouping: an
          // alias document carries the ALIAS's privacy level as
          // its source, so a restricted search must drop it here
          // -- a private alias of a public node must neither match
          // nor shift ranking (dbs-and-search, 5_plan.org). The
          // group-level filter below survives as a backstop.
          if ! a . is_all ()
          && ! a . contains_source (&source) {
            continue; }}
        let origin_type : Option < ContextOriginType > =
          retrieved_doc
            . get_first ( tantivy_index . context_origin_type_field )
            . and_then ( |v| v . as_str () )
            . and_then ( ContextOriginType::from_label );
        let multiplier : f32 =
          origin_type . map_or ( 1.0, |t| t . multiplier() );
        let coverage : f32 =
          coverage_factor (&matcher, &searchable_title);
        let adjusted_score : f32 = score * coverage * multiplier;
        if let (Some (id), Some (title)) = (id_opt, title_opt) {
          result_acc
            . entry (id)
            . or_insert_with ( || (
              source,
              Vec::new () ))
            . 1
            . push (( adjusted_score, title )); }},
      Err (e) => { tracing::error! (
        "Error retrieving document: {}", e ); }} }
  result_acc }

/// Builds a ViewForest representing the search results.
/// Returns the viewforest and the ordered list of result IDs.
///
/// Forest structure: each root is a search result, with AliasCol +
/// Alias children if aliases matched.
/// The result ids to drop from the top level because a USER-OWNED
/// result recursively overrides them: they will reappear as
/// overriddenward graft descendants of that owned result
/// (TODO/override-ancestry-in-search-results.org, "Suppression"). A
/// FOREIGN overrider never suppresses -- so a pure-foreign mutual
/// override shows both, and a "boring" foreign overrider does not hide
/// the node it overrides. Reachability follows edge-level-visible
/// outbound overrides, matching what the graft will actually draw.
/// Only search hits ('matches_by_id' keys) are ever suppressed. A
/// user-owned overrider that matched the query but ranks past the
/// display limit could suppress its target without itself being shown
/// (rare; suppression frees slots, so the anchor usually fits).
pub fn suppressed_result_ids (
  matches_by_id : &MatchGroups,
  graph         : &InRustGraph,
  config        : &SkgConfig,
  active         : &ActiveSourceSet,
) -> HashSet<ID> {
  let candidates : HashSet<ID> =
    matches_by_id . keys () . cloned () . collect ();
  let mut suppressed : HashSet<ID> = HashSet::new ();
  for owned in candidates . iter () . filter ( |id|
    graph . nodes . get (*id)
      . map_or ( false, |n| config . user_owns_source (&n . source) ) )
  { // Walk owned's overriddenward closure; any HIT in it is suppressed
    // (it will hang under 'owned'). Cycle-guarded: foreign edges in the
    // chain can form cycles even though user-owned ones cannot.
    let mut stack : Vec<ID> = vec![ owned . clone () ];
    let mut seen  : HashSet<ID> = HashSet::from ([ owned . clone () ]);
    while let Some (cur) = stack . pop () {
      for target in graph . outbound_pids_for_relation_gated (
        &cur, NodeRelation::OverridesViewOf, Some (active) ) {
        if ! seen . insert (target . clone ()) { continue; }
        if candidates . contains (&target) {
          suppressed . insert (target . clone ()); }
        stack . push (target); }} }
  suppressed }

pub fn build_search_viewforest (
  _search_terms : &str,
  matches_by_id : &MatchGroups,
  suppressed    : &HashSet<ID>,
) -> (ViewForest, Vec<ID>) {
  let mut viewforest : ViewForest =
    ViewForest::new ();
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
        // Suppress before truncation, so a dropped result frees a slot
        // for the next-ranked hit (design corner O2).
        . filter ( |entry| ! suppressed . contains (entry . 0) )
        . take (SEARCH_DISPLAY_LIMIT)
    { search_results . push ( (*id) . clone () );
      let mut sorted_matches : Vec < &(f32, String) > =
        // We borrow from matches_by_id.
        // Sort matches by score descending for display.
        matches . iter () . collect ();
      sorted_matches . sort_by ( |a, b|
        b . 0 . partial_cmp (&a . 0) . unwrap () );
      let (_score, title) : &(f32, String) = sorted_matches [0];
      let result_treeid : NodeId =
        viewforest . append_root (
          mk_indefinitive_viewnode (
            (*id) . clone (),
            (*source) . clone (),
            title . clone (),
            ParentIs::Absent ) );
      if sorted_matches . len () > 1 {
        // We bury all but the best match in an AliasCol.
        // PITFALL: The title might not be the best match,
        // in which case this makes it look like an alias.
        let aliascol_id : NodeId = {
          let mut result_mut : NodeMut<ViewNode> =
            viewforest . get_mut (result_treeid) . unwrap ();
          result_mut . append ( ViewNode {
            focused     : false,
            folded      : true,
            body_folded : false,
            kind        : ViewNodeKind::QualCol (
              QualCol::Alias ) } )
          . id () };
        for (_score, title) in sorted_matches . iter () . skip (1) {
          let mut aliascol_mut : NodeMut<ViewNode> =
            viewforest . get_mut (aliascol_id) . unwrap ();
          aliascol_mut . append ( ViewNode {
            focused     : false,
            folded      : false,
            body_folded : false,
            kind        : ViewNodeKind::Qual (Qual::Alias {
                text       : title . clone (),
                membership : MembershipAxes::default () } ) } ); }} }
  (viewforest, search_results) }
