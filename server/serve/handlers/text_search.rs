pub mod render_enriched_search_buffer;
mod coverage;

use coverage::{CoverageMatcher, build_coverage_matcher, coverage_factor};

/// PITFALL: Uses two layers of truncation.
/// Tantivy truncates its search after (unimaginable) 1e5 results.
/// The server (outside of Tantivy) then sorts those results
/// to surface roots and other high-value gometries,
/// which are truncated for display.

use crate::consts::SEARCH_DISPLAY_LIMIT;
use crate::prominence::ProminenceSource;
use crate::dbs::tantivy::background_writer::wait_for_tantivy_writes_idle;
use crate::dbs::tantivy::search::{
  SearchOptions, has_overPrivateText_telescope, search_index};
use crate::dbs::in_rust_graph::containerward_role_tree::{ ContainerwardRoleTree, containerward_role_trees_by_skgid_from_skgids};
use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::in_rust_graph::relation_accessors::NodeRelation;
use crate::dbs::in_rust_graph::stats::{
  AllGraphnodeStats,
  fetch_all_graphnodestats_with_skgrepo_set};
use crate::types::env::{RuntimeGeneration, SkgEnv};
use crate::org_to_text::viewforest_to_string;
use crate::update_buffer::set_viewnodestats_in_viewforest;
use crate::serve::ViewsState;
use crate::serve::handlers::text_release::{
  TextReleaseDecision,
  SearchOverPrivateTextChoice,
  decide as decide_text_release,
  search_challenge_response,
  search_choice_from_request};
use crate::serve::protocol::TcpToClient;
use crate::serve::util::{ send_response_with_length_prefix, tag_text_response};
use crate::types::git::RelationshipAxes;
use crate::types::views_state::ViewUri;
use crate::types::misc::{TantivyIndex, SkgConfig, ID, SkgRepoName};
use crate::skgrepo_sets::{ActiveSkgRepoSet, search_skgids_for_skgrepo_set_for_test as search_ids_for_skgrepo_set_for_test_impl};
use crate::types::sexp::extract_v_from_kv_pair_in_sexp;
use crate::types::tree::forest::ViewForest;
use crate::types::viewnode::{ Viewnode, ViewnodeKind, AffectsParent, mk_writeProtected_viewnode};
use crate::types::viewnode::{PropertyFolder, Property};

use ego_tree::{NodeId, NodeMut};
use sexp::{Sexp, Atom};
use std::collections::{HashMap, HashSet};
use std::net::TcpStream;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex, MutexGuard};
use tantivy::{TantivyDocument, Searcher};
use tantivy::schema::document::Value;

/// Maps each ID to search hits (plural -- IDs can have aliases,
/// so one ID might get multiple matches).
/// The score incorporates a context-based multiplier:
/// Each result's BM25 score from Tantivy is multiplied by
/// the multiplier corresponding to its prominence_source.
/// Non-origins keep their raw score (multiplier = 1).
pub type MatchGroups =
  HashMap < ID, ( SkgRepoName,
                  Vec < ( f32,           // score (after multiplier)
                          String ) >) >; // title or alias

pub fn search_skgids_for_skgrepo_set_for_test (
  tantivy_index : &TantivyIndex,
  config        : &SkgConfig,
  active        : &ActiveSkgRepoSet,
  terms         : &str,
  limit         : usize,
) -> Result<Vec<ID>, Box<dyn std::error::Error>> {
  search_ids_for_skgrepo_set_for_test_impl (
    tantivy_index, config, active, terms, limit ) }

pub fn enriched_search_buffer_for_skgrepo_set_for_test (
  graph                             : &InRustGraph,
  terms                             : &str,
  matches_by_skgid                  : &MatchGroups,
  search_results                    : &[ID],
  containerward_role_trees_by_skgid : &HashMap<ID, ContainerwardRoleTree>,
  tantivy_index                     : &TantivyIndex,
  config                            : &SkgConfig,
  active                            : &ActiveSkgRepoSet,
) -> Result<String, Box<dyn std::error::Error>> {
  let (mut viewforest, _ids) : (ViewForest, Vec<ID>) =
    build_search_viewforest (terms, matches_by_skgid, &HashSet::new ());
  render_enriched_search_buffer::insert_full_containerward_role_trees_into_search_view (
    &mut viewforest,
    graph,
    search_results,
    containerward_role_trees_by_skgid,
    tantivy_index,
    config,
    active );
  render_enriched_search_buffer::insert_overrideward_view_subtrees (
    &mut viewforest,
    graph,
    search_results,
    active );
  set_viewnodestats_in_viewforest (
    // Mirror the production enrichment path (handle_snapshot_response):
    // compute view-relative stats so the rendered buffer carries the
    // homeRepoHerald at skgrepo boundaries. Empty containment maps suffice
    // here -- homeRepoAtBoundary is derived from the tree alone; the maps
    // only feed the containsParent stat, which this test does not assert.
    &mut viewforest,
    graph,
    & HashMap::new (),
    & HashMap::new (),
    config,
    Some (active) );
  Ok ( viewforest_to_string ( &viewforest, config )? ) }

/// Structured enrichment data passed through the slot,
/// replacing the raw rendered String.
pub struct SearchEnrichmentPayload {
  pub runtime                           : Arc<RuntimeGeneration>,
  pub terms                             : String,
  pub search_results                    : Vec<ID>,
  pub containerward_role_trees_by_skgid : HashMap<ID, ContainerwardRoleTree>,
  pub graphnodestats                    : AllGraphnodeStats,
  /// Load-bearing across the asynchronous buffer-snapshot exchange: enrichment
  /// must not broaden a preflight decision to exclude overPrivateText telescopes.
  pub include_overPrivateText_telescopes : bool,
}

/// Provides two responses, one fast and one slow.
/// The slower one is 'enriched'
/// with containerward role trees and graphnodestats at each search hit,
/// and is processed in the background -- the user need not await it.
///
/// Every 'search-results' is eventually followed by exactly one
/// 'search-enrichment', because that is what releases the client's
/// stream guard. When there is nothing to enrich (no matches, or an
/// error), that message follows at once, without content. Otherwise it
/// is owed: this returns the search terms, and the caller must see that
/// the enrichment, or a contentless stand-in, is eventually sent.
/// The overPrivateText challenge is the exception: it sends neither
/// message, and the client releases its guard on receiving it.
pub fn handle_text_search_request (
  stream           : &mut TcpStream,
  request          : &str,
  env              : &SkgEnv,
  enrichment_slot  : &Arc<Mutex<Option<SearchEnrichmentPayload>>>,
  search_cancelled : &Arc<AtomicBool>,
  views_state       : &mut ViewsState,
  active           : &ActiveSkgRepoSet,
) -> Option<String> {
  let parsed_sexp : Result < Sexp, String > =
    sexp::parse (request)
    . map_err ( |e| format! (
      "Failed to parse S-expression: {}", e ) );
  let sexp : Sexp = match parsed_sexp {
    Ok (s) => s,
    Err (err) => {
      tracing::error! ( "{}", err );
      send_search_results_without_enrichment (stream, "", &err);
      return None; } };
  let search_terms : Result < String, String > =
    extract_v_from_kv_pair_in_sexp ( &sexp, "terms" );
  let search_choice : Option<SearchOverPrivateTextChoice> =
    match search_choice_from_request (&sexp) {
      Ok (choice) => choice,
      Err (error) => {
        send_search_results_without_enrichment (
          stream,
          search_terms . as_deref () . unwrap_or (""),
          &error );
        return None; }};
  match search_terms {
    Ok (search_terms) => {
      // Wait for any in-flight background search-index writes to commit, so
      // the search reflects every save issued so far (read-your-writes).
      wait_for_tantivy_writes_idle ();
      let runtime = env . runtime_snapshot ();
      let index_has_overPrivateText : bool =
        match has_overPrivateText_telescope (&runtime . tantivy_index) {
          Ok (has_overPrivateText) => has_overPrivateText,
          Err (error) => {
            send_search_results_without_enrichment (
              stream,
              &search_terms,
              &format! ("Error checking search privacy: {}", error) );
            return None; }};
      if ! active . is_all ()
         && index_has_overPrivateText
         && search_choice . is_none () {
        send_response_with_length_prefix (
          stream, &search_challenge_response () );
        return None; }
      let include_overPrivateText_telescopes : bool =
        active . is_all ()
        || search_choice == Some (SearchOverPrivateTextChoice::Include);
      let search_opts : SearchOptions = SearchOptions {
        regex     : bool_key ( &sexp, "regex" ),
        body      : bool_key ( &sexp, "body" ),
        operators : bool_key ( &sexp, "operators" ),
        exclude_overPrivateText_telescope : ! include_overPrivateText_telescopes,
      };
      // --- Phase 1: immediate results without paths ---
      match search_index ( &runtime . tantivy_index,
                           &search_terms,
                           &search_opts ) {
        Ok (( best_matches, searcher )) => {
          if best_matches . is_empty () {
            send_search_results_without_enrichment (
              stream, &search_terms, "No matches found." );
            return None; }
          let matches_by_skgid : MatchGroups =
            filter_match_groups_to_active_skgrepos (
              group_matches_by_skgid (
              best_matches,
              searcher,
              &runtime . tantivy_index,
              &search_terms,
              &search_opts,
              Some (active) ),
              active );
          if matches_by_skgid . is_empty () {
            send_search_results_without_enrichment (
              stream, &search_terms, "No matches found." );
            return None; }
          let suppressed : HashSet<ID> =
            suppressed_result_skgids (
              &matches_by_skgid,
              &runtime . graph,
              &runtime . config,
              active );
          let (viewforest, search_results) : (ViewForest, Vec<ID>) =
            build_search_viewforest (
              &search_terms,
              &matches_by_skgid,
              &suppressed );
          let approved : HashSet<ID> =
            if include_overPrivateText_telescopes {
              search_results . iter () . cloned () . collect ()
            } else { HashSet::new () };
          let release = decide_text_release (
            "text-search", active, &search_results,
            &runtime . graph, &approved );
          if matches! (
            release, TextReleaseDecision::Challenge { .. } ) {
            send_response_with_length_prefix (
              stream, &search_challenge_response () );
            return None; }
          let warnings : Vec<String> = match release {
            TextReleaseDecision::AllowWithWarning { warning } =>
              vec! [warning],
            _ => Vec::new (), };
          let rendered : String =
            // Render first, before register_view moves the viewforest
            viewforest_to_string ( &viewforest, &runtime . config )
            . expect ("search viewforest rendering never fails");
          let uri : ViewUri =
            ViewUri::SearchView ( search_terms . clone () );
          if views_state . open_views . views . contains_key (&uri) {
            // Replace prior search with the same terms.
            views_state . open_views . unregister_view (&uri); }
          views_state . open_views . register_view (
            &runtime . graph, uri, viewforest, &search_results );
          send_response_with_length_prefix (
            // phase 1 (unenriched) tagged LP response
            stream,
            & mk_search_results_sexp (&rendered, &warnings) );
          spawn_enrichment_thread (
            // phase 2 (enriched) search results, backgrounded
            enrichment_slot, search_cancelled,
            runtime . clone (),
            &search_terms, &search_results, active,
            include_overPrivateText_telescopes );
          Some (search_terms) },
        Err (e) => {
          send_search_results_without_enrichment (
            stream,
            &search_terms,
            & format! ("Error querying the search index: {}", e) );
          None }} },
    Err (err) => {
      let error_msg : String =
        format! (
          "Error extracting search terms: {}", err );
      tracing::error! ( "{}", error_msg ) ;
      send_search_results_without_enrichment (
        stream, "", &error_msg );
      None }} }

/// Sends TEXT as the phase-1 'search-results', then at once the
/// contentless 'search-enrichment' that tells the client no
/// enrichment is coming.
fn send_search_results_without_enrichment (
  stream       : &mut TcpStream,
  search_terms : &str,
  text         : &str,
) {
  send_response_with_length_prefix (
    stream,
    & tag_text_response (TcpToClient::SearchResults, text) );
  send_response_with_length_prefix (
    stream,
    & mk_search_enrichment_sexp (search_terms, None, &[]) ); }

/// For when the client closes a search buffer whose enrichment is
/// still owed. The client can then no longer answer the buffer-snapshot
/// request, so without this the enrichment would never be sent and
/// the client's stream guard would never be released.
pub fn abandon_search_enrichment (
  stream           : &mut TcpStream,
  search_terms     : &str,
  enrichment_slot  : &Arc<Mutex<Option<SearchEnrichmentPayload>>>,
  search_cancelled : &Arc<AtomicBool>,
) {
  cancel_search_enrichment (enrichment_slot, search_cancelled);
  send_response_with_length_prefix (
    stream,
    & mk_search_enrichment_sexp (search_terms, None, &[]) ); }

/// Stops any running enrichment thread and discards any payload it
/// already wrote. The thread checks the flag while holding the slot's
/// lock, so once this returns the slot stays empty.
pub fn cancel_search_enrichment (
  enrichment_slot  : &Arc<Mutex<Option<SearchEnrichmentPayload>>>,
  search_cancelled : &Arc<AtomicBool>,
) {
  search_cancelled . store (true, Ordering::SeqCst);
  if let Ok (mut slot) = enrichment_slot . lock () {
    *slot = None; }}

/// Read a boolean axis flag from the request sexp. Absent, empty, or
/// any non-"true" string is treated as false.
fn bool_key (
  sexp : &Sexp,
  key  : &str,
) -> bool {
  extract_v_from_kv_pair_in_sexp ( sexp, key )
    . unwrap_or_default ()
    == "true" }

fn filter_match_groups_to_active_skgrepos (
  matches_by_skgid : MatchGroups,
  active        : &ActiveSkgRepoSet,
) -> MatchGroups {
  if active . is_all () {
    return matches_by_skgid; }
  matches_by_skgid . into_iter ()
    . filter ( |(_, (skgrepo, _))|
      active . contains_skgrepo (skgrepo) )
    . collect () }

/// Spawn a background thread to compute containerward acnestries
/// and graphnodestats, then write the structured payload
/// to the shared slot. Clears any stale enrichment and resets
/// the cancellation flag before spawning.
fn spawn_enrichment_thread (
  enrichment_slot  : &Arc<Mutex<Option<SearchEnrichmentPayload>>>,
  search_cancelled : &Arc<AtomicBool>,
  runtime          : Arc<RuntimeGeneration>,
  search_terms     : &str,
  search_results   : &[ID],
  active           : &ActiveSkgRepoSet,
  include_overPrivateText_telescopes : bool,
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
  let active_clone  : ActiveSkgRepoSet   = active . clone ();
  let terms_clone   : String            = search_terms . to_string ();
  let ids_clone     : Vec<ID>           = search_results . to_vec ();
  let max_depth : usize = runtime . config . max_role_tree_depth;
  std::thread::spawn ( move || {
    tracing::info! (
      generation = runtime . generation,
      result_count = ids_clone . len (),
      "search enrichment thread started");
    let containerward_role_trees_by_skgid : HashMap<ID, ContainerwardRoleTree> =
      containerward_role_trees_by_skgid_from_skgids (
        &runtime . graph, &ids_clone, max_depth );
    tracing::info! ("search enrichment: role tree computed ({} entries)",
              containerward_role_trees_by_skgid . len ());
    if cancel_clone . load (Ordering::SeqCst) {
      tracing::info! ("search enrichment: cancelled after role tree");
      return; }
    let all_enriched_skgids : Vec<ID> = {
      // Collect result IDs + every ID from role trees + every
      // override relative the enrichment will graft (so those grafted
      // nodes get their graphStats, hence their override heralds).
      let mut skgid_set : HashSet<ID> = HashSet::new ();
      for skgid in &ids_clone {
        skgid_set . insert ( skgid . clone () ); }
      for tree in containerward_role_trees_by_skgid . values () {
        collect_skgids_from_role_tree_node ( tree, &mut skgid_set ); }
      skgid_set . extend (
        render_enriched_search_buffer::collect_overrideward_view_subtree_skgids (
          &runtime . graph, &ids_clone, &active_clone ) );
      skgid_set . into_iter () . collect () };
    let graphnodestats : AllGraphnodeStats =
      fetch_all_graphnodestats_with_skgrepo_set (
        &runtime . graph,
        &all_enriched_skgids,
        Some (&active_clone) )
      . unwrap_or_else ( |e| {
        tracing::warn! ("search enrichment: graphnodestats failed: {}", e);
        AllGraphnodeStats::empty () } );
    tracing::info! ("search enrichment: graphnodestats fetched for {} IDs",
              all_enriched_skgids . len ());
    let mut guard : MutexGuard<Option<SearchEnrichmentPayload>> =
      slot_clone . lock () . unwrap ();
    if cancel_clone . load (Ordering::SeqCst) {
      // Checked under the lock, so that 'cancel_search_enrichment'
      // cannot clear the slot between this check and the write.
      tracing::info! ("search enrichment: cancelled after graphnodestats");
      return; }
    tracing::info! ("search enrichment: writing payload to slot");
    *guard = Some ( SearchEnrichmentPayload {
      runtime,
      terms          : terms_clone,
      search_results : ids_clone,
      containerward_role_trees_by_skgid,
      graphnodestats,
      include_overPrivateText_telescopes } ); } ); }

fn collect_skgids_from_role_tree_node(
  node      : &ContainerwardRoleTree,
  skgid_set : &mut HashSet<ID>,
) {
  skgid_set . insert ( node . skgid () . clone () );
  if let ContainerwardRoleTree::Inner ( _, children ) = node {
    for child in children {
      collect_skgids_from_role_tree_node ( child, skgid_set ); }}}

/// Build the tagged s-exp for a search enrichment payload.
/// Format: (("response-type" "search-enrichment")
///          ("terms" "TERMS") ("content" "ORG") ("warnings" ()))
/// Without CONTENT the "content" pair is omitted. That tells the
/// client no enrichment is coming: it releases its stream guard,
/// shows any warnings, and leaves the search buffer alone.
pub fn mk_search_enrichment_sexp (
  terms    : &str,
  content  : Option<&str>,
  warnings : &[String],
) -> String {
  let mut fields : Vec<Sexp> = vec! [
    Sexp::List ( vec! [
      Sexp::Atom ( Atom::S ( "response-type" . to_string () )),
      Sexp::Atom ( Atom::S ( TcpToClient::SearchEnrichment
                             . repr_in_client () . to_string () )), ] ),
    Sexp::List ( vec! [
      Sexp::Atom ( Atom::S ( "terms"   . to_string () )),
      Sexp::Atom ( Atom::S ( terms     . to_string () )), ] ), ];
  if let Some (content) = content {
    fields . push (
      Sexp::List ( vec! [
        Sexp::Atom ( Atom::S ( "content" . to_string () )),
        Sexp::Atom ( Atom::S ( content   . to_string () )), ] )); }
  fields . push (
    Sexp::List ( vec! [
      Sexp::Atom ( Atom::S ( "warnings" . to_string () )),
      Sexp::List (
        warnings . iter ()
        . map ( |warning|
          Sexp::Atom ( Atom::S (warning . clone ()) ) )
        . collect () ), ] ));
  Sexp::List (fields) . to_string () }

fn mk_search_results_sexp (
  content  : &str,
  warnings : &[String],
) -> String {
  Sexp::List ( vec! [
    Sexp::List ( vec! [
      Sexp::Atom ( Atom::S ("response-type" . to_string ()) ),
      Sexp::Atom ( Atom::S (
        TcpToClient::SearchResults . repr_in_client () . to_string ()) ),
    ] ),
    Sexp::List ( vec! [
      Sexp::Atom ( Atom::S ("content" . to_string ()) ),
      Sexp::Atom ( Atom::S (content . to_string ()) ),
    ] ),
    Sexp::List ( vec! [
      Sexp::Atom ( Atom::S ("warnings" . to_string ()) ),
      Sexp::List (
        warnings . iter ()
        . map ( |warning|
          Sexp::Atom ( Atom::S (warning . clone ()) ) )
        . collect () ),
    ] ),
  ] ) . to_string ()
}

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
/// - Context multiplier (Root, CycleMember, Mentioned, HadID,
///   MultiContained, or 1.0 for none).
///
/// adjusted_score = bm25_score * coverage * context_multiplier
pub fn group_matches_by_skgid (
  best_matches  : Vec < (f32, tantivy::DocAddress) >,
  searcher      : Searcher,
  tantivy_index : &TantivyIndex,
  search_terms  : &str,
  search_opts   : &SearchOptions,
  active        : Option<&ActiveSkgRepoSet>,
) -> MatchGroups {
  let matcher : CoverageMatcher = // pre-build once
    build_coverage_matcher (search_terms, search_opts);
  let mut result_acc : MatchGroups =
    HashMap::new();
  for (score, doc_address) in best_matches {
    match searcher . doc (doc_address) {
      Ok (retrieved_doc) => {
        let retrieved_doc : TantivyDocument = retrieved_doc;
        let skgid_opt : Option < ID > =
          retrieved_doc
            . get_first ( tantivy_index . id_field )
            . and_then ( |v| v . as_str() )
            . map ( |s| ID::from (s) );
        // Prefer raw_title (un-reduced, only on is_title="true"
        // docs) so a link in the title shows as
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
        // it's the field the search index actually matched against.
        let searchable_title : String =
          retrieved_doc
            . get_first ( tantivy_index . title_or_alias_field )
            . and_then ( |v| v . as_str () )
            . map ( |s| s . to_string () )
            . unwrap_or_default ();
        let skgrepo : SkgRepoName =
          SkgRepoName::from (
            retrieved_doc
              . get_first ( tantivy_index . skgrepo_field )
              . and_then ( |v| v . as_str () )
              . unwrap_or ("") );
        if let Some (a) = active {
          // Per-DOCUMENT skgrepo filtering, BEFORE grouping: an
          // alias document carries the ALIAS's relRepo as
          // its skgrepo, so a restricted search must drop it here
          // -- a private alias of a public node must neither match
          // nor shift ranking (dbs-and-search, 5_plan.org). The
          // group-level filter below survives as a backstop.
          if ! a . is_all ()
          && ! a . contains_skgrepo (&skgrepo) {
            continue; }}
        let origin_type : Option < ProminenceSource > =
          retrieved_doc
            . get_first ( tantivy_index . prominence_source_field )
            . and_then ( |v| v . as_str () )
            . and_then ( ProminenceSource::from_label );
        let multiplier : f32 =
          origin_type . map_or ( 1.0, |t| t . multiplier() );
        let coverage : f32 =
          coverage_factor (&matcher, &searchable_title);
        let adjusted_score : f32 = score * coverage * multiplier;
        if let (Some (skgid), Some (title)) = (skgid_opt, title_opt) {
          result_acc
            . entry (skgid)
            . or_insert_with ( || (
              skgrepo,
              Vec::new () ))
            . 1
            . push (( adjusted_score, title )); }},
      Err (e) => { tracing::error! (
        "Error retrieving document: {}", e ); }} }
  result_acc }

/// Builds a ViewForest representing the search results.
/// Returns the viewforest and the ordered list of result IDs.
///
/// Forest structure: each root is a search result, with AliasFolder +
/// Alias children if aliases matched.
/// The result ids to drop from the top level because an OWNED
/// result recursively overrides them: they will reappear as
/// overriddenward role-graft descendants of that owned result
/// (TODO/DONE/override-ancestry-in-search-results.org, "Suppression"). A
/// FOREIGN overrider never suppresses -- so a pure-foreign mutual
/// override shows both, and a "boring" foreign overrider does not hide
/// the node it overrides. Reachability follows relRepo-visible
/// outbound overrides, matching what the role graft will actually draw.
/// Only search hits ('matches_by_id' keys) are ever suppressed. A
/// owned overrider that matched the query but ranks past the
/// display limit could suppress its target without itself being shown
/// (rare; suppression frees slots, so the anchor usually fits).
pub fn suppressed_result_skgids (
  matches_by_skgid : &MatchGroups,
  graph            : &InRustGraph,
  config           : &SkgConfig,
  active           : &ActiveSkgRepoSet,
) -> HashSet<ID> {
  let candidates : HashSet<ID> =
    matches_by_skgid . keys () . cloned () . collect ();
  let mut suppressed : HashSet<ID> = HashSet::new ();
  for owned in candidates . iter () . filter ( |skgid|
    graph . nodes . get (*skgid)
      . map_or ( false, |n| config . skgrepo_is_owned (&n . home_skgrepo) ) )
  { // Walk owned's overriddenward closure; any HIT in it is suppressed
    // (it will hang under 'owned'). Cycle-guarded: foreign relationships in the
    // chain can form cycles even though owned ones cannot.
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
  _search_terms    : &str,
  matches_by_skgid : &MatchGroups,
  suppressed       : &HashSet<ID>,
) -> (ViewForest, Vec<ID>) {
  let mut viewforest : ViewForest =
    ViewForest::new ();
  let mut id_entries : Vec < ( &ID,
                               &SkgRepoName,
                               &Vec < ( f32, String ) > ) > =
    matches_by_skgid . iter ()
    . map ( |(skgid, (skgrepo, matches))| // flatten
             (skgid, skgrepo, matches) )
    . collect ();
  id_entries . sort_by ( |a, b| { // sort by best score (descending)
    let score_a : f32 =
      a . 2 . first () . map ( |(s, _)| *s ) . unwrap_or (0.0);
    let score_b : f32 =
      b . 2 . first () . map ( |(s, _)| *s ) . unwrap_or (0.0);
    score_b . partial_cmp (& score_a)
    . unwrap_or (std::cmp::Ordering::Equal) } );
  let mut search_results : Vec < ID > = Vec::new ();
  for (skgid, skgrepo, matches) in id_entries . iter ()
        // Suppress before truncation, so a dropped result frees a slot
        // for the next-ranked hit (design corner O2).
        . filter ( |entry| ! suppressed . contains (entry . 0) )
        . take (SEARCH_DISPLAY_LIMIT)
    { search_results . push ( (*skgid) . clone () );
      let mut sorted_matches : Vec < &(f32, String) > =
        // We borrow from matches_by_id.
        // Sort matches by score descending for display.
        matches . iter () . collect ();
      sorted_matches . sort_by ( |a, b|
        b . 0 . partial_cmp (&a . 0) . unwrap () );
      let (_score, title) : &(f32, String) = sorted_matches [0];
      let result_treeid : NodeId =
        viewforest . append_root (
          mk_writeProtected_viewnode (
            (*skgid) . clone (),
            (*skgrepo) . clone (),
            title . clone (),
            AffectsParent::NA ) );
      if sorted_matches . len () > 1 {
        // We bury all but the best match in an AliasFolder.
        // PITFALL: The title might not be the best match,
        // in which case this makes it look like an alias.
        let aliasfolder_skgid : NodeId = {
          let mut result_mut : NodeMut<Viewnode> =
            viewforest . get_mut (result_treeid) . unwrap ();
          result_mut . append ( Viewnode {
            focused     : false,
            folded      : true,
            body_folded : false,
            kind        : ViewnodeKind::PropertyFolder (
              PropertyFolder::Alias ) } )
          . id () };
        for (_score, title) in sorted_matches . iter () . skip (1) {
          let mut aliasfolder_mut : NodeMut<Viewnode> =
            viewforest . get_mut (aliasfolder_skgid) . unwrap ();
          aliasfolder_mut . append ( Viewnode {
            focused     : false,
            folded      : false,
            body_folded : false,
            kind        : ViewnodeKind::Property (Property::Alias {
                text       : title . clone (),
                relRepo : None,
                relRepo_request : None,
                relationship_axes : RelationshipAxes::default () } ) } ); }} }
  (viewforest, search_results) }
