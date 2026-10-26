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
use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::in_rust_graph::relation_accessors::NodeRelation;
use crate::types::env::{RuntimeGeneration, SkgEnv};
use crate::org_to_text::viewforest_to_string;
use crate::refresh::{Truth, refresh_viewforest};
use crate::serve::ViewsState;
use crate::serve::handlers::text_release::{
  TextReleaseDecision,
  SearchOverPrivateTextChoice,
  decide as decide_text_release,
  exclude_overPrivateText_nodes_from_viewforest,
  search_challenge_response,
  search_choice_from_request};
use crate::serve::protocol::TcpToClient;
use crate::serve::util::{ send_response_with_length_prefix, sexp_to_client_string, tag_text_response};
use crate::types::views_state::{ViewforestId, ViewnodeIdRecord};
use crate::types::misc::{TantivyIndex, SkgConfig, ID, SkgrepoName};
use crate::skgrepo_sets::{SkgrepoRestriction, search_gnids_for_skgrepo_set_for_test as search_ids_for_skgrepo_set_for_test_impl};
use crate::types::sexp::extract_v_from_kv_pair_in_sexp;
use crate::types::tree::forest::ViewForest;
use crate::types::viewnode::{ FoldAndFocus, Viewnode, ViewnodeKind, mk_writeProtected_viewnode};
use crate::types::viewnode::{Folder, FolderRelation, PropertyMember, ViewRequest, Vognode, viewnode_of};
use crate::update_buffer::completion_mode::CompletionMode;

use ego_tree::{NodeId, NodeMut};
use sexp::{Sexp, Atom};
use std::collections::{HashMap, HashSet};
use std::error::Error;
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
  HashMap < ID, ( SkgrepoName,
                  Vec < ( f32,           // score (after multiplier)
                          String ) >) >; // title or alias

pub fn search_gnids_for_skgrepo_set_for_test (
  tantivy_index : &TantivyIndex,
  config        : &SkgConfig,
  restriction   : &SkgrepoRestriction,
  terms         : &str,
  limit         : usize,
) -> Result<Vec<ID>, Box<dyn std::error::Error>> {
  search_ids_for_skgrepo_set_for_test_impl (
    tantivy_index, config, restriction, terms, limit ) }

pub fn enriched_search_buffer_for_skgrepo_set_for_test (
  graph            : &InRustGraph,
  config           : &SkgConfig,
  terms            : &str,
  matches_by_gnid : &MatchGroups,
  restriction      : &SkgrepoRestriction,
) -> Result<String, Box<dyn Error>> {
  let (mut viewforest, search_results) : (ViewForest, Vec<ID>) =
    build_search_viewforest (terms, matches_by_gnid, &HashSet::new ());
  enrich_search_viewforest (
    &mut viewforest, &mut ViewnodeIdRecord::default (), graph, config,
    &search_results, restriction, false ) }

/// What the enrichment of a search buffer needs besides its snapshot
/// ('enrich_search_viewforest'), passed through the slot.
pub struct SearchEnrichmentPayload {
  pub runtime                           : Arc<RuntimeGeneration>,
  pub terms                             : String,
  pub search_results                    : Vec<ID>,
  /// Load-bearing across the asynchronous buffer-snapshot exchange: enrichment
  /// must not broaden a preflight decision to exclude overPrivateText telescopes.
  pub include_overPrivateText_telescopes : bool,
}

/// Provides two responses, one fast and one slow.
/// The slower one is 'enriched' with partner folders at each search
/// hit ('enrich_search_viewforest'), once the client has sent the
/// buffer snapshot the idle loop asks for when the payload is ready
/// -- the user need not await it.
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
  restriction      : &SkgrepoRestriction,
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
      // Wait for any in-flight background search-index writes to tantivy-commit, so
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
      if ! restriction . is_all ()
         && index_has_overPrivateText
         && search_choice . is_none () {
        send_response_with_length_prefix (
          stream, &search_challenge_response () );
        return None; }
      let include_overPrivateText_telescopes : bool =
        restriction . is_all ()
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
          let matches_by_gnid : MatchGroups =
            filter_match_groups_to_unrestricted_skgrepos (
              group_matches_by_gnid (
              best_matches,
              searcher,
              &runtime . tantivy_index,
              &search_terms,
              &search_opts,
              Some (restriction) ),
              restriction );
          if matches_by_gnid . is_empty () {
            send_search_results_without_enrichment (
              stream, &search_terms, "No matches found." );
            return None; }
          let suppressed : HashSet<ID> =
            suppressed_result_gnids (
              &matches_by_gnid,
              &runtime . graph,
              &runtime . config,
              restriction );
          let (mut viewforest, search_results) : (ViewForest, Vec<ID>) =
            build_search_viewforest (
              &search_terms,
              &matches_by_gnid,
              &suppressed );
          let approved : HashSet<ID> =
            if include_overPrivateText_telescopes {
              search_results . iter () . cloned () . collect ()
            } else { HashSet::new () };
          let release = decide_text_release (
            "text-search", restriction, &search_results,
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
          ViewnodeIdRecord::default () // 'register_view' reads it back from the forest
            . assign_viewnode_ids (&mut viewforest);
          let rendered : String =
            // Render first, before register_view moves the viewforest
            viewforest_to_string ( &viewforest, &runtime . config )
            . expect ("search viewforest rendering never fails");
          let viewforest_id : ViewforestId =
            ViewforestId::SearchView ( search_terms . clone () );
          if views_state . open_views . views . contains_key (&viewforest_id) {
            // Replace prior search with the same terms.
            views_state . open_views . unregister_view (&viewforest_id); }
          views_state . open_views . register_view (
            &runtime . graph, viewforest_id, viewforest, &search_results );
          send_response_with_length_prefix (
            // phase 1 (unenriched) tagged LP response
            stream,
            & mk_search_results_sexp (&rendered, &warnings) );
          spawn_enrichment_thread (
            // phase 2 (enriched) search results, backgrounded
            enrichment_slot, search_cancelled,
            runtime . clone (),
            &search_terms, &search_results,
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

fn filter_match_groups_to_unrestricted_skgrepos (
  matches_by_gnid : MatchGroups,
  restriction   : &SkgrepoRestriction,
) -> MatchGroups {
  if restriction . is_all () {
    return matches_by_gnid; }
  matches_by_gnid . into_iter ()
    . filter ( |(_, (skgrepo, _))|
      restriction . contains_skgrepo (skgrepo) )
    . collect () }

/// Spawn a background thread to write the enrichment's payload to the
/// shared slot, where the idle loop finds it and asks the client for
/// the buffer snapshot. Clears any stale enrichment and resets the
/// cancellation flag before spawning.
fn spawn_enrichment_thread (
  enrichment_slot  : &Arc<Mutex<Option<SearchEnrichmentPayload>>>,
  search_cancelled : &Arc<AtomicBool>,
  runtime          : Arc<RuntimeGeneration>,
  search_terms     : &str,
  search_results   : &[ID],
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
  let terms_clone   : String            = search_terms . to_string ();
  let ids_clone     : Vec<ID>           = search_results . to_vec ();
  std::thread::spawn ( move || {
    let mut guard : MutexGuard<Option<SearchEnrichmentPayload>> =
      slot_clone . lock () . unwrap ();
    if cancel_clone . load (Ordering::SeqCst) {
      // Checked under the lock, so that 'cancel_search_enrichment'
      // cannot clear the slot between this check and the write.
      tracing::info! ("search enrichment: cancelled");
      return; }
    tracing::info! ("search enrichment: writing payload to slot");
    *guard = Some ( SearchEnrichmentPayload {
      runtime,
      terms          : terms_clone,
      search_results : ids_clone,
      include_overPrivateText_telescopes } ); } ); }

/// The search buffer's enrichment ('docs/api-and-formats.org', "Text
/// search"): the refresh, in content-view mode under 'restriction', of
/// a stub made of the search buffer. Each hit (a root standing for one
/// of 'search_results') is a write-protected occurrence asking for its
/// containers folder, and holds the folder of the nodes that override
/// it and the folder of the nodes it overrides, each only when it
/// would have members ('stub_root_of_hit'); the refresh fills the
/// folders, each member with its containerward chain. What the refresh
/// drew under a hit is then folded, so that the hit shows collapsed,
/// and unless the search included them, the occurrences of
/// overPrivateText nodes are entombstoned. Returns the buffer's text.
pub fn enrich_search_viewforest (
  viewforest         : &mut ViewForest,
  viewnode_id_record : &mut ViewnodeIdRecord,
  graph              : &InRustGraph,
  config             : &SkgConfig,
  search_results     : &[ID],
  restriction        : &SkgrepoRestriction,
  include_overPrivateText_telescopes : bool,
) -> Result<String, Box<dyn Error>> {
  let hits : Vec<NodeId> =
    viewforest . roots ()
    . filter ( |root| matches! ( &root . value () . kind,
                 ViewnodeKind::Vognode (Vognode::Occurrence (occurrence))
                   if search_results . contains (&occurrence . gnid) ))
    . map ( |root| root . id () )
    . collect ();
  let drawn_before : HashSet<NodeId> =
    viewforest . nodes () . map ( |node| node . id () ) . collect ();
  for hit in &hits {
    stub_root_of_hit (viewforest, *hit, graph, restriction); }
  { let truth : Truth = Truth { graph, config, restriction : Some (restriction) };
    refresh_viewforest (
      viewforest, viewnode_id_record, &truth, CompletionMode::ContentView,
      config . initial_node_limit, None ) ?; }
  for hit in &hits {
    let drawn : Vec<NodeId> =
      viewforest . get (*hit) . unwrap () . children ()
      . map ( |child| child . id () )
      . filter ( |child| ! drawn_before . contains (child) )
      . collect ();
    for child in drawn { // In the clients' encoding, folding a hit's children collapses it.
      viewforest . get_mut (child) . unwrap () . value () . foldAndFocus . folded = true; }}
  if ! include_overPrivateText_telescopes {
    exclude_overPrivateText_nodes_from_viewforest (viewforest, graph); }
  viewforest_to_string (viewforest, config) }

/// Make the hit at 'hit' a root of the enrichment's stub: a
/// write-protected occurrence asking for its containers folder,
/// holding first an empty folder of the nodes that override it and
/// then one of the nodes it overrides, each only when the restriction
/// lets it show some.
fn stub_root_of_hit (
  viewforest  : &mut ViewForest,
  hit         : NodeId,
  graph       : &InRustGraph,
  restriction : &SkgrepoRestriction,
) {
  let gnid : ID = {
    let mut node : NodeMut<Viewnode> = viewforest . get_mut (hit) . unwrap ();
    let ViewnodeKind::Vognode (Vognode::Occurrence (occurrence)) =
      &mut node . value () . kind
      else { return; };
    occurrence . write_protect ();
    occurrence . view_requests . insert (
      ViewRequest::Folder (FolderRelation::Containers) );
    occurrence . gnid . clone () };
  for folder in [ Folder::Overridden, Folder::Overrider ] {
    // Prepended last-first, so that they stand in the order named.
    if ! override_relatives (graph, &gnid, &folder, restriction) . is_empty () {
      viewforest . get_mut (hit) . unwrap ()
        . prepend ( viewnode_of ( ViewnodeKind::Folder (folder) )); }}}

/// The IDs of the hit's override relatives that a folder of 'kind'
/// shows: the nodes that override it (the overrider folder) or that it
/// overrides (the overridden folder), less those whose override
/// relationship or own skgrepo the restriction hides.
fn override_relatives (
  graph       : &InRustGraph,
  hit         : &ID,
  kind        : &Folder,
  restriction : &SkgrepoRestriction,
) -> Vec<ID> {
  let relatives : Vec<ID> = match kind {
    Folder::Overridden =>
      graph . outbound_pids_for_relation_gated (
        hit, NodeRelation::Overrides, Some (restriction) ),
    _ =>
      graph . inbound_pids_for_relation_gated (
        hit, NodeRelation::Overrides, Some (restriction) ), };
  relatives . into_iter ()
    . filter ( |relative| graph . nodes . get (relative)
      . is_some_and ( |node| restriction . contains_skgrepo (&node . home_skgrepo) ))
    . collect () }

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
  sexp_to_client_string ( &Sexp::List (fields) ) }

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
pub fn group_matches_by_gnid (
  best_matches  : Vec < (f32, tantivy::DocAddress) >,
  searcher      : Searcher,
  tantivy_index : &TantivyIndex,
  search_terms  : &str,
  search_opts   : &SearchOptions,
  restriction   : Option<&SkgrepoRestriction>,
) -> MatchGroups {
  let matcher : CoverageMatcher = // pre-build once
    build_coverage_matcher (search_terms, search_opts);
  let mut result_acc : MatchGroups =
    HashMap::new();
  for (score, doc_address) in best_matches {
    match searcher . doc (doc_address) {
      Ok (retrieved_doc) => {
        let retrieved_doc : TantivyDocument = retrieved_doc;
        let gnid_opt : Option < ID > =
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
        let skgrepo : SkgrepoName =
          SkgrepoName::from (
            retrieved_doc
              . get_first ( tantivy_index . skgrepo_field )
              . and_then ( |v| v . as_str () )
              . unwrap_or ("") );
        if let Some (a) = restriction {
          // Per-document skgrepo filtering, before grouping: an
          // alias document carries the alias's relRepo as
          // its skgrepo, so a restricted search must drop it here
          // -- a private alias of a public node must neither match
          // nor shift ranking ('docs/api-and-formats.org', "Text search"). The
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
        if let (Some (gnid), Some (title)) = (gnid_opt, title_opt) {
          result_acc
            . entry (gnid)
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
/// The result ids to drop from the top level because an owned
/// result recursively overrides them; those it overrides directly
/// reappear in its overridden folder (the API doc's "Text search"). A
/// foreign overrider never suppresses -- so a pure-foreign mutual
/// override shows both, and a "boring" foreign overrider does not hide
/// the node it overrides. Reachability follows relRepo-visible
/// outbound overrides.
/// Only search hits ('matches_by_id' keys) are ever suppressed. A
/// owned overrider that matched the query but ranks past the
/// display limit could suppress its target without itself being shown
/// (rare; suppression frees slots, so the anchor usually fits).
pub fn suppressed_result_gnids (
  matches_by_gnid : &MatchGroups,
  graph            : &InRustGraph,
  config           : &SkgConfig,
  restriction      : &SkgrepoRestriction,
) -> HashSet<ID> {
  let candidates : HashSet<ID> =
    matches_by_gnid . keys () . cloned () . collect ();
  let mut suppressed : HashSet<ID> = HashSet::new ();
  for owned in candidates . iter () . filter ( |gnid|
    graph . nodes . get (*gnid)
      . map_or ( false, |n| config . skgrepo_is_owned (&n . home_skgrepo) ) )
  { // Walk owned's overriddenward closure; any hit in it is suppressed
    // (it will hang under 'owned'). Cycle-guarded: foreign relationships in the
    // chain can form cycles even though owned ones cannot.
    let mut stack : Vec<ID> = vec![ owned . clone () ];
    let mut seen  : HashSet<ID> = HashSet::from ([ owned . clone () ]);
    while let Some (cur) = stack . pop () {
      for target in graph . outbound_pids_for_relation_gated (
        &cur, NodeRelation::Overrides, Some (restriction) ) {
        if ! seen . insert (target . clone ()) { continue; }
        if candidates . contains (&target) {
          suppressed . insert (target . clone ()); }
        stack . push (target); }} }
  suppressed }

pub fn build_search_viewforest (
  _search_terms    : &str,
  matches_by_gnid : &MatchGroups,
  suppressed       : &HashSet<ID>,
) -> (ViewForest, Vec<ID>) {
  let mut viewforest : ViewForest =
    ViewForest::new ();
  let mut id_entries : Vec < ( &ID,
                               &SkgrepoName,
                               &Vec < ( f32, String ) > ) > =
    matches_by_gnid . iter ()
    . map ( |(gnid, (skgrepo, matches))| // flatten
             (gnid, skgrepo, matches) )
    . collect ();
  id_entries . sort_by ( |a, b| { // sort by best score (descending)
    let score_a : f32 =
      a . 2 . first () . map ( |(s, _)| *s ) . unwrap_or (0.0);
    let score_b : f32 =
      b . 2 . first () . map ( |(s, _)| *s ) . unwrap_or (0.0);
    score_b . partial_cmp (& score_a)
    . unwrap_or (std::cmp::Ordering::Equal) } );
  let mut search_results : Vec < ID > = Vec::new ();
  for (gnid, skgrepo, matches) in id_entries . iter ()
        // Suppress before truncation, so a dropped result frees a slot
        // for the next-ranked hit (design corner O2).
        . filter ( |entry| ! suppressed . contains (entry . 0) )
        . take (SEARCH_DISPLAY_LIMIT)
    { search_results . push ( (*gnid) . clone () );
      let mut sorted_matches : Vec < &(f32, String) > =
        // We borrow from matches_by_id.
        // Sort matches by score descending for display.
        matches . iter () . collect ();
      sorted_matches . sort_by ( |a, b|
        b . 0 . partial_cmp (&a . 0) . unwrap () );
      let (_score, title) : &(f32, String) = sorted_matches [0];
      let result_treeid : NodeId =
        viewforest . append_root ( mk_writeProtected_viewnode (
          (*gnid) . clone (),
          (*skgrepo) . clone (),
          title . clone (),
          true ) );
      if sorted_matches . len () > 1 {
        // We bury all but the best match in an AliasFolder.
        // PITFALL: The title might not be the best match,
        // in which case this makes it look like an alias.
        let aliasfolder_gnid : NodeId = {
          let mut result_mut : NodeMut<Viewnode> =
            viewforest . get_mut (result_treeid) . unwrap ();
          result_mut . append ( Viewnode {
            foldAndFocus : FoldAndFocus { folded : true, .. FoldAndFocus::default () },
            .. viewnode_of ( ViewnodeKind::Folder (Folder::Alias) ) } )
          . id () };
        for (_score, title) in sorted_matches . iter () . skip (1) {
          let mut aliasfolder_mut : NodeMut<Viewnode> =
            viewforest . get_mut (aliasfolder_gnid) . unwrap ();
          aliasfolder_mut . append ( viewnode_of (
            ViewnodeKind::PropertyMember ( PropertyMember::alias (
              title . clone (), None, None ))) ); }} }
  (viewforest, search_results) }
