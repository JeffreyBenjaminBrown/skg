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
use crate::types::env::SkgEnv;
use crate::org_to_text::viewforest_to_string;
use crate::refresh::{Truth, refresh_content_viewforest};
use crate::refresh::buffer_corrections::BufferCorrectionPolicy;
use crate::refresh::prune_childless_tombstones;
use crate::serve::ViewsState;
use crate::serve::handlers::text_release::{
  TextReleaseDecision,
  SearchOverPrivateTextChoice,
  decide as decide_text_release,
  exclude_overPrivateText_nodes_from_viewforest,
  search_challenge_response,
  search_choice_from_request};
use crate::serve::protocol::TcpToClient;
use crate::serve::util::{ send_response_with_length_prefix, tag_text_response};
use crate::types::views_state::{ViewforestId, ViewnodeIdRecord, pids_from_viewforest};
use crate::types::misc::{TantivyIndex, SkgConfig, ID, SkgrepoName};
use crate::skgrepo_sets::{SkgrepoRestriction, search_gnids_for_skgrepo_set_for_test as search_ids_for_skgrepo_set_for_test_impl};
use crate::types::sexp::extract_v_from_kv_pair_in_sexp;
use crate::types::tree::forest::ViewForest;
use crate::types::viewnode::{ Viewnode, ViewnodeKind, mk_writeProtected_viewnode};
use crate::types::viewnode::{Folder, FolderRelation, ViewRequest, Vognode, viewnode_of};
use crate::update_buffer::reconcile::partner_folder::partnerFolder_members_in_graph;

use ego_tree::{NodeId, NodeMut};
use sexp::{Sexp, Atom};
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::net::TcpStream;
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
                  Vec < ( f32,             // score (after multiplier)
                          TitleOrAlias ) >) >;

/// The text a search document matched: the node's title, or one of
/// its aliases. (The search index holds one document per title or
/// alias, its field 'title_or_alias'.)
#[derive(Clone, Debug, PartialEq)]
pub enum TitleOrAlias {
  Title (String),
  Alias (String),
}

impl TitleOrAlias {
  pub fn text (
    &self,
  ) -> &str {
    match self {
      TitleOrAlias::Title (text) | TitleOrAlias::Alias (text) => text, } }

  pub fn is_alias (
    &self,
  ) -> bool {
    matches! ( self, TitleOrAlias::Alias (_) ) }
}

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
  draw_search_buffer (
    &mut viewforest, &mut ViewnodeIdRecord::default (), graph, config,
    &search_results, restriction, false ) }

/// Answers a text search with one 'search-results' message, whose
/// content is the search buffer: the hits, and what the enrichment
/// draws under each ('draw_search_buffer'). The exceptions: a search
/// with no hits, or one that fails, answers with a 'search-results'
/// whose content is a message to the user; and a search that might
/// reveal overPrivateText telescopes, before the user has chosen
/// whether to include them, answers with the overPrivateText
/// challenge instead.
pub fn handle_text_search_request (
  stream           : &mut TcpStream,
  request          : &str,
  env              : &SkgEnv,
  views_state      : &mut ViewsState,
  restriction      : &SkgrepoRestriction,
) {
  let parsed_sexp : Result < Sexp, String > =
    sexp::parse (request)
    . map_err ( |e| format! (
      "Failed to parse S-expression: {}", e ) );
  let sexp : Sexp = match parsed_sexp {
    Ok (s) => s,
    Err (err) => {
      tracing::error! ( "{}", err );
      send_search_results_text (stream, &err);
      return; } };
  let search_terms : Result < String, String > =
    extract_v_from_kv_pair_in_sexp ( &sexp, "terms" );
  let search_choice : Option<SearchOverPrivateTextChoice> =
    match search_choice_from_request (&sexp) {
      Ok (choice) => choice,
      Err (error) => {
        send_search_results_text (stream, &error);
        return; }};
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
            send_search_results_text (
              stream,
              &format! ("Error checking search privacy: {}", error) );
            return; }};
      if ! restriction . is_all ()
         && index_has_overPrivateText
         && search_choice . is_none () {
        send_response_with_length_prefix (
          stream, &search_challenge_response () );
        return; }
      let include_overPrivateText_telescopes : bool =
        restriction . is_all ()
        || search_choice == Some (SearchOverPrivateTextChoice::Include);
      let search_opts : SearchOptions = SearchOptions {
        regex     : bool_key ( &sexp, "regex" ),
        body      : bool_key ( &sexp, "body" ),
        operators : bool_key ( &sexp, "operators" ),
        exclude_overPrivateText_telescope : ! include_overPrivateText_telescopes,
      };
      match search_index ( &runtime . tantivy_index,
                           &search_terms,
                           &search_opts ) {
        Ok (( best_matches, searcher )) => {
          if best_matches . is_empty () {
            send_search_results_text (stream, "No matches found.");
            return; }
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
            send_search_results_text (stream, "No matches found.");
            return; }
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
          if matches! (
            release_decision (
              &search_results, restriction, &runtime . graph,
              include_overPrivateText_telescopes ),
            TextReleaseDecision::Challenge { .. } ) {
            send_response_with_length_prefix (
              stream, &search_challenge_response () );
            return; }
          let drawn : Result<String, Box<dyn Error>> =
            draw_search_buffer (
              &mut viewforest, &mut ViewnodeIdRecord::default (),
              &runtime . graph, &runtime . config,
              &search_results, restriction,
              include_overPrivateText_telescopes );
          let rendered : String = match drawn {
            Ok (rendered) => rendered,
            Err (error) => {
              send_search_results_text (
                stream,
                & format! ("Error drawing the search buffer: {}", error) );
              return; }};
          let drawn_pids : Vec<ID> =
            pids_from_viewforest (&viewforest) . into_iter () . collect ();
          let warnings : Vec<String> =
            match release_decision (
              &drawn_pids, restriction, &runtime . graph,
              include_overPrivateText_telescopes )
            { TextReleaseDecision::AllowWithWarning { warning } =>
                vec! [warning],
              TextReleaseDecision::Challenge { .. } => {
                // The challenge before the query, and the exclusion in
                // 'draw_search_buffer', make this unreachable. Fail
                // closed rather than serialize if a change violates either.
                send_search_results_text (
                  stream,
                  "The search buffer reached the release boundary without approval." );
                return; },
              TextReleaseDecision::Allow => Vec::new (), };
          let viewforest_id : ViewforestId =
            ViewforestId::SearchView ( search_terms . clone () );
          if views_state . open_views . views . contains_key (&viewforest_id) {
            // Replace prior search with the same terms.
            views_state . open_views . unregister_view (&viewforest_id); }
          views_state . open_views . register_view (
            &runtime . graph, viewforest_id, viewforest, &drawn_pids );
          send_response_with_length_prefix (
            stream,
            & mk_search_results_sexp (&rendered, &warnings) ); },
        Err (e) => {
          send_search_results_text (
            stream,
            & format! ("Error querying the search index: {}", e) ); }} },
    Err (err) => {
      let error_msg : String =
        format! (
          "Error extracting search terms: {}", err );
      tracing::error! ( "{}", error_msg ) ;
      send_search_results_text (stream, &error_msg); }} }

/// Sends TEXT, a message to the user, as the 'search-results'.
fn send_search_results_text (
  stream : &mut TcpStream,
  text   : &str,
) {
  send_response_with_length_prefix (
    stream,
    & tag_text_response (TcpToClient::SearchResults, text) ); }

/// Whether the search may release the text of 'pids': each of them
/// approved when the search includes overPrivateText telescopes,
/// none otherwise.
fn release_decision (
  pids        : &[ID],
  restriction : &SkgrepoRestriction,
  graph       : &InRustGraph,
  include_overPrivateText_telescopes : bool,
) -> TextReleaseDecision {
  let approved : HashSet<ID> =
    if include_overPrivateText_telescopes {
      pids . iter () . cloned () . collect ()
    } else { HashSet::new () };
  decide_text_release ("text-search", restriction, pids, graph, &approved) }

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

/// Draw the search buffer ('docs/api-and-formats.org', "Text search"):
/// the refresh, in content-view mode under 'restriction', of a stub
/// made of the hits ('build_search_viewforest'), the same code every
/// view is drawn with. Each hit (a root standing for one of
/// 'search_results') is a write-protected occurrence asking for its
/// containers folder, and holds the folder of the nodes that override
/// it and the folder of the nodes it overrides, each only when it
/// would have members ('stub_root_of_hit'), and, when it was found
/// through an alias, its alias folder; the refresh fills the folders,
/// each member of the containers folder with its containerward chain,
/// and titles each hit from the graph. What the refresh drew under
/// each hit is then folded ('fold_what_was_drawn_under_hit'), and,
/// unless the search included them, the occurrences of
/// overPrivateText nodes are entombstoned, after the refresh, so that
/// the members it drew are covered too, and the childless tombstones
/// pruned. Returns the buffer's text, rendered after all that. (The
/// refresh renders the buffer too, before the fold; that text goes
/// unused.)
pub fn draw_search_buffer (
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
  for hit in &hits {
    stub_root_of_hit (viewforest, *hit, graph, restriction); }
  refresh_content_viewforest (
    viewforest, viewnode_id_record,
    &Truth { graph, config, restriction : Some (restriction) },
    &BufferCorrectionPolicy::default () ) ?;
  for hit in &hits {
    fold_what_was_drawn_under_hit (viewforest, *hit); }
  if ! include_overPrivateText_telescopes {
    exclude_overPrivateText_nodes_from_viewforest (
      viewforest . as_internal_tree_mut (), graph);
    prune_childless_tombstones (viewforest . as_internal_tree_mut ()) ?; }
  viewforest_to_string (viewforest, config) }

/// Make the hit at 'hit' a root of the enrichment's stub: a
/// write-protected occurrence asking for its containers folder,
/// holding, after any alias folder it has, a folder of the nodes that
/// override it and then one of the nodes it overrides, each only when
/// the folder's own goal, the one the refresh fills it from, has
/// members.
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
  for folder in [ Folder::Overrider, Folder::Overridden ] {
    if partnerFolder_members_in_graph (graph, &gnid, &folder, Some (restriction))
       . is_some_and ( |members| ! members . is_empty () ) {
      viewforest . get_mut (hit) . unwrap ()
        . append ( viewnode_of ( ViewnodeKind::Folder (folder) )); }}}

/// Fold what the refresh drew under the hit at 'hit', so that it
/// shows collapsed. (In the clients' encoding, a viewnode marked
/// folded is hidden inside a folded viewparent.) A hit with an alias
/// folder shows its folders instead, the alias folder open, so that
/// the matching alias is seen, and each other folder collapsed.
fn fold_what_was_drawn_under_hit (
  viewforest : &mut ViewForest,
  hit        : NodeId,
) {
  let is_alias_folder : &dyn Fn (&NodeId) -> bool =
    &|treeid : &NodeId| -> bool {
      matches! ( viewforest . get (*treeid) . unwrap () . value () . kind,
                 ViewnodeKind::Folder (Folder::Alias) ) };
  let children : Vec<NodeId> =
    viewforest . get (hit) . unwrap () . children ()
    . map ( |child| child . id () )
    . collect ();
  let to_fold : Vec<NodeId> =
    if children . iter () . any (is_alias_folder) {
      children . iter ()
        . filter ( |child| ! is_alias_folder (child) )
        . flat_map ( |child| viewforest . get (*child) . unwrap () . children ()
                             . map ( |grandchild| grandchild . id () ) )
        . collect ()
    } else { children };
  for treeid in to_fold {
    viewforest . get_mut (treeid) . unwrap () . value () . foldAndFocus . folded = true; }}

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
        let title_opt : Option < TitleOrAlias > = {
          let raw : Option<String> =
            retrieved_doc
              . get_first ( tantivy_index . raw_title_field )
              . and_then ( |v| v . as_str () )
              . map ( |s| s . to_string () )
              . filter ( |s| ! s . is_empty () );
          let is_title : bool =
            retrieved_doc
              . get_first ( tantivy_index . is_title_field )
              . and_then ( |v| v . as_str () )
              == Some ("true");
          raw . or_else ( || retrieved_doc
            . get_first ( tantivy_index . title_or_alias_field )
            . and_then ( |v| v . as_str() )
            . map ( |s| s . to_string() ))
            . map ( |text| if is_title { TitleOrAlias::Title (text) }
                           else { TitleOrAlias::Alias (text) } ) };
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

/// Builds the stub of the search buffer ('draw_search_buffer'): one
/// root per hit, best first, each a write-protected occurrence titled
/// by the best match of its title, or else of an alias (the refresh
/// titles it from the graph). A hit found through an alias holds an
/// empty alias folder, unfolded, which the refresh fills with all of
/// the node's aliases. Returns the viewforest and the ordered list of
/// result IDs.
pub fn build_search_viewforest (
  _search_terms    : &str,
  matches_by_gnid : &MatchGroups,
  suppressed       : &HashSet<ID>,
) -> (ViewForest, Vec<ID>) {
  let mut viewforest : ViewForest =
    ViewForest::new ();
  let mut id_entries : Vec < ( &ID,
                               &SkgrepoName,
                               &Vec < ( f32, TitleOrAlias ) > ) > =
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
      let mut sorted_matches : Vec < &(f32, TitleOrAlias) > =
        // We borrow from matches_by_id.
        // Sort matches by score descending.
        matches . iter () . collect ();
      sorted_matches . sort_by ( |a, b|
        b . 0 . partial_cmp (&a . 0) . unwrap () );
      let title : &str =
        sorted_matches . iter ()
        . find ( |(_score, matched)| ! matched . is_alias () )
        . unwrap_or (&sorted_matches [0])
        . 1 . text ();
      let result_treeid : NodeId =
        viewforest . append_root ( mk_writeProtected_viewnode (
          (*gnid) . clone (),
          (*skgrepo) . clone (),
          title . to_string (),
          true ) );
      if sorted_matches . iter () . any ( |(_score, matched)| matched . is_alias () ) {
        viewforest . get_mut (result_treeid) . unwrap ()
          . append ( viewnode_of ( ViewnodeKind::Folder (Folder::Alias) )); }}
  (viewforest, search_results) }
