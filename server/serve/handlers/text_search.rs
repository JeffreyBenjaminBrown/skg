//! The "text search" request: search Tantivy ('dbs/tantivy/search.rs'),
//! group the hits by node ('group_matches_by_gnid'), rank them (with
//! 'text_search/coverage.rs'), and answer with a search buffer
//! ('draw_search_buffer', 'build_search_viewforest'), or with the
//! overPrivateText challenge when the hits might reveal such text.

mod coverage;

use coverage::{CoverageMatcher, build_coverage_matcher, coverage_factor};

/// PITFALL: Uses two layers of truncation.
/// Tantivy truncates its search after (unimaginable) 1e5 results.
/// The server (outside of Tantivy) then sorts those results
/// to surface roots and other high-value gometries,
/// which are truncated for display.

use crate::consts::SEARCH_DISPLAY_LIMIT;
use crate::prominence::ProminenceSource;
use crate::dbs::tantivy::{
  bool_field_eq_true, displayed_title_of_document, string_field};
use crate::dbs::tantivy::background_writer::wait_for_tantivy_writes_idle;
use crate::dbs::tantivy::search::{
  SearchOptions, has_overPrivateText_telescope, search_index};
use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::in_rust_graph::mirrors::{
  Mirror, Mirrors, first_ranked_mirrors, mirrors_of};
use crate::types::env::SkgEnv;
use crate::org_to_text::viewforest_to_string;
use crate::refresh::{Truth, refresh_content_viewforest};
use crate::types::views_state::Perspective;
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
use crate::types::config::SkgConfig;
use crate::types::misc::{TantivyIndex, ID, SkgrepoName};
use crate::skgrepo_sets::SkgrepoRestriction;
use crate::types::sexp::extract_v_from_kv_pair_in_sexp;
use crate::types::tree::forest::ViewForest;
use crate::types::viewnode::{ Viewnode, ViewnodeKind, mk_writeProtected_viewnode};
use crate::types::viewnode::{Folder, FolderRelation, ViewRequest, Vognode, viewnode_of};
use crate::refresh::goal::partnerFolder_members_in_graph;

use ego_tree::{NodeId, NodeMut};
use sexp::{Sexp, Atom};
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::net::TcpStream;
use tantivy::{TantivyDocument, Searcher};

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

pub fn search_buffer_for_skgrepo_set_for_test (
  graph            : &InRustGraph,
  config           : &SkgConfig,
  matches_by_gnid : &MatchGroups,
  restriction      : &SkgrepoRestriction,
) -> Result<String, Box<dyn Error>> {
  let (mut viewforest, search_results) : (ViewForest, Vec<ID>) =
    build_search_viewforest (matches_by_gnid, &HashSet::new ());
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
  let response : String =
    text_search_response (request, env, views_state, restriction);
  send_response_with_length_prefix (stream, &response); }

/// The one response 'handle_text_search_request' sends. A search
/// that draws its buffer registers it as a view first, replacing a
/// prior search with the same terms.
fn text_search_response (
  request          : &str,
  env              : &SkgEnv,
  views_state      : &mut ViewsState,
  restriction      : &SkgrepoRestriction,
) -> String {
  let sexp : Sexp = match sexp::parse (request) {
    Ok (s) => s,
    Err (e) => {
      let err : String = format! (
        "Failed to parse S-expression: {}", e );
      tracing::error! ( "{}", err );
      return search_results_text (&err); } };
  let search_choice : Option<SearchOverPrivateTextChoice> =
    match search_choice_from_request (&sexp) {
      Ok (choice) => choice,
      Err (error) => return search_results_text (&error), };
  let search_terms : String =
    match extract_v_from_kv_pair_in_sexp ( &sexp, "terms" ) {
      Ok (search_terms) => search_terms,
      Err (err) => {
        let error_msg : String =
          format! (
            "Error extracting search terms: {}", err );
        tracing::error! ( "{}", error_msg ) ;
        return search_results_text (&error_msg); }};
  // Wait for any in-flight background search-index writes to tantivy-commit, so
  // the search reflects every save issued so far (read-your-writes).
  wait_for_tantivy_writes_idle ();
  let runtime = env . runtime_snapshot ();
  let index_has_overPrivateText : bool =
    match has_overPrivateText_telescope (&runtime . tantivy_index) {
      Ok (has_overPrivateText) => has_overPrivateText,
      Err (error) =>
        return search_results_text (
          &format! ("Error checking search privacy: {}", error) ), };
  if ! restriction . is_unrestricted ()
     && index_has_overPrivateText
     && search_choice . is_none () {
    return search_challenge_response (); }
  let include_overPrivateText_telescopes : bool =
    restriction . is_unrestricted ()
    || search_choice == Some (SearchOverPrivateTextChoice::Include);
  let search_opts : SearchOptions = SearchOptions {
    regex     : bool_key ( &sexp, "regex" ),
    body      : bool_key ( &sexp, "body" ),
    operators : bool_key ( &sexp, "operators" ),
    exclude_overPrivateText_telescope : ! include_overPrivateText_telescopes,
  };
  let (best_matches, searcher) : (Vec<(f32, tantivy::DocAddress)>, Searcher) =
    match search_index ( &runtime . tantivy_index,
                         &search_terms,
                         &search_opts ) {
      Ok (found) => found,
      Err (e) =>
        return search_results_text (
          & format! ("Error querying the search index: {}", e) ), };
  if best_matches . is_empty () {
    return search_results_text ("No matches found."); }
  let matches_by_gnid : MatchGroups =
    filter_match_groups_to_unrestricted_skgrepos (
      group_matches_by_gnid (
      best_matches,
      searcher,
      &runtime . tantivy_index,
      &search_terms,
      &search_opts,
      restriction ),
      restriction );
  if matches_by_gnid . is_empty () {
    return search_results_text ("No matches found."); }
  let suppressed : HashSet<ID> =
    suppressed_result_gnids (
      &matches_by_gnid,
      &runtime . graph,
      &runtime . config,
      restriction );
  let (mut viewforest, search_results) : (ViewForest, Vec<ID>) =
    build_search_viewforest (
      &matches_by_gnid,
      &suppressed );
  if matches! (
    release_decision (
      &search_results, restriction, &runtime . graph,
      include_overPrivateText_telescopes ),
    TextReleaseDecision::Challenge { .. } ) {
    return search_challenge_response (); }
  let rendered : String =
    match draw_search_buffer (
      &mut viewforest, &mut ViewnodeIdRecord::default (),
      &runtime . graph, &runtime . config,
      &search_results, restriction,
      include_overPrivateText_telescopes ) {
      Ok (rendered) => rendered,
      Err (error) =>
        return search_results_text (
          & format! ("Error drawing the search buffer: {}", error) ), };
  let drawn_pids : Vec<ID> =
    pids_from_viewforest (&viewforest) . into_iter () . collect ();
  let warnings : Vec<String> =
    match release_decision (
      &drawn_pids, restriction, &runtime . graph,
      include_overPrivateText_telescopes )
    { TextReleaseDecision::AllowWithWarning { warning } =>
        vec! [warning],
      TextReleaseDecision::Challenge { .. } =>
        // The challenge before the query, and the exclusion in
        // 'draw_search_buffer', make this unreachable. Fail
        // closed rather than serialize if a change violates either.
        return search_results_text (
          "The search buffer reached the release boundary without approval." ),
      TextReleaseDecision::Allow => Vec::new (), };
  let viewforest_id : ViewforestId =
    ViewforestId::SearchView ( search_terms . clone () );
  if views_state . open_views . views . contains_key (&viewforest_id) {
    // Replace prior search with the same terms.
    views_state . open_views . unregister_view (&viewforest_id); }
  views_state . open_views . register_view (
    &runtime . graph, viewforest_id, viewforest, &drawn_pids, restriction );
  mk_search_results_sexp ( &rendered, &warnings ) }

/// TEXT, a message to the user, as the 'search-results'.
fn search_results_text (
  text   : &str,
) -> String {
  tag_text_response (TcpToClient::SearchResults, text) }

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
  matches_by_gnid . into_iter ()
    . filter ( |(_, (skgrepo, _))|
      ! SkgrepoRestriction::node_is_restricted (restriction, skgrepo) )
    . collect () }

/// Draw the search buffer ('docs/api-and-formats.org', "Text search"):
/// the refresh, in content-view mode under 'restriction', of a stub
/// made of the hits ('build_search_viewforest'), the same code every
/// view is drawn with. Each hit (a root standing for one of
/// 'search_results') is a write-protected occurrence asking for its
/// containers folder, and holds its forks folder (every mirror of it,
/// flat, as every forks folder is: so each hit suppressed in its
/// favor stands there) when that folder would have members
/// ('stub_root_of_hit'), and, when it was found through an alias,
/// its alias folder; the refresh fills the folders, each member of the
/// containers folder with its containerward chain, and titles each hit
/// from the graph. What the refresh drew under each hit is then folded
/// ('fold_what_was_drawn_under_hit'), and,
/// unless the search included them, the occurrences of
/// overPrivateText nodes are entombstoned, after the refresh, so that
/// the members it drew are covered too, and the childless tombstones
/// pruned once more ('prune_childless_tombstones', as the refresh's
/// tail prunes). Returns the buffer's text, rendered after all that.
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
    &Truth { graph, config, restriction },
    Perspective::RootHomes ) ?;
  for hit in &hits {
    fold_what_was_drawn_under_hit (viewforest, *hit); }
  if ! include_overPrivateText_telescopes {
    exclude_overPrivateText_nodes_from_viewforest (
      viewforest . as_internal_tree_mut (), graph);
    prune_childless_tombstones (viewforest . as_internal_tree_mut ()) ?; }
  viewforest_to_string (viewforest) }

/// Make the hit at 'hit' a root of the search's stub: a
/// write-protected occurrence asking for its containers folder,
/// holding, after any alias folder it has, its forks folder, only
/// when the folder's own goal, the one the refresh fills it from, has
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
  if partnerFolder_members_in_graph (graph, &gnid, &Folder::Forks, restriction)
     . is_some_and ( |members| ! members . is_empty () ) {
    viewforest . get_mut (hit) . unwrap ()
      . append ( viewnode_of ( ViewnodeKind::Folder (Folder::Forks) )); }}

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
  content     : &str,
  warnings    : &[String],
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
  restriction   : &SkgrepoRestriction,
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
          string_field ( &retrieved_doc, tantivy_index . id_field )
            . map (ID::from);
        // Prefer raw_title (see 'displayed_title_of_document'), so a
        // link in the title shows as `[[id:X][label]]` in search
        // results; an alias-doc hit shows its alias.
        let title_opt : Option < TitleOrAlias > = {
          let is_title : bool =
            bool_field_eq_true ( &retrieved_doc, tantivy_index . is_title_field );
          displayed_title_of_document ( tantivy_index, &retrieved_doc )
            . map ( |text| if is_title { TitleOrAlias::Title (text) }
                           else { TitleOrAlias::Alias (text) } ) };
        // Read title_or_alias separately for coverage counting --
        // it's the field the search index actually matched against.
        let searchable_title : String =
          string_field ( &retrieved_doc, tantivy_index . title_or_alias_field )
            . unwrap_or_default ();
        let skgrepo : SkgrepoName =
          SkgrepoName::from (
            string_field ( &retrieved_doc, tantivy_index . skgrepo_field )
              . unwrap_or_default () . as_str () );
        { // Per-document skgrepo filtering, before grouping: an
          // alias document carries the alias's relRepo as
          // its skgrepo, so a restricted search must drop it here
          // -- a private alias of a public node must neither match
          // nor shift ranking ('docs/api-and-formats.org', "Text search"). The
          // group-level filter below survives as a backstop.
          if ! SkgrepoRestriction::record_is_visible (restriction, &skgrepo) {
            continue; }}
        let origin_type : Option < ProminenceSource > =
          string_field ( &retrieved_doc, tantivy_index . prominence_source_field )
            . and_then ( |label| ProminenceSource::from_label (&label) );
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

/// The result ids to drop from the top level because another hit,
/// owned, is a mirror of them ('server/dbs/in_rust_graph/mirrors.rs')
/// that the user's own skgrepo list ranks first. Among the hits that
/// are mirrors of one another, under the skgrepo restriction, the one
/// that list ranks first stands ('hit_standing_among', counting only
/// the hits); if it is owned, every other hit among them is
/// suppressed. So among owned hits the most public stands. A foreign hit
/// never suppresses -- so a pure-foreign mutual pair shows both, and a
/// "boring" foreign fork does not hide the node it forks. A suppressed
/// hit reappears in its suppressor's forks folder, which holds every
/// mirror (the API doc's "Text search").
/// Only search hits ('matches_by_id' keys) are ever suppressed. An
/// owned hit that matched the query but ranks past the
/// display limit could suppress another without itself being shown
/// (rare; suppression frees slots, so the anchor usually fits).
pub fn suppressed_result_gnids (
  matches_by_gnid : &MatchGroups,
  graph            : &InRustGraph,
  config           : &SkgConfig,
  restriction      : &SkgrepoRestriction,
) -> HashSet<ID> {
  let mut hits : Vec<ID> =
    matches_by_gnid . keys () . cloned () . collect ();
  hits . sort ();
  let mut grouped : HashSet<ID> = HashSet::new ();
  let mut suppressed : HashSet<ID> = HashSet::new ();
  for hit in &hits {
    if grouped . contains (hit) { continue; }
    let hits_among_mirrors : Mirrors =
      hits_among_mirrors_of (graph, restriction, hit, matches_by_gnid);
    grouped . extend ( hits_among_mirrors . pids () );
    let Some (standing) : Option<ID> = hit_standing_among (config, &hits_among_mirrors)
      else { continue; };
    let standing_is_owned : bool = graph . nodes . get (&standing)
      . is_some_and ( |node| config . skgrepo_is_owned (&node . home_skgrepo) );
    if standing_is_owned {
      suppressed . extend ( hits_among_mirrors . pids () . into_iter ()
                            . filter ( |pid| pid != &standing ) ); }}
  suppressed }

/// The mirrors of 'hit' that are hits.
fn hits_among_mirrors_of (
  graph           : &InRustGraph,
  restriction     : &SkgrepoRestriction,
  hit             : &ID,
  matches_by_gnid : &MatchGroups,
) -> Mirrors {
  let members : Vec<Mirror> = mirrors_of (graph, restriction, hit, None)
    . members . into_iter ()
    . filter ( |mirror| matches_by_gnid . contains_key (&mirror . pid) )
    . collect ();
  Mirrors { members } }

/// Which of 'hits', hits that are mirrors of one another, the user's
/// own skgrepo list ranks first: the perspective of the config's
/// first owned skgrepo in declaration order (the most public;
/// 'SkgConfig::first_owned_else_first_declared_skgrepo'), then the others in
/// declaration order. Several there tie, and the least pid stands.
fn hit_standing_among (
  config : &SkgConfig,
  hits   : &Mirrors,
) -> Option<ID> {
  first_ranked_mirrors (
    config, &config . first_owned_else_first_declared_skgrepo (), hits )
    . into_iter () . map ( |mirror| mirror . pid ) . min () }

/// Builds the stub of the search buffer ('draw_search_buffer'): one
/// root per hit, best first, each a write-protected occurrence titled
/// by the best match of its title, or else of an alias (the refresh
/// titles it from the graph). A hit found through an alias holds an
/// empty alias folder, unfolded, which the refresh fills with all of
/// the node's aliases. Returns the viewforest and the ordered list of
/// result IDs.
pub fn build_search_viewforest (
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
