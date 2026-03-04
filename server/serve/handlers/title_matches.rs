use crate::dbs::filesystem::one_node::skgnode_from_pid_and_source;
use crate::dbs::tantivy::search_index;
use crate::dbs::typedb::search::all_graphnodestats::{ AllGraphNodeStats, fetch_all_graphnodestats, graphnodestats_for_pid, };
use crate::dbs::typedb::search::containerward_path_stats_bulk;
use crate::org_to_text::viewnode_to_text;
use crate::serve::util::send_response;
use crate::types::misc::{TantivyIndex, ID, SourceName, SkgConfig};
use crate::types::sexp::extract_v_from_kv_pair_in_sexp;
use crate::types::skgnode::SkgNode;
use crate::types::viewnode::{ViewNode, ViewNodeKind, TrueNode, GraphNodeStats, ContainerwardPathStats, default_truenode};

use futures::executor::block_on;
use sexp::Sexp;
use std::collections::HashMap;
use std::error::Error;
use std::net::TcpStream;
use tantivy::{Document, Searcher};
use typedb_driver::TypeDBDriver;

pub type MatchGroups =
  HashMap < String,              // ID
            Vec < ( f32,         // score
                    String ) >>; // title or alias

/// Extracts search terms from request,
/// finds matching titles,
/// and sends a corresponding document to Emacs.
pub fn handle_title_matches_request (
  stream        : &mut TcpStream,
  request       : &str,
  tantivy_index : &TantivyIndex,
  driver        : &TypeDBDriver,
  config        : &SkgConfig,
) {
  match search_terms_from_request (request) {
    Ok (search_terms) => {
      send_response (
        stream,
        & generate_title_matches_response (
          &search_terms,
          tantivy_index,
          driver,
          config )); },
    Err (err) => {
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
fn generate_title_matches_response (
  search_terms  : &str,
  tantivy_index : &TantivyIndex,
  driver        : &TypeDBDriver,
  config        : &SkgConfig,
) -> String {
  match search_index ( tantivy_index,
                       search_terms ) {
    Ok (( best_matches, searcher )) => {
      if best_matches . is_empty () {
        "No matches found." . to_string ()
      } else {
        let ( matches_by_id, source_by_id )
          : ( MatchGroups, HashMap < String, String > ) =
          group_matches_by_id (
            best_matches,
            searcher,
            tantivy_index );
        match build_graphnodestats_for_ids (
          & matches_by_id,
          & source_by_id,
          driver,
          config )
        { Ok (stats_by_id) =>
            format_matches_as_org_mode (
              search_terms,
              matches_by_id,
              & stats_by_id ),
          Err (e) =>
            format! ("Error fetching graph stats: {}", e) } }},
    Err (e) => {
      format!("Error searching index: {}", e) }} }

pub fn group_matches_by_id (
  best_matches  : Vec < (f32, tantivy::DocAddress) >,
  searcher      : Searcher,
  tantivy_index : &TantivyIndex,
) -> ( MatchGroups, HashMap < String, String > ) {
  let mut result_acc : MatchGroups =
    HashMap::new();
  let mut source_acc : HashMap < String, String > =
    HashMap::new ();
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
        let source_opt : Option < String > =
          retrieved_doc
            . get_first ( tantivy_index . source_field )
            . and_then ( |v| v . as_text() )
            . map ( |s| s . to_string() );
        if let (Some (id), Some (title)) = (id_opt, title_opt) {
          if let Some (source) = source_opt {
            source_acc . entry (id . clone ())
              . or_insert (source); }
          result_acc
            . entry (id)
            . or_insert_with (Vec::new)
            . push (( score, title )); }},
      Err (e) => { eprintln! (
        "Error retrieving document: {}", e ); }} }
  ( result_acc, source_acc ) }

/// Fetch graphnodestats from TypeDB and disk for all matched IDs.
fn build_graphnodestats_for_ids (
  matches_by_id : &MatchGroups,
  source_by_id  : &HashMap < String, String >,
  driver        : &TypeDBDriver,
  config        : &SkgConfig,
) -> Result < HashMap < String, GraphNodeStats >,
             Box < dyn std::error::Error > > {
  let pids : Vec < ID > =
    matches_by_id . keys ()
    . map ( |id| ID::from ( id . clone () ))
    . collect ();
  // TypeDB stats: numContainers, numContents, numLinksIn,
  // overriding, subscribing.
  // Also compute containerward path stats in parallel.
  let ( typedb_stats, path_stats_map )
    : ( Result < AllGraphNodeStats, Box < dyn Error > >,
        Result < HashMap < ID, ContainerwardPathStats >,
                 Box < dyn Error > > )
    = block_on ( async {
      let typedb_future =
        fetch_all_graphnodestats (
          & config . db_name, driver, & pids );
      let path_future =
        containerward_path_stats_bulk (
          & config . db_name, driver, & pids );
      futures::join! ( typedb_future, path_future ) });
  let ts : AllGraphNodeStats = typedb_stats ?;
  let mut result : HashMap < String, GraphNodeStats > =
    HashMap::new ();
  for id_str in matches_by_id . keys () {
    let pid : ID = ID::from ( id_str . clone () );
    let skgnode_opt : Option < SkgNode > =
      source_by_id . get (id_str)
      . and_then ( |src|
        skgnode_from_pid_and_source (
          config,
          pid . clone (),
          & SourceName::from ( src . clone () )
        ) . ok () );
    let mut stats : GraphNodeStats =
      graphnodestats_for_pid (
        &pid, &ts, skgnode_opt . as_ref () );
    if let Ok ( ref pm ) = path_stats_map {
      if let Some (cp) = pm . get (&pid) {
        stats . containerwardPath = Some ( cp . clone () ); } }
    result . insert ( id_str . clone (), stats ); }
  Ok (result) }

/// Formats grouped matches as an org-mode document.
/// Sorts IDs by best score, and matches within each ID by score.
pub fn format_matches_as_org_mode (
  search_terms  : &str,
  matches_by_id : MatchGroups,
  stats_by_id   : &HashMap < String, GraphNodeStats >,
) -> String {
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
    // Sort IDs by each one's best (and now first) match score.
    { let score_b : f32 =
        b . 1 . first() . map( |(s, _)| *s) . unwrap_or (0.0);
      score_b
    } . partial_cmp (
      & { let score_a : f32 =
            a . 1 . first() . map( |(s, _)| *s) . unwrap_or (0.0);
          score_a }
    ) . unwrap () } );
  for (id, matches) in id_entries {
    let graph_stats : GraphNodeStats =
      stats_by_id . get (&id)
      . cloned ()
      . unwrap_or_default ();
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
              graphStats : graph_stats . clone (),
              .. default_truenode (
                ID::from(id . clone()),
                SourceName::from ("search"),
                format! ( "score: {:.2}, [[id:{}][{}]]",
                          score, id, title )) } ), } )
      . expect ("TrueNode rendering never fails"));
    for (score, title) in matches . iter() . skip (1) {
      // The rest, if any, become level-3 headlines.
      result . push_str (
        & viewnode_to_text (
          3,
          & ViewNode {
            focused : false,
            folded  : false,
            kind    : ViewNodeKind::True (
              TrueNode {
                indefinitive : true,
                graphStats : graph_stats . clone (),
                .. default_truenode (
                  ID::from(id . clone()),
                  SourceName::from ("search"),
                  format! ( "score: {:.2}, [[id:{}][{}]]",
                            score, id, title )) } ), } )
        . expect ("TrueNode rendering never fails")); }}
  result }

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
