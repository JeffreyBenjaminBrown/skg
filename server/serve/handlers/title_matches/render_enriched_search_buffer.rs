/// TODO : This module could be simpler, by reusing the existing code
/// for executing a containerward view request from a saved buffer.

use crate::dbs::tantivy::title_by_id;
use crate::dbs::typedb::paths::{
  paths_to_first_nonlinearities,
  PathToFirstNonlinearity};
use crate::org_to_text::viewnode_to_text;
use crate::types::misc::{TantivyIndex, SkgConfig, ID, SourceName};
use crate::types::viewnode::mk_indefinitive_viewnode;

use std::collections::HashMap;
use typedb_driver::TypeDBDriver;

/// Append containerward path headlines after each search result
/// in an already-formatted org-mode string.
/// Each result's level-2 headline (and its AliasCol children, if any)
/// is followed by the path nodes as level-3, level-4, ... headlines.
pub(super) fn enrich_with_containerward_paths (
  base_org      : &str,
  result_ids    : &[ID],
  paths_by_id   : &HashMap < ID, Vec < PathToFirstNonlinearity > >,
  tantivy_index : &TantivyIndex,
) -> String {
  // Strategy: split the base org into lines. Walk through
  // the result IDs in order; for each one, find its level-2
  // headline block (from its "** " line to just before the next
  // "** " line or end), copy it, then append paths.
  let lines : Vec < &str > =
    base_org . lines () . collect ();
  let mut enriched : String = String::new ();
  // Find the indices where each level-2 headline block starts.
  let mut level2_starts : Vec < usize > = Vec::new ();
  for (i, line) in lines . iter () . enumerate () {
    if line . starts_with ("** ") {
      level2_starts . push (i); } }
  // Emit lines before the first level-2 headline (the level-1 root).
  let first_l2 : usize =
    * level2_starts . first () . unwrap_or ( &lines . len () );
  for line in &lines [ ..first_l2 ] {
    enriched . push_str (line);
    enriched . push ('\n'); }
  // For each level-2 block, emit it then append paths.
  for (block_idx, &start) in level2_starts . iter () . enumerate () {
    let end : usize =
      if block_idx + 1 < level2_starts . len ()
      { level2_starts [ block_idx + 1 ] }
      else { lines . len () };
    for line in &lines [ start..end ] {
      enriched . push_str (line);
      enriched . push ('\n'); }
    if block_idx < result_ids . len () {
      let id : &ID = &result_ids [ block_idx ];
      if let Some (paths) = paths_by_id . get (id) {
        render_containerward_paths (
          &mut enriched, 3, paths, tantivy_index ); } } }
  enriched }

/// Compute containerward paths for a list of search result IDs.
/// Returns a map from each ID to its paths.
pub(super) fn compute_paths_for_search_results (
  ids           : &[ID],
  typedb_driver : &TypeDBDriver,
  config        : &SkgConfig,
) -> HashMap < ID, Vec < PathToFirstNonlinearity > > {
  let mut result : HashMap < ID, Vec < PathToFirstNonlinearity > > =
    HashMap::new ();
  for id in ids {
    match futures::executor::block_on (
      paths_to_first_nonlinearities (
        &config . db_name, typedb_driver, id,
        "contains", "contained", "container" ) ) {
      Ok (paths) => {
        result . insert ( id . clone (), paths ); },
      Err (e) => {
        eprintln! (
          "Error computing containerward paths for {}: {}",
          id . as_str (), e ); } } }
  result }

/// Render containerward paths as nested org-mode headlines
/// under a search result.
fn render_containerward_paths (
  result        : &mut String,
  base_level    : usize,
  paths         : &[PathToFirstNonlinearity],
  tantivy_index : &TantivyIndex,
) {
  for path in paths {
    let mut level : usize = base_level;
    for path_node_id in &path . path {
      let title : String =
        title_by_id ( tantivy_index, path_node_id )
        . unwrap_or_else ( || path_node_id . as_str () . to_string () );
      result . push_str (
        & viewnode_to_text (
          level,
          & mk_indefinitive_viewnode (
            path_node_id . clone (),
            SourceName::from ("search"),
            title,
            true ) )
        . expect ("TrueNode rendering never fails"));
      level += 1; }
    if ! path . branches . is_empty () {
      let mut sorted_branches : Vec < &ID > =
        path . branches . iter () . collect ();
      sorted_branches . sort ();
      for branch_id in sorted_branches {
        let title : String =
          title_by_id ( tantivy_index, branch_id )
          . unwrap_or_else ( || branch_id . as_str () . to_string () );
        result . push_str (
          & viewnode_to_text (
            level,
            & mk_indefinitive_viewnode (
              branch_id . clone (),
              SourceName::from ("search"),
              title,
              true ) )
          . expect ("TrueNode rendering never fails")); } }
    if ! path . cycle_nodes . is_empty ()
      && path . branches . is_empty () {
      let mut sorted_cycles : Vec < &ID > =
        path . cycle_nodes . iter () . collect ();
      sorted_cycles . sort ();
      for cycle_id in sorted_cycles {
        let title : String =
          title_by_id ( tantivy_index, cycle_id )
          . unwrap_or_else ( || cycle_id . as_str () . to_string () );
        result . push_str (
          & viewnode_to_text (
            level,
            & mk_indefinitive_viewnode (
              cycle_id . clone (),
              SourceName::from ("search"),
              title,
              true ) )
          . expect ("TrueNode rendering never fails")); } } } }
