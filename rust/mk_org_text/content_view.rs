use crate::file_io::read_node;
use crate::util::path_from_pid;
use crate::typedb::search::{
  climb_containerward_and_fetch_rootish_context,
  contains_from_pids,
  count_containers,
  count_contents,
  count_link_sources};
use crate::typedb::util::pid_from_id;
use crate::types::{SkgNode, ID, SkgConfig, OrgNode};
use crate::types::orgnode::default_metadata;
use crate::mk_org_text::util::newline_to_space;
use crate::mk_org_text::orgnode::render_org_node_from_text;

use std::collections::{HashSet, HashMap};
use std::error::Error;
use std::io;
use typedb_driver::TypeDBDriver;

/// Pre-traverse to collect all PIDs that will appear in the view.
/// Mirrors the structure of org_from_node_recursive but only collects IDs.
async fn collect_pids_in_view (
  driver  : &TypeDBDriver,
  config  : &SkgConfig,
  node_id : &ID,
  visited : &mut HashSet<ID>,
  pids    : &mut Vec<ID>,
) -> Result<(), Box<dyn Error>> {

  let pid : ID =
    pid_from_id ( & config . db_name,
                    driver,
                    node_id,
  ). await ?
    . ok_or_else ( || format! (
      "ID '{}' not found in database", node_id ) ) ?;
  pids . push ( pid . clone () );
  if visited . contains ( node_id ) {
    return Ok (()); }
  visited . insert ( node_id . clone () );
  let path : String = path_from_pid (
    &config, pid );
  let node : SkgNode = read_node ( path )?;
  for child_id in &node.contains {
    Box::pin (
      collect_pids_in_view (
        driver, config,
        child_id, visited, pids
      )) . await ?; }
  Ok (()) }

/// Holds cached relationship data for all nodes in the view.
pub struct RelationshipData {
  num_containers : HashMap < ID, usize >,
  num_contents : HashMap < ID, usize >,
  num_links_in : HashMap < ID, usize >,
  container_to_contents : HashMap < ID, HashSet < ID > >,
  content_to_containers : HashMap < ID, HashSet < ID > >,
}

/// Run four batch queries to fetch all relationship data for the given PIDs.
async fn fetch_relationship_data (
  driver  : &TypeDBDriver,
  db_name : &str,
  pids    : &[ID],
) -> Result < RelationshipData, Box<dyn Error> > {

  let num_containers : HashMap < ID, usize > =
    count_containers ( db_name, driver, pids ) . await ?;
  let num_contents : HashMap < ID, usize > =
    count_contents ( db_name, driver, pids ) . await ?;
  let num_links_in : HashMap < ID, usize > =
    count_link_sources ( db_name, driver, pids ) . await ?;
  let ( container_to_contents, content_to_containers ) =
    contains_from_pids ( db_name, driver, pids ) . await ?;
  Ok ( RelationshipData {
    num_containers,
    num_contents,
    num_links_in,
    container_to_contents,
    content_to_containers,
  }) }

/// Given the id `focus`,
/// identifies its context (`climb_containerward_and_fetch_rootish_context()`),
/// and builds a view from that root,
/// by recursively following the `content` relationship.
pub async fn single_root_view (
  driver  : &TypeDBDriver,
  config  : &SkgConfig,
  focus   : &ID,
) -> Result < String, Box<dyn Error> > {

  let root_id : ID = climb_containerward_and_fetch_rootish_context (
    & config . db_name,
    driver , focus
  ) . await ?;

  // Collect all PIDs in the view
  let mut visited : HashSet<ID> = HashSet::new();
  let mut pids : Vec<ID> = Vec::new();
  collect_pids_in_view (
    driver, config,
    &root_id, &mut visited, &mut pids
  ) . await ?;

  // Fetch relationship data for all PIDs
  let rel_data : RelationshipData =
    fetch_relationship_data (
      driver, & config . db_name, &pids
    ) . await ?;

  // Render with relationship data
  visited . clear ();
  let org : String =
    org_from_node_recursive (
      driver, config,
      &root_id, focus, &mut visited, 1,
      None, &rel_data
    ) . await ?;
  Ok (org) }

/// Just like 'single_root_view',
/// except it builds a forest rather than a tree.
pub async fn multi_root_view (
  driver  : &TypeDBDriver,
  config  : &SkgConfig,
  focii   : &[ID],
) -> Result < String, Box<dyn Error> > {

  // Collect root IDs for all foci
  let mut root_ids : Vec<ID> = Vec::new();
  for focus in focii {
    let root_id : ID =
      climb_containerward_and_fetch_rootish_context (
        & config . db_name,
        driver , focus
      ) . await ?;
    root_ids . push ( root_id ); }

  // Collect all PIDs in all views
  let mut visited_collect : HashSet<ID> = HashSet::new();
  let mut pids : Vec<ID> = Vec::new();
  for root_id in &root_ids {
    collect_pids_in_view (
      driver, config,
      root_id, &mut visited_collect, &mut pids
    ) . await ?; }

  // Fetch relationship data for all PIDs
  let rel_data : RelationshipData =
    fetch_relationship_data (
      driver, & config . db_name, &pids
    ) . await ?;

  // Render each root with relationship data
  let mut result : String = String::new();
  let mut visited_render : HashSet<ID> = HashSet::new();
  for (root_id, focus) in root_ids . iter () . zip ( focii . iter () ) {
    let org : String =
      org_from_node_recursive (
        driver, config,
        root_id, focus, &mut visited_render, 1,
        None, &rel_data
      ) . await ?;
    result . push_str ( & org ); }
  Ok ( result ) }

/// Recursively render a node and its branches into Org.
/// `level` controls the number of leading `*` on the headline.
/// `org_parent_id` is the ID of the org-mode parent (if any).
/// `rel_data` contains cached relationship data for metadata population.
pub async fn org_from_node_recursive (
  driver        : &TypeDBDriver,
  config        : &SkgConfig,
  node_id       : &ID,
  focus         : &ID,
  visited       : &mut HashSet<ID>,
  level         : usize,
  org_parent_id : Option<&ID>,
  rel_data      : &RelationshipData,
) -> Result<String, Box<dyn Error>> {

  let pid : ID =
    pid_from_id ( & config . db_name,
                    driver,
                    node_id,
  ). await ?
    . ok_or_else ( || format! (
      "ID '{}' not found in database", node_id ) ) ?;
  let path : String = path_from_pid (
    &config, pid . clone () );
  let node : SkgNode = read_node ( path )?;
  if node.title.is_empty () {
    return Err ( Box::new ( io::Error::new (
      io::ErrorKind::InvalidData,
      format! ( "SkgNode with ID {} has an empty title",
                 node_id ),
    )) ); }
  if visited.contains (node_id) {
    return Ok ( format_repeated_node (
      node_id, level, & node.title )); }
  visited.insert ( node_id.clone () );
  let orgnode2 : OrgNode =
    OrgNode {
      metadata : {
        let mut md = default_metadata ();
        md.id = Some ( node_id.clone () );
        // Check parent relationships
        if let Some ( parent_id ) = org_parent_id {
          md.parentIsContainer =
            rel_data . content_to_containers
            . get ( & pid )
            . map_or ( false, |containers|
                       containers . contains ( parent_id ));
          md.parentIsContent =
            rel_data . container_to_contents
            . get ( & pid )
            . map_or ( false, |contents|
                       contents . contains ( parent_id )); }
        // Set relationship counts
        md.numContainers = rel_data . num_containers . get ( & pid ) . copied ();
        md.numContents = rel_data . num_contents . get ( & pid ) . copied ();
        md.numLinksIn = rel_data . num_links_in . get ( & pid ) . copied ();
        md },
      title : newline_to_space ( &node.title ),
      // Over-cautious, because the title should contain no newlines, but probably cheap.
      body : node.body.clone (), };
  let mut out : String =
    render_org_node_from_text (
      level,
      &orgnode2
    );
  for child_id in &node.contains { // Recurse at next level.
    let child = Box::pin (
      org_from_node_recursive (
        driver, config,
        child_id, focus, visited, level + 1,
        Some ( node_id ), rel_data
      )) . await? ;
    out.push_str ( &child ); }
  Ok (out) }

/// When a node is encountered again in the same document:
/// Produce a headline with a `repeated` herald and a short body.
/// Do not recurse into children.
pub fn format_repeated_node (
  node_id : &ID,
  level   : usize,
  title   : &String
) -> String {
  let orgnode2 : OrgNode =
    OrgNode {
      metadata : { let mut md = default_metadata ();
                   md.id = Some ( node_id.clone () );
                   md.repeat = true;
                   md },
      title : newline_to_space ( title ),
      body : Some (
        "Repeated, probably above. Edit there, not here."
          . to_string () ), };
  render_org_node_from_text (
    level,
    &orgnode2 ) }
