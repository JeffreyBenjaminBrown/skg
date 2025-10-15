use crate::file_io::read_node;
use crate::util::path_from_pid;
use crate::typedb::search::{
  climb_containerward_and_fetch_rootish_context,
  contains_from_pids,
  count_containers,
  count_contents,
  count_link_sources};
use crate::typedb::util::pid_from_id;
use crate::types::{SkgNode, ID, SkgConfig, OrgNode, OrgnodeMetadata};
use crate::types::orgnode::default_metadata;
use crate::mk_org_text::util::newline_to_space;
use crate::mk_org_text::orgnode::render_org_node_from_text;

use std::collections::{HashSet, HashMap};
use std::error::Error;
use std::io;
use typedb_driver::TypeDBDriver;

/// Given the id `focus`,
/// identifies its context (`climb_containerward_and_fetch_rootish_context()`),
/// and builds a view from that root,
/// by recursively following the `content` relationship.
pub async fn single_root_view (
  driver  : &TypeDBDriver,
  config  : &SkgConfig,
  focus   : &ID,
) -> Result < String, Box<dyn Error> > {
  multi_root_view (
    driver, config, & [ focus . clone () ] ) . await }

/// Just like 'single_root_view',
/// except it builds a forest rather than a tree.
pub async fn multi_root_view (
  driver  : &TypeDBDriver,
  config  : &SkgConfig,
  focii   : &[ID],
) -> Result < String, Box<dyn Error> > {

  let root_id_focus_pairs : Vec < ( ID, ID ) > =
    collect_root_ids (
      driver, & config . db_name, focii ) . await ?;
  let root_ids : Vec < ID > =
    root_id_focus_pairs
    . iter() . map ( | (root, _) |
                        root . clone ()
    ). collect ();
  let pids : Vec < ID > =
    collect_all_pids (
      driver, config, & root_ids ). await ?;
  let rel_data : MapsFromIdForView =
    fetch_relationship_data (
      driver, & config . db_name, & pids ). await ?;
  render_all_roots (
    driver, config, & root_id_focus_pairs, & rel_data
  ). await }

async fn collect_root_ids (
  driver  : &TypeDBDriver,
  db_name : &str,
  focii   : &[ID],
) -> Result < Vec < ( ID, ID ) >, Box<dyn Error> > {
  let mut root_id_focus_pairs
    : Vec < ( ID, ID ) >
    = Vec::new();
  for focus in focii {
    let root_id : ID =
      climb_containerward_and_fetch_rootish_context (
        db_name, driver, focus
      ). await ?;
    root_id_focus_pairs . push (( root_id,
                                  focus . clone () )); }
  Ok ( root_id_focus_pairs ) }

async fn collect_all_pids (
  driver   : &TypeDBDriver,
  config   : &SkgConfig,
  root_ids : &[ID],
) -> Result < Vec < ID >, Box<dyn Error> > {
  let mut visited : HashSet < ID > = HashSet::new();
  let mut pids : Vec < ID > = Vec::new();
  for root_id in root_ids {
    collect_pids_in_view (
      driver, config, root_id, &mut visited, &mut pids
    ) . await ?; }
  Ok ( pids ) }

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

/// Run four batch queries to fetch all relationship data for the given PIDs.
async fn fetch_relationship_data (
  driver  : &TypeDBDriver,
  db_name : &str,
  pids    : &[ID],
) -> Result < MapsFromIdForView, Box<dyn Error> > {

  let num_containers : HashMap < ID, usize > =
    count_containers ( db_name, driver, pids ) . await ?;
  let num_contents : HashMap < ID, usize > =
    count_contents ( db_name, driver, pids ) . await ?;
  let num_links_in : HashMap < ID, usize > =
    count_link_sources ( db_name, driver, pids ) . await ?;
  let ( container_to_contents, content_to_containers ) =
    contains_from_pids ( db_name, driver, pids ) . await ?;
  Ok ( MapsFromIdForView {
    num_containers,
    num_contents,
    num_links_in,
    container_to_contents,
    content_to_containers,
  }) }

async fn render_all_roots (
  driver               : &TypeDBDriver,
  config               : &SkgConfig,
  root_id_focus_pairs  : & [ ( ID, ID ) ],
  rel_data             : &MapsFromIdForView,
) -> Result < String, Box<dyn Error> > {
  let mut result : String = String::new();
  let mut visited : HashSet < ID > = HashSet::new();
  for (root_id, focus) in root_id_focus_pairs {
    let org : String =
      org_from_node_recursive (
        driver, config,
        root_id, focus, &mut visited, 1,
        None, rel_data
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
  rel_data      : &MapsFromIdForView,
) -> Result<String, Box<dyn Error>> {
  let ( pid, node ) =
    fetch_node_with_pid (
      driver, config, node_id ) . await ?;
  validate_node_title ( node_id, & node ) ?;
  let metadata : OrgnodeMetadata =
    build_metadata_for_node (
      node_id, org_parent_id, & pid, rel_data );
  if visited . contains ( node_id ) {
    return Ok (
      format_repeated_node (
        level, & node . title, metadata )); }
  visited . insert ( node_id . clone () );
  render_node_and_children (
    driver, config, node_id, focus,
    visited, level, & node, metadata, rel_data
  ) . await }

async fn fetch_node_with_pid (
  driver  : &TypeDBDriver,
  config  : &SkgConfig,
  node_id : &ID,
) -> Result < ( ID, SkgNode ), Box<dyn Error> > {
  let pid : ID =
    pid_from_id (
      & config . db_name, driver, node_id ). await ?
    . ok_or_else ( || format! (
      "ID '{}' not found in database", node_id )) ?;
  let path : String =
    path_from_pid ( &config, pid . clone () );
  let node : SkgNode =
    read_node ( path ) ?;
  Ok (( pid, node )) }

fn validate_node_title (
  node_id : &ID,
  node    : &SkgNode,
) -> Result < (), Box<dyn Error> > {
  if node . title . is_empty () {
    return Err ( Box::new ( io::Error::new (
      io::ErrorKind::InvalidData,
      format! ( "SkgNode with ID {} has an empty title", node_id ),
    ))); }
  Ok (()) }

async fn render_node_and_children (
  driver   : &TypeDBDriver,
  config   : &SkgConfig,
  node_id  : &ID,
  focus    : &ID,
  visited  : &mut HashSet<ID>,
  level    : usize,
  node     : &SkgNode,
  metadata : OrgnodeMetadata,
  rel_data : &MapsFromIdForView,
) -> Result < String, Box<dyn Error> > {
  let orgnode : OrgNode =
    OrgNode {
      metadata,
      title : newline_to_space ( & node . title ),
      body  : node . body . clone (), };
  let mut out : String =
    render_org_node_from_text ( level, & orgnode );
  for child_id in & node . contains {
    let child : String =
      Box::pin (
        org_from_node_recursive (
          driver, config,
          child_id, focus, visited, level + 1,
          Some ( node_id ), rel_data
        )) . await ?;
    out . push_str ( & child ); }
  Ok ( out ) }

fn build_metadata_for_node (
  node_id       : &ID,
  org_parent_id : Option<&ID>,
  pid           : &ID,
  rel_data      : &MapsFromIdForView,
) -> OrgnodeMetadata {
  let mut md = default_metadata ();
  md . id = Some ( node_id . clone () );
  if let Some ( parent_id ) = org_parent_id {
    md . relationships . parentIsContainer =
      rel_data . content_to_containers
      . get ( pid )
      . map_or ( false, | containers |
                 containers . contains ( parent_id ));
    md . relationships . parentIsContent =
      rel_data . container_to_contents
      . get ( pid )
      . map_or ( false, | contents |
                 contents . contains ( parent_id )); }
  md . relationships . numContainers =
    rel_data . num_containers . get ( pid ) . copied ();
  md . relationships . numContents =
    rel_data . num_contents . get ( pid ) . copied ();
  md . relationships . numLinksIn =
    rel_data . num_links_in . get ( pid ) . copied ();
  md }

/// When a node is encountered again in the same document:
/// Produce a headline with a `repeated` herald and a short body.
/// Do not recurse into children.
pub fn format_repeated_node (
  level    : usize,
  title    : &String,
  metadata : OrgnodeMetadata,
) -> String {
  let mut md = metadata;
  md . repeat = true;
  let orgnode : OrgNode =
    OrgNode {
      metadata : md,
      title : newline_to_space ( title ),
      body : Some (
        "Repeated, probably above. Edit there, not here."
          . to_string () ), };
  render_org_node_from_text (
    level,
    &orgnode ) }

/// Each of these describes some kind of relationship,
/// for each of a view's nodes.
pub struct MapsFromIdForView {
  num_containers : HashMap < ID, usize >, // number of contains relationships for which the node plays the 'contained' role
  num_contents : HashMap < ID, usize >, // number of contains relationships for which the node plays the 'container' role
  num_links_in : HashMap < ID, usize >, // number of hyperlinks relationship for which the node plays the 'target' role
  container_to_contents : HashMap < ID, HashSet < ID > >, // if the value would be empty, the key is omitted
  content_to_containers : HashMap < ID, HashSet < ID > >, // if the value would be empty, the key is omitted
}
