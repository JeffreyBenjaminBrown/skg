use crate::media::file_io::read_node;
use crate::util::path_from_pid_and_source;
use crate::media::typedb::util::pid_and_source_from_id;
use crate::types::{SkgNode, ID, SkgConfig, OrgNode};
use crate::types::orgnode::default_metadata;

use std::error::Error;
use std::io;
use typedb_driver::TypeDBDriver;

/// Fetch a SkgNode from disk (queries TypeDB for source).
/// Make an OrgNode from it, with validated title.
/// Return both.
pub async fn skgnode_and_orgnode_from_id (
  config : &SkgConfig,
  driver : &TypeDBDriver,
  id     : &ID,
) -> Result < ( OrgNode, SkgNode ), Box<dyn Error> > {
  let (pid_resolved, source) : (ID, String) =
    pid_and_source_from_id( // Query TypeDB for them
      &config.db_name, driver, id).await?
    . ok_or_else( || format!(
      "ID '{}' not found in database", id ))?;
  skgnode_and_orgnode_from_pid_and_source (
    config, &pid_resolved, &source ) }

/// Fetch a SkgNode from disk given PID and source.
/// Make an OrgNode from it, with validated title.
/// Return both.
pub fn skgnode_and_orgnode_from_pid_and_source (
  config : &SkgConfig,
  pid    : &ID,
  source : &str,
) -> Result < ( OrgNode, SkgNode ), Box<dyn Error> > {
  let path : String =
    path_from_pid_and_source (
      config, source, pid.clone() );
  let mut skgnode : SkgNode = read_node ( path ) ?;
  skgnode.source = source.to_string();
  let orgnode : OrgNode = OrgNode {
    metadata : { let mut md = default_metadata ();
                 md . id = Some ( pid . clone () );
                 md . source = Some ( source . to_string () );
                 md },
    title : newline_to_space ( & skgnode . title ),
    body  : skgnode . body . clone (), };
  if orgnode . title . is_empty () { // Validate title
    return Err ( Box::new ( io::Error::new (
      io::ErrorKind::InvalidData,
      format! ( "SkgNode with ID {} has an empty title",
                 pid ), )) ); }
  Ok (( orgnode, skgnode )) }

/// Make an OrgNode marked 'repeated' by fetching it from disk.
pub async fn mk_repeated_orgnode_from_id (
  config : &SkgConfig,
  driver : &TypeDBDriver,
  id     : &ID,
) -> Result < OrgNode, Box<dyn Error> > {
  let (pid_resolved, source) : (ID, String) =
    pid_and_source_from_id( // Query TypeDB for them
      &config.db_name, driver, id).await?
    . ok_or_else( || format!(
      "ID '{}' not found in database", id))?;
  let (mut orgnode, _skgnode) : ( OrgNode, SkgNode ) =
    skgnode_and_orgnode_from_pid_and_source (
      config, &pid_resolved, &source ) ?;
  orgnode . metadata . viewData . repeat = true;
  orgnode . metadata . code . indefinitive = true; // Any repeated node is indefinitive, although not vice-versa.
  orgnode . metadata . id = Some ( id . clone () );
  orgnode . metadata . source = Some ( source . clone () );
  orgnode . body = Some (
    "Repeated, probably above. Edit there, not here."
      . to_string () );
  Ok ( orgnode ) }

pub fn newline_to_space ( s: &str ) -> String {
  s.replace ( '\n', " " ) }

pub fn org_bullet ( level: usize ) -> String {
  "*" . repeat ( level.max ( 1 )) }
