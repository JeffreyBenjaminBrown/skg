//! 'skg-migrate-to-telescopes' (5_plan.org, work item migration):
//! re-level every owned node's edges to at least their DEFAULTS --
//! raising the privacy of leak-shaped memberships (a public file
//! naming a more-private ID) into the right telescope sections,
//! never making anything more public -- then rewrite the
//! telescopes (byte-stable, so already-conforming files are
//! untouched) and rebuild the databases from the migrated files.
//! Refused unless the active source-set is "all": migration must
//! see and rewrite every privacy level.
//!
//! The report reminds what migration cannot fix: a public repo's
//! git HISTORY keeps any IDs it already leaked; rewriting history
//! is out of scope and manual.

use crate::dbs::filesystem::one_node::write_nodecomplete_telescope;
use crate::serve::ViewsState;
use crate::serve::handlers::rebuild_dbs::rebuild_dbs_in_place;
use crate::serve::util::send_response_with_length_prefix;
use crate::source_sets::ActiveSourceSet;
use crate::types::env::SkgEnv;
use crate::types::misc::{ID, MSV, PrivaciedMember, SkgConfig, SourceName};
use crate::types::nodes::complete::NodeComplete;
use crate::types::nodes::rust::NodeRust;

use std::net::TcpStream;

pub fn handle_migrate_to_telescopes_request (
  stream      : &mut TcpStream,
  env         : &mut SkgEnv,
  views_state : &mut ViewsState,
  active      : &ActiveSourceSet,
) {
  if ! active . is_all () {
    send_migrate_response ( stream, & format! (
      "Migration refused: it must see and rewrite every privacy level, so the active source-set must be 'all' (it is '{}').",
      active . name . 0 ));
    return; }
  let (changed, privacy_raises) : (usize, usize) =
    match migrate_all_owned_telescopes (env) {
      Ok (counts) => counts,
      Err (e) => {
        send_migrate_response ( stream, & format! (
          "Migration failed: {}", e ));
        return; }};
  if changed > 0 {
    // The files changed under the running databases; rebuild from
    // disk exactly as the rebuild-dbs endpoint would.
    if let Err (e) = rebuild_dbs_in_place (env, views_state) {
      send_migrate_response ( stream, & format! (
        "Migration rewrote {} node(s) ({} edge(s) made more private) but the rebuild failed: {}. Run 'rebuild dbs'.",
        changed, privacy_raises, e ));
      return; }}
  send_migrate_response ( stream, & format! (
    "Migration complete: {} node(s) rewritten, {} edge(s) made more private (raised to their default privacy). Reminder: git HISTORY in public repos keeps any IDs they leaked before migration; repairing that is manual.",
    changed, privacy_raises )); }

/// Raise every owned node's edge privacy to at least their defaults
/// (never making anything more public), and rewrite the telescopes
/// of the nodes that changed. Returns (nodes rewritten, edges with
/// privacy raised).
fn migrate_all_owned_telescopes (
  env : &mut SkgEnv,
) -> Result<(usize, usize), String> {
  let config : SkgConfig = env . config . clone ();
  let snap = env . in_rust_graph . load_full ();
  let mut changed_nodes : usize = 0;
  let mut privacy_raised_edges : usize = 0;
  for (pid, node) in snap . nodes . iter () {
    if ! config . user_owns_source ( &node . source ) { continue; }
    let (migrated, privacy_raises) : (NodeComplete, usize) =
      raise_privacy_to_defaults ( &config, &snap, pid, node );
    if privacy_raises > 0 {
      write_nodecomplete_telescope ( &migrated, &config )
        . map_err ( |e| format! (
          "writing '{}': {}", pid, e )) ?;
      changed_nodes += 1;
      privacy_raised_edges += privacy_raises; }}
  Ok (( changed_nodes, privacy_raised_edges )) }

/// One node with every edge's privacy raised to at least its
/// default (the more private of the endpoints' homes), clamped at
/// the owner's home; existing more-private levels survive
/// untouched.
fn raise_privacy_to_defaults (
  config : &SkgConfig,
  snap   : &crate::dbs::in_rust_graph::InRustGraph,
  pid    : &ID,
  node   : &NodeRust,
) -> (NodeComplete, usize) {
  let owner_home : SourceName = node . source . clone ();
  let mut privacy_raises : usize = 0;
  let mut raise_privacy = |m : &PrivaciedMember<ID>| -> PrivaciedMember<ID> {
    let target_home : Option<SourceName> =
      snap . pid_of ( &m . member )
      . and_then ( |p| snap . nodes . get (&p) )
      . map ( |n| n . source . clone () );
    let floor : SourceName = match target_home {
      Some (h) => config . more_private_of (
        owner_home . clone (), h ),
      None => owner_home . clone (), };
    let level : SourceName = config . more_private_of (
      m . level . clone (), floor );
    if level != m . level { privacy_raises += 1; }
    PrivaciedMember::at ( level, m . member . clone () ) };
  let raise_privacy_msv = |msv : &MSV<PrivaciedMember<ID>>,
                           raise_privacy : &mut dyn FnMut (&PrivaciedMember<ID>)
                           -> PrivaciedMember<ID>|
  -> MSV<PrivaciedMember<ID>> {
    match msv {
      MSV::Unspecified => MSV::Unspecified,
      MSV::Specified (v) =>
        MSV::Specified ( v . iter () . map ( |m| raise_privacy (m) )
                         . collect () ), }};
  let migrated : NodeComplete = NodeComplete {
    title     : node . title . clone (),
    aliases   : node . aliases . clone (),
    source    : node . source . clone (),
    pid       : pid . clone (),
    extra_ids : node . extra_ids . clone (),
    body      : node . body . clone (),
    contains  : node . contains . iter ()
      . map ( |m| raise_privacy (m) ) . collect (),
    subscribes_to :
      raise_privacy_msv ( &node . subscribes_to, &mut raise_privacy ),
    hides_from_its_subscriptions :
      raise_privacy_msv ( &node . hides_from_its_subscriptions, &mut raise_privacy ),
    overrides_view_of :
      raise_privacy_msv ( &node . overrides_view_of, &mut raise_privacy ),
    misc      : node . misc . clone (), };
  (migrated, privacy_raises) }

fn send_migrate_response (
  stream  : &mut TcpStream,
  content : &str,
) {
  let escaped : String =
    content . replace ('\\', "\\\\")
    . replace ('"', "\\\"")
    . replace ('\n', "\\n");
  let response : String = format! (
    "((response-type migrate-to-telescopes) (content \"{}\"))",
    escaped );
  send_response_with_length_prefix (stream, &response); }
