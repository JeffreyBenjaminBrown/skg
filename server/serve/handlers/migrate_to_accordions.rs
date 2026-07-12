//! 'skg-migrate-to-accordions' (5_plan.org, work item migration):
//! re-level every owned node's edges to at least their DEFAULTS --
//! raising leak-shaped memberships (a public file naming a
//! more-private ID) into the right accordion sections, never
//! lowering anything -- then rewrite the accordions (byte-stable,
//! so already-conforming files are untouched) and rebuild the
//! databases from the migrated files. Refused unless the active
//! source-set is "all": migration must see and rewrite every level.
//!
//! The report reminds what migration cannot fix: a public repo's
//! git HISTORY keeps any IDs it already leaked; rewriting history
//! is out of scope and manual.

use crate::dbs::filesystem::one_node::write_nodecomplete_accordion;
use crate::serve::ViewsState;
use crate::serve::handlers::rebuild_dbs::rebuild_dbs_in_place;
use crate::serve::util::send_response_with_length_prefix;
use crate::source_sets::ActiveSourceSet;
use crate::types::env::SkgEnv;
use crate::types::misc::{ID, MSV, PrivaciedMember, SkgConfig, SourceName};
use crate::types::nodes::complete::NodeComplete;
use crate::types::nodes::rust::NodeRust;

use std::net::TcpStream;

pub fn handle_migrate_to_accordions_request (
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
  let (changed, raised) : (usize, usize) =
    match migrate_all_owned_accordions (env) {
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
        "Migration rewrote {} node(s) ({} edge level(s) raised) but the rebuild failed: {}. Run 'rebuild dbs'.",
        changed, raised, e ));
      return; }}
  send_migrate_response ( stream, & format! (
    "Migration complete: {} node(s) rewritten, {} edge level(s) raised to their defaults. Reminder: git HISTORY in public repos keeps any IDs they leaked before migration; repairing that is manual.",
    changed, raised )); }

/// Raise every owned node's edge levels to at least their defaults
/// (never lowering), and rewrite the accordions of the nodes that
/// changed. Returns (nodes rewritten, edge levels raised).
fn migrate_all_owned_accordions (
  env : &mut SkgEnv,
) -> Result<(usize, usize), String> {
  let config : SkgConfig = env . config . clone ();
  let snap = env . in_rust_graph . load_full ();
  let mut changed_nodes : usize = 0;
  let mut raised_edges  : usize = 0;
  for (pid, node) in snap . nodes . iter () {
    if ! config . user_owns_source ( &node . source ) { continue; }
    let (migrated, raised) : (NodeComplete, usize) =
      raise_levels_to_defaults ( &config, &snap, pid, node );
    if raised > 0 {
      write_nodecomplete_accordion ( &migrated, &config )
        . map_err ( |e| format! (
          "writing '{}': {}", pid, e )) ?;
      changed_nodes += 1;
      raised_edges  += raised; }}
  Ok (( changed_nodes, raised_edges )) }

/// One node with every edge level raised to at least its default
/// (the more private of the endpoints' homes), clamped at the
/// owner's home; existing more-private levels survive untouched.
fn raise_levels_to_defaults (
  config : &SkgConfig,
  snap   : &crate::dbs::in_rust_graph::InRustGraph,
  pid    : &ID,
  node   : &NodeRust,
) -> (NodeComplete, usize) {
  let owner_home : SourceName = node . source . clone ();
  let mut raised : usize = 0;
  let mut raise = |m : &PrivaciedMember<ID>| -> PrivaciedMember<ID> {
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
    if level != m . level { raised += 1; }
    PrivaciedMember::at ( level, m . member . clone () ) };
  let raise_msv = |msv : &MSV<PrivaciedMember<ID>>,
                   raise : &mut dyn FnMut (&PrivaciedMember<ID>)
                           -> PrivaciedMember<ID>|
  -> MSV<PrivaciedMember<ID>> {
    match msv {
      MSV::Unspecified => MSV::Unspecified,
      MSV::Specified (v) =>
        MSV::Specified ( v . iter () . map ( |m| raise (m) )
                         . collect () ), }};
  let migrated : NodeComplete = NodeComplete {
    title     : node . title . clone (),
    aliases   : node . aliases . clone (),
    source    : node . source . clone (),
    pid       : pid . clone (),
    extra_ids : node . extra_ids . clone (),
    body      : node . body . clone (),
    contains  : node . contains . iter ()
      . map ( |m| raise (m) ) . collect (),
    subscribes_to :
      raise_msv ( &node . subscribes_to, &mut raise ),
    hides_from_its_subscriptions :
      raise_msv ( &node . hides_from_its_subscriptions, &mut raise ),
    overrides_view_of :
      raise_msv ( &node . overrides_view_of, &mut raise ),
    misc      : node . misc . clone (), };
  (migrated, raised) }

fn send_migrate_response (
  stream  : &mut TcpStream,
  content : &str,
) {
  let escaped : String =
    content . replace ('\\', "\\\\")
    . replace ('"', "\\\"")
    . replace ('\n', "\\n");
  let response : String = format! (
    "((response-type migrate-to-accordions) (content \"{}\"))",
    escaped );
  send_response_with_length_prefix (stream, &response); }
