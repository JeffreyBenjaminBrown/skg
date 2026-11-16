use std::collections::HashSet;

use crate::dbs::in_rust_graph::InRustGraph;
use crate::types::misc::{ID, members_of};

pub fn find_related_nodes (
  graph       : &InRustGraph,
  nodes       : &[ID],
  relation    : &str,
  input_role  : &str,
  output_role : &str,
) -> HashSet<ID> {
  let mut out : HashSet<ID> = HashSet::new ();
  for input_gnid in nodes {
    let pid : ID = match graph . pid_of (input_gnid) {
      Some (p) => p,
      None     => continue };
    // Forward fields on GraphnodeInRust mirror disk and thus carry raw IDs;
    // callers expect canonical pids; map
    // would return after its has_extra_id lookups). Map the ID of
    // each second member (see docs/data-model_technical.org) to its corresponding PID
    // (which might be itself) before inserting.
    let pid_or_self = |gnid: &ID| -> ID {
      graph . pid_else_raw_id (gnid) };
    match (relation, input_role, output_role) {
      // Forward lookups: read the field on the node.
      ("contains",                     "container",   "content")  =>
        if let Some (n) = graph . nodes . get (&pid) {
          out . extend ( members_of (& n . contains) . iter () . map (&pid_or_self) ); },
      ("forks",                        "forker",      "forkee")     =>
        if let Some (n) = graph . nodes . get (&pid) {
          out . extend ( members_of ( n . forks . or_default () )
                         . iter () . map (&pid_or_self) ); },
      ("hidesFromSubs", "hider",                      "hidden")     =>
        if let Some (n) = graph . nodes . get (&pid) {
          out . extend ( members_of ( n . hidesFromSubs . or_default () )
                         . iter () . map (&pid_or_self) ); },
      ("linksTo",                      "mentioner",   "mentioned")  =>
        if let Some (n) = graph . nodes . get (&pid) {
          out . extend ( n . linksTo
                         . iter () . map (&pid_or_self) ); },
      // Inverse lookups: consult the recorderward relmap.
      ("contains",                     "content",   "container")   =>
        if let Some (s) = graph . contained_by . get (&pid) {
          out . extend ( s . iter () . cloned () ); },
      ("forks",                        "forkee",      "forker")      =>
        if let Some (s) = graph . forkers_of . get (&pid) {
          out . extend ( s . iter () . cloned () ); },
      ("hidesFromSubs",                "hidden",      "hider")       =>
        if let Some (s) = graph . hiders_of . get (&pid) {
          out . extend ( s . iter () . cloned () ); },
      ("linksTo",                      "mentioned",   "mentioner")  =>
        if let Some (s) = graph . mentioners_of . get (&pid) {
          out . extend ( s . iter () . cloned () ); },
      // Unknown (relation, input, output) combination: a caller's
      // bug, since the four outbound types have exactly two roles
      // each. Answering nothing would hide a stale relation name.
      (relation, input_role, output_role) =>
        panic! ("find_related_nodes: unknown triple ({}, {}, {})",
                relation, input_role, output_role),
    } }
  out }
