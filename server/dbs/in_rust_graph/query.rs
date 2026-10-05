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
  for input_skgid in nodes {
    let pid : ID = match graph . pid_of (input_skgid) {
      Some (p) => p,
      None     => continue };
    // Forward fields on GraphnodeInRust mirror disk and thus carry raw IDs;
    // callers expect canonical pids; map
    // would return after its has_extra_id lookups). Map the ID of
    // each second member (see docs/data-model_technical.org) to its corresponding PID
    // (which might be itself) before inserting.
    let pid_or_self = |skgid: &ID| -> ID {
      graph . pid_of (skgid) . unwrap_or_else ( || skgid . clone () ) };
    match (relation, input_role, output_role) {
      // Forward lookups: read the field on the node.
      ("contains",                     "container",   "content")  =>
        if let Some (n) = graph . nodes . get (&pid) {
          out . extend ( members_of (& n . contains) . iter () . map (&pid_or_self) ); },
      ("subscribesTo",                 "subscriber",  "subscribee") =>
        if let Some (n) = graph . nodes . get (&pid) {
          out . extend ( members_of ( n . subscribesTo . or_default () )
                         . iter () . map (&pid_or_self) ); },
      ("hidesFromSubs", "hider",                      "hidden")     =>
        if let Some (n) = graph . nodes . get (&pid) {
          out . extend ( members_of ( n . hidesFromSubs . or_default () )
                         . iter () . map (&pid_or_self) ); },
      ("overrides",                    "overrider",   "overridden") =>
        if let Some (n) = graph . nodes . get (&pid) {
          out . extend ( members_of ( n . overrides . or_default () )
                         . iter () . map (&pid_or_self) ); },
      ("linksTo",                      "mentioner",   "mentioned")  =>
        if let Some (n) = graph . nodes . get (&pid) {
          out . extend ( n . linksTo
                         . iter () . map (&pid_or_self) ); },
      // Inverse lookups: consult the recorderward relmap.
      ("contains",                     "content",   "container")   =>
        if let Some (s) = graph . contained_by . get (&pid) {
          out . extend ( s . iter () . cloned () ); },
      ("subscribesTo",                 "subscribee",  "subscriber")  =>
        if let Some (s) = graph . subscribers_of . get (&pid) {
          out . extend ( s . iter () . cloned () ); },
      ("hidesFromSubs",                "hidden",      "hider")       =>
        if let Some (s) = graph . hiders_of . get (&pid) {
          out . extend ( s . iter () . cloned () ); },
      ("overrides",                    "overridden",  "overrider")   =>
        if let Some (s) = graph . overriders_of . get (&pid) {
          out . extend ( s . iter () . cloned () ); },
      ("linksTo",                      "mentioned",   "mentioner")  =>
        if let Some (s) = graph . mentioners_of . get (&pid) {
          out . extend ( s . iter () . cloned () ); },
      // Unknown (relation, input, output) combination: shouldn't
      // happen — the five outbound types have exactly two roles
      // each. Return empty (caller will get no matches).
      _ => {},
    } }
  out }
