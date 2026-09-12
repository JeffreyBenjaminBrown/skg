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
  for input_id in nodes {
    let pid : ID = match graph . pid_of (input_id) {
      Some (p) => p,
      None     => continue };
    // Forward fields on NodeRust mirror disk and thus carry raw IDs;
    // callers expect canonical pids; map
    // would return after its has_extra_id lookups). Map the ID of
    // each second member (see docs/data-model_technical.org) to its corresponding PID
    // (which might be itself) before inserting.
    let pid_or_self = |id: &ID| -> ID {
      graph . pid_of (id) . unwrap_or_else ( || id . clone () ) };
    match (relation, input_role, output_role) {
      // Forward lookups: read the field on the node.
      ("contains",                     "container",   "contained")  =>
        if let Some (n) = graph . nodes . get (&pid) {
          out . extend ( members_of (& n . contains) . iter () . map (&pid_or_self) ); },
      ("subscribes",                   "subscriber",  "subscribee") =>
        if let Some (n) = graph . nodes . get (&pid) {
          out . extend ( members_of ( n . subscribes_to . or_default () )
                         . iter () . map (&pid_or_self) ); },
      ("hides_from_its_subscriptions", "hider",       "hidden")     =>
        if let Some (n) = graph . nodes . get (&pid) {
          out . extend ( members_of ( n . hides_from_its_subscriptions . or_default () )
                         . iter () . map (&pid_or_self) ); },
      ("overrides_view_of",            "overrider",   "overridden") =>
        if let Some (n) = graph . nodes . get (&pid) {
          out . extend ( members_of ( n . overrides_view_of . or_default () )
                         . iter () . map (&pid_or_self) ); },
      ("textlinks_to",                 "source",      "dest")       =>
        if let Some (n) = graph . nodes . get (&pid) {
          out . extend ( n . textlinks_to
                         . iter () . map (&pid_or_self) ); },
      // Inverse lookups: consult the inverse index.
      ("contains",                     "contained",   "container")   =>
        if let Some (s) = graph . contained_by . get (&pid) {
          out . extend ( s . iter () . cloned () ); },
      ("subscribes",                   "subscribee",  "subscriber")  =>
        if let Some (s) = graph . subscribers_of . get (&pid) {
          out . extend ( s . iter () . cloned () ); },
      ("hides_from_its_subscriptions", "hidden",      "hider")       =>
        if let Some (s) = graph . hiders_of . get (&pid) {
          out . extend ( s . iter () . cloned () ); },
      ("overrides_view_of",            "overridden",  "overrider")   =>
        if let Some (s) = graph . overriders_of . get (&pid) {
          out . extend ( s . iter () . cloned () ); },
      ("textlinks_to",                 "dest",        "source")      =>
        if let Some (s) = graph . textlinks_in . get (&pid) {
          out . extend ( s . iter () . cloned () ); },
      // Unknown (relation, input, output) combination: shouldn't
      // happen — the five outbound types have exactly two roles
      // each. Return empty (caller will get no matches).
      _ => {},
    } }
  out }
