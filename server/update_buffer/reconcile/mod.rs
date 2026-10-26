// The goal computers the refresh ('server/refresh/goal.rs') reads.

pub mod content;
pub mod partner_folder;

use crate::dbs::in_rust_graph::InRustGraph;
use crate::skgrepo_sets::SkgrepoRestriction;
use crate::types::misc::ID;

/// Under a skgrepo restriction (the API doc's "Restricted-skgrepo nodes"): rendering omits every
/// restricted member from goal lists (no placeholders are created).  A
/// member the graph cannot resolve is omitted too: it might be private,
/// and rendering must not leak ('InRustGraph::restriction_keeps_out');
/// saving preserves it
/// regardless (the weave / set-difference merge treat unresolvable as
/// invisible).  A tombstone standing where a now-restricted node was (one
/// kept because it hosts visible descendants after a skgrepo-set
/// reduction) does not come back through the goal list: the refresh
/// keeps it while it holds anything, and its tail prunes it once it
/// holds nothing.
pub fn omit_restricted_members (
  goal        : Vec<ID>,
  graph       : &InRustGraph,
  restriction : Option<&SkgrepoRestriction>,
) -> Vec<ID> {
  goal . into_iter ()
    . filter ( |gnid| ! graph . restriction_keeps_out (gnid, restriction) )
    . collect () }
