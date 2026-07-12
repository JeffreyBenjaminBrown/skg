//! NodeComplete: the full in-Rust-graph node.
//!
//! Carries every field a node has: the on-disk fields (mirrored
//! from NodeFS) plus 'source', which is inferred from file
//! location and held only in the in-Rust graph.
//!
//! NodeComplete itself is NOT Serialize/Deserialize. The on-disk
//! round-trip goes through 'NodeFS'
//! ([[./fs.rs][server/types/nodes/fs.rs]]):
//! read YAML as 'NodeFS', then attach source via
//! 'NodeFS::into_complete(source)' to get a 'NodeComplete'. To
//! write, convert 'NodeComplete' -> 'NodeFS' via 'From' (dropping
//! 'source'), then serialize the 'NodeFS'. This way the type
//! system enforces that 'source' never appears in YAML.

use crate::types::misc::{ID, MSV, PrivaciedMember, SourceName};

/// This could be extended.
/// A .skg file can have any number of associated FileProperties.
#[derive(Clone, Debug, Eq, Hash, PartialEq, serde::Serialize, serde::Deserialize)]
pub enum FileProperty {
  Had_ID_Before_Import, // Node had an :ID: property before org-roam import.
  Was_Overloaded, // Multiple org-roam nodes used the same ID (as an ID, not in a link). This guards against a bug in my org-roam data (I can't say it's a bug in org-roam; I don't know.) The importer merges their content into a single node with the ID that was overloaded in org-roam.
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct NodeComplete {
  // There is a 1-to-1 correspondence between NodeCompletes and privacy TELESCOPES (families of same-pid .skg files, one section per source; see docs/telescopes.md). Reading FOLDS the sections into a NodeComplete; writing UNFOLDS it back into sections, byte-stably. The files are the only permanent data. NodeComplete is the format used to initialize the TypeDB and Tantivy databases (via narrowing conversions to NodeTypedb / NodeTantivy at their boundaries).
  // Tantivy receives some of this data, and TypeDB some other subset. Tantivy associates IDs with titles. TypeDB represents all the connections between nodes (see 'schema.tql' for how). At least one field, `body`, is known to neither database; it is instead read directly from the files on disk when Rust builds a document for Emacs.
  // PITFALL: 'MSV<T>' (Maybe-Specified Vector; see types/misc.rs) distinguishes 'Unspecified' ("user didn't mention this field") from 'Specified(vec![...])' ("user wants it to be this value, even if empty"). This matters when reconciling multiple NodeCompletes (e.g. 'reconcile_same_id_instructions' and supplement_unspecified_fields_from_disk). PITFALL: since telescopes, the distinction is meaningful ON DISK too: a section that omits a field has no opinion about it (Unspecified), while under unfold each section records exactly the edges leveled at it -- so what a given section file shows is not the node's whole list, and an absent field in one section says nothing about the fold.

  pub title: String,
  pub aliases: MSV<PrivaciedMember<String>>, // A node can be searched for using its title or any of its aliases, and so far using its body text too. (I might later decide not to index bodies, or to give the choice to the user.) Each alias carries the privacy level of the telescope section that records it.
  pub source: SourceName, // source name, inferred from file location and SkgConfig
  pub pid: ID, // Primary ID. Determines filename, TypeDB identity, Tantivy key, map key. Never changes.
  pub extra_ids: Vec<ID>, // Extra IDs accumulated through nodeMerges. Usually empty.
  pub body: Option<String>, // Unknown to both Tantivy & TypeDB. The body is all text (if any) between the preceding org headline, to which it belongs, and the next (if there is a next).

  // Each relationship member carries the privacy LEVEL of the edge
  // (see 'PrivaciedMember'). List order is fold order.
  pub contains                     : Vec<PrivaciedMember<ID>>, // See schema.tql.
  pub subscribes_to                : MSV<PrivaciedMember<ID>>, // See schema.tql.
  pub hides_from_its_subscriptions : MSV<PrivaciedMember<ID>>, // See schema.tql.
  pub overrides_view_of            : MSV<PrivaciedMember<ID>>, // See schema.tql.

  pub misc: Vec<FileProperty>,
}

impl NodeComplete {
  pub fn all_ids (&self) -> impl Iterator<Item = &ID> {
    std::iter::once (&self . pid)
      . chain (self . extra_ids . iter()) }
}

//
// Functions
//

/// Normalize a node body: drop leading and trailing whitespace-only
/// lines (a line is whitespace-only iff it trims to empty); if nothing
/// remains, the body becomes 'None'. After this, "bodyless" is exactly
/// 'body == None' -- the invariant the surprising-links =bodyless= test
/// relies on. Idempotent; enforced at every disk write ('NodeFS::from')
/// and on the in-memory save NodeComplete ('into_nodecomplete'), with a
/// one-time migration ('data/bash/trim-node-bodies.org') for old data.
pub fn normalize_body (
  body : Option<String>,
) -> Option<String> {
  let s : String = body ?;
  let lines : Vec<&str> = s . lines () . collect ();
  let first : Option<usize> =
    lines . iter () . position ( |l| ! l . trim () . is_empty () );
  let last : Option<usize> =
    lines . iter () . rposition ( |l| ! l . trim () . is_empty () );
  match (first, last) {
    (Some (a), Some (b)) => Some ( lines [a ..= b] . join ("\n") ),
    _ => None, }}

/// Useful for making tests more readable.
pub fn empty_node_complete () -> NodeComplete {
  NodeComplete {
    title                        : String::new (),
    aliases                      : MSV::Unspecified,
    source                       : SourceName::from ("main"),
    pid                          : ID::new (""),
    extra_ids                    : Vec::new (),
    body                         : None,
    contains                     : Vec::new(),
    subscribes_to                : MSV::Unspecified,
    hides_from_its_subscriptions : MSV::Unspecified,
    overrides_view_of            : MSV::Unspecified,
    misc                         : Vec::new (),
  }}
