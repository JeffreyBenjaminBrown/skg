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

use crate::types::misc::{ID, MSV, SourceName};

/// This could be extended.
/// A .skg file can have any number of associated FileProperties.
#[derive(Clone, Debug, Eq, Hash, PartialEq, serde::Serialize, serde::Deserialize)]
pub enum FileProperty {
  Had_ID_Before_Import, // Node had an :ID: property before org-roam import.
  Was_Overloaded, // Multiple org-roam nodes used the same ID (as an ID, not in a link). This guards against a bug in my org-roam data (I can't say it's a bug in org-roam; I don't know.) The importer merges their content into a single node with the ID that was overloaded in org-roam.
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct NodeComplete {
  // There is a 1-to-1 correspondence between NodeCompletes and actual .skg files. A file can be read into a NodeFS, then given a source to produce a NodeComplete; a NodeComplete can be converted to NodeFS and written back. The files are the only permanent data. NodeComplete is the format used to initialize the TypeDB and Tantivy databases (via narrowing conversions to NodeTypedb / NodeTantivy at their boundaries).
  // Tantivy receives some of this data, and TypeDB some other subset. Tantivy associates IDs with titles. TypeDB represents all the connections between nodes (see 'schema.tql' for how). At least one field, `body`, is known to neither database; it is instead read directly from the files on disk when Rust builds a document for Emacs.
  // PITFALL: 'MSV<T>' (Maybe-Specified Vector; see types/misc.rs) distinguishes 'Unspecified' ("user didn't mention this field") from 'Specified(vec![...])' ("user wants it to be this value, even if empty"). This matters when reconciling multiple NodeCompletes (e.g. 'reconcile_same_id_instructions' and supplement_unspecified_fields_from_disk). On disk the distinction is not needed: both 'Unspecified' and Specified(vec![])' are rendered as a missing field.

  pub title: String,

  pub aliases: MSV<String>, // A node can be searched for using its title or any of its aliases, and so far using its body text too. (I might later decide not to index bodies, or to give the choice to the user.)

  pub source: SourceName, // source name, inferred from file location and SkgConfig

  pub pid: ID, // Primary ID. Determines filename, TypeDB identity, Tantivy key, map key. Never changes.

  pub extra_ids: Vec<ID>, // Extra IDs accumulated through merges. Usually empty.

  pub body: Option<String>, // Unknown to both Tantivy & TypeDB. The body is all text (if any) between the preceding org headline, to which it belongs, and the next (if there is a next).

  pub contains                     : Vec<ID>, // See schema.tql.
  pub subscribes_to                : MSV<ID>, // See schema.tql.
  pub hides_from_its_subscriptions : MSV<ID>, // See schema.tql.
  pub overrides_view_of            : MSV<ID>, // See schema.tql.

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
