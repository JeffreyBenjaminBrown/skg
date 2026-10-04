//! Graphnode: the full in-Rust-graph node.
//!
//! Carries every field a node has: the on-disk fields (mirrored
//! from GraphnodeOnDisk) plus 'repo', which is inferred from file
//! location and held only in the in-Rust graph.
//!
//! Graphnode itself is NOT Serialize/Deserialize. The on-disk
//! round-trip goes through 'GraphnodeOnDisk'
//! ([[./fs.rs][server/types/nodes/fs.rs]]):
//! read YAML as 'GraphnodeOnDisk', then attach repo via
//! 'GraphnodeOnDisk::into_complete(repo)' to get a 'Graphnode'. To
//! write, convert 'Graphnode' -> 'GraphnodeOnDisk' via 'From' (dropping
//! 'repo'), then serialize the 'GraphnodeOnDisk'. This way the type
//! system enforces that 'repo' never appears in YAML.

use crate::types::misc::{ID, MSV, RelPartner, RepoName};

use std::collections::HashSet;

/// This could be extended.
/// A .skg file can have any number of associated Flags.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, serde::Serialize, serde::Deserialize)]
pub enum Flag {
  Had_ID_Before_Import, // Node had an :ID: property before import.
  Was_Overloaded, // Multiple org-roam nodes used the same ID (as an ID, not in a link). This guards against a bug in my org-roam data (I can't say it's a bug in org-roam; I don't know.) The importer merges their content into a single node with the ID that was overloaded in org-roam.
  NoSearchMatching, // Title, aliases and body cannot produce a direct text-search match. This is search decluttering, not access control.
}

impl Flag {
  /// Stable display order for the generated flags folder.  Disk order is
  /// deliberately independent: `misc` remains a small, byte-stable vector.
  pub const ALL : [Flag; 3] = [
    Flag::Had_ID_Before_Import,
    Flag::Was_Overloaded,
    Flag::NoSearchMatching,
  ];

  pub fn wire_name (self) -> &'static str {
    match self {
      Flag::Had_ID_Before_Import => "hadId",
      Flag::Was_Overloaded       => "wasOverloaded",
      Flag::NoSearchMatching     => "noSearchMatching",
    } }

  pub fn from_wire_name (name : &str) -> Option<Flag> {
    Self::ALL . into_iter ()
      . find (|flag| flag . wire_name () == name) }

  pub fn herald_text (self) -> &'static str {
    match self {
      Flag::Had_ID_Before_Import =>
        "had ID before import",
      Flag::Was_Overloaded =>
        "was overloaded during org-roam import",
      Flag::NoSearchMatching =>
        "no search matching",
    } }

  pub fn is_mutable (self) -> bool {
    matches! (self, Flag::NoSearchMatching) }
}

pub fn flag_is_true (
  misc     : &[Flag],
  flag : Flag,
) -> bool {
  misc . contains (&flag) }

/// Set one flag without perturbing the other entries' relative order.
/// True adds one copy at the end iff absent; false removes every copy so old,
/// hand-edited duplicate vectors are repaired by an explicit clear gesture.
pub fn set_flag (
  misc     : &mut Vec<Flag>,
  flag : Flag,
  value    : bool,
) {
  if value {
    if ! misc . contains (&flag) { misc . push (flag); }
  } else {
    misc . retain (|candidate| *candidate != flag); }}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Graphnode {
  // There is a 1-to-1 correspondence between Graphnodes and privacy TELESCOPES (families of same-pid .skg files, one section per repo; see docs/telescopes.org). Reading FOLDS the sections into a Graphnode; writing UNFOLDS it back into sections, byte-stably. The files are the only permanent data. Graphnode initializes the in-memory graph and Tantivy index.
  // The graph indexes this complete record for structural queries. Tantivy
  // receives the searchable subset. The filesystem remains authoritative.
  // PITFALL: 'MSV<T>' (Maybe-Specified Vector; see types/misc.rs) distinguishes 'Unspecified' ("user didn't mention this field") from 'Specified(vec![...])' ("user wants it to be this value, even if empty"). This matters when reconciling multiple Graphnodes (e.g. 'reconcile_same_id_instructions' and supplement_unspecified_fields_from_disk). PITFALL: since telescopes, the distinction is meaningful ON DISK too: a section that omits a field has no opinion about it (Unspecified), while under unfold each section records exactly the edges recorded there -- so what a given section file shows is not the node's whole list, and an absent field in one section says nothing about the fold.

  pub title: String,
  /// True when the selected title or body came from below the home.
  /// Precise title/body-text repos remain a fold/save-time fact; runtime
  /// release decisions intentionally use this coarse flag.
  pub overPrivateText_telescope: bool,
  pub aliases: MSV<RelPartner<String>>, // A node can be searched for using its title or any of its aliases, and so far using its body text too. (I might later decide not to index bodies, or to give the choice to the user.) Each alias carries its relRepo.
  pub home_repo: RepoName, // repo name, inferred from file location and SkgConfig
  pub pid: ID, // Primary ID. Determines filename, graph identity, Tantivy key, and map key. Never changes.
  pub extra_ids: Vec<ID>, // Extra IDs accumulated through nodeMerges. Usually empty.
  pub body: Option<String>, // Not indexed by the structural graph. The body is all text (if any) between the preceding org headline, to which it belongs, and the next (if there is a next).

  // Each relationship member carries the privacy LEVEL of the edge
  // (see 'RelPartner'). List order is fold order.
  pub contains                     : Vec<RelPartner<ID>>, // See docs/data-model_technical.org.
  pub subscribes_to                : MSV<RelPartner<ID>>, // See docs/data-model_technical.org.
  pub hides_from_its_subscriptions : MSV<RelPartner<ID>>, // See docs/data-model_technical.org.
  pub overrides_view_of            : MSV<RelPartner<ID>>, // See docs/data-model_technical.org.

  pub misc: Vec<Flag>,
}

impl Graphnode {
  pub fn all_ids (&self) -> impl Iterator<Item = &ID> {
    std::iter::once (&self . pid)
      . chain (self . extra_ids . iter()) }

  /// Make this node's ID list a stable set.  The primary ID is logically the
  /// first claim, so it never also appears among the extras; otherwise the
  /// first occurrence wins and the user's meaningful extra-ID order remains
  /// intact.  This is idempotent.
  pub fn normalize_ids (
    &mut self,
  ) {
    self . extra_ids = self . normalized_extra_ids (); }

  pub fn normalized_extra_ids (
    &self,
  ) -> Vec<ID> {
    let mut seen : HashSet<ID> = HashSet::new ();
    seen . insert (self . pid . clone ());
    self . extra_ids . iter ()
      . filter_map ( |id| {
        if seen . insert (id . clone ()) { Some (id . clone ()) }
        else                             { None } } )
      . collect () }
}

//
// Functions
//

/// Normalize a node body: drop leading and trailing whitespace-only
/// lines (a line is whitespace-only iff it trims to empty); if nothing
/// remains, the body becomes 'None'. After this, "bodyless" is exactly
/// 'body == None' -- the invariant the substantive-mentioner predicate
/// relies on. Idempotent; enforced at every disk write ('GraphnodeOnDisk::from')
/// and on the in-memory save Graphnode ('into_graphnode'), with a
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
pub fn empty_node_complete () -> Graphnode {
  Graphnode {
    title                        : String::new (),
    overPrivateText_telescope               : false,
    aliases                      : MSV::Unspecified,
    home_repo                       : RepoName::from ("main"),
    pid                          : ID::new (""),
    extra_ids                    : Vec::new (),
    body                         : None,
    contains                     : Vec::new(),
    subscribes_to                : MSV::Unspecified,
    hides_from_its_subscriptions : MSV::Unspecified,
    overrides_view_of            : MSV::Unspecified,
    misc                         : Vec::new (),
  }}

#[cfg(test)]
#[path = "../../../tests/unit/graphnode.rs"]
mod tests;
