//! The changes to a node's files that no change-forest leaf records,
//! so that no mark can cover them ('docs/diff-view.org', "Changes no
//! mark covers"). A commit of the processed leaves out a node that has
//! one, and says which.
//!
//! Two checks find them. The graph's ('unmarkable_changes_between'),
//! made when the change-forest is built, compares the two graphs'
//! versions of a node field by field, so a later field of
//! 'GraphnodeInRust' must be placed here before the code compiles.
//! The files' ('files_hold_more_than_their_node'), made only when a
//! commit of the processed chooses its nodes, compares each changed
//! '.skg' file, at HEAD and in the worktree, with what skg writes for
//! the node as that side's graph holds it: where both are what skg
//! writes, the files differ only as the two nodes do, which the graph's
//! check and the leaves cover; where one is not, the difference may
//! hold something no graph shows (formatting, say).

use crate::dbs::in_rust_graph::InRustGraph;
use crate::git_ops::stage_and_commit::{ChangedSkgPath, blobs_at_head_and_in_worktree};
use crate::telescope::decompose::{
  DecomposedTelescope, DecompositionInput, decompose_node};
use crate::types::misc::{ID, MSV, RelPartner, SkgConfig, SkgrepoName};
use crate::types::nodes::rust::GraphnodeInRust;

use git2::{ObjectType, Oid};
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::hash::Hash;

/// One way a node's files changed that no leaf records.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum UnmarkableChange {
  /// Its title or body moved between its home and a more private
  /// section: it gained or lost overPrivateText.
  TextSection,
  /// Its attribution (what a fork records of its original) changed.
  Attribution,
  /// An alias it has in both graphs is recorded in another skgrepo.
  AliasSkgrepo,
  /// The members both graphs give one of its lists stand in another
  /// order.
  Order (OrderedList),
  /// Something else the graph holds of it changed: an ID in a list
  /// replaced by another ID of the same node, or a list written empty
  /// where none was, or the reverse.
  OtherInGraph,
  /// One of its changed files, at HEAD or in the worktree, is not what
  /// skg writes for the node: a change in it may be one no graph shows
  /// (formatting, a comment, text in a section below its home).
  FileNotAsSkgWritesIt,
}

/// The lists of a node whose order a leaf does not record.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum OrderedList {
  Content,
  Forks,
  Hidden,
  Aliases,
  ExtraIds,
  Flags,
}

impl UnmarkableChange {
  /// The change in the user's words, as a commit's answer names it.
  pub fn in_words (
    self,
  ) -> &'static str {
    match self {
      UnmarkableChange::TextSection =>
        "its text moved between a private and a public section",
      UnmarkableChange::Attribution => "its attribution changed",
      UnmarkableChange::AliasSkgrepo => "an alias moved to another skgrepo",
      UnmarkableChange::Order (list) => match list {
        OrderedList::Content  => "the order of its content changed",
        OrderedList::Forks    => "the order of its forks changed",
        OrderedList::Hidden   => "the order of the nodes it hides changed",
        OrderedList::Aliases  => "the order of its aliases changed",
        OrderedList::ExtraIds => "the order of its IDs changed",
        OrderedList::Flags    => "the order of its flags changed", },
      UnmarkableChange::OtherInGraph =>
        "something else in its file changed",
      UnmarkableChange::FileNotAsSkgWritesIt =>
        "its file changed in a way the diff view does not show (formatting, say)", }}
}

/// What changed between 'before' and 'after', two versions of one
/// node, that no leaf records, in the order of 'UnmarkableChange'.
/// The leaves record its text, its home skgrepo, which aliases, extra
/// IDs and flags it has, and each relationship of its three lists that
/// was added, removed or moved to another skgrepo; 'linksTo' follows
/// from its text. Everything else is here.
pub fn unmarkable_changes_between (
  before_graph : &InRustGraph,
  after_graph  : &InRustGraph,
  before       : &GraphnodeInRust,
  after        : &GraphnodeInRust,
) -> Vec<UnmarkableChange> {
  let GraphnodeInRust {
    pid : _, home_skgrepo : _, title : _, body : _, linksTo : _,
    extra_ids : extra_ids_before,
    overPrivateText_telescope : overPrivateText_before,
    aliases : aliases_before,
    contains : contains_before,
    forks : forks_before,
    hides : hides_before,
    flags : flags_before,
    attribution : attribution_before, } = before;
  let GraphnodeInRust {
    pid : _, home_skgrepo : _, title : _, body : _, linksTo : _,
    extra_ids : extra_ids_after,
    overPrivateText_telescope : overPrivateText_after,
    aliases : aliases_after,
    contains : contains_after,
    forks : forks_after,
    hides : hides_after,
    flags : flags_after,
    attribution : attribution_after, } = after;
  let mut changes : BTreeSet<UnmarkableChange> = BTreeSet::new ();
  if overPrivateText_before != overPrivateText_after {
    changes . insert (UnmarkableChange::TextSection); }
  if attribution_before != attribution_after {
    changes . insert (UnmarkableChange::Attribution); }
  { // aliases: which texts it has is the alias leaves'
    let (old, new) : (&[RelPartner<String>], &[RelPartner<String>]) =
      ( aliases_before . or_default (), aliases_after . or_default () );
    let texts = |aliases : &[RelPartner<String>]| -> Vec<String> {
      aliases . iter () . map ( |alias| alias . member . clone () ) . collect () };
    if order_of_common_members_differs (&texts (old), &texts (new)) {
      changes . insert ( UnmarkableChange::Order (OrderedList::Aliases) ); }
    if relRepos_of_common_members_differ (old, new) {
      changes . insert (UnmarkableChange::AliasSkgrepo); }
    if specified_differs (aliases_before, aliases_after) && old == new {
      changes . insert (UnmarkableChange::OtherInGraph); }}
  if order_of_common_members_differs (extra_ids_before, extra_ids_after) {
    changes . insert ( UnmarkableChange::Order (OrderedList::ExtraIds) ); }
  if order_of_common_members_differs (flags_before, flags_after) {
    changes . insert ( UnmarkableChange::Order (OrderedList::Flags) ); }
  for (old, new, list) in [
    ( MSV::Specified ( contains_before . clone () ),
      MSV::Specified ( contains_after . clone () ), OrderedList::Content ),
    ( forks_before . clone (), forks_after . clone (), OrderedList::Forks ),
    ( hides_before . clone (), hides_after . clone (), OrderedList::Hidden ), ] {
    let (old_members, new_members) : (Vec<ID>, Vec<ID>) = (
      old . or_default () . iter () . map ( |m| m . member . clone () ) . collect (),
      new . or_default () . iter () . map ( |m| m . member . clone () ) . collect () );
    if order_of_common_members_differs (&old_members, &new_members) {
      changes . insert ( UnmarkableChange::Order (list) ); }
    if ids_replaced_by_ids_of_the_same_node (
      before_graph, after_graph, &old_members, &new_members )
      || ( specified_differs (&old, &new)
           && old . or_default () == new . or_default () ) {
      changes . insert (UnmarkableChange::OtherInGraph); }}
  changes . into_iter () . collect () }

/// Whether the members both lists hold stand in another order in one
/// than in the other, or one holds a member more often.
fn order_of_common_members_differs<T : Eq + Hash + Clone> (
  old : &[T],
  new : &[T],
) -> bool {
  let common = |list : &[T], other : &[T]| -> Vec<T> {
    list . iter () . filter ( |x| other . contains (x) ) . cloned () . collect () };
  common (old, new) != common (new, old) }

/// Whether some member both lists hold is recorded in other skgrepos
/// in one than in the other.
fn relRepos_of_common_members_differ<T : Eq + Hash + Clone + Ord> (
  old : &[RelPartner<T>],
  new : &[RelPartner<T>],
) -> bool {
  let relRepos_by_member = |list : &[RelPartner<T>]|
    -> HashMap<T, BTreeSet<SkgrepoName>> {
    let mut by_member : HashMap<T, BTreeSet<SkgrepoName>> = HashMap::new ();
    for partner in list {
      by_member . entry ( partner . member . clone () ) . or_default ()
        . insert ( partner . relRepo . clone () ); }
    by_member };
  let (old_by_member, new_by_member) = ( relRepos_by_member (old), relRepos_by_member (new) );
  old_by_member . iter () . any ( |(member, relRepos)|
    new_by_member . get (member) . is_some_and ( |other| other != relRepos )) }

fn specified_differs<T> (
  old : &MSV<T>,
  new : &MSV<T>,
) -> bool {
  matches! (old, MSV::Specified (_)) != matches! (new, MSV::Specified (_)) }

/// Whether a list lost an ID whose node it still names by another ID,
/// or gained one naming a node it named already: no relationship
/// changed, so no leaf records it. A member whose ID resolves to a node
/// in neither graph stands for itself.
fn ids_replaced_by_ids_of_the_same_node (
  before_graph : &InRustGraph,
  after_graph  : &InRustGraph,
  old          : &[ID],
  new          : &[ID],
) -> bool {
  let old_pids : BTreeSet<ID> =
    old . iter () . map ( |gnid| before_graph . pid_else_raw_id (gnid) ) . collect ();
  let new_pids : BTreeSet<ID> =
    new . iter () . map ( |gnid| after_graph . pid_else_raw_id (gnid) ) . collect ();
  old . iter () . filter ( |gnid| ! new . contains (gnid) )
    . any ( |gnid| new_pids . contains ( &before_graph . pid_else_raw_id (gnid) ))
  || new . iter () . filter ( |gnid| ! old . contains (gnid) )
    . any ( |gnid| old_pids . contains ( &after_graph . pid_else_raw_id (gnid) )) }

/// Whether some changed '.skg' file of the node 'pid' ('paths', those
/// Git lists as differing from HEAD) is, at HEAD or in the worktree,
/// other than what skg writes for the node as that side's graph holds
/// it. A side whose graph lacks the node is not asked at HEAD: a new
/// node has no file there, and a deleted node's files are all removed,
/// which its existence leaf records. A file that cannot be read, or a
/// node that cannot be decomposed, counts as other.
pub fn files_hold_more_than_their_node (
  config : &SkgConfig,
  before : Option<&GraphnodeInRust>,
  after  : Option<&GraphnodeInRust>,
  paths  : &[ChangedSkgPath],
) -> bool {
  let written_before : Option<BTreeMap<SkgrepoName, Oid>> =
    match (before, after) {
      (Some (node), Some (_)) => Some ( blobs_skg_writes (config, node) ),
      _ => None, };
  let written_after : Option<BTreeMap<SkgrepoName, Oid>> =
    after . map ( |node| blobs_skg_writes (config, node) );
  paths . iter () . any ( |path| {
    let Ok ((at_head, in_worktree)) : Result<(Option<Oid>, Option<Oid>), String> =
      blobs_at_head_and_in_worktree (config, path)
    else { return true; };
    let differs = |written : &Option<BTreeMap<SkgrepoName, Oid>>,
                   actual  : Option<Oid>| -> bool {
      written . as_ref () . is_some_and (
        |written| written . get (&path . skgrepo) . copied () != actual ) };
    differs (&written_before, at_head) || differs (&written_after, in_worktree) } ) }

/// The blob of each section skg writes for 'node', by skgrepo: its
/// decomposition, each section serialized as the writer does
/// ('prepare_graphnode_telescope'). Empty when it cannot be
/// decomposed, so that every file of it differs.
fn blobs_skg_writes (
  config : &SkgConfig,
  node   : &GraphnodeInRust,
) -> BTreeMap<SkgrepoName, Oid> {
  let decomposed : Option<DecomposedTelescope> = decompose_node (
    &DecompositionInput {
      pid         : &node . pid,
      extra_ids   : &node . extra_ids,
      flags       : &node . flags,
      attribution : node . attribution . as_ref (),
      title       : Some ( &node . title ),
      body        : node . body . as_deref (),
      home        : &node . home_skgrepo,
      aliases     : node . aliases . or_default (),
      contains    : &node . contains,
      forks       : node . forks . or_default (),
      hides       : node . hides . or_default (), },
    config ) . ok ();
  decomposed . into_iter ()
    . flat_map ( |decomposed| decomposed . into_sections () )
    . filter_map ( |(skgrepo, section)| {
      let yaml : String = section . to_yaml () . ok () ?;
      let blob : Oid = Oid::hash_object ( ObjectType::Blob, yaml . as_bytes () ) . ok () ?;
      Some (( skgrepo, blob )) } )
    . collect () }
