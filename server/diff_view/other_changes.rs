//! The changes to a node's files that no other change-forest leaf
//! records ('docs/diff-view.org', "Other changes to a node's file").
//!
//! The graph's ('other_changes_between'), made when the change-forest
//! is built, compares the two graphs' versions of a node field by
//! field, so a later field of 'GraphnodeInRust' must be placed here
//! before the code compiles. Together they are one leaf of the node,
//! its other changes, which a mark covers like any other; its
//! fingerprint hashes the values they compare
//! ('hash_other_changes_values'), so a later change of the same kind
//! unmarks it.
//!
//! The files' ('files_hold_more_than_their_node'), made only when a
//! commit of the processed chooses its nodes, compares each changed
//! '.skg' file, at HEAD and in the worktree, with what skg writes for
//! the node as that side's graph holds it: where both are what skg
//! writes, the files differ only as the two nodes do, which the leaves
//! cover; where one is not, the difference may hold something no graph
//! shows (formatting, say), which no mark covers.

use crate::dbs::in_rust_graph::InRustGraph;
use crate::git_ops::stage_and_commit::{
  ChangedSkgPath, blob_text, blobs_at_head_and_in_worktree};
use crate::telescope::decompose::{
  DecomposedTelescope, DecompositionInput, decompose_node};
use crate::types::misc::{ID, MSV, RelPartner, SkgConfig, SkgrepoName};
use crate::types::nodes::fs::GraphnodeOnDisk;
use crate::types::nodes::rust::GraphnodeInRust;
use crate::types::viewnode::{OrderedList, OtherChange};

use git2::{ObjectType, Oid};
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::hash::{Hash, Hasher};

/// A commit's words for a node it leaves out because one of its files
/// is not what skg writes for it ('files_hold_more_than_their_node').
pub const FILE_NOT_AS_SKG_WRITES_IT : &str =
  "its file changed in a way the diff view does not show (formatting, say)";

/// What changed between 'before' and 'after', two versions of one
/// node, that no other leaf records, in the order of 'OtherChange'.
/// The other leaves record its text, its home skgrepo, which aliases,
/// extra IDs and flags it has, and each relationship of its three
/// lists that was added, removed or moved to another skgrepo;
/// 'linksTo' follows from its text. Everything else is here.
pub fn other_changes_between (
  before_graph : &InRustGraph,
  after_graph  : &InRustGraph,
  before       : &GraphnodeInRust,
  after        : &GraphnodeInRust,
) -> Vec<OtherChange> {
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
  let mut changes : BTreeSet<OtherChange> = BTreeSet::new ();
  if overPrivateText_before != overPrivateText_after {
    changes . insert (OtherChange::TextSection); }
  if attribution_before != attribution_after {
    changes . insert (OtherChange::Attribution); }
  { // aliases: which texts it has is the alias leaves'
    let (old, new) : (&[RelPartner<String>], &[RelPartner<String>]) =
      ( aliases_before . or_default (), aliases_after . or_default () );
    let texts = |aliases : &[RelPartner<String>]| -> Vec<String> {
      aliases . iter () . map ( |alias| alias . member . clone () ) . collect () };
    if order_of_common_members_differs (&texts (old), &texts (new)) {
      changes . insert ( OtherChange::Order (OrderedList::Aliases) ); }
    if relRepos_of_common_members_differ (old, new) {
      changes . insert (OtherChange::AliasSkgrepo); }
    if specified_differs (aliases_before, aliases_after) && old == new {
      changes . insert (OtherChange::OtherInGraph); }}
  if order_of_common_members_differs (extra_ids_before, extra_ids_after) {
    changes . insert ( OtherChange::Order (OrderedList::ExtraIds) ); }
  if order_of_common_members_differs (flags_before, flags_after) {
    changes . insert ( OtherChange::Order (OrderedList::Flags) ); }
  for (old, new, list) in [
    ( MSV::Specified ( contains_before . clone () ),
      MSV::Specified ( contains_after . clone () ), OrderedList::Content ),
    ( forks_before . clone (), forks_after . clone (), OrderedList::Forks ),
    ( hides_before . clone (), hides_after . clone (), OrderedList::Hidden ), ] {
    let (old_members, new_members) : (Vec<ID>, Vec<ID>) = (
      old . or_default () . iter () . map ( |m| m . member . clone () ) . collect (),
      new . or_default () . iter () . map ( |m| m . member . clone () ) . collect () );
    if order_of_common_members_differs (&old_members, &new_members) {
      changes . insert ( OtherChange::Order (list) ); }
    if ids_replaced_by_ids_of_the_same_node (
      before_graph, after_graph, &old_members, &new_members )
      || ( specified_differs (&old, &new)
           && old . or_default () == new . or_default () ) {
      changes . insert (OtherChange::OtherInGraph); }}
  changes . into_iter () . collect () }

/// Hash into 'state' the values each of 'changes' compares in 'before'
/// and 'after', two versions of one node: what the fingerprint of its
/// other-changes leaf covers. A section move, its overPrivateText; an
/// attribution change, its attribution; an alias moved, the skgrepos
/// of the aliases both versions have; a reorder, the order of the
/// members both give that list ('common_members_in_order'); anything
/// else, its four lists whole. So the mark lapses when a later change
/// of the same kind changes them, and not when the node changes
/// otherwise (a member added to a reordered list, say), which its
/// other leaves record.
pub fn hash_other_changes_values<H : Hasher> (
  changes : &[OtherChange],
  before  : &GraphnodeInRust,
  after   : &GraphnodeInRust,
  state   : &mut H,
) {
  for change in changes {
    change . hash (state);
    match change {
      OtherChange::TextSection =>
        ( before . overPrivateText_telescope,
          after  . overPrivateText_telescope ) . hash (state),
      OtherChange::Attribution =>
        ( &before . attribution, &after . attribution ) . hash (state),
      OtherChange::AliasSkgrepo => {
        let relRepos_by_alias = |node : &GraphnodeInRust|
          -> BTreeMap<String, BTreeSet<SkgrepoName>> {
          let mut by_alias : BTreeMap<String, BTreeSet<SkgrepoName>> =
            BTreeMap::new ();
          for alias in node . aliases . or_default () {
            by_alias . entry ( alias . member . clone () ) . or_default ()
              . insert ( alias . relRepo . clone () ); }
          by_alias };
        let (old, new) = ( relRepos_by_alias (before), relRepos_by_alias (after) );
        for (text, relRepos) in &old {
          if let Some (other) = new . get (text) {
            ( text, relRepos, other ) . hash (state); }} },
      OtherChange::Order (list) =>
        common_members_in_order (*list, before, after) . hash (state),
      OtherChange::OtherInGraph =>
        format! ( "{:?}", ( &before . aliases, &before . contains,
                            &before . forks, &before . hides,
                            &after . aliases, &after . contains,
                            &after . forks, &after . hides ) )
        . hash (state), }} }

/// The members of the list 'list' of 'node', in order, each by its
/// key: an ID, an alias's text, a flag's wire name.
pub fn members_in_order (
  list : OrderedList,
  node : &GraphnodeInRust,
) -> Vec<String> {
  let ids = |partners : &[RelPartner<ID>]| -> Vec<String> {
    partners . iter () . map ( |partner| partner . member . 0 . clone () ) . collect () };
  match list {
    OrderedList::Content  => ids (&node . contains),
    OrderedList::Forks    => ids ( node . forks . or_default () ),
    OrderedList::Hidden   => ids ( node . hides . or_default () ),
    OrderedList::Aliases  => node . aliases . or_default () . iter ()
      . map ( |alias| alias . member . clone () ) . collect (),
    OrderedList::ExtraIds => node . extra_ids . iter ()
      . map ( |id| id . 0 . clone () ) . collect (),
    OrderedList::Flags    => node . flags . iter ()
      . map ( |flag| flag . wire_name () . to_string () ) . collect (), } }

/// The members both versions give the list 'list', in the order of
/// each: (before's, after's). They differ exactly when the list was
/// reordered.
pub fn common_members_in_order (
  list   : OrderedList,
  before : &GraphnodeInRust,
  after  : &GraphnodeInRust,
) -> (Vec<String>, Vec<String>) {
  let (old, new) = ( members_in_order (list, before), members_in_order (list, after) );
  let common = |list : &[String], other : &[String]| -> Vec<String> {
    list . iter () . filter ( |x| other . contains (x) ) . cloned () . collect () };
  ( common (&old, &new), common (&new, &old) ) }

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
/// which its existence leaf records. A side whose node keeps its text
/// in a section more private than its home (overPrivateText) is one
/// skg does not write (it writes the text at the home), and its graph
/// does not say which section holds the text, so each file of that
/// side is asked only whether it is a section as skg writes one
/// ('section_is_as_skg_writes_it'); where the text moved, its other
/// changes say so ('OtherChange::TextSection'). A file that cannot be
/// read, or a node that cannot be decomposed, counts as other.
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
    let differs = |node    : Option<&GraphnodeInRust>,
                   written : &Option<BTreeMap<SkgrepoName, Oid>>,
                   actual  : Option<Oid>| -> bool {
      match (node, written) {
        (Some (node), Some (_)) if node . overPrivateText_telescope =>
          actual . is_some_and ( |blob|
            ! blob_text (config, &path . skgrepo, blob)
              . is_ok_and ( |text| section_is_as_skg_writes_it (&text) )),
        _ => written . as_ref () . is_some_and (
          |written| written . get (&path . skgrepo) . copied () != actual ), } };
    differs (before, &written_before, at_head)
      || differs (after, &written_after, in_worktree) } ) }

/// Whether 'text' is a section as skg writes one: what its parse
/// serializes to ('GraphnodeOnDisk::to_yaml').
fn section_is_as_skg_writes_it (
  text : &str,
) -> bool {
  yaml_serde::from_str::<GraphnodeOnDisk> (text) . ok ()
    . and_then ( |section| section . to_yaml () . ok () )
    . is_some_and ( |written| written == text ) }

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
