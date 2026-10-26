//! The goal of a parent: what the truth says its children should
//! contain. For an occurrence's content, its 'contains' list in file
//! order (for a subscribee as such, less what the subscriber hides or
//! contains); for a node of the containerward chain under a folder's
//! member, its containers; for a folder, its partners in the folder's
//! relation, or the compound folders' derived lists; for a property
//! folder, the node's aliases, IDs or true flags. In the diff view the
//! goal also holds what HEAD alone warrants ('diff_view.rs'). Each
//! item names a gnid or a property value. A parent whose position
//! implies no relationship (the buffer root, a tombstone, an unknown)
//! has no goal.
//!
//! The goal computers it reuses: 'partnerFolder_members_in_graph',
//! the two compound folders' goal lists, 'unintegrated_content_gnids'
//! and 'omit_restricted_members'.

use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::in_rust_graph::relation_accessors::{NodeRelation, RelationRole};
use crate::graph_straddle::types::Partner;
use crate::refresh::Walk;
use crate::refresh::ancestry::required_ancestor_of;
use crate::refresh::diff_view;
use crate::refresh::extent::is_chain_parent;
use crate::to_org::complete::partner_folder::goal_list::{
  goal_list_for_hiddenInSubscribee_folder,
  goal_list_for_hiddenOutsideOfSubscribee_folder};
use crate::types::misc::{ID, RelPartner, RelationshipMemberKey, SkgrepoName};
use crate::types::nodes::complete::Flag;
use crate::types::nodes::rust::GraphnodeInRust;
use crate::types::tree::forest::in_content_position;
use crate::types::viewnode::{
  Existence, Folder, PropertyMember, Viewnode, ViewnodeKind, Vognode, Warrant};
use crate::update_buffer::reconcile::content::unintegrated_content_gnids;
use crate::update_buffer::reconcile::omit_restricted_members;
use crate::update_buffer::reconcile::partner_folder::partnerFolder_members_in_graph;

use ego_tree::{NodeId, NodeRef, Tree};
use std::collections::HashMap;
use std::iter::once;

/// What one goal item names.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub(crate) enum GoalMember {
  /// A node, by the gnid its parent's list resolves to: its pid, or
  /// the raw ID when nothing answers to it.
  Gnid (ID),
  Alias (String),
  Id (ID),
  Flag (Flag),
}

/// One item of a goal.
#[derive(Clone, Debug)]
pub(crate) struct GoalItem {
  pub member  : GoalMember,
  /// 'ByGraph' when the truth's part of the goal names it, 'ByHead'
  /// when only HEAD's part does.
  pub warrant : Warrant,
  /// For a gnid nothing answers to: its relationship's relRepo, when
  /// that differs from the recorder's home (the unknown's herald).
  pub relRepo : Option<SkgrepoName>,
  /// In the diff view, the item's existence change in its list.
  pub rel     : Option<Existence>,
  /// In the diff view, the partner the item is, over the straddle.
  pub partner : Option<Partner>,
}

/// A parent's goal: its items, and whether the children it warrants
/// take their order.
pub(crate) struct Goal {
  pub items   : Vec<GoalItem>,
  pub ordered : bool,
  index       : HashMap<GoalMember, usize>,
}

impl GoalItem {
  pub fn by_graph (
    member : GoalMember,
  ) -> GoalItem {
    GoalItem { member, warrant : Warrant::ByGraph,
               relRepo : None, rel : None, partner : None } }
}

impl Goal {
  pub fn new (
    items   : Vec<GoalItem>,
    ordered : bool,
  ) -> Goal {
    let mut unique : Vec<GoalItem> = Vec::with_capacity (items . len ());
    let mut index : HashMap<GoalMember, usize> = HashMap::new ();
    for item in items {
      if index . contains_key (&item . member) { continue; }
      index . insert ( item . member . clone (), unique . len () );
      unique . push (item); }
    Goal { items : unique, ordered, index } }

  pub fn item_for (
    &self,
    member : &GoalMember,
  ) -> Option<&GoalItem> {
    self . index . get (member) . map ( |i| &self . items [*i] ) }

  /// The position of 'member' in the goal's order.
  pub fn position_of (
    &self,
    member : &GoalMember,
  ) -> Option<usize> {
    self . index . get (member) . copied () }

  pub fn names_gnid (
    &self,
    gnid : &ID,
  ) -> bool {
    self . index . contains_key ( &GoalMember::Gnid ( gnid . clone () )) }
}

/// What a child stands for in its parent's goal, if anything: an
/// occurrence stands for the gnid its parent's list would hold (the
/// original of a shown overrider), a ghost or an unknown for its gnid,
/// a property member for its value.
pub(crate) fn member_of_child (
  viewnode : &Viewnode,
) -> Option<GoalMember> {
  match &viewnode . kind {
    ViewnodeKind::Vognode (Vognode::Occurrence (occurrence)) =>
      Some ( GoalMember::Gnid ( occurrence . collected_gnid () )),
    ViewnodeKind::Vognode (Vognode::Ghost (ghost)) =>
      Some ( GoalMember::Gnid ( ghost . gnid . clone () )),
    ViewnodeKind::Vognode (Vognode::Unknown (unknown)) =>
      Some ( GoalMember::Gnid ( unknown . gnid . clone () )),
    ViewnodeKind::PropertyMember (PropertyMember::Alias { text, .. }) =>
      Some ( GoalMember::Alias ( text . clone () )),
    ViewnodeKind::PropertyMember (PropertyMember::ID { gnid, .. }) =>
      Some ( GoalMember::Id ( gnid . clone () )),
    ViewnodeKind::PropertyMember (PropertyMember::Flag { flag, .. }) =>
      Some ( GoalMember::Flag (*flag) ),
    ViewnodeKind::Folder (_) | ViewnodeKind::ChangeViewnode (_)
      | ViewnodeKind::BufferRoot | ViewnodeKind::Tombstone => None, } }

/// The goal of the parent at 'parent', whose kind is settled. None
/// when its position implies no relationship.
pub(crate) fn goal_of_parent (
  tree   : &Tree<Viewnode>,
  parent : NodeId,
  walk   : &Walk,
) -> Option<Goal> {
  let node : NodeRef<Viewnode> = tree . get (parent) ?;
  match &node . value () . kind {
    ViewnodeKind::Vognode (Vognode::Occurrence (occurrence)) => {
      let pid : &ID = &occurrence . gnid;
      Some ( if is_chain_parent (tree, parent) {
               chain_goal (pid, walk)
             } else { content_goal (tree, parent, pid, walk) } ) },
    ViewnodeKind::Vognode (Vognode::Ghost (ghost))
      if walk . is_diff_view () && in_content_position (node) =>
      diff_view::ghost_content_goal (&ghost . gnid, walk),
    ViewnodeKind::Folder (folder) =>
      folder_goal (tree, parent, folder, walk),
    ViewnodeKind::Vognode (_) | ViewnodeKind::PropertyMember (_)
      | ViewnodeKind::ChangeViewnode (_)
      | ViewnodeKind::BufferRoot | ViewnodeKind::Tombstone => None, } }

/// Under a node of the containerward chain: its containers, in no
/// order of the graph's.
fn chain_goal (
  pid  : &ID,
  walk : &Walk,
) -> Goal {
  if let Some (goal) = diff_view::chain_goal (pid, walk) {
    return goal; }
  let graph : &InRustGraph = walk . truth . graph;
  let containers : Vec<ID> = omit_hidden (
    graph . inbound_pids_for_relation_gated (
      pid, NodeRelation::Contains, walk . truth . restriction ),
    walk );
  Goal::new ( containers . into_iter ()
              . map ( |gnid| GoalItem::by_graph (GoalMember::Gnid (gnid)) )
              . collect (),
              false ) }

/// An occurrence's content: its 'contains' list, its unresolved
/// members as raw IDs, less what the restriction hides; for a
/// subscribee as such, less what its subscriber hides or contains
/// ('unintegrated_content_gnids').
fn content_goal (
  tree   : &Tree<Viewnode>,
  parent : NodeId,
  pid    : &ID,
  walk   : &Walk,
) -> Goal {
  let graph : &InRustGraph = walk . truth . graph;
  let subscriber : Option<ID> = subscriber_of_subscribee_as_such (tree, parent);
  let unintegrated = |contents : Vec<ID>| -> Vec<ID> {
    match &subscriber {
      None => contents,
      Some (subscriber) => unintegrated_content_gnids (
        graph, &contents,
        &graph . outbound_gnids_for_relation_gated (
          subscriber, NodeRelation::HidesFromSubs, walk . truth . restriction ),
        &graph . outbound_gnids_for_relation_gated (
          subscriber, NodeRelation::Contains, walk . truth . restriction )), } };
  if let Some (goal) = diff_view::content_goal (pid, &unintegrated, walk) {
    return goal; }
  let members : Vec<RelPartner<RelationshipMemberKey>> =
    graph . outbound_member_keys_for_relation_gated (
      pid, NodeRelation::Contains, walk . truth . restriction );
  let relRepos : HashMap<ID, SkgrepoName> =
    unknown_members_relRepos (graph, pid, &members);
  let contents : Vec<ID> = omit_hidden (
    unintegrated ( members . into_iter ()
                   . map ( |member| member . member . into_id () )
                   . collect () ),
    walk );
  gnid_goal (contents, &relRepos, true) }

/// When the occurrence at 'treeid' is a subscribee as such (a member
/// of a subscribee folder that the folder's relation warrants), its
/// subscriber.
pub(crate) fn subscriber_of_subscribee_as_such (
  tree   : &Tree<Viewnode>,
  treeid : NodeId,
) -> Option<ID> {
  let node : NodeRef<Viewnode> = tree . get (treeid) ?;
  let ViewnodeKind::Vognode (Vognode::Occurrence (occurrence)) =
    &node . value () . kind else { return None; };
  if occurrence . viewStats . isWarranted != Warrant::ByGraph { return None; }
  let folder : NodeRef<Viewnode> = node . parent () ?;
  if ! matches! ( folder . value () . kind,
                  ViewnodeKind::Folder (Folder::Subscribee) ) {
    return None; }
  occurrence_pid ( folder . parent () ? ) }

/// A folder's goal, by kind.
fn folder_goal (
  tree   : &Tree<Viewnode>,
  folder : NodeId,
  kind   : &Folder,
  walk   : &Walk,
) -> Option<Goal> {
  let graph : &InRustGraph = walk . truth . graph;
  let ancestor_pid = |i : usize| -> Option<ID> {
    occurrence_pid ( tree . get ( required_ancestor_of (tree, folder, i) ? ) ? ) };
  match kind {
    Folder::Alias | Folder::ID | Folder::Flags { .. } =>
      property_goal (kind, &ancestor_pid (0) ?, walk),
    Folder::AcquiredBy | Folder::Acquired =>
      Some ( diff_view::acquisition_goal (tree, folder, kind, walk)
             . unwrap_or_else ( || Goal::new (Vec::new (), false) )),
    Folder::HiddenInSubscribee =>
      Some ( gnid_goal ( hidden_in_subscribee (
                           &ancestor_pid (0) ?, &ancestor_pid (2) ?, walk ),
                         &HashMap::new (), false )),
    Folder::HiddenOutsideOfSubscribee =>
      Some ( gnid_goal ( hidden_outside_of_subscribees (&ancestor_pid (1) ?, walk),
                         &HashMap::new (), true )),
    Folder::Subscribee | Folder::Subscriber
      | Folder::Overridden | Folder::Overrider
      | Folder::Hider | Folder::Hidden
      | Folder::Container | Folder::Mentioner => {
      let anchor : ID = ancestor_pid (0) ?;
      let member_role : RelationRole = kind . relation_member_role () ?;
      if let Some (goal) =
        diff_view::folder_goal (kind, &anchor, member_role, walk)
      { return Some (goal); }
      let members : Vec<ID> = partnerFolder_members_in_graph (
        graph, walk . truth . config, &anchor, kind, walk . truth . restriction ) ?;
      let relRepos : HashMap<ID, SkgrepoName> =
        if member_role . opposite_role () . is_first_role () {
          // The folder shows a list in the anchor's own file.
          unknown_members_relRepos (
            graph, &anchor,
            &graph . outbound_member_keys_for_relation_gated (
              &anchor, member_role . relation, walk . truth . restriction ))
        } else { HashMap::new () };
      Some ( gnid_goal (members, &relRepos, kind . goal_is_ordered ()) ) }, } }

/// What 'subscriber' hides in the content of 'subscribee', less what
/// the restriction hides: the goal of the compound folder
/// 'HiddenInSubscribee' under the subscribee as such.
pub(crate) fn hidden_in_subscribee (
  subscribee : &ID,
  subscriber : &ID,
  walk       : &Walk,
) -> Vec<ID> {
  let graph : &InRustGraph = walk . truth . graph;
  let goal : Vec<ID> = goal_list_for_hiddenInSubscribee_folder (
    graph,
    &graph . outbound_gnids_for_relation_gated (
      subscribee, NodeRelation::Contains, walk . truth . restriction ),
    &graph . outbound_gnids_for_relation_gated (
      subscriber, NodeRelation::HidesFromSubs, walk . truth . restriction ));
  resolve_all ( omit_hidden (goal, walk), graph ) }

/// What 'subscriber' hides outside the content of its subscribees,
/// less what the restriction hides: the goal of its compound folder
/// 'HiddenOutsideOfSubscribee', in the order of its hides list.
pub(crate) fn hidden_outside_of_subscribees (
  subscriber : &ID,
  walk       : &Walk,
) -> Vec<ID> {
  let graph : &InRustGraph = walk . truth . graph;
  let goal : Vec<ID> = goal_list_for_hiddenOutsideOfSubscribee_folder (
    graph,
    &graph . outbound_gnids_for_relation_gated (
      subscriber, NodeRelation::HidesFromSubs, walk . truth . restriction ),
    &graph . outbound_gnids_for_relation_gated (
      subscriber, NodeRelation::SubscribesTo, walk . truth . restriction ),
    walk . truth . config );
  resolve_all ( omit_hidden (goal, walk), graph ) }

/// A property folder's goal: the node's aliases (in order, each with
/// its relRepo when that differs from the node's home), its IDs (pid
/// first) or its true flags.
fn property_goal (
  kind : &Folder,
  pid  : &ID,
  walk : &Walk,
) -> Option<Goal> {
  if let Some (goal) = diff_view::property_goal (kind, pid, walk) {
    return Some (goal); }
  let node : &GraphnodeInRust = walk . truth . graph . get (pid) ?;
  Some ( match kind {
    Folder::Alias => Goal::new (
      node . aliases . or_default () . iter ()
        . map ( |alias| GoalItem {
          relRepo : ( alias . relRepo != node . home_skgrepo )
                    . then ( || alias . relRepo . clone () ),
          .. GoalItem::by_graph ( GoalMember::Alias ( alias . member . clone () )) } )
        . collect (),
      true ),
    Folder::ID => Goal::new (
      once ( node . pid . clone () )
        . chain ( node . extra_ids . iter () . cloned () )
        . map ( |id| GoalItem::by_graph (GoalMember::Id (id)) )
        . collect (),
      false ),
    _ => Goal::new (
      Flag::ALL . into_iter ()
        . filter ( |flag| node . flags . contains (flag) )
        . map ( |flag| GoalItem::by_graph (GoalMember::Flag (flag)) )
        . collect (),
      false ), } ) }

/// A goal of gnids, each warranted by the truth.
fn gnid_goal (
  gnids    : Vec<ID>,
  relRepos : &HashMap<ID, SkgrepoName>,
  ordered  : bool,
) -> Goal {
  Goal::new ( gnids . into_iter ()
              . map ( |gnid| GoalItem {
                relRepo : relRepos . get (&gnid) . cloned (),
                .. GoalItem::by_graph ( GoalMember::Gnid (gnid) ) } )
              . collect (),
              ordered ) }

/// The relRepos of the members of 'recorder''s list that resolve to no
/// node, where they differ from the recorder's home.
fn unknown_members_relRepos (
  graph    : &InRustGraph,
  recorder : &ID,
  members  : &[RelPartner<RelationshipMemberKey>],
) -> HashMap<ID, SkgrepoName> {
  let home : Option<SkgrepoName> =
    graph . get (recorder) . map ( |node| node . home_skgrepo . clone () );
  members . iter ()
    . filter ( |member| member . member . is_unresolved ()
                        && Some (&member . relRepo) != home . as_ref () )
    . map ( |member| ( member . member . id () . clone (),
                       member . relRepo . clone () ))
    . collect () }

/// Each ID as the graph resolves it: its pid, or itself when nothing
/// answers to it.
fn resolve_all (
  gnids : Vec<ID>,
  graph : &InRustGraph,
) -> Vec<ID> {
  gnids . into_iter ()
    . map ( |gnid| graph . relationship_member_key (&gnid) . into_id () )
    . collect () }

/// 'gnids' less what the skgrepo restriction hides.
fn omit_hidden (
  gnids : Vec<ID>,
  walk  : &Walk,
) -> Vec<ID> {
  let graph : &InRustGraph = walk . truth . graph;
  omit_restricted_members (
    gnids, walk . truth . restriction,
    |gnid : &ID| graph . pid_and_skgrepo (gnid) . map ( |(_, home)| home )) }

/// The pid of the occurrence 'node', if it is one.
pub(crate) fn occurrence_pid (
  node : NodeRef<Viewnode>,
) -> Option<ID> {
  match &node . value () . kind {
    ViewnodeKind::Vognode (Vognode::Occurrence (occurrence)) =>
      Some ( occurrence . gnid . clone () ),
    _ => None, } }
