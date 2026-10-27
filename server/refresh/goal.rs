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
//! 'partnerFolder_members_in_graph' serves the search's enrichment
//! too.

use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::node_lookup::graphnode_from_graph;
use crate::dbs::in_rust_graph::relation_accessors::{NodeRelation, RelationRole};
use crate::graph_straddle::types::Partner;
use crate::refresh::Walk;
use crate::refresh::ancestry::{
  CompoundFolderParties, compound_folder_parties, described_node, required_ancestor_of};
use crate::refresh::diff_view;
use crate::refresh::extent::is_chain_parent;
use crate::skgrepo_sets::SkgrepoRestriction;
use crate::types::misc::{ID, RelPartner, RelationshipMemberKey, SkgrepoName, members_of};
use crate::types::nodes::complete::Flag;
use crate::types::nodes::rust::GraphnodeInRust;
use crate::types::tree::forest::in_content_position;
use crate::types::viewnode::{
  Existence, Folder, PropertyMember, Viewnode, ViewnodeKind, Vognode, Warrant};
use crate::util::setlike_vector_subtraction;

use ego_tree::{NodeId, NodeRef, Tree};
use std::collections::{HashMap, HashSet};

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
  /// A goal whose warranted children take its order.
  pub fn ordered (
    items : Vec<GoalItem>,
  ) -> Goal {
    Goal::with_order (items, true) }

  /// A goal whose warranted children keep the buffer's order.
  pub fn unordered (
    items : Vec<GoalItem>,
  ) -> Goal {
    Goal::with_order (items, false) }

  /// For a goal whose order is computed, as a folder kind's is.
  pub fn with_order (
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
  let containers : Vec<ID> = omit_restricted (
    graph . inbound_pids_for_relation_gated (
      pid, NodeRelation::Contains, walk . truth . restriction ),
    walk );
  Goal::unordered ( containers . into_iter ()
                    . map ( |gnid| GoalItem::by_graph (GoalMember::Gnid (gnid)) )
                    . collect () ) }

/// An occurrence's content ('content_goal_gnids'), with the relRepo
/// of each unknown member.
fn content_goal (
  tree   : &Tree<Viewnode>,
  parent : NodeId,
  pid    : &ID,
  walk   : &Walk,
) -> Goal {
  let graph : &InRustGraph = walk . truth . graph;
  let restriction : Option<&SkgrepoRestriction> = walk . truth . restriction;
  let subscriber : Option<ID> = subscriber_of_subscribee_as_such (tree, parent);
  let unintegrated : &dyn Fn (Vec<ID>) -> Vec<ID> = &|contents : Vec<ID>| -> Vec<ID> {
    match &subscriber {
      None => contents,
      Some (subscriber) => less_what_the_subscriber_integrates (
        graph, restriction, subscriber, &contents ), } };
  if let Some (goal) = diff_view::content_goal (pid, unintegrated, walk) {
    return goal; }
  let members : Vec<RelPartner<RelationshipMemberKey>> =
    graph . outbound_member_keys_for_relation_gated (
      pid, NodeRelation::Contains, restriction );
  let relRepos : HashMap<ID, SkgrepoName> =
    unknown_members_relRepos (graph, pid, &members);
  Goal::ordered ( gnid_items (
    content_goal_gnids ( graph, restriction, &members, subscriber . as_ref () ),
    &relRepos )) }

/// The content an occurrence shows outside the diff view: the members
/// of its 'contains' list ('members', a raw ID where nothing answers),
/// less what 'subscriber' hides or contains when the occurrence is a
/// subscribee as such, less the restricted members. The stats pass
/// counts a subscribee's unintegrated content from it, so that the
/// count is what is drawn.
pub(crate) fn content_goal_gnids (
  graph       : &InRustGraph,
  restriction : Option<&SkgrepoRestriction>,
  members     : &[RelPartner<RelationshipMemberKey>],
  subscriber  : Option<&ID>,
) -> Vec<ID> {
  let contents : Vec<ID> = members . iter ()
    . map ( |member| member . member . id () . clone () )
    . collect ();
  omit_restricted_members (
    match subscriber {
      None => contents,
      Some (subscriber) => less_what_the_subscriber_integrates (
        graph, restriction, subscriber, &contents ), },
    graph, restriction ) }

/// 'contents' less what 'subscriber' hides or contains, as the truth
/// has it ('unintegrated_content_gnids'): what a subscribee as such
/// shows of its content.
pub(crate) fn less_what_the_subscriber_integrates (
  graph       : &InRustGraph,
  restriction : Option<&SkgrepoRestriction>,
  subscriber  : &ID,
  contents    : &[ID],
) -> Vec<ID> {
  unintegrated_content_gnids (
    graph, contents,
    &graph . outbound_gnids_for_relation_gated (
      subscriber, NodeRelation::HidesFromSubs, restriction ),
    &graph . outbound_gnids_for_relation_gated (
      subscriber, NodeRelation::Contains, restriction )) }

/// What a subscribee as such shows of its content: its content less
/// what its subscriber hides or contains. Inputs preserve raw IDs; extra IDs are
/// resolved before comparing membership, without replacing overridden nodes.
fn unintegrated_content_gnids (
  graph               : &InRustGraph,
  content_gnids      : &[ID],
  subscriber_hidden   : &[ID],
  subscriber_contains : &[ID],
) -> Vec<ID> {
  let resolve_pids = |gnids : &[ID]| -> Vec<ID> {
    gnids . iter ()
      . map (|gnid| graph . pid_of (gnid) . unwrap_or_else (|| gnid . clone ()))
      . collect () };
  let contents : Vec<ID> = resolve_pids (content_gnids);
  let hidden : Vec<ID> = resolve_pids (subscriber_hidden);
  let contained : Vec<ID> = resolve_pids (subscriber_contains);
  setlike_vector_subtraction (
    setlike_vector_subtraction (contents, &hidden), &contained) }

/// When the occurrence at 'treeid' is a subscribee as such (a member
/// of a subscribee folder that the folder's relation warrants), its
/// subscriber.
pub(crate) fn subscriber_of_subscribee_as_such (
  tree   : &Tree<Viewnode>,
  treeid : NodeId,
) -> Option<ID> {
  let node : NodeRef<Viewnode> = tree . get (treeid) ?;
  let ViewnodeKind::Vognode (Vognode::Occurrence (occurrence)) : &ViewnodeKind =
    &node . value () . kind else { return None; };
  if occurrence . viewStats . warrant != Warrant::ByGraph { return None; }
  let folder : NodeRef<Viewnode> = node . parent () ?;
  if ! matches! ( folder . value () . kind,
                  ViewnodeKind::Folder (Folder::Subscribee) ) {
    return None; }
  folder . parent () ? . value () . occurrence_gnid () . cloned () }

/// A folder's goal, by kind.
fn folder_goal (
  tree   : &Tree<Viewnode>,
  folder : NodeId,
  kind   : &Folder,
  walk   : &Walk,
) -> Option<Goal> {
  let graph : &InRustGraph = walk . truth . graph;
  let occurrence_pid_of : &dyn Fn (NodeId) -> Option<ID> =
    &|treeid : NodeId| -> Option<ID> {
      tree . get (treeid) ? . value () . occurrence_gnid () . cloned () };
  let party_pid : &dyn Fn (usize) -> Option<ID> =
    &|index : usize| -> Option<ID> {
      occurrence_pid_of ( required_ancestor_of (tree, folder, index) ? ) };
  let described_pid : &dyn Fn () -> Option<ID> =
    &|| -> Option<ID> {
      occurrence_pid_of ( described_node (tree, folder) ? ) };
  match kind {
    Folder::Alias | Folder::ID | Folder::Flags { .. } =>
      property_goal (kind, &described_pid () ?, walk),
    Folder::AcquiredBy | Folder::Acquired =>
      Some ( diff_view::acquisition_goal (tree, folder, kind, walk)
             . unwrap_or_else ( || Goal::unordered (Vec::new ()) )),
    Folder::HiddenInSubscribee => {
      let parties : CompoundFolderParties = compound_folder_parties (kind) ?;
      Some ( Goal::unordered ( gnid_items (
        hidden_in_subscribee (
          &party_pid (parties . subscribee ?) ?,
          &party_pid (parties . subscriber) ?, walk ),
        &HashMap::new () ))) },
    Folder::HiddenOutsideOfSubscribee => {
      let parties : CompoundFolderParties = compound_folder_parties (kind) ?;
      Some ( Goal::ordered ( gnid_items (
        hidden_outside_of_subscribees (
          &party_pid (parties . subscriber) ?, walk ),
        &HashMap::new () ))) },
    Folder::Subscribee | Folder::Subscriber
      | Folder::Overridden | Folder::Overrider
      | Folder::Hider | Folder::Hidden
      | Folder::Container | Folder::Mentioner => {
      let described : ID = described_pid () ?;
      let member_role : RelationRole = kind . relation_member_role () ?;
      if let Some (goal) =
        diff_view::folder_goal (kind, &described, member_role, walk)
      { return Some (goal); }
      let members : Vec<ID> = partnerFolder_members_in_graph (
        graph, &described, kind, walk . truth . restriction ) ?;
      let relRepos : HashMap<ID, SkgrepoName> =
        if member_role . opposite_role () . is_first_role () {
          // The folder shows a list in the described node's own file.
          unknown_members_relRepos (
            graph, &described,
            &graph . outbound_member_keys_for_relation_gated (
              &described, member_role . relation, walk . truth . restriction ))
        } else { HashMap::new () };
      Some ( Goal::with_order ( gnid_items (members, &relRepos),
                                kind . goal_is_ordered () )) }, } }

/// The members a folder of this kind under the viewparent should hold,
/// from 'graph': its partners in the folder's relation, each extra ID
/// resolved to its node's pid (an unresolved ID kept as it is), less
/// the restricted members. In graph order. None if
/// the kind has no relation member role. The goal of such a folder in
/// a content view ('folder_goal').
pub(crate) fn partnerFolder_members_in_graph (
  graph               : &InRustGraph,
  viewparent_pid      : &ID,
  kind                : &Folder,
  skgrepo_restriction : Option<&SkgrepoRestriction>,
) -> Option<Vec<ID>> {
  let member_role : RelationRole = kind . relation_member_role () ?;
  let recorder_role : RelationRole =
    member_role . opposite_role ();
  let members : Vec<ID> =
    if recorder_role . is_first_role () { // the folder shows a list in the recorder's file
      raw_outbound_members (graph, viewparent_pid, member_role, skgrepo_restriction)
        . into_iter ()
        . map ( |member| member . member . into_id () )
        . collect ()
    } else { graph . other_member_pids_gated (
      viewparent_pid, recorder_role, skgrepo_restriction ) };
  Some ( // Under a skgrepo restriction (the API doc's "Restricted-skgrepo nodes"): these folders omit
         // restricted members. A tombstone child already in the folder
         // stays while it holds anything; the refresh's tail prunes it
         // once it holds nothing.
    omit_restricted_members (
      members, graph, skgrepo_restriction )) }

/// The viewparent's partners in the member role's relation, as its file
/// lists them, each tagged by whether the graph resolves it, if the
/// folder shows a list in the viewparent's file (the viewparent is the
/// recorder); else none.
fn raw_outbound_members (
  graph               : &InRustGraph,
  viewparent_pid      : &ID,
  member_role         : RelationRole,
  skgrepo_restriction : Option<&SkgrepoRestriction>,
) -> Vec<RelPartner<RelationshipMemberKey>> {
  if member_role . opposite_role () . is_first_role () {
    // Preserve an unresolved ID through this outbound surface.  The old
    // canonical-PID accessor is still right for inverse/write-protected folders, but
    // would erase an Unknown from the writable overriddenFolder.
    graph . outbound_member_keys_for_relation_gated (
      viewparent_pid, member_role . relation, skgrepo_restriction )
  } else { Vec::new () }}

/// What 'subscriber' hides in the content of 'subscribee', less the
/// restricted members: the goal of the compound folder
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
  resolve_all ( omit_restricted (goal, walk), graph ) }

/// Goal list for a hiddenInSubscribeeFolder: the intersection of the
/// subscriber's hides-list and the subscribee's contains-list, in the
/// order of the hides list.
fn goal_list_for_hiddenInSubscribee_folder (
  graph                : &InRustGraph,
  subscribee_contains : &[ID],
  subscriber_hides    : &[ID],
) -> Vec<ID> {
  let contains_set : HashSet<RelationshipMemberKey> =
    subscribee_contains . iter ()
    . map (|gnid| relationship_member_key (graph, gnid)) . collect ();
  subscriber_hides . iter ()
    . filter ( |gnid| contains_set . contains (&relationship_member_key (graph, gnid)) )
    . cloned () . collect () }

/// What 'subscriber' hides outside the content of its subscribees,
/// less the restricted members: the goal of its compound folder
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
      subscriber, NodeRelation::SubscribesTo, walk . truth . restriction ));
  resolve_all ( omit_restricted (goal, walk), graph ) }

/// Goal list for a hiddenOutsideOfSubscribeeFolder: the subscriber's
/// hides-list minus everything contained by any subscribee.
fn goal_list_for_hiddenOutsideOfSubscribee_folder (
  graph                : &InRustGraph,
  wt_subscriber_hides  : &[ID],
  wt_subscribees       : &[ID],
) -> Vec<ID> {
  let wt_subscribee_content_of = | pid : &ID | -> Vec<ID> {
    graphnode_from_graph (graph, pid)
      . map ( |skg| members_of (& skg . contains) )
      . unwrap_or_default () };
  let wt_all_subscribee_content : HashSet<RelationshipMemberKey> =
    wt_subscribees . iter ()
      . flat_map ( |pid| wt_subscribee_content_of (pid) )
      . map (|gnid| relationship_member_key (graph, &gnid))
      . collect ();
  wt_subscriber_hides . iter ()
    . filter ( |gnid| ! wt_all_subscribee_content
              . contains (&relationship_member_key (graph, gnid)) )
    . cloned () . collect () }

fn relationship_member_key (
  graph : &InRustGraph,
  gnid    : &ID,
) -> RelationshipMemberKey {
  graph . relationship_member_key (gnid)
}

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
    Folder::Alias => Goal::ordered (
      node . aliases_with_relRepo_unless_home () . into_iter ()
        . map ( |(text, relRepo)| GoalItem {
          relRepo, .. GoalItem::by_graph ( GoalMember::Alias (text) ) } )
        . collect () ),
    Folder::ID => Goal::unordered (
      node . ids () . into_iter ()
        . map ( |id| GoalItem::by_graph (GoalMember::Id (id)) )
        . collect () ),
    _ => Goal::unordered (
      Flag::ALL . into_iter ()
        . filter ( |flag| node . flags . contains (flag) )
        . map ( |flag| GoalItem::by_graph (GoalMember::Flag (flag)) )
        . collect () ), } ) }

/// Goal items of gnids, each warranted by the truth.
fn gnid_items (
  gnids    : Vec<ID>,
  relRepos : &HashMap<ID, SkgrepoName>,
) -> Vec<GoalItem> {
  gnids . into_iter ()
    . map ( |gnid| GoalItem {
      relRepo : relRepos . get (&gnid) . cloned (),
      .. GoalItem::by_graph ( GoalMember::Gnid (gnid) ) } )
    . collect () }

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

/// 'gnids' less the members the skgrepo restriction keeps out
/// ('omit_restricted_members').
fn omit_restricted (
  gnids : Vec<ID>,
  walk  : &Walk,
) -> Vec<ID> {
  omit_restricted_members (gnids, walk . truth . graph, walk . truth . restriction) }

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
fn omit_restricted_members (
  goal        : Vec<ID>,
  graph       : &InRustGraph,
  restriction : Option<&SkgrepoRestriction>,
) -> Vec<ID> {
  goal . into_iter ()
    . filter ( |gnid| ! graph . restriction_keeps_out (gnid, restriction) )
    . collect () }
