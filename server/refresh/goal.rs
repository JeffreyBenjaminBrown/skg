//! The goal of a parent: what the truth says its children should
//! contain. For an occurrence's content, its 'contains' list in file
//! order (for a forkee as such, less what the forker hides or
//! contains); for a node of the containerward chain under a folder's
//! member, its containers; for a folder, its partners in the folder's
//! relation (for the containers folder, those the view does not
//! already show above the node; for the forks folder, the notes it
//! forks, then those forking it that it neither forks nor hides), or
//! the compound folders' derived lists; for a property
//! folder, the node's aliases, IDs or true flags. In the diff view the
//! goal also holds what HEAD alone warrants ('diff_view.rs'). Each
//! item names a gnid or a property value. A parent whose position
//! implies no relationship (the buffer root, a tombstone, an unknown,
//! a restricted viewnode) has no goal.
//!
//! Under a skgrepo restriction (the API doc's "Restricted-skgrepo
//! nodes") a goal omits each member whose record the skgrepo restriction
//! keeps out: the gated accessors leave it out of every list read
//! here. It keeps each member whose record is visible: a node the
//! skgrepo restriction keeps out, which the ID rule makes a Restricted
//! vognode, and a gnid nothing answers to, an unknown, as under no
//! skgrepo restriction.
//!
//! 'partnerFolder_members_in_graph' serves the search's enrichment
//! too.

use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::in_rust_graph::relation_accessors::{GnidPresence, NodeRelation, RelationRole};
use crate::graph_straddle::types::Partner;
use crate::refresh::Walk;
use crate::refresh::ancestry::{
  CompoundFolderParties, compound_folder_parties, described_node,
  parent_reads_claims, view_ancestor_gnids};
use crate::refresh::diff_view;
use crate::refresh::extent::is_chain_parent;
use crate::skgrepo_sets::SkgrepoRestriction;
use crate::telescope::types::IllegalEdit;
use crate::types::misc::{ID, RelPartner, RelationshipMemberKey, SkgrepoName};
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
  /// For a gnid nothing answers to, or a restricted one: its
  /// relationship's relRepo, when that differs from the recorder's
  /// home (the unknown's herald, the restricted viewnode's).
  pub relRepo : Option<SkgrepoName>,
  /// In the diff view, the item's existence change in its list.
  pub rel     : Option<Existence>,
  /// In the diff view, the partner the item is, over the straddle.
  pub partner : Option<Partner>,
  /// Whether a child standing for it may claim membership: false for
  /// a forker-only member of a forks folder, whose relationship the
  /// member records, not the folder's viewparent. A child claiming
  /// such an item is unwarranted ('warrant_of_child'), and one drawn
  /// for it claims nothing ('draw_item').
  pub claimable : bool,
}

/// A parent's goal: its items, and whether the children it warrants
/// take their order.
pub(crate) struct Goal {
  pub items      : Vec<GoalItem>,
  pub ordered    : bool,
  index          : HashMap<GoalMember, usize>,
  /// Where an ordered goal's order ends: the children standing for
  /// items from here on all follow those standing for earlier ones,
  /// and keep, among themselves, the order the view has. The length of
  /// the items, but for a forks folder's goal, whose forker-only
  /// members no list orders ('Goal::ordered_then_unordered').
  unordered_from : usize,
}

impl GoalItem {
  pub fn by_graph (
    member : GoalMember,
  ) -> GoalItem {
    GoalItem { member, warrant : Warrant::ByGraph,
               relRepo : None, rel : None, partner : None,
               claimable : true } }
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
    let unordered_from : usize = unique . len ();
    Goal { items : unique, ordered, index, unordered_from } }

  /// A goal whose 'ordered' items lead, in their order, and whose
  /// 'unordered' items follow them all, in the order the view has
  /// them (a drawn one after those present). An item of 'unordered'
  /// that 'ordered' names already is dropped.
  pub fn ordered_then_unordered (
    ordered   : Vec<GoalItem>,
    unordered : Vec<GoalItem>,
  ) -> Goal {
    let mut goal : Goal = Goal::ordered (ordered);
    goal . unordered_from = goal . items . len ();
    for item in unordered {
      if goal . index . contains_key (&item . member) { continue; }
      goal . index . insert ( item . member . clone (), goal . items . len () );
      goal . items . push (item); }
    goal }

  pub fn item_for (
    &self,
    member : &GoalMember,
  ) -> Option<&GoalItem> {
    self . index . get (member) . map ( |i| &self . items [*i] ) }

  /// Whether the children standing for 'member' keep the order the
  /// view has: every item of an unordered goal, and those of an ordered
  /// one from 'unordered_from' on.
  pub fn keeps_the_views_order (
    &self,
    member : &GoalMember,
  ) -> bool {
    ! self . ordered
    || self . index . get (member)
       . is_some_and ( |position| *position >= self . unordered_from ) }

  /// The position of 'member' in the goal's order: one shared by
  /// every item from 'unordered_from' on.
  pub fn position_of (
    &self,
    member : &GoalMember,
  ) -> Option<usize> {
    self . index . get (member)
      . map ( |position| (*position) . min (self . unordered_from) ) }

  pub fn names_gnid (
    &self,
    gnid : &ID,
  ) -> bool {
    self . index . contains_key ( &GoalMember::Gnid ( gnid . clone () )) }

  /// The goal less each item naming one of 'gnids', in the same order.
  pub fn without_gnids (
    self,
    gnids : &HashSet<ID>,
  ) -> Goal {
    let ordered : bool = self . ordered;
    let unordered_from : usize = self . unordered_from;
    let keeps : &dyn Fn (&GoalItem) -> bool = &|item : &GoalItem| -> bool {
      ! matches! ( &item . member,
                   GoalMember::Gnid (gnid) if gnids . contains (gnid) ) };
    let mut leading   : Vec<GoalItem> = Vec::new ();
    let mut following : Vec<GoalItem> = Vec::new ();
    for (position, item) in self . items . into_iter () . enumerate () {
      if ! keeps (&item) { continue; }
      if position < unordered_from { leading . push (item); }
      else { following . push (item); }}
    if ordered { Goal::ordered_then_unordered (leading, following) }
    else { Goal::unordered ( leading . into_iter () . chain (following) . collect () ) } }
}

/// What a child stands for in its parent's goal, if anything: an
/// occurrence stands for the gnid its parent's list would hold (the
/// original of a shown overrider), a ghost or a gnid-only vognode for
/// its gnid, a property member for its value.
pub(crate) fn member_of_child (
  viewnode : &Viewnode,
) -> Option<GoalMember> {
  match &viewnode . kind {
    ViewnodeKind::Vognode (Vognode::Occurrence (occurrence)) =>
      Some ( GoalMember::Gnid ( occurrence . collected_gnid () )),
    ViewnodeKind::Vognode (Vognode::Ghost (ghost)) =>
      Some ( GoalMember::Gnid ( ghost . gnid . clone () )),
    ViewnodeKind::Vognode (Vognode::GnidOnly (gnid_only)) =>
      Some ( GoalMember::Gnid ( gnid_only . gnid . clone () )),
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
pub(crate) fn chain_goal (
  pid  : &ID,
  walk : &Walk,
) -> Goal {
  if let Some (goal) = diff_view::chain_goal (pid, walk) {
    return goal; }
  let containers : Vec<ID> =
    walk . truth . graph . inbound_pids_for_relation_gated (
      pid, NodeRelation::Contains, walk . truth . restriction );
  Goal::unordered ( gnid_items (containers, &HashMap::new ())) }

/// An occurrence's content ('content_goal_gnids'), with the relRepo
/// of each unknown or restricted member.
fn content_goal (
  tree   : &Tree<Viewnode>,
  parent : NodeId,
  pid    : &ID,
  walk   : &Walk,
) -> Goal {
  let graph : &InRustGraph = walk . truth . graph;
  let restriction : &SkgrepoRestriction = walk . truth . restriction;
  let forker : Option<ID> = forker_of_forkee_as_such (tree, parent);
  let unintegrated : &dyn Fn (Vec<ID>) -> Vec<ID> = &|contents : Vec<ID>| -> Vec<ID> {
    less_what_the_forker_integrates (
      graph, restriction, forker . as_ref (), &contents ) };
  if let Some (goal) = diff_view::content_goal (pid, unintegrated, walk) {
    return goal; }
  let members : Vec<RelPartner<RelationshipMemberKey>> =
    graph . outbound_member_keys_for_relation_gated (
      pid, NodeRelation::Contains, restriction );
  let relRepos : HashMap<ID, SkgrepoName> =
    unknown_or_restricted_members_relRepos (graph, restriction, pid, &members);
  Goal::ordered ( gnid_items (
    content_goal_gnids ( graph, restriction, &members, forker . as_ref () ),
    &relRepos )) }

/// The content an occurrence shows outside the diff view: the members
/// of its 'contains' list whose records the skgrepo restriction shows
/// ('members', gated, a raw ID where nothing answers), less what
/// 'forker' hides or contains when the occurrence is a forkee
/// as such. The restricted members among them are drawn as Restricted
/// vognodes. The stats pass counts a forkee's unintegrated content
/// from it, so that the count is what is drawn.
pub(crate) fn content_goal_gnids (
  graph       : &InRustGraph,
  restriction : &SkgrepoRestriction,
  members     : &[RelPartner<RelationshipMemberKey>],
  forker      : Option<&ID>,
) -> Vec<ID> {
  let contents : Vec<ID> = members . iter ()
    . map ( |member| member . member . id () . clone () )
    . collect ();
  less_what_the_forker_integrates (graph, restriction, forker, &contents) }

/// 'contents' less what 'forker' hides or contains, as the truth
/// has it ('unintegrated_content_gnids'): what a forkee as such
/// shows of its content. All of 'contents' when there is no
/// forker.
fn less_what_the_forker_integrates (
  graph       : &InRustGraph,
  restriction : &SkgrepoRestriction,
  forker      : Option<&ID>,
  contents    : &[ID],
) -> Vec<ID> {
  let Some (forker) : Option<&ID> = forker
    else { return contents . to_vec (); };
  unintegrated_content_gnids (
    graph, contents,
    &graph . outbound_gnids_for_relation_gated (
      forker, NodeRelation::Hides, restriction ),
    &graph . outbound_gnids_for_relation_gated (
      forker, NodeRelation::Contains, restriction )) }

/// What a forkee as such shows of its content: its content less
/// what its forker hides or contains. Inputs preserve raw IDs; extra IDs are
/// resolved before comparing membership, without replacing overridden nodes.
fn unintegrated_content_gnids (
  graph               : &InRustGraph,
  content_gnids      : &[ID],
  forker_hidden       : &[ID],
  forker_contains     : &[ID],
) -> Vec<ID> {
  let contents : Vec<ID> = resolve_all (content_gnids, graph);
  let hidden : Vec<ID> = resolve_all (forker_hidden, graph);
  let contained : Vec<ID> = resolve_all (forker_contains, graph);
  setlike_vector_subtraction (
    setlike_vector_subtraction (contents, &hidden), &contained) }

/// When the occurrence at 'treeid' is a forkee as such (a member of a
/// forks folder that the folder's relation warrants, as one its
/// viewparent forks), its forker, the folder's viewparent. Where the
/// folder reads its members' claims ('parent_reads_claims'), a member
/// claiming nothing is none: a forker-only member, or a forkee the user
/// detached.
pub(crate) fn forker_of_forkee_as_such (
  tree   : &Tree<Viewnode>,
  treeid : NodeId,
) -> Option<ID> {
  let node : NodeRef<Viewnode> = tree . get (treeid) ?;
  let ViewnodeKind::Vognode (Vognode::Occurrence (occurrence)) : &ViewnodeKind =
    &node . value () . kind else { return None; };
  if occurrence . viewStats . warrant != Warrant::ByGraph { return None; }
  let folder : NodeRef<Viewnode> = node . parent () ?;
  if ! matches! ( folder . value () . kind,
                  ViewnodeKind::Folder (Folder::Forks) ) {
    return None; }
  if parent_reads_claims (tree, folder . id ()) && ! occurrence . claimsMembership {
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
  let described_pid : &dyn Fn () -> Option<ID> =
    &|| -> Option<ID> {
      occurrence_pid_of ( described_node (tree, folder) ? ) };
  match kind {
    Folder::Alias | Folder::ID | Folder::Flags { .. } =>
      property_goal (kind, &described_pid () ?, walk),
    Folder::AcquiredBy | Folder::Acquired =>
      Some ( diff_view::acquisition_goal (tree, folder, kind, walk)
             . unwrap_or_else ( || Goal::unordered (Vec::new ()) )),
    Folder::IllegalEdit { .. } =>
      Some ( Goal::ordered ( gnid_items (
        visible_illegal_edits (&described_pid () ?, walk) . iter ()
          . flat_map ( |edit| edit . contains . iter () )
          . map ( |gnid| graph . pid_else_raw_id (gnid) )
          . collect (),
        &HashMap::new () ))),
    Folder::HiddenInForkPartner => {
      let parties : CompoundFolderParties = compound_folder_parties (tree, folder) ?;
      Some ( Goal::unordered ( gnid_items (
        hidden_in_forkee (
          &occurrence_pid_of (parties . forkee ?) ?,
          &occurrence_pid_of (parties . forker) ?, walk ),
        &HashMap::new ()))) },
    Folder::HiddenOutsideOfForkPartners => {
      let parties : CompoundFolderParties = compound_folder_parties (tree, folder) ?;
      Some ( Goal::ordered ( gnid_items (
        hidden_outside_of_forkees (
          &occurrence_pid_of (parties . forker) ?, walk ),
        &HashMap::new ()))) },
    Folder::Forks =>
      Some ( forks_folder_goal (&described_pid () ?, walk) ),
    Folder::Hider | Folder::Hidden
      | Folder::Container | Folder::Mentioner => {
      let goal : Goal = partnerFolder_goal (kind, &described_pid () ?, walk) ?;
      Some ( if kind . omits_view_ancestors () {
               goal . without_gnids ( &view_ancestor_gnids (
                 tree, described_node (tree, folder) ? ))
             } else { goal } ) }, } }

/// The goal of a partner folder of this kind describing 'described':
/// its partners over the straddle in the diff view, else in the graph.
fn partnerFolder_goal (
  kind      : &Folder,
  described : &ID,
  walk      : &Walk,
) -> Option<Goal> {
  let graph : &InRustGraph = walk . truth . graph;
  let member_role : RelationRole = kind . relation_member_role () ?;
  if let Some (goal) =
    diff_view::folder_goal (kind, described, member_role, walk)
  { return Some (goal); }
  let members : Vec<ID> = partnerFolder_members_in_graph (
    graph, described, kind, walk . truth . restriction ) ?;
  let relRepos : HashMap<ID, SkgrepoName> =
    if member_role . opposite_role () . is_first_role () {
      // The folder shows a list in the described node's own file.
      unknown_or_restricted_members_relRepos (
        graph, walk . truth . restriction, described,
        &graph . outbound_member_keys_for_relation_gated (
          described, member_role . relation, walk . truth . restriction ))
    } else { HashMap::new () };
  Some ( Goal::with_order ( gnid_items (members, &relRepos ),
                            kind . goal_is_ordered () )) }

/// The goal of the forks folder describing 'described', over the
/// straddle in the diff view, else in the graph: the notes it forks,
/// in its list's order, each claimable, with the relRepo of each
/// unknown or restricted one; then its forker-only members
/// ('forker_only_members'), claiming nothing, after them all in the
/// order the view has them.
fn forks_folder_goal (
  described : &ID,
  walk      : &Walk,
) -> Goal {
  if let Some (goal) = diff_view::forks_folder_goal (described, walk) {
    return goal; }
  let graph : &InRustGraph = walk . truth . graph;
  let restriction : &SkgrepoRestriction = walk . truth . restriction;
  let forkee_keys : Vec<RelPartner<RelationshipMemberKey>> =
    graph . outbound_member_keys_for_relation_gated (
      described, NodeRelation::Forks, restriction );
  let relRepos : HashMap<ID, SkgrepoName> =
    unknown_or_restricted_members_relRepos (graph, restriction, described, &forkee_keys);
  let forkees : Vec<ID> = forkee_keys . into_iter ()
    . map ( |member| member . member . into_id () )
    . collect ();
  let forker_only : Vec<ID> =
    forker_only_members (graph, restriction, described, &forkees);
  Goal::ordered_then_unordered (
    gnid_items (forkees, &relRepos),
    forker_only . into_iter ()
      . map ( |gnid| GoalItem { claimable : false,
                                .. GoalItem::by_graph ( GoalMember::Gnid (gnid) ) } )
      . collect () ) }

/// The forker-only members of the forks folder describing 'described':
/// the notes that fork it by records the skgrepo restriction shows,
/// in graph order, less those it forks ('forkees', as its list has
/// them) and those it hides by such records. A forker it hides stands
/// in the folder of what it hides outside its fork partners instead.
fn forker_only_members (
  graph       : &InRustGraph,
  restriction : &SkgrepoRestriction,
  described   : &ID,
  forkees     : &[ID],
) -> Vec<ID> {
  let left_out : HashSet<ID> =
    resolve_all (forkees, graph) . into_iter ()
    . chain ( resolve_all (
      &graph . outbound_gnids_for_relation_gated (
        described, NodeRelation::Hides, restriction ),
      graph ))
    . collect ();
  graph . inbound_pids_for_relation_gated (described, NodeRelation::Forks, restriction)
    . into_iter ()
    . filter ( |forker| ! left_out . contains (forker) )
    . collect () }

/// Whether 'pid' has a fork partner, either way, by a record the
/// skgrepo restriction shows: a note it forks, or one forking it
/// (hidden or not). Where it has, the forks folder is drawn unasked
/// ('add_default_folders').
pub(crate) fn has_a_fork_partner (
  graph       : &InRustGraph,
  restriction : &SkgrepoRestriction,
  pid         : &ID,
) -> bool {
  ! graph . outbound_member_keys_for_relation_gated (
      pid, NodeRelation::Forks, restriction ) . is_empty ()
  || ! graph . inbound_pids_for_relation_gated (
         pid, NodeRelation::Forks, restriction ) . is_empty () }

/// The members a folder of this kind under the viewparent should hold,
/// from 'graph': its partners in the folder's relation whose records
/// the skgrepo restriction shows, each extra ID resolved to its node's
/// pid (an unresolved ID kept as it is). In graph order; for the forks
/// folder, its forkees and then its forker-only members, as its goal
/// has them ('forks_folder_goal'). None if the kind has no relation
/// member role. The goal of such a folder in a content view
/// ('folder_goal').
pub(crate) fn partnerFolder_members_in_graph (
  graph               : &InRustGraph,
  viewparent_pid      : &ID,
  kind                : &Folder,
  skgrepo_restriction : &SkgrepoRestriction,
) -> Option<Vec<ID>> {
  if matches! ( kind, Folder::Forks ) {
    let forkees : Vec<ID> =
      graph . outbound_member_keys_for_relation_gated (
        viewparent_pid, NodeRelation::Forks, skgrepo_restriction )
      . into_iter ()
      . map ( |member| member . member . into_id () )
      . collect ();
    let forker_only : Vec<ID> = forker_only_members (
      graph, skgrepo_restriction, viewparent_pid, &forkees );
    return Some ( forkees . into_iter () . chain (forker_only) . collect () ); }
  let member_role : RelationRole = kind . relation_member_role () ?;
  let recorder_role : RelationRole =
    member_role . opposite_role ();
  let members : Vec<ID> =
    if recorder_role . is_first_role () { // the folder shows a list in the recorder's file
      // Preserve an unresolved ID through this outbound surface. The
      // canonical-pid accessor below is right for the inverse folders,
      // but would erase an unknown from the writable forksFolder.
      graph . outbound_member_keys_for_relation_gated (
        viewparent_pid, member_role . relation, skgrepo_restriction )
        . into_iter ()
        . map ( |member| member . member . into_id () )
        . collect ()
    } else { graph . other_member_pids_gated (
      viewparent_pid, recorder_role, skgrepo_restriction ) };
  Some (members) }

/// What 'forker' hides in the content of 'forkee', by the
/// records the skgrepo restriction shows: the goal of the compound
/// folder 'HiddenInForkPartner' under the forkee as such.
pub(crate) fn hidden_in_forkee (
  forkee     : &ID,
  forker     : &ID,
  walk       : &Walk,
) -> Vec<ID> {
  let graph : &InRustGraph = walk . truth . graph;
  let goal : Vec<ID> = goal_list_for_hiddenInForkPartner_folder (
    graph,
    &graph . outbound_gnids_for_relation_gated (
      forkee, NodeRelation::Contains, walk . truth . restriction ),
    &graph . outbound_gnids_for_relation_gated (
      forker, NodeRelation::Hides, walk . truth . restriction ));
  resolve_all (&goal, graph) }

/// Goal list for a hiddenInForkPartnerFolder: the intersection of the
/// forker's hides-list and the forkee's contains-list, in the
/// order of the hides list.
fn goal_list_for_hiddenInForkPartner_folder (
  graph                : &InRustGraph,
  forkee_contains     : &[ID],
  forker_hides        : &[ID],
) -> Vec<ID> {
  let contains_set : HashSet<ID> =
    resolve_all (forkee_contains, graph) . into_iter () . collect ();
  forker_hides . iter ()
    . filter ( |gnid| contains_set . contains (&graph . pid_else_raw_id (gnid)) )
    . cloned () . collect () }

/// What 'forker' hides outside the content of its forkees, by
/// the records the skgrepo restriction shows: the goal of its compound
/// folder 'HiddenOutsideOfForkPartners', in the order of its hides list.
/// A forkee's content is read by the same records, so a hide of
/// something a forkee contains only by a restricted record stands
/// here, outside, as it does in the view.
pub(crate) fn hidden_outside_of_forkees (
  forker     : &ID,
  walk       : &Walk,
) -> Vec<ID> {
  let graph : &InRustGraph = walk . truth . graph;
  let goal : Vec<ID> = goal_list_for_hiddenOutsideOfForkPartners_folder (
    graph,
    &graph . outbound_gnids_for_relation_gated (
      forker, NodeRelation::Hides, walk . truth . restriction ),
    &graph . outbound_gnids_for_relation_gated (
      forker, NodeRelation::Forks, walk . truth . restriction ),
    walk . truth . restriction );
  resolve_all (&goal, graph) }

/// Goal list for a hiddenOutsideOfForkPartnersFolder: the forker's
/// hides-list minus everything any forkee contains by a record
/// the skgrepo restriction shows.
fn goal_list_for_hiddenOutsideOfForkPartners_folder (
  graph             : &InRustGraph,
  forker_hides      : &[ID],
  forkees           : &[ID],
  restriction       : &SkgrepoRestriction,
) -> Vec<ID> {
  let forkee_content_of : &dyn Fn (&ID) -> Vec<ID> = &|forkee : &ID| -> Vec<ID> {
    graph . outbound_gnids_for_relation_gated (
      &graph . pid_else_raw_id (forkee), NodeRelation::Contains, restriction ) };
  let all_forkee_content : HashSet<ID> =
    forkees . iter ()
      . flat_map ( |pid| forkee_content_of (pid) )
      . map ( |gnid| graph . pid_else_raw_id (&gnid) )
      . collect ();
  forker_hides . iter ()
    . filter ( |gnid| ! all_forkee_content
              . contains (&graph . pid_else_raw_id (gnid)) )
    . cloned () . collect () }

/// A property folder's goal: the node's aliases whose records the
/// skgrepo restriction shows (in order, each with its relRepo when
/// that differs from the node's home), its IDs (pid first) or its true
/// flags.
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
      node . aliases_with_relRepo_unless_home (walk . truth . restriction) . into_iter ()
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

/// Goal items of gnids, each warranted by the truth, with its relRepo
/// from 'relRepos' where that holds one.
/// The illegal edits to the node 'pid' ('InRustGraph::illegal_edits')
/// in skgrepos the skgrepo restriction shows.
pub(crate) fn visible_illegal_edits (
  pid  : &ID,
  walk : &Walk,
) -> Vec<IllegalEdit> {
  walk . truth . graph . illegal_edits . get (pid) . into_iter () . flatten ()
    . filter ( |edit| SkgrepoRestriction::record_is_visible (
      walk . truth . restriction, &edit . skgrepo ))
    . cloned ()
    . collect () }

fn gnid_items (
  gnids    : Vec<ID>,
  relRepos : &HashMap<ID, SkgrepoName>,
) -> Vec<GoalItem> {
  gnids . into_iter ()
    . map ( |gnid| GoalItem {
      relRepo : relRepos . get (&gnid) . cloned (),
      .. GoalItem::by_graph ( GoalMember::Gnid (gnid) ) } )
    . collect () }

/// The relRepos of the members of 'recorder''s list that no
/// occurrence stands for, where they differ from the recorder's home:
/// those that resolve to no node, and those the skgrepo restriction
/// keeps out.
fn unknown_or_restricted_members_relRepos (
  graph       : &InRustGraph,
  restriction : &SkgrepoRestriction,
  recorder    : &ID,
  members     : &[RelPartner<RelationshipMemberKey>],
) -> HashMap<ID, SkgrepoName> {
  let home : Option<SkgrepoName> =
    graph . get (recorder) . map ( |node| node . home_skgrepo . clone () );
  let drawn_without_a_node : &dyn Fn (&RelationshipMemberKey) -> bool =
    &|member : &RelationshipMemberKey| -> bool {
      member . is_unresolved ()
      || graph . gnid_presence_gated (member . id (), restriction)
         == GnidPresence::Restricted };
  members . iter ()
    . filter ( |member| drawn_without_a_node (&member . member)
                        && Some (&member . relRepo) != home . as_ref () )
    . map ( |member| ( member . member . id () . clone (),
                       member . relRepo . clone () ))
    . collect () }

/// Each ID as the graph resolves it ('InRustGraph::pid_else_raw_id'),
/// the one key by which the goal compares members.
fn resolve_all (
  gnids : &[ID],
  graph : &InRustGraph,
) -> Vec<ID> {
  gnids . iter () . map ( |gnid| graph . pid_else_raw_id (gnid) ) . collect () }
