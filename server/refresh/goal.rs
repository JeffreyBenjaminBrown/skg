//! The goal of a parent: what the truth says its children should
//! contain. For an occurrence's content, its 'contains' list in file
//! order (for a member as such of a forks folder, a forkee or a
//! forker as such, less what the note whose folder holds it hides or
//! contains, and less what an owned note but the member contains:
//! 'docs/glossary.org', "what a member shows"); for a node of the
//! containerward chain under a folder's
//! member, its containers; for a folder, its partners in the folder's
//! relation (for the containers folder, those the view does not
//! already show above the node; for the forks folder, every mirror
//! of it: the notes it forks, then those forking it that it neither
//! forks nor hides, then its other mirrors that it does not hide), or
//! the compound folders' derived lists (among them the member's
//! content the user keeps elsewhere, 'integrated_elsewhere'); for a property
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
use crate::dbs::in_rust_graph::mirrors::{Mirror, mirrors_of_note_forking};
use crate::dbs::in_rust_graph::relation_accessors::{GnidPresence, NodeRelation, RelationRole};
use crate::graph_straddle::types::Partner;
use crate::refresh::Walk;
use crate::refresh::ancestry::{
  CompoundFolderParties, compound_folder_parties, described_node,
  view_ancestor_gnids};
use crate::refresh::diff_view;
use crate::refresh::extent::is_chain_parent;
use crate::skgrepo_sets::SkgrepoRestriction;
use crate::telescope::types::IllegalEdit;
use crate::types::misc::{ID, RelPartner, RelationshipMemberKey, SkgConfig, SkgrepoName};
use crate::types::nodes::complete::Flag;
use crate::types::nodes::rust::GraphnodeInRust;
use crate::types::tree::forest::in_content_position;
use crate::types::viewnode::{
  Existence, Folder, PropertyMember, Viewnode, ViewnodeKind, Vognode, Warrant};
use crate::util::setlike_vector_subtraction;

use ego_tree::{NodeId, NodeRef, Tree};
use std::collections::{HashMap, HashSet};
use std::iter::once;
use std::slice::from_ref;

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
  /// a member of a forks folder that the folder's viewparent does not
  /// fork (a forker-only member, whose relationship the member
  /// records, or an indirect member, joined to the viewparent only
  /// through other mirrors). A child claiming
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
  /// the items, but for the diff view's forks folder goal, whose
  /// forker-only members no list orders ('Goal::ordered_then_unordered').
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
/// original of a shown mirror), a ghost or a gnid-only vognode for
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
      Some ( if is_chain_parent (tree, parent, walk . is_diff_view ()) {
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
  let config : &SkgConfig = walk . truth . config;
  let restriction : &SkgrepoRestriction = walk . truth . restriction;
  let note : Option<ID> = note_of_member_as_such (tree, parent);
  let unintegrated : &dyn Fn (Vec<ID>) -> Vec<ID> = &|contents : Vec<ID>| -> Vec<ID> {
    // The diff view draws no folder of what the user keeps
    // elsewhere, so there a member keeps it among its content.
    less_what_the_note_integrates (
      graph, config, restriction, pid, note . as_ref (), &contents,
      ! walk . is_diff_view () ) };
  if let Some (goal) = diff_view::content_goal (pid, unintegrated, walk) {
    return goal; }
  let members : Vec<RelPartner<RelationshipMemberKey>> =
    graph . outbound_member_keys_for_relation_gated (
      pid, NodeRelation::Contains, restriction );
  let relRepos : HashMap<ID, SkgrepoName> =
    unknown_or_restricted_members_relRepos (graph, restriction, pid, &members);
  Goal::ordered ( gnid_items (
    content_goal_gnids ( graph, config, restriction, pid, &members, note . as_ref (), true ),
    &relRepos )) }

/// The content the occurrence of 'member' shows: the members of its
/// 'contains' list whose records the skgrepo restriction shows
/// ('members', gated, a raw ID where nothing answers), less what
/// 'note' hides or contains and, where 'kept_elsewhere_counts' (outside
/// the diff view, which draws no folder of it), what the user keeps
/// elsewhere, when the occurrence is a member as such of the note's
/// forks folder (a forkee or forker as such). The restricted members
/// among them are drawn as Restricted vognodes. The stats pass counts
/// a member's unintegrated content from it, so that the count is what
/// is drawn.
pub(crate) fn content_goal_gnids (
  graph                 : &InRustGraph,
  config                : &SkgConfig,
  restriction           : &SkgrepoRestriction,
  member                : &ID,
  members               : &[RelPartner<RelationshipMemberKey>],
  note                  : Option<&ID>,
  kept_elsewhere_counts : bool,
) -> Vec<ID> {
  let contents : Vec<ID> = members . iter ()
    . map ( |member| member . member . id () . clone () )
    . collect ();
  less_what_the_note_integrates (
    graph, config, restriction, member, note, &contents, kept_elsewhere_counts ) }

/// 'contents', the content of 'member', less what 'note' hides or
/// contains and, where 'kept_elsewhere_counts', less what an owned
/// note but 'member' contains ('owned_notes_containing'), as the truth
/// has it ('unintegrated_content_gnids'): what a member as such of the
/// note's forks folder shows of its content ('docs/glossary.org',
/// "what a member shows"), whichever role the member plays there. All
/// of 'contents' when there is no note.
fn less_what_the_note_integrates (
  graph                 : &InRustGraph,
  config                : &SkgConfig,
  restriction           : &SkgrepoRestriction,
  member                : &ID,
  note                  : Option<&ID>,
  contents              : &[ID],
  kept_elsewhere_counts : bool,
) -> Vec<ID> {
  let Some (note) : Option<&ID> = note
    else { return contents . to_vec (); };
  unintegrated_content_gnids (
    graph, contents,
    &graph . outbound_gnids_for_relation_gated (
      note, NodeRelation::Hides, restriction ),
    &graph . outbound_gnids_for_relation_gated (
      note, NodeRelation::Contains, restriction ),
    &|gnid : &ID| -> bool {
      kept_elsewhere_counts
      && ! owned_notes_containing (
             graph, config, restriction, gnid,
             from_ref (member), None ) . is_empty () } ) }

/// What a member as such shows of its content: its content less
/// what the note whose forks folder holds it hides or contains, and
/// less what 'kept_elsewhere' says the user keeps in another owned
/// note. Inputs preserve raw IDs; extra IDs are resolved before
/// comparing membership, without substituting mirrors.
fn unintegrated_content_gnids (
  graph               : &InRustGraph,
  content_gnids      : &[ID],
  note_hides          : &[ID],
  note_contains       : &[ID],
  kept_elsewhere      : &dyn Fn (&ID) -> bool,
) -> Vec<ID> {
  let contents : Vec<ID> = resolve_all (content_gnids, graph);
  let hidden : Vec<ID> = resolve_all (note_hides, graph);
  let contained : Vec<ID> = resolve_all (note_contains, graph);
  setlike_vector_subtraction (
    setlike_vector_subtraction (contents, &hidden), &contained)
    . into_iter ()
    . filter ( |gnid| ! kept_elsewhere (gnid) )
    . collect () }

/// The notes a save writes, as the question which owned notes
/// contain a node after it reads them ('owned_notes_containing'): each
/// note it saves with its contains specified, by its pid, with its
/// home and those contents, each resolved
/// ('InRustGraph::pid_else_raw_id'); and the notes it deletes. A note
/// it names in neither stands as the graph has it.
#[derive(Default)]
pub(crate) struct ContainsAfterSave {
  pub saved   : Vec<(ID, SkgrepoName, HashSet<ID>)>,
  pub deleted : HashSet<ID>,
}

impl ContainsAfterSave {
  /// The contents the save leaves to 'pid', if it saves that note's
  /// contains.
  fn contents_saved_for (
    &self,
    pid : &ID,
  ) -> Option<&HashSet<ID>> {
    self . saved . iter ()
      . find ( |(saved, _, _)| saved == pid )
      . map ( |(_, _, contents)| contents ) }
}

/// The notes of the user's graph ('docs/glossary.org', "integrated")
/// that contain 'gnid', but those 'excepted' names: notes homed in an
/// owned skgrepo, each shown by the skgrepo restriction, whose record
/// of 'gnid' it shows. As the graph has them, or, given 'after_save',
/// as that save leaves them: a note it saves by its contains then, a
/// note it deletes not at all, any other as the graph has it. The
/// graph's containers first, in its order, then those the save adds.
/// IDs are compared resolved, without substituting mirrors: a note
/// containing a mirror of 'gnid' does not contain 'gnid'.
pub(crate) fn owned_notes_containing (
  graph       : &InRustGraph,
  config      : &SkgConfig,
  restriction : &SkgrepoRestriction,
  gnid        : &ID,
  excepted    : &[ID],
  after_save  : Option<&ContainsAfterSave>,
) -> Vec<ID> {
  let target : ID = graph . pid_else_raw_id (gnid);
  let excepted : HashSet<ID> = resolve_all (excepted, graph) . into_iter () . collect ();
  let unwritten : ContainsAfterSave = ContainsAfterSave::default ();
  let save : &ContainsAfterSave = after_save . unwrap_or (&unwritten);
  let in_the_graph : Vec<ID> =
    graph . inbound_pids_for_relation_gated (&target, NodeRelation::Contains, restriction)
    . into_iter ()
    . filter ( |container| ! excepted . contains (container)
                           && ! save . deleted . contains (container)
                           && graph . node_is_shown (container, restriction)
                           && graph . get (container) . is_some_and (
                                |node| config . skgrepo_is_owned (&node . home_skgrepo) )
                           && save . contents_saved_for (container)
                              . is_none_or ( |contents| contents . contains (&target) ) )
    . collect ();
  let added_by_the_save : Vec<ID> =
    save . saved . iter ()
    . filter ( |(pid, home, contents)|
               contents . contains (&target)
               && ! excepted . contains (pid)
               && ! in_the_graph . contains (pid)
               && config . skgrepo_is_owned (home)
               && ! SkgrepoRestriction::node_is_restricted (restriction, home) )
    . map ( |(pid, _, _)| pid . clone () )
    . collect ();
  in_the_graph . into_iter () . chain (added_by_the_save) . collect () }

/// What 'member', a member as such of the forks folder of 'note',
/// contains that the user keeps elsewhere: the goal of its folder
/// 'Folder::IntegratedElsewhere'. Its content by the records the
/// skgrepo restriction shows that 'note' contains, or that an owned
/// note but 'member' contains ('owned_notes_containing'), less what
/// 'note' hides (which stands in the member's folder of what the note
/// hides in it instead), in the member's list's order.
pub(crate) fn integrated_elsewhere (
  member : &ID,
  note   : &ID,
  walk   : &Walk,
) -> Vec<ID> {
  let graph : &InRustGraph = walk . truth . graph;
  let restriction : &SkgrepoRestriction = walk . truth . restriction;
  let contents : Vec<ID> = resolve_all (
    &graph . outbound_gnids_for_relation_gated (
      member, NodeRelation::Contains, restriction ),
    graph );
  let hidden : HashSet<ID> = resolve_all (
    &graph . outbound_gnids_for_relation_gated (
      note, NodeRelation::Hides, restriction ),
    graph ) . into_iter () . collect ();
  let contained_by_the_note : HashSet<ID> = resolve_all (
    &graph . outbound_gnids_for_relation_gated (
      note, NodeRelation::Contains, restriction ),
    graph ) . into_iter () . collect ();
  contents . into_iter ()
    . filter ( |gnid| ! hidden . contains (gnid) )
    . filter ( |gnid| contained_by_the_note . contains (gnid)
                      || ! owned_notes_containing (
                             graph, walk . truth . config, restriction, gnid,
                             from_ref (member), None ) . is_empty () )
    . collect () }

/// When the occurrence at 'treeid' is a member as such of a forks
/// folder (a member the folder's goal warrants: a forkee as such,
/// which claims, its viewparent forking it, or a forker as such,
/// which claims nothing: in a view the refresh draws, a forker-only
/// or an indirect member, which the viewparent does not fork),
/// the note whose folder holds it, the folder's viewparent. Either
/// shows its content less what that note contains or hides.
pub(crate) fn note_of_member_as_such (
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
  folder . parent () ? . value () . occurrence_gnid () . cloned () }

/// When the occurrence at 'treeid' is a forker as such (a member as
/// such claiming nothing, 'note_of_member_as_such': a forker-only or
/// an indirect member), the note whose forks folder holds it.
pub(crate) fn note_of_forker_as_such (
  tree   : &Tree<Viewnode>,
  treeid : NodeId,
) -> Option<ID> {
  let ViewnodeKind::Vognode (Vognode::Occurrence (occurrence)) : &ViewnodeKind =
    &tree . get (treeid) ? . value () . kind else { return None; };
  if occurrence . claimsMembership { return None; }
  note_of_member_as_such (tree, treeid) }

/// When the occurrence at 'treeid' is a forker as such that forks the
/// note whose forks folder holds it, by a record the skgrepo
/// restriction shows, that note: where the folder of what it hides of
/// the note's content ('Folder::HiddenByForker') may stand. An
/// indirect member claims nothing too, but forks no such note, and
/// its hides do not apply to the note's content.
pub(crate) fn note_forked_by_forker_as_such (
  tree   : &Tree<Viewnode>,
  treeid : NodeId,
  walk   : &Walk,
) -> Option<ID> {
  let note : ID = note_of_forker_as_such (tree, treeid) ?;
  let forker : ID = tree . get (treeid) ? . value () . occurrence_gnid () ? . clone ();
  let graph : &InRustGraph = walk . truth . graph;
  let note_pid : ID = graph . pid_else_raw_id (&note);
  graph . outbound_gnids_for_relation_gated (
    &forker, NodeRelation::Forks, walk . truth . restriction )
    . iter () . any ( |forkee| graph . pid_else_raw_id (forkee) == note_pid )
    . then_some (note) }

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
    Folder::IntegratedElsewhere => {
      let parties : CompoundFolderParties = compound_folder_parties (tree, folder) ?;
      Some ( Goal::ordered ( gnid_items (
        integrated_elsewhere (
          &occurrence_pid_of (parties . container ?) ?,
          &occurrence_pid_of (parties . hider) ?, walk ),
        &HashMap::new ()))) },
    Folder::HiddenInForkPartner => {
      let parties : CompoundFolderParties = compound_folder_parties (tree, folder) ?;
      Some ( Goal::unordered ( gnid_items (
        hidden_in_fork_partner (
          &occurrence_pid_of (parties . container ?) ?,
          &occurrence_pid_of (parties . hider) ?, walk ),
        &HashMap::new ()))) },
    Folder::HiddenByForker => {
      // Under a forker as such that forks the note only: under a
      // forkee as such (a forker the user made a forkee) or an
      // indirect member it holds nothing, and goes.
      let parties : CompoundFolderParties = compound_folder_parties (tree, folder) ?;
      Some ( Goal::unordered ( gnid_items (
        if note_forked_by_forker_as_such (tree, parties . hider, walk) . is_some () {
          hidden_by_forker (
            &occurrence_pid_of (parties . hider) ?,
            &occurrence_pid_of (parties . container ?) ?, walk )
        } else { Vec::new () },
        &HashMap::new ()))) },
    Folder::HiddenOutsideOfForkPartners => {
      let parties : CompoundFolderParties = compound_folder_parties (tree, folder) ?;
      Some ( Goal::ordered ( gnid_items (
        hidden_outside_of_fork_partners (
          &occurrence_pid_of (parties . hider) ?, walk ),
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
/// straddle in the diff view, else in the graph: every mirror of it,
/// each once ('docs/forks.org', "Every mirror, flat"). First the notes
/// it forks, in its list's order, each claimable, with the relRepo of
/// each unknown or restricted one; then its forker-only members
/// ('forker_only_members'), by the declaration order of their home
/// skgrepos, then pid; then its indirect members ('indirect_members'),
/// nearest first, then by the declaration order of their homes, then
/// pid. None but the forkees is claimable. The whole is ordered: the
/// tails' order is the graph's and the config's, not the user's.
fn forks_folder_goal (
  described : &ID,
  walk      : &Walk,
) -> Goal {
  if let Some (goal) = diff_view::forks_folder_goal (described, walk) {
    return goal; }
  let graph : &InRustGraph = walk . truth . graph;
  let config : &SkgConfig = walk . truth . config;
  let restriction : &SkgrepoRestriction = walk . truth . restriction;
  let forkee_keys : Vec<RelPartner<RelationshipMemberKey>> =
    graph . outbound_member_keys_for_relation_gated (
      described, NodeRelation::Forks, restriction );
  let relRepos : HashMap<ID, SkgrepoName> =
    unknown_or_restricted_members_relRepos (graph, restriction, described, &forkee_keys);
  let forkees : Vec<ID> = forkee_keys . into_iter ()
    . map ( |member| member . member . into_id () )
    . collect ();
  let declaration_rank_of : &dyn Fn (&ID) -> usize = &|pid : &ID| -> usize {
    graph . nodes . get (pid)
      . map_or ( usize::MAX, |node| config . declaration_rank (&node . home_skgrepo) ) };
  let mut forker_only : Vec<ID> =
    forker_only_members (graph, restriction, described, &forkees);
  forker_only . sort_by ( |a, b|
    ( declaration_rank_of (a), a ) . cmp (&( declaration_rank_of (b), b )) );
  let mut indirect : Vec<Mirror> =
    indirect_members (graph, restriction, described, &forkees);
  indirect . sort_by ( |a, b|
    ( a . distance, config . declaration_rank (&a . home), &a . pid )
    . cmp (&( b . distance, config . declaration_rank (&b . home), &b . pid )) );
  let claiming_nothing : &dyn Fn (ID) -> GoalItem = &|gnid : ID| -> GoalItem {
    GoalItem { claimable : false,
               .. GoalItem::by_graph ( GoalMember::Gnid (gnid) ) } };
  Goal::ordered (
    gnid_items (forkees, &relRepos) . into_iter ()
      . chain ( forker_only . into_iter () . map (claiming_nothing) )
      . chain ( indirect . into_iter ()
                . map ( |mirror| claiming_nothing (mirror . pid) ))
      . collect () ) }

/// The forker-only members of the forks folder describing 'described':
/// the notes that fork it by records the skgrepo restriction shows,
/// in graph order, less those it forks ('forkees', as its list has
/// them) and those it hides by such records. A forker it hides stands
/// in the folder of what it hides outside its fork partners instead.
pub(crate) fn forker_only_members (
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

/// The indirect members of the forks folder describing 'described'
/// ('docs/glossary.org', "direct member, indirect member"): its
/// mirrors under the skgrepo restriction, were the notes it forks
/// those 'forkees' names ('mirrors_of_note_forking'), less itself,
/// its fork partners (those 'forkees' names and those forking it) and
/// those it hides by records the skgrepo restriction shows, nearest
/// first. A mirror reached only through a note it hides is still one:
/// the hide leaves out that note alone.
pub(crate) fn indirect_members (
  graph       : &InRustGraph,
  restriction : &SkgrepoRestriction,
  described   : &ID,
  forkees     : &[ID],
) -> Vec<Mirror> {
  let described_pid : ID = graph . pid_else_raw_id (described);
  let left_out : HashSet<ID> =
    resolve_all (forkees, graph) . into_iter ()
    . chain ( graph . inbound_pids_for_relation_gated (
      described, NodeRelation::Forks, restriction ))
    . chain ( resolve_all (
      &graph . outbound_gnids_for_relation_gated (
        described, NodeRelation::Hides, restriction ),
      graph ))
    . chain ( once (described_pid) )
    . collect ();
  mirrors_of_note_forking (graph, restriction, described, forkees)
    . members . into_iter ()
    . filter ( |mirror| ! left_out . contains (&mirror . pid) )
    . collect () }

/// The members of the forks folder of 'note', by the records the
/// skgrepo restriction shows: the notes it forks ('forkees', which
/// the caller reads as the view drew them or as a save leaves them),
/// each resolved to its pid, then its forker-only members
/// ('forker_only_members'), then its indirect members
/// ('indirect_members'). These are the partners that explain a hide
/// of the note's ('docs/glossary.org', "hides"): what one of them
/// contains, the view draws under it, and the note's hide of it
/// stands there.
pub(crate) fn fork_partners_shown (
  graph       : &InRustGraph,
  restriction : &SkgrepoRestriction,
  note        : &ID,
  forkees     : &[ID],
) -> Vec<ID> {
  let forker_only : Vec<ID> =
    forker_only_members (graph, restriction, note, forkees);
  let indirect : Vec<ID> =
    indirect_members (graph, restriction, note, forkees) . into_iter ()
    . map ( |mirror| mirror . pid )
    . collect ();
  resolve_all (forkees, graph) . into_iter ()
    . chain (forker_only)
    . chain (indirect)
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
/// folder, every member its goal holds ('forks_folder_goal'): its
/// forkees, its forker-only members, then its indirect members, the
/// tails not in the goal's order. None if the kind has no relation
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
    return Some ( fork_partners_shown (
      graph, skgrepo_restriction, viewparent_pid, &forkees )); }
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

/// What 'hider' hides in the content of 'container', by the records
/// the skgrepo restriction shows, in the order of the hides list: the
/// goal of the compound folder 'HiddenInForkPartner' under a member
/// as such (the member the container, the note whose forks folder
/// holds it the hider), and of the folder of what a forker as such
/// hides of the note's content (the roles swapped).
pub(crate) fn hidden_in_fork_partner (
  container  : &ID,
  hider      : &ID,
  walk       : &Walk,
) -> Vec<ID> {
  let graph : &InRustGraph = walk . truth . graph;
  let goal : Vec<ID> = goal_list_for_hiddenInForkPartner_folder (
    graph,
    &graph . outbound_gnids_for_relation_gated (
      container, NodeRelation::Contains, walk . truth . restriction ),
    &graph . outbound_gnids_for_relation_gated (
      hider, NodeRelation::Hides, walk . truth . restriction ));
  resolve_all (&goal, graph) }

/// What 'forker', a forker as such in the forks folder of 'note',
/// hides of the note's content, by the records the skgrepo
/// restriction shows: the goal of the compound folder
/// 'HiddenByForker' under the forker. The same intersection as
/// the hidden-in folder's, with the parties' roles swapped.
pub(crate) fn hidden_by_forker (
  forker     : &ID,
  note       : &ID,
  walk       : &Walk,
) -> Vec<ID> {
  hidden_in_fork_partner (note, forker, walk) }

/// Goal list for a hiddenInForkPartnerFolder: the intersection of the
/// hider's hides-list and the container's contains-list, in the
/// order of the hides list.
fn goal_list_for_hiddenInForkPartner_folder (
  graph                : &InRustGraph,
  container_contains   : &[ID],
  hider_hides          : &[ID],
) -> Vec<ID> {
  let contains_set : HashSet<ID> =
    resolve_all (container_contains, graph) . into_iter () . collect ();
  hider_hides . iter ()
    . filter ( |gnid| contains_set . contains (&graph . pid_else_raw_id (gnid)) )
    . cloned () . collect () }

/// What 'note' hides that none of its fork partners contains, by the
/// records the skgrepo restriction shows: the goal of its compound
/// folder 'HiddenOutsideOfForkPartners', in the order of its hides
/// list. Its fork partners are the members its forks folder shows,
/// forkees, forkers and indirect members alike
/// ('fork_partners_shown'): a hide one of
/// them contains stands under that member instead, in its folder of
/// what the note hides in it. A partner's content is read by the same
/// records, so a hide of something a partner contains only by a
/// restricted record stands here, outside, as it does in the view.
pub(crate) fn hidden_outside_of_fork_partners (
  note       : &ID,
  walk       : &Walk,
) -> Vec<ID> {
  let graph : &InRustGraph = walk . truth . graph;
  let restriction : &SkgrepoRestriction = walk . truth . restriction;
  let goal : Vec<ID> = goal_list_for_hiddenOutsideOfForkPartners_folder (
    graph,
    &graph . outbound_gnids_for_relation_gated (
      note, NodeRelation::Hides, restriction ),
    &fork_partners_shown (
      graph, restriction, note,
      &graph . outbound_gnids_for_relation_gated (
        note, NodeRelation::Forks, restriction )),
    restriction );
  resolve_all (&goal, graph) }

/// Goal list for a hiddenOutsideOfForkPartnersFolder: the note's
/// hides-list minus everything any of its fork partners contains by a
/// record the skgrepo restriction shows.
fn goal_list_for_hiddenOutsideOfForkPartners_folder (
  graph             : &InRustGraph,
  note_hides        : &[ID],
  fork_partners     : &[ID],
  restriction       : &SkgrepoRestriction,
) -> Vec<ID> {
  let partner_content_of : &dyn Fn (&ID) -> Vec<ID> = &|partner : &ID| -> Vec<ID> {
    graph . outbound_gnids_for_relation_gated (
      &graph . pid_else_raw_id (partner), NodeRelation::Contains, restriction ) };
  let all_partner_content : HashSet<ID> =
    fork_partners . iter ()
      . flat_map ( |pid| partner_content_of (pid) )
      . map ( |gnid| graph . pid_else_raw_id (&gnid) )
      . collect ();
  note_hides . iter ()
    . filter ( |gnid| ! all_partner_content
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
