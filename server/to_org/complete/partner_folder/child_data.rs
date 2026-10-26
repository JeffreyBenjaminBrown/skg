/// Shared per-child information for PartnerFolder reconciliation.
///
/// Used by the rerender-time completers for SubscribeeFolder,
/// HiddenInSubscribeeFolder, and HiddenOutsideOfSubscribeeFolder.
///
/// Vocabulary for this module:
///
/// - A `goal_list` is the ordered list of goal items, one per node
///   ID, that a folder should present after completion.  The list is
///   computed from the graph, or from the union of a graph straddle,
///   whose items carry a change and a shape ('FolderGoalItem').
/// - A goal child is a child Viewnode whose UnrestrictedVognode ID appears in
///   that `goal_list`, whether it already existed in the buffer or
///   was created during reconciliation.
/// - A managed child is one this reconciliation pass is allowed to
///   manage: for PartnerFolders, an UnrestrictedVognode that is a member,
///   a removed member, or an unknown or historical phantom (the children
///   'skgid_a_folder_child_stands_for' gives an ID).
///   Managed children whose IDs are not in the goal list are converted
///   ('convert_unwarranted_child'), and so are extra copies of one.
/// - `ChildData` is the pre-fetched title/repo metadata
///   needed to create any missing goal child without querying while
///   the tree is being mutated.

use crate::dbs::in_rust_graph::InRustGraph;
use crate::types::env::{RuntimeGeneration, SkgEnv};
use crate::types::misc::{ID, SkgrepoName};
use crate::dbs::node_lookup::graphnode_graphFirst_by_pid_and_skgrepo;
use crate::diff_view::shallow_model_subtree::{
  convert_unwarranted_subtree_taking_along, give_model_facts_to,
  is_unrestricted_removed_member};
use crate::to_org::extend::containerward_chain::is_drawn_chain_node;
use crate::types::viewnode::{DiffChange, Unknown, Viewnode, ViewnodeKind, Vognode, Folder, mk_writeProtected_viewnode, mk_unknown_viewnode};
use crate::update_buffer::util::{
  Conversion, KeptChildOrder, RepairSummary, Unwarranted, complete_managed_children};
use crate::update_buffer::util::treat_certain_children;

use ego_tree::{NodeId, NodeMut, NodeRef, Tree};
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::io;

/// Per-child information needed to build a viewnode for a sharing
/// folder's child (subscribee, hidden-in-subscribee, or
/// hidden-outside-of-subscribees).
pub struct ChildData {
  pub home_skgrepo : SkgrepoName,
  pub title        : String,
  /// True when the exact stored relationship member has no current
  /// node.  It is rendered as an Unknown, never as a title-less unrestricted
  /// fallback.
  pub unknown : bool,
  pub relRepo : Option<SkgrepoName>,
}

/// One item of a folder's goal list.
#[derive(Clone, Debug)]
pub struct FolderGoalItem {
  pub skgid    : ID,
  /// The change of its node and of its relationship to the folder's
  /// viewparent, when the folder shows the union of a graph straddle;
  /// None from the live graph.
  pub change   : Option<DiffChange>,
  pub shape    : FolderGoalShape,
}

/// How a goal item is shown.
#[derive(Clone, Debug)]
pub enum FolderGoalShape {
  /// A member: a write-protected vognode that is a member,
  /// or an unknown phantom when the live graph lacks its node.
  Member,
  /// A member whose relationship is only in the before-graph: a
  /// write-protected vognode marked noClaim (so that
  /// saving the buffer does not restore it), or an unknown phantom
  /// when the live graph lacks its node.
  Removed,
  /// A node only in the before-graph: this historical phantom.
  Historical (Viewnode),
}

impl FolderGoalItem {
  /// The goal items of a goal list from the live graph.
  pub fn live_members (
    skgids : &[ID],
  ) -> Vec<FolderGoalItem> {
    skgids . iter ()
      . map ( |skgid| FolderGoalItem {
        skgid    : skgid . clone (),
        change   : None,
        shape    : FolderGoalShape::Member, } )
      . collect () }
}

/// Which ID a child of a partner folder stands for, over the union of
/// a graph straddle: a member or a removed member, its skgid as
/// 'graph' (the live graph) resolves it ('relationship_member_key':
/// an extra ID becomes its node's pid, an ID the graph lacks stays as
/// it is); an unknown phantom, its raw ID; a historical phantom, its
/// ID in the before-graph, which the live graph does not resolve.
/// None for any other child. The folder reconciler matches children
/// to goal items by it, and the save's write-protected-occurrence
/// check reads a folder's members by it, so the two agree.
pub fn skgid_a_folder_child_stands_for (
  viewnode : &Viewnode,
  graph    : &InRustGraph,
) -> Option<ID> {
  match &viewnode . kind {
    ViewnodeKind::Vognode (Vognode::Unrestricted (t))
      if t . claimsMembership
         || is_unrestricted_removed_member (viewnode) =>
      Some ( graph . relationship_member_key (&t . skgid) . into_id () ),
    ViewnodeKind::Vognode (Vognode::Unknown (u)) =>
      Some ( u . skgid . clone () ),
    ViewnodeKind::Vognode (Vognode::Ghost (h)) =>
      Some ( h . skgid . clone () ),
    _ => None, }}

/// Build a map from child ID to ChildData for the create-child
/// closure of `complete_managed_children`, for every
/// goal item but a historical phantom.
/// The sharing completers build this before mutation so their flow
/// stays explicit: read tree and graph facts, compute the goal list,
/// prepare child data, then reconcile folder children.
pub fn build_child_data (
  tree                           : &Tree<Viewnode>,
  folder_node                       : NodeId,
  goal_list                      : &[FolderGoalItem],
  relRepos                       : &HashMap<ID, SkgrepoName>,
  runtime                        : &RuntimeGeneration,
) -> Result<HashMap<ID, ChildData>, Box<dyn Error>> {
  let existing_children : HashMap<ID, (SkgrepoName, String)> = {
    let node_ref : NodeRef<Viewnode> =
      tree . get (folder_node)
        . ok_or ("build_child_data: node not found") ?;
    let mut m : HashMap<ID, (SkgrepoName, String)> = HashMap::new ();
    for child_ref in node_ref . children () {
      if let ViewnodeKind::Vognode (Vognode::Unrestricted (t))
        = & child_ref . value () . kind
        { m . insert ( t . skgid . clone (),
                       ( t . home_skgrepo . clone (),
                         t . title . clone () )); }}
    m };
  let mut result : HashMap<ID, ChildData> = HashMap::new ();
  for item in goal_list {
    // A historical phantom is made from the before-graph.
    if let FolderGoalShape::Historical (_) = item . shape { continue; }
    let child_skgid : &ID = &item . skgid;
    if result . contains_key (child_skgid) { continue; }
    match SkgEnv::find_skgrepo_in_generation (runtime, child_skgid) {
      None => { result . insert ( child_skgid . clone (),
        ChildData { home_skgrepo: SkgrepoName::not_found (), title: String::new (),
                    unknown: true,
                    relRepo: relRepos . get (child_skgid) . cloned () } ); },
      Some (child_src) => {
        // `find_repo` deliberately falls back through Tantivy.  During a
        // same-save rerender the search index can still name a just-deleted node;
        // do not let that stale hint turn a retained raw relationship member
        // into a failed disk read.  An unreadable, formerly indexed file
        // means precisely an Unknown relationship member.
        match graphnode_graphFirst_by_pid_and_skgrepo (
          &runtime . graph, &runtime . config, child_skgid, &child_src ) {
          Ok (skg) => if let Some ( (s, t) ) = existing_children . get (child_skgid) {
            result . insert ( child_skgid . clone (),
                              ChildData { home_skgrepo  : s . clone (),
                                          title   : t . clone (),
                                          unknown : false,
                                          relRepo : None } );
          } else {
            result . insert ( child_skgid . clone (),
                              ChildData { home_skgrepo  : skg . home_skgrepo . clone (),
                                          title   : skg . title . clone (),
                                          unknown : false,
                                          relRepo : None } ); },
          Err (e) if e . downcast_ref::<io::Error> ()
            . is_some_and (|io_error| io_error . kind () == io::ErrorKind::NotFound) => {
            result . insert ( child_skgid . clone (),
              ChildData { home_skgrepo: SkgrepoName::not_found (), title: String::new (),
                          unknown: true,
                          relRepo: relRepos . get (child_skgid) . cloned () } ); },
          Err (e) => return Err (e),
        }
      }
    }
  }
  Ok (result) }

/// Reconcile a PartnerFolder's children against a goal list.
///
/// The rerender-time PartnerFolder completers
/// (SubscribeeFolder, HiddenInSubscribeeFolder,
/// HiddenOutsideOfSubscribeeFolder) share the same call shape: build
/// per-child data before mutation, then call
/// `complete_managed_children` with identical
/// managed/key/create closures. Of several children of one key the
/// editable one is kept, else the first. A stale or duplicate child is
/// converted with the containerward chain drawn under it
/// ('convert_unwarranted_member_with_its_chain'). A member with an unknown ChildData
/// entry produces an unknown phantom; any other, a write-protected
/// viewnode that claims membership. An item with another shape
/// is shown as its shape says, and matches an existing child of the
/// same skgid whatever its shape, which then takes the item's
/// ('apply_goal_shapes').
pub fn reconcile_partnerFolder_children_against_goal_list (
  tree          : &mut Tree<Viewnode>,
  folder_node      : NodeId,
  kind          : &Folder,
  goal_list     : &[FolderGoalItem],
  child_data    : &HashMap<ID, ChildData>,
  graph         : &InRustGraph,
) -> Result<RepairSummary<ID>, Box<dyn Error>> {
  let label : &'static str = kind . caller_label ();
  let goal_skgids : Vec<ID> =
    goal_list . iter () . map ( |item| item . skgid . clone () ) . collect ();
  let items_by_skgid : HashMap<&ID, &FolderGoalItem> =
    goal_list . iter () . map ( |item| (&item . skgid, item) ) . collect ();
  normalize_relationship_backed_partner_unknowns (
    tree, folder_node, child_data ) ?;
  mark_goal_children_as_folder_members (
    tree, folder_node, goal_list, graph ) ?;
  let summary : RepairSummary<ID> =
    complete_managed_children (
    tree, folder_node,
    // The save reads the membership of a folder whose goal is
    // ordered, in the buffer's order.
    if kind . goal_is_ordered () { KeptChildOrder::FollowsGoal }
    else { KeptChildOrder::Unmoved },
    // A tombstone child is unmanaged (the API doc's
    // "Restricted-skgrepo nodes"): the goal omits every hidden member,
    // and a tombstone standing where one was survives as an unmanaged
    // child (preserved as-is, not goal-matched), so it needs no id.
    |vn : &Viewnode| skgid_a_folder_child_stands_for (vn, graph) . is_some (),
    |vn : &Viewnode| skgid_a_folder_child_stands_for (vn, graph) . ok_or_else ( || format! (
      "{}: managed child not a normal graphnode", label )),
    |vn : &Viewnode| vn . editable_skgid () . is_some (),
    &goal_skgids,
    |skgid : &ID| {
      let item : &FolderGoalItem = items_by_skgid . get (skgid) . ok_or_else (
        || format! ( "{}: no goal item for {}", label, skgid . 0 )) ?;
      viewnode_of_goal_item (item, child_data, label) },
    convert_unwarranted_member_with_its_chain,
  ) ?;
  apply_goal_shapes (
    tree, folder_node, &items_by_skgid, child_data, graph, label ) ?;
  Ok (summary) }

/// Convert 'child', a child of a partner folder found stale or an
/// extra copy, by the glossary's "retain as non-member or tombstone,
/// then prune", and, first, the containerward chain drawn under it
/// ('is_drawn_chain_node'), which goes with it: so a member holding
/// nothing but its chain becomes a tombstone the prune removes, while
/// one holding what the user put there is retained as non-member.
fn convert_unwarranted_member_with_its_chain (
  tree        : &mut Tree<Viewnode>,
  child       : NodeId,
  unwarranted : Unwarranted,
) -> Result<Conversion, Box<dyn Error>> {
  convert_unwarranted_subtree_taking_along (
    tree, child, unwarranted, &is_drawn_chain_node ) }

/// The viewnode made for a goal item when no child matches it. See
/// 'FolderGoalShape'.
fn viewnode_of_goal_item (
  item       : &FolderGoalItem,
  child_data : &HashMap<ID, ChildData>,
  label      : &str,
) -> Result<Viewnode, String> {
  let mut viewnode : Viewnode = match &item . shape {
    FolderGoalShape::Historical (phantom) => phantom . clone (),
    shape => {
      let d : &ChildData =
        child_data . get (&item . skgid) . ok_or_else (
          || format! ( "{}: child data not pre-fetched for {}",
                       label, item . skgid . 0 )) ?;
      if d . unknown {
        let mut unknown : Viewnode = mk_unknown_viewnode (item . skgid . clone ());
        if let ViewnodeKind::Vognode (Vognode::Unknown (u)) = &mut unknown . kind
        { u . relRepo = d . relRepo . clone (); }
        unknown
      } else { mk_writeProtected_viewnode (
        item . skgid . clone (), d . home_skgrepo . clone (),
        d . title . clone (),
        match shape { FolderGoalShape::Removed => false,
                      _ => true, } ) } }, };
  if let Some (change) = item . change {
    viewnode . change_facts . change = change; }
  Ok (viewnode) }

/// Give each child that a goal item matched the item's shape, keeping
/// the viewnode and its fold state, and the item's change. A member
/// keeps what it is (an editable subscribee stays editable), unless
/// it stood as a historical phantom, and is a member
/// (a member) even if it stood as a removed member; a
/// removed member is write-protected and retained as non-member.
fn apply_goal_shapes (
  tree           : &mut Tree<Viewnode>,
  folder_node    : NodeId,
  items_by_skgid : &HashMap<&ID, &FolderGoalItem>,
  child_data     : &HashMap<ID, ChildData>,
  graph          : &InRustGraph,
  label          : &str,
) -> Result<(), Box<dyn Error>> {
  let children : Vec<(NodeId, ID)> =
    tree . get (folder_node)
    . ok_or ("apply_goal_shapes: folder not found") ?
    . children ()
    . filter_map ( |child| skgid_a_folder_child_stands_for ( child . value (), graph )
                   . map ( |skgid| (child . id (), skgid) ))
    . collect ();
  for (child, skgid) in children {
    let Some (item) : Option<&&FolderGoalItem> = items_by_skgid . get (&skgid)
      else { continue; };
    let mut child_mut : NodeMut<Viewnode> = tree . get_mut (child) . unwrap ();
    let viewnode : &mut Viewnode = child_mut . value ();
    let historical : bool = matches! ( &viewnode . kind,
      ViewnodeKind::Vognode (Vognode::Ghost (_)) );
    let unrestricted : bool = matches! ( &viewnode . kind,
      ViewnodeKind::Vognode (Vognode::Unrestricted (_)) );
    match &item . shape {
      FolderGoalShape::Member if ! historical => {
        if let ViewnodeKind::Vognode (Vognode::Unrestricted (t)) = &mut viewnode . kind {
          t . claimsMembership = true; }},
      FolderGoalShape::Removed if unrestricted => {
        if let ViewnodeKind::Vognode (Vognode::Unrestricted (t)) = &mut viewnode . kind {
          t . claimsMembership = false;
          t . write_protect (); }},
      _ => give_model_facts_to (
        viewnode, &viewnode_of_goal_item (item, child_data, label) ? ), }
    if let Some (change) = item . change {
      viewnode . change_facts . change = change; }}
  Ok (( )) }

/// An already-open PartnerFolder can still hold an Unrestricted occurrence after its
/// graphnode was deleted.  When the rebuilt raw goal retains that membership,
/// turn it into Unknown before the generic deletion pass.  Replacing only the
/// kind keeps focus/folding state but removes title, body, home, and node edits.
fn normalize_relationship_backed_partner_unknowns (
  tree       : &mut Tree<Viewnode>,
  folder_node   : NodeId,
  child_data : &HashMap<ID, ChildData>,
) -> Result<(), Box<dyn Error>> {
  treat_certain_children (
    tree, folder_node,
    |vn : &Viewnode| match &vn . kind {
      ViewnodeKind::Vognode (Vognode::Unrestricted (restriction)) =>
        restriction . claimsMembership
        && child_data . get (&restriction . skgid)
           . is_some_and (|data| data . unknown),
      _ => false },
    |vn : &mut Viewnode| {
      let skgid : ID = match &vn . kind {
        ViewnodeKind::Vognode (Vognode::Unrestricted (restriction)) => restriction . skgid . clone (),
        _ => unreachable! (), };
      let relRepo : Option<SkgrepoName> = child_data . get (&skgid)
        . and_then (|data| data . relRepo . clone ());
      vn . replace_kind ( ViewnodeKind::Vognode (Vognode::Unknown (
        Unknown { skgid, relRepo, relationshipEditRequest: None }))); })
    . map_err ( |e| -> Box<dyn Error> { e . into () } )
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::types::viewnode::{mk_writeProtected_viewnode, viewnode_of};

  fn skgid (text : &str) -> ID { ID::from (text) }
  fn skgrepo (text : &str) -> SkgrepoName { SkgrepoName::from (text) }

  /// A member shown by an extra ID of x stands for x, so the
  /// reconciler keeps it for the goal item x, as the save's
  /// write-protected-occurrence check counts it as x.
  #[test]
  fn a_member_shown_by_an_extra_id_is_kept_for_its_node () {
    let graph : InRustGraph = InRustGraph::from_graphnodes (&[
      crate::types::nodes::complete::Graphnode {
        pid          : skgid ("x"),
        extra_ids    : vec! [skgid ("x2")],
        title        : "x" . to_string (),
        home_skgrepo : skgrepo ("main"),
        .. crate::types::nodes::complete::empty_graphnode () } ]);
    let mut tree : Tree<Viewnode> = Tree::new (
      viewnode_of (ViewnodeKind::Folder (Folder::Subscribee)));
    let folder : NodeId = tree . root () . id ();
    let member : Viewnode = mk_writeProtected_viewnode (
      skgid ("x2"), skgrepo ("main"), "x" . to_string (), true );
    assert_eq! ( skgid_a_folder_child_stands_for (&member, &graph),
                 Some (skgid ("x")) );
    let member_treeid : NodeId = tree . root_mut () . append (member) . id ();
    let mut child_data : HashMap<ID, ChildData> = HashMap::new ();
    child_data . insert ( skgid ("x"), ChildData {
      home_skgrepo : skgrepo ("main"), title : "x" . to_string (),
      unknown : false, relRepo : None } );
    reconcile_partnerFolder_children_against_goal_list (
      &mut tree, folder, &Folder::Subscribee,
      &FolderGoalItem::live_members (&[skgid ("x")]),
      &child_data, &graph ) . unwrap ();
    assert_eq! (
      tree . root () . children () . map ( |child| child . id () )
        . collect::<Vec<NodeId>> (),
      vec! [member_treeid] ); }

}

/// See this module's header for definition of "goal child".
///
/// Before the folder reconciler matches the managed children with the
/// goal list, this adopts a returning key: for each goal item (but a
/// historical phantom) that no managed child stands for, the
/// unrestricted non-member standing for its node -- the editable one,
/// else the first -- becomes a member, keeping its viewnode and fold
/// state, so that the reconciler matches it and creates no second
/// member beside it. (Such a non-member is typically one the reconciler
/// retained as non-member when the membership went stale:
/// 'convert_unwarranted_child'.)
fn mark_goal_children_as_folder_members (
  tree        : &mut Tree<Viewnode>,
  folder_node : NodeId,
  goal_list   : &[FolderGoalItem],
  graph       : &InRustGraph,
) -> Result<(), Box<dyn Error>> {
  let adoptees : Vec<NodeId> = {
    let folder_ref : NodeRef<Viewnode> = tree . get (folder_node)
      . ok_or ("mark_goal_children_as_folder_members: folder not found") ?;
    let standing_for : HashSet<ID> =
      folder_ref . children ()
      . filter_map ( |child| skgid_a_folder_child_stands_for (
        child . value (), graph ))
      . collect ();
    let nonMembers : Vec<(NodeId, ID, bool)> = // (treeid, its node's ID, editable)
      folder_ref . children ()
      . filter_map ( |child| match &child . value () . kind {
        ViewnodeKind::Vognode (Vognode::Unrestricted (t))
          if skgid_a_folder_child_stands_for ( child . value (), graph ) . is_none () =>
          Some (( child . id (),
                  graph . relationship_member_key (&t . skgid) . into_id (),
                  ! t . is_writeProtected () )),
        _ => None, } )
      . collect ();
    goal_list . iter ()
      . filter ( |item| ! matches! ( item . shape, FolderGoalShape::Historical (_) ))
      . filter ( |item| ! standing_for . contains (&item . skgid) )
      . filter_map ( |item| {
        let candidates : Vec<&(NodeId, ID, bool)> = nonMembers . iter ()
          . filter ( |(_, skgid, _)| *skgid == item . skgid )
          . collect ();
        candidates . iter () . find ( |(_, _, editable)| *editable )
          . or ( candidates . first () )
          . map ( |(treeid, _, _)| *treeid ) } )
      . collect () };
  for adoptee in adoptees {
    if let ViewnodeKind::Vognode (Vognode::Unrestricted (t))
      = &mut tree . get_mut (adoptee) . unwrap () . value () . kind
    { t . claimsMembership = true; }}
  Ok (( )) }
