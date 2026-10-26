//! The ladder: the corrections the refresh applies at each parent P
//! to its children, in order ('TODO/simplify-refresh/
//! 04_1_plan_rewrite-refresh.org', "the ladder, per parent, in
//! order"):
//!  1. re-kind: each child that stands for a gnid becomes what the ID
//!     rule says, where it stands; each descriptor (folder, property
//!     member, change viewnode) whose position the ancestry table
//!     forbids, or whose change is gone, is entombstoned;
//!  2. merge: siblings standing for one gnid are unified;
//!  3. the goal and the warrant: each child's warrant is set from P's
//!     goal;
//!  4. claims: under an editable parent, every child that claims and
//!     that the truth does not warrant is marked 'noClaim'; under any
//!     other, no child claims;
//!  5. reinstate: every goal item without a child is drawn, within the
//!     extent;
//!  6. reorder: where the goal has an order, the warranted children
//!     take it, and every other child stays right after the sibling it
//!     followed;
//!  7. substitution: in a content view an overridden node is shown as
//!     its overrider;
//!  8. requests: each child's view requests are fulfilled and dropped.
//! Before them, the budget is settled for P ('extent.rs'); between 5
//! and 6 the diff view draws its change viewnodes and tags the
//! children with their changes ('diff_view.rs'); after 6, in a
//! subscribee folder, the folder of what the subscriber hides outside
//! its subscribees goes last; after them, a folder left with no
//! children and nothing the user could add there is entombstoned, so
//! that the tail prunes it.

use crate::dbs::in_rust_graph::override_resolution::{
  carrier_on_owned_chain, resolve_override};
use crate::refresh::corrections::CorrectionKind;
use crate::refresh::extent::{
  Extent, drawn_occurrence_is_editable, extent_of_parent, settle_budget_at_visit};
use crate::refresh::goal::{
  Goal, GoalItem, GoalMember, goal_of_parent, hidden_in_subscribee,
  hidden_outside_of_subscribees, member_of_child, occurrence_pid,
  subscriber_of_subscribee_as_such};
use crate::update_buffer::reconcile::partner_folder::partnerFolder_members_in_graph;
use crate::refresh::id_rule::{
  IdRuleAnswer, entombstone, entombstone_gone_change, ghost_from_head, id_rule,
  occurrence_from_truth,
  rekind_vognode};
use crate::refresh::{Walk, write_protect_occurrence};
use crate::refresh::ancestry::{is_unsayable, parent_reads_claims};
use crate::refresh::diff_view;
use crate::to_org::extend::containerward_chain::{
  ContainerwardChainPartners, attach_containerward_chains_under_members};
use crate::types::misc::ID;
use crate::types::nodes::rust::GraphnodeInRust;
use crate::types::tree::moves::{move_after, move_to_first_child, move_to_last_child};
use crate::types::viewnode::{
  Editability, Folder, FolderRelation, Ghost, PropertyMember, Unknown, UnrestrictedVognode,
  ViewRequest, Viewnode, ViewnodeKind, ViewnodeStats, Vognode, Warrant, viewnode_of};
use crate::update_buffer::completion_mode::CompletionMode;

use ego_tree::{NodeId, NodeMut, NodeRef, Tree};
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::mem::discriminant;

/// Run the ladder at 'parent', whose own kind is settled.
pub(crate) fn visit_parent (
  tree   : &mut Tree<Viewnode>,
  parent : NodeId,
  walk   : &mut Walk,
) -> Result<(), Box<dyn Error>> {
  settle_budget_at_visit (tree, parent, walk);
  extend_requested_content (tree, parent, walk);
  add_default_folders (tree, parent, walk);
  let goal : Option<Goal> = goal_of_parent (tree, parent, walk);
  rekind_children (tree, parent, goal . as_ref (), walk) ?;
  merge_siblings (tree, parent, walk) ?;
  set_warrants (tree, parent, goal . as_ref ());
  mark_claims (tree, parent, goal . as_ref (), walk);
  if let Some (goal) = &goal {
    if extent_of_parent (tree, parent, walk) == Extent::All {
      reinstate (tree, parent, goal, walk) ?; }}
  diff_view::draw_change_viewnodes (tree, parent, walk) ?;
  diff_view::tag_children (tree, parent, goal . as_ref (), walk) ?;
  if let Some (goal) = &goal {
    if goal . ordered {
      reorder (tree, parent, goal) ?; }}
  keep_hidden_outside_folder_last (tree, parent) ?;
  substitute_overriders (tree, parent, goal . as_ref (), walk) ?;
  fulfill_view_requests (tree, parent, walk) ?;
  entombstone_if_an_empty_folder (tree, parent);
  Ok (( )) }

/// An editable-view request extends its occurrence's content, the
/// content it shows included: each write-protected child that claims
/// membership becomes editable as a drawn occurrence would (its node
/// has no editable occurrence, and the budget allows), and extends its
/// own content in turn, as if it carried the request.
fn extend_requested_content (
  tree   : &mut Tree<Viewnode>,
  parent : NodeId,
  walk   : &mut Walk,
) {
  if ! walk . had_editableView_request . contains (&parent)
     || tree . get (parent) . unwrap () . value () . editable_skgid () . is_none ()
  { return; }
  for child in children_of (tree, parent) {
    let gnid : ID = match &tree . get (child) . unwrap () . value () . kind {
      ViewnodeKind::Vognode (Vognode::Unrestricted (occurrence))
        if occurrence . is_writeProtected () && occurrence . claimsMembership =>
        occurrence . skgid . clone (),
      _ => continue, };
    if ! drawn_occurrence_is_editable (tree, parent, &gnid, walk) { continue; }
    if let ViewnodeKind::Vognode (Vognode::Unrestricted (occurrence)) =
      &mut tree . get_mut (child) . unwrap () . value () . kind
    { occurrence . editability = Editability::Editable { // the re-kind gives it its body
        body : None, edit_request : None }; }
    walk . editable_gnids . insert (gnid);
    walk . drawn_editable . insert (child);
    walk . had_editableView_request . insert (child); }}

/// The folders a content view shows without being asked, as it
/// always has: an occurrence that an editable-view request extends
/// (a fresh view's roots among them) shows its subscribee folder when
/// it subscribes to something the restriction shows (with the folder
/// of what it hides outside its subscribees, when that holds
/// anything); and an editable subscribee as such shows the folder of
/// what its subscriber hides in it, when that holds anything. Not in
/// the diff view.
fn add_default_folders (
  tree   : &mut Tree<Viewnode>,
  parent : NodeId,
  walk   : &mut Walk,
) {
  if walk . is_diff_view () { return; }
  let Some (pid) = tree . get (parent) . unwrap () . value () . editable_skgid () . cloned ()
    else { return; };
  if walk . had_editableView_request . contains (&parent)
     && partnerFolder_members_in_graph (
          walk . truth . graph, walk . truth . config, &pid, &Folder::Subscribee,
          walk . truth . restriction )
        . is_some_and ( |members| ! members . is_empty () )
  { let had_one : bool = child_folder (tree, parent, &Folder::Subscribee) . is_some ();
    add_folders (tree, parent, &[ Folder::Subscribee ], walk);
    // A new one goes after any folder the occurrence was asked for.
    if let Some (subscribee_folder) = child_folder (tree, parent, &Folder::Subscribee)
                                      . filter ( |_| ! had_one ) {
      let last_leading_folder : Option<NodeId> =
        tree . get (parent) . unwrap () . children ()
        . take_while ( |child| matches! ( child . value () . kind, ViewnodeKind::Folder (_) ))
        . filter ( |child| child . id () != subscribee_folder )
        . map ( |child| child . id () )
        . last ();
      if let Some (folder) = last_leading_folder {
        move_after (tree, folder, subscribee_folder)
          . expect ("add_default_folders: both are children"); }}}
  if let Some (subscriber) = subscriber_of_subscribee_as_such (tree, parent) {
    if child_folder (tree, parent, &Folder::HiddenInSubscribee) . is_none ()
       && ! hidden_in_subscribee (&pid, &subscriber, walk) . is_empty ()
    { let folder : NodeId = tree . get_mut (parent) . unwrap ()
        . append ( viewnode_of ( ViewnodeKind::Folder (Folder::HiddenInSubscribee) ))
        . id ();
      move_to_first_child (tree, parent, folder)
        . expect ("add_default_folders: the folder was just added");
      walk . newborn_folders . insert (folder);
      walk . added_folders . insert (folder); }}}

//
// 1. re-kind
//

fn rekind_children (
  tree   : &mut Tree<Viewnode>,
  parent : NodeId,
  goal   : Option<&Goal>,
  walk   : &mut Walk,
) -> Result<(), Box<dyn Error>> {
  for child in children_of (tree, parent) {
    match &tree . get (child) . unwrap () . value () . kind {
      ViewnodeKind::Vognode (_) =>
        rekind_vognode (tree, parent, child, goal, walk) ?,
      ViewnodeKind::Folder (_)
        | ViewnodeKind::PropertyMember (_)
        | ViewnodeKind::ChangeViewnode (_) =>
        if is_unsayable (tree, child) {
          entombstone (tree, child, walk); }
        else if diff_view::change_is_gone (tree, child, walk) {
          entombstone_gone_change (tree, child, walk); },
      ViewnodeKind::Tombstone | ViewnodeKind::BufferRoot => {}, }}
  Ok (( )) }

//
// 2. merge
//

/// Unify siblings that stand for one gnid. The first in position
/// survives, keeping its vnid and fold state; it is editable if any
/// of them was; the others' children are appended to it, each marked
/// 'noClaim' unless the truth warrants it under the survivor.
fn merge_siblings (
  tree   : &mut Tree<Viewnode>,
  parent : NodeId,
  walk   : &mut Walk,
) -> Result<(), Box<dyn Error>> {
  let mut survivors : HashMap<ID, NodeId> = HashMap::new ();
  for child in children_of (tree, parent) {
    let Some (GoalMember::Gnid (gnid)) =
      vognode_member (tree . get (child) . unwrap () . value ())
      else { continue; };
    match survivors . get (&gnid) {
      None => { survivors . insert (gnid, child); },
      Some (survivor) => merge_into (tree, *survivor, child, walk) ?, }}
  Ok (( )) }

/// What a vognode stands for; None for any other kind.
fn vognode_member (
  viewnode : &Viewnode,
) -> Option<GoalMember> {
  match &viewnode . kind {
    ViewnodeKind::Vognode (_) => member_of_child (viewnode),
    _ => None, } }

fn merge_into (
  tree     : &mut Tree<Viewnode>,
  survivor : NodeId,
  other    : NodeId,
  walk     : &mut Walk,
) -> Result<(), Box<dyn Error>> {
  let other_viewnode : Viewnode = tree . get (other) . unwrap () . value () . clone ();
  { let mut survivor_mut : NodeMut<Viewnode> = tree . get_mut (survivor) . unwrap ();
    let survivor_viewnode : &mut Viewnode = survivor_mut . value ();
    if other_viewnode . foldAndFocus . focused {
      survivor_viewnode . foldAndFocus . focused = true; }
    if let ( ViewnodeKind::Vognode (Vognode::Unrestricted (kept)),
             ViewnodeKind::Vognode (Vognode::Unrestricted (merged)) )
      = ( &mut survivor_viewnode . kind, &other_viewnode . kind )
    { kept . view_requests . extend ( merged . view_requests . iter () . copied () );
      if ! merged . is_writeProtected () {
        // The merged copy's editability passes to the survivor, unless
        // the survivor stands for another node (a node and its shown
        // overrider stand for one gnid) whose editable occurrence is
        // elsewhere.
        walk . editable_gnids . remove (&merged . skgid);
        let takes_it : bool = kept . is_writeProtected ()
          && ! walk . editable_gnids . contains (&kept . skgid);
        if takes_it {
          kept . editability = merged . editability . clone (); }
        if ! kept . is_writeProtected () {
          walk . editable_gnids . insert ( kept . skgid . clone () ); }
        if walk . drawn_editable . remove (&other) && takes_it {
          walk . drawn_editable . insert (survivor); }}}}
  let goal : Option<Goal> = goal_of_parent (tree, survivor, walk);
  for grandchild in children_of (tree, other) {
    move_to_last_child (tree, survivor, grandchild) ?;
    let viewnode : &Viewnode = tree . get (grandchild) . unwrap () . value ();
    if claims_membership (viewnode) == Some (true)
       && warrant_of_child (goal . as_ref (), viewnode) != Some (Warrant::ByGraph) {
      set_claim (tree, grandchild, false);
      walk . corrections . record (tree, grandchild, CorrectionKind::MarkedNoClaim); }}
  walk . corrections . record (tree, survivor, CorrectionKind::Merged);
  tree . get_mut (other) . unwrap () . detach ();
  Ok (( )) }

//
// 3. the goal and the warrant
//

/// Each child's warrant: 'ByGraph' when the truth's part of the goal
/// names it, 'ByHead' when only HEAD's does, 'No' otherwise; under a
/// parent with no goal, whose position implies no relationship,
/// 'ByGraph'. None for a child that carries no warrant.
pub(crate) fn warrant_of_child (
  goal     : Option<&Goal>,
  viewnode : &Viewnode,
) -> Option<Warrant> {
  let member : GoalMember = member_of_child (viewnode) ?;
  Some ( match goal {
    None => Warrant::ByGraph,
    Some (goal) => goal . item_for (&member)
      . map ( |item| item . warrant )
      . unwrap_or (Warrant::No), } ) }

fn set_warrants (
  tree   : &mut Tree<Viewnode>,
  parent : NodeId,
  goal   : Option<&Goal>,
) {
  let is_root : bool = matches! ( tree . get (parent) . unwrap () . value () . kind,
                                  ViewnodeKind::BufferRoot );
  for child in children_of (tree, parent) {
    let warrant : Warrant =
      if is_root { Warrant::ByGraph } // a root carries no warrant fact
      else { match warrant_of_child (
               goal, tree . get (child) . unwrap () . value () ) {
        Some (warrant) => warrant,
        None => continue, }};
    set_warrant (tree, child, warrant); }}

/// Store 'warrant' on the child at 'treeid', where its kind keeps one.
pub(crate) fn set_warrant (
  tree    : &mut Tree<Viewnode>,
  treeid  : NodeId,
  warrant : Warrant,
) {
  match &mut tree . get_mut (treeid) . unwrap () . value () . kind {
    ViewnodeKind::Vognode (Vognode::Unrestricted (occurrence)) =>
      occurrence . viewStats . isWarranted = warrant,
    ViewnodeKind::Vognode (Vognode::Ghost (ghost)) =>
      ghost . isWarranted = warrant,
    ViewnodeKind::PropertyMember (
      PropertyMember::Alias { isWarranted, .. }
      | PropertyMember::ID { isWarranted, .. }
      | PropertyMember::Flag { isWarranted, .. } ) =>
      *isWarranted = warrant,
    _ => {}, }}

//
// 4. claims
//

/// Under the buffer root every root claims. Under an editable
/// occurrence or an editable folder, each child that claims and that
/// the truth does not warrant (a ghost warrant included: the worktree
/// lacks that relationship) is marked 'noClaim'. Under any other
/// parent no child claims.
fn mark_claims (
  tree   : &mut Tree<Viewnode>,
  parent : NodeId,
  goal   : Option<&Goal>,
  walk   : &mut Walk,
) {
  let is_root : bool = matches! ( tree . get (parent) . unwrap () . value () . kind,
                                  ViewnodeKind::BufferRoot );
  let parent_is_editable : bool = parent_reads_claims (tree, parent);
  for child in children_of (tree, parent) {
    let viewnode : &Viewnode = tree . get (child) . unwrap () . value ();
    let Some (claims) = claims_membership (viewnode) else { continue; };
    if is_root {
      if viewnode . is_occurrence () { set_claim (tree, child, true); }}
    else if parent_is_editable {
      if claims && warrant_of_child (goal, viewnode) != Some (Warrant::ByGraph) {
        set_claim (tree, child, false);
        walk . corrections . record (tree, child, CorrectionKind::MarkedNoClaim); }}
    else if claims { set_claim (tree, child, false); }}}

/// Whether the viewnode claims membership; None for a kind that
/// carries no claim.
fn claims_membership (
  viewnode : &Viewnode,
) -> Option<bool> {
  match &viewnode . kind {
    ViewnodeKind::Vognode (Vognode::Unrestricted (occurrence)) =>
      Some (occurrence . claimsMembership),
    ViewnodeKind::Vognode (Vognode::Unknown (unknown)) =>
      Some (unknown . claimsMembership),
    ViewnodeKind::Vognode (Vognode::Ghost (ghost)) =>
      Some (ghost . claimsMembership),
    ViewnodeKind::PropertyMember (
      PropertyMember::Alias { claimsMembership, .. }
      | PropertyMember::ID { claimsMembership, .. } ) =>
      Some (*claimsMembership),
    _ => None, } }

pub(crate) fn set_claim (
  tree   : &mut Tree<Viewnode>,
  treeid : NodeId,
  claims : bool,
) {
  match &mut tree . get_mut (treeid) . unwrap () . value () . kind {
    ViewnodeKind::Vognode (Vognode::Unrestricted (occurrence)) =>
      occurrence . claimsMembership = claims,
    ViewnodeKind::Vognode (Vognode::Unknown (unknown)) =>
      unknown . claimsMembership = claims,
    ViewnodeKind::Vognode (Vognode::Ghost (ghost)) =>
      ghost . claimsMembership = claims,
    ViewnodeKind::PropertyMember (
      PropertyMember::Alias { claimsMembership, .. }
      | PropertyMember::ID { claimsMembership, .. } ) =>
      *claimsMembership = claims,
    _ => {}, }}

//
// 5. reinstate
//

/// Draw every goal item that no child stands for. Where the goal has
/// an order it is appended, and the reorder places it; where it has
/// none, it follows the last child the goal names (or, when none does,
/// leads), so that what the user placed after the members stays after
/// them. A drawn occurrence is editable as
/// 'drawn_occurrence_is_editable' says; a member of a folder whose
/// members carry the containerward chain gets its chain.
fn reinstate (
  tree   : &mut Tree<Viewnode>,
  parent : NodeId,
  goal   : &Goal,
  walk   : &mut Walk,
) -> Result<(), Box<dyn Error>> {
  let mut present : HashSet<GoalMember> =
    tree . get (parent) . unwrap () . children ()
    . filter_map ( |child| member_of_child (child . value ()) )
    . collect ();
  let mut last_named : Option<NodeId> = // the child a drawn one follows, where the goal has no order
    tree . get (parent) . unwrap () . children ()
    . filter ( |child| member_of_child (child . value ())
                       . is_some_and ( |member| goal . item_for (&member) . is_some () ))
    . map ( |child| child . id () )
    . last ();
  for item in &goal . items {
    if present . contains (&item . member) { continue; }
    let Some (drawn) = draw_item (tree, parent, item, goal, walk) ?
      else { continue; };
    if let Some (member) = member_of_child (&drawn) {
      // In the diff view two items can name one node: a list entry
      // the worktree cannot resolve, which HEAD resolves to a node
      // HEAD's list also names. Its ghost stands for both.
      if ! present . insert (member) { continue; }}
    let treeid : NodeId =
      tree . get_mut (parent) . unwrap () . append (drawn) . id ();
    if ! goal . ordered {
      match last_named {
        Some (named) => move_after (tree, named, treeid) ?,
        None => move_to_first_child (tree, parent, treeid) ?, }
      last_named = Some (treeid); }
    after_drawing (tree, parent, treeid, walk) ?; }
  Ok (( )) }

/// Book what a drawn child is: an editable occurrence claims its
/// node's editability; a folder member carries its chain. Then
/// report it, unless drawing it is extent ('Walk::record_drawn').
fn after_drawing (
  tree   : &mut Tree<Viewnode>,
  parent : NodeId,
  treeid : NodeId,
  walk   : &mut Walk,
) -> Result<(), Box<dyn Error>> {
  if let Some (gnid) = tree . get (treeid) . unwrap () . value () . editable_skgid () . cloned () {
    walk . editable_gnids . insert (gnid);
    walk . drawn_editable . insert (treeid);
    if walk . had_editableView_request . contains (&parent) {
      // The request extends the content it draws, as it extends the
      // content it shows ('extend_requested_content').
      walk . had_editableView_request . insert (treeid); }}
  let carries_chain : bool =
    matches! ( &tree . get (parent) . unwrap () . value () . kind,
               ViewnodeKind::Folder (folder)
                 if folder . members_carry_containerward_chain () )
    && tree . get (treeid) . unwrap () . value () . is_occurrence ();
  if carries_chain {
    let partners : ContainerwardChainPartners = match walk . mode {
      CompletionMode::ContentView => ContainerwardChainPartners::LiveGraph,
      CompletionMode::DiffView { straddle, .. } =>
        ContainerwardChainPartners::GraphStraddle (straddle), };
    attach_containerward_chains_under_members (
      tree, &[treeid], walk . truth . graph, walk . truth . config,
      walk . truth . restriction, partners ) ?; }
  if walk . newborn_folders . contains (&parent) {
    // A folder's collapse lives on its members.
    tree . get_mut (treeid) . unwrap () . value () . foldAndFocus . folded = true; }
  walk . record_drawn (tree, parent, treeid);
  Ok (( )) }

/// The viewnode that stands for 'item' under 'parent': for a gnid,
/// what the ID rule says (None if a tombstone, which there is no
/// point in drawing); for a property value, its member. It claims
/// membership only where the save reads claims, and not when only
/// HEAD warrants it.
fn draw_item (
  tree   : &Tree<Viewnode>,
  parent : NodeId,
  item   : &GoalItem,
  goal   : &Goal,
  walk   : &Walk,
) -> Result<Option<Viewnode>, Box<dyn Error>> {
  let claims : bool =
    item . warrant == Warrant::ByGraph && parent_reads_claims (tree, parent);
  Ok ( match &item . member {
    GoalMember::Gnid (gnid) => match id_rule (gnid, Some (goal), walk) {
      IdRuleAnswer::Occurrence (pid) => {
        let editable : bool =
          drawn_occurrence_is_editable (tree, parent, &pid, walk);
        let occurrence : UnrestrictedVognode =
          occurrence_from_truth (&pid, editable, claims, walk) ?;
        let mut viewnode : Viewnode = viewnode_of ( ViewnodeKind::Vognode (
          Vognode::Unrestricted ( UnrestrictedVognode {
            viewStats : ViewnodeStats { isWarranted : item . warrant,
                                        .. ViewnodeStats::default () },
            .. occurrence } )));
        if editable {
          diff_view::fold_body_of_drawn_occurrence (&mut viewnode, &pid, walk); }
        Some (viewnode) },
      IdRuleAnswer::Ghost (pid) =>
        ghost_from_head (&pid, walk) . map ( |ghost|
          viewnode_of ( ViewnodeKind::Vognode (Vognode::Ghost ( Ghost {
            claimsMembership : claims,
            isWarranted      : item . warrant,
            .. ghost } )))),
      IdRuleAnswer::Unknown =>
        Some ( viewnode_of ( ViewnodeKind::Vognode ( Vognode::Unknown ( Unknown {
          skgid                   : gnid . clone (),
          relRepo                 : item . relRepo . clone (),
          relationshipEditRequest : None,
          claimsMembership        : claims, } )))),
      IdRuleAnswer::Tombstone => None, },
    GoalMember::Alias (text) =>
      Some ( viewnode_of ( ViewnodeKind::PropertyMember ( PropertyMember::Alias {
        text                    : text . clone (),
        relRepo                 : item . relRepo . clone (),
        relationshipEditRequest : None,
        claimsMembership        : claims,
        isWarranted             : item . warrant, } ))),
    GoalMember::Id (id) =>
      Some ( viewnode_of ( ViewnodeKind::PropertyMember ( PropertyMember::ID {
        skgid            : id . clone (),
        claimsMembership : claims,
        isWarranted      : item . warrant, } ))),
    GoalMember::Flag (flag) =>
      Some ( viewnode_of ( ViewnodeKind::PropertyMember ( PropertyMember::Flag {
        flag        : *flag,
        title       : String::new (),
        body        : None,
        isWarranted : item . warrant, } ))), } ) }

//
// 6. reorder
//

/// The children the goal names take its order; every other child
/// rides right after the sibling it followed (those before the first
/// such child stay first).
fn reorder (
  tree   : &mut Tree<Viewnode>,
  parent : NodeId,
  goal   : &Goal,
) -> Result<(), Box<dyn Error>> {
  let children : Vec<NodeId> = children_of (tree, parent);
  let mut front : Vec<NodeId> = Vec::new ();
  let mut groups : Vec<(usize, Vec<NodeId>)> = Vec::new ();
  for child in &children {
    let position : Option<usize> =
      member_of_child (tree . get (*child) . unwrap () . value ())
      . and_then ( |member| goal . position_of (&member) );
    match (position, groups . last_mut ()) {
      (Some (position), _) => groups . push (( position, vec! [*child] )),
      (None, Some ((_, group))) => group . push (*child),
      (None, None) => front . push (*child), }}
  groups . sort_by_key ( |(position, _)| *position );
  let order : Vec<NodeId> =
    front . into_iter ()
    . chain ( groups . into_iter () . flat_map ( |(_, group)| group ))
    . collect ();
  if order != children {
    for child in order {
      move_to_last_child (tree, parent, child) ?; }}
  Ok (( )) }

/// In a subscribee folder, the compound folder of what the subscriber
/// hides outside its subscribees stands last, after the subscribees.
fn keep_hidden_outside_folder_last (
  tree   : &mut Tree<Viewnode>,
  parent : NodeId,
) -> Result<(), Box<dyn Error>> {
  if ! matches! ( tree . get (parent) . unwrap () . value () . kind,
                  ViewnodeKind::Folder (Folder::Subscribee) ) {
    return Ok (( )); }
  let hidden_outside : Option<NodeId> =
    tree . get (parent) . unwrap () . children ()
    . find ( |child| matches! ( child . value () . kind,
                                ViewnodeKind::Folder (Folder::HiddenOutsideOfSubscribee) ))
    . map ( |child| child . id () );
  if let Some (hidden_outside) = hidden_outside {
    move_to_last_child (tree, parent, hidden_outside) ?; }
  Ok (( )) }

//
// 7. substitution
//

/// In a content view, under an editable occurrence, an overridden
/// member is shown as its overrider, marked '(overridesHere N)' for
/// the original N, which the save collects instead. Not where the
/// parent is itself overridden and shown raw (a root, or a member of
/// a folder other than the subscribee folder): there its children are
/// raw too. When the override is gone, the shown overrider loses
/// 'overridesHere', is warranted only as itself (and so, unwarranted,
/// marked 'noClaim'), and the original is reinstated beside it. The
/// diff view substitutes nothing.
fn substitute_overriders (
  tree   : &mut Tree<Viewnode>,
  parent : NodeId,
  goal   : Option<&Goal>,
  walk   : &mut Walk,
) -> Result<(), Box<dyn Error>> {
  let Some (goal) = goal else { return Ok (( )); };
  if walk . is_diff_view ()
     || tree . get (parent) . unwrap () . value () . editable_skgid () . is_none ()
     || is_overridden_and_shown_raw (tree, parent, walk)
  { return Ok (( )); }
  for child in children_of (tree, parent) {
    let (gnid, overridesHere, warrant) : (ID, Option<ID>, Warrant) =
      match &tree . get (child) . unwrap () . value () . kind {
        ViewnodeKind::Vognode (Vognode::Unrestricted (occurrence)) =>
          ( occurrence . skgid . clone (), occurrence . overridesHere . clone (),
            occurrence . viewStats . isWarranted ),
        _ => continue, };
    match overridesHere {
      None => if warrant == Warrant::ByGraph {
        let effective : ID = resolve_override (
          walk . truth . config, walk . truth . graph, walk . truth . restriction,
          &gnid ) . effective;
        if effective != gnid {
          show_overrider (tree, child, &effective, Some (gnid), walk) ?; }},
      Some (original) => {
        let effective : ID = resolve_override (
          walk . truth . config, walk . truth . graph, walk . truth . restriction,
          &original ) . effective;
        if effective == original {
          stop_substituting (tree, parent, child, &original, goal, walk) ?;
        } else if ! carrier_on_owned_chain (
                      walk . truth . config, walk . truth . graph, &original, &gnid ) {
          show_overrider (tree, child, &effective, Some (original), walk) ?; }}, }}
  Ok (( )) }

/// Make the occurrence at 'child' show 'shown', with 'overridesHere'
/// as given, keeping where it stands, its fold state and its
/// editability, unless the node it now shows has its editable
/// occurrence elsewhere.
fn show_overrider (
  tree          : &mut Tree<Viewnode>,
  child         : NodeId,
  shown         : &ID,
  overridesHere : Option<ID>,
  walk          : &mut Walk,
) -> Result<(), Box<dyn Error>> {
  let node : &GraphnodeInRust = walk . truth . graph . get (shown)
    . ok_or ("show_overrider: the truth lacks the overrider") ?;
  let was_editable_as : Option<ID> =
    tree . get (child) . unwrap () . value () . editable_skgid () . cloned ();
  if let ViewnodeKind::Vognode (Vognode::Unrestricted (occurrence)) =
    &mut tree . get_mut (child) . unwrap () . value () . kind
  { occurrence . skgid         = shown . clone ();
    occurrence . title         = node . title . clone ();
    occurrence . home_skgrepo  = node . home_skgrepo . clone ();
    occurrence . overridesHere = overridesHere;
    if let Editability::Editable { body, .. } = &mut occurrence . editability
    { *body = node . body . clone (); }}
  if let Some (was) = was_editable_as {
    walk . editable_gnids . remove (&was);
    if walk . editable_gnids . contains (shown) {
      write_protect_occurrence (tree, child, walk);
    } else { walk . editable_gnids . insert ( shown . clone () ); }}
  Ok (( )) }

/// The override that 'child' stood for is gone: it shows itself, is
/// warranted only as itself, and the original is reinstated right
/// after it, unless a sibling already stands for it.
fn stop_substituting (
  tree     : &mut Tree<Viewnode>,
  parent   : NodeId,
  child    : NodeId,
  original : &ID,
  goal     : &Goal,
  walk     : &mut Walk,
) -> Result<(), Box<dyn Error>> {
  if let ViewnodeKind::Vognode (Vognode::Unrestricted (occurrence)) =
    &mut tree . get_mut (child) . unwrap () . value () . kind
  { occurrence . overridesHere = None; }
  let warrant : Warrant = warrant_of_child (
    Some (goal), tree . get (child) . unwrap () . value () )
    . unwrap_or (Warrant::No);
  set_warrant (tree, child, warrant);
  if warrant != Warrant::ByGraph
     && claims_membership (tree . get (child) . unwrap () . value ()) == Some (true) {
    set_claim (tree, child, false);
    walk . corrections . record (tree, child, CorrectionKind::MarkedNoClaim); }
  let member : GoalMember = GoalMember::Gnid ( original . clone () );
  let stands : bool = tree . get (parent) . unwrap () . children ()
    . any ( |sibling| member_of_child (sibling . value ()) . as_ref () == Some (&member) );
  if stands { return Ok (( )); }
  let Some (item) = goal . item_for (&member) else { return Ok (( )); };
  let item : GoalItem = item . clone ();
  if let Some (drawn) = draw_item (tree, parent, &item, goal, walk) ? {
    let treeid : NodeId = tree . get_mut (parent) . unwrap () . append (drawn) . id ();
    move_after (tree, child, treeid) ?;
    after_drawing (tree, parent, treeid, walk) ?; }
  Ok (( )) }

/// Whether the occurrence at 'treeid' is overridden and yet shown
/// raw, where it stands: at a root, or as a member of a folder other
/// than the subscribee folder.
fn is_overridden_and_shown_raw (
  tree   : &Tree<Viewnode>,
  treeid : NodeId,
  walk   : &Walk,
) -> bool {
  let node : NodeRef<Viewnode> = tree . get (treeid) . unwrap ();
  let raw_position : bool = node . parent () . is_some_and ( |parent|
    match &parent . value () . kind {
      ViewnodeKind::BufferRoot => true,
      ViewnodeKind::Folder (folder) =>
        ! folder . members_are_properties ()
        && ! matches! ( folder, Folder::Subscribee ),
      _ => false, } );
  let ViewnodeKind::Vognode (Vognode::Unrestricted (occurrence)) =
    &node . value () . kind else { return false; };
  raw_position
  && occurrence . overridesHere . is_none ()
  && resolve_override ( walk . truth . config, walk . truth . graph, None,
                        &occurrence . skgid ) . effective != occurrence . skgid }

//
// 8. requests
//

/// Fulfill and drop each child's view requests: a write-protect
/// request write-protects the occurrence (and, for the subtree form,
/// its content subtree); a folder or flags request adds the folders,
/// which their own visits fill from their goals; a tombstone request
/// entombstones a ghost. An editable-view request was fulfilled
/// before the walk ('fulfill_editableView_requests'), and a fork
/// request is the save's: it stays.
fn fulfill_view_requests (
  tree   : &mut Tree<Viewnode>,
  parent : NodeId,
  walk   : &mut Walk,
) -> Result<(), Box<dyn Error>> {
  for child in children_of (tree, parent) {
    let requests : Vec<ViewRequest> =
      match &tree . get (child) . unwrap () . value () . kind {
        ViewnodeKind::Vognode (Vognode::Unrestricted (occurrence)) =>
          occurrence . view_requests . iter () . copied ()
          . filter ( |request| *request != ViewRequest::Fork )
          . collect (),
        ViewnodeKind::Vognode (Vognode::Ghost (ghost)) => {
          if ghost . tombstone_request {
            entombstone (tree, child, walk); }
          continue; },
        _ => continue, };
    if requests . is_empty () { continue; }
    let mut requests : Vec<ViewRequest> = requests;
    requests . sort_by_key (folder_request_rank);
    let mut folders : Vec<Folder> = Vec::new ();
    for request in &requests {
      match request {
        ViewRequest::WriteProtected =>
          write_protect_occurrence (tree, child, walk),
        ViewRequest::WriteProtectedSubtree =>
          write_protect_content_subtree (tree, child, walk),
        ViewRequest::Folder (relation) =>
          folders . extend ( folders_of_relation (*relation) ),
        ViewRequest::Flags => folders . push (Folder::flags ()),
        ViewRequest::Editable | ViewRequest::Fork | ViewRequest::Tombstone => {}, }}
    add_folders (tree, child, &folders, walk);
    if let ViewnodeKind::Vognode (Vognode::Unrestricted (occurrence)) =
      &mut tree . get_mut (child) . unwrap () . value () . kind
    { occurrence . view_requests . retain ( |request| *request == ViewRequest::Fork ); }}
  Ok (( )) }

/// Where the folders a request adds stand among those added at one
/// visit, first first: the containers folder, the mentioners, the
/// aliases, then the subscription, override and hiding folders, the
/// flags last. (A node's view requests are a set.)
fn folder_request_rank (
  request : &ViewRequest,
) -> usize {
  match request {
    ViewRequest::Folder (FolderRelation::Containers)    => 0,
    ViewRequest::Folder (FolderRelation::Mentioners)    => 1,
    ViewRequest::Folder (FolderRelation::Aliases)       => 2,
    ViewRequest::Folder (FolderRelation::SubscribesTo)  => 3,
    ViewRequest::Folder (FolderRelation::Overrides)     => 4,
    ViewRequest::Folder (FolderRelation::HidesFromSubs) => 5,
    ViewRequest::Flags                                  => 6,
    ViewRequest::Editable | ViewRequest::WriteProtected
      | ViewRequest::WriteProtectedSubtree
      | ViewRequest::Fork | ViewRequest::Tombstone      => 7, } }

/// The folders a '(folder RELNAME)' request adds: both of the
/// relation's, or the one whose other side the node shows without a
/// folder (its content, its links).
fn folders_of_relation (
  relation : FolderRelation,
) -> Vec<Folder> {
  match relation {
    FolderRelation::Aliases       => vec! [ Folder::Alias ],
    FolderRelation::Overrides     => vec! [ Folder::Overridden, Folder::Overrider ],
    FolderRelation::HidesFromSubs => vec! [ Folder::Hider, Folder::Hidden ],
    FolderRelation::SubscribesTo  => vec! [ Folder::Subscribee, Folder::Subscriber ],
    FolderRelation::Containers    => vec! [ Folder::Container ],
    FolderRelation::Mentioners    => vec! [ Folder::Mentioner ], } }

/// Add each of 'folders' the occurrence at 'treeid' lacks, ahead of
/// its other children, in the order given. A subscribee folder holds
/// the compound folder of what the subscriber hides outside its
/// subscribees, when the subscriber hides something there; new, its
/// members are drawn folded ('Walk::newborn_folders').
fn add_folders (
  tree    : &mut Tree<Viewnode>,
  treeid  : NodeId,
  folders : &[Folder],
  walk    : &mut Walk,
) {
  let mut added : Vec<NodeId> = Vec::new ();
  for folder in folders {
    let folder_treeid : NodeId = match child_folder (tree, treeid, folder) {
      Some (present) => present,
      None => {
        let new : NodeId = tree . get_mut (treeid) . unwrap ()
          . append ( viewnode_of ( ViewnodeKind::Folder (folder . clone ()) )) . id ();
        added . push (new);
        walk . added_folders . insert (new);
        new }, };
    let hides_outside_its_subscribees = || -> bool {
      occurrence_pid ( tree . get (treeid) . unwrap () )
        . is_some_and ( |subscriber|
          ! hidden_outside_of_subscribees (&subscriber, walk) . is_empty () ) };
    if matches! ( folder, Folder::Subscribee )
       && child_folder (tree, folder_treeid, &Folder::HiddenOutsideOfSubscribee) . is_none ()
       && hides_outside_its_subscribees ()
    { let hidden_outside : NodeId = tree . get_mut (folder_treeid) . unwrap ()
        . append ( viewnode_of (
          ViewnodeKind::Folder (Folder::HiddenOutsideOfSubscribee) )) . id ();
      walk . newborn_folders . insert (hidden_outside);
      walk . added_folders . insert (hidden_outside); }}
  for folder_treeid in added . into_iter () . rev () {
    move_to_first_child (tree, treeid, folder_treeid)
      . expect ("add_folders: the folder was just added"); }}

/// The child of 'under' that is a folder of the kind of 'folder' (a
/// flags folder whatever its text), if any.
fn child_folder (
  tree   : &Tree<Viewnode>,
  under  : NodeId,
  folder : &Folder,
) -> Option<NodeId> {
  tree . get (under) . unwrap () . children ()
    . find ( |child| matches! (
      &child . value () . kind,
      ViewnodeKind::Folder (present)
        if discriminant (present) == discriminant (folder) ))
    . map ( |child| child . id () ) }

/// Write-protect the occurrence at 'treeid' and every occurrence of
/// its content subtree.
fn write_protect_content_subtree (
  tree   : &mut Tree<Viewnode>,
  treeid : NodeId,
  walk   : &mut Walk,
) {
  let mut pending : Vec<NodeId> = vec! [ treeid ];
  while let Some (occurrence) = pending . pop () {
    write_protect_occurrence (tree, occurrence, walk);
    pending . extend (
      tree . get (occurrence) . unwrap () . children ()
      . filter ( |child| child . value () . is_occurrence () )
      . map ( |child| child . id () )); }}

//
// after the ladder
//

/// A folder left childless is entombstoned, so that the tail prunes
/// it, unless the user could add something there
/// ('folder_goes_when_empty').
fn entombstone_if_an_empty_folder (
  tree   : &mut Tree<Viewnode>,
  parent : NodeId,
) {
  let empty_folder : bool = {
    let node : NodeRef<Viewnode> = tree . get (parent) . unwrap ();
    node . children () . next () . is_none ()
    && folder_goes_when_empty ( node . value () ) };
  if empty_folder {
    tree . get_mut (parent) . unwrap () . value ()
      . replace_kind (ViewnodeKind::Tombstone); }}

/// Whether the viewnode is a folder that goes when it holds nothing:
/// any but one whose membership the save reads, or a flags folder,
/// where the user could add something.
pub(crate) fn folder_goes_when_empty (
  viewnode : &Viewnode,
) -> bool {
  matches! ( &viewnode . kind,
             ViewnodeKind::Folder (folder)
               if ! folder . is_editable () && ! folder . is_flags () ) }

/// The children of 'parent', in order.
pub(crate) fn children_of (
  tree   : &Tree<Viewnode>,
  parent : NodeId,
) -> Vec<NodeId> {
  tree . get (parent) . unwrap () . children () . map ( |child| child . id () ) . collect () }
