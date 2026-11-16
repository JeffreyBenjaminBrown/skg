//! The visit of a parent: the refresh's rungs, applied at each
//! parent P to its children, in order ('docs/data-model_technical.org',
//! "The walk and the refresh's rungs"):
//!  1. re-kind: each child that stands for a gnid becomes what the ID
//!     rule says, where it stands (in a content view an occurrence
//!     the rule makes a tombstone vanishes instead); each descriptor
//!     (folder, property member, change viewnode) whose position the
//!     ancestry table forbids, or whose change is gone, is
//!     entombstoned, and so is an alias whose record the skgrepo
//!     restriction keeps out;
//!  2. viewnode merge: duplicate viewnode siblings, standing for one
//!     gnid, are merged into one;
//!  3. the goal and the warrant: each child's warrant is set from P's
//!     goal;
//!  4. claims: under an editable parent, every child that claims and
//!     that the truth does not warrant vanishes, with its subtree;
//!     under any other, no child claims;
//!  5. reinstate: every goal item without a child is drawn, within the
//!     extent;
//!  6. reorder: where the goal has an order, the warranted children
//!     take it, and every other child stays right after the sibling it
//!     followed;
//!  7. substitution: in a content view a node is shown as the mirror
//!     the perspective puts in its place;
//!  8. requests: each child's view requests are fulfilled and dropped,
//!     but for a fork request, which stays for the save.
//! Before them, an editable-view request on P is consumed, making P
//! editable if the budget allows, and P charges the budget
//! ('consume_editableView_request' in 'editability.rs'); the request
//! passes on to the content P already shows
//! ('extend_requested_content'); and P's default folders are added
//! ('add_default_folders'); then P's goal is computed. The request
//! goes when the visit ends. Between 5
//! and 6 the diff view draws its change viewnodes and tags the
//! children with their changes ('diff_view.rs'); after 6, in a
//! subscribee folder, the folder of what the subscriber hides outside
//! its subscribees goes last; after them, a folder left with no
//! children and nothing the user could add there is entombstoned, so
//! that the tail prunes it.

use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::in_rust_graph::relation_accessors::NodeRelation;
use crate::dbs::in_rust_graph::mirrors::{
  mirror_shown, perspective_owner_of_tree_containing};
use crate::refresh::editability::{
  DrawnEditability, EditabilityAndClaim, carries_editableView_request,
  consume_editableView_request,
  drawn_occurrence_editability, drop_editableView_request, extend_requested_content,
  merge_editability, write_protect_occurrence};
use crate::refresh::extent::{Extent, extent_of_parent};
use crate::refresh::goal::{
  Goal, GoalItem, GoalMember, goal_of_parent, hidden_in_subscribee,
  hidden_outside_of_subscribees, member_of_child,
  subscriber_of_subscribee_as_such, visible_illegal_edits};
use crate::refresh::goal::partnerFolder_members_in_graph;
use crate::telescope::types::IllegalEdit;
use crate::refresh::id_rule::{
  IdRuleAnswer, WhereTheHeadlineStands, entombstone, id_rule, point_occurrence_at,
  rekind_vognode, vognode_for_answer};
use crate::refresh::Walk;
use crate::refresh::ancestry::{
  described_node, is_unsayable, parent_reads_claims, view_ancestor_gnids};
use crate::refresh::mode::RefreshMode;
use crate::refresh::tail::remove_viewnode_transferring_focus;
use crate::types::nodes::rust::GraphnodeInRust;
use crate::refresh::diff_view;
use crate::refresh::chain::draw_containerward_chain;
use crate::skgrepo_sets::SkgrepoRestriction;
use crate::types::misc::{ID, SkgrepoOwner};
use crate::types::tree::moves::{move_after, move_to_first_child, move_to_last_child};
use crate::types::views_state::OverridesFollowed;
use crate::types::viewnode::{
  FoldAndFocus, Folder, FolderRelation, PropertyMember, ViewRequest, Viewnode,
  ViewnodeKind, Vognode, Warrant, viewnode_of};

use ego_tree::{NodeId, NodeMut, NodeRef, Tree};
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::mem::discriminant;

/// Run the refresh's rungs at 'parent', whose own kind is settled.
pub(crate) fn visit_parent (
  tree   : &mut Tree<Viewnode>,
  parent : NodeId,
  walk   : &mut Walk,
) -> Result<(), Box<dyn Error>> {
  consume_editableView_request (tree, parent, walk);
  extend_requested_content (tree, parent, walk);
  add_default_folders (tree, parent, walk);
  add_illegal_edit_folder (tree, parent, walk);
  let goal : Option<Goal> = goal_of_parent (tree, parent, walk);
  rekind_children (tree, parent, goal . as_ref (), walk) ?;
  viewnode_merge (tree, parent, walk) ?;
  set_warrants (tree, parent, goal . as_ref (), walk);
  mark_claims (tree, parent, goal . as_ref (), walk) ?;
  if let Some (goal) = &goal {
    if extent_of_parent (tree, parent, walk) == Extent::All {
      reinstate (tree, parent, goal, walk) ?; }}
  diff_view::draw_change_viewnodes (tree, parent, walk) ?;
  diff_view::tag_children (tree, parent, goal . as_ref (), walk) ?;
  if let Some (goal) = &goal {
    if goal . ordered {
      reorder (tree, parent, goal) ?; }}
  keep_hidden_outside_folder_last (tree, parent) ?;
  substitute_mirrors (tree, parent, goal . as_ref (), walk) ?;
  fulfill_view_requests (tree, parent, walk) ?;
  entombstone_if_an_empty_folder (tree, parent);
  drop_editableView_request (tree, parent);
  Ok (( )) }

/// The folders a content view shows without being asked, as it
/// always has: an occurrence that an editable-view request extends
/// (a fresh view's roots, and every occurrence the refresh draws
/// editable, among them) shows its subscribee folder when
/// it subscribes to something the skgrepo restriction shows (with the folder
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
  let Some (pid) : Option<ID> =
    tree . get (parent) . unwrap () . value () . editable_gnid () . cloned ()
    else { return; };
  if carries_editableView_request (tree, parent)
     && partnerFolder_members_in_graph (
          walk . truth . graph, &pid, &Folder::Subscribee,
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
      walk . added_compound_folders . insert (folder); }}}

/// Under an occurrence of a foreign node that the user illegally
/// edited ('InRustGraph::illegal_edits'), in a skgrepo the skgrepo
/// restriction shows, the illegal-edit folder, first, its title and
/// body those of the illegal sections, redrawn at every refresh. Not
/// in the diff view.
fn add_illegal_edit_folder (
  tree   : &mut Tree<Viewnode>,
  parent : NodeId,
  walk   : &mut Walk,
) {
  if walk . is_diff_view () { return; }
  let Some (pid) : Option<ID> = occurrence_pid (tree, parent) else { return; };
  let edits : Vec<IllegalEdit> = visible_illegal_edits (&pid, walk);
  if edits . is_empty () { return; }
  let drawn : Folder = Folder::IllegalEdit {
    title : edits . iter ()
      . filter_map ( |edit| edit . title . clone () )
      . collect::<Vec<String>> () . join (" / "),
    body : { let bodies : Vec<String> = edits . iter ()
               . filter_map ( |edit| edit . body . clone () ) . collect ();
             if bodies . is_empty () { None } else { Some ( bodies . join ("\n") ) }}, };
  match child_folder (tree, parent, &drawn) {
    Some (present) => {
      tree . get_mut (present) . unwrap () . value () . kind =
        ViewnodeKind::Folder (drawn); },
    None => {
      let new : NodeId = tree . get_mut (parent) . unwrap ()
        . append ( viewnode_of ( ViewnodeKind::Folder (drawn) )) . id ();
      move_to_first_child (tree, parent, new)
        . expect ("add_illegal_edit_folder: the folder was just added"); }, }}

/// The pid of the occurrence at 'treeid', if it is one.
fn occurrence_pid (
  tree   : &Tree<Viewnode>,
  treeid : NodeId,
) -> Option<ID> {
  match &tree . get (treeid) . unwrap () . value () . kind {
    ViewnodeKind::Vognode (Vognode::Occurrence (occurrence)) =>
      Some ( occurrence . gnid . clone () ),
    _ => None, }}

/// Whether the illegal-edit folder at 'child', under 'parent', no
/// longer stands for an illegal edit: its viewparent is no occurrence
/// of a node the user illegally edited, as the skgrepo restriction
/// lets it be seen, or this is the diff view.
fn illegal_edit_is_gone (
  tree   : &Tree<Viewnode>,
  parent : NodeId,
  child  : NodeId,
  walk   : &Walk,
) -> bool {
  if ! matches! ( tree . get (child) . unwrap () . value () . kind,
                  ViewnodeKind::Folder (Folder::IllegalEdit { .. }) ) {
    return false; }
  walk . is_diff_view ()
  || occurrence_pid (tree, parent)
     . is_none_or ( |pid| visible_illegal_edits (&pid, walk) . is_empty () ) }

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
        if is_unsayable (tree, child)
           || alias_whose_record_is_restricted (tree, child, walk)
           || diff_view::change_is_gone (tree, child, walk)
           || illegal_edit_is_gone (tree, parent, child, walk) {
          entombstone (tree, child); },
      ViewnodeKind::Tombstone | ViewnodeKind::BufferRoot => {}, }}
  Ok (( )) }

/// Whether the headline at 'treeid' is an alias whose record the
/// skgrepo restriction keeps out: the node its folder describes has
/// the alias (in the diff view, in the worktree or at HEAD) and no
/// record of it the skgrepo restriction shows. The property goal
/// omits such an alias, as a content goal omits a member whose record
/// is restricted; this takes it from a view drawn before the
/// skgrepo restriction narrowed.
fn alias_whose_record_is_restricted (
  tree   : &Tree<Viewnode>,
  treeid : NodeId,
  walk   : &Walk,
) -> bool {
  let restriction : &SkgrepoRestriction = walk . truth . restriction;
  if restriction . is_unrestricted () { return false; }
  let ViewnodeKind::PropertyMember (PropertyMember::Alias { text, .. }) =
    &tree . get (treeid) . unwrap () . value () . kind else { return false; };
  let Some (pid) : Option<ID> =
    described_node (tree, tree . get (treeid) . unwrap () . parent () . unwrap () . id ())
    . and_then ( |described| tree . get (described) ? . value () . occurrence_gnid () . cloned () )
    else { return false; };
  let head : Option<&GraphnodeInRust> = match walk . mode {
    RefreshMode::DiffView { straddle, .. } => straddle . before . get (&pid),
    RefreshMode::ContentView => None, };
  let nodes : Vec<&GraphnodeInRust> =
    walk . truth . graph . get (&pid) . into_iter () . chain (head) . collect ();
  let has_it : bool = nodes . iter () . any (
    |node| node . alias_texts () . contains (text) );
  let shows_it : bool = nodes . iter () . any (
    |node| node . aliases_with_relRepo_unless_home (restriction) . iter ()
           . any ( |(visible, _)| visible == text ));
  has_it && ! shows_it }

//
// 2. viewnode merge
//

/// Merge each set of duplicate viewnode siblings (siblings standing
/// for one gnid) into one viewnode, the result, which stands where
/// the first of them stood ('merge_into' says how the result is
/// made). The result's vnid is cleared, so that the tail mints a
/// fresh one. Only siblings, standing for one gnid under one parent, are ever merged:
/// two occurrences of a node elsewhere in the buffer stay apart.
fn viewnode_merge (
  tree   : &mut Tree<Viewnode>,
  parent : NodeId,
  walk   : &mut Walk,
) -> Result<(), Box<dyn Error>> {
  for (_gnid, duplicates) in duplicate_viewnode_siblings (tree, parent) {
    let result : NodeId = duplicates [0];
    for merged_sibling in &duplicates [1..] {
      merge_into (tree, result, *merged_sibling, walk) ?; }
    tree . get_mut (result) . unwrap () . value () . viewnode_id = None; }
  Ok (( )) }

/// The children of 'parent' grouped by the gnid they stand for, each
/// group in sibling order and with its gnid, the groups in order of
/// their first member; only the groups of more than one.
fn duplicate_viewnode_siblings (
  tree   : &Tree<Viewnode>,
  parent : NodeId,
) -> Vec<(ID, Vec<NodeId>)> {
  let mut groups : Vec<(ID, Vec<NodeId>)> = Vec::new ();
  let mut group_of_gnid : HashMap<ID, usize> = HashMap::new ();
  for child in children_of (tree, parent) {
    let Some (GoalMember::Gnid (gnid)) : Option<GoalMember> =
      vognode_member (tree . get (child) . unwrap () . value ())
      else { continue; };
    match group_of_gnid . get (&gnid) {
      Some (group) => groups [*group] . 1 . push (child),
      None => { group_of_gnid . insert (gnid . clone (), groups . len ());
                groups . push ( (gnid, vec! [child]) ); }, }}
  groups . into_iter ()
    . filter ( |(_, group)| group . len () > 1 )
    . collect () }

/// What a vognode stands for; None for any other kind.
fn vognode_member (
  viewnode : &Viewnode,
) -> Option<GoalMember> {
  match &viewnode . kind {
    ViewnodeKind::Vognode (_) => member_of_child (viewnode),
    _ => None, } }

/// Merge 'merged_sibling' into 'result', a duplicate earlier among
/// its siblings, and remove it. The result is folded only if both
/// were, and so is its body; it is focused if either was, claims
/// membership if either did, carries the requests of both
/// ('merge_requests') and its own processed request, else the merged
/// sibling's, and is editable if either was ('merge_editability'). It
/// takes the merged sibling's children after its own; the claims rung
/// of its own visit judges them, as it judges its own children.
fn merge_into (
  tree           : &mut Tree<Viewnode>,
  result         : NodeId,
  merged_sibling : NodeId,
  walk           : &mut Walk,
) -> Result<(), Box<dyn Error>> {
  let sibling_viewnode : Viewnode =
    tree . get (merged_sibling) . unwrap () . value () . clone ();
  { let mut result_mut : NodeMut<Viewnode> = tree . get_mut (result) . unwrap ();
    let result_viewnode : &mut Viewnode = result_mut . value ();
    merge_fold_and_focus ( &mut result_viewnode . foldAndFocus,
                           &sibling_viewnode . foldAndFocus );
    if result_viewnode . processed_request . is_none () {
      result_viewnode . processed_request =
        sibling_viewnode . processed_request . clone (); }
    merge_requests ( &mut result_viewnode . kind,
                     &sibling_viewnode . kind ); }
  if sibling_viewnode . claims () == Some (true) {
    // Validation lets at most one claiming child of a parent stand
    // for a gnid ('claiming_children_have_distinct_gnids'), so no
    // claim is lost.
    tree . get_mut (result) . unwrap () . value () . set_claim (true); }
  merge_editability (tree, result, merged_sibling, &sibling_viewnode, walk);
  for grandchild in children_of (tree, merged_sibling) {
    move_to_last_child (tree, result, grandchild) ?; }
  tree . get_mut (merged_sibling) . unwrap () . detach ();
  Ok (( )) }

/// Folded, and body-folded, only if both were; focused if either was.
fn merge_fold_and_focus (
  result         : &mut FoldAndFocus,
  merged_sibling : &FoldAndFocus,
) {
  result . folded     = result . folded     && merged_sibling . folded;
  result . bodyFolded = result . bodyFolded && merged_sibling . bodyFolded;
  result . focused    = result . focused    || merged_sibling . focused; }

/// The result carries the union of the view requests, and the
/// relationship request of whichever carried one (a gnid-only
/// vognode's too); on a ghost, the restoreNode request, if
/// either carried it. The node edit request rides with the
/// editability ('merge_editability').
fn merge_requests (
  result         : &mut ViewnodeKind,
  merged_sibling : &ViewnodeKind,
) {
  match ( result, merged_sibling ) {
    ( ViewnodeKind::Vognode (Vognode::Occurrence (result)),
      ViewnodeKind::Vognode (Vognode::Occurrence (merged)) ) => {
      result . view_requests . extend ( merged . view_requests . iter () . copied () );
      if result . relationshipEditRequest . is_none () {
        result . relationshipEditRequest = merged . relationshipEditRequest . clone (); }},
    ( ViewnodeKind::Vognode (Vognode::Ghost (result)),
      ViewnodeKind::Vognode (Vognode::Ghost (merged)) ) => {
      result . restoreNode_request |= merged . restoreNode_request; },
    ( ViewnodeKind::Vognode (Vognode::GnidOnly (result)),
      ViewnodeKind::Vognode (Vognode::GnidOnly (merged)) ) => {
      if result . relationshipEditRequest . is_none () {
        result . relationshipEditRequest = merged . relationshipEditRequest . clone (); }},
    _ => {}, }}

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

/// Set each child's warrant ('warrant_of_child'); a root's is
/// 'ByGraph', carrying no warrant fact. Under a tombstone, which has
/// no goal, an occurrence of an orphan graphnode (one no list the
/// skgrepo restriction shows contains) is unwarranted, as it is under
/// a write-protected occurrence ('orphan_under_a_tombstone').
fn set_warrants (
  tree   : &mut Tree<Viewnode>,
  parent : NodeId,
  goal   : Option<&Goal>,
  walk   : &Walk,
) {
  let is_root : bool = matches! ( tree . get (parent) . unwrap () . value () . kind,
                                  ViewnodeKind::BufferRoot );
  for child in children_of (tree, parent) {
    let warrant : Warrant =
      if is_root { Warrant::ByGraph } // a root carries no warrant fact
      else if orphan_under_a_tombstone (tree, parent, child, walk) { Warrant::No }
      else { match warrant_of_child (
               goal, tree . get (child) . unwrap () . value () ) {
        Some (warrant) => warrant,
        None => continue, }};
    tree . get_mut (child) . unwrap () . value () . set_warrant (warrant); }}

/// Whether 'child', under 'parent', is an occurrence of an orphan
/// graphnode under a tombstone: a node that no list the skgrepo
/// restriction shows contains, as a headline the user typed there
/// and saved is.
fn orphan_under_a_tombstone (
  tree   : &Tree<Viewnode>,
  parent : NodeId,
  child  : NodeId,
  walk   : &Walk,
) -> bool {
  matches! ( tree . get (parent) . unwrap () . value () . kind, ViewnodeKind::Tombstone )
  && tree . get (child) . unwrap () . value () . occurrence_gnid ()
     . is_some_and ( |pid| walk . truth . graph . inbound_pids_for_relation_gated (
       pid, NodeRelation::Contains, walk . truth . restriction ) . is_empty () ) }

//
// 4. claims
//

/// Under the buffer root every occurrence root claims; any other root
/// keeps what it carries. Under an editable occurrence or an editable
/// folder, each child that claims and that the truth does not warrant
/// (a ghost warrant included: the worktree lacks that relationship)
/// vanishes ('vanish_if_unwarranted'); a child that claims nothing (a
/// pending detachment the user wrote) stays. Under any other parent
/// no child claims; but an unwarranted child of a write-protected
/// folder vanishes where the folder no longer stands for it at all
/// ('leaves_its_write_protected_folder'): under the hides folder, a
/// graphnode a save unhid; under a folder that omits view-ancestors,
/// one the view shows above the node the folder describes. A hide the
/// skgrepo restriction keeps out stays, unwarranted, for a skgrepo
/// widening to show again.
fn mark_claims (
  tree   : &mut Tree<Viewnode>,
  parent : NodeId,
  goal   : Option<&Goal>,
  walk   : &mut Walk,
) -> Result<(), Box<dyn Error>> {
  let is_root : bool = matches! ( tree . get (parent) . unwrap () . value () . kind,
                                  ViewnodeKind::BufferRoot );
  let claims_are_read : bool = parent_reads_claims (tree, parent);
  for child in children_of (tree, parent) {
    let viewnode : &Viewnode = tree . get (child) . unwrap () . value ();
    let Some (claims) : Option<bool> = viewnode . claims () else { continue; };
    if is_root {
      if viewnode . is_occurrence () {
        tree . get_mut (child) . unwrap () . value () . set_claim (true); }}
    else if claims_are_read {
      let warrant : Option<Warrant> = warrant_of_child (goal, viewnode);
      vanish_if_unwarranted (tree, child, warrant, walk) ?; }
    else if warrant_of_child (goal, viewnode) == Some (Warrant::No)
            && leaves_its_write_protected_folder (tree, parent, child, walk) {
      vanish (tree, child, walk) ?; }
    else if claims {
      tree . get_mut (child) . unwrap () . value () . set_claim (false); }}
  Ok (( )) }

/// Whether 'child', unwarranted under a write-protected folder at
/// 'parent', vanishes from it rather than staying marked: the folder
/// no longer stands for it by any record, visible or not
/// ('unhidden_in_the_hides_folder'), or by design
/// ('view_ancestor_in_its_folder').
fn leaves_its_write_protected_folder (
  tree   : &Tree<Viewnode>,
  parent : NodeId,
  child  : NodeId,
  walk   : &Walk,
) -> bool {
  unhidden_in_the_hides_folder (tree, parent, child, walk)
  || view_ancestor_in_its_folder (tree, parent, child) }

/// Whether 'child' stands in a folder at 'parent' that omits
/// view-ancestors ('Folder::omits_view_ancestors') for a gnid the view
/// shows above the node the folder describes.
fn view_ancestor_in_its_folder (
  tree   : &Tree<Viewnode>,
  parent : NodeId,
  child  : NodeId,
) -> bool {
  let omits : bool = matches! ( &tree . get (parent) . unwrap () . value () . kind,
                                ViewnodeKind::Folder (folder)
                                  if folder . omits_view_ancestors () );
  if ! omits { return false; }
  let Some (described) : Option<NodeId> = described_node (tree, parent)
    else { return false; };
  let Some (GoalMember::Gnid (gnid)) : Option<GoalMember> =
    member_of_child ( tree . get (child) . unwrap () . value () )
    else { return false; };
  view_ancestor_gnids (tree, described) . contains (&gnid) }

/// Whether 'child' stands in the hides folder at 'parent' for a
/// graphnode its described node hides by no record at all, visible or
/// not: a hide a save removed.
fn unhidden_in_the_hides_folder (
  tree   : &Tree<Viewnode>,
  parent : NodeId,
  child  : NodeId,
  walk   : &Walk,
) -> bool {
  if ! matches! ( tree . get (parent) . unwrap () . value () . kind,
                  ViewnodeKind::Folder (Folder::Hidden) ) { return false; }
  let Some (hider) : Option<ID> = described_node (tree, parent)
    . and_then ( |described| tree . get (described) . unwrap () . value ()
                 . occurrence_gnid () . cloned () )
    else { return false; };
  let Some (hidden) : Option<ID> =
    tree . get (child) . unwrap () . value () . occurrence_gnid () . cloned ()
    else { return false; };
  let graph : &InRustGraph = walk . truth . graph;
  ! graph . outbound_gnids_for_relation (&hider, NodeRelation::HidesFromSubs)
    . iter () . any ( |gnid| graph . pid_else_raw_id (gnid) == hidden ) }

/// Where the save reads claims: the child at 'treeid', if it claims
/// and its warrant is not 'ByGraph', vanishes with its subtree.
///
/// Marking it 'noClaim' and keeping it instead would lose data: a
/// member whose record alone a skgrepo narrowing keeps out would be
/// marked so, the mark would outlive the skgrepo widening, and a save
/// would read it as a detachment and delete the record. Vanished, the
/// member is drawn again by the widening's refresh, where the goal
/// names it.
fn vanish_if_unwarranted (
  tree    : &mut Tree<Viewnode>,
  treeid  : NodeId,
  warrant : Option<Warrant>,
  walk    : &mut Walk,
) -> Result<(), Box<dyn Error>> {
  if tree . get (treeid) . unwrap () . value () . claims () == Some (true)
     && warrant != Some (Warrant::ByGraph) {
    vanish (tree, treeid, walk) ?; }
  Ok (( )) }

/// Remove the headline at 'treeid' and its subtree from the view: it
/// vanishes. Each editable occurrence in the subtree gives up its
/// node's editability, and focus inside the subtree moves to its
/// viewparent.
pub(crate) fn vanish (
  tree   : &mut Tree<Viewnode>,
  treeid : NodeId,
  walk   : &mut Walk,
) -> Result<(), Box<dyn Error>> {
  let editable : Vec<(ID, NodeId)> =
    tree . get (treeid) . unwrap () . descendants ()
    . filter_map ( |node| node . value () . editable_gnid ()
                   . map ( |gnid| ( gnid . clone (), node . id () )) )
    . collect ();
  for (gnid, occurrence) in editable {
    walk . release_editability (&gnid, occurrence); }
  remove_viewnode_transferring_focus (tree, treeid) }

//
// 5. reinstate
//

/// Draw every goal item that no child stands for. Where the goal has
/// an order it is appended, and the reorder places it; where it has
/// none, it follows the last child the goal names (or, when none does,
/// leads), so that what the user placed after the members stays after
/// them. A drawn occurrence stands as
/// 'drawn_occurrence_editability' says; a member of a folder whose
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
    let Some (drawn) : Option<Viewnode> = draw_item (tree, parent, item, goal, walk) ?
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

/// Book what a drawn child is: an editable occurrence holds its
/// node's editability ('Walk::hold_editability'); a folder member
/// carries its chain; under a compound folder the refresh added, it
/// is drawn folded.
fn after_drawing (
  tree   : &mut Tree<Viewnode>,
  parent : NodeId,
  treeid : NodeId,
  walk   : &mut Walk,
) -> Result<(), Box<dyn Error>> {
  if let Some (gnid) = tree . get (treeid) . unwrap () . value () . editable_gnid () . cloned () {
    walk . hold_editability (&gnid, treeid); }
  let carries_chain : bool =
    matches! ( &tree . get (parent) . unwrap () . value () . kind,
               ViewnodeKind::Folder (folder)
                 if folder . members_carry_containerward_chain () )
    && tree . get (treeid) . unwrap () . value () . is_occurrence ();
  if carries_chain {
    draw_containerward_chain (tree, treeid, walk) ?; }
  if walk . added_compound_folders . contains (&parent) {
    // A folder's collapse lives on its members.
    tree . get_mut (treeid) . unwrap () . value () . foldAndFocus . folded = true; }
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
    GoalMember::Gnid (gnid) => {
      let answer : IdRuleAnswer =
        id_rule (gnid, WhereTheHeadlineStands::Under (goal), walk);
      let editability : DrawnEditability = match &answer {
        IdRuleAnswer::Occurrence (pid) =>
          drawn_occurrence_editability (tree, parent, pid, walk),
        _ => DrawnEditability::WriteProtected, };
      vognode_for_answer (
        answer, gnid, item, EditabilityAndClaim { editability, claims }, walk ) ? },
    GoalMember::Alias (text) =>
      Some ( viewnode_of ( ViewnodeKind::PropertyMember ( PropertyMember::Alias {
        text                    : text . clone (),
        relRepo                 : item . relRepo . clone (),
        relationshipEditRequest : None,
        claimsMembership        : claims,
        warrant                 : item . warrant, } ))),
    GoalMember::Id (id) =>
      Some ( viewnode_of ( ViewnodeKind::PropertyMember ( PropertyMember::ID {
        gnid             : id . clone (),
        claimsMembership : claims,
        warrant          : item . warrant, } ))),
    GoalMember::Flag (flag) =>
      Some ( viewnode_of ( ViewnodeKind::PropertyMember ( PropertyMember::Flag {
        flag    : *flag,
        title   : String::new (),
        body    : None,
        warrant : item . warrant, } ))), } ) }

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

/// In a content view, under an editable occurrence, each member
/// stands as the mirror the perspective puts in its place
/// ('server/dbs/in_rust_graph/mirrors.rs'), marked '(overridesHere N)'
/// for the original N, which the save collects instead. The
/// perspective is that of the author of the parent's top-level tree's
/// root ('perspective_owner_of_tree_containing'): the user's under an
/// owned root, that author's under a foreign one; or one owner's
/// throughout, in a view switched to that owner's perspective. Not
/// where the parent itself is shown raw where another mirror would
/// stand (a root, or a member of a folder other than the subscribee
/// folder): there its children are raw too. A member already shown in
/// another's place is re-pointed whenever it is not the mirror shown,
/// as after a switch of perspective. Where the mirror shown is the
/// original itself, the member shown in its place loses
/// 'overridesHere', is warranted only as itself (and so, unwarranted,
/// vanishes), and the original is reinstated beside it. The diff view
/// substitutes nothing. At the buffer root, the view's roots are
/// substituted by their own rule ('substitute_roots').
fn substitute_mirrors (
  tree   : &mut Tree<Viewnode>,
  parent : NodeId,
  goal   : Option<&Goal>,
  walk   : &mut Walk,
) -> Result<(), Box<dyn Error>> {
  if matches! ( tree . get (parent) . unwrap () . value () . kind,
                ViewnodeKind::BufferRoot )
     && ! walk . is_diff_view ()
  { return substitute_roots (tree, parent, walk); }
  let Some (goal) : Option<&Goal> = goal else { return Ok (( )); };
  if walk . is_diff_view ()
     || tree . get (parent) . unwrap () . value () . editable_gnid () . is_none ()
  { return Ok (( )); }
  let owner : SkgrepoOwner = perspective_owner_of_tree_containing (
    walk . truth . config, walk . truth . graph, &walk . overrides_followed,
    tree . get (parent) . unwrap () );
  if is_raw_where_another_mirror_would_stand (tree, parent, &owner, walk)
  { return Ok (( )); }
  for child in children_of (tree, parent) {
    let (gnid, overridesHere, warrant) : (ID, Option<ID>, Warrant) =
      match &tree . get (child) . unwrap () . value () . kind {
        ViewnodeKind::Vognode (Vognode::Occurrence (occurrence)) =>
          ( occurrence . gnid . clone (), occurrence . overridesHere . clone (),
            occurrence . viewStats . warrant ),
        _ => continue, };
    let pid : ID = walk . truth . graph . pid_else_raw_id (&gnid);
    match overridesHere {
      None => if warrant == Warrant::ByGraph {
        let shown : ID = mirror_shown (
          walk . truth . config, walk . truth . graph, walk . truth . restriction,
          &owner, &gnid );
        if shown != pid {
          show_mirror (tree, child, &shown, Some (gnid), walk) ?; }},
      Some (original) => {
        let shown : ID = mirror_shown (
          walk . truth . config, walk . truth . graph, walk . truth . restriction,
          &owner, &original );
        if shown == walk . truth . graph . pid_else_raw_id (&original) {
          stop_substituting (tree, parent, child, &original, goal, walk) ?;
        } else if shown != pid {
          show_mirror (tree, child, &shown, Some (original), walk) ?; }}, }}
  Ok (( )) }

/// Each root of the view shows what the view's perspective makes of
/// the node it stands for (its '(overridesHere N)', else itself): the
/// root authors' show that node raw, as every fresh view does; one
/// owner's perspective shows the mirror it puts in that node's place
/// ('mirror_shown'), marked '(overridesHere N)' unless it is the node
/// itself.
/// The occurrence is re-pointed where it stands, so it keeps its
/// viewnode ID whichever way it switches, and its children follow
/// from its own visit. A re-pointed root's folders are drawn afresh,
/// for the node it now shows ('redraw_folders'). A fork of a root
/// re-roots the view at the clone, which stands for nothing
/// ('replace_saved_view_fork_roots').
fn substitute_roots (
  tree        : &mut Tree<Viewnode>,
  buffer_root : NodeId,
  walk        : &mut Walk,
) -> Result<(), Box<dyn Error>> {
  for root in children_of (tree, buffer_root) {
    let (gnid, overridesHere) : (ID, Option<ID>) =
      match &tree . get (root) . unwrap () . value () . kind {
        ViewnodeKind::Vognode (Vognode::Occurrence (occurrence)) =>
          ( occurrence . gnid . clone (), occurrence . overridesHere . clone () ),
        _ => continue, };
    let original : ID = overridesHere . clone ()
      . unwrap_or_else ( || walk . truth . graph . pid_else_raw_id (&gnid) );
    let shown : ID = match &walk . overrides_followed {
      OverridesFollowed::RootAuthors => original . clone (),
      OverridesFollowed::Owner (owner) => mirror_shown (
        walk . truth . config, walk . truth . graph, walk . truth . restriction,
        owner, &original ), };
    let marked : Option<ID> = ( shown != original ) . then ( || original . clone () );
    let repointed : bool = walk . truth . graph . pid_else_raw_id (&gnid) != shown;
    let was_editable : bool =
      tree . get (root) . unwrap () . value () . editable_gnid () . is_some ();
    if repointed || overridesHere != marked
    { show_mirror (tree, root, &shown, marked, walk) ?; }
    if repointed { redraw_folders (tree, root, was_editable, walk) ?; }}
  Ok (( )) }

/// The root at 'root' now shows another node, so every folder it
/// holds describes the node it showed before. Each vanishes, and in
/// its place stand the folders of its kind (and of its relation's
/// other side) for the node now shown ('kinds_redrawing'), empty, for
/// their own visits to fill, each hidden in the root if the old one
/// was (its members are drawn anew, unfolded): a write-protected one
/// goes if it stays
/// empty, and an editable one is drawn only where the node has members
/// for it, since an empty one says the user asked for it. Then the
/// root asks for what a fresh view of the node asks for
/// ('stub_of_fresh_content_view'): its containers folder, which the
/// requests rung draws (what contains it, but its view-ancestors, or
/// no folder where nothing does); and, if the root was editable, to be
/// extended as an editable root is, which at its own visit adds its
/// default folders ('add_default_folders'): the subscribee folder,
/// with its count, where it subscribes. A write-protected root, such as
/// a search hit, asks for no more than it had. So a root that was
/// write-protected only because another occurrence held the node it
/// was re-pointed to ('point_occurrence_at') stays so when switched
/// back: nothing remembers why it was write-protected. A root asks even if it
/// held no folder: the view was drawn for another node.
fn redraw_folders (
  tree         : &mut Tree<Viewnode>,
  root         : NodeId,
  was_editable : bool,
  walk         : &mut Walk,
) -> Result<(), Box<dyn Error>> {
  let Some (pid) : Option<ID> =
    tree . get (root) . unwrap () . value () . occurrence_gnid () . cloned ()
    else { return Ok (( )); };
  let folders : Vec<(NodeId, Folder)> =
    tree . get (root) . unwrap () . children ()
    . filter_map ( |child| match &child . value () . kind {
        ViewnodeKind::Folder (folder) => Some (( child . id (), folder . clone () )),
        _ => None, } )
    . collect ();
  let mut drawn : Vec<Folder> = Vec::new ();
  for (treeid, folder) in folders {
    let folded : bool = tree . get (treeid) . unwrap () . value () . foldAndFocus . folded;
    for kind in kinds_redrawing (&folder, was_editable) {
      if drawn . iter () . any ( |done| discriminant (done) == discriminant (&kind) )
         || ( kind . is_editable () && ! has_members (&pid, &kind, walk) )
      { continue; }
      let mut empty : Viewnode = viewnode_of ( ViewnodeKind::Folder (kind . clone ()) );
      empty . foldAndFocus . folded = folded;
      let new : NodeId = tree . get_mut (treeid) . unwrap () . insert_before (empty) . id ();
      if matches! ( kind, Folder::Subscribee ) {
        add_hidden_outside_folder (tree, new, &pid, walk); }
      drawn . push (kind); }
    vanish (tree, treeid, walk) ?; }
  if let ViewnodeKind::Vognode (Vognode::Occurrence (occurrence)) =
    &mut tree . get_mut (root) . unwrap () . value () . kind
  { occurrence . view_requests . insert (
      ViewRequest::Folder (FolderRelation::Containers) );
    if was_editable {
      occurrence . view_requests . insert (ViewRequest::Editable); }}
  Ok (( )) }

/// The folders that stand, for the node a re-pointed root now shows,
/// where a folder of the kind of 'folder' stood: those a request for
/// its relation draws, both sides of a partner relation ('(folder
/// hidesFromSubs)' draws the hider and hidden folders), but for the
/// subscribee folder of an editable root, alone, which such a root
/// shows by default where its node subscribes, and which the user's
/// request would have drawn with a subscriber folder. (A
/// write-protected root, such as a search hit, shows a subscribee
/// folder only by request or beside its subscriber folder
/// ('stub_root_of_hit').) None for the folders drawn inside others
/// or only in the diff view, for the illegal-edit folder, which each
/// visit draws, and for the ID folder, which no request draws.
fn kinds_redrawing (
  folder           : &Folder,
  root_is_editable : bool,
) -> Vec<Folder> {
  match folder {
    Folder::Subscribee if root_is_editable
                                   => vec! [ Folder::Subscribee ],
    Folder::Subscribee             => folders_of_relation (FolderRelation::SubscribesTo),
    Folder::Subscriber             => folders_of_relation (FolderRelation::SubscribesTo),
    Folder::Hider | Folder::Hidden => folders_of_relation (FolderRelation::HidesFromSubs),
    Folder::Container              => folders_of_relation (FolderRelation::Containers),
    Folder::Mentioner              => folders_of_relation (FolderRelation::Mentioners),
    Folder::Alias                  => folders_of_relation (FolderRelation::Aliases),
    Folder::Flags { .. }           => vec! [ Folder::flags () ],
    Folder::HiddenInSubscribee | Folder::HiddenOutsideOfSubscribee
      | Folder::AcquiredBy | Folder::Acquired
      | Folder::ID | Folder::IllegalEdit { .. } => Vec::new (), } }

/// Whether a folder of the kind of 'folder' under an occurrence of
/// 'pid' would hold anything: the node's aliases, or its partners in
/// the folder's relation, as the skgrepo restriction shows them.
fn has_members (
  pid    : &ID,
  folder : &Folder,
  walk   : &Walk,
) -> bool {
  match folder {
    Folder::Alias => walk . truth . graph . get (pid) . is_some_and (
      |node| ! node . aliases_with_relRepo_unless_home (walk . truth . restriction)
               . is_empty () ),
    _ => partnerFolder_members_in_graph (
           walk . truth . graph, pid, folder, walk . truth . restriction )
         . is_some_and ( |members| ! members . is_empty () ), } }

/// Make the occurrence at 'child' show 'shown', with 'overridesHere'
/// as given ('point_occurrence_at').
fn show_mirror (
  tree          : &mut Tree<Viewnode>,
  child         : NodeId,
  shown         : &ID,
  overridesHere : Option<ID>,
  walk          : &mut Walk,
) -> Result<(), Box<dyn Error>> {
  point_occurrence_at (tree, child, shown, walk) ?;
  if let ViewnodeKind::Vognode (Vognode::Occurrence (occurrence)) =
    &mut tree . get_mut (child) . unwrap () . value () . kind
  { occurrence . overridesHere = overridesHere; }
  Ok (( )) }

/// The original that 'child' stood for now stands as itself: 'child'
/// shows itself, is
/// warranted only as itself (and so, unwarranted, vanishes, as the
/// claims rung says), and the original is reinstated right after it,
/// unless a sibling already stands for it.
fn stop_substituting (
  tree     : &mut Tree<Viewnode>,
  parent   : NodeId,
  child    : NodeId,
  original : &ID,
  goal     : &Goal,
  walk     : &mut Walk,
) -> Result<(), Box<dyn Error>> {
  if let ViewnodeKind::Vognode (Vognode::Occurrence (occurrence)) =
    &mut tree . get_mut (child) . unwrap () . value () . kind
  { occurrence . overridesHere = None; }
  let warrant : Warrant = warrant_of_child (
    Some (goal), tree . get (child) . unwrap () . value () )
    . unwrap_or (Warrant::No);
  tree . get_mut (child) . unwrap () . value () . set_warrant (warrant);
  reinstate_original_after (tree, parent, child, original, goal, walk) ?;
  vanish_if_unwarranted (tree, child, Some (warrant), walk) }

/// Draw 'original' right after 'child', unless a sibling already
/// stands for it or the goal does not name it.
fn reinstate_original_after (
  tree     : &mut Tree<Viewnode>,
  parent   : NodeId,
  child    : NodeId,
  original : &ID,
  goal     : &Goal,
  walk     : &mut Walk,
) -> Result<(), Box<dyn Error>> {
  let member : GoalMember = GoalMember::Gnid ( original . clone () );
  let stands : bool = tree . get (parent) . unwrap () . children ()
    . any ( |sibling| member_of_child (sibling . value ()) . as_ref () == Some (&member) );
  if stands { return Ok (( )); }
  let Some (item) : Option<&GoalItem> = goal . item_for (&member) else { return Ok (( )); };
  let item : GoalItem = item . clone ();
  if let Some (drawn) = draw_item (tree, parent, &item, goal, walk) ? {
    let treeid : NodeId = tree . get_mut (parent) . unwrap () . append (drawn) . id ();
    move_after (tree, child, treeid) ?;
    after_drawing (tree, parent, treeid, walk) ?; }
  Ok (( )) }

/// Whether the occurrence at 'treeid' is shown raw where, from the
/// perspective of 'owner', another of its mirrors would stand in its
/// place: at a root, or as a member of a folder other than the
/// subscribee folder.
fn is_raw_where_another_mirror_would_stand (
  tree   : &Tree<Viewnode>,
  treeid : NodeId,
  owner  : &SkgrepoOwner,
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
  let ViewnodeKind::Vognode (Vognode::Occurrence (occurrence)) : &ViewnodeKind =
    &node . value () . kind else { return false; };
  raw_position
  && occurrence . overridesHere . is_none ()
  && mirror_shown ( walk . truth . config, walk . truth . graph,
                    &walk . unrestricted, owner, &occurrence . gnid )
     != walk . truth . graph . pid_else_raw_id (&occurrence . gnid) }

//
// 8. requests
//

/// Fulfill and drop each child's view requests: a write-protect
/// request write-protects the occurrence (and, for the subtree form,
/// its content subtree); a folder or flags request adds the folders,
/// which their own visits fill from their goals. An editable-view
/// request stays for the child's own visit to consume
/// ('consume_editableView_request'), and a fork request is the save's:
/// it stays too.
fn fulfill_view_requests (
  tree   : &mut Tree<Viewnode>,
  parent : NodeId,
  walk   : &mut Walk,
) -> Result<(), Box<dyn Error>> {
  for child in children_of (tree, parent) {
    let requests : Vec<ViewRequest> =
      match &tree . get (child) . unwrap () . value () . kind {
        ViewnodeKind::Vognode (Vognode::Occurrence (occurrence)) =>
          occurrence . view_requests . iter () . copied ()
          . filter ( |request| ! request_stays (request) )
          . collect (),
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
        ViewRequest::Editable | ViewRequest::Fork => {}, }}
    add_folders (tree, child, &folders, walk);
    if let ViewnodeKind::Vognode (Vognode::Occurrence (occurrence)) =
      &mut tree . get_mut (child) . unwrap () . value () . kind
    { occurrence . view_requests . retain (request_stays); }}
  Ok (( )) }

/// Whether 'fulfill_view_requests' leaves the request where it is.
fn request_stays (
  request : &ViewRequest,
) -> bool {
  matches! ( request, ViewRequest::Editable | ViewRequest::Fork ) }

/// Where the folders a request adds stand among those added at one
/// visit, first first: the containers folder, the mentioners, the
/// aliases, then the subscription and hiding folders, the
/// flags last. (A node's view requests are a set.)
fn folder_request_rank (
  request : &ViewRequest,
) -> usize {
  match request {
    ViewRequest::Folder (FolderRelation::Containers)    => 0,
    ViewRequest::Folder (FolderRelation::Mentioners)    => 1,
    ViewRequest::Folder (FolderRelation::Aliases)       => 2,
    ViewRequest::Folder (FolderRelation::SubscribesTo)  => 3,
    ViewRequest::Folder (FolderRelation::HidesFromSubs) => 4,
    ViewRequest::Flags                                  => 5,
    ViewRequest::Editable | ViewRequest::WriteProtected
      | ViewRequest::WriteProtectedSubtree
      | ViewRequest::Fork                               => 6, } }

/// The folders a '(folder RELNAME)' request adds: both of the
/// relation's, or the one whose other side the node shows without a
/// folder (its content, its links).
fn folders_of_relation (
  relation : FolderRelation,
) -> Vec<Folder> {
  match relation {
    FolderRelation::Aliases       => vec! [ Folder::Alias ],
    FolderRelation::HidesFromSubs => vec! [ Folder::Hider, Folder::Hidden ],
    FolderRelation::SubscribesTo  => vec! [ Folder::Subscribee, Folder::Subscriber ],
    FolderRelation::Containers    => vec! [ Folder::Container ],
    FolderRelation::Mentioners    => vec! [ Folder::Mentioner ], } }

/// Add each of 'folders' the occurrence at 'treeid' lacks, ahead of
/// its other children, in the order given. A subscribee folder holds
/// the compound folder of what the subscriber hides outside its
/// subscribees, when the subscriber hides something there; new, its
/// members are drawn folded ('Walk::added_compound_folders').
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
        new }, };
    if matches! ( folder, Folder::Subscribee ) {
      let subscriber : Option<ID> =
        tree . get (treeid) . unwrap () . value () . occurrence_gnid () . cloned ();
      if let Some (subscriber) = subscriber {
        add_hidden_outside_folder (tree, folder_treeid, &subscriber, walk); }}}
  for folder_treeid in added . into_iter () . rev () {
    move_to_first_child (tree, treeid, folder_treeid)
      . expect ("add_folders: the folder was just added"); }}

/// In the subscribee folder at 'subscribee_folder', of 'subscriber',
/// the compound folder of what the subscriber hides outside its
/// subscribees, when it hides something there and the folder lacks
/// one; new, its members are drawn folded
/// ('Walk::added_compound_folders').
fn add_hidden_outside_folder (
  tree              : &mut Tree<Viewnode>,
  subscribee_folder : NodeId,
  subscriber        : &ID,
  walk              : &mut Walk,
) {
  if child_folder (tree, subscribee_folder, &Folder::HiddenOutsideOfSubscribee) . is_none ()
     && ! hidden_outside_of_subscribees (subscriber, walk) . is_empty ()
  { let hidden_outside : NodeId = tree . get_mut (subscribee_folder) . unwrap ()
      . append ( viewnode_of (
        ViewnodeKind::Folder (Folder::HiddenOutsideOfSubscribee) )) . id ();
    walk . added_compound_folders . insert (hidden_outside); }}

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
// after the rungs
//

/// A folder left childless is entombstoned, so that the tail prunes
/// it, unless it is one that stays when empty
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
/// any but one whose membership the save reads, where the user could
/// add something, a flags folder, which shows that the node has no
/// true flag, or an illegal-edit folder, whose text says something.
pub(crate) fn folder_goes_when_empty (
  viewnode : &Viewnode,
) -> bool {
  matches! ( &viewnode . kind,
             ViewnodeKind::Folder (folder)
               if ! folder . is_editable () && ! folder . is_flags ()
                  && ! matches! ( folder, Folder::IllegalEdit { .. } )) }

/// The children of 'parent', in order.
pub(crate) fn children_of (
  tree   : &Tree<Viewnode>,
  parent : NodeId,
) -> Vec<NodeId> {
  tree . get (parent) . unwrap () . children () . map ( |child| child . id () ) . collect () }
