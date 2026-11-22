//! The visit of a parent: the refresh's rungs, applied at each
//! parent P to its children, in order ('visit_parent'). Each rung,
//! and what the visit does before and after them, is stated in
//! 'docs/data-model_technical.org', "The walk and the refresh's
//! rungs". The claims, the fourth rung, are 'claims.rs'; mirror
//! substitution, the seventh, is 'substitution.rs'.

use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::in_rust_graph::relation_accessors::NodeRelation;
use crate::dbs::in_rust_graph::mirrors::mirrors_of;
use crate::refresh::editability::{
  DrawnEditability, EditabilityAndClaim, carries_editableView_request,
  consume_editableView_request,
  drawn_occurrence_editability, drop_editableView_request, extend_requested_content,
  merge_editability, write_protect_occurrence};
use crate::refresh::extent::{Extent, extent_of_parent};
use crate::refresh::goal::{
  Goal, GoalItem, GoalMember, goal_of_parent, has_a_fork_partner,
  hidden_in_fork_partner, hidden_outside_of_fork_partners,
  integrated_elsewhere, member_of_child, note_forked_by_forker_as_such,
  note_of_member_as_such, visible_illegal_edits};
use crate::refresh::goal::partnerFolder_members_in_graph;
use crate::telescope::types::IllegalEdit;
use crate::refresh::id_rule::{
  IdRuleAnswer, WhereTheHeadlineStands, entombstone, id_rule,
  rekind_vognode, vognode_for_answer};
use crate::refresh::Walk;
use crate::refresh::ancestry::{
  described_node, is_unsayable, parent_reads_claims, view_ancestor_gnids};
use crate::refresh::mode::RefreshMode;
use crate::types::nodes::rust::GraphnodeInRust;
use crate::refresh::diff_view;
use crate::refresh::chain::draw_containerward_chain;
use crate::refresh::claims::{
  claim_forks_folder_members_as_the_graph_does, mark_claims, vanish};
use crate::refresh::substitution::substitute_mirrors;
use crate::skgrepo_sets::SkgrepoRestriction;
use crate::types::misc::ID;
use crate::types::tree::moves::{move_after, move_to_first_child, move_to_last_child};
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
  add_containers_folder_to_a_standing_member (tree, parent, walk) ?;
  add_default_folders (tree, parent, walk);
  add_illegal_edit_folder (tree, parent, walk);
  add_hidden_outside_folder_to_forks_folder (tree, parent, walk);
  let goal : Option<Goal> = goal_of_parent (tree, parent, walk);
  rekind_children (tree, parent, goal . as_ref (), walk) ?;
  viewnode_merge (tree, parent, walk) ?;
  claim_forks_folder_members_as_the_graph_does (tree, parent, goal . as_ref (), walk);
  set_warrants (tree, parent, goal . as_ref (), walk);
  mark_claims (tree, parent, goal . as_ref (), walk) ?;
  if let Some (goal) = &goal {
    if extent_of_parent (tree, parent, walk) == Extent::All {
      reinstate (tree, parent, goal, walk) ?; }}
  diff_view::draw_change_viewnodes (tree, parent, walk) ?;
  diff_view::tag_children (tree, parent, goal . as_ref (), walk) ?;
  if let Some (goal) = &goal {
    if goal . ordered {
      reorder (tree, parent, goal, walk . truth . graph) ?; }}
  keep_hidden_outside_folder_last (tree, parent) ?;
  substitute_mirrors (tree, parent, goal . as_ref (), walk) ?;
  fulfill_view_requests (tree, parent, walk) ?;
  entombstone_if_an_empty_folder (tree, parent);
  drop_editableView_request (tree, parent);
  Ok (( )) }

/// The folders a content view shows without being asked, as
/// 'docs/data-model_technical.org' says ("The walk and the refresh's
/// rungs", its paragraph on 'add_default_folders'). Not in the diff
/// view.
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
     && note_of_member_as_such (tree, parent) . is_none ()
     && has_a_fork_partner (walk . truth . graph, walk . truth . restriction, &pid)
  { let had_one : bool = child_folder (tree, parent, &Folder::Forks) . is_some ();
    add_folders (tree, parent, &[ Folder::Forks ], walk);
    // A new one goes after any folder the occurrence was asked for.
    if let Some (forks_folder) = child_folder (tree, parent, &Folder::Forks)
                                 . filter ( |_| ! had_one ) {
      let last_leading_folder : Option<NodeId> =
        tree . get (parent) . unwrap () . children ()
        . take_while ( |child| matches! ( child . value () . kind, ViewnodeKind::Folder (_) ))
        . filter ( |child| child . id () != forks_folder )
        . map ( |child| child . id () )
        . last ();
      if let Some (folder) = last_leading_folder {
        move_after (tree, folder, forks_folder)
          . expect ("add_default_folders: both are children"); }}}
  if let Some (note) = note_of_member_as_such (tree, parent) {
    // The folder of what the user keeps elsewhere goes first; the
    // folder of what the note hides in the member right after it;
    // the folder of what a forker as such hides of the note's content
    // right after them. Each new one follows the last of those
    // before it that stands.
    let mut last_placed : Option<NodeId> = None;
    for (kind, holds_something) in [
      ( Folder::IntegratedElsewhere,
        ! integrated_elsewhere (&pid, &note, walk) . is_empty () ),
      ( Folder::HiddenInForkPartner,
        ! hidden_in_fork_partner (&pid, &note, walk) . is_empty () ),
      ( Folder::HiddenByForker,
        note_forked_by_forker_as_such (tree, parent, walk) . is_some ()
        && ! hidden_in_fork_partner (&note, &pid, walk) . is_empty () ), ]
    { last_placed = match child_folder (tree, parent, &kind) {
        Some (present) => Some (present),
        None if holds_something => {
          let folder : NodeId = tree . get_mut (parent) . unwrap ()
            . append ( viewnode_of ( ViewnodeKind::Folder (kind) )) . id ();
          match last_placed {
            Some (before) => move_after (tree, before, folder),
            None => move_to_first_child (tree, parent, folder), }
            . expect ("add_default_folders: the folder was just added");
          walk . folders_whose_drawn_members_are_folded . insert (folder);
          Some (folder) },
        None => last_placed, }; }}}

/// In a forks folder of a content view, the folder of what its
/// viewparent hides outside its fork partners ('add_hidden_outside_folder'):
/// so a forker that a save hid, which leaves the forks folder, stands
/// there, where removing it unhides it.
fn add_hidden_outside_folder_to_forks_folder (
  tree   : &mut Tree<Viewnode>,
  parent : NodeId,
  walk   : &mut Walk,
) {
  if walk . is_diff_view ()
     || ! matches! ( tree . get (parent) . unwrap () . value () . kind,
                     ViewnodeKind::Folder (Folder::Forks) )
  { return; }
  let Some (note) : Option<ID> =
    tree . get (parent) . unwrap () . parent ()
    . and_then ( |note| note . value () . occurrence_gnid () . cloned () )
    else { return; };
  add_hidden_outside_folder (tree, parent, &note, walk); }

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
  let Some (pid) : Option<ID> =
    tree . get (parent) . unwrap () . value () . occurrence_gnid () . cloned ()
    else { return; };
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
  || tree . get (parent) . unwrap () . value () . occurrence_gnid () . cloned ()
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
  for (_gnid, duplicates) in duplicate_viewnode_siblings (tree, parent, walk . truth . graph) {
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
  graph  : &InRustGraph,
) -> Vec<(ID, Vec<NodeId>)> {
  let mut groups : Vec<(ID, Vec<NodeId>)> = Vec::new ();
  let mut group_of_gnid : HashMap<ID, usize> = HashMap::new ();
  for child in children_of (tree, parent) {
    let Some (GoalMember::Gnid (gnid)) : Option<GoalMember> =
      vognode_member (tree . get (child) . unwrap () . value (), graph)
      else { continue; };
    match group_of_gnid . get (&gnid) {
      Some (group) => groups [*group] . 1 . push (child),
      None => { group_of_gnid . insert (gnid . clone (), groups . len ());
                groups . push ( (gnid, vec! [child]) ); }, }}
  groups . into_iter ()
    . filter ( |(_, group)| group . len () > 1 )
    . collect () }

fn vognode_member (
  viewnode : &Viewnode,
  graph    : &InRustGraph,
) -> Option<GoalMember> {
  match &viewnode . kind {
    ViewnodeKind::Vognode (_) => member_of_child (viewnode, graph),
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
/// 'ByGraph'. A child claiming an item no child may claim (a forks
/// folder's forker-only or indirect member: 'GoalItem::claimable') is warranted
/// 'No': its viewparent records no such relationship. None for a child
/// that carries no warrant.
pub(crate) fn warrant_of_child (
  goal     : Option<&Goal>,
  viewnode : &Viewnode,
  graph    : &InRustGraph,
) -> Option<Warrant> {
  let member : GoalMember = member_of_child (viewnode, graph) ?;
  let claims : bool = viewnode . claims () == Some (true);
  Some ( match goal {
    None => Warrant::ByGraph,
    Some (goal) => goal . item_for (&member)
      . map ( |item| if claims && ! item . claimable { Warrant::No }
                     else { item . warrant } )
      . unwrap_or (Warrant::No), } ) }

/// Set each child's warrant ('warrant_of_child'); a root's is
/// 'ByGraph', carrying no warrant fact. Under a tombstone, which has
/// no goal, an occurrence of an orphan graphnode (one no list the
/// skgrepo restriction shows contains) is unwarranted, as it is under
/// a write-protected occurrence ('orphan_under_a_tombstone'). Under a
/// parent that reads no claims ('parent_reads_claims'), each child
/// first claims nothing, as 'mark_claims' leaves it: the printer
/// writes no 'noClaim' there, so a child read back from the text
/// claims, and a forker-only or indirect member of a forks folder
/// under a write-protected occurrence would be unwarranted.
fn set_warrants (
  tree   : &mut Tree<Viewnode>,
  parent : NodeId,
  goal   : Option<&Goal>,
  walk   : &Walk,
) {
  let is_root : bool = matches! ( tree . get (parent) . unwrap () . value () . kind,
                                  ViewnodeKind::BufferRoot );
  let claims_are_read : bool = is_root || parent_reads_claims (tree, parent);
  for child in children_of (tree, parent) {
    if ! claims_are_read
       && tree . get (child) . unwrap () . value () . claims () == Some (true) {
      tree . get_mut (child) . unwrap () . value () . set_claim (false); }
    let warrant : Warrant =
      if is_root { Warrant::ByGraph } // a root carries no warrant fact
      else if orphan_under_a_tombstone (tree, parent, child, walk) { Warrant::No }
      else { match warrant_of_child (
               goal, tree . get (child) . unwrap () . value (), walk . truth . graph ) {
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
// 4. claims: 'claims.rs'
//

//
// 5. reinstate
//

/// Draw every goal item that no child stands for. Where the goal
/// orders it, it is appended, and the reorder places it; where the
/// goal keeps the view's order for it ('Goal::keeps_the_views_order'),
/// it follows the last child the goal names (or, when none does,
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
    . filter_map ( |child| member_of_child (child . value (), walk . truth . graph) )
    . collect ();
  let mut last_named : Option<NodeId> = // the child a drawn one follows, where the goal has no order
    tree . get (parent) . unwrap () . children ()
    . filter ( |child| member_of_child (child . value (), walk . truth . graph)
                       . is_some_and ( |member| goal . item_for (&member) . is_some () ))
    . map ( |child| child . id () )
    . last ();
  for item in &goal . items {
    if present . contains (&item . member) { continue; }
    let Some (drawn) : Option<Viewnode> = draw_item (tree, parent, item, goal, walk) ?
      else { continue; };
    if let Some (member) = member_of_child (&drawn, walk . truth . graph) {
      // In the diff view two items can name one node: a list entry
      // the worktree cannot resolve, which HEAD resolves to a node
      // HEAD's list also names. Its ghost stands for both.
      if ! present . insert (member) { continue; }}
    let treeid : NodeId =
      tree . get_mut (parent) . unwrap () . append (drawn) . id ();
    if goal . keeps_the_views_order (&item . member) {
      match last_named {
        Some (named) => move_after (tree, named, treeid) ?,
        None => move_to_first_child (tree, parent, treeid) ?, }
      last_named = Some (treeid); }
    after_drawing (tree, parent, treeid, walk) ?; }
  Ok (( )) }

/// Book what a drawn child is: an editable occurrence holds its
/// node's editability ('Walk::hold_editability'); a folder member
/// carries its chain, but for a member of a forks folder in a content
/// view, or of a folder of what a member as such contains that the
/// user keeps elsewhere, which carries it in a containers folder
/// ('add_containers_folder_under_member'); under a folder
/// the refresh added to be drawn folded, it is drawn folded, and so it
/// is under one of those kinds that already stood drawn folded
/// ('stands_drawn_folded').
pub(super) fn after_drawing (
  tree   : &mut Tree<Viewnode>,
  parent : NodeId,
  treeid : NodeId,
  walk   : &mut Walk,
) -> Result<(), Box<dyn Error>> {
  if let Some (gnid) = tree . get (treeid) . unwrap () . value () . editable_gnid () . cloned () {
    walk . hold_editability (&gnid, treeid); }
  let (carries_chain, carries_containers_folder) : (bool, bool) =
    if ! tree . get (treeid) . unwrap () . value () . is_occurrence () { (false, false) }
    else { match &tree . get (parent) . unwrap () . value () . kind {
      ViewnodeKind::Folder (folder) =>
        ( folder . members_carry_containerward_chain (walk . is_diff_view ()),
          ( matches! ( folder, Folder::Forks ) && ! walk . is_diff_view () )
          || matches! ( folder, Folder::IntegratedElsewhere ) ),
      _ => (false, false), }};
  if carries_chain {
    draw_containerward_chain (tree, treeid, walk) ?; }
  if carries_containers_folder {
    add_containers_folder_under_member (tree, treeid, walk); }
  if walk . folders_whose_drawn_members_are_folded . contains (&parent)
     || stands_drawn_folded (tree, parent, treeid) {
    // A folder's collapse lives on its members.
    tree . get_mut (treeid) . unwrap () . value () . foldAndFocus . folded = true; }
  Ok (( )) }

/// Whether the folder at 'parent', into which 'drawn' was just drawn,
/// is of a kind the refresh draws folded (a compound folder, or a
/// containers folder) and stands drawn folded: it holds another
/// member, and every member but 'drawn' is folded. A member drawn
/// into it then joins its siblings, folded, so that the folder stays
/// collapsed, as the refresh would draw it afresh.
fn stands_drawn_folded (
  tree   : &Tree<Viewnode>,
  parent : NodeId,
  drawn  : NodeId,
) -> bool {
  let folder : NodeRef<Viewnode> = tree . get (parent) . unwrap ();
  if ! matches! ( folder . value () . kind,
                  ViewnodeKind::Folder ( Folder::IntegratedElsewhere
                                         | Folder::HiddenInForkPartner
                                         | Folder::HiddenByForker
                                         | Folder::HiddenOutsideOfForkPartners
                                         | Folder::Container )) { return false; }
  let others : Vec<NodeRef<Viewnode>> =
    folder . children () . filter ( |child| child . id () != drawn ) . collect ();
  ! others . is_empty ()
  && others . iter () . all ( |child| child . value () . foldAndFocus . folded ) }

/// Under the occurrence at 'member', just drawn into a forks folder of
/// a content view (or into a folder of what a member as such contains
/// that the user keeps elsewhere, where it shows where the user keeps
/// it), a containers folder, drawn folded
/// ('Walk::folders_whose_drawn_members_are_folded'), whose members
/// carry their chains: the member's containers but its view-ancestors
/// ('Folder::omits_view_ancestors'). Where it would hold nothing its
/// own visit entombstones it. None under a repeat
/// ('Walk::is_a_repeat'), which shows nothing. The member was just
/// drawn, so the folder is its only child: the folders an editable
/// member's visit adds stand around it (the integrated-elsewhere
/// folder, the hidden-in folder and the hidden-by folder before it, a
/// forks folder asked for after it), and its content follows.
pub(crate) fn add_containers_folder_under_member (
  tree   : &mut Tree<Viewnode>,
  member : NodeId,
  walk   : &mut Walk,
) {
  let Some (gnid) : Option<ID> =
    tree . get (member) . unwrap () . value () . occurrence_gnid () . cloned ()
    else { return; };
  if walk . is_a_repeat (&gnid, member)
     || child_folder (tree, member, &Folder::Container) . is_some ()
  { return; }
  let folder : NodeId = tree . get_mut (member) . unwrap ()
    . prepend ( viewnode_of ( ViewnodeKind::Folder (Folder::Container) )) . id ();
  walk . folders_whose_drawn_members_are_folded . insert (folder); }

/// A member that stood in the buffer before gets, at its visit, the
/// containers folder a freshly drawn member gets
/// ('add_containers_folder_under_member'), so that the view is the
/// one a fresh view draws:
/// - a member of a content view's forks folder that an
///   editable-view request extends (one standing write-protected and
///   bare, extended by C-c s e on its note);
/// - a member of a folder of what a member as such contains that the
///   user keeps elsewhere, whenever its containers folder would name
///   a keeper ('names_a_container_outside_its_view_ancestors'): one
///   whose only keeper was a view-ancestor stood there with none, and
///   a save can give it another. Such a member that has become a
///   repeat ('Walk::is_a_repeat': the note, say, contains it now)
///   loses the containers folder it had, as a fresh view draws none
///   there.
/// Before the default folders, which place themselves around it.
fn add_containers_folder_to_a_standing_member (
  tree   : &mut Tree<Viewnode>,
  member : NodeId,
  walk   : &mut Walk,
) -> Result<(), Box<dyn Error>> {
  if walk . is_diff_view () { return Ok (( )); }
  let folder : Option<Folder> =
    tree . get (member) . unwrap () . parent ()
    . and_then ( |parent| match &parent . value () . kind {
        ViewnodeKind::Folder (folder) => Some (folder . clone ()),
        _ => None, } );
  match folder {
    Some (Folder::Forks) => {
      if carries_editableView_request (tree, member) {
        add_containers_folder_under_member (tree, member, walk); }},
    Some (Folder::IntegratedElsewhere) => {
      let is_a_repeat : bool =
        tree . get (member) . unwrap () . value () . occurrence_gnid ()
        . is_some_and ( |gnid| walk . is_a_repeat (gnid, member) );
      match child_folder (tree, member, &Folder::Container) {
        Some (containers_folder) if is_a_repeat =>
          vanish (tree, containers_folder, walk) ?,
        None if names_a_container_outside_its_view_ancestors (tree, member, walk) =>
          add_containers_folder_under_member (tree, member, walk),
        _ => {}, }},
    _ => {}, }
  Ok (( )) }

/// Whether a containers folder under the occurrence at 'member'
/// would hold anything: a container of its node, by a record the
/// skgrepo restriction shows, that is no view-ancestor of it
/// ('Folder::omits_view_ancestors').
fn names_a_container_outside_its_view_ancestors (
  tree   : &Tree<Viewnode>,
  member : NodeId,
  walk   : &Walk,
) -> bool {
  let Some (gnid) : Option<&ID> =
    tree . get (member) . unwrap () . value () . occurrence_gnid ()
    else { return false; };
  let view_ancestors : HashSet<ID> = view_ancestor_gnids (tree, member, walk . truth . graph);
  partnerFolder_members_in_graph (
    walk . truth . graph, gnid, &Folder::Container, walk . truth . restriction )
    . is_some_and ( |containers| containers . iter ()
                    . any ( |container| ! view_ancestors . contains (container) )) }

/// The viewnode that stands for 'item' under 'parent': for a gnid,
/// what the ID rule says (None if a tombstone, which there is no
/// point in drawing); for a property value, its member. It claims
/// membership only where the save reads claims, and not when only
/// HEAD warrants it, nor for an item no child may claim (a forker-only
/// or an indirect member of a forks folder: were it to claim, the next
/// save would make the folder's viewparent fork it).
pub(super) fn draw_item (
  tree   : &Tree<Viewnode>,
  parent : NodeId,
  item   : &GoalItem,
  goal   : &Goal,
  walk   : &Walk,
) -> Result<Option<Viewnode>, Box<dyn Error>> {
  let claims : bool =
    item . warrant == Warrant::ByGraph && item . claimable
    && parent_reads_claims (tree, parent);
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
  graph  : &InRustGraph,
) -> Result<(), Box<dyn Error>> {
  let children : Vec<NodeId> = children_of (tree, parent);
  let mut front : Vec<NodeId> = Vec::new ();
  let mut groups : Vec<(usize, Vec<NodeId>)> = Vec::new ();
  for child in &children {
    let position : Option<usize> =
      member_of_child (tree . get (*child) . unwrap () . value (), graph)
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

fn keep_hidden_outside_folder_last (
  tree   : &mut Tree<Viewnode>,
  parent : NodeId,
) -> Result<(), Box<dyn Error>> {
  if ! matches! ( tree . get (parent) . unwrap () . value () . kind,
                  ViewnodeKind::Folder (Folder::Forks) ) {
    return Ok (( )); }
  let hidden_outside : Option<NodeId> =
    tree . get (parent) . unwrap () . children ()
    . find ( |child| matches! ( child . value () . kind,
                                ViewnodeKind::Folder (Folder::HiddenOutsideOfForkPartners) ))
    . map ( |child| child . id () );
  if let Some (hidden_outside) = hidden_outside {
    move_to_last_child (tree, parent, hidden_outside) ?; }
  Ok (( )) }

//
// 7. substitution: 'substitution.rs'
//

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

fn request_stays (
  request : &ViewRequest,
) -> bool {
  matches! ( request, ViewRequest::Editable | ViewRequest::Fork ) }

/// Where the folders a request adds stand among those added at one
/// visit, first first: the containers folder, the mentioners, the
/// aliases, then the forks and hiding folders, the
/// flags last. (A node's view requests are a set.)
fn folder_request_rank (
  request : &ViewRequest,
) -> usize {
  match request {
    ViewRequest::Folder (FolderRelation::Containers)    => 0,
    ViewRequest::Folder (FolderRelation::Mentioners)    => 1,
    ViewRequest::Folder (FolderRelation::Aliases)       => 2,
    ViewRequest::Folder (FolderRelation::Forks)         => 3,
    ViewRequest::Folder (FolderRelation::Hides) => 4,
    ViewRequest::Flags                                  => 5,
    ViewRequest::Editable | ViewRequest::WriteProtected
      | ViewRequest::WriteProtectedSubtree
      | ViewRequest::Fork                               => 6, } }

/// The folders a '(folder RELNAME)' request adds: both of the
/// relation's, or the one whose other side the node shows without a
/// folder (its content, its links).
pub(super) fn folders_of_relation (
  relation : FolderRelation,
) -> Vec<Folder> {
  match relation {
    FolderRelation::Aliases       => vec! [ Folder::Alias ],
    FolderRelation::Hides => vec! [ Folder::Hider, Folder::Hidden ],
    FolderRelation::Forks         => vec! [ Folder::Forks ],
    FolderRelation::Containers    => vec! [ Folder::Container ],
    FolderRelation::Mentioners    => vec! [ Folder::Mentioner ], } }

/// Add each of 'folders' the occurrence at 'treeid' lacks, ahead of
/// its other children, in the order given, and to a forks folder its
/// hidden-outside folder ('add_hidden_outside_folder'). A forks folder
/// lists the mirrors of the occurrence's node
/// ('Walk::pids_whose_mirrors_are_listed').
pub(crate) fn add_folders (
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
    if matches! ( folder, Folder::Forks ) {
      let forker : Option<ID> =
        tree . get (treeid) . unwrap () . value () . occurrence_gnid () . cloned ();
      if let Some (forker) = forker {
        walk . pids_whose_mirrors_are_listed . extend ( mirrors_of (
          walk . truth . graph, walk . truth . restriction, &forker, None ) . pids () );
        add_hidden_outside_folder (tree, folder_treeid, &forker, walk); }}}
  for folder_treeid in added . into_iter () . rev () {
    move_to_first_child (tree, treeid, folder_treeid)
      . expect ("add_folders: the folder was just added"); }}

/// In the forks folder at 'forks_folder', of 'hider', the
/// compound folder of what it hides outside its fork partners, when it
/// hides something there and the folder lacks one; new, its members
/// are drawn folded ('Walk::folders_whose_drawn_members_are_folded').
pub(super) fn add_hidden_outside_folder (
  tree              : &mut Tree<Viewnode>,
  forks_folder      : NodeId,
  hider             : &ID,
  walk              : &mut Walk,
) {
  if child_folder (tree, forks_folder, &Folder::HiddenOutsideOfForkPartners) . is_none ()
     && ! hidden_outside_of_fork_partners (hider, walk) . is_empty ()
  { let hidden_outside : NodeId = tree . get_mut (forks_folder) . unwrap ()
      . append ( viewnode_of (
        ViewnodeKind::Folder (Folder::HiddenOutsideOfForkPartners) )) . id ();
    walk . folders_whose_drawn_members_are_folded . insert (hidden_outside); }}

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
/// The folder of what a note hides outside its fork partners is read
/// by the save, but goes too: it is drawn only when it holds
/// something ('add_hidden_outside_folder'), so a view whose save
/// emptied it is drawn as a fresh one is.
pub(crate) fn folder_goes_when_empty (
  viewnode : &Viewnode,
) -> bool {
  matches! ( &viewnode . kind,
             ViewnodeKind::Folder (folder)
               if ( ! folder . is_editable ()
                    || matches! ( folder, Folder::HiddenOutsideOfForkPartners ))
                  && ! folder . is_flags ()
                  && ! matches! ( folder, Folder::IllegalEdit { .. } )) }

pub(crate) fn children_of (
  tree   : &Tree<Viewnode>,
  parent : NodeId,
) -> Vec<NodeId> {
  tree . get (parent) . unwrap () . children () . map ( |child| child . id () ) . collect () }
