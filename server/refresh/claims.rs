//! Claims, the fourth of the refresh's rungs ('visit.rs'): which
//! children claim membership of their viewparent's list, and which
//! vanish because the graph no longer relates them to it
//! ('mark_claims'); in a content view's forks folder, the claim the
//! graph gives each member ('claim_forks_folder_members_as_the_graph_does').

use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::in_rust_graph::relation_accessors::NodeRelation;
use crate::refresh::goal::{
  Goal, GoalMember, member_of_child, note_forked_by_forker_as_such};
use crate::refresh::Walk;
use crate::refresh::ancestry::{
  described_node, parent_reads_claims, view_ancestor_gnids};
use crate::refresh::tail::remove_viewnode_transferring_focus;
use crate::refresh::visit::{children_of, warrant_of_child};
use crate::types::misc::ID;
use crate::types::viewnode::{Folder, Viewnode, ViewnodeKind, Warrant};

use ego_tree::{NodeId, NodeRef, Tree};
use std::error::Error;

/// In a content view's forks folder whose claims the save reads, each
/// member the goal names takes the claim the graph gives it: a note
/// the folder's viewparent forks claims, and a forker-only or an
/// indirect member claims nothing ('GoalItem::claimable'). A refresh draws a view from
/// the graph, and a buffer it draws holds no gesture the user has yet
/// to save: it is one the server drew, or one a save just consumed. So
/// a member drawn claiming nothing before another view, another
/// client or this very save made the note fork it claims once
/// redrawn, rather than standing as a detachment nobody wrote, which
/// the clients would show and the next save would carry out
/// ('docs/forks.org', "Claims in a redrawn view"). A member the goal
/// does not name keeps what it carries. Not in the diff view.
pub(super) fn claim_forks_folder_members_as_the_graph_does (
  tree   : &mut Tree<Viewnode>,
  parent : NodeId,
  goal   : Option<&Goal>,
  walk   : &Walk,
) {
  let Some (goal) : Option<&Goal> = goal else { return; };
  if walk . is_diff_view ()
     || ! matches! ( tree . get (parent) . unwrap () . value () . kind,
                     ViewnodeKind::Folder (Folder::Forks) )
     || ! parent_reads_claims (tree, parent)
  { return; }
  for child in children_of (tree, parent) {
    let Some (claimable) : Option<bool> =
      member_of_child ( tree . get (child) . unwrap () . value (), walk . truth . graph )
      . and_then ( |member| goal . item_for (&member) )
      . filter ( |item| item . warrant == Warrant::ByGraph )
      . map ( |item| item . claimable )
      else { continue; };
    tree . get_mut (child) . unwrap () . value () . set_claim (claimable); }}

/// Under the buffer root every occurrence root claims; any other root
/// keeps what it carries. Under an editable occurrence or an editable
/// folder, each child that claims and that the truth does not warrant
/// (a ghost warrant included: the worktree lacks that relationship)
/// vanishes ('vanish_if_unwarranted'); a child that claims nothing
/// stays (in content, one the user detached; in a forks folder, the
/// goal's members already claim as the graph says,
/// 'claim_forks_folder_members_as_the_graph_does'). Under any other parent
/// no child claims; but an unwarranted child of a write-protected
/// folder vanishes where the folder no longer stands for it at all
/// ('leaves_its_write_protected_folder'): under the hides folder, a
/// graphnode a save unhid; under a folder that omits view-ancestors,
/// one the view shows above the node the folder describes. A hide the
/// skgrepo restriction keeps out stays, unwarranted, for a skgrepo
/// widening to show again. In a forks folder, whether or not it reads
/// claims, an unwarranted member claiming nothing that its viewparent
/// hides vanishes too ('is_hidden_by_the_forks_folders_viewparent').
pub(super) fn mark_claims (
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
      let warrant : Option<Warrant> = warrant_of_child (goal, viewnode, walk . truth . graph);
      if ! claims && warrant == Some (Warrant::No)
         && is_hidden_by_the_forks_folders_viewparent (tree, parent, child, walk) {
        vanish (tree, child, walk) ?; }
      else { vanish_if_unwarranted (tree, child, warrant, walk) ?; }}
    else if warrant_of_child (goal, viewnode, walk . truth . graph) == Some (Warrant::No)
            && leaves_its_write_protected_folder (tree, parent, child, walk) {
      vanish (tree, child, walk) ?; }
    else if claims {
      tree . get_mut (child) . unwrap () . value () . set_claim (false); }}
  Ok (( )) }

/// Whether 'child', unwarranted under a write-protected folder at
/// 'parent', vanishes from it rather than staying marked: the folder
/// no longer stands for it by any record, visible or not
/// ('unhidden_in_the_hides_folder'), or by design
/// ('view_ancestor_in_its_folder',
/// 'is_hiddenByForkerFolder_under_no_forker_as_such'), or the folder
/// is the folder of what a member as such contains that the user keeps
/// elsewhere, whose members are derived each time from where the user
/// keeps them: one the user keeps there no longer, or that a skgrepo
/// narrowing keeps out of the user's graph, the member shows again
/// among its content instead.
fn leaves_its_write_protected_folder (
  tree   : &Tree<Viewnode>,
  parent : NodeId,
  child  : NodeId,
  walk   : &Walk,
) -> bool {
  matches! ( tree . get (parent) . unwrap () . value () . kind,
             ViewnodeKind::Folder (Folder::IntegratedElsewhere) )
  || unhidden_in_the_hides_folder (tree, parent, child, walk)
  || view_ancestor_in_its_folder (tree, parent, child, walk . truth . graph)
  || is_hidden_by_the_forks_folders_viewparent (tree, parent, child, walk)
  || is_hiddenByForkerFolder_under_no_forker_as_such (tree, parent, walk) }

/// Whether 'parent' is a folder of what a forker hides of the
/// note's content ('Folder::HiddenByForker') whose viewparent is
/// no forker as such that forks the note ('note_forked_by_forker_as_such').
/// That folder is drawn under such a forker only; under a forker a cut
/// made a forkee (now claiming) it holds nothing, so its members
/// vanish and it goes, as a fresh view would not draw it; under an
/// indirect member, whose hides do not apply to the note, likewise.
fn is_hiddenByForkerFolder_under_no_forker_as_such (
  tree   : &Tree<Viewnode>,
  parent : NodeId,
  walk   : &Walk,
) -> bool {
  let folder : NodeRef<Viewnode> = tree . get (parent) . unwrap ();
  matches! ( folder . value () . kind,
             ViewnodeKind::Folder (Folder::HiddenByForker) )
  && folder . parent () . map_or (
       true, |member| note_forked_by_forker_as_such (tree, member . id (), walk) . is_none () ) }

/// Whether 'child' stands in the forks folder at 'parent' for a note
/// the folder's viewparent hides, by a record the skgrepo restriction
/// shows: a forker or an indirect member it hides is no member of the
/// folder ('forker_only_members', 'indirect_members') but of the
/// folder of what it hides outside its fork partners, as a save that
/// deleted it from another view leaves it.
fn is_hidden_by_the_forks_folders_viewparent (
  tree   : &Tree<Viewnode>,
  parent : NodeId,
  child  : NodeId,
  walk   : &Walk,
) -> bool {
  if ! matches! ( tree . get (parent) . unwrap () . value () . kind,
                  ViewnodeKind::Folder (Folder::Forks) ) { return false; }
  let Some (note) : Option<ID> = described_node (tree, parent)
    . and_then ( |described| tree . get (described) . unwrap () . value ()
                 . occurrence_gnid () . cloned () )
    else { return false; };
  let Some (member) : Option<ID> =
    tree . get (child) . unwrap () . value () . occurrence_gnid () . cloned ()
    else { return false; };
  let graph : &InRustGraph = walk . truth . graph;
  let member : ID = graph . pid_else_raw_id (&member);
  graph . outbound_gnids_for_relation_gated (
    &note, NodeRelation::Hides, walk . truth . restriction )
    . iter () . any ( |hidden| graph . pid_else_raw_id (hidden) == member ) }

/// Whether 'child' stands in a folder at 'parent' that omits
/// view-ancestors ('Folder::omits_view_ancestors') for a gnid the view
/// shows above the node the folder describes.
fn view_ancestor_in_its_folder (
  tree   : &Tree<Viewnode>,
  parent : NodeId,
  child  : NodeId,
  graph  : &InRustGraph,
) -> bool {
  let omits : bool = matches! ( &tree . get (parent) . unwrap () . value () . kind,
                                ViewnodeKind::Folder (folder)
                                  if folder . omits_view_ancestors () );
  if ! omits { return false; }
  let Some (described) : Option<NodeId> = described_node (tree, parent)
    else { return false; };
  let Some (GoalMember::Gnid (gnid)) : Option<GoalMember> =
    member_of_child ( tree . get (child) . unwrap () . value (), graph )
    else { return false; };
  view_ancestor_gnids (tree, described, graph) . contains (&gnid) }

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
  ! graph . outbound_gnids_for_relation (&hider, NodeRelation::Hides)
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
