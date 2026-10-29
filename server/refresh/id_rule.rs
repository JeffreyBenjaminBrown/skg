//! The ID rule: what a headline that stands for a gnid becomes. Five
//! questions, in order, the first yes deciding. Does the truth have
//! the node? Then an occurrence. Otherwise, does the truth say the
//! node is restricted? Then a gnid-only vognode of kind Restricted, if
//! the goal of the headline's parent names the gnid (its record is
//! visible) or the headline is a root of the view (the view is the
//! user's own request, so its gnid reveals nothing they did not ask
//! for), and otherwise a tombstone. Otherwise, is the mode the diff
//! view, and does HEAD have the node? Then a ghost, unless the
//! restriction keeps out HEAD's node's home, which is answered as a
//! restricted node is: a restricted node is never a ghost. Otherwise,
//! does the list of the headline's parent name this gnid? Then a
//! gnid-only vognode of kind Unknown. Otherwise a tombstone.
//!
//! The headline is re-kinded where it stands, keeping its vnid, its
//! fold state, its children and, for an occurrence, its editability.
//! An edit request its new kind has no slot for is dropped, and
//! reported.

use crate::dbs::in_rust_graph::relation_accessors::GnidPresence;
use crate::diff_view::change_viewnodes::mk_historical_viewnode;
use crate::graph_straddle::types::GraphStraddle;
use crate::refresh::{Walk, write_protect_occurrence};
use crate::refresh::buffer_corrections::BufferCorrectionKind;
use crate::refresh::extent::{DrawnEditability, drawn_occurrence_editability};
use crate::refresh::visit::children_of;
use crate::refresh::goal::{Goal, GoalMember};
use crate::types::misc::{ID, SkgrepoName};
use crate::types::nodes::rust::GraphnodeInRust;
use crate::types::viewnode::{
  ChangeFacts, Editability, Ghost, GnidOnlyKind, GnidOnlyVognode, NodeEditRequest,
  PropertyMember, RelationshipEditRequest, Occurrence, ViewRequest, Viewnode, ViewnodeKind,
  Vognode, VognodeKind, Warrant, default_occurrence};
use crate::refresh::mode::RefreshMode;
use crate::skgrepo_sets::SkgrepoRestriction;

use ego_tree::{NodeId, NodeMut, NodeRef, Tree};
use std::error::Error;

/// What the ID rule makes of a gnid.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum IdRuleAnswer {
  /// An occurrence of the node with this pid.
  Occurrence (ID),
  /// The ghost of the node HEAD had under this pid.
  Ghost (ID),
  /// A gnid-only vognode: of kind Restricted where the node is
  /// restricted and the parent's goal names it, marked so, or it is a
  /// root of the view; of kind Unknown where no node answers to a gnid
  /// the parent's goal names.
  GnidOnly (GnidOnlyKind),
  Tombstone,
}

/// The ID rule for 'gnid', under a parent whose goal is
/// 'parent_goal', or under the buffer root when 'is_view_root' (which
/// has no goal). In the diff view, a node HEAD had under 'gnid' that
/// a nodeMerge has since made an extra ID of another node is not back:
/// 'gnid' is its ghost's, not an occurrence of the acquirer.
pub(crate) fn id_rule (
  gnid         : &ID,
  parent_goal  : Option<&Goal>,
  is_view_root : bool,
  walk         : &Walk,
) -> IdRuleAnswer {
  let head : Option<&GraphStraddle> = match walk . mode {
    RefreshMode::DiffView { straddle, .. } => Some (straddle),
    RefreshMode::ContentView => None, };
  let presence : GnidPresence =
    walk . truth . graph . gnid_presence_gated (gnid, walk . truth . restriction );
  if presence == GnidPresence::Present {
    let pid : ID = walk . truth . graph . pid_of (gnid)
      . expect ("a present gnid resolves");
    let acquired_since_head : bool =
      pid != *gnid
      && head . is_some_and ( |straddle|
           straddle . before . nodes . contains_key (gnid) );
    if ! acquired_since_head {
      return IdRuleAnswer::Occurrence (pid); }}
  let restricted_answer = || -> IdRuleAnswer {
    let named : bool = parent_goal . is_some_and ( |goal| goal . names_gnid (gnid) );
    if named || is_view_root {
      IdRuleAnswer::GnidOnly (GnidOnlyKind::Restricted) }
    else { IdRuleAnswer::Tombstone } };
  if presence == GnidPresence::Restricted {
    return restricted_answer (); }
  if let Some (straddle) = head {
    if let Some (pid) = straddle . before . pid_of (gnid) {
      if ! straddle . after . nodes . contains_key (&pid) {
        let restricted_at_head : bool = // by HEAD's home, the worktree lacking the node
          straddle . before . pid_and_skgrepo (gnid) . is_some_and (
            |(_pid, home)| SkgrepoRestriction::node_is_restricted (
              walk . truth . restriction, &home ));
        return if restricted_at_head { restricted_answer () }
               else { IdRuleAnswer::Ghost (pid) }; }}}
  if parent_goal . is_some_and ( |goal| goal . names_gnid (gnid) ) {
    return IdRuleAnswer::GnidOnly (GnidOnlyKind::Unknown); }
  IdRuleAnswer::Tombstone }

/// The edit requests a vognode carries, by their slots.
#[derive(Default)]
struct EditRequests {
  node_edit_request         : Option<NodeEditRequest>,
  relationship_edit_request : Option<RelationshipEditRequest>,
  restore_node              : bool,
}

impl EditRequests {
  fn of (
    vognode : &Vognode,
  ) -> EditRequests {
    match vognode {
      Vognode::Occurrence (occurrence) => EditRequests {
        node_edit_request         : occurrence . edit_request () . cloned (),
        relationship_edit_request : occurrence . relationshipEditRequest . clone (),
        restore_node              : false, },
      Vognode::Ghost (ghost) => EditRequests {
        restore_node : ghost . restoreNode_request,
        .. EditRequests::default () },
      Vognode::GnidOnly (gnid_only) => EditRequests {
        relationship_edit_request : gnid_only . relationshipEditRequest . clone (),
        .. EditRequests::default () }, } }

  /// What a gnid-only vognode keeps of these requests: a relRepo
  /// request, the one request it takes; and whether it drops any
  /// other.
  fn the_relRepo_request_only (
    &self,
  ) -> (Option<RelationshipEditRequest>, bool) {
    let kept : Option<RelationshipEditRequest> =
      self . relationship_edit_request . clone ()
      . filter ( |request| matches! ( request, RelationshipEditRequest::RelRepo (_) ));
    let dropped : bool =
      self . node_edit_request . is_some () || self . restore_node
      || kept != self . relationship_edit_request;
    (kept, dropped) }

  fn any (
    &self,
  ) -> bool {
    self . node_edit_request . is_some ()
    || self . relationship_edit_request . is_some ()
    || self . restore_node }
}

/// Re-kind the vognode at 'child' (a child of 'parent', whose goal
/// is 'parent_goal') by the ID rule, where it stands. Its ten cases,
/// by the answer and the kind the vognode was:
/// - an occurrence, from an occurrence: pointed at the pid as the
///   truth has it ('point_occurrence_at');
/// - an occurrence, from a ghost or a gnid-only vognode: its node is
///   back, or the restriction widened ('rekind_to_occurrence'), two
///   cases;
/// - a ghost, from a ghost: refreshed from HEAD, keeping its requests
///   and its claim ('refresh_ghost_in_place');
/// - a ghost, from an occurrence or a gnid-only vognode: its node was
///   deleted since HEAD ('rekind_to_ghost'), two cases;
/// - a gnid-only vognode, from one of the same kind: its relRepo taken
///   from the goal, where the goal records one;
/// - a gnid-only vognode, from one of the other kind, an occurrence or
///   a ghost ('rekind_to_gnid_only'), three cases;
/// - a tombstone, from any kind ('entombstone').
pub(crate) fn rekind_vognode (
  tree        : &mut Tree<Viewnode>,
  parent      : NodeId,
  child       : NodeId,
  parent_goal : Option<&Goal>,
  walk        : &mut Walk,
) -> Result<(), Box<dyn Error>> {
  let vognode : Vognode = match &tree . get (child) . unwrap () . value () . kind {
    ViewnodeKind::Vognode (vognode) => vognode . clone (),
    _ => return Ok (( )), };
  let gnid : ID = vognode . gnid () . clone ();
  let is_view_root : bool = matches! (
    tree . get (parent) . unwrap () . value () . kind, ViewnodeKind::BufferRoot );
  match ( id_rule (&gnid, parent_goal, is_view_root, walk), vognode ) {
    ( IdRuleAnswer::Occurrence (pid), Vognode::Occurrence (_) ) =>
      point_occurrence_at (tree, child, &pid, walk) ?,
    ( IdRuleAnswer::Occurrence (pid), other ) =>
      rekind_to_occurrence (tree, parent, child, &pid, other, walk) ?,
    ( IdRuleAnswer::Ghost (pid), Vognode::Ghost (ghost) ) =>
      refresh_ghost_in_place (tree, child, ghost, &pid, walk),
    ( IdRuleAnswer::Ghost (pid), other ) =>
      rekind_to_ghost (tree, child, &pid, other, parent_goal, walk) ?,
    ( IdRuleAnswer::GnidOnly (kind), Vognode::GnidOnly (gnid_only) )
      if gnid_only . kind == kind => {
      let relRepo : Option<SkgrepoName> = goal_relRepo (parent_goal, &gnid);
      if let ViewnodeKind::Vognode (Vognode::GnidOnly (gnid_only_mut)) =
        &mut tree . get_mut (child) . unwrap () . value () . kind
      { gnid_only_mut . relRepo = relRepo . or ( gnid_only . relRepo ); }},
    ( IdRuleAnswer::GnidOnly (kind), other ) =>
      rekind_to_gnid_only (tree, child, &gnid, kind, other, parent_goal, walk),
    ( IdRuleAnswer::Tombstone, other ) => {
      forget_editable_occurrence (&other, child, walk);
      entombstone (tree, child, walk); }, }
  Ok (( )) }

/// Make the ghost or gnid-only vognode 'other' at 'child' (a child of
/// 'parent') an occurrence of 'pid', where it stands: its node is
/// back, or the restriction widened to show it. It stands as an
/// occurrence drawn there would ('drawn_occurrence_editability'). It keeps its claim (a ghost's
/// becomes a claim) and its relationship request; a restoreNode
/// request it drops, and reports. Under what was a gnid-only vognode,
/// every child claims again ('reset_the_claims_of_children'). Its
/// re-kind from a Restricted vognode is no buffer correction after a
/// change of the restriction, which widened ('BufferCorrectionPolicy').
fn rekind_to_occurrence (
  tree   : &mut Tree<Viewnode>,
  parent : NodeId,
  child  : NodeId,
  pid    : &ID,
  other  : Vognode,
  walk   : &mut Walk,
) -> Result<(), Box<dyn Error>> {
  let requests : EditRequests = EditRequests::of (&other);
  let (claims, was_gnid_only, quiet) : (bool, bool, bool) = match &other {
    Vognode::GnidOnly (gnid_only) =>
      ( gnid_only . claimsMembership,
        true,
        // the restriction widened
        gnid_only . kind == GnidOnlyKind::Restricted
        && ! walk . buffer_correction_policy . reports_rekind_across_the_restriction () ),
    _ => (true, false, false), };
  let editability : DrawnEditability =
    drawn_occurrence_editability (tree, parent, pid, walk);
  let occurrence : Occurrence =
    occurrence_from_truth (
      pid, EditabilityAndClaim { editability, claims }, walk) ?;
  let occurrence : Occurrence = Occurrence {
    relationshipEditRequest : requests . relationship_edit_request . clone (),
    .. occurrence };
  set_vognode_keeping_readiness (tree, child, Vognode::Occurrence (occurrence));
  if was_gnid_only {
    reset_the_claims_of_children (tree, child); }
  if editability == DrawnEditability::Editable {
    walk . hold_editability (pid, child); }
  if requests . restore_node {
    walk . buffer_corrections . record (tree, child, BufferCorrectionKind::EditRequestDropped); }
  if ! quiet {
    walk . buffer_corrections . record (tree, child, BufferCorrectionKind::Rekinded (VognodeKind::Occurrence)); }
  Ok (( )) }

/// Make the occurrence or gnid-only vognode 'other' at 'child' the
/// ghost of 'pid', where it stands. It keeps the claim it carried,
/// where the list still names it; every edit request it carried it
/// drops, and reports. Its re-kind from a Restricted vognode is no
/// buffer correction after a change of the restriction, which widened
/// ('BufferCorrectionPolicy').
fn rekind_to_ghost (
  tree        : &mut Tree<Viewnode>,
  child       : NodeId,
  pid         : &ID,
  other       : Vognode,
  parent_goal : Option<&Goal>,
  walk        : &mut Walk,
) -> Result<(), Box<dyn Error>> {
  let requests : EditRequests = EditRequests::of (&other);
  let quiet : bool = // the restriction widened
    matches! ( &other, Vognode::GnidOnly (gnid_only)
                       if gnid_only . kind == GnidOnlyKind::Restricted )
    && ! walk . buffer_correction_policy . reports_rekind_across_the_restriction ();
  forget_editable_occurrence (&other, child, walk);
  let named_by_the_list : bool =
    parent_goal . and_then ( |goal| goal . item_for ( &GoalMember::Gnid ( pid . clone () )))
    . is_some_and ( |item| item . warrant == Warrant::ByGraph );
  let ghost : Ghost = Ghost {
    // The claim it carried, where the list still names it: a ghost
    // claims nothing else.
    claimsMembership : other . claims () && named_by_the_list,
    .. ghost_from_head (pid, walk)
       . ok_or ("rekind_vognode: HEAD lacks the node it has") ? };
  set_vognode_keeping_readiness (tree, child, Vognode::Ghost (ghost));
  if requests . any () {
    walk . buffer_corrections . record (tree, child, BufferCorrectionKind::EditRequestDropped); }
  if ! quiet {
    walk . buffer_corrections . record (tree, child, BufferCorrectionKind::Rekinded (VognodeKind::Ghost)); }
  Ok (( )) }

/// Make the vognode 'other' at 'child' a gnid-only vognode of 'kind'
/// for 'gnid', where it stands, keeping its claim, its relRepo request
/// (an edit request of another kind it drops, and reports) and its
/// readiness request. Its re-kind to kind Restricted is no buffer
/// correction after a change of the restriction, which made its node
/// restricted ('BufferCorrectionPolicy').
fn rekind_to_gnid_only (
  tree        : &mut Tree<Viewnode>,
  child       : NodeId,
  gnid        : &ID,
  kind        : GnidOnlyKind,
  other       : Vognode,
  parent_goal : Option<&Goal>,
  walk        : &mut Walk,
) {
  let (relationship_edit_request, dropped) : (Option<RelationshipEditRequest>, bool) =
    EditRequests::of (&other) . the_relRepo_request_only ();
  forget_editable_occurrence (&other, child, walk);
  set_vognode_keeping_readiness (
    tree, child, Vognode::GnidOnly ( GnidOnlyVognode {
      relationshipEditRequest : relationship_edit_request,
      .. GnidOnlyVognode::new (
        kind, gnid . clone (), goal_relRepo (parent_goal, gnid),
        other . claims (),
        Warrant::ByGraph ) } )); // the warrant rung sets it
  if dropped {
    walk . buffer_corrections . record (tree, child, BufferCorrectionKind::EditRequestDropped); }
  let quiet : bool = // the restriction narrowed
    kind == GnidOnlyKind::Restricted
    && ! walk . buffer_correction_policy . reports_rekind_across_the_restriction ();
  if ! quiet {
    walk . buffer_corrections . record (tree, child, BufferCorrectionKind::Rekinded (VognodeKind::GnidOnly (kind))); }}

/// Each child of the headline at 'treeid', which stood as a gnid-only
/// vognode and is now an occurrence, claims membership again. Under
/// such a parent the save reads no claim, so the claims rung
/// ('mark_claims') marked every child as claiming nothing, and the
/// printer wrote no 'noClaim': the buffer's text, parsed, says each
/// child claims, while a view kept between refreshes (a switch's
/// rerender) said none did, which a save would read as the removal of
/// every member. Each claims, as parsed text would; the claims rung
/// then marks 'noClaim' those the truth does not warrant.
fn reset_the_claims_of_children (
  tree   : &mut Tree<Viewnode>,
  treeid : NodeId,
) {
  for grandchild in children_of (tree, treeid) {
    tree . get_mut (grandchild) . unwrap () . value () . set_claim (true); }}

/// Make the headline at 'treeid' a tombstone, keeping its children,
/// and report it, and any edit request it carried. Its entombstoning
/// is no buffer correction when the policy says so of the gnid it
/// stood for, or when its viewparent was entombstoned quietly
/// ('Walk::quiet_tombstones').
pub(crate) fn entombstone (
  tree   : &mut Tree<Viewnode>,
  treeid : NodeId,
  walk   : &mut Walk,
) {
  let reported : bool = {
    let node : NodeRef<Viewnode> = tree . get (treeid) . unwrap ();
    let gnid : Option<&ID> = match &node . value () . kind {
      ViewnodeKind::Vognode (vognode) => Some ( vognode . gnid () ),
      _ => None, };
    walk . buffer_correction_policy . reports_entombstoning (gnid)
    && ! node . parent () . is_some_and (
         |parent| walk . quiet_tombstones . contains (&parent . id ()) ) };
  if reported {
    walk . buffer_corrections . record (tree, treeid, BufferCorrectionKind::Entombstoned); }
  else { walk . quiet_tombstones . insert (treeid); }
  entombstone_reporting_requests_only (tree, treeid, walk); }

/// Make the change viewnode at 'treeid', whose change is gone, a
/// tombstone, keeping its children. In the diff view a change viewnode
/// describes the straddle and is recomputed at every refresh, so its
/// going is no buffer correction; only a request it carried, dropped, is
/// reported. In a content view, where it can describe nothing, it is
/// reported as any entombstoned headline is.
pub(crate) fn entombstone_gone_change (
  tree   : &mut Tree<Viewnode>,
  treeid : NodeId,
  walk   : &mut Walk,
) {
  if walk . is_diff_view () {
    entombstone_reporting_requests_only (tree, treeid, walk); }
  else { entombstone (tree, treeid, walk); }}

fn entombstone_reporting_requests_only (
  tree   : &mut Tree<Viewnode>,
  treeid : NodeId,
  walk   : &mut Walk,
) {
  let carried_requests : bool = {
    let viewnode : &Viewnode = tree . get (treeid) . unwrap () . value ();
    viewnode . readiness_request . is_some ()
    || match &viewnode . kind {
      ViewnodeKind::Vognode (vognode) => EditRequests::of (vognode) . any (),
      ViewnodeKind::PropertyMember (PropertyMember::Alias {
        relationshipEditRequest, .. }) => relationshipEditRequest . is_some (),
      _ => false, } };
  if carried_requests {
    walk . buffer_corrections . record (tree, treeid, BufferCorrectionKind::EditRequestDropped); }
  tree . get_mut (treeid) . unwrap () . value ()
    . replace_kind (ViewnodeKind::Tombstone); }

/// Point the occurrence at 'child' at the node 'pid' as the truth has
/// it: its pid, title and home skgrepo, and, when editable, its body.
/// It keeps where it stands, its fold state and its requests. When it
/// was editable as another gnid (one that became an extra ID, or the
/// overridden node an overrider is shown in place of), the gnid's one
/// editability passes to 'pid', unless another occurrence holds that,
/// and then it is write-protected.
pub(crate) fn point_occurrence_at (
  tree  : &mut Tree<Viewnode>,
  child : NodeId,
  pid   : &ID,
  walk  : &mut Walk,
) -> Result<(), Box<dyn Error>> {
  let node : &GraphnodeInRust = walk . truth . graph . get (pid)
    . ok_or ("point_occurrence_at: the truth lacks the node") ?;
  let was_editable_as : Option<ID> =
    tree . get (child) . unwrap () . value () . editable_gnid () . cloned ();
  if let Some (was) = was_editable_as . filter ( |was| was != pid ) {
    // Write-protecting releases the gnid it holds, so it goes before
    // the gnid becomes 'pid', which the other occurrence keeps.
    walk . release_editability (&was, child);
    if ! walk . hold_editability (pid, child) {
      write_protect_occurrence (tree, child, walk); }}
  if let ViewnodeKind::Vognode (Vognode::Occurrence (occurrence)) =
    &mut tree . get_mut (child) . unwrap () . value () . kind
  { occurrence . gnid        = pid . clone ();
    occurrence . title        = node . title . clone ();
    occurrence . home_skgrepo = node . home_skgrepo . clone ();
    if let Editability::Editable { body, .. } = &mut occurrence . editability
    { *body = node . body . clone (); }}
  Ok (( )) }

/// A ghost HEAD still has, with HEAD's title, body and home skgrepo.
fn refresh_ghost_in_place (
  tree  : &mut Tree<Viewnode>,
  child : NodeId,
  ghost : Ghost,
  pid   : &ID,
  walk  : &Walk,
) {
  if let Some (fresh) = ghost_from_head (pid, walk) {
    if let ViewnodeKind::Vognode (Vognode::Ghost (ghost_mut)) =
      &mut tree . get_mut (child) . unwrap () . value () . kind
    { *ghost_mut = Ghost { restoreNode_request : ghost . restoreNode_request,
                           claimsMembership    : ghost . claimsMembership,
                           warrant         : ghost . warrant,
                           .. fresh }; }}}

/// How an occurrence the refresh draws stands: its editability, and
/// whether it claims membership of its viewparent's list.
pub(crate) struct EditabilityAndClaim {
  pub editability : DrawnEditability,
  pub claims      : bool,
}

/// An occurrence of 'pid' as the truth has it: editable (with its
/// body), write-protected, or write-protected carrying an
/// editable-view request, which its visit consumes.
pub(crate) fn occurrence_from_truth (
  pid                   : &ID,
  editability_and_claim : EditabilityAndClaim,
  walk                  : &Walk,
) -> Result<Occurrence, Box<dyn Error>> {
  let node : &GraphnodeInRust = walk . truth . graph . get (pid)
    . ok_or ("occurrence_from_truth: the truth lacks the node") ?;
  let mut occurrence : Occurrence = Occurrence {
    claimsMembership : editability_and_claim . claims,
    editability      : match editability_and_claim . editability {
      DrawnEditability::Editable =>
        Editability::Editable { body : node . body . clone (), edit_request : None },
      DrawnEditability::WriteProtected | DrawnEditability::Requested =>
        Editability::WriteProtected, },
    .. default_occurrence (
      pid . clone (), node . home_skgrepo . clone (), node . title . clone () ) };
  if editability_and_claim . editability == DrawnEditability::Requested {
    occurrence . view_requests . insert (ViewRequest::Editable); }
  Ok (occurrence) }

/// The ghost of 'pid', from HEAD. None outside the diff view, or if
/// HEAD lacks the node.
pub(crate) fn ghost_from_head (
  pid  : &ID,
  walk : &Walk,
) -> Option<Ghost> {
  let RefreshMode::DiffView { straddle, .. } : RefreshMode = walk . mode
    else { return None; };
  match mk_historical_viewnode (straddle, pid, None) ? . kind {
    ViewnodeKind::Vognode (Vognode::Ghost (ghost)) => Some (ghost),
    _ => None, } }

/// The relRepo the goal records for a gnid-only vognode's 'gnid'.
fn goal_relRepo (
  goal : Option<&Goal>,
  gnid : &ID,
) -> Option<SkgrepoName> {
  goal . and_then ( |goal| goal . item_for ( &GoalMember::Gnid ( gnid . clone () )) )
    . and_then ( |item| item . relRepo . clone () ) }

/// When the vognode at 'treeid' was an editable occurrence, its node
/// has no editable occurrence now.
fn forget_editable_occurrence (
  vognode : &Vognode,
  treeid  : NodeId,
  walk    : &mut Walk,
) {
  if let Vognode::Occurrence (occurrence) = vognode {
    if ! occurrence . is_writeProtected () {
      walk . release_editability (&occurrence . gnid, treeid); }}}

/// Set the vognode at 'treeid' to 'vognode', keeping its fold state,
/// vnid and readiness request (an edit request every vognode has a
/// slot for). Its change facts describe the kind replaced, so they go.
fn set_vognode_keeping_readiness (
  tree    : &mut Tree<Viewnode>,
  treeid  : NodeId,
  vognode : Vognode,
) {
  let mut node : NodeMut<Viewnode> = tree . get_mut (treeid) . unwrap ();
  let viewnode : &mut Viewnode = node . value ();
  viewnode . kind         = ViewnodeKind::Vognode (vognode);
  viewnode . change_facts = ChangeFacts::default (); }
