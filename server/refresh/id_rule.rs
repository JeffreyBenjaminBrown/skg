//! The ID rule: what a headline that stands for a gnid becomes. Four
//! questions, in order, the first yes deciding. Does the truth have
//! the node? Then an occurrence. Otherwise, is the mode the diff view,
//! and does HEAD have the node? Then a ghost. Otherwise, does the list
//! of the headline's parent name this gnid? Then an unknown.
//! Otherwise a tombstone. A restricted node answers no to every
//! question: the truth answers 'Restricted', and the gated goal names
//! no restricted member.
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
use crate::refresh::extent::drawn_occurrence_is_editable;
use crate::refresh::goal::{Goal, GoalMember};
use crate::types::misc::{ID, SkgrepoName};
use crate::types::nodes::rust::GraphnodeInRust;
use crate::types::viewnode::{
  ChangeFacts, Editability, Ghost, NodeEditRequest, PropertyMember, RelationshipEditRequest,
  Unknown, Occurrence, Viewnode, ViewnodeKind, Vognode, VognodeKind, Warrant,
  default_occurrence};
use crate::refresh::mode::RefreshMode;

use ego_tree::{NodeId, NodeMut, NodeRef, Tree};
use std::error::Error;

/// What the ID rule makes of a gnid.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum IdRuleAnswer {
  /// An occurrence of the node with this pid.
  Occurrence (ID),
  /// The ghost of the node HEAD had under this pid.
  Ghost (ID),
  Unknown,
  Tombstone,
}

/// The ID rule for 'gnid', under a parent whose goal is
/// 'parent_goal'. In the diff view, a node HEAD had under 'gnid' that
/// a nodeMerge has since made an extra ID of another node is not back:
/// 'gnid' is its ghost's, not an occurrence of the acquirer.
pub(crate) fn id_rule (
  gnid        : &ID,
  parent_goal : Option<&Goal>,
  walk        : &Walk,
) -> IdRuleAnswer {
  let head : Option<&GraphStraddle> = match walk . mode {
    RefreshMode::DiffView { straddle, .. } => Some (straddle),
    RefreshMode::ContentView => None, };
  if walk . truth . graph . gnid_presence_gated (gnid, walk . truth . restriction )
     == GnidPresence::Present {
    let pid : ID = walk . truth . graph . pid_of (gnid)
      . expect ("a present gnid resolves");
    let acquired_since_head : bool =
      pid != *gnid
      && head . is_some_and ( |straddle|
           straddle . before . nodes . contains_key (gnid) );
    if ! acquired_since_head {
      return IdRuleAnswer::Occurrence (pid); }}
  if let Some (straddle) = head {
    if let Some (pid) = straddle . before . pid_of (gnid) {
      if ! straddle . after . nodes . contains_key (&pid) {
        return IdRuleAnswer::Ghost (pid); }}}
  if parent_goal . is_some_and ( |goal| goal . names_gnid (gnid) ) {
    return IdRuleAnswer::Unknown; }
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
      Vognode::Unknown (unknown) => EditRequests {
        relationship_edit_request : unknown . relationshipEditRequest . clone (),
        .. EditRequests::default () }, } }

  fn any (
    &self,
  ) -> bool {
    self . node_edit_request . is_some ()
    || self . relationship_edit_request . is_some ()
    || self . restore_node }
}

/// Re-kind the vognode at 'child' (a child of 'parent', whose goal
/// is 'parent_goal') by the ID rule, where it stands.
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
  match ( id_rule (&gnid, parent_goal, walk), vognode ) {
    ( IdRuleAnswer::Occurrence (pid), Vognode::Occurrence (_) ) =>
      point_occurrence_at (tree, child, &pid, walk) ?,
    ( IdRuleAnswer::Occurrence (pid), other ) => {
      let requests : EditRequests = EditRequests::of (&other);
      let claims : bool = match &other {
        Vognode::Unknown (unknown) => unknown . claimsMembership,
        _ => true, };
      let editable : bool =
        drawn_occurrence_is_editable (tree, parent, &pid, walk);
      let occurrence : Occurrence =
        occurrence_from_truth (
          &pid, EditabilityAndClaim { editable, claims }, walk) ?;
      let occurrence : Occurrence = Occurrence {
        relationshipEditRequest : requests . relationship_edit_request . clone (),
        .. occurrence };
      set_vognode_keeping_readiness (tree, child, Vognode::Occurrence (occurrence));
      if editable {
        walk . hold_editability (&pid);
        walk . drawn_editable . insert (child); }
      if requests . restore_node {
        walk . buffer_corrections . record (tree, child, BufferCorrectionKind::EditRequestDropped); }
      walk . buffer_corrections . record (tree, child, BufferCorrectionKind::Rekinded (VognodeKind::Occurrence)); },
    ( IdRuleAnswer::Ghost (pid), Vognode::Ghost (ghost) ) =>
      refresh_ghost_in_place (tree, child, ghost, &pid, walk),
    ( IdRuleAnswer::Ghost (pid), other ) => {
      let requests : EditRequests = EditRequests::of (&other);
      forget_editable_occurrence (&other, child, walk);
      let carried_claim : bool = match &other {
        Vognode::Occurrence (occurrence) => occurrence . claimsMembership,
        Vognode::Unknown (unknown) => unknown . claimsMembership,
        Vognode::Ghost (ghost) => ghost . claimsMembership, };
      let named_by_the_list : bool =
        parent_goal . and_then ( |goal| goal . item_for ( &GoalMember::Gnid ( pid . clone () )))
        . is_some_and ( |item| item . warrant == Warrant::ByGraph );
      let ghost : Ghost = Ghost {
        // The claim it carried, where the list still names it: a ghost
        // claims nothing else.
        claimsMembership : carried_claim && named_by_the_list,
        .. ghost_from_head (&pid, walk)
           . ok_or ("rekind_vognode: HEAD lacks the node it has") ? };
      set_vognode_keeping_readiness (tree, child, Vognode::Ghost (ghost));
      if requests . any () {
        walk . buffer_corrections . record (tree, child, BufferCorrectionKind::EditRequestDropped); }
      walk . buffer_corrections . record (tree, child, BufferCorrectionKind::Rekinded (VognodeKind::Ghost)); },
    ( IdRuleAnswer::Unknown, Vognode::Unknown (unknown) ) => {
      let relRepo : Option<SkgrepoName> = goal_relRepo (parent_goal, &gnid);
      if let ViewnodeKind::Vognode (Vognode::Unknown (unknown_mut)) =
        &mut tree . get_mut (child) . unwrap () . value () . kind
      { unknown_mut . relRepo = relRepo . or ( unknown . relRepo ); }},
    ( IdRuleAnswer::Unknown, other ) => {
      let requests : EditRequests = EditRequests::of (&other);
      forget_editable_occurrence (&other, child, walk);
      let relationship_edit_request : Option<RelationshipEditRequest> =
        // An unknown takes a relRepo request only.
        requests . relationship_edit_request . clone ()
        . filter ( |request| matches! ( request, RelationshipEditRequest::RelRepo (_) ));
      let dropped : bool =
        requests . node_edit_request . is_some () || requests . restore_node
        || relationship_edit_request != requests . relationship_edit_request;
      set_vognode_keeping_readiness (
        tree, child, Vognode::Unknown ( Unknown {
          gnid                   : gnid . clone (),
          relRepo                 : goal_relRepo (parent_goal, &gnid),
          relationshipEditRequest : relationship_edit_request,
          claimsMembership        : true,
          warrant                 : Warrant::ByGraph, } )); // the warrant rung sets it
      if dropped {
        walk . buffer_corrections . record (tree, child, BufferCorrectionKind::EditRequestDropped); }
      walk . buffer_corrections . record (tree, child, BufferCorrectionKind::Rekinded (VognodeKind::Unknown)); },
    ( IdRuleAnswer::Tombstone, other ) => {
      forget_editable_occurrence (&other, child, walk);
      entombstone (tree, child, walk); }, }
  Ok (( )) }

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
    walk . release_editability (&was);
    if ! walk . hold_editability (pid) {
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

/// How an occurrence the refresh draws stands: whether it is
/// editable, and whether it claims membership of its viewparent's
/// list.
pub(crate) struct EditabilityAndClaim {
  pub editable : bool,
  pub claims   : bool,
}

/// An occurrence of 'pid' as the truth has it, editable (with its
/// body) or write-protected.
pub(crate) fn occurrence_from_truth (
  pid                   : &ID,
  editability_and_claim : EditabilityAndClaim,
  walk                  : &Walk,
) -> Result<Occurrence, Box<dyn Error>> {
  let node : &GraphnodeInRust = walk . truth . graph . get (pid)
    . ok_or ("occurrence_from_truth: the truth lacks the node") ?;
  Ok ( Occurrence {
    claimsMembership : editability_and_claim . claims,
    editability      : if editability_and_claim . editable {
      Editability::Editable { body : node . body . clone (), edit_request : None }
    } else { Editability::WriteProtected },
    .. default_occurrence (
      pid . clone (), node . home_skgrepo . clone (), node . title . clone () ) } ) }

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

/// The relRepo the goal records for an unknown 'gnid'.
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
      walk . release_editability (&occurrence . gnid);
      walk . drawn_editable . remove (&treeid); }}}

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
