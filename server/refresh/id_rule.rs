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
use crate::refresh::corrections::CorrectionKind;
use crate::refresh::extent::drawn_occurrence_is_editable;
use crate::refresh::goal::{Goal, GoalMember};
use crate::types::misc::{ID, SkgrepoName};
use crate::types::nodes::rust::GraphnodeInRust;
use crate::types::viewnode::{
  ChangeFacts, Editability, Ghost, NodeEditRequest, PropertyMember, RelationshipEditRequest,
  Unknown, Occurrence, Viewnode, ViewnodeKind, Vognode, VognodeKind, Warrant,
  default_occurrence};
use crate::update_buffer::completion_mode::CompletionMode;

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
    CompletionMode::DiffView { straddle, .. } => Some (straddle),
    CompletionMode::ContentView => None, };
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
    ( IdRuleAnswer::Occurrence (pid), Vognode::Occurrence (occurrence) ) =>
      refresh_occurrence_in_place (tree, child, occurrence, &pid, walk) ?,
    ( IdRuleAnswer::Occurrence (pid), other ) => {
      let requests : EditRequests = EditRequests::of (&other);
      let claims : bool = match &other {
        Vognode::Unknown (unknown) => unknown . claimsMembership,
        _ => true, };
      let editable : bool =
        drawn_occurrence_is_editable (tree, parent, &pid, walk);
      let occurrence : Occurrence =
        occurrence_from_truth (&pid, editable, claims, walk) ?;
      let occurrence : Occurrence = Occurrence {
        relationshipEditRequest : requests . relationship_edit_request . clone (),
        .. occurrence };
      set_vognode_keeping_readiness (tree, child, Vognode::Occurrence (occurrence));
      if editable {
        walk . editable_gnids . insert (pid . clone ());
        walk . drawn_editable . insert (child); }
      if requests . restore_node {
        walk . corrections . record (tree, child, CorrectionKind::EditRequestDropped); }
      walk . corrections . record (tree, child, CorrectionKind::Rekinded (VognodeKind::Occurrence)); },
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
        walk . corrections . record (tree, child, CorrectionKind::EditRequestDropped); }
      walk . corrections . record (tree, child, CorrectionKind::Rekinded (VognodeKind::Ghost)); },
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
          claimsMembership        : true, } ));
      if dropped {
        walk . corrections . record (tree, child, CorrectionKind::EditRequestDropped); }
      walk . corrections . record (tree, child, CorrectionKind::Rekinded (VognodeKind::Unknown)); },
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
    walk . correction_policy . reports_entombstoning (gnid)
    && ! node . parent () . is_some_and (
         |parent| walk . quiet_tombstones . contains (&parent . id ()) ) };
  if reported {
    walk . corrections . record (tree, treeid, CorrectionKind::Entombstoned); }
  else { walk . quiet_tombstones . insert (treeid); }
  entombstone_reporting_requests_only (tree, treeid, walk); }

/// Make the change viewnode at 'treeid', whose change is gone, a
/// tombstone, keeping its children. In the diff view a change viewnode
/// describes the straddle and is recomputed at every refresh, so its
/// going is no correction; only a request it carried, dropped, is
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
    walk . corrections . record (tree, treeid, CorrectionKind::EditRequestDropped); }
  tree . get_mut (treeid) . unwrap () . value ()
    . replace_kind (ViewnodeKind::Tombstone); }

/// An occurrence that the truth still has: it stands for the node's
/// pid, with the truth's title and home skgrepo, and, when editable,
/// its body.
fn refresh_occurrence_in_place (
  tree       : &mut Tree<Viewnode>,
  child      : NodeId,
  occurrence : Occurrence,
  pid        : &ID,
  walk       : &mut Walk,
) -> Result<(), Box<dyn Error>> {
  let node : &GraphnodeInRust = walk . truth . graph . get (pid)
    . ok_or ("refresh_occurrence_in_place: the truth lacks a present node") ?;
  if occurrence . gnid != *pid && ! occurrence . is_writeProtected () {
    // A gnid that became an extra ID: the node it now names may have
    // its editable occurrence elsewhere.
    walk . editable_gnids . remove (&occurrence . gnid);
    if walk . editable_gnids . contains (pid) {
      write_protect_occurrence (tree, child, walk); }
    else { walk . editable_gnids . insert (pid . clone ()); }}
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
                           isWarranted         : ghost . isWarranted,
                           .. fresh }; }}}

/// An occurrence of 'pid' as the truth has it, editable (with its
/// body) or write-protected.
pub(crate) fn occurrence_from_truth (
  pid      : &ID,
  editable : bool,
  claims   : bool,
  walk     : &Walk,
) -> Result<Occurrence, Box<dyn Error>> {
  let node : &GraphnodeInRust = walk . truth . graph . get (pid)
    . ok_or ("occurrence_from_truth: the truth lacks the node") ?;
  Ok ( Occurrence {
    claimsMembership : claims,
    editability      : if editable {
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
  let CompletionMode::DiffView { straddle, .. } = walk . mode
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
      walk . editable_gnids . remove (&occurrence . gnid);
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
