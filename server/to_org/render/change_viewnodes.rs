//! The diff view's own viewnodes, drawn from the graph straddle
//! ('docs/diff-view.org', "What each kind of change
//! looks like"): the old text and its body diff, the home and relRepo
//! moves, the changeset markers, the historical phantom of a node
//! the after-graph lacks, and the two derived acquisition folders.
//! Each is save-inert. The presence facts are set here; readiness is
//! read from the store where they are placed.

use crate::dbs::in_rust_graph::relation_accessors::{
  BinaryRolePosition, NodeRelation, RelationRole};
use crate::graph_straddle::types::{GraphStraddle, NodeChanges, Partner, Presence};
use crate::types::git::Sign;
use crate::types::misc::{ID, SkgrepoName};
use crate::types::nodes::rust::GraphnodeInRust;
use crate::types::viewnode::{
  AffectsParent, ChangeFacts, ChangesetMarkerKind, DiffPresence,
  PartnerFolder, Phantom, PhantomHistorical, Property, Viewnode,
  ViewnodeKind, Vognode, mk_unknown_viewnode, mk_writeProtected_viewnode};

use ego_tree::{NodeId, Tree};
use similar::{ChangeTag, TextDiff};

/// Under 'parent', the old text of the node 'pid', with a body diff
/// under it when the body changed. None (and nothing appended) when
/// its text did not change.
pub fn append_old_text (
  tree     : &mut Tree<Viewnode>,
  parent   : NodeId,
  straddle : &GraphStraddle,
  pid      : &ID,
) -> Option<NodeId> {
  let changes : NodeChanges = straddle . node_changes (pid);
  if ! changes . text_changed () { return None; }
  let (before, after) : (&GraphnodeInRust, &GraphnodeInRust) =
    ( straddle . before . get (pid) ?, straddle . after . get (pid) ? );
  let old_text : Viewnode = viewnode_of ( ViewnodeKind::Property (
    Property::OldText {
      title : changes . title_changed . then ( || before . title . clone () ),
      body  : changes . body_changed
              . then ( || before . body . clone () . unwrap_or_default () ),
      change_facts      : ChangeFacts::default (),
      readiness_request : None } ));
  let old_text_treeid : NodeId =
    tree . get_mut (parent) ? . append (old_text) . id ();
  if changes . body_changed {
    tree . get_mut (old_text_treeid) ? . append (
      mk_body_diff_viewnode (
        before . body . as_deref (), after . body . as_deref () )); }
  Some (old_text_treeid) }

/// The line diff from 'old' to 'new': each line prefixed by ' '
/// (unchanged), '-' (removed) or '+' (added).
pub fn mk_body_diff_viewnode (
  old : Option<&str>,
  new : Option<&str>,
) -> Viewnode {
  let diff : String =
    TextDiff::from_lines ( old . unwrap_or (""), new . unwrap_or ("") )
    . iter_all_changes ()
    . map ( |change| format! ( "{}{}\n",
      match change . tag () {
        ChangeTag::Equal  => ' ',
        ChangeTag::Delete => '-',
        ChangeTag::Insert => '+', },
      change . value () . trim_end_matches ('\n') ))
    . collect ();
  viewnode_of ( ViewnodeKind::Property ( Property::BodyDiff { diff } )) }

/// "⌂ -old +new", when the node's home skgrepo changed.
pub fn mk_home_move_viewnode (
  straddle : &GraphStraddle,
  pid      : &ID,
) -> Option<Viewnode> {
  let (old, new) : (SkgrepoName, SkgrepoName) =
    straddle . node_changes (pid) . home ?;
  Some ( viewnode_of ( ViewnodeKind::Property (
    Property::HomeSkgrepoMove {
      old, new,
      change_facts      : ChangeFacts::default (),
      readiness_request : None } )) ) }

/// "relationship moved from X to Y", for a partner whose relationship
/// is in both graphs in different skgrepos. It goes under the member.
pub fn mk_relRepo_move_viewnode (
  partner : &Partner,
) -> Option<Viewnode> {
  if ! partner . relRepo_moved () { return None; }
  Some ( viewnode_of ( ViewnodeKind::Property (
    Property::RelRepoMove {
      old : partner . relRepo_before . clone () ?,
      new : partner . relRepo_after  . clone () ?,
      change_facts      : ChangeFacts::default (),
      readiness_request : None } )) ) }

pub fn mk_changeset_marker_viewnode (
  kind : ChangesetMarkerKind,
) -> Viewnode {
  viewnode_of ( ViewnodeKind::Property (
    Property::ChangesetMarker {
      kind,
      change_facts      : ChangeFacts::default (),
      readiness_request : None } )) }

/// The historical phantom of 'pid', a node of the before-graph that
/// the after-graph lacks. 'rel' is the presence sign of its
/// relationship to its viewparent: Minus under a former container,
/// None at a root or in a folder. None if the before-graph lacks it.
pub fn mk_historical_viewnode (
  straddle : &GraphStraddle,
  pid      : &ID,
  rel      : Option<Sign>,
) -> Option<Viewnode> {
  let before : &GraphnodeInRust = straddle . before . get (pid) ?;
  Some ( viewnode_of ( ViewnodeKind::Vognode ( Vognode::Phantom (
    Phantom::Historical ( PhantomHistorical {
      skgid        : pid . clone (),
      home_skgrepo : before . home_skgrepo . clone (),
      title        : before . title . clone (),
      body         : before . body . clone (),
      change_facts : ChangeFacts {
        presence : DiffPresence { node : Some (Sign::Minus), rel },
        ready    : None },
      readiness_request : None } )))) ) }

/// Under 'phantom', the historical phantom of 'pid' standing where
/// content stands, what it holds: if a nodeMerge acquired it, an "it
/// was acquired by" folder, and then its old content as removed
/// members: a deleted member as a historical phantom (whose own visit
/// draws its old content), a surviving one as a write-protected
/// vognode, and a dangling one as an unknown phantom.
pub fn append_historical_phantom_content (
  tree     : &mut Tree<Viewnode>,
  phantom  : NodeId,
  straddle : &GraphStraddle,
  pid      : &ID,
) -> Option<()> {
  if let Some (acquirer) = straddle . acquirer_of (pid) {
    if let Some (acquirer_viewnode) =
      mk_member_viewnode (straddle, &acquirer, None) {
      let folder : NodeId = tree . get_mut (phantom) ? . append (
        viewnode_of ( ViewnodeKind::PartnerFolder (
          PartnerFolder::AcquiredBy ))) . id ();
      tree . get_mut (folder) ? . append (acquirer_viewnode); }}
  for partner in straddle . partners (pid, CONTENT) {
    let member : Option<Viewnode> =
      match straddle . node_presence (&partner . id) {
        Some (Presence::OnlyBefore) =>
          mk_historical_viewnode (straddle, &partner . id, Some (Sign::Minus)),
        _ => mk_member_viewnode (straddle, &partner . id, Some (Sign::Minus)), };
    if let Some (member) = member {
      tree . get_mut (phantom) ? . append (member); }}
  Some (( )) }

/// Under 'parent' (the acquirer 'pid'), an "it acquired" folder of
/// the historical phantoms of the nodes it acquired since HEAD. None
/// (and nothing appended) when it acquired none.
pub fn append_acquired_folder (
  tree     : &mut Tree<Viewnode>,
  parent   : NodeId,
  straddle : &GraphStraddle,
  pid      : &ID,
) -> Option<NodeId> {
  let acquirees : Vec<ID> = straddle . acquirees_of (pid);
  if acquirees . is_empty () { return None; }
  let folder : NodeId = tree . get_mut (parent) ? . append (
    viewnode_of ( ViewnodeKind::PartnerFolder (
      PartnerFolder::Acquired ))) . id ();
  for acquiree in &acquirees {
    if let Some (phantom) = mk_historical_viewnode (straddle, acquiree, None) {
      tree . get_mut (folder) ? . append (phantom); }}
  Some (folder) }

const CONTENT : RelationRole =
  RelationRole { relation : NodeRelation::Contains,
                 position : BinaryRolePosition::Second };

/// A write-protected image of a node of the after-graph, whose
/// relationship to its viewparent has presence 'rel'; an unknown
/// phantom if the after-graph lacks it.
fn mk_member_viewnode (
  straddle : &GraphStraddle,
  pid      : &ID,
  rel      : Option<Sign>,
) -> Option<Viewnode> {
  let presence : DiffPresence = DiffPresence { node : None, rel };
  let mut viewnode : Viewnode = match straddle . after . get (pid) {
    Some (node) => mk_writeProtected_viewnode (
      pid . clone (), node . home_skgrepo . clone (), node . title . clone (),
      AffectsParent::True ),
    None => mk_unknown_viewnode ( pid . clone () ), };
  match &mut viewnode . kind {
    ViewnodeKind::Vognode (Vognode::Unrestricted (t)) =>
      t . change_facts . presence = presence,
    ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Unknown (u))) =>
      u . change_facts . presence = presence,
    _ => return None, }
  Some (viewnode) }

fn viewnode_of (
  kind : ViewnodeKind,
) -> Viewnode {
  Viewnode { focused : false, org_folded : false, org_body_folded : false, kind } }
