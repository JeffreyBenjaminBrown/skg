//! The diff view's own viewnodes, made from the graph straddle
//! ('docs/diff-view.org', "What each kind of change
//! looks like"): the old text and its body diff, the home and relRepo
//! moves, the changeset markers, the historical phantom of a node
//! the after-graph lacks, and the two derived acquisition folders.
//! Each is save-inert. The kinds of change they show are set here;
//! readiness is read from the store where they are placed.

use crate::dbs::in_rust_graph::relation_accessors::RelationRole;
use crate::graph_straddle::types::{GraphStraddle, Partner};
use crate::types::misc::{ID, SkgrepoChange};
use crate::types::nodes::rust::GraphnodeInRust;
use crate::types::viewnode::{
  ChangeFacts, ChangesetMarkerKind, DiffChange, Existence,
  Editability, PartnerFolder, Phantom, PhantomHistorical, ChangeViewnode,
  Viewnode, ViewnodeKind, Vognode, mk_unknown_viewnode, mk_viewnode,
  mk_writeProtected_viewnode, viewnode_of};

use ego_tree::{NodeId, Tree};
use similar::{ChangeTag, TextDiff};
use std::collections::{BTreeSet, HashSet};

/// Under 'parent', the old text of the node 'pid', with a body diff
/// under it when the body changed. None (and nothing appended) when
/// its text did not change.
pub fn append_old_text (
  tree     : &mut Tree<Viewnode>,
  parent   : NodeId,
  straddle : &GraphStraddle,
  pid      : &ID,
) -> Option<NodeId> {
  let (before, after) : (&GraphnodeInRust, &GraphnodeInRust) =
    ( straddle . before . get (pid) ?, straddle . after . get (pid) ? );
  let title_changed : bool = before . title != after . title;
  let body_changed  : bool = before . body  != after . body;
  if ! title_changed && ! body_changed { return None; }
  let old_text : Viewnode = viewnode_of ( ViewnodeKind::ChangeViewnode (
    ChangeViewnode::OldText {
      title : title_changed . then ( || before . title . clone () ),
      body  : body_changed
              . then ( || before . body . clone () . unwrap_or_default () ) } ));
  let old_text_treeid : NodeId =
    tree . get_mut (parent) ? . append (old_text) . id ();
  if body_changed {
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
  viewnode_of ( ViewnodeKind::ChangeViewnode ( ChangeViewnode::BodyDiff { diff } )) }

/// "⌂ -old +new", when the node's home skgrepo changed.
pub fn mk_homeSkgrepoMove_viewnode (
  straddle : &GraphStraddle,
  pid      : &ID,
) -> Option<Viewnode> {
  let (before, after) : (&GraphnodeInRust, &GraphnodeInRust) =
    ( straddle . before . get (pid) ?, straddle . after . get (pid) ? );
  if before . home_skgrepo == after . home_skgrepo { return None; }
  Some ( viewnode_of ( ViewnodeKind::ChangeViewnode (
    ChangeViewnode::HomeSkgrepoMove {
      skgrepo_change : SkgrepoChange {
        old : before . home_skgrepo . clone (),
        new : after  . home_skgrepo . clone () } } )) ) }

/// "relationship moved from X to Y", for a partner whose relationship
/// is in both graphs in different skgrepos. It goes under the member.
pub fn mk_relRepo_move_viewnode (
  partner : &Partner,
) -> Option<Viewnode> {
  if ! partner . relRepo_moved () { return None; }
  Some ( viewnode_of ( ViewnodeKind::ChangeViewnode (
    ChangeViewnode::RelRepoMove {
      skgrepo_change : SkgrepoChange {
        old : partner . relRepo_before . clone () ?,
        new : partner . relRepo_after  . clone () ? } } )) ) }

pub fn mk_changeset_marker_viewnode (
  kind : ChangesetMarkerKind,
) -> Viewnode {
  viewnode_of ( ViewnodeKind::ChangeViewnode (
    ChangeViewnode::ChangesetMarker { kind } )) }

/// The historical phantom of 'pid', a node of the before-graph that
/// the after-graph lacks. 'rel' is the existence change of its
/// relationship to its viewparent: removed under a former container,
/// None at a root or in a folder. None if the before-graph lacks it.
pub fn mk_historical_viewnode (
  straddle : &GraphStraddle,
  pid      : &ID,
  rel      : Option<Existence>,
) -> Option<Viewnode> {
  let before : &GraphnodeInRust = straddle . before . get (pid) ?;
  Some ( Viewnode {
    change_facts : ChangeFacts {
      change : DiffChange { node : Some (Existence::Removed), rel,
                            properties : false },
      ready    : None,
      leaves   : None,
      .. ChangeFacts::default () },
    .. viewnode_of ( ViewnodeKind::Vognode ( Vognode::Phantom (
      Phantom::Historical ( PhantomHistorical {
        skgid        : pid . clone (),
        home_skgrepo : before . home_skgrepo . clone (),
        title        : before . title . clone (),
        body         : before . body . clone (),
        restoreNode_request : false } )))) } ) }

/// Under 'phantom', the historical phantom of 'pid' standing where
/// content stands, what it holds: if a nodeMerge acquired it, an "it
/// was acquired by" folder, and then its old content as removed
/// members: a deleted member as a historical phantom (whose own visit
/// adds its old content), a surviving one as a removed member
/// ('mk_removed_member_viewnode', editable when its node is in 'u'),
/// and a dangling one as an unknown phantom.
pub fn append_historical_phantom_content (
  tree     : &mut Tree<Viewnode>,
  phantom  : NodeId,
  straddle : &GraphStraddle,
  pid      : &ID,
  u        : &BTreeSet<ID>,
) -> Option<()> {
  if let Some (acquirer) = straddle . acquirer_of (pid) {
    let acquirer_viewnode : Viewnode =
      mk_member_viewnode (straddle, &acquirer, None);
    let folder : NodeId = tree . get_mut (phantom) ? . append (
      viewnode_of ( ViewnodeKind::PartnerFolder (
        PartnerFolder::AcquiredBy ))) . id ();
    tree . get_mut (folder) ? . append (acquirer_viewnode); }
  for partner in straddle . partners (pid, RelationRole::CONTENT) {
    let member : Option<Viewnode> =
      if straddle . node_existence (&partner . id) == Some (Existence::Removed) {
        mk_historical_viewnode (straddle, &partner . id, Some (Existence::Removed))
      } else if straddle . node_in_either_graph (&partner . id) {
        mk_removed_member_viewnode (straddle, &partner . id, u)
      } else { Some ( mk_member_viewnode (
                 straddle, &partner . id, Some (Existence::Removed) )) };
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

/// A removed member: the image of a node of the after-graph whose
/// relationship to its viewparent is only in the before-graph, a
/// non-member with '(rel removed)', the one encoding of a removed
/// relationship ('is_unrestricted_removed_member' reads it). It is
/// created editable, with its body from the after-graph, when its
/// node is in 'u' (U of the buffer being completed), and
/// write-protected otherwise; the editability rule at its own visit
/// then write-protects an editable one whose node another occurrence
/// claimed first, as for a member. None if the after-graph lacks the
/// node.
pub fn mk_removed_member_viewnode (
  straddle : &GraphStraddle,
  pid      : &ID,
  u        : &BTreeSet<ID>,
) -> Option<Viewnode> {
  let node : &GraphnodeInRust = straddle . after . get (pid) ?;
  let mut member : Viewnode = mk_viewnode (
    pid . clone (), node . home_skgrepo . clone (), node . title . clone (),
    false,
    if u . contains (pid) {
      Editability::Editable { body : node . body . clone (), edit_request : None } }
    else { Editability::WriteProtected },
    HashSet::new () ); // view_requests
  member . change_facts . change =
    straddle . node_change (pid, Some (Existence::Removed));
  Some (member) }

/// A write-protected image of a node of the after-graph, whose
/// relationship to its viewparent changed by 'rel'; an unknown
/// phantom if the after-graph lacks it.
fn mk_member_viewnode (
  straddle : &GraphStraddle,
  pid      : &ID,
  rel      : Option<Existence>,
) -> Viewnode {
  let mut viewnode : Viewnode = match straddle . after . get (pid) {
    Some (node) => mk_writeProtected_viewnode (
      pid . clone (), node . home_skgrepo . clone (), node . title . clone (),
      true ),
    None => mk_unknown_viewnode ( pid . clone () ), };
  viewnode . change_facts . change = DiffChange { rel, .. DiffChange::default () };
  viewnode }
