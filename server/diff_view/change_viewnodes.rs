//! The diff view's own viewnodes, made from the graph straddle
//! ('docs/diff-view.org', "What each kind of change
//! looks like"): the old text and its body diff, the home and relRepo
//! moves, the changeset markers, the other changes, the ghost of a node
//! the after-graph lacks, and the two derived acquisition folders.
//! Each is save-inert. The kinds of change they show are set here;
//! whether each is processed is read from the store where they are placed.

use crate::diff_view::change_leaves::title_in_straddle;
use crate::diff_view::other_changes::common_members_in_order;
use crate::graph_straddle::types::{GraphStraddle, Partner};
use crate::types::misc::{ID, SkgrepoChange};
use crate::types::nodes::rust::GraphnodeInRust;
use crate::types::viewnode::{
  ChangeFacts, ChangesetMarkerKind, DiffChange, Existence,
  Folder, Ghost, ChangeViewnode, OrderedList, OtherChange,
  Viewnode, ViewnodeKind, Vognode, Warrant, viewnode_of};

use similar::{Algorithm, ChangeTag, TextDiff, capture_diff_slices};

/// The old text of the node 'pid', its title or body as HEAD has it
/// where either changed; None when neither did. Its body diff is a
/// child of its own ('mk_body_diff_viewnode').
pub fn mk_old_text_viewnode (
  straddle : &GraphStraddle,
  pid      : &ID,
) -> Option<Viewnode> {
  let (before, after) : (&GraphnodeInRust, &GraphnodeInRust) =
    ( straddle . before . get (pid) ?, straddle . after . get (pid) ? );
  let title_changed : bool = before . title != after . title;
  let body_changed  : bool = before . body  != after . body;
  if ! title_changed && ! body_changed { return None; }
  Some ( viewnode_of ( ViewnodeKind::ChangeViewnode (
    ChangeViewnode::OldText {
      title : title_changed . then ( || before . title . clone () ),
      body  : body_changed
              . then ( || before . body . clone () . unwrap_or_default () ) } ))) }

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

/// The other changes of the node 'pid' ('ChangeLeaves::other_changes'),
/// with, as its body, how each list it reordered was ordered at HEAD
/// and is now ('order_diff'). None when it has none.
pub fn mk_other_changes_viewnode (
  straddle : &GraphStraddle,
  pid      : &ID,
) -> Option<Viewnode> {
  let changes : Vec<OtherChange> =
    straddle . change_leaves () . other_changes (pid) . to_vec ();
  if changes . is_empty () { return None; }
  let (before, after) : (&GraphnodeInRust, &GraphnodeInRust) =
    ( straddle . before . get (pid) ?, straddle . after . get (pid) ? );
  let orders : String = changes . iter ()
    . filter_map ( |change| match change {
      OtherChange::Order (list) => Some (*list),
      _ => None, } )
    . map ( |list| order_diff (straddle, list, before, after) )
    . collect ();
  Some ( viewnode_of ( ViewnodeKind::ChangeViewnode (
    ChangeViewnode::OtherChanges { changes, orders } ))) }

/// How the members both versions of a node give the list 'list' were
/// ordered at HEAD and are now: a header naming the list, then their
/// line diff, as a body diff writes one ('mk_body_diff_viewnode'), one
/// member a line, by its title (a node's, in the after-graph, else the
/// before-graph), its text (an alias), or its ID or flag.
fn order_diff (
  straddle : &GraphStraddle,
  list     : OrderedList,
  before   : &GraphnodeInRust,
  after    : &GraphnodeInRust,
) -> String {
  let (old, new) : (Vec<String>, Vec<String>) =
    common_members_in_order (list, before, after);
  let shown = |member : &String| -> String {
    match list {
      OrderedList::Content | OrderedList::Forks | OrderedList::Hidden =>
        title_in_straddle ( straddle, &ID::from ( member . as_str () )),
      _ => member . clone (), } };
  let mut diff : String = format! (
    "The order of its {} (- at HEAD, + now):\n", list . name () );
  for op in capture_diff_slices (Algorithm::Myers, &old, &new) {
    for change in op . iter_changes (&old, &new) {
      diff . push_str ( &format! ( "{}{}\n",
        match change . tag () {
          ChangeTag::Equal  => ' ',
          ChangeTag::Delete => '-',
          ChangeTag::Insert => '+', },
        shown ( &change . value () ) )); }}
  diff }

pub fn mk_changeset_marker_viewnode (
  kind : ChangesetMarkerKind,
) -> Viewnode {
  viewnode_of ( ViewnodeKind::ChangeViewnode (
    ChangeViewnode::ChangesetMarker { kind } )) }

/// The ghost of 'pid', a node of the before-graph that
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
      processed : None,
      leaves   : None,
      .. ChangeFacts::default () },
    .. viewnode_of ( ViewnodeKind::Vognode ( Vognode::Ghost ( Ghost {
        gnid        : pid . clone (),
        home_skgrepo : before . home_skgrepo . clone (),
        title        : before . title . clone (),
        body         : before . body . clone (),
        restoreNode_request : false,
        claimsMembership    : false,
        warrant         : Warrant::ByHead } ))) } ) }

/// The "it acquired" folder of the acquirer 'pid', drawn empty: its
/// members, the ghosts of the nodes it acquired since HEAD, are its
/// goal's. None when it acquired none.
pub fn mk_acquired_folder_viewnode (
  straddle : &GraphStraddle,
  pid      : &ID,
) -> Option<Viewnode> {
  if straddle . acquirees_of (pid) . is_empty () { return None; }
  Some ( viewnode_of ( ViewnodeKind::Folder (Folder::Acquired) )) }
