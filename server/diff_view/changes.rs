//! The diff view's step at each unrestricted vognode's visit
//! in view completion ('docs/diff-view.org', "The
//! per-visit step" and "What each kind of change looks like"), and at each
//! historical phantom's.
//!
//! On an editable occurrence: the content members (which the content
//! reconciler drew from the after-graph) are tagged with the presence
//! of their node and of their relationship, members outside U that
//! were not editable already become unexpanded write-protected
//! members, and the former members stand where
//! 'phantom_insertion_plan' places them. A node in C also gets, ahead
//! of its content, its old text, its home move, its "it acquired"
//! folder, the alias, ID and flags folders that changed, the partner
//! folders whose membership changed (which the BFS fills from the
//! straddle), and its changeset markers. A node in U but not in C is
//! context: its body starts folded. A write-protected occurrence gets
//! none of this; its viewparent tags it. U and C are those of the
//! buffer being drawn ('DiffViewSource::sets'); in the order-only buffer
//! both are empty, and each node there gets an order-only marker.
//!
//! Nothing that exists is drawn again ('docs/diff-view.org',
//! "The fresh drawing, and what it keeps"):
//! each step builds a fresh drawing and reconciles what the node
//! holds with it ('server/diff_view/fresh_drawing.rs'), so every
//! viewnode that stays keeps its fold state, and is moved, not
//! recreated, to where it belongs. The server sets a fold state only
//! on a viewnode it creates.

use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::in_rust_graph::relation_accessors::{
  NodeRelation, RelationRole};
use crate::graph_straddle::types::{GraphStraddle, Partner, Presence};
use crate::diff_view::change_leaves::{
  ChangeLeaves, LeafKey, NodeAspect, RelationshipAspect};
use crate::diff_view::pcsvf::{PcsvfSets, DiffViewSource};
use crate::diff_view::change_viewnodes::{
  append_acquired_folder, append_historical_phantom_content, append_old_text,
  mk_changeset_marker_viewnode, mk_historical_viewnode, mk_home_move_viewnode,
  mk_relRepo_move_viewnode, mk_removed_member_viewnode};
use crate::diff_view::fresh_drawing::{
  adopt_fresh_facts, is_drawn_by_the_diff_view,
  is_unrestricted_removed_member, reconcile_children_with_fresh_drawing};
use crate::types::git::Sign;
use crate::types::list::{Diff_Item, interleaved_diff_without_reorders};
use crate::types::misc::ID;
use crate::types::nodes::rust::GraphnodeInRust;
use crate::types::nodes::complete::Flag;
use crate::types::tree::forest::in_content_position;
use crate::types::tree::moves::{
  arrange_first_children, arrange_last_children, move_before,
  move_to_first_child, move_to_last_child};
use crate::types::views_state::DiffViewBufferKind;
use crate::types::viewnode::{
  AffectsParent, Birth, ChangeFacts, ChangesetMarkerKind, DiffPresence,
  Editability, PartnerFolder, Phantom, PhantomDeleted, Property,
  PropertyFolder, Viewnode, ViewnodeKind, Vognode,
  mk_editable_viewnode, viewforest_root_viewnode,
  viewnode_of};

use ego_tree::{NodeId, NodeMut, NodeRef, Tree};
use std::collections::{BTreeSet, HashMap, HashSet};
use std::error::Error;
use std::hash::Hash;

/// The folders of a node's recorders (its in-set changed when one
/// was added or removed), then those of the relationships it records
/// (changed when one was added, removed or moved to another skgrepo).
const IN_FOLDERS : [PartnerFolder; 5] = [
  PartnerFolder::Container, PartnerFolder::Mentioner,
  PartnerFolder::Subscriber, PartnerFolder::Overrider,
  PartnerFolder::Hider ];
const OUT_FOLDERS : [PartnerFolder; 3] = [
  PartnerFolder::Subscribee, PartnerFolder::Overridden,
  PartnerFolder::Hidden ];

/// What the change leaves that mention a node say changed about it,
/// grouped by aspect: what the per-visit step draws ahead of its
/// content and as its changed partner folders. A new aspect of a leaf
/// is added here once, and C, the drawing and readiness follow.
#[derive(Debug, Default)]
struct ChangedAspects {
  text         : bool,
  home_skgrepo : bool,
  aliases      : bool,
  extra_ids    : bool,
  flags        : bool,
  /// The relations of the relationships the node records that were
  /// added or removed (existence) or moved (relRepo).
  recorded     : HashSet<(NodeRelation, RelationshipAspect)>,
  /// The relations by which a recorder gained or lost the node.
  recorders    : HashSet<NodeRelation>,
  /// Mentions are no leaves; see 'ChangeLeaves::mentioner_sets_changed'.
  mentioners   : bool,
}

impl ChangedAspects {
  fn of_node (
    leaves : &ChangeLeaves,
    pid    : &ID,
  ) -> ChangedAspects {
    let mut aspects : ChangedAspects = ChangedAspects {
      mentioners : leaves . mentioner_sets_changed () . contains (pid),
      .. ChangedAspects::default () };
    for leaf in leaves . leaves_under (pid) {
      match &leaf . key {
        LeafKey::Node { aspect, .. } => match aspect {
          NodeAspect::Existence    => {},
          NodeAspect::Text         => aspects . text         = true,
          NodeAspect::HomeSkgrepo  => aspects . home_skgrepo = true,
          NodeAspect::Alias (_)    => aspects . aliases      = true,
          NodeAspect::ExtraId (_)  => aspects . extra_ids    = true,
          NodeAspect::Flag (_)     => aspects . flags        = true, },
        LeafKey::Relationship { relationship, aspect } => {
          aspects . recorded . insert (( relationship . relation, *aspect )); }, }}
    for leaf in leaves . leaves_naming_member (pid) {
      if let LeafKey::Relationship {
        relationship, aspect : RelationshipAspect::Existence } = &leaf . key {
        aspects . recorders . insert ( relationship . relation ); }}
    aspects }

  /// Whether a recorded relationship of this relation changed in this
  /// aspect.
  fn recorded_changed (
    &self,
    relation : NodeRelation,
    aspect   : RelationshipAspect,
  ) -> bool {
    self . recorded . contains (&( relation, aspect )) }
}

/// See the module comment.
/// Two reconcilers run at an editable node's visit. The older one,
/// 'complete_managed_children' ('update_buffer/util.rs'), reconciles
/// the content list first, as in every view (and each folder's
/// members at the folder's own visit); then this step runs the diff
/// view's, 'reconcile_children_with_fresh_drawing', over the change
/// viewnodes, the removed members and what stands below them. They
/// obey one rule: a reconciler detaches only what it drew, and what
/// the user added survives, under a stale child too
/// ('keep_stale_child_holding_the_users').
/// 'created_in_this_completion' is whether
/// view completion created the viewnode at 'treeid': only then does
/// its body's fold state come from C. 'editable_before_completion'
/// holds the viewnodes that were editable when completion began.
pub(crate) fn process_unrestrictedVognode_changes (
  tree    : &mut Tree<Viewnode>,
  treeid  : NodeId,
  drawing : DiffViewSource,
  created_in_this_completion : bool,
  editable_before_completion : &HashSet<NodeId>,
) -> Result<(), Box<dyn Error>> {
  let straddle : &GraphStraddle = drawing . straddle;
  let (pid, editable) : (ID, bool) =
    match &tree . get (treeid)
      . ok_or ("process_unrestrictedVognode_changes: node not found") ?
      . value () . kind {
      ViewnodeKind::Vognode (Vognode::Unrestricted (t)) =>
        ( t . skgid . clone (), ! t . is_writeProtected () ),
      _ => return Ok (( )), };
  let content : Vec<Partner> =
    straddle . partners (&pid, RelationRole::CONTENT);
  if ! editable {
    // What an earlier visit drew here while it was editable goes, and
    // the members it still holds are tagged as they stand now.
    detach_change_viewnodes (tree, treeid, straddle) ?;
    tag_content_members (tree, treeid, straddle, &content, None) ?;
    return Ok (( )); }
  let sets : &PcsvfSets = drawing . sets ();
  let in_c : bool = sets . c . contains (&pid);
  if created_in_this_completion {
    tree . get_mut (treeid) . unwrap () . value () . org_body_folded = ! in_c; }
  tag_content_members (
    tree, treeid, straddle, &content,
    Some (( &sets . u, editable_before_completion )) ) ?;
  reconcile_removed_content_members (tree, treeid, straddle, &pid) ?;
  let aspects : ChangedAspects = // read only for a node in C
    if in_c { ChangedAspects::of_node (straddle . change_leaves (), &pid) }
    else { ChangedAspects::default () };
  let fresh : Tree<Viewnode> = fresh_front_change_viewnodes (
    straddle, &pid, &aspects, in_c,
    drawing . diffView_buffer_kind == DiffViewBufferKind::OrderOnlyChanges
      && straddle . order_only_pids () . contains (&pid) );
  let fronts : Vec<NodeId> = reconcile_children_with_fresh_drawing (
    tree, treeid, &fresh, fresh . root () . id (), &straddle . after,
    &is_front_change_viewnode,
    &|viewnode| ! matches! ( viewnode . kind, ViewnodeKind::Vognode (_) )) ?;
  let partner_folders : Vec<NodeId> =
    if in_c { changed_partner_folders (tree, treeid, &aspects) }
    else { Vec::new () };
  let is_marker = |treeid : &NodeId| -> bool {
    matches! ( tree . get (*treeid) . unwrap () . value () . kind,
               ViewnodeKind::Property (Property::ChangesetMarker { .. }) ) };
  let order : Vec<NodeId> =
    fronts . iter () . filter ( |front| ! is_marker (front) ) . copied ()
    . chain (partner_folders)
    . chain ( fronts . iter () . filter ( |front| is_marker (front) ) . copied () )
    . collect ();
  arrange_first_children (tree, treeid, &order) ?;
  Ok (( )) }

/// Before the content reconciler runs at an editable node's visit,
/// align its members with the straddle, so that the reconciler keeps
/// each viewnode instead of drawing another: a member, a removed
/// member or a historical phantom whose relationship is in the
/// after-graph is a member; a member whose relationship is only in
/// the before-graph is set aside (affectsParent false), where the
/// step after the reconciler finds it and draws it as removed.
pub(crate) fn align_content_members_with_straddle (
  tree     : &mut Tree<Viewnode>,
  treeid   : NodeId,
  straddle : &GraphStraddle,
) -> Result<(), Box<dyn Error>> {
  let Some (pid) : Option<ID> = tree . get (treeid)
    . ok_or ("align_content_members_with_straddle: node not found") ?
    . value () . editable_skgid () . cloned ()
    else { return Ok (( )); };
  let after_list : Vec<ID> = content_list (&straddle . after, &pid);
  let after_set : HashSet<&ID> = after_list . iter () . collect ();
  let removed : HashSet<ID> = removed_content_members (straddle, &pid);
  let children : Vec<NodeId> =
    tree . get (treeid) . unwrap () . children ()
    . map ( |child| child . id () ) . collect ();
  for child in children {
    let mut child_mut : NodeMut<Viewnode> = tree . get_mut (child) . unwrap ();
    let viewnode : &mut Viewnode = child_mut . value ();
    let member_or_removed : bool = // not a viewnode the user set aside
      is_unrestricted_removed_member (viewnode)
      || matches! ( &viewnode . kind,
        ViewnodeKind::Vognode (Vognode::Unrestricted (t))
          if t . affectsParent == AffectsParent::True );
    let returned : Option<Viewnode> = match &mut viewnode . kind {
      ViewnodeKind::Vognode (Vognode::Unrestricted (t)) if member_or_removed => {
        if after_set . contains (&t . skgid) {
          t . affectsParent = AffectsParent::True; }
        else if removed . contains (&t . skgid) {
          t . affectsParent = AffectsParent::False; }
        None },
      ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Historical (h)))
        if after_set . contains (&h . skgid) =>
        straddle . after . get (&h . skgid) . map ( |node|
          mk_editable_viewnode (
            h . skgid . clone (), node . home_skgrepo . clone (),
            node . title . clone (), node . body . clone () )),
      _ => None, };
    if let Some (member) = returned {
      viewnode . kind = member . kind; }}
  Ok (( )) }

/// What a historical phantom's visit did.
#[derive(Debug, PartialEq, Eq)]
pub(crate) enum HistoricalPhantomVisit {
  Done,
  /// The node is back, so the root is an editable vognode again; the
  /// caller visits it as one.
  RootIsLive,
}

/// The visit of a historical phantom in a diff view. Where
/// content stands (under a vognode, or as a root), it holds what
/// 'append_historical_phantom_content' draws, reconciled with what it
/// holds already; a node drawn above itself holds nothing. In a
/// folder it holds nothing new. A root whose node is back becomes an
/// editable vognode, and one whose node is in neither graph (a
/// deletion since committed) a deleted phantom. A member of its old
/// content that was editable before completion
/// ('editable_before_completion') stays editable.
pub(crate) fn process_historical_phantom_visit (
  tree     : &mut Tree<Viewnode>,
  treeid   : NodeId,
  straddle : &GraphStraddle,
  editable_before_completion : &HashSet<NodeId>,
) -> Result<HistoricalPhantomVisit, Box<dyn Error>> {
  let (skgid, at_root, in_content_position) : (ID, bool, bool) = {
    let node : NodeRef<Viewnode> = tree . get (treeid)
      . ok_or ("process_historical_phantom_visit: node not found") ?;
    let ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Historical (h))) =
      &node . value () . kind else {
        return Ok (HistoricalPhantomVisit::Done); };
    ( h . skgid . clone (),
      node . parent () . is_some_and ( |parent|
        parent . value () . kind == ViewnodeKind::BufferRoot ),
      in_content_position (node) ) };
  if at_root {
    if let Some (node) = straddle . after . get (&skgid) {
      tree . get_mut (treeid) . unwrap () . value () . kind =
        mk_editable_viewnode (
          skgid . clone (), node . home_skgrepo . clone (),
          node . title . clone (), node . body . clone () ) . kind;
      return Ok (HistoricalPhantomVisit::RootIsLive); }}
  let Some (fresh_phantom) : Option<Viewnode> =
    mk_historical_viewnode (straddle, &skgid, None) else {
      // In neither graph.
      let mut node_mut : NodeMut<Viewnode> = tree . get_mut (treeid) . unwrap ();
      if let ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Historical (h))) =
        &node_mut . value () . kind {
        let deleted : ViewnodeKind = ViewnodeKind::Vognode (Vognode::Phantom (
          Phantom::Deleted ( PhantomDeleted {
            skgid             : h . skgid . clone (),
            home_skgrepo      : h . home_skgrepo . clone (),
            title             : h . title . clone (),
            body              : h . body . clone (),
            change_facts      : ChangeFacts::default (),
            readiness_request : None } )));
        node_mut . value () . kind = deleted; }
      let empty : Tree<Viewnode> = Tree::new (fresh_root ());
      reconcile_children_with_fresh_drawing (
        tree, treeid, &empty, empty . root () . id (), &straddle . after,
        &|viewnode| is_drawn_by_the_diff_view (viewnode, false, &straddle . after),
        &|_| false ) ?;
      return Ok (HistoricalPhantomVisit::Done); };
  if at_root {
    adopt_fresh_facts (
      tree . get_mut (treeid) . unwrap () . value (), &fresh_phantom ); }
  if ! in_content_position { return Ok (HistoricalPhantomVisit::Done); }
  let drawn_above_itself : bool =
    tree . get (treeid) . unwrap () . ancestors ()
    . any ( |ancestor| matches! ( &ancestor . value () . kind,
      ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Historical (h)))
        if h . skgid == skgid ));
  let mut fresh : Tree<Viewnode> = Tree::new (fresh_root ());
  if ! drawn_above_itself {
    let root : NodeId = fresh . root () . id ();
    append_historical_phantom_content (&mut fresh, root, straddle, &skgid); }
  let editable_members : Vec<(NodeId, Editability)> =
    // The fresh drawing write-protects them; nothing writable is
    // write-protected again.
    tree . get (treeid) . unwrap () . children ()
    . filter ( |child| editable_before_completion . contains (&child . id ()) )
    . filter_map ( |child| match &child . value () . kind {
      ViewnodeKind::Vognode (Vognode::Unrestricted (t))
        if ! t . is_writeProtected () =>
        Some (( child . id (), t . editability . clone () )),
      _ => None, } )
    . collect ();
  let kept : Vec<NodeId> = reconcile_children_with_fresh_drawing (
    tree, treeid, &fresh, fresh . root () . id (), &straddle . after,
    &|viewnode| is_drawn_by_the_diff_view (viewnode, false, &straddle . after),
    &|viewnode| ! matches! ( viewnode . kind, ViewnodeKind::Vognode (_) )) ?;
  for (member, editability) in editable_members {
    if ! kept . contains (&member) { continue; }
    if let ViewnodeKind::Vognode (Vognode::Unrestricted (t)) =
      &mut tree . get_mut (member) . unwrap () . value () . kind {
      t . editability = editability; }}
  arrange_last_children (tree, treeid, &kept) ?;
  Ok (HistoricalPhantomVisit::Done) }

/// When a save deleted the node of the deleted phantom at 'treeid'
/// and the straddle holds it as only-before, it becomes the node's
/// historical phantom, in place. Returns whether it did.
pub(crate) fn deleted_phantom_becomes_historical (
  tree     : &mut Tree<Viewnode>,
  treeid   : NodeId,
  straddle : &GraphStraddle,
) -> Result<bool, Box<dyn Error>> {
  let (skgid, parent_pid) : (ID, Option<ID>) = {
    let node : NodeRef<Viewnode> = tree . get (treeid)
      . ok_or ("deleted_phantom_becomes_historical: node not found") ?;
    let ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Deleted (d))) =
      &node . value () . kind else { return Ok (false); };
    ( d . skgid . clone (),
      node . parent () . and_then ( |parent| match &parent . value () . kind {
        ViewnodeKind::Vognode (Vognode::Unrestricted (t)) =>
          Some ( t . skgid . clone () ),
        _ => None, } )) };
  if straddle . node_presence (&skgid) != Some (Presence::OnlyBefore) {
    return Ok (false); }
  let rel : Option<Sign> = // its relationship to a viewparent that contained it
    parent_pid . and_then ( |parent_pid|
      straddle . partners (&parent_pid, RelationRole::CONTENT) . into_iter ()
      . find ( |partner| partner . id == skgid )
      . and_then ( |partner| partner . presence . diff_sign () ));
  let Some (historical) : Option<Viewnode> =
    mk_historical_viewnode (straddle, &skgid, rel) else { return Ok (false); };
  tree . get_mut (treeid) . unwrap () . value () . kind = historical . kind;
  Ok (true) }

/// The root of a scratch tree that holds a fresh drawing.
fn fresh_root (
) -> Viewnode {
  viewforest_root_viewnode () }

/// The change viewnodes an editable node holds ahead of its content.
/// Partner folders other than the derived "it acquired" folder are not
/// among them: one the user requested may be among a node's folders,
/// so such a folder is never detached.
fn is_front_change_viewnode (
  viewnode : &Viewnode,
) -> bool {
  matches! ( &viewnode . kind,
    ViewnodeKind::Property (
      Property::OldText { .. } | Property::HomeSkgrepoMove { .. }
      | Property::ChangesetMarker { .. } )
    | ViewnodeKind::PropertyFolder (_)
    | ViewnodeKind::PartnerFolder (PartnerFolder::Acquired) ) }

/// Detach what this step draws under an editable node.
fn detach_change_viewnodes (
  tree     : &mut Tree<Viewnode>,
  treeid   : NodeId,
  straddle : &GraphStraddle,
) -> Result<(), Box<dyn Error>> {
  let empty : Tree<Viewnode> = Tree::new (fresh_root ());
  reconcile_children_with_fresh_drawing (
    tree, treeid, &empty, empty . root () . id (), &straddle . after,
    &|viewnode| is_front_change_viewnode (viewnode)
      || is_unrestricted_removed_member (viewnode)
      || matches! ( &viewnode . kind,
        ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Historical (_))) ),
    &|_| false ) ?;
  Ok (( )) }

/// The fresh drawing of what an editable node holds ahead of its
/// content, in order, partner folders aside: for a node in C, its old
/// text, its home move, its "it acquired" folder, its alias, ID and
/// flags folders that changed, and its content and relRepo markers,
/// each as 'aspects' says; then, in the order-only buffer, its
/// order-only marker.
fn fresh_front_change_viewnodes (
  straddle   : &GraphStraddle,
  pid        : &ID,
  aspects    : &ChangedAspects,
  in_c       : bool,
  order_only : bool,
) -> Tree<Viewnode> {
  let mut fresh : Tree<Viewnode> = Tree::new (fresh_root ());
  let root : NodeId = fresh . root () . id ();
  if in_c {
    if aspects . text {
      append_old_text (&mut fresh, root, straddle, pid); }
    if aspects . home_skgrepo {
      if let Some (home_move) = mk_home_move_viewnode (straddle, pid) {
        append (&mut fresh, root, home_move); }}
    append_acquired_folder (&mut fresh, root, straddle, pid);
    append_changed_property_folders (&mut fresh, root, straddle, pid, aspects);
    if aspects . recorded_changed (
      NodeRelation::Contains, RelationshipAspect::Existence ) {
      append ( &mut fresh, root,
        mk_changeset_marker_viewnode (ChangesetMarkerKind::Content) ); }
    if aspects . recorded_changed (
      NodeRelation::Contains, RelationshipAspect::RelRepo ) {
      append ( &mut fresh, root,
        mk_changeset_marker_viewnode (ChangesetMarkerKind::RelRepo) ); }}
  if order_only {
    append ( &mut fresh, root,
      mk_changeset_marker_viewnode (ChangesetMarkerKind::OrderOnly) ); }
  fresh }

/// The node's partner folders whose membership changed, each found
/// or created: the folders of its recorders (changed when one was
/// added or removed), then those of the relationships it records
/// (changed when one was added, removed or moved to another skgrepo).
/// The view completion's visit of each fills it from the straddle.
fn changed_partner_folders (
  tree    : &mut Tree<Viewnode>,
  treeid  : NodeId,
  aspects : &ChangedAspects,
) -> Vec<NodeId> {
  let mut folders : Vec<NodeId> = Vec::new ();
  for kind in IN_FOLDERS . into_iter () . chain (OUT_FOLDERS) {
    let relation : NodeRelation = kind . relation_member_role ()
      . expect ("a relation folder has a member role") . relation;
    let changed : bool =
      if OUT_FOLDERS . contains (&kind) {
        aspects . recorded_changed (relation, RelationshipAspect::Existence)
        || aspects . recorded_changed (relation, RelationshipAspect::RelRepo) }
      else if relation == NodeRelation::LinksTo { aspects . mentioners }
      else { aspects . recorders . contains (&relation) };
    if changed {
      folders . push ( child_folder_or_new (tree, treeid, kind) ); }}
  folders }

/// Tag each content member, and each non-member child whose node is
/// content, with the presence of its node and of its relationship,
/// draw a member outside U as an unexpanded write-protected member unless it was
/// editable before completion ('u_and_editable', when given), and
/// keep a relRepo move under exactly the members whose relationship
/// moved. A member's view requests stay: one that
/// carries an editable-view request is drawn as the request says at
/// its own visit.
fn tag_content_members (
  tree     : &mut Tree<Viewnode>,
  treeid   : NodeId,
  straddle : &GraphStraddle,
  content  : &[Partner],
  u_and_editable : Option<( &BTreeSet<ID>, &HashSet<NodeId> )>,
) -> Result<(), Box<dyn Error>> {
  let by_id : HashMap<&ID, &Partner> =
    content . iter () . map ( |partner| (&partner . id, partner) ) . collect ();
  let members : Vec<NodeId> =
    tree . get (treeid) . unwrap () . children ()
    . map ( |child| child . id () ) . collect ();
  for member in members {
    let mut moved : Option<Viewnode> = None;
    { let mut member_mut = tree . get_mut (member) . unwrap ();
      match &mut member_mut . value () . kind {
        ViewnodeKind::Vognode (Vognode::Unrestricted (t))
          if t . affectsParent == AffectsParent::True
          || by_id . contains_key (&t . skgid) => {
          // A non-member whose node is content (as under a
          // write-protected node) is tagged too, but never redrawn.
          let partner : Option<&&Partner> = by_id . get (&t . skgid);
          t . change_facts . presence = DiffPresence {
            node : straddle . node_presence (&t . skgid)
                   . and_then ( |presence| presence . diff_sign () ),
            rel  : partner . and_then ( |p| p . presence . diff_sign () ), };
          if t . affectsParent == AffectsParent::True
             && u_and_editable . is_some_and ( |(u, writable)|
            ! u . contains (&t . skgid) && ! writable . contains (&member) ) {
            // Not 'write_protect': the member keeps its view requests.
            t . editability = Editability::WriteProtected; }
          moved = partner . and_then ( |p| mk_relRepo_move_viewnode (p) ); },
        ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Unknown (unknown))) => {
          unknown . change_facts . presence . rel =
            by_id . get (&unknown . skgid)
            . and_then ( |p| p . presence . diff_sign () ); },
        _ => continue, }}
    let mut fresh : Tree<Viewnode> = Tree::new (fresh_root ());
    if let Some (moved) = moved {
      fresh . root_mut () . append (moved); }
    let kept : Vec<NodeId> = reconcile_children_with_fresh_drawing (
      tree, member, &fresh, fresh . root () . id (), &straddle . after,
      &|viewnode| matches! ( &viewnode . kind,
        ViewnodeKind::Property (Property::RelRepoMove { .. }) ),
      &|_| false ) ?;
    if let Some (move_viewnode) = kept . first () {
      move_to_first_child (tree, member, *move_viewnode) ?; }}
  Ok (( )) }

/// The members of the before-graph's content list that the
/// after-graph's lacks, each where 'phantom_insertion_plan' places it
/// (immediately before the next surviving member of the old order): a
/// deleted node as its historical phantom (whose own visit draws its
/// old content), a surviving one as a write-protected vognode that
/// does not affect its parent. Each is matched by its skgid to what
/// stands under the node already, and moved, not redrawn. Membership
/// is by set difference; the LCS only positions, so a reorder shows
/// nothing. A removed dangling ID is not drawn: an unknown phantom
/// here would be read back as a member. (Its change-forest leaf is
/// under the content changeset marker.)
fn reconcile_removed_content_members (
  tree     : &mut Tree<Viewnode>,
  treeid   : NodeId,
  straddle : &GraphStraddle,
  pid      : &ID,
) -> Result<(), Box<dyn Error>> {
  let plan : Vec<(ID, Option<ID>)> = removal_plan (straddle, pid);
  let removed : HashSet<ID> =
    plan . iter () . map ( |(removed, _)| removed . clone () ) . collect ();
  let mut fresh : Tree<Viewnode> = Tree::new (fresh_root ());
  let mut anchors : Vec<Option<ID>> = Vec::new ();
  for (removed, anchor) in plan {
    let drawn : Option<Viewnode> = match straddle . node_presence (&removed) {
      Some (Presence::OnlyBefore) =>
        mk_historical_viewnode (straddle, &removed, Some (Sign::Minus)),
      Some (_) => Some (
        mk_removed_member_viewnode (straddle, &removed)
        . ok_or ("reconcile_removed_content_members: a surviving member is not in the after-graph") ? ),
      None => None, };
    if let Some (drawn) = drawn {
      fresh . root_mut () . append (drawn);
      anchors . push (anchor); }}
  let kept : Vec<NodeId> = reconcile_children_with_fresh_drawing (
    tree, treeid, &fresh, fresh . root () . id (), &straddle . after,
    &|viewnode| match &viewnode . kind {
      ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Historical (_))) => true,
      ViewnodeKind::Vognode (Vognode::Unrestricted (t)) =>
        is_unrestricted_removed_member (viewnode)
        || ( t . affectsParent == AffectsParent::False
             && t . birth == Birth::Unremarkable
             && removed . contains (&t . skgid) ),
      _ => false, },
    &|_| false ) ?;
  let member_treeids : HashMap<ID, NodeId> =
    tree . get (treeid) . unwrap () . children ()
    . filter_map ( |child| match &child . value () . kind {
      ViewnodeKind::Vognode (Vognode::Unrestricted (t))
        if t . affectsParent == AffectsParent::True =>
        Some (( t . skgid . clone (), child . id () )),
      ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Unknown (u))) =>
        Some (( u . skgid . clone (), child . id () )),
      _ => None, } )
    . collect ();
  for (removed_treeid, anchor) in kept . into_iter () . zip (anchors) {
    match anchor . and_then ( |a| member_treeids . get (&a) . copied () ) {
      Some (anchor_treeid) => move_before (tree, anchor_treeid, removed_treeid) ?,
      None => move_to_last_child (tree, treeid, removed_treeid) ?, }}
  Ok (( )) }

/// 'phantom_insertion_plan' of the node's content lists.
fn removal_plan (
  straddle : &GraphStraddle,
  pid      : &ID,
) -> Vec<(ID, Option<ID>)> {
  phantom_insertion_plan ( &interleaved_diff_without_reorders (
    &content_list (&straddle . before, pid),
    &content_list (&straddle . after, pid) )) }

/// The members of the node's content list in the before-graph that
/// its list in the after-graph lacks.
fn removed_content_members (
  straddle : &GraphStraddle,
  pid      : &ID,
) -> HashSet<ID> {
  removal_plan (straddle, pid) . into_iter ()
    . map ( |(removed, _)| removed ) . collect () }

/// Decide where each removed-member phantom belongs among its surviving
/// siblings: insert it immediately before the *next* surviving child in
/// net HEAD->worktree order, or append it (anchor = None) when no surviving
/// child follows. Returns (removed_id, anchor_id) pairs in the order the
/// phantoms should be created, so consecutive removals before the same
/// survivor keep their relative order.
pub(crate) fn phantom_insertion_plan (
  net_contains : &[Diff_Item<ID>],
) -> Vec<(ID, Option<ID>)> {
  let mut plan : Vec<(ID, Option<ID>)> = Vec::new ();
  let mut next_survivor : Option<ID> = None;
  for item in net_contains . iter () . rev () {
    match item {
      Diff_Item::Unchanged (skgid) | Diff_Item::New (skgid)
        => { next_survivor = Some ( skgid . clone () ); },
      Diff_Item::Removed (skgid)
        => { plan . push ( ( skgid . clone (), next_survivor . clone () )); }, }}
  plan . reverse ();
  plan }

/// A node's content list in one graph, each member as that graph
/// resolves it.
fn content_list (
  graph : &InRustGraph,
  pid   : &ID,
) -> Vec<ID> {
  graph . outbound_member_keys_for_relation_gated (
    pid, NodeRelation::Contains, None )
    . into_iter ()
    . map ( |member| member . member . into_id () )
    . collect () }

/// Under 'treeid', the node's alias, ID and flags folders, those whose
/// list changed ('aspects'), each holding the union of the list: the
/// old order where the LCS puts a removed entry, + on a gained entry,
/// - on a lost one. A lost alias or ID is a 'Property::RemovedAlias'
/// or 'Property::RemovedID'.
fn append_changed_property_folders (
  tree     : &mut Tree<Viewnode>,
  treeid   : NodeId,
  straddle : &GraphStraddle,
  pid      : &ID,
  aspects  : &ChangedAspects,
) {
  let (Some (before), Some (after)) =
    ( straddle . before . get (pid), straddle . after . get (pid) ) else {
      return; };
  if aspects . aliases {
    append_property_folder (
      tree, treeid, PropertyFolder::Alias,
      signed_union ( &before . alias_texts (), &after . alias_texts () )
      . into_iter ()
      . map ( |(text, sign)| match sign {
        Some (Sign::Minus) => Property::RemovedAlias {
          text, change_facts : facts_of_entry (sign), readiness_request : None },
        _ => Property::Alias {
          text, relRepo : None, relRepo_request : None,
          change_facts : facts_of_entry (sign), readiness_request : None }, } )
      . collect () ); }
  if aspects . extra_ids {
    let ids = |node : &GraphnodeInRust| -> Vec<ID> {
      std::iter::once ( pid . clone () )
        . chain ( node . extra_ids . iter () . cloned () ) . collect () };
    append_property_folder (
      tree, treeid, PropertyFolder::ID,
      signed_union ( &ids (before), &ids (after) ) . into_iter ()
      . map ( |(skgid, sign)| match sign {
        Some (Sign::Minus) => Property::RemovedID {
          skgid, change_facts : facts_of_entry (sign), readiness_request : None },
        _ => Property::ID {
          skgid, change_facts : facts_of_entry (sign), readiness_request : None }, } )
      . collect () ); }
  if aspects . flags {
    append_property_folder (
      tree, treeid, PropertyFolder::flags (),
      Flag::ALL . into_iter ()
      . filter_map ( |flag| {
        let sign : Option<Sign> =
          match ( before . flags . contains (&flag),
                  after  . flags . contains (&flag) ) {
            (false, true)  => Some (Sign::Plus),
            (true,  false) => Some (Sign::Minus),
            (true,  true)  => None,
            (false, false) => return None, };
        Some ( Property::Flag {
          flag, title : String::new (), body : None,
          change_facts : facts_of_entry (sign), readiness_request : None } ) } )
      . collect () ); }
  }

/// The entries of the union of two lists, in the order of their
/// interleaved diff, each with + if only the new list holds it and
/// - if only the old one does.
fn signed_union<T : Clone + Eq + Hash + Ord> (
  old : &[T],
  new : &[T],
) -> Vec<(T, Option<Sign>)> {
  let old_set : HashSet<&T> = old . iter () . collect ();
  interleaved_diff_without_reorders (old, new) . into_iter ()
    . map ( |item| match item {
      Diff_Item::Removed (x) => (x, Some (Sign::Minus)),
      Diff_Item::Unchanged (x) | Diff_Item::New (x) => {
        let sign : Option<Sign> =
          ( ! old_set . contains (&x) ) . then_some (Sign::Plus);
        (x, sign) }, } )
    . collect () }

fn facts_of_entry (
  sign : Option<Sign>,
) -> ChangeFacts {
  ChangeFacts { presence : DiffPresence { node : None, rel : sign },
                ready    : None,
                leaves   : None } }

/// Under 'treeid', a folder 'kind' holding 'entries'.
fn append_property_folder (
  tree    : &mut Tree<Viewnode>,
  treeid  : NodeId,
  kind    : PropertyFolder,
  entries : Vec<Property>,
) {
  let folder : NodeId =
    append ( tree, treeid, viewnode_of ( ViewnodeKind::PropertyFolder (kind) ));
  for entry in entries {
    append ( tree, folder, viewnode_of ( ViewnodeKind::Property (entry) )); }}

/// The node's partner folder of this kind, or a new one appended.
fn child_folder_or_new (
  tree   : &mut Tree<Viewnode>,
  treeid : NodeId,
  kind   : PartnerFolder,
) -> NodeId {
  let existing : Option<NodeId> =
    tree . get (treeid) . unwrap () . children ()
    . find ( |child| child . value () . kind == ViewnodeKind::PartnerFolder (kind) )
    . map ( |child| child . id () );
  existing . unwrap_or_else ( ||
    append ( tree, treeid, viewnode_of ( ViewnodeKind::PartnerFolder (kind) ))) }

fn append (
  tree     : &mut Tree<Viewnode>,
  treeid   : NodeId,
  viewnode : Viewnode,
) -> NodeId {
  tree . get_mut (treeid) . unwrap () . append (viewnode) . id () }

#[cfg(test)]
#[path = "../../tests/unit/render_changes.rs"]
mod phantom_insertion_plan_tests;
