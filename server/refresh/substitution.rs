//! Mirror substitution, the seventh of the refresh's rungs
//! ('visit.rs'): under an editable occurrence of a content view, each
//! member stands as the mirror the perspective puts in its place
//! ('substitute_mirrors'), and the view's roots are substituted by
//! their own rule ('substitute_roots'). 'docs/mirrors.org', "Mirror
//! substitution", says what a node's mirrors are.

use crate::dbs::in_rust_graph::mirrors::{
  mirror_shown, perspective_skgrepo_of_tree_containing};
use crate::refresh::editability::write_protect_occurrence;
use crate::refresh::goal::{
  Goal, GoalItem, GoalMember, has_a_fork_partner, member_of_child, note_of_member_as_such};
use crate::refresh::goal::partnerFolder_members_in_graph;
use crate::refresh::id_rule::point_occurrence_at;
use crate::refresh::Walk;
use crate::refresh::claims::vanish;
use crate::refresh::visit::{
  add_hidden_outside_folder, after_drawing, children_of, draw_item,
  folders_of_relation, warrant_of_child};
use crate::types::misc::{ID, SkgrepoName};
use crate::types::tree::moves::{move_after, move_to_first_child};
use crate::types::viewnode::{
  Folder, FolderRelation, ViewRequest, Viewnode, ViewnodeKind, Vognode, Warrant, viewnode_of};

use ego_tree::{NodeId, NodeRef, Tree};
use std::error::Error;
use std::mem::discriminant;

/// In a content view, under an editable occurrence, each member
/// stands as the mirror the perspective puts in its place
/// ('server/dbs/in_rust_graph/mirrors.rs'), marked '(inPlaceOf N)'
/// for the original N, which the save collects instead. The
/// perspective is that of the home skgrepo of the parent's top-level
/// tree's root ('perspective_skgrepo_of_tree_containing'), or of one
/// skgrepo throughout, in a view switched to it. Not
/// where the parent itself is shown raw where another mirror would
/// stand (a root, or a member of a folder that is no member as such):
/// there its children are raw too. A member already shown in
/// another's place is re-pointed whenever it is not the mirror shown,
/// as after a switch of perspective. A re-pointed member's folders
/// are drawn afresh, for the node it now shows ('redraw_folders'):
/// those it held describe the node it showed before. Where the mirror shown is the
/// original itself, the member shown in its place loses
/// 'inPlaceOf', is warranted only as itself (and so, unwarranted,
/// vanishes), and the original is reinstated where it stood. The diff view
/// substitutes nothing. At the buffer root, the view's roots are
/// substituted by their own rule ('substitute_roots').
pub(super) fn substitute_mirrors (
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
  let skgrepo : SkgrepoName = perspective_skgrepo_of_tree_containing (
    walk . truth . config, walk . truth . graph, &walk . perspective,
    tree . get (parent) . unwrap () );
  if is_raw_where_another_mirror_would_stand (tree, parent, &skgrepo, walk)
  { return Ok (( )); }
  for child in children_of (tree, parent) {
    let (gnid, in_place_of, warrant) : (ID, Option<ID>, Warrant) =
      match &tree . get (child) . unwrap () . value () . kind {
        ViewnodeKind::Vognode (Vognode::Occurrence (occurrence)) =>
          ( occurrence . gnid . clone (), occurrence . in_place_of . clone (),
            occurrence . viewStats . warrant ),
        _ => continue, };
    let pid : ID = walk . truth . graph . pid_else_raw_id (&gnid);
    match in_place_of {
      None => if warrant == Warrant::ByGraph {
        let shown : ID = mirror_shown (
          walk . truth . config, walk . truth . graph, walk . truth . restriction,
          &skgrepo, &gnid );
        if shown != pid {
          write_protect_what_its_folders_hold (tree, child, walk);
          show_mirror (tree, child, &shown, Some (gnid), walk) ?;
          redraw_folders (tree, child, walk) ?; }},
      Some (original) => {
        let shown : ID = mirror_shown (
          walk . truth . config, walk . truth . graph, walk . truth . restriction,
          &skgrepo, &original );
        if shown == walk . truth . graph . pid_else_raw_id (&original) {
          stop_substituting (tree, parent, child, &original, goal, walk) ?;
        } else if shown != pid {
          write_protect_what_its_folders_hold (tree, child, walk);
          show_mirror (tree, child, &shown, Some (original), walk) ?;
          redraw_folders (tree, child, walk) ?; }}, }}
  Ok (( )) }

/// Each root of the view shows the mirror the perspective of its tree
/// ('perspective_skgrepo_of_tree_containing') puts in the place of the
/// node it stands for (its '(inPlaceOf N)', else itself)
/// ('mirror_shown'), marked '(inPlaceOf N)' unless it is the node
/// itself. Under the root homes that node stands in its own home
/// skgrepo, so it shows itself, as every fresh view does.
/// The occurrence is re-pointed where it stands, so it keeps its
/// viewnode ID whichever way it switches, and its children follow
/// from its own visit. A re-pointed root's folders are drawn afresh,
/// for the node it now shows ('redraw_folders'), and it asks for what
/// a fresh view of that node asks for ('ask_for_what_a_fresh_root_asks'). A fork of a root
/// re-roots the view at the clone, which stands for nothing
/// ('replace_saved_view_fork_roots'), and is substituted like any root.
fn substitute_roots (
  tree        : &mut Tree<Viewnode>,
  buffer_root : NodeId,
  walk        : &mut Walk,
) -> Result<(), Box<dyn Error>> {
  for root in children_of (tree, buffer_root) {
    let (gnid, in_place_of) : (ID, Option<ID>) =
      match &tree . get (root) . unwrap () . value () . kind {
        ViewnodeKind::Vognode (Vognode::Occurrence (occurrence)) =>
          ( occurrence . gnid . clone (), occurrence . in_place_of . clone () ),
        _ => continue, };
    let original : ID = in_place_of . clone ()
      . unwrap_or_else ( || walk . truth . graph . pid_else_raw_id (&gnid) );
    let skgrepo : SkgrepoName = perspective_skgrepo_of_tree_containing (
      walk . truth . config, walk . truth . graph, &walk . perspective,
      tree . get (root) . unwrap () );
    let shown : ID = mirror_shown (
      walk . truth . config, walk . truth . graph, walk . truth . restriction,
      &skgrepo, &original );
    let marked : Option<ID> = // as the root's '(inPlaceOf N)' says it, else none
      ( shown != walk . truth . graph . pid_else_raw_id (&original) )
      . then ( || original . clone () );
    let repointed : bool = walk . truth . graph . pid_else_raw_id (&gnid) != shown;
    let was_editable : bool =
      tree . get (root) . unwrap () . value () . editable_gnid () . is_some ();
    if repointed {
      write_protect_what_its_folders_hold (tree, root, walk); }
    if repointed || in_place_of != marked
    { show_mirror (tree, root, &shown, marked, walk) ?; }
    if repointed {
      redraw_folders (tree, root, walk) ?;
      ask_for_what_a_fresh_root_asks (tree, root, was_editable); }}
  Ok (( )) }

/// The occurrence at 'occurrence' is about to show another node, so
/// every folder it holds describes the node it shows now, and will be
/// drawn afresh ('redraw_folders'). Each occurrence in them is
/// write-protected first, so that an editability one of them holds
/// (a forkee as such there, drawn editable, may be the node the
/// occurrence will show) is free for the occurrence to take.
fn write_protect_what_its_folders_hold (
  tree       : &mut Tree<Viewnode>,
  occurrence : NodeId,
  walk       : &mut Walk,
) {
  let held : Vec<NodeId> =
    tree . get (occurrence) . unwrap () . children ()
    . filter ( |child| matches! ( child . value () . kind, ViewnodeKind::Folder (_) ))
    . flat_map ( |folder| folder . descendants () )
    . filter ( |node| node . value () . editable_gnid () . is_some () )
    . map ( |node| node . id () )
    . collect ();
  for treeid in held {
    write_protect_occurrence (tree, treeid, walk); }}

/// The occurrence at 'occurrence' now shows another node, so every
/// folder it holds describes the node it showed before. Each vanishes,
/// and in its place stand the folders of its kind (and of its
/// relation's other side) for the node now shown ('kinds_redrawing'),
/// empty, for their own visits to fill, each hidden in the occurrence
/// if the old one was (its members are drawn anew, unfolded): a
/// write-protected one goes if it stays empty, and an editable one is
/// drawn only where the node has members for it, since an empty one
/// says the user asked for it.
fn redraw_folders (
  tree       : &mut Tree<Viewnode>,
  occurrence : NodeId,
  walk       : &mut Walk,
) -> Result<(), Box<dyn Error>> {
  let Some (pid) : Option<ID> =
    tree . get (occurrence) . unwrap () . value () . occurrence_gnid () . cloned ()
    else { return Ok (( )); };
  let folders : Vec<(NodeId, Folder)> =
    tree . get (occurrence) . unwrap () . children ()
    . filter_map ( |child| match &child . value () . kind {
        ViewnodeKind::Folder (folder) => Some (( child . id (), folder . clone () )),
        _ => None, } )
    . collect ();
  let mut drawn : Vec<Folder> = Vec::new ();
  for (treeid, folder) in folders {
    let folded : bool = tree . get (treeid) . unwrap () . value () . foldAndFocus . folded;
    for kind in kinds_redrawing (&folder) {
      if drawn . iter () . any ( |done| discriminant (done) == discriminant (&kind) )
         || ( kind . is_editable () && ! has_members (&pid, &kind, walk) )
      { continue; }
      let mut empty : Viewnode = viewnode_of ( ViewnodeKind::Folder (kind . clone ()) );
      empty . foldAndFocus . folded = folded;
      let new : NodeId = tree . get_mut (treeid) . unwrap () . insert_before (empty) . id ();
      if matches! ( kind, Folder::Forks ) {
        add_hidden_outside_folder (tree, new, &pid, walk); }
      drawn . push (kind); }
    vanish (tree, treeid, walk) ?; }
  Ok (( )) }

/// A root re-pointed to another node asks for what a fresh view of
/// that node asks for ('stub_of_fresh_content_view'): its containers
/// folder, which the requests rung draws (what contains it, but its
/// view-ancestors, or no folder where nothing does); and, if the root
/// was editable, to be extended as an editable root is, which at its
/// own visit adds its default folders ('add_default_folders'): the
/// forks folder, where it has a fork partner. A write-protected root,
/// such as a search hit, asks for no more than it had. So a root that
/// was write-protected only because another occurrence held the node
/// it was re-pointed to ('point_occurrence_at') stays so when switched
/// back: nothing remembers why it was write-protected. A root asks
/// even if it held no folder: the view was drawn for another node.
fn ask_for_what_a_fresh_root_asks (
  tree         : &mut Tree<Viewnode>,
  root         : NodeId,
  was_editable : bool,
) {
  if let ViewnodeKind::Vognode (Vognode::Occurrence (occurrence)) =
    &mut tree . get_mut (root) . unwrap () . value () . kind
  { occurrence . view_requests . insert (
      ViewRequest::Folder (FolderRelation::Containers) );
    if was_editable {
      occurrence . view_requests . insert (ViewRequest::Editable); }}}

/// The folders that stand, for the node a re-pointed occurrence now shows,
/// where a folder of the kind of 'folder' stood: those a request for
/// its relation draws, both sides of a partner relation ('(folder
/// hides)' draws the hider and hidden folders; '(folder
/// forks)' the one forks folder). None for the folders drawn inside
/// others or only in the diff view, for the illegal-edit folder, which
/// each visit draws, and for the ID folder, which no request draws.
fn kinds_redrawing (
  folder : &Folder,
) -> Vec<Folder> {
  match folder {
    Folder::Forks                  => folders_of_relation (FolderRelation::Forks),
    Folder::Hider | Folder::Hidden => folders_of_relation (FolderRelation::Hides),
    Folder::Container              => folders_of_relation (FolderRelation::Containers),
    Folder::Mentioner              => folders_of_relation (FolderRelation::Mentioners),
    Folder::Alias                  => folders_of_relation (FolderRelation::Aliases),
    Folder::Flags { .. }           => vec! [ Folder::flags () ],
    Folder::IntegratedElsewhere
      | Folder::HiddenInForkPartner | Folder::HiddenByForker
      | Folder::HiddenOutsideOfForkPartners
      | Folder::AcquiredBy | Folder::Acquired
      | Folder::ID | Folder::IllegalEdit { .. } => Vec::new (), } }

/// Whether a folder of the kind of 'folder' under an occurrence of
/// 'pid' would hold anything: the node's aliases, or its partners in
/// the folder's relation, as the skgrepo restriction shows them (for
/// the forks folder, any fork partner, as 'add_default_folders' asks,
/// a hidden forker standing in its hidden-outside folder).
fn has_members (
  pid    : &ID,
  folder : &Folder,
  walk   : &Walk,
) -> bool {
  match folder {
    Folder::Alias => walk . truth . graph . get (pid) . is_some_and (
      |node| ! node . aliases_with_relRepo_unless_home (walk . truth . restriction)
               . is_empty () ),
    Folder::Forks => has_a_fork_partner (
      walk . truth . graph, walk . truth . restriction, pid ),
    _ => partnerFolder_members_in_graph (
           walk . truth . graph, pid, folder, walk . truth . restriction )
         . is_some_and ( |members| ! members . is_empty () ), } }

fn show_mirror (
  tree          : &mut Tree<Viewnode>,
  child         : NodeId,
  shown         : &ID,
  in_place_of : Option<ID>,
  walk          : &mut Walk,
) -> Result<(), Box<dyn Error>> {
  point_occurrence_at (tree, child, shown, walk) ?;
  if let ViewnodeKind::Vognode (Vognode::Occurrence (occurrence)) =
    &mut tree . get_mut (child) . unwrap () . value () . kind
  { occurrence . in_place_of = in_place_of; }
  Ok (( )) }

/// The original that 'child' stood for now stands as itself: 'child'
/// shows itself, is
/// warranted only as itself (and so, unwarranted, vanishes, as the
/// claims rung says), and the original is reinstated where it stood,
/// unless a sibling already stands for it. A child that vanishes does
/// so first, so that what it held is free for the original to take:
/// the original's editability, say, held by a forkee as such drawn
/// editable in the child's forks folder.
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
  { occurrence . in_place_of = None; }
  let warrant : Warrant = warrant_of_child (
    Some (goal), tree . get (child) . unwrap () . value (), walk . truth . graph )
    . unwrap_or (Warrant::No);
  tree . get_mut (child) . unwrap () . value () . set_warrant (warrant);
  let vanishes : bool = // as 'vanish_if_unwarranted' would have it
    tree . get (child) . unwrap () . value () . claims () == Some (true)
    && warrant != Warrant::ByGraph;
  let after : Option<NodeId> =
    if vanishes {
      let before : Option<NodeId> =
        tree . get (child) . unwrap () . prev_sibling () . map ( |sibling| sibling . id () );
      vanish (tree, child, walk) ?;
      before
    } else { Some (child) };
  reinstate_original_after (tree, parent, after, original, goal, walk) }

/// Draw 'original' right after the child at 'after' (first, when
/// None), unless a sibling already stands for it or the goal does not
/// name it, by its pid ('original' may be an extra ID).
fn reinstate_original_after (
  tree     : &mut Tree<Viewnode>,
  parent   : NodeId,
  after    : Option<NodeId>,
  original : &ID,
  goal     : &Goal,
  walk     : &mut Walk,
) -> Result<(), Box<dyn Error>> {
  let member : GoalMember =
    GoalMember::Gnid ( walk . truth . graph . pid_else_raw_id (original) );
  let stands : bool = tree . get (parent) . unwrap () . children ()
    . any ( |sibling| member_of_child (sibling . value (), walk . truth . graph) . as_ref ()
                      == Some (&member) );
  if stands { return Ok (( )); }
  let Some (item) : Option<&GoalItem> = goal . item_for (&member) else { return Ok (( )); };
  let item : GoalItem = item . clone ();
  if let Some (drawn) = draw_item (tree, parent, &item, goal, walk) ? {
    let treeid : NodeId = tree . get_mut (parent) . unwrap () . append (drawn) . id ();
    match after {
      Some (child) => move_after (tree, child, treeid) ?,
      None => move_to_first_child (tree, parent, treeid) ?, }
    after_drawing (tree, parent, treeid, walk) ?; }
  Ok (( )) }

/// Whether the occurrence at 'treeid' is shown raw where, from the
/// perspective of 'skgrepo', another of its mirrors would stand in its
/// place: at a root, or as a member of a folder that is no member as
/// such of a forks folder ('note_of_member_as_such'), as a member of
/// every other folder is.
fn is_raw_where_another_mirror_would_stand (
  tree    : &Tree<Viewnode>,
  treeid  : NodeId,
  skgrepo : &SkgrepoName,
  walk    : &Walk,
) -> bool {
  let node : NodeRef<Viewnode> = tree . get (treeid) . unwrap ();
  let raw_position : bool = node . parent () . is_some_and ( |parent|
    match &parent . value () . kind {
      ViewnodeKind::BufferRoot => true,
      ViewnodeKind::Folder (folder) =>
        ! folder . members_are_properties ()
        && note_of_member_as_such (tree, treeid) . is_none (),
      _ => false, } );
  let ViewnodeKind::Vognode (Vognode::Occurrence (occurrence)) : &ViewnodeKind =
    &node . value () . kind else { return false; };
  raw_position
  && occurrence . in_place_of . is_none ()
  && mirror_shown ( walk . truth . config, walk . truth . graph,
                    &walk . unrestricted, skgrepo, &occurrence . gnid )
     != walk . truth . graph . pid_else_raw_id (&occurrence . gnid) }
