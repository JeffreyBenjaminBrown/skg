use crate::types::misc::MSV;
use crate::types::viewnode::{ mk_indefinitive_viewnode, mk_indefinitive_viewnode_with_birth };
use crate::types::viewnode::viewforest_root_viewnode;

use super::*;

fn src () -> SourceName { SourceName::from ("main") }
fn id  (s: &str) -> ID { ID ( s . to_string () ) }

/// Build a NodeRust directly (no disk I/O) for fixture graphs.
fn mk_node (
  pid          : &str,
  extra_ids    : &[&str],
  contains     : &[&str],
  textlinks_to : &[&str],
) -> NodeRust {
  NodeRust {
    pid:          id (pid),
    source:       src (),
    extra_ids:    extra_ids . iter () . map ( |s| id (s) ) . collect (),
    title:        pid . to_string (),
    aliases:      MSV::Unspecified,
    body:         None,
    contains:     contains . iter () . map ( |s| id (s) ) . collect (),
    subscribes_to:                MSV::Unspecified,
    hides_from_its_subscriptions: MSV::Unspecified,
    overrides_view_of:            MSV::Unspecified,
    misc:         Vec::new (),
    textlinks_to: textlinks_to . iter () . map ( |s| id (s) ) . collect (),
  } }

/// Insert nodes into a fresh InRustGraph.
fn graph_with (nodes: Vec<NodeRust>) -> InRustGraph {
  let mut g : InRustGraph = InRustGraph::new ();
  // Populate extra_id_to_pid first so inverse indexes resolve right.
  for n in &nodes {
    for eid in &n . extra_ids {
      g . extra_id_to_pid . insert (eid . clone (), n . pid . clone ()); } }
  for n in nodes {
    g . nodes . insert (n . pid . clone (), n); }
  g }

fn parentIs_if_normal (
  viewforest : &Tree<ViewNode>,
  nid    : NodeId,
) -> ParentIs {
  match & viewforest . get (nid) . unwrap () . value () . kind {
    ViewNodeKind::Vognode (Vognode::Active (t)) => t . parentIs,
    _ => panic! ("expected TrueNode") } }

fn birth_if_normal (
  viewforest : &Tree<ViewNode>,
  nid    : NodeId,
) -> Birth {
  match & viewforest . get (nid) . unwrap () . value () . kind {
    ViewNodeKind::Vognode (Vognode::Active (t)) => t . birth,
    _ => panic! ("expected TrueNode") } }

#[test]
fn linksto_false_claim_flipped_to_independent () {
  // Parent P, child C with birth=LinksToParent, but C's textlinks_to
  // does NOT include P. The claim is false → flip.
  let graph : InRustGraph = graph_with (vec! [
    mk_node ("P", &[], &[], &[]),
    mk_node ("C", &[], &[], &[]),   // textlinks_to: empty
  ]);
  let mut viewforest : Tree<ViewNode> = Tree::new (viewforest_root_viewnode ());
  let root : NodeId = viewforest . root () . id ();
  let p_id : NodeId = viewforest . get_mut (root) . unwrap () . append (
    mk_indefinitive_viewnode (id ("P"), src (), "P" . to_string (),
                              ParentIs::Affected) ) . id ();
  let c_id : NodeId = viewforest . get_mut (p_id) . unwrap () . append (
    mk_indefinitive_viewnode_with_birth (
      id ("C"), src (), "C" . to_string (),
      ParentIs::Independent, Birth::LinksToParent) ) . id ();

  validate_parentIs_relationships (&mut viewforest, &graph);

  assert_eq! (parentIs_if_normal (&viewforest, c_id), ParentIs::Independent,
    "LinksToParent claim with no backing textlink should flip to Independent");
}

#[test]
fn linksto_true_claim_preserved () {
  // Parent P, child C with birth=LinksToParent and C's textlinks_to
  // DOES include P. The claim is true → preserve.
  let graph : InRustGraph = graph_with (vec! [
    mk_node ("P", &[], &[], &[]),
    mk_node ("C", &[], &[], &["P"]), // C textlinks to P
  ]);
  let mut viewforest : Tree<ViewNode> = Tree::new (viewforest_root_viewnode ());
  let root : NodeId = viewforest . root () . id ();
  let p_id : NodeId = viewforest . get_mut (root) . unwrap () . append (
    mk_indefinitive_viewnode (id ("P"), src (), "P" . to_string (),
                              ParentIs::Affected) ) . id ();
  let c_id : NodeId = viewforest . get_mut (p_id) . unwrap () . append (
    mk_indefinitive_viewnode_with_birth (
      id ("C"), src (), "C" . to_string (),
      ParentIs::Independent, Birth::LinksToParent) ) . id ();

  validate_parentIs_relationships (&mut viewforest, &graph);

  assert_eq! (birth_if_normal (&viewforest, c_id), Birth::LinksToParent,
    "LinksToParent claim with a backing textlink must be preserved");
}

#[test]
fn containerof_false_claim_flipped () {
  // Parent P, child C with birth=ContainsParent, but C's contains
  // does NOT include P.
  let graph : InRustGraph = graph_with (vec! [
    mk_node ("P", &[], &[],    &[]),
    mk_node ("C", &[], &["X"], &[]),  // C contains some other pid
  ]);
  let mut viewforest : Tree<ViewNode> = Tree::new (viewforest_root_viewnode ());
  let root : NodeId = viewforest . root () . id ();
  let p_id : NodeId = viewforest . get_mut (root) . unwrap () . append (
    mk_indefinitive_viewnode (id ("P"), src (), "P" . to_string (),
                              ParentIs::Affected) ) . id ();
  let c_id : NodeId = viewforest . get_mut (p_id) . unwrap () . append (
    mk_indefinitive_viewnode_with_birth (
      id ("C"), src (), "C" . to_string (),
      ParentIs::Independent, Birth::ContainsParent) ) . id ();

  validate_parentIs_relationships (&mut viewforest, &graph);

  assert_eq! (parentIs_if_normal (&viewforest, c_id), ParentIs::Independent);
}

#[test]
fn orphan_under_dead_parent_demoted_member_under_col_kept () {
  // §A (Jeff's invariant): an Affected Active node under a non-container
  // parent (Diff phantom / DeadScaffold) demotes to Independent; a legitimate
  // col MEMBER (Affected Normal under a PartnerCol) is left untouched.
  use crate::types::viewnode::{ mk_phantom_viewnode, RoleCol };
  use crate::types::git::{ ExistenceAxes, MembershipAxes };
  let mut vf : Tree<ViewNode> = Tree::new (viewforest_root_viewnode ());
  let root : NodeId = vf . root () . id ();
  let phantom : NodeId = vf . get_mut (root) . unwrap () . append (
    mk_phantom_viewnode ( id ("PH"), src (), "PH" . to_string (),
                          ExistenceAxes::default (), MembershipAxes::default () )
  ) . id ();
  let under_phantom : NodeId = vf . get_mut (phantom) . unwrap () . append (
    mk_indefinitive_viewnode (id ("A"), src (), "A" . to_string (),
                              ParentIs::Affected) ) . id ();
  let dead : NodeId = vf . get_mut (root) . unwrap () . append (
    ViewNode { focused: false, folded: false, body_folded: false,
               kind: ViewNodeKind::DeadScaffold } ) . id ();
  let under_dead : NodeId = vf . get_mut (dead) . unwrap () . append (
    mk_indefinitive_viewnode (id ("B"), src (), "B" . to_string (),
                              ParentIs::Affected) ) . id ();
  let col : NodeId = vf . get_mut (root) . unwrap () . append (
    ViewNode { focused: false, folded: false, body_folded: false,
               kind: ViewNodeKind::PartnerCol (RoleCol::Subscribee) } ) . id ();
  let member : NodeId = vf . get_mut (col) . unwrap () . append (
    mk_indefinitive_viewnode (id ("C"), src (), "C" . to_string (),
                              ParentIs::Affected) ) . id ();

  mark_orphans_under_dead_parents_independent (&mut vf);

  assert_eq! (parentIs_if_normal (&vf, under_phantom), ParentIs::Independent,
    "Affected child under a Diff phantom must demote to Independent");
  assert_eq! (parentIs_if_normal (&vf, under_dead), ParentIs::Independent,
    "Affected child under a DeadScaffold must demote to Independent");
  assert_eq! (parentIs_if_normal (&vf, member), ParentIs::Affected,
    "Affected member under a PartnerCol must stay Affected (legitimate membership)");
}

#[test]
fn containerof_via_merged_extra_id_preserved () {
  // The merge scenario. Before merge: P's pid was "P-old". Then
  // P-old got merged into P (P-old is now an extra_id of P).
  // C's contains list still has "P-old" (disk unchanged). The
  // view's parent is still titled/labelled as P (canonical).
  // Both sides must resolve through pid_of — C still contains
  // P in spirit.
  let graph : InRustGraph = graph_with (vec! [
    mk_node ("P", &["P-old"], &[],        &[]),
    mk_node ("C", &[],        &["P-old"], &[]),
  ]);
  let mut viewforest : Tree<ViewNode> = Tree::new (viewforest_root_viewnode ());
  let root : NodeId = viewforest . root () . id ();
  let p_id : NodeId = viewforest . get_mut (root) . unwrap () . append (
    mk_indefinitive_viewnode (id ("P"), src (), "P" . to_string (),
                              ParentIs::Affected) ) . id ();
  let c_id : NodeId = viewforest . get_mut (p_id) . unwrap () . append (
    mk_indefinitive_viewnode_with_birth (
      id ("C"), src (), "C" . to_string (),
      ParentIs::Independent, Birth::ContainsParent) ) . id ();

  validate_parentIs_relationships (&mut viewforest, &graph);

  assert_eq! (birth_if_normal (&viewforest, c_id), Birth::ContainsParent,
    "Extra_id-aliased parent pid should still satisfy the claim");
}

#[test]
fn containerof_view_parent_is_acquiree_preserved () {
  // Mirror of the prior test from the parent side: view tree has
  // parent_id = "P-old" (the pre-merge pid), which is now an
  // extra_id of "P". C's contains has "P" directly. Resolve both
  // sides → both map to P.
  let graph : InRustGraph = graph_with (vec! [
    mk_node ("P", &["P-old"], &[],    &[]),
    mk_node ("C", &[],        &["P"], &[]),
  ]);
  let mut viewforest : Tree<ViewNode> = Tree::new (viewforest_root_viewnode ());
  let root : NodeId = viewforest . root () . id ();
  let p_id : NodeId = viewforest . get_mut (root) . unwrap () . append (
    mk_indefinitive_viewnode (id ("P-old"), src (), "P" . to_string (),
                              ParentIs::Affected) ) . id ();
  let c_id : NodeId = viewforest . get_mut (p_id) . unwrap () . append (
    mk_indefinitive_viewnode_with_birth (
      id ("C"), src (), "C" . to_string (),
      ParentIs::Independent, Birth::ContainsParent) ) . id ();

  validate_parentIs_relationships (&mut viewforest, &graph);

  assert_eq! (birth_if_normal (&viewforest, c_id), Birth::ContainsParent,
    "When view parent id is the acquiree pid, the claim should \
     still hold via pid_of resolution on the parent side");
}

#[test]
fn contentof_indefinitive_parent_false_claim_flipped () {
  // Parent is indefinitive; its contains list does NOT include C.
  // So C's Container claim is false.
  let graph : InRustGraph = graph_with (vec! [
    mk_node ("P", &[], &[], &[]),   // P.contains empty
    mk_node ("C", &[], &[], &[]),
  ]);
  let mut viewforest : Tree<ViewNode> = Tree::new (viewforest_root_viewnode ());
  let root : NodeId = viewforest . root () . id ();
  let p_id : NodeId = viewforest . get_mut (root) . unwrap () . append (
    mk_indefinitive_viewnode (id ("P"), src (), "P" . to_string (),
                              ParentIs::Affected) ) . id ();
  let c_id : NodeId = viewforest . get_mut (p_id) . unwrap () . append (
    mk_indefinitive_viewnode (id ("C"), src (), "C" . to_string (),
                              ParentIs::Affected) ) . id ();

  validate_parentIs_relationships (&mut viewforest, &graph);

  assert_eq! (parentIs_if_normal (&viewforest, c_id), ParentIs::Independent);
}

#[test]
fn contentof_definitive_parent_skipped () {
  // Parent is DEFINITIVE; the check is skipped regardless of whether
  // the graph agrees (save just redefined P's contains to match
  // the buffer, so asking the graph would be circular). Claim preserved.
  let graph : InRustGraph = graph_with (vec! [
    mk_node ("P", &[], &[], &[]),  // P.contains empty in the in-Rust graph
    mk_node ("C", &[], &[], &[]),
  ]);
  let mut viewforest : Tree<ViewNode> = Tree::new (viewforest_root_viewnode ());
  let root : NodeId = viewforest . root () . id ();
  let p_id : NodeId = viewforest . get_mut (root) . unwrap () . append (
    mk_definitive_viewnode (id ("P"), src (),
                            "P" . to_string (), None) ) . id ();
  let c_id : NodeId = viewforest . get_mut (p_id) . unwrap () . append (
    mk_indefinitive_viewnode (id ("C"), src (), "C" . to_string (),
                              ParentIs::Affected) ) . id ();

  validate_parentIs_relationships (&mut viewforest, &graph);

  assert_eq! (parentIs_if_normal (&viewforest, c_id), ParentIs::Affected,
    "Definitive parent: Container claim is never flipped here");
}

#[test]
fn independent_always_preserved () {
  // Independent parentIs is never touched.
  let graph : InRustGraph = graph_with (vec! [
    mk_node ("P", &[], &[], &[]),
    mk_node ("C", &[], &[], &[]),
  ]);
  let mut viewforest : Tree<ViewNode> = Tree::new (viewforest_root_viewnode ());
  let root : NodeId = viewforest . root () . id ();
  let p_id : NodeId = viewforest . get_mut (root) . unwrap () . append (
    mk_indefinitive_viewnode (id ("P"), src (), "P" . to_string (),
                              ParentIs::Affected) ) . id ();
  let c_id : NodeId = viewforest . get_mut (p_id) . unwrap () . append (
    mk_indefinitive_viewnode (id ("C"), src (), "C" . to_string (),
                              ParentIs::Independent) ) . id ();

  validate_parentIs_relationships (&mut viewforest, &graph);

  assert_eq! (parentIs_if_normal (&viewforest, c_id), ParentIs::Independent);
}

#[test]
fn absent_under_visible_parent_becomes_container () {
  let graph : InRustGraph = graph_with (vec! [
    mk_node ("P", &[], &[], &["C"]),
    mk_node ("C", &[], &[], &[]),
  ]);
  let mut viewforest : Tree<ViewNode> = Tree::new (viewforest_root_viewnode ());
  let root : NodeId = viewforest . root () . id ();
  let p_id : NodeId = viewforest . get_mut (root) . unwrap () . append (
    mk_indefinitive_viewnode (id ("P"), src (), "P" . to_string (),
                              ParentIs::Affected) ) . id ();
  let c_id : NodeId = viewforest . get_mut (p_id) . unwrap () . append (
    mk_indefinitive_viewnode (id ("C"), src (), "C" . to_string (),
                              ParentIs::Absent) ) . id ();

  validate_parentIs_relationships (&mut viewforest, &graph);

  assert_eq! (parentIs_if_normal (&viewforest, c_id), ParentIs::Affected);
}

// ===== 3x2 matrix: moving a {content, container, linksToParent}
// child of a NEW parent where the relationship to the new
// parent does / doesn't hold. =====================================
//
// Each test sets up:
//   - p_old: the child's original parent (the relationship it
//     was originally marked with).
//   - p_new: the new parent it has been moved under.
//   - The view viewforest has the child as a child of p_new.
//   - The graph sometimes has a relationship from the child to
//     p_new ("holds"), and sometimes doesn't ("no longer holds").

// ---- content ------------------------------------------------

#[test]
fn moved_containerof_relationship_holds_preserved () {
  // Child C once had birth=containsParent under p_old (C.contains had
  // p_old). User moved C under p_new. Test case: C's contains
  // ALSO includes p_new → the content claim still holds for
  // the new parent.
  let graph : InRustGraph = graph_with (vec! [
    mk_node ("p_old", &[], &[],                   &[]),
    mk_node ("p_new", &[], &[],                   &[]),
    mk_node ("C",     &[], &["p_old", "p_new"],   &[]),
  ]);
  let mut viewforest : Tree<ViewNode> = Tree::new (viewforest_root_viewnode ());
  let root : NodeId = viewforest . root () . id ();
  let p_new_id : NodeId = viewforest . get_mut (root) . unwrap () . append (
    mk_indefinitive_viewnode (id ("p_new"), src (), "p_new" . to_string (),
                              ParentIs::Affected) ) . id ();
  let c_id : NodeId = viewforest . get_mut (p_new_id) . unwrap () . append (
    mk_indefinitive_viewnode_with_birth (
      id ("C"), src (), "C" . to_string (),
      ParentIs::Independent, Birth::ContainsParent) ) . id ();

  validate_parentIs_relationships (&mut viewforest, &graph);

  assert_eq! (birth_if_normal (&viewforest, c_id), Birth::ContainsParent,
    "Moved content child still contains its new parent — keep");
}

#[test]
fn moved_containerof_relationship_broken_flipped () {
  // Same setup but C's contains only has p_old, not p_new. Moving
  // C under p_new broke the claim.
  let graph : InRustGraph = graph_with (vec! [
    mk_node ("p_old", &[], &[],        &[]),
    mk_node ("p_new", &[], &[],        &[]),
    mk_node ("C",     &[], &["p_old"], &[]),
  ]);
  let mut viewforest : Tree<ViewNode> = Tree::new (viewforest_root_viewnode ());
  let root : NodeId = viewforest . root () . id ();
  let p_new_id : NodeId = viewforest . get_mut (root) . unwrap () . append (
    mk_indefinitive_viewnode (id ("p_new"), src (), "p_new" . to_string (),
                              ParentIs::Affected) ) . id ();
  let c_id : NodeId = viewforest . get_mut (p_new_id) . unwrap () . append (
    mk_indefinitive_viewnode_with_birth (
      id ("C"), src (), "C" . to_string (),
      ParentIs::Independent, Birth::ContainsParent) ) . id ();

  validate_parentIs_relationships (&mut viewforest, &graph);

  assert_eq! (parentIs_if_normal (&viewforest, c_id), ParentIs::Independent,
    "Moved content child no longer contains new parent — flip");
}

// ---- container (indefinitive parent on both sides) -------------

#[test]
fn moved_contentof_indef_parent_relationship_holds_preserved () {
  // Child C was content of an indefinitive p_old. Moved under
  // indefinitive p_new. Test case: p_new.contains ALSO includes C.
  let graph : InRustGraph = graph_with (vec! [
    mk_node ("p_old", &[], &["C"], &[]),
    mk_node ("p_new", &[], &["C"], &[]), // p_new really contains C
    mk_node ("C",     &[], &[],    &[]),
  ]);
  let mut viewforest : Tree<ViewNode> = Tree::new (viewforest_root_viewnode ());
  let root : NodeId = viewforest . root () . id ();
  let p_new_id : NodeId = viewforest . get_mut (root) . unwrap () . append (
    mk_indefinitive_viewnode (id ("p_new"), src (), "p_new" . to_string (),
                              ParentIs::Affected) ) . id ();
  let c_id : NodeId = viewforest . get_mut (p_new_id) . unwrap () . append (
    mk_indefinitive_viewnode (id ("C"), src (), "C" . to_string (),
                              ParentIs::Affected) ) . id ();

  validate_parentIs_relationships (&mut viewforest, &graph);

  assert_eq! (parentIs_if_normal (&viewforest, c_id), ParentIs::Affected,
    "Moved container: indef p_new actually contains C — keep");
}

#[test]
fn moved_contentof_indef_parent_relationship_broken_flipped () {
  // Same setup but p_new.contains does NOT include C.
  let graph : InRustGraph = graph_with (vec! [
    mk_node ("p_old", &[], &["C"], &[]),
    mk_node ("p_new", &[], &[],    &[]), // p_new doesn't contain C
    mk_node ("C",     &[], &[],    &[]),
  ]);
  let mut viewforest : Tree<ViewNode> = Tree::new (viewforest_root_viewnode ());
  let root : NodeId = viewforest . root () . id ();
  let p_new_id : NodeId = viewforest . get_mut (root) . unwrap () . append (
    mk_indefinitive_viewnode (id ("p_new"), src (), "p_new" . to_string (),
                              ParentIs::Affected) ) . id ();
  let c_id : NodeId = viewforest . get_mut (p_new_id) . unwrap () . append (
    mk_indefinitive_viewnode (id ("C"), src (), "C" . to_string (),
                              ParentIs::Affected) ) . id ();

  validate_parentIs_relationships (&mut viewforest, &graph);

  assert_eq! (parentIs_if_normal (&viewforest, c_id), ParentIs::Independent,
    "Moved container: indef p_new doesn't contain C — flip");
}

// ---- linksToParent ---------------------------------------------------

#[test]
fn moved_linksto_relationship_holds_preserved () {
  // C's body linked to both p_old and p_new. User moved C under
  // p_new; the linksToParent claim holds.
  let graph : InRustGraph = graph_with (vec! [
    mk_node ("p_old", &[], &[], &[]),
    mk_node ("p_new", &[], &[], &[]),
    mk_node ("C",     &[], &[], &["p_old", "p_new"]),
  ]);
  let mut viewforest : Tree<ViewNode> = Tree::new (viewforest_root_viewnode ());
  let root : NodeId = viewforest . root () . id ();
  let p_new_id : NodeId = viewforest . get_mut (root) . unwrap () . append (
    mk_indefinitive_viewnode (id ("p_new"), src (), "p_new" . to_string (),
                              ParentIs::Affected) ) . id ();
  let c_id : NodeId = viewforest . get_mut (p_new_id) . unwrap () . append (
    mk_indefinitive_viewnode_with_birth (
      id ("C"), src (), "C" . to_string (),
      ParentIs::Independent, Birth::LinksToParent) ) . id ();

  validate_parentIs_relationships (&mut viewforest, &graph);

  assert_eq! (birth_if_normal (&viewforest, c_id), Birth::LinksToParent,
    "Moved linksToParent: C still links to p_new — keep");
}

#[test]
fn moved_linksto_relationship_broken_flipped () {
  // C's body linked only to p_old, not p_new.
  let graph : InRustGraph = graph_with (vec! [
    mk_node ("p_old", &[], &[], &[]),
    mk_node ("p_new", &[], &[], &[]),
    mk_node ("C",     &[], &[], &["p_old"]),
  ]);
  let mut viewforest : Tree<ViewNode> = Tree::new (viewforest_root_viewnode ());
  let root : NodeId = viewforest . root () . id ();
  let p_new_id : NodeId = viewforest . get_mut (root) . unwrap () . append (
    mk_indefinitive_viewnode (id ("p_new"), src (), "p_new" . to_string (),
                              ParentIs::Affected) ) . id ();
  let c_id : NodeId = viewforest . get_mut (p_new_id) . unwrap () . append (
    mk_indefinitive_viewnode_with_birth (
      id ("C"), src (), "C" . to_string (),
      ParentIs::Independent, Birth::LinksToParent) ) . id ();

  validate_parentIs_relationships (&mut viewforest, &graph);

  assert_eq! (parentIs_if_normal (&viewforest, c_id), ParentIs::Independent,
    "Moved linksToParent: C doesn't link to p_new — flip");
}
