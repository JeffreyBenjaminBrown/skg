use crate::types::misc::{MSV, rel_partners_at_relRepo};
use crate::types::viewnode::mk_writeProtected_viewnode;
use crate::dbs::in_rust_graph::add_to_inverse_indexes;
use crate::types::viewnode::{viewforest_root_viewnode, viewnode_of};

use super::*;

fn src () -> SkgrepoName { SkgrepoName::from ("main") }
fn skgid  (s: &str) -> ID { ID ( s . to_string () ) }

/// Build a GraphnodeInRust directly (no disk I/O) for fixture graphs.
fn mk_node (
  pid          : &str,
  extra_ids    : &[&str],
  contains     : &[&str],
  linksTo      : &[&str],
) -> GraphnodeInRust {
  GraphnodeInRust {
    pid:          skgid (pid),
    home_skgrepo:       src (),
    extra_ids:    extra_ids . iter () . map ( |s| skgid (s) ) . collect (),
    title:        pid . to_string (),
    overPrivateText_telescope: false,
    aliases:      MSV::Unspecified,
    body:         None,
    contains:     rel_partners_at_relRepo (
      & src (),
      contains . iter () . map ( |s| skgid (s) ) . collect () ),
    subscribesTo:                MSV::Unspecified,
    hidesFromSubs: MSV::Unspecified,
    overrides:            MSV::Unspecified,
    flags:         Vec::new (),
    linksTo: linksTo . iter () . map ( |s| skgid (s) ) . collect (),
  } }

/// Insert nodes into a fresh InRustGraph.
fn graph_with (nodes: Vec<GraphnodeInRust>) -> InRustGraph {
  let mut g : InRustGraph = InRustGraph::new ();
  // Populate extra_id_to_pid first so recorderward relmaps resolve right.
  for n in &nodes {
    for eid in &n . extra_ids {
      g . extra_id_to_pid . insert (eid . clone (), n . pid . clone ()); } }
  for n in nodes {
    // Build the recorderward relmaps too (contained_by, mentioners_of, ...),
    // so the graph is complete.
    add_to_inverse_indexes (&mut g, &n);
    g . nodes . insert (n . pid . clone (), n); }
  g }

fn membership_if_normal (
  viewforest : &Tree<Viewnode>,
  treeid     : NodeId,
) -> bool {
  match & viewforest . get (treeid) . unwrap () . value () . kind {
    ViewnodeKind::Vognode (Vognode::Unrestricted (t)) => t . claimsMembership,
    _ => panic! ("expected UnrestrictedVognode") } }

#[test]
fn orphan_under_dead_parent_made_nonMember_but_folder_member_kept () {
  // §A (Jeff's invariant): a member Unrestricted node under a non-container
  // parent (a tombstone) is made a non-member; a legitimate
  // folder member (member Unrestricted under a PartnerFolder) is left untouched.
  use crate::types::viewnode::{ Folder };
  let mut vf : Tree<Viewnode> = Tree::new (viewforest_root_viewnode ());
  let root : NodeId = vf . root () . id ();
  let dead : NodeId = vf . get_mut (root) . unwrap () . append (
    viewnode_of (ViewnodeKind::Tombstone) ) . id ();
  let under_dead : NodeId = vf . get_mut (dead) . unwrap () . append (
    mk_writeProtected_viewnode (skgid ("B"), src (), "B" . to_string (),
                              true) ) . id ();
  let folder : NodeId = vf . get_mut (root) . unwrap () . append (
    viewnode_of (ViewnodeKind::Folder (Folder::Subscribee)) ) . id ();
  let member : NodeId = vf . get_mut (folder) . unwrap () . append (
    mk_writeProtected_viewnode (skgid ("C"), src (), "C" . to_string (),
                              true) ) . id ();

  mark_orphans_under_dead_parents_false (&mut vf);

  assert_eq! (membership_if_normal (&vf, under_dead), false,
    "A member child under a tombstone must be made a non-member");
  assert_eq! (membership_if_normal (&vf, member), true,
    "A member under a PartnerFolder must stay a member (legitimate membership)");
}

#[test]
fn a_members_claim_holds_through_a_merged_extra_id () {
  // The merge scenario. C's pid was "C-old" before it was merged into
  // C ("C-old" is now an extra_id of C). The write-protected P's
  // contains list still has "C-old" (disk unchanged). Both sides
  // resolve through pid_of, so P still contains C in spirit.
  let graph : InRustGraph = graph_with (vec! [
    mk_node ("P", &[],        &["C-old"], &[]),
    mk_node ("C", &["C-old"], &[],        &[]),
  ]);
  let mut viewforest : Tree<Viewnode> = Tree::new (viewforest_root_viewnode ());
  let root : NodeId = viewforest . root () . id ();
  let p_treeid : NodeId = viewforest . get_mut (root) . unwrap () . append (
    mk_writeProtected_viewnode (skgid ("P"), src (), "P" . to_string (),
                              true) ) . id ();
  let c_treeid : NodeId = viewforest . get_mut (p_treeid) . unwrap () . append (
    mk_writeProtected_viewnode (skgid ("C"), src (), "C" . to_string (),
                              true) ) . id ();

  validate_membership_claims (&mut viewforest, &graph);

  assert_eq! (membership_if_normal (&viewforest, c_treeid), true,
    "An extra_id-aliased member should still satisfy the claim");
}

#[test]
fn contentof_writeProtected_parent_false_claim_flipped () {
  // Parent is write-protected; its contains list does not include C.
  // So C's Container claim is false.
  let graph : InRustGraph = graph_with (vec! [
    mk_node ("P", &[], &[], &[]),   // P.contains empty
    mk_node ("C", &[], &[], &[]),
  ]);
  let mut viewforest : Tree<Viewnode> = Tree::new (viewforest_root_viewnode ());
  let root : NodeId = viewforest . root () . id ();
  let p_skgid : NodeId = viewforest . get_mut (root) . unwrap () . append (
    mk_writeProtected_viewnode (skgid ("P"), src (), "P" . to_string (),
                              true) ) . id ();
  let c_skgid : NodeId = viewforest . get_mut (p_skgid) . unwrap () . append (
    mk_writeProtected_viewnode (skgid ("C"), src (), "C" . to_string (),
                              true) ) . id ();

  validate_membership_claims (&mut viewforest, &graph);

  assert_eq! (membership_if_normal (&viewforest, c_skgid), false);
}

#[test]
fn contentof_editable_parent_skipped () {
  // Parent is editable; the check is skipped regardless of whether
  // the graph agrees (save just redefined P's contains to match
  // the buffer, so asking the graph would be circular). Claim preserved.
  let graph : InRustGraph = graph_with (vec! [
    mk_node ("P", &[], &[], &[]),  // P.contains empty in the in-Rust graph
    mk_node ("C", &[], &[], &[]),
  ]);
  let mut viewforest : Tree<Viewnode> = Tree::new (viewforest_root_viewnode ());
  let root : NodeId = viewforest . root () . id ();
  let p_skgid : NodeId = viewforest . get_mut (root) . unwrap () . append (
    mk_editable_viewnode (skgid ("P"), src (),
                            "P" . to_string (), None) ) . id ();
  let c_skgid : NodeId = viewforest . get_mut (p_skgid) . unwrap () . append (
    mk_writeProtected_viewnode (skgid ("C"), src (), "C" . to_string (),
                              true) ) . id ();

  validate_membership_claims (&mut viewforest, &graph);

  assert_eq! (membership_if_normal (&viewforest, c_skgid), true,
    "Editable parent: Container claim is never flipped here");
}

#[test]
fn independent_always_preserved () {
  // a non-member is never touched.
  let graph : InRustGraph = graph_with (vec! [
    mk_node ("P", &[], &[], &[]),
    mk_node ("C", &[], &[], &[]),
  ]);
  let mut viewforest : Tree<Viewnode> = Tree::new (viewforest_root_viewnode ());
  let root : NodeId = viewforest . root () . id ();
  let p_skgid : NodeId = viewforest . get_mut (root) . unwrap () . append (
    mk_writeProtected_viewnode (skgid ("P"), src (), "P" . to_string (),
                              true) ) . id ();
  let c_skgid : NodeId = viewforest . get_mut (p_skgid) . unwrap () . append (
    mk_writeProtected_viewnode (skgid ("C"), src (), "C" . to_string (),
                              false) ) . id ();

  validate_membership_claims (&mut viewforest, &graph);

  assert_eq! (membership_if_normal (&viewforest, c_skgid), false);
}

// ===== Moving a member under a new write-protected parent, where
// its containment by the new parent does / doesn't hold. ==========
//
// Each test sets up:
//   - p_old: the child's original parent.
//   - p_new: the new parent it has been moved under.
//   - The view viewforest has the child as a child of p_new.
//   - The graph sometimes has p_new containing the child
//     ("holds"), and sometimes doesn't ("no longer holds").

#[test]
fn moved_contentof_writeProtected_parent_relationship_holds_preserved () {
  // Child C was content of a write-protected p_old. Moved under
  // write-protected p_new. Test case: p_new.contains also includes C.
  let graph : InRustGraph = graph_with (vec! [
    mk_node ("p_old", &[], &["C"], &[]),
    mk_node ("p_new", &[], &["C"], &[]), // p_new really contains C
    mk_node ("C",     &[], &[],    &[]),
  ]);
  let mut viewforest : Tree<Viewnode> = Tree::new (viewforest_root_viewnode ());
  let root : NodeId = viewforest . root () . id ();
  let p_new_skgid : NodeId = viewforest . get_mut (root) . unwrap () . append (
    mk_writeProtected_viewnode (skgid ("p_new"), src (), "p_new" . to_string (),
                              true) ) . id ();
  let c_skgid : NodeId = viewforest . get_mut (p_new_skgid) . unwrap () . append (
    mk_writeProtected_viewnode (skgid ("C"), src (), "C" . to_string (),
                              true) ) . id ();

  validate_membership_claims (&mut viewforest, &graph);

  assert_eq! (membership_if_normal (&viewforest, c_skgid), true,
    "Moved container: writeProtected p_new actually contains C — keep");
}

#[test]
fn moved_contentof_writeProtected_parent_relationship_broken_flipped () {
  // Same setup but p_new.contains does not include C.
  let graph : InRustGraph = graph_with (vec! [
    mk_node ("p_old", &[], &["C"], &[]),
    mk_node ("p_new", &[], &[],    &[]), // p_new doesn't contain C
    mk_node ("C",     &[], &[],    &[]),
  ]);
  let mut viewforest : Tree<Viewnode> = Tree::new (viewforest_root_viewnode ());
  let root : NodeId = viewforest . root () . id ();
  let p_new_skgid : NodeId = viewforest . get_mut (root) . unwrap () . append (
    mk_writeProtected_viewnode (skgid ("p_new"), src (), "p_new" . to_string (),
                              true) ) . id ();
  let c_skgid : NodeId = viewforest . get_mut (p_new_skgid) . unwrap () . append (
    mk_writeProtected_viewnode (skgid ("C"), src (), "C" . to_string (),
                              true) ) . id ();

  validate_membership_claims (&mut viewforest, &graph);

  assert_eq! (membership_if_normal (&viewforest, c_skgid), false,
    "Moved container: writeProtected p_new doesn't contain C — flip");
}
