use super::*;
use crate::skgrepo_sets::SkgrepoSetName;
use crate::types::misc::{ID, SkgrepoName};
use crate::types::viewnode::{
  mk_writeProtected_viewnode, mk_editable_viewnode,
  viewforest_root_viewnode, viewnode_of };

use std::collections::BTreeSet;

fn restriction_public () -> SkgrepoRestriction {
  SkgrepoRestriction {
    name    : SkgrepoSetName ("public" . to_string ()),
    skgrepos : BTreeSet::from ([ SkgrepoName::from ("public") ]) }}

fn def (skgid : &str, skgrepo : &str) -> Viewnode {
  mk_editable_viewnode (
    ID::from (skgid), SkgrepoName::from (skgrepo),
    skgid . to_string (), None ) }

fn writeProtected (skgid : &str, skgrepo : &str) -> Viewnode {
  mk_writeProtected_viewnode (
    ID::from (skgid), SkgrepoName::from (skgrepo),
    skgid . to_string (), true ) }

fn folder (kind : Folder) -> Viewnode {
  viewnode_of (ViewnodeKind::Folder (kind)) }

// A now-restricted childless branch disappears; a now-restricted node
// with an unrestricted child is retained as a RestrictedVognode (converted,
// not pruned); the unrestricted child survives.
#[test]
fn conversion_and_retention () {
  let mut t : Tree<Viewnode> = Tree::new (viewforest_root_viewnode ());
  let root : NodeId = t . root () . id ();
  let parent : NodeId = t . get_mut (root) . unwrap ()
    . append (def ("parent", "public")) . id ();
  let gone : NodeId = t . get_mut (parent) . unwrap ()
    . append (def ("gone", "private")) . id ();
  let kept : NodeId = t . get_mut (parent) . unwrap ()
    . append (def ("kept", "private")) . id ();
  t . get_mut (kept) . unwrap ()
    . append (def ("survivor", "public"));
  convert_and_prune_for_skgrepo_switch (
    &mut t, &restriction_public ()) . unwrap ();
  assert! ( t . get (gone) . map ( |n| n . parent () . is_none () )
              . unwrap_or (true),
    "a childless now-restricted node is pruned" );
  assert! ( matches! (
      &t . get (kept) . unwrap () . value () . kind,
      ViewnodeKind::Vognode (Vognode::Restricted (_)) ),
    "a now-restricted node with an unrestricted child is retained as a RestrictedVognode" );
  assert! (
    t . get (kept) . unwrap () . children () . count () == 1,
    "the unrestricted child survives under the retained node" ); }

// Only what the restriction hides is pruned: a now-restricted
// childless partner goes, and a folder that loses its last member with
// it; an unrestricted write-protected partner stays, and so does its
// folder, and a folder that was empty before the switch.
#[test]
fn partners_and_folders_prune () {
  let mut t : Tree<Viewnode> = Tree::new (viewforest_root_viewnode ());
  let root : NodeId = t . root () . id ();
  let recorder : NodeId = t . get_mut (root) . unwrap ()
    . append (def ("recorder", "public")) . id ();
  let kept_folder : NodeId = t . get_mut (recorder) . unwrap ()
    . append (folder (Folder::Subscriber)) . id ();
  let kept_member : NodeId = t . get_mut (kept_folder) . unwrap ()
    . append (writeProtected ("unrestricted-member", "public")) . id ();
  let gone_member : NodeId = t . get_mut (kept_folder) . unwrap ()
    . append (writeProtected ("restricted-member", "private")) . id ();
  let emptied_folder : NodeId = t . get_mut (recorder) . unwrap ()
    . append (folder (Folder::Container)) . id ();
  t . get_mut (emptied_folder) . unwrap ()
    . append (writeProtected ("restricted-container", "private"));
  let empty_folder : NodeId = t . get_mut (recorder) . unwrap ()
    . append (folder (Folder::Subscribee)) . id ();
  convert_and_prune_for_skgrepo_switch (
    &mut t, &restriction_public ()) . unwrap ();
  let is_attached = |node : NodeId| -> bool {
    t . get (node) . map ( |n| n . parent () . is_some () ) . unwrap_or (false) };
  assert! ( is_attached (kept_member),
    "an unrestricted write-protected partner stays" );
  assert! ( ! is_attached (gone_member),
    "a childless now-restricted partner is pruned" );
  assert! ( is_attached (kept_folder),
    "a folder holding an unrestricted partner stays" );
  assert! ( ! is_attached (emptied_folder),
    "a folder emptied by the prune is itself pruned" );
  assert! ( is_attached (empty_folder),
    "a folder that was empty before the switch stays" ); }

// Pruning a focused subtree transfers focus to the surviving parent.
#[test]
fn focus_transfers_to_surviving_parent () {
  let mut t : Tree<Viewnode> = Tree::new (viewforest_root_viewnode ());
  let root : NodeId = t . root () . id ();
  let parent : NodeId = t . get_mut (root) . unwrap ()
    . append (def ("parent", "public")) . id ();
  let gone : NodeId = t . get_mut (parent) . unwrap ()
    . append (def ("gone", "private")) . id ();
  t . get_mut (gone) . unwrap () . value () . foldAndFocus . focused = true;
  convert_and_prune_for_skgrepo_switch (
    &mut t, &restriction_public ()) . unwrap ();
  assert! ( t . get (parent) . unwrap () . value () . foldAndFocus . focused,
    "focus transfers from a pruned subtree to its surviving parent" ); }
