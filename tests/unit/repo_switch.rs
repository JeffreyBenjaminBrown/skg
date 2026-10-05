use super::*;
use crate::skgrepo_sets::SkgRepoSetName;
use crate::types::misc::{ID, SkgRepoName};
use crate::types::viewnode::{
  mk_writeProtected_viewnode, mk_editable_viewnode,
  viewforest_root_viewnode, AffectsParent };

use std::collections::BTreeSet;

fn restriction_public () -> SkgrepoRestriction {
  SkgrepoRestriction {
    name    : SkgRepoSetName ("public" . to_string ()),
    skgrepos : BTreeSet::from ([ SkgRepoName::from ("public") ]) }}

fn def (skgid : &str, skgrepo : &str) -> Viewnode {
  mk_editable_viewnode (
    ID::from (skgid), SkgRepoName::from (skgrepo),
    skgid . to_string (), None ) }

fn writeProtected (skgid : &str, skgrepo : &str) -> Viewnode {
  mk_writeProtected_viewnode (
    ID::from (skgid), SkgRepoName::from (skgrepo),
    skgid . to_string (), AffectsParent::True ) }

fn folder (kind : PartnerFolder) -> Viewnode {
  Viewnode {
    focused     : false,
    folded      : false,
    body_folded : false,
    kind        : ViewnodeKind::PartnerFolder (kind) } }

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

// All write-protected leaf partners are pruned, unrestricted and restricted
// alike, and the emptied folder goes with them; an editable partner
// survives.
#[test]
fn partners_and_folders_prune () {
  let mut t : Tree<Viewnode> = Tree::new (viewforest_root_viewnode ());
  let root : NodeId = t . root () . id ();
  let recorder : NodeId = t . get_mut (root) . unwrap ()
    . append (def ("recorder", "public")) . id ();
  let emptied_folder : NodeId = t . get_mut (recorder) . unwrap ()
    . append (folder (PartnerFolder::Subscriber)) . id ();
  t . get_mut (emptied_folder) . unwrap ()
    . append (writeProtected ("unrestricted-member", "public"));
  t . get_mut (emptied_folder) . unwrap ()
    . append (writeProtected ("restricted-member", "private"));
  let surviving_folder : NodeId = t . get_mut (recorder) . unwrap ()
    . append (folder (PartnerFolder::Subscribee)) . id ();
  t . get_mut (surviving_folder) . unwrap ()
    . append (def ("editable-partner", "public"));
  convert_and_prune_for_skgrepo_switch (
    &mut t, &restriction_public ()) . unwrap ();
  assert! ( t . get (emptied_folder)
              . map ( |n| n . parent () . is_none () )
              . unwrap_or (true),
    "a folder emptied by partner pruning is itself pruned" );
  assert! ( t . get (surviving_folder)
              . map ( |n| n . parent () . is_some () )
              . unwrap_or (false),
    "a folder holding a editable partner survives" ); }

// Pruning a focused subtree transfers focus to the surviving parent.
#[test]
fn focus_transfers_to_surviving_parent () {
  let mut t : Tree<Viewnode> = Tree::new (viewforest_root_viewnode ());
  let root : NodeId = t . root () . id ();
  let parent : NodeId = t . get_mut (root) . unwrap ()
    . append (def ("parent", "public")) . id ();
  let gone : NodeId = t . get_mut (parent) . unwrap ()
    . append (def ("gone", "private")) . id ();
  t . get_mut (gone) . unwrap () . value () . focused = true;
  convert_and_prune_for_skgrepo_switch (
    &mut t, &restriction_public ()) . unwrap ();
  assert! ( t . get (parent) . unwrap () . value () . focused,
    "focus transfers from a pruned subtree to its surviving parent" ); }
