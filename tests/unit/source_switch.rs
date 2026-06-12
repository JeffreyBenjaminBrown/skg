use super::*;
use crate::source_sets::SourceSetName;
use crate::types::misc::{ID, SourceName};
use crate::types::viewnode::{
  mk_indefinitive_viewnode, mk_definitive_viewnode,
  viewforest_root_viewnode, ParentIs };

use std::collections::BTreeSet;

fn active_public () -> ActiveSourceSet {
  ActiveSourceSet {
    name    : SourceSetName ("public" . to_string ()),
    sources : BTreeSet::from ([ SourceName::from ("public") ]) }}

fn def (id : &str, source : &str) -> ViewNode {
  mk_definitive_viewnode (
    ID::from (id), SourceName::from (source),
    id . to_string (), None ) }

fn indef (id : &str, source : &str) -> ViewNode {
  mk_indefinitive_viewnode (
    ID::from (id), SourceName::from (source),
    id . to_string (), ParentIs::Affected ) }

fn col (kind : PartnerCol) -> ViewNode {
  ViewNode {
    focused     : false,
    folded      : false,
    body_folded : false,
    kind        : ViewNodeKind::PartnerCol (kind) } }

// A now-inactive childless branch disappears; a now-inactive node
// with an active child is retained as an InactiveNode (converted,
// not pruned); the active child survives.
#[test]
fn conversion_and_retention () {
  let mut t : Tree<ViewNode> = Tree::new (viewforest_root_viewnode ());
  let root : NodeId = t . root () . id ();
  let parent : NodeId = t . get_mut (root) . unwrap ()
    . append (def ("parent", "public")) . id ();
  let gone : NodeId = t . get_mut (parent) . unwrap ()
    . append (def ("gone", "private")) . id ();
  let kept : NodeId = t . get_mut (parent) . unwrap ()
    . append (def ("kept", "private")) . id ();
  t . get_mut (kept) . unwrap ()
    . append (def ("survivor", "public"));
  convert_and_prune_for_source_switch (
    &mut t, &active_public ()) . unwrap ();
  assert! ( t . get (gone) . map ( |n| n . parent () . is_none () )
              . unwrap_or (true),
    "a childless now-inactive node is pruned" );
  assert! ( matches! (
      &t . get (kept) . unwrap () . value () . kind,
      ViewNodeKind::Vognode (Vognode::Inactive (_)) ),
    "a now-inactive node with an active child is retained as an InactiveNode" );
  assert! (
    t . get (kept) . unwrap () . children () . count () == 1,
    "the active child survives under the retained node" ); }

// All indefinitive leaf partners are pruned, active and inactive
// alike, and the emptied col goes with them; a definitive partner
// survives.
#[test]
fn partners_and_cols_prune () {
  let mut t : Tree<ViewNode> = Tree::new (viewforest_root_viewnode ());
  let root : NodeId = t . root () . id ();
  let owner : NodeId = t . get_mut (root) . unwrap ()
    . append (def ("owner", "public")) . id ();
  let emptied_col : NodeId = t . get_mut (owner) . unwrap ()
    . append (col (PartnerCol::Subscriber)) . id ();
  t . get_mut (emptied_col) . unwrap ()
    . append (indef ("active-member", "public"));
  t . get_mut (emptied_col) . unwrap ()
    . append (indef ("inactive-member", "private"));
  let surviving_col : NodeId = t . get_mut (owner) . unwrap ()
    . append (col (PartnerCol::Subscribee)) . id ();
  t . get_mut (surviving_col) . unwrap ()
    . append (def ("definitive-partner", "public"));
  convert_and_prune_for_source_switch (
    &mut t, &active_public ()) . unwrap ();
  assert! ( t . get (emptied_col)
              . map ( |n| n . parent () . is_none () )
              . unwrap_or (true),
    "a col emptied by partner pruning is itself pruned" );
  assert! ( t . get (surviving_col)
              . map ( |n| n . parent () . is_some () )
              . unwrap_or (false),
    "a col holding a definitive partner survives" ); }

// Pruning a focused subtree transfers focus to the surviving parent.
#[test]
fn focus_transfers_to_surviving_parent () {
  let mut t : Tree<ViewNode> = Tree::new (viewforest_root_viewnode ());
  let root : NodeId = t . root () . id ();
  let parent : NodeId = t . get_mut (root) . unwrap ()
    . append (def ("parent", "public")) . id ();
  let gone : NodeId = t . get_mut (parent) . unwrap ()
    . append (def ("gone", "private")) . id ();
  t . get_mut (gone) . unwrap () . value () . focused = true;
  convert_and_prune_for_source_switch (
    &mut t, &active_public ()) . unwrap ();
  assert! ( t . get (parent) . unwrap () . value () . focused,
    "focus transfers from a pruned subtree to its surviving parent" ); }
