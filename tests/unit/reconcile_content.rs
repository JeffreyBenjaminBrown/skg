
use super::*;

fn skgrepo_name (s: &str) -> SkgrepoName { SkgrepoName ( s . to_string () ) }
fn skgid          (s: &str) -> ID          { ID ( s . to_string () ) }

fn parent_with_unknown_child (child : &ID) -> (Tree<Viewnode>, NodeId, NodeId) {
  use crate::types::viewnode::{mk_editable_viewnode, mk_unknown_viewnode};
  let mut tree : Tree<Viewnode> =
    Tree::new ( mk_editable_viewnode (
      skgid ("p"), skgrepo_name ("main"), "p" . to_string (), None ) );
  let parent : NodeId = tree . root () . id ();
  let child_treeid : NodeId =
    tree . root_mut () . append ( mk_unknown_viewnode (child . clone ()) ) . id ();
  (tree, parent, child_treeid) }

fn is_tombstone (tree : &Tree<Viewnode>, treeid : NodeId) -> bool {
  matches! ( tree . get (treeid) . unwrap () . value () . kind,
             ViewnodeKind::Tombstone ) }

fn is_unknown (tree : &Tree<Viewnode>, treeid : NodeId) -> bool {
  matches! ( & tree . get (treeid) . unwrap () . value () . kind,
             ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Unknown (_))) ) }

#[test]
fn nonmember_unknown_child_becomes_tombstone () {
  let (mut tree, parent, child) = parent_with_unknown_child (& skgid ("ghost"));
  convert_nonmember_unknown_children_to_tombstones (
    &mut tree, parent, &[ skgid ("kept") ] ) . unwrap ();
  assert! ( is_tombstone (&tree, child),
            "an Unknown no longer in contains should become Tombstone" );
}

#[test]
fn member_unknown_child_is_retained () {
  let (mut tree, parent, child) = parent_with_unknown_child (& skgid ("ghost"));
  convert_nonmember_unknown_children_to_tombstones (
    &mut tree, parent, &[ skgid ("ghost") ] ) . unwrap ();
  assert! ( is_unknown (&tree, child),
            "an Unknown still in contains should be retained" );
}

#[test]
fn same_session_surviving_content_membership_becomes_unknown () {
  use crate::types::viewnode::mk_editable_viewnode;
  let ghost : ID = skgid ("ghost");
  let mut tree : Tree<Viewnode> = Tree::new (mk_editable_viewnode (
    skgid ("parent"), skgrepo_name ("main"), "parent" . to_string (), None ));
  let parent : NodeId = tree . root () . id ();
  let mut restriction : Viewnode = mk_editable_viewnode (
    ghost . clone (), skgrepo_name ("main"), "last seen" . to_string (),
    Some ("last seen body" . to_string ()) );
  restriction . focused = true;
  restriction . org_folded = true;
  let child : NodeId = tree . root_mut () . append (restriction) . id ();
  let mut relRepos : HashMap<ID, SkgrepoName> = HashMap::new ();
  relRepos . insert (ghost . clone (), skgrepo_name ("private"));
  let graph_snap : std::sync::Arc<InRustGraph> =
    std::sync::Arc::new (InRustGraph::new ());

  normalize_relationship_backed_content_unknowns (
    &mut tree, parent, &[ghost . clone ()], &relRepos,
    &skgrepo_name ("main"), &graph_snap, &HashMap::new () ) . unwrap ();

  let rendered = tree . get (child) . unwrap () . value ();
  assert! (rendered . focused && rendered . org_folded,
    "same-session normalization preserves view wrapper state");
  match &rendered . kind {
    ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Unknown (unknown))) => {
      assert_eq! (unknown . skgid, ghost);
      assert_eq! (unknown . relRepo,
                  Some (skgrepo_name ("private")));
      assert_eq! (unknown . relRepo_request, None); },
    other => panic! ("surviving relationship must render Unknown, got {other:?}"), }
}

#[test]
fn same_session_extra_id_membership_becomes_unknown_with_raw_id () {
  use crate::types::viewnode::mk_editable_viewnode;
  let primary : ID = skgid ("deleted-primary");
  let raw_extra : ID = skgid ("surviving-extra-id");
  let mut tree : Tree<Viewnode> = Tree::new (mk_editable_viewnode (
    skgid ("parent"), skgrepo_name ("main"), "parent" . to_string (), None ));
  let parent : NodeId = tree . root () . id ();
  let mut restriction : Viewnode = mk_editable_viewnode (
    primary . clone (), skgrepo_name ("main"), "last seen" . to_string (), None );
  restriction . focused = true;
  let child : NodeId = tree . root_mut () . append (restriction) . id ();
  let mut relRepos : HashMap<ID, SkgrepoName> = HashMap::new ();
  relRepos . insert (raw_extra . clone (), skgrepo_name ("foreign"));
  let graph_snap : std::sync::Arc<InRustGraph> =
    std::sync::Arc::new (InRustGraph::new ());
  let mut deleted_extra_ids : HashMap<ID, HashSet<ID>> = HashMap::new ();
  deleted_extra_ids . insert (
    primary, [raw_extra . clone ()] . into_iter () . collect ());

  normalize_relationship_backed_content_unknowns (
    &mut tree, parent, &[raw_extra . clone ()], &relRepos,
    &skgrepo_name ("main"), &graph_snap, &deleted_extra_ids ) . unwrap ();

  let rendered = tree . get (child) . unwrap () . value ();
  assert! (rendered . focused,
    "extra-id normalization preserves the unrestricted child wrapper state");
  match &rendered . kind {
    ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Unknown (unknown))) => {
      assert_eq! (unknown . skgid, raw_extra,
        "the retained on-disk spelling, not the deleted primary, is rendered");
      assert_eq! (unknown . relRepo, Some (skgrepo_name ("foreign"))); },
    other => panic! ("surviving extra-id relationship must be Unknown, got {other:?}"), }
}

// Regression: a content goal id present only as a
// noClaim child must still get ChildData pre-fetched.
// complete_content_children counts only member Unrestricted children as
// "already present", so a non-member same-id child is sent to the create
// closure; if build_child_creation_data skipped pre-fetching it (because it
// collected the skip-set from all Unrestricted children, non-member included), the
// closure's child_data.get(id).expect(..) panics. The skip-set must match the
// present-set: a non-member same-id child must not be skipped.
#[test]
fn independent_same_skgid_child_is_prefetched () {
  use crate::types::viewnode::{ mk_editable_viewnode };
  let goal : ID = skgid ("regression_independent_child");
  let mut tree : Tree<Viewnode> =
    Tree::new ( mk_editable_viewnode (
      skgid ("p"), skgrepo_name ("main"), "p" . to_string (), None ));
  let parent : NodeId = tree . root () . id ();
  let mut child : Viewnode =
    mk_editable_viewnode (
      goal . clone (), skgrepo_name ("main"), "c" . to_string (), None );
  if let ViewnodeKind::Vognode (Vognode::Unrestricted (t)) = &mut child . kind
    { t . claimsMembership = false; }
  tree . root_mut () . append (child);
  let config : SkgConfig =
    SkgConfig::dummyFromSkgrepos ( HashMap::new () );
  let no_relRepos : HashMap<ID, SkgrepoName> = HashMap::new ();
  let graph_snap : std::sync::Arc<InRustGraph> =
    std::sync::Arc::new ( InRustGraph::new () );
  let data : HashMap<ID, ChildData> =
    build_child_creation_data (
      &tree, parent, &[ goal . clone () ], &no_relRepos,
      &config, &graph_snap, None, false )
      . unwrap ();
  assert! ( data . contains_key (&goal),
            "a non-member same-id child's goal id must be pre-fetched, not \
             skipped -- else complete_content_children panics on it" );
}
