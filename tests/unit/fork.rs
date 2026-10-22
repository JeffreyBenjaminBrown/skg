// Unit tests for fork repo-inference. Wired into the lib's test
// build from server/from_text/fork.rs via #[path].
//
// The rule under test ('owned_ancestor_repos_for_foreign_vognodes'):
// a foreign node's clone inherits the skgrepo of its nearest vognode
// ancestor, recorded only if that ancestor is an owned Unrestricted vognode.
// The walk skips non-vognodes (folders) but stops at the first vognode -- it
// never passes a foreign or restricted ancestor to reach a distant owned
// one.

use super::*;
use crate::types::misc::{Skgrepo, members_of, rel_partners_at_relRepo};
use crate::types::nodes::complete::{
  Flag, empty_graphnode};
use crate::types::tree::forest::ViewForest;
use crate::types::viewnode::{AffectsParent, Viewnode, ViewnodeKind, PartnerFolder,
                             mk_editable_viewnode, mk_writeProtected_viewnode,
                             viewnode_of};
use std::collections::HashMap;
use std::path::PathBuf;

fn config_two_owned_one_foreign () -> SkgConfig {
  let mut skgrepos : HashMap<SkgrepoName, Skgrepo> = HashMap::new ();
  for (name, owns) in [ ("owned1", true),
                        ("owned2", true),
                        ("foreign", false) ] {
    skgrepos . insert (
      SkgrepoName::from (name),
      Skgrepo {
        name         : SkgrepoName::from (name),
        abbreviation : None,
        path         : PathBuf::from (name),
        owned        : owns, } ); }
  SkgConfig::fromSkgreposAndTantivyFolder ( skgrepos, "/tmp/none" ) }

fn restriction (skgid : &str, skgrepo : &str) -> Viewnode {
  mk_editable_viewnode (
    ID::from (skgid), SkgrepoName::from (skgrepo), skgid . to_string (), None ) }

fn subscribee_folder () -> Viewnode {
  viewnode_of (ViewnodeKind::PartnerFolder (PartnerFolder::Subscribee)) }

/// Build a forest exercising the three shapes:
/// - owned2 P -> foreign F -> foreign N   (must infer nothing for N)
/// - owned2 Q -> foreign M                (must infer owned2 for M)
/// - owned1 R -> subscribeeFolder -> foreign S  (folder skipped: owned1 for S)
fn build_forest () -> ViewForest {
  let mut f : ViewForest = ViewForest::new ();
  let p : ego_tree::NodeId = f . append_root ( restriction ("P", "owned2") );
  let ff : ego_tree::NodeId =
    f . get_mut (p) . unwrap () . append ( restriction ("F", "foreign") ) . id ();
  let _n : ego_tree::NodeId =
    f . get_mut (ff) . unwrap () . append ( restriction ("N", "foreign") ) . id ();
  let q : ego_tree::NodeId = f . append_root ( restriction ("Q", "owned2") );
  let _m : ego_tree::NodeId =
    f . get_mut (q) . unwrap () . append ( restriction ("M", "foreign") ) . id ();
  let r : ego_tree::NodeId = f . append_root ( restriction ("R", "owned1") );
  let rc : ego_tree::NodeId =
    f . get_mut (r) . unwrap () . append ( subscribee_folder () ) . id ();
  let _s : ego_tree::NodeId =
    f . get_mut (rc) . unwrap () . append ( restriction ("S", "foreign") ) . id ();
  f }

#[test]
fn owned_foreign_N_infers_nothing () {
  // The bug: walking past the foreign ancestor F to the owned P. The
  // corrected rule stops at F (foreign) and infers nothing for N.
  let config : SkgConfig = config_two_owned_one_foreign ();
  let map = owned_ancestor_skgrepos_for_foreign_vognodes (
    & build_forest (), & HashMap::new (), & config );
  assert! ( ! map . contains_key (& ID::from ("N")),
    "owned -> foreign -> N must infer no repo; got {:?}",
    map . get (& ID::from ("N")) ); }

#[test]
fn owned_N_still_infers_the_owned_skgrepo () {
  // owned2 Q directly contains foreign M: M inherits owned2.
  let config : SkgConfig = config_two_owned_one_foreign ();
  let map = owned_ancestor_skgrepos_for_foreign_vognodes (
    & build_forest (), & HashMap::new (), & config );
  assert_eq! ( map . get (& ID::from ("M")),
               Some (& SkgrepoName::from ("owned2")),
    "owned -> M must infer the owned ancestor's repo" ); }

#[test]
fn non_vognode_ancestor_is_skipped () {
  // owned1 R -> subscribeeFolder -> foreign S: the folder is a non-vognode, so
  // S's nearest vognode ancestor is the owned R.
  let config : SkgConfig = config_two_owned_one_foreign ();
  let map = owned_ancestor_skgrepos_for_foreign_vognodes (
    & build_forest (), & HashMap::new (), & config );
  assert_eq! ( map . get (& ID::from ("S")),
               Some (& SkgrepoName::from ("owned1")),
    "a non-vognode between an owned ancestor and a foreign node is skipped" ); }

#[test]
fn writeProtected_ancestor_skgrepo_is_read_from_its_skgid () {
  // A write-protected occurrence of X shows owned1, but this save moves
  // X (via its editable occurrence elsewhere) to owned2. The foreign N
  // under the write-protected occurrence infers owned2.
  let config : SkgConfig = config_two_owned_one_foreign ();
  let mut forest : ViewForest = ViewForest::new ();
  let x : ego_tree::NodeId = forest . append_root (
    mk_writeProtected_viewnode (
      ID::from ("X"), SkgrepoName::from ("owned1"), "X" . to_string (),
      AffectsParent::NA ));
  forest . get_mut (x) . unwrap () . append ( restriction ("N", "foreign") );
  let writeProtected_skgrepos_after_save : HashMap<ID, SkgrepoName> =
    HashMap::from ([ ( ID::from ("X"), SkgrepoName::from ("owned2") ) ]);
  let map = owned_ancestor_skgrepos_for_foreign_vognodes (
    & forest, & writeProtected_skgrepos_after_save, & config );
  assert_eq! ( map . get (& ID::from ("N")),
               Some (& SkgrepoName::from ("owned2")),
    "a write-protected ancestor's repo must come from its ID, not its stale display" ); }

/// A fork-to-be (clone) with an edited title over the original N it
/// overrides, in N's foreign repo. (original_title is N's disk title.)
fn fork_spec_n_edited (
  skgrepo_confirmed : bool,
) -> ForkSpec {
  let buffer_node : Graphnode = Graphnode {
    title        : "N-edited" . to_string (),
    home_skgrepo : SkgrepoName::from ("foreign"),
    pid          : ID::from ("N"),
    .. empty_graphnode () };
  build_fork_clone (
    & buffer_node, "N-original", &[], SkgrepoName::from ("owned2"),
    skgrepo_confirmed ) }

#[test]
fn fork_clone_hides_children_the_edit_deleted () {
  // N had [N1, N2] on disk; the forking edit kept only N1. The clone
  // must hide N2, or it would reappear under the clone as
  // unintegrated subscribed content the user just dismissed.
  let buffer_node : Graphnode = Graphnode {
    title        : "N-edited" . to_string (),
    home_skgrepo : SkgrepoName::from ("foreign"),
    pid          : ID::from ("N"),
    contains     : rel_partners_at_relRepo (
      & SkgrepoName::from ("foreign"),
      vec! [ ID::from ("N1") ] ),
    .. empty_graphnode () };
  let spec : ForkSpec = build_fork_clone (
    & buffer_node, "N-original",
    & [ ID::from ("N1"), ID::from ("N2") ],
    SkgrepoName::from ("owned2"), false );
  assert_eq! (
    members_of (
      spec . clone . 0 . hidesFromSubs . or_default () ),
    vec! [ ID::from ("N2") ],
    "the clone must hide exactly the children the edit deleted" ); }

#[test]
fn confirmation_buffer_is_two_level_with_pO_on_the_child () {
  let buf : String =
    build_fork_confirmation_buffer ( & [ fork_spec_n_edited (false) ] );
  let lines : Vec<&str> = buf . lines () . collect ();
  // The explanation lives under an org headline (foldable), not a
  // long '#' comment block.
  assert! ( lines . first () . map_or ( false, |l|
      l . starts_with ("* Fork confirmation") ),
    "the buffer must open with the instructions headline:\n{}", buf );
  // The clone-to-be parent: a level-1 headline ("* "), edited title, and
  // the PICK-A-REPO placeholder skgrepo the user must replace (no id).
  // (starts_with pins the level marker so a "* " -> "** " drift is caught
  // -- the elisp walk keys off the level.)
  assert! ( lines . iter () . any ( |l|
      l . starts_with (
        & format! ("* (skg (node (repo {})", FORK_SKGREPO_PLACEHOLDER) )
      && l . ends_with ("N-edited") ),
    "clone-to-be parent (level-1, edited title, placeholder repo) missing:\n{}", buf );
  // The computed skgrepo is shown only as a suggestion comment,
  // directly above the clone-to-be (the client parses that adjacency
  // for the prompt's default).
  assert! ( lines . windows (2) . any ( |w|
      w[0] . starts_with ("# Suggested repo")
      && w[0] . contains ("owned2")
      && w[1] . starts_with ("* (skg") ),
    "the clone's suggested repo must sit directly above it:\n{}", buf );
  assert! ( ! buf . contains ("(id N) (repo owned2)"),
    "the clone-to-be must carry no id:\n{}", buf );
  // The original child: a level-2 headline ("** "), real id, foreign
  // skgrepo, write-protected, independent, pO, original title.
  assert! ( lines . iter () . any ( |l|
      l . starts_with ("** (skg (node (id N) (repo foreign)")
      && l . contains ("(affectsParent false)")
      && l . contains ("writeProtected")
      && l . contains ("parentOverrides")
      && l . ends_with ("N-original") ),
    "original child (level-2, id/foreign/writeProtected/independent/pO) missing:\n{}", buf ); }

#[test]
fn confirmation_buffer_shows_a_confirmed_skgrepo_as_settled () {
  // When the user already specified the clone's skgrepo (explicitly in
  // the saved metadata, or in a prior confirmation round), the buffer
  // shows that skgrepo -- no placeholder, no suggestion comment.
  let buf : String =
    build_fork_confirmation_buffer ( & [ fork_spec_n_edited (true) ] );
  let lines : Vec<&str> = buf . lines () . collect ();
  assert! ( lines . iter () . any ( |l|
      l . starts_with ("* (skg (node (repo owned2)")
      && l . ends_with ("N-edited") ),
    "a confirmed clone must show its real repo:\n{}", buf );
  assert! ( ! buf . contains (
      & format! ("(repo {})", FORK_SKGREPO_PLACEHOLDER) ),
    // (The instructions body may mention the placeholder; only the
    // metadata form matters.)
    "no placeholder repo when every repo is confirmed:\n{}", buf );
  assert! ( ! buf . contains ("# Suggested repo"),
    "no suggestion comment when every repo is confirmed:\n{}", buf ); }

#[test]
fn fork_clone_preserves_only_the_search_matching_flag () {
  for no_search_matching in [false, true] {
    let mut flags = vec![
      Flag::Had_ID_Before_Import,
      Flag::Was_Overloaded];
    if no_search_matching {
      flags . insert (1, Flag::NoSearchMatching); }
    let buffer_node : Graphnode = Graphnode {
      home_skgrepo : SkgrepoName::from ("foreign"),
      pid          : ID::from ("N"),
      flags,
      .. empty_graphnode () };
    let spec : ForkSpec = build_fork_clone (
      &buffer_node, "N", &[], SkgrepoName::from ("owned2"), false );
    assert_eq! (
      spec . clone . 0 . flags,
      if no_search_matching { vec![Flag::NoSearchMatching] }
      else { Vec::new () } ); }
}
