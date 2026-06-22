// Unit tests for fork source-inference. Wired into the lib's test
// build from server/from_text/fork.rs via #[path].
//
// The rule under test ('owned_ancestor_sources_for_foreign_vognodes'):
// a foreign node's clone inherits the source of its NEAREST vognode
// ancestor, recorded only if that ancestor is an owned Active vognode.
// The walk skips scaffolds (cols) but STOPS at the first vognode -- it
// never passes a foreign or inactive ancestor to reach a distant owned
// one.

use super::*;
use crate::types::misc::SkgfileSource;
use crate::types::nodes::complete::empty_node_complete;
use crate::types::tree::forest::ViewForest;
use crate::types::viewnode::{ViewNode, ViewNodeKind, PartnerCol,
                             mk_definitive_viewnode};
use std::collections::HashMap;
use std::path::PathBuf;

fn config_two_owned_one_foreign () -> SkgConfig {
  let mut sources : HashMap<SourceName, SkgfileSource> = HashMap::new ();
  for (name, owns) in [ ("owned1", true),
                        ("owned2", true),
                        ("foreign", false) ] {
    sources . insert (
      SourceName::from (name),
      SkgfileSource {
        name         : SourceName::from (name),
        abbreviation : None,
        path         : PathBuf::from (name),
        user_owns_it : owns, } ); }
  SkgConfig::fromSourcesAndDbName ( sources, "db", "/tmp/none" ) }

fn active (id : &str, source : &str) -> ViewNode {
  mk_definitive_viewnode (
    ID::from (id), SourceName::from (source), id . to_string (), None ) }

fn subscribee_col () -> ViewNode {
  ViewNode { focused : false, folded : false, body_folded : false,
             kind : ViewNodeKind::PartnerCol (PartnerCol::Subscribee) } }

/// Build a forest exercising the three shapes:
/// - owned2 P -> foreign F -> foreign N   (must infer NOTHING for N)
/// - owned2 Q -> foreign M                (must infer owned2 for M)
/// - owned1 R -> subscribeeCol -> foreign S  (col skipped: owned1 for S)
fn build_forest () -> ViewForest {
  let mut f : ViewForest = ViewForest::new ();
  let p : ego_tree::NodeId = f . append_root ( active ("P", "owned2") );
  let ff : ego_tree::NodeId =
    f . get_mut (p) . unwrap () . append ( active ("F", "foreign") ) . id ();
  let _n : ego_tree::NodeId =
    f . get_mut (ff) . unwrap () . append ( active ("N", "foreign") ) . id ();
  let q : ego_tree::NodeId = f . append_root ( active ("Q", "owned2") );
  let _m : ego_tree::NodeId =
    f . get_mut (q) . unwrap () . append ( active ("M", "foreign") ) . id ();
  let r : ego_tree::NodeId = f . append_root ( active ("R", "owned1") );
  let rc : ego_tree::NodeId =
    f . get_mut (r) . unwrap () . append ( subscribee_col () ) . id ();
  let _s : ego_tree::NodeId =
    f . get_mut (rc) . unwrap () . append ( active ("S", "foreign") ) . id ();
  f }

#[test]
fn owned_foreign_N_infers_nothing () {
  // The bug: walking PAST the foreign ancestor F to the owned P. The
  // corrected rule stops at F (foreign) and infers nothing for N.
  let config : SkgConfig = config_two_owned_one_foreign ();
  let map = owned_ancestor_sources_for_foreign_vognodes (
    & build_forest (), & config );
  assert! ( ! map . contains_key (& ID::from ("N")),
    "owned -> foreign -> N must infer no source; got {:?}",
    map . get (& ID::from ("N")) ); }

#[test]
fn owned_N_still_infers_the_owned_source () {
  // owned2 Q directly contains foreign M: M inherits owned2.
  let config : SkgConfig = config_two_owned_one_foreign ();
  let map = owned_ancestor_sources_for_foreign_vognodes (
    & build_forest (), & config );
  assert_eq! ( map . get (& ID::from ("M")),
               Some (& SourceName::from ("owned2")),
    "owned -> M must infer the owned ancestor's source" ); }

#[test]
fn scaffold_ancestor_is_skipped () {
  // owned1 R -> subscribeeCol -> foreign S: the col is a scaffold, so
  // S's nearest VOGNODE ancestor is the owned R.
  let config : SkgConfig = config_two_owned_one_foreign ();
  let map = owned_ancestor_sources_for_foreign_vognodes (
    & build_forest (), & config );
  assert_eq! ( map . get (& ID::from ("S")),
               Some (& SourceName::from ("owned1")),
    "a scaffold between an owned ancestor and a foreign node is skipped" ); }

/// A fork-to-be (clone) with an edited title over the original N it
/// overrides, in N's foreign source. (original_title is N's disk title.)
fn fork_spec_n_edited () -> ForkSpec {
  let buffer_node : NodeComplete = NodeComplete {
    title  : "N-edited" . to_string (),
    source : SourceName::from ("foreign"),
    pid    : ID::from ("N"),
    .. empty_node_complete () };
  build_fork_clone (
    & buffer_node, "N-original", SourceName::from ("owned2") ) }

#[test]
fn confirmation_buffer_is_two_level_with_pO_on_the_child () {
  let buf : String =
    build_fork_confirmation_buffer ( & [ fork_spec_n_edited () ] );
  let lines : Vec<&str> = buf . lines () . collect ();
  // The clone-to-be parent: a LEVEL-1 headline ("* "), edited title, and
  // the PICK-A-SOURCE placeholder source the user must replace (NO id).
  // (starts_with pins the level marker so a "* " -> "** " drift is caught
  // -- the elisp walk keys off the level.)
  assert! ( lines . iter () . any ( |l|
      l . starts_with (
        & format! ("* (skg (node (source {})", FORK_SOURCE_PLACEHOLDER) )
      && l . ends_with ("N-edited") ),
    "clone-to-be parent (level-1, edited title, placeholder source) missing:\n{}", buf );
  // The computed source is shown only as a SUGGESTION comment.
  assert! ( lines . iter () . any ( |l|
      l . starts_with ("# Suggested source")
      && l . contains ("owned2") ),
    "the clone's suggested source must be noted:\n{}", buf );
  assert! ( ! buf . contains ("(id N) (source owned2)"),
    "the clone-to-be must carry no id:\n{}", buf );
  // The original child: a LEVEL-2 headline ("** "), real id, foreign
  // source, indef, independent, pO, original title.
  assert! ( lines . iter () . any ( |l|
      l . starts_with ("** (skg (node (id N) (source foreign)")
      && l . contains ("(parentIs independent)")
      && l . contains ("indef")
      && l . contains ("parentOverrides")
      && l . ends_with ("N-original") ),
    "original child (level-2, id/foreign/indef/independent/pO) missing:\n{}", buf ); }
