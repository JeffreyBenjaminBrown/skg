// The forks folder in the diff view: one changed folder for a change
// of either side of 'forks', each member's change leaves keyed by the
// side or sides that changed ('docs/diff-view.org').
//
// The 'forks_folder' fixture (one skgrepo, main): at HEAD n forks e.
// Since HEAD, n forks m and k too, and f and m fork n. So n's changed
// forks folder holds e, unchanged, then m, a mutual whose two sides
// were both added, and k, each claiming, then f, which forks n,
// claiming nothing.

use skg::metadata_canon::without_viewnode_ids;
use crate::pruned_change_view::common::{
  all_changes_buffer, all_changes_buffer_with_marks, with_fixture};

use skg::dbs::in_rust_graph::relation_accessors::{NodeRelation, RelationshipKey};
use skg::diff_view::change_leaves::{LeafKey, RelationshipAspect};
use skg::types::misc::ID;

use std::error::Error;

/// The leaf of 'recorder' forking 'member', added since HEAD.
fn fork_leaf (
  recorder : &str,
  member   : &str,
) -> LeafKey {
  LeafKey::Relationship {
    relationship : RelationshipKey {
      relation : NodeRelation::Forks,
      recorder : ID::from (recorder),
      member   : ID::from (member), },
    aspect : RelationshipAspect::Existence } }

/// The line of the buffer under n's forks folder that shows 'member'.
fn member_line_under_n (
  buffer : &str,
  member : &str,
) -> String {
  let lines : Vec<&str> = buffer . lines () . collect ();
  let n : usize = lines . iter ()
    . position ( |line| line . starts_with ("* (skg node (gnid n)") )
    . unwrap_or_else ( || panic! ("n is a root:\n{}", buffer) );
  lines [n ..] . iter ()
    . find ( |line| line . starts_with (&format! ("*** (skg node (gnid {}) ", member)) )
    . unwrap_or_else ( || panic! ("{} under n:\n{}", member, buffer) )
    . to_string () }

#[test]
fn a_change_of_either_side_shows_in_the_one_folder (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("forks_folder", &["main"], |f| {
    let buffer : String = without_viewnode_ids (&all_changes_buffer (f) ? . unwrap ());
    let lines : Vec<&str> = buffer . lines () . collect ();
    let n : usize = lines . iter ()
      . position ( |line| line . starts_with ("* (skg node (gnid n)") ) . unwrap ();
    let under_n : Vec<String> = lines [n + 1 ..] . iter ()
      . take_while ( |line| ! line . starts_with ("* ") )
      . map ( |line| line . split (" (skgRepos") . next () . unwrap () . to_string () )
      . collect ();
    assert_eq! ( under_n, [
      "** (skg forksFolder)",
      "*** (skg node (gnid e)",
      "*** (skg node (gnid m)",
      "*** (skg node (gnid k)",
      "*** (skg node (gnid f)" ], "{}", buffer );
    assert! ( ! member_line_under_n (&buffer, "e") . contains ("(diff"), "{}", buffer );
    assert! ( member_line_under_n (&buffer, "m") . ends_with (
                "(birth (forks in 2) (forks out 2))) (diff (rel added) (leaves 2 rel))) m"),
              "m, a mutual, names both sides' leaves:\n{}", buffer );
    assert! ( member_line_under_n (&buffer, "k") . ends_with ("(diff (rel added) (leaves 1 rel))) k"),
              "{}", buffer );
    let forker : String = member_line_under_n (&buffer, "f");
    assert! ( forker . contains ("noClaim")
              && forker . ends_with ("(diff (rel added) (leaves 1 rel))) f"),
              "f, which forks n, claims nothing, and names the leaf it records:\n{}", buffer );
    Ok (( )) }) }

/// A processed mark on a mutual's relationship marks both sides: m is
/// processed only when both leaves are.
#[test]
fn a_mutual_is_processed_when_both_sides_are (
) -> Result<(), Box<dyn Error>> {
  with_fixture ("forks_folder", &["main"], |f| {
    let one_side : String = without_viewnode_ids (
      &all_changes_buffer_with_marks (f, &[ fork_leaf ("n", "m") ]) ? . unwrap ());
    assert! ( ! member_line_under_n (&one_side, "m") . contains ("(processed"),
              "{}", one_side );
    let both_sides : String = without_viewnode_ids (
      &all_changes_buffer_with_marks (
        f, &[ fork_leaf ("n", "m"), fork_leaf ("m", "n") ] ) ? . unwrap ());
    assert! ( member_line_under_n (&both_sides, "m") . contains ("(processed"),
              "{}", both_sides );
    Ok (( )) }) }
