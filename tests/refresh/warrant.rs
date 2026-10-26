// Rung 3: the goal and the warrant. Each child's warrant is set from
// its parent's goal: 'ByGraph' when the truth's part names it, 'No'
// otherwise (and in the diff view 'ByHead' when only HEAD's part
// does, which 'diff_view.rs' tests). Where the position implies no
// relationship, and at a root, a child is warranted.

use super::*;
use indoc::formatdoc;

#[test]
fn content_is_warranted_when_its_parent_lists_it () {
  let graph : InRustGraph = graph (&[
    Graphnode { contains : ids (&["c"]), .. node ("p") },
    node ("c"), node ("z"), ]);
  let input : String = formatdoc! ("
    * {} p
    ** {} c
    ** {} z
    ", occ ("p", ""), occ ("c", ""), occ ("z", "writeProtected noClaim") );
  assert_eq! ( run (&input, &graph) . outline (), "\
node p
  node c
  node z wp noClaim ⊥" ); }

#[test]
fn a_folder_member_is_warranted_when_the_folders_relation_holds () {
  let graph : InRustGraph = graph (&[
    Graphnode { contains : ids (&["p"]), .. node ("q") },
    node ("p"), node ("z"), ]);
  let input : String = formatdoc! ("
    * {} p
    ** (skg containerFolder)
    *** {} q
    *** {} z
    ", occ ("p", ""), occ ("q", "writeProtected"), occ ("z", "writeProtected") );
  assert_eq! ( run (&input, &graph) . outline (), "\
node p
  containerFolder
    node q wp noClaim
    node z wp noClaim ⊥" ); }

#[test]
fn a_chain_node_is_warranted_when_it_contains_the_node_above () {
  let graph : InRustGraph = graph (&[
    Graphnode { contains : ids (&["q"]), .. node ("top") },
    Graphnode { contains : ids (&["p"]), .. node ("q") },
    node ("p"), node ("z"), ]);
  let input : String = formatdoc! ("
    * {} p
    ** (skg containerFolder)
    *** {} q
    **** {} top
    **** {} z
    ", occ ("p", ""), occ ("q", "writeProtected"),
       occ ("top", "writeProtected noClaim"), occ ("z", "writeProtected noClaim") );
  assert_eq! ( run (&input, &graph) . outline (), "\
node p
  containerFolder
    node q wp noClaim
      node top wp noClaim
      node z wp noClaim ⊥",
    "the chain under a member already drawn is never reinstated, \
     and a node of it that does not contain the node above is unwarranted" ); }

#[test]
fn a_property_member_is_warranted_when_its_node_has_the_property () {
  let graph : InRustGraph = graph (&[
    Graphnode { aliases : MSV::Specified ( rel_partners_at_relRepo (
                  &SkgrepoName::from ("main"), vec! [ "a" . to_string () ] )),
                .. node ("p") } ]);
  let input : String = formatdoc! ("
    * {} p
    ** (skg aliasFolder)
    *** (skg alias) a
    *** (skg alias) b
    ", occ ("p", "") );
  let run : Run = run (&input, &graph);
  assert_eq! ( run . outline (), "\
node p
  aliasFolder
    alias a
    alias b noClaim ⊥" ); }

#[test]
fn a_child_of_a_tombstone_and_a_root_carry_no_warrant_fact () {
  let graph : InRustGraph = graph (&[ node ("p"), node ("c") ]);
  let input : String = formatdoc! ("
    * {} p
    ** (skg tombstone)
    *** {} c
    ", occ ("p", ""), occ ("c", "writeProtected") );
  let run : Run = run (&input, &graph);
  assert_eq! ( run . outline (), "\
node p
  tombstone
    node c wp noClaim" );
  assert! ( ! run . text . contains ("noWarrant"), "{}", run . text ); }
