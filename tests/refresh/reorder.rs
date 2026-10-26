// Rung 6: reorder. Where the goal has an order (content, aliases, the
// outbound folders), the warranted children take that order and every
// other child stays immediately after the sibling it followed. Where
// it has none (the folders of recorders), nothing moves.

use super::*;
use indoc::formatdoc;

#[test]
fn content_takes_the_goals_order_and_riders_stay_after_what_they_followed () {
  let graph : InRustGraph = graph (&[
    Graphnode { contains : ids (&["a", "b", "c"]), .. node ("p") },
    node ("a"), node ("b"), node ("c"), node ("z"), node ("y"), ]);
  let input : String = formatdoc! ("
    * {} p
    ** {} y
    ** {} c
    ** {} z
    ** {} b
    ** {} a
    ", occ ("p", ""), occ ("y", "writeProtected noClaim"), occ ("c", ""),
       occ ("z", "writeProtected noClaim"), occ ("b", ""), occ ("a", "") );
  assert_eq! ( run (&input, &graph) . outline (), "\
node p
  node y wp noClaim ⊥
  node a
  node b
  node c
  node z wp noClaim ⊥",
    "y precedes every member, so it stays first; z rides after c" ); }

#[test]
fn a_recorders_folder_keeps_its_order () {
  let graph : InRustGraph = graph (&[
    Graphnode { contains : ids (&["p"]), .. node ("a") },
    Graphnode { contains : ids (&["p"]), .. node ("b") },
    node ("p"), ]);
  let input : String = formatdoc! ("
    * {} p
    ** (skg containerFolder)
    *** {} b
    *** {} a
    ", occ ("p", ""), occ ("b", "writeProtected"), occ ("a", "writeProtected") );
  assert_eq! ( run (&input, &graph) . outline (), "\
node p
  containerFolder
    node b wp noClaim
    node a wp noClaim" ); }

#[test]
fn the_hidden_folder_takes_its_nodes_order () {
  let graph : InRustGraph = graph (&[
    Graphnode { hidesFromSubs : msv_ids (&["a", "b"]), .. node ("p") },
    node ("a"), node ("b"), ]);
  let input : String = formatdoc! ("
    * {} p
    ** (skg hiddenFolder)
    *** {} b
    *** {} a
    ", occ ("p", ""), occ ("b", "writeProtected"), occ ("a", "writeProtected") );
  assert_eq! ( run (&input, &graph) . outline (), "\
node p
  hiddenFolder
    node a wp noClaim
    node b wp noClaim" ); }
