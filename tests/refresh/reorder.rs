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
    ", occurrence_metadata ("p", ""),
       occurrence_metadata ("y", "writeProtected noClaim"),
       occurrence_metadata ("c", ""),
       occurrence_metadata ("z", "writeProtected noClaim"),
       occurrence_metadata ("b", ""), occurrence_metadata ("a", "") );
  assert_eq! ( run_asserting_fixpoint (&input, &graph) . outline (), "\
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
    ", occurrence_metadata ("p", ""), occurrence_metadata ("b", "writeProtected"),
       occurrence_metadata ("a", "writeProtected") );
  assert_eq! ( run_asserting_fixpoint (&input, &graph) . outline (), "\
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
    ", occurrence_metadata ("p", ""), occurrence_metadata ("b", "writeProtected"),
       occurrence_metadata ("a", "writeProtected") );
  assert_eq! ( run_asserting_fixpoint (&input, &graph) . outline (), "\
node p
  hiddenFolder
    node a wp noClaim
    node b wp noClaim" ); }

/// Another buffer's save reordered p's aliases, subscribees and
/// overridden nodes; this collateral view still holds the old orders.
/// Its editable folders take the new ones, since its next save would
/// write the order it shows.
#[test]
fn the_editable_ordered_folders_take_the_goals_order () {
  let graph : InRustGraph = graph (&[
    Graphnode { aliases      : MSV::Specified ( rel_partners_at_relRepo (
                  &SkgrepoName::from ("main"),
                  vec! [ "x" . to_string (), "y" . to_string () ] )),
                subscribesTo : msv_ids (&["a", "b"]),
                overrides    : msv_ids (&["c", "d"]),
                .. node ("p") },
    node ("a"), node ("b"), node ("c"), node ("d"), ]);
  let input : String = formatdoc! ("
    * {} p
    ** (skg aliasFolder)
    *** (skg alias) y
    *** (skg alias) x
    ** (skg subscribeeFolder)
    *** {} b
    *** {} a
    ** (skg overriddenFolder)
    *** {} d
    *** {} c
    ", occurrence_metadata ("p", ""),
       occurrence_metadata ("b", "writeProtected"),
       occurrence_metadata ("a", "writeProtected"),
       occurrence_metadata ("d", "writeProtected"),
       occurrence_metadata ("c", "writeProtected") );
  assert_eq! ( run_asserting_fixpoint (&input, &graph) . outline (), "\
node p
  aliasFolder
    alias x
    alias y
  subscribeeFolder
    node a wp
    node b wp
  overriddenFolder
    node c wp
    node d wp" ); }
