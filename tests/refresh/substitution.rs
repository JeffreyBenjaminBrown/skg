// Rung 7: substitution. In a content view a forked node is shown as
// the mirror its tree's perspective chooses, with '(inPlaceOf ID)',
// except in a folder, and as the content of a node shown raw where a
// mirror would stand. A note stands as itself in its own skgrepo, so
// the forked notes here are homed apart from their containers. When the fork is gone, the shown
// headline loses 'inPlaceOf', becomes unwarranted (so, under an
// editable parent, 'noClaim'), and the original is reinstated beside
// it. A shown mirror that
// forks the original draws the original in its forks folder, as any
// editable fork does; and an editable node that a note forks draws that
// forker there, claiming nothing.

use super::*;
use indoc::formatdoc;

/// p contains n, in 'private', which o forks. p and o are in 'main'.
fn forked () -> InRustGraph {
  graph (&[
    Graphnode { contains : ids (&["n"]), .. node ("p") },
    node_in ("private", "n"),
    Graphnode { forks : msv_ids (&["n"]), .. node ("o") }, ]) }

#[test]
fn a_drawn_member_is_shown_as_its_mirror () {
  assert_eq! ( run_asserting_fixpoint (
    &format! ("* {} p\n", occurrence_metadata ("p", "")), &forked () ) . outline (), "\
node p
  node o (inPlaceOf n)
    forksFolder
      node n
        forksFolder
          node o wp noClaim" ); }

#[test]
fn a_member_shown_raw_is_shown_as_its_mirror_in_place () {
  let input : String = formatdoc! ("
    * {} p
    ** (skg (vnid n1) folded node (gnid n) (skgRepos (nodeRepo private))) n
    ", occurrence_metadata ("p", "") );
  let run : Run = run_asserting_fixpoint (&input, &forked ());
  assert_eq! ( run . outline (), "\
node p
  node o (inPlaceOf n) folded" );
  let shown : NodeRef<Viewnode> =
    run . forest . roots () . next () . unwrap () . first_child () . unwrap ();
  assert_eq! ( shown . value () . viewnode_id, Some ( ViewnodeId ("n1" . into ()) )); }

#[test]
fn when_the_fork_is_gone_the_mirror_vanishes_and_the_original_returns () {
  let graph : InRustGraph = graph (&[
    Graphnode { contains : ids (&["n"]), .. node ("p") },
    node ("n"), node ("o"), ]);
  let input : String = formatdoc! ("
    * {} p
    ** {} o
    ", occurrence_metadata ("p", ""),
       occurrence_metadata ("o", "(inPlaceOf n)") );
  let run : Run = run_asserting_fixpoint (&input, &graph);
  assert_eq! ( run . outline (), "\
node p
  node n" ); }

/// n, a root, is forked by o; q, a member of n's containers
/// folder, is forked by v. Each is shown as itself.
#[test]
fn a_root_and_a_folder_member_are_shown_raw () {
  let graph : InRustGraph = graph (&[
    Graphnode { contains : ids (&["n"]), .. node ("q") },
    node ("n"),
    Graphnode { forks : msv_ids (&["n"]), .. node ("o") },
    Graphnode { forks : msv_ids (&["q"]), .. node ("v") }, ]);
  let input : String = formatdoc! ("
    * {} n
    ** (skg containerFolder)
    *** {} q
    ", occurrence_metadata ("n", ""), occurrence_metadata ("q", "writeProtected") );
  assert_eq! ( run_asserting_fixpoint (&input, &graph) . outline (), "\
node n
  containerFolder
    node q wp noClaim" ); }

/// A root shown raw where another mirror would stand shows its
/// content raw too: r, in 'theirs', contains n, in 'theirs', which o,
/// in 'main', forks; s, in 'private', forks r. From the perspective of
/// 'private' under the restriction to 'main', which keeps 'private'
/// out, r stands raw where s would stand, and n, which o would
/// otherwise stand for, is raw.
#[test]
fn the_content_of_a_forked_node_shown_raw_is_shown_raw () {
  let graph : InRustGraph = graph (&[
    Graphnode { contains : ids (&["n"]), .. node_in ("theirs", "r") },
    Graphnode { forks : msv_ids (&["r"]), .. node_in ("private", "s") },
    node_in ("theirs", "n"),
    Graphnode { forks : msv_ids (&["n"]), .. node ("o") }, ]);
  assert_eq! ( run_asserting_fixpoint_from (
    &format! ("* {} r\n", occurrence_metadata_in ("theirs", "r")), &graph,
    &main_only (), Perspective::Skgrepo ( SkgrepoName::from ("private") )) . outline (), "\
node r
  node n
    forksFolder
      node o noClaim
        forksFolder
          node n wp" ); }

// A mirror shown in place of an editable occurrence of the node
// it forks, while another occurrence holds the mirror's one
// editability, is write-protected, and the other keeps it: an
// occurrence of the mirror drawn later is write-protected too.
#[test]
fn a_mirror_editable_elsewhere_is_shown_write_protected () {
  let graph : InRustGraph = graph (&[
    Graphnode { contains : ids (&["n", "q"]), .. node ("p") },
    node_in ("private", "n"),
    Graphnode { contains : ids (&["o"]), .. node ("q") },
    Graphnode { forks : msv_ids (&["n"]), .. node ("o") }, ]);
  let input : String = formatdoc! ("
    * {} p
    ** {} n
    ** {} q
    * {} o
    ", occurrence_metadata ("p", ""), occurrence_metadata_in ("private", "n"),
       occurrence_metadata ("q", ""), occurrence_metadata ("o", "") );
  assert_eq! ( run_asserting_fixpoint (&input, &graph) . outline (), "\
node p
  node o wp (inPlaceOf n)
  node q
    node o wp
node o" ); }

/// r, in 'theirs', and p, in 'main', each contain m, in 'third'.
/// a, in 'theirs', and u, in 'main', each fork m.
fn m_forked_in_theirs_and_in_main () -> InRustGraph {
  graph (&[
    Graphnode { contains : ids (&["m"]), .. node_in ("theirs", "r") },
    Graphnode { contains : ids (&["m"]), .. node ("p") },
    node_in ("third", "m"),
    Graphnode { forks : msv_ids (&["m"]), .. node_in ("theirs", "a") },
    Graphnode { forks : msv_ids (&["m"]), .. node ("u") }, ]) }

#[test]
fn under_a_foreign_root_the_fork_in_its_home_is_shown_and_not_the_users () {
  assert_eq! ( run_asserting_fixpoint (
    &format! ("* {} r\n", occurrence_metadata_in ("theirs", "r")),
    &m_forked_in_theirs_and_in_main () ) . outline (), "\
node r
  node a (inPlaceOf m)
    forksFolder
      node m
        containerFolder
          node p wp noClaim folded
        forksFolder
          node a wp noClaim
          node u noClaim
            forksFolder
              node m wp" ); }

#[test]
fn under_an_owned_root_the_users_fork_is_shown () {
  assert_eq! ( run_asserting_fixpoint (
    &format! ("* {} p\n", occurrence_metadata ("p", "")),
    &m_forked_in_theirs_and_in_main () ) . outline (), "\
node p
  node u (inPlaceOf m)
    forksFolder
      node m
        containerFolder
          node r wp noClaim folded
        forksFolder
          node a noClaim
            forksFolder
              node m wp
          node u wp noClaim" ); }

/// r, in 'theirs', contains e, in 'third', which contains c, in
/// 'third'. c is forked by j, in 'theirs' (r's home), by d, in
/// 'third' (e's home), and by u, in 'main'. Under r, only the
/// perspective of r's home is followed, even beneath e.
#[test]
fn under_a_foreign_root_its_homes_perspective_holds_beneath_a_node_homed_elsewhere () {
  let graph : InRustGraph = graph (&[
    Graphnode { contains : ids (&["e"]), .. node_in ("theirs", "r") },
    Graphnode { contains : ids (&["c"]), .. node_in ("third", "e") },
    node_in ("third", "c"),
    Graphnode { forks : msv_ids (&["c"]), .. node_in ("theirs", "j") },
    Graphnode { forks : msv_ids (&["c"]), .. node_in ("third", "d") },
    Graphnode { forks : msv_ids (&["c"]), .. node ("u") }, ]);
  assert_eq! ( run_asserting_fixpoint (
    &format! ("* {} r\n", occurrence_metadata_in ("theirs", "r")),
    &graph ) . outline (), "\
node r
  node e
    node j (inPlaceOf c)
      forksFolder
        node c
          forksFolder
            node d noClaim
              forksFolder
                node c wp
            node j wp noClaim
            node u noClaim
              forksFolder
                node c wp" ); }

/// A chain of forks stops where the next is not in the perspective's
/// skgrepo: m, in 'third', is forked by a, in 'theirs', which u, in
/// 'main', forks. Under r, in 'theirs', a is shown.
#[test]
fn under_a_foreign_root_a_chain_stops_before_the_users_fork () {
  let graph : InRustGraph = graph (&[
    Graphnode { contains : ids (&["m"]), .. node_in ("theirs", "r") },
    node_in ("third", "m"),
    Graphnode { forks : msv_ids (&["m"]), .. node_in ("theirs", "a") },
    Graphnode { forks : msv_ids (&["a"]), .. node ("u") }, ]);
  assert_eq! ( run_asserting_fixpoint (
    &format! ("* {} r\n", occurrence_metadata_in ("theirs", "r")),
    &graph ) . outline (), "\
node r
  node a (inPlaceOf m)
    forksFolder
      node m
        forksFolder
          node a wp noClaim
      node u noClaim
        forksFolder
          node a wp" ); }

/// Whether a root is shown raw where another mirror would stand, and
/// so shows its content raw, asks of the view's perspective: from the
/// perspective of 'private', under the restriction to 'main', which
/// keeps 'private' out, r, in 'theirs', which k, in 'private', forks,
/// is raw where k would stand, so its m, in 'theirs', is raw though
/// a, in 'main', forks m; q, in 'theirs', forked by s, in 'third',
/// stands as itself (from 'private', the first skgrepo declared that
/// holds a mirror of q is q's own), so its m is shown as a. m in r's
/// content holds m's editability, so q's a, in m's place, shows m in
/// its forks folder write-protected.
#[test]
fn a_root_is_shown_raw_only_where_the_views_perspective_would_show_another () {
  let graph : InRustGraph = graph (&[
    Graphnode { contains : ids (&["m"]), .. node_in ("theirs", "r") },
    Graphnode { contains : ids (&["m"]), .. node_in ("theirs", "q") },
    node_in ("theirs", "m"),
    Graphnode { forks : msv_ids (&["m"]), .. node ("a") },
    Graphnode { forks : msv_ids (&["r"]), .. node_in ("private", "k") },
    Graphnode { forks : msv_ids (&["q"]), .. node_in ("third", "s") }, ]);
  assert_eq! ( run_asserting_fixpoint_from (
    &format! ("* {} r\n* {} q\n",
              occurrence_metadata_in ("theirs", "r"),
              occurrence_metadata_in ("theirs", "q")),
    &graph, &main_only (), Perspective::Skgrepo ( SkgrepoName::from ("private") ))
    . outline (), "\
node r
  node m
    forksFolder
      node a wp noClaim
node q
  node a (inPlaceOf m)
    forksFolder
      node m wp" ); }
