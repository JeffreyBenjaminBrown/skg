// Rung 7: substitution. In a content view a forked node is shown as
// the mirror its tree's perspective chooses, with '(inPlaceOf ID)',
// except at a root, in a folder, and as the content of a node shown
// raw where a mirror would stand. When the fork is gone, the shown
// headline loses 'inPlaceOf', becomes unwarranted (so, under an
// editable parent, 'noClaim'), and the original is reinstated beside
// it. A shown mirror that
// forks the original draws the original in its forks folder, as any
// editable fork does; and an editable node that a note forks draws that
// forker there, claiming nothing.

use super::*;
use indoc::formatdoc;

/// p contains n, which o forks.
fn forked () -> InRustGraph {
  graph (&[
    Graphnode { contains : ids (&["n"]), .. node ("p") },
    node ("n"),
    Graphnode { forks : msv_ids (&["n"]), .. node ("o") }, ]) }

#[test]
fn a_drawn_member_is_shown_as_its_mirror () {
  assert_eq! ( run_asserting_fixpoint (
    &format! ("* {} p\n", occurrence_metadata ("p", "")), &forked () ) . outline (), "\
node p
  node o (inPlaceOf n)
    forksFolder
      node n wp
        node p wp noClaim" ); }

#[test]
fn a_member_shown_raw_is_shown_as_its_mirror_in_place () {
  let input : String = formatdoc! ("
    * {} p
    ** (skg (vnid n1) folded node (gnid n) (skgRepos (nodeRepo main))) n
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

#[test]
fn the_content_of_a_forked_node_shown_raw_is_shown_raw () {
  let graph : InRustGraph = graph (&[
    Graphnode { contains : ids (&["n"]), .. node ("r") },
    Graphnode { forks : msv_ids (&["r"]), .. node ("s") },
    node ("n"),
    Graphnode { forks : msv_ids (&["n"]), .. node ("o") }, ]);
  assert_eq! ( run_asserting_fixpoint (
    &format! ("* {} r\n", occurrence_metadata ("r", "")), &graph ) . outline (), "\
node r
  node n
    forksFolder
      node o wp noClaim" ); }

// A mirror shown in place of an editable occurrence of the node
// it forks, while another occurrence holds the mirror's one
// editability, is write-protected, and the other keeps it: an
// occurrence of the mirror drawn later is write-protected too.
#[test]
fn a_mirror_editable_elsewhere_is_shown_write_protected () {
  let graph : InRustGraph = graph (&[
    Graphnode { contains : ids (&["n", "q"]), .. node ("p") },
    node ("n"),
    Graphnode { contains : ids (&["o"]), .. node ("q") },
    Graphnode { forks : msv_ids (&["n"]), .. node ("o") }, ]);
  let input : String = formatdoc! ("
    * {} p
    ** {} n
    ** {} q
    * {} o
    ", occurrence_metadata ("p", ""), occurrence_metadata ("n", ""),
       occurrence_metadata ("q", ""), occurrence_metadata ("o", "") );
  assert_eq! ( run_asserting_fixpoint (&input, &graph) . outline (), "\
node p
  node o wp (inPlaceOf n)
  node q
    node o wp
node o" ); }

/// 'node (pid)', but at home in 'skgrepo'.
fn node_in (
  skgrepo : &str,
  pid     : &str,
) -> Graphnode {
  Graphnode { home_skgrepo : SkgrepoName::from (skgrepo), .. node (pid) } }

/// The metadata of an occurrence of 'pid' in 'skgrepo'.
fn occurrence_metadata_in (
  skgrepo : &str,
  pid     : &str,
) -> String {
  format! ( "(skg node (gnid {}) (skgRepos (nodeRepo {})))", pid, skgrepo ) }

/// r, in 'theirs', and p, in 'main', each contain m, in 'theirs'.
/// a, in 'theirs', and u, in 'main', each fork m.
fn m_forked_by_its_author_and_the_user () -> InRustGraph {
  graph (&[
    Graphnode { contains : ids (&["m"]), .. node_in ("theirs", "r") },
    Graphnode { contains : ids (&["m"]), .. node ("p") },
    node_in ("theirs", "m"),
    Graphnode { forks : msv_ids (&["m"]), .. node_in ("theirs", "a") },
    Graphnode { forks : msv_ids (&["m"]), .. node ("u") }, ]) }

#[test]
fn under_a_foreign_root_its_authors_fork_is_shown_and_not_the_users () {
  assert_eq! ( run_asserting_fixpoint (
    &format! ("* {} r\n", occurrence_metadata_in ("theirs", "r")),
    &m_forked_by_its_author_and_the_user () ) . outline (), "\
node r
  node a (inPlaceOf m)
    forksFolder
      node m wp
        node p wp noClaim
        node r wp noClaim" ); }

#[test]
fn under_an_owned_root_the_users_fork_is_shown () {
  assert_eq! ( run_asserting_fixpoint (
    &format! ("* {} p\n", occurrence_metadata ("p", "")),
    &m_forked_by_its_author_and_the_user () ) . outline (), "\
node p
  node u (inPlaceOf m)
    forksFolder
      node m wp
        node p wp noClaim
        node r wp noClaim" ); }

/// r, in 'theirs', contains e, in 'third', which contains c, in
/// 'third'. c is forked by j, in 'theirs' (r's author), by d, in
/// 'third' (e's author), and by u, in 'main' (the user). Under r,
/// only r's author's perspective is followed, even beneath e.
#[test]
fn under_a_foreign_root_its_authors_perspective_holds_beneath_a_third_authors_node () {
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
        node c wp
          node e wp noClaim
            node r wp noClaim" ); }

/// A chain of forks stops where the next is not of the perspective:
/// m is forked by a, in 'theirs', which u, in 'main', forks.
/// Under r, in 'theirs', a is shown.
#[test]
fn under_a_foreign_root_a_chain_stops_before_the_users_fork () {
  let graph : InRustGraph = graph (&[
    Graphnode { contains : ids (&["m"]), .. node_in ("theirs", "r") },
    node_in ("theirs", "m"),
    Graphnode { forks : msv_ids (&["m"]), .. node_in ("theirs", "a") },
    Graphnode { forks : msv_ids (&["a"]), .. node ("u") }, ]);
  assert_eq! ( run_asserting_fixpoint (
    &format! ("* {} r\n", occurrence_metadata_in ("theirs", "r")),
    &graph ) . outline (), "\
node r
  node a (inPlaceOf m)
    forksFolder
      node m wp
        node r wp noClaim
      node u wp noClaim" ); }

/// Whether a foreign root is forked and so shows its content raw
/// asks of its author's perspective, not the user's: the user's k
/// forks r, yet r's author's fork of m is shown; r's
/// author's s forks q, so q's content is raw.
#[test]
fn a_foreign_root_is_shown_raw_only_when_its_author_forks_it () {
  let graph : InRustGraph = graph (&[
    Graphnode { contains : ids (&["m"]), .. node_in ("theirs", "r") },
    Graphnode { contains : ids (&["m"]), .. node_in ("theirs", "q") },
    node_in ("theirs", "m"),
    Graphnode { forks : msv_ids (&["m"]), .. node_in ("theirs", "a") },
    Graphnode { forks : msv_ids (&["r"]), .. node ("k") },
    Graphnode { forks : msv_ids (&["q"]), .. node_in ("theirs", "s") }, ]);
  assert_eq! ( run_asserting_fixpoint (
    &format! ("* {} r\n* {} q\n",
              occurrence_metadata_in ("theirs", "r"),
              occurrence_metadata_in ("theirs", "q")),
    &graph ) . outline (), "\
node r
  node a (inPlaceOf m)
    forksFolder
      node m wp
        node q wp noClaim
        node r wp noClaim
node q
  node m
    forksFolder
      node a wp noClaim" ); }
