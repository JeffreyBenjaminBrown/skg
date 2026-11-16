// Rung 7: substitution. In a content view an overridden node is shown
// as its overrider, with '(inPlaceOf ID)', except at a root, in a
// folder, and as the content of an overridden node shown raw. When
// the override is gone, the shown headline loses 'inPlaceOf',
// becomes unwarranted (so, under an editable parent, 'noClaim'), and
// the overridden node is reinstated beside it. A shown mirror that
// forks the original draws the original in its forks folder, as any
// editable fork does; and an editable node that a note forks draws that
// forker there, claiming nothing.

use super::*;
use indoc::formatdoc;

/// p contains n, which o overrides.
fn overridden () -> InRustGraph {
  graph (&[
    Graphnode { contains : ids (&["n"]), .. node ("p") },
    node ("n"),
    Graphnode { forks : msv_ids (&["n"]), .. node ("o") }, ]) }

#[test]
fn a_drawn_member_is_shown_as_its_overrider () {
  assert_eq! ( run_asserting_fixpoint (
    &format! ("* {} p\n", occurrence_metadata ("p", "")), &overridden () ) . outline (), "\
node p
  node o (inPlaceOf n)
    forksFolder
      node n wp
        node p wp noClaim" ); }

#[test]
fn a_member_shown_raw_is_shown_as_its_overrider_in_place () {
  let input : String = formatdoc! ("
    * {} p
    ** (skg (vnid n1) folded node (gnid n) (skgRepos (nodeRepo main))) n
    ", occurrence_metadata ("p", "") );
  let run : Run = run_asserting_fixpoint (&input, &overridden ());
  assert_eq! ( run . outline (), "\
node p
  node o (inPlaceOf n) folded" );
  let shown : NodeRef<Viewnode> =
    run . forest . roots () . next () . unwrap () . first_child () . unwrap ();
  assert_eq! ( shown . value () . viewnode_id, Some ( ViewnodeId ("n1" . into ()) )); }

#[test]
fn when_the_override_is_gone_the_overrider_vanishes_and_the_original_returns () {
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

/// n, a root, is overridden by o; q, a member of n's containers
/// folder, is overridden by v. Each is shown as itself.
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
fn the_content_of_an_overridden_node_shown_raw_is_shown_raw () {
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

// An overrider shown in place of an editable occurrence of the node
// it overrides, while another occurrence holds the overrider's one
// editability, is write-protected, and the other keeps it: an
// occurrence of the overrider drawn later is write-protected too.
#[test]
fn an_overrider_editable_elsewhere_is_shown_write_protected () {
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
/// a, in 'theirs', and u, in 'main', each override m.
fn m_overridden_by_its_author_and_the_user () -> InRustGraph {
  graph (&[
    Graphnode { contains : ids (&["m"]), .. node_in ("theirs", "r") },
    Graphnode { contains : ids (&["m"]), .. node ("p") },
    node_in ("theirs", "m"),
    Graphnode { forks : msv_ids (&["m"]), .. node_in ("theirs", "a") },
    Graphnode { forks : msv_ids (&["m"]), .. node ("u") }, ]) }

#[test]
fn under_a_foreign_root_its_authors_overrider_is_shown_and_not_the_users () {
  assert_eq! ( run_asserting_fixpoint (
    &format! ("* {} r\n", occurrence_metadata_in ("theirs", "r")),
    &m_overridden_by_its_author_and_the_user () ) . outline (), "\
node r
  node a (inPlaceOf m)
    forksFolder
      node m wp
        node p wp noClaim
        node r wp noClaim" ); }

#[test]
fn under_an_owned_root_the_users_overrider_is_shown () {
  assert_eq! ( run_asserting_fixpoint (
    &format! ("* {} p\n", occurrence_metadata ("p", "")),
    &m_overridden_by_its_author_and_the_user () ) . outline (), "\
node p
  node u (inPlaceOf m)
    forksFolder
      node m wp
        node p wp noClaim
        node r wp noClaim" ); }

/// r, in 'theirs', contains e, in 'third', which contains c, in
/// 'third'. c is overridden by j, in 'theirs' (r's author), by d, in
/// 'third' (e's author), and by u, in 'main' (the user). Under r,
/// only r's author's perspective is followed, even beneath e.
#[test]
fn under_a_foreign_root_its_authors_overrides_are_followed_beneath_a_third_authors_node () {
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

/// A chain of overriders stops where the next is not the follower's:
/// m is overridden by a, in 'theirs', which u, in 'main', overrides.
/// Under r, in 'theirs', a is shown.
#[test]
fn under_a_foreign_root_a_chain_stops_before_the_users_overrider () {
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

/// Whether a foreign root is overridden and so shows its content raw
/// asks of its author's perspective, not the user's: the user's k
/// overrides r, yet r's author's overrider of m is shown; r's
/// author's s overrides q, so q's content is raw.
#[test]
fn a_foreign_root_is_shown_raw_only_when_its_author_overrides_it () {
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
