// cargo nextest run --test grouped_forks -E 'test(forkers_drawn_editable::)'
//
// Forkers as such ('TODO/every-mirror/2_plan.org', slice 3;
// 'docs/glossary.org', "forker as such"). The fixture is the plan's
// "forks of one note meet", with a forkee and a foreign forker added.
// N, owned, contains X1 to X4, forks K and hides Z. R and T, owned,
// fork N: R contains X1, X2, X3; T contains X1, X3, Y, Z and hides X2.
// K contains k1. FF, foreign, forks N and contains X1 and f1. So in
// N's forks folder T shows Y (what it adds), with Z in its folder of
// what N hides in it, and X2 in its folder of what it hides of N's
// content; R shows nothing; FF shows f1. Each sub-test edits a view
// and saves it:
// - a buffer showing T editable, saved unchanged, writes nothing: T's
//   partial list is never read as its whole contains;
// - a fresh view of N draws K, its forkee, then its forkers FF, R and
//   T, each editable, claiming nothing, showing what it adds;
// - under T, the folder of what N hides in it (Z) comes first, then
//   the folder of what T hides of N's content (X2), then its forks
//   folder; K, a forkee, has no such folder;
// - in R's own view, R, standing in N's forks folder, is a repeat:
//   write-protected, showing nothing;
// - the fresh view of N, saved unchanged, writes nothing and draws
//   itself again;
// - T's title, edited there, is saved; FF's is refused;
// - a headline typed under T joins T's content after Y; one typed
//   under FF is refused;
// - Z, which only T contains, is in T's folder of what N hides, not
//   in the folder of what N hides outside its fork partners; moved
//   out of T's folder, it is unhidden in N;
// - a buffer drawing T write-protected and bare rerenders as it is,
//   and an editable-view request on N extends T.
// A separate test narrows the skgrepo restriction so that T's record
// of Y is kept out: Y leaves the view, and the widening draws it again.
// A cut from a forker as such is tested in 'tests/cuts_from_forkers.rs'.

use std::error::Error;
use std::collections::BTreeMap;
use indoc::indoc;

use crate::git_fixture::{
  Fixture, OwnedSkgrepos, open_content_view, switch_skgrepo_set, with_git_fixture,
  write_unchanged_skgrepos};
use crate::view_and_save_helpers::{
  children_of, content_of, depth, ids, line_of, node, refusal_of, save_cleanly,
  skg_files, under, view_of, with_line_after_subtree_of};
use skg::metadata_canon::without_viewnode_ids;
use skg::skgrepo_sets::SkgrepoRestriction;
use skg::test_utils::run_with_shared_test_stores;
use skg::types::misc::{ID, SkgConfig, TantivyIndex, members_of};
use skg::types::nodes::complete::Graphnode;

const FIXTURES : &str = "tests/forkers_drawn_editable/fixtures";

/// N's view as a buffer might hold it with T, a forker as such,
/// editable, showing Y, what it adds to N's content.
const N_WITH_T_EDITABLE : &str = indoc! {"
  * (skg node (gnid N) (skgRepos (nodeRepo main))) N
  ** (skg forksFolder)
  *** (skg node (gnid K) (skgRepos (nodeRepo main)) writeProtected) K
  *** (skg node (gnid FF) (skgRepos (nodeRepo theirs)) noClaim writeProtected) FF
  *** (skg node (gnid R) (skgRepos (nodeRepo main)) noClaim writeProtected) R
  *** (skg node (gnid T) (skgRepos (nodeRepo main)) noClaim) T
  **** (skg node (gnid Y) (skgRepos (nodeRepo main))) Y
  ** (skg node (gnid X1) (skgRepos (nodeRepo main))) X1
  ** (skg node (gnid X2) (skgRepos (nodeRepo main))) X2
  ** (skg node (gnid X3) (skgRepos (nodeRepo main))) X3
  ** (skg node (gnid X4) (skgRepos (nodeRepo main))) X4
  "};

#[test]
fn all_tests
  () -> Result<(), Box<dyn Error>> {
  run_with_shared_test_stores (
    "skg-test-forkers-drawn-editable",
    |s| Box::pin ( async move {
      s . reset ("an_unchanged_save_of_an_editable_forker_writes_nothing", FIXTURES) ?;
      an_unchanged_save_of_an_editable_forker_writes_nothing (&s . config, &mut s . tantivy) . await ?;
      s . reset ("a_fresh_view_draws_forkers_after_forkees_editable", FIXTURES) ?;
      a_fresh_view_draws_forkers_after_forkees_editable (&s . config, &mut s . tantivy) . await ?;
      s . reset ("a_forkers_folders_stand_in_order", FIXTURES) ?;
      a_forkers_folders_stand_in_order (&s . config, &mut s . tantivy) . await ?;
      s . reset ("a_repeat_forker_shows_nothing", FIXTURES) ?;
      a_repeat_forker_shows_nothing (&s . config, &mut s . tantivy) . await ?;
      s . reset ("an_unchanged_fresh_view_writes_nothing", FIXTURES) ?;
      an_unchanged_fresh_view_writes_nothing (&s . config, &mut s . tantivy) . await ?;
      s . reset ("an_owned_forkers_title_is_saved", FIXTURES) ?;
      an_owned_forkers_title_is_saved (&s . config, &mut s . tantivy) . await ?;
      s . reset ("a_foreign_forkers_title_is_refused", FIXTURES) ?;
      a_foreign_forkers_title_is_refused (&s . config, &mut s . tantivy) . await ?;
      s . reset ("a_child_typed_under_an_owned_forker_joins_its_content", FIXTURES) ?;
      a_child_typed_under_an_owned_forker_joins_its_content (&s . config, &mut s . tantivy) . await ?;
      s . reset ("a_child_typed_under_a_foreign_forker_is_refused", FIXTURES) ?;
      a_child_typed_under_a_foreign_forker_is_refused (&s . config, &mut s . tantivy) . await ?;
      s . reset ("a_hide_moved_out_under_a_forker_is_unhidden", FIXTURES) ?;
      a_hide_moved_out_under_a_forker_is_unhidden (&s . config, &mut s . tantivy) . await ?;
      s . reset ("a_write_protected_forker_stays_until_its_note_is_extended", FIXTURES) ?;
      a_write_protected_forker_stays_until_its_note_is_extended (&s . config, &mut s . tantivy) . await ?;
      Ok (( )) } )) }

//
// The sub-tests
//

/// A buffer showing T, a forker as such, editable, with only Y (what
/// T adds to N's content), saved unchanged, writes no file: T's list
/// keeps X1, X3 and Z, which N contains or hides.
async fn an_unchanged_save_of_an_editable_forker_writes_nothing (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let before : BTreeMap<String, String> = skg_files (config) ?;
  save_cleanly (N_WITH_T_EDITABLE, "N", config, tantivy) . await ?;
  assert_eq! ( skg_files (config) ?, before, "an unchanged save writes nothing" );
  Ok (( )) }

/// N's forks folder holds K, its forkee, claiming, then its forkers
/// FF, R and T, claiming nothing; each is drawn editable. Each forker
/// shows what it adds to N's content: FF f1, R nothing, T Y.
async fn a_fresh_view_draws_forkers_after_forkees_editable (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let view : String = view_of (config, tantivy, "N") ?;
  let members : Vec<&str> = children_of (&view, "N") . into_iter ()
    . filter ( |line| line . contains ("forksFolder") ) . collect ();
  assert_eq! ( members . len (), 1, "{}", view );
  let order : Vec<String> = view . lines ()
    . filter ( |line| line . starts_with ("*** (skg node ") )
    . map ( |line| line . split ("(gnid ") . nth (1) . unwrap ()
                       . split (')') . next () . unwrap () . to_string () )
    . collect ();
  assert_eq! ( order, vec! ["K", "FF", "R", "T"],
               "the forkee leads, the forkers follow:\n{}", view );
  assert! ( ! line_of (&view, "K") . contains ("noClaim"), "{}", view );
  for (member, shown) in [ ("K", vec! ["k1"]), ("FF", vec! ["f1"]),
                           ("R", vec! []), ("T", vec! ["Y"]) ] {
    let line : &str = line_of (&view, member);
    assert! ( line . starts_with ("*** ") && ! line . contains ("writeProtected"),
              "{} is drawn editable in N's forks folder:\n{}", member, view );
    if member != "K" {
      assert! ( line . contains ("noClaim"), "{} claims nothing:\n{}", member, view ); }
    let content : Vec<&str> = content_of (&view, member);
    assert_eq! ( content . len (), shown . len (), "{}:\n{}", member, view );
    for (line, gnid) in content . iter () . zip (shown) {
      assert! ( line . contains (&format! ("(gnid {})", gnid))
                && ! line . contains ("writeProtected"),
                "{} shows {}, editable:\n{}", member, gnid, view ); }}
  Ok (( )) }

/// Under T, the folder of what N hides in it (Z) comes first, then the
/// folder of what T hides of N's content (X2), then T's forks folder,
/// then its content (Y); each folder's members are folded. K, a
/// forkee, has neither folder. The second folder's label names T as
/// 'a' and N as 'c' (its heralds, pinned with the clients').
async fn a_forkers_folders_stand_in_order (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let view : String = view_of (config, tantivy, "N") ?;
  let children : Vec<&str> = children_of (&view, "T");
  let kinds : Vec<&str> = children . iter ()
    . map ( |line| if line . contains ("hiddenInForkPartnerFolder") { "hiddenIn" }
                   else if line . contains ("hiddenByForkPartnerFolder") { "hiddenBy" }
                   else if line . contains ("forksFolder") { "forks" }
                   else if line . contains ("(gnid Y)") { "Y" }
                   else { "other" } )
    . collect ();
  assert_eq! ( kinds, vec! ["hiddenIn", "hiddenBy", "forks", "Y"], "{}", view );
  let hidden_by_at : usize = view . lines ()
    . position ( |line| line . contains ("hiddenByForkPartnerFolder") ) . unwrap ();
  let first_member : &str = view . lines () . nth (hidden_by_at + 1) . unwrap ();
  assert! ( first_member . contains ("(gnid X2)") && first_member . contains ("folded"),
            "T's folder of what it hides of N's content holds X2, folded:\n{}", view );
  assert! ( under (&view, "T") . iter () . any ( |line|
              line . contains ("(gnid Z)") && line . contains ("folded") ),
            "T's folder of what N hides in it holds Z:\n{}", view );
  assert! ( ! under (&view, "K") . iter () . any ( |line|
              line . contains ("hiddenByForkPartnerFolder")
              || line . contains ("hiddenInForkPartnerFolder") ),
            "K has neither folder:\n{}", view );
  Ok (( )) }

/// In R's own view, N stands editable in R's forks folder, and R
/// stands in N's forks folder as a repeat: write-protected, showing
/// nothing. T stands there editable, showing Y.
async fn a_repeat_forker_shows_nothing (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let view : String = view_of (config, tantivy, "R") ?;
  assert! ( ! line_of (&view, "N") . contains ("writeProtected"), "{}", view );
  let repeat : &str = view . lines ()
    . filter ( |line| line . contains ("(gnid R)") ) . nth (1)
    . unwrap_or_else ( || panic! ("R stands in N's forks folder:\n{}", view ));
  assert! ( repeat . contains ("writeProtected") && repeat . contains ("noClaim"),
            "{}", view );
  let at : usize = view . lines () . position ( |line| line == repeat ) . unwrap ();
  let next : &str = view . lines () . nth (at + 1) . unwrap ();
  assert! ( depth (next) <= depth (repeat), "the repeat shows nothing:\n{}", view );
  assert! ( content_of (&view, "T") . iter () . any ( |line| line . contains ("(gnid Y)") ),
            "{}", view );
  Ok (( )) }

/// The fresh view of N, saved unchanged, writes no file and draws
/// itself again: no forker's list loses what N contains or hides.
async fn an_unchanged_fresh_view_writes_nothing (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let view : String = view_of (config, tantivy, "N") ?;
  let before : BTreeMap<String, String> = skg_files (config) ?;
  let saved : String = save_cleanly (&view, "N", config, tantivy) . await ?;
  assert_eq! ( skg_files (config) ?, before, "an unchanged save writes nothing" );
  assert_eq! ( saved, view,
               "and draws the view again as it was" );
  assert_eq! ( members_of (& node (config, "T") ? . contains),
               ids (&["X1", "X3", "Y", "Z"]) );
  Ok (( )) }

/// T's title, edited in N's forks folder, is saved, and nothing else
/// of T changes.
async fn an_owned_forkers_title_is_saved (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let view : String = view_of (config, tantivy, "N") ?;
  let t_line : &str = line_of (&view, "T");
  let edited : String = view . replacen (
    t_line, &t_line . replace (")) T", ")) T, retitled"), 1 );
  assert_ne! ( edited, view );
  let saved : String = save_cleanly (&edited, "N", config, tantivy) . await ?;
  let t : Graphnode = node (config, "T") ?;
  assert_eq! ( t . title, "T, retitled" );
  assert_eq! ( members_of (&t . contains), ids (&["X1", "X3", "Y", "Z"]) );
  assert_eq! ( members_of (t . hides . or_default ()), ids (&["X2"]) );
  assert! ( line_of (&saved, "T") . ends_with (" T, retitled"),
            "{}", saved );
  Ok (( )) }

/// FF's title, edited in N's forks folder, is refused: FF is foreign.
async fn a_foreign_forkers_title_is_refused (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let view : String = view_of (config, tantivy, "N") ?;
  let ff_line : &str = line_of (&view, "FF");
  let edited : String = view . replacen (
    ff_line, &ff_line . replace (")) FF", ")) FF, retitled"), 1 );
  assert_ne! ( edited, view );
  let before : BTreeMap<String, String> = skg_files (config) ?;
  let refusal : String = refusal_of (&edited, "N", config, tantivy) . await ?;
  assert! ( refusal . contains ("Cannot edit title/body")
            && refusal . contains ("foreign"),
            "{}", refusal );
  assert_eq! ( skg_files (config) ?, before, "nothing is written" );
  Ok (( )) }

/// A headline typed under T, after Y, joins T's content right after
/// Y (before Z, which N hides), and the saved view shows it there.
async fn a_child_typed_under_an_owned_forker_joins_its_content (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let view : String = view_of (config, tantivy, "N") ?;
  let stars : String = "*" . repeat ( depth (line_of (&view, "Y")) );
  let typed : String = with_line_after_subtree_of (
    &view, "Y", &format! ("{} typed under T", stars) );
  let saved_view : String = save_cleanly (&typed, "N", config, tantivy) . await ?;
  let contains : Vec<ID> = members_of (& node (config, "T") ? . contains);
  assert_eq! ( contains . len (), 5, "{:?}", contains );
  assert_eq! ( ( &contains [0], &contains [1], &contains [2], &contains [4] ),
               ( &ID::from ("X1"), &ID::from ("X3"), &ID::from ("Y"), &ID::from ("Z") ),
               "the typed child follows Y: {:?}", contains );
  let typed_pid : &ID = &contains [3];
  assert_eq! ( node (config, &typed_pid . 0) ? . title, "typed under T" );
  let shown : Vec<&str> = content_of (&saved_view, "T");
  assert! ( shown . len () == 2
            && shown [1] . contains (&format! ("(gnid {})", typed_pid . 0)),
            "the saved view shows it under T, after Y:\n{}", saved_view );
  assert_eq! ( members_of (& node (config, "N") ? . contains),
               ids (&["X1", "X2", "X3", "X4"]), "N's own content is as it was" );
  Ok (( )) }

/// A headline typed under FF is refused, saying to make FF one's own
/// first; nothing is written.
async fn a_child_typed_under_a_foreign_forker_is_refused (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let view : String = view_of (config, tantivy, "N") ?;
  let stars : String = "*" . repeat ( depth (line_of (&view, "f1")) );
  let typed : String = with_line_after_subtree_of (
    &view, "f1", &format! ("{} typed under FF", stars) );
  let before : BTreeMap<String, String> = skg_files (config) ?;
  let refusal : String = refusal_of (&typed, "N", config, tantivy) . await ?;
  assert! ( refusal . contains ("Cannot add a child under FF")
            && refusal . contains ("shown as a forker")
            && refusal . contains ("make the node your own first"),
            "{}", refusal );
  assert_eq! ( skg_files (config) ?, before, "nothing is written" );
  Ok (( )) }

/// Z, which N hides and T contains, stands in T's folder of what N
/// hides in it, and not in the folder of what N hides outside its
/// fork partners, since T, a fork partner, contains it (here that
/// folder holds nothing, so is not drawn). Moved out of T's folder to
/// stand under T, claiming, Z is unhidden in N, as under a forkee.
async fn a_hide_moved_out_under_a_forker_is_unhidden (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let view : String = view_of (config, tantivy, "N") ?;
  assert! ( ! view . contains ("hiddenOutsideOfForkPartnersFolder"),
            "nothing N hides is outside its fork partners:\n{}", view );
  let hidden_in : Vec<&str> = {
    let lines : Vec<&str> = view . lines () . collect ();
    let at : usize = lines . iter ()
      . position ( |line| line . contains ("hiddenInForkPartnerFolder") ) . unwrap ();
    let below : usize = lines [at + 1 ..] . iter ()
      . take_while ( |line| depth (line) > depth (lines [at]) ) . count ();
    lines [at ..= at + below] . to_vec () };
  let z_line : &str = hidden_in [1];
  assert! ( z_line . contains ("(gnid Z)"), "{:?}", hidden_in );
  let moved : String = view
    . replacen ( &(hidden_in . join ("\n") + "\n"), "", 1 )
    . replacen (
      &format! ("{}\n", line_of (&view, "Y")),
      &format! ("{}\n{} (skg node (gnid Z) (skgRepos (nodeRepo main))) Z\n",
                line_of (&view, "Y"),
                "*" . repeat ( depth (line_of (&view, "Y")) )),
      1 );
  save_cleanly (&moved, "N", config, tantivy) . await ?;
  assert_eq! ( members_of ( node (config, "N") ? . hides . or_default () ),
               Vec::<ID>::new (), "N no longer hides Z" );
  assert_eq! ( members_of (& node (config, "T") ? . contains),
               ids (&["X1", "X3", "Y", "Z"]), "T's list is as it was" );
  Ok (( )) }

/// A buffer that draws T write-protected and bare, as a server drew it
/// before forkers as such were drawn editable, rerenders as it is: a
/// plain rerender extends nothing standing. An editable-view request
/// on N (C-c s e) extends T, through N's forks folder.
async fn a_write_protected_forker_stays_until_its_note_is_extended (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let old : String = indoc! {"
    * (skg node (gnid N) (skgRepos (nodeRepo main))) N
    ** (skg forksFolder)
    *** (skg node (gnid K) (skgRepos (nodeRepo main)) writeProtected) K
    *** (skg node (gnid FF) (skgRepos (nodeRepo theirs)) noClaim writeProtected) FF
    *** (skg node (gnid R) (skgRepos (nodeRepo main)) noClaim writeProtected) R
    *** (skg node (gnid T) (skgRepos (nodeRepo main)) noClaim writeProtected) T
    ** (skg node (gnid X1) (skgRepos (nodeRepo main))) X1
    ** (skg node (gnid X2) (skgRepos (nodeRepo main))) X2
    ** (skg node (gnid X3) (skgRepos (nodeRepo main))) X3
    ** (skg node (gnid X4) (skgRepos (nodeRepo main))) X4
    "} . to_string ();
  let before : BTreeMap<String, String> = skg_files (config) ?;
  let rerendered : String = save_cleanly (&old, "N", config, tantivy) . await ?;
  assert_eq! ( skg_files (config) ?, before, "the old buffer writes nothing" );
  assert! ( line_of (&rerendered, "T") . contains ("writeProtected")
            && under (&rerendered, "T") . is_empty (),
            "T stays write-protected and bare:\n{}", rerendered );
  let requested : String = rerendered . replacen (
    "(skg node (gnid N)", "(skg node (viewRequests editableView) (gnid N)", 1 );
  assert_ne! ( requested, rerendered );
  let extended : String = save_cleanly (&requested, "N", config, tantivy) . await ?;
  assert! ( ! line_of (&extended, "T") . contains ("writeProtected")
            && content_of (&extended, "T") . iter () . any ( |line| line . contains ("(gnid Y)") ),
            "the request on N extends T:\n{}", extended );
  assert_eq! ( skg_files (config) ?, before, "nothing is written" );
  Ok (( )) }

//
// The narrowing
//

/// N, public, contains x; T, public, forks N, and contains y by a
/// record in its private section. Unrestricted, T shows y. Narrowed
/// to "public", T's record of y is kept out, so y leaves the view; the
/// widening draws it again.
#[test]
fn a_narrowing_that_keeps_out_a_forkers_record_removes_what_it_shows (
) -> Result<(), Box<dyn Error>> {
  let case : tempfile::TempDir = tempfile::TempDir::new () ?;
  write_unchanged_skgrepos (
    case . path (),
    &[ ( "public",
         &[ ("N", "pid: N\ntitle: N\ncontains:\n- x\n"),
            ("T", "pid: T\ntitle: T\nforks:\n- N\n"),
            ("x", "pid: x\ntitle: x\n"),
            ("y", "pid: y\ntitle: y\n") ] ),
       ( "private",
         &[ ("T", "pid: T\ncontains:\n- y\n") ] ) ] ) ?;
  with_git_fixture (
    case . path (), OwnedSkgrepos::Named (&["public", "private"]), &[],
    |f : &mut Fixture| {
      let mut restriction : SkgrepoRestriction =
        SkgrepoRestriction::unrestricted (&f . config);
      let opened : String = without_viewnode_ids (
        &open_content_view (f, "N", "view of N", &restriction) ? );
      assert! ( content_of (&opened, "T") . iter () . any ( |line| line . contains ("(gnid y)") ),
                "unrestricted, T shows y:\n{}", opened );
      let narrowed : String = without_viewnode_ids (
        &switch_skgrepo_set (f, &mut restriction, "public", "view of N") ? . 0 );
      assert! ( ! narrowed . contains ("(gnid y)"),
                "narrowed, y leaves the view:\n{}", narrowed );
      let widened : String = without_viewnode_ids (
        &switch_skgrepo_set (f, &mut restriction, "private", "view of N") ? . 0 );
      assert! ( content_of (&widened, "T") . iter () . any ( |line| line . contains ("(gnid y)") ),
                "widened, T shows y again:\n{}", widened );
      Ok (( )) } ) }
