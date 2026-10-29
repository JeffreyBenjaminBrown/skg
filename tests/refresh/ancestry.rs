// The ancestry table's rows for the property members (each under its
// property folder) and the change viewnodes (under an occurrence or a
// ghost; a body diff under an old text), and the old rows (folders,
// and the acquired-by folder under a ghost). A descriptor in a
// position its row forbids is unsayable, and is entombstoned keeping
// its children.

use super::*;
use crate::refresh::ancestry::{
  CompoundFolderParties, ExpectedAncestor, compound_folder_parties, described_node,
  is_unsayable, required_ancestors, required_ancestry};
use crate::types::viewnode::{ChangesetMarkerKind, Folder};
use indoc::formatdoc;

#[test]
fn the_rows_of_the_property_members_and_the_change_viewnodes () {
  let row = |kind : ViewnodeKind| -> Vec<ExpectedAncestor> {
    required_ancestry (&kind) . to_vec () };
  assert_eq! ( row ( ViewnodeKind::PropertyMember ( PropertyMember::alias (
                 "a" . into (), None, None ))),
               vec! [ ExpectedAncestor::Folder (Folder::Alias) ] );
  assert_eq! ( row ( ViewnodeKind::PropertyMember ( PropertyMember::id (ID::from ("i")) )),
               vec! [ ExpectedAncestor::Folder (Folder::ID) ] );
  assert! ( matches! (
    row ( ViewnodeKind::ChangeViewnode ( ChangeViewnode::BodyDiff { diff : String::new () } ))
      . as_slice (),
    [ ExpectedAncestor::OldText ] ));
  assert_eq! ( row ( ViewnodeKind::ChangeViewnode ( ChangeViewnode::ChangesetMarker {
                 kind : ChangesetMarkerKind::Content } )),
               vec! [ ExpectedAncestor::OccurrenceOrGhost ] );
  assert_eq! ( row ( ViewnodeKind::Folder (Folder::AcquiredBy) ),
               vec! [ ExpectedAncestor::Ghost ] );
  assert! ( row ( ViewnodeKind::Tombstone ) . is_empty () ); }

#[test]
fn a_property_member_outside_its_folder_is_unsayable () {
  let forest : ViewForest = parse ( &formatdoc! ("
    * {} p
    ** (skg aliasFolder)
    *** (skg alias) a
    *** (skg id) p
    ** (skg flagsFolder)
    *** (skg (flag noSearchMatching))
    ", occurrence_metadata ("p", "") ), IdResolution::AsWritten );
  let unsayable : Vec<String> = forest . nodes ()
    . filter ( |node| matches! ( node . value () . kind, ViewnodeKind::PropertyMember (_) ))
    . map ( |node| format! ( "{} {}", node . value () . kind_name (),
                             is_unsayable (&forest, node . id ()) ))
    . collect ();
  assert_eq! ( unsayable, vec! [ "alias false", "id true", "flag false" ] ); }

#[test]
fn an_unsayable_property_member_is_entombstoned () {
  let graph : InRustGraph = graph (&[ node ("p") ]);
  let input : String = formatdoc! ("
    * {} p
    ** (skg aliasFolder)
    *** (skg id) p
    ", occurrence_metadata ("p", "") );
  let run : Run = run_asserting_fixpoint (&input, &graph);
  assert_eq! ( run . outline (), "\
node p
  aliasFolder" );
  assert_eq! ( run . buffer_correction_kinds (), vec! [ BufferCorrectionKind::Entombstoned ] ); }

#[test]
fn a_body_diff_stands_only_under_an_old_text () {
  let forest : ViewForest = parse ( &formatdoc! ("
    * {} p
    ** (skg (oldText body))
    *** (skg bodyDiff)
    ** (skg bodyDiff)
    ", occurrence_metadata ("p", "") ), IdResolution::AsWritten );
  let unsayable : Vec<bool> = forest . nodes ()
    . filter ( |node| matches! ( node . value () . kind,
      ViewnodeKind::ChangeViewnode (ChangeViewnode::BodyDiff { .. }) ))
    . map ( |node| is_unsayable (&forest, node . id ()) )
    . collect ();
  assert_eq! ( unsayable, vec! [ false, true ] ); }

/// The deepest rows: the folder of what a subscriber hides in one
/// subscribee needs that subscribee, the subscribee folder and the
/// subscriber above it; the folder of what it hides outside its
/// subscribees needs the subscribee folder and the subscriber. Where
/// the subscriber is a ghost, both are unsayable, though their
/// viewparents are of the right kinds.
#[test]
fn the_deep_rows_of_the_hidden_folders_reach_the_subscriber () {
  let chain = |subscriber : &str| -> ViewForest { parse ( &formatdoc! ("
    * {} s
    ** (skg subscribeeFolder)
    *** {} e
    **** (skg hiddenInSubscribeeFolder)
    *** (skg hiddenOutsideOfSubscribeeFolder)
    ", subscriber, occurrence_metadata ("e", "writeProtected") ),
    IdResolution::AsWritten ) };
  let folder = |forest : &ViewForest, wanted : Folder| -> NodeId {
    forest . nodes ()
    . find ( |node| matches! ( &node . value () . kind, ViewnodeKind::Folder (folder)
                               if *folder == wanted ))
    . unwrap () . id () };
  { let forest : ViewForest = chain (&occurrence_metadata ("s", ""));
    let hidden_in : NodeId = folder (&forest, Folder::HiddenInSubscribee);
    let hidden_outside : NodeId = folder (&forest, Folder::HiddenOutsideOfSubscribee);
    assert! ( ! is_unsayable (&forest, hidden_in) );
    assert! ( ! is_unsayable (&forest, hidden_outside) );
    let title_at = |treeid : Option<NodeId>| -> Option<String> {
      treeid . map ( |treeid| outline_line (&forest, treeid) ) };
    assert_eq! ( required_ancestors (&forest, hidden_in) . into_iter ()
                 . map ( |treeid| outline_line (&forest, treeid) )
                 . collect::<Vec<String>> (),
                 vec! [ "node e wp", "subscribeeFolder", "node s" ] );
    let parties : CompoundFolderParties =
      compound_folder_parties (&forest, hidden_in) . unwrap ();
    assert_eq! ( title_at ( Some (parties . subscriber) ), Some ("node s" . to_string ()) );
    assert_eq! ( title_at ( parties . subscribee ), Some ("node e wp" . to_string ()) );
    let parties : CompoundFolderParties =
      compound_folder_parties (&forest, hidden_outside) . unwrap ();
    assert_eq! ( title_at ( Some (parties . subscriber) ), Some ("node s" . to_string ()) );
    assert_eq! ( parties . subscribee, None );
    assert_eq! ( title_at ( described_node (&forest, hidden_in) ),
                 Some ("node e wp" . to_string ()) );
    assert_eq! ( title_at ( described_node (&forest, hidden_outside) ),
                 Some ("node s" . to_string ()) ); }
  { let forest : ViewForest = chain (
      "(skg (vnid ss) ghost (gnid s) (skgRepos (nodeRepo main)))" );
    assert! ( is_unsayable ( &forest, folder (&forest, Folder::HiddenInSubscribee) ),
              "its viewparent is an occurrence, but its subscriber is a ghost" );
    assert! ( is_unsayable ( &forest, folder (&forest, Folder::HiddenOutsideOfSubscribee) ),
              "its viewparent is a subscribee folder, but that folder's is a ghost" ); }}

/// An occurrence has no row: wherever it stands it stays an
/// occurrence, so it keeps standing for its node. Here its viewparents
/// become tombstones (a hidden-outside folder not in a subscribee
/// folder, which is unsayable, and an old text, which describes
/// nothing in a content view); it does not.
#[test]
fn an_occurrence_is_never_unsayable () {
  let graph : InRustGraph = graph (&[ node ("p"), node ("c") ]);
  let input : String = formatdoc! ("
    * {} p
    ** (skg hiddenOutsideOfSubscribeeFolder)
    *** {} c
    ** (skg (oldText title)) old
    *** {} c
    ", occurrence_metadata ("p", ""), occurrence_metadata ("c", "writeProtected"),
       occurrence_metadata ("c", "writeProtected") );
  let forest : ViewForest = parse (&input, IdResolution::AsWritten);
  let unsayable : Vec<String> = forest . nodes ()
    . map ( |node| format! ( "{} {}", outline_line (&forest, node . id ()),
                             is_unsayable (&forest, node . id ()) ))
    . collect ();
  assert_eq! ( unsayable, vec! [ "node p false",
                                 "hiddenOutsideOfSubscribeeFolder true",
                                 "node c wp false",
                                 "oldText false",
                                 "node c wp false" ] );
  let run : Run = run_asserting_fixpoint (&input, &graph);
  assert_eq! ( run . outline (), "\
node p
  tombstone
    node c wp noClaim
  tombstone
    node c wp noClaim" ); }

trait KindName { fn kind_name (&self) -> &'static str; }
impl KindName for Viewnode {
  fn kind_name (&self) -> &'static str {
    match &self . kind {
      ViewnodeKind::PropertyMember (member) => member . repr_in_client (),
      _ => "other", } } }
