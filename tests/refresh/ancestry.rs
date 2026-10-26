// The ancestry table's rows for the property members (each under its
// property folder) and the change viewnodes (under an occurrence or a
// ghost; a body diff under an old text), and the old rows (folders,
// and the acquired-by folder under a ghost). A descriptor in a
// position its row forbids is unsayable, and is entombstoned keeping
// its children.

use super::*;
use crate::refresh::ancestry::{is_unsayable, required_ancestry, ExpectedAncestor};
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

trait KindName { fn kind_name (&self) -> &'static str; }
impl KindName for Viewnode {
  fn kind_name (&self) -> &'static str {
    match &self . kind {
      ViewnodeKind::PropertyMember (member) => member . repr_in_client (),
      _ => "other", } } }
