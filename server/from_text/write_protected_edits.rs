//! Detect edits that a save would otherwise ignore because the occurrence
//! carrying them is write-protected (or, for a flags folder, server-owned).
//! A write-protected occurrence emits nothing at save extraction, so each is
//! judged by the graph alone: it must say what the graph says.

use crate::types::errors::BufferValidationError;
use crate::types::misc::{ID, SkgrepoName, members_of};
use crate::types::nodes::complete::{Flag, Graphnode};
use crate::types::tree::forest::ViewForest;
use crate::types::viewnode::{AffectsParent, Property, PropertyFolder, UnrestrictedVognode, Viewnode, ViewnodeKind, Vognode};
use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::node_lookup::graphnode_from_graph;
use crate::types::nodes::complete::flag_is_true;

use ego_tree::NodeRef;

/// Every write-protected occurrence of the saved view must agree with
/// the graph: its title and home skgrepo, and its alias folder when
/// present, equal the node's, and it has no member child, since a
/// write-protected occurrence cannot write its content. Non-member
/// children are the user's own and are not checked. (Its ID folder is
/// checked for every recorder, by 'find_buffer_errors_for_saving'.)
/// Every present flags folder, under any recorder, is server-owned and
/// must equal the node's flags.
pub fn errors_for_writeProtected_occurrences (
  viewforest : &ViewForest,
  graph      : &InRustGraph,
) -> Vec<BufferValidationError> {
  let mut errors : Vec<BufferValidationError> =
    flags_surface_errors_against_graph (viewforest, graph);
  for node in viewforest . roots () . flat_map ( |root| root . descendants () ) {
    let ViewnodeKind::Vognode (Vognode::Unrestricted (occurrence)) =
      &node . value () . kind else { continue; };
    if ! occurrence . is_writeProtected () { continue; }
    if let Some (graphnode) = graphnode_from_graph (graph, &occurrence . skgid) {
      let differences : Vec<String> =
        writeProtected_differences_from_graph (node, occurrence, &graphnode);
      if ! differences . is_empty () {
        errors . push (BufferValidationError::EditedWriteProtectedOccurrence {
          skgid   : occurrence . skgid . clone (),
          title   : graphnode . title . clone (),
          changes : differences, }); }}
    if node . children () . any ( |child| is_member (child . value ()) ) {
      errors . push (BufferValidationError::MemberChildOfWriteProtectedOccurrence {
        skgid : occurrence . skgid . clone (),
        title : occurrence . title . clone (), }); }}
  errors }

/// What the occurrence says that the graph does not.
fn writeProtected_differences_from_graph (
  node       : NodeRef<Viewnode>,
  occurrence : &UnrestrictedVognode,
  graphnode  : &Graphnode,
) -> Vec<String> {
  let mut differences : Vec<String> = Vec::new ();
  if occurrence . title != graphnode . title {
    differences . push (format! (
      "title {:?} differs from the graph's {:?}",
      occurrence . title, graphnode . title )); }
  if occurrence . home_skgrepo != graphnode . home_skgrepo {
    differences . push (format! (
      "home skgrepo {} differs from the graph's {}",
      occurrence . home_skgrepo, graphnode . home_skgrepo )); }
  if let Some (aliases_in_view) = aliases_of_occurrence (node) {
    let aliases_in_graph : Vec<(String, Option<SkgrepoName>)> =
      members_of (graphnode . aliases . or_default ())
      . into_iter () . map ( |alias| (alias, None) ) . collect ();
    if aliases_in_view != aliases_in_graph {
      differences . push ("aliases differ from the graph's" . to_string ()); }}
  differences }

/// The aliases in the occurrence's alias folder, in order, each with
/// its relRepo request; None if it has no alias folder.
fn aliases_of_occurrence (
  node : NodeRef<Viewnode>,
) -> Option<Vec<(String, Option<SkgrepoName>)>> {
  node . children () . find ( |child| matches! (
    &child . value () . kind, ViewnodeKind::PropertyFolder (PropertyFolder::Alias)))
    . map ( |alias_folder| alias_folder . children () . filter_map ( |alias| {
      let ViewnodeKind::Property (Property::Alias { text, relRepo_request, .. }) =
        &alias . value () . kind else { return None; };
      Some ((text . clone (), relRepo_request . clone ()))
    }) . collect () ) }

fn is_member (
  viewnode : &Viewnode,
) -> bool {
  matches! ( &viewnode . kind,
             ViewnodeKind::Vognode (Vognode::Unrestricted (child))
             if child . affectsParent == AffectsParent::True ) }

#[derive(Clone, Debug, PartialEq, Eq)]
struct FlagsFolderSurface {
  title          : String,
  body           : Option<String>,
  viewnodes      : Vec<(Flag, String, Option<String>)>,
  other_children : Vec<String>,
}

#[derive(Clone, Debug)]
struct LocatedFlagsSurface {
  recorder_skgid : ID,
  folders        : Vec<FlagsFolderSurface>,
}

fn flags_surfaces_in (forest : &ViewForest) -> Vec<LocatedFlagsSurface> {
  let mut result : Vec<LocatedFlagsSurface> = Vec::new ();
  for root in forest . roots () {
    collect_flags_surfaces (root, &mut result); }
  result
}

fn collect_flags_surfaces (
  node   : NodeRef<Viewnode>,
  result : &mut Vec<LocatedFlagsSurface>,
) {
  if let ViewnodeKind::Vognode (Vognode::Unrestricted (recorder)) = &node . value () . kind {
    let folders : Vec<FlagsFolderSurface> = node . children ()
      . filter_map (|child| {
        let ViewnodeKind::PropertyFolder (PropertyFolder::Flags {
          title, body }) = &child . value () . kind
        else { return None; };
        let mut viewnodes : Vec<(Flag, String, Option<String>)> = Vec::new ();
        let mut other_children : Vec<String> = Vec::new ();
        for leaf in child . children () {
          match &leaf . value () . kind {
            ViewnodeKind::Property (Property::Flag {
              flag, title, body, .. }) =>
              viewnodes . push ((*flag, title . clone (), body . clone ())),
            other => other_children . push (format! ("{:?}", other)), } }
        viewnodes . sort_by_key ( |(flag, _, _)| // The flags are a set.
          Flag::ALL . iter () . position ( |each| each == flag ) );
        Some (FlagsFolderSurface {
          title : title . clone (), body : body . clone (),
          viewnodes, other_children }) })
      . collect ();
    result . push (LocatedFlagsSurface {
      recorder_skgid : recorder . skgid . clone (),
      folders, }); }
  for child in node . children () {
    collect_flags_surfaces (child, result); }
}

/// Validate every present flags folder against graph state. Absence is
/// fine (it expresses no opinion); presence must be canonical, up to the
/// order of the flags.
fn flags_surface_errors_against_graph (
  current : &ViewForest,
  graph   : &InRustGraph,
) -> Vec<BufferValidationError> {
  let mut errors : Vec<BufferValidationError> = Vec::new ();
  for surface in flags_surfaces_in (current) {
    if surface . folders . is_empty () { continue; }
    let Some (node) = graphnode_from_graph (graph, &surface . recorder_skgid)
    else { continue; };
    let expected_viewnodes : Vec<(Flag, String, Option<String>)> =
      Flag::ALL
      . into_iter ()
      . filter (|flag| flag_is_true (&node . flags, *flag))
      . map (|flag| (flag, String::new (), None))
      . collect ();
    let expected = vec![FlagsFolderSurface {
      title    : String::new (),
      body     : None,
      viewnodes : expected_viewnodes,
      other_children : Vec::new (), }];
    if surface . folders != expected {
      errors . push (BufferValidationError::FlagsSurfaceEdited {
        recorder_skgid : node . pid . clone (),
        recorder_title : node . title . clone (),
        changes        : flags_surface_changes (
          &expected, &surface . folders), }); }
  }
  errors
}

fn flags_surface_changes (
  before : &[FlagsFolderSurface],
  after  : &[FlagsFolderSurface],
) -> Vec<String> {
  if before . is_empty () && ! after . is_empty () {
    return vec!["added flagsFolder" . to_string ()]; }
  if ! before . is_empty () && after . is_empty () {
    return vec!["removed flagsFolder" . to_string ()]; }
  if before . len () != after . len () {
    return vec![format! ("changed flagsFolder count from {} to {}",
                         before . len (), after . len ())]; }
  let mut changes : Vec<String> = Vec::new ();
  for (old_folder, new_folder) in before . iter () . zip (after) {
    if old_folder . title != new_folder . title {
      changes . push (format! (
        "changed flagsFolder headline from {:?} to {:?}",
        old_folder . title, new_folder . title)); }
    describe_body_change (
      &mut changes, "flagsFolder", &old_folder . body, &new_folder . body);
    if old_folder . other_children != new_folder . other_children {
      changes . push ("changed non-flag children in flagsFolder"
                      . to_string ()); }
    for flag in Flag::ALL {
      let old = old_folder . viewnodes . iter ()
        . position (|(p, _, _)| *p == flag);
      let new = new_folder . viewnodes . iter ()
        . position (|(p, _, _)| *p == flag);
      match (old, new) {
        (Some (_), None) => changes . push (format! (
          "removed {}", flag . wire_name ())),
        (None, Some (_)) => changes . push (format! (
          "added {}", flag . wire_name ())),
        (Some (old_pos), Some (new_pos)) => {
          if old_pos != new_pos { changes . push (format! (
            "moved {} from position {} to position {} in flagsFolder", flag . wire_name (),
            old_pos + 1, new_pos + 1)); }
          let old_title = &old_folder . viewnodes [old_pos] . 1;
          let new_title = &new_folder . viewnodes [new_pos] . 1;
          if old_title != new_title { changes . push (format! (
            "changed {} headline from {:?} to {:?}",
            flag . wire_name (), old_title, new_title)); } },
        (None, None) => (), } }
    for (flag, _, old_body) in &old_folder . viewnodes {
      if let Some ((_, _, new_body)) = new_folder . viewnodes . iter ()
        . find (|(candidate, _, _)| candidate == flag)
      { describe_body_change (
          &mut changes, flag . wire_name (), old_body, new_body); } }
    for (index, ((old_flag, _, _), (new_flag, _, _))) in
      old_folder . viewnodes . iter () . zip (&new_folder . viewnodes) . enumerate ()
    { if old_flag != new_flag { changes . push (format! (
        "changed flag metadata in viewnode {} from {} to {}", index + 1,
        old_flag . wire_name (), new_flag . wire_name ())); } }
  }
  if changes . is_empty () {
    changes . push ("changed flags viewnodes" . to_string ()); }
  changes
}

fn describe_body_change (
  changes : &mut Vec<String>,
  label   : &str,
  before  : &Option<String>,
  after   : &Option<String>,
) {
  if before == after { return; }
  let description = match (before, after) {
    (None, Some (_)) => format! ("added body text to {}", label),
    (Some (_), None) => format! ("removed body text from {}", label),
    (Some (_), Some (_)) => format! ("changed body text on {}", label),
    (None, None) => return, };
  changes . push (description);
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::from_text::buffer_to_viewnodes::uninterpreted
    ::org_to_uninterpreted_viewforest;
  use crate::types::maybe_placed_viewnode::maybePlaced_to_placed_viewforest;
  use crate::types::misc::{MSV, rel_partners_at_relRepo};
  use crate::types::nodes::complete::empty_graphnode;
  use indoc::indoc;

  fn forest (text : &str) -> ViewForest {
    let (forest, errors, _warnings) =
      org_to_uninterpreted_viewforest (text) . unwrap ();
    assert! (errors . is_empty (), "unexpected parse errors: {:?}", errors);
    maybePlaced_to_placed_viewforest (forest) . unwrap ()
  }

  /// root contains recorder, which contains c1 then c2, and has the
  /// alias "another name" and the flag noSearchMatching.
  fn graph () -> InRustGraph {
    let main : SkgrepoName = SkgrepoName::from ("main");
    let node = |pid : &str| -> Graphnode { Graphnode {
      pid          : ID::from (pid),
      title        : pid . to_string (),
      home_skgrepo : main . clone (),
      .. empty_graphnode () }};
    InRustGraph::from_graphnodes (&[
      Graphnode {
        contains : rel_partners_at_relRepo (&main, vec! [ID::from ("recorder")]),
        .. node ("root") },
      Graphnode {
        contains : rel_partners_at_relRepo (
          &main, vec! [ID::from ("c1"), ID::from ("c2")]),
        aliases  : MSV::Specified (rel_partners_at_relRepo (
          &main, vec! ["another name" . to_string ()])),
        flags    : vec! [Flag::NoSearchMatching],
        .. node ("recorder") },
      node ("c1"),
      node ("c2"),
      node ("typed") ]) }

  fn errors_in (text : &str) -> Vec<BufferValidationError> {
    errors_for_writeProtected_occurrences (&forest (text), &graph ()) }

  #[test]
  fn an_occurrence_that_agrees_with_the_graph_passes () {
    assert_eq! (errors_in (indoc! {"
      * (skg (node (id root) (repo main))) root
      ** (skg (node (id recorder) (repo main) writeProtected)) recorder
      *** (skg aliasFolder)
      **** (skg alias) another name
      *** (skg flagsFolder)
      **** (skg (flag noSearchMatching))
    "}), vec! []);
  }

  #[test]
  fn a_member_child_under_a_writeProtected_occurrence_is_refused () {
    let errors : Vec<BufferValidationError> = errors_in (indoc! {"
      * (skg (node (id root) (repo main))) root
      ** (skg (node (id recorder) (repo main) writeProtected)) recorder
      *** (skg (node (id typed) (repo main))) typed
    "});
    assert! (matches! (&errors[..],
      [BufferValidationError::MemberChildOfWriteProtectedOccurrence {
        skgid, title }] if skgid == &ID::from ("recorder") && title == "recorder"),
      "{:?}", errors);
    assert! (errors [0] . to_string () . contains ("C-c s d"), "{}", errors [0]);
  }

  #[test]
  fn a_member_child_under_a_requested_writeProtected_occurrence_is_refused () {
    // The editable-view request does not license the typed child.
    let errors : Vec<BufferValidationError> = errors_in (indoc! {"
      * (skg (node (id recorder) (repo main) writeProtected (viewRequests editableView))) recorder
      ** (skg (node (id typed) (repo main))) typed
    "});
    assert! (matches! (&errors[..],
      [BufferValidationError::MemberChildOfWriteProtectedOccurrence {
        skgid, .. }] if skgid == &ID::from ("recorder")),
      "{:?}", errors);
  }

  #[test]
  fn reordered_non_member_children_pass () {
    // The graph has c1 before c2; non-members are the user's own.
    assert_eq! (errors_in (indoc! {"
      * (skg (node (id recorder) (repo main) writeProtected)) recorder
      ** (skg (node (id c2) (repo main) (affectsParent false))) c2
      ** (skg (node (id c1) (repo main) (affectsParent false))) c1
    "}), vec! []);
  }

  #[test]
  fn a_changed_title_is_refused () {
    let errors : Vec<BufferValidationError> = errors_in (indoc! {"
      * (skg (node (id recorder) (repo main) writeProtected)) changed
    "});
    assert! (matches! (&errors[..],
      [BufferValidationError::EditedWriteProtectedOccurrence {
        skgid, title, changes }] if skgid == &ID::from ("recorder")
          && title == "recorder"
          && changes == &vec! ["title \"changed\" differs from the graph's \"recorder\"" . to_string ()]),
      "{:?}", errors);
    assert! (errors [0] . to_string () . contains ("differs from the graph"),
             "{}", errors [0]);
  }

  #[test]
  fn an_alias_folder_that_differs_is_refused_and_an_absent_one_passes () {
    for differing in [
      "*** (skg alias) other name\n",
      "*** (skg alias) another name\n*** (skg alias) one more\n",
      "*** (skg alias (editRequest (relRepo main))) another name\n", ]
    { let text : String = format! (
        "* (skg (node (id recorder) (repo main) writeProtected)) recorder\n** (skg aliasFolder)\n{}",
        differing);
      let errors : Vec<BufferValidationError> = errors_in (&text);
      assert! (matches! (&errors[..],
        [BufferValidationError::EditedWriteProtectedOccurrence {
          changes, .. }] if changes == &vec! ["aliases differ from the graph's" . to_string ()]),
        "{}: {:?}", text, errors); }
    assert_eq! (errors_in (
      "* (skg (node (id recorder) (repo main) writeProtected)) recorder\n"),
      vec! []);
  }

  #[test]
  fn an_editable_occurrence_is_not_judged_by_the_graph () {
    assert_eq! (errors_in (indoc! {"
      * (skg (node (id recorder) (repo main))) changed
      ** (skg aliasFolder)
      *** (skg alias) other name
      ** (skg (node (id typed) (repo main))) typed
    "}), vec! []);
  }

  #[test]
  fn body_on_an_writeProtected_occurrence_is_a_parse_error () {
    let (_forest, errors, _warnings) = org_to_uninterpreted_viewforest (
      indoc! {"
        * (skg (node (id recorder) (repo main) writeProtected)) recorder
        body that would otherwise disappear
      "}) . unwrap ();
    assert! (matches! (&errors[..],
      [BufferValidationError::EditedWriteProtectedOccurrence {
        skgid, title, changes }] if skgid == &ID::from ("recorder")
          && title == "recorder" && changes == &vec!["added body text" . to_string ()]));
  }

  #[test]
  fn flags_surface_edits_report_recorder_identity_and_concrete_changes () {
    let errors : Vec<BufferValidationError> = errors_in (indoc! {"
      * (skg (node (id recorder) (repo main))) recorder
      ** (skg flagsFolder) edited folder headline
      added folder body
      *** (skg (flag noSearchMatching)) renamed
      added leaf body
      *** (skg (flag wasOverloaded))
    "});
    assert! (matches! (&errors[..],
      [BufferValidationError::FlagsSurfaceEdited {
        recorder_skgid, recorder_title, changes }]
      if recorder_skgid == &ID::from ("recorder")
        && recorder_title == "recorder"
        && changes . iter () . any (|c| c . contains ("added wasOverloaded"))
        && changes . iter () . any (|c| c . contains ("headline"))
        && changes . iter () . any (|c| c == "added body text to flagsFolder")
        && changes . iter () . any (|c| c == "added body text to noSearchMatching")),
      "{:?}", errors);
  }

  #[test]
  fn an_absent_flags_folder_passes () {
    assert_eq! (errors_in (
      "* (skg (node (id recorder) (repo main))) recorder\n"), vec! []);
  }
}
