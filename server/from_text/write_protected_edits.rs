//! Detect changes that a save would otherwise ignore because their recorder is
//! rendered write-protected. The server keeps the last rendered `ViewForest`
//! for every open view, so this compares the incoming forest with that image
//! rather than guessing from the graph (which cannot represent view-local
//! folder occurrences).

use crate::from_text::local_fieldintent_collection::predicates::{
  unrestricted_child_counts_as_content, member_counts_for_partnerFolder};
use crate::types::errors::BufferValidationError;
use crate::types::misc::{ID, SkgrepoName};
use crate::types::nodes::complete::Flag;
use crate::types::tree::forest::ViewForest;
use crate::types::viewnode::{PartnerFolder, Property, PropertyFolder, Phantom, Viewnode, ViewnodeKind, Vognode};
use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::node_lookup::graphnode_from_graph;
use crate::types::nodes::complete::flag_is_true;

use ego_tree::{NodeId, NodeRef};
use std::collections::HashSet;

#[derive(Clone, Debug, PartialEq, Eq)]
enum OccurrencePathStep {
  Unrestricted (ID),
  Restricted,
  DiffPhantom (ID),
  DeletedPhantom (ID),
  UnknownPhantom (ID),
  PropertyFolder (PropertyFolder),
  Alias,
  ID,
  Flag,
  TextChanged,
  PartnerFolder (PartnerFolder),
  DeadViewnode,
}

/// The parts of a write-protected occurrence that save extraction does not read.
/// Descendant vognodes deliberately do not contribute their own title/body
/// here: an editable descendant remains a self-writer even below an
/// write-protected ancestor. The occurrence's own folders do contribute,
/// because their recorder emits no `SetContains` or defining-folder instruction.
#[derive(Debug, PartialEq)]
struct WriteProtectedOccurrence {
  skgid       : ID,
  title    : String,
  home_skgrepo : SkgrepoName,
  content  : Vec<(ID, Option<SkgrepoName>)>,
  aliases  : Option<Vec<(String, Option<SkgrepoName>)>>,
  subscribes : Option<Vec<(ID, Option<SkgrepoName>)>>,
  overrides  : Option<Vec<(ID, Option<SkgrepoName>)>>,
  hidden_outside : Option<Vec<ID>>,
}

struct LocatedWriteProtectedOccurrence {
  treeid      : NodeId,
  parent_path : Vec<OccurrencePathStep>,
  state       : WriteProtectedOccurrence,
}

/// Reject edits to write-protected occurrences that were present at the same
/// location in the server's last rendering. An unmatched current occurrence
/// is new, so it is allowed; its direct Unrestricted-node children are made
/// non-members because that new occurrence cannot write a contains relation.
pub fn errors_and_normalize_new_writeProtected_occurrences (
  current  : &mut ViewForest,
  previous : &ViewForest,
) -> Vec<BufferValidationError> {
  let mut errors : Vec<BufferValidationError> =
    flags_surface_errors (current, previous);
  let current_occurrences : Vec<LocatedWriteProtectedOccurrence> =
    occurrences_in (current);
  let previous_occurrences : Vec<LocatedWriteProtectedOccurrence> =
    occurrences_in (previous);
  let mut current_is_matched : Vec<bool> =
    vec! [false; current_occurrences . len ()];
  let mut previous_is_matched : Vec<bool> =
    vec! [false; previous_occurrences . len ()];
  let mut reported : HashSet<ID> = HashSet::new ();

  // First preserve exact occurrence identity (same parent path and ID),
  // preferring an unchanged duplicate if more than one candidate exists.
  for (previous_index, previous_occurrence)
    in previous_occurrences . iter () . enumerate ()
  { let matching_indices : Vec<usize> = current_occurrences . iter ()
      . enumerate ()
      . filter ( |(current_index, current_occurrence)|
        ! current_is_matched [*current_index]
        && current_occurrence . parent_path
           == previous_occurrence . parent_path
        && current_occurrence . state . skgid
           == previous_occurrence . state . skgid )
      . map ( |(index, _)| index )
      . collect ();
    let chosen : Option<usize> = matching_indices . iter ()
      . find ( |index| current_occurrences [**index] . state
                       == previous_occurrence . state )
      . copied ()
      . or_else ( || matching_indices . first () . copied () );
    if let Some (current_index) = chosen {
      current_is_matched [current_index] = true;
      previous_is_matched [previous_index] = true;
      let current_occurrence : &LocatedWriteProtectedOccurrence =
        & current_occurrences [current_index];
      if current_occurrence . state != previous_occurrence . state
         && reported . insert (current_occurrence . state . skgid . clone ())
      { errors . push (BufferValidationError::EditedWriteProtectedOccurrence {
          skgid   : previous_occurrence . state . skgid . clone (),
          title   : previous_occurrence . state . title . clone (),
          changes : write_protected_changes (
            &previous_occurrence . state, &current_occurrence . state),
        }); }} }

  // If an occurrence's ID itself changed, its parent path is the remaining
  // stable location signal. Pair unmatched old/current occurrences there.
  for (previous_index, previous_occurrence)
    in previous_occurrences . iter () . enumerate ()
  { if previous_is_matched [previous_index] { continue; }
    let Some (current_index) = current_occurrences . iter ()
      . enumerate ()
      . find ( |(current_index, current_occurrence)|
        ! current_is_matched [*current_index]
        && current_occurrence . parent_path
           == previous_occurrence . parent_path )
      . map ( |(index, _)| index )
      else { continue; };
    current_is_matched [current_index] = true;
    previous_is_matched [previous_index] = true;
    let skgid : ID = previous_occurrence . state . skgid . clone ();
    if reported . insert (skgid . clone ()) {
      errors . push (BufferValidationError::EditedWriteProtectedOccurrence {
        skgid,
        title   : previous_occurrence . state . title . clone (),
        changes : write_protected_changes (
          &previous_occurrence . state,
          &current_occurrences [current_index] . state),
      }); }}

  let new_occurrence_skgids : Vec<NodeId> = current_occurrences . iter ()
    . enumerate ()
    . filter ( |(index, _)| ! current_is_matched [*index] )
    . map ( |(_, occurrence)| occurrence . treeid )
    . collect ();
  make_direct_unrestricted_children_independent (current, &new_occurrence_skgids);
  errors
}

fn write_protected_changes (
  previous : &WriteProtectedOccurrence,
  current  : &WriteProtectedOccurrence,
) -> Vec<String> {
  let mut changes : Vec<String> = Vec::new ();
  if previous . skgid != current . skgid { changes . push (format! (
    "changed ID from {} to {}", previous . skgid, current . skgid)); }
  if previous . title != current . title { changes . push (format! (
    "changed title from {:?} to {:?}", previous . title, current . title)); }
  if previous . home_skgrepo != current . home_skgrepo { changes . push (format! (
    "changed repo from {} to {}", previous . home_skgrepo, current . home_skgrepo)); }
  if previous . content != current . content {
    changes . push ("changed content membership" . to_string ()); }
  if previous . aliases != current . aliases {
    changes . push ("changed aliases" . to_string ()); }
  if previous . subscribes != current . subscribes {
    changes . push ("changed subscriptions" . to_string ()); }
  if previous . overrides != current . overrides {
    changes . push ("changed overrides" . to_string ()); }
  if previous . hidden_outside != current . hidden_outside {
    changes . push ("changed hidden subscription content" . to_string ()); }
  if changes . is_empty () {
    changes . push ("changed occurrence identity or placement" . to_string ()); }
  changes
}

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
  recorder_title : String,
  recorder_path  : Vec<OccurrencePathStep>,
  folders        : Vec<FlagsFolderSurface>,
}

fn flags_surfaces_in (forest : &ViewForest) -> Vec<LocatedFlagsSurface> {
  let mut result : Vec<LocatedFlagsSurface> = Vec::new ();
  for root in forest . roots () {
    collect_flags_surfaces (root, &[], &mut result); }
  result
}

fn collect_flags_surfaces (
  node        : NodeRef<Viewnode>,
  parent_path : &[OccurrencePathStep],
  result      : &mut Vec<LocatedFlagsSurface>,
) {
  let mut own_path : Vec<OccurrencePathStep> = parent_path . to_vec ();
  own_path . push (path_step (node . value ()));
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
              flag, title, body }) =>
              viewnodes . push ((*flag, title . clone (), body . clone ())),
            other => other_children . push (format! ("{:?}", other)), } }
        Some (FlagsFolderSurface {
          title : title . clone (), body : body . clone (),
          viewnodes, other_children }) })
      . collect ();
    result . push (LocatedFlagsSurface {
      recorder_skgid    : recorder . skgid . clone (),
      recorder_title : recorder . title . clone (),
      recorder_path  : parent_path . to_vec (),
      folders, }); }
  for child in node . children () {
    collect_flags_surfaces (child, &own_path, result); }
}

fn flags_surface_errors (
  current  : &ViewForest,
  previous : &ViewForest,
) -> Vec<BufferValidationError> {
  let current_surfaces = flags_surfaces_in (current);
  let previous_surfaces = flags_surfaces_in (previous);
  let mut errors : Vec<BufferValidationError> = Vec::new ();
  for before in &previous_surfaces {
    let Some (after) = current_surfaces . iter () . find (|surface|
      surface . recorder_skgid == before . recorder_skgid
      && surface . recorder_path == before . recorder_path)
    else { continue; };
    // Like an aliases or role tree branch, this is an optional viewbranch:
    // deleting the whole folder dismisses it from the view and says nothing
    // about the recorder's flags.  A retained folder is still
    // server-owned, so edits within it remain validation errors.
    if ! before . folders . is_empty () && after . folders . is_empty () {
      continue; }
    if before . folders == after . folders { continue; }
    let changes : Vec<String> = flags_surface_changes (
      &before . folders, &after . folders);
    errors . push (BufferValidationError::FlagsSurfaceEdited {
      recorder_skgid    : before . recorder_skgid . clone (),
      recorder_title : before . recorder_title . clone (),
      changes, }); }
  for after in &current_surfaces {
    if after . folders . is_empty () { continue; }
    let existed_before = previous_surfaces . iter () . any (|surface|
      surface . recorder_skgid == after . recorder_skgid
      && surface . recorder_path == after . recorder_path);
    if ! existed_before {
      errors . push (BufferValidationError::FlagsSurfaceEdited {
        recorder_skgid : after . recorder_skgid . clone (),
        recorder_title : after . recorder_title . clone (),
        changes        : vec!["added flagsFolder" . to_string ()], }); }
  }
  errors
}

/// Direct/internal save callers may have no last-rendered forest.  In that
/// case validate every present flags folder against graph state.  Absence
/// is fine (the user never requested the view); presence must be canonical.
pub fn flags_surface_errors_against_graph (
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

fn occurrences_in (
  viewforest : &ViewForest,
) -> Vec<LocatedWriteProtectedOccurrence> {
  let mut occurrences : Vec<LocatedWriteProtectedOccurrence> = Vec::new ();
  for root in viewforest . roots () {
    collect_occurrences (root, &[], &mut occurrences); }
  occurrences
}

fn collect_occurrences (
  node        : NodeRef<Viewnode>,
  parent_path : &[OccurrencePathStep],
  occurrences : &mut Vec<LocatedWriteProtectedOccurrence>,
) {
  let mut own_path : Vec<OccurrencePathStep> = parent_path . to_vec ();
  own_path . push (path_step (node . value ()));
  if let ViewnodeKind::Vognode (Vognode::Unrestricted (restriction)) =
    &node . value () . kind
  { if restriction . is_writeProtected () {
    occurrences . push (LocatedWriteProtectedOccurrence {
      treeid      : node . id (),
      parent_path : parent_path . to_vec (),
      state       : WriteProtectedOccurrence {
        skgid       : restriction . skgid . clone (),
        title    : restriction . title . clone (),
        home_skgrepo   : restriction . home_skgrepo . clone (),
        content  : content_members (node),
        aliases  : aliases (node),
        subscribes : partner_members (node, PartnerFolder::Subscribee),
        overrides  : partner_members (node, PartnerFolder::Overridden),
        hidden_outside : hidden_outside_members (node),
      }}); }}
  for child in node . children () {
    collect_occurrences (child, &own_path, occurrences); }
}

fn path_step (
  node : &Viewnode,
) -> OccurrencePathStep {
  match &node . kind {
    ViewnodeKind::Vognode (Vognode::Unrestricted (restriction)) =>
      OccurrencePathStep::Unrestricted (restriction . skgid . clone ()),
    ViewnodeKind::Vognode (Vognode::Restricted (_)) =>
      OccurrencePathStep::Restricted,
    ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Diff (phantom))) =>
      OccurrencePathStep::DiffPhantom (phantom . skgid . clone ()),
    ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Deleted (phantom))) =>
      OccurrencePathStep::DeletedPhantom (phantom . skgid . clone ()),
    ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Unknown (phantom))) =>
      OccurrencePathStep::UnknownPhantom (phantom . skgid . clone ()),
    ViewnodeKind::PropertyFolder (folder) =>
      OccurrencePathStep::PropertyFolder (folder . clone ()),
    ViewnodeKind::Property (Property::Alias { .. }) =>
      OccurrencePathStep::Alias,
    ViewnodeKind::Property (Property::ID { .. }) =>
      OccurrencePathStep::ID,
    ViewnodeKind::Property (Property::Flag { .. }) =>
      OccurrencePathStep::Flag,
    ViewnodeKind::Property (Property::TextChanged { .. }) =>
      OccurrencePathStep::TextChanged,
    ViewnodeKind::PartnerFolder (folder) =>
      OccurrencePathStep::PartnerFolder (*folder),
    ViewnodeKind::DeadViewnode =>
      OccurrencePathStep::DeadViewnode,
    ViewnodeKind::BufferRoot => unreachable! (
      "the internal forest root is not traversed as an occurrence"),
  }
}

fn make_direct_unrestricted_children_independent (
  viewforest            : &mut ViewForest,
  new_occurrence_skgids : &[NodeId],
) {
  let child_skgids : Vec<NodeId> = new_occurrence_skgids . iter ()
    . flat_map ( |treeid| viewforest . get (*treeid)
      . into_iter ()
      . flat_map ( |node| node . children () . map ( |child| child . id ()) ))
    . collect ();
  for child_skgid in child_skgids {
    if let Some (mut child) = viewforest . get_mut (child_skgid) {
      if let ViewnodeKind::Vognode (Vognode::Unrestricted (restriction)) =
        &mut child . value () . kind
      { restriction . affectsParent = crate::types::viewnode::AffectsParent::False; }} }
}

fn content_members (
  node : NodeRef<Viewnode>,
) -> Vec<(ID, Option<SkgrepoName>)> {
  node . children () . filter_map ( |child| match &child . value () . kind {
    ViewnodeKind::Vognode (Vognode::Unrestricted (restriction))
      if unrestricted_child_counts_as_content (restriction) =>
        Some ((restriction . collected_skgid (), restriction . relRepo_request . clone ())),
    ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Unknown (unknown))) =>
      Some ((unknown . skgid . clone (), unknown . relRepo_request . clone ())),
    _ => None,
  }) . collect ()
}

fn aliases (
  node : NodeRef<Viewnode>,
) -> Option<Vec<(String, Option<SkgrepoName>)>> {
  node . children () . find ( |child| matches! (
    &child . value () . kind, ViewnodeKind::PropertyFolder (PropertyFolder::Alias)))
    . map ( |alias_folder| alias_folder . children () . filter_map ( |alias| {
      let ViewnodeKind::Property (Property::Alias { text, relRepo_request, .. }) =
        &alias . value () . kind else { return None; };
      Some ((text . clone (), relRepo_request . clone ()))
    }) . collect () )
}

fn partner_members (
  node : NodeRef<Viewnode>,
  wanted : PartnerFolder,
) -> Option<Vec<(ID, Option<SkgrepoName>)>> {
  node . children () . find ( |child| matches! (
    &child . value () . kind, ViewnodeKind::PartnerFolder (folder) if *folder == wanted))
    . map ( |folder| folder . children () . filter_map ( |member| {
      match &member . value () . kind {
        ViewnodeKind::Vognode (Vognode::Unrestricted (restriction))
          if member_counts_for_partnerFolder (restriction) =>
            Some ((restriction . skgid . clone (), restriction . relRepo_request . clone ())),
        ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Unknown (unknown))) =>
          Some ((unknown . skgid . clone (), unknown . relRepo_request . clone ())),
        _ => None,
      }
    }) . collect () )
}

fn hidden_outside_members (
  node : NodeRef<Viewnode>,
) -> Option<Vec<ID>> {
  node . children () . find ( |child| matches! (
    &child . value () . kind,
    ViewnodeKind::PartnerFolder (PartnerFolder::Subscribee)))
    . and_then ( |subscribee_folder| subscribee_folder . children () . find ( |child|
      matches! (&child . value () . kind,
        ViewnodeKind::PartnerFolder (PartnerFolder::HiddenOutsideOfSubscribee))))
    . map ( |hidden_outside| hidden_outside . children () . filter_map ( |member|
      match &member . value () . kind {
        ViewnodeKind::Vognode (Vognode::Unrestricted (restriction))
          if member_counts_for_partnerFolder (restriction) => Some (restriction . skgid . clone ()),
        ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Unknown (unknown))) =>
          Some (unknown . skgid . clone ()),
        _ => None,
      }) . collect () )
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::from_text::buffer_to_viewnodes::uninterpreted
    ::org_to_uninterpreted_viewforest;
  use crate::types::maybe_placed_viewnode::maybePlaced_to_placed_viewforest;
  use crate::types::viewnode::AffectsParent;
  use indoc::indoc;

  fn forest (text : &str) -> ViewForest {
    let (forest, errors, _warnings) =
      org_to_uninterpreted_viewforest (text) . unwrap ();
    assert! (errors . is_empty (), "unexpected parse errors: {:?}", errors);
    maybePlaced_to_placed_viewforest (forest) . unwrap ()
  }

  #[test]
  fn catches_title_content_and_writable_folder_edits_but_not_child_text () {
    let original = forest (indoc! {"
      * (skg (node (id recorder) (repo main) writeProtected)) recorder
      ** (skg (node (id content) (repo main))) content
      ** (skg subscribeeFolder)
      *** (skg (node (id subscribee) (repo main))) subscribee
    "});
    let mut child_text_changed = forest (indoc! {"
      * (skg (node (id recorder) (repo main) writeProtected (viewRequests editableView))) recorder
      ** (skg (node (id content) (repo main))) changed child text
      ** (skg subscribeeFolder)
      *** (skg (node (id subscribee) (repo main))) subscribee
    "});
    assert! (errors_and_normalize_new_writeProtected_occurrences (
      &mut child_text_changed, &original) . is_empty ());

    let mut changed = forest (indoc! {"
      * (skg (node (id recorder) (repo main) writeProtected)) changed recorder
      ** (skg (node (id other) (repo main))) other content
      ** (skg subscribeeFolder)
      *** (skg (node (id other-subscribee) (repo main))) other subscribee
    "});
    let errors = errors_and_normalize_new_writeProtected_occurrences (
      &mut changed, &original);
    assert! (matches! (&errors[..],
      [BufferValidationError::EditedWriteProtectedOccurrence {
        skgid, title, changes }] if skgid == &ID::from ("recorder")
          && title == "recorder" && changes . len () >= 2));
  }

  #[test]
  fn allows_a_new_writeProtected_occurrence_and_parks_its_viewnode_children () {
    let original = forest (indoc! {"
      * (skg (node (id root) (repo main))) root
    "});
    let mut current = forest (indoc! {"
      * (skg (node (id root) (repo main))) root
      ** (skg (node (id root) (repo main) writeProtected)) new self occurrence
      *** (skg (node (id child) (repo main))) child
    "});
    assert! (errors_and_normalize_new_writeProtected_occurrences (
      &mut current, &original) . is_empty ());
    let child = current . nodes () . find_map ( |node| match
      &node . value () . kind
    { ViewnodeKind::Vognode (Vognode::Unrestricted (restriction))
        if restriction . skgid == ID::from ("child") => Some (restriction),
      _ => None, }) . unwrap ();
    assert_eq! (child . affectsParent, AffectsParent::False);
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
    let original = forest (indoc! {"
      * (skg (node (id recorder) (repo main))) Recorder title
      ** (skg flagsFolder)
      *** (skg (flag hadId))
      *** (skg (flag noSearchMatching))
    "});
    let mut changed = forest (indoc! {"
      * (skg (node (id recorder) (repo main))) Recorder title
      ** (skg flagsFolder) edited folder headline
      added folder body
      *** (skg (flag noSearchMatching)) renamed
      added leaf body
      *** (skg (flag wasOverloaded))
    "});
    let errors = errors_and_normalize_new_writeProtected_occurrences (
      &mut changed, &original);
    assert! (matches! (&errors[..],
      [BufferValidationError::FlagsSurfaceEdited {
        recorder_skgid, recorder_title, changes }]
      if recorder_skgid == &ID::from ("recorder")
        && recorder_title == "Recorder title"
        && changes . iter () . any (|c| c . contains ("removed hadId"))
        && changes . iter () . any (|c| c . contains ("added wasOverloaded"))
        && changes . iter () . any (|c| c . contains ("headline"))
        && changes . iter () . any (|c| c == "added body text to flagsFolder")
        && changes . iter () . any (|c| c == "added body text to noSearchMatching")));
  }

  #[test]
  fn deleting_the_flags_viewbranch_is_inert () {
    let original = forest (indoc! {"
      * (skg (node (id recorder) (repo main))) Recorder title
      ** (skg flagsFolder)
      *** (skg (flag noSearchMatching))
    "});
    let mut without_viewbranch = forest (indoc! {"
      * (skg (node (id recorder) (repo main))) Recorder title
    "});
    assert! (errors_and_normalize_new_writeProtected_occurrences (
      &mut without_viewbranch, &original) . is_empty ());
  }

  #[test]
  fn deleting_an_optional_sibling_does_not_make_the_flags_surface_edited () {
    let original = forest (indoc! {"
      * (skg (node (id recorder) (repo main))) Recorder title
      ** (skg aliasFolder) aliases
      *** (skg alias) Another name
      ** (skg flagsFolder)
      *** (skg (flag noSearchMatching))
    "});
    let mut without_alias_viewbranch = forest (indoc! {"
      * (skg (node (id recorder) (repo main))) Recorder title
      ** (skg flagsFolder)
      *** (skg (flag noSearchMatching))
    "});
    assert! (errors_and_normalize_new_writeProtected_occurrences (
      &mut without_alias_viewbranch, &original) . is_empty ());
  }
}
