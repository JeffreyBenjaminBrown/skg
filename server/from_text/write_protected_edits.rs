//! Detect changes that a save would otherwise ignore because their owner is
//! rendered write-protected. The server keeps the last rendered `ViewForest`
//! for every open view, so this compares the incoming forest with that image
//! rather than guessing from the graph (which cannot represent view-local
//! folder occurrences).

use crate::from_text::local_instruction_collection::predicates::{
  active_child_counts_as_content, member_counts_for_partnerFolder};
use crate::types::errors::BufferValidationError;
use crate::types::misc::{ID, SourceName};
use crate::types::nodes::complete::FileProperty;
use crate::types::tree::forest::ViewForest;
use crate::types::viewnode::{PartnerFolder, Qual, QualFolder, Phantom, ViewNode, ViewNodeKind, Vognode};
use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::node_lookup::nodecomplete_from_graph;
use crate::types::nodes::complete::file_property_is_true;

use ego_tree::{NodeId, NodeRef};
use std::collections::HashSet;

#[derive(Clone, Debug, PartialEq, Eq)]
enum OccurrencePathStep {
  Active (ID),
  Inactive,
  DiffPhantom (ID),
  DeletedPhantom (ID),
  UnknownPhantom (ID),
  QualFolder (QualFolder),
  Alias,
  ID,
  BoolProp,
  TextChanged,
  PartnerFolder (PartnerFolder),
  DeadScaffold,
}

/// The parts of a write-protected occurrence that save extraction does not read.
/// Descendant vognodes deliberately do not contribute their own title/body
/// here: a definitive descendant remains a self-writer even below an
/// write-protected ancestor. The occurrence's own folders do contribute,
/// because their owner emits no `SetContains` or defining-folder instruction.
#[derive(Debug, PartialEq)]
struct WriteProtectedOccurrence {
  id       : ID,
  title    : String,
  source   : SourceName,
  content  : Vec<(ID, Option<SourceName>)>,
  aliases  : Option<Vec<(String, Option<SourceName>)>>,
  subscribes : Option<Vec<(ID, Option<SourceName>)>>,
  overrides  : Option<Vec<(ID, Option<SourceName>)>>,
  hidden_outside : Option<Vec<ID>>,
}

struct LocatedWriteProtectedOccurrence {
  node_id     : NodeId,
  parent_path : Vec<OccurrencePathStep>,
  state       : WriteProtectedOccurrence,
}

/// Reject edits to write-protected occurrences that were present at the same
/// location in the server's last rendering. An unmatched current occurrence
/// is new, so it is allowed; its direct Active-node children are made
/// Independent because that new occurrence cannot write a contains relation.
pub fn errors_and_normalize_new_writeProtected_occurrences (
  current  : &mut ViewForest,
  previous : &ViewForest,
) -> Vec<BufferValidationError> {
  let mut errors : Vec<BufferValidationError> =
    boolprops_surface_errors (current, previous);
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
        && current_occurrence . state . id
           == previous_occurrence . state . id )
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
         && reported . insert (current_occurrence . state . id . clone ())
      { errors . push (BufferValidationError::EditedWriteProtectedOccurrence {
          id      : previous_occurrence . state . id . clone (),
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
    let id : ID = previous_occurrence . state . id . clone ();
    if reported . insert (id . clone ()) {
      errors . push (BufferValidationError::EditedWriteProtectedOccurrence {
        id,
        title   : previous_occurrence . state . title . clone (),
        changes : write_protected_changes (
          &previous_occurrence . state,
          &current_occurrences [current_index] . state),
      }); }}

  let new_occurrence_ids : Vec<NodeId> = current_occurrences . iter ()
    . enumerate ()
    . filter ( |(index, _)| ! current_is_matched [*index] )
    . map ( |(_, occurrence)| occurrence . node_id )
    . collect ();
  make_direct_active_children_independent (current, &new_occurrence_ids);
  errors
}

fn write_protected_changes (
  previous : &WriteProtectedOccurrence,
  current  : &WriteProtectedOccurrence,
) -> Vec<String> {
  let mut changes : Vec<String> = Vec::new ();
  if previous . id != current . id { changes . push (format! (
    "changed ID from {} to {}", previous . id, current . id)); }
  if previous . title != current . title { changes . push (format! (
    "changed title from {:?} to {:?}", previous . title, current . title)); }
  if previous . source != current . source { changes . push (format! (
    "changed source from {} to {}", previous . source, current . source)); }
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
struct BoolPropsFolderSurface {
  title          : String,
  body           : Option<String>,
  rows           : Vec<(FileProperty, String, Option<String>)>,
  other_children : Vec<String>,
}

#[derive(Clone, Debug)]
struct LocatedBoolPropsSurface {
  owner_id    : ID,
  owner_title : String,
  owner_path  : Vec<OccurrencePathStep>,
  folders     : Vec<BoolPropsFolderSurface>,
}

fn boolprops_surfaces_in (forest : &ViewForest) -> Vec<LocatedBoolPropsSurface> {
  let mut result : Vec<LocatedBoolPropsSurface> = Vec::new ();
  for root in forest . roots () {
    collect_boolprops_surfaces (root, &[], &mut result); }
  result
}

fn collect_boolprops_surfaces (
  node        : NodeRef<ViewNode>,
  parent_path : &[OccurrencePathStep],
  result      : &mut Vec<LocatedBoolPropsSurface>,
) {
  let mut own_path : Vec<OccurrencePathStep> = parent_path . to_vec ();
  own_path . push (path_step (node . value ()));
  if let ViewNodeKind::Vognode (Vognode::Active (owner)) = &node . value () . kind {
    let folders : Vec<BoolPropsFolderSurface> = node . children ()
      . filter_map (|child| {
        let ViewNodeKind::QualFolder (QualFolder::BoolProps {
          title, body }) = &child . value () . kind
        else { return None; };
        let mut rows : Vec<(FileProperty, String, Option<String>)> = Vec::new ();
        let mut other_children : Vec<String> = Vec::new ();
        for leaf in child . children () {
          match &leaf . value () . kind {
            ViewNodeKind::Qual (Qual::BoolProp {
              property, title, body }) =>
              rows . push ((*property, title . clone (), body . clone ())),
            other => other_children . push (format! ("{:?}", other)), } }
        Some (BoolPropsFolderSurface {
          title : title . clone (), body : body . clone (),
          rows, other_children }) })
      . collect ();
    result . push (LocatedBoolPropsSurface {
      owner_id    : owner . id . clone (),
      owner_title : owner . title . clone (),
      owner_path  : parent_path . to_vec (),
      folders, }); }
  for child in node . children () {
    collect_boolprops_surfaces (child, &own_path, result); }
}

fn boolprops_surface_errors (
  current  : &ViewForest,
  previous : &ViewForest,
) -> Vec<BufferValidationError> {
  let current_surfaces = boolprops_surfaces_in (current);
  let previous_surfaces = boolprops_surfaces_in (previous);
  let mut errors : Vec<BufferValidationError> = Vec::new ();
  for before in &previous_surfaces {
    let Some (after) = current_surfaces . iter () . find (|surface|
      surface . owner_id == before . owner_id
      && surface . owner_path == before . owner_path)
    else { continue; };
    // Like an aliases or backpath branch, this is an optional projection:
    // deleting the whole folder dismisses it from the view and says nothing
    // about the owner's properties.  A retained folder is still
    // server-owned, so edits within it remain validation errors.
    if ! before . folders . is_empty () && after . folders . is_empty () {
      continue; }
    if before . folders == after . folders { continue; }
    let changes : Vec<String> = boolprops_surface_changes (
      &before . folders, &after . folders);
    errors . push (BufferValidationError::BoolPropsSurfaceEdited {
      owner_id    : before . owner_id . clone (),
      owner_title : before . owner_title . clone (),
      changes, }); }
  for after in &current_surfaces {
    if after . folders . is_empty () { continue; }
    let existed_before = previous_surfaces . iter () . any (|surface|
      surface . owner_id == after . owner_id
      && surface . owner_path == after . owner_path);
    if ! existed_before {
      errors . push (BufferValidationError::BoolPropsSurfaceEdited {
        owner_id    : after . owner_id . clone (),
        owner_title : after . owner_title . clone (),
        changes     : vec!["added propertiesFolder" . to_string ()], }); }
  }
  errors
}

/// Direct/internal save callers may have no last-rendered forest.  In that
/// case validate every present properties folder against graph state.  Absence
/// is fine (the user never requested the view); presence must be canonical.
pub fn boolprops_surface_errors_against_graph (
  current : &ViewForest,
  graph   : &InRustGraph,
) -> Vec<BufferValidationError> {
  let mut errors : Vec<BufferValidationError> = Vec::new ();
  for surface in boolprops_surfaces_in (current) {
    if surface . folders . is_empty () { continue; }
    let Some (node) = nodecomplete_from_graph (graph, &surface . owner_id)
    else { continue; };
    let expected_rows : Vec<(FileProperty, String, Option<String>)> =
      FileProperty::ALL
      . into_iter ()
      . filter (|property| file_property_is_true (&node . misc, *property))
      . map (|property| (property, String::new (), None))
      . collect ();
    let expected = vec![BoolPropsFolderSurface {
      title    : String::new (),
      body     : None,
      rows     : expected_rows,
      other_children : Vec::new (), }];
    if surface . folders != expected {
      errors . push (BufferValidationError::BoolPropsSurfaceEdited {
        owner_id    : node . pid . clone (),
        owner_title : node . title . clone (),
        changes     : boolprops_surface_changes (
          &expected, &surface . folders), }); }
  }
  errors
}

fn boolprops_surface_changes (
  before : &[BoolPropsFolderSurface],
  after  : &[BoolPropsFolderSurface],
) -> Vec<String> {
  if before . is_empty () && ! after . is_empty () {
    return vec!["added propertiesFolder" . to_string ()]; }
  if ! before . is_empty () && after . is_empty () {
    return vec!["removed propertiesFolder" . to_string ()]; }
  if before . len () != after . len () {
    return vec![format! ("changed propertiesFolder count from {} to {}",
                         before . len (), after . len ())]; }
  let mut changes : Vec<String> = Vec::new ();
  for (old_folder, new_folder) in before . iter () . zip (after) {
    if old_folder . title != new_folder . title {
      changes . push (format! (
        "changed propertiesFolder headline from {:?} to {:?}",
        old_folder . title, new_folder . title)); }
    describe_body_change (
      &mut changes, "propertiesFolder", &old_folder . body, &new_folder . body);
    if old_folder . other_children != new_folder . other_children {
      changes . push ("changed non-property children in propertiesFolder"
                      . to_string ()); }
    for property in FileProperty::ALL {
      let old = old_folder . rows . iter ()
        . position (|(p, _, _)| *p == property);
      let new = new_folder . rows . iter ()
        . position (|(p, _, _)| *p == property);
      match (old, new) {
        (Some (_), None) => changes . push (format! (
          "removed {}", property . wire_name ())),
        (None, Some (_)) => changes . push (format! (
          "added {}", property . wire_name ())),
        (Some (old_pos), Some (new_pos)) => {
          if old_pos != new_pos { changes . push (format! (
            "moved {} from row {} to row {}", property . wire_name (),
            old_pos + 1, new_pos + 1)); }
          let old_title = &old_folder . rows [old_pos] . 1;
          let new_title = &new_folder . rows [new_pos] . 1;
          if old_title != new_title { changes . push (format! (
            "changed {} headline from {:?} to {:?}",
            property . wire_name (), old_title, new_title)); } },
        (None, None) => (), } }
    for (property, _, old_body) in &old_folder . rows {
      if let Some ((_, _, new_body)) = new_folder . rows . iter ()
        . find (|(candidate, _, _)| candidate == property)
      { describe_body_change (
          &mut changes, property . wire_name (), old_body, new_body); } }
    for (index, ((old_property, _, _), (new_property, _, _))) in
      old_folder . rows . iter () . zip (&new_folder . rows) . enumerate ()
    { if old_property != new_property { changes . push (format! (
        "changed property metadata in row {} from {} to {}", index + 1,
        old_property . wire_name (), new_property . wire_name ())); } }
  }
  if changes . is_empty () {
    changes . push ("changed properties rows" . to_string ()); }
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
  node        : NodeRef<ViewNode>,
  parent_path : &[OccurrencePathStep],
  occurrences : &mut Vec<LocatedWriteProtectedOccurrence>,
) {
  let mut own_path : Vec<OccurrencePathStep> = parent_path . to_vec ();
  own_path . push (path_step (node . value ()));
  if let ViewNodeKind::Vognode (Vognode::Active (active)) =
    &node . value () . kind
  { if active . is_writeProtected () {
    occurrences . push (LocatedWriteProtectedOccurrence {
      node_id     : node . id (),
      parent_path : parent_path . to_vec (),
      state       : WriteProtectedOccurrence {
        id       : active . id . clone (),
        title    : active . title . clone (),
        source   : active . source . clone (),
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
  node : &ViewNode,
) -> OccurrencePathStep {
  match &node . kind {
    ViewNodeKind::Vognode (Vognode::Active (active)) =>
      OccurrencePathStep::Active (active . id . clone ()),
    ViewNodeKind::Vognode (Vognode::Inactive (_)) =>
      OccurrencePathStep::Inactive,
    ViewNodeKind::Phantom (Phantom::Diff (phantom)) =>
      OccurrencePathStep::DiffPhantom (phantom . id . clone ()),
    ViewNodeKind::Phantom (Phantom::Deleted (phantom)) =>
      OccurrencePathStep::DeletedPhantom (phantom . id . clone ()),
    ViewNodeKind::Phantom (Phantom::Unknown (phantom)) =>
      OccurrencePathStep::UnknownPhantom (phantom . id . clone ()),
    ViewNodeKind::QualFolder (folder) =>
      OccurrencePathStep::QualFolder (folder . clone ()),
    ViewNodeKind::Qual (Qual::Alias { .. }) =>
      OccurrencePathStep::Alias,
    ViewNodeKind::Qual (Qual::ID { .. }) =>
      OccurrencePathStep::ID,
    ViewNodeKind::Qual (Qual::BoolProp { .. }) =>
      OccurrencePathStep::BoolProp,
    ViewNodeKind::Qual (Qual::TextChanged { .. }) =>
      OccurrencePathStep::TextChanged,
    ViewNodeKind::PartnerFolder (folder) =>
      OccurrencePathStep::PartnerFolder (*folder),
    ViewNodeKind::DeadScaffold =>
      OccurrencePathStep::DeadScaffold,
    ViewNodeKind::BufferRoot => unreachable! (
      "the internal forest root is not traversed as an occurrence"),
  }
}

fn make_direct_active_children_independent (
  viewforest          : &mut ViewForest,
  new_occurrence_ids  : &[NodeId],
) {
  let child_ids : Vec<NodeId> = new_occurrence_ids . iter ()
    . flat_map ( |node_id| viewforest . get (*node_id)
      . into_iter ()
      . flat_map ( |node| node . children () . map ( |child| child . id ()) ))
    . collect ();
  for child_id in child_ids {
    if let Some (mut child) = viewforest . get_mut (child_id) {
      if let ViewNodeKind::Vognode (Vognode::Active (active)) =
        &mut child . value () . kind
      { active . affectsParent = crate::types::viewnode::AffectsParent::False; }} }
}

fn content_members (
  node : NodeRef<ViewNode>,
) -> Vec<(ID, Option<SourceName>)> {
  node . children () . filter_map ( |child| match &child . value () . kind {
    ViewNodeKind::Vognode (Vognode::Active (active))
      if active_child_counts_as_content (active) =>
        Some ((active . collected_id (), active . rel_source_request . clone ())),
    ViewNodeKind::Phantom (Phantom::Unknown (unknown)) =>
      Some ((unknown . id . clone (), unknown . rel_source_request . clone ())),
    _ => None,
  }) . collect ()
}

fn aliases (
  node : NodeRef<ViewNode>,
) -> Option<Vec<(String, Option<SourceName>)>> {
  node . children () . find ( |child| matches! (
    &child . value () . kind, ViewNodeKind::QualFolder (QualFolder::Alias)))
    . map ( |alias_folder| alias_folder . children () . filter_map ( |alias| {
      let ViewNodeKind::Qual (Qual::Alias { text, rel_source_request, .. }) =
        &alias . value () . kind else { return None; };
      Some ((text . clone (), rel_source_request . clone ()))
    }) . collect () )
}

fn partner_members (
  node : NodeRef<ViewNode>,
  wanted : PartnerFolder,
) -> Option<Vec<(ID, Option<SourceName>)>> {
  node . children () . find ( |child| matches! (
    &child . value () . kind, ViewNodeKind::PartnerFolder (folder) if *folder == wanted))
    . map ( |folder| folder . children () . filter_map ( |member| {
      match &member . value () . kind {
        ViewNodeKind::Vognode (Vognode::Active (active))
          if member_counts_for_partnerFolder (active) =>
            Some ((active . id . clone (), active . rel_source_request . clone ())),
        ViewNodeKind::Phantom (Phantom::Unknown (unknown)) =>
          Some ((unknown . id . clone (), unknown . rel_source_request . clone ())),
        _ => None,
      }
    }) . collect () )
}

fn hidden_outside_members (
  node : NodeRef<ViewNode>,
) -> Option<Vec<ID>> {
  node . children () . find ( |child| matches! (
    &child . value () . kind,
    ViewNodeKind::PartnerFolder (PartnerFolder::Subscribee)))
    . and_then ( |subscribee_folder| subscribee_folder . children () . find ( |child|
      matches! (&child . value () . kind,
        ViewNodeKind::PartnerFolder (PartnerFolder::HiddenOutsideOfSubscribee))))
    . map ( |hidden_outside| hidden_outside . children () . filter_map ( |member|
      match &member . value () . kind {
        ViewNodeKind::Vognode (Vognode::Active (active))
          if member_counts_for_partnerFolder (active) => Some (active . id . clone ()),
        ViewNodeKind::Phantom (Phantom::Unknown (unknown)) =>
          Some (unknown . id . clone ()),
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
      * (skg (node (id owner) (source main) writeProtected)) owner
      ** (skg (node (id content) (source main))) content
      ** (skg subscribeeFolder)
      *** (skg (node (id subscribee) (source main))) subscribee
    "});
    let mut child_text_changed = forest (indoc! {"
      * (skg (node (id owner) (source main) writeProtected (viewRequests definitiveView))) owner
      ** (skg (node (id content) (source main))) changed child text
      ** (skg subscribeeFolder)
      *** (skg (node (id subscribee) (source main))) subscribee
    "});
    assert! (errors_and_normalize_new_writeProtected_occurrences (
      &mut child_text_changed, &original) . is_empty ());

    let mut changed = forest (indoc! {"
      * (skg (node (id owner) (source main) writeProtected)) changed owner
      ** (skg (node (id other) (source main))) other content
      ** (skg subscribeeFolder)
      *** (skg (node (id other-subscribee) (source main))) other subscribee
    "});
    let errors = errors_and_normalize_new_writeProtected_occurrences (
      &mut changed, &original);
    assert! (matches! (&errors[..],
      [BufferValidationError::EditedWriteProtectedOccurrence {
        id, title, changes }] if id == &ID::from ("owner")
          && title == "owner" && changes . len () >= 2));
  }

  #[test]
  fn allows_a_new_writeProtected_occurrence_and_parks_its_viewnode_children () {
    let original = forest (indoc! {"
      * (skg (node (id root) (source main))) root
    "});
    let mut current = forest (indoc! {"
      * (skg (node (id root) (source main))) root
      ** (skg (node (id root) (source main) writeProtected)) new self occurrence
      *** (skg (node (id child) (source main))) child
    "});
    assert! (errors_and_normalize_new_writeProtected_occurrences (
      &mut current, &original) . is_empty ());
    let child = current . nodes () . find_map ( |node| match
      &node . value () . kind
    { ViewNodeKind::Vognode (Vognode::Active (active))
        if active . id == ID::from ("child") => Some (active),
      _ => None, }) . unwrap ();
    assert_eq! (child . affectsParent, AffectsParent::False);
  }

  #[test]
  fn body_on_an_writeProtected_occurrence_is_a_parse_error () {
    let (_forest, errors, _warnings) = org_to_uninterpreted_viewforest (
      indoc! {"
        * (skg (node (id owner) (source main) writeProtected)) owner
        body that would otherwise disappear
      "}) . unwrap ();
    assert! (matches! (&errors[..],
      [BufferValidationError::EditedWriteProtectedOccurrence {
        id, title, changes }] if id == &ID::from ("owner")
          && title == "owner" && changes == &vec!["added body text" . to_string ()]));
  }

  #[test]
  fn boolprops_surface_edits_report_owner_identity_and_concrete_changes () {
    let original = forest (indoc! {"
      * (skg (node (id owner) (source main))) Owner title
      ** (skg propertiesFolder)
      *** (skg (property hadId))
      *** (skg (property noSearchMatching))
    "});
    let mut changed = forest (indoc! {"
      * (skg (node (id owner) (source main))) Owner title
      ** (skg propertiesFolder) edited folder headline
      added folder body
      *** (skg (property noSearchMatching)) renamed
      added leaf body
      *** (skg (property wasOverloaded))
    "});
    let errors = errors_and_normalize_new_writeProtected_occurrences (
      &mut changed, &original);
    assert! (matches! (&errors[..],
      [BufferValidationError::BoolPropsSurfaceEdited {
        owner_id, owner_title, changes }]
      if owner_id == &ID::from ("owner")
        && owner_title == "Owner title"
        && changes . iter () . any (|c| c . contains ("removed hadId"))
        && changes . iter () . any (|c| c . contains ("added wasOverloaded"))
        && changes . iter () . any (|c| c . contains ("headline"))
        && changes . iter () . any (|c| c == "added body text to propertiesFolder")
        && changes . iter () . any (|c| c == "added body text to noSearchMatching")));
  }

  #[test]
  fn deleting_the_properties_projection_is_inert () {
    let original = forest (indoc! {"
      * (skg (node (id owner) (source main))) Owner title
      ** (skg propertiesFolder)
      *** (skg (property noSearchMatching))
    "});
    let mut without_projection = forest (indoc! {"
      * (skg (node (id owner) (source main))) Owner title
    "});
    assert! (errors_and_normalize_new_writeProtected_occurrences (
      &mut without_projection, &original) . is_empty ());
  }

  #[test]
  fn deleting_an_optional_sibling_does_not_make_the_properties_surface_edited () {
    let original = forest (indoc! {"
      * (skg (node (id owner) (source main))) Owner title
      ** (skg aliasFolder) aliases
      *** (skg alias) Another name
      ** (skg propertiesFolder)
      *** (skg (property noSearchMatching))
    "});
    let mut without_alias_projection = forest (indoc! {"
      * (skg (node (id owner) (source main))) Owner title
      ** (skg propertiesFolder)
      *** (skg (property noSearchMatching))
    "});
    assert! (errors_and_normalize_new_writeProtected_occurrences (
      &mut without_alias_projection, &original) . is_empty ());
  }
}
