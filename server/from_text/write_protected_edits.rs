//! Detect changes that a save would otherwise ignore because their owner is
//! rendered write-protected. The server keeps the last rendered `ViewForest`
//! for every open view, so this compares the incoming forest with that image
//! rather than guessing from the graph (which cannot represent view-local
//! folder occurrences).

use crate::from_text::local_instruction_collection::predicates::{
  active_child_counts_as_content, member_counts_for_partnerFolder};
use crate::types::errors::BufferValidationError;
use crate::types::misc::{ID, SourceName};
use crate::types::tree::forest::ViewForest;
use crate::types::viewnode::{PartnerFolder, Qual, QualFolder, Phantom, ViewNode, ViewNodeKind, Vognode};

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
  let current_occurrences : Vec<LocatedWriteProtectedOccurrence> =
    occurrences_in (current);
  let previous_occurrences : Vec<LocatedWriteProtectedOccurrence> =
    occurrences_in (previous);
  let mut current_is_matched : Vec<bool> =
    vec! [false; current_occurrences . len ()];
  let mut previous_is_matched : Vec<bool> =
    vec! [false; previous_occurrences . len ()];
  let mut reported : HashSet<ID> = HashSet::new ();
  let mut errors : Vec<BufferValidationError> = Vec::new ();

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
      { errors . push (BufferValidationError::EditedWriteProtectedOccurrence (
          current_occurrence . state . id . clone ())); }} }

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
    let id : ID = current_occurrences [current_index] . state . id . clone ();
    if reported . insert (id . clone ()) {
      errors . push (BufferValidationError::EditedWriteProtectedOccurrence (id)); }}

  let new_occurrence_ids : Vec<NodeId> = current_occurrences . iter ()
    . enumerate ()
    . filter ( |(index, _)| ! current_is_matched [*index] )
    . map ( |(_, occurrence)| occurrence . node_id )
    . collect ();
  make_direct_active_children_independent (current, &new_occurrence_ids);
  errors
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
      OccurrencePathStep::QualFolder (*folder),
    ViewNodeKind::Qual (Qual::Alias { .. }) =>
      OccurrencePathStep::Alias,
    ViewNodeKind::Qual (Qual::ID { .. }) =>
      OccurrencePathStep::ID,
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
    assert_eq! (
      errors_and_normalize_new_writeProtected_occurrences (
        &mut changed, &original),
      vec! [BufferValidationError::EditedWriteProtectedOccurrence (ID::from ("owner"))]);
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
    assert_eq! (errors,
      vec! [BufferValidationError::EditedWriteProtectedOccurrence (ID::from ("owner"))]);
  }
}
