//! Detect changes that a save would otherwise ignore because their owner is
//! rendered indefinitively. The server keeps the last rendered `ViewForest`
//! for every open view, so this compares the incoming forest with that image
//! rather than guessing from the graph (which cannot represent view-local
//! collection occurrences).

use crate::from_text::local_instruction_collection::predicates::{
  active_child_counts_as_content, member_counts_for_partnerCol};
use crate::types::errors::BufferValidationError;
use crate::types::misc::{ID, SourceName};
use crate::types::tree::forest::ViewForest;
use crate::types::viewnode::{PartnerCol, Qual, QualCol, Phantom, ViewNode, ViewNodeKind, Vognode};

use ego_tree::{NodeId, NodeRef};
use std::collections::HashSet;

#[derive(Clone, Debug, PartialEq, Eq)]
enum OccurrencePathStep {
  Active (ID),
  Inactive,
  DiffPhantom (ID),
  DeletedPhantom (ID),
  UnknownPhantom (ID),
  QualCol (QualCol),
  Alias,
  ID,
  TextChanged,
  PartnerCol (PartnerCol),
  DeadScaffold,
}

/// The parts of an indefinitive occurrence that save extraction does not read.
/// Descendant vognodes deliberately do not contribute their own title/body
/// here: a definitive descendant remains a self-writer even below an
/// indefinitive ancestor. The occurrence's own collections do contribute,
/// because their owner emits no `SetContains` or defining-col instruction.
#[derive(Debug, PartialEq)]
struct IndefinitiveOccurrence {
  id       : ID,
  title    : String,
  source   : SourceName,
  content  : Vec<(ID, Option<SourceName>)>,
  aliases  : Option<Vec<(String, Option<SourceName>)>>,
  subscribes : Option<Vec<(ID, Option<SourceName>)>>,
  overrides  : Option<Vec<(ID, Option<SourceName>)>>,
  hidden_outside : Option<Vec<ID>>,
}

struct LocatedIndefinitiveOccurrence {
  node_id     : NodeId,
  parent_path : Vec<OccurrencePathStep>,
  state       : IndefinitiveOccurrence,
}

/// Reject edits to indefinitive occurrences that were present at the same
/// location in the server's last rendering. An unmatched current occurrence
/// is new, so it is allowed; its direct Active-node children are made
/// Independent because that new occurrence cannot write a contains relation.
pub fn errors_and_normalize_new_indefinitive_occurrences (
  current  : &mut ViewForest,
  previous : &ViewForest,
) -> Vec<BufferValidationError> {
  let current_occurrences : Vec<LocatedIndefinitiveOccurrence> =
    occurrences_in (current);
  let previous_occurrences : Vec<LocatedIndefinitiveOccurrence> =
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
      let current_occurrence : &LocatedIndefinitiveOccurrence =
        & current_occurrences [current_index];
      if current_occurrence . state != previous_occurrence . state
         && reported . insert (current_occurrence . state . id . clone ())
      { errors . push (BufferValidationError::EditedIndefinitive (
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
      errors . push (BufferValidationError::EditedIndefinitive (id)); }}

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
) -> Vec<LocatedIndefinitiveOccurrence> {
  let mut occurrences : Vec<LocatedIndefinitiveOccurrence> = Vec::new ();
  for root in viewforest . roots () {
    collect_occurrences (root, &[], &mut occurrences); }
  occurrences
}

fn collect_occurrences (
  node        : NodeRef<ViewNode>,
  parent_path : &[OccurrencePathStep],
  occurrences : &mut Vec<LocatedIndefinitiveOccurrence>,
) {
  let mut own_path : Vec<OccurrencePathStep> = parent_path . to_vec ();
  own_path . push (path_step (node . value ()));
  if let ViewNodeKind::Vognode (Vognode::Active (active)) =
    &node . value () . kind
  { if active . is_indefinitive () {
    occurrences . push (LocatedIndefinitiveOccurrence {
      node_id     : node . id (),
      parent_path : parent_path . to_vec (),
      state       : IndefinitiveOccurrence {
        id       : active . id . clone (),
        title    : active . title . clone (),
        source   : active . source . clone (),
        content  : content_members (node),
        aliases  : aliases (node),
        subscribes : partner_members (node, PartnerCol::Subscribee),
        overrides  : partner_members (node, PartnerCol::Overridden),
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
    ViewNodeKind::QualCol (col) =>
      OccurrencePathStep::QualCol (*col),
    ViewNodeKind::Qual (Qual::Alias { .. }) =>
      OccurrencePathStep::Alias,
    ViewNodeKind::Qual (Qual::ID { .. }) =>
      OccurrencePathStep::ID,
    ViewNodeKind::Qual (Qual::TextChanged { .. }) =>
      OccurrencePathStep::TextChanged,
    ViewNodeKind::PartnerCol (col) =>
      OccurrencePathStep::PartnerCol (*col),
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
      { active . parentIs = crate::types::viewnode::ParentIs::Independent; }} }
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
    &child . value () . kind, ViewNodeKind::QualCol (QualCol::Alias)))
    . map ( |alias_col| alias_col . children () . filter_map ( |alias| {
      let ViewNodeKind::Qual (Qual::Alias { text, rel_source_request, .. }) =
        &alias . value () . kind else { return None; };
      Some ((text . clone (), rel_source_request . clone ()))
    }) . collect () )
}

fn partner_members (
  node : NodeRef<ViewNode>,
  wanted : PartnerCol,
) -> Option<Vec<(ID, Option<SourceName>)>> {
  node . children () . find ( |child| matches! (
    &child . value () . kind, ViewNodeKind::PartnerCol (col) if *col == wanted))
    . map ( |col| col . children () . filter_map ( |member| {
      match &member . value () . kind {
        ViewNodeKind::Vognode (Vognode::Active (active))
          if member_counts_for_partnerCol (active) =>
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
    ViewNodeKind::PartnerCol (PartnerCol::Subscribee)))
    . and_then ( |subscribee_col| subscribee_col . children () . find ( |child|
      matches! (&child . value () . kind,
        ViewNodeKind::PartnerCol (PartnerCol::HiddenOutsideOfSubscribee))))
    . map ( |hidden_outside| hidden_outside . children () . filter_map ( |member|
      match &member . value () . kind {
        ViewNodeKind::Vognode (Vognode::Active (active))
          if member_counts_for_partnerCol (active) => Some (active . id . clone ()),
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
  use crate::types::viewnode::ParentIs;
  use indoc::indoc;

  fn forest (text : &str) -> ViewForest {
    let (forest, errors, _warnings) =
      org_to_uninterpreted_viewforest (text) . unwrap ();
    assert! (errors . is_empty (), "unexpected parse errors: {:?}", errors);
    maybePlaced_to_placed_viewforest (forest) . unwrap ()
  }

  #[test]
  fn catches_title_content_and_writable_collection_edits_but_not_child_text () {
    let original = forest (indoc! {"
      * (skg (node (id owner) (source main) indef)) owner
      ** (skg (node (id content) (source main))) content
      ** (skg subscribeeCol)
      *** (skg (node (id subscribee) (source main))) subscribee
    "});
    let mut child_text_changed = forest (indoc! {"
      * (skg (node (id owner) (source main) indef (viewRequests definitiveView))) owner
      ** (skg (node (id content) (source main))) changed child text
      ** (skg subscribeeCol)
      *** (skg (node (id subscribee) (source main))) subscribee
    "});
    assert! (errors_and_normalize_new_indefinitive_occurrences (
      &mut child_text_changed, &original) . is_empty ());

    let mut changed = forest (indoc! {"
      * (skg (node (id owner) (source main) indef)) changed owner
      ** (skg (node (id other) (source main))) other content
      ** (skg subscribeeCol)
      *** (skg (node (id other-subscribee) (source main))) other subscribee
    "});
    assert_eq! (
      errors_and_normalize_new_indefinitive_occurrences (
        &mut changed, &original),
      vec! [BufferValidationError::EditedIndefinitive (ID::from ("owner"))]);
  }

  #[test]
  fn allows_a_new_indefinitive_occurrence_and_parks_its_viewnode_children () {
    let original = forest (indoc! {"
      * (skg (node (id root) (source main))) root
    "});
    let mut current = forest (indoc! {"
      * (skg (node (id root) (source main))) root
      ** (skg (node (id root) (source main) indef)) new self occurrence
      *** (skg (node (id child) (source main))) child
    "});
    assert! (errors_and_normalize_new_indefinitive_occurrences (
      &mut current, &original) . is_empty ());
    let child = current . nodes () . find_map ( |node| match
      &node . value () . kind
    { ViewNodeKind::Vognode (Vognode::Active (active))
        if active . id == ID::from ("child") => Some (active),
      _ => None, }) . unwrap ();
    assert_eq! (child . parentIs, ParentIs::Independent);
  }

  #[test]
  fn body_on_an_indefinitive_occurrence_is_a_parse_error () {
    let (_forest, errors, _warnings) = org_to_uninterpreted_viewforest (
      indoc! {"
        * (skg (node (id owner) (source main) indef)) owner
        body that would otherwise disappear
      "}) . unwrap ();
    assert_eq! (errors,
      vec! [BufferValidationError::EditedIndefinitive (ID::from ("owner"))]);
  }
}
