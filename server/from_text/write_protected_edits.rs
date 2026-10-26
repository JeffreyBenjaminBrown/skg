//! Detect edits that a save would otherwise ignore because the occurrence
//! carrying them is write-protected (or, for a flags folder, server-owned).
//! A write-protected occurrence emits nothing at save extraction, so each is
//! judged by the graph alone: it must say what the graph says.

use crate::types::errors::{BufferValidationError, HeadlineName};
use crate::types::misc::{ID, SkgConfig, SkgrepoName, members_of};
use crate::types::nodes::complete::{Flag, Graphnode};
use crate::types::tree::forest::ViewForest;
use crate::types::viewnode::{RelationshipEditRequest, Folder, PropertyMember, UnrestrictedVognode, Viewnode, ViewnodeKind, Vognode};
use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::node_lookup::graphnode_from_graph;
use crate::to_org::complete::partner_folder::child_data::skgid_a_folder_child_stands_for;
use crate::skgrepo_sets::SkgrepoRestriction;
use crate::types::nodes::complete::flag_is_true;
use crate::update_buffer::reconcile::partner_folder::partnerFolder_members_in_graph;

use ego_tree::NodeRef;
use std::collections::HashSet;

/// Every write-protected occurrence of the saved view must agree with
/// the graph: its title and home skgrepo, and its alias folder and
/// partner folders when present, equal the node's, and it has no
/// member child, since a write-protected occurrence cannot write its
/// content. Non-member children are the user's own and are not
/// checked. (Its ID folder is checked for every recorder, by
/// 'find_buffer_errors_for_saving'.) Every present flags folder, under
/// any recorder, is server-owned and must equal the node's flags.
pub fn errors_for_writeProtected_occurrences (
  viewforest          : &ViewForest,
  graph               : &InRustGraph,
  config              : &SkgConfig,
  skgrepo_restriction : Option<&SkgrepoRestriction>,
) -> Vec<BufferValidationError> {
  let mut errors : Vec<BufferValidationError> =
    flags_surface_errors_against_graph (viewforest, graph);
  for node in viewforest . nodes () {
    let ViewnodeKind::Vognode (Vognode::Unrestricted (occurrence)) =
      &node . value () . kind else { continue; };
    if ! occurrence . is_writeProtected () { continue; }
    if let Some (graphnode) = graphnode_from_graph (graph, &occurrence . skgid) {
      let differences : Vec<String> =
        writeProtected_differences_from_graph (
          node, occurrence, &graphnode, graph, config, skgrepo_restriction );
      if ! differences . is_empty () {
        errors . push (BufferValidationError::EditedWriteProtectedOccurrence {
          skgid    : occurrence . skgid . clone (),
          title    : graphnode . title . clone (),
          changes  : differences,
          headline : node . value () . headline_name (), }); }}
    if node . children () . any ( |child| is_member (child . value ()) ) {
      errors . push (BufferValidationError::MemberChildOfWriteProtectedOccurrence {
        skgid    : occurrence . skgid . clone (),
        title    : occurrence . title . clone (),
        headline : node . value () . headline_name (), }); }}
  errors }

/// What the occurrence says that the graph does not.
fn writeProtected_differences_from_graph (
  node                : NodeRef<Viewnode>,
  occurrence          : &UnrestrictedVognode,
  graphnode           : &Graphnode,
  graph               : &InRustGraph,
  config              : &SkgConfig,
  skgrepo_restriction : Option<&SkgrepoRestriction>,
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
  differences . extend (
    partnerFolder_differences_from_graph (
      node, graphnode, graph, config, skgrepo_restriction ));
  differences }

/// One change per partner folder under the occurrence whose members
/// are not the node's partners in the folder's relation, as the folder
/// reconciler would show them, or of which a member carries a relRepo
/// request. An absent folder, and a folder with no relation member
/// role, says nothing.
fn partnerFolder_differences_from_graph (
  node                : NodeRef<Viewnode>,
  graphnode           : &Graphnode,
  graph               : &InRustGraph,
  config              : &SkgConfig,
  skgrepo_restriction : Option<&SkgrepoRestriction>,
) -> Vec<String> {
  node . children () . filter_map ( |folder| {
    let ViewnodeKind::Folder (kind) = &folder . value () . kind
      else { return None; };
    if kind . members_are_properties () { return None; }
    let members_in_graph : HashSet<ID> =
      partnerFolder_members_in_graph (
        graph, config, &graphnode . pid, kind, skgrepo_restriction ) ?
      . into_iter () . collect ();
    let members_in_view : Vec<(ID, Option<SkgrepoName>)> =
      partnerFolder_members_in_view (folder, graph);
    let differs : bool =
      members_in_view . iter () . any ( |(_, request)| request . is_some () )
      || members_in_view . into_iter () . map ( |(skgid, _)| skgid )
         . collect::<HashSet<ID>> () != members_in_graph;
    differs . then ( || changed_partners_phrase (kind) . to_string () ) } )
    . collect () }

/// The members of a partner folder, as the check of a write-protected
/// occurrence's folder counts them, each with its relRepo request and
/// by the ID the folder reconciler matches it by
/// ('skgid_a_folder_child_stands_for'). A member is a child that
/// claims the node has that partner now, which the check compares
/// with the graph:
/// - an unrestricted vognode that affects the folder;
/// - an unknown phantom (a partner no node has) that claims
///   membership.
/// Every other child is the user's or the diff view's and is not
/// counted. In particular a removed member, of either kind, is not:
/// it stands for a relationship the before-graph had and the
/// after-graph lacks, so counting it would refuse the folder the diff
/// view rendered. The diff view shows a removed member as a non-member
/// unrestricted vognode with '(rel removed)', or, when the partner
/// is dangling, as an unknown phantom with that part; each carries
/// 'noClaim', which excludes it.
fn partnerFolder_members_in_view (
  folder : NodeRef<Viewnode>,
  graph  : &InRustGraph,
) -> Vec<(ID, Option<SkgrepoName>)> {
  folder . children () . filter_map ( |child| {
    let relRepo_request : Option<SkgrepoName> = match &child . value () . kind {
      ViewnodeKind::Vognode (Vognode::Unrestricted (member))
        if member . claimsMembership =>
        RelationshipEditRequest::relRepo_of (&member . relationshipEditRequest) . cloned (),
      ViewnodeKind::Vognode (Vognode::Unknown (unknown))
        if unknown . claimsMembership =>
        RelationshipEditRequest::relRepo_of (&unknown . relationshipEditRequest) . cloned (),
      _ => return None, };
    Some (( skgid_a_folder_child_stands_for (child . value (), graph) ?,
            relRepo_request )) } )
    . collect () }

/// How an error names a change to a partner folder of this kind.
fn changed_partners_phrase (
  kind : &Folder,
) -> &'static str {
  match kind {
    Folder::Subscribee                => "changed subscriptions",
    Folder::Subscriber                => "changed subscribers",
    Folder::Overridden                => "changed overrides",
    Folder::Overrider                 => "changed overriders",
    Folder::Hider                     => "changed hiders",
    Folder::Hidden                    => "changed hidden nodes",
    Folder::Container                 => "changed containers",
    Folder::Mentioner                 => "changed mentioners",
    Folder::HiddenInSubscribee // Not checked: these have no
      | Folder::HiddenOutsideOfSubscribee // relation member role.
      | Folder::AcquiredBy
      | Folder::Acquired              => "changed memberships",
    Folder::Alias | Folder::ID | Folder::Flags { .. } => // not partner folders
      "changed properties", }}

/// The aliases in the occurrence's alias folder, in order, each with
/// its relRepo request; None if it has no alias folder.
fn aliases_of_occurrence (
  node : NodeRef<Viewnode>,
) -> Option<Vec<(String, Option<SkgrepoName>)>> {
  node . children () . find ( |child| matches! (
    &child . value () . kind, ViewnodeKind::Folder (Folder::Alias)))
    . map ( |alias_folder| alias_folder . children () . filter_map ( |alias| {
      let ViewnodeKind::PropertyMember (PropertyMember::Alias {
        text, relationshipEditRequest, claimsMembership, .. }) =
        &alias . value () . kind else { return None; };
      if ! claimsMembership { // one the worktree lost
        return None; }
      Some ((text . clone (),
             RelationshipEditRequest::relRepo_of (relationshipEditRequest) . cloned ()))
    }) . collect () ) }

fn is_member (
  viewnode : &Viewnode,
) -> bool {
  matches! ( &viewnode . kind,
             ViewnodeKind::Vognode (Vognode::Unrestricted (child))
             if child . claimsMembership ) }

#[derive(Clone, Debug, PartialEq, Eq)]
struct FlagsFolderSurface {
  title          : String,
  body           : Option<String>,
  viewnodes      : Vec<(Flag, String, Option<String>)>,
  other_children : Vec<String>,
}

#[derive(Clone, Debug)]
struct LocatedFlagsSurface {
  recorder_skgid    : ID,
  recorder_headline : HeadlineName,
  folders           : Vec<FlagsFolderSurface>,
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
        let ViewnodeKind::Folder (Folder::Flags {
          title, body }) = &child . value () . kind
        else { return None; };
        let mut viewnodes : Vec<(Flag, String, Option<String>)> = Vec::new ();
        let mut other_children : Vec<String> = Vec::new ();
        for leaf in child . children () {
          match &leaf . value () . kind {
            ViewnodeKind::PropertyMember (PropertyMember::Flag {
              flag, title, body, .. }) =>
              viewnodes . push ((*flag, title . clone (), body . clone ())),
            // A tombstone (a stale flag holding something) and a
            // non-member standing in the folder are no part of its surface.
            ViewnodeKind::Tombstone => (),
            ViewnodeKind::Vognode (Vognode::Unrestricted (t))
              if ! t . claimsMembership => (),
            other => other_children . push (format! ("{:?}", other)), } }
        viewnodes . sort_by_key ( |(flag, _, _)| // The flags are a set.
          Flag::ALL . iter () . position ( |each| each == flag ) );
        Some (FlagsFolderSurface {
          title : title . clone (), body : body . clone (),
          viewnodes, other_children }) })
      . collect ();
    result . push (LocatedFlagsSurface {
      recorder_skgid    : recorder . skgid . clone (),
      recorder_headline : node . value () . headline_name (),
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
          &expected, &surface . folders),
        headline       : surface . recorder_headline . clone (), }); }
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
  use crate::dbs::in_rust_graph::InRustGraph;
  use crate::from_text::buffer_to_viewnodes::uninterpreted
    ::org_to_viewforest;
  use crate::types::misc::{MSV, rel_partners_at_relRepo};
  use crate::skgrepo_sets::SkgrepoSetName;
  use crate::types::misc::Skgrepo;
  use crate::types::nodes::complete::empty_graphnode;
  use indoc::indoc;
  use std::collections::{BTreeSet, HashMap};
  use std::path::PathBuf;

  fn forest (text : &str) -> ViewForest {
    let (forest, errors, _warnings, _provenance) =
      org_to_viewforest (text, &InRustGraph::new ()) . unwrap ();
    assert! (errors . is_empty (), "unexpected parse errors: {:?}", errors);
    forest
  }

  /// root contains recorder, which contains c1 then c2, has the alias
  /// "another name" and the flag noSearchMatching, and subscribes to
  /// sub and to private_sub, whose home is the skgrepo private.
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
        subscribesTo : MSV::Specified (rel_partners_at_relRepo (
          &main, vec! [ID::from ("sub"), ID::from ("private_sub")])),
        .. node ("recorder") },
      node ("c1"),
      node ("c2"),
      node ("typed"),
      node ("sub"),
      Graphnode {
        home_skgrepo : SkgrepoName::from ("private"),
        .. node ("private_sub") } ]) }

  fn config () -> SkgConfig {
    SkgConfig::dummyFromSkgrepos ( ["main", "private"] . into_iter ()
      . map ( |name| ( SkgrepoName::from (name), Skgrepo {
        name         : SkgrepoName::from (name),
        abbreviation : None,
        path         : PathBuf::from (name),
        owned        : true, } ))
      . collect::<HashMap<SkgrepoName, Skgrepo>> () ) }

  fn errors_in (text : &str) -> Vec<BufferValidationError> {
    errors_for_writeProtected_occurrences (
      &forest (text), &graph (), &config (), None ) }

  /// The changes the one refusal of 'errors' names.
  fn refused_changes (
    errors : &[BufferValidationError],
  ) -> Vec<String> {
    match errors {
      [BufferValidationError::EditedWriteProtectedOccurrence {
        skgid, changes, .. }] if skgid == &ID::from ("recorder") =>
        changes . clone (),
      _ => panic! ("expected one refusal of recorder, got {:?}", errors), }}

  #[test]
  fn an_occurrence_that_agrees_with_the_graph_passes () {
    assert_eq! (errors_in (indoc! {"
      * (skg node (gnid root) (skgRepos (nodeRepo main))) root
      ** (skg node (gnid recorder) (skgRepos (nodeRepo main)) writeProtected) recorder
      *** (skg aliasFolder)
      **** (skg alias) another name
      *** (skg flagsFolder)
      **** (skg (flag noSearchMatching))
    "}), vec! []);
  }

  #[test]
  fn a_member_child_under_a_writeProtected_occurrence_is_refused () {
    let errors : Vec<BufferValidationError> = errors_in (indoc! {"
      * (skg node (gnid root) (skgRepos (nodeRepo main))) root
      ** (skg node (gnid recorder) (skgRepos (nodeRepo main)) writeProtected) recorder
      *** (skg node (gnid typed) (skgRepos (nodeRepo main))) typed
    "});
    assert! (matches! (&errors[..],
      [BufferValidationError::MemberChildOfWriteProtectedOccurrence {
        skgid, title, .. }] if skgid == &ID::from ("recorder") && title == "recorder"),
      "{:?}", errors);
    assert! (errors [0] . to_string () . contains ("C-c s d"), "{}", errors [0]);
  }

  #[test]
  fn a_member_child_under_a_requested_writeProtected_occurrence_is_refused () {
    // The editable-view request does not license the typed child.
    let errors : Vec<BufferValidationError> = errors_in (indoc! {"
      * (skg node (gnid recorder) (skgRepos (nodeRepo main)) writeProtected (viewRequests editableView)) recorder
      ** (skg node (gnid typed) (skgRepos (nodeRepo main))) typed
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
      * (skg node (gnid recorder) (skgRepos (nodeRepo main)) writeProtected) recorder
      ** (skg node (gnid c2) (skgRepos (nodeRepo main)) noClaim) c2
      ** (skg node (gnid c1) (skgRepos (nodeRepo main)) noClaim) c1
    "}), vec! []);
  }

  #[test]
  fn a_changed_title_is_refused () {
    let errors : Vec<BufferValidationError> = errors_in (indoc! {"
      * (skg node (gnid recorder) (skgRepos (nodeRepo main)) writeProtected) changed
    "});
    assert! (matches! (&errors[..],
      [BufferValidationError::EditedWriteProtectedOccurrence {
        skgid, title, changes, .. }] if skgid == &ID::from ("recorder")
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
        "* (skg node (gnid recorder) (skgRepos (nodeRepo main)) writeProtected) recorder\n** (skg aliasFolder)\n{}",
        differing);
      let errors : Vec<BufferValidationError> = errors_in (&text);
      assert! (matches! (&errors[..],
        [BufferValidationError::EditedWriteProtectedOccurrence {
          changes, .. }] if changes == &vec! ["aliases differ from the graph's" . to_string ()]),
        "{}: {:?}", text, errors); }
    assert_eq! (errors_in (
      "* (skg node (gnid recorder) (skgRepos (nodeRepo main)) writeProtected) recorder\n"),
      vec! []);
  }

  #[test]
  fn a_subscribee_folder_with_an_extra_member_is_refused () {
    let errors : Vec<BufferValidationError> = errors_in (indoc! {"
      * (skg node (gnid recorder) (skgRepos (nodeRepo main)) writeProtected) recorder
      ** (skg subscribeeFolder)
      *** (skg node (gnid sub) (skgRepos (nodeRepo main)) writeProtected) sub
      *** (skg node (gnid private_sub) (skgRepos (nodeRepo private)) writeProtected) private_sub
      *** (skg node (gnid c1) (skgRepos (nodeRepo main)) writeProtected) c1
    "});
    assert_eq! (refused_changes (&errors),
                vec! ["changed subscriptions" . to_string ()]);
    assert! (errors [0] . to_string () . contains ("changed subscriptions"),
             "{}", errors [0]);
  }

  /// As a buffer of the diff view shows the folder: each member
  /// with the existence of its relationship.
  #[test]
  fn a_subscribee_folder_with_an_extra_member_is_refused_in_a_diffView_buffer () {
    let errors : Vec<BufferValidationError> = errors_in (indoc! {"
      * (skg node (gnid recorder) (skgRepos (nodeRepo main)) writeProtected) recorder
      ** (skg subscribeeFolder)
      *** (skg node (gnid sub) (skgRepos (nodeRepo main)) writeProtected (diff (rel added))) sub
      *** (skg node (gnid private_sub) (skgRepos (nodeRepo private)) writeProtected) private_sub
      *** (skg node (gnid c1) (skgRepos (nodeRepo main)) writeProtected (diff (rel added))) c1
    "});
    assert_eq! (refused_changes (&errors),
                vec! ["changed subscriptions" . to_string ()]);
  }

  #[test]
  fn a_partner_folder_that_matches_the_graph_passes_and_an_absent_one_passes () {
    assert_eq! (errors_in (indoc! {"
      * (skg node (gnid recorder) (skgRepos (nodeRepo main)) writeProtected) recorder
      ** (skg subscribeeFolder)
      *** (skg node (gnid private_sub) (skgRepos (nodeRepo private)) writeProtected) private_sub
      *** (skg node (gnid sub) (skgRepos (nodeRepo main)) writeProtected) sub
      ** (skg containerFolder)
      *** (skg node (gnid root) (skgRepos (nodeRepo main)) writeProtected) root
      ** (skg node (gnid c2) (skgRepos (nodeRepo main)) noClaim) a non-member
    "}), vec! []);
    assert_eq! (errors_in (indoc! {"
      * (skg node (gnid recorder) (skgRepos (nodeRepo main)) writeProtected) recorder
      ** (skg containerFolder)
      *** (skg node (gnid root) (skgRepos (nodeRepo main)) writeProtected) root
    "}), vec! []);
  }

  /// A container folder is a write-protected set the server
  /// generates; a missing or an extra member is refused alike.
  #[test]
  fn a_container_folder_that_differs_is_refused () {
    for members in [
      "",
      "*** (skg node (gnid root) (skgRepos (nodeRepo main)) writeProtected) root
*** (skg node (gnid c1) (skgRepos (nodeRepo main)) writeProtected) c1
", ]
    { let text : String = format! (
        "* (skg node (gnid recorder) (skgRepos (nodeRepo main)) writeProtected) recorder
** (skg containerFolder)
{}",
        members );
      assert_eq! (refused_changes (&errors_in (&text)),
                  vec! ["changed containers" . to_string ()], "{}", text); }
  }

  /// recorder subscribes to x by its extra ID x2, and to ghost, which
  /// no node has. A member shown by the extra ID, or by x's pid, passes,
  /// because the check reads each member as the folder reconciler
  /// matches it ('skgid_a_folder_child_stands_for'): by the pid the
  /// graph resolves its ID to.
  #[test]
  fn a_member_shown_by_an_extra_id_passes_when_the_graph_lists_the_extra_id () {
    let graph : InRustGraph = graph_listing_an_extra_id_and_a_dangling_id ();
    for shown in ["x2", "x"] {
      let text : String = format! (
        "* (skg node (gnid recorder) (skgRepos (nodeRepo main)) writeProtected) recorder
** (skg subscribeeFolder)
*** (skg node (gnid {}) (skgRepos (nodeRepo main)) writeProtected) x
*** (skg unknown (gnid ghost))
", shown );
      assert_eq! (
        errors_for_writeProtected_occurrences (
          &forest (&text), &graph, &config (), None ),
        vec! [], "{}", text ); }
  }

  /// An unknown phantom member is compared by its raw ID: shown with
  /// the ID the recorder lists it passes, shown with another it is
  /// refused.
  #[test]
  fn an_unknown_member_is_compared_by_raw_id () {
    let graph : InRustGraph = graph_listing_an_extra_id_and_a_dangling_id ();
    let text_with = |unknown : &str| -> String { format! (
      "* (skg node (gnid recorder) (skgRepos (nodeRepo main)) writeProtected) recorder
** (skg subscribeeFolder)
*** (skg node (gnid x) (skgRepos (nodeRepo main)) writeProtected) x
*** (skg unknown (gnid {}))
", unknown ) };
    assert_eq! (
      errors_for_writeProtected_occurrences (
        &forest (&text_with ("ghost")), &graph, &config (), None ),
      vec! [] );
    assert_eq! (
      refused_changes ( &errors_for_writeProtected_occurrences (
        &forest (&text_with ("other_ghost")), &graph, &config (), None )),
      vec! ["changed subscriptions" . to_string ()] );
  }

  /// A dangling removed member, as the diff view shows one (an
  /// unknown phantom with 'noClaim' and '(rel removed)'), is not
  /// counted: the folder beside it says what the graph says, and
  /// passes. The same phantom claiming membership is a member,
  /// compared by its raw ID as any unknown member is, and recorder
  /// does not list it.
  #[test]
  fn an_unknown_phantom_whose_relationship_was_removed_is_not_counted () {
    let graph : InRustGraph = graph_listing_an_extra_id_and_a_dangling_id ();
    let text_with = |gone : &str| -> String { format! (
      "* (skg node (gnid recorder) (skgRepos (nodeRepo main)) writeProtected) recorder
** (skg subscribeeFolder)
*** (skg node (gnid x) (skgRepos (nodeRepo main)) writeProtected) x
*** (skg unknown (gnid ghost))
*** {}
", gone ) };
    assert_eq! (
      errors_for_writeProtected_occurrences (
        &forest (&text_with ("(skg unknown (gnid gone) noClaim (diff (rel removed)))")),
        &graph, &config (), None ),
      vec! [] );
    assert_eq! (
      refused_changes ( &errors_for_writeProtected_occurrences (
        &forest (&text_with ("(skg unknown (gnid gone))")),
        &graph, &config (), None )),
      vec! ["changed subscriptions" . to_string ()] );
  }

  /// recorder subscribes to x2, an extra ID of x, and to ghost, which
  /// no node has.
  fn graph_listing_an_extra_id_and_a_dangling_id () -> InRustGraph {
    let main : SkgrepoName = SkgrepoName::from ("main");
    let node = |pid : &str| -> Graphnode { Graphnode {
      pid          : ID::from (pid),
      title        : pid . to_string (),
      home_skgrepo : main . clone (),
      .. empty_graphnode () }};
    InRustGraph::from_graphnodes (&[
      Graphnode {
        subscribesTo : MSV::Specified (rel_partners_at_relRepo (
          &main, vec! [ID::from ("x2"), ID::from ("ghost")])),
        .. node ("recorder") },
      Graphnode {
        extra_ids : vec! [ID::from ("x2")],
        .. node ("x") } ]) }

  /// A removed member, as the diff view shows one (a non-member with
  /// '(rel removed)'), is not counted, nor is any other non-member.
  #[test]
  fn a_removed_member_is_not_counted () {
    assert_eq! (errors_in (indoc! {"
      * (skg node (gnid recorder) (skgRepos (nodeRepo main)) writeProtected) recorder
      ** (skg containerFolder)
      *** (skg node (gnid root) (skgRepos (nodeRepo main)) writeProtected) root
      *** (skg node (gnid c1) (skgRepos (nodeRepo main)) noClaim writeProtected (diff (rel removed))) c1
      *** (skg node (gnid c2) (skgRepos (nodeRepo main)) writeProtected noClaim) c2
    "}), vec! []);
  }

  /// A member is counted whatever its diff facts say: a removed
  /// member is never a member.
  #[test]
  fn a_member_marked_removed_is_counted () {
    assert_eq! (refused_changes (&errors_in (indoc! {"
      * (skg node (gnid recorder) (skgRepos (nodeRepo main)) writeProtected) recorder
      ** (skg containerFolder)
      *** (skg node (gnid root) (skgRepos (nodeRepo main)) writeProtected) root
      *** (skg node (gnid c1) (skgRepos (nodeRepo main)) writeProtected (diff (rel removed))) c1
    "})), vec! ["changed containers" . to_string ()]);
  }

  #[test]
  fn a_partner_folder_member_with_a_relRepo_request_is_refused () {
    assert_eq! (refused_changes (&errors_in (indoc! {"
      * (skg node (gnid recorder) (skgRepos (nodeRepo main)) writeProtected) recorder
      ** (skg containerFolder)
      *** (skg node (gnid root) (skgRepos (nodeRepo main)) writeProtected (editRequest (relRepo main))) root
    "})), vec! ["changed containers" . to_string ()]);
  }

  /// Under a skgrepo restriction the folder is shown without the
  /// members it hides, and so shown it passes; shown with them, it
  /// differs.
  #[test]
  fn under_a_skgrepo_restriction_a_folder_without_the_hidden_members_passes () {
    let restriction : SkgrepoRestriction = SkgrepoRestriction {
      name     : SkgrepoSetName::from ("mainOnly"),
      skgrepos : BTreeSet::from ([SkgrepoName::from ("main")]), };
    let errors_under_restriction = |text : &str| -> Vec<BufferValidationError> {
      errors_for_writeProtected_occurrences (
        &forest (text), &graph (), &config (), Some (&restriction) ) };
    let without_hidden : &str = indoc! {"
      * (skg node (gnid recorder) (skgRepos (nodeRepo main)) writeProtected) recorder
      ** (skg subscribeeFolder)
      *** (skg node (gnid sub) (skgRepos (nodeRepo main)) writeProtected) sub
    "};
    assert_eq! (errors_under_restriction (without_hidden), vec! []);
    assert_eq! (refused_changes (&errors_in (without_hidden)),
                vec! ["changed subscriptions" . to_string ()]);
    assert_eq! (refused_changes (&errors_under_restriction (indoc! {"
      * (skg node (gnid recorder) (skgRepos (nodeRepo main)) writeProtected) recorder
      ** (skg subscribeeFolder)
      *** (skg node (gnid sub) (skgRepos (nodeRepo main)) writeProtected) sub
      *** (skg node (gnid private_sub) (skgRepos (nodeRepo private)) writeProtected) private_sub
    "})), vec! ["changed subscriptions" . to_string ()]);
  }

  #[test]
  fn an_editable_occurrence_is_not_judged_by_the_graph () {
    assert_eq! (errors_in (indoc! {"
      * (skg node (gnid recorder) (skgRepos (nodeRepo main))) changed
      ** (skg aliasFolder)
      *** (skg alias) other name
      ** (skg node (gnid typed) (skgRepos (nodeRepo main))) typed
    "}), vec! []);
  }

  #[test]
  fn body_on_an_writeProtected_occurrence_is_a_parse_error () {
    let (_forest, errors, _warnings, _provenance) = org_to_viewforest (
      indoc! {"
        * (skg node (gnid recorder) (skgRepos (nodeRepo main)) writeProtected) recorder
        body that would otherwise disappear
      "}, &InRustGraph::new ()) . unwrap ();
    assert! (matches! (&errors[..],
      [BufferValidationError::EditedWriteProtectedOccurrence {
        skgid, title, changes, .. }] if skgid == &ID::from ("recorder")
          && title == "recorder" && changes == &vec!["added body text" . to_string ()]));
  }

  #[test]
  fn flags_surface_edits_report_recorder_identity_and_concrete_changes () {
    let errors : Vec<BufferValidationError> = errors_in (indoc! {"
      * (skg node (gnid recorder) (skgRepos (nodeRepo main))) recorder
      ** (skg flagsFolder) edited folder headline
      added folder body
      *** (skg (flag noSearchMatching)) renamed
      added leaf body
      *** (skg (flag wasOverloaded))
    "});
    assert! (matches! (&errors[..],
      [BufferValidationError::FlagsSurfaceEdited {
        recorder_skgid, recorder_title, changes, .. }]
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
      "* (skg node (gnid recorder) (skgRepos (nodeRepo main))) recorder\n"), vec! []);
  }
}
