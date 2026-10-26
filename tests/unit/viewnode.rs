use crate::from_text::buffer_to_viewnodes::add_missing_info::EnrichmentProvenance;
use crate::dbs::in_rust_graph::InRustGraph;
use super::*;
use crate::from_text::buffer_to_viewnodes::uninterpreted::org_to_viewforest;
use crate::org_to_text::viewnode_to_string;
use crate::serve::parse_metadata_sexp::{ViewnodeMetadata, parse_metadata_to_viewnodemd, viewnode_from_metadata};
use crate::types::errors::BufferValidationError;
use crate::types::viewnode::Viewnode;
use crate::types::misc::SkgConfig;
use crate::types::tree::forest::{ViewForest, editable_occurrences};
use std::collections::HashMap;

// 'Folder::goal_is_ordered', 'Folder::is_editable' and
// 'Folder::members_are_properties' are the per-kind facts that the
// save, the refresh and the heralds consult; this pins them.
#[test]
fn folder_kind_facts () {
  let ordered : Vec<Folder> = vec! [
    Folder::Subscribee, Folder::Overridden, Folder::Hidden,
    Folder::HiddenOutsideOfSubscribee, Folder::Alias ];
  let unordered : Vec<Folder> = vec! [
    Folder::Subscriber, Folder::Overrider, Folder::Hider,
    Folder::HiddenInSubscribee, Folder::Container, Folder::Mentioner,
    Folder::AcquiredBy, Folder::Acquired, Folder::ID, Folder::flags () ];
  for folder in &ordered {
    assert! ( folder . goal_is_ordered (), "{:?}", folder ); }
  for folder in &unordered {
    assert! ( ! folder . goal_is_ordered (), "{:?}", folder ); }
  for folder in ordered . iter () . chain (&unordered) {
    assert_eq! ( folder . is_editable (),
                 matches! ( folder,
                            Folder::Subscribee | Folder::Overridden
                            | Folder::HiddenOutsideOfSubscribee
                            | Folder::Alias ),
                 "{:?}", folder ); }
  for folder in ordered . iter () . chain (&unordered) {
    assert_eq! ( folder . members_are_properties (),
                 matches! ( folder,
                            Folder::Alias | Folder::ID | Folder::Flags { .. } ),
                 "{:?}", folder );
    assert_eq! ( Folder::from_client_string (folder . repr_in_client ()),
                 Some ( match folder { Folder::Flags { .. } => Folder::flags (),
                                       other => other . clone () } ),
                 "{:?}", folder ); } }

#[test]
fn consuming_edit_requests_covers_every_carrier_but_not_view_requests () {
  let mut restriction : Viewnode = mk_viewnode (
    ID::from ("unrestricted"), SkgrepoName::from ("public"), "unrestricted" . into (),
    true,
    Editability::Editable {
      body : None,
      edit_request : Some (NodeEditRequest::Delete) },
    [ViewRequest::Editable] . into_iter () . collect () );
  if let ViewnodeKind::Vognode (Vognode::Unrestricted (node)) = &mut restriction . kind {
    node . relationshipEditRequest = Some ( RelationshipEditRequest::RelRepo ( SkgrepoName::from ("private") )); }
  restriction . readiness_request = Some (ReadinessRequest {
    ready : true, objects : [DiffObject::All] . into_iter () . collect () });
  restriction . consume_edit_request_after_save ();
  assert_eq! (restriction . readiness_request, None);
  let ViewnodeKind::Vognode (Vognode::Unrestricted (restriction)) = &restriction . kind
  else { panic! ("expected unrestricted node"); };
  assert_eq! (restriction . relationshipEditRequest, None);
  assert_eq! (restriction . edit_request (), None);
  assert! (restriction . view_requests . contains (&ViewRequest::Editable));

  let mut unknown : Viewnode = Viewnode {
    readiness_request : Some (ReadinessRequest {
      ready : true, objects : BTreeSet::new () }),
    .. viewnode_of ( ViewnodeKind::Vognode (Vognode::Unknown (
      Unknown {
        gnid : ID::from ("unknown"),
        relRepo : None,
        relationshipEditRequest : Some (RelationshipEditRequest::RelRepo (SkgrepoName::from ("private"))),
        claimsMembership : true, }))) };
  unknown . consume_edit_request_after_save ();
  assert_eq! (unknown . readiness_request, None);
  let ViewnodeKind::Vognode (Vognode::Unknown (unknown)) = &unknown . kind
  else { panic! ("expected unknown node"); };
  assert_eq! (unknown . relationshipEditRequest, None);

  let mut alias : Viewnode = Viewnode {
    readiness_request : Some (ReadinessRequest {
      ready : false, objects : BTreeSet::new () }),
    .. viewnode_of ( ViewnodeKind::PropertyMember (PropertyMember::alias ( "alias" . into (), None, Some (RelationshipEditRequest::RelRepo (SkgrepoName::from ("private"))) ))) };
  alias . consume_edit_request_after_save ();
  assert_eq! (alias . readiness_request, None);
  let ViewnodeKind::PropertyMember (PropertyMember::Alias { relationshipEditRequest, .. }) =
    &alias . kind
  else { panic! ("expected alias"); };
  assert_eq! (*relationshipEditRequest, None);
}

// The write-protect requests render as their atoms, and parse back.
#[test]
fn writeProtect_requests_round_trip () {
  for (request, atom) in [
    ( ViewRequest::WriteProtected,        "writeProtectedView" ),
    ( ViewRequest::WriteProtectedSubtree, "writeProtectedSubtreeView" ) ] {
    assert_eq! ( request . to_string (), atom );
    let metadata : ViewnodeMetadata =
      parse_metadata_to_viewnodemd (
        &format! ( "(skg node (gnid x) (viewRequests {}))", atom ) )
      . unwrap ();
    assert_eq! ( metadata . view_requests,
                 [request] . into_iter () . collect () ); }}

// 'write_protect' discards the body and edit request and drops the
// editable-view request, and touches nothing else: not the other view
// requests, not 'membership'.
#[test]
fn write_protect_drops_the_editable_request_and_keeps_membership () {
  let mut viewnode : Viewnode = mk_viewnode (
    ID::from ("node"), SkgrepoName::from ("public"), "node" . into (),
    false,
    Editability::Editable {
      body : Some ("body" . into ()),
      edit_request : Some (NodeEditRequest::Delete) },
    [ ViewRequest::Editable, ViewRequest::Flags ] . into_iter () . collect () );
  let ViewnodeKind::Vognode (Vognode::Unrestricted (t)) = &mut viewnode . kind
  else { panic! ("expected unrestricted node"); };
  t . write_protect ();
  assert_eq! (t . editability, Editability::WriteProtected);
  assert_eq! ( t . view_requests,
               [ ViewRequest::Flags ] . into_iter () . collect () );
  assert_eq! (t . claimsMembership, false); }

// 'editable_occurrences' collects the IDs of the editable unrestricted
// vognodes at any depth, and no write-protected one, phantom or
// folder.
#[test]
fn editable_occurrences_are_the_editable_unrestricted_vognodes () {
  let mut viewforest : ViewForest = ViewForest::new ();
  let buffer_root : ego_tree::NodeId = viewforest . internal_root_gnid ();
  let tree : &mut ego_tree::Tree<Viewnode> = viewforest . as_internal_tree_mut ();
  let editable = |gnid : &str| -> Viewnode {
    mk_editable_viewnode ( ID::from (gnid), SkgrepoName::from ("public"),
                           gnid . into (), None ) };
  let root : ego_tree::NodeId =
    tree . get_mut (buffer_root) . unwrap () . append (editable ("root")) . id ();
  let folder : ego_tree::NodeId = tree . get_mut (root) . unwrap ()
    . append ( viewnode_of ( ViewnodeKind::Folder (Folder::Container) ))
    . id ();
  tree . get_mut (folder) . unwrap () . append (editable ("in-folder"));
  tree . get_mut (root) . unwrap () . append (editable ("member"));
  tree . get_mut (root) . unwrap () . append ( mk_writeProtected_viewnode (
    ID::from ("protected"), SkgrepoName::from ("public"), "protected" . into (),
    true ));
  tree . get_mut (root) . unwrap () . append ( mk_unknown_viewnode (ID::from ("unknown")) );
  assert_eq! ( editable_occurrences (&viewforest),
               [ "root", "in-folder", "member" ] . into_iter ()
               . map (ID::from) . collect::<HashSet<ID>> () ); }


// A viewnode ID is base 36, at least two characters long, and reads back
// as the counter value it was made from.
#[test]
fn viewnode_ids_are_base_36_and_at_least_two_characters_long () {
  for (counter, text) in [ (0, "00"), (10, "0a"), (35, "0z"), (36, "10"),
                           (1295, "zz"), (1296, "100") ] {
    assert_eq! ( ViewnodeId::from_counter (counter), ViewnodeId (text . to_string ()) );
    assert_eq! ( ViewnodeId::from_counter (counter) . counter_value (), Some (counter) ); }
  assert! ( ViewnodeId::parse ("0a") . is_ok () );
  for not_a_viewnode_id in [ "", "0A", "a-b", "a b" ] {
    assert! ( ViewnodeId::parse (not_a_viewnode_id) . is_err (), "{:?}", not_a_viewnode_id ); }}

// Every kind of viewnode prints its viewnode ID as the first part of its
// '(skg ...)' wrapper, and the parser reads it back.
#[test]
fn every_kind_prints_its_viewnode_id_first_and_parses_it_back () {
  let config : SkgConfig = SkgConfig::dummyFromSkgrepos (HashMap::new ());
  let kinds : Vec<Viewnode> = vec! [
    mk_editable_viewnode ( ID::from ("n"), SkgrepoName::from ("main"),
                           "n" . into (), None ),
    mk_writeProtected_viewnode ( ID::from ("w"), SkgrepoName::from ("main"),
                                 "w" . into (), false ),
    mk_unknown_viewnode (ID::from ("u")),
    viewnode_of (ViewnodeKind::Tombstone),
    viewnode_of (ViewnodeKind::Folder (Folder::Container)),
    viewnode_of (ViewnodeKind::Folder (Folder::Alias)),
    viewnode_of (ViewnodeKind::PropertyMember (PropertyMember::alias ( "an alias" . into (), None, None ))),
    viewnode_of (ViewnodeKind::Vognode (Vognode::Ghost (
      Ghost { gnid : ID::from ("g"), home_skgrepo : SkgrepoName::from ("main"),
              title : "g" . into (), body : None,
              restoreNode_request : false, tombstone_request : false,
              claimsMembership : false, isWarranted : Warrant::ByHead } ))) ];
  for (counter, kind) in kinds . into_iter () . enumerate () {
    let viewnode_id : ViewnodeId = ViewnodeId::from_counter (10 + counter as u64);
    let viewnode : Viewnode = Viewnode { viewnode_id : Some (viewnode_id . clone ()), .. kind };
    let rendered : String = viewnode_to_string (&viewnode, &config) . unwrap ();
    assert! ( rendered . starts_with (&format! ("(vnid {}) ", viewnode_id)), "{}", rendered );
    let metadata : ViewnodeMetadata =
      parse_metadata_to_viewnodemd (&format! ("(skg {})", rendered)) . unwrap ();
    let (parsed, _error, _warning)
      : (Viewnode, Option<BufferValidationError>, Option<String>)
      = viewnode_from_metadata (
      &metadata, viewnode . title () . to_string (), None );
    assert_eq! ( parsed . viewnode_id, Some (viewnode_id), "{}", rendered ); }}

// Of two headlines with one viewnode ID, the first in buffer order keeps it;
// the second, a copy, loses it. A headline with no viewnode ID has none.
#[test]
fn a_repeated_viewnode_id_stays_with_its_first_headline () {
  let (forest, errors, _warnings, _provenance)
    : (ViewForest, Vec<BufferValidationError>, Vec<String>, EnrichmentProvenance) =
    org_to_viewforest (concat! (
      "* (skg (vnid 0a) node (gnid a) (skgRepos (nodeRepo main))) a\n",
      "** (skg (vnid 0b) node (gnid b) (skgRepos (nodeRepo main))) b\n",
      "** (skg (vnid 0b) node (gnid b) (skgRepos (nodeRepo main))) b\n",
      "** typed\n" ), &InRustGraph::new ()) . unwrap ();
  assert! ( errors . is_empty (), "{:?}", errors );
  let viewnode_ids : Vec<Option<ViewnodeId>> =
    forest . nodes () . map ( |node| node . value () . viewnode_id . clone () ) . collect ();
  assert_eq! ( viewnode_ids, vec! [ Some (ViewnodeId ("0a" . into ())), Some (ViewnodeId ("0b" . into ())),
                            None, None ] ); }
