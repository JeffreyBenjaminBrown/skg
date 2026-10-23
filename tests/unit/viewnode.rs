use super::*;
use crate::from_text::buffer_to_viewnodes::uninterpreted::org_to_uninterpreted_viewforest;
use crate::org_to_text::viewnode_to_string;
use crate::serve::parse_metadata_sexp::{ViewnodeMetadata, parse_metadata_to_viewnodemd, viewnode_from_metadata};
use crate::types::errors::BufferValidationError;
use crate::types::maybe_placed_viewnode::MpViewnode;
use crate::types::misc::SkgConfig;
use crate::types::tree::forest::{MpViewForest, ViewForest, editable_occurrences};
use std::collections::HashMap;

// 'PartnerFolder::policy' is the single source of truth for how each
// folder's membership relates to user edits; this pins the mapping.
#[test]
fn partnerFolder_policy_mapping () {
  assert_eq! ( PartnerFolder::Subscribee . policy (),
               FolderPolicy::EditableSet );
  assert_eq! ( PartnerFolder::Overridden . policy (),
               FolderPolicy::EditableSet );
  assert_eq! ( PartnerFolder::Subscriber . policy (),
               FolderPolicy::WriteProtectedSet );
  assert_eq! ( PartnerFolder::Overrider . policy (),
               FolderPolicy::WriteProtectedSet );
  assert_eq! ( PartnerFolder::Hider . policy (),
               FolderPolicy::WriteProtectedSet );
  assert_eq! ( PartnerFolder::Hidden . policy (),
               FolderPolicy::WriteProtectedSet );
  assert_eq! ( PartnerFolder::HiddenInSubscribee . policy (),
               FolderPolicy::WriteProtectedFilter );
  assert_eq! ( PartnerFolder::HiddenOutsideOfSubscribee . policy (),
               FolderPolicy::EditableFilter ); }

#[test]
fn consuming_edit_requests_covers_every_carrier_but_not_view_requests () {
  let mut restriction : Viewnode = mk_viewnode (
    ID::from ("unrestricted"), SkgrepoName::from ("public"), "unrestricted" . into (),
    AffectsParent::True, Birth::Unremarkable,
    Editability::Editable {
      body : None,
      edit_request : Some (NodeEditRequest::Delete) },
    [ViewRequest::Editable] . into_iter () . collect () );
  if let ViewnodeKind::Vognode (Vognode::Unrestricted (node)) = &mut restriction . kind {
    node . relRepo_request = Some (SkgrepoName::from ("private")); }
  restriction . readiness_request = Some (ReadinessRequest {
    ready : true, objects : [DiffObject::All] . into_iter () . collect () });
  restriction . consume_edit_request_after_save ();
  assert_eq! (restriction . readiness_request, None);
  let ViewnodeKind::Vognode (Vognode::Unrestricted (restriction)) = &restriction . kind
  else { panic! ("expected unrestricted node"); };
  assert_eq! (restriction . relRepo_request, None);
  assert_eq! (restriction . edit_request (), None);
  assert! (restriction . view_requests . contains (&ViewRequest::Editable));

  let mut unknown : Viewnode = Viewnode {
    readiness_request : Some (ReadinessRequest {
      ready : true, objects : BTreeSet::new () }),
    .. viewnode_of ( ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Unknown (
      PhantomUnknown {
        skgid : ID::from ("unknown"),
        relRepo : None,
        relRepo_request : Some (SkgrepoName::from ("private")), })))) };
  unknown . consume_edit_request_after_save ();
  assert_eq! (unknown . readiness_request, None);
  let ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Unknown (unknown))) = &unknown . kind
  else { panic! ("expected unknown node"); };
  assert_eq! (unknown . relRepo_request, None);

  let mut alias : Viewnode = Viewnode {
    readiness_request : Some (ReadinessRequest {
      ready : false, objects : BTreeSet::new () }),
    .. viewnode_of ( ViewnodeKind::Property (Property::Alias {
      text : "alias" . into (),
      relRepo : None,
      relRepo_request : Some (SkgrepoName::from ("private")), })) };
  alias . consume_edit_request_after_save ();
  assert_eq! (alias . readiness_request, None);
  let ViewnodeKind::Property (Property::Alias { relRepo_request, .. }) =
    &alias . kind
  else { panic! ("expected alias"); };
  assert_eq! (*relRepo_request, None);
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
        &format! ( "(skg (node (id x) (viewRequests {})))", atom ) )
      . unwrap ();
    assert_eq! ( metadata . view_requests,
                 [request] . into_iter () . collect () ); }}

// 'write_protect' discards the body and edit request and drops the
// editable-view request, and touches nothing else: not the other view
// requests, not 'affectsParent', not 'birth'.
#[test]
fn write_protect_drops_the_editable_request_and_keeps_membership_and_birth () {
  let mut viewnode : Viewnode = mk_viewnode (
    ID::from ("node"), SkgrepoName::from ("public"), "node" . into (),
    AffectsParent::True, Birth::RoleGraft (RelationRole::CONTAINER),
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
  assert_eq! (t . affectsParent, AffectsParent::True);
  assert_eq! (t . birth, Birth::RoleGraft (RelationRole::CONTAINER)); }

// 'editable_occurrences' collects the IDs of the editable unrestricted
// vognodes at any depth, and no write-protected one, phantom or
// folder.
#[test]
fn editable_occurrences_are_the_editable_unrestricted_vognodes () {
  let mut viewforest : ViewForest = ViewForest::new ();
  let buffer_root : ego_tree::NodeId = viewforest . internal_root_skgid ();
  let tree : &mut ego_tree::Tree<Viewnode> = viewforest . as_internal_tree_mut ();
  let editable = |skgid : &str| -> Viewnode {
    mk_editable_viewnode ( ID::from (skgid), SkgrepoName::from ("public"),
                           skgid . into (), None ) };
  let root : ego_tree::NodeId =
    tree . get_mut (buffer_root) . unwrap () . append (editable ("root")) . id ();
  let folder : ego_tree::NodeId = tree . get_mut (root) . unwrap ()
    . append ( viewnode_of ( ViewnodeKind::PartnerFolder (PartnerFolder::Container) ))
    . id ();
  tree . get_mut (folder) . unwrap () . append (editable ("in-folder"));
  tree . get_mut (root) . unwrap () . append (editable ("member"));
  tree . get_mut (root) . unwrap () . append ( mk_writeProtected_viewnode (
    ID::from ("protected"), SkgrepoName::from ("public"), "protected" . into (),
    AffectsParent::True ));
  tree . get_mut (root) . unwrap () . append ( mk_unknown_viewnode (ID::from ("unknown")) );
  assert_eq! ( editable_occurrences (&viewforest),
               [ "root", "in-folder", "member" ] . into_iter ()
               . map (ID::from) . collect::<HashSet<ID>> () ); }


// A view id is base 36, at least two characters long, and reads back
// as the counter value it was made from.
#[test]
fn vids_are_base_36_and_at_least_two_characters_long () {
  for (counter, text) in [ (0, "00"), (10, "0a"), (35, "0z"), (36, "10"),
                           (1295, "zz"), (1296, "100") ] {
    assert_eq! ( Vid::from_counter (counter), Vid (text . to_string ()) );
    assert_eq! ( Vid::from_counter (counter) . counter_value (), Some (counter) ); }
  assert! ( Vid::parse ("0a") . is_ok () );
  for not_a_vid in [ "", "0A", "a-b", "a b" ] {
    assert! ( Vid::parse (not_a_vid) . is_err (), "{:?}", not_a_vid ); }}

// Every kind of viewnode prints its vid as the last part of its
// '(skg ...)' wrapper, and the parser reads it back.
#[test]
fn every_kind_prints_its_vid_last_and_parses_it_back () {
  let config : SkgConfig = SkgConfig::dummyFromSkgrepos (HashMap::new ());
  let kinds : Vec<Viewnode> = vec! [
    mk_editable_viewnode ( ID::from ("n"), SkgrepoName::from ("main"),
                           "n" . into (), None ),
    mk_writeProtected_viewnode ( ID::from ("w"), SkgrepoName::from ("main"),
                                 "w" . into (), AffectsParent::False ),
    mk_unknown_viewnode (ID::from ("u")),
    mk_restricted_viewnode (),
    viewnode_of (ViewnodeKind::DeadViewnode),
    viewnode_of (ViewnodeKind::PartnerFolder (PartnerFolder::Container)),
    viewnode_of (ViewnodeKind::PropertyFolder (PropertyFolder::Alias)),
    viewnode_of (ViewnodeKind::Property (Property::Alias {
      text : "an alias" . into (), relRepo : None, relRepo_request : None })),
    viewnode_of (ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Deleted (
      PhantomDeleted { skgid : ID::from ("d"), home_skgrepo : SkgrepoName::from ("main"),
                       title : "d" . into (), body : None } )))) ];
  for (counter, kind) in kinds . into_iter () . enumerate () {
    let vid : Vid = Vid::from_counter (10 + counter as u64);
    let viewnode : Viewnode = Viewnode { vid : Some (vid . clone ()), .. kind };
    let rendered : String = viewnode_to_string (&viewnode, &config) . unwrap ();
    assert! ( rendered . ends_with (&format! (" (vid {})", vid)), "{}", rendered );
    let metadata : ViewnodeMetadata =
      parse_metadata_to_viewnodemd (&format! ("(skg {})", rendered)) . unwrap ();
    let (parsed, _error, _warning)
      : (MpViewnode, Option<BufferValidationError>, Option<String>)
      = viewnode_from_metadata (
      &metadata, viewnode . title () . to_string (), None );
    assert_eq! ( parsed . vid, Some (vid), "{}", rendered ); }}

// Of two headlines with one vid, the first in buffer order keeps it;
// the second, a copy, loses it. A headline with no vid has none.
#[test]
fn a_repeated_vid_stays_with_its_first_headline () {
  let (forest, errors, _warnings)
    : (MpViewForest, Vec<BufferValidationError>, Vec<String>) =
    org_to_uninterpreted_viewforest ( concat! (
      "* (skg (node (id a) (repo main)) (vid 0a)) a\n",
      "** (skg (node (id b) (repo main)) (vid 0b)) b\n",
      "** (skg (node (id b) (repo main)) (vid 0b)) b\n",
      "** typed\n" )) . unwrap ();
  assert! ( errors . is_empty (), "{:?}", errors );
  let vids : Vec<Option<Vid>> =
    forest . nodes () . map ( |node| node . value () . vid . clone () ) . collect ();
  assert_eq! ( vids, vec! [ Some (Vid ("0a" . into ())), Some (Vid ("0b" . into ())),
                            None, None ] ); }
