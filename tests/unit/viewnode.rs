use super::*;
use crate::serve::parse_metadata_sexp::{ViewnodeMetadata, parse_metadata_to_viewnodemd};

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
    node . relRepo_request = Some (SkgrepoName::from ("private"));
    node . readiness_request = Some (ReadinessRequest {
      ready : true, axes : [ReadinessAxis::All] . into_iter () . collect () }); }
  restriction . consume_edit_request_after_save ();
  let ViewnodeKind::Vognode (Vognode::Unrestricted (restriction)) = &restriction . kind
  else { panic! ("expected unrestricted node"); };
  assert_eq! (restriction . relRepo_request, None);
  assert_eq! (restriction . readiness_request, None);
  assert_eq! (restriction . edit_request (), None);
  assert! (restriction . view_requests . contains (&ViewRequest::Editable));

  let mut unknown : Viewnode = Viewnode {
    focused : false, org_folded : false, org_body_folded : false,
    kind : ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Unknown (PhantomUnknown {
      skgid : ID::from ("unknown"),
      relRepo : None,
      relRepo_request : Some (SkgrepoName::from ("private")),
      change_facts : ChangeFacts::default (),
      readiness_request : Some (ReadinessRequest {
        ready : true, axes : BTreeSet::new () }), }))) };
  unknown . consume_edit_request_after_save ();
  let ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Unknown (unknown))) = &unknown . kind
  else { panic! ("expected unknown node"); };
  assert_eq! (unknown . relRepo_request, None);
  assert_eq! (unknown . readiness_request, None);

  let mut alias : Viewnode = Viewnode {
    focused : false, org_folded : false, org_body_folded : false,
    kind : ViewnodeKind::Property (Property::Alias {
      text : "alias" . into (),
      relRepo : None,
      relRepo_request : Some (SkgrepoName::from ("private")),
      change_facts : ChangeFacts::default (),
      readiness_request : Some (ReadinessRequest {
        ready : false, axes : BTreeSet::new () }), }) };
  alias . consume_edit_request_after_save ();
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
