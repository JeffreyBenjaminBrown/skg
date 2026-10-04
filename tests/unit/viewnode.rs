use super::*;

// 'PartnerFolder::policy' is the single source of truth for how each
// folder's membership relates to user edits; this pins the mapping.
#[test]
fn partnerFolder_policy_mapping () {
  assert_eq! ( PartnerFolder::Subscribee . policy (),
               FolderPolicy::WritableSet );
  assert_eq! ( PartnerFolder::Overridden . policy (),
               FolderPolicy::WritableSet );
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
  let mut active : Viewnode = mk_viewnode (
    ID::from ("active"), RepoName::from ("public"), "active" . into (),
    AffectsParent::True, Birth::Unremarkable,
    Editability::Definitive {
      body : None,
      edit_request : Some (NodeEditRequest::Delete) },
    [ViewRequest::Definitive] . into_iter () . collect () );
  if let ViewnodeKind::Vognode (Vognode::Active (node)) = &mut active . kind {
    node . relRepo_request = Some (RepoName::from ("private")); }
  active . consume_edit_request_after_save ();
  let ViewnodeKind::Vognode (Vognode::Active (active)) = &active . kind
  else { panic! ("expected active node"); };
  assert_eq! (active . relRepo_request, None);
  assert_eq! (active . edit_request (), None);
  assert! (active . view_requests . contains (&ViewRequest::Definitive));

  let mut unknown : Viewnode = Viewnode {
    focused : false, folded : false, body_folded : false,
    kind : ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Unknown (PhantomUnknown {
      id : ID::from ("unknown"),
      relRepo : None,
      relRepo_request : Some (RepoName::from ("private")), }))) };
  unknown . consume_edit_request_after_save ();
  let ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Unknown (unknown))) = &unknown . kind
  else { panic! ("expected unknown node"); };
  assert_eq! (unknown . relRepo_request, None);

  let mut alias : Viewnode = Viewnode {
    focused : false, folded : false, body_folded : false,
    kind : ViewnodeKind::Property (Property::Alias {
      text : "alias" . into (),
      relRepo : None,
      relRepo_request : Some (RepoName::from ("private")),
      relationship_axes : RelationshipAxes::default (), }) };
  alias . consume_edit_request_after_save ();
  let ViewnodeKind::Property (Property::Alias { relRepo_request, .. }) =
    &alias . kind
  else { panic! ("expected alias"); };
  assert_eq! (*relRepo_request, None);
}
