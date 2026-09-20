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
               FolderPolicy::ReadOnlySet );
  assert_eq! ( PartnerFolder::Overrider . policy (),
               FolderPolicy::ReadOnlySet );
  assert_eq! ( PartnerFolder::Hider . policy (),
               FolderPolicy::ReadOnlySet );
  assert_eq! ( PartnerFolder::Hidden . policy (),
               FolderPolicy::ReadOnlySet );
  assert_eq! ( PartnerFolder::HiddenInSubscribee . policy (),
               FolderPolicy::ReadOnlyFilter );
  assert_eq! ( PartnerFolder::HiddenOutsideOfSubscribee . policy (),
               FolderPolicy::EditableFilter ); }

#[test]
fn consuming_edit_requests_covers_every_carrier_but_not_view_requests () {
  let mut active : ViewNode = mk_viewnode (
    ID::from ("active"), SourceName::from ("public"), "active" . into (),
    AffectsParent::True, Birth::Unremarkable,
    Editability::Definitive {
      body : None,
      edit_request : Some (NodeEditRequest::Delete) },
    [ViewRequest::Definitive] . into_iter () . collect () );
  if let ViewNodeKind::Vognode (Vognode::Active (node)) = &mut active . kind {
    node . relSource_request = Some (SourceName::from ("private")); }
  active . consume_edit_request_after_save ();
  let ViewNodeKind::Vognode (Vognode::Active (active)) = &active . kind
  else { panic! ("expected active node"); };
  assert_eq! (active . relSource_request, None);
  assert_eq! (active . edit_request (), None);
  assert! (active . view_requests . contains (&ViewRequest::Definitive));

  let mut unknown : ViewNode = ViewNode {
    focused : false, folded : false, body_folded : false,
    kind : ViewNodeKind::Phantom (Phantom::Unknown (PhantomUnknown {
      id : ID::from ("unknown"),
      relSource : None,
      relSource_request : Some (SourceName::from ("private")), })) };
  unknown . consume_edit_request_after_save ();
  let ViewNodeKind::Phantom (Phantom::Unknown (unknown)) = &unknown . kind
  else { panic! ("expected unknown node"); };
  assert_eq! (unknown . relSource_request, None);

  let mut alias : ViewNode = ViewNode {
    focused : false, folded : false, body_folded : false,
    kind : ViewNodeKind::Qual (Qual::Alias {
      text : "alias" . into (),
      relSource : None,
      relSource_request : Some (SourceName::from ("private")),
      membership : MembershipAxes::default (), }) };
  alias . consume_edit_request_after_save ();
  let ViewNodeKind::Qual (Qual::Alias { relSource_request, .. }) =
    &alias . kind
  else { panic! ("expected alias"); };
  assert_eq! (*relSource_request, None);
}
