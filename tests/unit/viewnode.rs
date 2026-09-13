use super::*;

// 'PartnerCol::policy' is the single source of truth for how each
// col's membership relates to user edits; this pins the mapping.
#[test]
fn partnerCol_policy_mapping () {
  assert_eq! ( PartnerCol::Subscribee . policy (),
               ColPolicy::WritableSet );
  assert_eq! ( PartnerCol::Overridden . policy (),
               ColPolicy::WritableSet );
  assert_eq! ( PartnerCol::Subscriber . policy (),
               ColPolicy::ReadOnlySet );
  assert_eq! ( PartnerCol::Overrider . policy (),
               ColPolicy::ReadOnlySet );
  assert_eq! ( PartnerCol::Hider . policy (),
               ColPolicy::ReadOnlySet );
  assert_eq! ( PartnerCol::Hidden . policy (),
               ColPolicy::ReadOnlySet );
  assert_eq! ( PartnerCol::HiddenInSubscribee . policy (),
               ColPolicy::ReadOnlyFilter );
  assert_eq! ( PartnerCol::HiddenOutsideOfSubscribee . policy (),
               ColPolicy::EditableFilter ); }

#[test]
fn consuming_edit_requests_covers_every_carrier_but_not_view_requests () {
  let mut active : ViewNode = mk_viewnode (
    ID::from ("active"), SourceName::from ("public"), "active" . into (),
    ParentIs::Affected, Birth::Unremarkable,
    IndefOrDef::Definitive {
      body : None,
      edit_request : Some (NodeEditRequest::Delete) },
    [ViewRequest::Definitive] . into_iter () . collect () );
  if let ViewNodeKind::Vognode (Vognode::Active (node)) = &mut active . kind {
    node . rel_source_request = Some (SourceName::from ("private")); }
  active . consume_edit_request_after_save ();
  let ViewNodeKind::Vognode (Vognode::Active (active)) = &active . kind
  else { panic! ("expected active node"); };
  assert_eq! (active . rel_source_request, None);
  assert_eq! (active . edit_request (), None);
  assert! (active . view_requests . contains (&ViewRequest::Definitive));

  let mut unknown : ViewNode = ViewNode {
    focused : false, folded : false, body_folded : false,
    kind : ViewNodeKind::Phantom (Phantom::Unknown (PhantomUnknown {
      id : ID::from ("unknown"),
      rel_source : None,
      rel_source_request : Some (SourceName::from ("private")), })) };
  unknown . consume_edit_request_after_save ();
  let ViewNodeKind::Phantom (Phantom::Unknown (unknown)) = &unknown . kind
  else { panic! ("expected unknown node"); };
  assert_eq! (unknown . rel_source_request, None);

  let mut alias : ViewNode = ViewNode {
    focused : false, folded : false, body_folded : false,
    kind : ViewNodeKind::Qual (Qual::Alias {
      text : "alias" . into (),
      rel_source : None,
      rel_source_request : Some (SourceName::from ("private")),
      membership : MembershipAxes::default (), }) };
  alias . consume_edit_request_after_save ();
  let ViewNodeKind::Qual (Qual::Alias { rel_source_request, .. }) =
    &alias . kind
  else { panic! ("expected alias"); };
  assert_eq! (*rel_source_request, None);
}
