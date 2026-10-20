use super::*;
use crate::serve::parse_metadata_sexp::{ViewnodeMetadata, parse_metadata_to_viewnodemd};
use crate::types::tree::forest::{ViewForest, editable_occurrences};

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
      ready : true, objects : [DiffObject::All] . into_iter () . collect () }); }
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
        ready : true, objects : BTreeSet::new () }), }))) };
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
        ready : false, objects : BTreeSet::new () }), }) };
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

// The four accessors of the (change facts, readiness request) pair
// reach it on every kind that carries it, and answer None on every
// kind that does not.
#[test]
fn the_change_facts_and_readiness_request_accessors_cover_every_kind () {
  let facts : ChangeFacts = ChangeFacts {
    presence : DiffPresence { node : Some (Sign::Plus), rel : None },
    ready    : None };
  let request : ReadinessRequest = ReadinessRequest {
    ready : true, objects : [DiffObject::Node] . into_iter () . collect () };
  let unrestricted : ViewnodeKind = {
    let mut t : UnrestrictedVognode = default_unrestrictedVognode (
      ID::from ("node"), SkgrepoName::from ("public"), "node" . into () );
    t . change_facts      = facts . clone ();
    t . readiness_request = Some (request . clone ());
    ViewnodeKind::Vognode (Vognode::Unrestricted (t)) };
  let carriers : Vec<ViewnodeKind> = vec! [
    unrestricted,
    ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Deleted (PhantomDeleted {
      skgid : ID::from ("deleted"), home_skgrepo : SkgrepoName::from ("public"),
      title : "deleted" . into (), body : None,
      change_facts : facts . clone (), readiness_request : Some (request . clone ()) }))),
    ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Unknown (PhantomUnknown {
      skgid : ID::from ("unknown"), relRepo : None, relRepo_request : None,
      change_facts : facts . clone (), readiness_request : Some (request . clone ()) }))),
    ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Historical (PhantomHistorical {
      skgid : ID::from ("historical"), home_skgrepo : SkgrepoName::from ("public"),
      title : "historical" . into (), body : None,
      change_facts : facts . clone (), readiness_request : Some (request . clone ()) }))),
    ViewnodeKind::Property (Property::Alias {
      text : "alias" . into (), relRepo : None, relRepo_request : None,
      change_facts : facts . clone (), readiness_request : Some (request . clone ()) }),
    ViewnodeKind::Property (Property::ID {
      skgid : ID::from ("id"),
      change_facts : facts . clone (), readiness_request : Some (request . clone ()) }),
    ViewnodeKind::Property (Property::Flag {
      flag : Flag::NoSearchMatching, title : "flag" . into (), body : None,
      change_facts : facts . clone (), readiness_request : Some (request . clone ()) }),
    ViewnodeKind::Property (Property::RemovedAlias {
      text : "alias" . into (),
      change_facts : facts . clone (), readiness_request : Some (request . clone ()) }),
    ViewnodeKind::Property (Property::RemovedID {
      skgid : ID::from ("id"),
      change_facts : facts . clone (), readiness_request : Some (request . clone ()) }),
    ViewnodeKind::Property (Property::OldText {
      title : Some ("old" . into ()), body : None,
      change_facts : facts . clone (), readiness_request : Some (request . clone ()) }),
    ViewnodeKind::Property (Property::HomeSkgrepoMove {
      old : SkgrepoName::from ("public"), new : SkgrepoName::from ("private"),
      change_facts : facts . clone (), readiness_request : Some (request . clone ()) }),
    ViewnodeKind::Property (Property::RelRepoMove {
      old : SkgrepoName::from ("public"), new : SkgrepoName::from ("private"),
      change_facts : facts . clone (), readiness_request : Some (request . clone ()) }),
    ViewnodeKind::Property (Property::ChangesetMarker {
      kind : ChangesetMarkerKind::Content,
      change_facts : facts . clone (), readiness_request : Some (request . clone ()) }), ];
  for mut kind in carriers {
    assert_eq! (kind . change_facts (), Some (&facts), "{:?}", kind);
    assert_eq! (kind . readiness_request (), Some (&request), "{:?}", kind);
    kind . change_facts_mut () . unwrap () . ready = Some (BTreeSet::new ());
    assert_eq! ( kind . change_facts () . unwrap () . ready,
                 Some (BTreeSet::new ()), "{:?}", kind );
    *kind . readiness_request_mut () . unwrap () = None;
    assert_eq! (kind . readiness_request (), None, "{:?}", kind);
    assert! (kind . readiness_request_mut () . is_some (), "{:?}", kind); }
  let others : Vec<ViewnodeKind> = vec! [
    ViewnodeKind::Vognode (Vognode::Restricted (RestrictedVognode)),
    ViewnodeKind::PropertyFolder (PropertyFolder::ID),
    ViewnodeKind::PropertyFolder (PropertyFolder::Alias),
    ViewnodeKind::PropertyFolder (PropertyFolder::flags ()),
    ViewnodeKind::Property (Property::BodyDiff { diff : "diff" . into () }),
    ViewnodeKind::PartnerFolder (PartnerFolder::Container),
    ViewnodeKind::BufferRoot,
    ViewnodeKind::DeadViewnode, ];
  for mut kind in others {
    assert_eq! (kind . change_facts (), None, "{:?}", kind);
    assert_eq! (kind . readiness_request (), None, "{:?}", kind);
    assert! (kind . change_facts_mut () . is_none (), "{:?}", kind);
    assert! (kind . readiness_request_mut () . is_none (), "{:?}", kind); }}
