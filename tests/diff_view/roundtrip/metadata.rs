// Each of the diff view's viewnodes renders its metadata, then
// parses back to the same viewnode.

use crate::git_fixture_helpers::with_facts;
use skg::dbs::in_rust_graph::relation_accessors::RelationRole;
use skg::org_to_text::viewnode_to_string;
use skg::serve::parse_metadata_sexp::{
  RESTORE_NODE_ON_A_NODE_NOT_DELETED, RESTORE_REL_ON_A_DELETED_NODE,
  RESTORE_REL_WITHOUT_A_REMOVED_MEMBERSHIP,
  ViewnodeMetadata, parse_metadata_to_viewnodemd, viewnode_from_metadata};
use skg::types::maybe_placed_viewnode::MpViewnode;
use skg::types::misc::{ID, SkgConfig, SkgrepoChange, SkgrepoName};
use skg::types::viewnode::{
  AffectsParent, Birth, ChangeFacts, ChangesetMarkerKind, DiffChange, Existence, PartnerFolder, Phantom,
  PhantomHistorical, Property, DiffObject, ReadinessRequest, Viewnode,
  ViewnodeKind, Vognode, mk_editable_viewnode,
  mk_writeProtected_viewnode_with_birth, viewnode_of};

use std::collections::{BTreeSet, HashMap};

fn ready (
  objects : &[DiffObject],
) -> ChangeFacts {
  ChangeFacts { change   : DiffChange::default (),
                ready    : Some ( objects . iter () . copied () . collect () ),
                leaves   : None } }

fn request_all () -> Option<ReadinessRequest> {
  Some ( ReadinessRequest {
    ready   : true,
    objects : [ DiffObject::All ] . into_iter () . collect::<BTreeSet<_>> () } ) }

/// Render 'node', check its metadata against 'expected', parse it
/// back with its headline and body, and check that the result is
/// 'node' again.
fn assert_round_trip (
  node     : Viewnode,
  expected : &str,
) {
  let config : SkgConfig = SkgConfig::dummyFromSkgrepos (HashMap::new ());
  let rendered : String = viewnode_to_string (&node, &config) . unwrap ();
  assert_eq! ( rendered, expected );
  let metadata : ViewnodeMetadata =
    parse_metadata_to_viewnodemd ( &format! ("(skg {})", rendered) )
    . unwrap ();
  let (parsed, error, warning) : (MpViewnode, _, _) =
    viewnode_from_metadata (
      &metadata, node . title () . to_string (), node . body () . cloned () );
  assert_eq! ( error, None );
  assert_eq! ( warning, None );
  assert_eq! ( Viewnode::try_from (parsed) . unwrap (), node ); }

fn property (
  property          : Property,
  change_facts      : ChangeFacts,
  readiness_request : Option<ReadinessRequest>,
) -> Viewnode {
  with_facts ( viewnode_of ( ViewnodeKind::Property (property) ),
               change_facts, readiness_request ) }

#[test]
fn old_text_round_trips_each_aspect () {
  for (title, body, expected) in [
    ( Some ("the old title"), Some ("the old body\n"), "(oldText titleAndBody) (diff (ready))" ),
    ( Some ("the old title"), None,                    "(oldText title) (diff (ready))" ),
    ( None,                   Some ("the old body\n"), "(oldText body) (diff (ready))" ),
    // A body HEAD lacked is the empty string.
    ( None,                   Some (""),               "(oldText body) (diff (ready))" ), ] {
    assert_round_trip (
      property ( Property::OldText {
                   title : title . map (String::from),
                   body  : body  . map (String::from) },
                 ready (&[]), None ),
      expected ); }}

#[test]
fn body_diff_round_trips () {
  assert_round_trip (
    property ( Property::BodyDiff { diff : " line one\n-line two\n+line 2\n" . into () },
               ChangeFacts::default (), None ),
    "bodyDiff" ); }

#[test]
fn skgrepo_moves_round_trip () {
  assert_round_trip (
    property ( Property::HomeSkgrepoMove {
      skgrepo_change : SkgrepoChange { old : SkgrepoName::from ("pub"),
                                       new : SkgrepoName::from ("priv") } },
      ready (&[]), None ),
    "(homeSkgrepoMove (old pub) (new priv)) (diff (ready))" );
  assert_round_trip (
    property ( Property::RelRepoMove {
      skgrepo_change : SkgrepoChange { old : SkgrepoName::from ("pub"),
                                       new : SkgrepoName::from ("a repo") } },
      ChangeFacts::default (), request_all () ),
    "(relRepoMove (old pub) (new \"a repo\")) (diff (readyRequest all))" ); }

#[test]
fn changeset_markers_round_trip () {
  for (kind, expected) in [
    ( ChangesetMarkerKind::Content,   "(changesetMarker content) (diff (ready all))" ),
    ( ChangesetMarkerKind::RelRepo,   "(changesetMarker relRepo) (diff (ready all))" ),
    ( ChangesetMarkerKind::OrderOnly, "(changesetMarker orderOnly) (diff (ready all))" ), ] {
    assert_round_trip (
      property ( Property::ChangesetMarker { kind }, ready (&[ DiffObject::All ]), None ),
      expected ); }}

#[test]
fn removed_aliases_and_ids_round_trip () {
  let removed : ChangeFacts = ChangeFacts {
    change   : DiffChange { rel : Some (Existence::Removed), .. DiffChange::default () },
    ready    : Some (BTreeSet::new ()),
    leaves   : None };
  assert_round_trip (
    property ( Property::RemovedAlias { text : "an old alias" . into () },
               removed . clone (), None ),
    "removedAlias (diff (rel removed) (ready))" );
  assert_round_trip (
    property ( Property::RemovedID { skgid : ID::from ("old-id") },
               removed, request_all () ),
    "removedID (diff (rel removed) (ready) (readyRequest all))" ); }

#[test]
fn a_historical_phantom_round_trips () {
  assert_round_trip (
    with_facts (
      viewnode_of ( ViewnodeKind::Vognode ( Vognode::Phantom (
        Phantom::Historical ( PhantomHistorical {
          skgid : ID::from ("gone"), home_skgrepo : SkgrepoName::from ("main"),
          title : "gone" . into (), body : Some ("its body\n" . into ()),
          restoreNode_request : false } )))),
      ChangeFacts {
        change   : DiffChange { node : Some (Existence::Removed),
                                rel  : Some (Existence::Removed),
                                properties : false },
        ready    : Some ( [ DiffObject::Node ] . into_iter () . collect () ),
        leaves   : None },
      request_all () ),
    "(historical (id gone) (repo main) (diff (node removed) (rel removed) (ready node) (readyRequest all)))" ); }

#[test]
fn the_acquisition_folders_round_trip () {
  assert_round_trip (
    viewnode_of ( ViewnodeKind::PartnerFolder (PartnerFolder::AcquiredBy) ),
    "acquiredByFolder" );
  assert_round_trip (
    viewnode_of ( ViewnodeKind::PartnerFolder (PartnerFolder::Acquired) ),
    "acquiredFolder" ); }

/// A role graft's birth survives a save, so the diff view's
/// reconciler can tell a graft from a note the user added.
#[test]
fn a_role_graft_round_trips_with_its_birth () {
  for (role, expected) in [
    ( RelationRole::CONTAINER,
      "(node (id g) (repo main) (affectsParent false) (birth roleGraft container) writeProtected)" ),
    ( RelationRole::SUBSCRIBEE,
      "(node (id g) (repo main) (affectsParent false) (birth roleGraft subscribee) writeProtected)" ), ] {
    let graft : Viewnode = mk_writeProtected_viewnode_with_birth (
      ID::from ("g"), SkgrepoName::from ("main"), "g" . into (),
      AffectsParent::False, Birth::RoleGraft (role) );
    assert_round_trip (graft, expected); }}

#[test]
fn malformed_birth_metadata_is_refused () {
  for metadata in [
    "(skg (node (id g) (birth roleGraft)))",              // no role
    "(skg (node (id g) (birth roleGraft content)))",      // no partner role
    "(skg (node (id g) (birth unremarkable container)))", ] {
    assert! ( parse_metadata_to_viewnodemd (metadata) . is_err (),
              "should refuse {}", metadata ); }}

#[test]
fn malformed_diff_view_metadata_is_refused () {
  for metadata in [
    "(skg (oldText))",                               // no aspect
    "(skg (oldText title body))",                    // two aspects
    "(skg (oldText sideways))",
    "(skg (homeSkgrepoMove (old a)))",                      // no new
    "(skg (homeSkgrepoMove (old a) (old b) (new c)))",      // old twice
    "(skg (relRepoMove a b))",                       // bare names
    "(skg (changesetMarker sideways))",
    "(skg bodyDiff (diff (ready)))", ] {                    // a body diff is no leaf
    assert! ( parse_metadata_to_viewnodemd (metadata) . is_err (),
              "should refuse {}", metadata ); }}

#[test]
fn a_historical_phantom_round_trips_with_its_restore_request () {
  assert_round_trip (
    with_facts (
      viewnode_of ( ViewnodeKind::Vognode ( Vognode::Phantom (
        Phantom::Historical ( PhantomHistorical {
          skgid : ID::from ("gone"), home_skgrepo : SkgrepoName::from ("main"),
          title : "gone" . into (), body : None,
          restoreNode_request : true } )))),
      ChangeFacts {
        change : DiffChange { node : Some (Existence::Removed),
                              rel  : None,
                              properties : false },
        ready  : None,
        leaves : None },
      None ),
    "(historical (id gone) (repo main) (editRequest restoreNode) (diff (node removed)))" ); }

/// A restoreRel request lives outside the editability, so an
/// editable and a write-protected removed member both carry it.
#[test]
fn a_removed_membership_round_trips_with_its_restore_request () {
  let removed_rel : ChangeFacts = ChangeFacts {
    change : DiffChange { node : None,
                          rel  : Some (Existence::Removed),
                          properties : false },
    ready  : None,
    leaves : None };
  let mut editable : Viewnode = mk_editable_viewnode (
    ID::from ("x"), SkgrepoName::from ("main"), "x" . into (), None );
  let mut write_protected : Viewnode = mk_writeProtected_viewnode_with_birth (
    ID::from ("x"), SkgrepoName::from ("main"), "x" . into (),
    AffectsParent::False, Birth::Unremarkable );
  for (node, expected) in [
    ( &mut editable,
      "(node (id x) (repo main) (editRequest restoreRel) (diff (rel removed)))" ),
    ( &mut write_protected,
      "(node (id x) (repo main) (affectsParent false) writeProtected (editRequest restoreRel) (diff (rel removed)))" ), ] {
    if let ViewnodeKind::Vognode (Vognode::Unrestricted (t)) = &mut node . kind
    { t . restoreRel_request = true; }
    assert_round_trip (
      with_facts (node . clone (), removed_rel . clone (), None ),
      expected ); }}

/// Each restore request is refused on a headline of the wrong kind,
/// with the text the clients show.
#[test]
fn restore_requests_on_the_wrong_kind_are_refused () {
  for (metadata, message) in [
    ( "(skg (historical (id g) (repo main) (editRequest restoreRel) (diff (node removed))))",
      RESTORE_REL_ON_A_DELETED_NODE ),
    ( "(skg (node (id x) (repo main) (editRequest restoreNode) (diff (rel removed))))",
      RESTORE_NODE_ON_A_NODE_NOT_DELETED ),
    ( "(skg (deleted (id x) (repo main) (editRequest restoreNode)))",
      RESTORE_NODE_ON_A_NODE_NOT_DELETED ),
    ( "(skg (unknown (id x) (editRequest restoreNode)))",
      RESTORE_NODE_ON_A_NODE_NOT_DELETED ),
    ( "(skg (node (id x) (repo main) (editRequest restoreRel)))",
      RESTORE_REL_WITHOUT_A_REMOVED_MEMBERSHIP ),
    ( "(skg (node (id x) (repo main) (editRequest restoreRel) (diff (rel added))))",
      RESTORE_REL_WITHOUT_A_REMOVED_MEMBERSHIP ),
    ( "(skg (node (id g) (repo main) (birth roleGraft container) (editRequest restoreRel) (diff (rel removed))))",
      RESTORE_REL_WITHOUT_A_REMOVED_MEMBERSHIP ),
    ( "(skg (unknown (id x) (editRequest restoreRel) (diff (rel removed))))",
      RESTORE_REL_WITHOUT_A_REMOVED_MEMBERSHIP ), ] {
    assert_eq! ( parse_metadata_to_viewnodemd (metadata),
                 Err ( message . to_string () ),
                 "{}", metadata ); }}

/// One edit request per node, and a phantom takes no other.
#[test]
fn a_phantom_takes_no_other_edit_request () {
  for metadata in [
    "(skg (historical (id g) (repo main) (editRequest delete)))",
    "(skg (deleted (id g) (repo main) (editRequest delete)))",
    "(skg (historical (id g) (repo main) (editRequest (relRepo private))))",
    "(skg (historical (id g) (repo main) (editRequest restoreNode) (editRequest restoreNode)))",
    "(skg (node (id x) (editRequest restoreRel) (editRequest delete) (diff (rel removed))))", ] {
    assert! ( parse_metadata_to_viewnodemd (metadata) . is_err (),
              "should refuse {}", metadata ); }}
