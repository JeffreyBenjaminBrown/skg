// Each of the diff view's viewnodes renders its metadata, then
// parses back to the same viewnode, less its change facts, which the
// parser skips.

use crate::git_fixture_helpers::with_facts;
use skg::org_to_text::viewnode_to_string;
use skg::serve::parse_metadata_sexp::{
  RESTORE_NODE_ON_A_NODE_NOT_DELETED, RESTORE_REL_ON_A_DELETED_NODE,
  RESTORE_REL_WITHOUT_A_REMOVED_MEMBERSHIP,
  ViewnodeMetadata, parse_metadata_to_viewnodemd, viewnode_from_metadata};
use skg::types::misc::{ID, SkgConfig, SkgrepoChange, SkgrepoName};
use skg::types::viewnode::{
  ChangeFacts, ChangesetMarkerKind, DiffChange, Existence, Folder, Ghost, ChangeViewnode, PropertyMember, DiffObject, ReadinessRequest, RelationshipEditRequest, Viewnode,
  ViewnodeKind, Vognode, Warrant, mk_editable_viewnode,
  mk_writeProtected_viewnode, viewnode_of};

use std::collections::{BTreeSet, HashMap};

fn ready (
  objects : &[DiffObject],
) -> ChangeFacts {
  ChangeFacts { change   : DiffChange::default (),
                ready    : Some ( objects . iter () . copied () . collect () ),
                leaves   : None,
                .. ChangeFacts::default () } }

fn request_all () -> Option<ReadinessRequest> {
  Some ( ReadinessRequest {
    ready   : true,
    objects : [ DiffObject::All ] . into_iter () . collect::<BTreeSet<_>> () } ) }

/// Render 'node', check its metadata against 'expected', parse it
/// back with its headline and body, and check that the result is
/// 'node' again, less its change facts.
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
  let (parsed, error, warning) : (Viewnode, _, _) =
    viewnode_from_metadata (
      &metadata, node . title () . to_string (), node . body () . cloned () );
  assert_eq! ( error, None );
  assert_eq! ( warning, None );
  assert_eq! ( Viewnode::try_from (parsed) . unwrap (),
               Viewnode { change_facts : ChangeFacts::default (), .. node } ); }

fn change_viewnode (
  change            : ChangeViewnode,
  change_facts      : ChangeFacts,
  readiness_request : Option<ReadinessRequest>,
) -> Viewnode {
  with_facts ( viewnode_of ( ViewnodeKind::ChangeViewnode (change) ),
               change_facts, readiness_request ) }

fn property_member (
  member            : PropertyMember,
  change_facts      : ChangeFacts,
  readiness_request : Option<ReadinessRequest>,
) -> Viewnode {
  with_facts ( viewnode_of ( ViewnodeKind::PropertyMember (member) ),
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
      change_viewnode ( ChangeViewnode::OldText {
                   title : title . map (String::from),
                   body  : body  . map (String::from) },
                 ready (&[]), None ),
      expected ); }}

#[test]
fn body_diff_round_trips () {
  assert_round_trip (
    change_viewnode ( ChangeViewnode::BodyDiff { diff : " line one\n-line two\n+line 2\n" . into () },
               ChangeFacts::default (), None ),
    "bodyDiff" ); }

#[test]
fn skgrepo_moves_round_trip () {
  assert_round_trip (
    change_viewnode ( ChangeViewnode::HomeSkgrepoMove {
      skgrepo_change : SkgrepoChange { old : SkgrepoName::from ("pub"),
                                       new : SkgrepoName::from ("priv") } },
      ready (&[]), None ),
    "(homeSkgrepoMove (old pub) (new priv)) (diff (ready))" );
  assert_round_trip (
    change_viewnode ( ChangeViewnode::RelRepoMove {
      skgrepo_change : SkgrepoChange { old : SkgrepoName::from ("pub"),
                                       new : SkgrepoName::from ("a repo") } },
      ChangeFacts::default (), request_all () ),
    "(relRepoMove (old pub) (new \"a repo\")) (editRequest (ready all))" ); }

#[test]
fn changeset_markers_round_trip () {
  for (kind, expected) in [
    ( ChangesetMarkerKind::Content,   "(changesetMarker content) (diff (ready all))" ),
    ( ChangesetMarkerKind::RelRepo,   "(changesetMarker relRepo) (diff (ready all))" ),
    ( ChangesetMarkerKind::OrderOnly, "(changesetMarker orderOnly) (diff (ready all))" ), ] {
    assert_round_trip (
      change_viewnode ( ChangeViewnode::ChangesetMarker { kind }, ready (&[ DiffObject::All ]), None ),
      expected ); }}

#[test]
/// An alias or ID the worktree lost since HEAD is a member with a
/// ghost warrant, written '(view ghostWarrant)' beside its '(rel
/// removed)', which claims no membership.
fn ghost_warranted_aliases_and_ids_round_trip () {
  let removed : ChangeFacts = ChangeFacts {
    change   : DiffChange { rel : Some (Existence::Removed), .. DiffChange::default () },
    ready    : Some (BTreeSet::new ()),
    leaves   : None,
    .. ChangeFacts::default () };
  assert_round_trip (
    property_member ( PropertyMember::Alias { text : "an old alias" . into (),
                                              relRepo : None,
                                              relationshipEditRequest : None,
                                              claimsMembership : false,
                                              isWarranted : Warrant::ByGraph },
                      removed . clone (), None ),
    "alias noClaim (view ghostWarrant) (diff (rel removed) (ready))" );
  assert_round_trip (
    property_member ( PropertyMember::ID { skgid : ID::from ("old-id"),
                                           claimsMembership : false,
                                           isWarranted : Warrant::ByGraph },
                      removed, request_all () ),
    "id noClaim (editRequest (ready all)) (view ghostWarrant) (diff (rel removed) (ready))" ); }

#[test]
fn a_ghost_round_trips () {
  assert_round_trip (
    with_facts (
      viewnode_of ( ViewnodeKind::Vognode ( Vognode::Ghost ( Ghost {
          skgid : ID::from ("gone"), home_skgrepo : SkgrepoName::from ("main"),
          title : "gone" . into (), body : Some ("its body\n" . into ()),
          restoreNode_request : false,
          tombstone_request : false,
          claimsMembership : true, isWarranted : Warrant::ByGraph } ))),
      ChangeFacts {
        change   : DiffChange { node : Some (Existence::Removed),
                                rel  : Some (Existence::Removed),
                                properties : false },
        ready    : Some ( [ DiffObject::Node ] . into_iter () . collect () ),
        leaves   : None,
        .. ChangeFacts::default () },
      request_all () ),
    "ghost (gnid gone) (skgRepos (nodeRepo main)) (editRequest (ready all)) (diff (node removed) (rel removed) (ready node))" ); }

#[test]
fn the_acquisition_folders_round_trip () {
  assert_round_trip (
    viewnode_of ( ViewnodeKind::Folder (Folder::AcquiredBy) ),
    "acquiredByFolder" );
  assert_round_trip (
    viewnode_of ( ViewnodeKind::Folder (Folder::Acquired) ),
    "acquiredFolder" ); }

/// A chain node is known by where it stands; the parser knows no
/// birth.
#[test]
fn the_birth_atom_is_refused () {
  assert_eq! (
    parse_metadata_to_viewnodemd (
      "(skg node (gnid g) (skgRepos (nodeRepo main)) noClaim (birth roleGraft container) writeProtected)" ),
    Err ( "Unknown metadata key: birth" . to_string () )); }

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
fn a_ghost_round_trips_with_its_restore_request () {
  assert_round_trip (
    with_facts (
      viewnode_of ( ViewnodeKind::Vognode ( Vognode::Ghost ( Ghost {
          skgid : ID::from ("gone"), home_skgrepo : SkgrepoName::from ("main"),
          title : "gone" . into (), body : None,
          restoreNode_request : true,
          tombstone_request : false,
          claimsMembership : true, isWarranted : Warrant::ByGraph } ))),
      ChangeFacts {
        change : DiffChange { node : Some (Existence::Removed),
                              rel  : None,
                              properties : false },
        ready  : None,
        leaves : None,
        .. ChangeFacts::default () },
      None ),
    "ghost (gnid gone) (skgRepos (nodeRepo main)) (editRequest restoreNode) (diff (node removed))" ); }

/// A restoreRel request lives outside the editability, so an
/// editable and a write-protected removed member both carry it.
#[test]
fn a_removed_membership_round_trips_with_its_restore_request () {
  let removed_rel : ChangeFacts = ChangeFacts {
    change : DiffChange { node : None,
                          rel  : Some (Existence::Removed),
                          properties : false },
    ready  : None,
    leaves : None,
    .. ChangeFacts::default () };
  let mut editable : Viewnode = mk_editable_viewnode (
    ID::from ("x"), SkgrepoName::from ("main"), "x" . into (), None );
  let mut write_protected : Viewnode = mk_writeProtected_viewnode (
    ID::from ("x"), SkgrepoName::from ("main"), "x" . into (),
    false );
  for (node, expected) in [
    ( &mut editable,
      "node (gnid x) (skgRepos (nodeRepo main)) (editRequest restoreRel) (diff (rel removed))" ),
    ( &mut write_protected,
      "node (gnid x) (skgRepos (nodeRepo main)) noClaim writeProtected (editRequest restoreRel) (diff (rel removed))" ), ] {
    if let ViewnodeKind::Vognode (Vognode::Unrestricted (t)) = &mut node . kind
    { t . relationshipEditRequest = Some (RelationshipEditRequest::RestoreRel); }
    assert_round_trip (
      with_facts (node . clone (), removed_rel . clone (), None ),
      expected ); }}

/// Each restore request is refused on a headline of the wrong kind,
/// with the text the clients show. A restoreRel on a node is the
/// save's to check, by where the node stands
/// ('restore_requests_on_the_wrong_kind_stop_the_save').
#[test]
fn restore_requests_on_the_wrong_kind_are_refused () {
  for (metadata, message) in [
    ( "(skg ghost (gnid g) (skgRepos (nodeRepo main)) (editRequest restoreRel) (diff (node removed)))",
      RESTORE_REL_ON_A_DELETED_NODE ),
    ( "(skg node (gnid x) (skgRepos (nodeRepo main)) (editRequest restoreNode) (diff (rel removed)))",
      RESTORE_NODE_ON_A_NODE_NOT_DELETED ),
    ( "(skg unknown (gnid x) (editRequest restoreNode))",
      RESTORE_NODE_ON_A_NODE_NOT_DELETED ),
    ( "(skg unknown (gnid x) noClaim (editRequest restoreRel) (diff (rel removed)))",
      RESTORE_REL_WITHOUT_A_REMOVED_MEMBERSHIP ), ] {
    assert_eq! ( parse_metadata_to_viewnodemd (metadata),
                 Err ( message . to_string () ),
                 "{}", metadata ); }}

/// One edit request per node, and a ghost takes no other.
#[test]
fn a_ghost_takes_no_other_edit_request () {
  for metadata in [
    "(skg ghost (gnid g) (skgRepos (nodeRepo main)) (editRequest delete))",
    "(skg ghost (gnid g) (skgRepos (nodeRepo main)) (editRequest (relRepo private)))",
    "(skg ghost (gnid g) (skgRepos (nodeRepo main)) (editRequest restoreNode) (editRequest restoreNode))",
    "(skg node (gnid x) (editRequest restoreRel) (editRequest delete) (diff (rel removed)))", ] {
    assert! ( parse_metadata_to_viewnodemd (metadata) . is_err (),
              "should refuse {}", metadata ); }}
