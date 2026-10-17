// Each of the total diff view's viewnodes renders its metadata, then
// parses back to the same viewnode.

use skg::org_to_text::viewnode_to_string;
use skg::serve::parse_metadata_sexp::{
  ViewnodeMetadata, parse_metadata_to_viewnodemd, viewnode_from_metadata};
use skg::types::git::Sign;
use skg::types::maybe_placed_viewnode::MpViewnode;
use skg::types::misc::{ID, SkgConfig, SkgrepoName};
use skg::types::viewnode::{
  ChangeFacts, ChangesetMarkerKind, DiffPresence, PartnerFolder, Phantom,
  PhantomHistorical, Property, ReadinessAxis, ReadinessRequest, Viewnode,
  ViewnodeKind, Vognode};

use std::collections::{BTreeSet, HashMap};

fn ready (
  axes : &[ReadinessAxis],
) -> ChangeFacts {
  ChangeFacts { presence : DiffPresence::default (),
                ready    : Some ( axes . iter () . copied () . collect () ) } }

fn request_all () -> Option<ReadinessRequest> {
  Some ( ReadinessRequest {
    ready : true,
    axes  : [ ReadinessAxis::All ] . into_iter () . collect::<BTreeSet<_>> () } ) }

fn viewnode (
  kind : ViewnodeKind,
) -> Viewnode {
  Viewnode { focused : false, folded : false, body_folded : false, kind } }

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
  property : Property,
) -> Viewnode {
  viewnode ( ViewnodeKind::Property (property) ) }

#[test]
fn old_text_round_trips_each_aspect () {
  for (title, body, expected) in [
    ( Some ("the old title"), Some ("the old body\n"), "(oldText titleAndBody) (ready)" ),
    ( Some ("the old title"), None,                    "(oldText title) (ready)" ),
    ( None,                   Some ("the old body\n"), "(oldText body) (ready)" ),
    // A body HEAD lacked is the empty string.
    ( None,                   Some (""),               "(oldText body) (ready)" ), ] {
    assert_round_trip (
      property ( Property::OldText {
        title : title . map (String::from),
        body  : body  . map (String::from),
        change_facts : ready (&[]), readiness_request : None } ),
      expected ); }}

#[test]
fn body_diff_round_trips () {
  assert_round_trip (
    property ( Property::BodyDiff { diff : " line one\n-line two\n+line 2\n" . into () } ),
    "bodyDiff" ); }

#[test]
fn skgrepo_moves_round_trip () {
  assert_round_trip (
    property ( Property::HomeSkgrepoMove {
      old : SkgrepoName::from ("pub"), new : SkgrepoName::from ("priv"),
      change_facts : ready (&[]), readiness_request : None } ),
    "(homeMove (old pub) (new priv)) (ready)" );
  assert_round_trip (
    property ( Property::RelRepoMove {
      old : SkgrepoName::from ("pub"), new : SkgrepoName::from ("a repo"),
      change_facts : ChangeFacts::default (), readiness_request : request_all () } ),
    "(relRepoMove (old pub) (new \"a repo\")) (readyRequest all)" ); }

#[test]
fn changeset_markers_round_trip () {
  for (kind, expected) in [
    ( ChangesetMarkerKind::Content,   "(changesetMarker content) (ready)" ),
    ( ChangesetMarkerKind::RelRepo,   "(changesetMarker relRepo) (ready)" ),
    ( ChangesetMarkerKind::OrderOnly, "(changesetMarker orderOnly) (ready)" ), ] {
    assert_round_trip (
      property ( Property::ChangesetMarker {
        kind, change_facts : ready (&[]), readiness_request : None } ),
      expected ); }}

#[test]
fn a_historical_phantom_round_trips () {
  assert_round_trip (
    viewnode ( ViewnodeKind::Vognode ( Vognode::Phantom (
      Phantom::Historical ( PhantomHistorical {
        skgid : ID::from ("gone"), home_skgrepo : SkgrepoName::from ("main"),
        title : "gone" . into (), body : Some ("its body\n" . into ()),
        change_facts : ChangeFacts {
          presence : DiffPresence { node : Some (Sign::Minus),
                                    rel  : Some (Sign::Minus) },
          ready    : Some ( [ ReadinessAxis::Node ] . into_iter () . collect () ) },
        readiness_request : request_all () } )))),
    "(historical (id gone) (repo main) (diff node- rel-) (ready node) (readyRequest all))" ); }

#[test]
fn the_acquisition_folders_round_trip () {
  assert_round_trip (
    viewnode ( ViewnodeKind::PartnerFolder (PartnerFolder::AcquiredBy) ),
    "acquiredByFolder" );
  assert_round_trip (
    viewnode ( ViewnodeKind::PartnerFolder (PartnerFolder::Acquired) ),
    "acquiredFolder" ); }

#[test]
fn malformed_total_diff_view_metadata_is_refused () {
  for metadata in [
    "(skg (oldText))",                               // no aspect
    "(skg (oldText title body))",                    // two aspects
    "(skg (oldText sideways))",
    "(skg (homeMove (old a)))",                      // no new
    "(skg (homeMove (old a) (old b) (new c)))",      // old twice
    "(skg (relRepoMove a b))",                       // bare names
    "(skg (changesetMarker sideways))",
    "(skg bodyDiff (ready))", ] {                    // a body diff is no leaf
    assert! ( parse_metadata_to_viewnodemd (metadata) . is_err (),
              "should refuse {}", metadata ); }}
