// cargo nextest run --test grouped_unit -E 'test(change_facts_metadata::)'
//
// The diff view's derived facts '(diff ...)' -- its parts '(node
// ...)', '(rel ...)', '(properties changed)', '(leaves ...)' and
// '(processed ...)' -- and the processed requests '(editRequest (processed
// ...))' and '(editRequest (unprocessed ...))', on every viewnode that
// carries them: each renders; the parser skips '(diff ...)' whole and
// reads the processed request back.

use crate::git_fixture_helpers::{assert_round_trip, with_facts};
use skg::org_to_text::viewnode_to_string;
use skg::serve::parse_metadata_sexp::{
  MetadataKind, OccurrenceMetadata, ViewnodeMetadata, parse_metadata_to_viewnodemd};
use skg::types::misc::{ID, SkgrepoName};
use skg::types::nodes::complete::Flag;
use skg::types::viewnode::{
  ChangeFacts, DiffChange, Existence, Ghost, GnidOnlyKind, GnidOnlyVognode, LeavesFact, PropertyMember, DiffObject, ProcessedRequest, Viewnode,
  ViewnodeKind, Vognode, Warrant, mk_editable_viewnode,
  mk_writeProtected_viewnode, viewnode_of};

use std::collections::BTreeSet;

/// The claim of a vognode built by hand that claims membership.
const CLAIMS_MEMBERSHIP : bool = true;

fn objects (
  objects : &[DiffObject],
) -> BTreeSet<DiffObject> {
  objects . iter () . copied () . collect () }

fn facts (
  node  : Option<Existence>,
  rel   : Option<Existence>,
  processed : Option<&[DiffObject]>,
) -> ChangeFacts {
  ChangeFacts { change : DiffChange { node, rel, properties : false },
                processed : processed . map (objects),
                leaves : None,
                .. ChangeFacts::default () } }

/// The facts of a node in both graphs whose other properties changed.
fn modified (
  rel   : Option<Existence>,
  processed : Option<&[DiffObject]>,
) -> ChangeFacts {
  let mut change_facts : ChangeFacts = facts (None, rel, processed);
  change_facts . change . properties = true;
  change_facts }

fn request (
  processed : bool,
  on    : &[DiffObject],
) -> Option<ProcessedRequest> {
  Some ( ProcessedRequest { processed, objects : objects (on) } ) }



#[test]
fn a_vognode_renders_its_change_and_processed_and_round_trips_its_request () {
  assert_round_trip (
    with_facts (
      mk_editable_viewnode (
        ID::from ("a"), SkgrepoName::from ("main"), "A" . into (), None ),
      facts ( Some (Existence::Added), Some (Existence::Removed),
              Some (&[DiffObject::Node, DiffObject::Rel]) ),
      request (true, &[DiffObject::All]) ),
    "node (gnid a) (skgRepos (nodeRepo main)) (editRequest (processed all)) (diff (node added) (rel removed) (processed node rel))" );
  // A processed request is allowed on a write-protected occurrence.
  assert_round_trip (
    with_facts (
      mk_writeProtected_viewnode (
        ID::from ("b"), SkgrepoName::from ("main"), "B" . into (),
        true ),
      facts ( None, Some (Existence::Added), Some (&[]) ),
      request (false, &[DiffObject::Rel]) ),
    "node (gnid b) (skgRepos (nodeRepo main)) writeProtected (editRequest (unprocessed rel)) (diff (rel added) (processed))" );
  // A modified node: its other properties changed, and '(processed all)'
  // says every leaf it records is marked.
  assert_round_trip (
    with_facts (
      mk_writeProtected_viewnode (
        ID::from ("m"), SkgrepoName::from ("main"), "M" . into (),
        true ),
      modified ( Some (Existence::Added),
                 Some (&[DiffObject::Rel, DiffObject::All]) ),
      request (false, &[DiffObject::Rel, DiffObject::All]) ),
    "node (gnid m) (skgRepos (nodeRepo main)) writeProtected (editRequest (unprocessed rel all)) (diff (rel added) (properties changed) (processed rel all))" ); }

/// A node's own edit request and its processed request share one
/// '(editRequest ...)', the processed request last.
#[test]
fn an_edit_request_and_a_processed_request_share_one_form () {
  let metadata : ViewnodeMetadata = parse_metadata_to_viewnodemd (
    "(skg node (gnid a) (editRequest delete (processed node)))" ) . unwrap ();
  assert_eq! ( metadata . processed_request, request (true, &[DiffObject::Node]) );
  assert! ( matches! ( metadata . kind,
                       MetadataKind::Occurrence ( OccurrenceMetadata {
                         edit_request : Some (_), .. } ))); }

#[test]
fn ghosts_and_unknowns_round_trip_their_processed_requests () {
  assert_round_trip (
    with_facts (
      viewnode_of ( ViewnodeKind::Vognode ( Vognode::Ghost ( Ghost {
          gnid : ID::from ("d"), home_skgrepo : SkgrepoName::from ("main"),
          title : "D" . into (), body : None,
          restoreNode_request : false,
          claimsMembership : true, warrant : Warrant::ByGraph, } ))),
      facts ( Some (Existence::Removed), None, Some (&[DiffObject::Node]) ),
      request (false, &[DiffObject::Node]) ),
    "ghost (gnid d) (skgRepos (nodeRepo main)) (editRequest (unprocessed node)) (diff (node removed) (processed node))" );
  assert_round_trip (
    with_facts (
      viewnode_of ( ViewnodeKind::Vognode ( Vognode::GnidOnly ( GnidOnlyVognode::new (
          GnidOnlyKind::Unknown, ID::from ("u"), None, CLAIMS_MEMBERSHIP,
          Warrant::ByGraph ) ))),
      facts ( None, Some (Existence::Added), None ),
      request (true, &[]) ),
    "unknown (gnid u) (editRequest (processed)) (diff (rel added))" ); }

#[test]
fn property_leaves_round_trip_their_processed_requests () {
  assert_round_trip (
    with_facts (
      viewnode_of ( ViewnodeKind::PropertyMember ( PropertyMember::alias (
        "an alias" . into (), None, None ))),
      facts ( None, Some (Existence::Added), Some (&[]) ),
      None ),
    "alias (diff (rel added) (processed))" );
  assert_round_trip (
    with_facts (
      viewnode_of ( ViewnodeKind::PropertyMember ( PropertyMember::id (
        ID::from ("x") ))),
      facts ( None, Some (Existence::Added), None ),
      request (true, &[]) ),
    "id (editRequest (processed)) (diff (rel added))" );
  assert_round_trip (
    with_facts (
      viewnode_of ( ViewnodeKind::PropertyMember ( PropertyMember::flag (
        Flag::NoSearchMatching, String::new (), None ))),
      facts ( None, Some (Existence::Added), None ),
      None ),
    &format! ( "(flag {}) (diff (rel added))", Flag::NoSearchMatching . wire_name () )); }

/// An alias or ID the worktree lost: HEAD alone warrants it, it claims
/// no membership, and the claim is what the parser reads back.
#[test]
fn a_ghost_warranted_property_member_claims_nothing () {
  let lost_id : Viewnode = with_facts (
    viewnode_of ( ViewnodeKind::PropertyMember ( PropertyMember::ID {
      gnid : ID::from ("x"),
      claimsMembership : false,
      warrant : skg::types::viewnode::Warrant::ByHead } )),
    facts ( None, Some (Existence::Removed), None ),
    request (true, &[]) );
  assert_round_trip (
    lost_id,
    "id noClaim (editRequest (processed)) (view ghostWarrant) (diff (rel removed))" ); }

/// '(leaves N OBJECT...)' is written before '(processed ...)'; the parser
/// skips the whole '(diff ...)'.
#[test]
fn the_leaves_fact_renders_and_the_parser_skips_it () {
  let mut change_facts : ChangeFacts =
    facts ( None, Some (Existence::Added), Some (&[DiffObject::Rel]) );
  change_facts . leaves = Some ( LeavesFact {
    count : 2, objects : objects (&[DiffObject::Node, DiffObject::Rel]) } );
  let node : Viewnode = with_facts (
    mk_editable_viewnode (
      ID::from ("a"), SkgrepoName::from ("main"), "A" . into (), None ),
    change_facts, None );
  let rendered : String = viewnode_to_string (&node) . unwrap ();
  assert_eq! ( rendered,
               "node (gnid a) (skgRepos (nodeRepo main)) (diff (rel added) (leaves 2 node rel) (processed rel))" );
  let marker : &str = "(skg (changesetMarker content) (editRequest (processed all)) (diff (leaves 3 all)))";
  assert_eq! ( parse_metadata_to_viewnodemd (marker) . unwrap () . processed_request,
               request (true, &[DiffObject::All]) ); }

/// Every part of '(diff ...)' together, in their order; a headline
/// with nothing to say carries no '(diff ...)'.
#[test]
fn each_part_of_the_diff_form_renders_together_and_alone () {
  let mut change_facts : ChangeFacts =
    facts ( Some (Existence::Added), Some (Existence::Added), Some (&[DiffObject::Node]) );
  change_facts . leaves = Some ( LeavesFact {
    count : 2, objects : objects (&[DiffObject::Node, DiffObject::Rel]) } );
  let node : Viewnode = with_facts (
    mk_editable_viewnode (
      ID::from ("a"), SkgrepoName::from ("main"), "A" . into (), None ),
    change_facts, request (true, &[DiffObject::Rel]) );
  let rendered : String = viewnode_to_string (&node) . unwrap ();
  assert_eq! ( rendered,
               "node (gnid a) (skgRepos (nodeRepo main)) (editRequest (processed rel)) (diff (node added) (rel added) (leaves 2 node rel) (processed node))" );
  let alias = || viewnode_of ( ViewnodeKind::PropertyMember ( PropertyMember::alias (
    "an alias" . into (), None, None )));
  assert_round_trip (
    with_facts ( alias (), facts ( None, Some (Existence::Added), None ), None ),
    "alias (diff (rel added))" );
  assert_round_trip (
    with_facts ( alias (), facts ( None, None, Some (&[]) ), None ),
    "alias (diff (processed))" );
  assert_round_trip (
    with_facts ( alias (), ChangeFacts::default (), request (false, &[]) ),
    "alias (editRequest (unprocessed))" );
  assert_round_trip (
    with_facts ( alias (), ChangeFacts::default (), None ),
    "alias" ); }

/// What the parser refuses about the derived facts and the processed
/// requests. Inside '(diff ...)' and '(view ...)' it reads nothing, so
/// it refuses nothing there.
#[test]
fn malformed_facts_and_processed_requests_are_refused () {
  for metadata in [
    "(skg tombstone (diff (rel added)))",             // a tombstone carries none
    "(skg aliasFolder (diff (processed)))",               // nor does a folder
    "(skg (aliasFolder changed))",                    // a folder is its bare atom
    "(skg bodyDiff (editRequest (processed)))",           // a body diff takes no request
    "(skg tombstone (view noWarrant))",               // nor any view fact
    "(skg node (gnid a) (editRequest (processed) (unprocessed node)))", // two requests
    "(skg node (gnid a) (editRequest (processed sideways)))",
    "(skg node (gnid a) (editRequest delete (merge b)))",       // two edit requests
    "(skg id (editRequest (relRepo main)))",          // an id takes only processed requests
    "(skg alias (processed))",                            // a request is in '(editRequest ...)'
    "(skg node (gnid a) (processed node))",
    "(skg node (gnid a) (diff (processed)) (diff (rel added)))", // two forms
    "(skg node (gnid a) (view cycle) (view noWarrant))",
    // retired spellings
    "(skg node (gnid a) (readyRequest all))",
    "(skg node (gnid a) (unreadyRequest node))",
    "(skg node (gnid a) (rels (contains (in 1))))",
    "(skg node (gnid a) (viewStats cycle))",
    "(skg node (gnid a) writeProtected omittedBody)",
    "(skg node (gnid a) (homeRepoHerald main))", ] {
    assert! ( parse_metadata_to_viewnodemd (metadata) . is_err (),
              "should refuse {}", metadata ); }
  for metadata in [
    "(skg node (gnid a) (diff (sideways)) (view (anything at all)))",
    "(skg alias noClaim (view ghostWarrant) (diff (rel removed) (leaves 1)))", ] {
    assert! ( parse_metadata_to_viewnodemd (metadata) . is_ok (),
              "should skip the facts of {}", metadata ); }}
