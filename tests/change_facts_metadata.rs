// cargo nextest run --test grouped_unit -E 'test(change_facts_metadata::)'
//
// The diff view's derived facts '(diff ...)' -- its parts '(node
// ...)', '(rel ...)', '(properties changed)', '(leaves ...)' and
// '(ready ...)' -- and the readiness requests '(editRequest (ready
// ...))' and '(editRequest (unready ...))', on every viewnode that
// carries them: each renders; the parser skips '(diff ...)' whole and
// reads the readiness request back.

use crate::git_fixture_helpers::{assert_round_trip, with_facts};
use skg::org_to_text::viewnode_to_string;
use skg::serve::parse_metadata_sexp::{
  ViewnodeMetadata, parse_metadata_to_viewnodemd};
use skg::types::misc::{ID, SkgConfig, SkgrepoName};
use skg::types::nodes::complete::Flag;
use skg::types::viewnode::{
  ChangeFacts, DiffChange, Existence, Ghost, GnidOnlyKind, GnidOnlyVognode, LeavesFact, PropertyMember, DiffObject, ReadinessRequest, Viewnode,
  ViewnodeKind, Vognode, Warrant, mk_editable_viewnode,
  mk_writeProtected_viewnode, viewnode_of};

use std::collections::{BTreeSet, HashMap};

fn objects (
  objects : &[DiffObject],
) -> BTreeSet<DiffObject> {
  objects . iter () . copied () . collect () }

fn facts (
  node  : Option<Existence>,
  rel   : Option<Existence>,
  ready : Option<&[DiffObject]>,
) -> ChangeFacts {
  ChangeFacts { change : DiffChange { node, rel, properties : false },
                ready  : ready . map (objects),
                leaves : None,
                .. ChangeFacts::default () } }

/// The facts of a node in both graphs whose other properties changed.
fn modified (
  rel   : Option<Existence>,
  ready : Option<&[DiffObject]>,
) -> ChangeFacts {
  let mut change_facts : ChangeFacts = facts (None, rel, ready);
  change_facts . change . properties = true;
  change_facts }

fn request (
  ready : bool,
  on    : &[DiffObject],
) -> Option<ReadinessRequest> {
  Some ( ReadinessRequest { ready, objects : objects (on) } ) }



#[test]
fn a_vognode_renders_its_change_and_readiness_and_round_trips_its_request () {
  assert_round_trip (
    with_facts (
      mk_editable_viewnode (
        ID::from ("a"), SkgrepoName::from ("main"), "A" . into (), None ),
      facts ( Some (Existence::Added), Some (Existence::Removed),
              Some (&[DiffObject::Node, DiffObject::Rel]) ),
      request (true, &[DiffObject::All]) ),
    "node (gnid a) (skgRepos (nodeRepo main)) (editRequest (ready all)) (diff (node added) (rel removed) (ready node rel))" );
  // A readiness request is allowed on a write-protected occurrence.
  assert_round_trip (
    with_facts (
      mk_writeProtected_viewnode (
        ID::from ("b"), SkgrepoName::from ("main"), "B" . into (),
        true ),
      facts ( None, Some (Existence::Added), Some (&[]) ),
      request (false, &[DiffObject::Rel]) ),
    "node (gnid b) (skgRepos (nodeRepo main)) writeProtected (editRequest (unready rel)) (diff (rel added) (ready))" );
  // A modified node: its other properties changed, and '(ready all)'
  // says every leaf it records is marked.
  assert_round_trip (
    with_facts (
      mk_writeProtected_viewnode (
        ID::from ("m"), SkgrepoName::from ("main"), "M" . into (),
        true ),
      modified ( Some (Existence::Added),
                 Some (&[DiffObject::Rel, DiffObject::All]) ),
      request (false, &[DiffObject::Rel, DiffObject::All]) ),
    "node (gnid m) (skgRepos (nodeRepo main)) writeProtected (editRequest (unready rel all)) (diff (rel added) (properties changed) (ready rel all))" ); }

/// A node's own edit request and its readiness request share one
/// '(editRequest ...)', the readiness request last.
#[test]
fn an_edit_request_and_a_readiness_request_share_one_form () {
  let metadata : ViewnodeMetadata = parse_metadata_to_viewnodemd (
    "(skg node (gnid a) (editRequest delete (ready node)))" ) . unwrap ();
  assert_eq! ( metadata . readiness_request, request (true, &[DiffObject::Node]) );
  assert! ( metadata . edit_request . is_some () ); }

#[test]
fn ghosts_and_unknowns_round_trip_their_readiness_requests () {
  assert_round_trip (
    with_facts (
      viewnode_of ( ViewnodeKind::Vognode ( Vognode::Ghost ( Ghost {
          gnid : ID::from ("d"), home_skgrepo : SkgrepoName::from ("main"),
          title : "D" . into (), body : None,
          restoreNode_request : false,
          claimsMembership : true, warrant : Warrant::ByGraph, } ))),
      facts ( Some (Existence::Removed), None, Some (&[DiffObject::Node]) ),
      request (false, &[DiffObject::Node]) ),
    "ghost (gnid d) (skgRepos (nodeRepo main)) (editRequest (unready node)) (diff (node removed) (ready node))" );
  assert_round_trip (
    with_facts (
      viewnode_of ( ViewnodeKind::Vognode ( Vognode::GnidOnly ( GnidOnlyVognode::new (
          GnidOnlyKind::Unknown, ID::from ("u"), None, true, Warrant::ByGraph ) ))),
      facts ( None, Some (Existence::Added), None ),
      request (true, &[]) ),
    "unknown (gnid u) (editRequest (ready)) (diff (rel added))" ); }

#[test]
fn property_leaves_round_trip_their_readiness_requests () {
  assert_round_trip (
    with_facts (
      viewnode_of ( ViewnodeKind::PropertyMember ( PropertyMember::alias (
        "an alias" . into (), None, None ))),
      facts ( None, Some (Existence::Added), Some (&[]) ),
      None ),
    "alias (diff (rel added) (ready))" );
  assert_round_trip (
    with_facts (
      viewnode_of ( ViewnodeKind::PropertyMember ( PropertyMember::id (
        ID::from ("x") ))),
      facts ( None, Some (Existence::Added), None ),
      request (true, &[]) ),
    "id (editRequest (ready)) (diff (rel added))" );
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
    "id noClaim (editRequest (ready)) (view ghostWarrant) (diff (rel removed))" ); }

/// '(leaves N OBJECT...)' is written before '(ready ...)'; the parser
/// skips the whole '(diff ...)'.
#[test]
fn the_leaves_fact_renders_and_the_parser_skips_it () {
  let config : SkgConfig = SkgConfig::dummyFromSkgrepos (HashMap::new ());
  let mut change_facts : ChangeFacts =
    facts ( None, Some (Existence::Added), Some (&[DiffObject::Rel]) );
  change_facts . leaves = Some ( LeavesFact {
    count : 2, objects : objects (&[DiffObject::Node, DiffObject::Rel]) } );
  let node : Viewnode = with_facts (
    mk_editable_viewnode (
      ID::from ("a"), SkgrepoName::from ("main"), "A" . into (), None ),
    change_facts, None );
  let rendered : String = viewnode_to_string (&node, &config) . unwrap ();
  assert_eq! ( rendered,
               "node (gnid a) (skgRepos (nodeRepo main)) (diff (rel added) (leaves 2 node rel) (ready rel))" );
  let marker : &str = "(skg (changesetMarker content) (editRequest (ready all)) (diff (leaves 3 all)))";
  assert_eq! ( parse_metadata_to_viewnodemd (marker) . unwrap () . readiness_request,
               request (true, &[DiffObject::All]) ); }

/// Every part of '(diff ...)' together, in their order; a headline
/// with nothing to say carries no '(diff ...)'.
#[test]
fn each_part_of_the_diff_form_renders_together_and_alone () {
  let config : SkgConfig = SkgConfig::dummyFromSkgrepos (HashMap::new ());
  let mut change_facts : ChangeFacts =
    facts ( Some (Existence::Added), Some (Existence::Added), Some (&[DiffObject::Node]) );
  change_facts . leaves = Some ( LeavesFact {
    count : 2, objects : objects (&[DiffObject::Node, DiffObject::Rel]) } );
  let node : Viewnode = with_facts (
    mk_editable_viewnode (
      ID::from ("a"), SkgrepoName::from ("main"), "A" . into (), None ),
    change_facts, request (true, &[DiffObject::Rel]) );
  let rendered : String = viewnode_to_string (&node, &config) . unwrap ();
  assert_eq! ( rendered,
               "node (gnid a) (skgRepos (nodeRepo main)) (editRequest (ready rel)) (diff (node added) (rel added) (leaves 2 node rel) (ready node))" );
  let alias = || viewnode_of ( ViewnodeKind::PropertyMember ( PropertyMember::alias (
    "an alias" . into (), None, None )));
  assert_round_trip (
    with_facts ( alias (), facts ( None, Some (Existence::Added), None ), None ),
    "alias (diff (rel added))" );
  assert_round_trip (
    with_facts ( alias (), facts ( None, None, Some (&[]) ), None ),
    "alias (diff (ready))" );
  assert_round_trip (
    with_facts ( alias (), ChangeFacts::default (), request (false, &[]) ),
    "alias (editRequest (unready))" );
  assert_round_trip (
    with_facts ( alias (), ChangeFacts::default (), None ),
    "alias" ); }

/// What the parser refuses about the derived facts and the readiness
/// requests. Inside '(diff ...)' and '(view ...)' it reads nothing, so
/// it refuses nothing there.
#[test]
fn malformed_facts_and_readiness_requests_are_refused () {
  for metadata in [
    "(skg tombstone (diff (rel added)))",             // a tombstone carries none
    "(skg aliasFolder (diff (ready)))",               // nor does a folder
    "(skg (aliasFolder changed))",                    // a folder is its bare atom
    "(skg bodyDiff (editRequest (ready)))",           // a body diff takes no request
    "(skg tombstone (view noWarrant))",               // nor any view fact
    "(skg node (gnid a) (editRequest (ready) (unready node)))", // two requests
    "(skg node (gnid a) (editRequest (ready sideways)))",
    "(skg node (gnid a) (editRequest delete (merge b)))",       // two edit requests
    "(skg id (editRequest (relRepo main)))",          // an id takes only readiness
    "(skg alias (ready))",                            // a request is in '(editRequest ...)'
    "(skg node (gnid a) (ready node))",
    "(skg node (gnid a) (diff (ready)) (diff (rel added)))", // two forms
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
