// cargo nextest run --test grouped_unit -E 'test(change_facts_metadata::)'
//
// The total diff view's metadata forms -- '(diff ...)', '(ready ...)',
// '(readyRequest ...)' and '(unreadyRequest ...)' -- on every viewnode
// that carries them: each renders, then parses back to the same
// viewnode.

use skg::org_to_text::viewnode_to_string;
use skg::serve::parse_metadata_sexp::{
  ViewnodeMetadata, parse_metadata_to_viewnodemd, viewnode_from_metadata};
use skg::types::git::Sign;
use skg::types::maybe_placed_viewnode::MpViewnode;
use skg::types::misc::{ID, SkgConfig, SkgrepoName};
use skg::types::nodes::complete::Flag;
use skg::types::viewnode::{
  AffectsParent, ChangeFacts, DiffPresence, Phantom, PhantomDeleted,
  PhantomUnknown, Property, DiffObject, ReadinessRequest, Viewnode,
  ViewnodeKind, Vognode, mk_editable_viewnode, mk_writeProtected_viewnode};

use std::collections::{BTreeSet, HashMap};

fn objects (
  objects : &[DiffObject],
) -> BTreeSet<DiffObject> {
  objects . iter () . copied () . collect () }

fn facts (
  node  : Option<Sign>,
  rel   : Option<Sign>,
  ready : Option<&[DiffObject]>,
) -> ChangeFacts {
  ChangeFacts { presence : DiffPresence { node, rel },
                ready    : ready . map (objects) } }

fn request (
  ready : bool,
  on    : &[DiffObject],
) -> Option<ReadinessRequest> {
  Some ( ReadinessRequest { ready, objects : objects (on) } ) }

fn viewnode (
  kind : ViewnodeKind,
) -> Viewnode {
  Viewnode { focused : false, org_folded : false, org_body_folded : false, kind } }

/// Render 'node', check its metadata against 'expected', parse it
/// back, and check that the result is 'node' again.
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
  let (parsed, error, _warning) : (MpViewnode, _, _) =
    viewnode_from_metadata (
      &metadata, node . title () . to_string (), node . body () . cloned () );
  assert_eq! ( error, None );
  assert_eq! ( Viewnode::try_from (parsed) . unwrap (), node ); }

fn with_facts (
  mut node : Viewnode,
  change_facts      : ChangeFacts,
  readiness_request : Option<ReadinessRequest>,
) -> Viewnode {
  if let ViewnodeKind::Vognode (Vognode::Unrestricted (t)) = &mut node . kind {
    t . change_facts = change_facts;
    t . readiness_request = readiness_request; }
  node }

#[test]
fn a_vognode_round_trips_its_presence_readiness_and_request () {
  assert_round_trip (
    with_facts (
      mk_editable_viewnode (
        ID::from ("a"), SkgrepoName::from ("main"), "A" . into (), None ),
      facts ( Some (Sign::Plus), Some (Sign::Minus),
              Some (&[DiffObject::Node, DiffObject::Rel]) ),
      request (true, &[DiffObject::All]) ),
    "(node (id a) (repo main) (diff node+ rel-) (ready node rel) (readyRequest all))" );
  // A readiness request is allowed on a write-protected occurrence.
  assert_round_trip (
    with_facts (
      mk_writeProtected_viewnode (
        ID::from ("b"), SkgrepoName::from ("main"), "B" . into (),
        AffectsParent::True ),
      facts ( None, Some (Sign::Plus), Some (&[]) ),
      request (false, &[DiffObject::Rel]) ),
    "(node (id b) (repo main) writeProtected (diff rel+) (ready) (unreadyRequest rel))" ); }

#[test]
fn phantoms_round_trip_their_change_facts () {
  assert_round_trip (
    viewnode ( ViewnodeKind::Vognode ( Vognode::Phantom (
      Phantom::Deleted ( PhantomDeleted {
        skgid : ID::from ("d"), home_skgrepo : SkgrepoName::from ("main"),
        title : "D" . into (), body : None,
        change_facts : facts ( Some (Sign::Minus), None, Some (&[]) ),
        readiness_request : request (false, &[]), } )))),
    "(deleted (id d) (repo main) (diff node-) (ready) (unreadyRequest))" );
  assert_round_trip (
    viewnode ( ViewnodeKind::Vognode ( Vognode::Phantom (
      Phantom::Unknown ( PhantomUnknown {
        skgid : ID::from ("u"), relRepo : None, relRepo_request : None,
        change_facts : facts ( None, Some (Sign::Plus), None ),
        readiness_request : request (true, &[]), } )))),
    "(unknown (id u) (diff rel+) (readyRequest))" ); }

#[test]
fn property_leaves_round_trip_their_change_facts () {
  assert_round_trip (
    viewnode ( ViewnodeKind::Property ( Property::Alias {
      text : "an alias" . into (), relRepo : None, relRepo_request : None,
      change_facts : facts ( None, Some (Sign::Plus), Some (&[]) ),
      readiness_request : None, } )),
    "alias (diff rel+) (ready)" );
  assert_round_trip (
    viewnode ( ViewnodeKind::Property ( Property::ID {
      skgid : ID::from ("x"),
      change_facts : facts ( None, Some (Sign::Minus), None ),
      readiness_request : request (true, &[]), } )),
    "id (diff rel-) (readyRequest)" );
  assert_round_trip (
    viewnode ( ViewnodeKind::Property ( Property::Flag {
      flag : Flag::NoSearchMatching, title : String::new (), body : None,
      change_facts : facts ( None, Some (Sign::Plus), None ),
      readiness_request : None, } )),
    &format! ( "(flag {}) (diff rel+)", Flag::NoSearchMatching . wire_name () )); }

#[test]
fn malformed_change_facts_are_refused () {
  for metadata in [
    "(skg (diff rel+))",                              // top level, not a property
    "(skg aliasFolder (diff rel+))",                  // a folder's only fact is '(ready)'
    "(skg (aliasFolder sideways))",                   // a folder's form is '(KIND changed)'
    "(skg (node (id a) (ready all)))",                // 'all' only in a request
    "(skg (node (id a) (diff node+ node-)))",         // an object twice
    "(skg (node (id a) (readyRequest) (unreadyRequest node)))", // two requests
    "(skg (node (id a) (readyRequest sideways)))", ] {
    assert! ( parse_metadata_to_viewnodemd (metadata) . is_err (),
              "should refuse {}", metadata ); }}
